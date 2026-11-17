# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Loop continuations for tiered execution (see `src/tiered.c`).

When a loop in an interpreted frame gets hot, the interpreter calls [`_tier_loop`](@ref)
with the frame's lowered code, its `MethodInstance`, the loop header and a snapshot of the
frame state. This module then compiles a *continuation* for the outermost loop around that
header (just the loop, not the rest of the function), runs it from the frame state, and
writes the values that are live at the loop exit back into the state. The interpreter
resumes at the exit that was taken.

A continuation is an `OpaqueClosure` whose arguments are the values that are live into the
loop header. Its signature is widened to the types inference finds for those values at the
header (the loop's fixpoint), not the types observed when the counter tripped, so which
continuation runs never depends on how many iterations happened to run first.
Continuations are cached per (`MethodInstance`, header, live-in types), so a later
interpreted call enters the compiled loop directly.
"""
module Tiered

using Core: CodeInfo, SSAValue, SlotNumber, GotoNode, GotoIfNot, ReturnNode, EnterNode,
    NewvarNode, GlobalRef, MethodInstance, Argument, PhiNode, PhiCNode, PiNode, UpsilonNode
using Base: IdDict, ReentrantLock, @lock, BitSet, isexpr
using Base.Compiler: widenconst

const Compiler = Base.Compiler

# Marks a live-in variable that is undefined at loop entry in a cache key.
struct Undef end

## Control flow and loops in lowered code

# Successors of statement `i`, including the edge from an `EnterNode` to its catch block.
function succs(code::Vector{Any}, i::Int)
    st = code[i]
    if st isa GotoNode
        return (st.label, 0)
    elseif st isa GotoIfNot
        return (i + 1, st.dest)
    elseif st isa ReturnNode
        return (0, 0)
    elseif st isa EnterNode
        return (i + 1, st.catch_dest)
    else
        return (i + 1, 0)
    end
end

function cfg_edges(code::Vector{Any})
    n = length(code)
    preds = [Int[] for _ in 1:n]
    for i in 1:n, s in succs(code, i)
        1 <= s <= n && push!(preds[s], i)
    end
    return preds
end

# Immediate dominators of the statement graph (Cooper, Harvey & Kennedy).
# Unreachable statements have idom 0.
function dominators(code::Vector{Any}, preds::Vector{Vector{Int}})
    n = length(code)
    post = Int[]
    seen = falses(n)
    stack = Tuple{Int,Int}[(1, 1)]
    seen[1] = true
    while !isempty(stack)
        i, k = stack[end]
        ss = succs(code, i)
        if k > 2
            pop!(stack)
            push!(post, i)
            continue
        end
        stack[end] = (i, k + 1)
        s = ss[k]
        if 1 <= s <= n && !seen[s]
            seen[s] = true
            push!(stack, (s, 1))
        end
    end
    order = zeros(Int, n) # postorder number
    for (k, i) in enumerate(post)
        order[i] = k
    end
    idom = zeros(Int, n)
    idom[1] = 1
    changed = true
    while changed
        changed = false
        for k in length(post)-1:-1:1
            b = post[k]
            new = 0
            for p in preds[b]
                idom[p] == 0 && continue
                if new == 0
                    new = p
                else
                    a, c = p, new
                    while a != c
                        while order[a] < order[c]; a = idom[a]; end
                        while order[c] < order[a]; c = idom[c]; end
                    end
                    new = a
                end
            end
            if new != 0 && idom[b] != new
                idom[b] = new
                changed = true
            end
        end
    end
    return idom
end

function dominates(idom::Vector{Int}, a::Int, b::Int)
    idom[b] == 0 && return false
    while true
        b == a && return true
        b == 1 && return false
        b = idom[b]
    end
end

# The natural loop with header `h`, or `nothing` if `h` has no back-edge.
function natural_loop(preds::Vector{Vector{Int}}, idom::Vector{Int}, h::Int)
    body = BitSet(h)
    work = Int[]
    for p in preds[h]
        dominates(idom, h, p) && push!(work, p)
    end
    isempty(work) && return nothing
    while !isempty(work)
        i = pop!(work)
        i in body && continue
        push!(body, i)
        for p in preds[i]
            idom[p] != 0 && !(p in body) && push!(work, p)
        end
    end
    return body
end

## Liveness

# Variables are numbered as in the interpreter's frame: slots first, then SSA values.
function foreach_use(f, @nospecialize(x), nslots::Int)
    if x isa SlotNumber
        f(x.id)
    elseif x isa SSAValue
        f(nslots + x.id)
    elseif x isa GotoIfNot
        foreach_use(f, x.cond, nslots)
    elseif x isa ReturnNode
        isdefined(x, :val) && foreach_use(f, x.val, nslots)
    elseif x isa EnterNode
        isdefined(x, :scope) && foreach_use(f, x.scope, nslots)
    elseif x isa Expr
        args = x.args
        if x.head === :(=)
            foreach_use(f, args[2], nslots)
        else
            for a in args
                foreach_use(f, a, nslots)
            end
        end
    end
    nothing
end

# Does statement `st` produce an SSA value?
defines_ssa(@nospecialize st) =
    !(st isa GotoNode || st isa GotoIfNot || st isa ReturnNode || st isa NewvarNode ||
      (st isa Expr && st.head === :(=)))

# The slot written by statement `st`, or 0.
function slot_def(@nospecialize st)
    if st isa NewvarNode
        return st.slot.id
    elseif st isa Expr && st.head === :(=)
        lhs = st.args[1]
        return lhs isa SlotNumber ? lhs.id : 0
    end
    return 0
end

# Variables live on entry to each statement. `exc` lists, for each statement inside a
# `try`, the catch block an exception there would reach.
function liveness(code::Vector{Any}, nslots::Int, exc::Vector{Int})
    n = length(code)
    live = [BitSet() for _ in 1:n]
    uses = [BitSet() for _ in 1:n]
    for i in 1:n
        u = uses[i]
        foreach_use(v -> push!(u, v), code[i], nslots)
    end
    changed = true
    while changed
        changed = false
        for i in n:-1:1
            out = BitSet()
            for s in succs(code, i)
                1 <= s <= n && union!(out, live[s])
            end
            e = exc[i]
            e != 0 && union!(out, live[e])
            d = slot_def(code[i])
            d != 0 && delete!(out, d)
            delete!(out, nslots + i)
            union!(out, uses[i])
            if out != live[i]
                live[i] = out
                changed = true
            end
        end
    end
    return live
end

## Loop plans

struct LoopPlan
    header::Int                # entry statement (1-based)
    region::BitSet             # statements compiled into the continuation
    livein::Vector{Int}        # variables passed in at the header
    exits::Vector{Int}         # statements outside the region control can reach
    liveout::Vector{Vector{Int}} # per exit: variables to write back
    excsave::Vector{Int}       # variables to write back when an exception escapes
    handlers::Vector{Int}      # does an enclosing handler in the frame see exceptions?
    ssaslots::Vector{Int}      # SSA values carried in slots in the continuation
end

const SUPPORTED_HEADS = Base.IdSet{Symbol}(Any[:call, :invoke, :new, :splatnew, :(=), :leave,
    :pop_exception, :the_exception, :isdefined, :throw_undef_if_not, :static_parameter,
    :boundscheck, :meta, :inbounds, :loopinfo, :gc_preserve_begin, :gc_preserve_end,
    :new_opaque_closure, :copyast, :coverageeffect, :aliasscope, :popaliasscope, :inline,
    :noinline])

function supported(@nospecialize(x), sparams::Core.SimpleVector)
    if x isa Expr
        x.head in SUPPORTED_HEADS || return false
        if x.head === :static_parameter
            n = x.args[1]
            n isa Int && 1 <= n <= length(sparams) || return false
            sp = sparams[n]
            return !(sp isa TypeVar || sp isa Core.SimpleVector || Base.isvarargtype(sp))
        elseif x.head === :(=)
            x.args[1] isa SlotNumber || return false
        end
        for a in x.args
            supported(a, sparams) || return false
        end
        return true
    elseif x isa GotoIfNot
        return supported(x.cond, sparams)
    elseif x isa ReturnNode
        return !isdefined(x, :val) || supported(x.val, sparams)
    elseif x isa EnterNode
        return !isdefined(x, :scope) || supported(x.scope, sparams)
    elseif x isa Argument || x isa PhiNode || x isa PhiCNode || x isa PiNode || x isa UpsilonNode
        return false
    end
    return true
end

# Plan the continuation entered at loop header `h`, or `nothing` if it cannot have one.
function plan_loop(src::CodeInfo, sparams::Core.SimpleVector, h::Int)
    code = src.code
    n = length(code)
    nslots = length(src.slotnames)
    1 <= h <= n || return nothing
    for st in code
        supported(st, sparams) || return nothing
    end
    preds = cfg_edges(code)
    idom = dominators(code, preds)
    natural_loop(preds, idom, h) === nothing && return nothing # irreducible

    hinfo = Compiler.compute_trycatch(code)
    ctx(i) = hinfo === nothing ? (0, 0) : hinfo.handler_at[i]
    hctx = ctx(h)

    # The unit is the outermost loop around `h` that is in the same handler context.
    region = BitSet()
    for g in 1:n
        ctx(g) == hctx || continue
        body = natural_loop(preds, idom, g)
        body === nothing && continue
        h in body || continue
        if length(body) > length(region)
            region = body
        end
    end

    # Close the region over handler contexts: anything control reaches inside a `try`
    # or `catch` that was entered in the loop stays in the continuation, so every exit
    # leaves at the context of the header.
    work = collect(region)
    while !isempty(work)
        i = pop!(work)
        for s in succs(code, i)
            if 1 <= s <= n && !(s in region) && ctx(s) != hctx
                push!(region, s)
                push!(work, s)
            end
        end
    end

    # Handlers and exception stacks entered in the region must be left in it.
    for i in region
        st = code[i]
        if st isa ReturnNode && isdefined(st, :val)
            return nothing
        elseif isexpr(st, :leave)
            for a in st.args
                a isa SSAValue && !(a.id in region) && return nothing
            end
        elseif isexpr(st, :pop_exception)
            a = st.args[1]
            a isa SSAValue && !(a.id in region) && return nothing
        end
    end

    # Exceptional edges, for liveness: each statement inside a `try` can reach its catch.
    exc = zeros(Int, n)
    handlers = Int[]
    if hinfo !== nothing
        for i in 1:n
            hid = hinfo.handler_at[i][1]
            if hid != 0
                enter = code[Compiler.get_enter_idx(hinfo.handlers[hid])]::EnterNode
                exc[i] = enter.catch_dest
            end
        end
        # handlers in the frame that enclose the loop
        hid = hctx[1]
        while hid != 0
            eidx = Compiler.get_enter_idx(hinfo.handlers[hid])
            c = (code[eidx]::EnterNode).catch_dest
            c != 0 && push!(handlers, c)
            hid = hinfo.handler_at[eidx][1]
        end
    end
    live = liveness(code, nslots, exc)

    used = BitSet()
    written = BitSet()
    for i in region
        st = code[i]
        foreach_use(v -> push!(used, v), st, nslots)
        d = slot_def(st)
        d != 0 && push!(written, d)
        defines_ssa(st) && push!(written, nslots + i)
    end
    # Variables the loop reads, and variables it may write (on paths that don't write
    # them, the incoming value reaches the exit).
    livein = sort!(collect(intersect(live[h], union(used, written))))

    exits = Int[]
    for i in region, s in succs(code, i)
        1 <= s <= n && !(s in region) && !(s in exits) && push!(exits, s)
    end
    sort!(exits)
    liveout = [sort!(collect(intersect(live[t], written))) for t in exits]
    excsave = BitSet()
    for c in handlers
        union!(excsave, intersect(live[c], written))
    end
    excsave = sort!(collect(excsave))

    ssaslots = BitSet()
    for v in livein
        v > nslots && push!(ssaslots, v - nslots)
    end
    for vs in liveout, v in vs
        v > nslots && push!(ssaslots, v - nslots)
    end
    for v in excsave
        v > nslots && push!(ssaslots, v - nslots)
    end
    for j in ssaslots
        if j in region
            st = code[j]
            (st isa EnterNode || !defines_ssa(st)) && return nothing
        end
    end
    # SSA values used in the region must be defined in it or carried in.
    for i in region
        uses = BitSet()
        foreach_use(v -> push!(uses, v), code[i], nslots)
        for v in uses
            v > nslots && !((v - nslots) in region) && !((v - nslots) in ssaslots) && return nothing
        end
    end
    return LoopPlan(h, region, livein, exits, liveout, excsave, handlers, sort!(collect(ssaslots)))
end

## Building the continuation

@inline setstate!(state::Vector{Any}, i::Int, @nospecialize(v)) = (@inbounds state[i] = v; nothing)
@inline unsetstate!(state::Vector{Any}, i::Int) = (Base._unsetindex!(state, i); nothing)

# Appends statements to a CodeInfo under construction, returning each one's position.
# Each statement records the original statement it came from, for line information.
mutable struct Emitter
    const code::Vector{Any}
    const flags::Vector{UInt32}
    const origin::Vector{Int32}
    at::Int32 # the original statement being emitted, or 0
end
function (e::Emitter)(@nospecialize(st), flag::UInt32=UInt32(0))
    push!(e.code, st)
    push!(e.flags, flag)
    push!(e.origin, e.at)
    return length(e.code)
end

# Build the continuation for `plan`, given which live-in variables are defined. With
# `probe`, statements reading each live-in variable are placed at the loop header, so
# inference reports the variables' types at the loop's fixpoint.
function build_continuation(src::CodeInfo, sparams::Core.SimpleVector, plan::LoopPlan,
                            defined::Vector{Bool}, probe::Bool)
    code = src.code
    n = length(code)
    nslots = length(src.slotnames)
    h = plan.header

    # slot layout: #self#, state, arguments, original slots, SSA values carried in slots
    slotnames = Symbol[Symbol("#self#"), Symbol("#state#")]
    slotflags = UInt8[0x00, 0x08]
    argslot = Dict{Int,Int}()
    for (k, v) in enumerate(plan.livein)
        defined[k] || continue
        push!(slotnames, v <= nslots ? src.slotnames[v] : Symbol("#ssa", v - nslots))
        push!(slotflags, 0x08)
        argslot[v] = length(slotnames)
    end
    nargs = length(slotnames)
    varslot = zeros(Int, nslots)
    for k in 1:nslots
        push!(slotnames, src.slotnames[k])
        push!(slotflags, (src.slotflags[k] & ~0x10) | 0x02 | 0x08)
        varslot[k] = length(slotnames)
    end
    ssaslot = Dict{Int,Int}()
    for j in plan.ssaslots
        push!(slotnames, Symbol("#ssa", j))
        push!(slotflags, 0x02 | 0x08)
        ssaslot[j] = length(slotnames)
    end
    slotof(v::Int) = v <= nslots ? varslot[v] : ssaslot[v - nslots]

    nprobe = count(defined)
    has_handler = !isempty(plan.handlers)
    shift = length(argslot) + (has_handler ? 1 : 0) + 1
    pos(i::Int) = shift + i + (i >= h ? nprobe : 0)
    label(i::Int) = i == h ? shift + h : pos(i)
    nbody = shift + n + nprobe

    newcode = Any[]
    ssaflags = UInt32[]
    emit! = Emitter(newcode, ssaflags, Int32[], 0)

    # stubs are laid out after the body; compute their labels first
    exitlabel = Dict{Int,Int}()
    stubsize(vs) = 5 * length(vs) + (has_handler ? 1 : 0) + 1
    next = nbody + 1
    for (k, t) in enumerate(plan.exits)
        exitlabel[t] = next
        next += stubsize(plan.liveout[k])
    end
    catchlabel = next
    enter_pos = shift - 1 # position of the EnterNode in the prologue, if any

    stateslot = SlotNumber(2)
    rw(@nospecialize x) = rewrite(x, nslots, varslot, ssaslot, pos, label, sparams)

    # prologue
    for (k, v) in enumerate(plan.livein)
        defined[k] || continue
        emit!(Expr(:(=), SlotNumber(slotof(v)), SlotNumber(argslot[v])))
    end
    has_handler && emit!(EnterNode(catchlabel))
    emit!(GotoNode(label(h)))
    @assert length(newcode) == shift

    for i in 1:n
        if i == h
            for (k, v) in enumerate(plan.livein)
                defined[k] || continue
                emit!(probe ? SlotNumber(slotof(v)) : nothing)
            end
        end
        emit!.at = i
        if i in plan.region
            st = rw(code[i])
            if (i in plan.ssaslots)
                st = Expr(:(=), SlotNumber(ssaslot[i]), st)
            end
            emit!(st, src.ssaflags[i])
        elseif haskey(exitlabel, i)
            emit!(GotoNode(exitlabel[i]))
        else
            emit!(ReturnNode())
        end
    end
    @assert length(newcode) == nbody
    emit!.at = 0

    save = GlobalRef(Tiered, :setstate!)
    unsave = GlobalRef(Tiered, :unsetstate!)
    function emit_save!(v::Int)
        s = SlotNumber(slotof(v))
        d = emit!(Expr(:isdefined, s))
        here = length(newcode)
        emit!(GotoIfNot(SSAValue(d), here + 4))
        emit!(Expr(:call, save, stateslot, v, s))
        emit!(GotoNode(here + 5))
        emit!(Expr(:call, unsave, stateslot, v))
    end
    for (k, t) in enumerate(plan.exits)
        @assert length(newcode) + 1 == exitlabel[t]
        emit!.at = t
        foreach(emit_save!, plan.liveout[k])
        has_handler && emit!(Expr(:leave, SSAValue(enter_pos)))
        emit!(ReturnNode(t))
    end
    if has_handler
        @assert length(newcode) + 1 == catchlabel
        foreach(emit_save!, plan.excsave)
        emit!(Expr(:call, GlobalRef(Base, :rethrow)))
        emit!(ReturnNode())
    end

    cont = copy(src)
    cont.code = newcode
    cont.ssaflags = ssaflags
    cont.ssavaluetypes = length(newcode)
    cont.slotnames = slotnames
    cont.slotflags = slotflags
    cont.slottypes = nothing
    cont.nargs = nargs
    cont.isva = false
    cont.debuginfo = continuation_debuginfo(src.debuginfo, emit!.origin)
    cont.purity = 0x0000
    cont.inlining = 0x00
    cont.propagate_inbounds = false
    cont.method_for_inference_limit_heuristics = nothing
    cont.edges = nothing
    return cont, nargs - 1, h + shift # last: position of the first probe
end

# Line information for the continuation: each statement has the location of the original
# statement it came from.
function continuation_debuginfo(di::Core.DebugInfo, origin::Vector{Int32})
    n = length(origin)
    stream = Compiler.DebugInfoStream(nothing, di, n)
    for k in 1:n
        stream.codelocs[3k-2] = origin[k]
    end
    stream.def = di.def
    return Core.DebugInfo(stream, n)
end

function rewrite(@nospecialize(x), nslots::Int, varslot::Vector{Int}, ssaslot::Dict{Int,Int},
                 pos, label, sparams::Core.SimpleVector)
    rw(@nospecialize y) = rewrite(y, nslots, varslot, ssaslot, pos, label, sparams)
    if x isa SlotNumber
        return SlotNumber(varslot[x.id])
    elseif x isa SSAValue
        s = get(ssaslot, x.id, 0)
        return s == 0 ? SSAValue(pos(x.id)) : SlotNumber(s)
    elseif x isa GotoNode
        return GotoNode(label(x.label))
    elseif x isa GotoIfNot
        return GotoIfNot(rw(x.cond), label(x.dest))
    elseif x isa ReturnNode
        return x # only `unreachable` occurs in a region
    elseif x isa EnterNode
        dest = x.catch_dest == 0 ? 0 : label(x.catch_dest)
        return isdefined(x, :scope) ? EnterNode(dest, rw(x.scope)) : EnterNode(dest)
    elseif x isa NewvarNode
        return NewvarNode(SlotNumber(varslot[x.slot.id]))
    elseif x isa Expr
        if x.head === :static_parameter
            return QuoteNode(sparams[x.args[1]::Int])
        elseif x.head === :isdefined && length(x.args) == 1 && isexpr(x.args[1], :static_parameter)
            return true
        end
        return Expr(x.head, Any[rw(a) for a in x.args]...)
    end
    return x
end

## Compiling and caching continuations

mutable struct Continuation
    const sig::Type         # widened argument types (after the state vector)
    const source::Method    # the continuation's method
    oc::Core.OpaqueClosure  # wrapper for the world it was last used in
end

mutable struct LoopEntry
    const plan::Union{Nothing,LoopPlan}
    const byobserved::IdDict{Any,Continuation} # observed live-in types => continuation
    const bywidened::IdDict{Any,Continuation}  # widened live-in types => continuation
    const pending::IdDict{Any,Any}             # observed live-in types => job being compiled
    const failed::IdDict{Any,Nothing}          # observed live-in types whose compile failed
end
LoopEntry(plan) = LoopEntry(plan, IdDict{Any,Continuation}(), IdDict{Any,Continuation}(),
                            IdDict{Any,Any}(), IdDict{Any,Nothing}())

# A continuation to compile in the background: the compile thread runs it (see
# `_tier_compile_job`), in the world of the frame whose loop got hot. The C queue holds it
# unrooted; `entry.pending` keeps it alive until it is done. It is mutable so that the
# object queued is the one `entry.pending` holds: an immutable one would be queued as a
# copy that nothing roots.
mutable struct ContinuationJob
    const entry::LoopEntry
    const src::CodeInfo
    const mi::MethodInstance
    const h::Int
    const defined::Vector{Bool}
    const observed::Vector{Any}
    const key::Any
    const world::UInt
end

const cache_lock = ReentrantLock()
const loops = IdDict{MethodInstance,Dict{Int,LoopEntry}}()
# Loop headers with cached continuations, per MethodInstance. The interpreter reads these
# vectors without rooting them, so they are copied on write and every version is kept.
const headers = IdDict{MethodInstance,Vector{Int}}()
const all_headers = Vector{Int}[]

# For tests and diagnostics: continuations compiled, continuations entered, and time (ns)
# spent widening live-in types and compiling continuations.
const stats = [0, 0, 0, 0]

function loop_entry(src::CodeInfo, mi::MethodInstance, h::Int)
    entry = @lock cache_lock begin
        d = get(loops, mi, nothing)
        d === nothing ? nothing : get(d, h, nothing)
    end
    entry === nothing || return entry
    plan = try
        plan_loop(src, mi.sparam_vals, h)
    catch err
        err isa InterruptException && rethrow()
        nothing
    end
    entry = LoopEntry(plan)
    return @lock cache_lock begin
        d = get!(() -> Dict{Int,LoopEntry}(), loops, mi)
        get!(d, h, entry)
    end
end

# The types of the live-in variables at the loop header once inference reaches the loop's
# fixpoint, starting from the observed types.
function widen_livein(src::CodeInfo, mi::MethodInstance, plan::LoopPlan,
                      defined::Vector{Bool}, observed::Vector{Any})
    probe, nargs, firstprobe = build_continuation(src, mi.sparam_vals, plan, defined, true)
    oc = Base.Experimental.generate_opaque_closure(Tuple{Vector{Any}, observed...}, Union{}, Any,
        probe, nargs, false; mod=(mi.def::Method).module, do_compile=false, isinferred=false)
    pmi = first(Base.specializations(oc.source))::MethodInstance
    interp = Compiler.NativeInterpreter(Base.tls_world_age())
    frame = Compiler.typeinf_frame(interp, pmi, false)
    widened = copy(observed)
    frame === nothing && return widened
    for k in 1:length(observed)
        t = widenconst(frame.ssavaluetypes[firstprobe + k - 1])
        if t !== Union{}
            widened[k] = observed[k] <: t ? t : Any
        end
    end
    return widened
end

function compile_continuation(src::CodeInfo, mi::MethodInstance, plan::LoopPlan,
                              defined::Vector{Bool}, widened::Vector{Any})
    F = Tuple{Vector{Any}, widened...}
    cont, nargs, _ = build_continuation(src, mi.sparam_vals, plan, defined, false)
    m = mi.def::Method
    oc = Base.Experimental.generate_opaque_closure(F, Union{}, Int, cont, nargs, false;
        mod=m.module, do_compile=false, isinferred=false)
    # Name the continuation after the method it comes from, so that it reads as that
    # method in backtraces and profiles. This must precede compiling it.
    source = oc.source
    source.name = m.name
    oc = ccall(:jl_tier_continuation_in_world, Any, (Any, Any, Any), F, Int, source)::Core.OpaqueClosure
    return Continuation(F, source, oc)
end

# A cache key for live-in types, recording which variables are undefined.
function livein_key(defined::Vector{Bool}, types::Vector{Any})
    key = Vector{Any}(undef, length(defined))
    j = 0
    for k in 1:length(defined)
        key[k] = defined[k] ? types[j += 1] : Undef
    end
    return Tuple{key...}
end

# Compile a continuation for the loop at `h` from live-in types `observed`, and cache it for
# them. Returns it, or `nothing` if it could not be compiled.
function compile_and_cache!(entry::LoopEntry, src::CodeInfo, mi::MethodInstance, h::Int,
                            defined::Vector{Bool}, observed::Vector{Any}, key)
    plan = entry.plan::LoopPlan
    c = try
        t0 = time_ns()
        widened = widen_livein(src, mi, plan, defined, observed)
        t1 = time_ns()
        @lock cache_lock stats[3] += t1 - t0
        wkey = livein_key(defined, widened)
        c = @lock cache_lock get(entry.bywidened, wkey, nothing)
        if c === nothing
            c = compile_continuation(src, mi, plan, defined, widened)
            t2 = time_ns()
            @lock cache_lock begin
                c = get!(entry.bywidened, wkey, c)
                stats[1] += 1
                stats[4] += t2 - t1
            end
        end
        c
    catch err
        err isa InterruptException && rethrow()
        nothing
    end
    c === nothing && return nothing
    @lock cache_lock begin
        entry.byobserved[key] = c
        hs = get(headers, mi, Int[])
        if !(h in hs)
            hs = push!(copy(hs), h)
            push!(all_headers, hs)
            headers[mi] = hs
        end
    end
    ccall(:jl_tier_note_loop_cache, Cvoid, (Any,), mi.def)
    return c
end

# Run by the compile thread (see `tier_compile_continuation` in src/tiered.c).
_tier_compile_job(job::ContinuationJob) = Base.invoke_in_world(job.world, compile_job, job)

function compile_job(job::ContinuationJob)
    entry = job.entry
    c = compile_and_cache!(entry, job.src, job.mi, job.h, job.defined, job.observed, job.key)
    @lock cache_lock begin
        c === nothing && (entry.failed[job.key] = nothing)
        delete!(entry.pending, job.key)
    end
    return nothing
end

"""
    _tier_loop(src, mi, h, state, compile) -> Int

Called by the interpreter at loop header `h` (a 1-based statement index) of a frame
running `src` for `mi`, with the frame's slots and SSA values in `state`. Runs a
continuation for the loop and returns the statement to resume at, after writing the live-out
values into `state`. Returns 0 if there is no continuation (with `compile` unset, only
cached ones are used), -1 if this loop can never have one, and -2 if one is being compiled in
the background, when the frame keeps interpreting and asks again later.
"""
function _tier_loop(src::CodeInfo, mi::MethodInstance, h::Int, state::Vector{Any}, compile::Bool)
    entry = loop_entry(src, mi, h)
    plan = entry.plan
    plan === nothing && return -1
    livein = plan.livein
    nin = length(livein)
    defined = Vector{Bool}(undef, nin)
    observed = Any[]
    for k in 1:nin
        v = livein[k]
        if isassigned(state, v)
            t = Core.Typeof(state[v])
            Base.has_free_typevars(t) && return -1
            defined[k] = true
            push!(observed, t)
        else
            defined[k] = false
        end
    end
    key = livein_key(defined, observed)
    c = @lock cache_lock get(entry.byobserved, key, nothing)
    if c === nothing
        compile || return 0
        if ccall(:jl_tier_get_background, Cint, ()) != 0
            # compile it on the compile thread, interpreting meanwhile
            job = @lock cache_lock begin
                haskey(entry.failed, key) && return -1
                haskey(entry.pending, key) && return -2
                entry.pending[key] = ContinuationJob(entry, src, mi, h, defined, observed, key,
                                                     Base.tls_world_age())
            end
            ccall(:jl_tier_enqueue_continuation, Cvoid, (Any,), job)
            return -2
        end
        c = compile_and_cache!(entry, src, mi, h, defined, observed, key)
        c === nothing && return -1
    end
    oc = c.oc
    if getfield(oc, :world) != Base.tls_world_age()
        oc = ccall(:jl_tier_continuation_in_world, Any, (Any, Any, Any), c.sig, Int, c.source)::Core.OpaqueClosure
        c.oc = oc
    end
    args = Any[state]
    for k in 1:nin
        defined[k] && push!(args, state[livein[k]])
    end
    @lock cache_lock stats[2] += 1
    return oc(args...)::Int
end

_tier_loop_headers(mi::MethodInstance) = @lock cache_lock get(headers, mi, nothing)

"""
    Tiered.compiled_tier!(on::Bool=true) -> Bool

Run code called from the current task, and from tasks it starts afterwards, in the
compiled tier (as if tiering were off) if `on`, or tiered again if not. Returns the
previous setting. Tests of compilation behavior use this, since they assume that calling
a function compiles it.
"""
function compiled_tier!(on::Bool=true)
    old = ccall(:jl_tier_compiled_enter, UInt8, ())
    on || ccall(:jl_tier_compiled_leave, Cvoid, (UInt8,), 0x0)
    return old != 0
end

# An idle thread of the default pool runs this, as a task of its own, to help compile what
# tiering has queued (see jl_tier_steal_task in src/tiered.c).
function _tier_steal()
    try
        while ccall(:jl_tier_steal_one, Cint, ()) != 0
        end
    finally
        ccall(:jl_tier_steal_done, Cvoid, ())
    end
    return nothing
end

# The machinery must itself run compiled: with tiering on, anything not in the system
# image would start out in the interpreter.
precompile(_tier_steal, ())
precompile(_tier_loop, (CodeInfo, MethodInstance, Int, Vector{Any}, Bool))
precompile(_tier_loop_headers, (MethodInstance,))
precompile(plan_loop, (CodeInfo, Core.SimpleVector, Int))
precompile(widen_livein, (CodeInfo, MethodInstance, LoopPlan, Vector{Bool}, Vector{Any}))
precompile(compile_continuation, (CodeInfo, MethodInstance, LoopPlan, Vector{Bool}, Vector{Any}))

end # module Tiered

const _tier_loop = Tiered._tier_loop
const _tier_loop_headers = Tiered._tier_loop_headers
const _tier_steal = Tiered._tier_steal
const _tier_compile_job = Tiered._tier_compile_job
