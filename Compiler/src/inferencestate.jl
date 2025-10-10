# This file is a part of Julia. License is MIT: https://julialang.org/license

# data structures
# ===============

mutable struct BitSetBoundedMinPrioritySet <: AbstractSet{Int}
    elems::BitSet
    min::Int
    # Stores whether min is exact or a lower bound
    # If exact, it is not set in elems
    min_exact::Bool
    max::Int
end

function BitSetBoundedMinPrioritySet(max::Int)
    bs = BitSet()
    bs.offset = 0
    BitSetBoundedMinPrioritySet(bs, max+1, true, max)
end

@noinline function _advance_bsbmp!(bsbmp::BitSetBoundedMinPrioritySet)
    @assert !bsbmp.min_exact
    bsbmp.min = _bits_findnext(bsbmp.elems.bits, bsbmp.min)::Int
    bsbmp.min < 0 && (bsbmp.min = bsbmp.max + 1)
    bsbmp.min_exact = true
    delete!(bsbmp.elems, bsbmp.min)
    return nothing
end

function isempty(bsbmp::BitSetBoundedMinPrioritySet)
    if bsbmp.min > bsbmp.max
        return true
    end
    bsbmp.min_exact && return false
    _advance_bsbmp!(bsbmp)
    return bsbmp.min > bsbmp.max
end

function popfirst!(bsbmp::BitSetBoundedMinPrioritySet)
    bsbmp.min_exact || _advance_bsbmp!(bsbmp)
    m = bsbmp.min
    m > bsbmp.max && throw(ArgumentError("BitSetBoundedMinPrioritySet must be non-empty"))
    bsbmp.min = m+1
    bsbmp.min_exact = false
    return m
end

function push!(bsbmp::BitSetBoundedMinPrioritySet, idx::Int)
    if idx <= bsbmp.min
        if bsbmp.min_exact && bsbmp.min < bsbmp.max && idx != bsbmp.min
            push!(bsbmp.elems, bsbmp.min)
        end
        bsbmp.min = idx
        bsbmp.min_exact = true
        return nothing
    end
    push!(bsbmp.elems, idx)
    return nothing
end

function in(idx::Int, bsbmp::BitSetBoundedMinPrioritySet)
    if bsbmp.min_exact && idx == bsbmp.min
        return true
    end
    return idx in bsbmp.elems
end

iterate(bsbmp::BitSetBoundedMinPrioritySet, s...) = iterate(bsbmp.elems, s...)

function append!(bsbmp::BitSetBoundedMinPrioritySet, itr)
    for val in itr
        push!(bsbmp, val)
    end
end

mutable struct TwoPhaseVectorView <: AbstractVector{Int}
    const data::Vector{Int}
    count::Int
    const range::UnitRange{Int}
end
size(tpvv::TwoPhaseVectorView) = (tpvv.count,)
function getindex(tpvv::TwoPhaseVectorView, i::Int)
    checkbounds(tpvv, i)
    @inbounds tpvv.data[first(tpvv.range) + i - 1]
end
function push!(tpvv::TwoPhaseVectorView, v::Int)
    tpvv.count += 1
    tpvv.data[first(tpvv.range) + tpvv.count - 1] = v
    return nothing
end

"""
    mutable struct TwoPhaseDefUseMap

This struct is intended as a memory- and GC-pressure-efficient mechanism
for incrementally computing def-use maps. The idea is that the def-use map
is constructed into two passes over the IR. In the first, we simply count the
the number of uses, computing the number of uses for each def as well as the
total number of uses. In the second pass, we actually fill in the def-use
information.

The idea is that either of these two phases can be combined with other useful
work that needs to scan the instruction stream anyway, while avoiding the
significant allocation pressure of e.g. allocating an array for every SSA value
or attempting to dynamically move things around as new uses are discovered.

The def-use map is presented as a vector of vectors. For every def, indexing
into the map will return a vector of uses.
"""
mutable struct TwoPhaseDefUseMap <: AbstractVector{TwoPhaseVectorView}
    ssa_uses::Vector{Int}
    data::Vector{Int}
    complete::Bool
end

function complete!(tpdum::TwoPhaseDefUseMap)
    cumsum = 0
    for i = 1:length(tpdum.ssa_uses)
        this_val = cumsum + 1
        cumsum += tpdum.ssa_uses[i]
        tpdum.ssa_uses[i] = this_val
    end
    resize!(tpdum.data, cumsum)
    fill!(tpdum.data, 0)
    tpdum.complete = true
end

function TwoPhaseDefUseMap(nssas::Int)
    ssa_uses = zeros(Int, nssas)
    data = Int[]
    complete = false
    return TwoPhaseDefUseMap(ssa_uses, data, complete)
end

function count!(tpdum::TwoPhaseDefUseMap, arg::SSAValue)
    @assert !tpdum.complete
    tpdum.ssa_uses[arg.id] += 1
end

function kill_def_use!(tpdum::TwoPhaseDefUseMap, def::Int, use::Int)
    if !tpdum.complete
        tpdum.ssa_uses[def] -= 1
    else
        range = tpdum.ssa_uses[def]:(def == length(tpdum.ssa_uses) ? length(tpdum.data) : (tpdum.ssa_uses[def + 1] - 1))
        # TODO: Sorted
        useidx = findfirst(idx->tpdum.data[idx] == use, range)
        @assert useidx !== nothing
        idx = range[useidx]
        while idx < lastindex(range)
            ndata = tpdum.data[idx+1]
            ndata == 0 && break
            tpdum.data[idx] = ndata
            idx += 1
        end
        tpdum.data[idx] = 0
    end
end
kill_def_use!(tpdum::TwoPhaseDefUseMap, def::SSAValue, use::Int) =
    kill_def_use!(tpdum, def.id, use)

function getindex(tpdum::TwoPhaseDefUseMap, idx::Int)
    @assert tpdum.complete
    range = tpdum.ssa_uses[idx]:(idx == length(tpdum.ssa_uses) ? length(tpdum.data) : (tpdum.ssa_uses[idx + 1] - 1))
    # TODO: Make logarithmic
    nelems = 0
    for i in range
        tpdum.data[i] == 0 && break
        nelems += 1
    end
    return TwoPhaseVectorView(tpdum.data, nelems, range)
end

mutable struct LazyCFGReachability
    ir::IRCode
    reachability::CFGReachability
    LazyCFGReachability(ir::IRCode) = new(ir)
end
function get!(x::LazyCFGReachability)
    isdefined(x, :reachability) && return x.reachability
    domtree = construct_domtree(x.ir)
    return x.reachability = CFGReachability(x.ir.cfg, domtree)
end

mutable struct LazyGenericDomtree{IsPostDom}
    ir::IRCode
    domtree::GenericDomTree{IsPostDom}
    LazyGenericDomtree{IsPostDom}(ir::IRCode) where {IsPostDom} = new{IsPostDom}(ir)
end
function get!(x::LazyGenericDomtree{IsPostDom}) where {IsPostDom}
    isdefined(x, :domtree) && return x.domtree
    return @zone "CC: DOMTREE_2" x.domtree = IsPostDom ?
        construct_postdomtree(x.ir) :
        construct_domtree(x.ir)
end

const LazyDomtree = LazyGenericDomtree{false}
const LazyPostDomtree = LazyGenericDomtree{true}

# InferenceState
# ==============

"""
    const VarTable = Vector{VarState}

The extended lattice that maps local variables to inferred type represented as `AbstractLattice`.
Each index corresponds to the `id` of `SlotNumber` which identifies each local variable.
Note that `InferenceState` will maintain multiple `VarTable`s at each SSA statement
to enable flow-sensitive analysis.
"""
const VarTable = Vector{VarState}

struct StatementState
    vtypes::Union{VarTable,Nothing}
    saw_latestworld::Bool
end

const CACHE_MODE_NULL     = 0x00      # not cached, optimization optional
const CACHE_MODE_GLOBAL   = 0x01 << 0 # cached globally, optimization required
const CACHE_MODE_LOCAL    = 0x01 << 1 # cached locally, optimization required

abstract type Handler end
get_enter_idx(handler::Handler) = get_enter_idx_impl(handler)::Int

mutable struct TryCatchFrame <: Handler
    exct
    scopet
    const enter_idx::Int
    scope_uses::Vector{Int}
    TryCatchFrame(@nospecialize(exct), @nospecialize(scopet), enter_idx::Int) =
        new(exct, scopet, enter_idx)
end
TryCatchFrame(stmt::EnterNode, pc::Int) =
    TryCatchFrame(Bottom, isdefined(stmt, :scope) ? Bottom : nothing, pc)
get_enter_idx_impl((; enter_idx)::TryCatchFrame) = enter_idx

struct SimpleHandler <: Handler
    enter_idx::Int
end
SimpleHandler(::EnterNode, pc::Int) = SimpleHandler(pc)
get_enter_idx_impl((; enter_idx)::SimpleHandler) = enter_idx

struct HandlerInfo{T<:Handler}
    handlers::Vector{T}
    handler_at::Vector{Tuple{Int,Int}} # tuple of current (handler, exception stack) value at the pc
end

struct WorldWithRange
    this::UInt
    valid_worlds::WorldRange
    function WorldWithRange(world::UInt, valid_worlds::WorldRange)
        if !(world in valid_worlds)
            error("invalid age range update")
        end
        return new(world, valid_worlds)
    end
end

intersect(world::WorldWithRange, valid_worlds::WorldRange) =
    WorldWithRange(world.this, intersect(world.valid_worlds, valid_worlds))

mutable struct InferenceState
    #= information about this method instance =#
    linfo::MethodInstance
    valid_worlds::WorldRange
    mod::Module
    sptypes::Vector{VarState}
    slottypes::Vector{Any}
    src::CodeInfo
    cfg::CFG
    spec_info::SpecInfo

    #= intermediate states for local abstract interpretation =#
    currbb::Int
    currpc::Int
    ip::BitSet#=TODO BoundedMinPrioritySet=# # current active instruction pointers
    handler_info::Union{Nothing,HandlerInfo{TryCatchFrame}}
    ssavalue_uses::Vector{BitSet} # ssavalue sparsity and restart info
    # TODO: Could keep this sparsely by doing structural liveness analysis ahead of time.
    bb_vartables::Vector{Union{Nothing,VarTable}} # nothing if not analyzed yet
    bb_saw_latestworld::Vector{Bool}
    ssavaluetypes::Vector{Any}
    ssaflags::Vector{UInt32}
    edges::Vector{Any}
    stmt_info::Vector{CallInfo}

    #= intermediate states for interprocedural abstract interpretation =#
    tasks::Vector{WorkThunk}
    pclimitations::IdSet{InferenceState} # causes of precision restrictions (LimitedAccuracy) on currpc ssavalue
    limitations::IdSet{InferenceState} # causes of precision restrictions (LimitedAccuracy) on return
    cycle_backedges::Vector{Tuple{InferenceState, Int}} # call-graph backedges connecting from callee to caller

    # IPO tracking of in-process work, shared with all frames given AbstractInterpreter
    callstack #::Vector{AbsIntState}
    parentid::Int # index into callstack of the parent frame that originally added this frame (call cycle_parent to extract the current parent of the SCC)
    frameid::Int # index into callstack at which this object is found (or zero, if this is not a cached frame and has no parent)
    cycleid::Int # index into the callstack of the topmost frame in the cycle (all frames in the same cycle share the same cycleid)

    #= results =#
    result::InferenceResult # remember where to put the result
    unreachable::BitSet # statements that were found to be statically unreachable
    bestguess #::Type
    exc_bestguess
    ipo_effects::Effects
    time_start::UInt64
    time_caches::Float64
    time_paused::UInt64
    time_self_ns::UInt64

    #= flags =#
    # Whether to restrict inference of abstract call sites to avoid excessive work
    # Set by default for toplevel frame.
    restrict_abstract_call_sites::Bool
    cache_mode::UInt8 # TODO move this to InferenceResult?
    insert_coverage::Bool

    # The interpreter that created this inference state. Not looked at by
    # NativeInterpreter. But other interpreters may use this to detect cycles
    interp::AbstractInterpreter

    # src is assumed to be a newly-allocated CodeInfo, that can be modified in-place to contain intermediate results
    function InferenceState(result::InferenceResult, src::CodeInfo, cache_mode::UInt8,
                            interp::AbstractInterpreter)
        mi = result.linfo
        world = get_inference_world(interp)
        if world == typemax(UInt)
            error("Entering inference from a generated function with an invalid world")
        end
        def = mi.def
        mod = isa(def, Method) ? def.module : def
        sptypes = sptypes_from_meth_instance(mi)
        code = src.code::Vector{Any}
        cfg = compute_basic_blocks(code)
        spec_info = SpecInfo(src)

        currbb = currpc = 1
        ip = BitSet(1) # TODO BitSetBoundedMinPrioritySet(1)
        handler_info = ComputeTryCatch{TryCatchFrame}()(code)
        nssavalues = src.ssavaluetypes::Int
        ssavalue_uses = find_ssavalue_uses(code, nssavalues)
        nstmts = length(code)
        edges = []
        stmt_info = CallInfo[ NoCallInfo() for _ = 1:nstmts ]

        nslots = length(src.slotflags)
        slottypes = Vector{Any}(undef, nslots)
        bb_saw_latestworld = Bool[false for _ = 1:length(cfg.blocks)]
        bb_vartables = Union{Nothing,VarTable}[ nothing for _ = 1:length(cfg.blocks) ]
        bb_vartable1 = bb_vartables[1] = VarTable(undef, nslots)
        argtypes = result.argtypes

        argtypes = va_process_argtypes(typeinf_lattice(interp), argtypes, src.nargs, src.isva)

        nargtypes = length(argtypes)
        for i = 1:nslots
            argtyp = (i > nargtypes) ? Bottom : argtypes[i]
            if argtyp === Bool && has_conditional(typeinf_lattice(interp))
                argtyp = Conditional(i, Const(true), Const(false))
            end
            slottypes[i] = argtyp
            bb_vartable1[i] = VarState(argtyp, i > nargtypes)
        end
        src.ssavaluetypes = ssavaluetypes = Any[ NOT_FOUND for _ = 1:nssavalues ]
        ssaflags = copy(src.ssaflags)

        unreachable = BitSet()
        pclimitations = IdSet{InferenceState}()
        limitations = IdSet{InferenceState}()
        cycle_backedges = Tuple{InferenceState,Int}[]
        callstack = AbsIntState[]
        tasks = WorkThunk[]

        valid_worlds = WorldRange(1, get_world_counter())
        bestguess = Bottom
        exc_bestguess = Bottom
        ipo_effects = EFFECTS_TOTAL

        insert_coverage = should_insert_coverage(mod, src.debuginfo)
        if insert_coverage
            ipo_effects = Effects(ipo_effects; effect_free = ALWAYS_FALSE)
        end

        if def isa Method
            nonoverlayed = is_nonoverlayed(def) ? ALWAYS_TRUE :
                is_effect_overridden(def, :consistent_overlay) ? CONSISTENT_OVERLAY :
                ALWAYS_FALSE
            ipo_effects = Effects(ipo_effects; nonoverlayed)
        end

        restrict_abstract_call_sites = isa(def, Module)

        parentid = frameid = cycleid = 0

        this = new(
            mi, valid_worlds, mod, sptypes, slottypes, src, cfg, spec_info,
            currbb, currpc, ip, handler_info, ssavalue_uses, bb_vartables, bb_saw_latestworld, ssavaluetypes, ssaflags, edges, stmt_info,
            tasks, pclimitations, limitations, cycle_backedges, callstack, parentid, frameid, cycleid,
            result, unreachable, bestguess, exc_bestguess, ipo_effects,
            _time_ns(), 0.0, 0, 0,
            restrict_abstract_call_sites, cache_mode, insert_coverage,
            interp)

        # some more setups
        if !iszero(cache_mode & CACHE_MODE_LOCAL)
            push!(get_inference_cache(interp), result)
        end
        if !iszero(cache_mode & CACHE_MODE_GLOBAL)
            push!(callstack, this)
            this.cycleid = this.frameid = length(callstack)
        end

        # Apply generated function restrictions
        if src.min_world != 1 || src.max_world != typemax(UInt)
            # From generated functions
            update_valid_age!(this, WorldRange(src.min_world, src.max_world))
        end

        return this
    end
end

gethandler(frame::InferenceState, pc::Int=frame.currpc) = gethandler(frame.handler_info, pc)
gethandler(::Nothing, ::Int) = nothing
function gethandler(handler_info::HandlerInfo, pc::Int)
    handler_idx = handler_info.handler_at[pc][1]
    handler_idx == 0 && return nothing
    return handler_info.handlers[handler_idx]
end

is_nonoverlayed(m::Method) = !isdefined(m, :external_mt)
is_nonoverlayed(interp::AbstractInterpreter) = !isoverlayed(method_table(interp))
isoverlayed(::MethodTableView) = error("unsatisfied MethodTableView interface")
isoverlayed(::InternalMethodTable) = false
isoverlayed(::OverlayMethodTable) = true
isoverlayed(mt::CachedMethodTable) = isoverlayed(mt.table)

is_inferred(sv::InferenceState) = is_inferred(sv.result)
is_inferred(result::InferenceResult) = result.result !== nothing

was_reached(sv::InferenceState, pc::Int) = sv.ssavaluetypes[pc] !== NOT_FOUND

struct ComputeTryCatch{T<:Handler} end

const compute_trycatch = ComputeTryCatch{SimpleHandler}()

(compute_trycatch::ComputeTryCatch{SimpleHandler})(ir::IRCode) =
    compute_trycatch(ir.stmts.stmt, ir.cfg.blocks)

"""
    (::ComputeTryCatch{Handler})(code, [, bbs]) -> handler_info::Union{Nothing,HandlerInfo{Handler}}
    const compute_trycatch = ComputeTryCatch{SimpleHandler}()

Given the code of a function, compute, at every statement, the current
try/catch handler, and the current exception stack top. This function returns
a tuple of:

    1. `handler_info.handler_at`: A statement length vector of tuples
       `(catch_handler, exception_stack)`, which are indices into `handlers`

    2. `handler_info.handlers`: A `Handler` vector of handlers
"""
function (::ComputeTryCatch{Handler})(code::Vector{Any}, bbs::Union{Vector{BasicBlock},Nothing}=nothing) where Handler
    # The goal initially is to record the frame like this for the state at exit:
    # 1: (enter 3) # == 0
    # 3: (expr)    # == 1
    # 3: (leave %1) # == 1
    # 4: (expr)    # == 0
    # then we can find all `try`s by walking backwards from :enter statements,
    # and all `catch`es by looking at the statement after the :enter
    n = length(code)
    ip = BitSet()
    ip.offset = 0 # for _bits_findnext
    push!(ip, n + 1)
    handler_info = nothing

    # start from all :enter statements and record the location of the try
    for pc = 1:n
        stmt = code[pc]
        if isa(stmt, EnterNode)
            (;handlers, handler_at) = handler_info =
                (handler_info === nothing ? HandlerInfo{Handler}(Handler[], fill((0, 0), n)) : handler_info)
            l = stmt.catch_dest
            (bbs !== nothing) && (l != 0) && (l = first(bbs[l].stmts))
            push!(handlers, Handler(stmt, pc))
            handler_id = length(handlers)
            handler_at[pc + 1] = (handler_id, 0)
            push!(ip, pc + 1)
            if l != 0
                handler_at[l] = (0, handler_id)
                push!(ip, l)
            end
        end
    end

    if handler_info === nothing
        return nothing
    end

    # now forward those marks to all :leave statements
    (;handlers, handler_at) = handler_info
    while true
        # make progress on the active ip set
        pc = _bits_findnext(ip.bits, 0)::Int
        pc > n && break
        while true # inner loop optimizes the common case where it can run straight from pc to pc + 1
            pc´ = pc + 1 # next program-counter (after executing instruction)
            delete!(ip, pc)
            cur_stacks = handler_at[pc]
            @assert cur_stacks != (0, 0) "unbalanced try/catch"
            stmt = code[pc]
            if isa(stmt, GotoNode)
                pc´ = stmt.label
                (bbs !== nothing) && (pc´ = first(bbs[pc´].stmts))
            elseif isa(stmt, GotoIfNot)
                l = stmt.dest::Int
                (bbs !== nothing) && (l = first(bbs[l].stmts))
                if handler_at[l] != cur_stacks
                    @assert handler_at[l][1] == 0 || handler_at[l][1] == cur_stacks[1] "unbalanced try/catch"
                    handler_at[l] = cur_stacks
                    push!(ip, l)
                end
            elseif isa(stmt, ReturnNode)
                @assert !isdefined(stmt, :val) || cur_stacks[1] == 0 "unbalanced try/catch"
                break
            elseif isa(stmt, EnterNode)
                l = stmt.catch_dest
                (bbs !== nothing) && (l != 0) && (l = first(bbs[l].stmts))
                # We assigned a handler number above. Here we just merge that
                # with out current handler information.
                if l != 0
                    handler_at[l] = (cur_stacks[1], handler_at[l][2])
                end
                cur_stacks = (handler_at[pc´][1], cur_stacks[2])
            elseif isa(stmt, Expr)
                head = stmt.head
                if head === :leave
                    l = 0
                    for j = 1:length(stmt.args)
                        arg = stmt.args[j]
                        if arg === nothing
                            continue
                        else
                            enter_stmt = code[(arg::SSAValue).id]
                            if enter_stmt === nothing
                                continue
                            end
                            @assert isa(enter_stmt, EnterNode) "malformed :leave"
                        end
                        l += 1
                    end
                    cur_hand = cur_stacks[1]
                    for _ = 1:l
                        cur_hand = handler_at[get_enter_idx(handlers[cur_hand])][1]
                    end
                    cur_stacks = (cur_hand, cur_stacks[2])
                    cur_stacks == (0, 0) && break
                elseif head === :pop_exception
                    cur_stacks = (cur_stacks[1], handler_at[(stmt.args[1]::SSAValue).id][2])
                    cur_stacks == (0, 0) && break
                end
            end

            pc´ > n && break # can't proceed with the fast-path fall-through
            if handler_at[pc´] != cur_stacks
                handler_at[pc´] = cur_stacks
            elseif !in(pc´, ip)
                break  # already visited
            end
            pc = pc´
        end
    end

    @assert first(ip) == n + 1
    return handler_info
end

# check if coverage mode is enabled
should_insert_coverage(mod::Module, debuginfo::DebugInfo) = should_instrument(mod, debuginfo, true)

function should_instrument(mod::Module, debuginfo::DebugInfo, only_if_affects_optimizer::Bool=false)
    instrumentation_enabled(mod, only_if_affects_optimizer) && return true
    JLOptions().code_coverage == 3 || JLOptions().malloc_log == 3 || return false
    # path-specific coverage mode: if any line falls in a tracked file enable coverage for all
    return _should_instrument(debuginfo)
end

_should_instrument(loc::Symbol) = is_file_tracked(loc)
_should_instrument(loc::Method) = _should_instrument(loc.file)
_should_instrument(loc::MethodInstance) = _should_instrument(loc.def)
_should_instrument(::Module) = false
_should_instrument(::Nothing) = false
function _should_instrument(info::DebugInfo)
    linetable = info.linetable
    linetable === nothing || (_should_instrument(linetable) && return true)
    _should_instrument(info.def) && return true
    return false
end

function InferenceState(result::InferenceResult, cache_mode::UInt8, interp::AbstractInterpreter)
    # prepare an InferenceState object for inferring lambda
    world = get_inference_world(interp)
    mi = result.linfo
    src = retrieve_code_info(mi, world)
    src === nothing && return nothing
    maybe_validate_code(mi, src, "lowered")
    return InferenceState(result, src, cache_mode, interp)
end
InferenceState(result::InferenceResult, cache_mode::Symbol, interp::AbstractInterpreter) =
    InferenceState(result, convert_cache_mode(cache_mode), interp)
InferenceState(result::InferenceResult, src::CodeInfo, cache_mode::Symbol, interp::AbstractInterpreter) =
    InferenceState(result, src, convert_cache_mode(cache_mode), interp)

function convert_cache_mode(cache_mode::Symbol)
    if cache_mode === :global
        return CACHE_MODE_GLOBAL
    elseif cache_mode === :local
        return CACHE_MODE_LOCAL
    elseif cache_mode === :no
        return CACHE_MODE_NULL
    end
    error("unexpected `cache_mode` is given")
end

"""
    constrains_param(var::TypeVar, sig, covariant::Bool, type_constrains::Bool)

Check if `var` will be constrained to have a definite value
in any concrete leaftype subtype of `sig`.

It is used as a helper to determine whether type intersection is guaranteed to be able to
find a value for a particular type parameter.
A necessary condition for type intersection to not assign a parameter is that it only
appears in a `Union[All]` and during subtyping some other union component (that does not
constrain the type parameter) is selected.

The `type_constrains` flag determines whether Type{T} is considered to be constraining
`T`. This is not true in general, because of the existence of types with free type
parameters, however, some callers would like to ignore this corner case.
"""
function constrains_param(var::TypeVar, @nospecialize(typ), covariant::Bool, type_constrains::Bool=false)
    typ === var && return true
    while typ isa UnionAll
        covariant && constrains_param(var, typ.var.ub, covariant, type_constrains) && return true
        # typ.var.lb doesn't constrain var
        typ = typ.body
    end
    if typ isa Union
        # for unions, verify that both options would constrain var
        ba = constrains_param(var, typ.a, covariant, type_constrains)
        bb = constrains_param(var, typ.b, covariant, type_constrains)
        (ba && bb) && return true
    elseif typ isa DataType
        # return true if any param constrains var
        fc = length(typ.parameters)
        if fc > 0
            if typ.name === Tuple.name
                # vararg tuple needs special handling
                for i in 1:(fc - 1)
                    p = typ.parameters[i]
                    constrains_param(var, p, covariant, type_constrains) && return true
                end
                lastp = typ.parameters[fc]
                vararg = unwrap_unionall(lastp)
                if vararg isa Core.TypeofVararg && isdefined(vararg, :N)
                    constrains_param(var, vararg.N, covariant, type_constrains) && return true
                    # T = vararg.parameters[1] doesn't constrain var
                else
                    constrains_param(var, lastp, covariant, type_constrains) && return true
                end
            else
                if typ.name === typename(Type) && typ.parameters[1] === var && var.ub === Any
                    # Types with free type parameters are <: Type cause the typevar
                    # to be unconstrained because Type{T} with free typevars is illegal
                    return type_constrains
                end
                for i in 1:fc
                    p = typ.parameters[i]
                    constrains_param(var, p, false, type_constrains) && return true
                end
            end
        end
    end
    return false
end

const EMPTY_SPTYPES = VarState[]

function sptypes_from_meth_instance(mi::MethodInstance)
    def = mi.def
    isa(def, Method) || return EMPTY_SPTYPES # toplevel
    sig = def.sig
    if isempty(mi.sparam_vals)
        isa(sig, UnionAll) || return EMPTY_SPTYPES
        # mi is unspecialized
        spvals = Any[]
        sig′ = sig
        while isa(sig′, UnionAll)
            push!(spvals, sig′.var)
            sig′ = sig′.body
        end
    else
        spvals = mi.sparam_vals
    end
    nvals = length(spvals)
    sptypes = Vector{VarState}(undef, nvals)
    for i = 1:nvals
        v = spvals[i]
        if v isa TypeVar
            temp = sig
            for j = 1:i-1
                temp = temp.body
            end
            vᵢ = (temp::UnionAll).var
            sigtypes = (unwrap_unionall(temp)::DataType).parameters
            for j = 1:length(sigtypes)
                sⱼ = sigtypes[j]
                if isType(sⱼ) && sⱼ.parameters[1] === vᵢ
                    # if this parameter came from `arg::Type{T}`,
                    # then `arg` is more precise than `Type{T} where lb<:T<:ub`
                    ty = fieldtype(mi.specTypes, j)
                    @goto ty_computed
                elseif (va = va_from_vatuple(sⱼ)) !== nothing
                    # if this parameter came from `::Tuple{.., Vararg{T,vᵢ}}`,
                    # then `vᵢ` is known to be `Int`
                    if isdefined(va, :N) && va.N === vᵢ
                        ty = Int
                        @goto ty_computed
                    end
                end
            end
            ub = unwraptv_ub(v)
            if has_free_typevars(ub)
                ub = Any
            end
            lb = unwraptv_lb(v)
            if has_free_typevars(lb)
                lb = Bottom
            end
            if Any === ub && lb === Bottom
                ty = Any
            else
                tv = TypeVar(v.name, lb, ub)
                ty = UnionAll(tv, Type{tv})
            end
            @label ty_computed
            undef = !(let sig=sig
                # if the specialized signature `linfo.specTypes` doesn't contain any free
                # type variables, we can use it for a more accurate analysis of whether `v`
                # is constrained or not, otherwise we should use `def.sig` which always
                # doesn't contain any free type variables
                if !has_free_typevars(mi.specTypes)
                    sig = mi.specTypes
                end
                @assert !has_free_typevars(sig)
                constrains_param(v, sig, #=covariant=#true)
            end)
        elseif isvarargtype(v)
            # if this parameter came from `func(..., ::Vararg{T,v})`,
            # so the type is known to be `Int`
            ty = Int
            undef = false
        else
            ty = Const(v)
            undef = false
        end
        sptypes[i] = VarState(ty, undef)
    end
    return sptypes
end

function va_from_vatuple(@nospecialize(t))
    @_foldable_meta
    t = unwrap_unionall(t)
    if isa(t, DataType)
        n = length(t.parameters)
        if n > 0
            va = t.parameters[n]
            if isvarargtype(va)
               return va
            end
        end
    end
    return nothing
end

_topmod(sv::InferenceState) = _topmod(frame_module(sv))

function record_ssa_assign!(𝕃ᵢ::AbstractLattice, ssa_id::Int, @nospecialize(new), frame::InferenceState)
    ssavaluetypes = frame.ssavaluetypes
    old = ssavaluetypes[ssa_id]
    if old === NOT_FOUND || !is_lattice_equal(𝕃ᵢ, new, old)
        ssavaluetypes[ssa_id] = new
        W = frame.ip
        for r in frame.ssavalue_uses[ssa_id]
            if was_reached(frame, r)
                usebb = block_for_inst(frame.cfg, r)
                if usebb != frame.currbb || r < ssa_id
                    push!(W, usebb)
                end
            end
        end
    end
    return nothing
end

function narguments(sv::InferenceState, include_va::Bool=true)
    nargs = Int(sv.src.nargs)
    if !include_va
        nargs -= sv.src.isva
    end
    return nargs
end

# IRInterpretationState
# =====================

# TODO add `result::InferenceResult` and put the irinterp result into the inference cache?
mutable struct IRInterpretationState
    const spec_info::SpecInfo
    const ir::IRCode
    const mi::MethodInstance
    valid_worlds::WorldRange
    curridx::Int
    time_caches::Float64
    time_paused::UInt64
    const argtypes_refined::Vector{Bool}
    const sptypes::Vector{VarState}
    const tpdum::TwoPhaseDefUseMap
    const ssa_refined::BitSet
    const lazyreachability::LazyCFGReachability
    const tasks::Vector{WorkThunk}
    const edges::Vector{Any}
    callstack #::Vector{AbsIntState}
    frameid::Int
    parentid::Int

    function IRInterpretationState(
            interp::AbstractInterpreter, spec_info::SpecInfo, ir::IRCode,
            mi::MethodInstance, argtypes::Vector{Any}, min_world::UInt, max_world::UInt
        )
        curridx = 1
        given_argtypes = Vector{Any}(undef, length(argtypes))
        for i = 1:length(given_argtypes)
            given_argtypes[i] = widenslotwrapper(argtypes[i])
        end
        if isa(mi.def, Method)
            argtypes_refined = Bool[!⊑(optimizer_lattice(interp), ir.argtypes[i], given_argtypes[i])
                for i = 1:length(given_argtypes)]
        else
            argtypes_refined = Bool[false for i = 1:length(given_argtypes)]
        end
        empty!(ir.argtypes)
        append!(ir.argtypes, given_argtypes)
        tpdum = TwoPhaseDefUseMap(length(ir.stmts))
        ssa_refined = BitSet()
        lazyreachability = LazyCFGReachability(ir)
        valid_worlds = WorldRange(min_world, max_world == typemax(UInt) ? get_world_counter() : max_world)
        tasks = WorkThunk[]
        edges = Any[]
        callstack = AbsIntState[]
        return new(spec_info, ir, mi, valid_worlds,
                curridx, 0.0, 0, argtypes_refined, ir.sptypes, tpdum,
                ssa_refined, lazyreachability, tasks, edges, callstack, 0, 0)
    end
end

function IRInterpretationState(
        interp::AbstractInterpreter, codeinst::CodeInstance, mi::MethodInstance,
        argtypes::Vector{Any}
    )
    @assert get_ci_mi(codeinst) === mi "method instance is not synced with code instance"
    src = ci_get_source(interp, codeinst)
    if isa(src, String)
        src = _uncompressed_ir(codeinst, src)
    else
        isa(src, CodeInfo) || return nothing
    end
    spec_info = SpecInfo(src)
    ir = inflate_ir(src, mi)
    argtypes = va_process_argtypes(optimizer_lattice(interp), argtypes, src.nargs, src.isva)
    return IRInterpretationState(interp, spec_info, ir, mi, argtypes,
                                 codeinst.min_world, codeinst.max_world)
end

# AbsIntState
# ===========

const AbsIntState = Union{InferenceState,IRInterpretationState}

function print_callstack(frame::AbsIntState)
    print("=================== Callstack: ==================\n")
    frames = frame.callstack::Vector{AbsIntState}
    for idx = (frame.frameid == 0 ? 0 : 1):length(frames)
        sv = (idx == 0 ? frame : frames[idx])
        idx == frame.frameid && print("*")
        print("[")
        print(idx)
        if sv isa InferenceState && !isa(sv.interp, NativeInterpreter)
            print(", ")
            print(typeof(sv.interp))
        end
        print("] ")
        print(frame_instance(sv))
        is_cached(sv) || print("  [not globally cached]")
        sv.parentid == idx - 1 || print(" [parent=", sv.parentid, "]")
        isempty(callers_in_cycle(sv)) || print(" [cycle=", sv.cycleid, "]")
        println()
        @assert sv.frameid == idx
    end
    print("================= End callstack ==================\n")
end

frame_instance(sv::InferenceState) = sv.linfo
frame_instance(sv::IRInterpretationState) = sv.mi

function frame_module(sv::AbsIntState)
    mi = frame_instance(sv)
    def = mi.def
    isa(def, Module) && return def
    return def.module
end

frame_parent(sv::AbsIntState) = sv.parentid == 0 ? nothing : (sv.callstack::Vector{AbsIntState})[sv.parentid]

function cycle_parent(sv::InferenceState)
    sv.parentid == 0 && return nothing
    callstack = sv.callstack::Vector{AbsIntState}
    sv = callstack[sv.cycleid]::InferenceState
    sv.parentid == 0 && return nothing
    return callstack[sv.parentid]
end
cycle_parent(sv::IRInterpretationState) = frame_parent(sv)


# add the orphan child to the parent and the parent to the child
function assign_parentchild!(child::InferenceState, parent::AbsIntState)
    @assert child.frameid in (0, 1)
    child.callstack = callstack = parent.callstack::Vector{AbsIntState}
    child.parentid = parent.frameid
    push!(callstack, child)
    child.cycleid = child.frameid = length(callstack)
    nothing
end
function assign_parentchild!(child::IRInterpretationState, parent::AbsIntState)
    @assert child.frameid in (0, 1)
    child.callstack = callstack = parent.callstack::Vector{AbsIntState}
    child.parentid = parent.frameid
    push!(callstack, child)
    child.frameid = length(callstack)
    nothing
end

function is_constproped(sv::InferenceState)
    (;overridden_by_const) = sv.result
    return overridden_by_const !== nothing
end
is_constproped(::IRInterpretationState) = true

is_cached(sv::InferenceState) = !iszero(sv.cache_mode & CACHE_MODE_GLOBAL)
is_cached(::IRInterpretationState) = false

spec_info(sv::InferenceState) = sv.spec_info
spec_info(sv::IRInterpretationState) = sv.spec_info

propagate_inbounds(sv::AbsIntState) = spec_info(sv).propagate_inbounds
method_for_inference_limit_heuristics(sv::AbsIntState) = spec_info(sv).method_for_inference_limit_heuristics

function is_effect_overridden(sv::AbsIntState, effect::Symbol)
    if is_effect_overridden(frame_instance(sv), effect)
        return true
    elseif is_effect_overridden(decode_statement_effects_override(sv), effect)
        return true
    end
    return false
end
function is_effect_overridden(mi::MethodInstance, effect::Symbol)
    def = mi.def
    return isa(def, Method) && is_effect_overridden(def, effect)
end
is_effect_overridden(method::Method, effect::Symbol) = is_effect_overridden(decode_effects_override(method.purity), effect)
is_effect_overridden(override::EffectsOverride, effect::Symbol) = getfield(override, effect)

has_conditional(𝕃::AbstractLattice, ::InferenceState) = has_conditional(𝕃)
has_conditional(::AbstractLattice, ::IRInterpretationState) = false

# work towards converging the valid age range for sv
function update_valid_age!(sv::AbsIntState, valid_worlds::WorldRange)
    sv.valid_worlds = intersect(sv.valid_worlds, valid_worlds)
    return sv.valid_worlds
end

"""
    AbsIntStackUnwind(sv::AbsIntState)

Iterate through all callers of the given `AbsIntState` in the abstract interpretation stack
(including the given `AbsIntState` itself), visiting children before their parents (i.e.
ascending the tree from the given `AbsIntState`).
Note that cycles may be visited in any order.
"""
struct AbsIntStackUnwind
    callstack::Vector{AbsIntState}
    AbsIntStackUnwind(sv::AbsIntState) = new(sv.callstack::Vector{AbsIntState})
end
function iterate(unw::AbsIntStackUnwind, frame::Int=length(unw.callstack))
    frame == 0 && return nothing
    return (unw.callstack[frame], frame - 1)
end

struct AbsIntCycle
    frames::Vector{AbsIntState}
    cycleid::Int
    cycletop::Int
end
iterate(unw::AbsIntCycle) = unw.cycleid == 0 ? nothing : (unw.frames[unw.cycletop], unw.cycletop)
function iterate(unw::AbsIntCycle, frame::Int)
    frame == unw.cycleid && return nothing
    return (unw.frames[frame - 1], frame - 1)
end

"""
    callers_in_cycle(sv::AbsIntState)

Iterate through all callers of the given `AbsIntState` in the abstract
interpretation stack (including the given `AbsIntState` itself) that are part
of the same cycle, only if it is part of a cycle with multiple frames.
"""
function callers_in_cycle(sv::InferenceState)
    callstack = sv.callstack::Vector{AbsIntState}
    cycletop = cycleid = sv.cycleid
    while cycletop < length(callstack)
        frame = callstack[cycletop + 1]
        frame isa InferenceState || break
        frame.cycleid == cycleid || break
        cycletop += 1
    end
    return AbsIntCycle(callstack, cycletop == cycleid ? 0 : cycleid, cycletop)
end
callers_in_cycle(sv::IRInterpretationState) = AbsIntCycle(sv.callstack::Vector{AbsIntState}, 0, 0)

get_curr_ssaflag(sv::InferenceState) = sv.ssaflags[sv.currpc]
get_curr_ssaflag(sv::IRInterpretationState) = sv.ir.stmts[sv.curridx][:flag]

has_curr_ssaflag(sv::InferenceState, flag::UInt32) = has_flag(sv.ssaflags[sv.currpc], flag)
has_curr_ssaflag(sv::IRInterpretationState, flag::UInt32) = has_flag(sv.ir.stmts[sv.curridx][:flag], flag)

function set_curr_ssaflag!(sv::InferenceState, flag::UInt32, mask::UInt32=typemax(UInt32))
    curr_flag = sv.ssaflags[sv.currpc]
    sv.ssaflags[sv.currpc] = (curr_flag & ~mask) | flag
    nothing
end

add_curr_ssaflag!(sv::InferenceState, flag::UInt32) = sv.ssaflags[sv.currpc] |= flag
add_curr_ssaflag!(sv::IRInterpretationState, flag::UInt32) = add_flag!(sv.ir.stmts[sv.curridx], flag)

sub_curr_ssaflag!(sv::InferenceState, flag::UInt32) = sv.ssaflags[sv.currpc] &= ~flag
sub_curr_ssaflag!(sv::IRInterpretationState, flag::UInt32) = sub_flag!(sv.ir.stmts[sv.curridx], flag)

function merge_effects!(::AbstractInterpreter, caller::InferenceState, effects::Effects)
    if effects.effect_free === EFFECT_FREE_GLOBALLY
        # This tracks the global effects
        effects = Effects(effects; effect_free=ALWAYS_TRUE)
    end
    caller.ipo_effects = merge_effects(caller.ipo_effects, effects)
    nothing
end
merge_effects!(::AbstractInterpreter, ::IRInterpretationState, ::Effects) = return

decode_statement_effects_override(sv::InferenceState) = decode_statement_effects_override(sv.src.ssaflags[sv.currpc])
decode_statement_effects_override(sv::IRInterpretationState) = decode_statement_effects_override(UInt32(0))

struct InferenceLoopState
    rt
    effects::Effects
    function InferenceLoopState(@nospecialize(rt), effects::Effects)
        new(rt, effects)
    end
end

bail_out_toplevel_call(::AbstractInterpreter, sv::InferenceState) = sv.restrict_abstract_call_sites
bail_out_toplevel_call(::AbstractInterpreter, ::IRInterpretationState) = false

bail_out_call(::AbstractInterpreter, state::InferenceLoopState, ::InferenceState) =
    state.rt === Any && !is_foldable(state.effects)
bail_out_call(::AbstractInterpreter, state::InferenceLoopState, ::IRInterpretationState) =
    state.rt === Any && !is_foldable(state.effects)

bail_out_apply(::AbstractInterpreter, state::InferenceLoopState, ::InferenceState) =
    state.rt === Any
bail_out_apply(::AbstractInterpreter, state::InferenceLoopState, ::IRInterpretationState) =
    state.rt === Any

add_remark!(::AbstractInterpreter, ::InferenceState, remark) = return
add_remark!(::AbstractInterpreter, ::IRInterpretationState, remark) = return

function get_max_methods(interp::AbstractInterpreter, @nospecialize(f), sv::AbsIntState)
    fmax = get_max_methods_for_func(f)
    fmax !== nothing && return fmax
    return get_max_methods(interp, sv)
end
function get_max_methods(interp::AbstractInterpreter, @nospecialize(f))
    fmax = get_max_methods_for_func(f)
    fmax !== nothing && return fmax
    return get_max_methods(interp)
end
function get_max_methods(interp::AbstractInterpreter, sv::AbsIntState)
    mmax = get_max_methods_for_module(sv)
    mmax !== nothing && return mmax
    return get_max_methods(interp)
end
get_max_methods(interp::AbstractInterpreter) = InferenceParams(interp).max_methods

function get_max_methods_for_func(@nospecialize(f))
    if f !== nothing
        fmm = typeof(f).name.max_methods
        fmm !== UInt8(0) && return Int(fmm)
    end
    return nothing
end
get_max_methods_for_module(sv::AbsIntState) = get_max_methods_for_module(frame_module(sv))
function get_max_methods_for_module(mod::Module)
    max_methods = ccall(:jl_get_module_max_methods, Cint, (Any,), mod) % Int
    max_methods < 0 && return nothing
    return max_methods
end

"""
    Future{T}

Assign-once delayed return value for a value of type `T`, similar to RefValue{T}.
Can be constructed in one of three ways:

1. With an immediate as `Future{T}(val)`
2. As an assign-once storage location with `Future{T}()`. Assigned (once) using `f[] = val`.
3. As a delayed computation with `Future{T}(callback, dep, interp, sv)` to have
   `sv` arrange to call the `callback` with the result of `dep` when it is ready.

Use `isready` to check if the value is ready, and `getindex` to get the value.
"""
struct Future{T}
    later::Union{Nothing,RefValue{T}}
    now::Union{Nothing,T}
    function Future{T}() where {T}
        later = RefValue{T}()
        @assert !isassigned(later) "Future{T}() is not allowed for inlinealloc T"
        new{T}(later, nothing)
    end
    Future{T}(x) where {T} = new{T}(nothing, x)
    Future(x::T) where {T} = new{T}(nothing, x)
end
isready(f::Future) = f.later === nothing || isassigned(f.later)
getindex(f::Future{T}) where {T} = (later = f.later; later === nothing ? f.now::T : later[])
function setindex!(f::Future, v)
    later = something(f.later)
    @assert !isassigned(later)
    later[] = v
    return f
end
convert(::Type{Future{T}}, x) where {T} = Future{T}(x) # support return type conversion
convert(::Type{Future{T}}, x::Future) where {T} = x::Future{T}
function Future{T}(f, immediate::Bool, interp::AbstractInterpreter, sv::AbsIntState) where {T}
    if immediate
        return Future{T}(f(interp, sv))
    else
        @assert applicable(f, interp, sv)
        result = Future{T}()
        push!(sv.tasks, function (interp, sv)
            result[] = f(interp, sv)
            return true
        end)
        return result
    end
end
function Future{T}(f, prev::Future{S}, interp::AbstractInterpreter, sv::AbsIntState) where {T, S}
    later = prev.later
    if later === nothing
        return Future{T}(f(prev[], interp, sv))
    else
        @assert Core._hasmethod(Tuple{Core.Typeof(f), S, typeof(interp), typeof(sv)})
        result = Future{T}()
        @assert !isa(sv, InferenceState) || interp === sv.interp
        push!(sv.tasks, function (interp, sv)
            result[] = f(later[], interp, sv) # capture just later, instead of all of prev
            return true
        end)
        return result
    end
end

"""
    doworkloop(args...)

Run a tasks inside the abstract interpreter, returning false if there are none.
Tasks will be run in DFS post-order tree order, such that all child tasks will
be run in the order scheduled, prior to running any subsequent tasks. This
allows tasks to generate more child tasks, which will be run before anything else.
Each task will be run repeatedly when returning `false`, until it returns `true`.
"""
function doworkloop(interp::AbstractInterpreter, sv::AbsIntState)
    tasks = sv.tasks
    prev = length(tasks)
    prevcallstack = length(sv.callstack)
    prev == 0 && return false
    task = pop!(tasks)
    completed = task(interp, sv)
    tasks = sv.tasks # allow dropping gc root over the previous call
    completed isa Bool || throw(TypeError(:return, "", Bool, task)) # print the task on failure as part of the error message, instead of just "@ workloop:line"
    if !completed
        @assert (length(tasks) >= prev || length(sv.callstack) > prevcallstack) "Task did not complete, but also did not create any child tasks"
        push!(tasks, task)
    end
    # efficient post-order visitor: items pushed are executed in reverse post order such
    # that later items are executed before earlier ones, but are fully executed
    # (including any dependencies scheduled by them) before going on to the next item
    reverse!(tasks, #=start=#prev)
    return true
end


#macro workthunk(name::Symbol, body)
#    name = esc(name)
#    body = esc(body)
#    return replace_linenums!(
#        :(function $name($(esc(interp)), $(esc(sv)))
#              $body
#          end), __source__)
#end
