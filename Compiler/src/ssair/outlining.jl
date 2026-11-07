# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    outline_cold_regions!(ir::IRCode, sv::OptimizationState) -> IRCode

Move each cold region of `ir` into a function of its own, and replace it with a call to
that function.

A basic block is *cold* if every path from it ends in a statement that never returns: a
`throw`, or a call that inference found never returns (such as `error`). Inference already
ends such paths with an unreachable `ReturnNode()`, so a block is cold if it ends in one,
or if all of its successors are cold. Statements in cold blocks only run on the way to an
error, e.g. building the message of `throw(ArgumentError("bad \$x"))`. In the function body
they are pure overhead: they take up code space and registers, and they count against the
function's inlining cost, which keeps small checked functions from being inlined.

A *region* is a connected set of cold blocks. It is outlined when it has a single entry
block, and only if the function is cheap enough to be inlined once its regions are
outlined: then the regions no longer count against its inlining cost (which may make it
inlinable in the first place), and every caller it is inlined into gets a call instead of
a copy of each region. A function that is too large to be inlined either way gains little,
since LLVM already places the blocks of its error paths after the rest of its code, and
each outlined region costs a function's worth of code and metadata.

The `PhiNode`s at the top of the entry block (and any exception-handler statements before
the region's other code) stay in the function, and everything after them moves into a new
function, created by `jl_new_outlined_codeinst`: a `CodeInstance` holding the region's
already-optimized code, as a specialization of a method that holds all the regions
outlined from the function's method and is not added to any method table. Values that the
region uses from the rest of the function are passed as arguments,
with the function's own `#self#` argument first, so that the outlined code reports itself
as part of the original function in backtraces. The region's statements keep their debug
info, so backtraces show the original lines. The region is replaced by an `:invoke` of the
new `CodeInstance` that returns `Union{}`, which the inlining cost model counts as free.

A throw inside a `try` still reaches the function's handler: the call stays inside the
`try`, and the exception propagates out of it as usual.

Regions are left in place when:
- compilation is off or minimal (`--compile=no`/`--compile=min`, globally or for the
  function's module): outlined code has no lowered source, so it can only be compiled, not
  interpreted;
- the function is not inlinable even without its regions, or is declared `@noinline`;
- the region has more than one entry block (TODO);
- a value passed into the region might be undefined;
- the region uses an exception-handler token or `UpsilonNode` value from outside it, or
  hands a value to a `catch` block outside it (an `UpsilonNode` for a `PhiCNode` there);
- the region may use the function's static parameters other than through
  `Expr(:static_parameter)` (whose value is substituted, or passed in if it is not known),
  e.g. in a `ccall`;
- the region is a single statement, e.g. just the `throw`, or a call to a `throw_*` helper,
  which outlining would only wrap in another call.
"""
function outline_cold_regions!(ir::IRCode, sv::OptimizationState)
    interp = sv.inlining.interp
    mi = sv.linfo
    isa(mi.def, Method) || return ir
    # Outlined code has no lowered source to interpret, so only outline where it may be
    # compiled.
    compile_allowed(mi.def.module) || return ir
    # The outlined code is created in the native code cache.
    cache_owner(interp) === nothing || return ir
    isempty(ir.new_nodes.stmts) || return ir
    blocks = ir.cfg.blocks
    nblocks = length(blocks)
    nblocks > 1 || return ir
    cold = find_cold_blocks(ir)
    cold === nothing && return ir
    regions = cold_regions(ir.cfg, cold)
    isempty(regions) && return ir
    inregion = fill(false, nblocks)
    entries = Int[region_entry(blocks, region, inregion) for region in regions]
    inlinable_without_regions(ir, sv, regions, entries) || return ir
    newidx = zeros(Int, length(ir.stmts))
    # Values a `try` body hands to its `catch` block: an `UpsilonNode` (by statement) and
    # the block of the `PhiCNode` that receives it.
    upsilons = Pair{Int,Int}[]
    for b = 1:nblocks, i in blocks[b].stmts
        stmt = ir[SSAValue(i)][:stmt]
        if isa(stmt, PhiCNode)
            for v in stmt.values
                isa(v, SSAValue) && push!(upsilons, v.id => b)
            end
        end
    end
    changed = false
    for (region, entry) in zip(regions, entries)
        entry == 0 && continue
        for b in region
            inregion[b] = true
        end
        changed |= try_outline_region!(ir, sv, region, entry, inregion, newidx, upsilons)
        for b in region
            inregion[b] = false
        end
    end
    changed || return ir
    return compact!(ir, true)
end

# The cold blocks of `ir`: those from which every path ends in an unreachable `ReturnNode()`.
# Returns `nothing` if there are none.
function find_cold_blocks(ir::IRCode)
    blocks = ir.cfg.blocks
    nblocks = length(blocks)
    cold = fill(false, nblocks)
    anycold = false
    for b = 1:nblocks
        term = ir[SSAValue(last(blocks[b].stmts))][:stmt]
        if isa(term, ReturnNode) && !isdefined(term, :val)
            cold[b] = anycold = true
        end
    end
    anycold || return nothing
    changed = true
    while changed
        changed = false
        for b = nblocks:-1:1
            cold[b] && continue
            succs = blocks[b].succs
            isempty(succs) && continue
            allcold = true
            for s in succs
                if !cold[s]
                    allcold = false
                    break
                end
            end
            if allcold
                cold[b] = changed = true
            end
        end
    end
    return cold
end

# The connected components of the cold blocks, each in layout order. The entry block is
# never part of a region.
function cold_regions(cfg::CFG, cold::Vector{Bool})
    blocks = cfg.blocks
    nblocks = length(blocks)
    region_of = zeros(Int, nblocks)
    nregions = 0
    work = Int[]
    for b = 2:nblocks
        (cold[b] && region_of[b] == 0) || continue
        nregions += 1
        region_of[b] = nregions
        push!(work, b)
        while !isempty(work)
            x = pop!(work)
            for y in blocks[x].succs
                if y > 1 && cold[y] && region_of[y] == 0
                    region_of[y] = nregions
                    push!(work, y)
                end
            end
            for y in blocks[x].preds
                if y > 1 && cold[y] && region_of[y] == 0
                    region_of[y] = nregions
                    push!(work, y)
                end
            end
        end
    end
    regions = Vector{Int}[Int[] for _ = 1:nregions]
    for b = 1:nblocks
        r = region_of[b]
        r == 0 || push!(regions[r], b)
    end
    return regions
end

# Statements at the start of a region's entry block that must stay in the function: they
# merge values from the function's other blocks, or act on its exception handlers.
function must_stay_in_parent(@nospecialize stmt)
    return isa(stmt, PhiNode) || isa(stmt, PhiCNode) || isa(stmt, UpsilonNode) ||
        isa(stmt, EnterNode) || isexpr(stmt, :leave) || isexpr(stmt, :pop_exception)
end

# Whether the value defined by `def` can be passed into an outlined region.
function can_pass_to_outlined(@nospecialize def)
    if isa(def, PhiNode) || isa(def, PhiCNode)
        # A value merged from a path on which it was never assigned may be undefined.
        values = def.values
        for i = 1:length(values)
            isassigned(values, i) || return false
        end
        return true
    end
    # Exception-handler tokens and the values stored for `catch` blocks only have meaning
    # in the function that defines them.
    return !(isa(def, EnterNode) || isa(def, UpsilonNode) || isexpr(def, :gc_preserve_begin))
end

function has_static_parameter(@nospecialize x)
    isa(x, Expr) || return false
    x.head === :static_parameter && return true
    for a in x.args
        has_static_parameter(a) && return true
    end
    return false
end

# Whether the region statement `stmt` only counts as bookkeeping when deciding whether the
# region is large enough to be worth outlining.
function is_outlining_bookkeeping(@nospecialize stmt)
    stmt === nothing && return true
    isa(stmt, Union{GotoNode, GotoIfNot, ReturnNode, PhiNode, PiNode}) && return true
    if isa(stmt, Expr)
        head = stmt.head
        return head === :meta || head === :code_coverage_effect || head === :boundscheck ||
            head === :gc_preserve_end || head === :static_parameter
    end
    return false
end

# Values from outside a region are recorded in `newidx` (for SSA values) and `argmap` (for
# arguments): a positive number is the value's statement in the outlined code (`newidx`) or
# its argument number there (`argmap`); a negative one is the negated argument number, or,
# below `-OUTLINED_CONST_BASE`, the index of a constant that is substituted for it.
const OUTLINED_CONST_BASE = 1 << 40

function self_argument_is_used(sv::OptimizationState)
    slotnames = sv.src.slotnames
    return !isempty(slotnames) && slotnames[1] !== Symbol("#unused#")
end

function remap_outlined_value(@nospecialize(val), newidx::Vector{Int}, argmap::Vector{Int},
                              consts::Vector{Any})
    if isa(val, SSAValue)
        k = newidx[val.id]
        k > 0 && return SSAValue(k)
        k <= -OUTLINED_CONST_BASE && return quoted(consts[-k - OUTLINED_CONST_BASE])
        return Argument(-k)
    elseif isa(val, Argument)
        k = argmap[val.n]
        k > 0 && return Argument(k)
        return quoted(consts[-k])
    end
    return val
end

# Copy `stmt` into the outlined code, renumbering values and blocks.
function remap_outlined_stmt(@nospecialize(stmt), newidx::Vector{Int}, argmap::Vector{Int},
                             consts::Vector{Any}, bbmap::Vector{Int})
    if isa(stmt, GotoNode)
        return GotoNode(bbmap[stmt.label])
    elseif isa(stmt, GotoIfNot)
        return GotoIfNot(remap_outlined_value(stmt.cond, newidx, argmap, consts), bbmap[stmt.dest])
    elseif isa(stmt, EnterNode)
        catch_dest = stmt.catch_dest == 0 ? 0 : bbmap[stmt.catch_dest]
        if isdefined(stmt, :scope)
            return EnterNode(catch_dest, remap_outlined_value(stmt.scope, newidx, argmap, consts))
        end
        return EnterNode(catch_dest)
    elseif isa(stmt, PhiNode)
        values = copy(stmt.values)
        for i = 1:length(values)
            if isassigned(values, i)
                values[i] = remap_outlined_value(values[i], newidx, argmap, consts)
            end
        end
        return PhiNode(Int32[bbmap[e] for e in stmt.edges], values)
    elseif isa(stmt, Expr)
        stmt = copy(stmt)
    elseif isa(stmt, PhiCNode)
        stmt = PhiCNode(copy(stmt.values))
    end
    urs = userefs(stmt)
    for op in urs
        op[] = remap_outlined_value(op[], newidx, argmap, consts)
    end
    return urs[]
end

# The `CodeInstance` for the code `src` with signature `sig`, outlined from `mi`'s method.
# All regions outlined from a method are specializations of one method that is in no method
# table (see `jl_new_outlined_codeinst`), and each specialization holds a single region. A
# region that has already been outlined is reused: the same method instance is often
# optimized many times (e.g. for each constant-propagated call site whose result gets
# inlined), and different specializations often contain the same region. Different regions
# whose arguments have the same types (e.g. two `throw(BoundsError(a, i))` on different
# lines) are told apart by an extra argument of type `Val{k}`, which holds no data. Returns
# the `CodeInstance` and that `k`, or `0` if there is no extra argument.
function outlined_codeinst!(mi::MethodInstance, src::CodeInfo, newargtypes::Vector{Any},
                            min_world::UInt)
    tagged = src
    k = 0
    while true
        sig = argtypes_to_type(k == 0 ? newargtypes : Any[newargtypes..., Val{k}])
        existing = ccall(:jl_outlined_codeinst, Any, (Any, Any), mi, sig)
        if existing === nothing
            ci = ccall(:jl_new_outlined_codeinst, Any,
                (Any, Any, Any, Any, UInt, UInt, UInt32),
                mi, tagged, sig, Union{}, min_world, typemax(UInt),
                encode_effects(EFFECTS_UNKNOWN))
            ci === nothing || return Pair{CodeInstance,Int}(ci::CodeInstance, k)
            # Another thread outlined a region with this signature in the meantime.
            continue
        end
        if same_outlined_codeinst(existing::CodeInstance, tagged, min_world)
            return Pair{CodeInstance,Int}(existing, k)
        end
        k += 1
        tagged = ccall(:jl_copy_code_info, Ref{CodeInfo}, (Any,), src)
        tagged.slotnames = Symbol[src.slotnames..., Symbol("")]
        tagged.slotflags = UInt8[src.slotflags..., 0x00]
        tagged.slottypes = Any[(src.slottypes::Vector{Any})..., Val{k}]
        tagged.nargs = src.nargs + 1
    end
end

function same_outlined_codeinst(c::CodeInstance, src::CodeInfo, min_world::UInt)
    cmi = get_ci_mi(c)
    # Code that has not been compiled yet calls `c` through its method instance, which is
    # looked up in the caller's world.
    c.min_world <= min_world || return false
    inferred = c.inferred
    if isa(inferred, String)
        inferred = ccall(:jl_uncompress_ir, Ref{CodeInfo}, (Any, Any, Any),
            cmi.def, c, inferred)
    end
    return isa(inferred, CodeInfo) && same_outlined_code(inferred, src)
end

function same_outlined_code(a::CodeInfo, b::CodeInfo)
    # The argument types are equal: they are the (equal) signatures' parameters.
    a.debuginfo === b.debuginfo || return false
    a.ssaflags == b.ssaflags || return false
    acode, bcode = a.code, b.code
    length(acode) == length(bcode) || return false
    atypes, btypes = a.ssavaluetypes::Vector{Any}, b.ssavaluetypes::Vector{Any}
    for i = 1:length(acode)
        atypes[i] === btypes[i] || return false
        same_outlined_stmt(acode[i], bcode[i]) || return false
    end
    return true
end

function same_outlined_stmt(@nospecialize(x), @nospecialize(y))
    if isa(x, Expr)
        isa(y, Expr) || return false
        x.head === y.head || return false
        length(x.args) == length(y.args) || return false
        for i = 1:length(x.args)
            same_outlined_stmt(x.args[i], y.args[i]) || return false
        end
        return true
    elseif isa(x, PhiNode)
        isa(y, PhiNode) || return false
        x.edges == y.edges || return false
        length(x.values) == length(y.values) || return false
        for i = 1:length(x.values)
            isassigned(x.values, i) == isassigned(y.values, i) || return false
            isassigned(x.values, i) && !same_outlined_stmt(x.values[i], y.values[i]) && return false
        end
        return true
    elseif isa(x, QuoteNode)
        return isa(y, QuoteNode) && x.value === y.value
    end
    return x === y
end

# The entry block of `region`: the only region block with a predecessor outside the region,
# or `0` if the region cannot be outlined because it has several (TODO) or none. The virtual
# predecessor `0` of a `catch` block does not count: its handler is either in the region
# too, or the block also has the handler's block as a predecessor. `inregion` must be all
# `false`, and is again on return.
function region_entry(blocks::Vector{BasicBlock}, region::Vector{Int}, inregion::Vector{Bool})
    for b in region
        inregion[b] = true
    end
    entry = 0
    for b in region
        for p in blocks[b].preds
            (p == 0 || inregion[p]) && continue
            if entry != 0 && entry != b
                entry = -1
                break
            end
            entry = b
        end
        entry == -1 && break
    end
    # The entry block dominates the region, so the layout puts it first. The outlined code
    # starts with it, so it cannot be the target of a back edge.
    if entry > 0 && entry == region[1]
        for p in blocks[entry].preds
            if p != 0 && inregion[p]
                entry = 0
                break
            end
        end
    else
        entry = 0
    end
    for b in region
        inregion[b] = false
    end
    return entry
end

# Whether code in `mod` may be compiled: not with `--compile=no` or `--compile=min`, nor in a
# module that sets either for itself (as `jl_compile_method_internal` decides).
function compile_allowed(mod::Module)
    setting = ccall(:jl_get_module_compile, Cint, (Any,), mod)
    (setting == JL_OPTIONS_COMPILE_OFF || setting == JL_OPTIONS_COMPILE_MIN) && return false
    setting = JLOptions().compile_enabled
    return !(setting == JL_OPTIONS_COMPILE_OFF || setting == JL_OPTIONS_COMPILE_MIN)
end

# Whether the function is cheap enough to be inlined into its callers once the cold regions
# with an entry in `entries` are outlined. Only then is outlining worth its cost: a function
# that is not inlined either way keeps its error paths out of the way of its own code by
# itself (LLVM places blocks that end in a throw after the rest), and outlining would only
# add a function. A function that is inlined (or now can be) has its error paths copied into
# every caller, and the copies shrink to a call.
function inlinable_without_regions(ir::IRCode, sv::OptimizationState,
                                   regions::Vector{Vector{Int}}, entries::Vector{Int})
    flag = ccall(:jl_ir_flag_inlining, UInt8, (Any,), sv.src)
    flag === SRC_FLAG_DECLARED_NOINLINE && return false
    declared_inline = flag === SRC_FLAG_DECLARED_INLINE
    mi = sv.linfo
    declared_inline && isdispatchtuple(mi.specTypes) && return true
    rt = sv.src.rettype
    declared_inline || rt !== Union{} || return false
    interp = sv.inlining.interp
    threshold = inline_cost_threshold(interp, mi.def::Method, rt, declared_inline)
    params = OptimizationParams(interp)
    blocks = ir.cfg.blocks
    outlined = fill(false, length(blocks))
    for (region, entry) in zip(regions, entries)
        entry == 0 && continue
        for b in region
            outlined[b] = true
        end
    end
    cost = 0
    for b = 1:length(blocks)
        outlined[b] && continue
        for i in blocks[b].stmts
            stmt = ir[SSAValue(i)][:stmt]
            cost = plus_saturate(cost, statement_or_branch_cost(stmt, i, ir, ir.sptypes, params))
            cost > threshold && return false
        end
    end
    return true
end

function try_outline_region!(ir::IRCode, sv::OptimizationState, region::Vector{Int},
                             entry::Int, inregion::Vector{Bool}, newidx::Vector{Int},
                             upsilons::Vector{Pair{Int,Int}})
    blocks = ir.cfg.blocks

    # The statements of the entry block that move: everything after the last statement that
    # has to stay.
    entry_stmts = blocks[entry].stmts
    split = first(entry_stmts)
    for i in entry_stmts
        must_stay_in_parent(ir[SSAValue(i)][:stmt]) && (split = i + 1)
    end
    split > last(entry_stmts) && return false

    ranges = UnitRange{Int}[split:last(entry_stmts)]
    for k = 2:length(region)
        push!(ranges, blocks[region[k]].stmts)
    end
    nnew = 0
    for r in ranges, i in r
        newidx[i] = (nnew += 1)
    end
    # A value the region hands to a `catch` block outside it must be stored in this
    # function's frame, so the region cannot move.
    ok = true
    for (u, b) in upsilons
        if newidx[u] > 0 && !inregion[b]
            ok = false
            break
        end
    end
    ok && (ok = outline_region_body!(ir, sv, region, entry, split, ranges, nnew, newidx))
    for r in ranges, i in r
        newidx[i] = 0
    end
    return ok
end

function outline_region_body!(ir::IRCode, sv::OptimizationState, region::Vector{Int},
                              entry::Int, split::Int, ranges::Vector{UnitRange{Int}},
                              nnew::Int, newidx::Vector{Int})
    mi = sv.linfo
    blocks = ir.cfg.blocks
    # The outlined code has no static parameters of its own: those the region uses are
    # substituted (if known) or passed in (see below). Code that may refer to them through
    # the method's environment instead cannot be outlined.
    has_sparams = !isempty(ir.sptypes)

    # Find the values that come from outside the region. They become arguments, numbered
    # after the function's own `#self#` argument. Arguments are recorded in `argmap`, values
    # in `newidx` as the negated argument number.
    argmap = zeros(Int, length(ir.argtypes))
    argmap[1] = 1
    # The parent's own `#self#` comes first: as a constant when its type is a singleton
    # (it may then be an unused slot of the parent, which cannot be read), else read from
    # the parent's argument, if the parent uses it.
    selftype = widenconst(ir.argtypes[1])
    if isa(selftype, DataType) && isdefined(selftype, :instance)
        callargs = Any[quoted(selftype.instance)]
    elseif !self_argument_is_used(sv)
        return false
    else
        callargs = Any[Argument(1)]
    end
    newargtypes = Any[widenconst(ir.argtypes[1])]
    # Static parameters without a known value are evaluated in the function and passed in.
    hoisted = Pair{Int,Int}[] # static parameter => argument
    # Values known to be constant are substituted rather than passed: the optimized code
    # may depend on them being constant (e.g. the type in an `Expr(:new)`).
    consts = Any[]
    livein_ssas = Int[]
    function undo_liveins!()
        for id in livein_ssas
            newidx[id] = 0
        end
    end
    nreal = 0
    for r in ranges, i in r
        inst = ir[SSAValue(i)]
        stmt = inst[:stmt]
        is_outlining_bookkeeping(stmt) || (nreal += 1)
        if isexpr(stmt, :static_parameter)
            # Its uses become the parameter's value, or the argument it is passed in.
            n = stmt.args[1]::Int
            sptype = ir.sptypes[n]
            t = sptype.typ
            if !sptype.undef && isa(t, Const) && !isa(t.val, TypeVar)
                push!(consts, t.val)
                newidx[i] = -(OUTLINED_CONST_BASE + length(consts))
            elseif sptype.undef
                undo_liveins!()
                return false
            else
                push!(callargs, nothing) # filled in once the parameter is evaluated
                push!(newargtypes, widenconst(t))
                push!(hoisted, n => length(callargs))
                newidx[i] = -length(callargs)
            end
            continue
        end
        if isexpr(stmt, :gc_preserve_end)
            # Ending the preservation of objects preserved by the function is dropped (see
            # below), so its operands do not need to be passed in.
            all(x::Any -> !isa(x, SSAValue) || newidx[x.id] > 0, stmt.args) || continue
        end
        if has_sparams && isa(stmt, Expr)
            head = stmt.head
            # These may refer to static parameters through the method's environment.
            if head === :foreigncall || head === :foreignglobal || head === :cfunction ||
               head === :new_opaque_closure || has_static_parameter(stmt)
                undo_liveins!()
                return false
            end
        end
        for ur in userefs(stmt)
            val = ur[]
            if isa(val, SSAValue)
                newidx[val.id] == 0 || continue
                if !can_pass_to_outlined(ir[val][:stmt])
                    undo_liveins!()
                    return false
                end
                push!(livein_ssas, val.id)
                t = ir[val][:type]
                if isa(t, Const)
                    push!(consts, t.val)
                    newidx[val.id] = -(OUTLINED_CONST_BASE + length(consts))
                else
                    push!(callargs, val)
                    push!(newargtypes, widenconst(t))
                    newidx[val.id] = -length(callargs)
                end
            elseif isa(val, Argument)
                argmap[val.n] == 0 || continue
                t = ir.argtypes[val.n]
                if isa(t, Const)
                    push!(consts, t.val)
                    argmap[val.n] = -length(consts)
                else
                    push!(callargs, val)
                    push!(newargtypes, widenconst(t))
                    argmap[val.n] = length(callargs)
                end
            elseif has_static_parameter(val)
                undo_liveins!()
                return false
            end
        end
    end
    if nreal <= 1
        undo_liveins!()
        return false
    end

    # Build the outlined code.
    bbmap = zeros(Int, length(blocks))
    for (k, b) in enumerate(region)
        bbmap[b] = k
    end
    stmts = InstructionStream(nnew)
    newblocks = Vector{BasicBlock}(undef, length(region))
    # The statements keep their order: the `j`th one is the region's `j`th statement.
    # (`newidx` of a static parameter's statement is what its uses are replaced with.)
    j = 0
    for (k, r) in enumerate(ranges)
        b = region[k]
        first_new = j + 1
        for i in r
            inst = ir[SSAValue(i)]
            j += 1
            stmt = inst[:stmt]
            if isexpr(stmt, :static_parameter)
                stmt = nothing # its uses refer to its value or the argument it is passed in
            elseif isexpr(stmt, :gc_preserve_end) &&
                   !all(x::Any -> !isa(x, SSAValue) || newidx[x.id] > 0, stmt.args)
                # The function keeps these objects preserved until the call throws.
                stmt = nothing
            else
                stmt = remap_outlined_stmt(stmt, newidx, argmap, consts, bbmap)
            end
            stmts.stmt[j] = stmt
            stmts.type[j] = inst[:type]
            stmts.info[j] = inst[:info]
            stmts.flag[j] = inst[:flag]
            stmts.line[3j-2] = ir.stmts.line[3i-2]
            stmts.line[3j-1] = ir.stmts.line[3i-1]
            stmts.line[3j] = ir.stmts.line[3i]
        end
        preds = Int[]
        k == 1 && push!(preds, 0)
        for p in blocks[b].preds
            if p == 0
                k == 1 || push!(preds, 0)
            elseif bbmap[p] != 0
                push!(preds, bbmap[p])
            end
        end
        succs = Int[bbmap[s] for s in blocks[b].succs]
        newblocks[k] = BasicBlock(StmtRange(first_new, first_new + length(r) - 1), preds, succs)
    end
    undo_liveins!()
    cfg = CFG(newblocks, Int[first(newblocks[k].stmts) for k = 2:length(newblocks)])
    di = copy(ir.debuginfo)
    # Keep only the debug info of the inlined calls the region contains, so that the
    # outlined code does not depend on (and can be shared despite) the rest of the parent.
    edges = di.edges
    edgemap = zeros(Int32, length(edges))
    newedges = DebugInfo[]
    for j = 1:nnew
        e = stmts.line[3j-1]
        e > 0 || continue
        if edgemap[e] == 0
            push!(newedges, edges[e])
            edgemap[e] = length(newedges)
        end
        stmts.line[3j-1] = edgemap[e]
    end
    di.edges = newedges
    di.codelocs = stmts.line
    outlined = IRCode(stmts, cfg, di, newargtypes, Expr[], VarState[], ir.valid_worlds)
    if is_asserts()
        verify_ir(outlined, true, false, optimizer_lattice(sv.inlining.interp), mi)
    end

    nargs = length(newargtypes)
    src = ccall(:jl_new_code_info_uninit, Ref{CodeInfo}, ())
    slotnames = Symbol[Symbol("#self#")]
    for k = 2:nargs
        push!(slotnames, Symbol(""))
    end
    src.slotnames = slotnames
    src.slotflags = fill(zero(UInt8), nargs)
    src.slottypes = copy(newargtypes)
    src.nargs = UInt(nargs)
    src.isva = false
    src.min_world = ir.valid_worlds.min_world
    src.max_world = typemax(UInt)
    src.edges = Core.svec()
    src = ir_to_codeinf!(src, outlined)
    # Name the parent method, not its specialization, as the definition the debug info
    # describes. Backtraces name the frame after the outlined method instance anyway, and
    # this lets specializations that produce the same region share one copy of it.
    di = src.debuginfo
    if di.def === mi
        src.debuginfo = Core.DebugInfo(mi.def::Method, di.linetable, di.edges, di.codelocs)
    end
    src.rettype = Union{}
    src.inlining_cost = MAX_INLINE_COST
    argtypes_to_type(newargtypes) === Union{} && return false
    ci, tag = outlined_codeinst!(mi, src, newargtypes, ir.valid_worlds.min_world)
    tag == 0 || push!(callargs, Val{tag}())

    # Replace the region with the call: hoisted static parameters and the call go before
    # the entry block's terminator, which becomes unreachable, and the region's other
    # statements are deleted with the blocks that are no longer reachable.
    term = last(blocks[entry].stmts)
    # The call gets the location of the statement that throws (the first one in the region
    # that never returns), so that the function's frame in a backtrace points at the throw.
    throwidx = split
    for r in ranges, i in r
        if ir[SSAValue(i)][:type] === Union{} && !isa(ir[SSAValue(i)][:stmt], ReturnNode)
            throwidx = i
            break
        end
    end
    line = (ir.stmts.line[3throwidx-2], ir.stmts.line[3throwidx-1], ir.stmts.line[3throwidx])
    for (n, k) in hoisted
        callargs[k] = insert_node!(ir, SSAValue(term),
            NewInstruction(Expr(:static_parameter, n), ir.sptypes[n].typ, NoCallInfo(), line,
                           IR_FLAG_CONSISTENT | IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW))
    end
    insert_node!(ir, SSAValue(term),
        NewInstruction(Expr(:invoke, ci, callargs...), Union{}, NoCallInfo(), line, IR_FLAG_NULL))
    for i = split:term-1
        ir[SSAValue(i)][:stmt] = nothing
        ir[SSAValue(i)][:type] = Nothing
        ir[SSAValue(i)][:flag] = IR_FLAGS_REMOVABLE
    end
    ir[SSAValue(term)][:stmt] = ReturnNode()
    ir[SSAValue(term)][:type] = Union{}
    for s in copy(blocks[entry].succs)
        cfg_delete_edge!(ir.cfg, entry, s)
    end
    return true
end
