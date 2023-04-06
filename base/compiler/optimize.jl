# This file is a part of Julia. License is MIT: https://julialang.org/license

#############
# constants #
#############

# The slot has uses that are not statically dominated by any assignment
# This is implied by `SLOT_USEDUNDEF`.
# If this is not set, all the uses are (statically) dominated by the defs.
# In particular, if a slot has `AssignedOnce && !StaticUndef`, it is an SSA.
const SLOT_STATICUNDEF  = 1 # slot might be used before it is defined (structurally)
const SLOT_ASSIGNEDONCE = 16 # slot is assigned to only once
const SLOT_USEDUNDEF    = 32 # slot has uses that might raise UndefVarError
# const SLOT_CALLED      = 64

# NOTE make sure to sync the flag definitions below with julia.h and `jl_code_info_set_ir` in method.c

const IR_FLAG_NULL        = 0x00
# This statement is marked as @inbounds by user.
# Ff replaced by inlining, any contained boundschecks may be removed.
const IR_FLAG_INBOUNDS    = 0x01 << 0
# This statement is marked as @inline by user
const IR_FLAG_INLINE      = 0x01 << 1
# This statement is marked as @noinline by user
const IR_FLAG_NOINLINE    = 0x01 << 2
const IR_FLAG_THROW_BLOCK = 0x01 << 3
# This statement may be removed if its result is unused. In particular,
# it must be both :effect_free and :nothrow.
# TODO: Separate these out.
const IR_FLAG_EFFECT_FREE = 0x01 << 4
# This statement was proven not to throw
const IR_FLAG_NOTHROW     = 0x01 << 5
# This is :consistent
const IR_FLAG_CONSISTENT  = 0x01 << 6

const TOP_TUPLE = GlobalRef(Core, :tuple)

# This corresponds to the type of `CodeInfo`'s `inlining_cost` field
const InlineCostType = UInt16
const MAX_INLINE_COST = typemax(InlineCostType)
const MIN_INLINE_COST = InlineCostType(10)
const MaybeCompressed = Union{CodeInfo, Vector{UInt8}}

is_inlineable(@nospecialize src::MaybeCompressed) =
    ccall(:jl_ir_inlining_cost, InlineCostType, (Any,), src) != MAX_INLINE_COST
set_inlineable!(src::CodeInfo, val::Bool) =
    src.inlining_cost = (val ? MIN_INLINE_COST : MAX_INLINE_COST)

function inline_cost_clamp(x::Int)::InlineCostType
    x > MAX_INLINE_COST && return MAX_INLINE_COST
    x < MIN_INLINE_COST && return MIN_INLINE_COST
    return convert(InlineCostType, x)
end

is_declared_inline(@nospecialize src::MaybeCompressed) =
    ccall(:jl_ir_flag_inlining, UInt8, (Any,), src) == 1

is_declared_noinline(@nospecialize src::MaybeCompressed) =
    ccall(:jl_ir_flag_inlining, UInt8, (Any,), src) == 2

#####################
# OptimizationState #
#####################

struct EdgeTracker
    edges::Vector{Any}
    valid_worlds::RefValue{WorldRange}
    EdgeTracker(edges::Vector{Any}, range::WorldRange) =
        new(edges, RefValue{WorldRange}(range))
end
EdgeTracker() = EdgeTracker(Any[], 0:typemax(UInt))

intersect!(et::EdgeTracker, range::WorldRange) =
    et.valid_worlds[] = intersect(et.valid_worlds[], range)

function add_backedge!(et::EdgeTracker, mi::MethodInstance)
    push!(et.edges, mi)
    return nothing
end
function add_invoke_backedge!(et::EdgeTracker, @nospecialize(invokesig), mi::MethodInstance)
    push!(et.edges, invokesig, mi)
    return nothing
end

is_source_inferred(@nospecialize src::MaybeCompressed) =
    ccall(:jl_ir_flag_inferred, Bool, (Any,), src)

function inlining_policy(interp::AbstractInterpreter,
    @nospecialize(src), @nospecialize(info::CallInfo), stmt_flag::UInt8, mi::MethodInstance,
    argtypes::Vector{Any})
    if isa(src, MaybeCompressed)
        is_source_inferred(src) || return nothing
        src_inlineable = is_stmt_inline(stmt_flag) || is_inlineable(src)
        return src_inlineable ? src : nothing
    elseif src === nothing && is_stmt_inline(stmt_flag)
        # if this statement is forced to be inlined, make an additional effort to find the
        # inferred source in the local cache
        # we still won't find a source for recursive call because the "single-level" inlining
        # seems to be more trouble and complex than it's worth
        inf_result = cache_lookup(optimizer_lattice(interp), mi, argtypes, get_inference_cache(interp))
        inf_result === nothing && return nothing
        src = inf_result.src
        if isa(src, CodeInfo)
            src_inferred = is_source_inferred(src)
            return src_inferred ? src : nothing
        else
            return nothing
        end
    elseif isa(src, IRCode)
        return src
    elseif isa(src, SemiConcreteResult)
        if is_declared_noinline(mi.def::Method)
            # For `NativeInterpreter`, `SemiConcreteResult` may be produced for
            # a `@noinline`-declared method when it's marked as `@constprop :aggressive`.
            # Suppress the inlining here.
            return nothing
        end
        return src
    end
    return nothing
end

struct InliningState{Interp<:AbstractInterpreter}
    et::Union{EdgeTracker,Nothing}
    world::UInt
    interp::Interp
end
function InliningState(sv::InferenceState, interp::AbstractInterpreter)
    et = EdgeTracker(sv.stmt_edges[1]::Vector{Any}, sv.valid_worlds)
    return InliningState(et, sv.world, interp)
end
function InliningState(interp::AbstractInterpreter)
    return InliningState(nothing, get_world_counter(interp), interp)
end

# get `code_cache(::AbstractInterpreter)` from `state::InliningState`
code_cache(state::InliningState) = WorldView(code_cache(state.interp), state.world)

include("compiler/ssair/driver.jl")

mutable struct OptimizationState{Interp<:AbstractInterpreter}
    linfo::MethodInstance
    src::CodeInfo
    ir::Union{Nothing, IRCode}
    stmt_info::Vector{CallInfo}
    mod::Module
    sptypes::Vector{VarState}
    slottypes::Vector{Any}
    inlining::InliningState{Interp}
    cfg::Union{Nothing,CFG}
    insert_coverage::Bool
end
function OptimizationState(sv::InferenceState, interp::AbstractInterpreter,
                           recompute_cfg::Bool=true)
    inlining = InliningState(sv, interp)
    cfg = recompute_cfg ? nothing : sv.cfg
    return OptimizationState(sv.linfo, sv.src, nothing, sv.stmt_info, sv.mod,
                             sv.sptypes, sv.slottypes, inlining, cfg, sv.insert_coverage)
end
function OptimizationState(linfo::MethodInstance, src::CodeInfo, interp::AbstractInterpreter)
    # prepare src for running optimization passes if it isn't already
    nssavalues = src.ssavaluetypes
    if nssavalues isa Int
        src.ssavaluetypes = Any[ Any for i = 1:nssavalues ]
    else
        nssavalues = length(src.ssavaluetypes::Vector{Any})
    end
    sptypes = sptypes_from_meth_instance(linfo)
    nslots = length(src.slotflags)
    slottypes = src.slottypes
    if slottypes === nothing
        slottypes = Any[ Any for i = 1:nslots ]
    end
    stmt_info = CallInfo[ NoCallInfo() for i = 1:nssavalues ]
    # cache some useful state computations
    def = linfo.def
    mod = isa(def, Method) ? def.module : def
    # Allow using the global MI cache, but don't track edges.
    # This method is mostly used for unit testing the optimizer
    inlining = InliningState(interp)
    return OptimizationState(linfo, src, nothing, stmt_info, mod, sptypes, slottypes, inlining, nothing, false)
end
function OptimizationState(linfo::MethodInstance, interp::AbstractInterpreter)
    world = get_world_counter(interp)
    src = retrieve_code_info(linfo, world)
    src === nothing && return nothing
    return OptimizationState(linfo, src, interp)
end

function ir_to_codeinf!(opt::OptimizationState)
    (; linfo, src) = opt
    src = ir_to_codeinf!(src, opt.ir::IRCode)
    opt.ir = nothing
    validate_code_in_debug_mode(linfo, src, "optimized")
    return src
end

function ir_to_codeinf!(src::CodeInfo, ir::IRCode)
    replace_code_newstyle!(src, ir)
    widen_all_consts!(src)
    src.inferred = true
    return src
end

# widen all Const elements in type annotations
function widen_all_consts!(src::CodeInfo)
    ssavaluetypes = src.ssavaluetypes::Vector{Any}
    for i = 1:length(ssavaluetypes)
        ssavaluetypes[i] = widenconst(ssavaluetypes[i])
    end

    for i = 1:length(src.code)
        x = src.code[i]
        if isa(x, PiNode)
            src.code[i] = PiNode(x.val, widenconst(x.typ))
        end
    end

    src.rettype = widenconst(src.rettype)

    return src
end

#########
# logic #
#########

_topmod(sv::OptimizationState) = _topmod(sv.mod)

is_stmt_inline(stmt_flag::UInt8)      = stmt_flag & IR_FLAG_INLINE      ≠ 0
is_stmt_noinline(stmt_flag::UInt8)    = stmt_flag & IR_FLAG_NOINLINE    ≠ 0
is_stmt_throw_block(stmt_flag::UInt8) = stmt_flag & IR_FLAG_THROW_BLOCK ≠ 0

"""
    stmt_effect_flags(stmt, rt, src::Union{IRCode,IncrementalCompact}) ->
        (consistent::Bool, effect_free_and_nothrow::Bool, nothrow::Bool)

Returns a tuple of `(:consistent, :effect_free_and_nothrow, :nothrow)` flags for a given statement.
"""
function stmt_effect_flags(𝕃ₒ::AbstractLattice, @nospecialize(stmt), @nospecialize(rt), src::Union{IRCode,IncrementalCompact})
    # TODO: We're duplicating analysis from inference here.
    isa(stmt, PiNode) && return (true, true, true)
    isa(stmt, PhiNode) && return (true, true, true)
    isa(stmt, ReturnNode) && return (true, false, true)
    isa(stmt, GotoNode) && return (true, false, true)
    isa(stmt, GotoIfNot) && return (true, false, ⊑(𝕃ₒ, argextype(stmt.cond, src), Bool))
    if isa(stmt, GlobalRef)
        nothrow = isdefined(stmt.mod, stmt.name)
        consistent = nothrow && isconst(stmt.mod, stmt.name)
        return (consistent, nothrow, nothrow)
    elseif isa(stmt, Expr)
        (; head, args) = stmt
        if head === :static_parameter
            # if we aren't certain enough about the type, it might be an UndefVarError at runtime
            sptypes = isa(src, IRCode) ? src.sptypes : src.ir.sptypes
            nothrow = !sptypes[args[1]::Int].undef
            return (true, nothrow, nothrow)
        end
        if head === :call
            f = argextype(args[1], src)
            f = singleton_type(f)
            f === nothing && return (false, false, false)
            if f === UnionAll
                # TODO: This is a weird special case - should be determined in inference
                argtypes = Any[argextype(args[arg], src) for arg in 2:length(args)]
                nothrow = _builtin_nothrow(𝕃ₒ, f, argtypes, rt)
                return (true, nothrow, nothrow)
            end
            if f === Intrinsics.cglobal
                # TODO: these are not yet linearized
                return (false, false, false)
            end
            isa(f, Builtin) || return (false, false, false)
            # Needs to be handled in inlining to look at the callee effects
            f === Core._apply_iterate && return (false, false, false)
            argtypes = Any[argextype(args[arg], src) for arg in 1:length(args)]
            effects = builtin_effects(𝕃ₒ, f, ArgInfo(args, argtypes), rt)
            consistent = is_consistent(effects)
            effect_free = is_effect_free(effects)
            nothrow = is_nothrow(effects)
            return (consistent, effect_free & nothrow, nothrow)
        elseif head === :new
            atyp = argextype(args[1], src)
            # `Expr(:new)` of unknown type could raise arbitrary TypeError.
            typ, isexact = instanceof_tfunc(atyp)
            if !isexact
                atyp = unwrap_unionall(widenconst(atyp))
                if isType(atyp) && isTypeDataType(atyp.parameters[1])
                    typ = atyp.parameters[1]
                else
                    return (false, false, false)
                end
                isabstracttype(typ) && return (false, false, false)
            else
                isconcretedispatch(typ) || return (false, false, false)
            end
            typ = typ::DataType
            fcount = datatype_fieldcount(typ)
            fcount === nothing && return (false, false, false)
            fcount >= length(args) - 1 || return (false, false, false)
            for fld_idx in 1:(length(args) - 1)
                eT = argextype(args[fld_idx + 1], src)
                fT = fieldtype(typ, fld_idx)
                # Currently, we cannot represent any type equality constraints
                # in the lattice, so if we see any type of type parameter,
                # there is very little we can say about it
                if !isexact && has_free_typevars(fT)
                    return (false, false, false)
                end
                ⊑(𝕃ₒ, eT, fT) || return (false, false, false)
            end
            return (false, true, true)
        elseif head === :foreigncall
            effects = foreigncall_effects(stmt) do @nospecialize x
                argextype(x, src)
            end
            consistent = is_consistent(effects)
            effect_free = is_effect_free(effects)
            nothrow = is_nothrow(effects)
            return (consistent, effect_free & nothrow, nothrow)
        elseif head === :new_opaque_closure
            length(args) < 4 && return (false, false, false)
            typ = argextype(args[1], src)
            typ, isexact = instanceof_tfunc(typ)
            isexact || return (false, false, false)
            ⊑(𝕃ₒ, typ, Tuple) || return (false, false, false)
            rt_lb = argextype(args[2], src)
            rt_ub = argextype(args[3], src)
            source = argextype(args[4], src)
            if !(⊑(𝕃ₒ, rt_lb, Type) && ⊑(𝕃ₒ, rt_ub, Type) && ⊑(𝕃ₒ, source, Method))
                return (false, false, false)
            end
            return (false, true, true)
        elseif head === :isdefined || head === :the_exception || head === :copyast || head === :inbounds || head === :boundscheck
            return (true, true, true)
        else
            # e.g. :loopinfo
            return (false, false, false)
        end
    end
    isa(stmt, UnoptSlot) && error("unexpected IR elements")
    return (true, true, true)
end

"""
    argextype(x, src::Union{IRCode,IncrementalCompact}) -> t
    argextype(x, src::CodeInfo, sptypes::Vector{VarState}) -> t

Return the type of value `x` in the context of inferred source `src`.
Note that `t` might be an extended lattice element.
Use `widenconst(t)` to get the native Julia type of `x`.
"""
argextype(@nospecialize(x), ir::IRCode, sptypes::Vector{VarState} = ir.sptypes) =
    argextype(x, ir, sptypes, ir.argtypes)
function argextype(@nospecialize(x), compact::IncrementalCompact, sptypes::Vector{VarState} = compact.ir.sptypes)
    isa(x, AnySSAValue) && return types(compact)[x]
    return argextype(x, compact, sptypes, compact.ir.argtypes)
end
argextype(@nospecialize(x), src::CodeInfo, sptypes::Vector{VarState}) = argextype(x, src, sptypes, src.slottypes::Vector{Any})
function argextype(
    @nospecialize(x), src::Union{IRCode,IncrementalCompact,CodeInfo},
    sptypes::Vector{VarState}, slottypes::Vector{Any})
    if isa(x, Expr)
        if x.head === :static_parameter
            return sptypes[x.args[1]::Int].typ
        elseif x.head === :boundscheck
            return Bool
        elseif x.head === :copyast
            return argextype(x.args[1], src, sptypes, slottypes)
        end
        @assert false "argextype only works on argument-position values"
    elseif isa(x, SlotNumber)
        return slottypes[x.id]
    elseif isa(x, TypedSlot)
        return x.typ
    elseif isa(x, SSAValue)
        return abstract_eval_ssavalue(x, src)
    elseif isa(x, Argument)
        return slottypes[x.n]
    elseif isa(x, QuoteNode)
        return Const(x.value)
    elseif isa(x, GlobalRef)
        return abstract_eval_globalref(x)
    elseif isa(x, PhiNode)
        return Any
    elseif isa(x, PiNode)
        return x.typ
    else
        return Const(x)
    end
end
abstract_eval_ssavalue(s::SSAValue, src::CodeInfo) = abstract_eval_ssavalue(s, src.ssavaluetypes::Vector{Any})
abstract_eval_ssavalue(s::SSAValue, src::Union{IRCode,IncrementalCompact}) = types(src)[s]

"""
    finish(interp::AbstractInterpreter, opt::OptimizationState,
           ir::IRCode, caller::InferenceResult)

Post-process information derived by Julia-level optimizations for later use.
In particular, this function determines the inlineability of the optimized code.
"""
function finish(interp::AbstractInterpreter, opt::OptimizationState,
                ir::IRCode, caller::InferenceResult)
    (; src, linfo) = opt
    (; def, specTypes) = linfo

    force_noinline = is_declared_noinline(src)

    # compute inlining and other related optimizations
    result = caller.result
    @assert !(result isa LimitedAccuracy)
    result = widenslotwrapper(result)

    opt.ir = ir

    # determine and cache inlineability
    union_penalties = false
    if !force_noinline
        sig = unwrap_unionall(specTypes)
        if isa(sig, DataType) && sig.name === Tuple.name
            for P in sig.parameters
                P = unwrap_unionall(P)
                if isa(P, Union)
                    union_penalties = true
                    break
                end
            end
        else
            force_noinline = true
        end
        if !is_declared_inline(src) && result === Bottom
            force_noinline = true
        end
    end
    if force_noinline
        set_inlineable!(src, false)
    elseif isa(def, Method)
        if is_declared_inline(src) && isdispatchtuple(specTypes)
            # obey @inline declaration if a dispatch barrier would not help
            set_inlineable!(src, true)
        else
            # compute the cost (size) of inlining this code
            params = OptimizationParams(interp)
            cost_threshold = default = params.inline_cost_threshold
            if ⊑(optimizer_lattice(interp), result, Tuple) && !isconcretetype(widenconst(result))
                cost_threshold += params.inline_tupleret_bonus
            end
            # if the method is declared as `@inline`, increase the cost threshold 20x
            if is_declared_inline(src)
                cost_threshold += 19*default
            end
            # a few functions get special treatment
            if def.module === _topmod(def.module)
                name = def.name
                if name === :iterate || name === :unsafe_convert || name === :cconvert
                    cost_threshold += 4*default
                end
            end
            src.inlining_cost = inline_cost(ir, params, union_penalties, cost_threshold)
        end
    end
    return nothing
end

# run the optimization work
function optimize(interp::AbstractInterpreter, opt::OptimizationState, caller::InferenceResult)
    @timeit "optimizer" ir = run_passes(opt.src, opt, caller)
    return finish(interp, opt, ir, caller)
end

using .EscapeAnalysis
import .EscapeAnalysis: EscapeState, ArgEscapeCache, is_ipo_profitable

"""
    cache_escapes!(caller::InferenceResult, estate::EscapeState)

Transforms escape information of call arguments of `caller`,
and then caches it into a global cache for later interprocedural propagation.
"""
cache_escapes!(caller::InferenceResult, estate::EscapeState) =
    caller.argescapes = ArgEscapeCache(estate)

function ipo_escape_cache(mi_cache::MICache) where MICache
    return function (linfo::Union{InferenceResult,MethodInstance})
        if isa(linfo, InferenceResult)
            argescapes = linfo.argescapes
        else
            codeinst = get(mi_cache, linfo, nothing)
            isa(codeinst, CodeInstance) || return nothing
            argescapes = codeinst.argescapes
        end
        return argescapes !== nothing ? argescapes::ArgEscapeCache : nothing
    end
end
null_escape_cache(linfo::Union{InferenceResult,MethodInstance}) = nothing

macro pass(name, expr)
    optimize_until = esc(:optimize_until)
    stage = esc(:__stage__)
    macrocall = :(@timeit $(esc(name)) $(esc(expr)))
    macrocall.args[2] = __source__  # `@timeit` may want to use it
    quote
        $macrocall
        matchpass($optimize_until, ($stage += 1), $(esc(name))) && $(esc(:(@goto __done__)))
    end
end

matchpass(optimize_until::Int, stage, _) = optimize_until == stage
matchpass(optimize_until::String, _, name) = optimize_until == name
matchpass(::Nothing, _, _) = false

function run_passes(
    ci::CodeInfo,
    sv::OptimizationState,
    caller::InferenceResult,
    optimize_until = nothing,  # run all passes by default
)
    __stage__ = 0  # used by @pass
    # NOTE: The pass name MUST be unique for `optimize_until::AbstractString` to work
    @pass "convert"   ir = convert_to_ircode(ci, sv)
    @pass "slot2reg"  ir = slot2reg(ir, ci, sv)
    # TODO: Domsorting can produce an updated domtree - no need to recompute here
    @pass "compact 1" ir = compact!(ir)
    @pass "Inlining"  ir = ssa_inlining_pass!(ir, sv.inlining, ci.propagate_inbounds)
    # @timeit "verify 2" verify_ir(ir)
    @pass "compact 2" ir = compact!(ir)
    @pass "SROA"      ir = sroa_pass!(ir, sv.inlining)
    @pass "ADCE"      ir = adce_pass!(ir, sv.inlining)
    @pass "type lift" ir = type_lift_pass!(ir)
    @pass "compact 3" ir = compact!(ir)
    if JLOptions().debug_level == 2
        @timeit "verify 3" (verify_ir(ir); verify_linetable(ir.linetable))
    end
    @label __done__  # used by @pass
    return ir
end

function convert_to_ircode(ci::CodeInfo, sv::OptimizationState)
    linetable = ci.linetable
    if !isa(linetable, Vector{LineInfoNode})
        linetable = collect(LineInfoNode, linetable::Vector{Any})::Vector{LineInfoNode}
    end

    # Go through and add an unreachable node after every
    # Union{} call. Then reindex labels.
    code = copy_exprargs(ci.code)
    stmtinfo = sv.stmt_info
    codelocs = ci.codelocs
    ssavaluetypes = ci.ssavaluetypes::Vector{Any}
    ssaflags = ci.ssaflags
    meta = Expr[]
    idx = 1
    oldidx = 1
    nstmts = length(code)
    ssachangemap = labelchangemap = nothing
    prevloc = zero(eltype(ci.codelocs))
    while idx <= length(code)
        codeloc = codelocs[idx]
        if sv.insert_coverage && codeloc != prevloc && codeloc != 0
            # insert a side-effect instruction before the current instruction in the same basic block
            insert!(code, idx, Expr(:code_coverage_effect))
            insert!(codelocs, idx, codeloc)
            insert!(ssavaluetypes, idx, Nothing)
            insert!(stmtinfo, idx, NoCallInfo())
            insert!(ssaflags, idx, IR_FLAG_NULL)
            if ssachangemap === nothing
                ssachangemap = fill(0, nstmts)
            end
            if labelchangemap === nothing
                labelchangemap = fill(0, nstmts)
            end
            ssachangemap[oldidx] += 1
            if oldidx < length(labelchangemap)
                labelchangemap[oldidx + 1] += 1
            end
            idx += 1
            prevloc = codeloc
        end
        if code[idx] isa Expr && ssavaluetypes[idx] === Union{}
            if !(idx < length(code) && isa(code[idx + 1], ReturnNode) && !isdefined((code[idx + 1]::ReturnNode), :val))
                # insert unreachable in the same basic block after the current instruction (splitting it)
                insert!(code, idx + 1, ReturnNode())
                insert!(codelocs, idx + 1, codelocs[idx])
                insert!(ssavaluetypes, idx + 1, Union{})
                insert!(stmtinfo, idx + 1, NoCallInfo())
                insert!(ssaflags, idx + 1, IR_FLAG_NOTHROW)
                if ssachangemap === nothing
                    ssachangemap = fill(0, nstmts)
                end
                if labelchangemap === nothing
                    labelchangemap = sv.insert_coverage ? fill(0, nstmts) : ssachangemap
                end
                if oldidx < length(ssachangemap)
                    ssachangemap[oldidx + 1] += 1
                    sv.insert_coverage && (labelchangemap[oldidx + 1] += 1)
                end
                idx += 1
            end
        end
        idx += 1
        oldidx += 1
    end

    cfg = sv.cfg
    if ssachangemap !== nothing && labelchangemap !== nothing
        renumber_ir_elements!(code, ssachangemap, labelchangemap)
        cfg = nothing # recompute CFG
    end

    for i = 1:length(code)
        code[i] = process_meta!(meta, code[i])
    end
    strip_trailing_junk!(ci, code, stmtinfo)
    types = Any[]
    stmts = InstructionStream(code, types, stmtinfo, codelocs, ssaflags)
    if cfg === nothing
        cfg = compute_basic_blocks(code)
    end
    # NOTE this `argtypes` contains types of slots yet: it will be modified to contain the
    # types of call arguments only once `slot2reg` converts this `IRCode` to the SSA form
    # and eliminates slots (see below)
    argtypes = sv.slottypes
    return IRCode(stmts, cfg, linetable, argtypes, meta, sv.sptypes)
end

function process_meta!(meta::Vector{Expr}, @nospecialize stmt)
    if isexpr(stmt, :meta) && length(stmt.args) ≥ 1
        push!(meta, stmt)
        return nothing
    end
    return stmt
end

function slot2reg(ir::IRCode, ci::CodeInfo, sv::OptimizationState)
    # need `ci` for the slot metadata, IR for the code
    svdef = sv.linfo.def
    nargs = isa(svdef, Method) ? Int(svdef.nargs) : 0
    @timeit "domtree 1" domtree = construct_domtree(ir.cfg.blocks)
    defuse_insts = scan_slot_def_use(nargs, ci, ir.stmts.inst)
    𝕃ₒ = optimizer_lattice(sv.inlining.interp)
    @timeit "construct_ssa" ir = construct_ssa!(ci, ir, domtree, defuse_insts, sv.slottypes, 𝕃ₒ) # consumes `ir`
    # NOTE now we have converted `ir` to the SSA form and eliminated slots
    # let's resize `argtypes` now and remove unnecessary types for the eliminated slots
    resize!(ir.argtypes, nargs)
    return ir
end

## Computing the cost of a function body

# saturating sum (inputs are nonnegative), prevents overflow with typemax(Int) below
plus_saturate(x::Int, y::Int) = max(x, y, x+y)

# known return type
isknowntype(@nospecialize T) = (T === Union{}) || isa(T, Const) || isconcretetype(widenconst(T))

function statement_cost(ex::Expr, line::Int, src::Union{CodeInfo, IRCode}, sptypes::Vector{VarState},
                        union_penalties::Bool, params::OptimizationParams, error_path::Bool = false)
    head = ex.head
    if is_meta_expr_head(head)
        return 0
    elseif head === :call
        farg = ex.args[1]
        ftyp = argextype(farg, src, sptypes)
        if ftyp === IntrinsicFunction && farg isa SSAValue
            # if this comes from code that was already inlined into another function,
            # Consts have been widened. try to recover in simple cases.
            farg = isa(src, CodeInfo) ? src.code[farg.id] : src.stmts[farg.id][:inst]
            if isa(farg, GlobalRef) || isa(farg, QuoteNode) || isa(farg, IntrinsicFunction) || isexpr(farg, :static_parameter)
                ftyp = argextype(farg, src, sptypes)
            end
        end
        f = singleton_type(ftyp)
        if isa(f, IntrinsicFunction)
            iidx = Int(reinterpret(Int32, f::IntrinsicFunction)) + 1
            if !isassigned(T_IFUNC_COST, iidx)
                # unknown/unhandled intrinsic
                return params.inline_nonleaf_penalty
            end
            return T_IFUNC_COST[iidx]
        end
        if isa(f, Builtin) && f !== invoke
            # The efficiency of operations like a[i] and s.b
            # depend strongly on whether the result can be
            # inferred, so check the type of ex
            if f === Core.getfield || f === Core.tuple || f === Core.getglobal
                # we might like to penalize non-inferrability, but
                # tuple iteration/destructuring makes that impossible
                # return plus_saturate(argcost, isknowntype(extyp) ? 1 : params.inline_nonleaf_penalty)
                return 0
            elseif (f === Core.arrayref || f === Core.const_arrayref || f === Core.arrayset) && length(ex.args) >= 3
                atyp = argextype(ex.args[3], src, sptypes)
                return isknowntype(atyp) ? 4 : error_path ? params.inline_error_path_cost : params.inline_nonleaf_penalty
            elseif f === typeassert && isconstType(widenconst(argextype(ex.args[3], src, sptypes)))
                return 1
            elseif f === Core.isa
                # If we're in a union context, we penalize type computations
                # on union types. In such cases, it is usually better to perform
                # union splitting on the outside.
                if union_penalties && isa(argextype(ex.args[2],  src, sptypes), Union)
                    return params.inline_nonleaf_penalty
                end
            end
            fidx = find_tfunc(f)
            if fidx === nothing
                # unknown/unhandled builtin
                # Use the generic cost of a direct function call
                return 20
            end
            return T_FFUNC_COST[fidx]
        end
        extyp = line == -1 ? Any : argextype(SSAValue(line), src, sptypes)
        if extyp === Union{}
            return 0
        end
        return error_path ? params.inline_error_path_cost : params.inline_nonleaf_penalty
    elseif head === :foreigncall || head === :invoke || head === :invoke_modify
        # Calls whose "return type" is Union{} do not actually return:
        # they are errors. Since these are not part of the typical
        # run-time of the function, we omit them from
        # consideration. This way, non-inlined error branches do not
        # prevent inlining.
        extyp = line == -1 ? Any : argextype(SSAValue(line), src, sptypes)
        return extyp === Union{} ? 0 : 20
    elseif head === :(=)
        if ex.args[1] isa GlobalRef
            cost = 20
        else
            cost = 0
        end
        a = ex.args[2]
        if a isa Expr
            cost = plus_saturate(cost, statement_cost(a, -1, src, sptypes, union_penalties, params, error_path))
        end
        return cost
    elseif head === :copyast
        return 100
    elseif head === :enter
        # try/catch is a couple function calls,
        # but don't inline functions with try/catch
        # since these aren't usually performance-sensitive functions,
        # and llvm is more likely to miscompile them when these functions get large
        return typemax(Int)
    end
    return 0
end

function statement_or_branch_cost(@nospecialize(stmt), line::Int, src::Union{CodeInfo, IRCode}, sptypes::Vector{VarState},
                                  union_penalties::Bool, params::OptimizationParams)
    thiscost = 0
    dst(tgt) = isa(src, IRCode) ? first(src.cfg.blocks[tgt].stmts) : tgt
    if stmt isa Expr
        thiscost = statement_cost(stmt, line, src, sptypes, union_penalties, params,
                                  is_stmt_throw_block(isa(src, IRCode) ? src.stmts.flag[line] : src.ssaflags[line]))::Int
    elseif stmt isa GotoNode
        # loops are generally always expensive
        # but assume that forward jumps are already counted for from
        # summing the cost of the not-taken branch
        thiscost = dst(stmt.label) < line ? 40 : 0
    elseif stmt isa GotoIfNot
        thiscost = dst(stmt.dest) < line ? 40 : 0
    end
    return thiscost
end

function inline_cost(ir::IRCode, params::OptimizationParams, union_penalties::Bool=false,
                       cost_threshold::Integer=params.inline_cost_threshold)::InlineCostType
    bodycost::Int = 0
    for line = 1:length(ir.stmts)
        stmt = ir.stmts[line][:inst]
        thiscost = statement_or_branch_cost(stmt, line, ir, ir.sptypes, union_penalties, params)
        bodycost = plus_saturate(bodycost, thiscost)
        bodycost > cost_threshold && return MAX_INLINE_COST
    end
    return inline_cost_clamp(bodycost)
end

function statement_costs!(cost::Vector{Int}, body::Vector{Any}, src::Union{CodeInfo, IRCode}, sptypes::Vector{VarState}, unionpenalties::Bool, params::OptimizationParams)
    maxcost = 0
    for line = 1:length(body)
        stmt = body[line]
        thiscost = statement_or_branch_cost(stmt, line, src, sptypes,
                                            unionpenalties, params)
        cost[line] = thiscost
        if thiscost > maxcost
            maxcost = thiscost
        end
    end
    return maxcost
end

function renumber_ir_elements!(body::Vector{Any}, ssachangemap::Vector{Int})
    return renumber_ir_elements!(body, ssachangemap, ssachangemap)
end

function cumsum_ssamap!(ssachangemap::Vector{Int})
    any_change = false
    rel_change = 0
    for i = 1:length(ssachangemap)
        val = ssachangemap[i]
        any_change |= val ≠ 0
        rel_change += val
        if val == -1
            # Keep a marker that this statement was deleted
            ssachangemap[i] = typemin(Int)
        else
            ssachangemap[i] = rel_change
        end
    end
    return any_change
end

function renumber_ir_elements!(body::Vector{Any}, ssachangemap::Vector{Int}, labelchangemap::Vector{Int})
    any_change = cumsum_ssamap!(labelchangemap)
    if ssachangemap !== labelchangemap
        any_change |= cumsum_ssamap!(ssachangemap)
    end
    any_change || return
    for i = 1:length(body)
        el = body[i]
        if isa(el, GotoNode)
            body[i] = GotoNode(el.label + labelchangemap[el.label])
        elseif isa(el, GotoIfNot)
            cond = el.cond
            if isa(cond, SSAValue)
                cond = SSAValue(cond.id + ssachangemap[cond.id])
            end
            was_deleted = labelchangemap[el.dest] == typemin(Int)
            body[i] = was_deleted ? cond : GotoIfNot(cond, el.dest + labelchangemap[el.dest])
        elseif isa(el, ReturnNode)
            if isdefined(el, :val)
                val = el.val
                if isa(val, SSAValue)
                    body[i] = ReturnNode(SSAValue(val.id + ssachangemap[val.id]))
                end
            end
        elseif isa(el, SSAValue)
            body[i] = SSAValue(el.id + ssachangemap[el.id])
        elseif isa(el, PhiNode)
            i = 1
            edges = el.edges
            values = el.values
            while i <= length(edges)
                was_deleted = ssachangemap[edges[i]] == typemin(Int)
                if was_deleted
                    deleteat!(edges, i)
                    deleteat!(values, i)
                else
                    edges[i] += ssachangemap[edges[i]]
                    val = values[i]
                    if isa(val, SSAValue)
                        values[i] = SSAValue(val.id + ssachangemap[val.id])
                    end
                    i += 1
                end
            end
        elseif isa(el, Expr)
            if el.head === :(=) && el.args[2] isa Expr
                el = el.args[2]::Expr
            end
            if el.head === :enter
                tgt = el.args[1]::Int
                el.args[1] = tgt + labelchangemap[tgt]
            elseif !is_meta_expr_head(el.head)
                args = el.args
                for i = 1:length(args)
                    el = args[i]
                    if isa(el, SSAValue)
                        args[i] = SSAValue(el.id + ssachangemap[el.id])
                    end
                end
            end
        end
    end
end
