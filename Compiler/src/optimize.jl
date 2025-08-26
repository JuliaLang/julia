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

const IR_FLAG_NULL        = zero(UInt32)
# This statement is marked as @inbounds by user.
# If replaced by inlining, any contained boundschecks may be removed.
const IR_FLAG_INBOUNDS    = one(UInt32) << 0
# This statement is marked as @inline by user
const IR_FLAG_INLINE      = one(UInt32) << 1
# This statement is marked as @noinline by user
const IR_FLAG_NOINLINE    = one(UInt32) << 2
# This statement is proven :consistent
const IR_FLAG_CONSISTENT  = one(UInt32) << 3
# This statement is proven :effect_free
const IR_FLAG_EFFECT_FREE = one(UInt32) << 4
# This statement is proven :nothrow
const IR_FLAG_NOTHROW     = one(UInt32) << 5
# This statement is proven :terminates_globally
const IR_FLAG_TERMINATES  = one(UInt32) << 6
#const IR_FLAG_TERMINATES_LOCALLY = one(UInt32) << 7
#const IR_FLAG_NOTASKSTATE = one(UInt32) << 8
#const IR_FLAG_INACCESSIBLEMEM = one(UInt32) << 9
const IR_FLAG_NOUB        = one(UInt32) << 10
#const IR_FLAG_NOUBINIB   = one(UInt32) << 11
#const IR_FLAG_CONSISTENTOVERLAY = one(UInt32) << 12
# This statement is :nortcall
const IR_FLAG_NORTCALL = one(UInt32) << 13
# An optimization pass has updated this statement in a way that may
# have exposed information that inference did not see. Re-running
# inference on this statement may be profitable.
const IR_FLAG_REFINED     = one(UInt32) << 16
# This statement has no users and may be deleted if flags get refined to IR_FLAGS_REMOVABLE
const IR_FLAG_UNUSED      = one(UInt32) << 17
# TODO: Both of these next two should eventually go away once
# This statement is :effect_free == EFFECT_FREE_IF_INACCESSIBLEMEMONLY
const IR_FLAG_EFIIMO      = one(UInt32) << 18
# This statement is :inaccessiblememonly == INACCESSIBLEMEM_OR_ARGMEMONLY
const IR_FLAG_INACCESSIBLEMEM_OR_ARGMEM = one(UInt32) << 19

const NUM_IR_FLAGS = 3 # sync with julia.h

const IR_FLAGS_EFFECTS =
    IR_FLAG_CONSISTENT | IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW |
    IR_FLAG_TERMINATES | IR_FLAG_NOUB | IR_FLAG_NORTCALL

const IR_FLAGS_REMOVABLE = IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW | IR_FLAG_TERMINATES

const IR_FLAGS_NEEDS_EA = IR_FLAG_EFIIMO | IR_FLAG_INACCESSIBLEMEM_OR_ARGMEM

has_flag(curr::UInt32, flag::UInt32) = (curr & flag) == flag

function iscallstmt(@nospecialize stmt)
    stmt isa Expr || return false
    head = stmt.head
    return head === :call || head === :invoke || head === :foreigncall
end

function flags_for_effects(effects::Effects)
    flags = zero(UInt32)
    if is_consistent(effects)
        flags |= IR_FLAG_CONSISTENT
    end
    if is_effect_free(effects)
        flags |= IR_FLAG_EFFECT_FREE
    elseif is_effect_free_if_inaccessiblememonly(effects)
        flags |= IR_FLAG_EFIIMO
    end
    if is_nothrow(effects)
        flags |= IR_FLAG_NOTHROW
    end
    if is_terminates(effects)
        flags |= IR_FLAG_TERMINATES
    end
    if is_inaccessiblemem_or_argmemonly(effects)
        flags |= IR_FLAG_INACCESSIBLEMEM_OR_ARGMEM
    end
    if is_noub(effects)
        flags |= IR_FLAG_NOUB
    end
    if is_nortcall(effects)
        flags |= IR_FLAG_NORTCALL
    end
    return flags
end

const TOP_TUPLE = GlobalRef(Core, :tuple)

inlining_cost(@nospecialize src) =
    src isa Union{MaybeCompressed,UInt8} ? ccall(:jl_ir_inlining_cost, InlineCostType, (Any,), src) : MAX_INLINE_COST
is_inlineable(@nospecialize src) = inlining_cost(src) != MAX_INLINE_COST
set_inlineable!(src::CodeInfo, val::Bool) =
    src.inlining_cost = (val ? MIN_INLINE_COST : MAX_INLINE_COST)

function inline_cost_clamp(x::Int)
    x > MAX_INLINE_COST && return MAX_INLINE_COST
    x < MIN_INLINE_COST && return MIN_INLINE_COST
    x = ccall(:jl_encode_inlining_cost, UInt8, (InlineCostType,), x)
    x = ccall(:jl_decode_inlining_cost, InlineCostType, (UInt8,), x)
    return x
end

const SRC_FLAG_DECLARED_INLINE = 0x1
const SRC_FLAG_DECLARED_NOINLINE = 0x2

is_declared_inline(@nospecialize src::MaybeCompressed) =
    ccall(:jl_ir_flag_inlining, UInt8, (Any,), src) == SRC_FLAG_DECLARED_INLINE

is_declared_noinline(@nospecialize src::MaybeCompressed) =
    ccall(:jl_ir_flag_inlining, UInt8, (Any,), src) == SRC_FLAG_DECLARED_NOINLINE

#####################
# OptimizationState #
#####################

# return whether this src should be inlined. If so, retrieve_ir_for_inlining must return an IRCode from it

function src_inlining_policy(interp::AbstractInterpreter, mi::MethodInstance,
    @nospecialize(src), @nospecialize(info::CallInfo), stmt_flag::UInt32)
    # If we have a generator, but we can't invoke it (because argument type information is lacking),
    # don't inline so we defer its invocation to runtime where we'll have precise type information.
    if isa(mi.def, Method) && hasgenerator(mi)
        may_invoke_generator(mi) || return false
    end
    return src_inlining_policy(interp, src, info, stmt_flag)
end

function src_inlining_policy(::AbstractInterpreter,
    @nospecialize(src), @nospecialize(info::CallInfo), stmt_flag::UInt32)
    isa(src, OptimizationState) && (src = src.src)
    if isa(src, MaybeCompressed)
        src_inlineable = is_stmt_inline(stmt_flag) || is_inlineable(src)
        return src_inlineable
    elseif isa(src, IRCode)
        return true
    end
    @assert !isa(src, CodeInstance) # handled by caller
    return false
end

struct InliningState{Interp<:AbstractInterpreter}
    edges::Vector{Any}
    world::UInt
    interp::Interp
    opt_cache::IdDict{MethodInstance,CodeInstance}
end
function InliningState(sv::InferenceState, interp::AbstractInterpreter,
                       opt_cache::IdDict{MethodInstance,CodeInstance}=IdDict{MethodInstance,CodeInstance}())
    return InliningState(sv.edges, frame_world(sv), interp, opt_cache)
end
function InliningState(interp::AbstractInterpreter,
                       opt_cache::IdDict{MethodInstance,CodeInstance}=IdDict{MethodInstance,CodeInstance}())
    return InliningState(Any[], get_inference_world(interp), interp, opt_cache)
end

struct OptimizerCache{CodeCache}
    wvc::WorldView{CodeCache}
    owner
    opt_cache::IdDict{MethodInstance,CodeInstance}
    function OptimizerCache(
        wvc::WorldView{CodeCache},
        @nospecialize(owner),
        opt_cache::IdDict{MethodInstance,CodeInstance}) where CodeCache
        new{CodeCache}(wvc, owner, opt_cache)
    end
end
function get((; wvc, owner, opt_cache)::OptimizerCache, mi::MethodInstance, default)
    if haskey(opt_cache, mi)
        codeinst = opt_cache[mi]
        @assert codeinst.min_world ≤ wvc.worlds.min_world &&
                wvc.worlds.max_world ≤ codeinst.max_world &&
                codeinst.owner === owner
        @assert isdefined(codeinst, :inferred) && codeinst.inferred === nothing
        return codeinst
    end
    return get(wvc, mi, default)
end

# get `code_cache(::AbstractInterpreter)` from `state::InliningState`
function code_cache(state::InliningState)
    cache = WorldView(code_cache(state.interp), state.world)
    owner = cache_owner(state.interp)
    return OptimizerCache(cache, owner, state.opt_cache)
end

mutable struct OptimizationResult
    ir::IRCode
    inline_flag::UInt8
    simplified::Bool # indicates whether the IR was processed with `cfg_simplify!`
end

function simplify_ir!(result::OptimizationResult)
    result.ir = cfg_simplify!(result.ir)
    result.simplified = true
end

mutable struct OptimizationState{Interp<:AbstractInterpreter}
    linfo::MethodInstance
    src::CodeInfo
    optresult::Union{Nothing, OptimizationResult}
    stmt_info::Vector{CallInfo}
    mod::Module
    sptypes::Vector{VarState}
    slottypes::Vector{Any}
    inlining::InliningState{Interp}
    cfg::CFG
    unreachable::BitSet
    bb_vartables::Vector{Union{Nothing,VarTable}}
    insert_coverage::Bool
end
function OptimizationState(sv::InferenceState, interp::AbstractInterpreter,
                           opt_cache::IdDict{MethodInstance,CodeInstance}=IdDict{MethodInstance,CodeInstance}())
    inlining = InliningState(sv, interp, opt_cache)
    return OptimizationState(sv.linfo, sv.src, nothing, sv.stmt_info, sv.mod,
                             sv.sptypes, sv.slottypes, inlining, sv.cfg,
                             sv.unreachable, sv.bb_vartables, sv.insert_coverage)
end
function OptimizationState(mi::MethodInstance, src::CodeInfo, interp::AbstractInterpreter,
                           opt_cache::IdDict{MethodInstance,CodeInstance}=IdDict{MethodInstance,CodeInstance}())
    # prepare src for running optimization passes if it isn't already
    nssavalues = src.ssavaluetypes
    if nssavalues isa Int
        src.ssavaluetypes = Any[ Any for _ = 1:nssavalues ]
    else
        nssavalues = length(src.ssavaluetypes::Vector{Any})
    end
    sptypes = sptypes_from_meth_instance(mi)
    nslots = length(src.slotflags)
    slottypes = src.slottypes
    if slottypes === nothing
        slottypes = Any[ Any for _ = 1:nslots ]
    end
    stmt_info = CallInfo[ NoCallInfo() for _ = 1:nssavalues ]
    # cache some useful state computations
    def = mi.def
    mod = isa(def, Method) ? def.module : def
    # Allow using the global MI cache, but don't track edges.
    # This method is mostly used for unit testing the optimizer
    inlining = InliningState(interp, opt_cache)
    cfg = compute_basic_blocks(src.code)
    unreachable = BitSet()
    bb_vartables = Union{VarTable,Nothing}[]
    for _ = 1:length(cfg.blocks)
        push!(bb_vartables, VarState[
            VarState(slottypes[slot], src.slotflags[slot] & SLOT_USEDUNDEF != 0)
            for slot = 1:nslots
        ])
    end
    return OptimizationState(mi, src, nothing, stmt_info, mod, sptypes, slottypes, inlining, cfg, unreachable, bb_vartables, false)
end
function OptimizationState(mi::MethodInstance, interp::AbstractInterpreter)
    world = get_inference_world(interp)
    src = retrieve_code_info(mi, world)
    src === nothing && return nothing
    return OptimizationState(mi, src, interp)
end

function argextype end # imported by EscapeAnalysis
function try_compute_field end # imported by EscapeAnalysis

include("ssair/heap.jl")
include("ssair/slot2ssa.jl")
include("ssair/inlining.jl")
include("ssair/verify.jl")
include("ssair/legacy.jl")
include("ssair/EscapeAnalysis.jl")
include("ssair/passes.jl")
include("ssair/irinterp.jl")

function ir_to_codeinf!(opt::OptimizationState, frame::InferenceState, edges::SimpleVector)
    ir_to_codeinf!(opt, edges, compute_inlining_cost(frame.interp, frame.result, opt.optresult))
end

function ir_to_codeinf!(opt::OptimizationState, edges::SimpleVector, inlining_cost::InlineCostType)
    src = ir_to_codeinf!(opt, edges)
    src.inlining_cost = inlining_cost
    src
end

function ir_to_codeinf!(opt::OptimizationState, edges::SimpleVector)
    src = ir_to_codeinf!(opt)
    src.edges = edges
    src
end

function ir_to_codeinf!(opt::OptimizationState)
    (; linfo, src, optresult) = opt
    if optresult === nothing
        return src
    end
    src = ir_to_codeinf!(src, optresult.ir)
    opt.optresult = nothing
    opt.src = src
    maybe_validate_code(linfo, src, "optimized")
    return src
end

function ir_to_codeinf!(src::CodeInfo, ir::IRCode)
    replace_code_newstyle!(src, ir)
    widen_all_consts!(src)
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

    return src
end

#########
# logic #
#########

_topmod(sv::OptimizationState) = _topmod(sv.mod)

is_stmt_inline(stmt_flag::UInt32) = has_flag(stmt_flag, IR_FLAG_INLINE)
is_stmt_noinline(stmt_flag::UInt32) = has_flag(stmt_flag, IR_FLAG_NOINLINE)

function new_expr_effect_flags(𝕃ₒ::AbstractLattice, args::Vector{Any}, src::Union{IRCode,IncrementalCompact}, pattern_match=nothing)
    Targ = args[1]
    atyp = argextype(Targ, src)
    # `Expr(:new)` of unknown type could raise arbitrary TypeError.
    typ, isexact = instanceof_tfunc(atyp, true)
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
    for fidx in 1:(length(args) - 1)
        farg = args[fidx + 1]
        eT = argextype(farg, src)
        fT = fieldtype(typ, fidx)
        if !isexact && has_free_typevars(fT)
            if pattern_match !== nothing && pattern_match(src, typ, fidx, Targ, farg)
                continue
            end
            return (false, false, false)
        end
        ⊑(𝕃ₒ, eT, fT) || return (false, false, false)
    end
    return (false, true, true)
end

# Returns a tuple of `(:consistent, :removable, :nothrow)` flags for a given statement.
function stmt_effect_flags(𝕃ₒ::AbstractLattice, @nospecialize(stmt), @nospecialize(rt), src::Union{IRCode,IncrementalCompact})
    # TODO: We're duplicating analysis from inference here.
    isa(stmt, PiNode) && return (true, true, true)
    isa(stmt, PhiNode) && return (true, true, true)
    isa(stmt, ReturnNode) && return (true, false, true)
    isa(stmt, EnterNode) && return (true, false, true)
    isa(stmt, GotoNode) && return (true, false, true)
    isa(stmt, GotoIfNot) && return (true, false, ⊑(𝕃ₒ, argextype(stmt.cond, src), Bool))
    if isa(stmt, GlobalRef)
        # Modeled more precisely in abstract_eval_globalref. In general, if a
        # GlobalRef was moved to statement position, it is probably not `const`,
        # so we can't say much about it anyway.
        return (false, false, false)
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
            if f === Intrinsics.cglobal || f === Intrinsics.llvmcall
                # TODO: these are not yet linearized
                return (false, false, false)
            end
            isa(f, Builtin) || return (false, false, false)
            # Needs to be handled in inlining to look at the callee effects
            f === Core._apply_iterate && return (false, false, false)
            argtypes = Any[argextype(args[arg], src) for arg in 2:length(args)]
            effects = builtin_effects(𝕃ₒ, f, argtypes, rt)
            consistent = is_consistent(effects)
            effect_free = is_effect_free(effects)
            nothrow = is_nothrow(effects)
            terminates = is_terminates(effects)
            removable = effect_free & nothrow & terminates
            return (consistent, removable, nothrow)
        elseif head === :new
            return new_expr_effect_flags(𝕃ₒ, args, src)
        elseif head === :foreigncall
            effects = foreigncall_effects(stmt) do @nospecialize x
                argextype(x, src)
            end
            consistent = is_consistent(effects)
            effect_free = is_effect_free(effects)
            nothrow = is_nothrow(effects)
            terminates = is_terminates(effects)
            removable = effect_free & nothrow & terminates
            return (consistent, removable, nothrow)
        elseif head === :new_opaque_closure
            length(args) < 4 && return (false, false, false)
            typ = argextype(args[1], src)
            typ, isexact = instanceof_tfunc(typ, true)
            isexact || return (false, false, false)
            ⊑(𝕃ₒ, typ, Tuple) || return (false, false, false)
            rt_lb = argextype(args[2], src)
            rt_ub = argextype(args[3], src)
            source = argextype(args[5], src)
            if !(⊑(𝕃ₒ, rt_lb, Type) && ⊑(𝕃ₒ, rt_ub, Type) && ⊑(𝕃ₒ, source, Method))
                return (false, false, false)
            end
            return (false, true, true)
        elseif head === :inbounds
            return (true, true, true)
        elseif head === :boundscheck || head === :isdefined || head === :the_exception || head === :copyast
            return (false, true, true)
        else
            # e.g. :loopinfo
            return (false, false, false)
        end
    end
    isa(stmt, SlotNumber) && error("unexpected IR elements")
    return (true, true, true)
end

function recompute_effects_flags(𝕃ₒ::AbstractLattice, @nospecialize(stmt), @nospecialize(rt),
                                 src::Union{IRCode,IncrementalCompact})
    flag = IR_FLAG_NULL
    (consistent, removable, nothrow) = stmt_effect_flags(𝕃ₒ, stmt, rt, src)
    if consistent
        flag |= IR_FLAG_CONSISTENT
    end
    if removable
        flag |= IR_FLAGS_REMOVABLE
    elseif nothrow
        flag |= IR_FLAG_NOTHROW
    end
    if !iscallstmt(stmt)
        # There is a bit of a subtle point here, which is that some non-call
        # statements (e.g. PiNode) can be UB:, however, we consider it
        # illegal to introduce such statements that actually cause UB (for any
        # input). Ideally that'd be handled at insertion time (TODO), but for
        # the time being just do that here.
        flag |= IR_FLAG_NOUB
    end
    return flag
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
function argextype(@nospecialize(x), src::CodeInfo, sptypes::Vector{VarState})
    return argextype(x, src, sptypes, src.slottypes::Union{Vector{Any},Nothing})
end
function argextype(
    @nospecialize(x), src::Union{IRCode,IncrementalCompact,CodeInfo},
    sptypes::Vector{VarState}, slottypes::Union{Vector{Any},Nothing})
    if isa(x, Expr)
        if x.head === :static_parameter
            idx = x.args[1]::Int
            (1 ≤ idx ≤ length(sptypes)) || throw(InvalidIRError())
            return sptypes[idx].typ
        elseif x.head === :boundscheck
            return Bool
        elseif x.head === :copyast
            length(x.args) == 0 && throw(InvalidIRError())
            return argextype(x.args[1], src, sptypes, slottypes)
        end
        Core.println("argextype called on Expr with head ", x.head,
                     " which is not valid for IR in argument-position.")
        @assert false
    elseif isa(x, SlotNumber)
        slottypes === nothing && return Any
        (1 ≤ x.id ≤ length(slottypes)) || throw(InvalidIRError())
        return slottypes[x.id]
    elseif isa(x, SSAValue)
        return abstract_eval_ssavalue(x, src)
    elseif isa(x, Argument)
        slottypes === nothing && return Any
        (1 ≤ x.n ≤ length(slottypes)) || throw(InvalidIRError())
        return slottypes[x.n]
    elseif isa(x, QuoteNode)
        return Const(x.value)
    elseif isa(x, GlobalRef)
        return abstract_eval_globalref_type(x, src)
    elseif isa(x, PhiNode) || isa(x, PhiCNode) || isa(x, UpsilonNode)
        return Any
    elseif isa(x, PiNode)
        return x.typ
    else
        return Const(x)
    end
end
function abstract_eval_ssavalue(s::SSAValue, src::CodeInfo)
    ssavaluetypes = src.ssavaluetypes
    if ssavaluetypes isa Int
        (1 ≤ s.id ≤ ssavaluetypes) || throw(InvalidIRError())
        return Any
    else
        return abstract_eval_ssavalue(s, ssavaluetypes::Vector{Any})
    end
end
abstract_eval_ssavalue(s::SSAValue, src::Union{IRCode,IncrementalCompact}) = types(src)[s]

"""
    finishopt!(interp::AbstractInterpreter, opt::OptimizationState, ir::IRCode)

Called at the end of optimization to store the resulting IR back into the OptimizationState.
"""
function finishopt!(::AbstractInterpreter, opt::OptimizationState, ir::IRCode)
    opt.optresult = OptimizationResult(ir, ccall(:jl_ir_flag_inlining, UInt8, (Any,), opt.src), false)
    return nothing
end

function visit_bb_phis!(callback, ir::IRCode, bb::Int)
    stmts = ir.cfg.blocks[bb].stmts
    for idx in stmts
        stmt = ir[SSAValue(idx)][:stmt]
        if !isa(stmt, PhiNode)
            if !is_valid_phiblock_stmt(stmt)
                return
            end
        else
            callback(idx)
        end
    end
end

function any_stmt_may_throw(ir::IRCode, bb::Int)
    for idx in ir.cfg.blocks[bb].stmts
        if !has_flag(ir[SSAValue(idx)], IR_FLAG_NOTHROW)
            return true
        end
    end
    return false
end

visit_conditional_successors(callback, ir::IRCode, bb::Int) = # used for test
    visit_conditional_successors(callback, LazyPostDomtree(ir), ir, bb)
function visit_conditional_successors(callback, lazypostdomtree::LazyPostDomtree, ir::IRCode, bb::Int)
    visited = BitSet((bb,))
    worklist = Int[bb]
    while !isempty(worklist)
        thisbb = popfirst!(worklist)
        for succ in ir.cfg.blocks[thisbb].succs
            succ in visited && continue
            push!(visited, succ)
            if postdominates(get!(lazypostdomtree), succ, bb)
                # this successor is not conditional, so no need to visit it further
                continue
            elseif callback(succ)
                return true
            else
                push!(worklist, succ)
            end
        end
    end
    return false
end

struct AugmentedDomtree
    cfg::CFG
    domtree::DomTree
end

mutable struct LazyAugmentedDomtree
    const ir::IRCode
    agdomtree::AugmentedDomtree
    LazyAugmentedDomtree(ir::IRCode) = new(ir)
end

function get!(lazyagdomtree::LazyAugmentedDomtree)
    isdefined(lazyagdomtree, :agdomtree) && return lazyagdomtree.agdomtree
    ir = lazyagdomtree.ir
    cfg = copy(ir.cfg)
    # Add a virtual basic block to represent the exit
    push!(cfg.blocks, BasicBlock(StmtRange(0:-1)))
    for bb = 1:(length(cfg.blocks)-1)
        terminator = ir[SSAValue(last(cfg.blocks[bb].stmts))][:stmt]
        if isa(terminator, ReturnNode) && isdefined(terminator, :val)
            cfg_insert_edge!(cfg, bb, length(cfg.blocks))
        end
    end
    domtree = construct_domtree(cfg)
    return lazyagdomtree.agdomtree = AugmentedDomtree(cfg, domtree)
end

mutable struct PostOptAnalysisState
    const result::InferenceResult
    const ir::IRCode
    const inconsistent::BitSetBoundedMinPrioritySet
    const tpdum::TwoPhaseDefUseMap
    const lazypostdomtree::LazyPostDomtree
    const lazyagdomtree::LazyAugmentedDomtree
    const ea_analysis_pending::Vector{Int}
    all_retpaths_consistent::Bool
    all_effect_free::Bool
    effect_free_if_argmem_only::Union{Nothing,Bool}
    all_nothrow::Bool
    all_noub::Bool
    any_conditional_ub::Bool
    nortcall::Bool
    function PostOptAnalysisState(result::InferenceResult, ir::IRCode)
        inconsistent = BitSetBoundedMinPrioritySet(length(ir.stmts))
        tpdum = TwoPhaseDefUseMap(length(ir.stmts))
        lazypostdomtree = LazyPostDomtree(ir)
        lazyagdomtree = LazyAugmentedDomtree(ir)
        return new(result, ir, inconsistent, tpdum, lazypostdomtree, lazyagdomtree, Int[],
                   true, true, nothing, true, true, false, true)
    end
end

give_up_refinements!(sv::PostOptAnalysisState) =
    sv.all_retpaths_consistent = sv.all_effect_free = sv.effect_free_if_argmem_only =
    sv.all_nothrow = sv.all_noub = sv.nortcall = false

function any_refinable(sv::PostOptAnalysisState)
    effects = sv.result.ipo_effects
    return ((!is_consistent(effects) & sv.all_retpaths_consistent) |
            (!is_effect_free(effects) & sv.all_effect_free) |
            (!is_nothrow(effects) & sv.all_nothrow) |
            (!is_noub(effects) & sv.all_noub) |
            (!is_nortcall(effects) & sv.nortcall))
end

struct GetNativeEscapeCache{CodeCache}
    code_cache::CodeCache
    GetNativeEscapeCache(code_cache::CodeCache) where CodeCache = new{CodeCache}(code_cache)
end
GetNativeEscapeCache(interp::AbstractInterpreter) = GetNativeEscapeCache(code_cache(interp))
function ((; code_cache)::GetNativeEscapeCache)(codeinst::Union{CodeInstance,MethodInstance})
    if codeinst isa MethodInstance
        codeinst = get(code_cache, codeinst, nothing)
        codeinst isa CodeInstance || return false
    end
    argescapes = traverse_analysis_results(codeinst) do @nospecialize result
        return result isa EscapeAnalysis.ArgEscapeCache ? result : nothing
    end
    if argescapes !== nothing
        return argescapes
    end
    effects = decode_effects(codeinst.ipo_purity_bits)
    if is_effect_free(effects) && is_inaccessiblememonly(effects)
        # We might not have run EA on simple frames without any escapes (e.g. when optimization
        # is skipped when result is constant-folded by abstract interpretation). If those
        # frames aren't inlined, the accuracy of EA for caller context takes a big hit.
        # This is a HACK to avoid that, but obviously, a more comprehensive fix would be ideal.
        return true
    end
    return false
end

function refine_effects!(interp::AbstractInterpreter, opt::OptimizationState, sv::PostOptAnalysisState)
    if !is_effect_free(sv.result.ipo_effects) && sv.all_effect_free && !isempty(sv.ea_analysis_pending)
        ir = sv.ir
        nargs = Int(opt.src.nargs)
        estate = EscapeAnalysis.analyze_escapes(ir, nargs, optimizer_lattice(interp), get_escape_cache(interp))
        argescapes = EscapeAnalysis.ArgEscapeCache(estate)
        stack_analysis_result!(sv.result, argescapes)
        validate_mutable_arg_escapes!(estate, sv)
    end

    any_refinable(sv) || return false
    effects = sv.result.ipo_effects
    sv.result.ipo_effects = Effects(effects;
        consistent = sv.all_retpaths_consistent ? ALWAYS_TRUE : effects.consistent,
        effect_free = sv.all_effect_free ? ALWAYS_TRUE :
                      sv.effect_free_if_argmem_only === true ? EFFECT_FREE_IF_INACCESSIBLEMEMONLY : effects.effect_free,
        nothrow = sv.all_nothrow ? true : effects.nothrow,
        noub = sv.all_noub ? (sv.any_conditional_ub ? NOUB_IF_NOINBOUNDS : ALWAYS_TRUE) : effects.noub,
        nortcall = sv.nortcall ? true : effects.nortcall)
    return true
end

function is_ipo_dataflow_analysis_profitable(effects::Effects)
    return !(is_consistent(effects) && is_effect_free(effects) &&
             is_nothrow(effects) && is_noub(effects))
end

function iscall_with_boundscheck(@nospecialize(stmt), sv::PostOptAnalysisState)
    isexpr(stmt, :call) || return false
    ft = argextype(stmt.args[1], sv.ir)
    f = singleton_type(ft)
    f === nothing && return false
    if f === getfield
        nargs = 4
    elseif f === memoryrefnew
        nargs= 3
    elseif f === memoryrefget || f === memoryref_isassigned
        nargs = 4
    elseif f === memoryrefset!
        nargs = 5
    else
        return false
    end
    length(stmt.args) < nargs && return false
    boundscheck = stmt.args[end]
    argextype(boundscheck, sv.ir) === Bool || return false
    isa(boundscheck, SSAValue) || return false
    return true
end

function check_all_args_noescape!(sv::PostOptAnalysisState, ir::IRCode, @nospecialize(stmt),
                                  estate::EscapeAnalysis.EscapeState)
    stmt isa Expr || return false
    if isexpr(stmt, :invoke)
        startidx = 2
    elseif isexpr(stmt, :new)
        startidx = 1
    else
        return false
    end
    has_no_escape(x::EscapeAnalysis.EscapeInfo) =
        EscapeAnalysis.has_no_escape(EscapeAnalysis.ignore_argescape(x))
    for i = startidx:length(stmt.args)
        arg = stmt.args[i]
        argt = argextype(arg, ir)
        if is_mutation_free_argtype(argt)
            continue
        end
        # See if we can find the allocation
        if isa(arg, Argument)
            if has_no_escape(estate[arg])
                # Even if we prove everything else effect_free, the best we can
                # say is :effect_free_if_argmem_only
                if sv.effect_free_if_argmem_only === nothing
                    sv.effect_free_if_argmem_only = true
                end
            else
                sv.effect_free_if_argmem_only = false
            end
            return false
        elseif isa(arg, SSAValue)
            has_no_escape(estate[arg]) || return false
            check_all_args_noescape!(sv, ir, ir[arg][:stmt], estate) || return false
        else
            return false
        end
    end
    return true
end

function validate_mutable_arg_escapes!(estate::EscapeAnalysis.EscapeState, sv::PostOptAnalysisState)
    ir = sv.ir
    for idx in sv.ea_analysis_pending
        # See if any mutable memory was allocated in this function and determined
        # not to escape.
        inst = ir[SSAValue(idx)]
        stmt = inst[:stmt]
        if !check_all_args_noescape!(sv, ir, stmt, estate)
            return sv.all_effect_free = false
        end
    end
    return true
end

function is_conditional_noub(inst::Instruction, sv::PostOptAnalysisState)
    stmt = inst[:stmt]
    iscall_with_boundscheck(stmt, sv) || return false
    barg = stmt.args[end]::SSAValue
    bstmt = sv.ir[barg][:stmt]
    isexpr(bstmt, :boundscheck) || return false
    # If IR_FLAG_INBOUNDS is already set, no more conditional ub
    (!isempty(bstmt.args) && bstmt.args[1] === false) && return false
    return true
end

function scan_non_dataflow_flags!(inst::Instruction, sv::PostOptAnalysisState)
    flag = inst[:flag]
    # If we can prove that the argmem does not escape the current function, we can
    # refine this to :effect_free.
    needs_ea_validation = has_flag(flag, IR_FLAGS_NEEDS_EA)
    stmt = inst[:stmt]
    if !needs_ea_validation
        if !isterminator(stmt) && stmt !== nothing
            # ignore control flow node – they are not removable on their own and thus not
            # have `IR_FLAG_EFFECT_FREE` but still do not taint `:effect_free`-ness of
            # the whole method invocation
            sv.all_effect_free &= has_flag(flag, IR_FLAG_EFFECT_FREE)
        end
    elseif sv.all_effect_free
        if (isexpr(stmt, :invoke) || isexpr(stmt, :new) ||
            # HACK for performance: limit the scope of EA to code with object field access only,
            # since its abilities to reason about e.g. arrays are currently very limited anyways.
            is_known_call(stmt, setfield!, sv.ir))
            push!(sv.ea_analysis_pending, inst.idx)
        else
            sv.all_effect_free = false
        end
    end
    sv.all_nothrow &= has_flag(flag, IR_FLAG_NOTHROW)
    if !has_flag(flag, IR_FLAG_NOUB)
        # Special case: `:boundscheck` into `getfield` or memory operations is `:noub_if_noinbounds`
        if is_conditional_noub(inst, sv)
            sv.any_conditional_ub = true
        else
            sv.all_noub = false
        end
    end
    if !has_flag(flag, IR_FLAG_NORTCALL)
        # if a function call that might invoke `Core.Compiler.return_type` has been deleted,
        # there's no need to taint with `:nortcall`, allowing concrete evaluation
        if iscallstmt(stmt)
            sv.nortcall = false
        end
    end
    nothing
end

function scan_inconsistency!(inst::Instruction, sv::PostOptAnalysisState)
    flag = inst[:flag]
    stmt_inconsistent = !has_flag(flag, IR_FLAG_CONSISTENT)
    stmt = inst[:stmt]
    # Special case: For `getfield` and memory operations, we allow inconsistency of the :boundscheck argument
    (; inconsistent, tpdum) = sv
    if iscall_with_boundscheck(stmt, sv)
        for i = 1:(length(stmt.args)-1)
            val = stmt.args[i]
            if isa(val, SSAValue)
                stmt_inconsistent |= val.id in inconsistent
                count!(tpdum, val)
            end
        end
    else
        for ur in userefs(stmt)
            val = ur[]
            if isa(val, SSAValue)
                stmt_inconsistent |= val.id in inconsistent
                count!(tpdum, val)
            end
        end
    end
    stmt_inconsistent && push!(inconsistent, inst.idx)
    return stmt_inconsistent
end

struct ScanStmt
    sv::PostOptAnalysisState
end

function ((; sv)::ScanStmt)(inst::Instruction, lstmt::Int, bb::Int)
    stmt = inst[:stmt]

    if isa(stmt, EnterNode)
        # try/catch not yet modeled
        give_up_refinements!(sv)
        return nothing
    end

    scan_non_dataflow_flags!(inst, sv)

    stmt_inconsistent = scan_inconsistency!(inst, sv)

    if stmt_inconsistent
        if !has_flag(inst[:flag], IR_FLAG_NOTHROW)
            # Taint :consistent if this statement may raise since :consistent requires
            # consistent termination. TODO: Separate :consistent_return and :consistent_termination from :consistent.
            sv.all_retpaths_consistent = false
        end
        if inst.idx == lstmt
            if isa(stmt, ReturnNode) && isdefined(stmt, :val)
                sv.all_retpaths_consistent = false
            elseif isa(stmt, GotoIfNot)
                # Conditional Branch with inconsistent condition.
                # If we do not know this function terminates, taint consistency, now,
                # :consistent requires consistent termination. TODO: Just look at the
                # inconsistent region.
                if !sv.result.ipo_effects.terminates
                    sv.all_retpaths_consistent = false
                elseif visit_conditional_successors(sv.lazypostdomtree, sv.ir, bb) do succ::Int
                        return any_stmt_may_throw(sv.ir, succ)
                    end
                    # check if this `GotoIfNot` leads to conditional throws, which taints consistency
                    sv.all_retpaths_consistent = false
                else
                    (; cfg, domtree) = get!(sv.lazyagdomtree)
                    for succ in iterated_dominance_frontier(cfg, BlockLiveness(sv.ir.cfg.blocks[bb].succs, nothing), domtree)
                        if succ == length(cfg.blocks)
                            # Phi node in the virtual exit -> We have a conditional
                            # return. TODO: Check if all the retvals are egal.
                            sv.all_retpaths_consistent = false
                        else
                            visit_bb_phis!(sv.ir, succ) do phiidx::Int
                                push!(sv.inconsistent, phiidx)
                            end
                        end
                    end
                end
            end
        end
    end

    # bail out early if there are no possibilities to refine the effects
    if !any_refinable(sv)
        return nothing
    end

    return true
end

function check_inconsistentcy!(sv::PostOptAnalysisState, scanner::BBScanner)
    (; ir, inconsistent, tpdum) = sv

    scan!(ScanStmt(sv), scanner, false)
    complete!(tpdum); push!(scanner.bb_ip, 1)
    populate_def_use_map!(tpdum, scanner)

    stmt_ip = BitSetBoundedMinPrioritySet(length(ir.stmts))
    for def in inconsistent
        for use in tpdum[def]
            if !(use in inconsistent)
                push!(inconsistent, use)
                append!(stmt_ip, tpdum[use])
            end
        end
    end
    lazydomtree = LazyDomtree(ir)
    while !isempty(stmt_ip)
        idx = popfirst!(stmt_ip)
        inst = ir[SSAValue(idx)]
        stmt = inst[:stmt]
        if iscall_with_boundscheck(stmt, sv)
            any_non_boundscheck_inconsistent = false
            for i = 1:(length(stmt.args)-1)
                val = stmt.args[i]
                if isa(val, SSAValue)
                    any_non_boundscheck_inconsistent |= val.id in inconsistent
                    any_non_boundscheck_inconsistent && break
                end
            end
            any_non_boundscheck_inconsistent || continue
        elseif isa(stmt, ReturnNode)
            sv.all_retpaths_consistent = false
        elseif isa(stmt, GotoIfNot)
            bb = block_for_inst(ir, idx)
            cfg = ir.cfg
            blockliveness = BlockLiveness(cfg.blocks[bb].succs, nothing)
            for succ in iterated_dominance_frontier(cfg, blockliveness, get!(lazydomtree))
                visit_bb_phis!(ir, succ) do phiidx::Int
                    push!(inconsistent, phiidx)
                    push!(stmt_ip, phiidx)
                end
            end
        end
        sv.all_retpaths_consistent || break
        append!(inconsistent, tpdum[idx])
        append!(stmt_ip, tpdum[idx])
    end
end

function ipo_dataflow_analysis!(interp::AbstractInterpreter, opt::OptimizationState,
                                ir::IRCode, result::InferenceResult)
    if !is_ipo_dataflow_analysis_profitable(result.ipo_effects)
        return false
    end

    @assert isempty(ir.new_nodes) "IRCode should be compacted before post-opt analysis"

    sv = PostOptAnalysisState(result, ir)
    scanner = BBScanner(ir)

    completed_scan = scan!(ScanStmt(sv), scanner, true)

    if !completed_scan
        if sv.all_retpaths_consistent
            check_inconsistentcy!(sv, scanner)
        else
            # No longer any dataflow concerns, just scan the flags
            scan!(scanner, false) do inst::Instruction, ::Int, ::Int
                scan_non_dataflow_flags!(inst, sv)
                # bail out early if there are no possibilities to refine the effects
                if !any_refinable(sv)
                    return nothing
                end
                return true
            end
        end
    end

    return refine_effects!(interp, opt, sv)
end

# run the optimization work
function optimize(interp::AbstractInterpreter, opt::OptimizationState, caller::InferenceResult)
    @zone "CC: OPTIMIZER" ir = run_passes_ipo_safe(opt.src, opt)
    ipo_dataflow_analysis!(interp, opt, ir, caller)
    finishopt!(interp, opt, ir)
    return nothing
end

const ALL_PASS_NAMES = String[]
macro pass(name::String, expr)
    optimize_until = esc(:optimize_until)
    stage = esc(:__stage__)
    macrocall = :(@zone $name $(esc(expr)))
    macrocall.args[2] = __source__  # `@timeit` may want to use it
    push!(ALL_PASS_NAMES, name)
    quote
        $macrocall
        matchpass($optimize_until, ($stage += 1), $name) && $(esc(:(@goto __done__)))
    end
end

matchpass(optimize_until::Int, stage, _) = optimize_until == stage
matchpass(optimize_until::String, _, name) = optimize_until == name
matchpass(::Nothing, _, _) = false

function run_passes_ipo_safe(
    ci::CodeInfo,
    sv::OptimizationState,
    optimize_until::Union{Nothing, Int, String} = nothing)  # run all passes by default
    if optimize_until isa String && !contains_is(ALL_PASS_NAMES, optimize_until)
        error("invalid `optimize_until` argument, no such optimization pass")
    elseif optimize_until isa Int && (optimize_until < 1 || optimize_until > length(ALL_PASS_NAMES))
        error("invalid `optimize_until` argument, no such optimization pass")
    end

    __stage__ = 0  # used by @pass
    # NOTE: The pass name MUST be unique for `optimize_until::String` to work
    @pass "CC: CONVERT"   ir = convert_to_ircode(ci, sv)
    @pass "CC: SLOT2REG"  ir = slot2reg(ir, ci, sv)
    # TODO: Domsorting can produce an updated domtree - no need to recompute here
    @pass "CC: COMPACT_1" ir = compact!(ir)
    @pass "CC: INLINING"  ir = ssa_inlining_pass!(ir, sv.inlining, ci.propagate_inbounds)
    # @zone "CC: VERIFY 2" verify_ir(ir)
    @pass "CC: COMPACT_2" ir = compact!(ir)
    @pass "CC: SROA"      ir = sroa_pass!(ir, sv.inlining)
    @pass "CC: ADCE"      (ir, made_changes) = adce_pass!(ir, sv.inlining)
    if made_changes
        @pass "CC: COMPACT_3" ir = compact!(ir, true)
    end
    if is_asserts()
        @zone "CC: VERIFY_3" begin
            verify_ir(ir, true, false, optimizer_lattice(sv.inlining.interp), sv.linfo)
            verify_linetable(ir.debuginfo, length(ir.stmts))
        end
    end
    @label __done__  # used by @pass
    return ir
end

function strip_trailing_junk!(code::Vector{Any}, ssavaluetypes::Vector{Any}, ssaflags::Vector, debuginfo::DebugInfoStream, cfg::CFG, info::Vector{CallInfo})
    # Remove `nothing`s at the end, we don't handle them well
    # (we expect the last instruction to be a terminator)
    codelocs = debuginfo.codelocs
    for i = length(code):-1:1
        if code[i] !== nothing
            resize!(code, i)
            resize!(ssavaluetypes, i)
            resize!(codelocs, 3i)
            resize!(info, i)
            resize!(ssaflags, i)
            break
        end
    end
    # If the last instruction is not a terminator, add one. This can
    # happen for implicit return on dead branches.
    term = code[end]
    if !isa(term, GotoIfNot) && !isa(term, GotoNode) && !isa(term, ReturnNode)
        push!(code, ReturnNode())
        push!(ssavaluetypes, Union{})
        push!(codelocs, 0, 0, 0)
        push!(info, NoCallInfo())
        push!(ssaflags, IR_FLAG_NOTHROW)

        # Update CFG to include appended terminator
        old_range = cfg.blocks[end].stmts
        new_range = StmtRange(first(old_range), last(old_range) + 1)
        cfg.blocks[end] = BasicBlock(cfg.blocks[end], new_range)
        (length(cfg.index) == length(cfg.blocks)) && (cfg.index[end] += 1)
    end
    nothing
end

function changed_lineinfo(di::DebugInfo, codeloc::Int, prevloc::Int)
    while true
        next = getdebugidx(di, codeloc)
        line = next[1]
        line < 0 && return false # invalid info
        line == 0 && next[2] == 0 && return false # no new info
        prevloc <= 0 && return true # no old info
        prev = getdebugidx(di, prevloc)
        next === prev && return false # exactly identical
        prevline = prev[1]
        prevline < 0 && return true # previous invalid info, now valid
        edge = next[2]
        edge === prev[2] || return true # change to this edge
        linetable = di.linetable
        # check for change to line number here
        if linetable === nothing || line == 0
            line == prevline || return true
        else
            changed_lineinfo(linetable::DebugInfo, Int(line), Int(prevline)) && return true
        end
        # check for change to edge here
        edge == 0 && return false # no edge here
        di = di.edges[Int(edge)]::DebugInfo
        codeloc = Int(next[3])
        prevloc = Int(prev[3])
    end
end

function convert_to_ircode(ci::CodeInfo, sv::OptimizationState)
    # Update control-flow to reflect any unreachable branches.
    ssavaluetypes = ci.ssavaluetypes::Vector{Any}
    ci.code = code = copy_exprargs(ci.code)
    di = DebugInfoStream(sv.linfo, ci.debuginfo, length(code))
    codelocs = di.codelocs
    ssaflags = ci.ssaflags
    for i = 1:length(code)
        expr = code[i]
        if !(i in sv.unreachable)
            if isa(expr, GotoIfNot)
                # Replace this live GotoIfNot with:
                # - no-op if :nothrow and the branch target is unreachable
                # - cond if :nothrow and both targets are unreachable
                # - typeassert if must-throw
                block = block_for_inst(sv.cfg, i)
                if ssavaluetypes[i] === Bottom
                    destblock = block_for_inst(sv.cfg, expr.dest)
                    cfg_delete_edge!(sv.cfg, block, block + 1)
                    ((block + 1) != destblock) && cfg_delete_edge!(sv.cfg, block, destblock)
                    expr = Expr(:call, Core.typeassert, expr.cond, Bool)
                elseif i + 1 in sv.unreachable
                    @assert has_flag(ssaflags[i], IR_FLAG_NOTHROW)
                    cfg_delete_edge!(sv.cfg, block, block + 1)
                    expr = GotoNode(expr.dest)
                elseif expr.dest in sv.unreachable
                    @assert has_flag(ssaflags[i], IR_FLAG_NOTHROW)
                    cfg_delete_edge!(sv.cfg, block, block_for_inst(sv.cfg, expr.dest))
                    expr = nothing
                end
                code[i] = expr
            elseif isa(expr, EnterNode)
                catchdest = expr.catch_dest
                if catchdest in sv.unreachable
                    cfg_delete_edge!(sv.cfg, block_for_inst(sv.cfg, i), block_for_inst(sv.cfg, catchdest))
                    if isdefined(expr, :scope)
                        # We've proven that nothing inside the enter region throws,
                        # but we don't yet know whether something might read the scope,
                        # so we need to retain this enter for the time being. However,
                        # we use the special marker `0` to indicate that setting up
                        # the try/catch frame is not required.
                        code[i] = EnterNode(expr, 0)
                    else
                        code[i] = nothing
                    end
                end
            elseif isa(expr, PhiNode)
                new_edges = Int32[]
                new_vals = Any[]
                for j = 1:length(expr.edges)
                    edge = expr.edges[j]
                    (edge in sv.unreachable || (ssavaluetypes[edge] === Union{} && !isa(code[edge], PhiNode))) && continue
                    push!(new_edges, edge)
                    if isassigned(expr.values, j)
                        push!(new_vals, expr.values[j])
                    else
                        resize!(new_vals, length(new_edges))
                    end
                end
                code[i] = PhiNode(new_edges, new_vals)
            end
        end
    end

    # Go through and add an unreachable node after every
    # Union{} call. Then reindex labels.
    stmtinfo = sv.stmt_info
    meta = Expr[]
    idx = 1
    oldidx = 1
    nstmts = length(code)
    ssachangemap = labelchangemap = blockchangemap = nothing
    prevloc = 0
    while idx <= length(code)
        if sv.insert_coverage && changed_lineinfo(ci.debuginfo, oldidx, prevloc)
            # insert a side-effect instruction before the current instruction in the same basic block
            insert!(code, idx, Expr(:code_coverage_effect))
            splice!(codelocs, 3idx-2:3idx-3, (codelocs[3idx-2], codelocs[3idx-1], codelocs[3idx-0]))
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
            if blockchangemap === nothing
                blockchangemap = fill(0, length(sv.cfg.blocks))
            end
            blockchangemap[block_for_inst(sv.cfg, oldidx)] += 1
            idx += 1
            prevloc = oldidx
        end
        if ssavaluetypes[idx] === Union{} && !(oldidx in sv.unreachable) && !isa(code[idx], PhiNode)
            # We should have converted any must-throw terminators to an equivalent w/o control-flow edges
            @assert !isterminator(code[idx])

            block = block_for_inst(sv.cfg, oldidx)
            block_end = last(sv.cfg.blocks[block].stmts) + (idx - oldidx)

            # Delete all successors to this basic block
            for succ in sv.cfg.blocks[block].succs
                preds = sv.cfg.blocks[succ].preds
                deleteat!(preds, findfirst(x::Int->x==block, preds)::Int)
            end
            empty!(sv.cfg.blocks[block].succs)

            if !(idx < length(code) && isa(code[idx + 1], ReturnNode) && !isdefined((code[idx + 1]::ReturnNode), :val))
                # Any statements from here to the end of the block have been wrapped in Core.Const(...)
                # by type inference (effectively deleting them). Only task left is to replace the block
                # terminator with an explicit `unreachable` marker.

                if block_end > idx
                    if is_asserts()
                        # Verify that type-inference did its job
                        for i = (oldidx + 1):last(sv.cfg.blocks[block].stmts)
                            @assert i in sv.unreachable
                        end
                    end
                    code[block_end] = ReturnNode()
                    codelocs[3block_end-2], codelocs[3block_end-1], codelocs[3block_end-0] = (codelocs[3idx-2], codelocs[3idx-1], codelocs[3idx-0])
                    ssavaluetypes[block_end] = Union{}
                    stmtinfo[block_end] = NoCallInfo()
                    ssaflags[block_end] = IR_FLAG_NOTHROW
                    idx += block_end - idx
                else
                    insert!(code, idx + 1, ReturnNode())
                    splice!(codelocs, 3idx-2:3idx-3, (codelocs[3idx-2], codelocs[3idx-1], codelocs[3idx-0]))
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
                    if blockchangemap === nothing
                        blockchangemap = fill(0, length(sv.cfg.blocks))
                    end
                    blockchangemap[block] += 1
                    idx += 1
                end
                oldidx = last(sv.cfg.blocks[block].stmts)
            end
        end
        idx += 1
        oldidx += 1
    end
    empty!(sv.unreachable)

    if ssachangemap !== nothing && labelchangemap !== nothing
        renumber_ir_elements!(code, ssachangemap, labelchangemap)
    end
    if blockchangemap !== nothing
        renumber_cfg_stmts!(sv.cfg, blockchangemap)
    end

    for i = 1:length(code)
        code[i] = process_meta!(meta, code[i])
    end
    strip_trailing_junk!(code, ssavaluetypes, ssaflags, di, sv.cfg, stmtinfo)
    types = Any[]
    stmts = InstructionStream(code, types, stmtinfo, codelocs, ssaflags)
    # NOTE this `argtypes` contains types of slots yet: it will be modified to contain the
    # types of call arguments only once `slot2reg` converts this `IRCode` to the SSA form
    # and eliminates slots (see below)
    argtypes = sv.slottypes
    return IRCode(stmts, sv.cfg, di, argtypes, meta, sv.sptypes, world_range(ci))
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
    @zone "CC: DOMTREE_1" domtree = construct_domtree(ir)
    defuse_insts = scan_slot_def_use(Int(ci.nargs), ci, ir.stmts.stmt)
    𝕃ₒ = optimizer_lattice(sv.inlining.interp)
    @zone "CC: CONSTRUCT_SSA" ir = construct_ssa!(ci, ir, sv, domtree, defuse_insts, 𝕃ₒ) # consumes `ir`
    # NOTE now we have converted `ir` to the SSA form and eliminated slots
    # let's resize `argtypes` now and remove unnecessary types for the eliminated slots
    resize!(ir.argtypes, ci.nargs)
    return ir
end

## Computing the cost of a function body

# saturating sum (inputs are non-negative), prevents overflow with typemax(Int) below
plus_saturate(x::Int, y::Int) = max(x, y, x+y)

# known return type
isknowntype(@nospecialize T) = (T === Union{}) || isa(T, Const) || isconcretetype(widenconst(T))

function statement_cost(ex::Expr, line::Int, src::Union{CodeInfo, IRCode}, sptypes::Vector{VarState},
                        params::OptimizationParams)
    #=const=# UNKNOWN_CALL_COST = 20
    head = ex.head
    if is_meta_expr_head(head)
        return 0
    elseif head === :call
        farg = ex.args[1]
        ftyp = argextype(farg, src, sptypes)
        if ftyp === IntrinsicFunction && farg isa SSAValue
            # if this comes from code that was already inlined into another function,
            # Consts have been widened. try to recover in simple cases.
            farg = isa(src, CodeInfo) ? src.code[farg.id] : src[farg][:stmt]
            if isa(farg, GlobalRef) || isa(farg, QuoteNode) || isa(farg, IntrinsicFunction) || isexpr(farg, :static_parameter)
                ftyp = argextype(farg, src, sptypes)
            end
        end
        f = singleton_type(ftyp)
        if isa(f, IntrinsicFunction)
            iidx = Int(reinterpret(Int32, f::IntrinsicFunction)) + 1
            if isassigned(T_IFUNC, iidx)
                minarg, maxarg, = T_IFUNC[iidx]
                nargs = length(ex.args)
                if minarg + 1 <= nargs <= maxarg + 1
                    # With mostly constant arguments, all Intrinsics tend to become very cheap
                    # and are likely to combine with the operations around them,
                    # so reduce their cost by half.
                    cost = T_IFUNC_COST[iidx]
                    if cost == 0 || nargs < 3 ||
                       (f === Intrinsics.cglobal || f === Intrinsics.llvmcall) # these hold malformed IR, so argextype will crash on them
                        return cost
                    end
                    aty2 = widenconditional(argextype(ex.args[2], src, sptypes))
                    nconst = Int(aty2 isa Const)
                    for i = 3:nargs
                        aty = widenconditional(argextype(ex.args[i], src, sptypes))
                        if widenconst(aty) != widenconst(aty2)
                            nconst = 0
                            break
                        end
                        nconst += aty isa Const
                    end
                    if nconst + 2 >= nargs
                        cost = (cost - 1) ÷ 2
                    end
                    return cost
                end
            end
            # unknown/unhandled intrinsic: hopefully the caller gets a slightly better answer after the inlining
            return UNKNOWN_CALL_COST
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
            elseif (f === Core.memoryrefget || f === Core.memoryref_isassigned) && length(ex.args) >= 3
                atyp = argextype(ex.args[2], src, sptypes)
                return isknowntype(atyp) ? 1 : params.inline_nonleaf_penalty
            elseif f === Core.memoryrefset! && length(ex.args) >= 3
                atyp = argextype(ex.args[2], src, sptypes)
                return isknowntype(atyp) ? 5 : params.inline_nonleaf_penalty
            elseif f === typeassert && isconstType(widenconst(argextype(ex.args[3], src, sptypes)))
                return 1
            end
            fidx = find_tfunc(f)
            if fidx === nothing
                # unknown/unhandled builtin
                # Use the generic cost of a direct function call
                return UNKNOWN_CALL_COST
            end
            return T_FFUNC_COST[fidx]
        end
        extyp = line == -1 ? Any : argextype(SSAValue(line), src, sptypes)
        if extyp === Union{}
            return 0
        end
        return params.inline_nonleaf_penalty
    elseif head === :foreigncall
        foreigncall = ex.args[1]
        if isexpr(foreigncall, :tuple, 1)
            foreigncall = foreigncall.args[1]
            if foreigncall isa QuoteNode && foreigncall.value === :jl_string_ptr
                return 1
            end
        end
        return 20
    elseif head === :invoke || head === :invoke_modify
        # Calls whose "return type" is Union{} do not actually return:
        # they are errors. Since these are not part of the typical
        # run-time of the function, we omit them from
        # consideration. This way, non-inlined error branches do not
        # prevent inlining.
        extyp = line == -1 ? Any : argextype(SSAValue(line), src, sptypes)
        return extyp === Union{} ? 0 : UNKNOWN_CALL_COST
    elseif head === :(=)
        return statement_cost(ex.args[2], -1, src, sptypes, params)
    elseif head === :copyast
        return 100
    end
    return 0
end

function statement_or_branch_cost(@nospecialize(stmt), line::Int, src::Union{CodeInfo, IRCode}, sptypes::Vector{VarState},
                                  params::OptimizationParams)
    thiscost = 0
    dst(tgt) = isa(src, IRCode) ? first(src.cfg.blocks[tgt].stmts) : tgt
    if stmt isa Expr
        thiscost = statement_cost(stmt, line, src, sptypes, params)::Int
    elseif stmt isa GotoNode
        # loops are generally always expensive
        # but assume that forward jumps are already counted for from
        # summing the cost of the not-taken branch
        thiscost = dst(stmt.label) < line ? 40 : 0
    elseif stmt isa GotoIfNot
        thiscost = dst(stmt.dest) < line ? 40 : 0
    elseif stmt isa EnterNode
        # try/catch is a couple function calls,
        # but don't inline functions with try/catch
        # since these aren't usually performance-sensitive functions,
        # and llvm is more likely to miscompile them when these functions get large
        thiscost = typemax(Int)
    end
    return thiscost
end

function inline_cost_model(ir::IRCode, params::OptimizationParams, cost_threshold::Int)
    bodycost = 0
    for i = 1:length(ir.stmts)
        stmt = ir[SSAValue(i)][:stmt]
        thiscost = statement_or_branch_cost(stmt, i, ir, ir.sptypes, params)
        bodycost = plus_saturate(bodycost, thiscost)
        if bodycost > cost_threshold
            return MAX_INLINE_COST
        end
    end
    return inline_cost_clamp(bodycost)
end

function statement_costs!(cost::Vector{Int}, body::Vector{Any}, src::Union{CodeInfo, IRCode}, sptypes::Vector{VarState}, params::OptimizationParams)
    maxcost = 0
    for line = 1:length(body)
        stmt = body[line]
        thiscost = statement_or_branch_cost(stmt, line, src, sptypes,
                                            params)
        cost[line] = thiscost
        if thiscost > maxcost
            maxcost = thiscost
        end
    end
    return maxcost
end

function renumber_ir_elements!(body::Vector{Any}, cfg::Union{CFG,Nothing}, ssachangemap::Vector{Int})
    return renumber_ir_elements!(body, cfg, ssachangemap, ssachangemap)
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
        elseif isa(el, EnterNode)
            tgt = el.catch_dest
            if tgt != 0
                was_deleted = labelchangemap[tgt] == typemin(Int)
                if was_deleted
                    @assert !isdefined(el, :scope)
                    body[i] = nothing
                else
                    if isdefined(el, :scope) && isa(el.scope, SSAValue)
                        body[i] = EnterNode(tgt + labelchangemap[tgt], SSAValue(el.scope.id + ssachangemap[el.scope.id]))
                    else
                        body[i] = EnterNode(el, tgt + labelchangemap[tgt])
                    end
                end
            end
        elseif isa(el, Expr)
            if el.head === :(=) && el.args[2] isa Expr
                el = el.args[2]::Expr
            end
            if !is_meta_expr_head(el.head)
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

function renumber_cfg_stmts!(cfg::CFG, blockchangemap::Vector{Int})
    cumsum_ssamap!(blockchangemap) || return
    for i = 1:length(cfg.blocks)
        old_range = cfg.blocks[i].stmts
        new_range = StmtRange(first(old_range) + ((i > 1) ? blockchangemap[i - 1] : 0),
                              last(old_range) + blockchangemap[i])
        cfg.blocks[i] = BasicBlock(cfg.blocks[i], new_range)
        if i <= length(cfg.index)
            cfg.index[i] = cfg.index[i] + blockchangemap[i]
        end
    end
end
