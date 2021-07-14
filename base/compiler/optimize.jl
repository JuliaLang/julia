# This file is a part of Julia. License is MIT: https://julialang.org/license

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

push!(et::EdgeTracker, mi::MethodInstance) = push!(et.edges, mi)
function push!(et::EdgeTracker, ci::CodeInstance)
    intersect!(et, WorldRange(min_world(li), max_world(li)))
    push!(et, ci.def)
end

struct InliningState{S <: Union{EdgeTracker, Nothing}, T, P}
    params::OptimizationParams
    et::S
    mi_cache::T
    policy::P
end

function default_inlining_policy(@nospecialize(src))
    if isa(src, CodeInfo) || isa(src, Vector{UInt8})
        src_inferred = ccall(:jl_ir_flag_inferred, Bool, (Any,), src)
        src_inlineable = ccall(:jl_ir_flag_inlineable, Bool, (Any,), src)
        return src_inferred && src_inlineable ? src : nothing
    end
    if isa(src, OptimizationState) && isdefined(src, :ir)
        return src.src.inlineable ? src.ir : nothing
    end
    return nothing
end

include("compiler/ssair/driver.jl")

mutable struct OptimizationState
    linfo::MethodInstance
    src::CodeInfo
    ir::Union{Nothing, IRCode}
    stmt_info::Vector{Any}
    mod::Module
    nargs::Int
    sptypes::Vector{Any} # static parameters
    slottypes::Vector{Any}
    const_api::Bool
    inlining::InliningState
    function OptimizationState(frame::InferenceState, params::OptimizationParams, interp::AbstractInterpreter)
        s_edges = frame.stmt_edges[1]::Vector{Any}
        inlining = InliningState(params,
            EdgeTracker(s_edges, frame.valid_worlds),
            WorldView(code_cache(interp), frame.world),
            inlining_policy(interp))
        return new(frame.linfo,
                   frame.src, nothing, frame.stmt_info, frame.mod, frame.nargs,
                   frame.sptypes, frame.slottypes, false,
                   inlining)
    end
    function OptimizationState(linfo::MethodInstance, src::CodeInfo, params::OptimizationParams, interp::AbstractInterpreter)
        # prepare src for running optimization passes
        # if it isn't already
        nssavalues = src.ssavaluetypes
        if nssavalues isa Int
            src.ssavaluetypes = Any[ Any for i = 1:nssavalues ]
        end
        nslots = length(src.slotflags)
        slottypes = src.slottypes
        if slottypes === nothing
            slottypes = Any[ Any for i = 1:nslots ]
        end
        stmt_info = Any[nothing for i = 1:nssavalues]
        # cache some useful state computations
        def = linfo.def
        mod, nargs = isa(def, Method) ? (def.module, def.nargs) : (def, 0)
        # Allow using the global MI cache, but don't track edges.
        # This method is mostly used for unit testing the optimizer
        inlining = InliningState(params,
            nothing,
            WorldView(code_cache(interp), get_world_counter()),
            inlining_policy(interp))
        return new(linfo,
                   src, nothing, stmt_info, mod, nargs,
                   sptypes_from_meth_instance(linfo), slottypes, false,
                   inlining)
        end
end

function OptimizationState(linfo::MethodInstance, params::OptimizationParams, interp::AbstractInterpreter)
    src = retrieve_code_info(linfo)
    src === nothing && return nothing
    return OptimizationState(linfo, src, params, interp)
end

function ir_to_codeinf!(opt::OptimizationState)
    replace_code_newstyle!(opt.src, opt.ir::IRCode, opt.nargs - 1)
    opt.ir = nothing
    let src = opt.src::CodeInfo
        widen_all_consts!(src)
        src.inferred = true
        # finish updating the result struct
        validate_code_in_debug_mode(opt.linfo, src, "optimized")
        return src
    end
end

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

# This statement was marked as @inbounds by the user. If replaced by inlining,
# any contained boundschecks may be removed
const IR_FLAG_INBOUNDS       = 0x01
# This statement may be removed if its result is unused. In particular it must
# thus be both pure and effect free.
const IR_FLAG_EFFECT_FREE    = 0x01 << 4

# known to be always effect-free (in particular nothrow)
const _PURE_BUILTINS = Any[tuple, svec, ===, typeof, nfields]

# known to be effect-free if the are nothrow
const _PURE_OR_ERROR_BUILTINS = [
    fieldtype, apply_type, isa, UnionAll,
    getfield, arrayref, const_arrayref, isdefined, Core.sizeof,
    Core.kwfunc, ifelse, Core._typevar, (<:)
]

const TOP_TUPLE = GlobalRef(Core, :tuple)

#########
# logic #
#########

_topmod(sv::OptimizationState) = _topmod(sv.mod)

function isinlineable(m::Method, me::OptimizationState, params::OptimizationParams, union_penalties::Bool, bonus::Int=0)
    # compute the cost (size) of inlining this code
    inlineable = false
    cost_threshold = params.inline_cost_threshold
    if m.module === _topmod(m.module)
        # a few functions get special treatment
        name = m.name
        sig = m.sig
        if ((name === :+ || name === :* || name === :min || name === :max) &&
            isa(sig,DataType) &&
            sig == Tuple{sig.parameters[1],Any,Any,Any,Vararg{Any}})
            inlineable = true
        elseif (name === :iterate || name === :unsafe_convert ||
                name === :cconvert)
            cost_threshold *= 4
        end
    end
    if !inlineable
        inlineable = inline_worthy(me.ir::IRCode, params, union_penalties, cost_threshold + bonus)
    end
    return inlineable
end

# These affect control flow within the function (so may not be removed
# if there is no usage within the function), but don't affect the purity
# of the function as a whole.
function stmt_affects_purity(@nospecialize(stmt), ir)
    if isa(stmt, GotoNode) || isa(stmt, ReturnNode)
        return false
    end
    if isa(stmt, GotoIfNot)
        t = argextype(stmt.cond, ir, ir.sptypes)
        return !(t ⊑ Bool)
    end
    if isa(stmt, Expr)
        return stmt.head !== :loopinfo && stmt.head !== :enter
    end
    return true
end

# Convert IRCode back to CodeInfo and compute inlining cost and sideeffects
function finish(interp::AbstractInterpreter, opt::OptimizationState, params::OptimizationParams, ir::IRCode, @nospecialize(result))
    (; def) = linfo = opt.linfo
    nargs = Int(opt.nargs) - 1

    force_noinline = _any(@nospecialize(x) -> isexpr(x, :meta) && x.args[1] === :noinline, ir.meta)

    # compute inlining and other related optimizations
    if (isa(result, Const) || isconstType(result))
        proven_pure = false
        # must be proven pure to use const_api; otherwise we might skip throwing errors
        # (issue #20704)
        # TODO: Improve this analysis; if a function is marked @pure we should really
        # only care about certain errors (e.g. method errors and type errors).
        if length(ir.stmts) < 10
            proven_pure = true
            for i in 1:length(ir.stmts)
                node = ir.stmts[i]
                stmt = node[:inst]
                if stmt_affects_purity(stmt, ir) && !stmt_effect_free(stmt, node[:type], ir, ir.sptypes)
                    proven_pure = false
                    break
                end
            end
            if proven_pure
                for fl in opt.src.slotflags
                    if (fl & SLOT_USEDUNDEF) != 0
                        proven_pure = false
                        break
                    end
                end
            end
        end
        if proven_pure
            opt.src.pure = true
        end

        if proven_pure
            # use constant calling convention
            # Do not emit `jl_fptr_const_return` if coverage is enabled
            # so that we don't need to add coverage support
            # to the `jl_call_method_internal` fast path
            # Still set pure flag to make sure `inference` tests pass
            # and to possibly enable more optimization in the future
            if !(isa(result, Const) && !is_inlineable_constant(result.val))
                opt.const_api = true
            end
            force_noinline || (opt.src.inlineable = true)
        end
    end

    opt.ir = ir

    # determine and cache inlineability
    union_penalties = false
    if !force_noinline
        sig = unwrap_unionall(linfo.specTypes)
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
        if !opt.src.inlineable && result === Union{}
            force_noinline = true
        end
    end
    if force_noinline
        opt.src.inlineable = false
    elseif isa(def, Method)
        if opt.src.inlineable && isdispatchtuple(linfo.specTypes)
            # obey @inline declaration if a dispatch barrier would not help
        else
            bonus = 0
            if result ⊑ Tuple && !isconcretetype(widenconst(result))
                bonus = params.inline_tupleret_bonus
            end
            if opt.src.inlineable
                # For functions declared @inline, increase the cost threshold 20x
                bonus += params.inline_cost_threshold*19
            end
            opt.src.inlineable = isinlineable(def, opt, params, union_penalties, bonus)
        end
    end

    nothing
end

# run the optimization work
function optimize(interp::AbstractInterpreter, opt::OptimizationState, params::OptimizationParams, @nospecialize(result))
    nargs = Int(opt.nargs) - 1
    @timeit "optimizer" ir = run_passes(opt.src, nargs, opt)
    finish(interp, opt, params, ir, result)
end

function run_passes(ci::CodeInfo, nargs::Int, sv::OptimizationState)
    preserve_coverage = coverage_enabled(sv.mod)
    ir = convert_to_ircode(ci, copy_exprargs(ci.code), preserve_coverage, nargs, sv)
    ir = slot2reg(ir, ci, nargs, sv)
    #@Base.show ("after_construct", ir)
    # TODO: Domsorting can produce an updated domtree - no need to recompute here
    @timeit "compact 1" ir = compact!(ir)
    @timeit "Inlining" ir = ssa_inlining_pass!(ir, ir.linetable, sv.inlining, ci.propagate_inbounds)
    #@timeit "verify 2" verify_ir(ir)
    ir = compact!(ir)
    #@Base.show ("before_sroa", ir)
    @timeit "SROA" ir = getfield_elim_pass!(ir)
    #@Base.show ir.new_nodes
    #@Base.show ("after_sroa", ir)
    ir = adce_pass!(ir)
    #@Base.show ("after_adce", ir)
    @timeit "type lift" ir = type_lift_pass!(ir)
    @timeit "compact 3" ir = compact!(ir)
    #@Base.show ir
    if JLOptions().debug_level == 2
        @timeit "verify 3" (verify_ir(ir); verify_linetable(ir.linetable))
    end
    return ir
end

function convert_to_ircode(ci::CodeInfo, code::Vector{Any}, coverage::Bool, nargs::Int, sv::OptimizationState)
    # Go through and add an unreachable node after every
    # Union{} call. Then reindex labels.
    idx = 1
    oldidx = 1
    changemap = fill(0, length(code))
    labelmap = coverage ? fill(0, length(code)) : changemap
    prevloc = zero(eltype(ci.codelocs))
    stmtinfo = sv.stmt_info
    ssavaluetypes = ci.ssavaluetypes::Vector{Any}
    while idx <= length(code)
        codeloc = ci.codelocs[idx]
        if coverage && codeloc != prevloc && codeloc != 0
            # insert a side-effect instruction before the current instruction in the same basic block
            insert!(code, idx, Expr(:code_coverage_effect))
            insert!(ci.codelocs, idx, codeloc)
            insert!(ssavaluetypes, idx, Nothing)
            insert!(stmtinfo, idx, nothing)
            changemap[oldidx] += 1
            if oldidx < length(labelmap)
                labelmap[oldidx + 1] += 1
            end
            idx += 1
            prevloc = codeloc
        end
        if code[idx] isa Expr && ssavaluetypes[idx] === Union{}
            if !(idx < length(code) && isa(code[idx + 1], ReturnNode) && !isdefined((code[idx + 1]::ReturnNode), :val))
                # insert unreachable in the same basic block after the current instruction (splitting it)
                insert!(code, idx + 1, ReturnNode())
                insert!(ci.codelocs, idx + 1, ci.codelocs[idx])
                insert!(ssavaluetypes, idx + 1, Union{})
                insert!(stmtinfo, idx + 1, nothing)
                if oldidx < length(changemap)
                    changemap[oldidx + 1] += 1
                    coverage && (labelmap[oldidx + 1] += 1)
                end
                idx += 1
            end
        end
        idx += 1
        oldidx += 1
    end
    renumber_ir_elements!(code, changemap, labelmap)

    inbounds_depth = 0 # Number of stacked inbounds
    meta = Any[]
    flags = fill(0x00, length(code))
    for i = 1:length(code)
        stmt = code[i]
        if isexpr(stmt, :inbounds)
            arg1 = stmt.args[1]
            if arg1 === true # push
                inbounds_depth += 1
            elseif arg1 === false # clear
                inbounds_depth = 0
            elseif inbounds_depth > 0 # pop
                inbounds_depth -= 1
            end
            stmt = nothing
        else
            stmt = normalize(stmt, meta)
        end
        code[i] = stmt
        if !(stmt === nothing)
            if inbounds_depth > 0
                flags[i] |= IR_FLAG_INBOUNDS
            end
        end
    end
    strip_trailing_junk!(ci, code, stmtinfo, flags)
    cfg = compute_basic_blocks(code)
    types = Any[]
    stmts = InstructionStream(code, types, stmtinfo, ci.codelocs, flags)
    ir = IRCode(stmts, cfg, collect(LineInfoNode, ci.linetable::Union{Vector{LineInfoNode},Vector{Any}}), sv.slottypes, meta, sv.sptypes)
    return ir
end

function normalize(@nospecialize(stmt), meta::Vector{Any})
    if isa(stmt, Expr)
        if stmt.head === :meta
            args = stmt.args
            if length(args) > 0
                push!(meta, stmt)
            end
            return nothing
        end
    end
    return stmt
end

function slot2reg(ir::IRCode, ci::CodeInfo, nargs::Int, sv::OptimizationState)
    # need `ci` for the slot metadata, IR for the code
    @timeit "domtree 1" domtree = construct_domtree(ir.cfg.blocks)
    defuse_insts = scan_slot_def_use(nargs, ci, ir.stmts.inst)
    @timeit "construct_ssa" ir = construct_ssa!(ci, ir, domtree, defuse_insts, nargs, sv.slottypes) # consumes `ir`
    return ir
end

# whether `f` is pure for inference
function is_pure_intrinsic_infer(f::IntrinsicFunction)
    return !(f === Intrinsics.pointerref || # this one is volatile
             f === Intrinsics.pointerset || # this one is never effect-free
             f === Intrinsics.llvmcall ||   # this one is never effect-free
             f === Intrinsics.arraylen ||   # this one is volatile
             f === Intrinsics.sqrt_llvm ||  # this one may differ at runtime (by a few ulps)
             f === Intrinsics.sqrt_llvm_fast ||  # this one may differ at runtime (by a few ulps)
             f === Intrinsics.cglobal)  # cglobal lookup answer changes at runtime
end

# whether `f` is effect free if nothrow
intrinsic_effect_free_if_nothrow(f) = f === Intrinsics.pointerref || is_pure_intrinsic_infer(f)

## Computing the cost of a function body

# saturating sum (inputs are nonnegative), prevents overflow with typemax(Int) below
plus_saturate(x::Int, y::Int) = max(x, y, x+y)

# known return type
isknowntype(@nospecialize T) = (T === Union{}) || isa(T, Const) || isconcretetype(widenconst(T))

function statement_cost(ex::Expr, line::Int, src::Union{CodeInfo, IRCode}, sptypes::Vector{Any},
                        slottypes::Vector{Any}, union_penalties::Bool,
                        params::OptimizationParams, error_path::Bool = false)
    head = ex.head
    if is_meta_expr_head(head)
        return 0
    elseif head === :call
        farg = ex.args[1]
        ftyp = argextype(farg, src, sptypes, slottypes)
        if ftyp === IntrinsicFunction && farg isa SSAValue
            # if this comes from code that was already inlined into another function,
            # Consts have been widened. try to recover in simple cases.
            farg = isa(src, CodeInfo) ? src.code[farg.id] : src.stmts[farg.id][:inst]
            if isa(farg, GlobalRef) || isa(farg, QuoteNode) || isa(farg, IntrinsicFunction) || isexpr(farg, :static_parameter)
                ftyp = argextype(farg, src, sptypes, slottypes)
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
        if isa(f, Builtin)
            # The efficiency of operations like a[i] and s.b
            # depend strongly on whether the result can be
            # inferred, so check the type of ex
            if f === Core.getfield || f === Core.tuple
                # we might like to penalize non-inferrability, but
                # tuple iteration/destructuring makes that impossible
                # return plus_saturate(argcost, isknowntype(extyp) ? 1 : params.inline_nonleaf_penalty)
                return 0
            elseif (f === Core.arrayref || f === Core.const_arrayref || f === Core.arrayset) && length(ex.args) >= 3
                atyp = argextype(ex.args[3], src, sptypes, slottypes)
                return isknowntype(atyp) ? 4 : error_path ? params.inline_error_path_cost : params.inline_nonleaf_penalty
            elseif f === typeassert && isconstType(widenconst(argextype(ex.args[3], src, sptypes, slottypes)))
                return 1
            elseif f === Core.isa
                # If we're in a union context, we penalize type computations
                # on union types. In such cases, it is usually better to perform
                # union splitting on the outside.
                if union_penalties && isa(argextype(ex.args[2],  src, sptypes, slottypes), Union)
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
        extyp = line == -1 ? Any : argextype(SSAValue(line), src, sptypes, slottypes)
        if extyp === Union{}
            return 0
        end
        return error_path ? params.inline_error_path_cost : params.inline_nonleaf_penalty
    elseif head === :foreigncall || head === :invoke
        # Calls whose "return type" is Union{} do not actually return:
        # they are errors. Since these are not part of the typical
        # run-time of the function, we omit them from
        # consideration. This way, non-inlined error branches do not
        # prevent inlining.
        extyp = line == -1 ? Any : argextype(SSAValue(line), src, sptypes, slottypes)
        return extyp === Union{} ? 0 : 20
    elseif head === :(=)
        if ex.args[1] isa GlobalRef
            cost = 20
        else
            cost = 0
        end
        a = ex.args[2]
        if a isa Expr
            cost = plus_saturate(cost, statement_cost(a, -1, src, sptypes, slottypes, union_penalties, params, error_path))
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

function statement_or_branch_cost(@nospecialize(stmt), line::Int, src::Union{CodeInfo, IRCode}, sptypes::Vector{Any},
                                  slottypes::Vector{Any}, union_penalties::Bool, params::OptimizationParams,
                                  throw_blocks::Union{Nothing,BitSet})
    thiscost = 0
    dst(tgt) = isa(src, IRCode) ? first(src.cfg.blocks[tgt].stmts) : tgt
    if stmt isa Expr
        thiscost = statement_cost(stmt, line, src, sptypes, slottypes, union_penalties, params,
                                  throw_blocks !== nothing && line in throw_blocks)::Int
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

function inline_worthy(ir::IRCode,
                       params::OptimizationParams, union_penalties::Bool=false, cost_threshold::Integer=params.inline_cost_threshold)
    bodycost::Int = 0
    throw_blocks = params.unoptimize_throw_blocks ? find_throw_blocks(ir.stmts.inst, RefValue(ir)) : nothing
    for line = 1:length(ir.stmts)
        stmt = ir.stmts[line][:inst]
        thiscost = statement_or_branch_cost(stmt, line, ir, ir.sptypes, ir.argtypes, union_penalties, params, throw_blocks)
        bodycost = plus_saturate(bodycost, thiscost)
        bodycost > cost_threshold && return false
    end
    return true
end

function statement_costs!(cost::Vector{Int}, body::Vector{Any}, src::Union{CodeInfo, IRCode}, sptypes::Vector{Any}, unionpenalties::Bool, params::OptimizationParams)
    throw_blocks = params.unoptimize_throw_blocks ? find_throw_blocks(body) : nothing
    maxcost = 0
    for line = 1:length(body)
        stmt = body[line]
        thiscost = statement_or_branch_cost(stmt, line, src, sptypes,
                                            src isa CodeInfo ? src.slottypes : src.argtypes,
                                            unionpenalties, params, throw_blocks)
        cost[line] = thiscost
        if thiscost > maxcost
            maxcost = thiscost
        end
    end
    return maxcost
end

function is_known_call(e::Expr, @nospecialize(func), src, sptypes::Vector{Any}, slottypes::Vector{Any} = empty_slottypes)
    if e.head !== :call
        return false
    end
    f = argextype(e.args[1], src, sptypes, slottypes)
    return isa(f, Const) && f.val === func
end

function renumber_ir_elements!(body::Vector{Any}, changemap::Vector{Int})
    return renumber_ir_elements!(body, changemap, changemap)
end

function cumsum_ssamap!(ssamap::Vector{Int})
    rel_change = 0
    for i = 1:length(ssamap)
        rel_change += ssamap[i]
        if ssamap[i] == -1
            # Keep a marker that this statement was deleted
            ssamap[i] = typemin(Int)
        else
            ssamap[i] = rel_change
        end
    end
end

function renumber_ir_elements!(body::Vector{Any}, ssachangemap::Vector{Int}, labelchangemap::Vector{Int})
    cumsum_ssamap!(labelchangemap)
    if ssachangemap !== labelchangemap
        cumsum_ssamap!(ssachangemap)
    end
    if labelchangemap[end] == 0 && ssachangemap[end] == 0
        return
    end
    for i = 1:length(body)
        el = body[i]
        if isa(el, GotoNode)
            body[i] = GotoNode(el.label + labelchangemap[el.label])
        elseif isa(el, GotoIfNot)
            cond = el.cond
            if isa(cond, SSAValue)
                cond = SSAValue(cond.id + ssachangemap[cond.id])
            end
            body[i] = GotoIfNot(cond, el.dest + labelchangemap[el.dest])
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
