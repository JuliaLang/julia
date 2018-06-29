# This file is a part of Julia. License is MIT: https://julialang.org/license

#####################
# OptimizationState #
#####################

mutable struct OptimizationState
    linfo::MethodInstance
    result_vargs::Vector{Any}
    calledges::Vector{Any}
    src::CodeInfo
    mod::Module
    nargs::Int
    min_valid::UInt
    max_valid::UInt
    params::Params
    sp::SimpleVector # static parameters
    slottypes::Vector{Any}
    const_api::Bool
    function OptimizationState(frame::InferenceState)
        s_edges = frame.stmt_edges[1]
        if s_edges === ()
            s_edges = []
            frame.stmt_edges[1] = s_edges
        end
        src = frame.src
        return new(frame.linfo, frame.result.vargs,
                   s_edges::Vector{Any},
                   src, frame.mod, frame.nargs,
                   frame.min_valid, frame.max_valid,
                   frame.params, frame.sp, frame.slottypes, false)
    end
    function OptimizationState(linfo::MethodInstance, src::CodeInfo,
                               params::Params)
        # prepare src for running optimization passes
        # if it isn't already
        nssavalues = src.ssavaluetypes
        if nssavalues isa Int
            src.ssavaluetypes = Any[ Any for i = 1:nssavalues ]
        end
        nslots = length(src.slotnames)
        slottypes = Any[ Any for i = 1:nslots ]
        s_edges = []
        # cache some useful state computations
        toplevel = !isa(linfo.def, Method)
        if !toplevel
            meth = linfo.def
            inmodule = meth.module
            nargs = meth.nargs
        else
            inmodule = linfo.def::Module
            nargs = 0
        end
        result_vargs = Any[] # if you want something more accurate, set it yourself :P
        return new(linfo, result_vargs,
                   s_edges::Vector{Any},
                   src, inmodule, nargs,
                   min_world(linfo), max_world(linfo),
                   params, spvals_from_meth_instance(linfo), slottypes, false)
        end
end

function OptimizationState(linfo::MethodInstance, params::Params)
    src = retrieve_code_info(linfo)
    src === nothing && return nothing
    return OptimizationState(linfo, src, params)
end


#############
# constants #
#############

# The slot has uses that are not statically dominated by any assignment
# This is implied by `SLOT_USEDUNDEF`.
# If this is not set, all the uses are (statically) dominated by the defs.
# In particular, if a slot has `AssignedOnce && !StaticUndef`, it is an SSA.
const SLOT_STATICUNDEF  = 1
const SLOT_ASSIGNEDONCE = 16 # slot is assigned to only once
const SLOT_USEDUNDEF    = 32 # slot has uses that might raise UndefVarError
# const SLOT_CALLED      = 64

const IR_FLAG_INBOUNDS = 0x01

# known affect-free calls (also effect-free)
const _PURE_BUILTINS = Any[tuple, svec, fieldtype, apply_type, ===, isa, typeof, UnionAll, nfields]

# known effect-free calls (might not be affect-free)
const _PURE_BUILTINS_VOLATILE = Any[getfield, arrayref, isdefined, Core.sizeof]

const TOP_TUPLE = GlobalRef(Core, :tuple)

#########
# logic #
#########

_topmod(sv::OptimizationState) = _topmod(sv.mod)

function update_valid_age!(min_valid::UInt, max_valid::UInt, sv::OptimizationState)
    sv.min_valid = max(sv.min_valid, min_valid)
    sv.max_valid = min(sv.max_valid, max_valid)
    @assert(!isa(sv.linfo.def, Method) ||
            (sv.min_valid == typemax(UInt) && sv.max_valid == typemin(UInt)) ||
            sv.min_valid <= sv.params.world <= sv.max_valid,
            "invalid age range update")
    nothing
end

update_valid_age!(li::MethodInstance, sv::OptimizationState) = update_valid_age!(min_world(li), max_world(li), sv)

function add_backedge!(li::MethodInstance, caller::OptimizationState)
    isa(caller.linfo.def, Method) || return # don't add backedges to toplevel exprs
    push!(caller.calledges, li)
    update_valid_age!(li, caller)
    nothing
end

function isinlineable(m::Method, me::OptimizationState, bonus::Int=0)
    # compute the cost (size) of inlining this code
    inlineable = false
    cost_threshold = me.params.inline_cost_threshold
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
        inlineable = inline_worthy(me.src.code, me.src, me.sp, me.slottypes, me.params, cost_threshold + bonus)
    end
    return inlineable
end

# run the optimization work
function optimize(opt::OptimizationState, @nospecialize(result))
    def = opt.linfo.def
    nargs = Int(opt.nargs) - 1
    @timeit "optimizer" ir = run_passes(opt.src, nargs, opt)
    force_noinline = _any(@nospecialize(x) -> isexpr(x, :meta) && x.args[1] == :noinline, ir.meta)

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
                stmt = ir.stmts[i]
                # These affect control flow within the function (so may not be removed
                # if there is no usage within the function), but don't affect the purity
                # of the function as a whole.
                if isa(stmt, GotoIfNot) || isa(stmt, GotoNode) || isa(stmt, ReturnNode)
                    continue
                end
                if !stmt_effect_free(stmt, ir, ir.spvals)
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

        if proven_pure && !coverage_enabled()
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

    replace_code_newstyle!(opt.src, ir, nargs)

    # determine and cache inlineability
    if !force_noinline
        # don't keep ASTs for functions specialized on a Union argument
        # TODO: this helps avoid a type-system bug mis-computing sparams during intersection
        sig = unwrap_unionall(opt.linfo.specTypes)
        if isa(sig, DataType) && sig.name === Tuple.name
            for P in sig.parameters
                P = unwrap_unionall(P)
                if isa(P, Union)
                    force_noinline = true
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
        bonus = 0
        if result ⊑ Tuple && !isbitstype(widenconst(result))
            bonus = opt.params.inline_tupleret_bonus
        end
        if opt.src.inlineable
            # For functions declared @inline, increase the cost threshold 20x
            bonus += opt.params.inline_cost_threshold*19
        end
        opt.src.inlineable = isinlineable(def, opt, bonus)
    end
    nothing
end


# whether `f` is pure for inference
function is_pure_intrinsic_infer(f::IntrinsicFunction)
    return !(f === Intrinsics.pointerref || # this one is volatile
             f === Intrinsics.pointerset || # this one is never effect-free
             f === Intrinsics.llvmcall ||   # this one is never effect-free
             f === Intrinsics.arraylen ||   # this one is volatile
             f === Intrinsics.sqrt_llvm ||  # this one may differ at runtime (by a few ulps)
             f === Intrinsics.cglobal)  # cglobal lookup answer changes at runtime
end

# whether `f` is pure for optimizations
function is_pure_intrinsic_optim(f::IntrinsicFunction)
    return !(f === Intrinsics.pointerref || # this one is volatile
             f === Intrinsics.pointerset || # this one is never effect-free
             f === Intrinsics.llvmcall ||   # this one is never effect-free
             f === Intrinsics.arraylen ||   # this one is volatile
             f === Intrinsics.checked_sdiv_int ||  # these may throw errors
             f === Intrinsics.checked_udiv_int ||
             f === Intrinsics.checked_srem_int ||
             f === Intrinsics.checked_urem_int ||
             f === Intrinsics.cglobal)  # cglobal throws an error for symbol-not-found
end

function is_pure_builtin(@nospecialize(f))
    if isa(f, IntrinsicFunction)
        return is_pure_intrinsic_optim(f)
    elseif isa(f, Builtin)
        return (contains_is(_PURE_BUILTINS, f) ||
                contains_is(_PURE_BUILTINS_VOLATILE, f))
    else
        return f === return_type
    end
end

## Computing the cost of a function body

# saturating sum (inputs are nonnegative), prevents overflow with typemax(Int) below
plus_saturate(x, y) = max(x, y, x+y)

# known return type
isknowntype(@nospecialize T) = (T == Union{}) || isconcretetype(T)

function statement_cost(ex::Expr, line::Int, src::CodeInfo, spvals::SimpleVector, slottypes::Vector{Any}, params::Params)
    head = ex.head
    if is_meta_expr_head(head)
        return 0
    elseif head === :call
        ftyp = argextype(ex.args[1], src, spvals, slottypes)
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
            if f === Main.Core.getfield || f === Main.Core.tuple
                # we might like to penalize non-inferrability, but
                # tuple iteration/destructuring makes that impossible
                # return plus_saturate(argcost, isknowntype(extyp) ? 1 : params.inline_nonleaf_penalty)
                return 0
            elseif f === Main.Core.arrayref && length(ex.args) >= 3
                atyp = argextype(ex.args[3], src, spvals, slottypes)
                return isknowntype(atyp) ? 4 : params.inline_nonleaf_penalty
            end
            fidx = findfirst(x->x===f, T_FFUNC_KEY)
            if fidx === nothing
                # unknown/unhandled builtin or anonymous function
                # Use the generic cost of a direct function call
                return 20
            end
            return T_FFUNC_COST[fidx]
        end
        return params.inline_nonleaf_penalty
    elseif head === :foreigncall || head === :invoke
        # Calls whose "return type" is Union{} do not actually return:
        # they are errors. Since these are not part of the typical
        # run-time of the function, we omit them from
        # consideration. This way, non-inlined error branches do not
        # prevent inlining.
        extyp = line == -1 ? Any : src.ssavaluetypes[line]
        return extyp === Union{} ? 0 : 20
    elseif head === :return
        a = ex.args[1]
        if a isa Expr
            return statement_cost(a, -1, src, spvals, slottypes, params)
        end
        return 0
    elseif head === :(=)
        if ex.args[1] isa GlobalRef
            cost = 20
        else
            cost = 0
        end
        a = ex.args[2]
        if a isa Expr
            cost = plus_saturate(cost, statement_cost(a, -1, src, spvals, slottypes, params))
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
    elseif head === :gotoifnot
        target = ex.args[2]::Int
        # loops are generally always expensive
        # but assume that forward jumps are already counted for from
        # summing the cost of the not-taken branch
        return target < line ? 40 : 0
    end
    return 0
end

function inline_worthy(body::Array{Any,1}, src::CodeInfo, spvals::SimpleVector, slottypes::Vector{Any},
                       params::Params, cost_threshold::Integer=params.inline_cost_threshold)
    bodycost::Int = 0
    for line = 1:length(body)
        stmt = body[line]
        if stmt isa Expr
            thiscost = statement_cost(stmt, line, src, spvals, slottypes, params)::Int
        elseif stmt isa GotoNode
            # loops are generally always expensive
            # but assume that forward jumps are already counted for from
            # summing the cost of the not-taken branch
            thiscost = stmt.label < line ? 40 : 0
        else
            continue
        end
        bodycost = plus_saturate(bodycost, thiscost)
        bodycost > cost_threshold && return false
    end
    return true
end

function is_known_call(e::Expr, @nospecialize(func), src, spvals::SimpleVector, slottypes::Vector{Any} = empty_slottypes)
    if e.head !== :call
        return false
    end
    f = argextype(e.args[1], src, spvals, slottypes)
    return isa(f, Const) && f.val === func
end

function is_known_call_p(e::Expr, @nospecialize(pred), src, spvals::SimpleVector, slottypes::Vector{Any})
    if e.head !== :call
        return false
    end
    f = argextype(e.args[1], src, spvals, slottypes)
    return (isa(f, Const) && pred(f.val)) || (isType(f) && pred(f.parameters[1]))
end

function renumber_ir_elements!(body::Vector{Any}, changemap::Vector{Int}, preprocess::Bool = true)
    if preprocess
        for i = 2:length(changemap)
            changemap[i] += changemap[i - 1]
        end
    end
    changemap[end] != 0 || return
    for i = 1:length(body)
        el = body[i]
        if isa(el, GotoNode)
            body[i] = GotoNode(el.label + changemap[el.label])
        elseif isa(el, SSAValue)
            body[i] = SSAValue(el.id + changemap[el.id])
        elseif isa(el, Expr)
            if el.head === :gotoifnot
                cond = el.args[1]
                if isa(cond, SSAValue)
                    el.args[1] = SSAValue(cond.id + changemap[cond.id])
                end
                tgt = el.args[2]::Int
                el.args[2] = tgt + changemap[tgt]
            elseif el.head === :enter
                tgt = el.args[1]::Int
                el.args[1] = tgt + changemap[tgt]
            elseif !is_meta_expr_head(el.head)
                renumber_ir_elements!(el.args, changemap, false)
            end
        end
    end
end

include("compiler/ssair/driver.jl")
