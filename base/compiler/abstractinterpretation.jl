# This file is a part of Julia. License is MIT: https://julialang.org/license

#############
# constants #
#############

const CoreNumType = Union{Int32, Int64, Float32, Float64}

const DEPRECATED_SYM = Symbol("deprecated.jl")

const _REF_NAME = Ref.body.name

#########
# logic #
#########

function abstract_call_gf_by_type(@nospecialize(f), argtypes::Vector{Any}, @nospecialize(atype), sv::InferenceState)
    atype_params = unwrap_unionall(atype).parameters
    ft = unwrap_unionall(atype_params[1]) # TODO: ccall jl_first_argument_datatype here
    isa(ft, DataType) || return Any # the function being called is unknown. can't properly handle this backedge right now
    ftname = ft.name
    isdefined(ftname, :mt) || return Any # not callable. should be Bottom, but can't track this backedge right now
    if ftname === _TYPE_NAME
        tname = ft.parameters[1]
        if isa(tname, TypeVar)
            tname = tname.ub
        end
        tname = unwrap_unionall(tname)
        if !isa(tname, DataType)
            # can't track the backedge to the ctor right now
            # for things like Union
            return Any
        end
    end
    min_valid = UInt[typemin(UInt)]
    max_valid = UInt[typemax(UInt)]
    splitunions = 1 < countunionsplit(atype_params) <= sv.params.MAX_UNION_SPLITTING
    if splitunions
        splitsigs = switchtupleunion(atype)
        applicable = Any[]
        for sig_n in splitsigs
            xapplicable = _methods_by_ftype(sig_n, sv.params.MAX_METHODS, sv.params.world, min_valid, max_valid)
            xapplicable === false && return Any
            append!(applicable, xapplicable)
        end
    else
        applicable = _methods_by_ftype(atype, sv.params.MAX_METHODS, sv.params.world, min_valid, max_valid)
        if applicable === false
            # this means too many methods matched
            # (assume this will always be true, so we don't compute / update valid age in this case)
            return Any
        end
    end
    update_valid_age!(min_valid[1], max_valid[1], sv)
    applicable = applicable::Array{Any,1}
    napplicable = length(applicable)
    rettype = Bottom
    edgecycle = false
    for i in 1:napplicable
        match = applicable[i]::SimpleVector
        method = match[3]::Method
        sig = match[1]
        sigtuple = unwrap_unionall(sig)::DataType
        splitunions = false
        # TODO: splitunions = 1 < countunionsplit(sigtuple.parameters) * napplicable <= sv.params.MAX_UNION_SPLITTING
        # currently this triggers a bug in inference recursion detection
        if splitunions
            splitsigs = switchtupleunion(sig)
            for sig_n in splitsigs
                rt, edgecycle1 = abstract_call_method(method, sig_n, svec(), sv)
                edgecycle |= edgecycle1::Bool
                rettype = tmerge(rettype, rt)
                rettype === Any && break
            end
            rettype === Any && break
        else
            rt, edgecycle = abstract_call_method(method, sig, match[2]::SimpleVector, sv)
            rettype = tmerge(rettype, rt)
            rettype === Any && break
        end
    end
    if napplicable == 1 && !edgecycle && isa(rettype, Type) && sv.params.ipo_constant_propagation
        # if there's a possibility we could constant-propagate a better result
        # (hopefully without doing too much work), try to do that now
        # TODO: it feels like this could be better integrated into abstract_call_method / typeinf_edge
        const_rettype = abstract_call_method_with_const_args(f, argtypes, applicable[1]::SimpleVector, sv)
        if const_rettype ⊑ rettype
            # use the better result, if it's a refinement of rettype
            rettype = const_rettype
        end
    end
    if !(rettype === Any) # adding a new method couldn't refine (widen) this type
        fullmatch = false
        for i in napplicable:-1:1
            match = applicable[i]::SimpleVector
            method = match[3]::Method
            if atype <: method.sig
                fullmatch = true
                break
            end
        end
        if !fullmatch
            # also need an edge to the method table in case something gets
            # added that did not intersect with any existing method
            add_mt_backedge!(ftname.mt, atype, sv)
        end
    end
    #print("=> ", rettype, "\n")
    return rettype
end

function abstract_call_method_with_const_args(@nospecialize(f), argtypes::Vector{Any}, match::SimpleVector, sv::InferenceState)
    method = match[3]::Method
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    length(argtypes) >= nargs || return Any
    haveconst = false
    for a in argtypes
        if isa(a, Const) && !isdefined(typeof(a.val), :instance) && !(isa(a.val, Type) && issingletontype(a.val))
            # have new information from argtypes that wasn't available from the signature
            haveconst = true
            break
        end
    end
    haveconst || return Any
    sig = match[1]
    sparams = match[2]::SimpleVector
    code = code_for_method(method, sig, sparams, sv.params.world)
    code === nothing && return Any
    code = code::MethodInstance
    # decide if it's likely to be worthwhile
    cache_inlineable = false
    if isdefined(code, :inferred)
        cache_inf = code.inferred
        if !(cache_inf === nothing)
            cache_src_inferred = ccall(:jl_ast_flag_inferred, Bool, (Any,), cache_inf)
            cache_src_inlineable = ccall(:jl_ast_flag_inlineable, Bool, (Any,), cache_inf)
            cache_inlineable = cache_src_inferred && cache_src_inlineable
        end
    end
    if !cache_inlineable && !sv.params.aggressive_constant_propagation
        tm = _topmod(sv)
        if !istopfunction(f, :getproperty) && !istopfunction(f, :setproperty!)
            # in this case, see if all of the arguments are constants
            for a in argtypes
                if !isa(a, Const) && !isconstType(a)
                    return Any
                end
            end
        end
    end
    inf_result = cache_lookup(code, argtypes, sv.params.cache)
    if inf_result === nothing
        inf_result = InferenceResult(code)
        atypes = get_argtypes(inf_result)
        if method.isva
            vargs = argtypes[(nargs + 1):end]
            for i in 1:length(vargs)
                a = vargs[i]
                if i > length(inf_result.vargs)
                    push!(inf_result.vargs, a)
                elseif a isa Const
                    inf_result.vargs[i] = a
                end
            end
        end
        for i in 1:nargs
            a = argtypes[i]
            if a isa Const
                atypes[i] = a # inject Const argtypes into inference
            end
        end
        frame = InferenceState(inf_result, #=cache=#false, sv.params)
        frame.limited = true
        frame.parent = sv
        push!(sv.params.cache, inf_result)
        typeinf(frame) || return Any
    end
    result = inf_result.result
    isa(result, InferenceState) && return Any # TODO: is this recursive constant inference?
    add_backedge!(inf_result.linfo, sv)
    return result
end

function abstract_call_method(method::Method, @nospecialize(sig), sparams::SimpleVector, sv::InferenceState)
    # TODO: remove with 0.7 deprecations
    if method.file === DEPRECATED_SYM && method.sig == (Tuple{Type{T},Any} where T)
        return Any, false
    end
    if method.name === :depwarn && isdefined(Main, :Base) && method.module === Main.Base
        return Any, false
    end
    topmost = nothing
    # Limit argument type tuple growth of functions:
    # look through the parents list to see if there's a call to the same method
    # and from the same method.
    # Returns the topmost occurrence of that repeated edge.
    cyclei = 0
    infstate = sv
    edgecycle = false
    # The `method_for_inference_heuristics` will expand the given method's generator if
    # necessary in order to retrieve this field from the generated `CodeInfo`, if it exists.
    # The other `CodeInfo`s we inspect will already have this field inflated, so we just
    # access it directly instead (to avoid regeneration).
    method2 = method_for_inference_heuristics(method, sig, sparams, sv.params.world) # Union{Method, Nothing}
    sv_method2 = sv.src.method_for_inference_limit_heuristics # limit only if user token match
    sv_method2 isa Method || (sv_method2 = nothing) # Union{Method, Nothing}
    while !(infstate === nothing)
        infstate = infstate::InferenceState
        if method === infstate.linfo.def
            if infstate.linfo.specTypes == sig
                # avoid widening when detecting self-recursion
                # TODO: merge call cycle and return right away
                topmost = nothing
                edgecycle = true
                break
            end
            inf_method2 = infstate.src.method_for_inference_limit_heuristics # limit only if user token match
            inf_method2 isa Method || (inf_method2 = nothing) # Union{Method, Nothing}
            if topmost === nothing && method2 === inf_method2
                # inspect the parent of this edge,
                # to see if they are the same Method as sv
                # in which case we'll need to ensure it is convergent
                # otherwise, we don't
                for parent in infstate.callers_in_cycle
                    # check in the cycle list first
                    # all items in here are mutual parents of all others
                    parent_method2 = parent.src.method_for_inference_limit_heuristics # limit only if user token match
                    parent_method2 isa Method || (parent_method2 = nothing) # Union{Method, Nothing}
                    if parent.linfo.def === sv.linfo.def && sv_method2 === parent_method2
                        topmost = infstate
                        edgecycle = true
                        break
                    end
                end
                let parent = infstate.parent
                    # then check the parent link
                    if topmost === nothing && parent !== nothing
                        parent = parent::InferenceState
                        parent_method2 = parent.src.method_for_inference_limit_heuristics # limit only if user token match
                        parent_method2 isa Method || (parent_method2 = nothing) # Union{Method, Nothing}
                        if parent.cached && parent.linfo.def === sv.linfo.def && sv_method2 === parent_method2
                            topmost = infstate
                            edgecycle = true
                        end
                    end
                end
            end
        end
        # iterate through the cycle before walking to the parent
        if cyclei < length(infstate.callers_in_cycle)
            cyclei += 1
            infstate = infstate.callers_in_cycle[cyclei]
        else
            cyclei = 0
            infstate = infstate.parent
        end
    end

    if !(topmost === nothing)
        topmost = topmost::InferenceState
        sigtuple = unwrap_unionall(sig)::DataType
        msig = unwrap_unionall(method.sig)::DataType
        spec_len = length(msig.parameters) + 1
        ls = length(sigtuple.parameters)
        if method === sv.linfo.def
            # Under direct self-recursion, permit much greater use of reducers.
            # here we assume that complexity(specTypes) :>= complexity(sig)
            comparison = sv.linfo.specTypes
            l_comparison = length(unwrap_unionall(comparison).parameters)
            spec_len = max(spec_len, l_comparison)
        else
            comparison = method.sig
        end
        # see if the type is actually too big (relative to the caller), and limit it if required
        newsig = limit_type_size(sig, comparison, sv.linfo.specTypes, sv.params.TUPLE_COMPLEXITY_LIMIT_DEPTH, spec_len)

        if newsig !== sig
            # continue inference, but note that we've limited parameter complexity
            # on this call (to ensure convergence), so that we don't cache this result
            infstate = sv
            topmost = topmost::InferenceState
            while !(infstate.parent === topmost.parent)
                infstate.limited = true
                for infstate_cycle in infstate.callers_in_cycle
                    infstate_cycle.limited = true
                end
                infstate = infstate.parent
            end
            sig = newsig
            sparams = svec()
        end
    end

    # if sig changed, may need to recompute the sparams environment
    if isa(method.sig, UnionAll) && isempty(sparams)
        recomputed = ccall(:jl_type_intersection_with_env, Any, (Any, Any), sig, method.sig)::SimpleVector
        sig = recomputed[1]
        if !isa(unwrap_unionall(sig), DataType) # probably Union{}
            return Any, false
        end
        sparams = recomputed[2]::SimpleVector
    end

    rt, edge = typeinf_edge(method, sig, sparams, sv)
    if edge === nothing
        edgecycle = true
    else
        add_backedge!(edge::MethodInstance, sv)
    end
    return rt, edgecycle
end

# This is only for use with `Conditional`.
# In general, usage of this is wrong.
function ssa_def_expr(@nospecialize(arg), sv::InferenceState)
    while isa(arg, SSAValue)
        arg = sv.src.code[arg.id]
    end
    return arg
end

# `typ` is the inferred type for expression `arg`.
# if the expression constructs a container (e.g. `svec(x,y,z)`),
# refine its type to an array of element types.
# Union of Tuples of the same length is converted to Tuple of Unions.
# returns an array of types
function precise_container_type(@nospecialize(arg), @nospecialize(typ), vtypes::VarTable, sv::InferenceState)
    if isa(typ, Const)
        val = typ.val
        if isa(val, SimpleVector) || isa(val, Tuple)
            return Any[ Const(val[i]) for i in 1:length(val) ] # avoid making a tuple Generator here!
        end
    end

    arg = ssa_def_expr(arg, sv)
    if is_specializable_vararg_slot(arg, sv.nargs, sv.result.vargs)
        return sv.result.vargs
    end

    tti0 = widenconst(typ)
    tti = unwrap_unionall(tti0)
    if isa(arg, Expr) && arg.head === :call && (abstract_evals_to_constant(arg.args[1], svec, vtypes, sv) ||
                                                abstract_evals_to_constant(arg.args[1], tuple, vtypes, sv))
        aa = arg.args
        result = Any[ abstract_eval(aa[j],vtypes,sv) for j=2:length(aa) ]
        if _any(isvarargtype, result)
            return Any[Vararg{Any}]
        end
        return result
    elseif isa(tti, Union)
        utis = uniontypes(tti)
        if _any(t -> !isa(t,DataType) || !(t <: Tuple) || !isknownlength(t), utis)
            return Any[Vararg{Any}]
        end
        result = Any[rewrap_unionall(p, tti0) for p in utis[1].parameters]
        for t in utis[2:end]
            if length(t.parameters) != length(result)
                return Any[Vararg{Any}]
            end
            for j in 1:length(t.parameters)
                result[j] = tmerge(result[j], rewrap_unionall(t.parameters[j], tti0))
            end
        end
        return result
    elseif isa(tti0,DataType) && tti0 <: Tuple
        if isvatuple(tti0) && length(tti0.parameters) == 1
            return Any[Vararg{unwrapva(tti0.parameters[1])}]
        else
            return Any[ p for p in tti0.parameters ]
        end
    elseif tti0 <: Array
        return Any[Vararg{eltype(tti0)}]
    else
        return abstract_iteration(typ, vtypes, sv)
    end
end

# simulate iteration protocol on container type up to fixpoint
function abstract_iteration(@nospecialize(itertype), vtypes::VarTable, sv::InferenceState)
    if !isdefined(Main, :Base) || !isdefined(Main.Base, :iterate) || !isconst(Main.Base, :iterate)
        return Any[Vararg{Any}]
    end
    iteratef = getfield(Main.Base, :iterate)
    stateordonet = abstract_call(iteratef, (), Any[Const(iteratef), itertype], vtypes, sv)
    # Return Bottom if this is not an iterator.
    # WARNING: Changes to the iteration protocol must be reflected here,
    # this is not just an optimization.
    stateordonet === Bottom && return Any[Bottom]
    valtype = statetype = Bottom
    ret = Any[]
    stateordonet = widenconst(stateordonet)
    while !(Nothing <: stateordonet) && length(ret) < sv.params.MAX_TUPLE_SPLAT
        if !isa(stateordonet, DataType) || !(stateordonet <: Tuple) || isvatuple(stateordonet) || length(stateordonet.parameters) != 2
            break
        end
        if stateordonet.parameters[2] <: statetype
            # infinite (or failing) iterator
            return Any[Bottom]
        end
        valtype = stateordonet.parameters[1]
        statetype = stateordonet.parameters[2]
        push!(ret, valtype)
        stateordonet = abstract_call(iteratef, (), Any[Const(iteratef), itertype, statetype], vtypes, sv)
        stateordonet = widenconst(stateordonet)
    end
    if stateordonet === Nothing
        return ret
    end
    while valtype !== Any
        nounion = typesubtract(stateordonet, Nothing)
        if !isa(nounion, DataType) || !(nounion <: Tuple) || isvatuple(nounion) || length(nounion.parameters) != 2
            valtype = Any
            break
        end
        if nounion.parameters[1] <: valtype && nounion.parameters[2] <: statetype
            break
        end
        valtype = tmerge(valtype, nounion.parameters[1])
        statetype = tmerge(statetype, nounion.parameters[2])
        stateordonet = abstract_call(iteratef, (), Any[Const(iteratef), itertype, statetype], vtypes, sv)
        stateordonet = widenconst(stateordonet)
    end
    push!(ret, Vararg{valtype})
    return ret
end

# do apply(af, fargs...), where af is a function value
function abstract_apply(@nospecialize(aft), fargs::Vector{Any}, aargtypes::Vector{Any}, vtypes::VarTable, sv::InferenceState)
    if !isa(aft, Const) && (!isType(aft) || has_free_typevars(aft))
        if !isconcretetype(aft) || (aft <: Builtin)
            # non-constant function of unknown type: bail now,
            # since it seems unlikely that abstract_call will be able to do any better after splitting
            # this also ensures we don't call abstract_call_gf_by_type below on an IntrinsicFunction or Builtin
            return Any
        end
    end
    res = Union{}
    nargs = length(fargs)
    @assert nargs == length(aargtypes)
    splitunions = 1 < countunionsplit(aargtypes) <= sv.params.MAX_APPLY_UNION_ENUM
    ctypes = Any[Any[aft]]
    for i = 1:nargs
        ctypes´ = []
        for ti in (splitunions ? uniontypes(aargtypes[i]) : Any[aargtypes[i]])
            cti = precise_container_type(fargs[i], ti, vtypes, sv)
            if _any(t -> t === Bottom, cti)
                continue
            end
            for ct in ctypes
                if isvarargtype(ct[end])
                    tail = tuple_tail_elem(unwrapva(ct[end]), cti)
                    push!(ctypes´, push!(ct[1:(end - 1)], tail))
                else
                    push!(ctypes´, append_any(ct, cti))
                end
            end
        end
        ctypes = ctypes´
    end
    for ct in ctypes
        if isa(aft, Const)
            rt = abstract_call(aft.val, (), ct, vtypes, sv)
        elseif isconstType(aft)
            rt = abstract_call(aft.parameters[1], (), ct, vtypes, sv)
        else
            astype = argtypes_to_type(ct)
            rt = abstract_call_gf_by_type(nothing, ct, astype, sv)
        end
        res = tmerge(res, rt)
        if res === Any
            break
        end
    end
    return res
end

function pure_eval_call(@nospecialize(f), argtypes::Vector{Any}, @nospecialize(atype), sv::InferenceState)
    for i = 2:length(argtypes)
        a = argtypes[i]
        if !(isa(a,Const) || isconstType(a))
            return false
        end
    end

    min_valid = UInt[typemin(UInt)]
    max_valid = UInt[typemax(UInt)]
    meth = _methods_by_ftype(atype, 1, sv.params.world, min_valid, max_valid)
    if meth === false || length(meth) != 1
        return false
    end
    meth = meth[1]::SimpleVector
    method = meth[3]::Method
    # TODO: check pure on the inferred thunk
    if isdefined(method, :generator) || !method.pure
        return false
    end

    args = Any[ (a=argtypes[i]; isa(a,Const) ? a.val : a.parameters[1]) for i in 2:length(argtypes) ]
    try
        value = Core._apply_pure(f, args)
        # TODO: add some sort of edge(s)
        return Const(value, true)
    catch
        return false
    end
end

function abstract_call(@nospecialize(f), fargs::Union{Tuple{},Vector{Any}}, argtypes::Vector{Any}, vtypes::VarTable, sv::InferenceState)
    if f === _apply
        length(fargs) > 1 || return Any
        return abstract_apply(argtypes[2], fargs[3:end], argtypes[3:end], vtypes, sv)
    end

    la = length(argtypes)
    for i = 2:(la - 1)
        if isvarargtype(argtypes[i])
            return Any
        end
    end

    if isa(f, Builtin) || isa(f, IntrinsicFunction)
        if f === ifelse && fargs isa Vector{Any} && length(argtypes) == 4 && argtypes[2] isa Conditional
            cnd = argtypes[2]
            tx = argtypes[3]
            ty = argtypes[4]
            if isa(fargs[3], Slot) && slot_id(cnd.var) == slot_id(fargs[3])
                tx = typeintersect(tx, cnd.vtype)
            end
            if isa(fargs[4], Slot) && slot_id(cnd.var) == slot_id(fargs[4])
                ty = typeintersect(ty, cnd.elsetype)
            end
            return tmerge(tx, ty)
        end
        rt = builtin_tfunction(f, argtypes[2:end], sv)
        if f === getfield && isa(fargs, Vector{Any}) && length(argtypes) == 3 && isa(argtypes[3], Const) && isa(argtypes[3].val, Int) && argtypes[2] ⊑ Tuple
            cti = precise_container_type(fargs[2], argtypes[2], vtypes, sv)
            idx = argtypes[3].val
            if 1 <= idx <= length(cti)
                rt = unwrapva(cti[idx])
            end
        elseif (rt === Bool || (isa(rt, Const) && isa(rt.val, Bool))) && isa(fargs, Vector{Any})
            # perform very limited back-propagation of type information for `is` and `isa`
            if f === isa
                a = ssa_def_expr(fargs[2], sv)
                if isa(a, Slot)
                    aty = widenconst(argtypes[2])
                    tty_ub, isexact_tty = instanceof_tfunc(argtypes[3])
                    if isexact_tty && !isa(tty_ub, TypeVar)
                        tty_lb = tty_ub # TODO: this would be wrong if !isexact_tty, but instanceof_tfunc doesn't preserve this info
                        if !has_free_typevars(tty_lb) && !has_free_typevars(tty_ub)
                            ifty = typeintersect(aty, tty_ub)
                            elsety = typesubtract(aty, tty_lb)
                            if ifty != elsety
                                return Conditional(a, ifty, elsety)
                            end
                        end
                    end
                    return Bool
                end
            elseif f === (===)
                a = ssa_def_expr(fargs[2], sv)
                b = ssa_def_expr(fargs[3], sv)
                aty = argtypes[2]
                bty = argtypes[3]
                # if doing a comparison to a singleton, consider returning a `Conditional` instead
                if isa(aty, Const) && isa(b, Slot)
                    if isdefined(typeof(aty.val), :instance) # can only widen a if it is a singleton
                        return Conditional(b, aty, typesubtract(widenconst(bty), typeof(aty.val)))
                    end
                    return isa(rt, Const) ? rt : Conditional(b, aty, bty)
                end
                if isa(bty, Const) && isa(a, Slot)
                    if isdefined(typeof(bty.val), :instance) # same for b
                        return Conditional(a, bty, typesubtract(widenconst(aty), typeof(bty.val)))
                    end
                    return isa(rt, Const) ? rt : Conditional(a, bty, aty)
                end
            elseif f === Core.Compiler.not_int
                aty = argtypes[2]
                if isa(aty, Conditional)
                    return Conditional(aty.var, aty.elsetype, aty.vtype)
                end
            end
        end
        return isa(rt, TypeVar) ? rt.ub : rt
    elseif f === Core.kwfunc
        if length(argtypes) == 2
            ft = widenconst(argtypes[2])
            if isa(ft, DataType) && isdefined(ft.name, :mt) && isdefined(ft.name.mt, :kwsorter)
                return Const(ft.name.mt.kwsorter)
            end
        end
        return Any
    elseif f === TypeVar
        lb = Union{}
        ub = Any
        ub_certain = lb_certain = true
        if length(argtypes) >= 2 && isa(argtypes[2], Const)
            nv = argtypes[2].val
            ubidx = 3
            if length(argtypes) >= 4
                ubidx = 4
                if isa(argtypes[3], Const)
                    lb = argtypes[3].val
                elseif isType(argtypes[3])
                    lb = argtypes[3].parameters[1]
                    lb_certain = false
                else
                    return TypeVar
                end
            end
            if length(argtypes) >= ubidx
                if isa(argtypes[ubidx], Const)
                    ub = argtypes[ubidx].val
                elseif isType(argtypes[ubidx])
                    ub = argtypes[ubidx].parameters[1]
                    ub_certain = false
                else
                    return TypeVar
                end
            end
            tv = TypeVar(nv, lb, ub)
            return PartialTypeVar(tv, lb_certain, ub_certain)
        end
        return TypeVar
    elseif f === UnionAll
        if length(argtypes) == 3
            canconst = true
            if isa(argtypes[3], Const)
                body = argtypes[3].val
            elseif isType(argtypes[3])
                body = argtypes[3].parameters[1]
                canconst = false
            else
                return Any
            end
            if !isa(body, Type) && !isa(body, TypeVar)
                return Any
            end
            if has_free_typevars(body)
                if isa(argtypes[2], Const)
                    tv = argtypes[2].val
                elseif isa(argtypes[2], PartialTypeVar)
                    ptv = argtypes[2]
                    tv = ptv.tv
                    canconst = false
                else
                    return Any
                end
                !isa(tv, TypeVar) && return Any
                body = UnionAll(tv, body)
            end
            ret = canconst ? AbstractEvalConstant(body) : Type{body}
            return ret
        end
        return Any
    elseif f === return_type
        rt_rt = return_type_tfunc(argtypes, vtypes, sv)
        if rt_rt !== NOT_FOUND
            return rt_rt
        end
    elseif length(argtypes) == 2 && istopfunction(f, :!)
        # handle Conditional propagation through !Bool
        aty = argtypes[2]
        if isa(aty, Conditional)
            abstract_call_gf_by_type(f, Any[Const(f), Bool], Tuple{typeof(f), Bool}, sv) # make sure we've inferred `!(::Bool)`
            return Conditional(aty.var, aty.elsetype, aty.vtype)
        end
    elseif length(argtypes) == 3 && istopfunction(f, :!==)
        # mark !== as exactly a negated call to ===
        rty = abstract_call((===), fargs, argtypes, vtypes, sv)
        if isa(rty, Conditional)
            return Conditional(rty.var, rty.elsetype, rty.vtype) # swap if-else
        elseif isa(rty, Const)
            return Const(rty.val === false)
        end
        return rty
    elseif length(argtypes) == 3 && istopfunction(f, :(>:))
        # mark issupertype as a exact alias for issubtype
        # swap T1 and T2 arguments and call <:
        if length(fargs) == 3
            fargs = Any[<:, fargs[3], fargs[2]]
        else
            fargs = ()
        end
        argtypes = Any[typeof(<:), argtypes[3], argtypes[2]]
        rty = abstract_call(<:, fargs, argtypes, vtypes, sv)
        return rty
    elseif length(argtypes) == 2 && isa(argtypes[2], Const) && isa(argtypes[2].val, SimpleVector) && istopfunction(f, :length)
        # mark length(::SimpleVector) as @pure
        return Const(length(argtypes[2].val))
    elseif length(argtypes) == 3 && isa(argtypes[2], Const) && isa(argtypes[3], Const) &&
            isa(argtypes[2].val, SimpleVector) && isa(argtypes[3].val, Int) && istopfunction(f, :getindex)
        # mark getindex(::SimpleVector, i::Int) as @pure
        svecval = argtypes[2].val::SimpleVector
        idx = argtypes[3].val::Int
        if 1 <= idx <= length(svecval) && isassigned(svecval, idx)
            return Const(getindex(svecval, idx))
        end
    elseif length(argtypes) == 2 && istopfunction(f, :typename)
        return typename_static(argtypes[2])
    end

    atype = argtypes_to_type(argtypes)
    t = pure_eval_call(f, argtypes, atype, sv)
    t !== false && return t

    if istopfunction(f, :typejoin) || f === return_type
        return Type # don't try to infer these function edges directly -- it won't actually come up with anything useful
    end

    if sv.params.inlining
        # need to model the special inliner for ^
        # to ensure we have added the same edge
        if isdefined(Main, :Base) &&
            ((isdefined(Main.Base, :^) && f === Main.Base.:^) ||
             (isdefined(Main.Base, :.^) && f === Main.Base.:.^)) &&
            length(argtypes) == 3 && (argtypes[3] ⊑ Int32 || argtypes[3] ⊑ Int64)

            a1 = argtypes[2]
            basenumtype = Union{CoreNumType, Main.Base.ComplexF32, Main.Base.ComplexF64, Main.Base.Rational}
            if a1 ⊑ basenumtype
                ftimes = Main.Base.:*
                ta1 = widenconst(a1)
                abstract_call_gf_by_type(ftimes, Any[ftimes, a1, a1], Tuple{typeof(ftimes), ta1, ta1}, sv)
            end
        end
    end
    return abstract_call_gf_by_type(f, argtypes, atype, sv)
end

# wrapper around `abstract_call` for first computing if `f` is available
function abstract_eval_call(fargs::Union{Tuple{},Vector{Any}}, argtypes::Vector{Any}, vtypes::VarTable, sv::InferenceState)
    #print("call ", e.args[1], argtypes, "\n\n")
    for x in argtypes
        x === Bottom && return Bottom
    end
    ft = argtypes[1]
    if isa(ft, Const)
        f = ft.val
    elseif isconstType(ft)
        f = ft.parameters[1]
    elseif isa(ft, DataType) && isdefined(ft, :instance)
        f = ft.instance
    else
        for i = 2:(length(argtypes) - 1)
            if isvarargtype(argtypes[i])
                return Any
            end
        end
        # non-constant function, but the number of arguments is known
        # and the ft is not a Builtin or IntrinsicFunction
        if typeintersect(widenconst(ft), Builtin) != Union{}
            return Any
        end
        return abstract_call_gf_by_type(nothing, argtypes, argtypes_to_type(argtypes), sv)
    end
    return abstract_call(f, fargs, argtypes, vtypes, sv)
end

function sp_type_rewrap(@nospecialize(T), linfo::MethodInstance, isreturn::Bool)
    isref = false
    if T === Bottom
        return Bottom
    elseif isa(T, Type)
        if isa(T, DataType) && (T::DataType).name === _REF_NAME
            isref = true
            T = T.parameters[1]
            if isreturn && T === Any
                return Bottom # a return type of Ref{Any} is invalid
            end
        end
    else
        return Any
    end
    if isa(linfo.def, Method)
        spsig = linfo.def.sig
        if isa(spsig, UnionAll)
            if !isempty(linfo.sparam_vals)
                env = pointer_from_objref(linfo.sparam_vals) + sizeof(Ptr{Cvoid})
                T = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), T, spsig, env)
                isref && isreturn && T === Any && return Bottom # catch invalid return Ref{T} where T = Any
                for v in linfo.sparam_vals
                    if isa(v, TypeVar)
                        T = UnionAll(v, T)
                    end
                end
            else
                T = rewrap_unionall(T, spsig)
            end
        end
    end
    while isa(T, TypeVar)
        T = T.ub
    end
    return T
end

function abstract_eval_cfunction(e::Expr, vtypes::VarTable, sv::InferenceState)
    f = abstract_eval(e.args[2], vtypes, sv)
    # rt = sp_type_rewrap(e.args[3], sv.linfo, true)
    at = Any[ sp_type_rewrap(argt, sv.linfo, false) for argt in e.args[4]::SimpleVector ]
    pushfirst!(at, f)
    # this may be the wrong world for the call,
    # but some of the result is likely to be valid anyways
    # and that may help generate better codegen
    abstract_eval_call((), at, vtypes, sv)
    nothing
end

# convert an inferred static parameter value to the inferred type of a static_parameter expression
function sparam_type(@nospecialize(val))
    if isa(val, TypeVar)
        if Any <: val.ub
            # static param bound to typevar
            # if the tvar is not known to refer to anything more specific than Any,
            # the static param might actually be an integer, symbol, etc.
            return Any
        else
            return UnionAll(val, Type{val})
        end
    end
    return AbstractEvalConstant(val)
end

function abstract_eval(@nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if isa(e, QuoteNode)
        return AbstractEvalConstant((e::QuoteNode).value)
    elseif isa(e, SSAValue)
        return abstract_eval_ssavalue(e::SSAValue, sv.src)
    elseif isa(e, Slot)
        return vtypes[slot_id(e)].typ
    elseif isa(e, Symbol)
        return abstract_eval_global(sv.mod, e)
    elseif isa(e,GlobalRef)
        return abstract_eval_global(e.mod, e.name)
    end

    if !isa(e, Expr)
        return AbstractEvalConstant(e)
    end
    e = e::Expr
    if e.head === :call
        argtypes = Any[ abstract_eval(a, vtypes, sv) for a in e.args ]
        t = abstract_eval_call(e.args, argtypes, vtypes, sv)
    elseif e.head === :new
        t = instanceof_tfunc(abstract_eval(e.args[1], vtypes, sv))[1]
        for i = 2:length(e.args)
            if abstract_eval(e.args[i], vtypes, sv) === Bottom
                rt = Bottom
            end
        end
    elseif e.head === :&
        abstract_eval(e.args[1], vtypes, sv)
        t = Any
    elseif e.head === :foreigncall
        abstract_eval(e.args[1], vtypes, sv)
        t = sp_type_rewrap(e.args[2], sv.linfo, true)
        for i = 3:length(e.args)
            if abstract_eval(e.args[i], vtypes, sv) === Bottom
                t = Bottom
            end
        end
    elseif e.head === :cfunction
        t = e.args[1]
        isa(t, Type) || (t = Any)
        abstract_eval_cfunction(e, vtypes, sv)
    elseif e.head === :static_parameter
        n = e.args[1]
        t = Any
        if 1 <= n <= length(sv.sp)
            t = sparam_type(sv.sp[n])
        end
    elseif e.head === :method
        t = (length(e.args) == 1) ? Any : Nothing
    elseif e.head === :copyast
        t = abstract_eval(e.args[1], vtypes, sv)
        if t isa Const && t.val isa Expr
            # `copyast` makes copies of Exprs
            t = Expr
        end
    elseif e.head === :invoke
        error("type inference data-flow error: tried to double infer a function")
    elseif e.head === :boundscheck
        return Bool
    elseif e.head === :isdefined
        sym = e.args[1]
        t = Bool
        if isa(sym, Slot)
            vtyp = vtypes[slot_id(sym)]
            if vtyp.typ === Bottom
                t = Const(false) # never assigned previously
            elseif !vtyp.undef
                t = Const(true) # definitely assigned previously
            end
        elseif isa(sym, Symbol)
            if isdefined(sv.mod, sym.name)
                t = Const(true)
            end
        elseif isa(sym, GlobalRef)
            if isdefined(sym.mod, sym.name)
                t = Const(true)
            end
        elseif isa(sym, Expr) && sym.head === :static_parameter
            n = sym.args[1]
            if 1 <= n <= length(sv.sp)
                val = sv.sp[n]
                if !isa(val, TypeVar)
                    t = Const(true)
                end
            end
        end
    else
        t = Any
    end
    @assert !isa(t, TypeVar)
    if isa(t, DataType) && isdefined(t, :instance)
        # replace singleton types with their equivalent Const object
        t = Const(t.instance)
    end
    return t
end

function abstract_eval_global(M::Module, s::Symbol)
    if isdefined(M,s) && isconst(M,s)
        return AbstractEvalConstant(getfield(M,s))
    end
    return Any
end

function abstract_eval_ssavalue(s::SSAValue, src::CodeInfo)
    typ = src.ssavaluetypes[s.id]
    if typ === NOT_FOUND
        return Bottom
    end
    return typ
end

# determine whether `ex` abstractly evals to constant `c`
function abstract_evals_to_constant(@nospecialize(ex), @nospecialize(c), vtypes::VarTable, sv::InferenceState)
    av = abstract_eval(ex, vtypes, sv)
    return isa(av,Const) && av.val === c
end

# make as much progress on `frame` as possible (without handling cycles)
function typeinf_local(frame::InferenceState)
    @assert !frame.inferred
    frame.dont_work_on_me = true # mark that this function is currently on the stack
    W = frame.ip
    s = frame.stmt_types
    n = frame.nstmts
    while frame.pc´´ <= n
        # make progress on the active ip set
        local pc::Int = frame.pc´´ # current program-counter
        while true # inner loop optimizes the common case where it can run straight from pc to pc + 1
            #print(pc,": ",s[pc],"\n")
            local pc´::Int = pc + 1 # next program-counter (after executing instruction)
            if pc == frame.pc´´
                # need to update pc´´ to point at the new lowest instruction in W
                min_pc = _bits_findnext(W.bits, pc + 1)
                frame.pc´´ = min_pc == -1 ? n + 1 : min_pc
            end
            delete!(W, pc)
            frame.currpc = pc
            frame.cur_hand = frame.handler_at[pc]
            frame.stmt_edges[pc] === () || empty!(frame.stmt_edges[pc])
            stmt = frame.src.code[pc]
            changes = s[pc]::VarTable
            t = nothing

            hd = isa(stmt, Expr) ? stmt.head : nothing

            if isa(stmt, NewvarNode)
                sn = slot_id(stmt.slot)
                changes[sn] = VarState(Bottom, true)
            elseif isa(stmt, GotoNode)
                pc´ = (stmt::GotoNode).label
            elseif hd === :gotoifnot
                condt = abstract_eval(stmt.args[1], s[pc], frame)
                if condt === Bottom
                    break
                end
                condval = maybe_extract_const_bool(condt)
                l = stmt.args[2]::Int
                # constant conditions
                if condval === true
                elseif condval === false
                    pc´ = l
                else
                    # general case
                    frame.handler_at[l] = frame.cur_hand
                    changes_else = changes
                    if isa(condt, Conditional)
                        if condt.elsetype !== Any && condt.elsetype !== changes[slot_id(condt.var)]
                            changes_else = StateUpdate(condt.var, VarState(condt.elsetype, false), changes_else)
                        end
                        if condt.vtype !== Any && condt.vtype !== changes[slot_id(condt.var)]
                            changes = StateUpdate(condt.var, VarState(condt.vtype, false), changes)
                        end
                    end
                    newstate_else = stupdate!(s[l], changes_else)
                    if newstate_else !== false
                        # add else branch to active IP list
                        if l < frame.pc´´
                            frame.pc´´ = l
                        end
                        push!(W, l)
                        s[l] = newstate_else
                    end
                end
            elseif hd === :return
                pc´ = n + 1
                rt = abstract_eval(stmt.args[1], s[pc], frame)
                if !isa(rt, Const) && !isa(rt, Type)
                    # only propagate information we know we can store
                    # and is valid inter-procedurally
                    rt = widenconst(rt)
                end
                if tchanged(rt, frame.bestguess)
                    # new (wider) return type for frame
                    frame.bestguess = tmerge(frame.bestguess, rt)
                    for (caller, caller_pc) in frame.cycle_backedges
                        # notify backedges of updated type information
                        if caller.stmt_types[caller_pc] !== ()
                            if caller_pc < caller.pc´´
                                caller.pc´´ = caller_pc
                            end
                            push!(caller.ip, caller_pc)
                        end
                    end
                end
            elseif hd === :enter
                l = stmt.args[1]::Int
                frame.cur_hand = (l, frame.cur_hand)
                # propagate type info to exception handler
                l = frame.cur_hand[1]
                old = s[l]
                new = s[pc]::Array{Any,1}
                newstate_catch = stupdate!(old, new)
                if newstate_catch !== false
                    if l < frame.pc´´
                        frame.pc´´ = l
                    end
                    push!(W, l)
                    s[l] = newstate_catch
                end
                typeassert(s[l], VarTable)
                frame.handler_at[l] = frame.cur_hand
            elseif hd === :leave
                for i = 1:((stmt.args[1])::Int)
                    frame.cur_hand = frame.cur_hand[2]
                end
            else
                if hd === :(=)
                    t = abstract_eval(stmt.args[2], changes, frame)
                    t === Bottom && break
                    frame.src.ssavaluetypes[pc] = t
                    lhs = stmt.args[1]
                    if isa(lhs, Slot)
                        changes = StateUpdate(lhs, VarState(t, false), changes)
                    end
                elseif hd === :method
                    fname = stmt.args[1]
                    if isa(fname, Slot)
                        changes = StateUpdate(fname, VarState(Any, false), changes)
                    end
                elseif hd === :inbounds || hd === :meta || hd === :simdloop
                else
                    t = abstract_eval(stmt, changes, frame)
                    t === Bottom && break
                    if !isempty(frame.ssavalue_uses[pc])
                        record_ssa_assign(pc, t, frame)
                    else
                        frame.src.ssavaluetypes[pc] = t
                    end
                end
                if frame.cur_hand !== () && isa(changes, StateUpdate)
                    # propagate new type info to exception handler
                    # the handling for Expr(:enter) propagates all changes from before the try/catch
                    # so this only needs to propagate any changes
                    l = frame.cur_hand[1]
                    if stupdate1!(s[l]::VarTable, changes::StateUpdate) !== false
                        if l < frame.pc´´
                            frame.pc´´ = l
                        end
                        push!(W, l)
                    end
                end
            end

            if t === nothing
                # mark other reached expressions as `Any` to indicate they don't throw
                frame.src.ssavaluetypes[pc] = Any
            end

            pc´ > n && break # can't proceed with the fast-path fall-through
            frame.handler_at[pc´] = frame.cur_hand
            newstate = stupdate!(s[pc´], changes)
            if isa(stmt, GotoNode) && frame.pc´´ < pc´
                # if we are processing a goto node anyways,
                # (such as a terminator for a loop, if-else, or try block),
                # consider whether we should jump to an older backedge first,
                # to try to traverse the statements in approximate dominator order
                if newstate !== false
                    s[pc´] = newstate
                end
                push!(W, pc´)
                pc = frame.pc´´
            elseif newstate !== false
                s[pc´] = newstate
                pc = pc´
            elseif pc´ in W
                pc = pc´
            else
                break
            end
        end
    end
    frame.dont_work_on_me = false
    nothing
end

# make as much progress on `frame` as possible (by handling cycles)
function typeinf_nocycle(frame::InferenceState)
    typeinf_local(frame)

    # If the current frame is part of a cycle, solve the cycle before finishing
    no_active_ips_in_callers = false
    while !no_active_ips_in_callers
        no_active_ips_in_callers = true
        for caller in frame.callers_in_cycle
            caller.dont_work_on_me && return false # cycle is above us on the stack
            if caller.pc´´ <= caller.nstmts # equivalent to `isempty(caller.ip)`
                # Note that `typeinf_local(caller)` can potentially modify the other frames
                # `frame.callers_in_cycle`, which is why making incremental progress requires the
                # outer while loop.
                typeinf_local(caller)
                no_active_ips_in_callers = false
            end
            if caller.min_valid < frame.min_valid
                caller.min_valid = frame.min_valid
            end
            if caller.max_valid > frame.max_valid
                caller.max_valid = frame.max_valid
            end
        end
    end
    return true
end
