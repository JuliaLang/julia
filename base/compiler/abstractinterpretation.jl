# This file is a part of Julia. License is MIT: https://julialang.org/license

#############
# constants #
#############

const CoreNumType = Union{Int32, Int64, Float32, Float64}

const _REF_NAME = Ref.body.name

#########
# logic #
#########

# see if the inference result might affect the final answer
call_result_unused(frame::InferenceState, pc::LineNum=frame.currpc) =
    isexpr(frame.src.code[frame.currpc], :call) && isempty(frame.ssavalue_uses[pc])

function abstract_call_gf_by_type(@nospecialize(f), argtypes::Vector{Any}, @nospecialize(atype), sv::InferenceState,
                                  max_methods = sv.params.MAX_METHODS)
    atype_params = unwrap_unionall(atype).parameters
    ft = unwrap_unionall(atype_params[1]) # TODO: ccall jl_method_table_for here
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
            xapplicable = _methods_by_ftype(sig_n, max_methods, sv.params.world, min_valid, max_valid)
            xapplicable === false && return Any
            append!(applicable, xapplicable)
        end
    else
        applicable = _methods_by_ftype(atype, max_methods, sv.params.world, min_valid, max_valid)
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
    edges = Any[]
    nonbot = 0  # the index of the only non-Bottom inference result if > 0
    seen = 0    # number of signatures actually inferred
    istoplevel = sv.linfo.def isa Module
    multiple_matches = napplicable > 1
    for i in 1:napplicable
        match = applicable[i]::SimpleVector
        method = match[3]::Method
        sig = match[1]
        if istoplevel && !isdispatchtuple(sig)
            # only infer concrete call sites in top-level expressions
            rettype = Any
            break
        end
        sigtuple = unwrap_unionall(sig)::DataType
        splitunions = false
        this_rt = Bottom
        # TODO: splitunions = 1 < countunionsplit(sigtuple.parameters) * napplicable <= sv.params.MAX_UNION_SPLITTING
        # currently this triggers a bug in inference recursion detection
        if splitunions
            splitsigs = switchtupleunion(sig)
            for sig_n in splitsigs
                rt, edgecycle1, edge = abstract_call_method(method, sig_n, svec(), multiple_matches, sv)
                if edge !== nothing
                    push!(edges, edge)
                end
                edgecycle |= edgecycle1::Bool
                this_rt = tmerge(this_rt, rt)
                this_rt === Any && break
            end
        else
            this_rt, edgecycle1, edge = abstract_call_method(method, sig, match[2]::SimpleVector, multiple_matches, sv)
            edgecycle |= edgecycle1::Bool
            if edge !== nothing
                push!(edges, edge)
            end
        end
        if this_rt !== Bottom
            if nonbot === 0
                nonbot = i
            else
                nonbot = -1
            end
        end
        seen += 1
        rettype = tmerge(rettype, this_rt)
        rettype === Any && break
    end
    # try constant propagation if only 1 method is inferred to non-Bottom
    # this is in preparation for inlining, or improving the return result
    if nonbot > 0 && seen == napplicable && !edgecycle && isa(rettype, Type) && sv.params.ipo_constant_propagation
        # if there's a possibility we could constant-propagate a better result
        # (hopefully without doing too much work), try to do that now
        # TODO: it feels like this could be better integrated into abstract_call_method / typeinf_edge
        const_rettype = abstract_call_method_with_const_args(rettype, f, argtypes, applicable[nonbot]::SimpleVector, sv)
        if const_rettype ⊑ rettype
            # use the better result, if it's a refinement of rettype
            rettype = const_rettype
        end
    end
    if call_result_unused(sv) && !(rettype === Bottom)
        # We're mainly only here because the optimizer might want this code,
        # but we ourselves locally don't typically care about it locally
        # (beyond checking if it always throws).
        # So avoid adding an edge, since we don't want to bother attempting
        # to improve our result even if it does change (to always throw),
        # and avoid keeping track of a more complex result type.
        rettype = Any
    end
    if !(rettype === Any) # adding a new method couldn't refine (widen) this type
        for edge in edges
            add_backedge!(edge::MethodInstance, sv)
        end
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


function const_prop_profitable(@nospecialize(arg))
    # have new information from argtypes that wasn't available from the signature
    if isa(arg, PartialStruct)
        for b in arg.fields
            isconstType(b) && return true
            const_prop_profitable(b) && return true
        end
    elseif !isa(arg, Const) || (isa(arg.val, Symbol) || isa(arg.val, Type) || (!isa(arg.val, String) && isimmutable(arg.val)))
        # don't consider mutable values or Strings useful constants
        return true
    end
    return false
end

function abstract_call_method_with_const_args(@nospecialize(rettype), @nospecialize(f), argtypes::Vector{Any}, match::SimpleVector, sv::InferenceState)
    method = match[3]::Method
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    length(argtypes) >= nargs || return Any
    haveconst = false
    allconst = true
    # see if any or all of the arguments are constant and propagating constants may be worthwhile
    for a in argtypes
        a = widenconditional(a)
        if allconst && !isa(a, Const) && !isconstType(a) && !isa(a, PartialStruct)
            allconst = false
        end
        if !haveconst && has_nontrivial_const_info(a) && const_prop_profitable(a)
            haveconst = true
        end
        if haveconst && !allconst
            break
        end
    end
    haveconst || improvable_via_constant_propagation(rettype) || return Any
    if nargs > 1
        if istopfunction(f, :getindex) || istopfunction(f, :setindex!)
            arrty = argtypes[2]
            # don't propagate constant index into indexing of non-constant array
            if arrty isa Type && arrty <: AbstractArray && !issingletontype(arrty)
                return Any
            end
        elseif istopfunction(f, :iterate)
            itrty = argtypes[2]
            if itrty isa Type && !issingletontype(itrty)
                return Any
            end
        end
    end
    if !allconst && (istopfunction(f, :+) || istopfunction(f, :-) || istopfunction(f, :*) ||
                     istopfunction(f, :(==)) || istopfunction(f, :!=) ||
                     istopfunction(f, :<=) || istopfunction(f, :>=) || istopfunction(f, :<) || istopfunction(f, :>) ||
                     istopfunction(f, :<<) || istopfunction(f, :>>))
        return Any
    end
    force_inference = allconst || sv.params.aggressive_constant_propagation
    if istopfunction(f, :getproperty) || istopfunction(f, :setproperty!)
        force_inference = true
    end
    sig = match[1]
    sparams = match[2]::SimpleVector
    mi = specialize_method(method, sig, sparams, !force_inference)
    mi === nothing && return Any
    mi = mi::MethodInstance
    # decide if it's likely to be worthwhile
    if !force_inference
        code = inf_for_methodinstance(mi, sv.params.world)
        declared_inline = isdefined(method, :source) && ccall(:jl_ast_flag_inlineable, Bool, (Any,), method.source)
        cache_inlineable = declared_inline
        if isdefined(code, :inferred) && !cache_inlineable
            cache_inf = code.inferred
            if !(cache_inf === nothing)
                cache_src_inferred = ccall(:jl_ast_flag_inferred, Bool, (Any,), cache_inf)
                cache_src_inlineable = ccall(:jl_ast_flag_inlineable, Bool, (Any,), cache_inf)
                cache_inlineable = cache_src_inferred && cache_src_inlineable
            end
        end
        if !cache_inlineable
            return Any
        end
    end
    inf_result = cache_lookup(mi, argtypes, sv.params.cache)
    if inf_result === nothing
        inf_result = InferenceResult(mi, argtypes)
        frame = InferenceState(inf_result, #=cache=#false, sv.params)
        frame.limited = true
        frame.parent = sv
        push!(sv.params.cache, inf_result)
        typeinf(frame) || return Any
    end
    result = inf_result.result
    isa(result, InferenceState) && return Any # TODO: unexpected, is this recursive constant inference?
    add_backedge!(inf_result.linfo, sv)
    return result
end

function abstract_call_method(method::Method, @nospecialize(sig), sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)
    if method.name === :depwarn && isdefined(Main, :Base) && method.module === Main.Base
        return Any, false, nothing
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
    method2 = method_for_inference_heuristics(method, sig, sparams) # Union{Method, Nothing}
    sv_method2 = sv.src.method_for_inference_limit_heuristics # limit only if user token match
    sv_method2 isa Method || (sv_method2 = nothing) # Union{Method, Nothing}
    while !(infstate === nothing)
        infstate = infstate::InferenceState
        if method === infstate.linfo.def
            if infstate.linfo.specTypes == sig
                # avoid widening when detecting self-recursion
                # TODO: merge call cycle and return right away
                if call_result_unused(sv)
                    # since we don't use the result (typically),
                    # we have a self-cycle in the call-graph, but not in the inference graph (typically):
                    # break this edge now (before we record it) by returning early
                    # (non-typically, this means that we lose the ability to detect a guaranteed StackOverflow in some cases)
                    return Any, true, nothing
                end
                topmost = nothing
                edgecycle = true
                break
            end
            inf_method2 = infstate.src.method_for_inference_limit_heuristics # limit only if user token match
            inf_method2 isa Method || (inf_method2 = nothing) # Union{Method, Nothing}
            if topmost === nothing && method2 === inf_method2
                if hardlimit
                    topmost = infstate
                    edgecycle = true
                else
                    # if this is a soft limit,
                    # also inspect the parent of this edge,
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
                            if (parent.cached || parent.limited) && parent.linfo.def === sv.linfo.def && sv_method2 === parent_method2
                                topmost = infstate
                                edgecycle = true
                            end
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
        newsig = limit_type_size(sig, comparison, hardlimit ? comparison : sv.linfo.specTypes, sv.params.TUPLE_COMPLEXITY_LIMIT_DEPTH, spec_len)

        if newsig !== sig
            # continue inference, but note that we've limited parameter complexity
            # on this call (to ensure convergence), so that we don't cache this result
            if call_result_unused(sv)
                # if we don't (typically) actually care about this result,
                # don't bother trying to examine some complex abstract signature
                # since it's very unlikely that we'll try to inline this,
                # or want make an invoke edge to its calling convention return type.
                # (non-typically, this means that we lose the ability to detect a guaranteed StackOverflow in some cases)
                return Any, true, nothing
            end
            poison_callstack(sv, topmost::InferenceState, true)
            sig = newsig
            sparams = svec()
        end
    end

    # if sig changed, may need to recompute the sparams environment
    if isa(method.sig, UnionAll) && isempty(sparams)
        recomputed = ccall(:jl_type_intersection_with_env, Any, (Any, Any), sig, method.sig)::SimpleVector
        #@assert recomputed[1] !== Bottom
        # We must not use `sig` here, since that may re-introduce structural complexity that
        # our limiting heuristic sought to eliminate. The alternative would be to not increment depth over covariant contexts,
        # but we prefer to permit inference of tuple-destructuring, so we don't do that right now
        # For example, with a signature such as `Tuple{T, Ref{T}} where {T <: S}`
        # we might want to limit this to `Tuple{S, Ref}`, while type-intersection can instead give us back the original type
        # (which moves `S` back up to a lower comparison depth)
        # Optionally, we could try to drive this to a fixed point, but I think this is getting too complex,
        # and this would only cause more questions and more problems
        # (the following is only an example, most of the statements are probable in the wrong order):
        #     newsig = sig
        #     seen = IdSet()
        #     while !(newsig in seen)
        #         push!(seen, newsig)
        #         lsig = length((unwrap_unionall(sig)::DataType).parameters)
        #         newsig = limit_type_size(newsig, sig, sv.linfo.specTypes, sv.params.TUPLE_COMPLEXITY_LIMIT_DEPTH, lsig)
        #         recomputed = ccall(:jl_type_intersection_with_env, Any, (Any, Any), newsig, method.sig)::SimpleVector
        #         newsig = recomputed[2]
        #     end
        #     sig = ?
        sparams = recomputed[2]::SimpleVector
    end

    rt, edge = typeinf_edge(method, sig, sparams, sv)
    if edge === nothing
        edgecycle = true
    end
    return rt, edgecycle, edge
end

# This is only for use with `Conditional`.
# In general, usage of this is wrong.
function ssa_def_slot(@nospecialize(arg), sv::InferenceState)
    init = sv.currpc
    while isa(arg, SSAValue)
        init = arg.id
        arg = sv.src.code[init]
    end
    arg isa SlotNumber || return nothing
    for i = init:(sv.currpc - 1)
        # conservatively make sure there isn't potentially another conflicting assignment to
        # the same slot between the def and usage
        # we can assume the IR is sorted, since the front-end only creates SSA values in order
        e = sv.src.code[i]
        e isa Expr || continue
        if e.head === :(=) && e.args[1] === arg
            return nothing
        end
    end
    return arg
end

# `typ` is the inferred type for expression `arg`.
# if the expression constructs a container (e.g. `svec(x,y,z)`),
# refine its type to an array of element types.
# Union of Tuples of the same length is converted to Tuple of Unions.
# returns an array of types
function precise_container_type(@nospecialize(typ), vtypes::VarTable, sv::InferenceState)
    if isa(typ, PartialStruct) && typ.typ.name === Tuple.name
        return typ.fields
    end

    if isa(typ, Const)
        val = typ.val
        if isa(val, SimpleVector) || isa(val, Tuple)
            return Any[ Const(val[i]) for i in 1:length(val) ] # avoid making a tuple Generator here!
        end
    end

    tti0 = widenconst(typ)
    tti = unwrap_unionall(tti0)
    if isa(tti, DataType) && tti.name === NamedTuple_typename
        # A NamedTuple iteration is the the same as the iteration of its Tuple parameter:
        # compute a new `tti == unwrap_unionall(tti0)` based on that Tuple type
        tti = tti.parameters[2]
        while isa(tti, TypeVar)
            tti = tti.ub
        end
        tti0 = rewrap_unionall(tti, tti0)
    end
    if isa(tti, Union)
        utis = uniontypes(tti)
        if _any(t -> !isa(t, DataType) || !(t <: Tuple) || !isknownlength(t), utis)
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
    elseif tti0 <: Tuple
        if isa(tti0, DataType)
            if isvatuple(tti0) && length(tti0.parameters) == 1
                return Any[Vararg{unwrapva(tti0.parameters[1])}]
            else
                return Any[ p for p in tti0.parameters ]
            end
        elseif !isa(tti, DataType)
            return Any[Vararg{Any}]
        else
            len = length(tti.parameters)
            last = tti.parameters[len]
            va = isvarargtype(last)
            elts = Any[ fieldtype(tti0, i) for i = 1:len ]
            if va
                elts[len] = Vararg{elts[len]}
            end
            return elts
        end
    elseif tti0 === SimpleVector || tti0 === Any
        return Any[Vararg{Any}]
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
    stateordonet = abstract_call(iteratef, nothing, Any[Const(iteratef), itertype], vtypes, sv)
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
        stateordonet = abstract_call(iteratef, nothing, Any[Const(iteratef), itertype, statetype], vtypes, sv)
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
        stateordonet = abstract_call(iteratef, nothing, Any[Const(iteratef), itertype, statetype], vtypes, sv)
        stateordonet = widenconst(stateordonet)
    end
    push!(ret, Vararg{valtype})
    return ret
end

# do apply(af, fargs...), where af is a function value
function abstract_apply(@nospecialize(aft), aargtypes::Vector{Any}, vtypes::VarTable, sv::InferenceState,
                        max_methods = sv.params.MAX_METHODS)
    aftw = widenconst(aft)
    if !isa(aft, Const) && (!isType(aftw) || has_free_typevars(aftw))
        if !isconcretetype(aftw) || (aftw <: Builtin)
            # non-constant function of unknown type: bail now,
            # since it seems unlikely that abstract_call will be able to do any better after splitting
            # this also ensures we don't call abstract_call_gf_by_type below on an IntrinsicFunction or Builtin
            return Any
        end
    end
    res = Union{}
    nargs = length(aargtypes)
    splitunions = 1 < countunionsplit(aargtypes) <= sv.params.MAX_APPLY_UNION_ENUM
    ctypes = Any[Any[aft]]
    for i = 1:nargs
        ctypes´ = []
        for ti in (splitunions ? uniontypes(aargtypes[i]) : Any[aargtypes[i]])
            cti = precise_container_type(ti, vtypes, sv)
            if _any(t -> t === Bottom, cti)
                continue
            end
            for ct in ctypes
                if isvarargtype(ct[end])
                    tail = tuple_tail_elem(unwrapva(ct[end]), cti)
                    push!(ctypes´, push!(ct[1:(end - 1)], tail))
                else
                    push!(ctypes´, append!(ct[:], cti))
                end
            end
        end
        ctypes = ctypes´
    end
    for ct in ctypes
        if isa(aft, Const)
            rt = abstract_call(aft.val, nothing, ct, vtypes, sv, max_methods)
        elseif isconstType(aft)
            rt = abstract_call(aft.parameters[1], nothing, ct, vtypes, sv, max_methods)
        else
            astype = argtypes_to_type(ct)
            rt = abstract_call_gf_by_type(nothing, ct, astype, sv, max_methods)
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
        a = widenconditional(argtypes[i])
        if !(isa(a, Const) || isconstType(a))
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
    sig = meth[1]::DataType
    sparams = meth[2]::SimpleVector
    method = meth[3]::Method

    if isdefined(method, :generator)
        method.generator.expand_early || return false
        mi = specialize_method(method, sig, sparams, false)
        isa(mi, MethodInstance) || return false
        staged = get_staged(mi)
        (staged isa CodeInfo && (staged::CodeInfo).pure) || return false
    elseif !method.pure
        return false
    end

    args = Any[ (a = widenconditional(argtypes[i]); isa(a, Const) ? a.val : a.parameters[1]) for i in 2:length(argtypes) ]
    try
        value = Core._apply_pure(f, args)
        # TODO: add some sort of edge(s)
        return Const(value, true)
    catch
        return false
    end
end

function abstract_call(@nospecialize(f), fargs::Union{Nothing,Vector{Any}}, argtypes::Vector{Any}, vtypes::VarTable, sv::InferenceState, max_methods = sv.params.MAX_METHODS)
    if f === _apply
        return abstract_apply(argtypes[2], argtypes[3:end], vtypes, sv, max_methods)
    end

    la = length(argtypes)
    for i = 2:(la - 1)
        if isvarargtype(argtypes[i])
            return Any
        end
    end

    if isa(f, Builtin) || isa(f, IntrinsicFunction)
        if f === ifelse && fargs isa Vector{Any} && length(argtypes) == 4 && argtypes[2] isa Conditional
            # try to simulate this as a real conditional (`cnd ? x : y`), so that the penalty for using `ifelse` instead isn't too high
            cnd = argtypes[2]::Conditional
            tx = argtypes[3]
            ty = argtypes[4]
            a = ssa_def_slot(fargs[3], sv)
            b = ssa_def_slot(fargs[4], sv)
            if isa(a, Slot) && slot_id(cnd.var) == slot_id(a)
                tx = typeintersect(tx, cnd.vtype)
            end
            if isa(b, Slot) && slot_id(cnd.var) == slot_id(b)
                ty = typeintersect(ty, cnd.elsetype)
            end
            return tmerge(tx, ty)
        end
        rt = builtin_tfunction(f, argtypes[2:end], sv)
        if f === getfield && isa(fargs, Vector{Any}) && length(argtypes) == 3 && isa(argtypes[3], Const) && isa(argtypes[3].val, Int) && argtypes[2] ⊑ Tuple
            cti = precise_container_type(argtypes[2], vtypes, sv)
            idx = argtypes[3].val
            if 1 <= idx <= length(cti)
                rt = unwrapva(cti[idx])
            end
        elseif (rt === Bool || (isa(rt, Const) && isa(rt.val, Bool))) && isa(fargs, Vector{Any})
            # perform very limited back-propagation of type information for `is` and `isa`
            if f === isa
                a = ssa_def_slot(fargs[2], sv)
                if isa(a, Slot)
                    aty = widenconst(argtypes[2])
                    if rt === Const(false)
                        return Conditional(a, Union{}, aty)
                    elseif rt === Const(true)
                        return Conditional(a, aty, Union{})
                    end
                    tty_ub, isexact_tty = instanceof_tfunc(argtypes[3])
                    if isexact_tty && !isa(tty_ub, TypeVar)
                        tty_lb = tty_ub # TODO: this would be wrong if !isexact_tty, but instanceof_tfunc doesn't preserve this info
                        if !has_free_typevars(tty_lb) && !has_free_typevars(tty_ub)
                            ifty = typeintersect(aty, tty_ub)
                            elty = typesubtract(aty, tty_lb)
                            return Conditional(a, ifty, elty)
                        end
                    end
                end
            elseif f === (===)
                a = ssa_def_slot(fargs[2], sv)
                b = ssa_def_slot(fargs[3], sv)
                aty = argtypes[2]
                bty = argtypes[3]
                # if doing a comparison to a singleton, consider returning a `Conditional` instead
                if isa(aty, Const) && isa(b, Slot)
                    if rt === Const(false)
                        aty = Union{}
                    elseif rt === Const(true)
                        bty = Union{}
                    elseif bty isa Type && isdefined(typeof(aty.val), :instance) # can only widen a if it is a singleton
                        bty = typesubtract(bty, typeof(aty.val))
                    end
                    return Conditional(b, aty, bty)
                end
                if isa(bty, Const) && isa(a, Slot)
                    if rt === Const(false)
                        bty = Union{}
                    elseif rt === Const(true)
                        aty = Union{}
                    elseif aty isa Type && isdefined(typeof(bty.val), :instance) # same for b
                        aty = typesubtract(aty, typeof(bty.val))
                    end
                    return Conditional(a, bty, aty)
                end
                if isa(b, Slot)
                    return Conditional(b, bty, bty)
                end
                if isa(a, Slot)
                    return Conditional(a, aty, aty)
                end
            elseif f === Core.Compiler.not_int
                aty = argtypes[2]
                if isa(aty, Conditional)
                    ifty = aty.elsetype
                    elty = aty.vtype
                    if rt === Const(false)
                        ifty = Union{}
                    elseif rt === Const(true)
                        elty = Union{}
                    end
                    return Conditional(aty.var, ifty, elty)
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
        # Manually look through the definition of TypeVar to
        # make sure to be able to get `PartialTypeVar`s out.
        (length(argtypes) < 2 || length(argtypes) > 4) && return Union{}
        n = argtypes[2]
        ub_var = Const(Any)
        lb_var = Const(Union{})
        if length(argtypes) == 4
            ub_var = argtypes[4]
            lb_var = argtypes[3]
        elseif length(argtypes) == 3
            ub_var = argtypes[3]
        end
        return typevar_tfunc(n, lb_var, ub_var)
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
    elseif is_return_type(f)
        rt_rt = return_type_tfunc(argtypes, vtypes, sv)
        if rt_rt !== nothing
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
            fargs = nothing
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

    if istopfunction(f, :typejoin) || is_return_type(f)
        return Type # don't try to infer these function edges directly -- it won't actually come up with anything useful
    end

    return abstract_call_gf_by_type(f, argtypes, atype, sv, max_methods)
end

# wrapper around `abstract_call` for first computing if `f` is available
function abstract_eval_call(fargs::Union{Nothing,Vector{Any}}, argtypes::Vector{Any}, vtypes::VarTable, sv::InferenceState)
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
    abstract_eval_call(nothing, at, vtypes, sv)
    nothing
end

function abstract_eval(@nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if isa(e, QuoteNode)
        return AbstractEvalConstant((e::QuoteNode).value)
    elseif isa(e, SSAValue)
        return abstract_eval_ssavalue(e::SSAValue, sv.src)
    elseif isa(e, Slot)
        return vtypes[slot_id(e)].typ
    elseif isa(e, GlobalRef)
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
        if isconcretetype(t) && !t.mutable
            args = Vector{Any}(undef, length(e.args)-1)
            ats = Vector{Any}(undef, length(e.args)-1)
            anyconst = false
            allconst = true
            for i = 2:length(e.args)
                at = abstract_eval(e.args[i], vtypes, sv)
                if !anyconst
                    anyconst = has_nontrivial_const_info(at)
                end
                ats[i-1] = at
                if at === Bottom
                    t = Bottom
                    allconst = anyconst = false
                    break
                elseif at isa Const
                    if !(at.val isa fieldtype(t, i - 1))
                        t = Bottom
                        allconst = anyconst = false
                        break
                    end
                    args[i-1] = at.val
                else
                    allconst = false
                end
            end
            # For now, don't allow partially initialized Const/PartialStruct
            if t !== Bottom && fieldcount(t) == length(ats)
                if allconst
                    t = Const(ccall(:jl_new_structv, Any, (Any, Ptr{Cvoid}, UInt32), t, args, length(args)))
                elseif anyconst
                    t = PartialStruct(t, ats)
                end
            end
        end
    elseif e.head === :splatnew
        t = instanceof_tfunc(abstract_eval(e.args[1], vtypes, sv))[1]
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
        if 1 <= n <= length(sv.sptypes)
            t = sv.sptypes[n]
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
            if 1 <= n <= length(sv.sptypes)
                spty = sv.sptypes[n]
                if isa(spty, Const)
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
    return isa(av, Const) && av.val === c
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
            frame.stmt_edges[pc] === nothing || empty!(frame.stmt_edges[pc])
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
                rt = widenconditional(abstract_eval(stmt.args[1], s[pc], frame))
                if !isa(rt, Const) && !isa(rt, Type) && !isa(rt, PartialStruct)
                    # only propagate information we know we can store
                    # and is valid inter-procedurally
                    rt = widenconst(rt)
                elseif isa(rt, Const) && rt.actual
                    rt = Const(rt.val)
                end
                if tchanged(rt, frame.bestguess)
                    # new (wider) return type for frame
                    frame.bestguess = tmerge(frame.bestguess, rt)
                    for (caller, caller_pc) in frame.cycle_backedges
                        # notify backedges of updated type information
                        typeassert(caller.stmt_types[caller_pc], VarTable) # we must have visited this statement before
                        if !(caller.src.ssavaluetypes[caller_pc] === Any)
                            # no reason to revisit if that call-site doesn't affect the final result
                            if caller_pc < caller.pc´´
                                caller.pc´´ = caller_pc
                            end
                            push!(caller.ip, caller_pc)
                        end
                    end
                end
            elseif hd === :enter
                l = stmt.args[1]::Int
                frame.cur_hand = Pair{Any,Any}(l, frame.cur_hand)
                # propagate type info to exception handler
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
                    frame.cur_hand = (frame.cur_hand::Pair{Any,Any}).second
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
                elseif hd === :inbounds || hd === :meta || hd === :loopinfo
                else
                    t = abstract_eval(stmt, changes, frame)
                    t === Bottom && break
                    if !isempty(frame.ssavalue_uses[pc])
                        record_ssa_assign(pc, t, frame)
                    else
                        frame.src.ssavaluetypes[pc] = t
                    end
                end
                if frame.cur_hand !== nothing && isa(changes, StateUpdate)
                    # propagate new type info to exception handler
                    # the handling for Expr(:enter) propagates all changes from before the try/catch
                    # so this only needs to propagate any changes
                    l = frame.cur_hand.first::Int
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
