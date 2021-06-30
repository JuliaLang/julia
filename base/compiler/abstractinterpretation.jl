# This file is a part of Julia. License is MIT: https://julialang.org/license

#############
# constants #
#############

const _REF_NAME = Ref.body.name

#########
# logic #
#########

# See if the inference result of the current statement's result value might affect
# the final answer for the method (aside from optimization potential and exceptions).
# To do that, we need to check both for slot assignment and SSA usage.
call_result_unused(frame::InferenceState) =
    isexpr(frame.src.code[frame.currpc], :call) && isempty(frame.ssavalue_uses[frame.currpc])

# check if this return type is improvable (i.e. whether it's possible that with
# more information, we might get a more precise type)
function is_improvable(@nospecialize(rtype))
    if isa(rtype, Type)
        # Could always be improved to Const or PartialStruct, unless we're
        # already at Bottom
        return rtype !== Union{}
    end
    # Could be improved to `Const` or a more precise wrapper
    return isa(rtype, PartialStruct) || isa(rtype, InterConditional)
end

function abstract_call_gf_by_type(interp::AbstractInterpreter, @nospecialize(f),
                                  fargs::Union{Nothing,Vector{Any}}, argtypes::Vector{Any}, @nospecialize(atype),
                                  sv::InferenceState, max_methods::Int = InferenceParams(interp).MAX_METHODS)
    if sv.params.unoptimize_throw_blocks && sv.currpc in sv.throw_blocks
        add_remark!(interp, sv, "Skipped call in throw block")
        return CallMeta(Any, false)
    end
    valid_worlds = WorldRange()
    # NOTE this is valid as far as any "constant" lattice element doesn't represent `Union` type
    splitunions = 1 < unionsplitcost(argtypes) <= InferenceParams(interp).MAX_UNION_SPLITTING
    mts = Core.MethodTable[]
    fullmatch = Bool[]
    if splitunions
        split_argtypes = switchtupleunion(argtypes)
        applicable = Any[]
        applicable_argtypes = Vector{Any}[] # arrays like `argtypes`, including constants, for each match
        infos = MethodMatchInfo[]
        for arg_n in split_argtypes
            sig_n = argtypes_to_type(arg_n)
            mt = ccall(:jl_method_table_for, Any, (Any,), sig_n)
            if mt === nothing
                add_remark!(interp, sv, "Could not identify method table for call")
                return CallMeta(Any, false)
            end
            mt = mt::Core.MethodTable
            matches = findall(sig_n, method_table(interp); limit=max_methods)
            if matches === missing
                add_remark!(interp, sv, "For one of the union split cases, too many methods matched")
                return CallMeta(Any, false)
            end
            push!(infos, MethodMatchInfo(matches))
            for m in matches
                push!(applicable, m)
                push!(applicable_argtypes, arg_n)
            end
            valid_worlds = intersect(valid_worlds, matches.valid_worlds)
            thisfullmatch = _any(match->(match::MethodMatch).fully_covers, matches)
            found = false
            for (i, mt′) in enumerate(mts)
                if mt′ === mt
                    fullmatch[i] &= thisfullmatch
                    found = true
                    break
                end
            end
            if !found
                push!(mts, mt)
                push!(fullmatch, thisfullmatch)
            end
        end
        info = UnionSplitInfo(infos)
    else
        mt = ccall(:jl_method_table_for, Any, (Any,), atype)
        if mt === nothing
            add_remark!(interp, sv, "Could not identify method table for call")
            return CallMeta(Any, false)
        end
        mt = mt::Core.MethodTable
        matches = findall(atype, method_table(interp, sv); limit=max_methods)
        if matches === missing
            # this means too many methods matched
            # (assume this will always be true, so we don't compute / update valid age in this case)
            add_remark!(interp, sv, "Too many methods matched")
            return CallMeta(Any, false)
        end
        push!(mts, mt)
        push!(fullmatch, _any(match->(match::MethodMatch).fully_covers, matches))
        info = MethodMatchInfo(matches)
        applicable = matches.matches
        valid_worlds = matches.valid_worlds
        applicable_argtypes = nothing
    end
    update_valid_age!(sv, valid_worlds)
    applicable = applicable::Array{Any,1}
    napplicable = length(applicable)
    rettype = Bottom
    edges = MethodInstance[]
    conditionals = nothing # keeps refinement information of call argument types when the return type is boolean
    seen = 0               # number of signatures actually inferred
    any_const_result = false
    const_results = Union{InferenceResult,Nothing}[]
    multiple_matches = napplicable > 1

    if f !== nothing && napplicable == 1 && is_method_pure(applicable[1]::MethodMatch)
        val = pure_eval_call(f, argtypes)
        if val !== false
            # TODO: add some sort of edge(s)
            return CallMeta(val, MethodResultPure(info))
        end
    end

    for i in 1:napplicable
        match = applicable[i]::MethodMatch
        method = match.method
        sig = match.spec_types
        if bail_out_toplevel_call(interp, sig, sv)
            # only infer concrete call sites in top-level expressions
            add_remark!(interp, sv, "Refusing to infer non-concrete call site in top-level expression")
            rettype = Any
            break
        end
        this_rt = Bottom
        splitunions = false
        # TODO: this used to trigger a bug in inference recursion detection, and is unmaintained now
        # sigtuple = unwrap_unionall(sig)::DataType
        # splitunions = 1 < unionsplitcost(sigtuple.parameters) * napplicable <= InferenceParams(interp).MAX_UNION_SPLITTING
        if splitunions
            splitsigs = switchtupleunion(sig)
            for sig_n in splitsigs
                result = abstract_call_method(interp, method, sig_n, svec(), multiple_matches, sv)
                rt, edge = result.rt, result.edge
                if edge !== nothing
                    push!(edges, edge)
                end
                this_argtypes = applicable_argtypes === nothing ? argtypes : applicable_argtypes[i]
                const_rt, const_result = abstract_call_method_with_const_args(interp, result, f, this_argtypes, match, sv, false)
                if const_rt !== rt && const_rt ⊑ rt
                    rt = const_rt
                end
                push!(const_results, const_result)
                if const_result !== nothing
                    any_const_result = true
                end
                this_rt = tmerge(this_rt, rt)
                if bail_out_call(interp, this_rt, sv)
                    break
                end
            end
        else
            result = abstract_call_method(interp, method, sig, match.sparams, multiple_matches, sv)
            this_rt, edge = result.rt, result.edge
            if edge !== nothing
                push!(edges, edge)
            end
            # try constant propagation with argtypes for this match
            # this is in preparation for inlining, or improving the return result
            this_argtypes = applicable_argtypes === nothing ? argtypes : applicable_argtypes[i]
            const_this_rt, const_result = abstract_call_method_with_const_args(interp, result, f, this_argtypes, match, sv, false)
            if const_this_rt !== this_rt && const_this_rt ⊑ this_rt
                this_rt = const_this_rt
            end
            push!(const_results, const_result)
            if const_result !== nothing
                any_const_result = true
            end
        end
        this_conditional = ignorelimited(this_rt)
        this_rt = widenwrappedconditional(this_rt)
        @assert !(this_conditional isa Conditional) "invalid lattice element returned from inter-procedural context"
        seen += 1
        rettype = tmerge(rettype, this_rt)
        if this_conditional !== Bottom && is_lattice_bool(rettype) && fargs !== nothing
            if conditionals === nothing
                conditionals = Any[Bottom for _ in 1:length(argtypes)],
                               Any[Bottom for _ in 1:length(argtypes)]
            end
            condval = maybe_extract_const_bool(this_conditional)
            for i = 1:length(argtypes)
                fargs[i] isa SlotNumber || continue
                if this_conditional isa InterConditional && this_conditional.slot == i
                    vtype = this_conditional.vtype
                    elsetype = this_conditional.elsetype
                else
                    elsetype = vtype = tmeet(argtypes[i], fieldtype(sig, i))
                    condval === true && (elsetype = Union{})
                    condval === false && (vtype = Union{})
                end
                conditionals[1][i] = tmerge(conditionals[1][i], vtype)
                conditionals[2][i] = tmerge(conditionals[2][i], elsetype)
            end
        end
        if bail_out_call(interp, rettype, sv)
            break
        end
    end

    # inliner uses this information only when there is a single match that has been improved
    # by constant analysis, but let's create `ConstCallInfo` if there has been any successful
    # constant propagation happened since other consumers may be interested in this
    if any_const_result && seen == napplicable
        info = ConstCallInfo(info, const_results)
    end

    if rettype isa LimitedAccuracy
        union!(sv.pclimitations, rettype.causes)
        rettype = rettype.typ
    end
    # if we have argument refinement information, apply that now to get the result
    if is_lattice_bool(rettype) && conditionals !== nothing && fargs !== nothing
        slot = 0
        vtype = elsetype = Any
        condval = maybe_extract_const_bool(rettype)
        for i in 1:length(fargs)
            # find the first argument which supports refinment,
            # and intersect all equvalent arguments with it
            arg = fargs[i]
            arg isa SlotNumber || continue # can't refine
            old = argtypes[i]
            old isa Type || continue # unlikely to refine
            id = slot_id(arg)
            if slot == 0 || id == slot
                new_vtype = conditionals[1][i]
                if condval === false
                    vtype = Union{}
                elseif new_vtype ⊑ vtype
                    vtype = new_vtype
                else
                    vtype = tmeet(vtype, widenconst(new_vtype))
                end
                new_elsetype = conditionals[2][i]
                if condval === true
                    elsetype = Union{}
                elseif new_elsetype ⊑ elsetype
                    elsetype = new_elsetype
                else
                    elsetype = tmeet(elsetype, widenconst(new_elsetype))
                end
                if (slot > 0 || condval !== false) && !(old ⊑ vtype) # essentially vtype ⋤ old
                    slot = id
                elseif (slot > 0 || condval !== true) && !(old ⊑ elsetype) # essentially elsetype ⋤ old
                    slot = id
                else # reset: no new useful information for this slot
                    vtype = elsetype = Any
                    if slot > 0
                        slot = 0
                    end
                end
            end
        end
        if vtype === Bottom && elsetype === Bottom
            rettype = Bottom # accidentally proved this call to be dead / throw !
        elseif slot > 0
            rettype = Conditional(SlotNumber(slot), vtype, elsetype) # record a Conditional improvement to this slot
        end
    end
    @assert !(rettype isa InterConditional) "invalid lattice element returned from inter-procedural context"

    if call_result_unused(sv) && !(rettype === Bottom)
        add_remark!(interp, sv, "Call result type was widened because the return value is unused")
        # We're mainly only here because the optimizer might want this code,
        # but we ourselves locally don't typically care about it locally
        # (beyond checking if it always throws).
        # So avoid adding an edge, since we don't want to bother attempting
        # to improve our result even if it does change (to always throw),
        # and avoid keeping track of a more complex result type.
        rettype = Any
    end
    add_call_backedges!(interp, rettype, edges, fullmatch, mts, atype, sv)
    if !isempty(sv.pclimitations) # remove self, if present
        delete!(sv.pclimitations, sv)
        for caller in sv.callers_in_cycle
            delete!(sv.pclimitations, caller)
        end
    end
    #print("=> ", rettype, "\n")
    return CallMeta(rettype, info)
end

function add_call_backedges!(interp::AbstractInterpreter,
                             @nospecialize(rettype),
                             edges::Vector{MethodInstance},
                             fullmatch::Vector{Bool}, mts::Vector{Core.MethodTable}, @nospecialize(atype),
                             sv::InferenceState)
    if rettype === Any
        # for `NativeInterpreter`, we don't add backedges when a new method couldn't refine
        # (widen) this type
        return
    end
    for edge in edges
        add_backedge!(edge, sv)
    end
    for (thisfullmatch, mt) in zip(fullmatch, mts)
        if !thisfullmatch
            # also need an edge to the method table in case something gets
            # added that did not intersect with any existing method
            add_mt_backedge!(mt, atype, sv)
        end
    end
end

const RECURSION_UNUSED_MSG = "Bounded recursion detected with unused result. Annotated return type may be wider than true result."
const RECURSION_MSG = "Bounded recursion detected. Call was widened to force convergence."

function abstract_call_method(interp::AbstractInterpreter, method::Method, @nospecialize(sig), sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)
    if method.name === :depwarn && isdefined(Main, :Base) && method.module === Main.Base
        add_remark!(interp, sv, "Refusing to infer into `depwarn`")
        return MethodCallResult(Any, false, false, nothing)
    end
    topmost = nothing
    # Limit argument type tuple growth of functions:
    # look through the parents list to see if there's a call to the same method
    # and from the same method.
    # Returns the topmost occurrence of that repeated edge.
    edgecycle = false
    edgelimited = false
    # The `method_for_inference_heuristics` will expand the given method's generator if
    # necessary in order to retrieve this field from the generated `CodeInfo`, if it exists.
    # The other `CodeInfo`s we inspect will already have this field inflated, so we just
    # access it directly instead (to avoid regeneration).
    callee_method2 = method_for_inference_heuristics(method, sig, sparams) # Union{Method, Nothing}
    sv_method2 = sv.src.method_for_inference_limit_heuristics # limit only if user token match
    sv_method2 isa Method || (sv_method2 = nothing) # Union{Method, Nothing}

    function matches_sv(parent::InferenceState)
        parent_method2 = parent.src.method_for_inference_limit_heuristics # limit only if user token match
        parent_method2 isa Method || (parent_method2 = nothing) # Union{Method, Nothing}
        return parent.linfo.def === sv.linfo.def && sv_method2 === parent_method2
    end

    function edge_matches_sv(frame::InferenceState)
        inf_method2 = frame.src.method_for_inference_limit_heuristics # limit only if user token match
        inf_method2 isa Method || (inf_method2 = nothing) # Union{Method, Nothing}
        if callee_method2 !== inf_method2
            return false
        end
        if !hardlimit
            # if this is a soft limit,
            # also inspect the parent of this edge,
            # to see if they are the same Method as sv
            # in which case we'll need to ensure it is convergent
            # otherwise, we don't

            # check in the cycle list first
            # all items in here are mutual parents of all others
            if !_any(matches_sv, frame.callers_in_cycle)
                let parent = frame.parent
                    parent !== nothing || return false
                    parent = parent::InferenceState
                    (parent.cached || parent.parent !== nothing) || return false
                    matches_sv(parent) || return false
                end
            end

            # If the method defines a recursion relation, give it a chance
            # to tell us that this recursion is actually ok.
            if isdefined(method, :recursion_relation)
                if Core._apply_pure(method.recursion_relation, Any[method, callee_method2, sig, frame.linfo.specTypes])
                    return false
                end
            end
        end
        return true
    end

    for infstate in InfStackUnwind(sv)
        if method === infstate.linfo.def
            if infstate.linfo.specTypes == sig
                # avoid widening when detecting self-recursion
                # TODO: merge call cycle and return right away
                if call_result_unused(sv)
                    add_remark!(interp, sv, RECURSION_UNUSED_MSG)
                    # since we don't use the result (typically),
                    # we have a self-cycle in the call-graph, but not in the inference graph (typically):
                    # break this edge now (before we record it) by returning early
                    # (non-typically, this means that we lose the ability to detect a guaranteed StackOverflow in some cases)
                    return MethodCallResult(Any, true, true, nothing)
                end
                topmost = nothing
                edgecycle = true
                break
            end
            topmost === nothing || continue
            if edge_matches_sv(infstate)
                topmost = infstate
                edgecycle = true
            end
        end
    end

    if topmost !== nothing
        sigtuple = unwrap_unionall(sig)::DataType
        msig = unwrap_unionall(method.sig)::DataType
        spec_len = length(msig.parameters) + 1
        ls = length(sigtuple.parameters)

        if method === sv.linfo.def
            # Under direct self-recursion, permit much greater use of reducers.
            # here we assume that complexity(specTypes) :>= complexity(sig)
            comparison = sv.linfo.specTypes
            l_comparison = length(unwrap_unionall(comparison).parameters)::Int
            spec_len = max(spec_len, l_comparison)
        else
            comparison = method.sig
        end

        if isdefined(method, :recursion_relation)
            # We don't recquire the recursion_relation to be transitive, so
            # apply a hard limit
            hardlimit = true
        end

        # see if the type is actually too big (relative to the caller), and limit it if required
        newsig = limit_type_size(sig, comparison, hardlimit ? comparison : sv.linfo.specTypes, InferenceParams(interp).TUPLE_COMPLEXITY_LIMIT_DEPTH, spec_len)

        if newsig !== sig
            # continue inference, but note that we've limited parameter complexity
            # on this call (to ensure convergence), so that we don't cache this result
            if call_result_unused(sv)
                add_remark!(interp, sv, RECURSION_UNUSED_MSG)
                # if we don't (typically) actually care about this result,
                # don't bother trying to examine some complex abstract signature
                # since it's very unlikely that we'll try to inline this,
                # or want make an invoke edge to its calling convention return type.
                # (non-typically, this means that we lose the ability to detect a guaranteed StackOverflow in some cases)
                return MethodCallResult(Any, true, true, nothing)
            end
            add_remark!(interp, sv, RECURSION_MSG)
            topmost = topmost::InferenceState
            parentframe = topmost.parent
            poison_callstack(sv, parentframe === nothing ? topmost : parentframe)
            sig = newsig
            sparams = svec()
            edgelimited = true
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
        #         newsig = limit_type_size(newsig, sig, sv.linfo.specTypes, InferenceParams(interp).TUPLE_COMPLEXITY_LIMIT_DEPTH, lsig)
        #         recomputed = ccall(:jl_type_intersection_with_env, Any, (Any, Any), newsig, method.sig)::SimpleVector
        #         newsig = recomputed[2]
        #     end
        #     sig = ?
        sparams = recomputed[2]::SimpleVector
    end

    rt, edge = typeinf_edge(interp, method, sig, sparams, sv)
    if edge === nothing
        edgecycle = edgelimited = true
    end
    return MethodCallResult(rt, edgecycle, edgelimited, edge)
end

# keeps result and context information of abstract method call, will be used by succeeding constant-propagation
struct MethodCallResult
    rt
    edgecycle::Bool
    edgelimited::Bool
    edge::Union{Nothing,MethodInstance}
    function MethodCallResult(@nospecialize(rt),
                              edgecycle::Bool,
                              edgelimited::Bool,
                              edge::Union{Nothing,MethodInstance})
        return new(rt, edgecycle, edgelimited, edge)
    end
end

function abstract_call_method_with_const_args(interp::AbstractInterpreter, result::MethodCallResult,
                                              @nospecialize(f), argtypes::Vector{Any}, match::MethodMatch,
                                              sv::InferenceState, va_override::Bool)
    mi = maybe_get_const_prop_profitable(interp, result, f, argtypes, match, sv)
    mi === nothing && return Any, nothing
    # try constant prop'
    inf_cache = get_inference_cache(interp)
    inf_result = cache_lookup(mi, argtypes, inf_cache)
    if inf_result === nothing
        # if there might be a cycle, check to make sure we don't end up
        # calling ourselves here.
        if result.edgecycle && _any(InfStackUnwind(sv)) do infstate
                # if the type complexity limiting didn't decide to limit the call signature (`result.edgelimited = false`)
                # we can relax the cycle detection by comparing `MethodInstance`s and allow inference to
                # propagate different constant elements if the recursion is finite over the lattice
                return (result.edgelimited ? match.method === infstate.linfo.def : mi === infstate.linfo) &&
                        any(infstate.result.overridden_by_const)
            end
            add_remark!(interp, sv, "[constprop] Edge cycle encountered")
            return Any, nothing
        end
        inf_result = InferenceResult(mi, argtypes, va_override)
        frame = InferenceState(inf_result, #=cache=#false, interp)
        frame === nothing && return Any, nothing # this is probably a bad generated function (unsound), but just ignore it
        frame.parent = sv
        push!(inf_cache, inf_result)
        typeinf(interp, frame) || return Any, nothing
    end
    result = inf_result.result
    # if constant inference hits a cycle, just bail out
    isa(result, InferenceState) && return Any, nothing
    add_backedge!(mi, sv)
    return result, inf_result
end

# if there's a possibility we could get a better result (hopefully without doing too much work)
# returns `MethodInstance` with constant arguments, returns nothing otherwise
function maybe_get_const_prop_profitable(interp::AbstractInterpreter, result::MethodCallResult,
                                         @nospecialize(f), argtypes::Vector{Any}, match::MethodMatch,
                                         sv::InferenceState)
    const_prop_entry_heuristic(interp, result, sv) || return nothing
    method = match.method
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    if length(argtypes) < nargs
        return nothing
    end
    const_prop_argument_heuristic(interp, argtypes) || const_prop_rettype_heuristic(interp, result.rt) || return nothing
    allconst = is_allconst(argtypes)
    force = force_const_prop(interp, f, method)
    if !force
        if !const_prop_function_heuristic(interp, f, argtypes, nargs, allconst)
            add_remark!(interp, sv, "[constprop] Disabled by function heuristic")
            return nothing
        end
    end
    force |= allconst
    mi = specialize_method(match, !force)
    if mi === nothing
        add_remark!(interp, sv, "[constprop] Failed to specialize")
        return nothing
    end
    mi = mi::MethodInstance
    if !force && !const_prop_methodinstance_heuristic(interp, method, mi)
        add_remark!(interp, sv, "[constprop] Disabled by method instance heuristic")
        return nothing
    end
    return mi
end

function const_prop_entry_heuristic(interp::AbstractInterpreter, result::MethodCallResult, sv::InferenceState)
    if call_result_unused(sv) && result.edgecycle
        add_remark!(interp, sv, "[constprop] Edgecycle with unused result")
        return false
    end
    return is_improvable(result.rt) && InferenceParams(interp).ipo_constant_propagation
end

# see if propagating constants may be worthwhile
function const_prop_argument_heuristic(interp::AbstractInterpreter, argtypes::Vector{Any})
    for a in argtypes
        a = widenconditional(a)
        if has_nontrivial_const_info(a) && is_const_prop_profitable_arg(a)
            return true
        end
    end
    return false
end

function is_const_prop_profitable_arg(@nospecialize(arg))
    # have new information from argtypes that wasn't available from the signature
    if isa(arg, PartialStruct)
        for b in arg.fields
            isconstType(b) && return true
            is_const_prop_profitable_arg(b) && return true
        end
    end
    isa(arg, PartialOpaque) && return true
    isa(arg, Const) || return true
    val = arg.val
    # don't consider mutable values or Strings useful constants
    return isa(val, Symbol) || isa(val, Type) || (!isa(val, String) && !ismutable(val))
end

function const_prop_rettype_heuristic(interp::AbstractInterpreter, @nospecialize(rettype))
    return improvable_via_constant_propagation(rettype)
end

function is_allconst(argtypes::Vector{Any})
    for a in argtypes
        a = widenconditional(a)
        if !isa(a, Const) && !isconstType(a) && !isa(a, PartialStruct) && !isa(a, PartialOpaque)
            return false
        end
    end
    return true
end

function force_const_prop(interp::AbstractInterpreter, @nospecialize(f), method::Method)
    return method.aggressive_constprop ||
           InferenceParams(interp).aggressive_constant_propagation ||
           istopfunction(f, :getproperty) ||
           istopfunction(f, :setproperty!)
end

function const_prop_function_heuristic(interp::AbstractInterpreter, @nospecialize(f), argtypes::Vector{Any}, nargs::Int, allconst::Bool)
    if nargs > 1
        if istopfunction(f, :getindex) || istopfunction(f, :setindex!)
            arrty = argtypes[2]
            # don't propagate constant index into indexing of non-constant array
            if arrty isa Type && arrty <: AbstractArray && !issingletontype(arrty)
                return false
            elseif arrty ⊑ Array
                return false
            end
        elseif istopfunction(f, :iterate)
            itrty = argtypes[2]
            if itrty ⊑ Array
                return false
            end
        end
    end
    if !allconst && (istopfunction(f, :+) || istopfunction(f, :-) || istopfunction(f, :*) ||
                     istopfunction(f, :(==)) || istopfunction(f, :!=) ||
                     istopfunction(f, :<=) || istopfunction(f, :>=) || istopfunction(f, :<) || istopfunction(f, :>) ||
                     istopfunction(f, :<<) || istopfunction(f, :>>))
        # it is almost useless to inline the op when all the same type,
        # but highly worthwhile to inline promote of a constant
        length(argtypes) > 2 || return false
        t1 = widenconst(argtypes[2])
        all_same = true
        for i in 3:length(argtypes)
            if widenconst(argtypes[i]) !== t1
                all_same = false
                break
            end
        end
        return !all_same
    end
    return true
end

# This is a heuristic to avoid trying to const prop through complicated functions
# where we would spend a lot of time, but are probably unlikely to get an improved
# result anyway.
function const_prop_methodinstance_heuristic(interp::AbstractInterpreter, method::Method, mi::MethodInstance)
    if method.is_for_opaque_closure
        # Not inlining an opaque closure can be very expensive, so be generous
        # with the const-prop-ability. It is quite possible that we can't infer
        # anything at all without const-propping, so the inlining check below
        # isn't particularly helpful here.
        return true
    end
    # Peek at the inferred result for the function to determine if the optimizer
    # was able to cut it down to something simple (inlineable in particular).
    # If so, there's a good chance we might be able to const prop all the way
    # through and learn something new.
    code = get(code_cache(interp), mi, nothing)
    declared_inline = isdefined(method, :source) && ccall(:jl_ir_flag_inlineable, Bool, (Any,), method.source)
    cache_inlineable = declared_inline
    if isdefined(code, :inferred) && !cache_inlineable
        cache_inf = code.inferred
        if !(cache_inf === nothing)
            cache_inlineable = inlining_policy(interp)(cache_inf) !== nothing
        end
    end
    if !cache_inlineable
        return false
    end
    return true
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
function precise_container_type(interp::AbstractInterpreter, @nospecialize(itft), @nospecialize(typ), sv::InferenceState)
    if isa(typ, PartialStruct) && typ.typ.name === Tuple.name
        return typ.fields, nothing
    end

    if isa(typ, Const)
        val = typ.val
        if isa(val, SimpleVector) || isa(val, Tuple)
            return Any[ Const(val[i]) for i in 1:length(val) ], nothing # avoid making a tuple Generator here!
        end
    end

    tti0 = widenconst(typ)
    tti = unwrap_unionall(tti0)
    if isa(tti, DataType) && tti.name === NamedTuple_typename
        # A NamedTuple iteration is the same as the iteration of its Tuple parameter:
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
            return Any[Vararg{Any}], nothing
        end
        result = Any[rewrap_unionall(p, tti0) for p in (utis[1]::DataType).parameters]
        for t::DataType in utis[2:end]
            if length(t.parameters) != length(result)
                return Any[Vararg{Any}], nothing
            end
            for j in 1:length(t.parameters)
                result[j] = tmerge(result[j], rewrap_unionall(t.parameters[j], tti0))
            end
        end
        return result, nothing
    elseif tti0 <: Tuple
        if isa(tti0, DataType)
            if isvatuple(tti0) && length(tti0.parameters) == 1
                return Any[Vararg{unwrapva(tti0.parameters[1])}], nothing
            else
                return Any[ p for p in tti0.parameters ], nothing
            end
        elseif !isa(tti, DataType)
            return Any[Vararg{Any}], nothing
        else
            len = length(tti.parameters)
            last = tti.parameters[len]
            va = isvarargtype(last)
            elts = Any[ fieldtype(tti0, i) for i = 1:len ]
            if va
                elts[len] = Vararg{elts[len]}
            end
            return elts, nothing
        end
    elseif tti0 === SimpleVector || tti0 === Any
        return Any[Vararg{Any}], nothing
    elseif tti0 <: Array
        return Any[Vararg{eltype(tti0)}], nothing
    else
        return abstract_iteration(interp, itft, typ, sv)
    end
end

# simulate iteration protocol on container type up to fixpoint
function abstract_iteration(interp::AbstractInterpreter, @nospecialize(itft), @nospecialize(itertype), sv::InferenceState)
    if isa(itft, Const)
        iteratef = itft.val
    else
        return Any[Vararg{Any}], nothing
    end
    @assert !isvarargtype(itertype)
    call = abstract_call_known(interp, iteratef, nothing, Any[itft, itertype], sv)
    stateordonet = call.rt
    info = call.info
    # Return Bottom if this is not an iterator.
    # WARNING: Changes to the iteration protocol must be reflected here,
    # this is not just an optimization.
    # TODO: this doesn't realize that Array, SimpleVector, Tuple, and NamedTuple do not use the iterate protocol
    stateordonet === Bottom && return Any[Bottom], AbstractIterationInfo(CallMeta[CallMeta(Bottom, info)])
    valtype = statetype = Bottom
    ret = Any[]
    calls = CallMeta[call]

    # Try to unroll the iteration up to MAX_TUPLE_SPLAT, which covers any finite
    # length iterators, or interesting prefix
    while true
        stateordonet_widened = widenconst(stateordonet)
        if stateordonet_widened === Nothing
            return ret, AbstractIterationInfo(calls)
        end
        if Nothing <: stateordonet_widened || length(ret) >= InferenceParams(interp).MAX_TUPLE_SPLAT
            break
        end
        if !isa(stateordonet_widened, DataType) || !(stateordonet_widened <: Tuple) || isvatuple(stateordonet_widened) || length(stateordonet_widened.parameters) != 2
            break
        end
        nstatetype = getfield_tfunc(stateordonet, Const(2))
        # If there's no new information in this statetype, don't bother continuing,
        # the iterator won't be finite.
        if nstatetype ⊑ statetype
            return Any[Bottom], nothing
        end
        valtype = getfield_tfunc(stateordonet, Const(1))
        push!(ret, valtype)
        statetype = nstatetype
        call = abstract_call_known(interp, iteratef, nothing, Any[Const(iteratef), itertype, statetype], sv)
        stateordonet = call.rt
        push!(calls, call)
    end
    # From here on, we start asking for results on the widened types, rather than
    # the precise (potentially const) state type
    statetype = widenconst(statetype)
    valtype = widenconst(valtype)
    while valtype !== Any
        stateordonet = abstract_call_known(interp, iteratef, nothing, Any[Const(iteratef), itertype, statetype], sv).rt
        stateordonet = widenconst(stateordonet)
        nounion = typesubtract(stateordonet, Nothing, 0)
        if !isa(nounion, DataType) || !(nounion <: Tuple) || isvatuple(nounion) || length(nounion.parameters) != 2
            valtype = Any
            break
        end
        if nounion.parameters[1] <: valtype && nounion.parameters[2] <: statetype
            if typeintersect(stateordonet, Nothing) === Union{}
                # Reached a fixpoint, but Nothing is not possible => iterator is infinite or failing
                return Any[Bottom], nothing
            end
            break
        end
        valtype = tmerge(valtype, nounion.parameters[1])
        statetype = tmerge(statetype, nounion.parameters[2])
    end
    push!(ret, Vararg{valtype})
    return ret, nothing
end

# do apply(af, fargs...), where af is a function value
function abstract_apply(interp::AbstractInterpreter, argtypes::Vector{Any}, sv::InferenceState,
                        max_methods::Int = InferenceParams(interp).MAX_METHODS)
    itft = argtype_by_index(argtypes, 2)
    aft = argtype_by_index(argtypes, 3)
    (itft === Bottom || aft === Bottom) && return CallMeta(Bottom, false)
    aargtypes = argtype_tail(argtypes, 4)
    aftw = widenconst(aft)
    if !isa(aft, Const) && !isa(aft, PartialOpaque) && (!isType(aftw) || has_free_typevars(aftw))
        if !isconcretetype(aftw) || (aftw <: Builtin)
            add_remark!(interp, sv, "Core._apply_iterate called on a function of a non-concrete type")
            # bail now, since it seems unlikely that abstract_call will be able to do any better after splitting
            # this also ensures we don't call abstract_call_gf_by_type below on an IntrinsicFunction or Builtin
            return CallMeta(Any, false)
        end
    end
    res = Union{}
    nargs = length(aargtypes)
    splitunions = 1 < unionsplitcost(aargtypes) <= InferenceParams(interp).MAX_APPLY_UNION_ENUM
    ctypes = [Any[aft]]
    infos = [Union{Nothing, AbstractIterationInfo}[]]
    for i = 1:nargs
        ctypes´ = Vector{Any}[]
        infos′ = Vector{Union{Nothing, AbstractIterationInfo}}[]
        for ti in (splitunions ? uniontypes(aargtypes[i]) : Any[aargtypes[i]])
            if !isvarargtype(ti)
                cti_info = precise_container_type(interp, itft, ti, sv)
                cti = cti_info[1]::Vector{Any}
                info = cti_info[2]::Union{Nothing,AbstractIterationInfo}
            else
                cti_info = precise_container_type(interp, itft, unwrapva(ti), sv)
                cti = cti_info[1]::Vector{Any}
                info = cti_info[2]::Union{Nothing,AbstractIterationInfo}
                # We can't represent a repeating sequence of the same types,
                # so tmerge everything together to get one type that represents
                # everything.
                argt = cti[end]
                if isvarargtype(argt)
                    argt = unwrapva(argt)
                end
                for i in 1:(length(cti)-1)
                    argt = tmerge(argt, cti[i])
                end
                cti = Any[Vararg{argt}]
            end
            if _any(t -> t === Bottom, cti)
                continue
            end
            for j = 1:length(ctypes)
                ct = ctypes[j]::Vector{Any}
                if isvarargtype(ct[end])
                    # This is vararg, we're not gonna be able to do any inling,
                    # drop the info
                    info = nothing

                    tail = tuple_tail_elem(unwrapva(ct[end]), cti)
                    push!(ctypes´, push!(ct[1:(end - 1)], tail))
                else
                    push!(ctypes´, append!(ct[:], cti))
                end
                push!(infos′, push!(copy(infos[j]), info))
            end
        end
        ctypes = ctypes´
        infos = infos′
    end
    retinfos = ApplyCallInfo[]
    retinfo = UnionSplitApplyCallInfo(retinfos)
    for i = 1:length(ctypes)
        ct = ctypes[i]
        arginfo = infos[i]
        lct = length(ct)
        # truncate argument list at the first Vararg
        for i = 1:lct-1
            if isvarargtype(ct[i])
                ct[i] = tuple_tail_elem(ct[i], ct[(i+1):lct])
                resize!(ct, i)
                break
            end
        end
        call = abstract_call(interp, nothing, ct, sv, max_methods)
        push!(retinfos, ApplyCallInfo(call.info, arginfo))
        res = tmerge(res, call.rt)
        if bail_out_apply(interp, res, sv)
            if i != length(ctypes)
                # No point carrying forward the info, we're not gonna inline it anyway
                retinfo = false
            end
            break
        end
    end
    # TODO: Add a special info type to capture all the iteration info.
    # For now, only propagate info if we don't also union-split the iteration
    return CallMeta(res, retinfo)
end

function is_method_pure(method::Method, @nospecialize(sig), sparams::SimpleVector)
    if isdefined(method, :generator)
        method.generator.expand_early || return false
        mi = specialize_method(method, sig, sparams, false)
        isa(mi, MethodInstance) || return false
        staged = get_staged(mi)
        (staged isa CodeInfo && (staged::CodeInfo).pure) || return false
        return true
    end
    return method.pure
end
is_method_pure(match::MethodMatch) = is_method_pure(match.method, match.spec_types, match.sparams)

function pure_eval_call(@nospecialize(f), argtypes::Vector{Any})
    for i = 2:length(argtypes)
        a = widenconditional(argtypes[i])
        if !(isa(a, Const) || isconstType(a))
            return false
        end
    end

    args = Any[ (a = widenconditional(argtypes[i]); isa(a, Const) ? a.val : a.parameters[1]) for i in 2:length(argtypes) ]
    try
        value = Core._apply_pure(f, args)
        return Const(value)
    catch
        return false
    end
end

function argtype_by_index(argtypes::Vector{Any}, i::Int)
    n = length(argtypes)
    na = argtypes[n]
    if isvarargtype(na)
        return i >= n ? unwrapva(na) : argtypes[i]
    else
        return i > n ? Bottom : argtypes[i]
    end
end

function argtype_tail(argtypes::Vector{Any}, i::Int)
    n = length(argtypes)
    if isvarargtype(argtypes[n]) && i > n
        i = n
    end
    return argtypes[i:n]
end

function abstract_call_builtin(interp::AbstractInterpreter, f::Builtin, fargs::Union{Nothing,Vector{Any}},
        argtypes::Vector{Any}, sv::InferenceState, max_methods::Int)
    @nospecialize f
    la = length(argtypes)
    if f === ifelse && fargs isa Vector{Any} && la == 4
        cnd = argtypes[2]
        if isa(cnd, Conditional)
            newcnd = widenconditional(cnd)
            tx = argtypes[3]
            ty = argtypes[4]
            if isa(newcnd, Const)
                # if `cnd` is constant, we should just respect its constantness to keep inference accuracy
                return newcnd.val::Bool ? tx : ty
            else
                # try to simulate this as a real conditional (`cnd ? x : y`), so that the penalty for using `ifelse` instead isn't too high
                a = ssa_def_slot(fargs[3], sv)
                b = ssa_def_slot(fargs[4], sv)
                if isa(a, SlotNumber) && slot_id(cnd.var) == slot_id(a)
                    tx = (cnd.vtype ⊑ tx ? cnd.vtype : tmeet(tx, widenconst(cnd.vtype)))
                end
                if isa(b, SlotNumber) && slot_id(cnd.var) == slot_id(b)
                    ty = (cnd.elsetype ⊑ ty ? cnd.elsetype : tmeet(ty, widenconst(cnd.elsetype)))
                end
                return tmerge(tx, ty)
            end
        end
    end
    rt = builtin_tfunction(interp, f, argtypes[2:end], sv)
    if f === getfield && isa(fargs, Vector{Any}) && la == 3 &&
       (a3 = argtypes[3]; isa(a3, Const)) && (idx = a3.val; isa(idx, Int)) &&
       (a2 = argtypes[2]; a2 ⊑ Tuple)
        # TODO: why doesn't this use the getfield_tfunc?
        cti_info = precise_container_type(interp, iterate, a2, sv)
        cti = cti_info[1]::Vector{Any}
        if 1 <= idx <= length(cti)
            rt = unwrapva(cti[idx])
        end
    elseif (rt === Bool || (isa(rt, Const) && isa(rt.val, Bool))) && isa(fargs, Vector{Any})
        # perform very limited back-propagation of type information for `is` and `isa`
        if f === isa
            a = ssa_def_slot(fargs[2], sv)
            if isa(a, SlotNumber)
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
                        elty = typesubtract(aty, tty_lb, InferenceParams(interp).MAX_UNION_SPLITTING)
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
            if isa(aty, Const) && isa(b, SlotNumber)
                if rt === Const(false)
                    aty = Union{}
                elseif rt === Const(true)
                    bty = Union{}
                elseif bty isa Type && isdefined(typeof(aty.val), :instance) # can only widen a if it is a singleton
                    bty = typesubtract(bty, typeof(aty.val), InferenceParams(interp).MAX_UNION_SPLITTING)
                end
                return Conditional(b, aty, bty)
            end
            if isa(bty, Const) && isa(a, SlotNumber)
                if rt === Const(false)
                    bty = Union{}
                elseif rt === Const(true)
                    aty = Union{}
                elseif aty isa Type && isdefined(typeof(bty.val), :instance) # same for b
                    aty = typesubtract(aty, typeof(bty.val), InferenceParams(interp).MAX_UNION_SPLITTING)
                end
                return Conditional(a, bty, aty)
            end
            # narrow the lattice slightly (noting the dependency on one of the slots), to promote more effective smerge
            if isa(b, SlotNumber)
                return Conditional(b, rt === Const(false) ? Union{} : bty, rt === Const(true) ? Union{} : bty)
            end
            if isa(a, SlotNumber)
                return Conditional(a, rt === Const(false) ? Union{} : aty, rt === Const(true) ? Union{} : aty)
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
end

function abstract_call_unionall(argtypes::Vector{Any})
    if length(argtypes) == 3
        canconst = true
        a3 = argtypes[3]
        if isa(a3, Const)
            body = a3.val
        elseif isType(a3)
            body = a3.parameters[1]
            canconst = false
        else
            return Any
        end
        if !isa(body, Type) && !isa(body, TypeVar)
            return Any
        end
        if has_free_typevars(body)
            a2 = argtypes[2]
            if isa(a2, Const)
                tv = a2.val
            elseif isa(a2, PartialTypeVar)
                tv = a2.tv
                canconst = false
            else
                return Any
            end
            !isa(tv, TypeVar) && return Any
            body = UnionAll(tv, body)
        end
        ret = canconst ? Const(body) : Type{body}
        return ret
    end
    return Any
end

function abstract_invoke(interp::AbstractInterpreter, argtypes::Vector{Any}, sv::InferenceState)
    ft′ = argtype_by_index(argtypes, 2)
    ft = widenconst(ft′)
    ft === Bottom && return CallMeta(Bottom, false)
    (types, isexact, isconcrete, istype) = instanceof_tfunc(argtype_by_index(argtypes, 3))
    types === Bottom && return CallMeta(Bottom, false)
    isexact || return CallMeta(Any, false)
    argtype = argtypes_to_type(argtype_tail(argtypes, 4))
    nargtype = typeintersect(types, argtype)
    nargtype === Bottom && return CallMeta(Bottom, false)
    nargtype isa DataType || return CallMeta(Any, false) # other cases are not implemented below
    isdispatchelem(ft) || return CallMeta(Any, false) # check that we might not have a subtype of `ft` at runtime, before doing supertype lookup below
    types = rewrap_unionall(Tuple{ft, unwrap_unionall(types).parameters...}, types)
    nargtype = Tuple{ft, nargtype.parameters...}
    argtype = Tuple{ft, argtype.parameters...}
    result = findsup(types, method_table(interp))
    result === nothing && return CallMeta(Any, false)
    method, valid_worlds = result
    update_valid_age!(sv, valid_worlds)
    (ti, env::SimpleVector) = ccall(:jl_type_intersection_with_env, Any, (Any, Any), nargtype, method.sig)::SimpleVector
    (; rt, edge) = result = abstract_call_method(interp, method, ti, env, false, sv)
    edge !== nothing && add_backedge!(edge::MethodInstance, sv)
    match = MethodMatch(ti, env, method, argtype <: method.sig)
    # try constant propagation with manual inlinings of some of the heuristics
    # since some checks within `abstract_call_method_with_const_args` seem a bit costly
    const_prop_entry_heuristic(interp, result, sv) || return CallMeta(rt, InvokeCallInfo(match, nothing))
    argtypes′ = argtypes[4:end]
    const_prop_argument_heuristic(interp, argtypes′) || const_prop_rettype_heuristic(interp, rt) || return CallMeta(rt, InvokeCallInfo(match, nothing))
    pushfirst!(argtypes′, ft)
    # # typeintersect might have narrowed signature, but the accuracy gain doesn't seem worth the cost involved with the lattice comparisons
    # for i in 1:length(argtypes′)
    #     t, a = ti.parameters[i], argtypes′[i]
    #     argtypes′[i] = t ⊑ a ? t : a
    # end
    const_rt, const_result = abstract_call_method_with_const_args(interp, result, argtype_to_function(ft′), argtypes′, match, sv, false)
    if const_rt !== rt && const_rt ⊑ rt
        return CallMeta(const_rt, InvokeCallInfo(match, const_result))
    else
        return CallMeta(rt, InvokeCallInfo(match, nothing))
    end
end

# call where the function is known exactly
function abstract_call_known(interp::AbstractInterpreter, @nospecialize(f),
        fargs::Union{Nothing,Vector{Any}}, argtypes::Vector{Any},
        sv::InferenceState,
        max_methods::Int = InferenceParams(interp).MAX_METHODS)

    la = length(argtypes)

    if isa(f, Builtin)
        if f === _apply_iterate
            return abstract_apply(interp, argtypes, sv, max_methods)
        elseif f === invoke
            return abstract_invoke(interp, argtypes, sv)
        end
        return CallMeta(abstract_call_builtin(interp, f, fargs, argtypes, sv, max_methods), false)
    elseif f === Core.kwfunc
        if la == 2
            ft = widenconst(argtypes[2])
            if isa(ft, DataType) && isdefined(ft.name, :mt) && isdefined(ft.name.mt, :kwsorter)
                return CallMeta(Const(ft.name.mt.kwsorter), MethodResultPure())
            end
        end
        return CallMeta(Any, false)
    elseif f === TypeVar
        # Manually look through the definition of TypeVar to
        # make sure to be able to get `PartialTypeVar`s out.
        (la < 2 || la > 4) && return CallMeta(Union{}, false)
        n = argtypes[2]
        ub_var = Const(Any)
        lb_var = Const(Union{})
        if la == 4
            ub_var = argtypes[4]
            lb_var = argtypes[3]
        elseif la == 3
            ub_var = argtypes[3]
        end
        return CallMeta(typevar_tfunc(n, lb_var, ub_var), false)
    elseif f === UnionAll
        return CallMeta(abstract_call_unionall(argtypes), false)
    elseif f === Tuple && la == 2 && !isconcretetype(widenconst(argtypes[2]))
        return CallMeta(Tuple, false)
    elseif is_return_type(f)
        return return_type_tfunc(interp, argtypes, sv)
    elseif la == 2 && istopfunction(f, :!)
        # handle Conditional propagation through !Bool
        aty = argtypes[2]
        if isa(aty, Conditional)
            call = abstract_call_gf_by_type(interp, f, fargs, Any[Const(f), Bool], Tuple{typeof(f), Bool}, sv) # make sure we've inferred `!(::Bool)`
            return CallMeta(Conditional(aty.var, aty.elsetype, aty.vtype), call.info)
        end
    elseif la == 3 && istopfunction(f, :!==)
        # mark !== as exactly a negated call to ===
        rty = abstract_call_known(interp, (===), fargs, argtypes, sv).rt
        if isa(rty, Conditional)
            return CallMeta(Conditional(rty.var, rty.elsetype, rty.vtype), false) # swap if-else
        elseif isa(rty, Const)
            return CallMeta(Const(rty.val === false), MethodResultPure())
        end
        return CallMeta(rty, false)
    elseif la == 3 && istopfunction(f, :(>:))
        # mark issupertype as a exact alias for issubtype
        # swap T1 and T2 arguments and call <:
        if fargs !== nothing && length(fargs) == 3
            fargs = Any[<:, fargs[3], fargs[2]]
        else
            fargs = nothing
        end
        argtypes = Any[typeof(<:), argtypes[3], argtypes[2]]
        return CallMeta(abstract_call_known(interp, <:, fargs, argtypes, sv).rt, false)
    elseif la == 2 &&
           (a2 = argtypes[2]; isa(a2, Const)) && (svecval = a2.val; isa(svecval, SimpleVector)) &&
           istopfunction(f, :length)
        # mark length(::SimpleVector) as @pure
        return CallMeta(Const(length(svecval)), MethodResultPure())
    elseif la == 3 &&
           (a2 = argtypes[2]; isa(a2, Const)) && (svecval = a2.val; isa(svecval, SimpleVector)) &&
           (a3 = argtypes[3]; isa(a3, Const)) && (idx = a3.val; isa(idx, Int)) &&
           istopfunction(f, :getindex)
        # mark getindex(::SimpleVector, i::Int) as @pure
        if 1 <= idx <= length(svecval) && isassigned(svecval, idx)
            return CallMeta(Const(getindex(svecval, idx)), MethodResultPure())
        end
    elseif la == 2 && istopfunction(f, :typename)
        return CallMeta(typename_static(argtypes[2]), MethodResultPure())
    elseif max_methods > 1 && istopfunction(f, :copyto!)
        max_methods = 1
    elseif la == 3 && istopfunction(f, :typejoin)
        val = pure_eval_call(f, argtypes)
        return CallMeta(val === false ? Type : val, MethodResultPure())
    end
    atype = argtypes_to_type(argtypes)
    return abstract_call_gf_by_type(interp, f, fargs, argtypes, atype, sv, max_methods)
end

function abstract_call_opaque_closure(interp::AbstractInterpreter, closure::PartialOpaque, argtypes::Vector{Any}, sv::InferenceState)
    pushfirst!(argtypes, closure.env)
    sig = argtypes_to_type(argtypes)
    (; rt, edge) = result = abstract_call_method(interp, closure.source::Method, sig, Core.svec(), false, sv)
    edge !== nothing && add_backedge!(edge, sv)
    tt = closure.typ
    sigT = unwrap_unionall(tt).parameters[1]
    match = MethodMatch(sig, Core.svec(), closure.source::Method, sig <: rewrap_unionall(sigT, tt))
    info = OpaqueClosureCallInfo(match)
    if !result.edgecycle
        const_rettype, const_result = abstract_call_method_with_const_args(interp, result, closure, argtypes,
            match, sv, closure.isva)
        if const_rettype ⊑ rt
           rt = const_rettype
        end
        if const_result !== nothing
            info = ConstCallInfo(info, Union{Nothing,InferenceResult}[const_result])
        end
    end
    return CallMeta(rt, info)
end

function most_general_argtypes(closure::PartialOpaque)
    ret = Any[]
    cc = widenconst(closure)
    argt = unwrap_unionall(cc).parameters[1]
    if !isa(argt, DataType) || argt.name !== typename(Tuple)
        argt = Tuple
    end
    return most_general_argtypes(closure.source, argt, closure.isva, false)
end

# call where the function is any lattice element
function abstract_call(interp::AbstractInterpreter, fargs::Union{Nothing,Vector{Any}}, argtypes::Vector{Any},
                       sv::InferenceState, max_methods::Int = InferenceParams(interp).MAX_METHODS)
    #print("call ", e.args[1], argtypes, "\n\n")
    ft = argtypes[1]
    f = argtype_to_function(ft)
    if isa(ft, PartialOpaque)
        return abstract_call_opaque_closure(interp, ft, argtypes[2:end], sv)
    elseif isa(unwrap_unionall(ft), DataType) && unwrap_unionall(ft).name === typename(Core.OpaqueClosure)
        return CallMeta(rewrap_unionall(unwrap_unionall(ft).parameters[2], ft), false)
    elseif f === nothing
        # non-constant function, but the number of arguments is known
        # and the ft is not a Builtin or IntrinsicFunction
        if typeintersect(widenconst(ft), Union{Builtin, Core.OpaqueClosure}) != Union{}
            add_remark!(interp, sv, "Could not identify method table for call")
            return CallMeta(Any, false)
        end
        return abstract_call_gf_by_type(interp, nothing, fargs, argtypes, argtypes_to_type(argtypes), sv, max_methods)
    end
    return abstract_call_known(interp, f, fargs, argtypes, sv, max_methods)
end

function argtype_to_function(@nospecialize(ft))
    if isa(ft, Const)
        return ft.val
    elseif isconstType(ft)
        return ft.parameters[1]
    elseif isa(ft, DataType) && isdefined(ft, :instance)
        return ft.instance
    else
        return nothing
    end
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
                sparam_vals = Any[isa(v, Core.TypeofVararg) ? TypeVar(:N, Union{}, Any) :
                                  v for v in  linfo.sparam_vals]
                T = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), T, spsig, sparam_vals)
                isref && isreturn && T === Any && return Bottom # catch invalid return Ref{T} where T = Any
                for v in sparam_vals
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

function abstract_eval_cfunction(interp::AbstractInterpreter, e::Expr, vtypes::VarTable, sv::InferenceState)
    f = abstract_eval_value(interp, e.args[2], vtypes, sv)
    # rt = sp_type_rewrap(e.args[3], sv.linfo, true)
    at = Any[ sp_type_rewrap(argt, sv.linfo, false) for argt in e.args[4]::SimpleVector ]
    pushfirst!(at, f)
    # this may be the wrong world for the call,
    # but some of the result is likely to be valid anyways
    # and that may help generate better codegen
    abstract_call(interp, nothing, at, sv)
    nothing
end

function abstract_eval_value_expr(interp::AbstractInterpreter, e::Expr, vtypes::VarTable, sv::InferenceState)
    if e.head === :static_parameter
        n = e.args[1]::Int
        t = Any
        if 1 <= n <= length(sv.sptypes)
            t = sv.sptypes[n]
        end
        return t
    elseif e.head === :boundscheck
        return Bool
    else
        return Any
    end
end

function abstract_eval_special_value(interp::AbstractInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if isa(e, QuoteNode)
        return Const((e::QuoteNode).value)
    elseif isa(e, SSAValue)
        return abstract_eval_ssavalue(e::SSAValue, sv.src)
    elseif isa(e, SlotNumber) || isa(e, Argument)
        return (vtypes[slot_id(e)]::VarState).typ
    elseif isa(e, GlobalRef)
        return abstract_eval_global(e.mod, e.name)
    end

    return Const(e)
end

function abstract_eval_value(interp::AbstractInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if isa(e, Expr)
        return abstract_eval_value_expr(interp, e, vtypes, sv)
    else
        typ = abstract_eval_special_value(interp, e, vtypes, sv)
        if typ isa LimitedAccuracy
            union!(sv.pclimitations, typ.causes)
            typ = typ.typ
        end
        return typ
    end
end

function collect_argtypes(interp::AbstractInterpreter, ea::Vector{Any}, vtypes::VarTable, sv::InferenceState)
    n = length(ea)
    argtypes = Vector{Any}(undef, n)
    @inbounds for i = 1:n
        ai = abstract_eval_value(interp, ea[i], vtypes, sv)
        if ai === Bottom
            return nothing
        end
        argtypes[i] = ai
    end
    return argtypes
end

function abstract_eval_statement(interp::AbstractInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if !isa(e, Expr)
        if isa(e, PhiNode)
            rt = Union{}
            for val in e.values
                rt = tmerge(rt, abstract_eval_special_value(interp, val, vtypes, sv))
            end
            return rt
        end
        return abstract_eval_special_value(interp, e, vtypes, sv)
    end
    e = e::Expr
    if e.head === :call
        ea = e.args
        argtypes = collect_argtypes(interp, ea, vtypes, sv)
        if argtypes === nothing
            t = Bottom
        else
            callinfo = abstract_call(interp, ea, argtypes, sv)
            sv.stmt_info[sv.currpc] = callinfo.info
            t = callinfo.rt
        end
    elseif e.head === :new
        t = instanceof_tfunc(abstract_eval_value(interp, e.args[1], vtypes, sv))[1]
        if isconcretetype(t) && !ismutabletype(t)
            args = Vector{Any}(undef, length(e.args)-1)
            ats = Vector{Any}(undef, length(e.args)-1)
            anyconst = false
            allconst = true
            for i = 2:length(e.args)
                at = abstract_eval_value(interp, e.args[i], vtypes, sv)
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
        t = instanceof_tfunc(abstract_eval_value(interp, e.args[1], vtypes, sv))[1]
        if length(e.args) == 2 && isconcretetype(t) && !ismutabletype(t)
            at = abstract_eval_value(interp, e.args[2], vtypes, sv)
            n = fieldcount(t)
            if isa(at, Const) && isa(at.val, Tuple) && n == length(at.val) &&
                let t = t; _all(i->getfield(at.val, i) isa fieldtype(t, i), 1:n); end
                t = Const(ccall(:jl_new_structt, Any, (Any, Any), t, at.val))
            elseif isa(at, PartialStruct) && at ⊑ Tuple && n == length(at.fields) &&
                let t = t, at = at; _all(i->at.fields[i] ⊑ fieldtype(t, i), 1:n); end
                t = PartialStruct(t, at.fields)
            end
        end
    elseif e.head === :new_opaque_closure
        t = Union{}
        if length(e.args) >= 5
            ea = e.args
            argtypes = collect_argtypes(interp, ea, vtypes, sv)
            if argtypes === nothing
                t = Bottom
            else
                t = _opaque_closure_tfunc(argtypes[1], argtypes[2], argtypes[3],
                    argtypes[4], argtypes[5], argtypes[6:end], sv.linfo)
                if isa(t, PartialOpaque)
                    # Infer this now so that the specialization is available to
                    # optimization.
                    callinfo = abstract_call_opaque_closure(interp, t,
                        most_general_argtypes(t), sv)
                    sv.stmt_info[sv.currpc] = OpaqueClosureCreateInfo(callinfo)
                end
            end
        end
    elseif e.head === :foreigncall
        abstract_eval_value(interp, e.args[1], vtypes, sv)
        t = sp_type_rewrap(e.args[2], sv.linfo, true)
        for i = 3:length(e.args)
            if abstract_eval_value(interp, e.args[i], vtypes, sv) === Bottom
                t = Bottom
            end
        end
    elseif e.head === :cfunction
        t = e.args[1]
        isa(t, Type) || (t = Any)
        abstract_eval_cfunction(interp, e, vtypes, sv)
    elseif e.head === :method
        t = (length(e.args) == 1) ? Any : Nothing
    elseif e.head === :copyast
        t = abstract_eval_value(interp, e.args[1], vtypes, sv)
        if t isa Const && t.val isa Expr
            # `copyast` makes copies of Exprs
            t = Expr
        end
    elseif e.head === :invoke
        error("type inference data-flow error: tried to double infer a function")
    elseif e.head === :isdefined
        sym = e.args[1]
        t = Bool
        if isa(sym, SlotNumber)
            vtyp = vtypes[slot_id(sym)]
            if vtyp.typ === Bottom
                t = Const(false) # never assigned previously
            elseif !vtyp.undef
                t = Const(true) # definitely assigned previously
            end
        elseif isa(sym, Symbol)
            if isdefined(sv.mod, sym)
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
        t = abstract_eval_value_expr(interp, e, vtypes, sv)
    end
    @assert !isa(t, TypeVar)
    if isa(t, DataType) && isdefined(t, :instance)
        # replace singleton types with their equivalent Const object
        t = Const(t.instance)
    end
    if !isempty(sv.pclimitations)
        if t isa Const || t === Union{}
            empty!(sv.pclimitations)
        else
            t = LimitedAccuracy(t, sv.pclimitations)
            sv.pclimitations = IdSet{InferenceState}()
        end
    end
    return t
end

function abstract_eval_global(M::Module, s::Symbol)
    if isdefined(M,s) && isconst(M,s)
        return Const(getfield(M,s))
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

function widenreturn(@nospecialize(rt), @nospecialize(bestguess), nslots::Int, slottypes::Vector{Any}, changes::VarTable)
    if !(bestguess ⊑ Bool) || bestguess === Bool
        # give up inter-procedural constraint back-propagation
        # when tmerge would widen the result anyways (as an optimization)
        rt = widenconditional(rt)
    else
        if isa(rt, Conditional)
            id = slot_id(rt.var)
            if 1 ≤ id ≤ nslots
                old_id_type = widenconditional(slottypes[id]) # same as `((s[1]::VarTable)[id]::VarState).typ`
                if (!(rt.vtype ⊑ old_id_type) || old_id_type ⊑ rt.vtype) &&
                   (!(rt.elsetype ⊑ old_id_type) || old_id_type ⊑ rt.elsetype)
                   # discard this `Conditional` since it imposes
                   # no new constraint on the argument type
                   # (the caller will recreate it if needed)
                   rt = widenconditional(rt)
               end
            else
                # discard this `Conditional` imposed on non-call arguments,
                # since it's not interesting in inter-procedural context;
                # we may give constraints on other call argument
                rt = widenconditional(rt)
            end
        end
        if isa(rt, Conditional)
            rt = InterConditional(slot_id(rt.var), rt.vtype, rt.elsetype)
        elseif is_lattice_bool(rt)
            if isa(bestguess, InterConditional)
                # if the bestguess so far is already `Conditional`, try to convert
                # this `rt` into `Conditional` on the slot to avoid overapproximation
                # due to conflict of different slots
                rt = bool_rt_to_conditional(rt, slottypes, changes, bestguess.slot)
            else
                # pick up the first "interesting" slot, convert `rt` to its `Conditional`
                # TODO: ideally we want `Conditional` and `InterConditional` to convey
                # constraints on multiple slots
                for slot_id in 1:nslots
                    rt = bool_rt_to_conditional(rt, slottypes, changes, slot_id)
                    rt isa InterConditional && break
                end
            end
        end
    end

    # only propagate information we know we can store
    # and is valid and good inter-procedurally
    isa(rt, Conditional) && return InterConditional(slot_id(rt.var), rt.vtype, rt.elsetype)
    isa(rt, InterConditional) && return rt
    isa(rt, Const) && return rt
    isa(rt, Type) && return rt
    if isa(rt, PartialStruct)
        fields = copy(rt.fields)
        haveconst = false
        for i in 1:length(fields)
            a = widenreturn(fields[i], bestguess, nslots, slottypes, changes)
            if !haveconst && has_const_info(a)
                # TODO: consider adding && const_prop_profitable(a) here?
                haveconst = true
            end
            fields[i] = a
        end
        haveconst && return PartialStruct(rt.typ, fields)
    end
    if isa(rt, PartialOpaque)
        return rt # XXX: this case was missed in #39512
    end
    return widenconst(rt)
end

# make as much progress on `frame` as possible (without handling cycles)
function typeinf_local(interp::AbstractInterpreter, frame::InferenceState)
    @assert !frame.inferred
    frame.dont_work_on_me = true # mark that this function is currently on the stack
    W = frame.ip
    states = frame.stmt_types
    n = frame.nstmts
    nargs = frame.nargs
    def = frame.linfo.def
    isva = isa(def, Method) && def.isva
    nslots = nargs - isva
    slottypes = frame.slottypes
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
            edges = frame.stmt_edges[pc]
            edges === nothing || empty!(edges)
            frame.stmt_info[pc] = nothing
            stmt = frame.src.code[pc]
            changes = states[pc]::VarTable
            t = nothing

            hd = isa(stmt, Expr) ? stmt.head : nothing

            if isa(stmt, NewvarNode)
                sn = slot_id(stmt.slot)
                changes[sn] = VarState(Bottom, true)
            elseif isa(stmt, GotoNode)
                pc´ = (stmt::GotoNode).label
            elseif isa(stmt, GotoIfNot)
                condx = stmt.cond
                condt = abstract_eval_value(interp, condx, changes, frame)
                if condt === Bottom
                    empty!(frame.pclimitations)
                    break
                end
                if !(isa(condt, Const) || isa(condt, Conditional)) && isa(condx, SlotNumber)
                    # if this non-`Conditional` object is a slot, we form and propagate
                    # the conditional constraint on it
                    condt = Conditional(condx, Const(true), Const(false))
                end
                condval = maybe_extract_const_bool(condt)
                l = stmt.dest::Int
                if !isempty(frame.pclimitations)
                    # we can't model the possible effect of control
                    # dependencies on the return value, so we propagate it
                    # directly to all the return values (unless we error first)
                    condval isa Bool || union!(frame.limitations, frame.pclimitations)
                    empty!(frame.pclimitations)
                end
                # constant conditions
                if condval === true
                elseif condval === false
                    pc´ = l
                else
                    # general case
                    frame.handler_at[l] = frame.cur_hand
                    changes_else = changes
                    if isa(condt, Conditional)
                        changes_else = conditional_changes(changes_else, condt.elsetype, condt.var)
                        changes      = conditional_changes(changes,      condt.vtype,    condt.var)
                    end
                    newstate_else = stupdate!(states[l], changes_else)
                    if newstate_else !== nothing
                        # add else branch to active IP list
                        if l < frame.pc´´
                            frame.pc´´ = l
                        end
                        push!(W, l)
                        states[l] = newstate_else
                    end
                end
            elseif isa(stmt, ReturnNode)
                pc´ = n + 1
                bestguess = frame.bestguess
                rt = abstract_eval_value(interp, stmt.val, changes, frame)
                rt = widenreturn(rt, bestguess, nslots, slottypes, changes)
                # narrow representation of bestguess slightly to prepare for tmerge with rt
                if rt isa InterConditional && bestguess isa Const
                    let slot_id = rt.slot
                        old_id_type = slottypes[slot_id]
                        if bestguess.val === true && rt.elsetype !== Bottom
                            bestguess = InterConditional(slot_id, old_id_type, Bottom)
                        elseif bestguess.val === false && rt.vtype !== Bottom
                            bestguess = InterConditional(slot_id, Bottom, old_id_type)
                        end
                    end
                end
                # copy limitations to return value
                if !isempty(frame.pclimitations)
                    union!(frame.limitations, frame.pclimitations)
                    empty!(frame.pclimitations)
                end
                if !isempty(frame.limitations)
                    rt = LimitedAccuracy(rt, copy(frame.limitations))
                end
                if tchanged(rt, bestguess)
                    # new (wider) return type for frame
                    bestguess = tmerge(bestguess, rt)
                    # TODO: if bestguess isa InterConditional && !interesting(bestguess); bestguess = widenconditional(bestguess); end
                    frame.bestguess = bestguess
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
                old = states[l]
                newstate_catch = stupdate!(old, changes)
                if newstate_catch !== nothing
                    if l < frame.pc´´
                        frame.pc´´ = l
                    end
                    push!(W, l)
                    states[l] = newstate_catch
                end
                typeassert(states[l], VarTable)
                frame.handler_at[l] = frame.cur_hand
            elseif hd === :leave
                for i = 1:((stmt.args[1])::Int)
                    frame.cur_hand = (frame.cur_hand::Pair{Any,Any}).second
                end
            else
                if hd === :(=)
                    t = abstract_eval_statement(interp, stmt.args[2], changes, frame)
                    if t === Bottom
                        break
                    end
                    frame.src.ssavaluetypes[pc] = t
                    lhs = stmt.args[1]
                    if isa(lhs, SlotNumber)
                        changes = StateUpdate(lhs, VarState(t, false), changes, false)
                    end
                elseif hd === :method
                    fname = stmt.args[1]
                    if isa(fname, SlotNumber)
                        changes = StateUpdate(fname, VarState(Any, false), changes, false)
                    end
                elseif hd === :inbounds || hd === :meta || hd === :loopinfo || hd === :code_coverage_effect
                    # these do not generate code
                else
                    t = abstract_eval_statement(interp, stmt, changes, frame)
                    if t === Bottom
                        break
                    end
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
                    if stupdate1!(states[l]::VarTable, changes::StateUpdate) !== false
                        if l < frame.pc´´
                            frame.pc´´ = l
                        end
                        push!(W, l)
                    end
                end
            end

            @assert isempty(frame.pclimitations) "unhandled LimitedAccuracy"

            if t === nothing
                # mark other reached expressions as `Any` to indicate they don't throw
                frame.src.ssavaluetypes[pc] = Any
            end

            pc´ > n && break # can't proceed with the fast-path fall-through
            frame.handler_at[pc´] = frame.cur_hand
            newstate = stupdate!(states[pc´], changes)
            if isa(stmt, GotoNode) && frame.pc´´ < pc´
                # if we are processing a goto node anyways,
                # (such as a terminator for a loop, if-else, or try block),
                # consider whether we should jump to an older backedge first,
                # to try to traverse the statements in approximate dominator order
                if newstate !== nothing
                    states[pc´] = newstate
                end
                push!(W, pc´)
                pc = frame.pc´´
            elseif newstate !== nothing
                states[pc´] = newstate
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

function conditional_changes(changes::VarTable, @nospecialize(typ), var::SlotNumber)
    oldtyp = (changes[slot_id(var)]::VarState).typ
    # approximate test for `typ ∩ oldtyp` being better than `oldtyp`
    # since we probably formed these types with `typesubstract`, the comparison is likely simple
    if ignorelimited(typ) ⊑ ignorelimited(oldtyp)
        # typ is better unlimited, but we may still need to compute the tmeet with the limit "causes" since we ignored those in the comparison
        oldtyp isa LimitedAccuracy && (typ = tmerge(typ, LimitedAccuracy(Bottom, oldtyp.causes)))
        return StateUpdate(var, VarState(typ, false), changes, true)
    end
    return changes
end

function bool_rt_to_conditional(@nospecialize(rt), slottypes::Vector{Any}, state::VarTable, slot_id::Int)
    old = slottypes[slot_id]
    new = widenconditional((state[slot_id]::VarState).typ) # avoid nested conditional
    if new ⊑ old && !(old ⊑ new)
        if isa(rt, Const)
            val = rt.val
            if val === true
                return InterConditional(slot_id, new, Bottom)
            elseif val === false
                return InterConditional(slot_id, Bottom, new)
            end
        elseif rt === Bool
            return InterConditional(slot_id, new, new)
        end
    end
    return rt
end

# make as much progress on `frame` as possible (by handling cycles)
function typeinf_nocycle(interp::AbstractInterpreter, frame::InferenceState)
    typeinf_local(interp, frame)

    # If the current frame is part of a cycle, solve the cycle before finishing
    no_active_ips_in_callers = false
    while !no_active_ips_in_callers
        no_active_ips_in_callers = true
        for caller in frame.callers_in_cycle
            caller.dont_work_on_me && return false # cycle is above us on the stack
            if caller.pc´´ <= caller.nstmts # equivalent to `isempty(caller.ip)`
                # Note that `typeinf_local(interp, caller)` can potentially modify the other frames
                # `frame.callers_in_cycle`, which is why making incremental progress requires the
                # outer while loop.
                typeinf_local(interp, caller)
                no_active_ips_in_callers = false
            end
            caller.valid_worlds = intersect(caller.valid_worlds, frame.valid_worlds)
        end
    end
    return true
end
