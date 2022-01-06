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

function get_max_methods(mod::Module, interp::AbstractInterpreter)
    max_methods = ccall(:jl_get_module_max_methods, Cint, (Any,), mod) % Int
    max_methods < 0 ? InferenceParams(interp).MAX_METHODS : max_methods
end

const empty_bitset = BitSet()

function abstract_call_gf_by_type(interp::AbstractInterpreter, @nospecialize(f),
                                  arginfo::ArgInfo, @nospecialize(atype),
                                  sv::InferenceState, max_methods::Int = get_max_methods(sv.mod, interp))
    if sv.params.unoptimize_throw_blocks && is_stmt_throw_block(get_curr_ssaflag(sv))
        add_remark!(interp, sv, "Skipped call in throw block")
        return CallMeta(⊤, false)
    end

    argtypes = arginfo.argtypes
    matches = find_matching_methods(argtypes, atype, method_table(interp, sv), InferenceParams(interp).MAX_UNION_SPLITTING, max_methods)
    if isa(matches, FailedMethodMatch)
        add_remark!(interp, sv, matches.reason)
        return CallMeta(⊤, false)
    end

    (; valid_worlds, applicable, info) = matches
    update_valid_age!(sv, valid_worlds)
    napplicable = length(applicable)
    rettype = ⊥
    edges = MethodInstance[]
    conditionals = nothing # keeps refinement information of call argument types when the return type is boolean
    seen = 0               # number of signatures actually inferred
    any_const_result = false
    const_results = Union{InferenceResult,Nothing}[]
    multiple_matches = napplicable > 1

    if f !== nothing && napplicable == 1 && is_method_pure(applicable[1]::MethodMatch)
        val = pure_eval_call(f, argtypes)
        if val !== nothing
            # TODO: add some sort of edge(s)
            return CallMeta(val, MethodResultPure(info))
        end
    end

    fargs = arginfo.fargs
    for i in 1:napplicable
        match = applicable[i]::MethodMatch
        method = match.method
        sig = match.spec_types
        if bail_out_toplevel_call(interp, sig, sv)
            # only infer concrete call sites in top-level expressions
            add_remark!(interp, sv, "Refusing to infer non-concrete call site in top-level expression")
            rettype = ⊤
            break
        end
        this_rt = ⊥
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
                this_argtypes = isa(matches, MethodMatches) ? argtypes : matches.applicable_argtypes[i]
                this_arginfo = ArgInfo(fargs, this_argtypes)
                const_result = abstract_call_method_with_const_args(interp, result, f, this_arginfo, match, sv, false)
                if const_result !== nothing
                    const_rt, const_result = const_result
                    if const_rt !== rt && const_rt ⊑ rt
                        rt = const_rt
                    end
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
            if infer_compilation_signature(interp)
                # Also infer the compilation signature for this method, so it's available
                # to the compiler in case it ends up needing it (which is likely).
                csig = get_compileable_sig(method, sig, match.sparams)
                if csig !== nothing && csig !== sig
                    # The result of this inference is not directly used, so temporarily empty
                    # the use set for the current SSA value.
                    saved_uses = sv.ssavalue_uses[sv.currpc]
                    sv.ssavalue_uses[sv.currpc] = empty_bitset
                    abstract_call_method(interp, method, csig, match.sparams, multiple_matches, sv)
                    sv.ssavalue_uses[sv.currpc] = saved_uses
                end
            end

            result = abstract_call_method(interp, method, sig, match.sparams, multiple_matches, sv)
            this_rt, edge = result.rt, result.edge
            if edge !== nothing
                push!(edges, edge)
            end
            # try constant propagation with argtypes for this match
            # this is in preparation for inlining, or improving the return result
            this_argtypes = isa(matches, MethodMatches) ? argtypes : matches.applicable_argtypes[i]
            this_arginfo = ArgInfo(fargs, this_argtypes)
            const_result = abstract_call_method_with_const_args(interp, result, f, this_arginfo, match, sv, false)
            if const_result !== nothing
                const_this_rt, const_result = const_result
                if const_this_rt !== this_rt && const_this_rt ⊑ this_rt
                    this_rt = const_this_rt
                end
            end
            push!(const_results, const_result)
            if const_result !== nothing
                any_const_result = true
            end
        end
        this_conditional = this_rt
        this_rt = widenconditional(this_rt)
        @assert !isConditional(this_conditional) "invalid lattice element returned from inter-procedural context"
        seen += 1
        rettype = tmerge(rettype, this_rt)
        if this_conditional !== ⊥ && is_lattice_bool(rettype) && fargs !== nothing
            if conditionals === nothing
                conditionals = LatticeElement[⊥ for _ in 1:length(argtypes)],
                               LatticeElement[⊥ for _ in 1:length(argtypes)]
            end
            for i = 1:length(argtypes)
                cnd = conditional_argtype(this_conditional, sig, argtypes, i)
                conditionals[1][i] = tmerge(conditionals[1][i], cnd.vtype)
                conditionals[2][i] = tmerge(conditionals[2][i], cnd.elsetype)
            end
        end
        if bail_out_call(interp, rettype, sv)
            break
        end
    end

    if any_const_result && seen == napplicable
        @assert napplicable == nmatches(info) == length(const_results)
        info = ConstCallInfo(info, const_results)
    end

    rettype = from_interprocedural!(rettype, sv, arginfo, conditionals)

    if call_result_unused(sv) && rettype !== ⊥
        add_remark!(interp, sv, "Call result type was widened because the return value is unused")
        # We're mainly only here because the optimizer might want this code,
        # but we ourselves locally don't typically care about it locally
        # (beyond checking if it always throws).
        # So avoid adding an edge, since we don't want to bother attempting
        # to improve our result even if it does change (to always throw),
        # and avoid keeping track of a more complex result type.
        rettype = ⊤
    end
    add_call_backedges!(interp, rettype, edges, matches, atype, sv)
    if !isempty(sv.pclimitations) # remove self, if present
        delete!(sv.pclimitations, sv)
        for caller in sv.callers_in_cycle
            delete!(sv.pclimitations, caller)
        end
    end
    return CallMeta(rettype, info)
end

struct FailedMethodMatch
    reason::String
end

struct MethodMatches
    applicable::Vector{Any}
    info::MethodMatchInfo
    valid_worlds::WorldRange
    mt::Core.MethodTable
    fullmatch::Bool
end

struct UnionSplitMethodMatches
    applicable::Vector{Any}
    applicable_argtypes::Vector{Argtypes}
    info::UnionSplitInfo
    valid_worlds::WorldRange
    mts::Vector{Core.MethodTable}
    fullmatches::Vector{Bool}
end

function find_matching_methods(argtypes::Argtypes, @nospecialize(atype), method_table::MethodTableView,
                               union_split::Int, max_methods::Int)
    # NOTE this is valid as far as any "constant" lattice element doesn't represent `Union` type
    if 1 < unionsplitcost(argtypes) <= union_split
        split_argtypes = switchtupleunion(argtypes)
        infos = MethodMatchInfo[]
        applicable = Any[]
        applicable_argtypes = Argtypes[] # arrays like `argtypes`, including constants, for each match
        valid_worlds = WorldRange()
        mts = Core.MethodTable[]
        fullmatches = Bool[]
        for i in 1:length(split_argtypes)
            arg_n = split_argtypes[i]::Argtypes
            sig_n = argtypes_to_type(arg_n)
            mt = ccall(:jl_method_table_for, Any, (Any,), sig_n)
            mt === nothing && return FailedMethodMatch("Could not identify method table for call")
            mt = mt::Core.MethodTable
            matches = findall(sig_n, method_table; limit = max_methods)
            if matches === missing
                return FailedMethodMatch("For one of the union split cases, too many methods matched")
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
                    fullmatches[i] &= thisfullmatch
                    found = true
                    break
                end
            end
            if !found
                push!(mts, mt)
                push!(fullmatches, thisfullmatch)
            end
        end
        return UnionSplitMethodMatches(applicable,
                                       applicable_argtypes,
                                       UnionSplitInfo(infos),
                                       valid_worlds,
                                       mts,
                                       fullmatches)
    else
        mt = ccall(:jl_method_table_for, Any, (Any,), atype)
        if mt === nothing
            return FailedMethodMatch("Could not identify method table for call")
        end
        mt = mt::Core.MethodTable
        matches = findall(atype, method_table; limit = max_methods)
        if matches === missing
            # this means too many methods matched
            # (assume this will always be true, so we don't compute / update valid age in this case)
            return FailedMethodMatch("Too many methods matched")
        end
        fullmatch = _any(match->(match::MethodMatch).fully_covers, matches)
        return MethodMatches(matches.matches,
                             MethodMatchInfo(matches),
                             matches.valid_worlds,
                             mt,
                             fullmatch)
    end
end

"""
    from_interprocedural!(rt, sv::InferenceState, arginfo::ArgInfo, maybecondinfo) -> newrt

Converts inter-procedural return type `rt` into a local lattice element `newrt`,
that is appropriate in the context of current local analysis frame `sv`, especially:
- unwraps `rt::LimitedAccuracy` and collects its limitations into the current frame `sv`
- converts boolean `rt` to new boolean `newrt` in a way `newrt` can propagate extra conditional
  refinement information, e.g. translating `rt::InterConditional` into `newrt::Conditional`
  that holds a type constraint information about a variable in `sv`

This function _should_ be used wherever we propagate results returned from
`abstract_call_method` or `abstract_call_method_with_const_args`.

When `maybecondinfo !== nothing`, this function also tries extra conditional argument type refinement.
In such cases `maybecondinfo` should be either of:
- `maybecondinfo::Tuple{Vector{Any},Vector{Any}}`: precomputed argument type refinement information
- method call signature tuple type
When we deal with multiple `MethodMatch`es, it's better to precompute `maybecondinfo` by
`tmerge`ing argument signature type of each method call.
"""
function from_interprocedural!(rt::LatticeElement, sv::InferenceState, arginfo::ArgInfo, @nospecialize(maybecondinfo))
    rt = collect_limitations!(rt, sv)
    if is_lattice_bool(rt)
        if maybecondinfo === nothing
            rt = widenconditional(rt)
        else
            rt = from_interconditional(rt, sv, arginfo, maybecondinfo)
        end
    end
    @assert !isInterConditional(rt) "invalid lattice element returned from inter-procedural context"
    return rt
end

function collect_limitations!(typ::LatticeElement, sv::InferenceState)
    if isLimitedAccuracy(typ)
        union!(sv.pclimitations, causes(typ))
        return _ignorelimited(typ)
    end
    return typ
end

function from_interconditional(typ::LatticeElement, sv::InferenceState, (; fargs, argtypes)::ArgInfo, @nospecialize(maybecondinfo))
    fargs === nothing && return widenconditional(typ)
    slot = 0
    vtype = elsetype = ⊤
    condval = maybe_extract_const_bool(typ)
    for i in 1:length(fargs)
        # find the first argument which supports refinement,
        # and intersect all equivalent arguments with it
        arg = ssa_def_slot(fargs[i], sv)
        arg isa SlotNumber || continue # can't refine
        old = argtypes[i]
        unwraptype(old) isa Type || continue # unlikely to refine
        id = slot_id(arg)
        if slot == 0 || id == slot
            if isa(maybecondinfo, Tuple{Vector{LatticeElement},Vector{LatticeElement}})
                # if we have already computed argument refinement information, apply that now to get the result
                new_vtype = maybecondinfo[1][i]
                new_elsetype = maybecondinfo[2][i]
            else
                # otherwise compute it on the fly
                cnd = conditional_argtype(typ, maybecondinfo, argtypes, i)
                new_vtype = cnd.vtype
                new_elsetype = cnd.elsetype
            end
            if condval === false
                vtype = ⊥
            elseif new_vtype ⊑ vtype
                vtype = new_vtype
            else
                vtype = vtype ⊓ widenconst(new_vtype)
            end
            if condval === true
                elsetype = ⊥
            elseif new_elsetype ⊑ elsetype
                elsetype = new_elsetype
            else
                elsetype = elsetype ⊓ widenconst(new_elsetype)
            end
            if (slot > 0 || condval !== false) && vtype ⋤ old
                slot = id
            elseif (slot > 0 || condval !== true) && elsetype ⋤ old
                slot = id
            else # reset: no new useful information for this slot
                vtype = elsetype = ⊤
                if slot > 0
                    slot = 0
                end
            end
        end
    end
    if vtype === ⊥ && elsetype === ⊥
        return ⊥ # accidentally proved this call to be dead / throw !
    elseif slot > 0
        return Conditional(slot, vtype, elsetype) # record a Conditional improvement to this slot
    end
    return widenconditional(typ)
end

function conditional_argtype(rt::LatticeElement, @nospecialize(sig), argtypes::Vector{LatticeElement}, i::Int)
    if isInterConditional(rt)
        cnd = interconditional(rt)
        if cnd.slot_id == i
            return cnd
        end
    end
    vtype = elsetype = widenconditional(argtypes[i]) ⊓ fieldtype(sig, i)
    condval = maybe_extract_const_bool(rt)
    condval === true && (elsetype = ⊥)
    condval === false && (vtype = ⊥)
    return ConditionalInfo(i, vtype, elsetype, true)
end

function add_call_backedges!(interp::AbstractInterpreter, rettype::LatticeElement, edges::Vector{MethodInstance},
                             matches::Union{MethodMatches,UnionSplitMethodMatches}, @nospecialize(atype),
                             sv::InferenceState)
    # for `NativeInterpreter`, we don't add backedges when a new method couldn't refine (widen) this type
    rettype === ⊤ && return
    for edge in edges
        add_backedge!(edge, sv)
    end
    # also need an edge to the method table in case something gets
    # added that did not intersect with any existing method
    if isa(matches, MethodMatches)
        matches.fullmatch || add_mt_backedge!(matches.mt, atype, sv)
    else
        for (thisfullmatch, mt) in zip(matches.fullmatches, matches.mts)
            thisfullmatch || add_mt_backedge!(mt, atype, sv)
        end
    end
end

const RECURSION_UNUSED_MSG = "Bounded recursion detected with unused result. Annotated return type may be wider than true result."
const RECURSION_MSG = "Bounded recursion detected. Call was widened to force convergence."

function abstract_call_method(interp::AbstractInterpreter, method::Method, @nospecialize(sig), sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)
    if method.name === :depwarn && isdefined(Main, :Base) && method.module === Main.Base
        add_remark!(interp, sv, "Refusing to infer into `depwarn`")
        return MethodCallResult(⊤, false, false, nothing)
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
                    return MethodCallResult(⊤, true, true, nothing)
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
                return MethodCallResult(⊤, true, true, nothing)
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
    return MethodCallResult(LatticeElement(rt), edgecycle, edgelimited, edge)
end

# keeps result and context information of abstract method call, will be used by succeeding constant-propagation
struct MethodCallResult
    rt::LatticeElement
    edgecycle::Bool
    edgelimited::Bool
    edge::Union{Nothing,MethodInstance}
    function MethodCallResult(rt::LatticeElement,
                              edgecycle::Bool,
                              edgelimited::Bool,
                              edge::Union{Nothing,MethodInstance})
        return new(rt, edgecycle, edgelimited, edge)
    end
end

function abstract_call_method_with_const_args(interp::AbstractInterpreter, result::MethodCallResult,
                                              @nospecialize(f), arginfo::ArgInfo, match::MethodMatch,
                                              sv::InferenceState, va_override::Bool)
    mi = maybe_get_const_prop_profitable(interp, result, f, arginfo, match, sv)
    mi === nothing && return nothing
    # try constant prop'
    inf_cache = get_inference_cache(interp)
    inf_result = cache_lookup(mi, arginfo.argtypes, inf_cache)
    if inf_result === nothing
        # if there might be a cycle, check to make sure we don't end up
        # calling ourselves here.
        let result = result # prevent capturing
            if result.edgecycle && _any(InfStackUnwind(sv)) do infstate
                    # if the type complexity limiting didn't decide to limit the call signature (`result.edgelimited = false`)
                    # we can relax the cycle detection by comparing `MethodInstance`s and allow inference to
                    # propagate different constant elements if the recursion is finite over the lattice
                    return (result.edgelimited ? match.method === infstate.linfo.def : mi === infstate.linfo) &&
                            any(infstate.result.overridden_by_const)
                end
                add_remark!(interp, sv, "[constprop] Edge cycle encountered")
                return nothing
            end
        end
        inf_result = InferenceResult(mi, (arginfo, sv), va_override)
        if !any(inf_result.overridden_by_const)
            add_remark!(interp, sv, "[constprop] Could not handle constant info in matching_cache_argtypes")
            return nothing
        end
        frame = InferenceState(inf_result, #=cache=#:local, interp)
        frame === nothing && return nothing # this is probably a bad generated function (unsound), but just ignore it
        frame.parent = sv
        typeinf(interp, frame) || return nothing
    end
    result = inf_result.result
    # if constant inference hits a cycle, just bail out
    isa(result, InferenceState) && return nothing
    add_backedge!(mi, sv)
    return LatticeElement(result), inf_result
end

# if there's a possibility we could get a better result (hopefully without doing too much work)
# returns `MethodInstance` with constant arguments, returns nothing otherwise
function maybe_get_const_prop_profitable(interp::AbstractInterpreter, result::MethodCallResult,
                                         @nospecialize(f), arginfo::ArgInfo, match::MethodMatch,
                                         sv::InferenceState)
    if !InferenceParams(interp).ipo_constant_propagation
        add_remark!(interp, sv, "[constprop] Disabled by parameter")
        return nothing
    end
    method = match.method
    if method.constprop == 0x02
        add_remark!(interp, sv, "[constprop] Disabled by method parameter")
        return nothing
    end
    force = force_const_prop(interp, f, method)
    force || const_prop_entry_heuristic(interp, result, sv) || return nothing
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    length(arginfo.argtypes) < nargs && return nothing
    if !const_prop_argument_heuristic(interp, arginfo, sv)
        add_remark!(interp, sv, "[constprop] Disabled by argument and rettype heuristics")
        return nothing
    end
    all_overridden = is_all_overridden(arginfo, sv)
    if !force && !const_prop_function_heuristic(interp, f, arginfo, nargs, all_overridden, sv)
        add_remark!(interp, sv, "[constprop] Disabled by function heuristic")
        return nothing
    end
    force |= all_overridden
    mi = specialize_method(match; preexisting=!force)
    if mi === nothing
        add_remark!(interp, sv, "[constprop] Failed to specialize")
        return nothing
    end
    mi = mi::MethodInstance
    if !force && !const_prop_methodinstance_heuristic(interp, match, mi, arginfo, sv)
        add_remark!(interp, sv, "[constprop] Disabled by method instance heuristic")
        return nothing
    end
    return mi
end

function const_prop_entry_heuristic(interp::AbstractInterpreter, result::MethodCallResult, sv::InferenceState)
    if call_result_unused(sv) && result.edgecycle
        add_remark!(interp, sv, "[constprop] Disabled by entry heuristic (edgecycle with unused result)")
        return false
    end
    # check if this return type is improvable (i.e. whether it's possible that with more
    # information, we might get a more precise type)
    rt = result.rt
    if isNativeType(rt)
        # could always be improved to `Const`, `PartialStruct` or just a more precise type,
        # unless we're already at `Bottom`
        if rt === ⊥
            add_remark!(interp, sv, "[constprop] Disabled by entry heuristic (erroneous result)")
            return false
        else
            return true
        end
    elseif isPartialStruct(rt) || isInterConditional(rt)
        # could be improved to `Const` or a more precise wrapper
        return true
    elseif isLimitedAccuracy(rt)
        # optimizations like inlining are disabled for limited frames,
        # thus there won't be much benefit in constant-prop' here
        add_remark!(interp, sv, "[constprop] Disabled by entry heuristic (limited accuracy)")
        return false
    else
        add_remark!(interp, sv, "[constprop] Disabled by entry heuristic (unimprovable return type)")
        return false
    end
end

# determines heuristically whether if constant propagation can be worthwhile
# by checking if any of given `argtypes` is "interesting" enough to be propagated
function const_prop_argument_heuristic(_::AbstractInterpreter, (; fargs, argtypes)::ArgInfo, sv::InferenceState)
    for i in 1:length(argtypes)
        a = argtypes[i]
        if isConditional(a) && fargs !== nothing
            is_const_prop_profitable_conditional(conditional(a), fargs, sv) && return true
        else
            has_nontrivial_const_info(a) && is_const_prop_profitable_arg(a) && return true
        end
    end
    return false
end

function is_const_prop_profitable_arg(arg::LatticeElement)
    # have new information from argtypes that wasn't available from the signature
    if isPartialStruct(arg)
        for b in partialfields(arg)
            isconstType(widenconst(b)) && return true
            is_const_prop_profitable_arg(b) && return true
        end
    end
    isPartialOpaque(arg) && return true
    isConst(arg) || return true
    val = constant(arg)
    # don't consider mutable values or Strings useful constants
    return isa(val, Symbol) || isa(val, Type) || (!isa(val, String) && !ismutable(val))
end

function is_const_prop_profitable_conditional(cnd::ConditionalInfo, fargs::Vector{Any}, sv::InferenceState)
    slotid = find_constrained_arg(cnd, fargs, sv)
    if slotid !== nothing
        return true
    end
    # as a minor optimization, we just check the result is a constant or not,
    # since both `has_nontrivial_const_info`/`is_const_prop_profitable_arg` return `true`
    # for `Const(::Bool)`
    return isConst(cnd)
end

function find_constrained_arg(cnd::ConditionalInfo, fargs::Vector{Any}, sv::InferenceState)
    slot = cnd.slot_id
    for i in 1:length(fargs)
        arg = ssa_def_slot(fargs[i], sv)
        if isa(arg, SlotNumber) && slot_id(arg) == slot
            return i
        end
    end
    return nothing
end

# checks if all argtypes has additional information other than what `Type` can provide
function is_all_overridden((; fargs, argtypes)::ArgInfo, sv::InferenceState)
    for a in argtypes
        if isConditional(a) && fargs !== nothing
            is_const_prop_profitable_conditional(conditional(a), fargs, sv) || return false
        else
            a = widenconditional(a)
            is_forwardable_argtype(a) || return false
        end
    end
    return true
end

function force_const_prop(interp::AbstractInterpreter, @nospecialize(f), method::Method)
    return method.constprop == 0x01 ||
           InferenceParams(interp).aggressive_constant_propagation ||
           istopfunction(f, :getproperty) ||
           istopfunction(f, :setproperty!)
end

function const_prop_function_heuristic(
    _::AbstractInterpreter, @nospecialize(f), (; argtypes)::ArgInfo,
    nargs::Int, all_overridden::Bool, _::InferenceState)
    if nargs > 1
        if istopfunction(f, :getindex) || istopfunction(f, :setindex!)
            arrty = unwraptype(argtypes[2])
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
    if !all_overridden && (istopfunction(f, :+) || istopfunction(f, :-) || istopfunction(f, :*) ||
                           istopfunction(f, :(==)) || istopfunction(f, :!=) ||
                           istopfunction(f, :<=) || istopfunction(f, :>=) || istopfunction(f, :<) || istopfunction(f, :>) ||
                           istopfunction(f, :<<) || istopfunction(f, :>>))
        # it is almost useless to inline the op when all the same type,
        # but highly worthwhile to inline promote of a constant
        length(argtypes) > 2 || return false
        t1 = widenconst(argtypes[2])
        for i in 3:length(argtypes)
            at = argtypes[i]
            ty = isVararg(at) ? unwraptv(vararg(at)) : widenconst(at)
            if ty !== t1
                return true
            end
        end
        return false
    end
    return true
end

# This is a heuristic to avoid trying to const prop through complicated functions
# where we would spend a lot of time, but are probably unlikely to get an improved
# result anyway.
function const_prop_methodinstance_heuristic(
    interp::AbstractInterpreter, match::MethodMatch, mi::MethodInstance,
    (; argtypes)::ArgInfo, sv::InferenceState)
    method = match.method
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
    if isdefined(method, :source) && ccall(:jl_ir_flag_inlineable, Bool, (Any,), method.source)
        return true
    else
        flag = get_curr_ssaflag(sv)
        if is_stmt_inline(flag)
            # force constant propagation for a call that is going to be inlined
            # since the inliner will try to find this constant result
            # if these constant arguments arrive there
            return true
        elseif is_stmt_noinline(flag)
            # this call won't be inlined, thus this constant-prop' will most likely be unfruitful
            return false
        else
            code = get(code_cache(interp), mi, nothing)
            if isdefined(code, :inferred) && inlining_policy(
                    interp, code.inferred, IR_FLAG_NULL, mi, argtypes) !== nothing
                return true
            end
        end
    end
    return false # the cache isn't inlineable, so this constant-prop' will most likely be unfruitful
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

const ANY_VARARGS = LatticeElement[NativeType(Vararg{Any})]

# `typ` is the inferred type for expression `arg`.
# if the expression constructs a container (e.g. `svec(x,y,z)`),
# refine its type to an array of element types.
# Union of Tuples of the same length is converted to Tuple of Unions.
# returns an array of types
function precise_container_type(interp::AbstractInterpreter, itft::LatticeElement, typ::LatticeElement, sv::InferenceState)
    if isPartialStruct(typ) && typ.typ.name === Tuple.name
        return partialfields(typ), nothing
    end

    if isConst(typ)
        val = constant(typ)
        if isa(val, SimpleVector) || isa(val, Tuple)
            return LatticeElement[ Const(val[i]) for i in 1:length(val) ], nothing # avoid making a tuple Generator here!
        end
    end

    tti0 = widenconst(typ)
    tti = unwrap_unionall(tti0)
    if isa(tti, DataType) && tti.name === NamedTuple_typename
        # A NamedTuple iteration is the same as the iteration of its Tuple parameter:
        # compute a new `tti == unwrap_unionall(tti0)` based on that Tuple type
        tti = unwraptv(tti.parameters[2])
        tti0 = rewrap_unionall(tti, tti0)
    end
    if isa(tti, Union)
        utis = uniontypes(tti)
        if _any(@nospecialize(t) -> !isa(t, DataType) || !(t <: Tuple) || !isknownlength(t), utis)
            return ANY_VARARGS, nothing
        end
        ltp = length((utis[1]::DataType).parameters)
        for t in utis
            if length((t::DataType).parameters) != ltp
                return ANY_VARARGS, nothing
            end
        end
        result = LatticeElement[ ⊥ for _ in 1:ltp ]
        for t in utis
            tps = (t::DataType).parameters
            _all(valid_as_lattice, tps) || continue
            for j in 1:ltp
                result[j] = tmerge(result[j], rewrap_unionall(tps[j], tti0))
            end
        end
        return result, nothing
    elseif tti0 <: Tuple
        if isa(tti0, DataType)
            return LatticeElement[ NativeType(p) for p in tti0.parameters ], nothing
        elseif !isa(tti, DataType)
            return ANY_VARARGS, nothing
        else
            len = length(tti.parameters)
            last = tti.parameters[len]
            va = isvarargtype(last)
            elts = LatticeElement[ NativeType(fieldtype(tti0, i)) for i = 1:len ]
            if va
                elts[len] = NativeType(Vararg{widenconst(elts[len])})
            end
            return elts, nothing
        end
    elseif tti0 === SimpleVector || tti0 === Any
        return ANY_VARARGS, nothing
    elseif tti0 <: Array
        return LatticeElement[NativeType(Vararg{eltype(tti0)})], nothing
    else
        return abstract_iteration(interp, itft, typ, sv)
    end
end

# simulate iteration protocol on container type up to fixpoint
function abstract_iteration(interp::AbstractInterpreter, itft::LatticeElement, itertype::LatticeElement, sv::InferenceState)
    if isConst(itft)
        iteratef = constant(itft)
    else
        return ANY_VARARGS, nothing
    end
    @assert !isvarargtype(widenconst(itertype))
    call = abstract_call_known(interp, iteratef, ArgInfo(nothing, LatticeElement[itft, itertype]), sv)
    stateordonet = call.rt
    info = call.info
    # Return Bottom if this is not an iterator.
    # WARNING: Changes to the iteration protocol must be reflected here,
    # this is not just an optimization.
    # TODO: this doesn't realize that Array, SimpleVector, Tuple, and NamedTuple do not use the iterate protocol
    stateordonet === ⊥ && return LatticeElement[⊥], AbstractIterationInfo(CallMeta[CallMeta(⊥, info)])
    valtype = statetype = ⊥
    ret = LatticeElement[]
    calls = CallMeta[call]
    stateordonet_widened = widenconst(stateordonet)

    # Try to unroll the iteration up to MAX_TUPLE_SPLAT, which covers any finite
    # length iterators, or interesting prefix
    while true
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
            return LatticeElement[⊥], nothing
        end
        valtype = getfield_tfunc(stateordonet, Const(1))
        push!(ret, valtype)
        statetype = nstatetype
        call = abstract_call_known(interp, iteratef, ArgInfo(nothing, LatticeElement[Const(iteratef), itertype, statetype]), sv)
        stateordonet = call.rt
        stateordonet_widened = widenconst(stateordonet)
        push!(calls, call)
    end
    # From here on, we start asking for results on the widened types, rather than
    # the precise (potentially const) state type
    # statetype and valtype are reinitialized in the first iteration below from the
    # (widened) stateordonet, which has not yet been fully analyzed in the loop above
    valtype = statetype = Bottom
    valtype0 = statetype0 = ⊥
    may_have_terminated = Nothing <: stateordonet_widened
    while valtype !== Any
        nounion = typeintersect(stateordonet_widened, Tuple{Any,Any})
        if nounion !== Bottom && !isa(nounion, DataType)
            # nounion is of a type we cannot handle
            valtype = Any
            break
        end
        if nounion === Bottom || (nounion.parameters[1] <: valtype && nounion.parameters[2] <: statetype)
            # reached a fixpoint or iterator failed/gave invalid answer
            if !hasintersect(stateordonet_widened, Nothing)
                # ... but cannot terminate
                if !may_have_terminated
                    #  ... and cannot have terminated prior to this loop
                    return LatticeElement[⊥], nothing
                else
                    # iterator may have terminated prior to this loop, but not during it
                    valtype = Bottom
                end
            end
            break
        end
        valtype0 = tmerge(valtype0, nounion.parameters[1])
        valtype = widenconst(valtype0)
        statetype0 = tmerge(statetype0, nounion.parameters[2])
        statetype = widenconst(statetype0)
        stateordonet = abstract_call_known(interp, iteratef, ArgInfo(nothing, LatticeElement[Const(iteratef), itertype, statetype0]), sv).rt
        stateordonet_widened = widenconst(stateordonet)
    end
    if valtype !== Bottom
        push!(ret, NativeType(Vararg{valtype}))
    end
    return ret, nothing
end

# do apply(af, fargs...), where af is a function value
function abstract_apply(interp::AbstractInterpreter, argtypes::Argtypes, sv::InferenceState,
                        max_methods::Int = get_max_methods(sv.mod, interp))
    itft = argtype_by_index(argtypes, 2)
    aft = argtype_by_index(argtypes, 3)
    (itft === ⊥ || aft === ⊥) && return CallMeta(⊥, false)
    aargtypes = argtype_tail(argtypes, 4)
    aftw = widenconst(aft)
    if !isConst(aft) && !isPartialOpaque(aft) && (!isType(aftw) || has_free_typevars(aftw))
        if !isconcretetype(aftw) || (aftw <: Builtin)
            add_remark!(interp, sv, "Core._apply_iterate called on a function of a non-concrete type")
            # bail now, since it seems unlikely that abstract_call will be able to do any better after splitting
            # this also ensures we don't call abstract_call_gf_by_type below on an IntrinsicFunction or Builtin
            return CallMeta(⊤, false)
        end
    end
    res = ⊥
    nargs = length(aargtypes)
    splitunions = 1 < unionsplitcost(aargtypes) <= InferenceParams(interp).MAX_APPLY_UNION_ENUM
    ctypes = Vector{LatticeElement}[LatticeElement[aft]]
    infos = Vector{MaybeAbstractIterationInfo}[MaybeAbstractIterationInfo[]]
    for i = 1:nargs
        ctypes´ = Vector{LatticeElement}[]
        infos′ = Vector{MaybeAbstractIterationInfo}[]
        at = unwraptype(aargtypes[i])
        for ti in (splitunions ? uniontypes(at) : Any[at])
            if !isVararg(ti)
                cti_info = precise_container_type(interp, itft, LatticeElement(ti), sv)
                cti = cti_info[1]::Argtypes
                info = cti_info[2]::MaybeAbstractIterationInfo
            else
                cti_info = precise_container_type(interp, itft, NativeType(unwrapva(vararg(ti))), sv)
                cti = cti_info[1]::Argtypes
                info = cti_info[2]::MaybeAbstractIterationInfo
                # We can't represent a repeating sequence of the same types,
                # so tmerge everything together to get one type that represents
                # everything.
                argt = cti[end]
                if isVararg(argt)
                    argt = NativeType(unwrapva(vararg(argt)))
                end
                for i in 1:(length(cti)-1)
                    argt = tmerge(argt, cti[i])
                end
                cti = LatticeElement[NativeType(Vararg{widenconst(argt)})]
            end
            if _any(t -> t === ⊥, cti)
                continue
            end
            for j = 1:length(ctypes)
                ct = ctypes[j]::Vector{LatticeElement}
                if isVararg(ct[end])
                    # This is vararg, we're not gonna be able to do any inling,
                    # drop the info
                    info = nothing
                    tail = tuple_tail_elem(ct[end], cti)
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
            if isVararg(ct[i])
                ct[i] = tuple_tail_elem(ct[i], ct[(i+1):lct])
                resize!(ct, i)
                break
            end
        end
        call = abstract_call(interp, ArgInfo(nothing, ct), sv, max_methods)
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
        mi = specialize_method(method, sig, sparams)
        isa(mi, MethodInstance) || return false
        staged = get_staged(mi)
        (staged isa CodeInfo && (staged::CodeInfo).pure) || return false
        return true
    end
    return method.pure
end
is_method_pure(match::MethodMatch) = is_method_pure(match.method, match.spec_types, match.sparams)

function pure_eval_call(@nospecialize(f), argtypes::Argtypes)
    for i = 2:length(argtypes)
        a = argtypes[i]
        if !(isConst(a) || isconstType(widenconst(a)))
            return nothing
        end
    end

    args = Any[ (a = widenconditional(argtypes[i]);
        isConst(a) ? constant(a) : (widenconst(a)::DataType).parameters[1]) for i in 2:length(argtypes) ]
    try
        value = Core._apply_pure(f, args)
        return Const(value)
    catch
        return nothing
    end
end

function argtype_by_index(argtypes::Argtypes, i::Int)
    n = length(argtypes)
    na = unwraptype(argtypes[n])
    if isVararg(na)
        return i >= n ? NativeType(unwrapva(vararg(na))) : argtypes[i]
    else
        return i > n ? ⊥ : argtypes[i]
    end
end

function argtype_tail(argtypes::Argtypes, i::Int)
    n = length(argtypes)
    if isVararg(argtypes[n]) && i > n
        i = n
    end
    return argtypes[i:n]
end

function abstract_call_builtin(
    interp::AbstractInterpreter, f::Builtin, (; fargs, argtypes)::ArgInfo,
    sv::InferenceState, max_methods::Int)
    @nospecialize f
    la = length(argtypes)
    if f === Core.ifelse && fargs isa Vector{Any} && la == 4
        typ = argtypes[2]
        if isConditional(typ)
            cnd = conditional(typ)
            tx = argtypes[3]
            ty = argtypes[4]
            if isConst(typ)
                # if `cnd` is constant, we should just respect its constantness to keep inference accuracy
                return constant(typ)::Bool ? tx : ty
            else
                # try to simulate this as a real conditional (`cnd ? x : y`), so that the penalty for using `ifelse` instead isn't too high
                a = ssa_def_slot(fargs[3], sv)
                b = ssa_def_slot(fargs[4], sv)
                if isa(a, SlotNumber) && cnd.slot_id == slot_id(a)
                    tx = (cnd.vtype ⊑ tx ? cnd.vtype : (tx ⊓ widenconst(cnd.vtype)))
                end
                if isa(b, SlotNumber) && cnd.slot_id == slot_id(b)
                    ty = (cnd.elsetype ⊑ ty ? cnd.elsetype : (ty ⊓ widenconst(cnd.elsetype)))
                end
                return tmerge(tx, ty)
            end
        end
    end
    rt = builtin_tfunction(interp, f, argtypes[2:end], sv)
    if is_lattice_bool(rt) && isa(fargs, Vector{Any})
        # perform very limited back-propagation of type information for `is` and `isa`
        val = isConst(rt) ? constant(rt)::Bool : nothing
        if f === isa
            a = ssa_def_slot(fargs[2], sv)
            if isa(a, SlotNumber)
                aty0 = widenconditional(argtypes[2])
                aty = widenconst(aty0)
                if val === false
                    return Conditional(slot_id(a), ⊥, aty0)
                elseif val === true
                    return Conditional(slot_id(a), aty0, ⊥)
                end
                tty_ub, isexact_tty = instanceof_tfunc(argtypes[3])
                if isexact_tty && !isa(tty_ub, TypeVar)
                    tty_lb = tty_ub # TODO: this would be wrong if !isexact_tty, but instanceof_tfunc doesn't preserve this info
                    if !has_free_typevars(tty_lb) && !has_free_typevars(tty_ub)
                        ifty = typeintersect(aty, tty_ub)
                        valid_as_lattice(ifty) || (ifty = Union{})
                        elty = typesubtract(aty, tty_lb, InferenceParams(interp).MAX_UNION_SPLITTING)
                        return Conditional(slot_id(a), NativeType(ifty), NativeType(elty))
                    end
                end
            end
        elseif f === (===)
            a = ssa_def_slot(fargs[2], sv)
            b = ssa_def_slot(fargs[3], sv)
            aty = argtypes[2]
            bty = argtypes[3]
            # if doing a comparison to a singleton, consider returning a `Conditional` instead
            if isConst(aty) && isa(b, SlotNumber)
                if val === false
                    aty = ⊥
                elseif val === true
                    bty = ⊥
                elseif widenconst(bty) isa Type && isdefined(typeof(constant(aty)), :instance) # can only widen a if it is a singleton
                    bty = NativeType(typesubtract(widenconst(bty), typeof(constant(aty)), InferenceParams(interp).MAX_UNION_SPLITTING))
                end
                return Conditional(slot_id(b), aty, bty)
            end
            if isConst(bty) && isa(a, SlotNumber)
                if val === false
                    bty = ⊥
                elseif val === true
                    aty = ⊥
                elseif widenconst(aty) isa Type && isdefined(typeof(constant(bty)), :instance) # same for b
                    aty = NativeType(typesubtract(widenconst(aty), typeof(constant(bty)), InferenceParams(interp).MAX_UNION_SPLITTING))
                end
                return Conditional(slot_id(a), bty, aty)
            end
            # narrow the lattice slightly (noting the dependency on one of the slots), to promote more effective smerge
            if isa(b, SlotNumber)
                return Conditional(slot_id(b), val === false ? ⊥ : bty, val === true ? ⊥ : bty)
            end
            if isa(a, SlotNumber)
                return Conditional(slot_id(a), val === false ? ⊥ : aty, val === true ? ⊥ : aty)
            end
        elseif f === Core.Compiler.not_int
            aty = argtypes[2]
            if isConditional(aty)
                cnd = conditional(aty)
                ifty = cnd.elsetype
                elty = cnd.vtype
                if val === false
                    ifty = ⊥
                elseif val === true
                    elty = ⊥
                end
                return Conditional(cnd.slot_id, ifty, elty)
            end
        elseif f === isdefined
            uty = widenconst(argtypes[2])
            a = ssa_def_slot(fargs[2], sv)
            if isa(uty, Union) && isa(a, SlotNumber)
                fld = argtypes[3]
                vtype = ⊥
                elsetype= ⊥
                for ty in uniontypes(uty)
                    ty = NativeType(ty)
                    cnd = isdefined_tfunc(ty, fld)
                    if isConst(cnd)
                        if constant(cnd)::Bool
                            vtype = tmerge(vtype, ty)
                        else
                            elsetype = tmerge(elsetype, ty)
                        end
                    else
                        vtype = tmerge(vtype, ty)
                        elsetype = tmerge(elsetype, ty)
                    end
                end
                return Conditional(a.id, vtype, elsetype)
            end
        end
    end
    return LatticeElement(rt)
end

function abstract_call_unionall(argtypes::Argtypes)
    if length(argtypes) == 3
        canconst = true
        a3 = argtypes[3]
        a3 = unwraptype(a3)
        if isConst(a3)
            body = constant(a3)
        elseif isType(a3)
            body = a3.parameters[1]
            canconst = false
        else
            return ⊤
        end
        if !isa(body, Type) && !isa(body, TypeVar)
            return ⊤
        end
        if has_free_typevars(body)
            a2 = argtypes[2]
            if isConst(a2)
                tv = constant(a2)
            elseif isPartialTypeVar(a2)
                tv = partialtypevar(a2).tv
                canconst = false
            else
                return ⊤
            end
            !isa(tv, TypeVar) && return ⊤
            body = UnionAll(tv, body)
        end
        return canconst ? Const(body) : NativeType(Type{body})
    end
    return ⊤
end

function abstract_invoke(interp::AbstractInterpreter, (; fargs, argtypes)::ArgInfo, sv::InferenceState)
    ft′ = argtype_by_index(argtypes, 2)
    ft = widenconst(ft′)
    ft === Bottom && return CallMeta(⊥, false)
    (types, isexact, isconcrete, istype) = instanceof_tfunc(argtype_by_index(argtypes, 3))
    types === Bottom && return CallMeta(⊥, false)
    isexact || return CallMeta(⊤, false)
    argtype = argtypes_to_type(argtype_tail(argtypes, 4))
    nargtype = typeintersect(types, argtype)
    nargtype === Bottom && return CallMeta(⊥, false)
    nargtype isa DataType || return CallMeta(⊤, false) # other cases are not implemented below
    isdispatchelem(ft) || return CallMeta(⊤, false) # check that we might not have a subtype of `ft` at runtime, before doing supertype lookup below
    ft = ft::DataType
    types = rewrap_unionall(Tuple{ft, unwrap_unionall(types).parameters...}, types)::Type
    nargtype = Tuple{ft, nargtype.parameters...}
    argtype = Tuple{ft, argtype.parameters...}
    result = findsup(types, method_table(interp))
    result === nothing && return CallMeta(⊤, false)
    method, valid_worlds = result
    update_valid_age!(sv, valid_worlds)
    (ti, env::SimpleVector) = ccall(:jl_type_intersection_with_env, Any, (Any, Any), nargtype, method.sig)::SimpleVector
    (; rt, edge) = result = abstract_call_method(interp, method, ti, env, false, sv)
    edge !== nothing && add_backedge!(edge::MethodInstance, sv)
    match = MethodMatch(ti, env, method, argtype <: method.sig)
    res = nothing
    sig = match.spec_types
    argtypes′ = invoke_rewrite(argtypes)
    fargs′ = fargs === nothing ? nothing : invoke_rewrite(fargs)
    arginfo = ArgInfo(fargs′, argtypes′)
    # # typeintersect might have narrowed signature, but the accuracy gain doesn't seem worth the cost involved with the lattice comparisons
    # for i in 1:length(argtypes′)
    #     t, a = ti.parameters[i], argtypes′[i]
    #     argtypes′[i] = t ⊑ a ? t : a
    # end
    const_result = abstract_call_method_with_const_args(interp, result, singleton_type(ft′), arginfo, match, sv, false)
    if const_result !== nothing
        const_rt, const_result = const_result
        if const_rt !== rt && const_rt ⊑ rt
            rt, res = const_rt, const_result
        end
    end
    return CallMeta(from_interprocedural!(rt, sv, arginfo, sig), InvokeCallInfo(match, res))
end

function invoke_rewrite(xs::Vector)
    x0 = xs[2]
    newxs = xs[3:end]
    newxs[1] = x0
    return newxs
end

# call where the function is known exactly
function abstract_call_known(
    interp::AbstractInterpreter, @nospecialize(f), arginfo::ArgInfo,
    sv::InferenceState, max_methods::Int = get_max_methods(sv.mod, interp))
    (; fargs, argtypes) = arginfo
    la = length(argtypes)

    if isa(f, Builtin)
        if f === _apply_iterate
            return abstract_apply(interp, argtypes, sv, max_methods)
        elseif f === invoke
            return abstract_invoke(interp, arginfo, sv)
        elseif f === modifyfield!
            return abstract_modifyfield!(interp, argtypes, sv)
        end
        return CallMeta(abstract_call_builtin(interp, f, arginfo, sv, max_methods), false)
    elseif isa(f, Core.OpaqueClosure)
        # calling an OpaqueClosure about which we have no information returns no information
        return CallMeta(Any, false)
    elseif f === Core.kwfunc
        if la == 2
            aty = argtypes[2]
            if !isVararg(aty)
                ft = widenconst(aty)
                if isa(ft, DataType) && isdefined(ft.name, :mt) && isdefined(ft.name.mt, :kwsorter)
                    return CallMeta(Const(ft.name.mt.kwsorter), MethodResultPure())
                end
            end
        end
        return CallMeta(⊤, false)
    elseif f === TypeVar
        # Manually look through the definition of TypeVar to
        # make sure to be able to get `PartialTypeVar`s out.
        (la < 2 || la > 4) && return CallMeta(⊥, false)
        n = argtypes[2]
        ub_var = Const(Any)
        lb_var = Const(Bottom)
        if la == 4
            ub_var = argtypes[4]
            lb_var = argtypes[3]
        elseif la == 3
            ub_var = argtypes[3]
        end
        return CallMeta(typevar_tfunc(n, lb_var, ub_var), false)
    elseif f === UnionAll
        return CallMeta(abstract_call_unionall(argtypes), false)
    elseif f === Tuple && la == 2
        aty = argtypes[2]
        ty = isVararg(aty) ? unwrapva(vararg(aty)) : widenconst(aty)
        if !isconcretetype(ty)
            return CallMeta(NativeType(Tuple), false)
        end
    elseif is_return_type(f)
        return return_type_tfunc(interp, argtypes, sv)
    elseif la == 2 && istopfunction(f, :!)
        # handle Conditional propagation through !Bool
        aty = argtypes[2]
        if isConditional(aty)
            cnd = conditional(aty)
            call = abstract_call_gf_by_type(interp, f, ArgInfo(fargs, LatticeElement[Const(f), LBool]), Tuple{typeof(f), Bool}, sv, max_methods) # make sure we've inferred `!(::Bool)`
            return CallMeta(Conditional(cnd.slot_id, cnd.elsetype, cnd.vtype), call.info)
        end
    elseif la == 3 && istopfunction(f, :!==)
        # mark !== as exactly a negated call to ===
        rty = abstract_call_known(interp, (===), ArgInfo(fargs, argtypes), sv, max_methods).rt
        if isConditional(rty)
            cnd = conditional(rty)
            return CallMeta(Conditional(cnd.slot_id, cnd.elsetype, cnd.vtype), false) # swap if-else
        elseif isConst(rty)
            return CallMeta(Const(constant(rty) === false), MethodResultPure())
        end
        return CallMeta(LatticeElement(rty), false)
    elseif la == 3 && istopfunction(f, :(>:))
        # mark issupertype as a exact alias for issubtype
        # swap T1 and T2 arguments and call <:
        if fargs !== nothing && length(fargs) == 3
            fargs = Any[<:, fargs[3], fargs[2]]
        else
            fargs = nothing
        end
        argtypes = LatticeElement[NativeType(typeof(<:)), argtypes[3], argtypes[2]]
        call = abstract_call_known(interp, <:, ArgInfo(fargs, argtypes), sv, max_methods)
        return CallMeta(call.rt, false)
    elseif la == 2 &&
           (a2 = argtypes[2]; isConst(a2)) && (svecval = constant(a2); isa(svecval, SimpleVector)) &&
           istopfunction(f, :length)
        # mark length(::SimpleVector) as @pure
        return CallMeta(Const(length(svecval)), MethodResultPure())
    elseif la == 3 &&
           (a2 = argtypes[2]; isConst(a2)) && (svecval = constant(a2); isa(svecval, SimpleVector)) &&
           (a3 = argtypes[3]; isConst(a3)) && (idx = constant(a3); isa(idx, Int)) &&
           istopfunction(f, :getindex)
        # mark getindex(::SimpleVector, i::Int) as @pure
        if 1 <= idx <= length(svecval) && isassigned(svecval, idx)
            return CallMeta(Const(getindex(svecval, idx)), MethodResultPure())
        end
    elseif la == 2 && istopfunction(f, :typename)
        return CallMeta(LatticeElement(typename_static(argtypes[2])), MethodResultPure())
    elseif max_methods > 1 && istopfunction(f, :copyto!)
        max_methods = 1
    elseif la == 3 && istopfunction(f, :typejoin)
        val = pure_eval_call(f, argtypes)
        return CallMeta(val === nothing ? NativeType(Type) : val, MethodResultPure())
    end
    atype = argtypes_to_type(argtypes)
    return abstract_call_gf_by_type(interp, f, arginfo, atype, sv, max_methods)
end

function abstract_call_opaque_closure(interp::AbstractInterpreter, closure::PartialOpaque, arginfo::ArgInfo, sv::InferenceState)
    sig = argtypes_to_type(arginfo.argtypes)
    (; rt, edge) = result = abstract_call_method(interp, closure.source, sig, Core.svec(), false, sv)
    edge !== nothing && add_backedge!(edge, sv)
    tt = closure.typ
    sigT = (unwrap_unionall(tt)::DataType).parameters[1]
    match = MethodMatch(sig, Core.svec(), closure.source, sig <: rewrap_unionall(sigT, tt))
    res = nothing
    if !result.edgecycle
        const_result = abstract_call_method_with_const_args(interp, result, closure,
            arginfo, match, sv, closure.isva)
        if const_result !== nothing
            const_rettype, const_result = const_result
            if const_rettype ⊑ rt
               rt, res = const_rettype, const_result
            end
        end
    end
    info = OpaqueClosureCallInfo(match, res)
    return CallMeta(from_interprocedural!(rt, sv, arginfo, match.spec_types), info)
end

function most_general_argtypes(closure::PartialOpaque)
    argt = (unwrap_unionall(closure.typ)::DataType).parameters[1]
    if !isa(argt, DataType) || argt.name !== typename(Tuple)
        argt = Tuple
    end
    return most_general_argtypes(closure.source, argt, closure.isva, false)
end

# call where the function is any lattice element
function abstract_call(interp::AbstractInterpreter, arginfo::ArgInfo,
                       sv::InferenceState, max_methods::Int = get_max_methods(sv.mod, interp))
    argtypes = arginfo.argtypes
    ft = argtypes[1]
    f = singleton_type(ft)
    if isPartialOpaque(ft)
        poc = partialopaque(ft)
        newargtypes = copy(argtypes)
        newargtypes[1] = LatticeElement(poc.env)
        return abstract_call_opaque_closure(interp, poc, ArgInfo(arginfo.fargs, newargtypes), sv)
    elseif (uft = unwrap_unionall(widenconst(ft)); isa(uft, DataType) && uft.name === typename(Core.OpaqueClosure))
        rt = NativeType(rewrap_unionall((uft::DataType).parameters[2], widenconst(ft)))
        return CallMeta(rt, false)
    elseif f === nothing
        # non-constant function, but the number of arguments is known
        # and the ft is not a Builtin or IntrinsicFunction
        if hasintersect(widenconst(ft), Union{Builtin, Core.OpaqueClosure})
            add_remark!(interp, sv, "Could not identify method table for call")
            return CallMeta(⊤, false)
        end
        return abstract_call_gf_by_type(interp, nothing, arginfo, argtypes_to_type(argtypes), sv, max_methods)
    end
    return abstract_call_known(interp, f, arginfo, sv, max_methods)
end

function sp_type_rewrap(@nospecialize(T), linfo::MethodInstance, isreturn::Bool)
    isref = false
    if T === Bottom
        return ⊥
    elseif isa(T, Type)
        if isa(T, DataType) && (T::DataType).name === _REF_NAME
            isref = true
            T = T.parameters[1]
            if isreturn && T === Any
                return ⊥ # a return type of Ref{Any} is invalid
            end
        end
    else
        return ⊤
    end
    if isa(linfo.def, Method)
        spsig = linfo.def.sig
        if isa(spsig, UnionAll)
            if !isempty(linfo.sparam_vals)
                sparam_vals = Any[isvarargtype(v) ? TypeVar(:N, Bottom, Any) :
                                  v for v in  linfo.sparam_vals]
                T = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), T, spsig, sparam_vals)
                isref && isreturn && T === Any && return ⊥ # catch invalid return Ref{T} where T = Any
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
    return NativeType(unwraptv(T))
end

function abstract_eval_cfunction(interp::AbstractInterpreter, e::Expr, vtypes::VarTable, sv::InferenceState)
    f = abstract_eval_value(interp, e.args[2], vtypes, sv)
    # rt = sp_type_rewrap(e.args[3], sv.linfo, true)
    at = LatticeElement[ sp_type_rewrap(argt, sv.linfo, false) for argt in e.args[4]::SimpleVector ]
    pushfirst!(at, f)
    # this may be the wrong world for the call,
    # but some of the result is likely to be valid anyways
    # and that may help generate better codegen
    abstract_call(interp, ArgInfo(nothing, at), sv)
    nothing
end

function abstract_eval_value_expr(interp::AbstractInterpreter, e::Expr, vtypes::VarTable, sv::InferenceState)
    if e.head === :static_parameter
        n = e.args[1]::Int
        return 1 <= n <= length(sv.sptypes) ? sv.sptypes[n] : ⊤
    elseif e.head === :boundscheck
        return LBool
    else
        return ⊤
    end
end

function abstract_eval_special_value(interp::AbstractInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if isa(e, QuoteNode)
        return Const((e::QuoteNode).value)
    elseif isa(e, SSAValue)
        return abstract_eval_ssavalue(e::SSAValue, sv.src)
    elseif isa(e, SlotNumber) || isa(e, Argument)
        return vtypes[slot_id(e)].typ
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
        return collect_limitations!(typ, sv)
    end
end

function collect_argtypes(interp::AbstractInterpreter, ea::Vector{Any}, vtypes::VarTable, sv::InferenceState)
    n = length(ea)
    argtypes = Vector{LatticeElement}(undef, n)
    @inbounds for i = 1:n
        ai = abstract_eval_value(interp, ea[i], vtypes, sv)
        if ai === ⊥
            return nothing
        end
        argtypes[i] = ai
    end
    return argtypes
end

function abstract_eval_statement(interp::AbstractInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if !isa(e, Expr)
        if isa(e, PhiNode)
            rt = ⊥
            for val in e.values
                rt = tmerge(rt, abstract_eval_special_value(interp, val, vtypes, sv))
            end
            return rt
        end
        return abstract_eval_special_value(interp, e, vtypes, sv)
    end
    e = e::Expr
    ehead = e.head
    if ehead === :call
        ea = e.args
        argtypes = collect_argtypes(interp, ea, vtypes, sv)
        if argtypes === nothing
            t = ⊥
        else
            callinfo = abstract_call(interp, ArgInfo(ea, argtypes), sv)
            sv.stmt_info[sv.currpc] = callinfo.info
            t = callinfo.rt
        end
    elseif ehead === :new
        ty = instanceof_tfunc(abstract_eval_value(interp, e.args[1], vtypes, sv))[1]
        t = NativeType(ty)
        if isconcretetype(ty) && !ismutabletype(ty)
            nargs = length(e.args) - 1
            ats = Vector{LatticeElement}(undef, nargs)
            local anyrefine = false
            local allconst = true
            for i = 2:length(e.args)
                at = widenconditional(abstract_eval_value(interp, e.args[i], vtypes, sv))
                ft = fieldtype(ty, i-1)
                at = at ⊓ ft
                if at === ⊥
                    t = ⊥
                    @goto t_computed
                elseif !isConst(at)
                    allconst = false
                end
                if !anyrefine
                    anyrefine = has_nontrivial_const_info(at) || # constant information
                                at ⋤ ft                          # just a type-level information, but more precise than the declared type
                end
                ats[i-1] = at
            end
            # For now, don't allow partially initialized Const/PartialStruct
            if fieldcount(ty) == nargs
                if allconst
                    argvals = Vector{Any}(undef, nargs)
                    for j in 1:nargs
                        argvals[j] = constant(ats[j])
                    end
                    t = Const(ccall(:jl_new_structv, Any, (Any, Ptr{Cvoid}, UInt32), ty, argvals, nargs))
                elseif anyrefine
                    t = PartialStruct(ty, ats)
                end
            end
        end
    elseif ehead === :splatnew
        ty = instanceof_tfunc(abstract_eval_value(interp, e.args[1], vtypes, sv))[1]
        t = NativeType(ty)
        if length(e.args) == 2 && isconcretetype(ty) && !ismutabletype(ty)
            at = abstract_eval_value(interp, e.args[2], vtypes, sv)
            n = fieldcount(ty)
            if isConst(at) && isa(constant(at), Tuple) && n == length(constant(at)::Tuple) &&
                let ty = ty, at = at; _all(i->getfield(constant(at)::Tuple, i) isa fieldtype(ty, i), 1:n); end
                t = Const(ccall(:jl_new_structt, Any, (Any, Any), ty, constant(at)))
            elseif isPartialStruct(at) && at ⊑ Tuple && n == length(partialfields(at)) &&
                let ty = ty, at = at; _all(i->partialfields(at)[i] ⊑ₜ fieldtype(ty, i), 1:n); end
                t = PartialStruct(ty, partialfields(at))
            end
        end
    elseif ehead === :new_opaque_closure
        t = ⊥
        if length(e.args) >= 5
            ea = e.args
            argtypes = collect_argtypes(interp, ea, vtypes, sv)
            if argtypes !== nothing
                t = _opaque_closure_tfunc(argtypes[1], argtypes[2], argtypes[3],
                    argtypes[4], argtypes[5], argtypes[6:end], sv.linfo)
                if isPartialOpaque(t)
                    # Infer this now so that the specialization is available to
                    # optimization.
                    poc = partialopaque(t)
                    argtypes = most_general_argtypes(poc)
                    pushfirst!(argtypes, LatticeElement(poc.env))
                    callinfo = abstract_call_opaque_closure(interp, poc,
                        ArgInfo(nothing, argtypes), sv)
                    sv.stmt_info[sv.currpc] = OpaqueClosureCreateInfo(callinfo)
                end
            end
        end
    elseif ehead === :foreigncall
        abstract_eval_value(interp, e.args[1], vtypes, sv)
        t = sp_type_rewrap(e.args[2], sv.linfo, true)
        for i = 3:length(e.args)
            if abstract_eval_value(interp, e.args[i], vtypes, sv) === ⊥
                t = ⊥
            end
        end
    elseif ehead === :cfunction
        t = NativeType(e.args[1])
        isa(unwraptype(t), Type) || (t = ⊤)
        abstract_eval_cfunction(interp, e, vtypes, sv)
    elseif ehead === :method
        t = NativeType((length(e.args) == 1) ? Any : Nothing)
    elseif ehead === :copyast
        t = abstract_eval_value(interp, e.args[1], vtypes, sv)
        if isConst(t) && constant(t) isa Expr
            # `copyast` makes copies of Exprs
            t = NativeType(Expr)
        end
    elseif ehead === :invoke || ehead === :invoke_modify
        error("type inference data-flow error: tried to double infer a function")
    elseif ehead === :isdefined
        sym = e.args[1]
        t = LBool
        if isa(sym, SlotNumber)
            vtyp = vtypes[slot_id(sym)]
            if vtyp.typ === ⊥
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
            n = sym.args[1]::Int
            if 1 <= n <= length(sv.sptypes)
                spty = sv.sptypes[n]
                if isConst(spty)
                    t = Const(true)
                end
            end
        end
    else
        t = abstract_eval_value_expr(interp, e, vtypes, sv)
    end
    @label t_computed
    ty = unwraptype(t)
    @assert !isa(ty, TypeVar) "unhandled TypeVar"
    if isa(ty, DataType) && isdefined(ty, :instance)
        # replace singleton types with their equivalent Const object
        t = Const(ty.instance)
    end
    if !isempty(sv.pclimitations)
        if isConst(t) || t === ⊥
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
    return ⊤
end

function abstract_eval_ssavalue(s::SSAValue, src::CodeInfo)
    typ = (src.ssavaluetypes::SSAValueTypes)[s.id]::SSAValueType
    if typ === NOT_FOUND
        return ⊥
    end
    return typ
end

function widenreturn(rt::LatticeElement, bestguess::LatticeElement, nslots::Int, slottypes::Argtypes, changes::VarTable)
    if !(bestguess ⊑ Bool) || unwraptype(bestguess) === Bool
        # give up inter-procedural constraint back-propagation
        # when tmerge would widen the result anyways (as an optimization)
        rt = widenconditional(rt)
    else
        if isConditional(rt)
            cnd = conditional(rt)
            id = cnd.slot_id
            if 1 ≤ id ≤ nslots
                old_id_type = widenconditional(slottypes[id]) # same as `(states[1]::VarTable)[id].typ`
                if (!(cnd.vtype ⊑ old_id_type) || old_id_type ⊑ cnd.vtype) &&
                   (!(cnd.elsetype ⊑ old_id_type) || old_id_type ⊑ cnd.elsetype)
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
        if isConditional(rt)
            cnd = conditional(rt)
            rt = InterConditional(cnd.slot_id, cnd.vtype, cnd.elsetype)
        elseif is_lattice_bool(rt)
            if isInterConditional(bestguess)
                # if the bestguess so far is already `Conditional`, try to convert
                # this `rt` into `Conditional` on the slot to avoid overapproximation
                # due to conflict of different slots
                rt = bool_rt_to_conditional(rt, slottypes, changes, interconditional(bestguess).slot_id)
            else
                # pick up the first "interesting" slot, convert `rt` to its `Conditional`
                # TODO: ideally we want `Conditional` and `InterConditional` to convey
                # constraints on multiple slots
                for slot_id in 1:nslots
                    rt = bool_rt_to_conditional(rt, slottypes, changes, slot_id)
                    isInterConditional(rt) && break
                end
            end
        end
    end

    # only propagate information we know we can store
    # and is valid and good inter-procedurally
    if isConditional(rt)
        cnd = conditional(rt)
        return InterConditional(cnd.slot_id, cnd.vtype, cnd.elsetype)
    end
    isInterConditional(rt) && return rt
    isConst(rt) && return rt
    if isPartialStruct(rt)
        fields = copy(partialfields(rt))
        local anyrefine = false
        for i in 1:length(fields)
            a = fields[i]
            a = isVararg(a) ? a : widenreturn(LatticeElement(a), bestguess, nslots, slottypes, changes)
            if !anyrefine
                # TODO: consider adding && const_prop_profitable(a) here?
                anyrefine = has_const_info(a) ||
                            a ⊏ₜ fieldtype(rt.typ, i)
            end
            fields[i] = a
        end
        anyrefine && return PartialStruct(rt.typ, fields)
    end
    if isPartialOpaque(rt)
        return rt # XXX: this case was missed in #39512
    end
    return rt
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
    ssavaluetypes = frame.src.ssavaluetypes::SSAValueTypes
    while frame.pc´´ <= n
        # make progress on the active ip set
        local pc::Int = frame.pc´´
        while true # inner loop optimizes the common case where it can run straight from pc to pc + 1
            local pc´::Int = pc + 1 # next program-counter (after executing instruction)
            if pc == frame.pc´´
                # want to update pc´´ to point at the new lowest instruction in W
                frame.pc´´ = pc´
            end
            delete!(W, pc)
            frame.currpc = pc
            edges = frame.stmt_edges[pc]
            edges === nothing || empty!(edges)
            frame.stmt_info[pc] = nothing
            stmt = frame.src.code[pc]
            changes = states[pc]::VarTable
            t = nothing

            hd = isa(stmt, Expr) ? stmt.head : nothing

            if isa(stmt, NewvarNode)
                sn = slot_id(stmt.slot)
                changes[sn] = VarState(⊥, true)
            elseif isa(stmt, GotoNode)
                pc´ = (stmt::GotoNode).label
            elseif isa(stmt, GotoIfNot)
                condx = stmt.cond
                condt = abstract_eval_value(interp, condx, changes, frame)
                if condt === ⊥
                    empty!(frame.pclimitations)
                    break
                end
                if !(isConst(condt) || isConditional(condt)) && isa(condx, SlotNumber)
                    # if this non-`Conditional` object is a slot, we form and propagate
                    # the conditional constraint on it
                    condt = Conditional(slot_id(condx), Const(true), Const(false))
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
                    changes_else = changes
                    if isConditional(condt)
                        cnd = conditional(condt)
                        changes_else = conditional_changes(changes_else, cnd.elsetype, cnd.slot_id)
                        changes      = conditional_changes(changes,      cnd.vtype,    cnd.slot_id)
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
                if isInterConditional(rt) && isConst(bestguess)
                    let cnd = interconditional(rt)
                        slot_id = cnd.slot_id
                        old_id_type = slottypes[slot_id]
                        if constant(bestguess) === true && cnd.elsetype !== ⊥
                            bestguess = InterConditional(slot_id, old_id_type, ⊥)
                        elseif constant(bestguess) === false && cnd.vtype !== ⊥
                            bestguess = InterConditional(slot_id, ⊥, old_id_type)
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
                    # TODO: if isInterConditional(bestguess) && !interesting(bestguess); bestguess = widenconditional(bestguess); end
                    frame.bestguess = bestguess
                    for (caller, caller_pc) in frame.cycle_backedges
                        # notify backedges of updated type information
                        typeassert(caller.stmt_types[caller_pc], VarTable) # we must have visited this statement before
                        if !((caller.src.ssavaluetypes::SSAValueTypes)[caller_pc] === ⊤)
                            # no reason to revisit if that call-site doesn't affect the final result
                            if caller_pc < caller.pc´´
                                caller.pc´´ = caller_pc
                            end
                            push!(caller.ip, caller_pc)
                        end
                    end
                end
            elseif hd === :enter
                stmt = stmt::Expr
                l = stmt.args[1]::Int
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
            elseif hd === :leave
            else
                if hd === :(=)
                    stmt = stmt::Expr
                    t = abstract_eval_statement(interp, stmt.args[2], changes, frame)
                    if t === ⊥
                        break
                    end
                    ssavaluetypes[pc] = t
                    lhs = stmt.args[1]
                    if isa(lhs, SlotNumber)
                        changes = StateUpdate(lhs, VarState(t, false), changes, false)
                    end
                elseif hd === :method
                    stmt = stmt::Expr
                    fname = stmt.args[1]
                    if isa(fname, SlotNumber)
                        changes = StateUpdate(fname, VarState(⊤, false), changes, false)
                    end
                elseif hd === :code_coverage_effect ||
                       (hd !== :boundscheck && # :boundscheck can be narrowed to Bool
                        hd !== nothing && is_meta_expr_head(hd))
                    # these do not generate code
                else
                    t = abstract_eval_statement(interp, stmt, changes, frame)
                    if t === ⊥
                        break
                    end
                    if !isempty(frame.ssavalue_uses[pc])
                        record_ssa_assign(pc, t, frame)
                    else
                        ssavaluetypes[pc] = t
                    end
                end
                if isa(changes, StateUpdate)
                    let cur_hand = frame.handler_at[pc], l, enter
                        while cur_hand != 0
                            enter = frame.src.code[cur_hand]
                            l = (enter::Expr).args[1]::Int
                            # propagate new type info to exception handler
                            # the handling for Expr(:enter) propagates all changes from before the try/catch
                            # so this only needs to propagate any changes
                            if stupdate1!(states[l]::VarTable, changes::StateUpdate) !== false
                                if l < frame.pc´´
                                    frame.pc´´ = l
                                end
                                push!(W, l)
                            end
                            cur_hand = frame.handler_at[cur_hand]
                        end
                    end
                end
            end

            @assert isempty(frame.pclimitations) "unhandled LimitedAccuracy"

            if t === nothing
                # mark other reached expressions as `Any` to indicate they don't throw
                ssavaluetypes[pc] = ⊤
            end

            pc´ > n && break # can't proceed with the fast-path fall-through
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
                break
            elseif newstate !== nothing
                states[pc´] = newstate
                pc = pc´
            elseif pc´ in W
                pc = pc´
            else
                break
            end
        end
        frame.pc´´ = _bits_findnext(W.bits, frame.pc´´)::Int # next program-counter
    end
    frame.dont_work_on_me = false
    nothing
end

function conditional_changes(changes::VarTable, typ::LatticeElement, slot_id::Int)
    oldtyp = changes[slot_id].typ
    # approximate test for `typ ∩ oldtyp` being better than `oldtyp`
    # since we probably formed these types with `typesubstract`, the comparison is likely simple
    typ′, oldtyp′ = ignorelimited(typ), ignorelimited(oldtyp)
    if typ′ ⊑ oldtyp′
        # typ is better unlimited, but we may still need to compute the meet with the limit "causes" since we ignored those in the comparison
        if isLimitedAccuracy(oldtyp)
            typ = LimitedAccuracy(typ′, causes(oldtyp))
        end
        return StateUpdate(SlotNumber(slot_id), VarState(typ, false), changes, true)
    end
    return changes
end

function bool_rt_to_conditional(rt::LatticeElement, slottypes::Argtypes, state::VarTable, slot_id::Int)
    old = slottypes[slot_id]
    new = widenconditional(state[slot_id].typ) # avoid nested conditional
    if new ⊑ old && !(old ⊑ new)
        if isConst(rt)
            val = constant(rt)
            if val === true
                return InterConditional(slot_id, new, ⊥)
            elseif val === false
                return InterConditional(slot_id, ⊥, new)
            end
        elseif is_lattice_bool(rt)
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
