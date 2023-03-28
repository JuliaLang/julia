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
call_result_unused(frame::InferenceState, currpc::Int) =
    isexpr(frame.src.code[currpc], :call) && isempty(frame.ssavalue_uses[currpc])
call_result_unused(si::StmtInfo) = !si.used

function get_max_methods(mod::Module, interp::AbstractInterpreter)
    max_methods = ccall(:jl_get_module_max_methods, Cint, (Any,), mod) % Int
    max_methods < 0 ? InferenceParams(interp).max_methods : max_methods
end

function get_max_methods(@nospecialize(f), mod::Module, interp::AbstractInterpreter)
    if f !== nothing
        fmm = typeof(f).name.max_methods
        fmm !== UInt8(0) && return Int(fmm)
    end
    return get_max_methods(mod, interp)
end

function should_infer_this_call(interp::AbstractInterpreter, sv::InferenceState)
    if InferenceParams(interp).unoptimize_throw_blocks
        # Disable inference of calls in throw blocks, since we're unlikely to
        # need their types. There is one exception however: If up until now, the
        # function has not seen any side effects, we would like to make sure there
        # aren't any in the throw block either to enable other optimizations.
        if is_stmt_throw_block(get_curr_ssaflag(sv))
            should_infer_for_effects(sv) || return false
        end
    end
    return true
end

function should_infer_for_effects(sv::InferenceState)
    effects = sv.ipo_effects
    return is_terminates(effects) && is_effect_free(effects)
end

function abstract_call_gf_by_type(interp::AbstractInterpreter, @nospecialize(f),
                                  arginfo::ArgInfo, si::StmtInfo, @nospecialize(atype),
                                  sv::InferenceState, max_methods::Int)
    ⊑ₚ = ⊑(ipo_lattice(interp))
    if !should_infer_this_call(interp, sv)
        add_remark!(interp, sv, "Skipped call in throw block")
        nonoverlayed = false
        if isoverlayed(method_table(interp)) && is_nonoverlayed(sv.ipo_effects)
            # as we may want to concrete-evaluate this frame in cases when there are
            # no overlayed calls, try an additional effort now to check if this call
            # isn't overlayed rather than just handling it conservatively
            matches = find_matching_methods(typeinf_lattice(interp), arginfo.argtypes, atype, method_table(interp),
                InferenceParams(interp).max_union_splitting, max_methods)
            if !isa(matches, FailedMethodMatch)
                nonoverlayed = matches.nonoverlayed
            end
        else
            nonoverlayed = true
        end
        # At this point we are guaranteed to end up throwing on this path,
        # which is all that's required for :consistent-cy. Of course, we don't
        # know anything else about this statement.
        effects = Effects(; consistent=ALWAYS_TRUE, nonoverlayed)
        return CallMeta(Any, effects, NoCallInfo())
    end

    argtypes = arginfo.argtypes
    matches = find_matching_methods(typeinf_lattice(interp), argtypes, atype, method_table(interp),
        InferenceParams(interp).max_union_splitting, max_methods)
    if isa(matches, FailedMethodMatch)
        add_remark!(interp, sv, matches.reason)
        return CallMeta(Any, Effects(), NoCallInfo())
    end

    (; valid_worlds, applicable, info) = matches
    update_valid_age!(sv, valid_worlds)
    napplicable = length(applicable)
    rettype = Bottom
    edges = MethodInstance[]
    conditionals = nothing # keeps refinement information of call argument types when the return type is boolean
    seen = 0               # number of signatures actually inferred
    any_const_result = false
    const_results = Union{Nothing,ConstResult}[]
    multiple_matches = napplicable > 1
    fargs = arginfo.fargs
    all_effects = EFFECTS_TOTAL
    if !matches.nonoverlayed
        # currently we don't have a good way to execute the overlayed method definition,
        # so we should give up concrete eval when any of the matched methods is overlayed
        f = nothing
        all_effects = Effects(all_effects; nonoverlayed=false)
    end

    𝕃ₚ = ipo_lattice(interp)
    for i in 1:napplicable
        match = applicable[i]::MethodMatch
        method = match.method
        sig = match.spec_types
        if bail_out_toplevel_call(interp, InferenceLoopState(sig, rettype, all_effects), sv)
            # only infer concrete call sites in top-level expressions
            add_remark!(interp, sv, "Refusing to infer non-concrete call site in top-level expression")
            break
        end
        this_rt = Bottom
        splitunions = false
        # TODO: this used to trigger a bug in inference recursion detection, and is unmaintained now
        # sigtuple = unwrap_unionall(sig)::DataType
        # splitunions = 1 < unionsplitcost(sigtuple.parameters) * napplicable <= InferenceParams(interp).max_union_splitting
        if splitunions
            splitsigs = switchtupleunion(sig)
            for sig_n in splitsigs
                result = abstract_call_method(interp, method, sig_n, svec(), multiple_matches, si, sv)
                (; rt, edge, effects) = result
                this_argtypes = isa(matches, MethodMatches) ? argtypes : matches.applicable_argtypes[i]
                this_arginfo = ArgInfo(fargs, this_argtypes)
                const_call_result = abstract_call_method_with_const_args(interp,
                    result, f, this_arginfo, si, match, sv)
                const_result = nothing
                if const_call_result !== nothing
                    if const_call_result.rt ⊑ₚ rt
                        rt = const_call_result.rt
                        (; effects, const_result, edge) = const_call_result
                    else
                        add_remark!(interp, sv, "[constprop] Discarded because the result was wider than inference")
                    end
                end
                all_effects = merge_effects(all_effects, effects)
                push!(const_results, const_result)
                any_const_result |= const_result !== nothing
                edge === nothing || push!(edges, edge)
                this_rt = tmerge(this_rt, rt)
                if bail_out_call(interp, this_rt, sv)
                    break
                end
            end
            this_conditional = ignorelimited(this_rt)
            this_rt = widenwrappedconditional(this_rt)
        else
            result = abstract_call_method(interp, method, sig, match.sparams, multiple_matches, si, sv)
            (; rt, edge, effects) = result
            this_conditional = ignorelimited(rt)
            this_rt = widenwrappedconditional(rt)
            # try constant propagation with argtypes for this match
            # this is in preparation for inlining, or improving the return result
            this_argtypes = isa(matches, MethodMatches) ? argtypes : matches.applicable_argtypes[i]
            this_arginfo = ArgInfo(fargs, this_argtypes)
            const_call_result = abstract_call_method_with_const_args(interp,
                result, f, this_arginfo, si, match, sv)
            const_result = nothing
            if const_call_result !== nothing
                this_const_conditional = ignorelimited(const_call_result.rt)
                this_const_rt = widenwrappedconditional(const_call_result.rt)
                # return type of const-prop' inference can be wider than that of non const-prop' inference
                # e.g. in cases when there are cycles but cached result is still accurate
                if this_const_rt ⊑ₚ this_rt
                    this_conditional = this_const_conditional
                    this_rt = this_const_rt
                    (; effects, const_result, edge) = const_call_result
                else
                    add_remark!(interp, sv, "[constprop] Discarded because the result was wider than inference")
                end
            end
            all_effects = merge_effects(all_effects, effects)
            push!(const_results, const_result)
            any_const_result |= const_result !== nothing
            edge === nothing || push!(edges, edge)
        end
        @assert !(this_conditional isa Conditional || this_rt isa MustAlias) "invalid lattice element returned from inter-procedural context"
        seen += 1
        rettype = tmerge(𝕃ₚ, rettype, this_rt)
        if has_conditional(𝕃ₚ) && this_conditional !== Bottom && is_lattice_bool(𝕃ₚ, rettype) && fargs !== nothing
            if conditionals === nothing
                conditionals = Any[Bottom for _ in 1:length(argtypes)],
                               Any[Bottom for _ in 1:length(argtypes)]
            end
            for i = 1:length(argtypes)
                cnd = conditional_argtype(this_conditional, sig, argtypes, i)
                conditionals[1][i] = tmerge(conditionals[1][i], cnd.thentype)
                conditionals[2][i] = tmerge(conditionals[2][i], cnd.elsetype)
            end
        end
        if bail_out_call(interp, InferenceLoopState(sig, rettype, all_effects), sv)
            add_remark!(interp, sv, "Call inference reached maximally imprecise information. Bailing on.")
            break
        end
    end

    if any_const_result && seen == napplicable
        @assert napplicable == nmatches(info) == length(const_results)
        info = ConstCallInfo(info, const_results)
    end

    if seen ≠ napplicable
        # there is unanalyzed candidate, widen type and effects to the top
        rettype = Any
        all_effects = Effects()
    elseif isa(matches, MethodMatches) ? (!matches.fullmatch || any_ambig(matches)) :
            (!all(matches.fullmatches) || any_ambig(matches))
        # Account for the fact that we may encounter a MethodError with a non-covered or ambiguous signature.
        all_effects = Effects(all_effects; nothrow=false)
    end

    rettype = from_interprocedural!(𝕃ₚ, rettype, sv, arginfo, conditionals)

    # Also considering inferring the compilation signature for this method, so
    # it is available to the compiler in case it ends up needing it.
    if infer_compilation_signature(interp) && 1 == seen == napplicable && rettype !== Any && rettype !== Union{} && !is_removable_if_unused(all_effects)
        match = applicable[1]::MethodMatch
        method = match.method
        sig = match.spec_types
        mi = specialize_method(match; preexisting=true)
        if mi !== nothing && !const_prop_methodinstance_heuristic(interp, mi, arginfo, sv)
            csig = get_compileable_sig(method, sig, match.sparams)
            if csig !== nothing && csig !== sig
                abstract_call_method(interp, method, csig, match.sparams, multiple_matches, StmtInfo(false), sv)
            end
        end
    end

    if call_result_unused(si) && !(rettype === Bottom)
        add_remark!(interp, sv, "Call result type was widened because the return value is unused")
        # We're mainly only here because the optimizer might want this code,
        # but we ourselves locally don't typically care about it locally
        # (beyond checking if it always throws).
        # So avoid adding an edge, since we don't want to bother attempting
        # to improve our result even if it does change (to always throw),
        # and avoid keeping track of a more complex result type.
        rettype = Any
    end
    add_call_backedges!(interp, rettype, all_effects, edges, matches, atype, sv)
    if !isempty(sv.pclimitations) # remove self, if present
        delete!(sv.pclimitations, sv)
        for caller in sv.callers_in_cycle
            delete!(sv.pclimitations, caller)
        end
    end
    return CallMeta(rettype, all_effects, info)
end

struct FailedMethodMatch
    reason::String
end

struct MethodMatches
    applicable::Vector{Any}
    info::MethodMatchInfo
    valid_worlds::WorldRange
    mt::MethodTable
    fullmatch::Bool
    nonoverlayed::Bool
end
any_ambig(info::MethodMatchInfo) = info.results.ambig
any_ambig(m::MethodMatches) = any_ambig(m.info)

struct UnionSplitMethodMatches
    applicable::Vector{Any}
    applicable_argtypes::Vector{Vector{Any}}
    info::UnionSplitInfo
    valid_worlds::WorldRange
    mts::Vector{MethodTable}
    fullmatches::Vector{Bool}
    nonoverlayed::Bool
end
any_ambig(m::UnionSplitMethodMatches) = any(any_ambig, m.info.matches)

function find_matching_methods(𝕃::AbstractLattice,
                               argtypes::Vector{Any}, @nospecialize(atype), method_table::MethodTableView,
                               max_union_splitting::Int, max_methods::Int)
    # NOTE this is valid as far as any "constant" lattice element doesn't represent `Union` type
    if 1 < unionsplitcost(𝕃, argtypes) <= max_union_splitting
        split_argtypes = switchtupleunion(𝕃, argtypes)
        infos = MethodMatchInfo[]
        applicable = Any[]
        applicable_argtypes = Vector{Any}[] # arrays like `argtypes`, including constants, for each match
        valid_worlds = WorldRange()
        mts = MethodTable[]
        fullmatches = Bool[]
        nonoverlayed = true
        for i in 1:length(split_argtypes)
            arg_n = split_argtypes[i]::Vector{Any}
            sig_n = argtypes_to_type(arg_n)
            mt = ccall(:jl_method_table_for, Any, (Any,), sig_n)
            mt === nothing && return FailedMethodMatch("Could not identify method table for call")
            mt = mt::MethodTable
            result = findall(sig_n, method_table; limit = max_methods)
            if result === nothing
                return FailedMethodMatch("For one of the union split cases, too many methods matched")
            end
            (; matches, overlayed) = result
            nonoverlayed &= !overlayed
            push!(infos, MethodMatchInfo(matches))
            for m in matches
                push!(applicable, m)
                push!(applicable_argtypes, arg_n)
            end
            valid_worlds = intersect(valid_worlds, matches.valid_worlds)
            thisfullmatch = any(match::MethodMatch->match.fully_covers, matches)
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
                                       fullmatches,
                                       nonoverlayed)
    else
        mt = ccall(:jl_method_table_for, Any, (Any,), atype)
        if mt === nothing
            return FailedMethodMatch("Could not identify method table for call")
        end
        mt = mt::MethodTable
        result = findall(atype, method_table; limit = max_methods)
        if result === nothing
            # this means too many methods matched
            # (assume this will always be true, so we don't compute / update valid age in this case)
            return FailedMethodMatch("Too many methods matched")
        end
        (; matches, overlayed) = result
        fullmatch = any(match::MethodMatch->match.fully_covers, matches)
        return MethodMatches(matches.matches,
                             MethodMatchInfo(matches),
                             matches.valid_worlds,
                             mt,
                             fullmatch,
                             !overlayed)
    end
end

"""
    from_interprocedural!(𝕃ₚ::AbstractLattice, rt, sv::InferenceState, arginfo::ArgInfo, maybecondinfo) -> newrt

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
function from_interprocedural!(𝕃ₚ::AbstractLattice, @nospecialize(rt), sv::InferenceState, arginfo::ArgInfo, @nospecialize(maybecondinfo))
    rt = collect_limitations!(rt, sv)
    if isa(rt, InterMustAlias)
        rt = from_intermustalias(rt, arginfo)
    elseif is_lattice_bool(𝕃ₚ, rt)
        if maybecondinfo === nothing
            rt = widenconditional(rt)
        else
            rt = from_interconditional(𝕃ₚ, rt, sv, arginfo, maybecondinfo)
        end
    end
    @assert !(rt isa InterConditional || rt isa InterMustAlias) "invalid lattice element returned from inter-procedural context"
    return rt
end

function collect_limitations!(@nospecialize(typ), sv::InferenceState)
    if isa(typ, LimitedAccuracy)
        union!(sv.pclimitations, typ.causes)
        return typ.typ
    end
    return typ
end

function from_intermustalias(rt::InterMustAlias, arginfo::ArgInfo)
    fargs = arginfo.fargs
    if fargs !== nothing && 1 ≤ rt.slot ≤ length(fargs)
        arg = fargs[rt.slot]
        if isa(arg, SlotNumber)
            argtyp = widenslotwrapper(arginfo.argtypes[rt.slot])
            if rt.vartyp ⊑ argtyp
                return MustAlias(arg, rt.vartyp, rt.fldidx, rt.fldtyp)
            else
                # TODO optimize this case?
            end
        end
    end
    return widenmustalias(rt)
end

function from_interconditional(𝕃ₚ::AbstractLattice, @nospecialize(typ),
        sv::InferenceState, (; fargs, argtypes)::ArgInfo, @nospecialize(maybecondinfo))
    𝕃 = widenlattice(𝕃ₚ)
    has_conditional(𝕃ₚ) || return widenconditional(typ)
    fargs === nothing && return widenconditional(typ)
    slot = 0
    alias = nothing
    thentype = elsetype = Any
    condval = maybe_extract_const_bool(typ)
    for i in 1:length(fargs)
        # find the first argument which supports refinement,
        # and intersect all equivalent arguments with it
        argtyp = argtypes[i]
        if alias === nothing
            if argtyp isa MustAlias
                old = argtyp.fldtyp
                id = argtyp.slot
            elseif alias === nothing && argtyp isa Type
                arg = ssa_def_slot(fargs[i], sv)
                arg isa SlotNumber || continue # can't refine
                old = argtyp
                id = slot_id(arg)
            else
                continue # unlikely to refine
            end
        elseif argtyp isa MustAlias && issubalias(argtyp, alias)
            old = alias.fldtyp
            id = alias.slot
        else
            continue
        end
        if slot == 0 || id == slot
            if isa(maybecondinfo, Tuple{Vector{Any},Vector{Any}})
                # if we have already computed argument refinement information, apply that now to get the result
                new_thentype = maybecondinfo[1][i]
                new_elsetype = maybecondinfo[2][i]
            else
                # otherwise compute it on the fly
                cnd = conditional_argtype(typ, maybecondinfo, argtypes, i)
                new_thentype = cnd.thentype
                new_elsetype = cnd.elsetype
            end
            if condval === false
                thentype = Bottom
            elseif ⊑(𝕃, new_thentype, thentype)
                thentype = new_thentype
            else
                thentype = tmeet(𝕃, thentype, widenconst(new_thentype))
            end
            if condval === true
                elsetype = Bottom
            elseif ⊑(𝕃, new_elsetype, elsetype)
                elsetype = new_elsetype
            else
                elsetype = tmeet(𝕃, elsetype, widenconst(new_elsetype))
            end
            if (slot > 0 || condval !== false) && ⋤(𝕃, thentype, old)
                slot = id
                if argtyp isa MustAlias
                    alias = argtyp
                end
            elseif (slot > 0 || condval !== true) && ⋤(𝕃, elsetype, old)
                slot = id
                if argtyp isa MustAlias
                    alias = argtyp
                end
            else # reset: no new useful information for this slot
                slot = 0
                alias = nothing
                thentype = elsetype = Any
            end
        end
    end
    if thentype === Bottom && elsetype === Bottom
        return Bottom # accidentally proved this call to be dead / throw !
    elseif slot > 0
        if alias !== nothing
            return form_mustalias_conditional(alias, thentype, elsetype)
        end
        return Conditional(slot, thentype, elsetype) # record a Conditional improvement to this slot
    end
    return widenconditional(typ)
end

function conditional_argtype(@nospecialize(rt), @nospecialize(sig), argtypes::Vector{Any}, i::Int)
    if isa(rt, InterConditional) && rt.slot == i
        return rt
    else
        thentype = elsetype = tmeet(widenslotwrapper(argtypes[i]), fieldtype(sig, i))
        condval = maybe_extract_const_bool(rt)
        condval === true && (elsetype = Bottom)
        condval === false && (thentype = Bottom)
        return InterConditional(i, thentype, elsetype)
    end
end

function add_call_backedges!(interp::AbstractInterpreter, @nospecialize(rettype), all_effects::Effects,
    edges::Vector{MethodInstance}, matches::Union{MethodMatches,UnionSplitMethodMatches}, @nospecialize(atype),
    sv::InferenceState)
    # don't bother to add backedges when both type and effects information are already
    # maximized to the top since a new method couldn't refine or widen them anyway
    if rettype === Any
        # ignore the `:nonoverlayed` property if `interp` doesn't use overlayed method table
        # since it will never be tainted anyway
        if !isoverlayed(method_table(interp))
            all_effects = Effects(all_effects; nonoverlayed=false)
        end
        if (# ignore the `:noinbounds` property if `:consistent`-cy is tainted already
            sv.ipo_effects.consistent === ALWAYS_FALSE || all_effects.consistent === ALWAYS_FALSE ||
            # or this `:noinbounds` doesn't taint it
            !stmt_taints_inbounds_consistency(sv))
            all_effects = Effects(all_effects; noinbounds=false)
        end
        all_effects === Effects() && return nothing
    end
    for edge in edges
        add_backedge!(sv, edge)
    end
    # also need an edge to the method table in case something gets
    # added that did not intersect with any existing method
    if isa(matches, MethodMatches)
        matches.fullmatch || add_mt_backedge!(sv, matches.mt, atype)
    else
        for (thisfullmatch, mt) in zip(matches.fullmatches, matches.mts)
            thisfullmatch || add_mt_backedge!(sv, mt, atype)
        end
    end
    return nothing
end

const RECURSION_UNUSED_MSG = "Bounded recursion detected with unused result. Annotated return type may be wider than true result."
const RECURSION_MSG = "Bounded recursion detected. Call was widened to force convergence."
const RECURSION_MSG_HARDLIMIT = "Bounded recursion detected under hardlimit. Call was widened to force convergence."

function abstract_call_method(interp::AbstractInterpreter, method::Method, @nospecialize(sig), sparams::SimpleVector, hardlimit::Bool, si::StmtInfo, sv::InferenceState)
    if method.name === :depwarn && isdefined(Main, :Base) && method.module === Main.Base
        add_remark!(interp, sv, "Refusing to infer into `depwarn`")
        return MethodCallResult(Any, false, false, nothing, Effects())
    end

    # Limit argument type tuple growth of functions:
    # look through the parents list to see if there's a call to the same method
    # and from the same method.
    # Returns the topmost occurrence of that repeated edge.
    edgecycle = edgelimited = false
    topmost = nothing

    for infstate in InfStackUnwind(sv)
        if method === infstate.linfo.def
            if infstate.linfo.specTypes::Type == sig::Type
                # avoid widening when detecting self-recursion
                # TODO: merge call cycle and return right away
                if call_result_unused(si)
                    add_remark!(interp, sv, RECURSION_UNUSED_MSG)
                    # since we don't use the result (typically),
                    # we have a self-cycle in the call-graph, but not in the inference graph (typically):
                    # break this edge now (before we record it) by returning early
                    # (non-typically, this means that we lose the ability to detect a guaranteed StackOverflow in some cases)
                    return MethodCallResult(Any, true, true, nothing, Effects())
                end
                topmost = nothing
                edgecycle = true
                break
            end
            topmost === nothing || continue
            if edge_matches_sv(interp, infstate, method, sig, sparams, hardlimit, sv)
                topmost = infstate
                edgecycle = true
            end
        end
    end
    washardlimit = hardlimit

    if topmost !== nothing
        sigtuple = unwrap_unionall(sig)::DataType
        msig = unwrap_unionall(method.sig)::DataType
        spec_len = length(msig.parameters) + 1
        ls = length(sigtuple.parameters)

        if method === sv.linfo.def
            # Under direct self-recursion, permit much greater use of reducers.
            # here we assume that complexity(specTypes) :>= complexity(sig)
            comparison = sv.linfo.specTypes
            l_comparison = length((unwrap_unionall(comparison)::DataType).parameters)
            spec_len = max(spec_len, l_comparison)
        else
            comparison = method.sig
        end

        if isdefined(method, :recursion_relation)
            # We don't require the recursion_relation to be transitive, so
            # apply a hard limit
            hardlimit = true
        end

        # see if the type is actually too big (relative to the caller), and limit it if required
        newsig = limit_type_size(sig, comparison, hardlimit ? comparison : sv.linfo.specTypes, InferenceParams(interp).tuple_complexity_limit_depth, spec_len)

        if newsig !== sig
            # continue inference, but note that we've limited parameter complexity
            # on this call (to ensure convergence), so that we don't cache this result
            if call_result_unused(si)
                add_remark!(interp, sv, RECURSION_UNUSED_MSG)
                # if we don't (typically) actually care about this result,
                # don't bother trying to examine some complex abstract signature
                # since it's very unlikely that we'll try to inline this,
                # or want make an invoke edge to its calling convention return type.
                # (non-typically, this means that we lose the ability to detect a guaranteed StackOverflow in some cases)
                return MethodCallResult(Any, true, true, nothing, Effects())
            end
            add_remark!(interp, sv, washardlimit ? RECURSION_MSG_HARDLIMIT : RECURSION_MSG)
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
        #         newsig = limit_type_size(newsig, sig, sv.linfo.specTypes, InferenceParams(interp).tuple_complexity_limit_depth, lsig)
        #         recomputed = ccall(:jl_type_intersection_with_env, Any, (Any, Any), newsig, method.sig)::SimpleVector
        #         newsig = recomputed[2]
        #     end
        #     sig = ?
        sparams = recomputed[2]::SimpleVector
    end

    (; rt, edge, effects) = typeinf_edge(interp, method, sig, sparams, sv)

    if edge === nothing
        edgecycle = edgelimited = true
    end

    # we look for the termination effect override here as well, since the :terminates effect
    # may have been tainted due to recursion at this point even if it's overridden
    if is_effect_overridden(sv, :terminates_globally)
        # this frame is known to terminate
        effects = Effects(effects, terminates=true)
    elseif is_effect_overridden(method, :terminates_globally)
        # this edge is known to terminate
        effects = Effects(effects; terminates=true)
    elseif edgecycle
        # Some sort of recursion was detected.
        if edge !== nothing && !edgelimited && !is_edge_recursed(edge, sv)
            # no `MethodInstance` cycles -- don't taint :terminate
        else
            # we cannot guarantee that the call will terminate
            effects = Effects(effects; terminates=false)
        end
    end

    return MethodCallResult(rt, edgecycle, edgelimited, edge, effects)
end

function edge_matches_sv(interp::AbstractInterpreter, frame::InferenceState, method::Method, @nospecialize(sig), sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)
    # The `method_for_inference_heuristics` will expand the given method's generator if
    # necessary in order to retrieve this field from the generated `CodeInfo`, if it exists.
    # The other `CodeInfo`s we inspect will already have this field inflated, so we just
    # access it directly instead (to avoid regeneration).
    world = get_world_counter(interp)
    callee_method2 = method_for_inference_heuristics(method, sig, sparams, world) # Union{Method, Nothing}

    inf_method2 = frame.src.method_for_inference_limit_heuristics # limit only if user token match
    inf_method2 isa Method || (inf_method2 = nothing)
    if callee_method2 !== inf_method2
        return false
    end
    if !hardlimit || InferenceParams(sv.interp).ignore_recursion_hardlimit
        # if this is a soft limit,
        # also inspect the parent of this edge,
        # to see if they are the same Method as sv
        # in which case we'll need to ensure it is convergent
        # otherwise, we don't

        # check in the cycle list first
        # all items in here are mutual parents of all others
        if !any(p::InferenceState->matches_sv(p, sv), frame.callers_in_cycle)
            let parent = frame.parent
                parent !== nothing || return false
                parent = parent::InferenceState
                (parent.cached || parent.parent !== nothing) || return false
                matches_sv(parent, sv) || return false
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

# This function is used for computing alternate limit heuristics
function method_for_inference_heuristics(method::Method, @nospecialize(sig), sparams::SimpleVector, world::UInt)
    if isdefined(method, :generator) && !(method.generator isa Core.GeneratedFunctionStub) && may_invoke_generator(method, sig, sparams)
        method_instance = specialize_method(method, sig, sparams)
        if isa(method_instance, MethodInstance)
            cinfo = get_staged(method_instance, world)
            if isa(cinfo, CodeInfo)
                method2 = cinfo.method_for_inference_limit_heuristics
                if method2 isa Method
                    return method2
                end
            end
        end
    end
    return nothing
end

function matches_sv(parent::InferenceState, sv::InferenceState)
    sv_method2 = sv.src.method_for_inference_limit_heuristics # limit only if user token match
    sv_method2 isa Method || (sv_method2 = nothing)
    parent_method2 = parent.src.method_for_inference_limit_heuristics # limit only if user token match
    parent_method2 isa Method || (parent_method2 = nothing)
    return parent.linfo.def === sv.linfo.def && sv_method2 === parent_method2
end

function is_edge_recursed(edge::MethodInstance, sv::InferenceState)
    return any(InfStackUnwind(sv)) do infstate
        return edge === infstate.linfo
    end
end

function is_method_recursed(method::Method, sv::InferenceState)
    return any(InfStackUnwind(sv)) do infstate
        return method === infstate.linfo.def
    end
end

function is_constprop_edge_recursed(edge::MethodInstance, sv::InferenceState)
    return any(InfStackUnwind(sv)) do infstate
        return edge === infstate.linfo && any(infstate.result.overridden_by_const)
    end
end

function is_constprop_method_recursed(method::Method, sv::InferenceState)
    return any(InfStackUnwind(sv)) do infstate
        return method === infstate.linfo.def && any(infstate.result.overridden_by_const)
    end
end

# keeps result and context information of abstract_method_call, which will later be used for
# backedge computation, and concrete evaluation or constant-propagation
struct MethodCallResult
    rt
    edgecycle::Bool
    edgelimited::Bool
    edge::Union{Nothing,MethodInstance}
    effects::Effects
    function MethodCallResult(@nospecialize(rt),
                              edgecycle::Bool,
                              edgelimited::Bool,
                              edge::Union{Nothing,MethodInstance},
                              effects::Effects)
        return new(rt, edgecycle, edgelimited, edge, effects)
    end
end

# - true: eligible for concrete evaluation
# - false: eligible for semi-concrete evaluation
# - nothing: not eligible for either of it
function concrete_eval_eligible(interp::AbstractInterpreter,
    @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState)
    # disable all concrete-evaluation if this function call is tainted by some overlayed
    # method since currently there is no direct way to execute overlayed methods
    if inbounds_option() === :off
        # Disable concrete evaluation in `--check-bounds=no` mode, since we cannot be sure
        # that inferred effects are accurate.
        return nothing
    elseif !result.effects.noinbounds && stmt_taints_inbounds_consistency(sv)
        # If the current statement is @inbounds or we propagate inbounds, the call's consistency
        # is tainted and not consteval eligible.
        add_remark!(interp, sv, "[constprop] Concrete evel disabled for inbounds")
        return nothing
    end
    isoverlayed(method_table(interp)) && !is_nonoverlayed(result.effects) && return nothing
    if result.edge !== nothing && is_foldable(result.effects)
        if f !== nothing && is_all_const_arg(arginfo, #=start=#2)
            return true
        else
            return false
        end
    end
    return nothing
end

is_all_const_arg(arginfo::ArgInfo, start::Int) = is_all_const_arg(arginfo.argtypes, start::Int)
function is_all_const_arg(argtypes::Vector{Any}, start::Int)
    for i = start:length(argtypes)
        a = widenslotwrapper(argtypes[i])
        isa(a, Const) || isconstType(a) || issingletontype(a) || return false
    end
    return true
end

collect_const_args(arginfo::ArgInfo, start::Int) = collect_const_args(arginfo.argtypes, start)
function collect_const_args(argtypes::Vector{Any}, start::Int)
    return Any[ let a = widenslotwrapper(argtypes[i])
                    isa(a, Const) ? a.val :
                    isconstType(a) ? (a::DataType).parameters[1] :
                    (a::DataType).instance
                end for i = start:length(argtypes) ]
end

struct InvokeCall
    types     # ::Type
    lookupsig # ::Type
    InvokeCall(@nospecialize(types), @nospecialize(lookupsig)) = new(types, lookupsig)
end

function concrete_eval_call(interp::AbstractInterpreter,
    @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo, si::StmtInfo,
    sv::InferenceState, invokecall::Union{Nothing,InvokeCall}=nothing)
    eligible = concrete_eval_eligible(interp, f, result, arginfo, sv)
    eligible === nothing && return false
    if eligible
        args = collect_const_args(arginfo, #=start=#2)
        if invokecall !== nothing
            # this call should be `invoke`d, rewrite `args` back now
            pushfirst!(args, f, invokecall.types)
            f = invoke
        end
        world = get_world_counter(interp)
        edge = result.edge::MethodInstance
        value = try
            Core._call_in_world_total(world, f, args...)
        catch
            # The evaluation threw. By :consistent-cy, we're guaranteed this would have happened at runtime
            return ConstCallResults(Union{}, ConcreteResult(edge, result.effects), result.effects, edge)
        end
        return ConstCallResults(Const(value), ConcreteResult(edge, EFFECTS_TOTAL, value), EFFECTS_TOTAL, edge)
    else # eligible for semi-concrete evaluation
        return true
    end
end

any_conditional(argtypes::Vector{Any}) = any(@nospecialize(x)->isa(x, Conditional), argtypes)
any_conditional(arginfo::ArgInfo) = any_conditional(arginfo.argtypes)

function const_prop_enabled(interp::AbstractInterpreter, sv::InferenceState, match::MethodMatch)
    if !InferenceParams(interp).ipo_constant_propagation
        add_remark!(interp, sv, "[constprop] Disabled by parameter")
        return false
    end
    if is_no_constprop(match.method)
        add_remark!(interp, sv, "[constprop] Disabled by method parameter")
        return false
    end
    return true
end

struct ConstCallResults
    rt::Any
    const_result::ConstResult
    effects::Effects
    edge::MethodInstance
    ConstCallResults(@nospecialize(rt),
                     const_result::ConstResult,
                     effects::Effects,
                     edge::MethodInstance) =
        new(rt, const_result, effects, edge)
end

# TODO MustAlias forwarding

struct ConditionalArgtypes <: ForwardableArgtypes
    arginfo::ArgInfo
    sv::InferenceState
end

"""
    matching_cache_argtypes(𝕃::AbstractLattice, linfo::MethodInstance, argtypes::ConditionalArgtypes)

The implementation is able to forward `Conditional` of `argtypes`,
as well as the other general extended lattice inforamtion.
"""
function matching_cache_argtypes(𝕃::AbstractLattice, linfo::MethodInstance, argtypes::ConditionalArgtypes)
    (; arginfo, sv) = argtypes
    (; fargs, argtypes) = arginfo
    given_argtypes = Vector{Any}(undef, length(argtypes))
    def = linfo.def::Method
    nargs = Int(def.nargs)
    cache_argtypes, overridden_by_const = matching_cache_argtypes(𝕃, linfo)
    local condargs = nothing
    for i in 1:length(argtypes)
        argtype = argtypes[i]
        # forward `Conditional` if it conveys a constraint on any other argument
        if isa(argtype, Conditional) && fargs !== nothing
            cnd = argtype
            slotid = find_constrained_arg(cnd, fargs, sv)
            if slotid !== nothing
                # using union-split signature, we may be able to narrow down `Conditional`
                sigt = widenconst(slotid > nargs ? argtypes[slotid] : cache_argtypes[slotid])
                thentype = tmeet(cnd.thentype, sigt)
                elsetype = tmeet(cnd.elsetype, sigt)
                if thentype === Bottom && elsetype === Bottom
                    # we accidentally proved this method match is impossible
                    # TODO bail out here immediately rather than just propagating Bottom ?
                    given_argtypes[i] = Bottom
                else
                    if condargs === nothing
                        condargs = Tuple{Int,Int}[]
                    end
                    push!(condargs, (slotid, i))
                    given_argtypes[i] = Conditional(slotid, thentype, elsetype)
                end
                continue
            end
        end
        given_argtypes[i] = widenslotwrapper(argtype)
    end
    if condargs !== nothing
        given_argtypes = let condargs=condargs
            va_process_argtypes(𝕃, given_argtypes, linfo) do isva_given_argtypes::Vector{Any}, last::Int
                # invalidate `Conditional` imposed on varargs
                for (slotid, i) in condargs
                    if slotid ≥ last && (1 ≤ i ≤ length(isva_given_argtypes)) # `Conditional` is already widened to vararg-tuple otherwise
                        isva_given_argtypes[i] = widenconditional(isva_given_argtypes[i])
                    end
                end
            end
        end
    else
        given_argtypes = va_process_argtypes(𝕃, given_argtypes, linfo)
    end
    return pick_const_args!(𝕃, cache_argtypes, overridden_by_const, given_argtypes)
end

function abstract_call_method_with_const_args(interp::AbstractInterpreter,
    result::MethodCallResult, @nospecialize(f), arginfo::ArgInfo, si::StmtInfo, match::MethodMatch,
    sv::InferenceState, invokecall::Union{Nothing,InvokeCall}=nothing)
    if !const_prop_enabled(interp, sv, match)
        return nothing
    end
    if is_removable_if_unused(result.effects)
        if isa(result.rt, Const) || call_result_unused(si)
            add_remark!(interp, sv, "[constprop] No more information to be gained")
            return nothing
        end
    end
    res = concrete_eval_call(interp, f, result, arginfo, si, sv, invokecall)
    isa(res, ConstCallResults) && return res
    mi = maybe_get_const_prop_profitable(interp, result, f, arginfo, si, match, sv)
    mi === nothing && return nothing
    # try semi-concrete evaluation
    if res::Bool && !any_conditional(arginfo)
        mi_cache = WorldView(code_cache(interp), sv.world)
        code = get(mi_cache, mi, nothing)
        if code !== nothing
            ir = codeinst_to_ir(interp, code)
            if isa(ir, IRCode)
                irinterp = switch_to_irinterp(interp)
                irsv = IRInterpretationState(irinterp, ir, mi, sv.world, arginfo.argtypes)
                rt, nothrow = ir_abstract_constant_propagation(irinterp, irsv)
                @assert !(rt isa Conditional || rt isa MustAlias) "invalid lattice element returned from IR interpretation"
                if !isa(rt, Type) || typeintersect(rt, Bool) === Union{}
                    new_effects = Effects(result.effects; nothrow=nothrow)
                    return ConstCallResults(rt, SemiConcreteResult(mi, ir, new_effects), new_effects, mi)
                end
            end
        end
    end
    # try constant prop'
    inf_cache = get_inference_cache(interp)
    𝕃ᵢ = typeinf_lattice(interp)
    inf_result = cache_lookup(𝕃ᵢ, mi, arginfo.argtypes, inf_cache)
    if inf_result === nothing
        # if there might be a cycle, check to make sure we don't end up
        # calling ourselves here.
        if result.edgecycle && (result.edgelimited ?
            is_constprop_method_recursed(match.method, sv) :
            # if the type complexity limiting didn't decide to limit the call signature (`result.edgelimited = false`)
            # we can relax the cycle detection by comparing `MethodInstance`s and allow inference to
            # propagate different constant elements if the recursion is finite over the lattice
            is_constprop_edge_recursed(mi, sv))
            add_remark!(interp, sv, "[constprop] Edge cycle encountered")
            return nothing
        end
        argtypes = has_conditional(𝕃ᵢ) ? ConditionalArgtypes(arginfo, sv) : SimpleArgtypes(arginfo.argtypes)
        inf_result = InferenceResult(mi, argtypes, typeinf_lattice(interp))
        if !any(inf_result.overridden_by_const)
            add_remark!(interp, sv, "[constprop] Could not handle constant info in matching_cache_argtypes")
            return nothing
        end
        frame = InferenceState(inf_result, #=cache=#:local, interp)
        if frame === nothing
            add_remark!(interp, sv, "[constprop] Could not retrieve the source")
            return nothing # this is probably a bad generated function (unsound), but just ignore it
        end
        frame.parent = sv
        if !typeinf(interp, frame)
            add_remark!(interp, sv, "[constprop] Fresh constant inference hit a cycle")
            return nothing
        end
        @assert inf_result.result !== nothing
    else
        if inf_result.result === nothing
            add_remark!(interp, sv, "[constprop] Found cached constant inference in a cycle")
            return nothing
        end
    end
    return ConstCallResults(inf_result.result, ConstPropResult(inf_result), inf_result.ipo_effects, mi)
end

# if there's a possibility we could get a better result with these constant arguments
# (hopefully without doing too much work), returns `MethodInstance`, or nothing otherwise
function maybe_get_const_prop_profitable(interp::AbstractInterpreter,
    result::MethodCallResult, @nospecialize(f), arginfo::ArgInfo, si::StmtInfo,
    match::MethodMatch, sv::InferenceState)
    method = match.method
    force = force_const_prop(interp, f, method)
    force || const_prop_entry_heuristic(interp, result, si, sv) || return nothing
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    length(arginfo.argtypes) < nargs && return nothing
    if !const_prop_argument_heuristic(interp, arginfo, sv)
        add_remark!(interp, sv, "[constprop] Disabled by argument and rettype heuristics")
        return nothing
    end
    all_overridden = is_all_overridden(interp, arginfo, sv)
    if !force && !const_prop_function_heuristic(interp, f, arginfo, nargs, all_overridden,
            is_nothrow(sv.ipo_effects), sv)
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
    if !force && !const_prop_methodinstance_heuristic(interp, mi, arginfo, sv)
        add_remark!(interp, sv, "[constprop] Disabled by method instance heuristic")
        return nothing
    end
    return mi
end

function const_prop_entry_heuristic(interp::AbstractInterpreter, result::MethodCallResult, si::StmtInfo, sv::InferenceState)
    if call_result_unused(si) && result.edgecycle
        add_remark!(interp, sv, "[constprop] Disabled by entry heuristic (edgecycle with unused result)")
        return false
    end
    # check if this return type is improvable (i.e. whether it's possible that with more
    # information, we might get a more precise type)
    rt = result.rt
    if isa(rt, Type)
        # could always be improved to `Const`, `PartialStruct` or just a more precise type,
        # unless we're already at `Bottom`
        if rt === Bottom
            add_remark!(interp, sv, "[constprop] Disabled by entry heuristic (erroneous result)")
            return false
        else
            return true
        end
    elseif isa(rt, PartialStruct) || isa(rt, InterConditional) || isa(rt, InterMustAlias)
        # could be improved to `Const` or a more precise wrapper
        return true
    elseif isa(rt, LimitedAccuracy)
        # optimizations like inlining are disabled for limited frames,
        # thus there won't be much benefit in constant-prop' here
        add_remark!(interp, sv, "[constprop] Disabled by entry heuristic (limited accuracy)")
        return false
    else
        if isa(rt, Const)
            if !is_nothrow(result.effects)
                # Could still be improved to Bottom (or at least could see the effects improved)
                return true
            end
        end
        add_remark!(interp, sv, "[constprop] Disabled by entry heuristic (unimprovable result)")
        return false
    end
end

# determines heuristically whether if constant propagation can be worthwhile
# by checking if any of given `argtypes` is "interesting" enough to be propagated
function const_prop_argument_heuristic(interp::AbstractInterpreter, arginfo::ArgInfo, sv::InferenceState)
    𝕃ᵢ = typeinf_lattice(interp)
    argtypes = arginfo.argtypes
    for i in 1:length(argtypes)
        a = argtypes[i]
        if has_conditional(𝕃ᵢ) && isa(a, Conditional) && arginfo.fargs !== nothing
            is_const_prop_profitable_conditional(a, arginfo.fargs, sv) && return true
        else
            a = widenslotwrapper(a)
            has_nontrivial_extended_info(𝕃ᵢ, a) && is_const_prop_profitable_arg(𝕃ᵢ, a) && return true
        end
    end
    return false
end

function is_const_prop_profitable_conditional(cnd::Conditional, fargs::Vector{Any}, sv::InferenceState)
    slotid = find_constrained_arg(cnd, fargs, sv)
    if slotid !== nothing
        return true
    end
    # as a minor optimization, we just check the result is a constant or not,
    # since both `has_nontrivial_extended_info`/`is_const_prop_profitable_arg` return `true`
    # for `Const(::Bool)`
    return isa(widenconditional(cnd), Const)
end

function find_constrained_arg(cnd::Conditional, fargs::Vector{Any}, sv::InferenceState)
    slot = cnd.slot
    for i in 1:length(fargs)
        arg = ssa_def_slot(fargs[i], sv)
        if isa(arg, SlotNumber) && slot_id(arg) == slot
            return i
        end
    end
    return nothing
end

# checks if all argtypes has additional information other than what `Type` can provide
function is_all_overridden(interp::AbstractInterpreter, (; fargs, argtypes)::ArgInfo, sv::InferenceState)
    𝕃ᵢ = typeinf_lattice(interp)
    for i in 1:length(argtypes)
        a = argtypes[i]
        if has_conditional(𝕃ᵢ) && isa(a, Conditional) && fargs !== nothing
            is_const_prop_profitable_conditional(a, fargs, sv) || return false
        else
            is_forwardable_argtype(𝕃ᵢ, widenslotwrapper(a)) || return false
        end
    end
    return true
end

function force_const_prop(interp::AbstractInterpreter, @nospecialize(f), method::Method)
    return is_aggressive_constprop(method) ||
           InferenceParams(interp).aggressive_constant_propagation ||
           istopfunction(f, :getproperty) ||
           istopfunction(f, :setproperty!)
end

function const_prop_function_heuristic(interp::AbstractInterpreter, @nospecialize(f), arginfo::ArgInfo,
    nargs::Int, all_overridden::Bool, still_nothrow::Bool, _::InferenceState)
    argtypes = arginfo.argtypes
    if nargs > 1
        𝕃ᵢ = typeinf_lattice(interp)
        if istopfunction(f, :getindex) || istopfunction(f, :setindex!)
            arrty = argtypes[2]
            # don't propagate constant index into indexing of non-constant array
            if arrty isa Type && arrty <: AbstractArray && !issingletontype(arrty)
                # For static arrays, allow the constprop if we could possibly
                # deduce nothrow as a result.
                if !still_nothrow || ismutabletype(arrty)
                    return false
                end
            elseif ⊑(𝕃ᵢ, arrty, Array)
                return false
            end
        elseif istopfunction(f, :iterate)
            itrty = argtypes[2]
            if ⊑(𝕃ᵢ, itrty, Array)
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
            ty = isvarargtype(at) ? unwraptv(at) : widenconst(at)
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
function const_prop_methodinstance_heuristic(interp::AbstractInterpreter,
    mi::MethodInstance, arginfo::ArgInfo, sv::InferenceState)
    method = mi.def::Method
    if method.is_for_opaque_closure
        # Not inlining an opaque closure can be very expensive, so be generous
        # with the const-prop-ability. It is quite possible that we can't infer
        # anything at all without const-propping, so the inlining check below
        # isn't particularly helpful here.
        return true
    end
    # now check if the source of this method instance is inlineable, since the extended type
    # information we have here would be discarded if it is not inlined into a callee context
    # (modulo the inferred return type that can be potentially refined)
    if is_declared_inline(method)
        # this method is declared as `@inline` and will be inlined
        return true
    end
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
        # Peek at the inferred result for the method to determine if the optimizer
        # was able to cut it down to something simple (inlineable in particular).
        # If so, there will be a good chance we might be able to const prop
        # all the way through and learn something new.
        code = get(code_cache(interp), mi, nothing)
        if isa(code, CodeInstance)
            inferred = @atomic :monotonic code.inferred
            # TODO propagate a specific `CallInfo` that conveys information about this call
            if inlining_policy(interp, inferred, NoCallInfo(), IR_FLAG_NULL, mi, arginfo.argtypes) !== nothing
                return true
            end
        end
    end
    return false # the cache isn't inlineable, so this constant-prop' will most likely be unfruitful
end

# This is only for use with `Conditional`.
# In general, usage of this is wrong.
ssa_def_slot(@nospecialize(arg), sv::IRCode) = nothing
function ssa_def_slot(@nospecialize(arg), sv::InferenceState)
    code = sv.src.code
    init = sv.currpc
    while isa(arg, SSAValue)
        init = arg.id
        arg = code[init]
    end
    if arg isa SlotNumber
        # found this kind of pattern:
        # %init = SlotNumber(x)
        # [...]
        # goto if not isa(%init, T)
        # now conservatively make sure there isn't potentially another conflicting assignment
        # to the same slot between the def and usage
        # we can assume the IR is sorted, since the front-end only creates SSA values in order
        for i = init:(sv.currpc-1)
            e = code[i]
            if isexpr(e, :(=)) && e.args[1] === arg
                return nothing
            end
        end
    else
        # there might still be the following kind of pattern (see #45499):
        # %init = ...
        # [...]
        # SlotNumber(x) = %init
        # [...]
        # goto if not isa(%init, T)
        # let's check if there is a slot assigned to the def SSA value but also there isn't
        # any potentially conflicting assignment to the same slot
        arg = nothing
        def = SSAValue(init)
        for i = (init+1):(sv.currpc-1)
            e = code[i]
            if isexpr(e, :(=))
                lhs = e.args[1]
                if isa(lhs, SlotNumber)
                    lhs === arg && return nothing
                    rhs = e.args[2]
                    if rhs === def
                        arg = lhs
                    end
                end
            end
        end
    end
    return arg
end

struct AbstractIterationResult
    cti::Vector{Any}
    info::MaybeAbstractIterationInfo
    ai_effects::Effects
end
AbstractIterationResult(cti::Vector{Any}, info::MaybeAbstractIterationInfo) =
    AbstractIterationResult(cti, info, EFFECTS_TOTAL)

# `typ` is the inferred type for expression `arg`.
# if the expression constructs a container (e.g. `svec(x,y,z)`),
# refine its type to an array of element types.
# Union of Tuples of the same length is converted to Tuple of Unions.
# returns an array of types
function precise_container_type(interp::AbstractInterpreter, @nospecialize(itft), @nospecialize(typ),
                                sv::Union{InferenceState, IRCode})
    if isa(typ, PartialStruct)
        widet = typ.typ
        if isa(widet, DataType) && widet.name === Tuple.name
            return AbstractIterationResult(typ.fields, nothing)
        end
    end

    if isa(typ, Const)
        val = typ.val
        if isa(val, SimpleVector) || isa(val, Tuple)
            return AbstractIterationResult(Any[ Const(val[i]) for i in 1:length(val) ], nothing) # avoid making a tuple Generator here!
        end
    end

    tti0 = widenconst(typ)
    tti = unwrap_unionall(tti0)
    if isa(tti, DataType) && tti.name === _NAMEDTUPLE_NAME
        # A NamedTuple iteration is the same as the iteration of its Tuple parameter:
        # compute a new `tti == unwrap_unionall(tti0)` based on that Tuple type
        tti = unwraptv(tti.parameters[2])
        tti0 = rewrap_unionall(tti, tti0)
    end
    if isa(tti, Union)
        utis = uniontypes(tti)
        if any(@nospecialize(t) -> !isa(t, DataType) || !(t <: Tuple) || !isknownlength(t), utis)
            return AbstractIterationResult(Any[Vararg{Any}], nothing, Effects())
        end
        ltp = length((utis[1]::DataType).parameters)
        for t in utis
            if length((t::DataType).parameters) != ltp
                return AbstractIterationResult(Any[Vararg{Any}], nothing)
            end
        end
        result = Any[ Union{} for _ in 1:ltp ]
        for t in utis
            tps = (t::DataType).parameters
            _all(valid_as_lattice, tps) || continue
            for j in 1:ltp
                result[j] = tmerge(result[j], rewrap_unionall(tps[j], tti0))
            end
        end
        return AbstractIterationResult(result, nothing)
    elseif tti0 <: Tuple
        if isa(tti0, DataType)
            return AbstractIterationResult(Any[ p for p in tti0.parameters ], nothing)
        elseif !isa(tti, DataType)
            return AbstractIterationResult(Any[Vararg{Any}], nothing)
        else
            len = length(tti.parameters)
            last = tti.parameters[len]
            va = isvarargtype(last)
            elts = Any[ fieldtype(tti0, i) for i = 1:len ]
            if va
                elts[len] = Vararg{elts[len]}
            end
            return AbstractIterationResult(elts, nothing)
        end
    elseif tti0 === SimpleVector
        return AbstractIterationResult(Any[Vararg{Any}], nothing)
    elseif tti0 === Any
        return AbstractIterationResult(Any[Vararg{Any}], nothing, Effects())
    elseif tti0 <: Array
        return AbstractIterationResult(Any[Vararg{eltype(tti0)}], nothing)
    else
        return abstract_iteration(interp, itft, typ, sv)
    end
end

# simulate iteration protocol on container type up to fixpoint
function abstract_iteration(interp::AbstractInterpreter, @nospecialize(itft), @nospecialize(itertype), sv::Union{InferenceState, IRCode})
    if isa(itft, Const)
        iteratef = itft.val
    else
        return AbstractIterationResult(Any[Vararg{Any}], nothing, Effects())
    end
    @assert !isvarargtype(itertype)
    call = abstract_call_known(interp, iteratef, ArgInfo(nothing, Any[itft, itertype]), StmtInfo(true), sv)
    stateordonet = call.rt
    info = call.info
    # Return Bottom if this is not an iterator.
    # WARNING: Changes to the iteration protocol must be reflected here,
    # this is not just an optimization.
    # TODO: this doesn't realize that Array, SimpleVector, Tuple, and NamedTuple do not use the iterate protocol
    stateordonet === Bottom && return AbstractIterationResult(Any[Bottom], AbstractIterationInfo(CallMeta[CallMeta(Bottom, call.effects, info)], true))
    valtype = statetype = Bottom
    ret = Any[]
    calls = CallMeta[call]
    stateordonet_widened = widenconst(stateordonet)
    𝕃ᵢ = typeinf_lattice(interp)

    # Try to unroll the iteration up to max_tuple_splat, which covers any finite
    # length iterators, or interesting prefix
    while true
        if stateordonet_widened === Nothing
            return AbstractIterationResult(ret, AbstractIterationInfo(calls, true))
        end
        if Nothing <: stateordonet_widened || length(ret) >= InferenceParams(interp).max_tuple_splat
            break
        end
        if !isa(stateordonet_widened, DataType) || !(stateordonet_widened <: Tuple) || isvatuple(stateordonet_widened) || length(stateordonet_widened.parameters) != 2
            break
        end
        nstatetype = getfield_tfunc(𝕃ᵢ, stateordonet, Const(2))
        # If there's no new information in this statetype, don't bother continuing,
        # the iterator won't be finite.
        if ⊑(𝕃ᵢ, nstatetype, statetype)
            return AbstractIterationResult(Any[Bottom], AbstractIterationInfo(calls, false), EFFECTS_THROWS)
        end
        valtype = getfield_tfunc(𝕃ᵢ, stateordonet, Const(1))
        push!(ret, valtype)
        statetype = nstatetype
        call = abstract_call_known(interp, iteratef, ArgInfo(nothing, Any[Const(iteratef), itertype, statetype]), StmtInfo(true), sv)
        stateordonet = call.rt
        stateordonet_widened = widenconst(stateordonet)
        push!(calls, call)
    end
    # From here on, we start asking for results on the widened types, rather than
    # the precise (potentially const) state type
    # statetype and valtype are reinitialized in the first iteration below from the
    # (widened) stateordonet, which has not yet been fully analyzed in the loop above
    valtype = statetype = Bottom
    may_have_terminated = Nothing <: stateordonet_widened
    while valtype !== Any
        nounion = typeintersect(stateordonet_widened, Tuple{Any,Any})
        if nounion !== Union{} && !isa(nounion, DataType)
            # nounion is of a type we cannot handle
            valtype = Any
            break
        end
        if nounion === Union{} || (nounion.parameters[1] <: valtype && nounion.parameters[2] <: statetype)
            # reached a fixpoint or iterator failed/gave invalid answer
            if !hasintersect(stateordonet_widened, Nothing)
                # ... but cannot terminate
                if !may_have_terminated
                    #  ... and cannot have terminated prior to this loop
                    return AbstractIterationResult(Any[Bottom], AbstractIterationInfo(calls, false), Effects())
                else
                    # iterator may have terminated prior to this loop, but not during it
                    valtype = Bottom
                end
            end
            break
        end
        valtype = tmerge(valtype, nounion.parameters[1])
        statetype = tmerge(statetype, nounion.parameters[2])
        call = abstract_call_known(interp, iteratef, ArgInfo(nothing, Any[Const(iteratef), itertype, statetype]), StmtInfo(true), sv)
        push!(calls, call)
        stateordonet = call.rt
        stateordonet_widened = widenconst(stateordonet)
    end
    if valtype !== Union{}
        push!(ret, Vararg{valtype})
    end
    return AbstractIterationResult(ret, AbstractIterationInfo(calls, false))
end

# do apply(af, fargs...), where af is a function value
function abstract_apply(interp::AbstractInterpreter, argtypes::Vector{Any}, si::StmtInfo,
                        sv::Union{InferenceState, IRCode},
                        max_methods::Int = get_max_methods(sv.mod, interp))
    itft = argtype_by_index(argtypes, 2)
    aft = argtype_by_index(argtypes, 3)
    (itft === Bottom || aft === Bottom) && return CallMeta(Bottom, EFFECTS_THROWS, NoCallInfo())
    aargtypes = argtype_tail(argtypes, 4)
    aftw = widenconst(aft)
    if !isa(aft, Const) && !isa(aft, PartialOpaque) && (!isType(aftw) || has_free_typevars(aftw))
        if !isconcretetype(aftw) || (aftw <: Builtin)
            add_remark!(interp, sv, "Core._apply_iterate called on a function of a non-concrete type")
            # bail now, since it seems unlikely that abstract_call will be able to do any better after splitting
            # this also ensures we don't call abstract_call_gf_by_type below on an IntrinsicFunction or Builtin
            return CallMeta(Any, Effects(), NoCallInfo())
        end
    end
    res = Union{}
    nargs = length(aargtypes)
    splitunions = 1 < unionsplitcost(typeinf_lattice(interp), aargtypes) <= InferenceParams(interp).max_apply_union_enum
    ctypes = [Any[aft]]
    infos = Vector{MaybeAbstractIterationInfo}[MaybeAbstractIterationInfo[]]
    effects = EFFECTS_TOTAL
    for i = 1:nargs
        ctypes´ = Vector{Any}[]
        infos′ = Vector{MaybeAbstractIterationInfo}[]
        for ti in (splitunions ? uniontypes(aargtypes[i]) : Any[aargtypes[i]])
            if !isvarargtype(ti)
                (;cti, info, ai_effects) = precise_container_type(interp, itft, ti, sv)
            else
                (;cti, info, ai_effects) = precise_container_type(interp, itft, unwrapva(ti), sv)
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
            effects = merge_effects(effects, ai_effects)
            if info !== nothing
                for call in info.each
                    effects = merge_effects(effects, call.effects)
                end
            end
            if any(@nospecialize(t) -> t === Bottom, cti)
                continue
            end
            for j = 1:length(ctypes)
                ct = ctypes[j]::Vector{Any}
                if isvarargtype(ct[end])
                    # This is vararg, we're not gonna be able to do any inlining,
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
    napplicable = length(ctypes)
    seen = 0
    for i = 1:napplicable
        ct = ctypes[i]
        arginfo = infos[i]
        lct = length(ct)
        # truncate argument list at the first Vararg
        for i = 1:lct-1
            cti = ct[i]
            if isvarargtype(cti)
                ct[i] = tuple_tail_elem(unwrapva(cti), ct[(i+1):lct])
                resize!(ct, i)
                break
            end
        end
        call = abstract_call(interp, ArgInfo(nothing, ct), si, sv, max_methods)
        seen += 1
        push!(retinfos, ApplyCallInfo(call.info, arginfo))
        res = tmerge(res, call.rt)
        effects = merge_effects(effects, call.effects)
        if bail_out_apply(interp, InferenceLoopState(ct, res, effects), sv)
            add_remark!(interp, sv, "_apply_iterate inference reached maximally imprecise information. Bailing on.")
            break
        end
    end
    if seen ≠ napplicable
        # there is unanalyzed candidate, widen type and effects to the top
        res = Any
        effects = Effects()
        retinfo = NoCallInfo() # NOTE this is necessary to prevent the inlining processing
    end
    # TODO: Add a special info type to capture all the iteration info.
    # For now, only propagate info if we don't also union-split the iteration
    return CallMeta(res, effects, retinfo)
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

struct ConditionalTypes
    thentype
    elsetype
    ConditionalTypes(thentype, elsetype) = (@nospecialize; new(thentype, elsetype))
end

@inline function isa_condition(@nospecialize(xt), @nospecialize(ty), max_union_splitting::Int,
    @nospecialize(rt))
    if isa(rt, Const)
        xt = widenslotwrapper(xt)
        if rt.val === false
            return ConditionalTypes(Bottom, xt)
        elseif rt.val === true
            return ConditionalTypes(xt, Bottom)
        end
    end
    return isa_condition(xt, ty, max_union_splitting)
end
@inline function isa_condition(@nospecialize(xt), @nospecialize(ty), max_union_splitting::Int)
    tty_ub, isexact_tty = instanceof_tfunc(ty)
    tty = widenconst(xt)
    if isexact_tty && !isa(tty_ub, TypeVar)
        tty_lb = tty_ub # TODO: this would be wrong if !isexact_tty, but instanceof_tfunc doesn't preserve this info
        if !has_free_typevars(tty_lb) && !has_free_typevars(tty_ub)
            thentype = typeintersect(tty, tty_ub)
            if iskindtype(tty_ub) && thentype !== Bottom
                # `typeintersect` may be unable narrow down `Type`-type
                thentype = tty_ub
            end
            valid_as_lattice(thentype) || (thentype = Bottom)
            elsetype = typesubtract(tty, tty_lb, max_union_splitting)
            return ConditionalTypes(thentype, elsetype)
        end
    end
    return nothing
end

@inline function egal_condition(c::Const, @nospecialize(xt), max_union_splitting::Int,
    @nospecialize(rt))
    thentype = c
    elsetype = widenslotwrapper(xt)
    if rt === Const(false)
        thentype = Bottom
    elseif rt === Const(true)
        elsetype = Bottom
    elseif elsetype isa Type && isdefined(typeof(c.val), :instance) # can only widen a if it is a singleton
        elsetype = typesubtract(elsetype, typeof(c.val), max_union_splitting)
    end
    return ConditionalTypes(thentype, elsetype)
end
@inline function egal_condition(c::Const, @nospecialize(xt), max_union_splitting::Int)
    thentype = c
    elsetype = widenslotwrapper(xt)
    if elsetype isa Type && issingletontype(typeof(c.val)) # can only widen a if it is a singleton
        elsetype = typesubtract(elsetype, typeof(c.val), max_union_splitting)
    end
    return ConditionalTypes(thentype, elsetype)
end

function abstract_call_builtin(interp::AbstractInterpreter, f::Builtin, (; fargs, argtypes)::ArgInfo,
                               sv::Union{InferenceState, IRCode}, max_methods::Int)
    @nospecialize f
    la = length(argtypes)
    𝕃ᵢ = typeinf_lattice(interp)
    ⊑ᵢ = ⊑(𝕃ᵢ)
    if has_conditional(𝕃ᵢ) && f === Core.ifelse && fargs isa Vector{Any} && la == 4
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
                if isa(a, SlotNumber) && cnd.slot == slot_id(a)
                    tx = (cnd.thentype ⊑ᵢ tx ? cnd.thentype : tmeet(𝕃ᵢ, tx, widenconst(cnd.thentype)))
                end
                if isa(b, SlotNumber) && cnd.slot == slot_id(b)
                    ty = (cnd.elsetype ⊑ᵢ ty ? cnd.elsetype : tmeet(𝕃ᵢ, ty, widenconst(cnd.elsetype)))
                end
                return tmerge(𝕃ᵢ, tx, ty)
            end
        end
    end
    rt = builtin_tfunction(interp, f, argtypes[2:end], sv)
    if has_mustalias(𝕃ᵢ) && f === getfield && isa(fargs, Vector{Any}) && la ≥ 3
        a3 = argtypes[3]
        if isa(a3, Const)
            if rt !== Bottom && !isalreadyconst(rt)
                var = fargs[2]
                if isa(var, SlotNumber)
                    vartyp = widenslotwrapper(argtypes[2])
                    fldidx = maybe_const_fldidx(vartyp, a3.val)
                    if fldidx !== nothing
                        # wrap this aliasable field into `MustAlias` for possible constraint propagations
                        return MustAlias(var, vartyp, fldidx, rt)
                    end
                end
            end
        end
    elseif has_conditional(𝕃ᵢ) && (rt === Bool || (isa(rt, Const) && isa(rt.val, Bool))) && isa(fargs, Vector{Any})
        # perform very limited back-propagation of type information for `is` and `isa`
        if f === isa
            a = ssa_def_slot(fargs[2], sv)
            a2 = argtypes[2]
            if isa(a, SlotNumber)
                cndt = isa_condition(a2, argtypes[3], InferenceParams(interp).max_union_splitting, rt)
                if cndt !== nothing
                    return Conditional(a, cndt.thentype, cndt.elsetype)
                end
            end
            if isa(a2, MustAlias)
                if !isa(rt, Const) # skip refinement when the field is known precisely (just optimization)
                    cndt = isa_condition(a2, argtypes[3], InferenceParams(interp).max_union_splitting)
                    if cndt !== nothing
                        return form_mustalias_conditional(a2, cndt.thentype, cndt.elsetype)
                    end
                end
            end
        elseif f === (===)
            a = ssa_def_slot(fargs[2], sv)
            b = ssa_def_slot(fargs[3], sv)
            aty = argtypes[2]
            bty = argtypes[3]
            # if doing a comparison to a singleton, consider returning a `Conditional` instead
            if isa(aty, Const)
                if isa(b, SlotNumber)
                    cndt = egal_condition(aty, bty, InferenceParams(interp).max_union_splitting, rt)
                    return Conditional(b, cndt.thentype, cndt.elsetype)
                elseif isa(bty, MustAlias) && !isa(rt, Const) # skip refinement when the field is known precisely (just optimization)
                    cndt = egal_condition(aty, bty.fldtyp, InferenceParams(interp).max_union_splitting)
                    return form_mustalias_conditional(bty, cndt.thentype, cndt.elsetype)
                end
            elseif isa(bty, Const)
                if isa(a, SlotNumber)
                    cndt = egal_condition(bty, aty, InferenceParams(interp).max_union_splitting, rt)
                    return Conditional(a, cndt.thentype, cndt.elsetype)
                elseif isa(aty, MustAlias) && !isa(rt, Const) # skip refinement when the field is known precisely (just optimization)
                    cndt = egal_condition(bty, aty.fldtyp, InferenceParams(interp).max_union_splitting)
                    return form_mustalias_conditional(aty, cndt.thentype, cndt.elsetype)
                end
            end
            # TODO enable multiple constraints propagation here, there are two possible improvements:
            # 1. propagate constraints for both lhs and rhs
            # 2. we can propagate both constraints on aliased fields and slots
            # As for 2, for now, we prioritize constraints on aliased fields, since currently
            # different slots that represent the same object can't share same field constraint,
            # and thus binding `MustAlias` to the other slot is less likely useful
            if !isa(rt, Const) # skip refinement when the field is known precisely (just optimization)
                if isa(bty, MustAlias)
                    thentype = widenslotwrapper(aty)
                    elsetype = bty.fldtyp
                    if thentype ⊏ elsetype
                        return form_mustalias_conditional(bty, thentype, elsetype)
                    end
                elseif isa(aty, MustAlias)
                    thentype = widenslotwrapper(bty)
                    elsetype = aty.fldtyp
                    if thentype ⊏ elsetype
                        return form_mustalias_conditional(aty, thentype, elsetype)
                    end
                end
            end
            # narrow the lattice slightly (noting the dependency on one of the slots), to promote more effective smerge
            if isa(b, SlotNumber)
                thentype = rt === Const(false) ? Bottom : widenslotwrapper(bty)
                elsetype = rt === Const(true)  ? Bottom : widenslotwrapper(bty)
                return Conditional(b, thentype, elsetype)
            elseif isa(a, SlotNumber)
                thentype = rt === Const(false) ? Bottom : widenslotwrapper(aty)
                elsetype = rt === Const(true)  ? Bottom : widenslotwrapper(aty)
                return Conditional(a, thentype, elsetype)
            end
        elseif f === Core.Compiler.not_int
            aty = argtypes[2]
            if isa(aty, Conditional)
                thentype = rt === Const(false) ? Bottom : aty.elsetype
                elsetype = rt === Const(true)  ? Bottom : aty.thentype
                return Conditional(aty.slot, thentype, elsetype)
            end
        elseif f === isdefined
            uty = argtypes[2]
            a = ssa_def_slot(fargs[2], sv)
            if isa(uty, Union) && isa(a, SlotNumber)
                fld = argtypes[3]
                thentype = Bottom
                elsetype = Bottom
                for ty in uniontypes(uty)
                    cnd = isdefined_tfunc(𝕃ᵢ, ty, fld)
                    if isa(cnd, Const)
                        if cnd.val::Bool
                            thentype = tmerge(thentype, ty)
                        else
                            elsetype = tmerge(elsetype, ty)
                        end
                    else
                        thentype = tmerge(thentype, ty)
                        elsetype = tmerge(elsetype, ty)
                    end
                end
                return Conditional(a, thentype, elsetype)
            end
        end
    end
    @assert !isa(rt, TypeVar) "unhandled TypeVar"
    return rt
end

function abstract_call_unionall(interp::AbstractInterpreter, argtypes::Vector{Any})
    if length(argtypes) == 3
        canconst = true
        a2 = argtypes[2]
        a3 = argtypes[3]
        ⊑ᵢ = ⊑(typeinf_lattice(interp))
        nothrow = a2 ⊑ᵢ TypeVar && (a3 ⊑ᵢ Type || a3 ⊑ᵢ TypeVar)
        if isa(a3, Const)
            body = a3.val
        elseif isType(a3)
            body = a3.parameters[1]
            canconst = false
        else
            return CallMeta(Any, Effects(EFFECTS_TOTAL; nothrow), NoCallInfo())
        end
        if !(isa(body, Type) || isa(body, TypeVar))
            return CallMeta(Any, EFFECTS_THROWS, NoCallInfo())
        end
        if has_free_typevars(body)
            if isa(a2, Const)
                tv = a2.val
            elseif isa(a2, PartialTypeVar)
                tv = a2.tv
                canconst = false
            else
                return CallMeta(Any, EFFECTS_THROWS, NoCallInfo())
            end
            isa(tv, TypeVar) || return CallMeta(Any, EFFECTS_THROWS, NoCallInfo())
            body = UnionAll(tv, body)
        end
        ret = canconst ? Const(body) : Type{body}
        return CallMeta(ret, Effects(EFFECTS_TOTAL; nothrow), NoCallInfo())
    end
    return CallMeta(Any, EFFECTS_UNKNOWN, NoCallInfo())
end

function abstract_invoke(interp::AbstractInterpreter, (; fargs, argtypes)::ArgInfo, si::StmtInfo, sv::InferenceState)
    ft′ = argtype_by_index(argtypes, 2)
    ft = widenconst(ft′)
    ft === Bottom && return CallMeta(Bottom, EFFECTS_THROWS, NoCallInfo())
    (types, isexact, isconcrete, istype) = instanceof_tfunc(argtype_by_index(argtypes, 3))
    isexact || return CallMeta(Any, Effects(), NoCallInfo())
    unwrapped = unwrap_unionall(types)
    if types === Bottom || !(unwrapped isa DataType) || unwrapped.name !== Tuple.name
        return CallMeta(Bottom, EFFECTS_THROWS, NoCallInfo())
    end
    argtype = argtypes_to_type(argtype_tail(argtypes, 4))
    nargtype = typeintersect(types, argtype)
    nargtype === Bottom && return CallMeta(Bottom, EFFECTS_THROWS, NoCallInfo())
    nargtype isa DataType || return CallMeta(Any, Effects(), NoCallInfo()) # other cases are not implemented below
    isdispatchelem(ft) || return CallMeta(Any, Effects(), NoCallInfo()) # check that we might not have a subtype of `ft` at runtime, before doing supertype lookup below
    ft = ft::DataType
    lookupsig = rewrap_unionall(Tuple{ft, unwrapped.parameters...}, types)::Type
    nargtype = Tuple{ft, nargtype.parameters...}
    argtype = Tuple{ft, argtype.parameters...}
    match, valid_worlds, overlayed = findsup(lookupsig, method_table(interp))
    match === nothing && return CallMeta(Any, Effects(), NoCallInfo())
    update_valid_age!(sv, valid_worlds)
    method = match.method
    tienv = ccall(:jl_type_intersection_with_env, Any, (Any, Any), nargtype, method.sig)::SimpleVector
    ti = tienv[1]; env = tienv[2]::SimpleVector
    result = abstract_call_method(interp, method, ti, env, false, si, sv)
    (; rt, edge, effects) = result
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
    𝕃ₚ = ipo_lattice(interp)
    f = overlayed ? nothing : singleton_type(ft′)
    invokecall = InvokeCall(types, lookupsig)
    const_call_result = abstract_call_method_with_const_args(interp,
        result, f, arginfo, si, match, sv, invokecall)
    const_result = nothing
    if const_call_result !== nothing
        if ⊑(𝕃ₚ, const_call_result.rt, rt)
            (; rt, effects, const_result, edge) = const_call_result
        end
    end
    rt = from_interprocedural!(𝕃ₚ, rt, sv, arginfo, sig)
    effects = Effects(effects; nonoverlayed=!overlayed)
    info = InvokeCallInfo(match, const_result)
    edge !== nothing && add_invoke_backedge!(sv, lookupsig, edge)
    return CallMeta(rt, effects, info)
end

function invoke_rewrite(xs::Vector{Any})
    x0 = xs[2]
    newxs = xs[3:end]
    newxs[1] = x0
    return newxs
end

function abstract_finalizer(interp::AbstractInterpreter, argtypes::Vector{Any}, sv::InferenceState)
    if length(argtypes) == 3
        finalizer_argvec = Any[argtypes[2], argtypes[3]]
        call = abstract_call(interp, ArgInfo(nothing, finalizer_argvec), StmtInfo(false), sv, 1)
        return CallMeta(Nothing, Effects(), FinalizerInfo(call.info, call.effects))
    end
    return CallMeta(Nothing, Effects(), NoCallInfo())
end

# call where the function is known exactly
function abstract_call_known(interp::AbstractInterpreter, @nospecialize(f),
        arginfo::ArgInfo, si::StmtInfo, sv::Union{InferenceState, IRCode},
        max_methods::Int = isa(sv, InferenceState) ? get_max_methods(f, sv.mod, interp) : 0)
    (; fargs, argtypes) = arginfo
    la = length(argtypes)

    𝕃ᵢ = typeinf_lattice(interp)
    if isa(f, Builtin)
        if f === _apply_iterate
            return abstract_apply(interp, argtypes, si, sv, max_methods)
        elseif f === invoke
            return abstract_invoke(interp, arginfo, si, sv)
        elseif f === modifyfield!
            return abstract_modifyfield!(interp, argtypes, si, sv)
        elseif f === Core.finalizer
            return abstract_finalizer(interp, argtypes, sv)
        elseif f === applicable
            return abstract_applicable(interp, argtypes, sv, max_methods)
        end
        rt = abstract_call_builtin(interp, f, arginfo, sv, max_methods)
        effects = builtin_effects(𝕃ᵢ, f, arginfo, rt)
        if f === getfield && (fargs !== nothing && isexpr(fargs[end], :boundscheck)) && !is_nothrow(effects) && isa(sv, InferenceState)
            # As a special case, we delayed tainting `noinbounds` for getfield calls in case we can prove
            # in-boundedness indepedently. Here we need to put that back in other cases.
            # N.B.: This isn't about the effects of the call itself, but a delayed contribution of the :boundscheck
            # statement, so we need to merge this directly into sv, rather than modifying thte effects.
            merge_effects!(interp, sv, Effects(EFFECTS_TOTAL; noinbounds=false,
                consistent = (get_curr_ssaflag(sv) & IR_FLAG_INBOUNDS) != 0 ? ALWAYS_FALSE : ALWAYS_TRUE))
        end
        return CallMeta(rt, effects, NoCallInfo())
    elseif isa(f, Core.OpaqueClosure)
        # calling an OpaqueClosure about which we have no information returns no information
        return CallMeta(typeof(f).parameters[2], Effects(), NoCallInfo())
    elseif f === TypeVar
        # Manually look through the definition of TypeVar to
        # make sure to be able to get `PartialTypeVar`s out.
        (la < 2 || la > 4) && return CallMeta(Union{}, EFFECTS_UNKNOWN, NoCallInfo())
        n = argtypes[2]
        ub_var = Const(Any)
        lb_var = Const(Union{})
        if la == 4
            ub_var = argtypes[4]
            lb_var = argtypes[3]
        elseif la == 3
            ub_var = argtypes[3]
        end
        pT = typevar_tfunc(𝕃ᵢ, n, lb_var, ub_var)
        effects = builtin_effects(𝕃ᵢ, Core._typevar, ArgInfo(nothing,
            Any[Const(Core._typevar), n, lb_var, ub_var]), pT)
        return CallMeta(pT, effects, NoCallInfo())
    elseif f === UnionAll
        return abstract_call_unionall(interp, argtypes)
    elseif f === Tuple && la == 2
        aty = argtypes[2]
        ty = isvarargtype(aty) ? unwrapva(aty) : widenconst(aty)
        if !isconcretetype(ty)
            return CallMeta(Tuple, EFFECTS_UNKNOWN, NoCallInfo())
        end
    elseif is_return_type(f)
        return return_type_tfunc(interp, argtypes, si, sv)
    elseif la == 2 && istopfunction(f, :!)
        # handle Conditional propagation through !Bool
        aty = argtypes[2]
        if isa(aty, Conditional)
            call = abstract_call_gf_by_type(interp, f, ArgInfo(fargs, Any[Const(f), Bool]), si, Tuple{typeof(f), Bool}, sv, max_methods) # make sure we've inferred `!(::Bool)`
            return CallMeta(Conditional(aty.slot, aty.elsetype, aty.thentype), call.effects, call.info)
        end
    elseif la == 3 && istopfunction(f, :!==)
        # mark !== as exactly a negated call to ===
        rty = abstract_call_known(interp, (===), arginfo, si, sv, max_methods).rt
        if isa(rty, Conditional)
            return CallMeta(Conditional(rty.slot, rty.elsetype, rty.thentype), EFFECTS_TOTAL, NoCallInfo()) # swap if-else
        elseif isa(rty, Const)
            return CallMeta(Const(rty.val === false), EFFECTS_TOTAL, MethodResultPure())
        end
        return CallMeta(rty, EFFECTS_TOTAL, NoCallInfo())
    elseif la == 3 && istopfunction(f, :(>:))
        # mark issupertype as a exact alias for issubtype
        # swap T1 and T2 arguments and call <:
        if fargs !== nothing && length(fargs) == 3
            fargs = Any[<:, fargs[3], fargs[2]]
        else
            fargs = nothing
        end
        argtypes = Any[typeof(<:), argtypes[3], argtypes[2]]
        return abstract_call_known(interp, <:, ArgInfo(fargs, argtypes), si, sv, max_methods)
    elseif la == 2 && istopfunction(f, :typename)
        return CallMeta(typename_static(argtypes[2]), EFFECTS_TOTAL, MethodResultPure())
    elseif f === Core._hasmethod
        return _hasmethod_tfunc(interp, argtypes, sv)
    end
    atype = argtypes_to_type(argtypes)
    return abstract_call_gf_by_type(interp, f, arginfo, si, atype, sv, max_methods)
end

function abstract_call_opaque_closure(interp::AbstractInterpreter,
    closure::PartialOpaque, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState, check::Bool=true)
    sig = argtypes_to_type(arginfo.argtypes)
    result = abstract_call_method(interp, closure.source, sig, Core.svec(), false, si, sv)
    (; rt, edge, effects) = result
    tt = closure.typ
    sigT = (unwrap_unionall(tt)::DataType).parameters[1]
    match = MethodMatch(sig, Core.svec(), closure.source, sig <: rewrap_unionall(sigT, tt))
    𝕃ₚ = ipo_lattice(interp)
    ⊑ₚ = ⊑(𝕃ₚ)
    const_result = nothing
    if !result.edgecycle
        const_call_result = abstract_call_method_with_const_args(interp, result,
            nothing, arginfo, si, match, sv)
        if const_call_result !== nothing
            if const_call_result.rt ⊑ₚ rt
                (; rt, effects, const_result, edge) = const_call_result
            end
        end
    end
    if check # analyze implicit type asserts on argument and return type
        ftt = closure.typ
        (aty, rty) = (unwrap_unionall(ftt)::DataType).parameters
        rty = rewrap_unionall(rty isa TypeVar ? rty.lb : rty, ftt)
        if !(rt ⊑ₚ rty && tuple_tfunc(𝕃ₚ, arginfo.argtypes[2:end]) ⊑ₚ rewrap_unionall(aty, ftt))
            effects = Effects(effects; nothrow=false)
        end
    end
    rt = from_interprocedural!(𝕃ₚ, rt, sv, arginfo, match.spec_types)
    info = OpaqueClosureCallInfo(match, const_result)
    edge !== nothing && add_backedge!(sv, edge)
    return CallMeta(rt, effects, info)
end

function most_general_argtypes(closure::PartialOpaque)
    ret = Any[]
    cc = widenconst(closure)
    argt = (unwrap_unionall(cc)::DataType).parameters[1]
    if !isa(argt, DataType) || argt.name !== typename(Tuple)
        argt = Tuple
    end
    return most_general_argtypes(closure.source, argt, #=withfirst=#false)
end

# call where the function is any lattice element
function abstract_call(interp::AbstractInterpreter, arginfo::ArgInfo, si::StmtInfo,
                       sv::Union{InferenceState, IRCode}, max_methods::Union{Int, Nothing} = isa(sv, IRCode) ? 0 : nothing)
    argtypes = arginfo.argtypes
    ft = widenslotwrapper(argtypes[1])
    f = singleton_type(ft)
    if f === nothing
        if isa(ft, PartialOpaque)
            newargtypes = copy(argtypes)
            newargtypes[1] = ft.env
            return abstract_call_opaque_closure(interp,
                ft, ArgInfo(arginfo.fargs, newargtypes), si, sv, #=check=#true)
        end
        wft = widenconst(ft)
        if hasintersect(wft, Builtin)
            add_remark!(interp, sv, "Could not identify method table for call")
            return CallMeta(Any, Effects(), NoCallInfo())
        elseif hasintersect(wft, Core.OpaqueClosure)
            uft = unwrap_unionall(wft)
            if isa(uft, DataType)
                return CallMeta(rewrap_unionall(uft.parameters[2], wft), Effects(), NoCallInfo())
            end
            return CallMeta(Any, Effects(), NoCallInfo())
        end
        # non-constant function, but the number of arguments is known and the `f` is not a builtin or intrinsic
        max_methods = max_methods === nothing ? get_max_methods(sv.mod, interp) : max_methods
        return abstract_call_gf_by_type(interp, nothing, arginfo, si, argtypes_to_type(argtypes), sv, max_methods)
    end
    max_methods = max_methods === nothing ? get_max_methods(f, sv.mod, interp) : max_methods
    return abstract_call_known(interp, f, arginfo, si, sv, max_methods)
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
                sparam_vals = Any[isvarargtype(v) ? TypeVar(:N, Union{}, Any) :
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
    return unwraptv(T)
end

function abstract_eval_cfunction(interp::AbstractInterpreter, e::Expr, vtypes::VarTable, sv::InferenceState)
    f = abstract_eval_value(interp, e.args[2], vtypes, sv)
    # rt = sp_type_rewrap(e.args[3], sv.linfo, true)
    at = Any[ sp_type_rewrap(argt, sv.linfo, false) for argt in e.args[4]::SimpleVector ]
    pushfirst!(at, f)
    # this may be the wrong world for the call,
    # but some of the result is likely to be valid anyways
    # and that may help generate better codegen
    abstract_call(interp, ArgInfo(nothing, at), StmtInfo(false), sv)
    nothing
end

function abstract_eval_value_expr(interp::AbstractInterpreter, e::Expr, vtypes::Union{VarTable, Nothing}, sv::Union{InferenceState, IRCode})
    rt = Any
    head = e.head
    if head === :static_parameter
        n = e.args[1]::Int
        nothrow = false
        if 1 <= n <= length(sv.sptypes)
            sp = sv.sptypes[n]
            rt = sp.typ
            nothrow = !sp.undef
        end
        merge_effects!(interp, sv, Effects(EFFECTS_TOTAL; nothrow))
        return rt
    elseif head === :boundscheck
        if isa(sv, InferenceState)
            stmt = sv.src.code[sv.currpc]
            if isexpr(stmt, :call)
                f = abstract_eval_value(interp, stmt.args[1], vtypes, sv)
                if f isa Const && f.val === getfield
                    # boundscheck of `getfield` call is analyzed by tfunc potentially without
                    # tainting :inbounds or :consistent when it's known to be nothrow
                    @goto delay_effects_analysis
                end
            end
            # If there is no particular `@inbounds` for this function, then we only taint `:noinbounds`,
            # which will subsequently taint `:consistent`-cy if this function is called from another
            # function that uses `@inbounds`. However, if this `:boundscheck` is itself within an
            # `@inbounds` region, its value depends on `--check-bounds`, so we need to taint
            # `:consistent`-cy here also.
            merge_effects!(interp, sv, Effects(EFFECTS_TOTAL; noinbounds=false,
                consistent = (get_curr_ssaflag(sv) & IR_FLAG_INBOUNDS) != 0 ? ALWAYS_FALSE : ALWAYS_TRUE))
        end
        @label delay_effects_analysis
        rt = Bool
    elseif head === :inbounds
        @assert false && "Expected this to have been moved into flags"
    elseif head === :the_exception
        merge_effects!(interp, sv, Effects(EFFECTS_TOTAL; consistent=ALWAYS_FALSE))
    end
    return rt
end

function abstract_eval_special_value(interp::AbstractInterpreter, @nospecialize(e), vtypes::Union{VarTable, Nothing}, sv::Union{InferenceState, IRCode})
    if isa(e, QuoteNode)
        return Const(e.value)
    elseif isa(e, SSAValue)
        return abstract_eval_ssavalue(e, sv)
    elseif isa(e, SlotNumber)
        vtyp = vtypes[slot_id(e)]
        if vtyp.undef
            merge_effects!(interp, sv, Effects(EFFECTS_TOTAL; nothrow=false))
        end
        return vtyp.typ
    elseif isa(e, Argument)
        if !isa(vtypes, Nothing)
            return vtypes[slot_id(e)].typ
        else
            @assert isa(sv, IRCode)
            return sv.argtypes[e.n]
        end
    elseif isa(e, GlobalRef)
        return abstract_eval_globalref(interp, e, sv)
    end

    return Const(e)
end

function abstract_eval_value(interp::AbstractInterpreter, @nospecialize(e), vtypes::Union{VarTable, Nothing}, sv::Union{InferenceState, IRCode})
    if isa(e, Expr)
        return abstract_eval_value_expr(interp, e, vtypes, sv)
    else
        typ = abstract_eval_special_value(interp, e, vtypes, sv)
        return collect_limitations!(typ, sv)
    end
end

function collect_argtypes(interp::AbstractInterpreter, ea::Vector{Any}, vtypes::Union{VarTable, Nothing}, sv::Union{InferenceState, IRCode})
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

struct RTEffects
    rt
    effects::Effects
    RTEffects(@nospecialize(rt), effects::Effects) = new(rt, effects)
end

function abstract_eval_statement_expr(interp::AbstractInterpreter, e::Expr, vtypes::Union{VarTable, Nothing},
                                      sv::Union{InferenceState, IRCode}, mi::Union{MethodInstance, Nothing})::RTEffects
    effects = EFFECTS_UNKNOWN
    ehead = e.head
    𝕃ᵢ = typeinf_lattice(interp)
    ⊑ᵢ = ⊑(𝕃ᵢ)
    if ehead === :call
        ea = e.args
        argtypes = collect_argtypes(interp, ea, vtypes, sv)
        if argtypes === nothing
            rt = Bottom
            effects = Effects()
        else
            arginfo = ArgInfo(ea, argtypes)
            si = StmtInfo(isa(sv, IRCode) ? true : !call_result_unused(sv, sv.currpc))
            (; rt, effects, info) = abstract_call(interp, arginfo, si, sv)
            if isa(sv, InferenceState)
                sv.stmt_info[sv.currpc] = info
                # mark this call statement as DCE-elgible
                # TODO better to do this in a single pass based on the `info` object at the end of abstractinterpret?
                if is_removable_if_unused(effects)
                    add_curr_ssaflag!(sv, IR_FLAG_EFFECT_FREE)
                else
                    sub_curr_ssaflag!(sv, IR_FLAG_EFFECT_FREE)
                end
            end
        end
        t = rt
    elseif ehead === :new
        t, isexact = instanceof_tfunc(abstract_eval_value(interp, e.args[1], vtypes, sv))
        ut = unwrap_unionall(t)
        consistent = ALWAYS_FALSE
        nothrow = false
        if isa(ut, DataType) && !isabstracttype(ut)
            ismutable = ismutabletype(ut)
            fcount = datatype_fieldcount(ut)
            nargs = length(e.args) - 1
            if fcount === nothing || (fcount > nargs && any(i::Int -> !is_undefref_fieldtype(fieldtype(t, i)), (nargs+1):fcount))
                # allocation with undefined field leads to undefined behavior and should taint `:consistent`-cy
                consistent = ALWAYS_FALSE
            elseif ismutable
                # mutable object isn't `:consistent`, but we can still give the return
                # type information a chance to refine this `:consistent`-cy later
                consistent = CONSISTENT_IF_NOTRETURNED
            else
                consistent = ALWAYS_TRUE
            end
            if isconcretedispatch(t)
                nothrow = true
                @assert fcount !== nothing && fcount ≥ nargs "malformed :new expression" # syntactically enforced by the front-end
                ats = Vector{Any}(undef, nargs)
                local anyrefine = false
                local allconst = true
                for i = 1:nargs
                    at = widenslotwrapper(abstract_eval_value(interp, e.args[i+1], vtypes, sv))
                    ft = fieldtype(t, i)
                    nothrow && (nothrow = at ⊑ᵢ ft)
                    at = tmeet(𝕃ᵢ, at, ft)
                    at === Bottom && @goto always_throw
                    if ismutable && !isconst(t, i)
                        ats[i] = ft # can't constrain this field (as it may be modified later)
                        continue
                    end
                    allconst &= isa(at, Const)
                    if !anyrefine
                        anyrefine = has_nontrivial_extended_info(𝕃ᵢ, at) || # extended lattice information
                                    ⋤(𝕃ᵢ, at, ft) # just a type-level information, but more precise than the declared type
                    end
                    ats[i] = at
                end
                # For now, don't allow:
                # - Const/PartialStruct of mutables (but still allow PartialStruct of mutables
                #   with `const` fields if anything refined)
                # - partially initialized Const/PartialStruct
                if fcount == nargs
                    if consistent === ALWAYS_TRUE && allconst
                        argvals = Vector{Any}(undef, nargs)
                        for j in 1:nargs
                            argvals[j] = (ats[j]::Const).val
                        end
                        t = Const(ccall(:jl_new_structv, Any, (Any, Ptr{Cvoid}, UInt32), t, argvals, nargs))
                    elseif anyrefine
                        t = PartialStruct(t, ats)
                    end
                end
            else
                t = refine_partial_type(t)
            end
        end
        effects = Effects(EFFECTS_TOTAL; consistent, nothrow)
    elseif ehead === :splatnew
        t, isexact = instanceof_tfunc(abstract_eval_value(interp, e.args[1], vtypes, sv))
        nothrow = false # TODO: More precision
        if length(e.args) == 2 && isconcretedispatch(t) && !ismutabletype(t)
            at = abstract_eval_value(interp, e.args[2], vtypes, sv)
            n = fieldcount(t)
            if isa(at, Const) && isa(at.val, Tuple) && n == length(at.val::Tuple) &&
                let t = t, at = at; all(i::Int->getfield(at.val::Tuple, i) isa fieldtype(t, i), 1:n); end
                nothrow = isexact
                t = Const(ccall(:jl_new_structt, Any, (Any, Any), t, at.val))
            elseif isa(at, PartialStruct) && at ⊑ᵢ Tuple && n == length(at.fields::Vector{Any}) &&
                let t = t, at = at; all(i::Int->(at.fields::Vector{Any})[i] ⊑ᵢ fieldtype(t, i), 1:n); end
                nothrow = isexact
                t = PartialStruct(t, at.fields::Vector{Any})
            end
        else
            t = refine_partial_type(t)
        end
        consistent = !ismutabletype(t) ? ALWAYS_TRUE : CONSISTENT_IF_NOTRETURNED
        effects = Effects(EFFECTS_TOTAL; consistent, nothrow)
    elseif ehead === :new_opaque_closure
        t = Union{}
        effects = Effects() # TODO
        merge_effects!(interp, sv, effects)
        if length(e.args) >= 4
            ea = e.args
            argtypes = collect_argtypes(interp, ea, vtypes, sv)
            if argtypes === nothing
                t = Bottom
            else
                mi′ = isa(sv, InferenceState) ? sv.linfo : mi
                t = _opaque_closure_tfunc(𝕃ᵢ, argtypes[1], argtypes[2], argtypes[3],
                    argtypes[4], argtypes[5:end], mi′)
                if isa(t, PartialOpaque) && isa(sv, InferenceState) && !call_result_unused(sv, sv.currpc)
                    # Infer this now so that the specialization is available to
                    # optimization.
                    argtypes = most_general_argtypes(t)
                    pushfirst!(argtypes, t.env)
                    callinfo = abstract_call_opaque_closure(interp, t,
                        ArgInfo(nothing, argtypes), StmtInfo(true), sv, #=check=#false)
                    sv.stmt_info[sv.currpc] = OpaqueClosureCreateInfo(callinfo)
                end
            end
        end
    elseif ehead === :foreigncall
        (;rt, effects) = abstract_eval_foreigncall(interp, e, vtypes, sv, mi)
        t = rt
        if isa(sv, InferenceState)
            # mark this call statement as DCE-elgible
            if is_removable_if_unused(effects)
                add_curr_ssaflag!(sv, IR_FLAG_EFFECT_FREE)
            else
                sub_curr_ssaflag!(sv, IR_FLAG_EFFECT_FREE)
            end
        end
    elseif ehead === :cfunction
        effects = EFFECTS_UNKNOWN
        t = e.args[1]
        isa(t, Type) || (t = Any)
        abstract_eval_cfunction(interp, e, vtypes, sv)
    elseif ehead === :method
        t = (length(e.args) == 1) ? Any : Nothing
        effects = EFFECTS_UNKNOWN
    elseif ehead === :copyast
        effects = EFFECTS_UNKNOWN
        t = abstract_eval_value(interp, e.args[1], vtypes, sv)
        if t isa Const && t.val isa Expr
            # `copyast` makes copies of Exprs
            t = Expr
        end
    elseif ehead === :invoke || ehead === :invoke_modify
        error("type inference data-flow error: tried to double infer a function")
    elseif ehead === :isdefined
        sym = e.args[1]
        t = Bool
        effects = EFFECTS_TOTAL
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
            elseif InferenceParams(interp).assume_bindings_static
                t = Const(false)
            end
        elseif isa(sym, GlobalRef)
            if isdefined(sym.mod, sym.name)
                t = Const(true)
            elseif InferenceParams(interp).assume_bindings_static
                t = Const(false)
            end
        elseif isexpr(sym, :static_parameter)
            n = sym.args[1]::Int
            if 1 <= n <= length(sv.sptypes)
                sp = sv.sptypes[n]
                if !sp.undef
                    t = Const(true)
                elseif sp.typ === Bottom
                    t = Const(false)
                end
            end
        end
    elseif false
        @label always_throw
        t = Bottom
        effects = EFFECTS_THROWS
    else
        t = abstract_eval_value_expr(interp, e, vtypes, sv)
        effects = EFFECTS_TOTAL
    end
    return RTEffects(t, effects)
end

# refine the result of instantiation of partially-known type `t` if some invariant can be assumed
function refine_partial_type(@nospecialize t)
    t′ = unwrap_unionall(t)
    if isa(t′, DataType) && t′.name === _NAMEDTUPLE_NAME && length(t′.parameters) == 2 &&
        (t′.parameters[1] === () || t′.parameters[2] === Tuple{})
        # if the first/second parameter of `NamedTuple` is known to be empty,
        # the second/first argument should also be empty tuple type,
        # so refine it here
        return Const(NamedTuple())
    end
    return t
end

function abstract_eval_foreigncall(interp::AbstractInterpreter, e::Expr, vtypes::Union{VarTable, Nothing}, sv::Union{InferenceState, IRCode}, mi::Union{MethodInstance, Nothing}=nothing)
    abstract_eval_value(interp, e.args[1], vtypes, sv)
    mi′ = isa(sv, InferenceState) ? sv.linfo : mi
    t = sp_type_rewrap(e.args[2], mi′, true)
    for i = 3:length(e.args)
        if abstract_eval_value(interp, e.args[i], vtypes, sv) === Bottom
            return RTEffects(Bottom, EFFECTS_THROWS)
        end
    end
    effects = foreigncall_effects(e) do @nospecialize x
        abstract_eval_value(interp, x, vtypes, sv)
    end
    cconv = e.args[5]
    if isa(cconv, QuoteNode) && (v = cconv.value; isa(v, Tuple{Symbol, UInt8}))
        override = decode_effects_override(v[2])
        effects = Effects(
            override.consistent          ? ALWAYS_TRUE : effects.consistent,
            override.effect_free         ? ALWAYS_TRUE : effects.effect_free,
            override.nothrow             ? true        : effects.nothrow,
            override.terminates_globally ? true        : effects.terminates,
            override.notaskstate         ? true        : effects.notaskstate,
            override.inaccessiblememonly ? ALWAYS_TRUE : effects.inaccessiblememonly,
            effects.nonoverlayed,
            effects.noinbounds)
    end
    return RTEffects(t, effects)
end

function abstract_eval_phi(interp::AbstractInterpreter, phi::PhiNode, vtypes::Union{VarTable, Nothing}, sv::Union{InferenceState, IRCode})
    rt = Union{}
    for i in 1:length(phi.values)
        isassigned(phi.values, i) || continue
        val = phi.values[i]
        rt = tmerge(typeinf_lattice(interp), rt, abstract_eval_special_value(interp, val, vtypes, sv))
    end
    return rt
end

function stmt_taints_inbounds_consistency(sv::InferenceState)
    sv.src.propagate_inbounds && return true
    return (get_curr_ssaflag(sv) & IR_FLAG_INBOUNDS) != 0
end

function abstract_eval_statement(interp::AbstractInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if !isa(e, Expr)
        if isa(e, PhiNode)
            return abstract_eval_phi(interp, e, vtypes, sv)
        end
        return abstract_eval_special_value(interp, e, vtypes, sv)
    end
    (;rt, effects) = abstract_eval_statement_expr(interp, e, vtypes, sv, nothing)
    if !effects.noinbounds
        if !sv.src.propagate_inbounds
            # The callee read our inbounds flag, but unless we propagate inbounds,
            # we ourselves don't read our parent's inbounds.
            effects = Effects(effects; noinbounds=true)
        end
        if (get_curr_ssaflag(sv) & IR_FLAG_INBOUNDS) != 0
            effects = Effects(effects; consistent=ALWAYS_FALSE)
        end
    end
    merge_effects!(interp, sv, effects)
    e = e::Expr
    @assert !isa(rt, TypeVar) "unhandled TypeVar"
    rt = maybe_singleton_const(rt)
    if !isempty(sv.pclimitations)
        if rt isa Const || rt === Union{}
            empty!(sv.pclimitations)
        else
            rt = LimitedAccuracy(rt, sv.pclimitations)
            sv.pclimitations = IdSet{InferenceState}()
        end
    end
    return rt
end

function isdefined_globalref(g::GlobalRef)
    return ccall(:jl_globalref_boundp, Cint, (Any,), g) != 0
end

function abstract_eval_globalref(g::GlobalRef)
    if isdefined_globalref(g) && isconst(g)
        return Const(ccall(:jl_get_globalref_value, Any, (Any,), g))
    end
    ty = ccall(:jl_get_binding_type, Any, (Any, Any), g.mod, g.name)
    ty === nothing && return Any
    return ty
end
abstract_eval_global(M::Module, s::Symbol) = abstract_eval_globalref(GlobalRef(M, s))

function abstract_eval_globalref(interp::AbstractInterpreter, g::GlobalRef, frame::Union{InferenceState, IRCode})
    rt = abstract_eval_globalref(g)
    consistent = inaccessiblememonly = ALWAYS_FALSE
    nothrow = false
    if isa(rt, Const)
        consistent = ALWAYS_TRUE
        if is_mutation_free_argtype(rt)
            inaccessiblememonly = ALWAYS_TRUE
            nothrow = true
        else
            nothrow = true
        end
    elseif isdefined_globalref(g)
        nothrow = true
    elseif InferenceParams(interp).assume_bindings_static
        consistent = inaccessiblememonly = ALWAYS_TRUE
        rt = Union{}
    end
    merge_effects!(interp, frame, Effects(EFFECTS_TOTAL; consistent, nothrow, inaccessiblememonly))
    return rt
end

function handle_global_assignment!(interp::AbstractInterpreter, frame::InferenceState, lhs::GlobalRef, @nospecialize(newty))
    effect_free = ALWAYS_FALSE
    nothrow = global_assignment_nothrow(lhs.mod, lhs.name, newty)
    inaccessiblememonly = ALWAYS_FALSE
    merge_effects!(interp, frame, Effects(EFFECTS_TOTAL; effect_free, nothrow, inaccessiblememonly))
    return nothing
end

abstract_eval_ssavalue(s::SSAValue, sv::InferenceState) = abstract_eval_ssavalue(s, sv.ssavaluetypes)
abstract_eval_ssavalue(s::SSAValue, src::CodeInfo) = abstract_eval_ssavalue(s, src.ssavaluetypes::Vector{Any})
function abstract_eval_ssavalue(s::SSAValue, ssavaluetypes::Vector{Any})
    typ = ssavaluetypes[s.id]
    if typ === NOT_FOUND
        return Bottom
    end
    return typ
end

struct BestguessInfo{Interp<:AbstractInterpreter}
    interp::Interp
    bestguess
    nargs::Int
    slottypes::Vector{Any}
    changes::VarTable
    function BestguessInfo(interp::Interp, @nospecialize(bestguess), nargs::Int,
        slottypes::Vector{Any}, changes::VarTable) where Interp<:AbstractInterpreter
        new{Interp}(interp, bestguess, nargs, slottypes, changes)
    end
end

function widenreturn(@nospecialize(rt), info::BestguessInfo)
    return widenreturn(typeinf_lattice(info.interp), rt, info)
end

function widenreturn(𝕃ᵢ::AbstractLattice, @nospecialize(rt), info::BestguessInfo)
    return widenreturn(widenlattice(𝕃ᵢ), rt, info)
end
function widenreturn_noslotwrapper(𝕃ᵢ::AbstractLattice, @nospecialize(rt), info::BestguessInfo)
    return widenreturn_noslotwrapper(widenlattice(𝕃ᵢ), rt, info)
end

function widenreturn(𝕃ᵢ::MustAliasesLattice, @nospecialize(rt), info::BestguessInfo)
    if isa(rt, MustAlias)
        if 1 ≤ rt.slot ≤ info.nargs
            rt = InterMustAlias(rt)
        else
            rt = widenmustalias(rt)
        end
    end
    isa(rt, InterMustAlias) && return rt
    return widenreturn(widenlattice(𝕃ᵢ), rt, info)
end

function widenreturn(𝕃ᵢ::ConditionalsLattice, @nospecialize(rt), info::BestguessInfo)
    ⊑ᵢ = ⊑(𝕃ᵢ)
    if !(⊑(ipo_lattice(info.interp), info.bestguess, Bool)) || info.bestguess === Bool
        # give up inter-procedural constraint back-propagation
        # when tmerge would widen the result anyways (as an optimization)
        rt = widenconditional(rt)
    else
        if isa(rt, Conditional)
            id = rt.slot
            if 1 ≤ id ≤ info.nargs
                old_id_type = widenconditional(info.slottypes[id]) # same as `(states[1]::VarTable)[id].typ`
                if (!(rt.thentype ⊑ᵢ old_id_type) || old_id_type ⊑ᵢ rt.thentype) &&
                   (!(rt.elsetype ⊑ᵢ old_id_type) || old_id_type ⊑ᵢ rt.elsetype)
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
            rt = InterConditional(rt.slot, rt.thentype, rt.elsetype)
        elseif is_lattice_bool(𝕃ᵢ, rt)
            rt = bool_rt_to_conditional(rt, info)
        end
    end
    if isa(rt, Conditional)
        rt = InterConditional(rt)
    end
    isa(rt, InterConditional) && return rt
    return widenreturn(widenlattice(𝕃ᵢ), rt, info)
end
function bool_rt_to_conditional(@nospecialize(rt), info::BestguessInfo)
    bestguess = info.bestguess
    if isa(bestguess, InterConditional)
        # if the bestguess so far is already `Conditional`, try to convert
        # this `rt` into `Conditional` on the slot to avoid overapproximation
        # due to conflict of different slots
        rt = bool_rt_to_conditional(rt, bestguess.slot, info)
    else
        # pick up the first "interesting" slot, convert `rt` to its `Conditional`
        # TODO: ideally we want `Conditional` and `InterConditional` to convey
        # constraints on multiple slots
        for slot_id = 1:info.nargs
            rt = bool_rt_to_conditional(rt, slot_id, info)
            rt isa InterConditional && break
        end
    end
    return rt
end
function bool_rt_to_conditional(@nospecialize(rt), slot_id::Int, info::BestguessInfo)
    ⊑ᵢ = ⊑(typeinf_lattice(info.interp))
    old = info.slottypes[slot_id]
    new = widenslotwrapper(info.changes[slot_id].typ) # avoid nested conditional
    if new ⊑ᵢ old && !(old ⊑ᵢ new)
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

function widenreturn(𝕃ᵢ::PartialsLattice, @nospecialize(rt), info::BestguessInfo)
    return widenreturn_partials(𝕃ᵢ, rt, info)
end
function widenreturn_noslotwrapper(𝕃ᵢ::PartialsLattice, @nospecialize(rt), info::BestguessInfo)
    return widenreturn_partials(𝕃ᵢ, rt, info)
end
function widenreturn_partials(𝕃ᵢ::PartialsLattice, @nospecialize(rt), info::BestguessInfo)
    if isa(rt, PartialStruct)
        fields = copy(rt.fields)
        local anyrefine = false
        𝕃 = typeinf_lattice(info.interp)
        for i in 1:length(fields)
            a = fields[i]
            a = isvarargtype(a) ? a : widenreturn_noslotwrapper(𝕃, a, info)
            if !anyrefine
                # TODO: consider adding && const_prop_profitable(a) here?
                anyrefine = has_extended_info(a) ||
                            ⊏(𝕃, a, fieldtype(rt.typ, i))
            end
            fields[i] = a
        end
        anyrefine && return PartialStruct(rt.typ, fields)
    end
    if isa(rt, PartialOpaque)
        return rt # XXX: this case was missed in #39512
    end
    return widenreturn(widenlattice(𝕃ᵢ), rt, info)
end

function widenreturn(::ConstsLattice, @nospecialize(rt), ::BestguessInfo)
    return widenreturn_consts(rt)
end
function widenreturn_noslotwrapper(::ConstsLattice, @nospecialize(rt), ::BestguessInfo)
    return widenreturn_consts(rt)
end
function widenreturn_consts(@nospecialize(rt))
    isa(rt, Const) && return rt
    return widenconst(rt)
end

function widenreturn(::JLTypeLattice, @nospecialize(rt), ::BestguessInfo)
    return widenconst(rt)
end
function widenreturn_noslotwrapper(::JLTypeLattice, @nospecialize(rt), ::BestguessInfo)
    return widenconst(rt)
end

function handle_control_backedge!(interp::AbstractInterpreter, frame::InferenceState, from::Int, to::Int)
    if from > to
        if is_effect_overridden(frame, :terminates_locally)
            # this backedge is known to terminate
        else
            merge_effects!(interp, frame, Effects(EFFECTS_TOTAL; terminates=false))
        end
    end
    return nothing
end

struct BasicStmtChange
    changes::Union{Nothing,StateUpdate}
    type::Any # ::Union{Type, Nothing} - `nothing` if this statement may not be used as an SSA Value
    # TODO effects::Effects
    BasicStmtChange(changes::Union{Nothing,StateUpdate}, @nospecialize type) = new(changes, type)
end

@inline function abstract_eval_basic_statement(interp::AbstractInterpreter,
    @nospecialize(stmt), pc_vartable::VarTable, frame::InferenceState)
    if isa(stmt, NewvarNode)
        changes = StateUpdate(stmt.slot, VarState(Bottom, true), pc_vartable, false)
        return BasicStmtChange(changes, nothing)
    elseif !isa(stmt, Expr)
        t = abstract_eval_statement(interp, stmt, pc_vartable, frame)
        return BasicStmtChange(nothing, t)
    end
    changes = nothing
    stmt = stmt::Expr
    hd = stmt.head
    if hd === :(=)
        t = abstract_eval_statement(interp, stmt.args[2], pc_vartable, frame)
        if t === Bottom
            return BasicStmtChange(nothing, Bottom)
        end
        lhs = stmt.args[1]
        if isa(lhs, SlotNumber)
            changes = StateUpdate(lhs, VarState(t, false), pc_vartable, false)
        elseif isa(lhs, GlobalRef)
            handle_global_assignment!(interp, frame, lhs, t)
        elseif !isa(lhs, SSAValue)
            merge_effects!(interp, frame, EFFECTS_UNKNOWN)
        end
        return BasicStmtChange(changes, t)
    elseif hd === :method
        fname = stmt.args[1]
        if isa(fname, SlotNumber)
            changes = StateUpdate(fname, VarState(Any, false), pc_vartable, false)
        end
        return BasicStmtChange(changes, nothing)
    elseif (hd === :code_coverage_effect || (
            hd !== :boundscheck && # :boundscheck can be narrowed to Bool
            is_meta_expr(stmt)))
        return BasicStmtChange(nothing, Nothing)
    else
        t = abstract_eval_statement(interp, stmt, pc_vartable, frame)
        return BasicStmtChange(nothing, t)
    end
end

function update_bbstate!(𝕃ᵢ::AbstractLattice, frame::InferenceState, bb::Int, vartable::VarTable)
    bbtable = frame.bb_vartables[bb]
    if bbtable === nothing
        # if a basic block hasn't been analyzed yet,
        # we can update its state a bit more aggressively
        frame.bb_vartables[bb] = copy(vartable)
        return true
    else
        return stupdate!(𝕃ᵢ, bbtable, vartable)
    end
end

function init_vartable!(vartable::VarTable, frame::InferenceState)
    nargtypes = length(frame.result.argtypes)
    for i = 1:length(vartable)
        vartable[i] = VarState(Bottom, i > nargtypes)
    end
    return vartable
end

# make as much progress on `frame` as possible (without handling cycles)
function typeinf_local(interp::AbstractInterpreter, frame::InferenceState)
    @assert !is_inferred(frame)
    frame.dont_work_on_me = true # mark that this function is currently on the stack
    W = frame.ip
    nargs = narguments(frame, #=include_va=#false)
    slottypes = frame.slottypes
    ssavaluetypes = frame.ssavaluetypes
    bbs = frame.cfg.blocks
    nbbs = length(bbs)
    𝕃ₚ, 𝕃ᵢ = ipo_lattice(interp), typeinf_lattice(interp)

    currbb = frame.currbb
    if currbb != 1
        currbb = frame.currbb = _bits_findnext(W.bits, 1)::Int # next basic block
    end

    states = frame.bb_vartables
    currstate = copy(states[currbb]::VarTable)
    while currbb <= nbbs
        delete!(W, currbb)
        bbstart = first(bbs[currbb].stmts)
        bbend = last(bbs[currbb].stmts)

        for currpc in bbstart:bbend
            frame.currpc = currpc
            empty_backedges!(frame, currpc)
            stmt = frame.src.code[currpc]
            # If we're at the end of the basic block ...
            if currpc == bbend
                # Handle control flow
                if isa(stmt, GotoNode)
                    succs = bbs[currbb].succs
                    @assert length(succs) == 1
                    nextbb = succs[1]
                    ssavaluetypes[currpc] = Any
                    handle_control_backedge!(interp, frame, currpc, stmt.label)
                    @goto branch
                elseif isa(stmt, GotoIfNot)
                    condx = stmt.cond
                    condt = abstract_eval_value(interp, condx, currstate, frame)
                    if condt === Bottom
                        ssavaluetypes[currpc] = Bottom
                        empty!(frame.pclimitations)
                        @goto find_next_bb
                    end
                    orig_condt = condt
                    if !(isa(condt, Const) || isa(condt, Conditional)) && isa(condx, SlotNumber)
                        # if this non-`Conditional` object is a slot, we form and propagate
                        # the conditional constraint on it
                        condt = Conditional(condx, Const(true), Const(false))
                    end
                    condval = maybe_extract_const_bool(condt)
                    if !isempty(frame.pclimitations)
                        # we can't model the possible effect of control
                        # dependencies on the return
                        # directly to all the return values (unless we error first)
                        condval isa Bool || union!(frame.limitations, frame.pclimitations)
                        empty!(frame.pclimitations)
                    end
                    ssavaluetypes[currpc] = Any
                    if condval === true
                        @goto fallthrough
                    else
                        succs = bbs[currbb].succs
                        if length(succs) == 1
                            @assert condval === false || (stmt.dest === currpc + 1)
                            nextbb = succs[1]
                            @goto branch
                        end
                        @assert length(succs) == 2
                        truebb = currbb + 1
                        falsebb = succs[1] == truebb ? succs[2] : succs[1]
                        if condval === false
                            nextbb = falsebb
                            handle_control_backedge!(interp, frame, currpc, stmt.dest)
                            @goto branch
                        else
                            if !⊑(𝕃ᵢ, orig_condt, Bool)
                                merge_effects!(interp, frame, EFFECTS_THROWS)
                                if !hasintersect(widenconst(orig_condt), Bool)
                                    ssavaluetypes[currpc] = Bottom
                                    @goto find_next_bb
                                end
                            end

                            # We continue with the true branch, but process the false
                            # branch here.
                            if isa(condt, Conditional)
                                else_change = conditional_change(𝕃ᵢ, currstate, condt.elsetype, condt.slot)
                                if else_change !== nothing
                                    false_vartable = stoverwrite1!(copy(currstate), else_change)
                                else
                                    false_vartable = currstate
                                end
                                changed = update_bbstate!(𝕃ᵢ, frame, falsebb, false_vartable)
                                then_change = conditional_change(𝕃ᵢ, currstate, condt.thentype, condt.slot)
                                if then_change !== nothing
                                    stoverwrite1!(currstate, then_change)
                                end
                            else
                                changed = update_bbstate!(𝕃ᵢ, frame, falsebb, currstate)
                            end
                            if changed
                                handle_control_backedge!(interp, frame, currpc, stmt.dest)
                                push!(W, falsebb)
                            end
                            @goto fallthrough
                        end
                    end
                elseif isa(stmt, ReturnNode)
                    bestguess = frame.bestguess
                    rt = abstract_eval_value(interp, stmt.val, currstate, frame)
                    rt = widenreturn(rt, BestguessInfo(interp, bestguess, nargs, slottypes, currstate))
                    # narrow representation of bestguess slightly to prepare for tmerge with rt
                    if rt isa InterConditional && bestguess isa Const
                        let slot_id = rt.slot
                            old_id_type = slottypes[slot_id]
                            if bestguess.val === true && rt.elsetype !== Bottom
                                bestguess = InterConditional(slot_id, old_id_type, Bottom)
                            elseif bestguess.val === false && rt.thentype !== Bottom
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
                    if tchanged(𝕃ₚ, rt, bestguess)
                        # new (wider) return type for frame
                        bestguess = tmerge(𝕃ₚ, bestguess, rt)
                        # TODO: if bestguess isa InterConditional && !interesting(bestguess); bestguess = widenconditional(bestguess); end
                        frame.bestguess = bestguess
                        for (caller, caller_pc) in frame.cycle_backedges
                            if !(caller.ssavaluetypes[caller_pc] === Any)
                                # no reason to revisit if that call-site doesn't affect the final result
                                push!(caller.ip, block_for_inst(caller.cfg, caller_pc))
                            end
                        end
                    end
                    ssavaluetypes[frame.currpc] = Any
                    @goto find_next_bb
                elseif isexpr(stmt, :enter)
                    # Propagate entry info to exception handler
                    l = stmt.args[1]::Int
                    catchbb = block_for_inst(frame.cfg, l)
                    if update_bbstate!(𝕃ᵢ, frame, catchbb, currstate)
                        push!(W, catchbb)
                    end
                    ssavaluetypes[currpc] = Any
                    @goto fallthrough
                end
                # Fall through terminator - treat as regular stmt
            end
            # Process non control-flow statements
            (; changes, type) = abstract_eval_basic_statement(interp,
                stmt, currstate, frame)
            if type === Bottom
                ssavaluetypes[currpc] = Bottom
                @goto find_next_bb
            end
            if changes !== nothing
                stoverwrite1!(currstate, changes)
                let cur_hand = frame.handler_at[currpc], l, enter
                    while cur_hand != 0
                        enter = frame.src.code[cur_hand]::Expr
                        l = enter.args[1]::Int
                        exceptbb = block_for_inst(frame.cfg, l)
                        # propagate new type info to exception handler
                        # the handling for Expr(:enter) propagates all changes from before the try/catch
                        # so this only needs to propagate any changes
                        if stupdate1!(𝕃ᵢ, states[exceptbb]::VarTable, changes)
                            push!(W, exceptbb)
                        end
                        cur_hand = frame.handler_at[cur_hand]
                    end
                end
            end
            if type === nothing
                ssavaluetypes[currpc] = Any
                continue
            end
            if !isempty(frame.ssavalue_uses[currpc])
                record_ssa_assign!(𝕃ᵢ, currpc, type, frame)
            else
                ssavaluetypes[currpc] = type
            end
        end # for currpc in bbstart:bbend

        # Case 1: Fallthrough termination
        begin @label fallthrough
            nextbb = currbb + 1
        end

        # Case 2: Directly branch to a different BB
        begin @label branch
            if update_bbstate!(𝕃ᵢ, frame, nextbb, currstate)
                push!(W, nextbb)
            end
        end

        # Case 3: Control flow ended along the current path (converged, return or throw)
        begin @label find_next_bb
            currbb = frame.currbb = _bits_findnext(W.bits, 1)::Int # next basic block
            currbb == -1 && break # the working set is empty
            currbb > nbbs && break

            nexttable = states[currbb]
            if nexttable === nothing
                init_vartable!(currstate, frame)
            else
                stoverwrite!(currstate, nexttable)
            end
        end
    end # while currbb <= nbbs

    frame.dont_work_on_me = false
    nothing
end

function conditional_change(𝕃ᵢ::AbstractLattice, state::VarTable, @nospecialize(typ), slot::Int)
    vtype = state[slot]
    oldtyp = vtype.typ
    if iskindtype(typ)
        # this code path corresponds to the special handling for `isa(x, iskindtype)` check
        # implemented within `abstract_call_builtin`
    elseif ⊑(𝕃ᵢ, ignorelimited(typ), ignorelimited(oldtyp))
        # approximate test for `typ ∩ oldtyp` being better than `oldtyp`
        # since we probably formed these types with `typesubstract`,
        # the comparison is likely simple
    else
        return nothing
    end
    if oldtyp isa LimitedAccuracy
        # typ is better unlimited, but we may still need to compute the tmeet with the limit
        # "causes" since we ignored those in the comparison
        typ = tmerge(𝕃ᵢ, typ, LimitedAccuracy(Bottom, oldtyp.causes))
    end
    return StateUpdate(SlotNumber(slot), VarState(typ, vtype.undef), state, true)
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
            if !isempty(caller.ip)
                # Note that `typeinf_local(interp, caller)` can potentially modify the other frames
                # `frame.callers_in_cycle`, which is why making incremental progress requires the
                # outer while loop.
                typeinf_local(interp, caller)
                no_active_ips_in_callers = false
            end
            update_valid_age!(caller, frame.valid_worlds)
        end
    end
    return true
end
