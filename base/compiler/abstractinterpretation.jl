# This file is a part of Julia. License is MIT: https://julialang.org/license

struct SlotRefinement
    slot::SlotNumber
    typ::Any
    SlotRefinement(slot::SlotNumber, @nospecialize(typ)) = new(slot, typ)
end

# See if the inference result of the current statement's result value might affect
# the final answer for the method (aside from optimization potential and exceptions).
# To do that, we need to check both for slot assignment and SSA usage.
call_result_unused(sv::InferenceState, currpc::Int) =
    isexpr(sv.src.code[currpc], :call) && isempty(sv.ssavalue_uses[currpc])
call_result_unused(si::StmtInfo) = !si.used

is_const_bool_or_bottom(@nospecialize(b)) = (isa(b, Const) && isa(b.val, Bool)) || b == Bottom
function can_propagate_conditional(@nospecialize(rt), argtypes::Vector{Any})
    isa(rt, InterConditional) || return false
    if rt.slot > length(argtypes)
        # In the vararg tail - can't be conditional
        @assert isvarargtype(argtypes[end])
        return false
    end
    return isa(argtypes[rt.slot], Conditional) &&
        is_const_bool_or_bottom(rt.thentype) && is_const_bool_or_bottom(rt.thentype)
end

function propagate_conditional(rt::InterConditional, cond::Conditional)
    new_thentype = rt.thentype === Const(false) ? cond.elsetype : cond.thentype
    new_elsetype = rt.elsetype === Const(true) ? cond.thentype : cond.elsetype
    if rt.thentype == Bottom
        @assert rt.elsetype != Bottom
        return Conditional(cond.slot, Bottom, new_elsetype)
    elseif rt.elsetype == Bottom
        @assert rt.thentype != Bottom
        return Conditional(cond.slot, new_thentype, Bottom)
    end
    return Conditional(cond.slot, new_thentype, new_elsetype)
end

function abstract_call_gf_by_type(interp::AbstractInterpreter, @nospecialize(f),
                                  arginfo::ArgInfo, si::StmtInfo, @nospecialize(atype),
                                  sv::AbsIntState, max_methods::Int)
    𝕃ₚ, 𝕃ᵢ = ipo_lattice(interp), typeinf_lattice(interp)
    ⊑ₚ, ⋤ₚ, ⊔ₚ, ⊔ᵢ  = partialorder(𝕃ₚ), strictneqpartialorder(𝕃ₚ), join(𝕃ₚ), join(𝕃ᵢ)
    argtypes = arginfo.argtypes
    matches = find_method_matches(interp, argtypes, atype; max_methods)
    if isa(matches, FailedMethodMatch)
        add_remark!(interp, sv, matches.reason)
        return Future(CallMeta(Any, Any, Effects(), NoCallInfo()))
    end

    (; valid_worlds, applicable, info) = matches
    update_valid_age!(sv, valid_worlds)

    # final result
    gfresult = Future{CallMeta}()
    # intermediate work for computing gfresult
    rettype = exctype = Bottom
    edges = MethodInstance[]
    conditionals = nothing # keeps refinement information of call argument types when the return type is boolean
    seenall = true
    const_results = nothing # or const_results::Vector{Union{Nothing,ConstResult}} if any const results are available
    fargs = arginfo.fargs
    all_effects = EFFECTS_TOTAL
    slotrefinements = nothing # keeps refinement information on slot types obtained from call signature

    # split the for loop off into a function, so that we can pause and restart it at will
    i::Int = 1
    f = Core.Box(f)
    atype = Core.Box(atype)
    function infercalls(interp, sv)
        napplicable = length(applicable)
        multiple_matches = napplicable > 1
        while i <= napplicable
            match = applicable[i]::MethodMatch
            method = match.method
            sig = match.spec_types
            if bail_out_toplevel_call(interp, InferenceLoopState(sig, rettype, all_effects), sv)
                # only infer concrete call sites in top-level expressions
                add_remark!(interp, sv, "Refusing to infer non-concrete call site in top-level expression")
                seenall = false
                break
            end
            # TODO: this is unmaintained now as it didn't seem to improve things, though it does avoid hard-coding the union split at the higher level,
            # it also can hurt infer-ability of some constrained parameter types (e.g. quacks like a duck)
            # sigtuple = unwrap_unionall(sig)::DataType
            # splitunions = 1 < unionsplitcost(sigtuple.parameters) * napplicable <= InferenceParams(interp).max_union_splitting
            #if splitunions
            #    splitsigs = switchtupleunion(sig)
            #    for sig_n in splitsigs
            #        result = abstract_call_method(interp, method, sig_n, svec(), multiple_matches, si, sv)::Future
            #        handle1(...)
            #    end
            #end
            mresult = abstract_call_method(interp, method, sig, match.sparams, multiple_matches, si, sv)::Future
            function handle1(interp, sv)
                local (; rt, exct, edge, effects, volatile_inf_result) = mresult[]
                this_conditional = ignorelimited(rt)
                this_rt = widenwrappedconditional(rt)
                this_exct = exct
                # try constant propagation with argtypes for this match
                # this is in preparation for inlining, or improving the return result
                this_argtypes = isa(matches, MethodMatches) ? argtypes : matches.applicable_argtypes[i]
                this_arginfo = ArgInfo(fargs, this_argtypes)
                const_call_result = abstract_call_method_with_const_args(interp,
                    mresult[], f.contents, this_arginfo, si, match, sv)
                const_result = volatile_inf_result
                if const_call_result !== nothing
                    this_const_conditional = ignorelimited(const_call_result.rt)
                    this_const_rt = widenwrappedconditional(const_call_result.rt)
                    if this_const_rt ⊑ₚ this_rt
                        # As long as the const-prop result we have is not *worse* than
                        # what we found out on types, we'd like to use it. Even if the
                        # end result is exactly equivalent, it is likely that the IR
                        # we produced while constproping is better than that with
                        # generic types.
                        # Return type of const-prop' inference can be wider than that of non const-prop' inference
                        # e.g. in cases when there are cycles but cached result is still accurate
                        this_conditional = this_const_conditional
                        this_rt = this_const_rt
                        (; effects, const_result, edge) = const_call_result
                    elseif is_better_effects(const_call_result.effects, effects)
                        (; effects, const_result, edge) = const_call_result
                    else
                        add_remark!(interp, sv, "[constprop] Discarded because the result was wider than inference")
                    end
                    # Treat the exception type separately. Currently, constprop often cannot determine the exception type
                    # because consistent-cy does not apply to exceptions.
                    if const_call_result.exct ⋤ this_exct
                        this_exct = const_call_result.exct
                        (; const_result, edge) = const_call_result
                    else
                        add_remark!(interp, sv, "[constprop] Discarded exception type because result was wider than inference")
                    end
                end

                all_effects = merge_effects(all_effects, effects)
                if const_result !== nothing
                    if const_results === nothing
                        const_results = fill!(Vector{Union{Nothing,ConstResult}}(undef, napplicable), nothing)
                    end
                    const_results[i] = const_result
                end
                edge === nothing || push!(edges, edge)
                @assert !(this_conditional isa Conditional || this_rt isa MustAlias) "invalid lattice element returned from inter-procedural context"
                if can_propagate_conditional(this_conditional, argtypes)
                    # The only case where we need to keep this in rt is where
                    # we can directly propagate the conditional to a slot argument
                    # that is not one of our arguments, otherwise we keep all the
                    # relevant information in `conditionals` below.
                    this_rt = this_conditional
                end

                rettype = rettype ⊔ₚ this_rt
                exctype = exctype ⊔ₚ this_exct
                if has_conditional(𝕃ₚ, sv) && this_conditional !== Bottom && is_lattice_bool(𝕃ₚ, rettype) && fargs !== nothing
                    if conditionals === nothing
                        conditionals = Any[Bottom for _ in 1:length(argtypes)],
                                       Any[Bottom for _ in 1:length(argtypes)]
                    end
                    for i = 1:length(argtypes)
                        cnd = conditional_argtype(𝕃ᵢ, this_conditional, sig, argtypes, i)
                        conditionals[1][i] = conditionals[1][i] ⊔ᵢ cnd.thentype
                        conditionals[2][i] = conditionals[2][i] ⊔ᵢ cnd.elsetype
                    end
                end
                if i < napplicable && bail_out_call(interp, InferenceLoopState(sig, rettype, all_effects), sv)
                    add_remark!(interp, sv, "Call inference reached maximally imprecise information. Bailing on.")
                    seenall = false
                    i = napplicable # break in outer function
                end
                i += 1
                return true
            end
            if isready(mresult) && handle1(interp, sv)
                continue
            else
                push!(sv.tasks, handle1)
                return false
            end
        end # while

        if const_results !== nothing
            @assert napplicable == nmatches(info) == length(const_results)
            info = ConstCallInfo(info, const_results)
        end

        if seenall
            if !fully_covering(matches) || any_ambig(matches)
                # Account for the fact that we may encounter a MethodError with a non-covered or ambiguous signature.
                all_effects = Effects(all_effects; nothrow=false)
                exctype = exctype ⊔ₚ MethodError
            end
            if sv isa InferenceState && fargs !== nothing
                slotrefinements = collect_slot_refinements(𝕃ᵢ, applicable, argtypes, fargs, sv)
            end
        else
            # there is unanalyzed candidate, widen type and effects to the top
            rettype = exctype = Any
            all_effects = Effects()
        end

        rettype = from_interprocedural!(interp, rettype, sv, arginfo, conditionals)

        # Also considering inferring the compilation signature for this method, so
        # it is available to the compiler in case it ends up needing it.
        if (isa(sv, InferenceState) && infer_compilation_signature(interp) &&
            (seenall && 1 == napplicable) && rettype !== Any && rettype !== Bottom &&
            !is_removable_if_unused(all_effects))
            match = applicable[1]::MethodMatch
            method = match.method
            sig = match.spec_types
            mi = specialize_method(match; preexisting=true)
            if mi !== nothing && !const_prop_methodinstance_heuristic(interp, mi, arginfo, sv)
                csig = get_compileable_sig(method, sig, match.sparams)
                if csig !== nothing && csig !== sig
                    abstract_call_method(interp, method, csig, match.sparams, multiple_matches, StmtInfo(false), sv)::Future
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
        any_slot_refined = slotrefinements !== nothing
        add_call_backedges!(interp, rettype, all_effects, any_slot_refined, edges, matches, atype.contents, sv)
        if isa(sv, InferenceState)
            # TODO (#48913) implement a proper recursion handling for irinterp:
            # This works just because currently the `:terminate` condition guarantees that
            # irinterp doesn't fail into unresolved cycles, but it's not a good solution.
            # We should revisit this once we have a better story for handling cycles in irinterp.
            if !isempty(sv.pclimitations) # remove self, if present
                delete!(sv.pclimitations, sv)
                for caller in callers_in_cycle(sv)
                    delete!(sv.pclimitations, caller)
                end
            end
        end

        gfresult[] = CallMeta(rettype, exctype, all_effects, info, slotrefinements)
        return true
    end # infercalls
    # start making progress on the first call
    infercalls(interp, sv) || push!(sv.tasks, infercalls)
    return gfresult
end

struct FailedMethodMatch
    reason::String
end

struct MethodMatches
    applicable::Vector{Any}
    info::MethodMatchInfo
    valid_worlds::WorldRange
end
any_ambig(result::MethodLookupResult) = result.ambig
any_ambig(info::MethodMatchInfo) = any_ambig(info.results)
any_ambig(m::MethodMatches) = any_ambig(m.info)
fully_covering(info::MethodMatchInfo) = info.fullmatch
fully_covering(m::MethodMatches) = fully_covering(m.info)
function add_uncovered_edges!(sv::AbsIntState, info::MethodMatchInfo, @nospecialize(atype))
    fully_covering(info) || add_mt_backedge!(sv, info.mt, atype)
    nothing
end
add_uncovered_edges!(sv::AbsIntState, matches::MethodMatches, @nospecialize(atype)) =
    add_uncovered_edges!(sv, matches.info, atype)

struct UnionSplitMethodMatches
    applicable::Vector{Any}
    applicable_argtypes::Vector{Vector{Any}}
    info::UnionSplitInfo
    valid_worlds::WorldRange
end
any_ambig(info::UnionSplitInfo) = any(any_ambig, info.split)
any_ambig(m::UnionSplitMethodMatches) = any_ambig(m.info)
fully_covering(info::UnionSplitInfo) = all(fully_covering, info.split)
fully_covering(m::UnionSplitMethodMatches) = fully_covering(m.info)
function add_uncovered_edges!(sv::AbsIntState, info::UnionSplitInfo, @nospecialize(atype))
    all(fully_covering, info.split) && return nothing
    # add mt backedges with removing duplications
    for mt in uncovered_method_tables(info)
        add_mt_backedge!(sv, mt, atype)
    end
end
add_uncovered_edges!(sv::AbsIntState, matches::UnionSplitMethodMatches, @nospecialize(atype)) =
    add_uncovered_edges!(sv, matches.info, atype)
function uncovered_method_tables(info::UnionSplitInfo)
    mts = MethodTable[]
    for mminfo in info.split
        fully_covering(mminfo) && continue
        any(mt′::MethodTable->mt′===mminfo.mt, mts) && continue
        push!(mts, mminfo.mt)
    end
    return mts
end

function find_method_matches(interp::AbstractInterpreter, argtypes::Vector{Any}, @nospecialize(atype);
                             max_union_splitting::Int = InferenceParams(interp).max_union_splitting,
                             max_methods::Int = InferenceParams(interp).max_methods)
    if is_union_split_eligible(typeinf_lattice(interp), argtypes, max_union_splitting)
        return find_union_split_method_matches(interp, argtypes, atype, max_methods)
    end
    return find_simple_method_matches(interp, atype, max_methods)
end

# NOTE this is valid as far as any "constant" lattice element doesn't represent `Union` type
is_union_split_eligible(𝕃::AbstractLattice, argtypes::Vector{Any}, max_union_splitting::Int) =
    1 < unionsplitcost(𝕃, argtypes) <= max_union_splitting

function find_union_split_method_matches(interp::AbstractInterpreter, argtypes::Vector{Any},
                                         @nospecialize(atype), max_methods::Int)
    split_argtypes = switchtupleunion(typeinf_lattice(interp), argtypes)
    infos = MethodMatchInfo[]
    applicable = Any[]
    applicable_argtypes = Vector{Any}[] # arrays like `argtypes`, including constants, for each match
    valid_worlds = WorldRange()
    for i in 1:length(split_argtypes)
        arg_n = split_argtypes[i]::Vector{Any}
        sig_n = argtypes_to_type(arg_n)
        mt = ccall(:jl_method_table_for, Any, (Any,), sig_n)
        mt === nothing && return FailedMethodMatch("Could not identify method table for call")
        mt = mt::MethodTable
        thismatches = findall(sig_n, method_table(interp); limit = max_methods)
        if thismatches === nothing
            return FailedMethodMatch("For one of the union split cases, too many methods matched")
        end
        for m in thismatches
            push!(applicable, m)
            push!(applicable_argtypes, arg_n)
        end
        valid_worlds = intersect(valid_worlds, thismatches.valid_worlds)
        thisfullmatch = any(match::MethodMatch->match.fully_covers, thismatches)
        thisinfo = MethodMatchInfo(thismatches, mt, thisfullmatch)
        push!(infos, thisinfo)
    end
    info = UnionSplitInfo(infos)
    return UnionSplitMethodMatches(
        applicable, applicable_argtypes, info, valid_worlds)
end

function find_simple_method_matches(interp::AbstractInterpreter, @nospecialize(atype), max_methods::Int)
    mt = ccall(:jl_method_table_for, Any, (Any,), atype)
    if mt === nothing
        return FailedMethodMatch("Could not identify method table for call")
    end
    mt = mt::MethodTable
    matches = findall(atype, method_table(interp); limit = max_methods)
    if matches === nothing
        # this means too many methods matched
        # (assume this will always be true, so we don't compute / update valid age in this case)
        return FailedMethodMatch("Too many methods matched")
    end
    fullmatch = any(match::MethodMatch->match.fully_covers, matches)
    info = MethodMatchInfo(matches, mt, fullmatch)
    return MethodMatches(matches.matches, info, matches.valid_worlds)
end

"""
    from_interprocedural!(interp::AbstractInterpreter, rt, sv::AbsIntState,
                          arginfo::ArgInfo, maybecondinfo) -> newrt

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
function from_interprocedural!(interp::AbstractInterpreter, @nospecialize(rt), sv::AbsIntState,
                               arginfo::ArgInfo, @nospecialize(maybecondinfo))
    rt = collect_limitations!(rt, sv)
    if isa(rt, InterMustAlias)
        rt = from_intermustalias(typeinf_lattice(interp), rt, arginfo, sv)
    elseif is_lattice_bool(ipo_lattice(interp), rt)
        if maybecondinfo === nothing
            rt = widenconditional(rt)
        else
            rt = from_interconditional(typeinf_lattice(interp), rt, sv, arginfo, maybecondinfo)
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

function from_intermustalias(𝕃ᵢ::AbstractLattice, rt::InterMustAlias, arginfo::ArgInfo, sv::AbsIntState)
    fargs = arginfo.fargs
    if fargs !== nothing && 1 ≤ rt.slot ≤ length(fargs)
        arg = ssa_def_slot(fargs[rt.slot], sv)
        if isa(arg, SlotNumber)
            argtyp = widenslotwrapper(arginfo.argtypes[rt.slot])
            ⊑ = partialorder(𝕃ᵢ)
            if rt.vartyp ⊑ argtyp
                return MustAlias(arg, rt.vartyp, rt.fldidx, rt.fldtyp)
            else
                # TODO optimize this case?
            end
        end
    end
    return widenmustalias(rt)
end

function from_interconditional(𝕃ᵢ::AbstractLattice, @nospecialize(rt), sv::AbsIntState,
                               arginfo::ArgInfo, @nospecialize(maybecondinfo))
    has_conditional(𝕃ᵢ, sv) || return widenconditional(rt)
    (; fargs, argtypes) = arginfo
    fargs === nothing && return widenconditional(rt)
    if can_propagate_conditional(rt, argtypes)
        return propagate_conditional(rt, argtypes[rt.slot]::Conditional)
    end
    slot = 0
    alias = nothing
    thentype = elsetype = Any
    condval = maybe_extract_const_bool(rt)
    ⊑, ⋤, ⊓ = partialorder(𝕃ᵢ), strictneqpartialorder(𝕃ᵢ), meet(𝕃ᵢ)
    for i in 1:length(fargs)
        # find the first argument which supports refinement,
        # and intersect all equivalent arguments with it
        argtyp = argtypes[i]
        if alias === nothing
            arg = ssa_def_slot(fargs[i], sv)
            if isa(arg, SlotNumber) && widenslotwrapper(argtyp) isa Type
                old = argtyp
                id = slot_id(arg)
            elseif argtyp isa MustAlias
                old = argtyp.fldtyp
                id = argtyp.slot
            else
                continue # unlikely to refine
            end
        elseif argtyp isa MustAlias && issubalias(argtyp, alias)
            arg = nothing
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
                cnd = conditional_argtype(𝕃ᵢ, rt, maybecondinfo, argtypes, i)
                new_thentype = cnd.thentype
                new_elsetype = cnd.elsetype
            end
            if condval === false
                thentype = Bottom
            elseif new_thentype ⊑ thentype
                thentype = new_thentype
            else
                thentype = thentype ⊓ widenconst(new_thentype)
            end
            if condval === true
                elsetype = Bottom
            elseif new_elsetype ⊑ elsetype
                elsetype = new_elsetype
            else
                elsetype = elsetype ⊓ widenconst(new_elsetype)
            end
            if (slot > 0 || condval !== false) && thentype ⋤ old
                slot = id
                if !(arg isa SlotNumber) && argtyp isa MustAlias
                    alias = argtyp
                end
            elseif (slot > 0 || condval !== true) && elsetype ⋤ old
                slot = id
                if !(arg isa SlotNumber) && argtyp isa MustAlias
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
    return widenconditional(rt)
end

function conditional_argtype(𝕃ᵢ::AbstractLattice, @nospecialize(rt), @nospecialize(sig),
                             argtypes::Vector{Any}, i::Int)
    if isa(rt, InterConditional) && rt.slot == i
        return rt
    else
        argt = widenslotwrapper(argtypes[i])
        if isvarargtype(argt)
            @assert fieldcount(sig) == i
            argt = unwrapva(argt)
        end
        thentype = elsetype = tmeet(𝕃ᵢ, argt, fieldtype(sig, i))
        condval = maybe_extract_const_bool(rt)
        condval === true && (elsetype = Bottom)
        condval === false && (thentype = Bottom)
        return InterConditional(i, thentype, elsetype)
    end
end

function collect_slot_refinements(𝕃ᵢ::AbstractLattice, applicable::Vector{Any},
    argtypes::Vector{Any}, fargs::Vector{Any}, sv::InferenceState)
    ⊏, ⊔ = strictpartialorder(𝕃ᵢ), join(𝕃ᵢ)
    slotrefinements = nothing
    for i = 1:length(fargs)
        fargᵢ = fargs[i]
        if fargᵢ isa SlotNumber
            fidx = slot_id(fargᵢ)
            argt = widenslotwrapper(argtypes[i])
            if isvarargtype(argt)
                argt = unwrapva(argt)
            end
            sigt = Bottom
            for j = 1:length(applicable)
                match = applicable[j]::MethodMatch
                sigt = sigt ⊔ fieldtype(match.spec_types, i)
            end
            if sigt ⊏ argt # i.e. signature type is strictly more specific than the type of the argument slot
                if slotrefinements === nothing
                    slotrefinements = fill!(Vector{Any}(undef, length(sv.slottypes)), nothing)
                end
                slotrefinements[fidx] = sigt
            end
        end
    end
    return slotrefinements
end

function add_call_backedges!(interp::AbstractInterpreter, @nospecialize(rettype),
    all_effects::Effects, any_slot_refined::Bool, edges::Vector{MethodInstance},
    matches::Union{MethodMatches,UnionSplitMethodMatches}, @nospecialize(atype),
    sv::AbsIntState)
    # don't bother to add backedges when both type and effects information are already
    # maximized to the top since a new method couldn't refine or widen them anyway
    if rettype === Any
        # ignore the `:nonoverlayed` property if `interp` doesn't use overlayed method table
        # since it will never be tainted anyway
        if !isoverlayed(method_table(interp))
            all_effects = Effects(all_effects; nonoverlayed=ALWAYS_FALSE)
        end
        if all_effects === Effects() && !any_slot_refined
            return nothing
        end
    end
    for edge in edges
        add_backedge!(sv, edge)
    end
    # also need an edge to the method table in case something gets
    # added that did not intersect with any existing method
    add_uncovered_edges!(sv, matches, atype)
    return nothing
end

const RECURSION_UNUSED_MSG = "Bounded recursion detected with unused result. Annotated return type may be wider than true result."
const RECURSION_MSG = "Bounded recursion detected. Call was widened to force convergence."
const RECURSION_MSG_HARDLIMIT = "Bounded recursion detected under hardlimit. Call was widened to force convergence."

function abstract_call_method(interp::AbstractInterpreter,
                              method::Method, @nospecialize(sig), sparams::SimpleVector,
                              hardlimit::Bool, si::StmtInfo, sv::AbsIntState)
    sigtuple = unwrap_unionall(sig)
    sigtuple isa DataType ||
        return Future(MethodCallResult(Any, Any, false, false, nothing, Effects()))
    all(@nospecialize(x) -> valid_as_lattice(unwrapva(x), true), sigtuple.parameters) ||
        return Future(MethodCallResult(Union{}, Any, false, false, nothing, EFFECTS_THROWS)) # catch bad type intersections early

    if is_nospecializeinfer(method)
        sig = get_nospecializeinfer_sig(method, sig, sparams)
    end

    # Limit argument type tuple growth of functions:
    # look through the parents list to see if there's a call to the same method
    # and from the same method.
    # Returns the topmost occurrence of that repeated edge.
    edgecycle = edgelimited = false
    topmost = nothing

    for sv′ in AbsIntStackUnwind(sv)
        infmi = frame_instance(sv′)
        if method === infmi.def
            if infmi.specTypes::Type == sig::Type
                # avoid widening when detecting self-recursion
                # TODO: merge call cycle and return right away
                if call_result_unused(si)
                    add_remark!(interp, sv, RECURSION_UNUSED_MSG)
                    # since we don't use the result (typically),
                    # we have a self-cycle in the call-graph, but not in the inference graph (typically):
                    # break this edge now (before we record it) by returning early
                    # (non-typically, this means that we lose the ability to detect a guaranteed StackOverflow in some cases)
                    return Future(MethodCallResult(Any, Any, true, true, nothing, Effects()))
                end
                topmost = nothing
                edgecycle = true
                break
            end
            topmost === nothing || continue
            if edge_matches_sv(interp, sv′, method, sig, sparams, hardlimit, sv)
                topmost = sv′
                edgecycle = true
            end
        end
    end
    washardlimit = hardlimit

    if topmost !== nothing
        msig = unwrap_unionall(method.sig)::DataType
        spec_len = length(msig.parameters) + 1
        mi = frame_instance(sv)

        if isdefined(method, :recursion_relation)
            # We don't require the recursion_relation to be transitive, so
            # apply a hard limit
            hardlimit = true
        end

        if method === mi.def
            # Under direct self-recursion, permit much greater use of reducers.
            # here we assume that complexity(specTypes) :>= complexity(sig)
            comparison = mi.specTypes
            l_comparison = length((unwrap_unionall(comparison)::DataType).parameters)
            spec_len = max(spec_len, l_comparison)
        elseif !hardlimit && isa(topmost, InferenceState)
            # Without a hardlimit, permit use of reducers too.
            comparison = frame_instance(topmost).specTypes
            # n.b. currently don't allow vararg reducers
            #l_comparison = length((unwrap_unionall(comparison)::DataType).parameters)
            #spec_len = max(spec_len, l_comparison)
        else
            comparison = method.sig
        end

        # see if the type is actually too big (relative to the caller), and limit it if required
        newsig = limit_type_size(sig, comparison, hardlimit ? comparison : mi.specTypes, InferenceParams(interp).tuple_complexity_limit_depth, spec_len)

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
                return Future(MethodCallResult(Any, Any, true, true, nothing, Effects()))
            end
            add_remark!(interp, sv, washardlimit ? RECURSION_MSG_HARDLIMIT : RECURSION_MSG)
            # TODO (#48913) implement a proper recursion handling for irinterp:
            # This works just because currently the `:terminate` condition usually means this is unreachable here
            # for irinterp because there are not unresolved cycles, but it's not a good solution.
            # We should revisit this once we have a better story for handling cycles in irinterp.
            if isa(sv, InferenceState)
                # since the hardlimit is against the edge to the parent frame,
                # we should try to poison the whole edge, not just the topmost frame
                parentframe = frame_parent(topmost)
                while !isa(parentframe, InferenceState)
                    # attempt to find a parent frame that can handle this LimitedAccuracy result correctly
                    # so we don't try to cache this incomplete intermediate result
                    parentframe === nothing && break
                    parentframe = frame_parent(parentframe)
                end
                if isa(parentframe, InferenceState)
                    poison_callstack!(sv, parentframe)
                elseif isa(topmost, InferenceState)
                    poison_callstack!(sv, topmost)
                end
            end
            # n.b. this heuristic depends on the non-local state, so we must record the limit later
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

    return typeinf_edge(interp, method, sig, sparams, sv, edgecycle, edgelimited)
end

function edge_matches_sv(interp::AbstractInterpreter, frame::AbsIntState,
                         method::Method, @nospecialize(sig), sparams::SimpleVector,
                         hardlimit::Bool, sv::AbsIntState)
    # The `method_for_inference_heuristics` will expand the given method's generator if
    # necessary in order to retrieve this field from the generated `CodeInfo`, if it exists.
    # The other `CodeInfo`s we inspect will already have this field inflated, so we just
    # access it directly instead (to avoid regeneration).
    world = get_inference_world(interp)
    callee_method2 = method_for_inference_heuristics(method, sig, sparams, world)
    inf_method2 = method_for_inference_limit_heuristics(frame)
    if callee_method2 !== inf_method2 # limit only if user token match
        return false
    end
    if isa(frame, InferenceState) && cache_owner(frame.interp) !== cache_owner(interp)
        # Don't assume that frames in different interpreters are the same
        return false
    end
    if !hardlimit || InferenceParams(interp).ignore_recursion_hardlimit
        # if this is a soft limit,
        # also inspect the parent of this edge,
        # to see if they are the same Method as sv
        # in which case we'll need to ensure it is convergent
        # otherwise, we don't

        # check in the cycle list first
        # all items in here are considered mutual parents of all others
        if !any(p::AbsIntState->matches_sv(p, sv), callers_in_cycle(frame))
            let parent = frame_parent(frame)
                parent === nothing && return false
                (is_cached(parent) || frame_parent(parent) !== nothing) || return false
                matches_sv(parent, sv) || return false
            end
        end

        # If the method defines a recursion relation, give it a chance
        # to tell us that this recursion is actually ok.
        if isdefined(method, :recursion_relation)
            if Core._apply_pure(method.recursion_relation, Any[method, callee_method2, sig, frame_instance(frame).specTypes])
                return false
            end
        end
    end
    return true
end

# This function is used for computing alternate limit heuristics
function method_for_inference_heuristics(method::Method, @nospecialize(sig), sparams::SimpleVector, world::UInt)
    if (hasgenerator(method) && !(method.generator isa Core.GeneratedFunctionStub) &&
        may_invoke_generator(method, sig, sparams))
        mi = specialize_method(method, sig, sparams)
        cinfo = get_staged(mi, world)
        if isa(cinfo, CodeInfo)
            method2 = cinfo.method_for_inference_limit_heuristics
            if method2 isa Method
                return method2
            end
        end
    end
    return nothing
end

function matches_sv(parent::AbsIntState, sv::AbsIntState)
    # limit only if user token match
    return (frame_instance(parent).def === frame_instance(sv).def &&
            method_for_inference_limit_heuristics(sv) === method_for_inference_limit_heuristics(parent))
end

function is_edge_recursed(edge::MethodInstance, caller::AbsIntState)
    return any(AbsIntStackUnwind(caller)) do sv::AbsIntState
        return edge === frame_instance(sv)
    end
end

function is_method_recursed(method::Method, caller::AbsIntState)
    return any(AbsIntStackUnwind(caller)) do sv::AbsIntState
        return method === frame_instance(sv).def
    end
end

function is_constprop_edge_recursed(edge::MethodInstance, caller::AbsIntState)
    return any(AbsIntStackUnwind(caller)) do sv::AbsIntState
        return edge === frame_instance(sv) && is_constproped(sv)
    end
end

function is_constprop_method_recursed(method::Method, caller::AbsIntState)
    return any(AbsIntStackUnwind(caller)) do sv::AbsIntState
        return method === frame_instance(sv).def && is_constproped(sv)
    end
end

# keeps result and context information of abstract_method_call, which will later be used for
# backedge computation, and concrete evaluation or constant-propagation
struct MethodCallResult
    rt
    exct
    edgecycle::Bool
    edgelimited::Bool
    edge::Union{Nothing,MethodInstance}
    effects::Effects
    volatile_inf_result::Union{Nothing,VolatileInferenceResult}
    function MethodCallResult(@nospecialize(rt), @nospecialize(exct),
                              edgecycle::Bool,
                              edgelimited::Bool,
                              edge::Union{Nothing,MethodInstance},
                              effects::Effects,
                              volatile_inf_result::Union{Nothing,VolatileInferenceResult}=nothing)
        return new(rt, exct, edgecycle, edgelimited, edge, effects, volatile_inf_result)
    end
end

struct InvokeCall
    types     # ::Type
    lookupsig # ::Type
    InvokeCall(@nospecialize(types), @nospecialize(lookupsig)) = new(types, lookupsig)
end

struct ConstCallResults
    rt::Any
    exct::Any
    const_result::ConstResult
    effects::Effects
    edge::MethodInstance
    function ConstCallResults(
        @nospecialize(rt), @nospecialize(exct),
        const_result::ConstResult,
        effects::Effects,
        edge::MethodInstance)
        return new(rt, exct, const_result, effects, edge)
    end
end

function abstract_call_method_with_const_args(interp::AbstractInterpreter,
    result::MethodCallResult, @nospecialize(f), arginfo::ArgInfo, si::StmtInfo,
    match::MethodMatch, sv::AbsIntState, invokecall::Union{Nothing,InvokeCall}=nothing)
    if bail_out_const_call(interp, result, si, match, sv)
        return nothing
    end
    eligibility = concrete_eval_eligible(interp, f, result, arginfo, sv)
    concrete_eval_result = nothing
    if eligibility === :concrete_eval
        concrete_eval_result = concrete_eval_call(interp, f, result, arginfo, sv, invokecall)
        # if we don't inline the result of this concrete evaluation,
        # give const-prop' a chance to inline a better method body
        if !may_optimize(interp) || (
            may_inline_concrete_result(concrete_eval_result.const_result::ConcreteResult) ||
            concrete_eval_result.rt === Bottom) # unless this call deterministically throws and thus is non-inlineable
            return concrete_eval_result
        end
        # TODO allow semi-concrete interp for this call?
    end
    mi = maybe_get_const_prop_profitable(interp, result, f, arginfo, si, match, sv)
    mi === nothing && return concrete_eval_result
    if is_constprop_recursed(result, mi, sv)
        add_remark!(interp, sv, "[constprop] Edge cycle encountered")
        return nothing
    end
    # try semi-concrete evaluation
    if eligibility === :semi_concrete_eval
        irinterp_result = semi_concrete_eval_call(interp, mi, result, arginfo, sv)
        if irinterp_result !== nothing
            return irinterp_result
        end
    end
    # try constant prop'
    return const_prop_call(interp, mi, result, arginfo, sv, concrete_eval_result)
end

function bail_out_const_call(interp::AbstractInterpreter, result::MethodCallResult,
                             si::StmtInfo, match::MethodMatch, sv::AbsIntState)
    if !InferenceParams(interp).ipo_constant_propagation
        add_remark!(interp, sv, "[constprop] Disabled by parameter")
        return true
    end
    if is_no_constprop(match.method)
        add_remark!(interp, sv, "[constprop] Disabled by method parameter")
        return true
    end
    if is_removable_if_unused(result.effects)
        if isa(result.rt, Const)
            add_remark!(interp, sv, "[constprop] No more information to be gained (const)")
            return true
        elseif call_result_unused(si)
            add_remark!(interp, sv, "[constprop] No more information to be gained (unused result)")
            return true
        end
    end
    if result.rt === Bottom
        if is_terminates(result.effects) && is_effect_free(result.effects)
            # In the future, we may want to add `&& isa(result.exct, Const)` to
            # the list of conditions here, but currently, our effect system isn't
            # precise enough to let us determine :consistency of `exct`, so we
            # would have to force constprop just to determine this, which is too
            # expensive.
            add_remark!(interp, sv, "[constprop] No more information to be gained (bottom)")
            return true
        end
    end
    return false
end

function concrete_eval_eligible(interp::AbstractInterpreter,
    @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo, sv::AbsIntState)
    (;effects) = result
    if inbounds_option() === :off
        if !is_nothrow(effects)
            # Disable concrete evaluation in `--check-bounds=no` mode,
            # unless it is known to not throw.
            return :none
        end
    end
    mi = result.edge
    if mi !== nothing && is_foldable(effects, #=check_rtcall=#true)
        if f !== nothing && is_all_const_arg(arginfo, #=start=#2)
            if (is_nonoverlayed(interp) || is_nonoverlayed(effects) ||
                # Even if overlay methods are involved, when `:consistent_overlay` is
                # explicitly applied, we can still perform concrete evaluation using the
                # original methods for executing them.
                # While there's a chance that the non-overlayed counterparts may raise
                # non-egal exceptions, it will not impact the compilation validity, since:
                # - the results of the concrete evaluation will not be inlined
                # - the exception types from the concrete evaluation will not be propagated
                is_consistent_overlay(effects))
                return :concrete_eval
            end
            # disable concrete-evaluation if this function call is tainted by some overlayed
            # method since currently there is no easy way to execute overlayed methods
            add_remark!(interp, sv, "[constprop] Concrete eval disabled for overlayed methods")
        end
        if !any_conditional(arginfo)
            if may_optimize(interp)
                return :semi_concrete_eval
            else
                # disable irinterp if optimization is disabled, since it requires optimized IR
                add_remark!(interp, sv, "[constprop] Semi-concrete interpretation disabled for non-optimizing interpreter")
            end
        end
    end
    return :none
end

is_all_const_arg(arginfo::ArgInfo, start::Int) = is_all_const_arg(arginfo.argtypes, start::Int)
function is_all_const_arg(argtypes::Vector{Any}, start::Int)
    for i = start:length(argtypes)
        argtype = widenslotwrapper(argtypes[i])
        is_const_argtype(argtype) || return false
    end
    return true
end

is_const_argtype(@nospecialize argtype) = isa(argtype, Const) || isconstType(argtype) || issingletontype(argtype)

any_conditional(argtypes::Vector{Any}) = any(@nospecialize(x)->isa(x, Conditional), argtypes)
any_conditional(arginfo::ArgInfo) = any_conditional(arginfo.argtypes)

collect_const_args(arginfo::ArgInfo, start::Int) = collect_const_args(arginfo.argtypes, start)
function collect_const_args(argtypes::Vector{Any}, start::Int)
    return Any[ let a = widenslotwrapper(argtypes[i])
                    isa(a, Const) ? a.val :
                    isconstType(a) ? a.parameters[1] :
                    (a::DataType).instance
                end for i = start:length(argtypes) ]
end

function concrete_eval_call(interp::AbstractInterpreter,
    @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo, sv::AbsIntState,
    invokecall::Union{InvokeCall,Nothing}=nothing)
    args = collect_const_args(arginfo, #=start=#2)
    if invokecall !== nothing
        # this call should be `invoke`d, rewrite `args` back now
        pushfirst!(args, f, invokecall.types)
        f = invoke
    end
    world = get_inference_world(interp)
    edge = result.edge::MethodInstance
    value = try
        Core._call_in_world_total(world, f, args...)
    catch e
        # The evaluation threw. By :consistent-cy, we're guaranteed this would have happened at runtime.
        # Howevever, at present, :consistency does not mandate the type of the exception
        return ConstCallResults(Bottom, Any, ConcreteResult(edge, result.effects), result.effects, edge)
    end
    return ConstCallResults(Const(value), Union{}, ConcreteResult(edge, EFFECTS_TOTAL, value), EFFECTS_TOTAL, edge)
end

# check if there is a cycle and duplicated inference of `mi`
function is_constprop_recursed(result::MethodCallResult, mi::MethodInstance, sv::AbsIntState)
    result.edgecycle || return false
    if result.edgelimited
        return is_constprop_method_recursed(mi.def::Method, sv)
    else
        # if the type complexity limiting didn't decide to limit the call signature (as
        # indicated by `result.edgelimited === false`), we can relax the cycle detection
        # by comparing `MethodInstance`s and allow inference to propagate different
        # constant elements if the recursion is finite over the lattice
        return is_constprop_edge_recursed(mi, sv)
    end
end

# if there's a possibility we could get a better result with these constant arguments
# (hopefully without doing too much work), returns `MethodInstance`, or nothing otherwise
function maybe_get_const_prop_profitable(interp::AbstractInterpreter,
    result::MethodCallResult, @nospecialize(f), arginfo::ArgInfo, si::StmtInfo,
    match::MethodMatch, sv::AbsIntState)
    method = match.method
    force = force_const_prop(interp, f, method)
    if !const_prop_rettype_heuristic(interp, result, si, sv, force)
        # N.B. remarks are emitted within `const_prop_rettype_heuristic`
        return nothing
    end
    if !const_prop_argument_heuristic(interp, arginfo, sv)
        add_remark!(interp, sv, "[constprop] Disabled by argument heuristics")
        return nothing
    end
    all_overridden = is_all_overridden(interp, arginfo, sv)
    if !force && !const_prop_function_heuristic(interp, f, arginfo, all_overridden, sv)
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

function const_prop_rettype_heuristic(interp::AbstractInterpreter, result::MethodCallResult,
                                      si::StmtInfo, sv::AbsIntState, force::Bool)
    rt = result.rt
    if rt isa LimitedAccuracy
        # optimizations like inlining are disabled for limited frames,
        # thus there won't be much benefit in constant-prop' here
        # N.B. don't allow forced constprop' for safety (xref #52763)
        add_remark!(interp, sv, "[constprop] Disabled by rettype heuristic (limited accuracy)")
        return false
    elseif force
        return true
    elseif call_result_unused(si) && result.edgecycle
        add_remark!(interp, sv, "[constprop] Disabled by rettype heuristic (edgecycle with unused result)")
        return false
    end
    # check if this return type is improvable (i.e. whether it's possible that with more
    # information, we might get a more precise type)
    if isa(rt, Type)
        # could always be improved to `Const`, `PartialStruct` or just a more precise type,
        # unless we're already at `Bottom`
        if rt === Bottom
            add_remark!(interp, sv, "[constprop] Disabled by rettype heuristic (erroneous result)")
            return false
        end
        return true
    elseif isa(rt, PartialStruct) || isa(rt, InterConditional) || isa(rt, InterMustAlias)
        # could be improved to `Const` or a more precise wrapper
        return true
    elseif isa(rt, Const)
        if is_nothrow(result.effects)
            add_remark!(interp, sv, "[constprop] Disabled by rettype heuristic (nothrow const)")
            return false
        end
        # Could still be improved to Bottom (or at least could see the effects improved)
        return true
    else
        add_remark!(interp, sv, "[constprop] Disabled by rettype heuristic (unimprovable result)")
        return false
    end
end

# determines heuristically whether if constant propagation can be worthwhile
# by checking if any of given `argtypes` is "interesting" enough to be propagated
function const_prop_argument_heuristic(interp::AbstractInterpreter, arginfo::ArgInfo, sv::AbsIntState)
    𝕃ᵢ = typeinf_lattice(interp)
    argtypes = arginfo.argtypes
    for i in 1:length(argtypes)
        a = argtypes[i]
        if has_conditional(𝕃ᵢ, sv) && isa(a, Conditional) && arginfo.fargs !== nothing
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
function is_all_overridden(interp::AbstractInterpreter, (; fargs, argtypes)::ArgInfo, sv::AbsIntState)
    𝕃ᵢ = typeinf_lattice(interp)
    for i in 1:length(argtypes)
        a = argtypes[i]
        if has_conditional(𝕃ᵢ, sv) && isa(a, Conditional) && fargs !== nothing
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
           typename(typeof(f)).constprop_heuristic === Core.FORCE_CONST_PROP
end

function const_prop_function_heuristic(interp::AbstractInterpreter, @nospecialize(f),
    arginfo::ArgInfo, all_overridden::Bool, sv::AbsIntState)
    argtypes = arginfo.argtypes
    heuristic = typename(typeof(f)).constprop_heuristic
    if length(argtypes) > 1
        𝕃ᵢ = typeinf_lattice(interp)
        if heuristic === Core.ARRAY_INDEX_HEURISTIC
            arrty = argtypes[2]
            # don't propagate constant index into indexing of non-constant array
            if arrty isa Type && arrty <: AbstractArray && !issingletontype(arrty)
                # For static arrays, allow the constprop if we could possibly
                # deduce nothrow as a result.
                still_nothrow = isa(sv, InferenceState) ? is_nothrow(sv.ipo_effects) : false
                if !still_nothrow || ismutabletype(arrty)
                    return false
                end
            elseif ⊑(𝕃ᵢ, arrty, Array) || ⊑(𝕃ᵢ, arrty, GenericMemory)
                return false
            end
        elseif heuristic === Core.ITERATE_HEURISTIC
            itrty = argtypes[2]
            if ⊑(𝕃ᵢ, itrty, Array) || ⊑(𝕃ᵢ, itrty, GenericMemory)
                return false
            end
        end
    end
    if !all_overridden && heuristic === Core.SAMETYPE_HEURISTIC
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
    mi::MethodInstance, arginfo::ArgInfo, sv::AbsIntState)
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
            if src_inlining_policy(interp, inferred, NoCallInfo(), IR_FLAG_NULL)
                return true
            end
        end
    end
    return false # the cache isn't inlineable, so this constant-prop' will most likely be unfruitful
end

function semi_concrete_eval_call(interp::AbstractInterpreter,
    mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::AbsIntState)
    world = frame_world(sv)
    mi_cache = WorldView(code_cache(interp), world)
    code = get(mi_cache, mi, nothing)
    if code !== nothing
        irsv = IRInterpretationState(interp, code, mi, arginfo.argtypes, world)
        if irsv !== nothing
            assign_parentchild!(irsv, sv)
            rt, (nothrow, noub) = ir_abstract_constant_propagation(interp, irsv)
            @assert !(rt isa Conditional || rt isa MustAlias) "invalid lattice element returned from irinterp"
            if !(isa(rt, Type) && hasintersect(rt, Bool))
                ir = irsv.ir
                # TODO (#48913) enable double inlining pass when there are any calls
                # that are newly resolved by irinterp
                # state = InliningState(interp)
                # ir = ssa_inlining_pass!(irsv.ir, state, propagate_inbounds(irsv))
                effects = result.effects
                if nothrow
                    effects = Effects(effects; nothrow=true)
                end
                if noub
                    effects = Effects(effects; noub=ALWAYS_TRUE)
                end
                exct = refine_exception_type(result.exct, effects)
                return ConstCallResults(rt, exct, SemiConcreteResult(mi, ir, effects, spec_info(irsv)), effects, mi)
            end
        end
    end
    return nothing
end

const_prop_result(inf_result::InferenceResult) =
    ConstCallResults(inf_result.result, inf_result.exc_result, ConstPropResult(inf_result),
                     inf_result.ipo_effects, inf_result.linfo)

# return cached result of constant analysis
return_localcache_result(::AbstractInterpreter, inf_result::InferenceResult, ::AbsIntState) =
    const_prop_result(inf_result)

function compute_forwarded_argtypes(interp::AbstractInterpreter, arginfo::ArgInfo, sv::AbsIntState)
    𝕃ᵢ = typeinf_lattice(interp)
    return has_conditional(𝕃ᵢ, sv) ? ConditionalSimpleArgtypes(arginfo, sv) : SimpleArgtypes(arginfo.argtypes)
end

function const_prop_call(interp::AbstractInterpreter,
    mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::AbsIntState,
    concrete_eval_result::Union{Nothing, ConstCallResults}=nothing)
    inf_cache = get_inference_cache(interp)
    𝕃ᵢ = typeinf_lattice(interp)
    forwarded_argtypes = compute_forwarded_argtypes(interp, arginfo, sv)
    # use `cache_argtypes` that has been constructed for fresh regular inference if available
    volatile_inf_result = result.volatile_inf_result
    if volatile_inf_result !== nothing
        cache_argtypes = volatile_inf_result.inf_result.argtypes
    else
        cache_argtypes = matching_cache_argtypes(𝕃ᵢ, mi)
    end
    argtypes = matching_cache_argtypes(𝕃ᵢ, mi, forwarded_argtypes, cache_argtypes)
    inf_result = cache_lookup(𝕃ᵢ, mi, argtypes, inf_cache)
    if inf_result !== nothing
        # found the cache for this constant prop'
        if inf_result.result === nothing
            add_remark!(interp, sv, "[constprop] Found cached constant inference in a cycle")
            return nothing
        end
        @assert inf_result.linfo === mi "MethodInstance for cached inference result does not match"
        return return_localcache_result(interp, inf_result, sv)
    end
    overridden_by_const = falses(length(argtypes))
    for i = 1:length(argtypes)
        if argtypes[i] !== argtype_by_index(cache_argtypes, i)
            overridden_by_const[i] = true
        end
    end
    if !any(overridden_by_const)
        add_remark!(interp, sv, "[constprop] Could not handle constant info in matching_cache_argtypes")
        return nothing
    end
    # perform fresh constant prop'
    inf_result = InferenceResult(mi, argtypes, overridden_by_const)
    frame = InferenceState(inf_result, #=cache_mode=#:local, interp) # TODO: this should also be converted to a stackless Future
    if frame === nothing
        add_remark!(interp, sv, "[constprop] Could not retrieve the source")
        return nothing # this is probably a bad generated function (unsound), but just ignore it
    end
    assign_parentchild!(frame, sv)
    if !typeinf(interp, frame)
        add_remark!(interp, sv, "[constprop] Fresh constant inference hit a cycle")
        @assert frame.frameid != 0 && frame.cycleid == frame.frameid
        callstack = frame.callstack::Vector{AbsIntState}
        @assert callstack[end] === frame && length(callstack) == frame.frameid
        pop!(callstack)
        return nothing
    end
    @assert frame.frameid != 0 && frame.cycleid == frame.frameid
    @assert frame.parentid == sv.frameid
    @assert inf_result.result !== nothing
    # ConditionalSimpleArgtypes is allowed, because the only case in which it modifies
    # the argtypes is when one of the argtypes is a `Conditional`, which case
    # concrete_eval_result will not be available.
    if concrete_eval_result !== nothing && isa(forwarded_argtypes, Union{SimpleArgtypes, ConditionalSimpleArgtypes})
        # override return type and effects with concrete evaluation result if available
        inf_result.result = concrete_eval_result.rt
        inf_result.ipo_effects = concrete_eval_result.effects
    end
    return const_prop_result(inf_result)
end

# TODO implement MustAlias forwarding

struct ConditionalSimpleArgtypes
    arginfo::ArgInfo
    sv::InferenceState
end

function matching_cache_argtypes(𝕃::AbstractLattice, mi::MethodInstance,
                                 conditional_argtypes::ConditionalSimpleArgtypes,
                                 cache_argtypes::Vector{Any})
    (; arginfo, sv) = conditional_argtypes
    (; fargs, argtypes) = arginfo
    given_argtypes = Vector{Any}(undef, length(argtypes))
    def = mi.def::Method
    nargs = Int(def.nargs)
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
                    given_argtypes[i] = Conditional(slotid, thentype, elsetype)
                end
                continue
            end
        end
        given_argtypes[i] = widenslotwrapper(argtype)
    end
    return pick_const_args!(𝕃, given_argtypes, cache_argtypes)
end

# This is only for use with `Conditional`.
# In general, usage of this is wrong.
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

# No slots in irinterp
ssa_def_slot(@nospecialize(arg), sv::IRInterpretationState) = nothing

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
                                sv::AbsIntState)
    if isa(typ, PartialStruct)
        widet = typ.typ
        if isa(widet, DataType)
            if widet.name === Tuple.name
                return Future(AbstractIterationResult(typ.fields, nothing))
            elseif widet.name === _NAMEDTUPLE_NAME
                return Future(AbstractIterationResult(typ.fields, nothing))
            end
        end
    end

    if isa(typ, Const)
        val = typ.val
        if isa(val, SimpleVector) || isa(val, Tuple) || isa(val, NamedTuple)
            return Future(AbstractIterationResult(Any[ Const(val[i]) for i in 1:length(val) ], nothing)) # avoid making a tuple Generator here!
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
        # refine the Union to remove elements that are not valid tags for objects
        filter!(@nospecialize(x) -> valid_as_lattice(x, true), utis)
        if length(utis) == 0
            return Future(AbstractIterationResult(Any[], nothing)) # oops, this statement was actually unreachable
        elseif length(utis) == 1
            tti = utis[1]
            tti0 = rewrap_unionall(tti, tti0)
        else
            if any(@nospecialize(t) -> !isa(t, DataType) || !(t <: Tuple) || !isknownlength(t), utis)
                return Future(AbstractIterationResult(Any[Vararg{Any}], nothing, Effects()))
            end
            ltp = length((utis[1]::DataType).parameters)
            for t in utis
                if length((t::DataType).parameters) != ltp
                    return Future(AbstractIterationResult(Any[Vararg{Any}], nothing))
                end
            end
            result = Any[ Union{} for _ in 1:ltp ]
            for t in utis
                tps = (t::DataType).parameters
                for j in 1:ltp
                    @assert valid_as_lattice(tps[j], true)
                    result[j] = tmerge(result[j], rewrap_unionall(tps[j], tti0))
                end
            end
            return Future(AbstractIterationResult(result, nothing))
        end
    end
    if tti0 <: Tuple
        if isa(tti0, DataType)
            return Future(AbstractIterationResult(Any[ p for p in tti0.parameters ], nothing))
        elseif !isa(tti, DataType)
            return Future(AbstractIterationResult(Any[Vararg{Any}], nothing))
        else
            len = length(tti.parameters)
            last = tti.parameters[len]
            va = isvarargtype(last)
            elts = Any[ fieldtype(tti0, i) for i = 1:len ]
            if va
                if elts[len] === Union{}
                    pop!(elts)
                else
                    elts[len] = Vararg{elts[len]}
                end
            end
            return Future(AbstractIterationResult(elts, nothing))
        end
    elseif tti0 === SimpleVector
        return Future(AbstractIterationResult(Any[Vararg{Any}], nothing))
    elseif tti0 === Any
        return Future(AbstractIterationResult(Any[Vararg{Any}], nothing, Effects()))
    elseif tti0 <: Array || tti0 <: GenericMemory
        if eltype(tti0) === Union{}
            return Future(AbstractIterationResult(Any[], nothing))
        end
        return Future(AbstractIterationResult(Any[Vararg{eltype(tti0)}], nothing))
    else
        return abstract_iteration(interp, itft, typ, sv)
    end
end

# simulate iteration protocol on container type up to fixpoint
function abstract_iteration(interp::AbstractInterpreter, @nospecialize(itft), @nospecialize(itertype), sv::AbsIntState)
    if isa(itft, Const)
        iteratef = itft.val
    else
        return Future(AbstractIterationResult(Any[Vararg{Any}], nothing, Effects()))
    end
    @assert !isvarargtype(itertype)

    iterateresult = Future{AbstractIterationResult}()
    call1future = abstract_call_known(interp, iteratef, ArgInfo(nothing, Any[itft, itertype]), StmtInfo(true), sv)::Future
    function inferiterate(interp, sv)
        call1 = call1future[]
        stateordonet = call1.rt
        # Return Bottom if this is not an iterator.
        # WARNING: Changes to the iteration protocol must be reflected here,
        # this is not just an optimization.
        # TODO: this doesn't realize that Array, GenericMemory, SimpleVector, Tuple, and NamedTuple do not use the iterate protocol
        if stateordonet === Bottom
            iterateresult[] = AbstractIterationResult(Any[Bottom], AbstractIterationInfo(CallMeta[CallMeta(Bottom, Any, call1.effects, call1.info)], true))
            return true
        end
        stateordonet_widened = widenconst(stateordonet)
        calls = CallMeta[call1]
        valtype = statetype = Bottom
        ret = Any[]
        𝕃ᵢ = typeinf_lattice(interp)
        may_have_terminated = false
        local call2future::Future{CallMeta}

        nextstate::UInt8 = 0x0
        function inferiterate_2arg(interp, sv)
            if nextstate === 0x1
                nextstate = 0xff
                @goto state1
            elseif nextstate === 0x2
                nextstate = 0xff
                @goto state2
            else
                @assert nextstate === 0x0
                nextstate = 0xff
            end

            # Try to unroll the iteration up to max_tuple_splat, which covers any finite
            # length iterators, or interesting prefix
            while true
                if stateordonet_widened === Nothing
                    iterateresult[] = AbstractIterationResult(ret, AbstractIterationInfo(calls, true))
                    return true
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
                    iterateresult[] = AbstractIterationResult(Any[Bottom], AbstractIterationInfo(calls, false), EFFECTS_THROWS)
                    return true
                end
                valtype = getfield_tfunc(𝕃ᵢ, stateordonet, Const(1))
                push!(ret, valtype)
                statetype = nstatetype
                call2future = abstract_call_known(interp, iteratef, ArgInfo(nothing, Any[Const(iteratef), itertype, statetype]), StmtInfo(true), sv)::Future
                if !isready(call2future)
                    nextstate = 0x1
                    return false
                    @label state1
                end
                let call = call2future[]
                    push!(calls, call)
                    stateordonet = call.rt
                    stateordonet_widened = widenconst(stateordonet)
                end
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
                        if may_have_terminated
                            # ... and iterator may have terminated prior to this loop, but not during it
                            valtype = Bottom
                        else
                            #  ... or cannot have terminated prior to this loop
                            iterateresult[] = AbstractIterationResult(Any[Bottom], AbstractIterationInfo(calls, false), Effects())
                            return true
                        end
                    end
                    break
                end
                valtype = tmerge(valtype, nounion.parameters[1])
                statetype = tmerge(statetype, nounion.parameters[2])
                call2future = abstract_call_known(interp, iteratef, ArgInfo(nothing, Any[Const(iteratef), itertype, statetype]), StmtInfo(true), sv)::Future
                if !isready(call2future)
                    nextstate = 0x2
                    return false
                    @label state2
                end
                let call = call2future[]
                    push!(calls, call)
                    stateordonet = call.rt
                    stateordonet_widened = widenconst(stateordonet)
                end
            end
            if valtype !== Union{}
                push!(ret, Vararg{valtype})
            end
            iterateresult[] = AbstractIterationResult(ret, AbstractIterationInfo(calls, false))
            return true
        end # inferiterate_2arg
        # continue making progress as much as possible, on iterate(arg, state)
        inferiterate_2arg(interp, sv) || push!(sv.tasks, inferiterate_2arg)
        return true
    end # inferiterate
    # continue making progress as soon as possible, on iterate(arg)
    if !(isready(call1future) && inferiterate(interp, sv))
        push!(sv.tasks, inferiterate)
    end
    return iterateresult
end

# do apply(af, fargs...), where af is a function value
function abstract_apply(interp::AbstractInterpreter, argtypes::Vector{Any}, si::StmtInfo,
                        sv::AbsIntState, max_methods::Int=get_max_methods(interp, sv))
    itft = Core.Box(argtype_by_index(argtypes, 2))
    aft = argtype_by_index(argtypes, 3)
    (itft.contents === Bottom || aft === Bottom) && return Future(CallMeta(Bottom, Any, EFFECTS_THROWS, NoCallInfo()))
    aargtypes = argtype_tail(argtypes, 4)
    aftw = widenconst(aft)
    if !isa(aft, Const) && !isa(aft, PartialOpaque) && (!isType(aftw) || has_free_typevars(aftw))
        if !isconcretetype(aftw) || (aftw <: Builtin)
            add_remark!(interp, sv, "Core._apply_iterate called on a function of a non-concrete type")
            # bail now, since it seems unlikely that abstract_call will be able to do any better after splitting
            # this also ensures we don't call abstract_call_gf_by_type below on an IntrinsicFunction or Builtin
            return Future(CallMeta(Any, Any, Effects(), NoCallInfo()))
        end
    end
    res = Union{}
    splitunions = 1 < unionsplitcost(typeinf_lattice(interp), aargtypes) <= InferenceParams(interp).max_apply_union_enum
    ctypes::Vector{Vector{Any}} = [Any[aft]]
    infos::Vector{Vector{MaybeAbstractIterationInfo}} = Vector{MaybeAbstractIterationInfo}[MaybeAbstractIterationInfo[]]
    all_effects::Effects = EFFECTS_TOTAL
    retinfos = ApplyCallInfo[]
    retinfo = UnionSplitApplyCallInfo(retinfos)
    exctype = Union{}
    ctypes´ = Vector{Any}[]
    infos´ = Vector{MaybeAbstractIterationInfo}[]
    local ti, argtypesi
    local ctfuture::Future{AbstractIterationResult}
    local callfuture::Future{CallMeta}

    applyresult = Future{CallMeta}()
    # split the rest into a resumable state machine
    i::Int = 1
    j::Int = 1
    nextstate::UInt8 = 0x0
    function infercalls(interp, sv)
        # n.b. Remember that variables will lose their values across restarts,
        # so be sure to manually hoist any values that must be preserved and do
        # not rely on program order.
        # This is a little more complex than the closure continuations often used elsewhere, but avoids needing to manage all of that indentation
        if nextstate === 0x1
            nextstate = 0xff
            @goto state1
        elseif nextstate === 0x2
            nextstate = 0xff
            @goto state2
        elseif nextstate === 0x3
            nextstate = 0xff
            @goto state3
        else
            @assert nextstate === 0x0
            nextstate = 0xff
        end
        while i <= length(aargtypes)
            argtypesi = (splitunions ? uniontypes(aargtypes[i]) : Any[aargtypes[i]])
            i += 1
            j = 1
            while j <= length(argtypesi)
                ti = argtypesi[j]
                j += 1
                if !isvarargtype(ti)
                    ctfuture = precise_container_type(interp, itft.contents, ti, sv)::Future
                    if !isready(ctfuture)
                        nextstate = 0x1
                        return false
                        @label state1
                    end
                    (;cti, info, ai_effects) = ctfuture[]
                else
                    ctfuture = precise_container_type(interp, itft.contents, unwrapva(ti), sv)::Future
                    if !isready(ctfuture)
                        nextstate = 0x2
                        return false
                        @label state2
                    end
                    (;cti, info, ai_effects) = ctfuture[]
                    # We can't represent a repeating sequence of the same types,
                    # so tmerge everything together to get one type that represents
                    # everything.
                    argt = cti[end]
                    if isvarargtype(argt)
                        argt = unwrapva(argt)
                    end
                    for k in 1:(length(cti)-1)
                        argt = tmerge(argt, cti[k])
                    end
                    cti = Any[Vararg{argt}]
                end
                all_effects = merge_effects(all_effects, ai_effects)
                if info !== nothing
                    for call in info.each
                        all_effects = merge_effects(all_effects, call.effects)
                    end
                end
                if any(@nospecialize(t) -> t === Bottom, cti)
                    continue
                end
                for k = 1:length(ctypes)
                    ct = ctypes[k]
                    if isvarargtype(ct[end])
                        # This is vararg, we're not gonna be able to do any inlining,
                        # drop the info
                        info = nothing
                        tail = tuple_tail_elem(typeinf_lattice(interp), unwrapva(ct[end]), cti)
                        push!(ctypes´, push!(ct[1:(end - 1)], tail))
                    else
                        push!(ctypes´, append!(ct[:], cti))
                    end
                    push!(infos´, push!(copy(infos[k]), info))
                end
            end
            # swap for the new array and empty the temporary one
            ctypes´, ctypes = ctypes, ctypes´
            infos´, infos = infos, infos´
            empty!(ctypes´)
            empty!(infos´)
        end
        all_effects.nothrow || (exctype = Any)

        i = 1
        while i <= length(ctypes)
            ct = ctypes[i]
            lct = length(ct)
            # truncate argument list at the first Vararg
            for k = 1:lct-1
                cti = ct[k]
                if isvarargtype(cti)
                    ct[k] = tuple_tail_elem(typeinf_lattice(interp), unwrapva(cti), ct[(k+1):lct])
                    resize!(ct, k)
                    break
                end
            end
            callfuture = abstract_call(interp, ArgInfo(nothing, ct), si, sv, max_methods)::Future
            if !isready(callfuture)
                nextstate = 0x3
                return false
                @label state3
            end
            let (; info, rt, exct, effects) = callfuture[]
                push!(retinfos, ApplyCallInfo(info, infos[i]))
                res = tmerge(typeinf_lattice(interp), res, rt)
                exctype = tmerge(typeinf_lattice(interp), exctype, exct)
                all_effects = merge_effects(all_effects, effects)
                if i < length(ctypes) && bail_out_apply(interp, InferenceLoopState(ctypes[i], res, all_effects), sv)
                    add_remark!(interp, sv, "_apply_iterate inference reached maximally imprecise information. Bailing on.")
                    # there is unanalyzed candidate, widen type and effects to the top
                    let retinfo = NoCallInfo() # NOTE this is necessary to prevent the inlining processing
                        applyresult[] = CallMeta(Any, Any, Effects(), retinfo)
                        return true
                    end
                end
            end
            i += 1
        end
        # TODO: Add a special info type to capture all the iteration info.
        # For now, only propagate info if we don't also union-split the iteration
        applyresult[] = CallMeta(res, exctype, all_effects, retinfo)
        return true
    end
    # start making progress on the first call
    infercalls(interp, sv) || push!(sv.tasks, infercalls)
    return applyresult
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
    tty_ub, isexact_tty = instanceof_tfunc(ty, true)
    tty = widenconst(xt)
    if isexact_tty && !isa(tty_ub, TypeVar)
        tty_lb = tty_ub # TODO: this would be wrong if !isexact_tty, but instanceof_tfunc doesn't preserve this info
        if !has_free_typevars(tty_lb) && !has_free_typevars(tty_ub)
            thentype = typeintersect(tty, tty_ub)
            if iskindtype(tty_ub) && thentype !== Bottom
                # `typeintersect` may be unable narrow down `Type`-type
                thentype = tty_ub
            end
            valid_as_lattice(thentype, true) || (thentype = Bottom)
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
    elseif elsetype isa Type && issingletontype(typeof(c.val)) # can only widen a if it is a singleton
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
                               sv::AbsIntState)
    @nospecialize f
    la = length(argtypes)
    𝕃ᵢ = typeinf_lattice(interp)
    ⊑, ⊏, ⊔, ⊓ = partialorder(𝕃ᵢ), strictpartialorder(𝕃ᵢ), join(𝕃ᵢ), meet(𝕃ᵢ)
    if has_conditional(𝕃ᵢ, sv) && f === Core.ifelse && fargs isa Vector{Any} && la == 4
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
                    tx = (cnd.thentype ⊑ tx ? cnd.thentype : tx ⊓ widenconst(cnd.thentype))
                end
                if isa(b, SlotNumber) && cnd.slot == slot_id(b)
                    ty = (cnd.elsetype ⊑ ty ? cnd.elsetype : ty ⊓ widenconst(cnd.elsetype))
                end
                return tx ⊔ ty
            end
        end
    end
    ft = popfirst!(argtypes)
    rt = builtin_tfunction(interp, f, argtypes, sv)
    pushfirst!(argtypes, ft)
    if has_mustalias(𝕃ᵢ) && f === getfield && isa(fargs, Vector{Any}) && la ≥ 3
        a3 = argtypes[3]
        if isa(a3, Const)
            if rt !== Bottom && !isalreadyconst(rt)
                var = ssa_def_slot(fargs[2], sv)
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
    elseif has_conditional(𝕃ᵢ, sv) && (rt === Bool || (isa(rt, Const) && isa(rt.val, Bool))) && isa(fargs, Vector{Any})
        # perform very limited back-propagation of type information for `is` and `isa`
        if f === isa
            # try splitting value argument, based on types
            a = ssa_def_slot(fargs[2], sv)
            a2 = argtypes[2]
            a3 = argtypes[3]
            if isa(a, SlotNumber)
                cndt = isa_condition(a2, a3, InferenceParams(interp).max_union_splitting, rt)
                if cndt !== nothing
                    return Conditional(a, cndt.thentype, cndt.elsetype)
                end
            end
            if isa(a2, MustAlias)
                if !isa(rt, Const) # skip refinement when the field is known precisely (just optimization)
                    cndt = isa_condition(a2, a3, InferenceParams(interp).max_union_splitting)
                    if cndt !== nothing
                        return form_mustalias_conditional(a2, cndt.thentype, cndt.elsetype)
                    end
                end
            end
            # try splitting type argument, based on value
            if isdispatchelem(widenconst(a2)) && a3 isa Union && !has_free_typevars(a3) && !isa(rt, Const)
                b = ssa_def_slot(fargs[3], sv)
                if isa(b, SlotNumber)
                    # !(x isa T) implies !(Type{a2} <: T)
                    # TODO: complete splitting, based on which portions of the Union a3 for which isa_tfunc returns Const(true) or Const(false) instead of Bool
                    elsetype = typesubtract(a3, Type{widenconst(a2)}, InferenceParams(interp).max_union_splitting)
                    return Conditional(b, a3, elsetype)
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
            a = ssa_def_slot(fargs[2], sv)
            if isa(a, SlotNumber)
                argtype2 = argtypes[2]
                if isa(argtype2, Union)
                    fld = argtypes[3]
                    thentype = Bottom
                    elsetype = Bottom
                    for ty in uniontypes(argtype2)
                        cnd = isdefined_tfunc(𝕃ᵢ, ty, fld)
                        if isa(cnd, Const)
                            if cnd.val::Bool
                                thentype = thentype ⊔ ty
                            else
                                elsetype = elsetype ⊔ ty
                            end
                        else
                            thentype = thentype ⊔ ty
                            elsetype = elsetype ⊔ ty
                        end
                    end
                    return Conditional(a, thentype, elsetype)
                else
                    thentype = form_partially_defined_struct(argtype2, argtypes[3])
                    if thentype !== nothing
                        elsetype = argtype2
                        if rt === Const(false)
                            thentype = Bottom
                        elseif rt === Const(true)
                            elsetype = Bottom
                        end
                        return Conditional(a, thentype, elsetype)
                    end
                end
            end
        end
    end
    @assert !isa(rt, TypeVar) "unhandled TypeVar"
    return rt
end

function form_partially_defined_struct(@nospecialize(obj), @nospecialize(name))
    obj isa Const && return nothing # nothing to refine
    name isa Const || return nothing
    objt0 = widenconst(obj)
    objt = unwrap_unionall(objt0)
    objt isa DataType || return nothing
    isabstracttype(objt) && return nothing
    fldidx = try_compute_fieldidx(objt, name.val)
    fldidx === nothing && return nothing
    nminfld = datatype_min_ninitialized(objt)
    if ismutabletype(objt)
        # A mutable struct can have non-contiguous undefined fields, but `PartialStruct` cannot
        # model such a state. So here `PartialStruct` can be used to represent only the
        # objects where the field following the minimum initialized fields is also defined.
        if fldidx ≠ nminfld+1
            # if it is already represented as a `PartialStruct`, we can add one more
            # `isdefined`-field information on top of those implied by its `fields`
            if !(obj isa PartialStruct && fldidx == length(obj.fields)+1)
                return nothing
            end
        end
    else
        fldidx > nminfld || return nothing
    end
    return PartialStruct(objt0, Any[obj isa PartialStruct && i≤length(obj.fields) ?
        obj.fields[i] : fieldtype(objt0,i) for i = 1:fldidx])
end

function abstract_call_unionall(interp::AbstractInterpreter, argtypes::Vector{Any}, call::CallMeta)
    na = length(argtypes)
    if isvarargtype(argtypes[end])
        if na ≤ 2
            return CallMeta(Any, Any, EFFECTS_THROWS, call.info)
        elseif na > 4
            return CallMeta(Bottom, Any, EFFECTS_THROWS, NoCallInfo())
        end
        a2 = argtypes[2]
        a3 = unwrapva(argtypes[3])
        nothrow = false
    elseif na == 3
        a2 = argtypes[2]
        a3 = argtypes[3]
        ⊑ = partialorder(typeinf_lattice(interp))
        nothrow = a2 ⊑ TypeVar && (a3 ⊑ Type || a3 ⊑ TypeVar)
    else
        return CallMeta(Bottom, Any, EFFECTS_THROWS, NoCallInfo())
    end
    canconst = true
    if isa(a3, Const)
        body = a3.val
    elseif isType(a3)
        body = a3.parameters[1]
        canconst = false
    else
        return CallMeta(Any, Any, Effects(EFFECTS_TOTAL; nothrow), call.info)
    end
    if !(isa(body, Type) || isa(body, TypeVar))
        return CallMeta(Any, Any, EFFECTS_THROWS, call.info)
    end
    if has_free_typevars(body)
        if isa(a2, Const)
            tv = a2.val
        elseif isa(a2, PartialTypeVar)
            tv = a2.tv
            canconst = false
        else
            return CallMeta(Any, Any, EFFECTS_THROWS, call.info)
        end
        isa(tv, TypeVar) || return CallMeta(Any, Any, EFFECTS_THROWS, call.info)
        body = UnionAll(tv, body)
    end
    ret = canconst ? Const(body) : Type{body}
    return CallMeta(ret, Any, Effects(EFFECTS_TOTAL; nothrow), call.info)
end

function abstract_invoke(interp::AbstractInterpreter, arginfo::ArgInfo, si::StmtInfo, sv::AbsIntState)
    argtypes = arginfo.argtypes
    ft′ = argtype_by_index(argtypes, 2)
    ft = widenconst(ft′)
    ft === Bottom && return Future(CallMeta(Bottom, Any, EFFECTS_THROWS, NoCallInfo()))
    (types, isexact, isconcrete, istype) = instanceof_tfunc(argtype_by_index(argtypes, 3), false)
    isexact || return Future(CallMeta(Any, Any, Effects(), NoCallInfo()))
    unwrapped = unwrap_unionall(types)
    types === Bottom && return Future(CallMeta(Bottom, Any, EFFECTS_THROWS, NoCallInfo()))
    if !(unwrapped isa DataType && unwrapped.name === Tuple.name)
        return Future(CallMeta(Bottom, TypeError, EFFECTS_THROWS, NoCallInfo()))
    end
    argtype = argtypes_to_type(argtype_tail(argtypes, 4))
    nargtype = typeintersect(types, argtype)
    nargtype === Bottom && return Future(CallMeta(Bottom, TypeError, EFFECTS_THROWS, NoCallInfo()))
    nargtype isa DataType || return Future(CallMeta(Any, Any, Effects(), NoCallInfo())) # other cases are not implemented below
    isdispatchelem(ft) || return Future(CallMeta(Any, Any, Effects(), NoCallInfo())) # check that we might not have a subtype of `ft` at runtime, before doing supertype lookup below
    ft = ft::DataType
    lookupsig = rewrap_unionall(Tuple{ft, unwrapped.parameters...}, types)::Type
    nargtype = Tuple{ft, nargtype.parameters...}
    argtype = Tuple{ft, argtype.parameters...}
    match, valid_worlds = findsup(lookupsig, method_table(interp))
    match === nothing && return Future(CallMeta(Any, Any, Effects(), NoCallInfo()))
    update_valid_age!(sv, valid_worlds)
    method = match.method
    tienv = ccall(:jl_type_intersection_with_env, Any, (Any, Any), nargtype, method.sig)::SimpleVector
    ti = tienv[1]
    env = tienv[2]::SimpleVector
    mresult = abstract_call_method(interp, method, ti, env, false, si, sv)::Future
    match = MethodMatch(ti, env, method, argtype <: method.sig)
    return Future{CallMeta}(mresult, interp, sv) do result, interp, sv
        (; rt, exct, edge, effects, volatile_inf_result) = result
        res = nothing
        sig = match.spec_types
        argtypes′ = invoke_rewrite(argtypes)
        fargs = arginfo.fargs
        fargs′ = fargs === nothing ? nothing : invoke_rewrite(fargs)
        arginfo = ArgInfo(fargs′, argtypes′)
        # # typeintersect might have narrowed signature, but the accuracy gain doesn't seem worth the cost involved with the lattice comparisons
        # for i in 1:length(argtypes′)
        #     t, a = ti.parameters[i], argtypes′[i]
        #     argtypes′[i] = t ⊑ a ? t : a
        # end
        𝕃ₚ = ipo_lattice(interp)
        ⊑, ⋤, ⊔ = partialorder(𝕃ₚ), strictneqpartialorder(𝕃ₚ), join(𝕃ₚ)
        f = singleton_type(ft′)
        invokecall = InvokeCall(types, lookupsig)
        const_call_result = abstract_call_method_with_const_args(interp,
            result, f, arginfo, si, match, sv, invokecall)
        const_result = volatile_inf_result
        if const_call_result !== nothing
            if const_call_result.rt ⊑ rt
                (; rt, effects, const_result, edge) = const_call_result
            end
            if const_call_result.exct ⋤ exct
                (; exct, const_result, edge) = const_call_result
            end
        end
        rt = from_interprocedural!(interp, rt, sv, arginfo, sig)
        info = InvokeCallInfo(match, const_result)
        edge !== nothing && add_invoke_backedge!(sv, lookupsig, edge)
        if !match.fully_covers
            effects = Effects(effects; nothrow=false)
            exct = exct ⊔ TypeError
        end
        return CallMeta(rt, exct, effects, info)
    end
end

function invoke_rewrite(xs::Vector{Any})
    x0 = xs[2]
    newxs = xs[3:end]
    newxs[1] = x0
    return newxs
end

function abstract_finalizer(interp::AbstractInterpreter, argtypes::Vector{Any}, sv::AbsIntState)
    if length(argtypes) == 3
        finalizer_argvec = Any[argtypes[2], argtypes[3]]
        call = abstract_call(interp, ArgInfo(nothing, finalizer_argvec), StmtInfo(false), sv, #=max_methods=#1)::Future
        return Future{CallMeta}(call, interp, sv) do call, interp, sv
            return CallMeta(Nothing, Any, Effects(), FinalizerInfo(call.info, call.effects))
        end
    end
    return Future(CallMeta(Nothing, Any, Effects(), NoCallInfo()))
end

function abstract_throw(interp::AbstractInterpreter, argtypes::Vector{Any}, ::AbsIntState)
    na = length(argtypes)
    ⊔ = join(typeinf_lattice(interp))
    if na == 2
        argtype2 = argtypes[2]
        if isvarargtype(argtype2)
            exct = unwrapva(argtype2) ⊔ ArgumentError
        else
            exct = argtype2
        end
    elseif na == 3 && isvarargtype(argtypes[3])
        exct = argtypes[2] ⊔ ArgumentError
    else
        exct = ArgumentError
    end
    return Future(CallMeta(Union{}, exct, EFFECTS_THROWS, NoCallInfo()))
end

function abstract_throw_methoderror(interp::AbstractInterpreter, argtypes::Vector{Any}, ::AbsIntState)
    exct = if length(argtypes) == 1
        ArgumentError
    elseif !isvarargtype(argtypes[2])
        MethodError
    else
        ⊔ = join(typeinf_lattice(interp))
        MethodError ⊔ ArgumentError
    end
    return Future(CallMeta(Union{}, exct, EFFECTS_THROWS, NoCallInfo()))
end

# call where the function is known exactly
function abstract_call_known(interp::AbstractInterpreter, @nospecialize(f),
        arginfo::ArgInfo, si::StmtInfo, sv::AbsIntState,
        max_methods::Int = get_max_methods(interp, f, sv))
    (; fargs, argtypes) = arginfo
    la = length(argtypes)
    𝕃ᵢ = typeinf_lattice(interp)
    if isa(f, Builtin)
        if f === _apply_iterate
            return abstract_apply(interp, argtypes, si, sv, max_methods)
        elseif f === invoke
            return abstract_invoke(interp, arginfo, si, sv)
        elseif f === modifyfield! || f === Core.modifyglobal! || f === Core.memoryrefmodify! || f === atomic_pointermodify
            return abstract_modifyop!(interp, f, argtypes, si, sv)
        elseif f === Core.finalizer
            return abstract_finalizer(interp, argtypes, sv)
        elseif f === applicable
            return abstract_applicable(interp, argtypes, sv, max_methods)
        elseif f === throw
            return abstract_throw(interp, argtypes, sv)
        elseif f === Core.throw_methoderror
            return abstract_throw_methoderror(interp, argtypes, sv)
        end
        rt = abstract_call_builtin(interp, f, arginfo, sv)
        ft = popfirst!(argtypes)
        effects = builtin_effects(𝕃ᵢ, f, argtypes, rt)
        if effects.nothrow
            exct = Union{}
        else
            exct = builtin_exct(𝕃ᵢ, f, argtypes, rt)
        end
        pushfirst!(argtypes, ft)
        refinements = nothing
        if sv isa InferenceState && f === typeassert
            # perform very limited back-propagation of invariants after this type assertion
            if rt !== Bottom && isa(fargs, Vector{Any})
                farg2 = fargs[2]
                if farg2 isa SlotNumber
                    refinements = SlotRefinement(farg2, rt)
                end
            end
        end
        return Future(CallMeta(rt, exct, effects, NoCallInfo(), refinements))
    elseif isa(f, Core.OpaqueClosure)
        # calling an OpaqueClosure about which we have no information returns no information
        return Future(CallMeta(typeof(f).parameters[2], Any, Effects(), NoCallInfo()))
    elseif f === TypeVar && !isvarargtype(argtypes[end])
        # Manually look through the definition of TypeVar to
        # make sure to be able to get `PartialTypeVar`s out.
        2 ≤ la ≤ 4 || return Future(CallMeta(Bottom, Any, EFFECTS_THROWS, NoCallInfo()))
        # make sure generic code is prepared for inlining if needed later
        let T = Any[Type{TypeVar}, Any, Any, Any]
            resize!(T, la)
            atype = Tuple{T...}
            T[1] = Const(TypeVar)
            let call = abstract_call_gf_by_type(interp, f, ArgInfo(nothing, T), si, atype, sv, max_methods)::Future
                return Future{CallMeta}(call, interp, sv) do call, interp, sv
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
                    typevar_argtypes = Any[n, lb_var, ub_var]
                    effects = builtin_effects(𝕃ᵢ, Core._typevar, typevar_argtypes, pT)
                    if effects.nothrow
                        exct = Union{}
                    else
                        exct = builtin_exct(𝕃ᵢ, Core._typevar, typevar_argtypes, pT)
                    end
                    return CallMeta(pT, exct, effects, call.info)
                end
            end
        end
    elseif f === UnionAll
        let call = abstract_call_gf_by_type(interp, f, ArgInfo(nothing, Any[Const(UnionAll), Any, Any]), si, Tuple{Type{UnionAll}, Any, Any}, sv, max_methods)::Future
            return Future{CallMeta}(call, interp, sv) do call, interp, sv
                return abstract_call_unionall(interp, argtypes, call)
            end
        end
    elseif f === Tuple && la == 2
        aty = argtypes[2]
        ty = isvarargtype(aty) ? unwrapva(aty) : widenconst(aty)
        if !isconcretetype(ty)
            return Future(CallMeta(Tuple, Any, EFFECTS_UNKNOWN, NoCallInfo()))
        end
    elseif is_return_type(f)
        return return_type_tfunc(interp, argtypes, si, sv)
    elseif la == 3 && f === Core.:(!==)
        # mark !== as exactly a negated call to ===
        let callfuture = abstract_call_gf_by_type(interp, f, ArgInfo(fargs, Any[Const(f), Any, Any]), si, Tuple{typeof(f), Any, Any}, sv, max_methods)::Future,
            rtfuture = abstract_call_known(interp, (===), arginfo, si, sv, max_methods)::Future
            return Future{CallMeta}(isready(callfuture) && isready(rtfuture), interp, sv) do interp, sv
                local rty = rtfuture[].rt
                if isa(rty, Conditional)
                    return CallMeta(Conditional(rty.slot, rty.elsetype, rty.thentype), Bottom, EFFECTS_TOTAL, NoCallInfo()) # swap if-else
                elseif isa(rty, Const)
                    return CallMeta(Const(rty.val === false), Bottom, EFFECTS_TOTAL, MethodResultPure())
                end
                return callfuture[]
            end
        end
    elseif la == 3 && f === Core.:(>:)
        # mark issupertype as a exact alias for issubtype
        # swap T1 and T2 arguments and call <:
        if fargs !== nothing && length(fargs) == 3
            fargs = Any[<:, fargs[3], fargs[2]]
        else
            fargs = nothing
        end
        argtypes = Any[typeof(<:), argtypes[3], argtypes[2]]
        return abstract_call_known(interp, <:, ArgInfo(fargs, argtypes), si, sv, max_methods)
    elseif la == 2 && f === Core.typename
        return Future(CallMeta(typename_static(argtypes[2]), Bottom, EFFECTS_TOTAL, MethodResultPure()))
    elseif f === Core._hasmethod
        return Future(_hasmethod_tfunc(interp, argtypes, sv))
    end
    atype = argtypes_to_type(argtypes)
    return abstract_call_gf_by_type(interp, f, arginfo, si, atype, sv, max_methods)::Future
end

function abstract_call_opaque_closure(interp::AbstractInterpreter,
    closure::PartialOpaque, arginfo::ArgInfo, si::StmtInfo, sv::AbsIntState, check::Bool=true)
    sig = argtypes_to_type(arginfo.argtypes)
    tt = closure.typ
    ocargsig = rewrap_unionall((unwrap_unionall(tt)::DataType).parameters[1], tt)
    ocargsig′ = unwrap_unionall(ocargsig)
    ocargsig′ isa DataType || return Future(CallMeta(Any, Any, Effects(), NoCallInfo()))
    ocsig = rewrap_unionall(Tuple{Tuple, ocargsig′.parameters...}, ocargsig)
    hasintersect(sig, ocsig) || return Future(CallMeta(Union{}, Union{MethodError,TypeError}, EFFECTS_THROWS, NoCallInfo()))
    ocmethod = closure.source::Method
    match = MethodMatch(sig, Core.svec(), ocmethod, sig <: ocsig)
    mresult = abstract_call_method(interp, ocmethod, sig, Core.svec(), false, si, sv)
    ocsig_box = Core.Box(ocsig)
    return Future{CallMeta}(mresult, interp, sv) do result, interp, sv
        (; rt, exct, edge, effects, volatile_inf_result, edgecycle) = result
        𝕃ₚ = ipo_lattice(interp)
        ⊑, ⋤, ⊔ = partialorder(𝕃ₚ), strictneqpartialorder(𝕃ₚ), join(𝕃ₚ)
        const_result = volatile_inf_result
        if !edgecycle
            const_call_result = abstract_call_method_with_const_args(interp, result,
                nothing, arginfo, si, match, sv)
            if const_call_result !== nothing
                if const_call_result.rt ⊑ rt
                    (; rt, effects, const_result, edge) = const_call_result
                end
                if const_call_result.exct ⋤ exct
                    (; exct, const_result, edge) = const_call_result
                end
            end
        end
        if check # analyze implicit type asserts on argument and return type
            ftt = closure.typ
            rty = (unwrap_unionall(ftt)::DataType).parameters[2]
            rty = rewrap_unionall(rty isa TypeVar ? rty.ub : rty, ftt)
            if !(rt ⊑ rty && sig ⊑ ocsig_box.contents)
                effects = Effects(effects; nothrow=false)
                exct = exct ⊔ TypeError
            end
        end
        rt = from_interprocedural!(interp, rt, sv, arginfo, match.spec_types)
        info = OpaqueClosureCallInfo(match, const_result)
        edge !== nothing && add_backedge!(sv, edge)
        return CallMeta(rt, exct, effects, info)
    end
end

function most_general_argtypes(closure::PartialOpaque)
    ret = Any[]
    cc = widenconst(closure)
    argt = (unwrap_unionall(cc)::DataType).parameters[1]
    if !isa(argt, DataType) || argt.name !== typename(Tuple)
        argt = Tuple
    end
    return Any[argt.parameters...]
end

function abstract_call_unknown(interp::AbstractInterpreter, @nospecialize(ft),
                               arginfo::ArgInfo, si::StmtInfo, sv::AbsIntState,
                               max_methods::Int)
    if isa(ft, PartialOpaque)
        newargtypes = copy(arginfo.argtypes)
        newargtypes[1] = ft.env
        return abstract_call_opaque_closure(interp,
            ft, ArgInfo(arginfo.fargs, newargtypes), si, sv, #=check=#true)
    end
    wft = widenconst(ft)
    if hasintersect(wft, Builtin)
        add_remark!(interp, sv, "Could not identify method table for call")
        return Future(CallMeta(Any, Any, Effects(), NoCallInfo()))
    elseif hasintersect(wft, Core.OpaqueClosure)
        uft = unwrap_unionall(wft)
        if isa(uft, DataType)
            return Future(CallMeta(rewrap_unionall(uft.parameters[2], wft), Any, Effects(), NoCallInfo()))
        end
        return Future(CallMeta(Any, Any, Effects(), NoCallInfo()))
    end
    # non-constant function, but the number of arguments is known and the `f` is not a builtin or intrinsic
    atype = argtypes_to_type(arginfo.argtypes)
    return abstract_call_gf_by_type(interp, nothing, arginfo, si, atype, sv, max_methods)::Future
end

# call where the function is any lattice element
function abstract_call(interp::AbstractInterpreter, arginfo::ArgInfo, si::StmtInfo,
                       sv::AbsIntState, max_methods::Int=typemin(Int))
    ft = widenslotwrapper(arginfo.argtypes[1])
    f = singleton_type(ft)
    if f === nothing
        max_methods = max_methods == typemin(Int) ? get_max_methods(interp, sv) : max_methods
        return abstract_call_unknown(interp, ft, arginfo, si, sv, max_methods)
    end
    max_methods = max_methods == typemin(Int) ? get_max_methods(interp, f, sv) : max_methods
    return abstract_call_known(interp, f, arginfo, si, sv, max_methods)
end

function sp_type_rewrap(@nospecialize(T), mi::MethodInstance, isreturn::Bool)
    isref = false
    if unwrapva(T) === Bottom
        return Bottom
    elseif isa(T, Type)
        if isa(T, DataType) && (T::DataType).name === Ref.body.name
            isref = true
            T = T.parameters[1]
            if isreturn && T === Any
                return Bottom # a return type of Ref{Any} is invalid
            end
        end
    else
        return Any
    end
    if isa(mi.def, Method)
        spsig = mi.def.sig
        if isa(spsig, UnionAll)
            if !isempty(mi.sparam_vals)
                sparam_vals = Any[isvarargtype(v) ? TypeVar(:N, Union{}, Any) :
                                  v for v in  mi.sparam_vals]
                T = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), T, spsig, sparam_vals)
                isref && isreturn && T === Any && return Bottom # catch invalid return Ref{T} where T = Any
                for v in sparam_vals
                    if isa(v, TypeVar)
                        T = UnionAll(v, T)
                    end
                end
                if has_free_typevars(T)
                    fv = ccall(:jl_find_free_typevars, Vector{Any}, (Any,), T)
                    for v in fv
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

function abstract_eval_cfunction(interp::AbstractInterpreter, e::Expr, vtypes::Union{VarTable,Nothing}, sv::AbsIntState)
    f = abstract_eval_value(interp, e.args[2], vtypes, sv)
    # rt = sp_type_rewrap(e.args[3], sv.linfo, true)
    atv = e.args[4]::SimpleVector
    at = Vector{Any}(undef, length(atv) + 1)
    at[1] = f
    for i = 1:length(atv)
        at[i + 1] = sp_type_rewrap(at[i], frame_instance(sv), false)
        at[i + 1] === Bottom && return
    end
    # this may be the wrong world for the call,
    # but some of the result is likely to be valid anyways
    # and that may help generate better codegen
    abstract_call(interp, ArgInfo(nothing, at), StmtInfo(false), sv)::Future
    rt = e.args[1]
    isa(rt, Type) || (rt = Any)
    return RTEffects(rt, Any, EFFECTS_UNKNOWN)
end

function abstract_eval_special_value(interp::AbstractInterpreter, @nospecialize(e), vtypes::Union{VarTable,Nothing}, sv::AbsIntState)
    if isa(e, SSAValue)
        return RTEffects(abstract_eval_ssavalue(e, sv), Union{}, EFFECTS_TOTAL)
    elseif isa(e, SlotNumber)
        if vtypes !== nothing
            vtyp = vtypes[slot_id(e)]
            if !vtyp.undef
                return RTEffects(vtyp.typ, Union{}, EFFECTS_TOTAL)
            end
            return RTEffects(vtyp.typ, UndefVarError, EFFECTS_THROWS)
        end
        return RTEffects(Any, UndefVarError, EFFECTS_THROWS)
    elseif isa(e, Argument)
        if vtypes !== nothing
            return RTEffects(vtypes[slot_id(e)].typ, Union{}, EFFECTS_TOTAL)
        else
            @assert isa(sv, IRInterpretationState)
            return RTEffects(sv.ir.argtypes[e.n], Union{}, EFFECTS_TOTAL) # TODO frame_argtypes(sv)[e.n] and remove the assertion
        end
    elseif isa(e, GlobalRef)
        return abstract_eval_globalref(interp, e, sv)
    end
    if isa(e, QuoteNode)
        e = e.value
    end
    effects = Effects(EFFECTS_TOTAL;
        inaccessiblememonly = is_mutation_free_argtype(typeof(e)) ? ALWAYS_TRUE : ALWAYS_FALSE)
    return RTEffects(Const(e), Union{}, effects)
end

function abstract_eval_value_expr(interp::AbstractInterpreter, e::Expr, sv::AbsIntState)
    if e.head === :call && length(e.args) ≥ 1
        # TODO: We still have non-linearized cglobal
        @assert e.args[1] === Core.tuple || e.args[1] === GlobalRef(Core, :tuple)
    else
        @assert e.head !== :(=)
        # Some of our tests expect us to handle invalid IR here and error later
        # - permit that for now.
        # @assert false "Unexpected EXPR head in value position"
        merge_effects!(interp, sv, EFFECTS_UNKNOWN)
    end
    return Any
end

function abstract_eval_value(interp::AbstractInterpreter, @nospecialize(e), vtypes::Union{VarTable,Nothing}, sv::AbsIntState)
    if isa(e, Expr)
        return abstract_eval_value_expr(interp, e, sv)
    else
        (;rt, effects) = abstract_eval_special_value(interp, e, vtypes, sv)
        merge_effects!(interp, sv, effects)
        return collect_limitations!(rt, sv)
    end
end

function collect_argtypes(interp::AbstractInterpreter, ea::Vector{Any}, vtypes::Union{VarTable,Nothing}, sv::AbsIntState)
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
    rt::Any
    exct::Any
    effects::Effects
    refinements # ::Union{Nothing,SlotRefinement,Vector{Any}}
    function RTEffects(rt, exct, effects::Effects, refinements=nothing)
        @nospecialize rt exct refinements
        return new(rt, exct, effects, refinements)
    end
end

function abstract_call(interp::AbstractInterpreter, arginfo::ArgInfo, sv::InferenceState)
    unused = call_result_unused(sv, sv.currpc)
    if unused
        add_curr_ssaflag!(sv, IR_FLAG_UNUSED)
    end
    si = StmtInfo(!unused)
    call = abstract_call(interp, arginfo, si, sv)::Future
    Future{Nothing}(call, interp, sv) do call, interp, sv
        # this only is needed for the side-effect, sequenced before any task tries to consume the return value,
        # which this will do even without returning this Future
        sv.stmt_info[sv.currpc] = call.info
        nothing
    end
    return call
end

function abstract_eval_call(interp::AbstractInterpreter, e::Expr, vtypes::Union{VarTable,Nothing},
                            sv::AbsIntState)
    ea = e.args
    argtypes = collect_argtypes(interp, ea, vtypes, sv)
    if argtypes === nothing
        return Future(RTEffects(Bottom, Any, Effects()))
    end
    arginfo = ArgInfo(ea, argtypes)
    call = abstract_call(interp, arginfo, sv)::Future
    return Future{RTEffects}(call, interp, sv) do call, interp, sv
        (; rt, exct, effects, refinements) = call
        return RTEffects(rt, exct, effects, refinements)
    end
end

function abstract_eval_new(interp::AbstractInterpreter, e::Expr, vtypes::Union{VarTable,Nothing},
                           sv::AbsIntState)
    𝕃ᵢ = typeinf_lattice(interp)
    rt, isexact = instanceof_tfunc(abstract_eval_value(interp, e.args[1], vtypes, sv), true)
    ut = unwrap_unionall(rt)
    exct = Union{ErrorException,TypeError}
    if isa(ut, DataType) && !isabstracttype(ut)
        ismutable = ismutabletype(ut)
        fcount = datatype_fieldcount(ut)
        nargs = length(e.args) - 1
        has_any_uninitialized = (fcount === nothing || (fcount > nargs && (let t = rt
                any(i::Int -> !is_undefref_fieldtype(fieldtype(t, i)), (nargs+1):fcount)
            end)))
        if has_any_uninitialized
            # allocation with undefined field is inconsistent always
            consistent = ALWAYS_FALSE
        elseif ismutable
            # mutable allocation isn't `:consistent`, but we still have a chance that
            # return type information later refines the `:consistent`-cy of the method
            consistent = CONSISTENT_IF_NOTRETURNED
        else
            consistent = ALWAYS_TRUE # immutable allocation is consistent
        end
        if isconcretedispatch(rt)
            nothrow = true
            @assert fcount !== nothing && fcount ≥ nargs "malformed :new expression" # syntactically enforced by the front-end
            ats = Vector{Any}(undef, nargs)
            local anyrefine = false
            local allconst = true
            for i = 1:nargs
                at = widenslotwrapper(abstract_eval_value(interp, e.args[i+1], vtypes, sv))
                ft = fieldtype(rt, i)
                nothrow && (nothrow = ⊑(𝕃ᵢ, at, ft))
                at = tmeet(𝕃ᵢ, at, ft)
                at === Bottom && return RTEffects(Bottom, TypeError, EFFECTS_THROWS)
                if ismutable && !isconst(rt, i)
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
            if fcount == nargs && consistent === ALWAYS_TRUE && allconst
                argvals = Vector{Any}(undef, nargs)
                for j in 1:nargs
                    argvals[j] = (ats[j]::Const).val
                end
                rt = Const(ccall(:jl_new_structv, Any, (Any, Ptr{Cvoid}, UInt32), rt, argvals, nargs))
            elseif anyrefine || nargs > datatype_min_ninitialized(rt)
                # propagate partially initialized struct as `PartialStruct` when:
                # - any refinement information is available (`anyrefine`), or when
                # - `nargs` is greater than `n_initialized` derived from the struct type
                #   information alone
                rt = PartialStruct(rt, ats)
            end
        else
            rt = refine_partial_type(rt)
            nothrow = false
        end
    else
        consistent = ALWAYS_FALSE
        nothrow = false
    end
    nothrow && (exct = Union{})
    effects = Effects(EFFECTS_TOTAL; consistent, nothrow)
    return RTEffects(rt, exct, effects)
end

function abstract_eval_splatnew(interp::AbstractInterpreter, e::Expr, vtypes::Union{VarTable,Nothing},
                                sv::AbsIntState)
    𝕃ᵢ = typeinf_lattice(interp)
    rt, isexact = instanceof_tfunc(abstract_eval_value(interp, e.args[1], vtypes, sv), true)
    nothrow = false
    if length(e.args) == 2 && isconcretedispatch(rt) && !ismutabletype(rt)
        at = abstract_eval_value(interp, e.args[2], vtypes, sv)
        n = fieldcount(rt)
        if (isa(at, Const) && isa(at.val, Tuple) && n == length(at.val::Tuple) &&
            (let t = rt, at = at
                all(i::Int -> getfield(at.val::Tuple, i) isa fieldtype(t, i), 1:n)
            end))
            nothrow = isexact
            rt = Const(ccall(:jl_new_structt, Any, (Any, Any), rt, at.val))
        elseif (isa(at, PartialStruct) && ⊑(𝕃ᵢ, at, Tuple) && n > 0 &&
                n == length(at.fields::Vector{Any}) && !isvarargtype(at.fields[end]) &&
                (let t = rt, at = at
                    all(i::Int -> ⊑(𝕃ᵢ, (at.fields::Vector{Any})[i], fieldtype(t, i)), 1:n)
                end))
            nothrow = isexact
            rt = PartialStruct(rt, at.fields::Vector{Any})
        end
    else
        rt = refine_partial_type(rt)
    end
    consistent = !ismutabletype(rt) ? ALWAYS_TRUE : CONSISTENT_IF_NOTRETURNED
    effects = Effects(EFFECTS_TOTAL; consistent, nothrow)
    return RTEffects(rt, Any, effects)
end

function abstract_eval_new_opaque_closure(interp::AbstractInterpreter, e::Expr, vtypes::Union{VarTable,Nothing},
                                          sv::AbsIntState)
    𝕃ᵢ = typeinf_lattice(interp)
    rt = Union{}
    effects = Effects() # TODO
    if length(e.args) >= 5
        ea = e.args
        argtypes = collect_argtypes(interp, ea, vtypes, sv)
        if argtypes === nothing
            rt = Bottom
            effects = EFFECTS_THROWS
        else
            mi = frame_instance(sv)
            rt = opaque_closure_tfunc(𝕃ᵢ, argtypes[1], argtypes[2], argtypes[3],
                argtypes[5], argtypes[6:end], mi)
            if ea[4] !== true && isa(rt, PartialOpaque)
                rt = widenconst(rt)
                # Propagation of PartialOpaque disabled
            end
            if isa(rt, PartialOpaque) && isa(sv, InferenceState) && !call_result_unused(sv, sv.currpc)
                # Infer this now so that the specialization is available to
                # optimization.
                argtypes = most_general_argtypes(rt)
                pushfirst!(argtypes, rt.env)
                callinfo = abstract_call_opaque_closure(interp, rt,
                    ArgInfo(nothing, argtypes), StmtInfo(true), sv, #=check=#false)::Future
                Future{Nothing}(callinfo, interp, sv) do callinfo, interp, sv
                    sv.stmt_info[sv.currpc] = OpaqueClosureCreateInfo(callinfo)
                    nothing
                end
            end
        end
    end
    return Future(RTEffects(rt, Any, effects))
end

function abstract_eval_copyast(interp::AbstractInterpreter, e::Expr, vtypes::Union{VarTable,Nothing},
                               sv::AbsIntState)
    effects = EFFECTS_UNKNOWN
    rt = abstract_eval_value(interp, e.args[1], vtypes, sv)
    if rt isa Const && rt.val isa Expr
        # `copyast` makes copies of Exprs
        rt = Expr
    end
    return RTEffects(rt, Any, effects)
end

function abstract_eval_isdefined(interp::AbstractInterpreter, e::Expr, vtypes::Union{VarTable,Nothing},
                                 sv::AbsIntState)
    sym = e.args[1]
    rt = Bool
    effects = EFFECTS_TOTAL
    exct = Union{}
    isa(sym, Symbol) && (sym = GlobalRef(frame_module(sv), sym))
    if isa(sym, SlotNumber) && vtypes !== nothing
        vtyp = vtypes[slot_id(sym)]
        if vtyp.typ === Bottom
            rt = Const(false) # never assigned previously
        elseif !vtyp.undef
            rt = Const(true) # definitely assigned previously
        else # form `Conditional` to refine `vtyp.undef` in the then branch
            rt = Conditional(sym, vtyp.typ, vtyp.typ; isdefined=true)
        end
    elseif isa(sym, GlobalRef)
        if InferenceParams(interp).assume_bindings_static
            rt = Const(isdefined_globalref(sym))
        elseif isdefinedconst_globalref(sym)
            rt = Const(true)
        else
            effects = Effects(EFFECTS_TOTAL; consistent=ALWAYS_FALSE)
        end
    elseif isexpr(sym, :static_parameter)
        n = sym.args[1]::Int
        if 1 <= n <= length(sv.sptypes)
            sp = sv.sptypes[n]
            if !sp.undef
                rt = Const(true)
            elseif sp.typ === Bottom
                rt = Const(false)
            end
        end
    else
        effects = EFFECTS_UNKNOWN
    end
    return RTEffects(rt, exct, effects)
end

function abstract_eval_throw_undef_if_not(interp::AbstractInterpreter, e::Expr, vtypes::Union{VarTable,Nothing}, sv::AbsIntState)
    condt = abstract_eval_value(interp, e.args[2], vtypes, sv)
    condval = maybe_extract_const_bool(condt)
    rt = Nothing
    exct = UndefVarError
    effects = EFFECTS_THROWS
    if condval isa Bool
        if condval
            effects = EFFECTS_TOTAL
            exct = Union{}
        else
            rt = Union{}
        end
    elseif !hasintersect(widenconst(condt), Bool)
        rt = Union{}
    end
    return RTEffects(rt, exct, effects)
end

function abstract_eval_the_exception(::AbstractInterpreter, sv::InferenceState)
    (;handlers, handler_at) = sv.handler_info::HandlerInfo
    return the_exception_info(handlers[handler_at[sv.currpc][2]].exct)
end
abstract_eval_the_exception(::AbstractInterpreter, ::IRInterpretationState) = the_exception_info(Any)
the_exception_info(@nospecialize t) = RTEffects(t, Union{}, Effects(EFFECTS_TOTAL; consistent=ALWAYS_FALSE))

function abstract_eval_static_parameter(::AbstractInterpreter, e::Expr, sv::AbsIntState)
    n = e.args[1]::Int
    nothrow = false
    if 1 <= n <= length(sv.sptypes)
        sp = sv.sptypes[n]
        rt = sp.typ
        nothrow = !sp.undef
    else
        rt = Any
    end
    exct = nothrow ? Union{} : UndefVarError
    effects = Effects(EFFECTS_TOTAL; nothrow)
    return RTEffects(rt, exct, effects)
end

function abstract_eval_statement_expr(interp::AbstractInterpreter, e::Expr, vtypes::Union{VarTable,Nothing},
                                      sv::AbsIntState)::Future{RTEffects}
    ehead = e.head
    if ehead === :call
        return abstract_eval_call(interp, e, vtypes, sv)
    elseif ehead === :new
        return abstract_eval_new(interp, e, vtypes, sv)
    elseif ehead === :splatnew
        return abstract_eval_splatnew(interp, e, vtypes, sv)
    elseif ehead === :new_opaque_closure
        return abstract_eval_new_opaque_closure(interp, e, vtypes, sv)
    elseif ehead === :foreigncall
        return abstract_eval_foreigncall(interp, e, vtypes, sv)
    elseif ehead === :cfunction
        return abstract_eval_cfunction(interp, e, vtypes, sv)
    elseif ehead === :method
        rt = (length(e.args) == 1) ? Any : Nothing
        return RTEffects(rt, Any, EFFECTS_UNKNOWN)
    elseif ehead === :copyast
        return abstract_eval_copyast(interp, e, vtypes, sv)
    elseif ehead === :invoke || ehead === :invoke_modify
        error("type inference data-flow error: tried to double infer a function")
    elseif ehead === :isdefined
        return abstract_eval_isdefined(interp, e, vtypes, sv)
    elseif ehead === :throw_undef_if_not
        return abstract_eval_throw_undef_if_not(interp, e, vtypes, sv)
    elseif ehead === :boundscheck
        return RTEffects(Bool, Union{}, Effects(EFFECTS_TOTAL; consistent=ALWAYS_FALSE))
    elseif ehead === :the_exception
        return abstract_eval_the_exception(interp, sv)
    elseif ehead === :static_parameter
        return abstract_eval_static_parameter(interp, e, sv)
    elseif ehead === :gc_preserve_begin || ehead === :aliasscope
        return RTEffects(Any, Union{}, Effects(EFFECTS_TOTAL; consistent=ALWAYS_FALSE, effect_free=EFFECT_FREE_GLOBALLY))
    elseif ehead === :gc_preserve_end || ehead === :leave || ehead === :pop_exception ||
           ehead === :global || ehead === :popaliasscope
        return RTEffects(Nothing, Union{}, Effects(EFFECTS_TOTAL; effect_free=EFFECT_FREE_GLOBALLY))
    elseif ehead === :globaldecl
        return RTEffects(Nothing, Any, EFFECTS_UNKNOWN)
    elseif ehead === :thunk
        return RTEffects(Any, Any, Effects())
    end
    # N.B.: abstract_eval_value_expr can modify the global effects, but
    # we move out any arguments with effects during SSA construction later
    # and recompute the effects.
    rt = abstract_eval_value_expr(interp, e, sv)
    return RTEffects(rt, Any, EFFECTS_TOTAL)
end

# refine the result of instantiation of partially-known type `t` if some invariant can be assumed
function refine_partial_type(@nospecialize t)
    t′ = unwrap_unionall(t)
    if isa(t′, DataType) && t′.name === _NAMEDTUPLE_NAME && length(t′.parameters) == 2 &&
        (t′.parameters[1] === () || t′.parameters[2] === Tuple{})
        # if the first/second parameter of `NamedTuple` is known to be empty,
        # the second/first argument should also be empty tuple type,
        # so refine it here
        return Const((;))
    end
    return t
end

function abstract_eval_foreigncall(interp::AbstractInterpreter, e::Expr, vtypes::Union{VarTable,Nothing}, sv::AbsIntState)
    mi = frame_instance(sv)
    t = sp_type_rewrap(e.args[2], mi, true)
    for i = 3:length(e.args)
        if abstract_eval_value(interp, e.args[i], vtypes, sv) === Bottom
            return RTEffects(Bottom, Any, EFFECTS_THROWS)
        end
    end
    effects = foreigncall_effects(e) do @nospecialize x
        abstract_eval_value(interp, x, vtypes, sv)
    end
    cconv = e.args[5]
    if isa(cconv, QuoteNode) && (v = cconv.value; isa(v, Tuple{Symbol, UInt16}))
        override = decode_effects_override(v[2])
        effects = override_effects(effects, override)
    end
    return RTEffects(t, Any, effects)
end

function abstract_eval_phi(interp::AbstractInterpreter, phi::PhiNode, vtypes::Union{VarTable,Nothing}, sv::AbsIntState)
    rt = Union{}
    for i in 1:length(phi.values)
        isassigned(phi.values, i) || continue
        val = phi.values[i]
        # N.B.: Phi arguments are restricted to not have effects, so we can drop
        # them here safely.
        thisval = abstract_eval_special_value(interp, val, vtypes, sv).rt
        rt = tmerge(typeinf_lattice(interp), rt, thisval)
    end
    return rt
end

function stmt_taints_inbounds_consistency(sv::AbsIntState)
    propagate_inbounds(sv) && return true
    return has_curr_ssaflag(sv, IR_FLAG_INBOUNDS)
end

function merge_override_effects!(interp::AbstractInterpreter, effects::Effects, sv::InferenceState)
    # N.B.: This only applies to the effects of the statement itself.
    # It is possible for arguments (GlobalRef/:static_parameter) to throw,
    # but these will be recomputed during SSA construction later.
    override = decode_statement_effects_override(sv)
    effects = override_effects(effects, override)
    set_curr_ssaflag!(sv, flags_for_effects(effects), IR_FLAGS_EFFECTS)
    merge_effects!(interp, sv, effects)
    return effects
end

function abstract_eval_statement(interp::AbstractInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    @assert !isa(e, Union{Expr, PhiNode, NewvarNode})
end

function override_effects(effects::Effects, override::EffectsOverride)
    return Effects(effects;
        consistent = override.consistent ? ALWAYS_TRUE : effects.consistent,
        effect_free = override.effect_free ? ALWAYS_TRUE : effects.effect_free,
        nothrow = override.nothrow ? true : effects.nothrow,
        terminates = override.terminates_globally ? true : effects.terminates,
        notaskstate = override.notaskstate ? true : effects.notaskstate,
        inaccessiblememonly = override.inaccessiblememonly ? ALWAYS_TRUE : effects.inaccessiblememonly,
        noub = override.noub ? ALWAYS_TRUE :
            (override.noub_if_noinbounds && effects.noub !== ALWAYS_TRUE) ? NOUB_IF_NOINBOUNDS :
            effects.noub,
        nortcall = override.nortcall ? true : effects.nortcall)
end

isdefined_globalref(g::GlobalRef) = !iszero(ccall(:jl_globalref_boundp, Cint, (Any,), g))
isdefinedconst_globalref(g::GlobalRef) = isconst(g) && isdefined_globalref(g)

function abstract_eval_globalref_type(g::GlobalRef)
    if isdefinedconst_globalref(g)
        return Const(ccall(:jl_get_globalref_value, Any, (Any,), g))
    end
    ty = ccall(:jl_get_binding_type, Any, (Any, Any), g.mod, g.name)
    ty === nothing && return Any
    return ty
end
abstract_eval_global(M::Module, s::Symbol) = abstract_eval_globalref_type(GlobalRef(M, s))

function abstract_eval_globalref(interp::AbstractInterpreter, g::GlobalRef, sv::AbsIntState)
    rt = abstract_eval_globalref_type(g)
    consistent = inaccessiblememonly = ALWAYS_FALSE
    nothrow = false
    if isa(rt, Const)
        consistent = ALWAYS_TRUE
        nothrow = true
        if is_mutation_free_argtype(rt)
            inaccessiblememonly = ALWAYS_TRUE
        end
    elseif InferenceParams(interp).assume_bindings_static
        consistent = inaccessiblememonly = ALWAYS_TRUE
        if isdefined_globalref(g)
            nothrow = true
        else
            rt = Union{}
        end
    elseif isdefinedconst_globalref(g)
        nothrow = true
    end
    return RTEffects(rt, nothrow ? Union{} : UndefVarError, Effects(EFFECTS_TOTAL; consistent, nothrow, inaccessiblememonly))
end

function handle_global_assignment!(interp::AbstractInterpreter, frame::InferenceState, lhs::GlobalRef, @nospecialize(newty))
    effect_free = ALWAYS_FALSE
    nothrow = global_assignment_nothrow(lhs.mod, lhs.name, ignorelimited(newty))
    inaccessiblememonly = ALWAYS_FALSE
    if !nothrow
        sub_curr_ssaflag!(frame, IR_FLAG_NOTHROW)
    end
    sub_curr_ssaflag!(frame, IR_FLAG_EFFECT_FREE)
    merge_effects!(interp, frame, Effects(EFFECTS_TOTAL; effect_free, nothrow, inaccessiblememonly))
    return nothing
end

abstract_eval_ssavalue(s::SSAValue, sv::InferenceState) = abstract_eval_ssavalue(s, sv.ssavaluetypes)

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

@nospecializeinfer function widenreturn(@nospecialize(rt), info::BestguessInfo)
    return widenreturn(typeinf_lattice(info.interp), rt, info)
end

@nospecializeinfer function widenreturn(𝕃ᵢ::AbstractLattice, @nospecialize(rt), info::BestguessInfo)
    return widenreturn(widenlattice(𝕃ᵢ), rt, info)
end
@nospecializeinfer function widenreturn_noslotwrapper(𝕃ᵢ::AbstractLattice, @nospecialize(rt), info::BestguessInfo)
    return widenreturn_noslotwrapper(widenlattice(𝕃ᵢ), rt, info)
end

@nospecializeinfer function widenreturn(𝕃ᵢ::MustAliasesLattice, @nospecialize(rt), info::BestguessInfo)
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

@nospecializeinfer function widenreturn(𝕃ᵢ::ConditionalsLattice, @nospecialize(rt), info::BestguessInfo)
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
@nospecializeinfer function bool_rt_to_conditional(@nospecialize(rt), info::BestguessInfo)
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
        for slot_id = 1:Int(info.nargs)
            rt = bool_rt_to_conditional(rt, slot_id, info)
            rt isa InterConditional && break
        end
    end
    return rt
end
@nospecializeinfer function bool_rt_to_conditional(@nospecialize(rt), slot_id::Int, info::BestguessInfo)
    ⊑ᵢ = ⊑(typeinf_lattice(info.interp))
    old = info.slottypes[slot_id]
    new = widenslotwrapper(info.changes[slot_id].typ) # avoid nested conditional
    if isvarargtype(old) || isvarargtype(new)
        return rt
    end
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

@nospecializeinfer function widenreturn(𝕃ᵢ::PartialsLattice, @nospecialize(rt), info::BestguessInfo)
    return widenreturn_partials(𝕃ᵢ, rt, info)
end
@nospecializeinfer function widenreturn_noslotwrapper(𝕃ᵢ::PartialsLattice, @nospecialize(rt), info::BestguessInfo)
    return widenreturn_partials(𝕃ᵢ, rt, info)
end
@nospecializeinfer function widenreturn_partials(𝕃ᵢ::PartialsLattice, @nospecialize(rt), info::BestguessInfo)
    if isa(rt, PartialStruct)
        fields = copy(rt.fields)
        anyrefine = !isvarargtype(rt.fields[end]) &&
            length(rt.fields) > datatype_min_ninitialized(unwrap_unionall(rt.typ))
        𝕃 = typeinf_lattice(info.interp)
        ⊏ = strictpartialorder(𝕃)
        for i in 1:length(fields)
            a = fields[i]
            a = isvarargtype(a) ? a : widenreturn_noslotwrapper(𝕃, a, info)
            if !anyrefine
                # TODO: consider adding && const_prop_profitable(a) here?
                anyrefine = has_extended_info(a) || a ⊏ fieldtype(rt.typ, i)
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

@nospecializeinfer function widenreturn(::ConstsLattice, @nospecialize(rt), ::BestguessInfo)
    return widenreturn_consts(rt)
end
@nospecializeinfer function widenreturn_noslotwrapper(::ConstsLattice, @nospecialize(rt), ::BestguessInfo)
    return widenreturn_consts(rt)
end
@nospecializeinfer function widenreturn_consts(@nospecialize(rt))
    isa(rt, Const) && return rt
    return widenconst(rt)
end

@nospecializeinfer function widenreturn(::JLTypeLattice, @nospecialize(rt), ::BestguessInfo)
    return widenconst(rt)
end
@nospecializeinfer function widenreturn_noslotwrapper(::JLTypeLattice, @nospecialize(rt), ::BestguessInfo)
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

function update_bestguess!(interp::AbstractInterpreter, frame::InferenceState,
                           currstate::VarTable, @nospecialize(rt))
    bestguess = frame.bestguess
    nargs = narguments(frame, #=include_va=#false)
    slottypes = frame.slottypes
    rt = widenreturn(rt, BestguessInfo(interp, bestguess, nargs, slottypes, currstate))
    # narrow representation of bestguess slightly to prepare for tmerge with rt
    if rt isa InterConditional && bestguess isa Const
        slot_id = rt.slot
        old_id_type = widenconditional(slottypes[slot_id])
        if bestguess.val === true && rt.elsetype !== Bottom
            bestguess = InterConditional(slot_id, old_id_type, Bottom)
        elseif bestguess.val === false && rt.thentype !== Bottom
            bestguess = InterConditional(slot_id, Bottom, old_id_type)
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
    𝕃ₚ = ipo_lattice(interp)
    if !⊑(𝕃ₚ, rt, bestguess)
        # TODO: if bestguess isa InterConditional && !interesting(bestguess); bestguess = widenconditional(bestguess); end
        frame.bestguess = tmerge(𝕃ₚ, bestguess, rt) # new (wider) return type for frame
        return true
    else
        return false
    end
end

function update_exc_bestguess!(interp::AbstractInterpreter, @nospecialize(exct), frame::InferenceState)
    𝕃ₚ = ipo_lattice(interp)
    handler = gethandler(frame)
    if handler === nothing
        if !⊑(𝕃ₚ, exct, frame.exc_bestguess)
            frame.exc_bestguess = tmerge(𝕃ₚ, frame.exc_bestguess, exct)
            update_cycle_worklists!(frame) do caller::InferenceState, caller_pc::Int
                caller_handler = gethandler(caller, caller_pc)
                caller_exct = caller_handler === nothing ?
                    caller.exc_bestguess : caller_handler.exct
                return caller_exct !== Any
            end
        end
    else
        if !⊑(𝕃ₚ, exct, handler.exct)
            handler.exct = tmerge(𝕃ₚ, handler.exct, exct)
            enter = frame.src.code[handler.enter_idx]::EnterNode
            exceptbb = block_for_inst(frame.cfg, enter.catch_dest)
            push!(frame.ip, exceptbb)
        end
    end
end

function propagate_to_error_handler!(currstate::VarTable, frame::InferenceState, 𝕃ᵢ::AbstractLattice)
    # If this statement potentially threw, propagate the currstate to the
    # exception handler, BEFORE applying any state changes.
    curr_hand = gethandler(frame)
    if curr_hand !== nothing
        enter = frame.src.code[curr_hand.enter_idx]::EnterNode
        exceptbb = block_for_inst(frame.cfg, enter.catch_dest)
        if update_bbstate!(𝕃ᵢ, frame, exceptbb, currstate)
            push!(frame.ip, exceptbb)
        end
    end
end

function update_cycle_worklists!(callback, frame::InferenceState)
    for (caller, caller_pc) in frame.cycle_backedges
        if callback(caller, caller_pc)
            push!(caller.ip, block_for_inst(caller.cfg, caller_pc))
        end
    end
end

# make as much progress on `frame` as possible (without handling cycles)
struct CurrentState
    result::Future
    currstate::VarTable
    bbstart::Int
    bbend::Int
    CurrentState(result::Future, currstate::VarTable, bbstart::Int, bbend::Int) = new(result, currstate, bbstart, bbend)
    CurrentState() = new()
end
function typeinf_local(interp::AbstractInterpreter, frame::InferenceState, nextresult::CurrentState)
    @assert !is_inferred(frame)
    W = frame.ip
    ssavaluetypes = frame.ssavaluetypes
    bbs = frame.cfg.blocks
    nbbs = length(bbs)
    𝕃ᵢ = typeinf_lattice(interp)
    states = frame.bb_vartables
    currbb = frame.currbb
    currpc = frame.currpc

    if isdefined(nextresult, :result)
        # for reasons that are fairly unclear, some state is arbitrarily on the stack instead in the InferenceState as normal
        bbstart = nextresult.bbstart
        bbend = nextresult.bbend
        currstate = nextresult.currstate
        @goto injectresult
    end

    if currbb != 1
        currbb = frame.currbb = _bits_findnext(W.bits, 1)::Int # next basic block
    end
    currstate = copy(states[currbb]::VarTable)
    while currbb <= nbbs
        delete!(W, currbb)
        bbstart = first(bbs[currbb].stmts)
        bbend = last(bbs[currbb].stmts)

        currpc = bbstart - 1
        while currpc < bbend
            currpc += 1
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
                    add_curr_ssaflag!(frame, IR_FLAG_NOTHROW)
                    @goto branch
                elseif isa(stmt, GotoIfNot)
                    condx = stmt.cond
                    condslot = ssa_def_slot(condx, frame)
                    condt = abstract_eval_value(interp, condx, currstate, frame)
                    if condt === Bottom
                        ssavaluetypes[currpc] = Bottom
                        empty!(frame.pclimitations)
                        @goto find_next_bb
                    end
                    orig_condt = condt
                    if !(isa(condt, Const) || isa(condt, Conditional)) && isa(condslot, SlotNumber)
                        # if this non-`Conditional` object is a slot, we form and propagate
                        # the conditional constraint on it
                        condt = Conditional(condslot, Const(true), Const(false))
                    end
                    condval = maybe_extract_const_bool(condt)
                    nothrow = (condval !== nothing) || ⊑(𝕃ᵢ, orig_condt, Bool)
                    if nothrow
                        add_curr_ssaflag!(frame, IR_FLAG_NOTHROW)
                    else
                        update_exc_bestguess!(interp, TypeError, frame)
                        propagate_to_error_handler!(currstate, frame, 𝕃ᵢ)
                        merge_effects!(interp, frame, EFFECTS_THROWS)
                    end

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
                        if !nothrow && !hasintersect(widenconst(orig_condt), Bool)
                            ssavaluetypes[currpc] = Bottom
                            @goto find_next_bb
                        end

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
                        end

                        # We continue with the true branch, but process the false
                        # branch here.
                        if isa(condt, Conditional)
                            else_change = conditional_change(𝕃ᵢ, currstate, condt, #=then_or_else=#false)
                            if else_change !== nothing
                                elsestate = copy(currstate)
                                stoverwrite1!(elsestate, else_change)
                            elseif condslot isa SlotNumber
                                elsestate = copy(currstate)
                            else
                                elsestate = currstate
                            end
                            if condslot isa SlotNumber # refine the type of this conditional object itself for this else branch
                                stoverwrite1!(elsestate, condition_object_change(currstate, condt, condslot, #=then_or_else=#false))
                            end
                            else_changed = update_bbstate!(𝕃ᵢ, frame, falsebb, elsestate)
                            then_change = conditional_change(𝕃ᵢ, currstate, condt, #=then_or_else=#true)
                            thenstate = currstate
                            if then_change !== nothing
                                stoverwrite1!(thenstate, then_change)
                            end
                            if condslot isa SlotNumber # refine the type of this conditional object itself for this then branch
                                stoverwrite1!(thenstate, condition_object_change(currstate, condt, condslot, #=then_or_else=#true))
                            end
                        else
                            else_changed = update_bbstate!(𝕃ᵢ, frame, falsebb, currstate)
                        end
                        if else_changed
                            handle_control_backedge!(interp, frame, currpc, stmt.dest)
                            push!(W, falsebb)
                        end
                        @goto fallthrough
                    end
                elseif isa(stmt, ReturnNode)
                    rt = abstract_eval_value(interp, stmt.val, currstate, frame)
                    if update_bestguess!(interp, frame, currstate, rt)
                        update_cycle_worklists!(frame) do caller::InferenceState, caller_pc::Int
                            # no reason to revisit if that call-site doesn't affect the final result
                            return caller.ssavaluetypes[caller_pc] !== Any
                        end
                    end
                    ssavaluetypes[currpc] = Any
                    @goto find_next_bb
                elseif isa(stmt, EnterNode)
                    ssavaluetypes[currpc] = Any
                    add_curr_ssaflag!(frame, IR_FLAG_NOTHROW)
                    if isdefined(stmt, :scope)
                        scopet = abstract_eval_value(interp, stmt.scope, currstate, frame)
                        handler = gethandler(frame, currpc + 1)::TryCatchFrame
                        @assert handler.scopet !== nothing
                        if !⊑(𝕃ᵢ, scopet, handler.scopet)
                            handler.scopet = tmerge(𝕃ᵢ, scopet, handler.scopet)
                            if isdefined(handler, :scope_uses)
                                for bb in handler.scope_uses
                                    push!(W, bb)
                                end
                            end
                        end
                    end
                    @goto fallthrough
                elseif isexpr(stmt, :leave)
                    ssavaluetypes[currpc] = Any
                    @goto fallthrough
                end
                # Fall through terminator - treat as regular stmt
            end
            # Process non control-flow statements
            @assert isempty(frame.tasks)
            rt = nothing
            exct = Bottom
            changes = nothing
            refinements = nothing
            effects = nothing
            if isa(stmt, NewvarNode)
                changes = StateUpdate(stmt.slot, VarState(Bottom, true))
            elseif isa(stmt, PhiNode)
                add_curr_ssaflag!(frame, IR_FLAGS_REMOVABLE)
                # Implement convergence for PhiNodes. In particular, PhiNodes need to tmerge over
                # the incoming values from all iterations, but `abstract_eval_phi` will only tmerge
                # over the first and last iterations. By tmerging in the current old_rt, we ensure that
                # we will not lose an intermediate value.
                rt = abstract_eval_phi(interp, stmt, currstate, frame)
                old_rt = frame.ssavaluetypes[currpc]
                rt = old_rt === NOT_FOUND ? rt : tmerge(typeinf_lattice(interp), old_rt, rt)
            else
                lhs = nothing
                if isexpr(stmt, :(=))
                    lhs = stmt.args[1]
                    stmt = stmt.args[2]
                end
                if !isa(stmt, Expr)
                    (; rt, exct, effects, refinements) = abstract_eval_special_value(interp, stmt, currstate, frame)
                else
                    hd = stmt.head
                    if hd === :method
                        fname = stmt.args[1]
                        if isa(fname, SlotNumber)
                            changes = StateUpdate(fname, VarState(Any, false))
                        end
                    elseif (hd === :code_coverage_effect || (
                            hd !== :boundscheck && # :boundscheck can be narrowed to Bool
                            is_meta_expr(stmt)))
                        rt = Nothing
                    else
                        result = abstract_eval_statement_expr(interp, stmt, currstate, frame)::Future
                        if !isready(result) || !isempty(frame.tasks)
                            return CurrentState(result, currstate, bbstart, bbend)
                            @label injectresult
                            # reload local variables
                            stmt = frame.src.code[currpc]
                            changes = nothing
                            lhs = nothing
                            if isexpr(stmt, :(=))
                                lhs = stmt.args[1]
                                stmt = stmt.args[2]
                            end
                            result = nextresult.result::Future{RTEffects}
                        end
                        result = result[]
                        (; rt, exct, effects, refinements) = result
                        if effects.noub === NOUB_IF_NOINBOUNDS
                            if has_curr_ssaflag(frame, IR_FLAG_INBOUNDS)
                                effects = Effects(effects; noub=ALWAYS_FALSE)
                            elseif !propagate_inbounds(frame)
                                # The callee read our inbounds flag, but unless we propagate inbounds,
                                # we ourselves don't read our parent's inbounds.
                                effects = Effects(effects; noub=ALWAYS_TRUE)
                            end
                        end
                        @assert !isa(rt, TypeVar) "unhandled TypeVar"
                        rt = maybe_singleton_const(rt)
                        if !isempty(frame.pclimitations)
                            if rt isa Const || rt === Union{}
                                empty!(frame.pclimitations)
                            else
                                rt = LimitedAccuracy(rt, frame.pclimitations)
                                frame.pclimitations = IdSet{InferenceState}()
                            end
                        end
                    end
                end
                effects === nothing || merge_override_effects!(interp, effects, frame)
                if lhs !== nothing && rt !== Bottom
                    if isa(lhs, SlotNumber)
                        changes = StateUpdate(lhs, VarState(rt, false))
                    elseif isa(lhs, GlobalRef)
                        handle_global_assignment!(interp, frame, lhs, rt)
                    elseif !isa(lhs, SSAValue)
                        merge_effects!(interp, frame, EFFECTS_UNKNOWN)
                    end
                end
            end
            if !has_curr_ssaflag(frame, IR_FLAG_NOTHROW)
                if exct !== Union{}
                    update_exc_bestguess!(interp, exct, frame)
                    # TODO: assert that these conditions match. For now, we assume the `nothrow` flag
                    # to be correct, but allow the exct to be an over-approximation.
                end
                propagate_to_error_handler!(currstate, frame, 𝕃ᵢ)
            end
            if rt === Bottom
                ssavaluetypes[currpc] = Bottom
                # Special case: Bottom-typed PhiNodes do not error (but must also be unused)
                if isa(stmt, PhiNode)
                    continue
                end
                @goto find_next_bb
            end
            if changes !== nothing
                stoverwrite1!(currstate, changes)
            end
            if refinements isa SlotRefinement
                apply_refinement!(𝕃ᵢ, refinements.slot, refinements.typ, currstate, changes)
            elseif refinements isa Vector{Any}
                for i = 1:length(refinements)
                    newtyp = refinements[i]
                    newtyp === nothing && continue
                    apply_refinement!(𝕃ᵢ, SlotNumber(i), newtyp, currstate, changes)
                end
            end
            if rt === nothing
                ssavaluetypes[currpc] = Any
                continue
            end
            record_ssa_assign!(𝕃ᵢ, currpc, rt, frame)
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

    return CurrentState()
end

function apply_refinement!(𝕃ᵢ::AbstractLattice, slot::SlotNumber, @nospecialize(newtyp),
                           currstate::VarTable, currchanges::Union{Nothing,StateUpdate})
    if currchanges !== nothing && currchanges.var == slot
        return # type propagation from statement (like assignment) should have the precedence
    end
    vtype = currstate[slot_id(slot)]
    oldtyp = vtype.typ
    ⊏ = strictpartialorder(𝕃ᵢ)
    if newtyp ⊏ oldtyp
        stmtupdate = StateUpdate(slot, VarState(newtyp, vtype.undef))
        stoverwrite1!(currstate, stmtupdate)
    end
end

function conditional_change(𝕃ᵢ::AbstractLattice, currstate::VarTable, condt::Conditional, then_or_else::Bool)
    vtype = currstate[condt.slot]
    oldtyp = vtype.typ
    newtyp = then_or_else ? condt.thentype : condt.elsetype
    if iskindtype(newtyp)
        # this code path corresponds to the special handling for `isa(x, iskindtype)` check
        # implemented within `abstract_call_builtin`
    elseif ⊑(𝕃ᵢ, ignorelimited(newtyp), ignorelimited(oldtyp))
        # approximate test for `typ ∩ oldtyp` being better than `oldtyp`
        # since we probably formed these types with `typesubstract`,
        # the comparison is likely simple
    else
        return nothing
    end
    if oldtyp isa LimitedAccuracy
        # typ is better unlimited, but we may still need to compute the tmeet with the limit
        # "causes" since we ignored those in the comparison
        newtyp = tmerge(𝕃ᵢ, newtyp, LimitedAccuracy(Bottom, oldtyp.causes))
    end
    # if this `Conditional` is from from `@isdefined condt.slot`, refine its `undef` information
    newundef = condt.isdefined ? !then_or_else : vtype.undef
    return StateUpdate(SlotNumber(condt.slot), VarState(newtyp, newundef), #=conditional=#true)
end

function condition_object_change(currstate::VarTable, condt::Conditional,
                                 condslot::SlotNumber, then_or_else::Bool)
    vtype = currstate[slot_id(condslot)]
    newcondt = Conditional(condt.slot,
        then_or_else ? condt.thentype : Union{},
        then_or_else ? Union{} : condt.elsetype)
    return StateUpdate(condslot, VarState(newcondt, vtype.undef))
end

# make as much progress on `frame` as possible (by handling cycles)
warnlength::Int = 2500
function typeinf(interp::AbstractInterpreter, frame::InferenceState)
    callstack = frame.callstack::Vector{AbsIntState}
    nextstates = CurrentState[]
    takenext = frame.frameid
    minwarn = warnlength
    takeprev = 0
    while takenext >= frame.frameid
        callee = takenext == 0 ? frame : callstack[takenext]::InferenceState
        if !isempty(callstack)
            if length(callstack) - frame.frameid >= minwarn
                topmethod = callstack[1].linfo
                topmethod.def isa Method || (topmethod = callstack[2].linfo)
                print(Core.stderr, "info: inference of ", topmethod, " exceeding ", length(callstack), " frames (may be slow).\n")
                minwarn *= 2
            end
            topcallee = (callstack[end]::InferenceState)
            if topcallee.cycleid != callee.cycleid
                callee = topcallee
                takenext = length(callstack)
            end
        end
        nextstateid = takenext + 1 - frame.frameid
        while length(nextstates) < nextstateid
            push!(nextstates, CurrentState())
        end
        if doworkloop(interp, callee)
            # First drain the workloop. Note that since some scheduled work doesn't
            # affect the result (e.g. cfunction or abstract_call_method on
            # get_compileable_sig), but still must be finished up since it may see and
            # change the local variables of the InferenceState at currpc, we do this
            # even if the nextresult status is already completed.
            continue
        elseif isdefined(nextstates[nextstateid], :result) || !isempty(callee.ip)
            # Next make progress on this frame
            prev = length(callee.tasks) + 1
            nextstates[nextstateid] = typeinf_local(interp, callee, nextstates[nextstateid])
            reverse!(callee.tasks, prev)
        elseif callee.cycleid == length(callstack)
            # With no active ip's and no cycles, frame is done
            finish_nocycle(interp, callee)
            callee.frameid == 0 && break
            takenext = length(callstack)
            nextstateid = takenext + 1 - frame.frameid
            #@assert length(nextstates) == nextstateid + 1
            #@assert all(i -> !isdefined(nextstates[i], :result), nextstateid+1:length(nextstates))
            resize!(nextstates, nextstateid)
        elseif callee.cycleid == callee.frameid
            # If the current frame is the top part of a cycle, check if the whole cycle
            # is done, and if not, pick the next item to work on.
            no_active_ips_in_cycle = true
            for i = callee.cycleid:length(callstack)
                caller = callstack[i]::InferenceState
                @assert caller.cycleid == callee.cycleid
                if !isempty(caller.tasks) || isdefined(nextstates[i+1-frame.frameid], :result) || !isempty(caller.ip)
                    no_active_ips_in_cycle = false
                    break
                end
            end
            if no_active_ips_in_cycle
                finish_cycle(interp, callstack, callee.cycleid)
            end
            takenext = length(callstack)
            nextstateid = takenext + 1 - frame.frameid
            if no_active_ips_in_cycle
                #@assert all(i -> !isdefined(nextstates[i], :result), nextstateid+1:length(nextstates))
                resize!(nextstates, nextstateid)
            else
                #@assert length(nextstates) == nextstateid
            end
        else
            # Continue to the next frame in this cycle
            takenext = takenext - 1
        end
    end
    #@assert all(nextresult -> !isdefined(nextresult, :result), nextstates)
    return is_inferred(frame)
end
