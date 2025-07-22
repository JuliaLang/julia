# This file is a part of Julia. License is MIT: https://julialang.org/license

using .Core: CodeInstance, MethodInstance
using .Base: JLOptions, Compiler, get_world_counter, _methods_by_ftype, get_methodtable, morespecific

const WORLD_AGE_REVALIDATION_SENTINEL::UInt = 1
const _jl_debug_method_invalidation = Ref{Union{Nothing,Vector{Any}}}(nothing)
debug_method_invalidation(onoff::Bool) =
    _jl_debug_method_invalidation[] = onoff ? Any[] : nothing

function get_ci_mi(codeinst::CodeInstance)
    def = codeinst.def
    if def isa Core.ABIOverride
        return def.def
    else
        return def::MethodInstance
    end
end

# Restore backedges to external targets
# `edges` = [caller1, ...], the list of worklist-owned code instances internally
# `ext_ci_list` = [caller1, ...], the list of worklist-owned code instances externally
function insert_backedges(edges::Vector{Any}, ext_ci_list::Union{Nothing,Vector{Any}}, extext_methods::Vector{Any}, internal_methods::Vector{Any})
    # determine which CodeInstance objects are still valid in our image
    # to enable any applicable new codes
    backedges_only = unsafe_load(cglobal(:jl_first_image_replacement_world, UInt)) == typemax(UInt)
    Base.scan_new_methods!(extext_methods, internal_methods, backedges_only)
    stack = CodeInstance[]
    visiting = IdDict{CodeInstance,Int}()
    _insert_backedges(edges, stack, visiting)
    if ext_ci_list !== nothing
        _insert_backedges(ext_ci_list, stack, visiting, #=external=#true)
    end
end

function _insert_backedges(edges::Vector{Any}, stack::Vector{CodeInstance}, visiting::IdDict{CodeInstance,Int}, external::Bool=false)
    for i = 1:length(edges)
        codeinst = edges[i]::CodeInstance
        validation_world = get_world_counter()
        verify_method_graph(codeinst, stack, visiting, validation_world)
        # After validation, under the world_counter_lock, set max_world to typemax(UInt) for all dependencies
        # (recursively). From that point onward the ordinary backedge mechanism is responsible for maintaining
        # validity.
        @ccall jl_promote_ci_to_current(codeinst::Any, validation_world::UInt)::Cvoid
        minvalid = codeinst.min_world
        maxvalid = codeinst.max_world
        # Finally, if this CI is still valid in some world age and and belongs to an external method(specialization),
        # poke it that mi's cache
        if maxvalid ≥ minvalid && external
            caller = get_ci_mi(codeinst)
            @assert isdefined(codeinst, :inferred) # See #53586, #53109
            inferred = @ccall jl_rettype_inferred(
                codeinst.owner::Any, caller::Any, minvalid::UInt, maxvalid::UInt)::Any
            if inferred !== nothing
                # We already got a code instance for this world age range from
                # somewhere else - we don't need this one.
            else
                @ccall jl_mi_cache_insert(caller::Any, codeinst::Any)::Cvoid
            end
        end
    end
end

function verify_method_graph(codeinst::CodeInstance, stack::Vector{CodeInstance}, visiting::IdDict{CodeInstance,Int}, validation_world::UInt)
    @assert isempty(stack); @assert isempty(visiting);
    child_cycle, minworld, maxworld = verify_method(codeinst, stack, visiting, validation_world)
    @assert child_cycle == 0
    @assert isempty(stack); @assert isempty(visiting);
    nothing
end

function gen_staged_sig(def::Method, mi::MethodInstance)
    isdefined(def, :generator) || return nothing
    isdispatchtuple(mi.specTypes) || return nothing
    gen = Core.Typeof(def.generator)
    return Tuple{gen, UInt, Method, Vararg}
    ## more precise method lookup, but more costly and likely not actually better?
    #tts = (mi.specTypes::DataType).parameters
    #sps = Any[Core.Typeof(mi.sparam_vals[i]) for i in 1:length(mi.sparam_vals)]
    #if def.isva
    #    return Tuple{gen, UInt, Method, sps..., tts[1:def.nargs - 1]..., Tuple{tts[def.nargs - 1:end]...}}
    #else
    #    return Tuple{gen, UInt, Method, sps..., tts...}
    #end
end

function needs_instrumentation(codeinst::CodeInstance, mi::MethodInstance, def::Method, validation_world::UInt)
    if JLOptions().code_coverage != 0 || JLOptions().malloc_log != 0
        # test if the code needs to run with instrumentation, in which case we cannot use existing generated code
        if isdefined(def, :debuginfo) ? # generated_only functions do not have debuginfo, so fall back to considering their codeinst debuginfo though this may be slower and less reliable
            Compiler.should_instrument(def.module, def.debuginfo) :
            isdefined(codeinst, :debuginfo) && Compiler.should_instrument(def.module, codeinst.debuginfo)
            return true
        end
        gensig = gen_staged_sig(def, mi)
        if gensig !== nothing
            # if this is defined by a generator, try to consider forcing re-running the generators too, to add coverage for them
            minworld = Ref{UInt}(1)
            maxworld = Ref{UInt}(typemax(UInt))
            has_ambig = Ref{Int32}(0)
            result = _methods_by_ftype(gensig, nothing, -1, validation_world, #=ambig=#false, minworld, maxworld, has_ambig)
            if result !== nothing
                for k = 1:length(result)
                    match = result[k]::Core.MethodMatch
                    genmethod = match.method
                    # no, I refuse to refuse to recurse into your cursed generated function generators and will only test one level deep here
                    if isdefined(genmethod, :debuginfo) && Compiler.should_instrument(genmethod.module, genmethod.debuginfo)
                        return true
                    end
                end
            end
        end
    end
    return false
end

# Test all edges relevant to a method:
# - Visit the entire call graph, starting from edges[idx] to determine if that method is valid
# - Implements Tarjan's SCC (strongly connected components) algorithm, simplified to remove the count variable
#   and slightly modified with an early termination option once the computation reaches its minimum
function verify_method(codeinst::CodeInstance, stack::Vector{CodeInstance}, visiting::IdDict{CodeInstance,Int}, validation_world::UInt)
    world = codeinst.min_world
    let max_valid2 = codeinst.max_world
        if max_valid2 ≠ WORLD_AGE_REVALIDATION_SENTINEL
            return 0, world, max_valid2
        end
    end
    mi = get_ci_mi(codeinst)
    def = mi.def::Method
    if needs_instrumentation(codeinst, mi, def, validation_world)
        return 0, world, UInt(0)
    end

    # Implicitly referenced bindings in the current module do not get explicit edges.
    # If they were invalidated, they'll have the flag set in did_scan_source. If they weren't, they imply a minworld
    # of `get_require_world`. In principle, this is only required for methods that do reference
    # an implicit globalref. However, we already don't perform this validation for methods that
    # don't have any (implicit or explicit) edges at all. The remaining corner case (some explicit,
    # but no implicit edges) is rare and there would be little benefit to lower the minworld for it
    # in any case, so we just always use `get_require_world` here.
    local minworld::UInt, maxworld::UInt = Base.get_require_world(), validation_world
    if haskey(visiting, codeinst)
        return visiting[codeinst], minworld, maxworld
    end
    push!(stack, codeinst)
    depth = length(stack)
    visiting[codeinst] = depth
    # TODO JL_TIMING(VERIFY_IMAGE, VERIFY_Methods)
    callees = codeinst.edges
    # Check for invalidation of the implicit edges from GlobalRef in the Method source
    if (def.did_scan_source & 0x1) == 0x0
        backedges_only = unsafe_load(cglobal(:jl_first_image_replacement_world, UInt)) == typemax(UInt)
        Base.scan_new_method!(def, backedges_only)
    end
    if (def.did_scan_source & 0x4) != 0x0
        maxworld = 0
        invalidations = _jl_debug_method_invalidation[]
        if invalidations !== nothing
            push!(invalidations, def, "method_globalref", codeinst, nothing)
        end
    end
    # verify current edges
    if isempty(callees)
        # quick return: no edges to verify (though we probably shouldn't have gotten here from WORLD_AGE_REVALIDATION_SENTINEL)
    elseif maxworld == Base.get_require_world()
        # if no new worlds were allocated since serializing the base module, then no new validation is worth doing right now either
    else
        matches = []
        j = 1
        while j ≤ length(callees)
            local min_valid2::UInt, max_valid2::UInt
            edge = callees[j]
            @assert !(edge isa Method) # `Method`-edge isn't allowed for the optimized one-edge format
            if edge isa CodeInstance
                edge = get_ci_mi(edge)
            end
            if edge isa MethodInstance
                sig = edge.specTypes
                min_valid2, max_valid2 = verify_call(sig, callees, j, 1, world, true, matches)
                j += 1
            elseif edge isa Int
                sig = callees[j+1]
                # Handle negative counts (fully_covers=false)
                nmatches = abs(edge)
                fully_covers = edge > 0
                min_valid2, max_valid2 = verify_call(sig, callees, j+2, nmatches, world, fully_covers, matches)
                j += 2 + nmatches
                edge = sig
            elseif edge isa Core.Binding
                j += 1
                min_valid2 = minworld
                max_valid2 = maxworld
                if !Base.binding_was_invalidated(edge)
                    if isdefined(edge, :partitions)
                        min_valid2 = edge.partitions.min_world
                        max_valid2 = edge.partitions.max_world
                    end
                else
                    # Binding was previously invalidated
                    min_valid2 = 1
                    max_valid2 = 0
                end
            else
                callee = callees[j+1]
                if callee isa Core.MethodTable # skip the legacy edge (missing backedge)
                    j += 2
                    continue
                end
                if callee isa CodeInstance
                    callee = get_ci_mi(callee)
                end
                if callee isa MethodInstance
                    meth = callee.def::Method
                else
                    meth = callee::Method
                end
                min_valid2, max_valid2 = verify_invokesig(edge, meth, world, matches)
                j += 2
            end
            if minworld < min_valid2
                minworld = min_valid2
            end
            if maxworld > max_valid2
                maxworld = max_valid2
            end
            invalidations = _jl_debug_method_invalidation[]
            if max_valid2 ≠ typemax(UInt) && invalidations !== nothing
                push!(invalidations, edge, "insert_backedges_callee", codeinst, copy(matches))
            end
            if max_valid2 == 0 && invalidations === nothing
                break
            end
        end
    end
    # verify recursive edges (if valid, or debugging)
    cycle = depth
    cause = codeinst
    if maxworld ≠ 0 || _jl_debug_method_invalidation[] !== nothing
        for j = 1:length(callees)
            edge = callees[j]
            if !(edge isa CodeInstance)
                continue
            end
            callee = edge
            local min_valid2::UInt, max_valid2::UInt
            child_cycle, min_valid2, max_valid2 = verify_method(callee, stack, visiting, validation_world)
            if minworld < min_valid2
                minworld = min_valid2
            end
            if minworld > max_valid2
                max_valid2 = 0
            end
            if maxworld > max_valid2
                cause = callee
                maxworld = max_valid2
            end
            if max_valid2 == 0
                # found what we were looking for, so terminate early
                break
            elseif child_cycle ≠ 0 && child_cycle < cycle
                # record the cycle will resolve at depth "cycle"
                cycle = child_cycle
            end
        end
    end
    if maxworld ≠ 0 && cycle ≠ depth
        return cycle, minworld, maxworld
    end
    # If we are the top of the current cycle, now mark all other parts of
    # our cycle with what we found.
    # Or if we found a failed edge, also mark all of the other parts of the
    # cycle as also having a failed edge.
    while length(stack) ≥ depth
        child = pop!(stack)
        if maxworld ≠ 0
            @atomic :monotonic child.min_world = minworld
        end
        @atomic :monotonic child.max_world = maxworld
        if maxworld == validation_world && validation_world == get_world_counter()
            Compiler.store_backedges(child, child.edges)
        end
        @assert visiting[child] == length(stack) + 1
        delete!(visiting, child)
        invalidations = _jl_debug_method_invalidation[]
        if invalidations !== nothing && maxworld < validation_world
            push!(invalidations, child, "verify_methods", cause)
        end
    end
    return 0, minworld, maxworld
end

function get_method_from_edge(@nospecialize t)
    if t isa Method
        return t
    else
        if t isa CodeInstance
            t = get_ci_mi(t)::MethodInstance
        else
            t = t::MethodInstance
        end
        return t.def::Method
    end
end

# Check if method2 is in method1's interferences set
# Returns true if method2 is found (meaning !morespecific(method1, method2))
function method_in_interferences(method2::Method, method1::Method)
    interferences = method1.interferences
    for k = 1:length(interferences)
        isassigned(interferences, k) || break
        interference_method = interferences[k]::Method
        if interference_method === method2
            return true
        end
    end
    return false
end

# Check if method1 is more specific than method2 via the interference graph
function method_morespecific_via_interferences(method1::Method, method2::Method)
    if method1 === method2
        return false
    end
    ms = method_in_interferences_recursive(method1, method2, IdSet{Method}())
    # slow check: @assert ms === morespecific(method1, method2) || typeintersect(method1.sig, method2.sig) === Union{} || typeintersect(method2.sig, method1.sig) === Union{}
    return ms
end

# Returns true if method1 is in method2's interferences (meaning !morespecific(method2, method1))
function method_in_interferences_recursive(method1::Method, method2::Method, visited::IdSet{Method})
    if method_in_interferences(method2, method1)
        return false
    end
    if method_in_interferences(method1, method2)
        return true
    end

    # Recursively check through interference graph
    method2 in visited && return false
    push!(visited, method2)
    interferences = method2.interferences
    for k = 1:length(interferences)
        isassigned(interferences, k) || break
        method3 = interferences[k]::Method
        if method_in_interferences(method2, method3)
            continue # only follow edges to morespecific methods in search of the morespecific target (skip ambiguities)
        end
        if method_in_interferences_recursive(method1, method3, visited)
            return true # found method1 in the interference graph
        end
    end

    return false
end

function verify_call(@nospecialize(sig), expecteds::Core.SimpleVector, i::Int, n::Int, world::UInt, fully_covers::Bool, matches::Vector{Any})
    # verify that these edges intersect with the same methods as before
    mi = nothing
    expected_deleted = false
    for j = 1:n
        t = expecteds[i+j-1]
        meth = get_method_from_edge(t)
        if iszero(meth.dispatch_status & METHOD_SIG_LATEST_WHICH)
            expected_deleted = true
            break
        end
    end
    if expected_deleted
        if _jl_debug_method_invalidation[] === nothing && world == get_world_counter()
            return UInt(1), UInt(0)
        end
    elseif n == 1
        # first, fast-path a check if the expected method simply dominates its sig anyways
        # so the result of ml_matches is already simply known
        let t = expecteds[i], meth, minworld, maxworld
            meth = get_method_from_edge(t)
            if !(t isa Method)
                if t isa CodeInstance
                    mi = get_ci_mi(t)::MethodInstance
                else
                    mi = t::MethodInstance
                end
                # Fast path is legal when fully_covers=true
                if fully_covers && !iszero(mi.dispatch_status & METHOD_SIG_LATEST_ONLY)
                    minworld = meth.primary_world
                    @assert minworld ≤ world
                    maxworld = typemax(UInt)
                    return minworld, maxworld
                end
            end
            # Fast path is legal when fully_covers=true
            if fully_covers && !iszero(meth.dispatch_status & METHOD_SIG_LATEST_ONLY)
                minworld = meth.primary_world
                @assert minworld ≤ world
                maxworld = typemax(UInt)
                return minworld, maxworld
            end
        end
    elseif n > 1
        # Try the interference set fast path: check if all interference sets are covered by expecteds
        interference_fast_path_success = fully_covers
        # If it didn't fail yet, then check that all interference methods are either expected, or not applicable.
        if interference_fast_path_success
            local interference_minworld::UInt = 1
            for j = 1:n
                meth = get_method_from_edge(expecteds[i+j-1])
                if interference_minworld < meth.primary_world
                    interference_minworld = meth.primary_world
                end
                interferences = meth.interferences
                for k = 1:length(interferences)
                    isassigned(interferences, k) || break # no more entries
                    interference_method = interferences[k]::Method
                    if iszero(interference_method.dispatch_status & METHOD_SIG_LATEST_WHICH)
                        # detected a deleted interference_method, so need the full lookup to compute minworld
                        interference_fast_path_success = false
                        break
                    end
                    world < interference_method.primary_world && break # this and later entries are for a future world
                    local found_in_expecteds = false
                    for j = 1:n
                        if interference_method === get_method_from_edge(expecteds[i+j-1])
                            found_in_expecteds = true
                            break
                        end
                    end
                    if !found_in_expecteds
                        ti = typeintersect(sig, interference_method.sig)
                        if !(ti === Union{})
                            # try looking for a different expected method that fully covers this interference_method anyways over their intersection
                            for j = 1:n
                                meth2 = get_method_from_edge(expecteds[i+j-1])
                                if method_morespecific_via_interferences(meth2, interference_method) && ti <: meth2.sig
                                    found_in_expecteds = true
                                    break
                                end
                            end
                            if !found_in_expecteds
                                meth2 = get_method_from_edge(expecteds[i])
                                interference_fast_path_success = false
                                break
                            end
                        end
                    end
                end
                if !interference_fast_path_success
                    break
                end
            end
            if interference_fast_path_success
                # All interference sets are covered by expecteds, can return success
                @assert interference_minworld ≤ world
                maxworld = typemax(UInt)
                return interference_minworld, maxworld
            end
        end
   end
    # next, compare the current result of ml_matches to the old result
    lim = _jl_debug_method_invalidation[] !== nothing ? Int(typemax(Int32)) : n
    minworld = Ref{UInt}(1)
    maxworld = Ref{UInt}(typemax(UInt))
    has_ambig = Ref{Int32}(0)
    result = _methods_by_ftype(sig, nothing, lim, world, #=ambig=#false, minworld, maxworld, has_ambig)
    if result === nothing
        empty!(matches)
        maxworld[] = 0
    else
        # setdiff!(result, expected)
        if length(result) ≠ n
            maxworld[] = 0
        end
        ins = 0
        for k = 1:length(result)
            match = result[k]::Core.MethodMatch
            local found = false
            for j = 1:n
                t = expecteds[i+j-1]
                if match.method == get_method_from_edge(t)
                    found = true
                    break
                end
            end
            if !found
                # intersection has a new method or a method was
                # deleted--this is now probably no good, just invalidate
                # everything about it now
                maxworld[] = 0
                if _jl_debug_method_invalidation[] === nothing
                    break
                end
                ins += 1
                result[ins] = match.method
            end
        end
        if maxworld[] ≠ typemax(UInt) && _jl_debug_method_invalidation[] !== nothing
            resize!(result, ins)
            copy!(matches, result)
        end
    end
    if maxworld[] == typemax(UInt) && mi isa MethodInstance
        ccall(:jl_promote_mi_to_current, Cvoid, (Any, UInt, UInt), mi, minworld[], world)
    end
    return minworld[], maxworld[]
end

# fast-path dispatch_status bit definitions (false indicates unknown)
# true indicates this method would be returned as the result from `which` when invoking `method.sig` in the current latest world
const METHOD_SIG_LATEST_WHICH = 0x1
# true indicates this method would be returned as the only result from `methods` when calling `method.sig` in the current latest world
const METHOD_SIG_LATEST_ONLY = 0x2

function verify_invokesig(@nospecialize(invokesig), expected::Method, world::UInt, matches::Vector{Any})
    @assert invokesig isa Type
    local minworld::UInt, maxworld::UInt
    empty!(matches)
    if invokesig === expected.sig && !iszero(expected.dispatch_status & METHOD_SIG_LATEST_WHICH)
        # the invoke match is `expected` for `expected->sig`, unless `expected` is replaced
        minworld = expected.primary_world
        @assert minworld ≤ world
        maxworld = typemax(UInt)
    else
        mt = get_methodtable(expected)
        if mt === nothing
            minworld = 1
            maxworld = 0
        else
            matched, valid_worlds = Compiler._findsup(invokesig, mt, world)
            minworld, maxworld = valid_worlds.min_world, valid_worlds.max_world
            if matched === nothing
                maxworld = 0
            else
                matched = matched.method
                push!(matches, matched)
                if matched !== expected
                    maxworld = 0
                end
            end
        end
    end
    return minworld, maxworld
end
