# This file is a part of Julia. License is MIT: https://julialang.org/license

using ..Compiler.Base
using ..Compiler: _findsup, store_backedges, JLOptions, get_world_counter,
    _methods_by_ftype, get_methodtable, get_ci_mi, should_instrument,
    morespecific, RefValue, get_require_world, Vector, IdDict
using .Core: CodeInstance, MethodInstance

const CI_FLAGS_NATIVE_CACHE_VALID = 0b1000
const WORLD_AGE_REVALIDATION_SENTINEL::UInt = 1
const _jl_debug_method_invalidation = RefValue{Union{Nothing,Vector{Any}}}(nothing)
debug_method_invalidation(onoff::Bool) =
    _jl_debug_method_invalidation[] = onoff ? Any[] : nothing

# Immutable structs for different categories of state data
struct VerifyMethodInitialState
    codeinst::CodeInstance
    mi::MethodInstance
    def::Method
    callees::Core.SimpleVector
end

struct VerifyMethodWorkState
    depth::Int
    cause::CodeInstance
    recursive_index::Int
    stage::Symbol
end

struct VerifyMethodResultState
    child_cycle::Int
    result_minworld::UInt
    result_maxworld::UInt
end

# Container for all the work arrays
struct VerifyMethodWorkspace
    # Arrays of different state categories
    initial_states::Vector{VerifyMethodInitialState}
    work_states::Vector{VerifyMethodWorkState}
    result_states::Vector{VerifyMethodResultState}

    # Tarjan's algorithm working data
    stack::Vector{CodeInstance}
    visiting::IdDict{CodeInstance,Int}

    function VerifyMethodWorkspace()
        new(VerifyMethodInitialState[], VerifyMethodWorkState[], VerifyMethodResultState[],
            CodeInstance[], IdDict{CodeInstance,Int}())
    end
end

# Helper functions to create default states
function VerifyMethodInitialState(codeinst::CodeInstance)
    mi = get_ci_mi(codeinst)
    def = mi.def::Method
    VerifyMethodInitialState(codeinst, mi, def, codeinst.edges)
end

function VerifyMethodWorkState(dummy_cause::CodeInstance)
    VerifyMethodWorkState(0, dummy_cause, 1, :init_and_process_callees)
end

function VerifyMethodResultState()
    VerifyMethodResultState(0, 0, 0)
end


# Restore backedges to external targets
# `internal_methods` = [caller1, ...], the list of worklist-owned code instances internally
function insert_backedges(internal_methods::Vector{Any})
    # determine which CodeInstance objects are still valid in our image
    # to enable any applicable new codes
    backedges_only = unsafe_load(cglobal(:jl_first_image_replacement_world, UInt)) == typemax(UInt)
    scan_new_methods!(internal_methods, backedges_only)
    workspace = VerifyMethodWorkspace()
    scan_new_code!(internal_methods, workspace)
    nothing
end

function scan_new_code!(internal_methods::Vector{Any}, workspace::VerifyMethodWorkspace)
    for i = 1:length(internal_methods)
        codeinst = internal_methods[i]
        codeinst isa CodeInstance || continue
        # codeinst.owner === nothing || continue
        validation_world = get_world_counter()
        verify_method_graph(codeinst, validation_world, workspace)
        # After validation, under the world_counter_lock, set max_world to typemax(UInt) for all dependencies
        # (recursively). From that point onward the ordinary backedge mechanism is responsible for maintaining
        # validity.
        @ccall jl_promote_ci_to_current(codeinst::Any, validation_world::UInt)::Cvoid
    end
end

function verify_method_graph(codeinst::CodeInstance, validation_world::UInt, workspace::VerifyMethodWorkspace)
    @assert isempty(workspace.stack) "workspace corrupted"
    @assert isempty(workspace.visiting) "workspace corrupted"
    @assert isempty(workspace.initial_states) "workspace corrupted"
    @assert isempty(workspace.work_states) "workspace corrupted"
    @assert isempty(workspace.result_states) "workspace corrupted"
    child_cycle, minworld, maxworld = verify_method(codeinst, validation_world, workspace)
    @assert child_cycle == 0
    @assert isempty(workspace.stack) "workspace corrupted"
    @assert isempty(workspace.visiting) "workspace corrupted"
    @assert isempty(workspace.initial_states) "workspace corrupted"
    @assert isempty(workspace.work_states) "workspace corrupted"
    @assert isempty(workspace.result_states) "workspace corrupted"
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
    # foreign CIs (owner !== nothing) aren't run as native code here, so instrumenting them is moot
    codeinst.owner === nothing || return false
    if JLOptions().code_coverage != 0 || JLOptions().malloc_log != 0
        # test if the code needs to run with instrumentation, in which case we cannot use existing generated code
        if isdefined(def, :debuginfo) ? # generated_only functions do not have debuginfo, so fall back to considering their codeinst debuginfo though this may be slower and less reliable
            should_instrument(def.module, def.debuginfo) :
            isdefined(codeinst, :debuginfo) && should_instrument(def.module, codeinst.debuginfo)
            return true
        end
        gensig = gen_staged_sig(def, mi)
        if gensig !== nothing
            # if this is defined by a generator, try to consider forcing re-running the generators too, to add coverage for them
            minworld = RefValue{UInt}(1)
            maxworld = RefValue{UInt}(typemax(UInt))
            has_ambig = RefValue{Int32}(0)
            result = _methods_by_ftype(gensig, nothing, -1, validation_world, #=ambig=#false, minworld, maxworld, has_ambig)
            if result !== nothing
                for k = 1:length(result)
                    match = result[k]::Core.MethodMatch
                    genmethod = match.method
                    # no, I refuse to refuse to recurse into your cursed generated function generators and will only test one level deep here
                    if isdefined(genmethod, :debuginfo) && should_instrument(genmethod.module, genmethod.debuginfo)
                        return true
                    end
                end
            end
        end
    end
    return false
end

# Test all edges relevant to a method:
# - Visit the entire call graph, starting from `codeinst` to determine if that method is valid
# - Implements Tarjan's SCC (strongly connected components) algorithm, simplified to remove the count variable
#   and slightly modified with an early termination option once the computation reaches its minimum
function verify_method(codeinst::CodeInstance, validation_world::UInt, workspace::VerifyMethodWorkspace)
    # Initialize root state
    push!(workspace.initial_states, VerifyMethodInitialState(codeinst))
    push!(workspace.work_states, VerifyMethodWorkState(codeinst))
    push!(workspace.result_states, VerifyMethodResultState())

    current_depth = 1 # == length(workspace._states) == end
    while true
        # Get current state indices
        initial = workspace.initial_states[current_depth]
        work = workspace.work_states[current_depth]

        if work.stage == :init_and_process_callees
            # Initialize state and handle early returns
            world = initial.codeinst.min_world
            let max_valid2 = initial.codeinst.max_world
                if max_valid2 ≠ WORLD_AGE_REVALIDATION_SENTINEL
                    workspace.result_states[current_depth] = VerifyMethodResultState(0, world, max_valid2)
                    workspace.work_states[current_depth] = VerifyMethodWorkState(work.depth, work.cause, work.recursive_index, :return_to_parent)
                    continue
                end
            end

            if needs_instrumentation(initial.codeinst, initial.mi, initial.def, validation_world)
                workspace.result_states[current_depth] = VerifyMethodResultState(0, world, UInt(0))
                workspace.work_states[current_depth] = VerifyMethodWorkState(work.depth, work.cause, work.recursive_index, :return_to_parent)
                continue
            end

            if haskey(workspace.visiting, initial.codeinst)
                workspace.result_states[current_depth] = VerifyMethodResultState(workspace.visiting[initial.codeinst], UInt(1), validation_world)
                workspace.work_states[current_depth] = VerifyMethodWorkState(work.depth, work.cause, work.recursive_index, :return_to_parent)
                continue
            end

            push!(workspace.stack, initial.codeinst)
            depth = length(workspace.stack)
            workspace.visiting[initial.codeinst] = depth

            # unable to backdate before require_world, since Bindings are not able to track that information
            minworld, maxworld = get_require_world(), validation_world

            # Check for invalidation of GlobalRef edges
            if (initial.def.did_scan_source & 0x1) == 0x0
                backedges_only = unsafe_load(cglobal(:jl_first_image_replacement_world, UInt)) == typemax(UInt)
                scan_new_method!(initial.def, backedges_only)
            end
            if (initial.def.did_scan_source & 0x4) != 0x0
                maxworld = 0
                invalidations = _jl_debug_method_invalidation[]
                if invalidations !== nothing
                    push!(invalidations, initial.def, "method_globalref", initial.codeinst, nothing)
                end
            end

            # Process all non-CodeInstance edges
            if !isempty(initial.callees) && maxworld != get_require_world()
                matches = []
                j = 1
                while j <= length(initial.callees)
                    local min_valid2::UInt, max_valid2::UInt
                    edge = initial.callees[j]
                    @assert !(edge isa Method) "unexpected Method edge indicates corrupt edges list creation"

                    if edge isa CodeInstance
                        # Convert CodeInstance to MethodInstance for validation (like original)
                        edge = get_ci_mi(edge)
                    end

                    if edge isa MethodInstance
                        sig = edge.specTypes
                        min_valid2, max_valid2 = verify_call(sig, initial.callees, j, 1, world, true, matches)
                        j += 1
                    elseif edge isa Int
                        sig = initial.callees[j+1]
                        nmatches = abs(edge)
                        fully_covers = edge > 0
                        min_valid2, max_valid2 = verify_call(sig, initial.callees, j+2, nmatches, world, fully_covers, matches)
                        j += 2 + nmatches
                        edge = sig
                    elseif edge isa Core.Binding
                        j += 1
                        min_valid2 = minworld
                        max_valid2 = maxworld
                        if !binding_was_invalidated(edge)
                            if isdefined(edge, :partitions)
                                min_valid2 = edge.partitions.min_world
                                max_valid2 = edge.partitions.max_world
                            end
                        else
                            min_valid2 = 1
                            max_valid2 = 0
                        end
                    else
                        callee = initial.callees[j+1]
                        if callee isa Core.MethodTable
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
                        push!(invalidations, edge, "insert_backedges_callee", initial.codeinst, copy(matches))
                    end
                    if max_valid2 == 0 && invalidations === nothing
                        break
                    end
                end
            end

            # Store computed minworld/maxworld in result state and transition to recursive phase
            workspace.result_states[current_depth] = VerifyMethodResultState(depth, minworld, maxworld)
            workspace.work_states[current_depth] = VerifyMethodWorkState(depth, work.cause, 1, :recursive_phase)

        elseif work.stage == :recursive_phase
            # Find next CodeInstance edge that needs processing
            recursive_index = work.recursive_index
            found_child = false
            while recursive_index ≤ length(initial.callees)
                edge = initial.callees[recursive_index]
                recursive_index += 1

                if edge isa CodeInstance
                    # Create child state and add to stack
                    workspace.work_states[current_depth] = VerifyMethodWorkState(work.depth, work.cause, recursive_index, :recursive_phase)
                    push!(workspace.initial_states, VerifyMethodInitialState(edge))
                    push!(workspace.work_states, VerifyMethodWorkState(edge))
                    push!(workspace.result_states, VerifyMethodResultState())
                    current_depth += 1
                    found_child = true
                    break
                end
            end

            if !found_child
                workspace.work_states[current_depth] = VerifyMethodWorkState(work.depth, work.cause, recursive_index, :cleanup)
            end

        elseif work.stage == :cleanup
            # If we are the top of the current cycle, now mark all other parts of
            # our cycle with what we found.
            # Or if we found a failed edge, also mark all of the other parts of the
            # cycle as also having a failed edge.
            result = workspace.result_states[current_depth]
            if result.result_maxworld == 0 || result.child_cycle == work.depth
                while length(workspace.stack) ≥ work.depth
                    child = pop!(workspace.stack)
                    if result.result_maxworld ≠ 0
                        @atomic :monotonic child.min_world = result.result_minworld
                        # Finally, if this CI is still valid in some world age and marked as valid in the native cache, poke it in that mi's cache now
                        if child.flags & CI_FLAGS_NATIVE_CACHE_VALID == CI_FLAGS_NATIVE_CACHE_VALID
                            @ccall jl_mi_cache_insert(get_ci_mi(child)::Any, child::Any)::Cvoid
                        end
                    end
                    @atomic :monotonic child.max_world = result.result_maxworld
                    if result.result_maxworld == validation_world && validation_world == get_world_counter() && isdefined(child, :edges)
                        store_backedges(child, child.edges)
                    end
                    @assert workspace.visiting[child] == length(workspace.stack) + 1 "internal error maintaining workspace"
                    delete!(workspace.visiting, child)
                    invalidations = _jl_debug_method_invalidation[]
                    if invalidations !== nothing && result.result_maxworld < validation_world
                        push!(invalidations, child, "verify_methods", work.cause)
                    end
                end

                workspace.result_states[current_depth] = VerifyMethodResultState(0, result.result_minworld, result.result_maxworld)
            end

            workspace.work_states[current_depth] = VerifyMethodWorkState(work.depth, work.cause, work.recursive_index, :return_to_parent)

        elseif work.stage == :return_to_parent
            # Pass results to parent and process them
            pop!(workspace.initial_states)
            pop!(workspace.work_states)
            result = pop!(workspace.result_states)
            current_depth -= 1
            if current_depth == 0 # Return results from the root call
                return (result.child_cycle, result.result_minworld, result.result_maxworld)
            end
            # Propagate results to parent
            parent_work = workspace.work_states[current_depth]
            parent_result = workspace.result_states[current_depth]
            callee = initial.codeinst
            child_cycle, min_valid2, max_valid2 = result.child_cycle, result.result_minworld, result.result_maxworld
            parent_cycle = parent_result.child_cycle
            parent_minworld = parent_result.result_minworld
            parent_maxworld = parent_result.result_maxworld
            parent_cause = parent_work.cause
            parent_stage = parent_work.stage
            if parent_minworld < min_valid2
                parent_minworld = min_valid2
            end
            if parent_minworld > max_valid2
                max_valid2 = 0
            end
            if parent_maxworld > max_valid2
                parent_cause = callee
                parent_maxworld = max_valid2
            end
            if max_valid2 == 0
                # found what we were looking for, so terminate early
                # The parent should break out of its loop in :recursive_phase
                parent_stage = :cleanup
            elseif child_cycle ≠ 0 && child_cycle < parent_cycle
                # record the cycle will resolve at depth "cycle"
                parent_cycle = child_cycle
            end
            workspace.work_states[current_depth] = VerifyMethodWorkState(parent_work.depth, parent_cause, parent_work.recursive_index, parent_stage)
            workspace.result_states[current_depth] = VerifyMethodResultState(parent_cycle, parent_minworld, parent_maxworld)
        end
    end
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

# Max interference-set size for which n==1 uses the interference fast path instead of
# `ml_matches`: the scan probes every member with `typeintersect`, so above this size the
# pruned `ml_matches` lookup is cheaper (~8 is the empirical crossover).
const VERIFY_INTERF_CAP = 8

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
    else # n >= 1
        if n == 1
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
                        @assert minworld ≤ world "expected method not present in verification world"
                        maxworld = typemax(UInt)
                        return minworld, maxworld
                    end
                end
                # Fast path is legal when fully_covers=true
                # (an empty interference set is the Method-level equivalent of
                # METHOD_SIG_LATEST_ONLY: the method beats everything it intersects)
                if fully_covers && isempty(meth.interferences)
                    minworld = meth.primary_world
                    @assert minworld ≤ world "expected method not present in verification world"
                    maxworld = typemax(UInt)
                    return minworld, maxworld
                end
            end
        end
        # Try the interference set fast path (used by both n==1, when the O(1) checks above
        # did not resolve it, and n>1): the result is unchanged as long as no interfering
        # method intersects sig outside of what the expected method(s) cover.
        interference_fast_path_success = fully_covers
        if interference_fast_path_success && n == 1
            # Skip to ml_matches for large interference sets (see VERIFY_INTERF_CAP). The set
            # is packed, so isassigned(., cap+1) tests "size > cap" without any typeintersect.
            let interf = get_method_from_edge(expecteds[i]).interferences, cap = VERIFY_INTERF_CAP
                if length(interf) > cap && isassigned(interf, cap + 1)
                    interference_fast_path_success = false
                end
            end
        end
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
                            # An unexpected method intersecting sig can change more than the
                            # match set: even when an expected method fully covers it (so the
                            # pruned result below would compare equal), it can make `ml_matches`
                            # flag an ambiguity -- a mutual pair or a specificity cycle with a
                            # reported match -- which must invalidate below.
                            if method_in_interferences(meth, interference_method)
                                # Mutually ambiguous with the expected method whose set is
                                # being scanned: the fresh sort always flags this pair (both
                                # stay in the intersecting-match list), so only the full
                                # lookup can decide this edge.
                                interference_fast_path_success = false
                                break
                            end
                            # This method strictly beats the scanned set's owner. Look for a
                            # different expected method that covers it over this intersection
                            # and provably certifies the removal as silent: only an expected
                            # with an EMPTY interference set qualifies, since such a method
                            # beats everything it intersects, so:
                            #  - nothing beats or ties it, so it can never sit in a
                            #    specificity cycle and always finalizes before the sort
                            #    needs it as a cover;
                            #  - every blocker transfer through it passes automatically;
                            #  - being recorded in the set of every method it intersects, it
                            #    patches any blocker-transfer region inside its signature
                            #    for the other drops too.
                            # The `Union{}` bottom-slurp methods are exactly this shape,
                            # keeping verification fast when a later-added method intersects
                            # sig only at the `Type{Union{}}` corner they resolve. Any other
                            # unexpected entry is left to the full lookup (the sort's
                            # dominance-transfer protocol): anything weaker re-opens the
                            # cycle counterexamples, where a cover tangled in a specificity
                            # cycle certifies a drop it must not.
                            #
                            # Additionally require every strict beater of this method to
                            # have an empty interference set itself (like the cover, such a
                            # beater cannot continue a specificity cycle). This is what
                            # makes accepting purely conservative -- an acceptance implies
                            # the full lookup would return exactly the expected methods with
                            # no ambiguity -- because scanning only the expected methods'
                            # sets then witnesses every dangerous change:
                            #  - fully_covers means every method intersecting sig intersects
                            #    some expected pointwise;
                            #  - methods invisible here (strictly beaten by every expected
                            #    they intersect, hence in no expected's set) always prune
                            #    silently: the union of their intersecting expecteds covers
                            #    them, and a failing blocker transfer would need a blocker
                            #    that beats an expected cover -- visible by recording;
                            #  - any cycle that could flag or change the result must thread
                            #    an expected, entering through a visible strict beater of
                            #    it -- an entry of this scan, which is either rejected here
                            #    or, by the beater condition, provably not on any cycle.
                            # Without the beater condition, a cycle through invisible
                            # methods could re-enter the beaten expected behind this scan's
                            # back (dragging it into an SCC mid-sort, where it stops
                            # covering the invisible members and they survive into the
                            # result): detecting that re-entry edge directly would mean
                            # intersecting this method's own set against sig -- the full
                            # lookup's price.
                            local beaters_safe = true
                            let interferences2 = interference_method.interferences
                                for k2 = 1:length(interferences2)
                                    isassigned(interferences2, k2) || break # no more entries
                                    beater = interferences2[k2]::Method
                                    world < beater.primary_world && break # this and later entries are for a future world
                                    if !isempty(beater.interferences) &&
                                        !method_in_interferences(interference_method, beater)
                                        # a strict beater that could itself sit on a cycle
                                        beaters_safe = false
                                        break
                                    end
                                end
                            end
                            if beaters_safe
                                for j2 = 1:n
                                    meth2 = get_method_from_edge(expecteds[i+j2-1])
                                    if isempty(meth2.interferences) &&
                                        method_in_interferences(meth2, interference_method) &&
                                        ti <: meth2.sig
                                        found_in_expecteds = true
                                        break
                                    end
                                end
                            end
                            if !found_in_expecteds
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
                @assert interference_minworld ≤ world "expected method not present in verification world"
                maxworld = typemax(UInt)
                return interference_minworld, maxworld
            end
        end
   end
    # next, compare the current result of ml_matches to the old result
    lim = _jl_debug_method_invalidation[] !== nothing ? Int(typemax(Int32)) : n
    minworld = RefValue{UInt}(1)
    maxworld = RefValue{UInt}(typemax(UInt))
    has_ambig = RefValue{Int32}(0)
    result = _methods_by_ftype(sig, nothing, lim, world, #=ambig=#false, minworld, maxworld, has_ambig)
    if result === nothing
        empty!(matches)
        maxworld[] = 0
    else
        # A method added after this edge was recorded can be ambiguous with an expected
        # match without changing this include_ambiguous=false result: the newcomer is
        # pruned when an expected match fully covers their overlap, yet dispatch in the
        # contested region now throws a MethodError that inference never accounted for.
        # Until edges record that inference itself saw an ambiguity
        # (JuliaLang/julia#62322), any ambiguity over sig must invalidate.
        if has_ambig[] != 0
            maxworld[] = 0
        end
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
# true indicates this method would be returned as the only result from `methods` when calling `method_instance.specTypes` in the current latest world
# it is equivalently tracked as `isempty(interferences)` on a `method`
const METHOD_SIG_LATEST_ONLY = 0x2
# true indicates this method is not strictly morespecific than any method it intersects
# (equivalently, that this method is not in the interference set of any other method without also being in this methods interference set too).
const METHOD_SIG_NO_LOSERS = 0x4

function verify_invokesig(@nospecialize(invokesig), expected::Method, world::UInt, matches::Vector{Any})
    @assert invokesig isa Type "corrupt edges list"
    local minworld::UInt, maxworld::UInt
    empty!(matches)
    if invokesig === expected.sig && !iszero(expected.dispatch_status & METHOD_SIG_LATEST_WHICH)
        # the invoke match is `expected` for `expected->sig`, unless `expected` is replaced
        minworld = expected.primary_world
        @assert minworld ≤ world "expected method not present in verification world"
        maxworld = typemax(UInt)
    else
        mt = get_methodtable(expected)
        if mt === nothing
            minworld = 1
            maxworld = 0
        else
            matched, valid_worlds = _findsup(invokesig, mt, world)
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

# Wrapper to call insert_backedges in typeinf_world for external calls
function insert_backedges_typeinf(internal_methods::Vector{Any})
    args = Any[insert_backedges, internal_methods]
    return ccall(:jl_call_in_typeinf_world, Any, (Ptr{Any}, Cint), args, length(args))
end
