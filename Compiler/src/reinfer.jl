# This file is a part of Julia. License is MIT: https://julialang.org/license

using ..Compiler.Base
using ..Compiler: _findsup, store_backedges, JLOptions, get_world_counter,
    _methods_by_ftype, get_methodtable, get_ci_mi, should_instrument,
    morespecific, RefValue, get_require_world, Vector, IdDict, IdSet,
    binding_access_range, is_leaf_partition, WorldWithRange, WorldRange, min_world, max_world
using .Core: CodeInstance, MethodInstance

const CI_FLAGS_NATIVE_CACHE_VALID = 0b1000
const CI_FLAGS_BACKEDGES_LOGGED = 0b100000 # the image's backedge log already holds this CodeInstance's backedges
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

# A memoized `_methods_by_ftype` lookup: the same callee signature is verified once per
# caller, and the answer is the same for every query world inside its validity range.
struct VerifyMethodLookup
    lim::Int
    world::UInt
    result::Union{Nothing,Vector{Any}}
    min_world::UInt
    max_world::UInt
    has_ambig::Int32
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

    # Scratch for `verify_call`, reused across calls to avoid allocating per edge
    matches::Vector{Any}
    expected::Vector{Method}
    minworld::RefValue{UInt}
    maxworld::RefValue{UInt}
    has_ambig::RefValue{Int32}
    lookups::IdDict{Any,VerifyMethodLookup}
    backedge_scratch::IdSet{Any}

    # The image has a backedge log that is applied in bulk after verification.
    # CodeInstances not covered by the log still register their backedges one by one.
    prelinked::Bool

    function VerifyMethodWorkspace(prelinked::Bool=false)
        new(VerifyMethodInitialState[], VerifyMethodWorkState[], VerifyMethodResultState[],
            CodeInstance[], IdDict{CodeInstance,Int}(),
            Any[], Method[], RefValue{UInt}(1), RefValue{UInt}(typemax(UInt)), RefValue{Int32}(0),
            IdDict{Any,VerifyMethodLookup}(), IdSet{Any}(), prelinked)
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


function binding_access_range_at(b::Core.Binding, world::UInt)
    wr, _ = binding_access_range(b, WorldWithRange(world, WorldRange(get_require_world(), world)), false)
    return wr
end

# Whether an access to `b` can resolve differently now than it did in any process that serialized code against it.
function binding_changed_since_require_world(b::Core.Binding, world::UInt)
    require_world = get_require_world()
    # Fast path: this binding has not been repartitioned since the require world at all, and it
    # resolves without crossing an import, so no walk is needed to know its range reaches back.
    # A non-leaf partition has to take the slow path: the walk continues into the binding it
    # imports, which may itself have been repartitioned after the require world even though `b`
    # was not.
    if isdefined(b, :partitions)
        p = b.partitions
        p.min_world <= require_world && is_leaf_partition(p) && return false
    end
    return min_world(binding_access_range_at(b, world)) > require_world
end

# Restore backedges to external targets
# `internal_methods` = [caller1, ...], the list of worklist-owned code instances internally
function insert_backedges(internal_methods::Vector{Any}, backedge_log::Union{Vector{Any}, Nothing})
    # determine which CodeInstance objects are still valid in our image
    # to enable any applicable new codes
    backedges_only = unsafe_load(cglobal(:jl_first_image_replacement_world, UInt)) == typemax(UInt)
    scan_new_methods!(internal_methods, get_world_counter(), backedges_only)
    workspace = VerifyMethodWorkspace(backedge_log !== nothing)
    # Verify all roots, then register backedges, then promote. A method defined after
    # registration invalidates the callers, so the promotion does nothing. A method defined
    # before registration leaves them unpromoted, valid only up to their validation world.
    worlds = scan_new_code!(internal_methods, workspace)
    if backedge_log !== nothing
        # Register the recorded backedges of every caller that verified at the current world.
        ccall(:jl_apply_backedge_log, Cvoid, (Any,), backedge_log)
    end
    for i = 1:length(internal_methods)
        codeinst = internal_methods[i]
        codeinst isa CodeInstance || continue
        # If the world has not moved since validation, this extends validity to the latest world,
        # for the root and its dependencies, under the world counter lock. From then on the
        # ordinary backedge mechanism keeps them valid.
        @ccall jl_promote_ci_to_current(codeinst::Any, worlds[i]::UInt)::Cvoid
    end
    nothing
end

function scan_new_code!(internal_methods::Vector{Any}, workspace::VerifyMethodWorkspace)
    worlds = Vector{UInt}(undef, length(internal_methods))
    lookup_world = get_world_counter()
    for i = 1:length(internal_methods)
        codeinst = internal_methods[i]
        validation_world = get_world_counter()
        worlds[i] = validation_world
        codeinst isa CodeInstance || continue
        if validation_world != lookup_world
            # a method was added or deleted since the memoized lookups were made (an open-ended
            # `max_world` in them is no longer trustworthy)
            empty!(workspace.lookups)
            lookup_world = validation_world
        end
        verify_method_graph(codeinst, validation_world, workspace)
    end
    return worlds
end

# An edge whose call signature has no method contributor outside the loading
# image's dependency closure (see jl_edge_sig_replayable) matches exactly the
# methods its precompile worker matched, so the worker's verdict is replayed
# instead of matching again. Invalidation debugging wants every edge matched.
@inline function edge_replayable(@nospecialize(sig))
    _jl_debug_method_invalidation[] === nothing || return false
    return ccall(:jl_edge_sig_replayable, Cint, (Any,), sig) != 0
end

# the min world of a replayed call edge: its recorded match set (targets
# i:i+n-1 of the edge list) became available in this session at those methods'
# activation worlds, as verify_call reports for an unchanged match set
function replayed_minworld(expecteds::Core.SimpleVector, i::Int, n::Int)
    minworld = get_require_world()
    for k = i:i+n-1
        pw = get_method_from_edge(expecteds[k]).primary_world
        if minworld < pw
            minworld = pw
        end
    end
    return minworld
end

# contributors are tracked for the global method table only, so an edge whose
# recorded targets (i:i+n-1) include a method of another table is matched live
function edge_targets_global(expecteds::Core.SimpleVector, i::Int, n::Int)
    for k = i:i+n-1
        get_methodtable(get_method_from_edge(expecteds[k])) === Core.methodtable || return false
    end
    return true
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
            # Compatible image code already has the requested counters.
            # Allocation tracking still needs fresh instrumentation.
            if JLOptions().malloc_log == 0 && ccall(:jl_codeinst_coverage_compatible, Cint, (Any,), codeinst) != 0
                return false
            end
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
                scan_new_method!(initial.def, validation_world, backedges_only)
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
                matches = workspace.matches
                empty!(matches)
                j = 1
                while j <= length(initial.callees)
                    local min_valid2::UInt, max_valid2::UInt
                    edge = initial.callees[j]
                    @assert !(edge isa Method) "unexpected Method edge indicates corrupt edges list creation"
                    possibly_ambiguous = edge isa Core.PossiblyAmbiguous
                    if possibly_ambiguous
                        j += 1
                        edge = initial.callees[j]
                    end

                    if edge isa CodeInstance
                        # Convert CodeInstance to MethodInstance for validation (like original)
                        edge = get_ci_mi(edge)
                    end

                    if edge isa MethodInstance
                        sig = edge.specTypes
                        if edge_targets_global(initial.callees, j, 1) && edge_replayable(sig)
                            min_valid2, max_valid2 = replayed_minworld(initial.callees, j, 1), validation_world
                        else
                            min_valid2, max_valid2 = verify_call(sig, initial.callees, j, 1, world, true, possibly_ambiguous, workspace)
                        end
                        j += 1
                    elseif edge isa Int
                        sig = initial.callees[j+1]
                        nmatches = abs(edge)
                        fully_covers = edge > 0
                        # An edge with no targets (a missing or ambiguous call) has no target world
                        # to bound its validity from below, so it is matched live.
                        if nmatches > 0 && edge_targets_global(initial.callees, j+2, nmatches) && edge_replayable(sig)
                            min_valid2, max_valid2 = replayed_minworld(initial.callees, j+2, nmatches), validation_world
                        else
                            min_valid2, max_valid2 = verify_call(sig, initial.callees, j+2, nmatches, world, fully_covers, possibly_ambiguous, workspace)
                        end
                        j += 2 + nmatches
                        edge = sig
                    elseif edge isa Core.Binding
                        j += 1
                        # Check that what of and how this code accessed the leaf partition is still valid.
                        wr = binding_access_range_at(edge, validation_world)
                        if min_world(wr) > get_require_world()
                            # Nothing can be backdated before the require world, so an access
                            # whose range does not reach it cannot be shown valid at all.
                            min_valid2 = 1
                            max_valid2 = 0
                        else
                            min_valid2 = min_world(wr)
                            max_valid2 = max_world(wr)
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
                        if get_methodtable(meth) === Core.methodtable && edge_replayable(edge)
                            min_valid2, max_valid2 = max(get_require_world(), meth.primary_world), validation_world
                        else
                            min_valid2, max_valid2 = verify_invokesig(edge, meth, world, matches)
                        end
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
                    if result.result_maxworld == validation_world && validation_world == get_world_counter() &&
                       (!workspace.prelinked || child.flags & CI_FLAGS_BACKEDGES_LOGGED == 0) && isdefined(child, :edges)
                        # The image's backedge log covers only some CodeInstances; register the rest here.
                        store_backedges(child, child.edges, workspace.backedge_scratch)
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

# fast-path dispatch_status bit definitions (false indicates unknown)
# true indicates this method would be returned as the result from `which` when invoking `method.sig` in the current latest world
const METHOD_SIG_LATEST_WHICH = 0x1
# true indicates this method would be returned as the only result from `methods` when calling `method_instance.specTypes` in the current latest world
# it is equivalently tracked as `isempty(interferences)` on a `method`
const METHOD_SIG_LATEST_ONLY = 0x2
# true indicates this method is not strictly morespecific than any method it intersects
# (equivalently, that this method is not in the interference set of any other method without also being in this methods interference set too).
const METHOD_SIG_NO_LOSERS = 0x4

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

# The MethodInstance behind an edge, or `nothing` for a bare Method edge
function get_mi_from_edge(@nospecialize t)
    t isa Method && return nothing
    t isa CodeInstance && return get_ci_mi(t)::MethodInstance
    return t::MethodInstance
end

# The single expected match provably dominates the queried signature, so the result of
# `ml_matches` is already known to be just this method (only valid when `fully_covers`)
function expected_dominates_sig(meth::Method, mi::Union{Nothing,MethodInstance})
    if mi !== nothing && !iszero(mi.dispatch_status & METHOD_SIG_LATEST_ONLY)
        return true # the sole match for `mi.specTypes`
    end
    # an empty interference set is the Method-level equivalent of
    # METHOD_SIG_LATEST_ONLY: the method beats everything it intersects
    return isempty(meth.interferences)
end

# Check if `m` is one of the expected methods of this run
function method_in_expecteds(m::Method, expected::Vector{Method})
    for meth in expected
        if m === meth
            return true
        end
    end
    return false
end

# Iterate a method's `interferences` set. The set is an idset used append-only,
# in method-definition order. Iteration therefore stops at the first
# unassigned slot and, given a world cutoff, at the first entry defined in a
# future world (which hides all later ones too).
struct EachInterference
    interferences::Memory{Any}
    cutoff::UInt # typemax(UInt) means no world cutoff
end
eachinterference(m::Method, cutoff::UInt=typemax(UInt)) = EachInterference(m.interferences, cutoff)
function Base.iterate(ei::EachInterference, k::Int=1)
    interferences = ei.interferences
    k > length(interferences) && return nothing
    isassigned(interferences, k) || return nothing # no more entries
    interference_method = interferences[k]::Method
    ei.cutoff < interference_method.primary_world && return nothing # this and later entries are for a future world
    return interference_method, k + 1
end

# Check if method2 is in method1's interferences set
# Returns true if method2 is found (meaning !morespecific(method1, method2))
function method_in_interferences(method2::Method, method1::Method)
    for interference_method in eachinterference(method1)
        if interference_method === method2
            return true
        end
    end
    return false
end

# Check if method1 is more specific than method2 via the interference graph
# equivalent to: morespecific(method1, method2) && typeintersect(method1.sig, method2.sig) !== Union{}
function method_morespecific_recorded(method1::Method, method2::Method)
    method1 === method2 && return false
    return method_in_interferences(method1, method2) && !method_in_interferences(method2, method1)
end

# The fast-path `dispatch_status` bit is set: `which` would return this method when
# invoking `method.sig` in the current latest world. False indicates unknown, so callers
# must treat a false result as conservatively as an actually deleted method.
method_is_latest_which(m::Method) = !iszero(m.dispatch_status & METHOD_SIG_LATEST_WHICH)

# Max interference-set size for which n==1 uses the interference fast path instead of
# `ml_matches`: the scan probes every member with `typeintersect`, so above this size the
# pruned `ml_matches` lookup is cheaper (~8 is the empirical crossover).
const VERIFY_INTERF_CAP = 8

# The unexpected `interference_method` (the new method), recorded in expected method
# `meth`'s interference set, intersects the queried `sig` over the nonempty `ti`. Decide
# whether the sort provably removes it: acceptance implies the full `ml_matches`
# lookup would return exactly the `expected` methods with no ambiguity
# flag caused by the new method. Returns `false` conservative when a full lookup is
# necessary to decide.
#
# Accepting stays conservative even though the caller only ever offers up the unexpected
# methods recorded in the expected methods' own sets, because that scan witnesses every
# visible change:
#  - fully_covers means every method intersecting sig intersects some expected pointwise;
#  - methods invisible to it (strictly less-specific than every expected they intersect,
#    hence in no expected's set) always removed silently: the union of their intersecting
#    expecteds covers them, and a failing blocker transfer would need a blocker that beats
#    an expected cover -- which recording makes visible;
#  - any cycle that could flag or change the result must thread an expected, entering through
#    a visible interference method strictly morespecific than it -- an entry of that scan,
#    which is either rejected by the conditions below or by `morespecific_cannot_cycle`,
#    are provably not on any cycle.
function newmethod_removed_silently(meth::Method, interference_method::Method, @nospecialize(ti), @nospecialize(sig),
                                    expected::Vector{Method}, world::UInt)
    # the caller's scan supplies `interference_method ∈ meth`'s set, so this single probe
    # decides `method_morespecific_recorded(interference_method, meth)`: found means the
    # two are mutually ambiguous, absent means the new method strictly beats `meth`
    if method_in_interferences(meth, interference_method)
        expected_is_minmax(meth, interference_method, sig, expected) || return false
    end
    # Either way, the drop needs a certifying cover. This cheap search is the common
    # bail, so it runs before the morespecific list scan (each probe of which is a linear set
    # scan of its own).
    has_empty_set_cover(interference_method, ti, expected) || return false
    return morespecific_cannot_cycle(interference_method, world)
end

# The mutually ambiguous case of `newmethod_removed_silently`, where `meth` (the owner of
# the interference set being scanned) is neither more nor less specific than the new method `interference_method`,
# its ambiguity partner. This is the canonical `Type{Union{}}`-slurp recovery shape --
# f(::Type{<:A}) and f(::Type{<:B}) overlap only at the corner the slurp resolves -- so it
# must stay on the fast path. The fresh sort keeps the pair silent only through the minmax
# exemption: the sort pre-marks the minmax match finalized and never visits it, so its
# `check_fully_ambiguous` scan (which would flag any mutual partner still in the raw match
# list, dropped or not) never runs. Certify set-locally that `meth` is that minmax:
#  - `meth` must fully cover sig (a visited owner always flags the pair, since the partner
#    intersects sig and so sits in the raw match list);
#  - the partner must NOT fully cover sig: minmax discovery runs over the raw list before
#    any drop, so a fully-covering mutual partner disqualifies `meth` there even though it
#    is later removed;
#  - `meth` must recorded-beat every other fully-covering expected (minmax is unique:
#    recorded strict-beat is antisymmetric, and this check also rejects the owner of any
#    second mutual pair);
#  - a fully-covering new method that `meth` does not beat would steal minmax, but needs no
#    check here: it is itself a visible entry of `meth`'s set, and its own certification
#    fails -- as a mutual entry by the partner-covers-sig condition above, and as a strict
#    morespecific choice because its empty-set cover would have to fully cover sig, and an expected with
#    those properties would have dropped `meth` from the recorded result in the first place.
# The drop of the partner and its blocker-transfer obligations are then certified by the
# caller's two remaining conditions, exactly as for a strictly morespecific method: an empty-set
# cover's transfers are all automatic (nothing is morespecific than it, and it patches any transfer
# region inside its signature), and the morespecific scan of expected ensures the partner is not
# tangled in a specificity cycle.
function expected_is_minmax(meth::Method, interference_method::Method, @nospecialize(sig),
                            expected::Vector{Method})
    sig <: meth.sig || return false                 # `meth` fully covers sig
    sig <: interference_method.sig && return false  # its partner does not
    for meth2 in expected      # and `meth` beats every other full cover
        meth2 === meth && continue
        if sig <: meth2.sig && !method_morespecific_recorded(meth, meth2)
            return false
        end
    end
    return true
end

# Look for an expected method that covers the removed `interference_method` over their
# intersection `ti` and provably certifies the removal as silent: only an expected with an
# empty interference set qualifies, since such a method beats everything it intersects, so:
#  - nothing beats or ties it, so it can never sit in a specificity cycle and always
#    finalizes before the sort needs it as a cover;
#  - every blocker transfer through it passes automatically;
#  - being recorded in the set of every method it intersects, it patches any
#    blocker-transfer region inside its signature for the other drops too.
# The `Union{}` bottom-slurp methods are exactly this shape, keeping verification fast when
# a later-added method intersects sig only at the `Type{Union{}}` corner they resolve. Any
# other unexpected entry is left to the full lookup (the sort's dominance-transfer
# protocol): anything weaker re-opens the cycle counterexamples, where a cover tangled in a
# specificity cycle certifies a drop it must not.
function has_empty_set_cover(interference_method::Method, @nospecialize(ti),
                             expected::Vector{Method})
    for meth2 in expected
        # the emptiness test is the O(1) bail, and also makes the second half of
        # `method_morespecific_recorded` a scan of the empty set
        if isempty(meth2.interferences) &&
            method_morespecific_recorded(meth2, interference_method) &&
            ti <: meth2.sig
            return true
        end
    end
    return false
end

# Require every strictly morespecific method of the removed `interference_method` to have an empty
# interference set itself: like the cover found by `has_empty_set_cover`, such a method
# cannot continue a specificity cycle. This is the condition the last bullet above
# `newmethod_removed_silently` relies on to keep an acceptance conservative.
#
# Without it, a cycle through invisible methods could encounter the less-specific expected behind
# the caller's scan's back (dragging it into an SCC mid-sort, where it stops covering the
# invisible members and they survive into the result): detecting that re-entry edge
# directly would mean intersecting `interference_method`'s own set against sig -- the full
# lookup's price.
function morespecific_cannot_cycle(interference_method::Method, world::UInt)
    for msp in eachinterference(interference_method, world)
        # the iteration supplies `msp ∈ interference_method`'s set, which is the first
        # half of `method_morespecific_recorded(msp, interference_method)`, so this only
        # needs to now check the second half
        if !isempty(msp.interferences) &&
            !method_in_interferences(interference_method, msp)
            # a strict morespecific method that could itself sit on a cycle
            return false
        end
    end
    return true
end

# `_methods_by_ftype` for `sig`, memoized in `workspace` unless the debug log is on (the log
# path mutates the result vector). A cached answer is reused when its validity range covers
# `world`, since the matches are the same in every world of that range.
function verify_call_lookup(@nospecialize(sig), lim::Int, world::UInt, workspace::VerifyMethodWorkspace, memoize::Bool)
    if memoize
        cached = get(workspace.lookups, sig, nothing)
        if cached !== nothing && cached.lim == lim
            if cached.result === nothing ? cached.world == world : (cached.min_world <= world <= cached.max_world)
                return cached.result, cached.min_world, cached.max_world, cached.has_ambig
            end
        end
    end
    minworld = workspace.minworld
    maxworld = workspace.maxworld
    has_ambig = workspace.has_ambig
    minworld[] = 1
    maxworld[] = typemax(UInt)
    has_ambig[] = 0
    result = _methods_by_ftype(sig, nothing, lim, world, #=ambig=#false, minworld, maxworld, has_ambig)
    if memoize
        workspace.lookups[sig] = VerifyMethodLookup(lim, world, result, minworld[], maxworld[], has_ambig[])
    end
    return result, minworld[], maxworld[], has_ambig[]
end

function verify_call(@nospecialize(sig), expecteds::Core.SimpleVector, i::Int, n::Int, world::UInt, fully_covers::Bool, possibly_ambiguous::Bool, workspace::VerifyMethodWorkspace)
    # verify that these edges intersect with the same methods as before
    matches = workspace.matches
    # Collect the expected methods once: indexing the edges list boxes the index (no inline
    # codegen for `_svec_ref`), and the loops below would otherwise do it per interference.
    expected = workspace.expected
    empty!(expected)
    mi = nothing
    expected_deleted = false
    for j = 1:n
        meth = get_method_from_edge(expecteds[i+j-1])
        push!(expected, meth)
        if !method_is_latest_which(meth)
            expected_deleted = true
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
            let t = expecteds[i], meth = expected[1], minworld
                mi = get_mi_from_edge(t) # also recorded for `jl_promote_mi_to_current` below
                # Fast path is legal when fully_covers=true
                if fully_covers && expected_dominates_sig(meth, mi)
                    minworld = meth.primary_world
                    @assert minworld ≤ world "expected method not present in verification world"
                    return minworld, typemax(UInt)
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
            let interf = expected[1].interferences, cap = VERIFY_INTERF_CAP
                if length(interf) > cap && isassigned(interf, cap + 1)
                    interference_fast_path_success = false
                end
            end
        end
        # If it didn't fail yet, then check that all interference methods are either expected, or not applicable.
        if interference_fast_path_success
            local interference_minworld::UInt = 1
            for meth in expected
                if interference_minworld < meth.primary_world
                    interference_minworld = meth.primary_world
                end
                # manual `eachinterference(meth, world)`: the deleted-entry check
                # below runs before the world cutoff, so a deleted future-world
                # entry conservatively fails the fast path instead of being hidden
                interferences = meth.interferences
                for k = 1:length(interferences)
                    isassigned(interferences, k) || break # no more entries
                    interference_method = interferences[k]::Method
                    if !method_is_latest_which(interference_method)
                        # detected a deleted interference_method, so need the full lookup to compute minworld
                        interference_fast_path_success = false
                        break
                    end
                    world < interference_method.primary_world && break # this and later entries are for a future world
                    if !method_in_expecteds(interference_method, expected)
                        ti = typeintersect(sig, interference_method.sig)
                        if !(ti === Union{})
                            # An unexpected method intersecting sig can change more than the
                            # match set: even when an expected method fully covers it (so
                            # the sorted result below would compare equal), it can make
                            # `ml_matches` flag an ambiguity -- a mutual pair or a
                            # specificity cycle with a reported match -- which must
                            # invalidate below unless the edge was recorded as
                            # `possibly_ambiguous`. The fast-path here doesn't use that bit:
                            # it checks that this method causes no ambiguity at all, without
                            # knowing whether it already existed (and was already ambiguous)
                            # when the edge was recorded, so a `possibly_ambiguous` edge
                            # whose recorded ambiguity involves an unexpected method always
                            # falls through to the full lookup.
                            if !newmethod_removed_silently(meth, interference_method, ti, sig, expected, world)
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
                return interference_minworld, typemax(UInt)
            end
        end
   end
    # next, compare the current result of ml_matches to the old result
    debug = _jl_debug_method_invalidation[] !== nothing
    lim = debug ? Int(typemax(Int32)) : n
    result, minworld, maxworld, has_ambig = verify_call_lookup(sig, lim, world, workspace, !debug)
    if result === nothing
        empty!(matches)
        maxworld = UInt(0)
    else
        # A method added after this edge was recorded can be ambiguous with an expected
        # match without changing this result: the new method is removed when an expected
        # match fully covers their overlap, yet dispatch in the contested region now throws
        # a MethodError that inference and optimizations didn't account for.
        if has_ambig != 0 && !possibly_ambiguous
            maxworld = UInt(0)
        end
        # setdiff!(result, expected)
        if length(result) ≠ n
            maxworld = UInt(0)
        end
        ins = 0
        for k = 1:length(result)
            match = result[k]::Core.MethodMatch
            if !method_in_expecteds(match.method, expected)
                # intersection has a new method or a method was
                # deleted--this is now probably no good, just invalidate
                # everything about it now
                maxworld = UInt(0)
                debug || break
                ins += 1
                result[ins] = match.method
            end
        end
        if maxworld ≠ typemax(UInt) && debug
            resize!(result, ins)
            copy!(matches, result)
        end
    end
    if maxworld == typemax(UInt) && mi isa MethodInstance
        ccall(:jl_promote_mi_to_current, Cvoid, (Any, UInt, UInt), mi, minworld, world)
    end
    return minworld, maxworld
end

function verify_invokesig(@nospecialize(invokesig), expected::Method, world::UInt, matches::Vector{Any})
    @assert invokesig isa Type "corrupt edges list"
    local minworld::UInt, maxworld::UInt
    empty!(matches)
    if invokesig === expected.sig && method_is_latest_which(expected)
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
function insert_backedges_typeinf(internal_methods::Vector{Any}, backedge_log::Union{Vector{Any}, Nothing})
    args = Any[insert_backedges, internal_methods, backedge_log]
    return ccall(:jl_call_in_typeinf_world, Any, (Ptr{Any}, Cint), args, length(args))
end
