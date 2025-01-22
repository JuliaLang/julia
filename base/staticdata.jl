# This file is a part of Julia. License is MIT: https://julialang.org/license

module StaticData

using Core: CodeInstance, MethodInstance
using Base: get_world_counter

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
function insert_backedges(edges::Vector{Any}, ext_ci_list::Union{Nothing,Vector{Any}})
    # determine which CodeInstance objects are still valid in our image
    # to enable any applicable new codes
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
        verify_method_graph(codeinst, stack, visiting)
        minvalid = codeinst.min_world
        maxvalid = codeinst.max_world
        if maxvalid ≥ minvalid
            if get_world_counter() == maxvalid
                # if this callee is still valid, add all the backedges
                Base.Compiler.store_backedges(codeinst, codeinst.edges)
            end
            if get_world_counter() == maxvalid
                maxvalid = typemax(UInt)
                @atomic :monotonic codeinst.max_world = maxvalid
            end
            if external
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
end

function verify_method_graph(codeinst::CodeInstance, stack::Vector{CodeInstance}, visiting::IdDict{CodeInstance,Int})
    @assert isempty(stack); @assert isempty(visiting);
    child_cycle, minworld, maxworld = verify_method(codeinst, stack, visiting)
    @assert child_cycle == 0
    @assert isempty(stack); @assert isempty(visiting);
    nothing
end

# Test all edges relevant to a method:
# - Visit the entire call graph, starting from edges[idx] to determine if that method is valid
# - Implements Tarjan's SCC (strongly connected components) algorithm, simplified to remove the count variable
#   and slightly modified with an early termination option once the computation reaches its minimum
function verify_method(codeinst::CodeInstance, stack::Vector{CodeInstance}, visiting::IdDict{CodeInstance,Int})
    world = codeinst.min_world
    let max_valid2 = codeinst.max_world
        if max_valid2 ≠ WORLD_AGE_REVALIDATION_SENTINEL
            return 0, world, max_valid2
        end
    end
    current_world = get_world_counter()
    local minworld::UInt, maxworld::UInt = 1, current_world
    @assert get_ci_mi(codeinst).def isa Method
    if haskey(visiting, codeinst)
        return visiting[codeinst], minworld, maxworld
    end
    push!(stack, codeinst)
    depth = length(stack)
    visiting[codeinst] = depth
    # TODO JL_TIMING(VERIFY_IMAGE, VERIFY_Methods)
    callees = codeinst.edges
    # verify current edges
    if isempty(callees)
        # quick return: no edges to verify (though we probably shouldn't have gotten here from WORLD_AGE_REVALIDATION_SENTINEL)
    elseif maxworld == unsafe_load(cglobal(:jl_require_world, UInt))
        # if no new worlds were allocated since serializing the base module, then no new validation is worth doing right now either
        minworld = maxworld
    else
        j = 1
        while j ≤ length(callees)
            local min_valid2::UInt, max_valid2::UInt
            edge = callees[j]
            @assert !(edge isa Method) # `Method`-edge isn't allowed for the optimized one-edge format
            if edge isa Core.BindingPartition
                j += 1
                continue
            end
            if edge isa CodeInstance
                edge = get_ci_mi(edge)
            end
            if edge isa MethodInstance
                sig = typeintersect((edge.def::Method).sig, edge.specTypes) # TODO??
                min_valid2, max_valid2, matches = verify_call(sig, callees, j, 1, world)
                j += 1
            elseif edge isa Int
                sig = callees[j+1]
                min_valid2, max_valid2, matches = verify_call(sig, callees, j+2, edge, world)
                j += 2 + edge
                edge = sig
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
                min_valid2, max_valid2 = verify_invokesig(edge, meth, world)
                matches = nothing
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
                push!(invalidations, edge, "insert_backedges_callee", codeinst, matches)
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
            child_cycle, min_valid2, max_valid2 = verify_method(callee, stack, visiting)
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
            @atomic  :monotonic child.min_world = minworld
        end
        @atomic :monotonic child.max_world = maxworld
        @assert visiting[child] == length(stack) + 1
        delete!(visiting, child)
        invalidations = _jl_debug_method_invalidation[]
        if invalidations !== nothing && maxworld < current_world
            push!(invalidations, child, "verify_methods", cause)
        end
    end
    return 0, minworld, maxworld
end

function verify_call(@nospecialize(sig), expecteds::Core.SimpleVector, i::Int, n::Int, world::UInt)
    # verify that these edges intersect with the same methods as before
    lim = _jl_debug_method_invalidation[] !== nothing ? Int(typemax(Int32)) : n
    minworld = Ref{UInt}(1)
    maxworld = Ref{UInt}(typemax(UInt))
    has_ambig = Ref{Int32}(0)
    result = Base._methods_by_ftype(sig, nothing, lim, world, #=ambig=#false, minworld, maxworld, has_ambig)
    if result === nothing
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
                if t isa Method
                    meth = t
                else
                    if t isa CodeInstance
                        t = get_ci_mi(t)
                    else
                        t = t::MethodInstance
                    end
                    meth = t.def::Method
                end
                if match.method == meth
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
        end
    end
    return minworld[], maxworld[], result
end

function verify_invokesig(@nospecialize(invokesig), expected::Method, world::UInt)
    @assert invokesig isa Type
    local minworld::UInt, maxworld::UInt
    if invokesig === expected.sig
        # the invoke match is `expected` for `expected->sig`, unless `expected` is invalid
        minworld = expected.primary_world
        maxworld = expected.deleted_world
        @assert minworld ≤ world
        if maxworld < world
            maxworld = 0
        end
    else
        minworld = 1
        maxworld = typemax(UInt)
        mt = Base.get_methodtable(expected)
        if mt === nothing
            maxworld = 0
        else
            matched, valid_worlds = Base.Compiler._findsup(invokesig, mt, world)
            minworld, maxworld = valid_worlds.min_world, valid_worlds.max_world
            if matched === nothing
                maxworld = 0
            elseif matched.method != expected
                maxworld = 0
            end
        end
    end
    return minworld, maxworld
end

end # module StaticData
