# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type MethodTableView; end

"""
    struct InternalMethodTable <: MethodTableView

A struct representing the state of the internal method table at a
particular world age.
"""
struct InternalMethodTable <: MethodTableView
    world::UInt
end

"""
    struct OverlayMethodTable <: MethodTableView

Overlays the internal method table such that specific queries can be redirected to an
external table, e.g., to override existing method.
"""
struct OverlayMethodTable <: MethodTableView
    world::UInt
    mt::Core.MethodTable
end

struct MethodLookupResult
    # Really Vector{Core.MethodMatch}, but it's easier to represent this as
    # and work with Vector{Any} on the C side.
    matches::Vector{Any}
    valid_worlds::WorldRange
    ambig::Bool
end
length(result::MethodLookupResult) = length(result.matches)
function iterate(result::MethodLookupResult, args...)
    r = iterate(result.matches, args...)
    r === nothing && return nothing
    match, state = r
    return (match::MethodMatch, state)
end
getindex(result::MethodLookupResult, idx::Int) = getindex(result.matches, idx)::MethodMatch

"""
    findall(sig::Type, view::MethodTableView; limit::Int=typemax(Int)) ->
        (matches::MethodLookupResult, overlayed::Bool) or missing

Find all methods in the given method table `view` that are applicable to the given signature `sig`.
If no applicable methods are found, an empty result is returned.
If the number of applicable methods exceeded the specified limit, `missing` is returned.
`overlayed` indicates if any of the matching methods comes from an overlayed method table.
"""
function findall(@nospecialize(sig::Type), table::InternalMethodTable; limit::Int=Int(typemax(Int32)))
    result = _findall(sig, nothing, table.world, limit)
    result === missing && return missing
    return result, false
end

function findall(@nospecialize(sig::Type), table::OverlayMethodTable; limit::Int=Int(typemax(Int32)))
    result = _findall(sig, table.mt, table.world, limit)
    result === missing && return missing
    nr = length(result)
    if nr ≥ 1 && result[nr].fully_covers
        # no need to fall back to the internal method table
        return result, true
    end
    # fall back to the internal method table
    fallback_result = _findall(sig, nothing, table.world, limit)
    fallback_result === missing && return missing
    # merge the fallback match results with the internal method table
    return MethodLookupResult(
        vcat(result.matches, fallback_result.matches),
        WorldRange(
            max(result.valid_worlds.min_world, fallback_result.valid_worlds.min_world),
            min(result.valid_worlds.max_world, fallback_result.valid_worlds.max_world)),
        result.ambig | fallback_result.ambig), !isempty(result)
end

function _findall(@nospecialize(sig::Type), mt::Union{Nothing,Core.MethodTable}, world::UInt, limit::Int)
    _min_val = RefValue{UInt}(typemin(UInt))
    _max_val = RefValue{UInt}(typemax(UInt))
    _ambig = RefValue{Int32}(0)
    ms = _methods_by_ftype(sig, mt, limit, world, false, _min_val, _max_val, _ambig)
    if ms === false
        return missing
    end
    return MethodLookupResult(ms::Vector{Any}, WorldRange(_min_val[], _max_val[]), _ambig[] != 0)
end

"""
    findsup(sig::Type, view::MethodTableView) ->
        (match::MethodMatch, valid_worlds::WorldRange, overlayed::Bool) or nothing

Find the (unique) method such that `sig <: match.method.sig`, while being more
specific than any other method with the same property. In other words, find the method
which is the least upper bound (supremum) under the specificity/subtype relation of
the queried `sig`nature. If `sig` is concrete, this is equivalent to asking for the method
that will be called given arguments whose types match the given signature.
Note that this query is also used to implement `invoke`.

Such a matching method `match` doesn't necessarily exist.
It is possible that no method is an upper bound of `sig`, or
it is possible that among the upper bounds, there is no least element.
In both cases `nothing` is returned.

`overlayed` indicates if any of the matching methods comes from an overlayed method table.
"""
function findsup(@nospecialize(sig::Type), table::InternalMethodTable)
    return (_findsup(sig, nothing, table.world)..., false)
end

function findsup(@nospecialize(sig::Type), table::OverlayMethodTable)
    match, valid_worlds = _findsup(sig, table.mt, table.world)
    match !== nothing && return match, valid_worlds, true
    # fall back to the internal method table
    fallback_match, fallback_valid_worlds = _findsup(sig, nothing, table.world)
    return (
        fallback_match,
        WorldRange(
            max(valid_worlds.min_world, fallback_valid_worlds.min_world),
            min(valid_worlds.max_world, fallback_valid_worlds.max_world)),
        false)
end

function _findsup(@nospecialize(sig::Type), mt::Union{Nothing,Core.MethodTable}, world::UInt)
    min_valid = RefValue{UInt}(typemin(UInt))
    max_valid = RefValue{UInt}(typemax(UInt))
    match = ccall(:jl_gf_invoke_lookup_worlds, Any, (Any, Any, UInt, Ptr{Csize_t}, Ptr{Csize_t}),
                   sig, mt, world, min_valid, max_valid)::Union{MethodMatch, Nothing}
    valid_worlds = WorldRange(min_valid[], max_valid[])
    return match, valid_worlds
end

isoverlayed(::MethodTableView)     = error("unsatisfied MethodTableView interface")
isoverlayed(::InternalMethodTable) = false
isoverlayed(::OverlayMethodTable)  = true
