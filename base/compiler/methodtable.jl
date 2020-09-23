abstract type MethodTableView; end

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
    struct InternalMethodTable <: MethodTableView

A struct representing the state of the internal method table at a
particular world age.
"""
struct InternalMethodTable <: MethodTableView
    world::UInt
end

"""
    struct CachedMethodTable <: MethodTableView

Overlays another method table view with an additional local fast path cache that
can respond to repeated, identical queries faster than the original method table.
"""
struct CachedMethodTable{T} <: MethodTableView
    cache::IdDict{Any, Union{Missing, MethodLookupResult}}
    table::T
end
CachedMethodTable(table::T) where T =
    CachedMethodTable{T}(IdDict{Any, Union{Missing, MethodLookupResult}}(),
        table)

"""
    findall(sig::Type{<:Tuple}, view::MethodTableView; limit=typemax(Int))

Find all methods in the given method table `view` that are applicable to the
given signature `sig`. If no applicable methods are found, an empty result is
returned. If the number of applicable methods exeeded the specified limit,
`missing` is returned.
"""
function findall(@nospecialize(sig::Type{<:Tuple}), table::InternalMethodTable; limit::Int=typemax(Int))
    _min_val = RefValue{UInt}(typemin(UInt))
    _max_val = RefValue{UInt}(typemax(UInt))
    _ambig = RefValue{Int32}(0)
    ms = _methods_by_ftype(sig, limit, table.world, false, _min_val, _max_val, _ambig)
    if ms === false
        return missing
    end
    return MethodLookupResult(ms::Vector{Any}, WorldRange(_min_val[], _max_val[]), _ambig[] != 0)
end

function findall(@nospecialize(sig::Type{<:Tuple}), table::CachedMethodTable; limit::Int=typemax(Int))
    box = Core.Box(sig)
    return get!(table.cache, sig) do
        findall(box.contents, table.table; limit=limit)
    end
end

"""
    findsup(sig::Type{<:Tuple}, view::MethodTableView)::Union{Tuple{MethodMatch, WorldRange}, Nothing}

Find the (unique) method `m` such that `sig <: m.sig`, while being more
specific than any other method with the same property. In other words, find
the method which is the least upper bound (supremum) under the specificity/subtype
relation of the queried `signature`. If `sig` is concrete, this is equivalent to
asking for the method that will be called given arguments whose types match the
given signature. This query is also used to implement `invoke`.

Such a method `m` need not exist. It is possible that no method is an
upper bound of `sig`, or it is possible that among the upper bounds, there
is no least element. In both cases `nothing` is returned.
"""
function findsup(@nospecialize(sig::Type{<:Tuple}), table::InternalMethodTable)
    min_valid = RefValue{UInt}(typemin(UInt))
    max_valid = RefValue{UInt}(typemax(UInt))
    result = ccall(:jl_gf_invoke_lookup_worlds, Any, (Any, UInt, Ptr{Csize_t}, Ptr{Csize_t}),
                   sig, table.world, min_valid, max_valid)::Union{Method, Nothing}
    result === nothing && return nothing
    (result, WorldRange(min_valid[], max_valid[]))
end

# This query is not cached
findsup(sig::Type{<:Tuple}, table::CachedMethodTable) = findsup(sig, table.table)
