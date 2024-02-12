# A disjoint set implementation adapted from
# https://github.com/JuliaCollections/DataStructures.jl/blob/f57330a3b46f779b261e6c07f199c88936f28839/src/disjoint_set.jl
# under the MIT license: https://github.com/JuliaCollections/DataStructures.jl/blob/master/License.md

# imports
import ._TOP_MOD:
    length,
    eltype,
    union!,
    push!
# usings
import ._TOP_MOD:
    OneTo, collect, zero, zeros, one, typemax

# Disjoint-Set

############################################################
#
#   A forest of disjoint sets of integers
#
#   Since each element is an integer, we can use arrays
#   instead of dictionary (for efficiency)
#
#   Disjoint sets over other key types can be implemented
#   based on an IntDisjointSet through a map from the key
#   to an integer index
#
############################################################

_intdisjointset_bounds_err_msg(T) = "the maximum number of elements in IntDisjointSet{$T} is $(typemax(T))"

"""
    IntDisjointSet{T<:Integer}(n::Integer)

A forest of disjoint sets of integers, which is a data structure
(also called a union–find data structure or merge–find set)
that tracks a set of elements partitioned
into a number of disjoint (non-overlapping) subsets.
"""
mutable struct IntDisjointSet{T<:Integer}
    parents::Vector{T}
    ranks::Vector{T}
    ngroups::T
end

IntDisjointSet(n::T) where {T<:Integer} = IntDisjointSet{T}(collect(OneTo(n)), zeros(T, n), n)
IntDisjointSet{T}(n::Integer) where {T<:Integer} = IntDisjointSet{T}(collect(OneTo(T(n))), zeros(T, T(n)), T(n))
length(s::IntDisjointSet) = length(s.parents)

"""
    num_groups(s::IntDisjointSet)

Get a number of groups.
"""
num_groups(s::IntDisjointSet) = s.ngroups
eltype(::Type{IntDisjointSet{T}}) where {T<:Integer} = T

# find the root element of the subset that contains x
# path compression is implemented here
function find_root_impl!(parents::Vector{T}, x::Integer) where {T<:Integer}
    p = parents[x]
    @inbounds if parents[p] != p
        parents[x] = p = _find_root_impl!(parents, p)
    end
    return p
end

# unsafe version of the above
function _find_root_impl!(parents::Vector{T}, x::Integer) where {T<:Integer}
    @inbounds p = parents[x]
    @inbounds if parents[p] != p
        parents[x] = p = _find_root_impl!(parents, p)
    end
    return p
end

"""
    find_root!(s::IntDisjointSet{T}, x::T)

Find the root element of the subset that contains an member `x`.
Path compression happens here.
"""
find_root!(s::IntDisjointSet{T}, x::T) where {T<:Integer} = find_root_impl!(s.parents, x)

"""
    in_same_set(s::IntDisjointSet{T}, x::T, y::T)

Returns `true` if `x` and `y` belong to the same subset in `s`, and `false` otherwise.
"""
in_same_set(s::IntDisjointSet{T}, x::T, y::T) where {T<:Integer} = find_root!(s, x) == find_root!(s, y)

"""
    union!(s::IntDisjointSet{T}, x::T, y::T)

Merge the subset containing `x` and that containing `y` into one
and return the root of the new set.
"""
function union!(s::IntDisjointSet{T}, x::T, y::T) where {T<:Integer}
    parents = s.parents
    xroot = find_root_impl!(parents, x)
    yroot = find_root_impl!(parents, y)
    return xroot != yroot ? root_union!(s, xroot, yroot) : xroot
end

"""
    root_union!(s::IntDisjointSet{T}, x::T, y::T)

Form a new set that is the union of the two sets whose root elements are
`x` and `y` and return the root of the new set.
Assume `x ≠ y` (unsafe).
"""
function root_union!(s::IntDisjointSet{T}, x::T, y::T) where {T<:Integer}
    parents = s.parents
    rks = s.ranks
    @inbounds xrank = rks[x]
    @inbounds yrank = rks[y]

    if xrank < yrank
        x, y = y, x
    elseif xrank == yrank
        rks[x] += one(T)
    end
    @inbounds parents[y] = x
    s.ngroups -= one(T)
    return x
end

"""
    push!(s::IntDisjointSet{T})

Make a new subset with an automatically chosen new element `x`.
Returns the new element. Throw an `ArgumentError` if the
capacity of the set would be exceeded.
"""
function push!(s::IntDisjointSet{T}) where {T<:Integer}
    l = length(s)
    l < typemax(T) || throw(ArgumentError(_intdisjointset_bounds_err_msg(T)))
    x = l + one(T)
    push!(s.parents, x)
    push!(s.ranks, zero(T))
    s.ngroups += one(T)
    return x
end
