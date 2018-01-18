# This file is a part of Julia. License is MIT: https://julialang.org/license

# TODO: remove the type stubs below between 0.7 and 1.0

"""
    RowVector(vector)

A lazy-view wrapper of an [`AbstractVector`](@ref), which turns a length-`n` vector into a `1×n`
shaped row vector and represents the transpose of a vector (the elements are also transposed
recursively).

By convention, a vector can be multiplied by a matrix on its left (`A * v`) whereas a row
vector can be multiplied by a matrix on its right (such that `RowVector(v) * A = RowVector(transpose(A) * v)`). It
differs from a `1×n`-sized matrix by the facts that its transpose returns a vector and the
inner product `RowVector(v1) * v2` returns a scalar, but will otherwise behave similarly.

# Examples
```jldoctest
julia> a = [1; 2; 3; 4]
4-element Array{Int64,1}:
 1
 2
 3
 4

julia> RowVector(a)
1×4 RowVector{Int64,Array{Int64,1}}:
 1  2  3  4

julia> RowVector(a)[3]
3

julia> RowVector(a)[1,3]
3

julia> RowVector(a)[3,1]
ERROR: BoundsError: attempt to access 1×4 RowVector{Int64,Array{Int64,1}} at index [3, 1]
[...]

julia> RowVector(a)*a
30

julia> B = [1 2; 3 4; 5 6; 7 8]
4×2 Array{Int64,2}:
 1  2
 3  4
 5  6
 7  8

julia> RowVector(a)*B
1×2 RowVector{Int64,Array{Int64,1}}:
 50  60
```
"""
struct RowVector{T,V<:AbstractVector} <: AbstractMatrix{T}
    vec::V
    function RowVector{T,V}(v::V) where V<:AbstractVector where T
        check_types(T,v)
        new(v)
    end
end
const ConjRowVector{T,CV<:ConjVector} = RowVector{T,CV}
