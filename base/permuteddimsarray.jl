# This file is a part of Julia. License is MIT: https://julialang.org/license

module PermutedDimsArrays

import Base: permutedims, permutedims!
export PermutedDimsArray

# Some day we will want storage-order-aware iteration, so put perm in the parameters
struct PermutedDimsArray{T,N,perm,iperm,AA<:AbstractArray} <: AbstractArray{T,N}
    parent::AA

    function PermutedDimsArray{T,N,perm,iperm,AA}(data::AA) where {T,N,perm,iperm,AA<:AbstractArray}
        (isa(perm, NTuple{N,Int}) && isa(iperm, NTuple{N,Int})) || error("perm and iperm must both be NTuple{$N,Int}")
        isperm(perm) || throw(ArgumentError(string(perm, " is not a valid permutation of dimensions 1:", N)))
        all(map(d->iperm[perm[d]]==d, 1:N)) || throw(ArgumentError(string(perm, " and ", iperm, " must be inverses")))
        new(data)
    end
end

"""
    PermutedDimsArray(A, perm) -> B

Given an AbstractArray `A`, create a view `B` such that the
dimensions appear to be permuted. Similar to `permutedims`, except
that no copying occurs (`B` shares storage with `A`).

See also: [`permutedims`](@ref).

# Examples
```jldoctest
julia> A = rand(3,5,4);

julia> B = PermutedDimsArray(A, (3,1,2));

julia> size(B)
(4, 3, 5)

julia> B[3,1,2] == A[1,2,3]
true
```
"""
function PermutedDimsArray(data::AbstractArray{T,N}, perm) where {T,N}
    length(perm) == N || throw(ArgumentError(string(perm, " is not a valid permutation of dimensions 1:", N)))
    iperm = invperm(perm)
    PermutedDimsArray{T,N,(perm...,),(iperm...,),typeof(data)}(data)
end

Base.parent(A::PermutedDimsArray) = A.parent
Base.size(A::PermutedDimsArray{T,N,perm}) where {T,N,perm} = genperm(size(parent(A)), perm)
Base.axes(A::PermutedDimsArray{T,N,perm}) where {T,N,perm} = genperm(axes(parent(A)), perm)

Base.similar(A::PermutedDimsArray, T::Type, dims::Base.Dims) = similar(parent(A), T, dims)

Base.unsafe_convert(::Type{Ptr{T}}, A::PermutedDimsArray{T}) where {T} = Base.unsafe_convert(Ptr{T}, parent(A))

# It's OK to return a pointer to the first element, and indeed quite
# useful for wrapping C routines that require a different storage
# order than used by Julia. But for an array with unconventional
# storage order, a linear offset is ambiguous---is it a memory offset
# or a linear index?
Base.pointer(A::PermutedDimsArray, i::Integer) = throw(ArgumentError("pointer(A, i) is deliberately unsupported for PermutedDimsArray"))

function Base.strides(A::PermutedDimsArray{T,N,perm}) where {T,N,perm}
    s = strides(parent(A))
    ntuple(d->s[perm[d]], Val(N))
end

@inline function Base.getindex(A::PermutedDimsArray{T,N,perm,iperm}, I::Vararg{Int,N}) where {T,N,perm,iperm}
    @boundscheck checkbounds(A, I...)
    @inbounds val = getindex(A.parent, genperm(I, iperm)...)
    val
end
@inline function Base.setindex!(A::PermutedDimsArray{T,N,perm,iperm}, val, I::Vararg{Int,N}) where {T,N,perm,iperm}
    @boundscheck checkbounds(A, I...)
    @inbounds setindex!(A.parent, val, genperm(I, iperm)...)
    val
end

@inline genperm(I::NTuple{N,Any}, perm::Dims{N}) where {N} = ntuple(d -> I[perm[d]], Val(N))
@inline genperm(I, perm::AbstractVector{Int}) = genperm(I, (perm...,))

"""
    permutedims(A::AbstractArray, perm)

Permute the dimensions of array `A`. `perm` is a vector specifying a permutation of length
`ndims(A)`.

See also: [`PermutedDimsArray`](@ref).

# Examples
```jldoctest
julia> A = reshape(Vector(1:8), (2,2,2))
2×2×2 Array{Int64,3}:
[:, :, 1] =
 1  3
 2  4

[:, :, 2] =
 5  7
 6  8

julia> permutedims(A, [3, 2, 1])
2×2×2 Array{Int64,3}:
[:, :, 1] =
 1  3
 5  7

[:, :, 2] =
 2  4
 6  8
```
"""
function permutedims(A::AbstractArray, perm)
    dest = similar(A, genperm(axes(A), perm))
    permutedims!(dest, A, perm)
end

"""
    permutedims(m::AbstractMatrix)

Permute the dimensions of the matrix `m`, by flipping the elements across the diagonal of
the matrix. Differs from `LinearAlgebra`'s [`transpose`](@ref) in that the
operation is not recursive.

# Examples
```jldoctest; setup = :(using LinearAlgebra)
julia> a = [1 2; 3 4];

julia> b = [5 6; 7 8];

julia> c = [9 10; 11 12];

julia> d = [13 14; 15 16];

julia> X = [[a] [b]; [c] [d]]
2×2 Array{Array{Int64,2},2}:
 [1 2; 3 4]     [5 6; 7 8]
 [9 10; 11 12]  [13 14; 15 16]

julia> permutedims(X)
2×2 Array{Array{Int64,2},2}:
 [1 2; 3 4]  [9 10; 11 12]
 [5 6; 7 8]  [13 14; 15 16]

julia> transpose(X)
2×2 Transpose{Transpose{Int64,Array{Int64,2}},Array{Array{Int64,2},2}}:
 [1 3; 2 4]  [9 11; 10 12]
 [5 7; 6 8]  [13 15; 14 16]
```
"""
permutedims(A::AbstractMatrix) = permutedims(A, (2,1))

"""
    permutedims(v::AbstractVector)

Reshape vector `v` into a `1 × length(v)` row matrix.
Differs from `LinearAlgebra`'s [`transpose`](@ref) in that
the operation is not recursive.

# Examples
```jldoctest; setup = :(using LinearAlgebra)
julia> permutedims([1, 2, 3, 4])
1×4 Array{Int64,2}:
 1  2  3  4

julia> V = [[[1 2; 3 4]]; [[5 6; 7 8]]]
2-element Array{Array{Int64,2},1}:
 [1 2; 3 4]
 [5 6; 7 8]

julia> permutedims(V)
1×2 Array{Array{Int64,2},2}:
 [1 2; 3 4]  [5 6; 7 8]

julia> transpose(V)
1×2 Transpose{Transpose{Int64,Array{Int64,2}},Array{Array{Int64,2},1}}:
 [1 3; 2 4]  [5 7; 6 8]
```
"""
permutedims(v::AbstractVector) = reshape(v, (1, length(v)))

"""
    permutedims!(dest, src, perm)

Permute the dimensions of array `src` and store the result in the array `dest`. `perm` is a
vector specifying a permutation of length `ndims(src)`. The preallocated array `dest` should
have `size(dest) == size(src)[perm]` and is completely overwritten. No in-place permutation
is supported and unexpected results will happen if `src` and `dest` have overlapping memory
regions.

See also [`permutedims`](@ref).
"""
function permutedims!(dest, src::AbstractArray, perm)
    Base.checkdims_perm(dest, src, perm)
    P = PermutedDimsArray(dest, invperm(perm))
    _copy!(P, src)
    return dest
end

function Base.copyto!(dest::PermutedDimsArray{T,N}, src::AbstractArray{T,N}) where {T,N}
    checkbounds(dest, axes(src)...)
    _copy!(dest, src)
end
Base.copyto!(dest::PermutedDimsArray, src::AbstractArray) = _copy!(dest, src)

function _copy!(P::PermutedDimsArray{T,N,perm}, src) where {T,N,perm}
    # If dest/src are "close to dense," then it pays to be cache-friendly.
    # Determine the first permuted dimension
    d = 0  # d+1 will hold the first permuted dimension of src
    while d < ndims(src) && perm[d+1] == d+1
        d += 1
    end
    if d == ndims(src)
        copyto!(parent(P), src) # it's not permuted
    else
        R1 = CartesianIndices(axes(src)[1:d])
        d1 = findfirst(isequal(d+1), perm)::Int  # first permuted dim of dest
        R2 = CartesianIndices(axes(src)[d+2:d1-1])
        R3 = CartesianIndices(axes(src)[d1+1:end])
        _permutedims!(P, src, R1, R2, R3, d+1, d1)
    end
    return P
end

@noinline function _permutedims!(P::PermutedDimsArray, src, R1::CartesianIndices{0}, R2, R3, ds, dp)
    ip, is = axes(src, dp), axes(src, ds)
    for jo in first(ip):8:last(ip), io in first(is):8:last(is)
        for I3 in R3, I2 in R2
            for j in jo:min(jo+7, last(ip))
                for i in io:min(io+7, last(is))
                    @inbounds P[i, I2, j, I3] = src[i, I2, j, I3]
                end
            end
        end
    end
    P
end

@noinline function _permutedims!(P::PermutedDimsArray, src, R1, R2, R3, ds, dp)
    ip, is = axes(src, dp), axes(src, ds)
    for jo in first(ip):8:last(ip), io in first(is):8:last(is)
        for I3 in R3, I2 in R2
            for j in jo:min(jo+7, last(ip))
                for i in io:min(io+7, last(is))
                    for I1 in R1
                        @inbounds P[I1, i, I2, j, I3] = src[I1, i, I2, j, I3]
                    end
                end
            end
        end
    end
    P
end

function Base.showarg(io::IO, A::PermutedDimsArray{T,N,perm}, toplevel) where {T,N,perm}
    print(io, "PermutedDimsArray(")
    Base.showarg(io, parent(A), false)
    print(io, ", ", perm, ')')
    toplevel && print(io, " with eltype ", eltype(A))
end

end
