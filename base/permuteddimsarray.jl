# This file is a part of Julia. License is MIT: http://julialang.org/license

module PermutedDimsArrays

export permutedims, PermutedDimsArray

# Some day we will want storage-order-aware iteration, so put perm in the parameters
immutable PermutedDimsArray{T,N,perm,iperm,AA<:AbstractArray} <: AbstractArray{T,N}
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

# Example

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
Base.size{T,N,perm}(A::PermutedDimsArray{T,N,perm})    = genperm(size(parent(A)),    perm)
Base.indices{T,N,perm}(A::PermutedDimsArray{T,N,perm}) = genperm(indices(parent(A)), perm)

Base.unsafe_convert{T}(::Type{Ptr{T}}, A::PermutedDimsArray{T}) = Base.unsafe_convert(Ptr{T}, parent(A))

# It's OK to return a pointer to the first element, and indeed quite
# useful for wrapping C routines that require a different storage
# order than used by Julia. But for an array with unconventional
# storage order, a linear offset is ambiguous---is it a memory offset
# or a linear index?
Base.pointer{T}(A::PermutedDimsArray{T}, i::Integer) = throw(ArgumentError("pointer(A, i) is deliberately unsupported for PermutedDimsArray"))

function Base.strides{T,N,perm}(A::PermutedDimsArray{T,N,perm})
    s = strides(parent(A))
    ntuple(d->s[perm[d]], Val{N})
end

@inline function Base.getindex{T,N,perm,iperm}(A::PermutedDimsArray{T,N,perm,iperm}, I::Vararg{Int,N})
    @boundscheck checkbounds(A, I...)
    @inbounds val = getindex(A.parent, genperm(I, iperm)...)
    val
end
@inline function Base.setindex!{T,N,perm,iperm}(A::PermutedDimsArray{T,N,perm,iperm}, val, I::Vararg{Int,N})
    @boundscheck checkbounds(A, I...)
    @inbounds setindex!(A.parent, val, genperm(I, iperm)...)
    val
end

# For some reason this is faster than ntuple(d->I[perm[d]], Val{N}) (#15276?)
@inline genperm{N}(I::NTuple{N,Any}, perm::Dims{N}) = _genperm((), I, perm...)
_genperm(out, I) = out
@inline _genperm(out, I, p, perm...) = _genperm((out..., I[p]), I, perm...)
@inline genperm(I, perm::AbstractVector{Int}) = genperm(I, (perm...,))

"""
    permutedims(A, perm)

Permute the dimensions of array `A`. `perm` is a vector specifying a permutation of length
`ndims(A)`. This is a generalization of transpose for multi-dimensional arrays. Transpose is
equivalent to `permutedims(A, [2,1])`.

See also: [`PermutedDimsArray`](@ref).

```jldoctest
julia> A = reshape(collect(1:8), (2,2,2))
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
function Base.permutedims{T,N}(A::AbstractArray{T,N}, perm)
    dest = similar(A, genperm(indices(A), perm))
    permutedims!(dest, A, perm)
end

"""
    permutedims!(dest, src, perm)

Permute the dimensions of array `src` and store the result in the array `dest`. `perm` is a
vector specifying a permutation of length `ndims(src)`. The preallocated array `dest` should
have `size(dest) == size(src)[perm]` and is completely overwritten. No in-place permutation
is supported and unexpected results will happen if `src` and `dest` have overlapping memory
regions.
"""
function Base.permutedims!(dest, src::AbstractArray, perm)
    Base.checkdims_perm(dest, src, perm)
    P = PermutedDimsArray(dest, invperm(perm))
    _copy!(P, src)
    return dest
end

function Base.copy!{T,N}(dest::PermutedDimsArray{T,N}, src::AbstractArray{T,N})
    checkbounds(dest, indices(src)...)
    _copy!(dest, src)
end
Base.copy!(dest::PermutedDimsArray, src::AbstractArray) = _copy!(dest, src)

function _copy!{T,N,perm}(P::PermutedDimsArray{T,N,perm}, src)
    # If dest/src are "close to dense," then it pays to be cache-friendly.
    # Determine the first permuted dimension
    d = 0  # d+1 will hold the first permuted dimension of src
    while d < ndims(src) && perm[d+1] == d+1
        d += 1
    end
    if d == ndims(src)
        copy!(parent(P), src) # it's not permuted
    else
        R1 = CartesianRange(indices(src)[1:d])
        d1 = findfirst(perm, d+1)  # first permuted dim of dest
        R2 = CartesianRange(indices(src)[d+2:d1-1])
        R3 = CartesianRange(indices(src)[d1+1:end])
        _permutedims!(P, src, R1, R2, R3, d+1, d1)
    end
    return P
end

@noinline function _permutedims!(P::PermutedDimsArray, src, R1::CartesianRange{CartesianIndex{0}}, R2, R3, ds, dp)
    ip, is = indices(src, dp), indices(src, ds)
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
    ip, is = indices(src, dp), indices(src, ds)
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

end
