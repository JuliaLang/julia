# This file is a part of Julia. License is MIT: http://julialang.org/license

module PermutedDimsArrays

using Base: IndicesStartAt1, IndicesBehavior, indicesbehavior

export permutedims

# Some day we will want storage-order-aware iteration, so put perm in the parameters
immutable PermutedDimsArray{T,N,AA<:AbstractArray,perm} <: AbstractArray{T,N}
    parent::AA
    iperm::NTuple{N,Int}
    dims::NTuple{N,Int}

    function PermutedDimsArray(data::AA)
        # TODO optimize isperm & invperm for low dimensions?
        isa(perm, NTuple{N,Int}) || error("perm must be an NTuple{$N,Int}")
        (length(perm) == N && isperm(perm)) || throw(ArgumentError(string(perm, " is not a valid permutation of dimensions 1:", N)))
        iperm = invperm(perm)
        new(data, iperm, genperm(size(data), perm))
    end
end

function PermutedDimsArray{T,N}(data::AbstractArray{T,N}, perm)
    length(perm) == N || throw(ArgumentError(string(p, " is not a valid permutation of dimensions 1:", N)))
    PermutedDimsArray{T,N,typeof(data),(perm...,)}(data)
end

Base.parent(A::PermutedDimsArray) = A.parent
Base.size(A::PermutedDimsArray) = A.dims
Base.indices{T,N,AA,perm}(A::PermutedDimsArray{T,N,AA,perm}, d) = indices(parent(A), perm[d])

@inline function Base.getindex{T,N}(A::PermutedDimsArray{T,N}, I::Vararg{Int,N})
    @boundscheck checkbounds(A, I...)
    @inbounds val = getindex(A.parent, genperm(I, A.iperm)...)
    val
end
@inline function Base.setindex!{T,N}(A::PermutedDimsArray{T,N}, val, I::Vararg{Int,N})
    @boundscheck checkbounds(A, I...)
    @inbounds setindex!(A.parent, val, genperm(I, A.iperm)...)
    val
end

# Could use ntuple(d->I[perm[d]], Val{N}) once #15276 is solved
@inline genperm{N}(I::NTuple{N}, perm::Dims{N}) = _genperm((), I, perm...)
_genperm(out, I) = out
@inline _genperm(out, I, p, perm...) = _genperm((out..., I[p]), I, perm...)
@inline genperm(I, perm::AbstractVector{Int}) = genperm(I, (perm...,))

function Base.permutedims{T,N}(A::AbstractArray{T,N}, perm)
    dest = similar_permute(A, perm)
    permutedims!(dest, A, perm)
end

similar_permute(A::AbstractArray, perm) = similar_permute(indicesbehavior(A), A, perm)
similar_permute{T,N}(::IndicesStartAt1, A::AbstractArray{T,N}, perm) = similar(A, genperm(size(A), perm))
similar_permute{T,N}(::IndicesBehavior, A::AbstractArray{T,N}, perm) = similar(A, genperm(indices(A), perm))

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

function _copy!{T,N,AA,perm}(P::PermutedDimsArray{T,N,AA,perm}, src)
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
