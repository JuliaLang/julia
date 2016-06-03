# This file is a part of Julia. License is MIT: http://julialang.org/license

module PermutedDimsArrays

export permutedims

immutable PermutedDimsArray{T,N,AA<:AbstractArray,perm} <: AbstractArray{T,N}
    parent::AA
    dims::NTuple{N,Int}

    function PermutedDimsArray(data::AA, p::AbstractVector)
        isa(perm, NTuple{N,Int}) || error("perm must be an NTuple{$N,Int}")
        (length(perm) == N && isperm(perm)) || throw(ArgumentError(string(perm, " is not a valid permutation of dimensions 1:", N)))
        for i = 1:N
            perm[p[i]] == i || throw(ArgumentError("size permutation must be the inverse of perm"))
        end
        new(data, ([size(data)...][p]...,))
    end
end

function PermutedDimsArray{T,N}(data::AbstractArray{T,N}, p::AbstractVector)
    length(p) == N || throw(ArgumentError(string(p, " is not a valid permutation of dimensions 1:", N)))
    PermutedDimsArray{T,N,typeof(data),(invperm(p)...,)}(data, p)
end

Base.size(A::PermutedDimsArray) = A.dims

@inline function Base.getindex{T,N,AA,perm}(A::PermutedDimsArray{T,N,AA,perm}, I::Int...)
    @boundscheck checkbounds(A, I...)
    @inbounds val = getindex(A.parent, _pda_reindex(perm, (), I)...)
    val
end
@inline function Base.setindex!{T,N,AA,perm}(A::PermutedDimsArray{T,N,AA,perm}, val, I::Int...)
    @boundscheck checkbounds(A, I...)
    @inbounds setindex!(A.parent, val, _pda_reindex(perm, (), I)...)
    val
end

@inline _pda_reindex(::Tuple{}, out, I) = out
@inline _pda_reindex(perm, out, I) = _pda_reindex(Base.tail(perm), (out..., I[perm[1]]), I)

function Base.permutedims{T,N}(A::AbstractArray{T,N}, perm)
    sz::NTuple{N,Int} = size(A)[perm]
    dest = similar(A, sz)
    permutedims!(dest, A, perm)
end

function Base.permutedims!(dest, src::AbstractArray, perm)
    Base.checkdims_perm(dest, src, perm)
    P = PermutedDimsArray(dest, invperm(perm))
    # If dest/src are "close to dense," then it pays to be cache-friendly.
    # Determine the first permuted dimension
    d = 0  # d+1 will hold the first permuted dimension of src
    while d < ndims(src) && perm[d+1] == d+1
        d += 1
    end
    sz1 = size(src)[1:d]
    if prod(sz1) > 1
        copy!(P, src)
    else
        R1 = CartesianRange(sz1)
        d1 = findfirst(perm, d+1)  # first permuted dim of dest
        R2 = CartesianRange(size(src)[d+2:d1-1])
        R3 = CartesianRange(size(src)[d1+1:end])
        _permutedims!(P, src, R1, R2, R3, d+1, d1)
    end
    dest
end

@noinline function _permutedims!(P::PermutedDimsArray, src, R1::CartesianRange{CartesianIndex{0}}, R2, R3, ds, dp)
    for jo in 1:8:size(src, dp), io in 1:8:size(src, ds)
        for I3 in R3, I2 in R2
            for j in jo:min(jo+7, size(src, dp))
                for i in io:min(io+7, size(src, ds))
                    @inbounds P[i, I2, j, I3] = src[i, I2, j, I3]
                end
            end
        end
    end
    P
end

@noinline function _permutedims!(P::PermutedDimsArray, src, R1, R2, R3, ds, dp)
    for jo in 1:8:size(src, dp), io in 1:8:size(src, ds)
        for I3 in R3, I2 in R2
            for j in jo:min(jo+7, size(src, dp))
                for i in io:min(io+7, size(src, ds))
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
