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

@inline Base.getindex{T,N,AA,perm}(A::PermutedDimsArray{T,N,AA,perm}, I::Int...) = getindex(A.parent, _pda_reindex(perm, (), I)...)
@inline Base.setindex!{T,N,AA,perm}(A::PermutedDimsArray{T,N,AA,perm}, val, I::Int...) = setindex!(A.parent, val, _pda_reindex(perm, (), I)...)

@inline _pda_reindex(::Tuple{}, out, I) = out
@inline _pda_reindex(perm, out, I) = _pda_reindex(Base.tail(perm), (out..., I[perm[1]]), I)

function Base.permutedims{T,N}(A::AbstractArray{T,N}, perm)
    sz::NTuple{N,Int} = size(A)[perm]
    dest = similar(A, sz)
    P = PermutedDimsArray(dest, invperm(perm))
    copy!(P, A)
    dest
end

end
