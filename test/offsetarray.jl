# This file is a part of Julia. License is MIT: http://julialang.org/license

# OffsetArrays (arrays with indexing that doesn't start at 1)

# This test file is designed to exercise support for generic indexing,
# even though offset arrays aren't implemented in Base.

module OAs

using Base: SimIdx, Indices

immutable OffsetArray{T,N,AA<:AbstractArray} <: AbstractArray{T,N}
    parent::AA
    offsets::NTuple{N,Int}
end
typealias OffsetVector{T,AA<:AbstractArray} OffsetArray{T,1,AA}

OffsetArray{T,N}(A::AbstractArray{T,N}, offsets::NTuple{N,Int}) = OffsetArray{T,N,typeof(A)}(A, offsets)
OffsetArray{T,N}(A::AbstractArray{T,N}, offsets::Vararg{Int,N}) = OffsetArray(A, offsets)

(::Type{OffsetArray{T,N}}){T,N}(inds::Indices{N}) = OffsetArray{T,N,Array{T,N}}(Array{T,N}(map(dimlength, inds)), map(indsoffset, inds))
(::Type{OffsetArray{T}}){T,N}(inds::Indices{N}) = OffsetArray{T,N}(inds)

Base.parent(A::OffsetArray) = A.parent
Base.size(A::OffsetArray) = size(parent(A))
Base.indices(A::OffsetArray, d) = 1 <= d <= length(A.offsets) ? ((1:size(parent(A),d))+A.offsets[d]) : (1:1)
Base.linearindexing(A::OffsetArray) = Base.linearindexing(parent(A))
Base.eachindex(::Base.LinearSlow, A::OffsetArray) = CartesianRange(indices(A))
Base.eachindex(::Base.LinearFast, A::OffsetVector) = indices1(A, 1)
Base.summary(A::OffsetArray) = string(typeof(A))*" with indices "*string(indices(A))

Base.similar(A::OffsetArray, T::Type) = similar(parent(A), T, indices(A))
function Base.similar(A::OffsetArray, T::Type, dims::Dims)
    B = similar(parent(A), T, dims)
end
function Base.similar(A::AbstractArray, T::Type, inds::Tuple{Vararg{SimIdx}})
    B = similar(A, T, map(dimlength, inds))
    OffsetArray(B, map(indsoffset, inds))
end

Base.ind2sub(A::OffsetArray, ind) = ind2sub(indices(A), ind)
@inline Base.sub2ind(A::OffsetArray, I...) = sub2ind(indices(A), I...)

@inline function Base.getindex{T,N}(A::OffsetArray{T,N}, I::Vararg{Int,N})
    @boundscheck checkbounds(A, I...)
    @inbounds ret = parent(A)[offset(A.offsets, I)...]
    ret
end
@inline function Base.setindex!{T,N}(A::OffsetArray{T,N}, val, I::Vararg{Int,N})
    @boundscheck checkbounds(A, I...)
    @inbounds parent(A)[offset(A.offsets, I)...] = val
    val
end

# Computing a shifted index (subtracting the offset)
offset{N}(offsets::NTuple{N,Int}, inds::NTuple{N,Int}) = _offset((), offsets, inds)
_offset(out, ::Tuple{}, ::Tuple{}) = out
@inline _offset(out, offsets, inds) = _offset((out..., inds[1]-offsets[1]), Base.tail(offsets), Base.tail(inds))

dimlength(r::Range) = length(r)
dimlength(i::Integer) = i

indsoffset(r::Range) = first(r) - 1
indsoffset(i::Integer) = 0

end

# Basics
A = OAs.OffsetArray([1 3; 2 4], (-1,2))
@test indices(A) == (0:1, 3:4)
@test A[0,3] == 1
@test A[1,3] == 2
@test A[0,4] == 3
@test A[1,4] == 4
@test_throws BoundsError A[1,1]

# Vector indexing
@test A[:, 3] == [1,2]
@test A[:, 4] == [3,4]
@test_throws BoundsError A[:, 1]
@test A[0, :] == [1,3]
@test A[1, :] == [2,4]
@test_throws BoundsError A[2, :]
@test A[0:1, 3] == [1,2]
@test A[[1,0], 3] == [2,1]
@test A[0, 3:4] == [1,3]
@test A[1, [4,3]] == [4,2]
@test A[:, :] == [1 3; 2 4]

# CartesianIndexing
@test A[CartesianIndex((0,3))] == 1
@test_throws BoundsError A[CartesianIndex(1,1)]
@test eachindex(A) == CartesianRange((0:1,3:4))

# Similar
B = similar(A, Float32)
@test isa(B, OAs.OffsetArray{Float32,2})
@test size(B) == size(A)
@test indices(B) == indices(A)
B = similar(A, (3,4))
@test isa(B, Array{Int,2})
@test size(B) == (3,4)
@test indices(B) == (1:3, 1:4)
B = similar(A, (-3:3,4))
@test isa(B, OAs.OffsetArray{Int,2})
@test indices(B) == (-3:3, 1:4)
B = similar(parent(A), (-3:3,4))
@test isa(B, OAs.OffsetArray{Int,2})
@test indices(B) == (-3:3, 1:4)
