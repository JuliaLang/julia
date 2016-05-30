# This file is a part of Julia. License is MIT: http://julialang.org/license

# OffsetArrays (arrays with indexing that doesn't start at 1)

# This test file is designed to exercise support for generic indexing,
# even though offset arrays aren't implemented in Base.

module OAs

using Base: SimIdx, Indices, LinearSlow, LinearFast

immutable OffsetArray{T,N,AA<:AbstractArray} <: AbstractArray{T,N}
    parent::AA
    offsets::NTuple{N,Int}
end
typealias OffsetVector{T,AA<:AbstractArray} OffsetArray{T,1,AA}

OffsetArray{T,N}(A::AbstractArray{T,N}, offsets::NTuple{N,Int}) = OffsetArray{T,N,typeof(A)}(A, offsets)
OffsetArray{T,N}(A::AbstractArray{T,N}, offsets::Vararg{Int,N}) = OffsetArray(A, offsets)

(::Type{OffsetArray{T,N}}){T,N}(inds::Indices{N}) = OffsetArray{T,N,Array{T,N}}(Array{T,N}(map(Base.dimlength, inds)), map(indsoffset, inds))
(::Type{OffsetArray{T}}){T,N}(inds::Indices{N}) = OffsetArray{T,N}(inds)

Base.parent(A::OffsetArray) = A.parent
Base.size(A::OffsetArray) = size(parent(A))
Base.indices(A::OffsetArray, d) = 1 <= d <= length(A.offsets) ? ((1:size(parent(A),d))+A.offsets[d]) : (1:1)
Base.linearindexing(A::OffsetArray) = Base.linearindexing(parent(A))
Base.eachindex(::LinearSlow, A::OffsetArray) = CartesianRange(indices(A))
Base.eachindex(::LinearFast, A::OffsetVector) = indices(A, 1)
Base.summary(A::OffsetArray) = string(typeof(A))*" with indices "*string(indices(A))

Base.similar(A::OffsetArray, T::Type) = similar(parent(A), T, indices(A))
function Base.similar(A::OffsetArray, T::Type, dims::Dims)
    B = similar(parent(A), T, dims)
end
function Base.similar(A::AbstractArray, T::Type, inds::Tuple{Vararg{SimIdx}})
    B = similar(A, T, map(Base.dimlength, inds))
    OffsetArray(B, map(indsoffset, inds))
end

Base.ind2sub(A::OffsetArray, ind) = ind2sub(indices(A), ind)
@inline Base.sub2ind(A::OffsetArray, I...) = sub2ind(indices(A), I...)

@inline function Base.getindex{T,N}(A::OffsetArray{T,N}, I::Vararg{Int,N})
    @boundscheck checkbounds(A, I...)
    @inbounds ret = parent(A)[offset(A.offsets, I)...]
    ret
end
@inline function Base._getindex(::LinearFast, A::OffsetVector, i::Int)
    @boundscheck checkbounds(A, i)
    @inbounds ret = parent(A)[offset(A.offsets, (i,))[1]]
    ret
end
@inline function Base._getindex(::LinearFast, A::OffsetArray, i::Int)
    @boundscheck checkbounds(A, i)
    @inbounds ret = parent(A)[i]
    ret
end
@inline function Base.setindex!{T,N}(A::OffsetArray{T,N}, val, I::Vararg{Int,N})
    @boundscheck checkbounds(A, I...)
    @inbounds parent(A)[offset(A.offsets, I)...] = val
    val
end
@inline function Base._setindex!(::LinearFast, A::OffsetVector, val, i::Int)
    @boundscheck checkbounds(A, i)
    @inbounds parent(A)[offset(A.offsets, (i,))[1]] = val
    val
end
@inline function Base._setindex!(::LinearFast, A::OffsetArray, val, i::Int)
    @boundscheck checkbounds(A, i)
    @inbounds parent(A)[i] = val
    val
end

# Ensuring that indexing with OffsetArray indices works as expected
@inline Base.index_shape_dim(A, dim, i::OffsetArray{Bool}, I...) = (sum(i)..., Base.index_shape_dim(A, dim+1, I...)...)
@inline Base.index_shape_dim{N}(A, dim, i::OffsetArray{CartesianIndex{N}}, I...) = (indices(i)..., Base.index_shape_dim(A, dim+N, I...)...)
@inline Base.index_shape_dim(A, dim, i::OffsetArray, I...) = (indices(i)..., Base.index_shape_dim(A, dim+1, I...)...)

# Computing a shifted index (subtracting the offset)
offset{N}(offsets::NTuple{N,Int}, inds::NTuple{N,Int}) = _offset((), offsets, inds)
_offset(out, ::Tuple{}, ::Tuple{}) = out
@inline _offset(out, offsets, inds) = _offset((out..., inds[1]-offsets[1]), Base.tail(offsets), Base.tail(inds))

indsoffset(r::Range) = first(r) - 1
indsoffset(i::Integer) = 0

end

# Basics
A0 = [1 3; 2 4]
A = OAs.OffsetArray(A0, (-1,2))                   # LinearFast
S = OAs.OffsetArray(slice(A0, 1:2, 1:2), (-1,2))  # LinearSlow
@test indices(A) == indices(S) == (0:1, 3:4)
@test A[0,3] == A[1] == S[0,3] == S[1] == 1
@test A[1,3] == A[2] == S[1,3] == S[2] == 2
@test A[0,4] == A[3] == S[0,4] == S[3] == 3
@test A[1,4] == A[4] == S[1,4] == S[4] == 4
@test_throws BoundsError A[1,1]
@test_throws BoundsError S[1,1]

# Vector indexing
@test A[:, 3] == S[:, 3] == [1,2]
@test A[:, 4] == S[:, 4] == [3,4]
@test_throws BoundsError A[:, 1]
@test_throws BoundsError S[:, 1]
@test A[0, :] == S[0, :] == [1,3]
@test A[1, :] == S[1, :] == [2,4]
@test_throws BoundsError A[2, :]
@test_throws BoundsError S[2, :]
@test A[0:1, 3] == S[0:1, 3] == [1,2]
@test A[[1,0], 3] == S[[1,0], 3] == [2,1]
@test A[0, 3:4] == S[0, 3:4] == [1,3]
@test A[1, [4,3]] == S[1, [4,3]] == [4,2]
@test A[:, :] == S[:, :] == A0

# CartesianIndexing
@test A[CartesianIndex((0,3))] == S[CartesianIndex((0,3))] == 1
@test_throws BoundsError A[CartesianIndex(1,1)]
@test_throws BoundsError S[CartesianIndex(1,1)]
@test eachindex(A) == 1:4
@test eachindex(S) == CartesianRange((0:1,3:4))

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

# Indexing with OffsetArray indices
A = [1 3; 2 4]
i1 = OAs.OffsetArray([2,1], (-5,))
i1 = OAs.OffsetArray([2,1], -5)
b = A[i1, 1]
@test indices(b) == (-4:-3,)
@test b[-4] == 2
@test b[-3] == 1
b = A[1,i1]
@test indices(b) == (-4:-3,)
@test b[-4] == 3
@test b[-3] == 1
