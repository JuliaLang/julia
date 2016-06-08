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

Base.linearindexing{T<:OffsetArray}(::Type{T}) = Base.linearindexing(parenttype(T))
Base.indicesbehavior{T<:OffsetArray}(::Type{T}) = Base.IndicesUnitRange()
parenttype{T,N,AA}(::Type{OffsetArray{T,N,AA}}) = AA
parenttype(A::OffsetArray) = parenttype(typeof(A))

Base.parent(A::OffsetArray) = A.parent
Base.size(A::OffsetArray) = size(parent(A))
Base.indices(A::OffsetArray, d) = 1 <= d <= length(A.offsets) ? ((1:size(parent(A),d))+A.offsets[d]) : (1:1)
Base.eachindex(::LinearSlow, A::OffsetArray) = CartesianRange(indices(A))
Base.eachindex(::LinearFast, A::OffsetVector) = indices(A, 1)
Base.summary(A::OffsetArray) = string(typeof(A))*" with indices "*string(indices(A))

function Base.similar(A::OffsetArray, T::Type, dims::Dims)
    B = similar(parent(A), T, dims)
end
function Base.similar(A::AbstractArray, T::Type, inds::Tuple{Vararg{SimIdx}})
    B = similar(A, T, map(Base.dimlength, inds))
    OffsetArray(B, map(indsoffset, inds))
end

Base.allocate_for(f, A::OffsetArray, dim::Integer) = OffsetArray(f(size(A,dim)), (A.offsets[dim],))
Base.allocate_for{N}(f, A::OffsetArray, dims::Dims{N}) = OffsetArray(f(size(A,dims)), A.offsets[dims]::Dims{N})

Base.reshape(A::AbstractArray, inds::Indices) = OffsetArray(reshape(A, map(Base.dimlength, inds)), map(indsoffset, inds))

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

# slice
@test slice(A, :, 3) == [1,2]
@test slice(A, 0, :) == [1,3]
@test slice(A, 0:0, 4) == [3]
@test slice(A, 1, 3:4) == [2,4]

# iteration
for (a,d) in zip(A, A0)
    @test a == d
end


# show
io = IOBuffer()
show(io, A)
str = takebuf_string(io)
@test str == "[1 3; 2 4]"
show(io, S)
str = takebuf_string(io)
@test str == "[1 3; 2 4]"
show(IOContext(io, multiline=true), A)
strs = split(strip(takebuf_string(io)), '\n')
@test strs[2] == " 1  3"
@test strs[3] == " 2  4"
v = OAs.OffsetArray(rand(3), (-2,))
show(io, v)
str = takebuf_string(io)
show(io, parent(v))
@test str == takebuf_string(io)

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

# copy!
a = OAs.OffsetArray{Int}((-3:-1,))
fill!(a, -1)
copy!(a, (1,2))   # non-array iterables
@test a[-3] == 1
@test a[-2] == 2
@test a[-1] == -1
fill!(a, -1)
copy!(a, -2, (1,2))
@test a[-3] == -1
@test a[-2] == 1
@test a[-1] == 2
@test_throws BoundsError copy!(a, 1, (1,2))
fill!(a, -1)
copy!(a, -2, (1,2,3), 2)
@test a[-3] == -1
@test a[-2] == 2
@test a[-1] == 3
@test_throws BoundsError copy!(a, -2, (1,2,3), 1)
fill!(a, -1)
copy!(a, -2, (1,2,3), 1, 2)
@test a[-3] == -1
@test a[-2] == 1
@test a[-1] == 2

b = 1:2    # copy between AbstractArrays
bo = OAs.OffsetArray(1:2, (-3,))
@test_throws BoundsError copy!(a, b)
fill!(a, -1)
copy!(a, bo)
@test a[-3] == -1
@test a[-2] == 1
@test a[-1] == 2
fill!(a, -1)
copy!(a, -2, bo)
@test a[-3] == -1
@test a[-2] == 1
@test a[-1] == 2
@test_throws BoundsError copy!(a, -4, bo)
@test_throws BoundsError copy!(a, -1, bo)
fill!(a, -1)
copy!(a, -3, b, 2)
@test a[-3] == 2
@test a[-2] == a[-1] == -1
@test_throws BoundsError copy!(a, -3, b, 1, 4)
am = OAs.OffsetArray{Int}((1:1, 7:9))  # for testing linear indexing
fill!(am, -1)
copy!(am, b)
@test am[1] == 1
@test am[2] == 2
@test am[3] == -1
@test am[1,7] == 1
@test am[1,8] == 2
@test am[1,9] == -1

dest = similar(am)
map!(+, dest, am, am)
@test dest[1,7] == 2
@test dest[1,8] == 4
@test dest[1,9] == -2

A = OAs.OffsetArray(rand(4,4), (-3,5))
@test maximum(A) == maximum(parent(A))
@test minimum(A) == minimum(parent(A))
@test extrema(A) == extrema(parent(A))
C = similar(A)
cumsum!(C, A, 1)
@test parent(C) == cumsum(parent(A), 1)
@test parent(cumsum(A, 1)) == cumsum(parent(A), 1)
cumsum!(C, A, 2)
@test parent(C) == cumsum(parent(A), 2)
R = similar(A, (-2:-2, 6:9))
maximum!(R, A)
@test parent(R) == maximum(parent(A), 1)
R = similar(A, (-2:1, 6:6))
maximum!(R, A)
@test parent(R) == maximum(parent(A), 2)
amin, iamin = findmin(A)
pmin, ipmin = findmin(parent(A))
@test amin == pmin
@test A[iamin] == amin
@test amin == parent(A)[ipmin]
amax, iamax = findmax(A)
pmax, ipmax = findmax(parent(A))
@test amax == pmax
@test A[iamax] == amax
@test amax == parent(A)[ipmax]

v  = OAs.OffsetArray([1,1e100,1,-1e100], (-3,))*1000
v2 = OAs.OffsetArray([1,-1e100,1,1e100], (5,))*1000
@test isa(v, OAs.OffsetArray)
cv  = OAs.OffsetArray([1,1e100,1e100,2], (-3,))*1000
cv2 = OAs.OffsetArray([1,-1e100,-1e100,2], (5,))*1000
@test isequal(cumsum_kbn(v), cv)
@test isequal(cumsum_kbn(v2), cv2)
@test isequal(sum_kbn(v), sum_kbn(parent(v)))

io = IOBuffer()
writedlm(io, A)
seek(io, 0)
@test readdlm(io, eltype(A)) == parent(A)

amin, amax = extrema(parent(A))
@test clamp(A, (amax+amin)/2, amax) == clamp(parent(A), (amax+amin)/2, amax)

@test unique(A, 1) == parent(A)
@test unique(A, 2) == parent(A)
v = OAs.OffsetArray(rand(8), (-2,))
@test sort(v) == OAs.OffsetArray(sort(parent(v)), v.offsets)
@test sortrows(A) == OAs.OffsetArray(sortrows(parent(A)), (A.offsets[1], 0))
@test sortcols(A) == OAs.OffsetArray(sortcols(parent(A)), (0, A.offsets[2]))
@test sort(A, 1) == OAs.OffsetArray(sort(parent(A), 1), A.offsets)
@test sort(A, 2) == OAs.OffsetArray(sort(parent(A), 2), A.offsets)

@test mapslices(v->sort(v), A, 1) == OAs.OffsetArray(mapslices(v->sort(v), parent(A), 1), (0,A.offsets[2]))
@test mapslices(v->sort(v), A, 2) == OAs.OffsetArray(mapslices(v->sort(v), parent(A), 2), (A.offsets[1], 0))

@test rotl90(A) == OAs.OffsetArray(rotl90(parent(A)), A.offsets[[2,1]])
@test rotr90(A) == OAs.OffsetArray(rotr90(parent(A)), A.offsets[[2,1]])
# @test flipdim(A, 1) == flipdim(parent(A), 1)
# @test flipdim(A, 2) == flipdim(parent(A), 2)

@test A+1 == OAs.OffsetArray(parent(A)+1, A.offsets)
@test 2*A == OAs.OffsetArray(2*parent(A), A.offsets)
@test A+A == OAs.OffsetArray(parent(A)+parent(A), A.offsets)
