# This file is a part of Julia. License is MIT: http://julialang.org/license

# OffsetArrays (arrays with indexing that doesn't start at 1)

# This test file is designed to exercise support for generic indexing,
# even though offset arrays aren't implemented in Base.

module OAs

using Base: SimIdx, Indices, LinearSlow, LinearFast

export OffsetArray

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
Base.summary(A::OffsetArray) = string(typeof(A))*" with indices "*string(indices(A))

# Implementations of indices and indices1. Since bounds-checking is
# performance-critical and relies on indices, these are usually worth
# optimizing thoroughly.
@inline Base.indices(A::OffsetArray, d) = 1 <= d <= length(A.offsets) ? (o = A.offsets[d]; (1+o:size(parent(A),d)+o)) : (1:1)
@inline Base.indices(A::OffsetArray) = _indices((), A)  # would rather use ntuple, but see #15276
@inline _indices{T,N}(out::NTuple{N}, A::OffsetArray{T,N}) = out
@inline _indices(out, A::OffsetArray) = (d = length(out)+1; o = A.offsets[d]; _indices((out..., (1+o:size(parent(A),d)+o)), A))
# By optimizing indices1 we can avoid a branch on the dim-check
Base.indices1{T}(A::OffsetArray{T,0}) = 1:1
@inline Base.indices1(A::OffsetArray) = (o = A.offsets[1]; 1+o:size(parent(A),1)+o)

function Base.similar(A::OffsetArray, T::Type, dims::Dims)
    B = similar(parent(A), T, dims)
end
function Base.similar(A::AbstractArray, T::Type, inds::Tuple{Vararg{SimIdx}})
    B = similar(A, T, map(Base.dimlength, inds))
    OffsetArray(B, map(indsoffset, inds))
end

Base.allocate_for(f, A::OffsetArray, shape::SimIdx) = OffsetArray(f(Base.dimlength(shape)), (indsoffset(shape),))
Base.allocate_for(f, A::OffsetArray, shape::Tuple{Vararg{SimIdx}}) = OffsetArray(f(map(Base.dimlength, shape)), map(indsoffset, shape))
Base.promote_indices(a::OffsetArray, b::OffsetArray) = a

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

using OAs

# Basics
A0 = [1 3; 2 4]
A = OffsetArray(A0, (-1,2))                   # LinearFast
S = OffsetArray(view(A0, 1:2, 1:2), (-1,2))  # LinearSlow
@test indices(A) == indices(S) == (0:1, 3:4)
@test A[0,3] == A[1] == S[0,3] == S[1] == 1
@test A[1,3] == A[2] == S[1,3] == S[2] == 2
@test A[0,4] == A[3] == S[0,4] == S[3] == 3
@test A[1,4] == A[4] == S[1,4] == S[4] == 4
@test_throws BoundsError A[1,1]
@test_throws BoundsError S[1,1]

# Vector indexing
@test A[:, 3] == S[:, 3] == OffsetArray([1,2], (A.offsets[1],))
@test A[:, 4] == S[:, 4] == OffsetArray([3,4], (A.offsets[1],))
@test_throws BoundsError A[:, 1]
@test_throws BoundsError S[:, 1]
@test A[0, :] == S[0, :] == OffsetArray([1,3], (A.offsets[2],))
@test A[1, :] == S[1, :] == OffsetArray([2,4], (A.offsets[2],))
@test_throws BoundsError A[2, :]
@test_throws BoundsError S[2, :]
@test A[0:1, 3] == S[0:1, 3] == [1,2]
@test A[[1,0], 3] == S[[1,0], 3] == [2,1]
@test A[0, 3:4] == S[0, 3:4] == [1,3]
@test A[1, [4,3]] == S[1, [4,3]] == [4,2]
@test A[:, :] == S[:, :] == A

# CartesianIndexing
@test A[CartesianIndex((0,3))] == S[CartesianIndex((0,3))] == 1
@test_throws BoundsError A[CartesianIndex(1,1)]
@test_throws BoundsError S[CartesianIndex(1,1)]
@test eachindex(A) == 1:4
@test eachindex(S) == CartesianRange((0:1,3:4))

# slice
S = view(A, :, 3)
@test S == OffsetArray([1,2], (A.offsets[1],))
@test S[0] == 1
@test S[1] == 2
@test_throws BoundsError S[2]
S = view(A, 0, :)
@test S == OffsetArray([1,3], (A.offsets[2],))
@test S[3] == 1
@test S[4] == 3
@test_throws BoundsError S[1]
S = view(A, 0:0, 4)
@test S == [3]
@test S[1] == 3
@test_throws BoundsError S[0]
S = view(A, 1, 3:4)
@test S == [2,4]
@test S[1] == 2
@test S[2] == 4
@test_throws BoundsError S[3]
S = view(A, :, :)
@test S == A
@test S[0,3] == S[1] == 1
@test S[1,3] == S[2] == 2
@test S[0,4] == S[3] == 3
@test S[1,4] == S[4] == 4
@test_throws BoundsError S[1,1]

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
show(io, MIME("text/plain"), A)
strs = split(strip(takebuf_string(io)), '\n')
@test strs[2] == " 1  3"
@test strs[3] == " 2  4"
v = OffsetArray(rand(3), (-2,))
show(io, v)
str = takebuf_string(io)
show(io, parent(v))
@test str == takebuf_string(io)
function cmp_showf(printfunc, io, A)
    ioc = IOContext(io, limit=true, compact=true)
    printfunc(ioc, A)
    str1 = takebuf_string(io)
    printfunc(ioc, parent(A))
    str2 = takebuf_string(io)
    @test str1 == str2
end
cmp_showf(Base.print_matrix, io, OffsetArray(rand(5,5), (10,-9)))       # rows&cols fit
cmp_showf(Base.print_matrix, io, OffsetArray(rand(10^3,5), (10,-9)))    # columns fit
cmp_showf(Base.print_matrix, io, OffsetArray(rand(5,10^3), (10,-9)))    # rows fit
cmp_showf(Base.print_matrix, io, OffsetArray(rand(10^3,10^3), (10,-9))) # neither fits

# Similar
B = similar(A, Float32)
@test isa(B, OffsetArray{Float32,2})
@test size(B) == size(A)
@test indices(B) == indices(A)
B = similar(A, (3,4))
@test isa(B, Array{Int,2})
@test size(B) == (3,4)
@test indices(B) == (1:3, 1:4)
B = similar(A, (-3:3,4))
@test isa(B, OffsetArray{Int,2})
@test indices(B) == (-3:3, 1:4)
B = similar(parent(A), (-3:3,4))
@test isa(B, OffsetArray{Int,2})
@test indices(B) == (-3:3, 1:4)

# Indexing with OffsetArray indices
i1 = OffsetArray([2,1], (-5,))
i1 = OffsetArray([2,1], -5)
b = A0[i1, 1]
@test indices(b) == (-4:-3,)
@test b[-4] == 2
@test b[-3] == 1
b = A0[1,i1]
@test indices(b) == (-4:-3,)
@test b[-4] == 3
@test b[-3] == 1

# logical indexing
@test A[A .> 2] == [3,4]

# copy!
a = OffsetArray{Int}((-3:-1,))
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
bo = OffsetArray(1:2, (-3,))
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
am = OffsetArray{Int}((1:1, 7:9))  # for testing linear indexing
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

A = OffsetArray(rand(4,4), (-3,5))
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

v  = OffsetArray([1,1e100,1,-1e100], (-3,))*1000
v2 = OffsetArray([1,-1e100,1,1e100], (5,))*1000
@test isa(v, OffsetArray)
cv  = OffsetArray([1,1e100,1e100,2], (-3,))*1000
cv2 = OffsetArray([1,-1e100,-1e100,2], (5,))*1000
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
v = OffsetArray(rand(8), (-2,))
@test sort(v) == OffsetArray(sort(parent(v)), v.offsets)
@test sortrows(A) == OffsetArray(sortrows(parent(A)), A.offsets)
@test sortcols(A) == OffsetArray(sortcols(parent(A)), A.offsets)
@test sort(A, 1) == OffsetArray(sort(parent(A), 1), A.offsets)
@test sort(A, 2) == OffsetArray(sort(parent(A), 2), A.offsets)

@test mapslices(v->sort(v), A, 1) == OffsetArray(mapslices(v->sort(v), parent(A), 1), A.offsets)
@test mapslices(v->sort(v), A, 2) == OffsetArray(mapslices(v->sort(v), parent(A), 2), A.offsets)

@test rotl90(A) == OffsetArray(rotl90(parent(A)), A.offsets[[2,1]])
@test rotr90(A) == OffsetArray(rotr90(parent(A)), A.offsets[[2,1]])
@test flipdim(A, 1) == OffsetArray(flipdim(parent(A), 1), A.offsets)
@test flipdim(A, 2) == OffsetArray(flipdim(parent(A), 2), A.offsets)

@test A+1 == OffsetArray(parent(A)+1, A.offsets)
@test 2*A == OffsetArray(2*parent(A), A.offsets)
@test A+A == OffsetArray(parent(A)+parent(A), A.offsets)
@test A.*A == OffsetArray(parent(A).*parent(A), A.offsets)
