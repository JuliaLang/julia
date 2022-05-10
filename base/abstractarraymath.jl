# This file is a part of Julia. License is MIT: https://julialang.org/license

 ## Basic functions ##

isreal(x::AbstractArray) = all(isreal,x)
iszero(x::AbstractArray) = all(iszero,x)
isreal(x::AbstractArray{<:Real}) = true

## Constructors ##

"""
    vec(a::AbstractArray) -> AbstractVector

Reshape the array `a` as a one-dimensional column vector. Return `a` if it is
already an `AbstractVector`. The resulting array
shares the same underlying data as `a`, so it will only be mutable if `a` is
mutable, in which case modifying one will also modify the other.

# Examples
```jldoctest
julia> a = [1 2 3; 4 5 6]
2×3 Matrix{Int64}:
 1  2  3
 4  5  6

julia> vec(a)
6-element Vector{Int64}:
 1
 4
 2
 5
 3
 6

julia> vec(1:3)
1:3
```

See also [`reshape`](@ref), [`dropdims`](@ref).
"""
vec(a::AbstractArray) = reshape(a,length(a))
vec(a::AbstractVector) = a

_sub(::Tuple{}, ::Tuple{}) = ()
_sub(t::Tuple, ::Tuple{}) = t
_sub(t::Tuple, s::Tuple) = _sub(tail(t), tail(s))

"""
    dropdims(A; dims)

Return an array with the same data as `A`, but with the dimensions specified by
`dims` removed. `size(A,d)` must equal 1 for every `d` in `dims`,
and repeated dimensions or numbers outside `1:ndims(A)` are forbidden.

The result shares the same underlying data as `A`, such that the
result is mutable if and only if `A` is mutable, and setting elements of one
alters the values of the other.

See also: [`reshape`](@ref), [`vec`](@ref).

# Examples
```jldoctest
julia> a = reshape(Vector(1:4),(2,2,1,1))
2×2×1×1 Array{Int64, 4}:
[:, :, 1, 1] =
 1  3
 2  4

julia> b = dropdims(a; dims=3)
2×2×1 Array{Int64, 3}:
[:, :, 1] =
 1  3
 2  4

julia> b[1,1,1] = 5; a
2×2×1×1 Array{Int64, 4}:
[:, :, 1, 1] =
 5  3
 2  4
```
"""
dropdims(A; dims) = _dropdims(A, dims)
function _dropdims(A::AbstractArray, dims::Dims)
    for i in eachindex(dims)
        1 <= dims[i] <= ndims(A) || throw(ArgumentError("dropped dims must be in range 1:ndims(A)"))
        length(axes(A, dims[i])) == 1 || throw(ArgumentError("dropped dims must all be size 1"))
        for j = 1:i-1
            dims[j] == dims[i] && throw(ArgumentError("dropped dims must be unique"))
        end
    end
    ax = _foldoneto((ds, d) -> d in dims ? ds : (ds..., axes(A,d)), (), Val(ndims(A)))
    reshape(A, ax::typeof(_sub(axes(A), dims)))
end
_dropdims(A::AbstractArray, dim::Integer) = _dropdims(A, (Int(dim),))

## Unary operators ##

"""
    conj!(A)

Transform an array to its complex conjugate in-place.

See also [`conj`](@ref).

# Examples
```jldoctest
julia> A = [1+im 2-im; 2+2im 3+im]
2×2 Matrix{Complex{Int64}}:
 1+1im  2-1im
 2+2im  3+1im

julia> conj!(A);

julia> A
2×2 Matrix{Complex{Int64}}:
 1-1im  2+1im
 2-2im  3-1im
```
"""
conj!(A::AbstractArray{<:Number}) = (@inbounds broadcast!(conj, A, A); A)
conj!(x::AbstractArray{<:Real}) = x

"""
    conj(A::AbstractArray)

Return an array containing the complex conjugate of each entry in array `A`.

Equivalent to `conj.(A)`, except that when `eltype(A) <: Real`
`A` is returned without copying, and that when `A` has zero dimensions,
a 0-dimensional array is returned (rather than a scalar).

# Examples
```jldoctest
julia> conj([1, 2im, 3 + 4im])
3-element Vector{Complex{Int64}}:
 1 + 0im
 0 - 2im
 3 - 4im

julia> conj(fill(2 - im))
0-dimensional Array{Complex{Int64}, 0}:
2 + 1im
```
"""
conj(A::AbstractArray) = broadcast_preserving_zero_d(conj, A)
conj(A::AbstractArray{<:Real}) = A

"""
    real(A::AbstractArray)

Return an array containing the real part of each entry in array `A`.

Equivalent to `real.(A)`, except that when `eltype(A) <: Real`
`A` is returned without copying, and that when `A` has zero dimensions,
a 0-dimensional array is returned (rather than a scalar).

# Examples
```jldoctest
julia> real([1, 2im, 3 + 4im])
3-element Vector{Int64}:
 1
 0
 3

julia> real(fill(2 - im))
0-dimensional Array{Int64, 0}:
2
```
"""
real(A::AbstractArray) = broadcast_preserving_zero_d(real, A)
real(A::AbstractArray{<:Real}) = A

"""
    imag(A::AbstractArray)

Return an array containing the imaginary part of each entry in array `A`.

Equivalent to `imag.(A)`, except that when `A` has zero dimensions,
a 0-dimensional array is returned (rather than a scalar).

# Examples
```jldoctest
julia> imag([1, 2im, 3 + 4im])
3-element Vector{Int64}:
 0
 2
 4

julia> imag(fill(2 - im))
0-dimensional Array{Int64, 0}:
-1
```
"""
imag(A::AbstractArray) = broadcast_preserving_zero_d(imag, A)
imag(A::AbstractArray{<:Real}) = zero(A)

"""
    reim(A::AbstractArray)

Return a tuple of two arrays containing respectively the real and the imaginary
part of each entry in `A`.

Equivalent to `(real.(A), imag.(A))`, except that when `eltype(A) <: Real`
`A` is returned without copying to represent the real part, and that when `A` has
zero dimensions, a 0-dimensional array is returned (rather than a scalar).

# Examples
```jldoctest
julia> reim([1, 2im, 3 + 4im])
([1, 0, 3], [0, 2, 4])

julia> reim(fill(2 - im))
(fill(2), fill(-1))
```
"""
reim(A::AbstractArray)

-(A::AbstractArray) = broadcast_preserving_zero_d(-, A)

+(x::AbstractArray{<:Number}) = x
*(x::AbstractArray{<:Number,2}) = x

# index A[:,:,...,i,:,:,...] where "i" is in dimension "d"

"""
    selectdim(A, d::Integer, i)

Return a view of all the data of `A` where the index for dimension `d` equals `i`.

Equivalent to `view(A,:,:,...,i,:,:,...)` where `i` is in position `d`.

See also: [`eachslice`](@ref).

# Examples
```jldoctest
julia> A = [1 2 3 4; 5 6 7 8]
2×4 Matrix{Int64}:
 1  2  3  4
 5  6  7  8

julia> selectdim(A, 2, 3)
2-element view(::Matrix{Int64}, :, 3) with eltype Int64:
 3
 7

julia> selectdim(A, 2, 3:4)
2×2 view(::Matrix{Int64}, :, 3:4) with eltype Int64:
 3  4
 7  8
```
"""
@inline selectdim(A::AbstractArray, d::Integer, i) = _selectdim(A, d, i, _setindex(i, d, map(Slice, axes(A))...))
@noinline function _selectdim(A, d, i, idxs)
    d >= 1 || throw(ArgumentError("dimension must be ≥ 1, got $d"))
    nd = ndims(A)
    d > nd && (i == 1 || throw(BoundsError(A, (ntuple(Returns(Colon()),d-1)..., i))))
    return view(A, idxs...)
end

function circshift(a::AbstractArray, shiftamt::Real)
    circshift!(similar(a), a, (Integer(shiftamt),))
end
circshift(a::AbstractArray, shiftamt::DimsInteger) = circshift!(similar(a), a, shiftamt)
"""
    circshift(A, shifts)

Circularly shift, i.e. rotate, the data in an array. The second argument is a tuple or
vector giving the amount to shift in each dimension, or an integer to shift only in the
first dimension.

See also: [`circshift!`](@ref), [`circcopy!`](@ref), [`bitrotate`](@ref), [`<<`](@ref).

# Examples
```jldoctest
julia> b = reshape(Vector(1:16), (4,4))
4×4 Matrix{Int64}:
 1  5   9  13
 2  6  10  14
 3  7  11  15
 4  8  12  16

julia> circshift(b, (0,2))
4×4 Matrix{Int64}:
  9  13  1  5
 10  14  2  6
 11  15  3  7
 12  16  4  8

julia> circshift(b, (-1,0))
4×4 Matrix{Int64}:
 2  6  10  14
 3  7  11  15
 4  8  12  16
 1  5   9  13

julia> a = BitArray([true, true, false, false, true])
5-element BitVector:
 1
 1
 0
 0
 1

julia> circshift(a, 1)
5-element BitVector:
 1
 1
 1
 0
 0

julia> circshift(a, -1)
5-element BitVector:
 1
 0
 0
 1
 1
```
"""
function circshift(a::AbstractArray, shiftamt)
    circshift!(similar(a), a, map(Integer, (shiftamt...,)))
end

## Other array functions ##

"""
    repeat(A::AbstractArray, counts::Integer...)

Construct an array by repeating array `A` a given number of times in each dimension, specified by `counts`.

See also: [`fill`](@ref), [`Iterators.repeated`](@ref), [`Iterators.cycle`](@ref).

# Examples
```jldoctest
julia> repeat([1, 2, 3], 2)
6-element Vector{Int64}:
 1
 2
 3
 1
 2
 3

julia> repeat([1, 2, 3], 2, 3)
6×3 Matrix{Int64}:
 1  1  1
 2  2  2
 3  3  3
 1  1  1
 2  2  2
 3  3  3
```
"""
function repeat(A::AbstractArray, counts...)
    return _RepeatInnerOuter.repeat(A, outer=counts)
end

"""
    repeat(A::AbstractArray; inner=ntuple(Returns(1), ndims(A)), outer=ntuple(Returns(1), ndims(A)))

Construct an array by repeating the entries of `A`. The i-th element of `inner` specifies
the number of times that the individual entries of the i-th dimension of `A` should be
repeated. The i-th element of `outer` specifies the number of times that a slice along the
i-th dimension of `A` should be repeated. If `inner` or `outer` are omitted, no repetition
is performed.

# Examples
```jldoctest
julia> repeat(1:2, inner=2)
4-element Vector{Int64}:
 1
 1
 2
 2

julia> repeat(1:2, outer=2)
4-element Vector{Int64}:
 1
 2
 1
 2

julia> repeat([1 2; 3 4], inner=(2, 1), outer=(1, 3))
4×6 Matrix{Int64}:
 1  2  1  2  1  2
 1  2  1  2  1  2
 3  4  3  4  3  4
 3  4  3  4  3  4
```
"""
function repeat(A::AbstractArray; inner = nothing, outer = nothing)
    return _RepeatInnerOuter.repeat(A, inner=inner, outer=outer)
end

module _RepeatInnerOuter

function repeat(arr; inner=nothing, outer=nothing)
    check(arr, inner, outer)
    arr, inner, outer = resolve(arr, inner, outer)
    repeat_inner_outer(arr, inner, outer)
end

to_tuple(t::Tuple) = t
to_tuple(x::Integer) = (x,)
to_tuple(itr) = tuple(itr...)

function pad(a, b)
    N = max(length(a), length(b))
    Base.fill_to_length(a, 1, Val(N)), Base.fill_to_length(b, 1, Val(N))
end
function pad(a, b, c)
    N = max(max(length(a), length(b)), length(c))
    Base.fill_to_length(a, 1, Val(N)), Base.fill_to_length(b, 1, Val(N)), Base.fill_to_length(c, 1, Val(N))
end

function resolve(arr::AbstractArray{<:Any, N}, inner::NTuple{N, Any}, outer::NTuple{N,Any}) where {N}
    arr, inner, outer
end
function resolve(arr, inner, outer)
    dims, inner, outer = pad(size(arr), to_tuple(inner), to_tuple(outer))
    reshape(arr, dims), inner, outer
end
function resolve(arr, inner::Nothing, outer::Nothing)
    return arr, inner, outer
end
function resolve(arr, inner::Nothing, outer)
    dims, outer = pad(size(arr), to_tuple(outer))
    reshape(arr, dims), inner, outer
end
function resolve(arr, inner, outer::Nothing)
    dims, inner = pad(size(arr), to_tuple(inner))
    reshape(arr, dims), inner, outer
end

function check(arr, inner, outer)
    if inner !== nothing
        # TODO: Currently one based indexing is demanded for inner !== nothing,
        # but not for outer !== nothing. Decide for something consistent.
        Base.require_one_based_indexing(arr)
        if any(<(0), inner)
            throw(ArgumentError("no inner repetition count may be negative; got $inner"))
        end
        if length(inner) < ndims(arr)
            throw(ArgumentError("number of inner repetitions ($(length(inner))) cannot be less than number of dimensions of input array ($(ndims(arr)))"))
        end
    end
    if outer !== nothing
        if any(<(0), outer)
            throw(ArgumentError("no outer repetition count may be negative; got $outer"))
        end
        if (length(outer) < ndims(arr)) && (inner !== nothing)
            throw(ArgumentError("number of outer repetitions ($(length(outer))) cannot be less than number of dimensions of input array ($(ndims(arr)))"))
        end
    end
end

repeat_inner_outer(arr, inner::Nothing, outer::Nothing) = arr
repeat_inner_outer(arr, ::Nothing, outer) = repeat_outer(arr, outer)
repeat_inner_outer(arr, inner, ::Nothing) = repeat_inner(arr, inner)
repeat_inner_outer(arr, inner, outer) = repeat_outer(repeat_inner(arr, inner), outer)

function repeat_outer(a::AbstractMatrix, (m,n)::NTuple{2, Any})
    o, p = size(a,1), size(a,2)
    b = similar(a, o*m, p*n)
    for j=1:n
        d = (j-1)*p+1
        R = d:d+p-1
        for i=1:m
            c = (i-1)*o+1
            @inbounds b[c:c+o-1, R] = a
        end
    end
    return b
end

function repeat_outer(a::AbstractVector, (m,)::Tuple{Any})
    o = length(a)
    b = similar(a, o*m)
    for i=1:m
        c = (i-1)*o+1
        @inbounds b[c:c+o-1] = a
    end
    return b
end

function repeat_outer(arr::AbstractArray{<:Any,N}, dims::NTuple{N,Any}) where {N}
    insize  = size(arr)
    outsize = map(*, insize, dims)
    out = similar(arr, outsize)
    for I in CartesianIndices(arr)
        for J in CartesianIndices(dims)
            TIJ = map(Tuple(I), Tuple(J), insize) do i, j, d
                i + d * (j-1)
            end
            IJ = CartesianIndex(TIJ)
            @inbounds out[IJ] = arr[I]
        end
    end
    return out
end

function repeat_inner(arr, inner)
    outsize = map(*, size(arr), inner)
    out = similar(arr, outsize)
    for I in CartesianIndices(arr)
        for J in CartesianIndices(inner)
            TIJ = map(Tuple(I), Tuple(J), inner) do i, j, d
                (i-1) * d + j
            end
            IJ = CartesianIndex(TIJ)
            @inbounds out[IJ] = arr[I]
        end
    end
    return out
end

end#module

"""
    eachrow(A::AbstractVecOrMat)

Create a generator that iterates over the first dimension of vector or matrix `A`,
returning the rows as `AbstractVector` views.

See also [`eachcol`](@ref), [`eachslice`](@ref), [`mapslices`](@ref).

!!! compat "Julia 1.1"
     This function requires at least Julia 1.1.

# Example

```jldoctest
julia> a = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> first(eachrow(a))
2-element view(::Matrix{Int64}, 1, :) with eltype Int64:
 1
 2

julia> collect(eachrow(a))
2-element Vector{SubArray{Int64, 1, Matrix{Int64}, Tuple{Int64, Base.Slice{Base.OneTo{Int64}}}, true}}:
 [1, 2]
 [3, 4]
```
"""
eachrow(A::AbstractVecOrMat) = (view(A, i, :) for i in axes(A, 1))


"""
    eachcol(A::AbstractVecOrMat)

Create a generator that iterates over the second dimension of matrix `A`, returning the
columns as `AbstractVector` views.

See also [`eachrow`](@ref) and [`eachslice`](@ref).

!!! compat "Julia 1.1"
     This function requires at least Julia 1.1.

# Example

```jldoctest
julia> a = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> first(eachcol(a))
2-element view(::Matrix{Int64}, :, 1) with eltype Int64:
 1
 3

julia> collect(eachcol(a))
2-element Vector{SubArray{Int64, 1, Matrix{Int64}, Tuple{Base.Slice{Base.OneTo{Int64}}, Int64}, true}}:
 [1, 3]
 [2, 4]
```
"""
eachcol(A::AbstractVecOrMat) = (view(A, :, i) for i in axes(A, 2))

"""
    eachslice(A::AbstractArray; dims)

Create a generator that iterates over dimensions `dims` of `A`, returning views that select all
the data from the other dimensions in `A`.

Only a single dimension in `dims` is currently supported. Equivalent to `(view(A,:,:,...,i,:,:
...)) for i in axes(A, dims))`, where `i` is in position `dims`.

See also [`eachrow`](@ref), [`eachcol`](@ref), [`mapslices`](@ref), and [`selectdim`](@ref).

!!! compat "Julia 1.1"
     This function requires at least Julia 1.1.

# Example

```jldoctest
julia> M = [1 2 3; 4 5 6; 7 8 9]
3×3 Matrix{Int64}:
 1  2  3
 4  5  6
 7  8  9

julia> first(eachslice(M, dims=1))
3-element view(::Matrix{Int64}, 1, :) with eltype Int64:
 1
 2
 3

julia> collect(eachslice(M, dims=2))
3-element Vector{SubArray{Int64, 1, Matrix{Int64}, Tuple{Base.Slice{Base.OneTo{Int64}}, Int64}, true}}:
 [1, 4, 7]
 [2, 5, 8]
 [3, 6, 9]
```
"""
@inline function eachslice(A::AbstractArray; dims)
    length(dims) == 1 || throw(ArgumentError("only single dimensions are supported"))
    dim = first(dims)
    dim <= ndims(A) || throw(DimensionMismatch("A doesn't have $dim dimensions"))
    inds_before = ntuple(Returns(:), dim-1)
    inds_after = ntuple(Returns(:), ndims(A)-dim)
    return (view(A, inds_before..., i, inds_after...) for i in axes(A, dim))
end
