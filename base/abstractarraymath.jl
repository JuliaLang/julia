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
2×3 Array{Int64,2}:
 1  2  3
 4  5  6

julia> vec(a)
6-element Array{Int64,1}:
 1
 4
 2
 5
 3
 6

julia> vec(1:3)
1:3
```

See also [`reshape`](@ref).
"""
vec(a::AbstractArray) = reshape(a,length(a))
vec(a::AbstractVector) = a

_sub(::Tuple{}, ::Tuple{}) = ()
_sub(t::Tuple, ::Tuple{}) = t
_sub(t::Tuple, s::Tuple) = _sub(tail(t), tail(s))

"""
    dropdims(A; dims)

Remove the dimensions specified by `dims` from array `A`.
Elements of `dims` must be unique and within the range `1:ndims(A)`.
`size(A,i)` must equal 1 for all `i` in `dims`.

# Examples
```jldoctest
julia> a = reshape(Vector(1:4),(2,2,1,1))
2×2×1×1 Array{Int64,4}:
[:, :, 1, 1] =
 1  3
 2  4

julia> dropdims(a; dims=3)
2×2×1 Array{Int64,3}:
[:, :, 1] =
 1  3
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
    d = ()
    for i = 1:ndims(A)
        if !in(i, dims)
            d = tuple(d..., axes(A, i))
        end
    end
    reshape(A, d::typeof(_sub(axes(A), dims)))
end
_dropdims(A::AbstractArray, dim::Integer) = _dropdims(A, (Int(dim),))

## Unary operators ##

conj(x::AbstractArray{<:Real}) = x
conj!(x::AbstractArray{<:Real}) = x

real(x::AbstractArray{<:Real}) = x
imag(x::AbstractArray{<:Real}) = zero(x)

+(x::AbstractArray{<:Number}) = x
*(x::AbstractArray{<:Number,2}) = x

# index A[:,:,...,i,:,:,...] where "i" is in dimension "d"

"""
    selectdim(A, d::Integer, i)

Return a view of all the data of `A` where the index for dimension `d` equals `i`.

Equivalent to `view(A,:,:,...,i,:,:,...)` where `i` is in position `d`.

# Examples
```jldoctest
julia> A = [1 2 3 4; 5 6 7 8]
2×4 Array{Int64,2}:
 1  2  3  4
 5  6  7  8

julia> selectdim(A, 2, 3)
2-element view(::Array{Int64,2}, :, 3) with eltype Int64:
 3
 7
```
"""
@inline selectdim(A::AbstractArray, d::Integer, i) = _selectdim(A, d, i, _setindex(i, d, map(Slice, axes(A))...))
@noinline function _selectdim(A, d, i, idxs)
    d >= 1 || throw(ArgumentError("dimension must be ≥ 1, got $d"))
    nd = ndims(A)
    d > nd && (i == 1 || throw(BoundsError(A, (ntuple(k->Colon(),d-1)..., i))))
    return view(A, idxs...)
end

"""
    reverse(A; dims::Integer)

Reverse `A` in dimension `dims`.

# Examples
```jldoctest
julia> b = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> reverse(b, dims=2)
2×2 Array{Int64,2}:
 2  1
 4  3
```
"""
function reverse(A::AbstractArray; dims::Integer)
    nd = ndims(A); d = dims
    1 ≤ d ≤ nd || throw(ArgumentError("dimension $d is not 1 ≤ $d ≤ $nd"))
    if isempty(A)
        return copy(A)
    elseif nd == 1
        return reverse(A)
    end
    inds = axes(A)
    B = similar(A)
    nnd = 0
    for i = 1:nd
        nnd += Int(length(inds[i])==1 || i==d)
    end
    indsd = inds[d]
    sd = first(indsd)+last(indsd)
    if nnd==nd
        # reverse along the only non-singleton dimension
        for i in indsd
            B[i] = A[sd-i]
        end
        return B
    end
    let B=B # workaround #15276
        alli = [ axes(B,n) for n in 1:nd ]
        for i in indsd
            B[[ n==d ? sd-i : alli[n] for n in 1:nd ]...] = selectdim(A, d, i)
        end
    end
    return B
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

# Examples
```jldoctest
julia> b = reshape(Vector(1:16), (4,4))
4×4 Array{Int64,2}:
 1  5   9  13
 2  6  10  14
 3  7  11  15
 4  8  12  16

julia> circshift(b, (0,2))
4×4 Array{Int64,2}:
  9  13  1  5
 10  14  2  6
 11  15  3  7
 12  16  4  8

julia> circshift(b, (-1,0))
4×4 Array{Int64,2}:
 2  6  10  14
 3  7  11  15
 4  8  12  16
 1  5   9  13

julia> a = BitArray([true, true, false, false, true])
5-element BitArray{1}:
 1
 1
 0
 0
 1

julia> circshift(a, 1)
5-element BitArray{1}:
 1
 1
 1
 0
 0

julia> circshift(a, -1)
5-element BitArray{1}:
 1
 0
 0
 1
 1
```

See also [`circshift!`](@ref).
"""
function circshift(a::AbstractArray, shiftamt)
    circshift!(similar(a), a, map(Integer, (shiftamt...,)))
end

## Other array functions ##

"""
    repeat(A::AbstractArray, counts::Integer...)

Construct an array by repeating array `A` a given number of times in each dimension, specified by `counts`.

# Examples
```jldoctest
julia> repeat([1, 2, 3], 2)
6-element Array{Int64,1}:
 1
 2
 3
 1
 2
 3

julia> repeat([1, 2, 3], 2, 3)
6×3 Array{Int64,2}:
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
    repeat(A::AbstractArray; inner=ntuple(x->1, ndims(A)), outer=ntuple(x->1, ndims(A)))

Construct an array by repeating the entries of `A`. The i-th element of `inner` specifies
the number of times that the individual entries of the i-th dimension of `A` should be
repeated. The i-th element of `outer` specifies the number of times that a slice along the
i-th dimension of `A` should be repeated. If `inner` or `outer` are omitted, no repetition
is performed.

# Examples
```jldoctest
julia> repeat(1:2, inner=2)
4-element Array{Int64,1}:
 1
 1
 2
 2

julia> repeat(1:2, outer=2)
4-element Array{Int64,1}:
 1
 2
 1
 2

julia> repeat([1 2; 3 4], inner=(2, 1), outer=(1, 3))
4×6 Array{Int64,2}:
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
        if any(<(0), inner)
            msg = """
            Number of repetitions cannot be negative. Got inner = $inner
            """
            throw(ArgumentError(msg))
        end
        if length(inner) < ndims(arr)
            msg = """
            Number of dimensions of repetitions and array must match. Got
            inner = $inner
            size(arr) = $(size(arr))
            """
            throw(ArgumentError(msg))
        end
    end
    if outer != nothing
        if any(<(0), outer)
            msg = """
            Number of repetitions cannot be negative. Got outer = $outer
            """
            throw(ArgumentError(msg))
        end
        if (length(outer) < ndims(arr)) && (inner != nothing)
            msg = """
            Number of dimensions of repetitions and array must match. Got
            outer = $outer
            size(arr) = $(size(arr))
            """
            throw(ArgumentError(msg))
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
    basedims = size(arr)
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

See also [`eachcol`](@ref) and [`eachslice`](@ref).

!!! compat "Julia 1.1"
     This function requires at least Julia 1.1.

# Example

```jldoctest
julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> first(eachrow(a))
2-element view(::Array{Int64,2}, 1, :) with eltype Int64:
 1
 2

julia> collect(eachrow(a))
2-element Array{SubArray{Int64,1,Array{Int64,2},Tuple{Int64,Base.Slice{Base.OneTo{Int64}}},true},1}:
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
2×2 Array{Int64,2}:
 1  2
 3  4

julia> first(eachcol(a))
2-element view(::Array{Int64,2}, :, 1) with eltype Int64:
 1
 3

julia> collect(eachcol(a))
2-element Array{SubArray{Int64,1,Array{Int64,2},Tuple{Base.Slice{Base.OneTo{Int64}},Int64},true},1}:
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

See also [`eachrow`](@ref), [`eachcol`](@ref), and [`selectdim`](@ref).

!!! compat "Julia 1.1"
     This function requires at least Julia 1.1.
"""
@inline function eachslice(A::AbstractArray; dims)
    length(dims) == 1 || throw(ArgumentError("only single dimensions are supported"))
    dim = first(dims)
    dim <= ndims(A) || throw(DimensionMismatch("A doesn't have $dim dimensions"))
    inds_before = ntuple(d->(:), dim-1)
    inds_after = ntuple(d->(:), ndims(A)-dim)
    return (view(A, inds_before..., i, inds_after...) for i in axes(A, dim))
end
