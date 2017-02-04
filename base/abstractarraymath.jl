# This file is a part of Julia. License is MIT: http://julialang.org/license

 ## Basic functions ##

isreal(x::AbstractArray) = all(isreal,x)
iszero(x::AbstractArray) = all(iszero,x)
isreal{T<:Real,n}(x::AbstractArray{T,n}) = true
all{T<:Integer}(::typeof(isinteger), ::AbstractArray{T}) = true

## Constructors ##

"""
    vec(a::AbstractArray) -> Vector

Reshape array `a` as a one-dimensional column vector.

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
```
"""
vec(a::AbstractArray) = reshape(a,_length(a))
vec(a::AbstractVector) = a

_sub(::Tuple{}, ::Tuple{}) = ()
_sub(t::Tuple, ::Tuple{}) = t
_sub(t::Tuple, s::Tuple) = _sub(tail(t), tail(s))

"""
    squeeze(A, dims)

Remove the dimensions specified by `dims` from array `A`.
Elements of `dims` must be unique and within the range `1:ndims(A)`.
`size(A,i)` must equal 1 for all `i` in `dims`.

```jldoctest
julia> a = reshape(collect(1:4),(2,2,1,1))
2×2×1×1 Array{Int64,4}:
[:, :, 1, 1] =
 1  3
 2  4

julia> squeeze(a,3)
2×2×1 Array{Int64,3}:
[:, :, 1] =
 1  3
 2  4
```
"""
function squeeze(A::AbstractArray, dims::Dims)
    for i in 1:length(dims)
        1 <= dims[i] <= ndims(A) || throw(ArgumentError("squeezed dims must be in range 1:ndims(A)"))
        size(A, dims[i]) == 1 || throw(ArgumentError("squeezed dims must all be size 1"))
        for j = 1:i-1
            dims[j] == dims[i] && throw(ArgumentError("squeezed dims must be unique"))
        end
    end
    d = ()
    for i = 1:ndims(A)
        if !in(i, dims)
            d = tuple(d..., size(A, i))
        end
    end
    reshape(A, d::typeof(_sub(size(A), dims)))
end

squeeze(A::AbstractArray, dim::Integer) = squeeze(A, (Int(dim),))


## Unary operators ##

conj{T<:Real}(x::AbstractArray{T}) = x
conj!{T<:Real}(x::AbstractArray{T}) = x

real{T<:Real}(x::AbstractArray{T}) = x
imag{T<:Real}(x::AbstractArray{T}) = zero(x)

+{T<:Number}(x::AbstractArray{T}) = x
*{T<:Number}(x::AbstractArray{T,2}) = x

# index A[:,:,...,i,:,:,...] where "i" is in dimension "d"

"""
    slicedim(A, d::Integer, i)

Return all the data of `A` where the index for dimension `d` equals `i`. Equivalent to
`A[:,:,...,i,:,:,...]` where `i` is in position `d`.

```jldoctest
julia> A = [1 2 3 4; 5 6 7 8]
2×4 Array{Int64,2}:
 1  2  3  4
 5  6  7  8

julia> slicedim(A,2,3)
2-element Array{Int64,1}:
 3
 7
```
"""
function slicedim(A::AbstractArray, d::Integer, i)
    d >= 1 || throw(ArgumentError("dimension must be ≥ 1"))
    nd = ndims(A)
    d > nd && (i == 1 || throw_boundserror(A, (ntuple(k->Colon(),nd)..., ntuple(k->1,d-1-nd)..., i)))
    A[setindex(indices(A), i, d)...]
end

function flipdim(A::AbstractVector, d::Integer)
    d == 1 || throw(ArgumentError("dimension to flip must be 1"))
    reverse(A)
end

"""
    flipdim(A, d::Integer)

Reverse `A` in dimension `d`.

```jldoctest
julia> b = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> flipdim(b,2)
2×2 Array{Int64,2}:
 2  1
 4  3
```
"""
function flipdim(A::AbstractArray, d::Integer)
    nd = ndims(A)
    1 ≤ d ≤ nd || throw(ArgumentError("dimension $d is not 1 ≤ $d ≤ $nd"))
    if isempty(A)
        return copy(A)
    end
    inds = indices(A)
    B = similar(A)
    nnd = 0
    for i = 1:nd
        nnd += Int(length(inds[i])==1 || i==d)
    end
    indsd = inds[d]
    sd = first(indsd)+last(indsd)
    if nnd==nd
        # flip along the only non-singleton dimension
        for i in indsd
            B[i] = A[sd-i]
        end
        return B
    end
    alli = [ indices(B,n) for n in 1:nd ]
    for i in indsd
        B[[ n==d ? sd-i : alli[n] for n in 1:nd ]...] = slicedim(A, d, i)
    end
    return B
end

function circshift(a::AbstractArray, shiftamt::Real)
    circshift!(similar(a), a, (Integer(shiftamt),))
end
circshift(a::AbstractArray, shiftamt::DimsInteger) = circshift!(similar(a), a, shiftamt)
"""
    circshift(A, shifts)

Circularly shift the data in an array. The second argument is a vector giving the amount to
shift in each dimension.

```jldoctest
julia> b = reshape(collect(1:16), (4,4))
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
```

See also [`circshift!`](@ref).
"""
function circshift(a::AbstractArray, shiftamt)
    circshift!(similar(a), a, map(Integer, (shiftamt...,)))
end

# Uses K-B-N summation
function cumsum_kbn{T<:AbstractFloat}(v::AbstractVector{T})
    r = similar(v)
    if isempty(v); return r; end

    inds = indices(v, 1)
    i1 = first(inds)
    s = r[i1] = v[i1]
    c = zero(T)
    for i=i1+1:last(inds)
        vi = v[i]
        t = s + vi
        if abs(s) >= abs(vi)
            c += ((s-t) + vi)
        else
            c += ((vi-t) + s)
        end
        s = t
        r[i] = s+c
    end
    return r
end

# Uses K-B-N summation
# TODO: Needs a separate LinearSlow method, this is only fast for LinearIndexing

"""
    cumsum_kbn(A, [dim::Integer=1])

Cumulative sum along a dimension, using the Kahan-Babuska-Neumaier compensated summation
algorithm for additional accuracy. The dimension defaults to 1.
"""
function cumsum_kbn{T<:AbstractFloat}(A::AbstractArray{T}, axis::Integer=1)
    dimsA = size(A)
    ndimsA = ndims(A)
    axis_size = dimsA[axis]
    axis_stride = 1
    for i = 1:(axis-1)
        axis_stride *= size(A,i)
    end

    if axis_size <= 1
        return A
    end

    B = similar(A)
    C = similar(A)

    for i = 1:length(A)
        if div(i-1, axis_stride) % axis_size == 0
            B[i] = A[i]
            C[i] = zero(T)
        else
            s = B[i-axis_stride]
            Ai = A[i]
            B[i] = t = s + Ai
            if abs(s) >= abs(Ai)
                C[i] = C[i-axis_stride] + ((s-t) + Ai)
            else
                C[i] = C[i-axis_stride] + ((Ai-t) + s)
            end
        end
    end

    return B + C
end

## Other array functions ##

"""
    repmat(A, m::Integer, n::Integer=1)

Construct a matrix by repeating the given matrix (or vector) `m` times in dimension 1 and `n` times in
dimension 2.

```jldoctest
julia> repmat([1, 2, 3], 2)
6-element Array{Int64,1}:
 1
 2
 3
 1
 2
 3

julia> repmat([1, 2, 3], 2, 3)
6×3 Array{Int64,2}:
 1  1  1
 2  2  2
 3  3  3
 1  1  1
 2  2  2
 3  3  3
```
"""
function repmat(a::AbstractVecOrMat, m::Int, n::Int=1)
    o, p = size(a,1), size(a,2)
    b = similar(a, o*m, p*n)
    for j=1:n
        d = (j-1)*p+1
        R = d:d+p-1
        for i=1:m
            c = (i-1)*o+1
            b[c:c+o-1, R] = a
        end
    end
    return b
end

function repmat(a::AbstractVector, m::Int)
    o = length(a)
    b = similar(a, o*m)
    for i=1:m
        c = (i-1)*o+1
        b[c:c+o-1] = a
    end
    return b
end

@inline repmat(a, m::Integer...) = repmat(a, convert(Dims, m)...)

"""
    repeat(A::AbstractArray; inner=ntuple(x->1, ndims(A)), outer=ntuple(x->1, ndims(A)))

Construct an array by repeating the entries of `A`. The i-th element of `inner` specifies
the number of times that the individual entries of the i-th dimension of `A` should be
repeated. The i-th element of `outer` specifies the number of times that a slice along the
i-th dimension of `A` should be repeated. If `inner` or `outer` are omitted, no repetition
is performed.

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
function repeat(A::AbstractArray;
                inner=ntuple(x->1, ndims(A)),
                outer=ntuple(x->1, ndims(A)))
    ndims_in = ndims(A)
    length_inner = length(inner)
    length_outer = length(outer)

    length_inner >= ndims_in || throw(ArgumentError("number of inner repetitions ($(length(inner))) cannot be less than number of dimensions of input ($(ndims(A)))"))
    length_outer >= ndims_in || throw(ArgumentError("number of outer repetitions ($(length(outer))) cannot be less than number of dimensions of input ($(ndims(A)))"))

    ndims_out = max(ndims_in, length_inner, length_outer)

    inner = vcat(collect(inner), ones(Int,ndims_out-length_inner))
    outer = vcat(collect(outer), ones(Int,ndims_out-length_outer))

    size_in = size(A)
    size_out = ntuple(i->inner[i]*size(A,i)*outer[i],ndims_out)::Dims
    inner_size_out = ntuple(i->inner[i]*size(A,i),ndims_out)::Dims

    indices_in = Vector{Int}(ndims_in)
    indices_out = Vector{Int}(ndims_out)

    length_out = prod(size_out)
    R = similar(A, size_out)

    for index_out in 1:length_out
        ind2sub!(indices_out, size_out, index_out)
        for t in 1:ndims_in
            # "Project" outer repetitions into inner repetitions
            indices_in[t] = mod1(indices_out[t], inner_size_out[t])
            # Find inner repetitions using flooring division
            indices_in[t] = fld1(indices_in[t], inner[t])
        end
        index_in = sub2ind(size_in, indices_in...)
        R[index_out] = A[index_in]
    end

    return R
end
