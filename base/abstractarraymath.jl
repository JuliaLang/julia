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
repeat(a::AbstractArray, counts::Integer...) = repeat(a, outer = counts)

function repeat(a::AbstractVecOrMat, m::Integer, n::Integer=1)
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

function repeat(a::AbstractVector, m::Integer)
    o = length(a)
    b = similar(a, o*m)
    for i=1:m
        c = (i-1)*o+1
        b[c:c+o-1] = a
    end
    return b
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
    return _repeat_inner_outer(A, inner, outer)
end

# we have optimized implementations of these cases above
_repeat_inner_outer(A::AbstractVecOrMat, ::Nothing, r::Union{Tuple{Integer},Tuple{Integer,Integer}}) = repeat(A, r...)
_repeat_inner_outer(A::AbstractVecOrMat, ::Nothing, r::Integer) = repeat(A, r)

_repeat_inner_outer(A, ::Nothing, ::Nothing) = A
_repeat_inner_outer(A, ::Nothing, outer) = _repeat(A, ntuple(n->1, Val(ndims(A))), rep_kw2tup(outer))
_repeat_inner_outer(A, inner, ::Nothing) = _repeat(A, rep_kw2tup(inner), ntuple(n->1, Val(ndims(A))))
_repeat_inner_outer(A, inner, outer)     = _repeat(A, rep_kw2tup(inner), rep_kw2tup(outer))

rep_kw2tup(n::Integer) = (n,)
rep_kw2tup(v::AbstractArray{<:Integer}) = (v...,)
rep_kw2tup(t::Tuple) = t

rep_shapes(A, i, o) = _rshps((), (), size(A), i, o)

_rshps(shp, shp_i, ::Tuple{}, ::Tuple{}, ::Tuple{}) = (shp, shp_i)
@inline _rshps(shp, shp_i, ::Tuple{}, ::Tuple{}, o) =
    _rshps((shp..., o[1]), (shp_i..., 1), (), (), tail(o))
@inline _rshps(shp, shp_i, ::Tuple{}, i, ::Tuple{}) = (n = i[1];
    _rshps((shp..., n), (shp_i..., n), (), tail(i), ()))
@inline _rshps(shp, shp_i, ::Tuple{}, i, o) = (n = i[1];
    _rshps((shp..., n * o[1]), (shp_i..., n), (), tail(i), tail(o)))
@inline _rshps(shp, shp_i, sz, i, o) = (n = sz[1] * i[1];
    _rshps((shp..., n * o[1]), (shp_i..., n), tail(sz), tail(i), tail(o)))
_rshps(shp, shp_i, sz, ::Tuple{}, ::Tuple{}) =
    (n = length(shp); N = n + length(sz); _reperr("inner", n, N))
_rshps(shp, shp_i, sz, ::Tuple{}, o) =
    (n = length(shp); N = n + length(sz); _reperr("inner", n, N))
_rshps(shp, shp_i, sz, i, ::Tuple{}) =
    (n = length(shp); N = n + length(sz); _reperr("outer", n, N))
_reperr(s, n, N) = throw(ArgumentError("number of " * s * " repetitions " *
    "($n) cannot be less than number of dimensions of input ($N)"))

@noinline function _repeat(A::AbstractArray, inner, outer)
    shape, inner_shape = rep_shapes(A, inner, outer)

    R = similar(A, shape)
    if any(iszero, shape)
        return R
    end

    # fill the first inner block
    if all(isequal(1), inner)
        idxs = (axes(A)..., ntuple(n->OneTo(1), ndims(R)-ndims(A))...) # keep dimension consistent
        R[idxs...] = A
    else
        inner_indices = [1:n for n in inner]
        for c in CartesianIndices(axes(A))
            for i in 1:ndims(A)
                n = inner[i]
                inner_indices[i] = (1:n) .+ ((c[i] - 1) * n)
            end
            fill!(view(R, inner_indices...), A[c])
        end
    end

    # fill the outer blocks along each dimension
    if all(isequal(1), outer)
        return R
    end
    src_indices  = [1:n for n in inner_shape]
    dest_indices = copy(src_indices)
    for i in eachindex(outer)
        B = view(R, src_indices...)
        for j in 2:outer[i]
            dest_indices[i] = dest_indices[i] .+ inner_shape[i]
            R[dest_indices...] = B
        end
        src_indices[i] = dest_indices[i] = 1:shape[i]
    end

    return R
end

"""
    eachrow(A::AbstractVecOrMat)

Create a generator that iterates over the first dimension of vector or matrix `A`,
returning the rows as views.

See also [`eachcol`](@ref) and [`eachslice`](@ref).

!!! compat "Julia 1.1"
     This function requires at least Julia 1.1.
"""
eachrow(A::AbstractVecOrMat) = (view(A, i, :) for i in axes(A, 1))


"""
    eachcol(A::AbstractVecOrMat)

Create a generator that iterates over the second dimension of matrix `A`, returning the
columns as views.

See also [`eachrow`](@ref) and [`eachslice`](@ref).

!!! compat "Julia 1.1"
     This function requires at least Julia 1.1.
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
