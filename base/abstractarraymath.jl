# This file is a part of Julia. License is MIT: https://julialang.org/license

 ## Basic functions ##

isreal(x::AbstractArray) = all(isreal,x)
iszero(x::AbstractArray) = all(iszero,x)
isreal(x::AbstractArray{<:Real}) = true
all(::typeof(isinteger), ::AbstractArray{<:Integer}) = true

## Constructors ##

"""
    vec(a::AbstractArray) -> Vector

Reshape the array `a` as a one-dimensional column vector. The resulting array
shares the same underlying data as `a`, so modifying one will also modify the
other.

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
```

See also [`reshape`](@ref).
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

# Examples
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
        length(indices(A, dims[i])) == 1 || throw(ArgumentError("squeezed dims must all be size 1"))
        for j = 1:i-1
            dims[j] == dims[i] && throw(ArgumentError("squeezed dims must be unique"))
        end
    end
    d = ()
    for i = 1:ndims(A)
        if !in(i, dims)
            d = tuple(d..., indices(A, i))
        end
    end
    reshape(A, d::typeof(_sub(indices(A), dims)))
end

squeeze(A::AbstractArray, dim::Integer) = squeeze(A, (Int(dim),))

## Transposition ##

"""
    transpose(A::AbstractMatrix)

The transposition operator (`.'`).

# Examples
```jldoctest
julia> A = [1 2 3; 4 5 6; 7 8 9]
3×3 Array{Int64,2}:
1  2  3
4  5  6
7  8  9

julia> transpose(A)
3×3 Array{Int64,2}:
1  4  7
2  5  8
3  6  9
```
"""
function transpose(A::AbstractMatrix)
    ind1, ind2 = indices(A)
    B = similar(A, (ind2, ind1))
    transpose!(B, A)
end

transpose(a::AbstractArray) = error("transpose not defined for $(typeof(a)). Consider using `permutedims` for higher-dimensional arrays.")

"""
    transpose!(dest, src)

Transpose array `src` and store the result in the preallocated array `dest`, which should
have a size corresponding to `(size(src,2),size(src,1))`. No in-place transposition is
supported and unexpected results will happen if `src` and `dest` have overlapping memory
regions.
"""
transpose!(B::AbstractMatrix, A::AbstractMatrix) = transpose_f!(identity, B, A)

function transpose!(B::AbstractVector, A::AbstractMatrix)
    indices(B,1) == indices(A,2) && indices(A,1) == 1:1 || throw(DimensionMismatch("transpose"))
    copy!(B, A)
end
function transpose!(B::AbstractMatrix, A::AbstractVector)
    indices(B,2) == indices(A,1) && indices(B,1) == 1:1 || throw(DimensionMismatch("transpose"))
    copy!(B, A)
end

const transposebaselength=64
function transpose_f!(f, B::AbstractMatrix, A::AbstractMatrix)
    inds = indices(A)
    indices(B,1) == inds[2] && indices(B,2) == inds[1] || throw(DimensionMismatch(string(f)))

    m, n = length(inds[1]), length(inds[2])
    if m*n<=4*transposebaselength
        @inbounds begin
            for j = inds[2]
                for i = inds[1]
                    B[j,i] = f(A[i,j])
                end
            end
        end
    else
        transposeblock!(f,B,A,m,n,first(inds[1])-1,first(inds[2])-1)
    end
    return B
end
function transposeblock!(f, B::AbstractMatrix, A::AbstractMatrix, m::Int, n::Int, offseti::Int, offsetj::Int)
    if m*n<=transposebaselength
        @inbounds begin
            for j = (offsetj+1):(offsetj+n)
                for i = (offseti+1):(offseti+m)
                    B[j,i] = f(A[i,j])
                end
            end
        end
    elseif m>n
        newm=m>>1
        transposeblock!(f,B,A,newm,n,offseti,offsetj)
        transposeblock!(f,B,A,m-newm,n,offseti+newm,offsetj)
    else
        newn=n>>1
        transposeblock!(f,B,A,m,newn,offseti,offsetj)
        transposeblock!(f,B,A,m,n-newn,offseti,offsetj+newn)
    end
    return B
end

## Unary operators ##

conj(x::AbstractArray{<:Real}) = x
conj!(x::AbstractArray{<:Real}) = x

real(x::AbstractArray{<:Real}) = x
imag(x::AbstractArray{<:Real}) = zero(x)

+(x::AbstractArray{<:Number}) = x
*(x::AbstractArray{<:Number,2}) = x

# index A[:,:,...,i,:,:,...] where "i" is in dimension "d"

"""
    slicedim(A, d::Integer, i)

Return all the data of `A` where the index for dimension `d` equals `i`. Equivalent to
`A[:,:,...,i,:,:,...]` where `i` is in position `d`.

# Examples
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

"""
    flipdim(A, d::Integer)

Reverse `A` in dimension `d`.

# Examples
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
    elseif nd == 1
        return reverse(A)
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
    let B=B # workaround #15276
        alli = [ indices(B,n) for n in 1:nd ]
        for i in indsd
            B[[ n==d ? sd-i : alli[n] for n in 1:nd ]...] = slicedim(A, d, i)
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

julia> a = BitArray([true, true, false, false, true])
5-element BitArray{1}:
  true
  true
 false
 false
  true

julia> circshift(a, 1)
5-element BitArray{1}:
  true
  true
  true
 false
 false

julia> circshift(a, -1)
5-element BitArray{1}:
  true
 false
 false
  true
  true
```

See also [`circshift!`](@ref).
"""
function circshift(a::AbstractArray, shiftamt)
    circshift!(similar(a), a, map(Integer, (shiftamt...,)))
end

# Uses K-B-N summation
function cumsum_kbn(v::AbstractVector{T}) where T<:AbstractFloat
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
# TODO: Needs a separate IndexCartesian method, this is only fast for IndexLinear

"""
    cumsum_kbn(A, dim::Integer)

Cumulative sum along a dimension, using the Kahan-Babuska-Neumaier compensated summation
algorithm for additional accuracy.
"""
function cumsum_kbn(A::AbstractArray{T}, axis::Integer) where T<:AbstractFloat
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

# Examples
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

@inline repmat(a::AbstractVecOrMat, m::Integer, n::Integer=1) = repmat(a, Int(m), Int(n))
@inline repmat(a::AbstractVector, m::Integer) = repmat(a, Int(m))

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
function repeat(A::AbstractArray;
                inner=ntuple(n->1, Val(ndims(A))),
                outer=ntuple(n->1, Val(ndims(A))))
    return _repeat(A, rep_kw2tup(inner), rep_kw2tup(outer))
end

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

# We need special handling when repeating arrays of arrays
cat_fill!(R, X, inds) = (R[inds...] = X)
cat_fill!(R, X::AbstractArray, inds) = fill!(view(R, inds...), X)

@noinline function _repeat(A::AbstractArray, inner, outer)
    shape, inner_shape = rep_shapes(A, inner, outer)

    R = similar(A, shape)
    if any(iszero, shape)
        return R
    end

    # fill the first inner block
    if all(x -> x == 1, inner)
        R[indices(A)...] = A
    else
        inner_indices = [1:n for n in inner]
        for c in CartesianRange(indices(A))
            for i in 1:ndims(A)
                n = inner[i]
                inner_indices[i] = (1:n) .+ ((c[i] - 1) * n)
            end
            cat_fill!(R, A[c], inner_indices)
        end
    end

    # fill the outer blocks along each dimension
    if all(x -> x == 1, outer)
        return R
    end
    src_indices  = [1:n for n in inner_shape]
    dest_indices = copy(src_indices)
    for i in 1:length(outer)
        B = view(R, src_indices...)
        for j in 2:outer[i]
            dest_indices[i] = dest_indices[i] .+ inner_shape[i]
            R[dest_indices...] = B
        end
        src_indices[i] = dest_indices[i] = 1:shape[i]
    end

    return R
end
