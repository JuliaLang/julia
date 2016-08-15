# This file is a part of Julia. License is MIT: http://julialang.org/license

## Unary operators ##

"""
    conj!(A)

Transform an array to its complex conjugate in-place.

See also [`conj`](:func:`conj`).
"""
function conj!{T<:Number}(A::AbstractArray{T})
    for i in eachindex(A)
        A[i] = conj(A[i])
    end
    return A
end

for f in (:-, :~, :conj, :sign)
    @eval begin
        function ($f)(A::AbstractArray)
            F = similar(A)
            for (iF, iA) in zip(eachindex(F), eachindex(A))
                F[iF] = ($f)(A[iA])
            end
            return F
        end
    end
end

(-)(A::AbstractArray{Bool}) = reshape([ -A[i] for i in eachindex(A) ], size(A))

real(A::AbstractArray) = reshape([ real(x) for x in A ], size(A))
imag(A::AbstractArray) = reshape([ imag(x) for x in A ], size(A))

function !(A::AbstractArray{Bool})
    F = similar(A)
    for (iF, iA) in zip(eachindex(F), eachindex(A))
        F[iF] = !A[iA]
    end
    return F
end

## Binary arithmetic operators ##

promote_array_type(F, ::Type, ::Type, T::Type) = T
promote_array_type{S<:Real, A<:AbstractFloat}(F, ::Type{S}, ::Type{A}, ::Type) = A
promote_array_type{S<:Integer, A<:Integer}(F, ::Type{S}, ::Type{A}, ::Type) = A
promote_array_type{S<:Integer, A<:Integer}(::typeof(./), ::Type{S}, ::Type{A}, T::Type) = T
promote_array_type{S<:Integer, A<:Integer}(::typeof(.\), ::Type{S}, ::Type{A}, T::Type) = T
promote_array_type{S<:Integer}(::typeof(./), ::Type{S}, ::Type{Bool}, T::Type) = T
promote_array_type{S<:Integer}(::typeof(.\), ::Type{S}, ::Type{Bool}, T::Type) = T
promote_array_type{S<:Integer}(F, ::Type{S}, ::Type{Bool}, T::Type) = T

for f in (:+, :-, :div, :mod, :&, :|, :$)
    @eval ($f){R,S}(A::AbstractArray{R}, B::AbstractArray{S}) =
        _elementwise($f, promote_op($f, R, S), A, B)
end
function _elementwise(op, ::Type{Any}, A::AbstractArray, B::AbstractArray)
    promote_shape(A, B) # check size compatibility
    return broadcast(op, A, B)
end
function _elementwise{T}(op, ::Type{T}, A::AbstractArray, B::AbstractArray)
    F = similar(A, T, promote_shape(A, B))
    for (iF, iA, iB) in zip(eachindex(F), eachindex(A), eachindex(B))
        @inbounds F[iF] = op(A[iA], B[iB])
    end
    return F
end

for f in (:.+, :.-, :.*, :./, :.\, :.^, :.÷, :.%, :.<<, :.>>, :div, :mod, :rem, :&, :|, :$)
    @eval begin
        function ($f){T}(A::Number, B::AbstractArray{T})
            R = promote_op($f, typeof(A), T)
            S = promote_array_type($f, typeof(A), T, R)
            S === Any && return [($f)(A, b) for b in B]
            F = similar(B, S)
            for (iF, iB) in zip(eachindex(F), eachindex(B))
                @inbounds F[iF] = ($f)(A, B[iB])
            end
            return F
        end
        function ($f){T}(A::AbstractArray{T}, B::Number)
            R = promote_op($f, T, typeof(B))
            S = promote_array_type($f, typeof(B), T, R)
            S === Any && return [($f)(a, B) for a in A]
            F = similar(A, S)
            for (iF, iA) in zip(eachindex(F), eachindex(A))
                @inbounds F[iF] = ($f)(A[iA], B)
            end
            return F
        end
    end
end

# familiar aliases for broadcasting operations of array ± scalar (#7226):
(+)(A::AbstractArray{Bool},x::Bool) = A .+ x
(+)(x::Bool,A::AbstractArray{Bool}) = x .+ A
(-)(A::AbstractArray{Bool},x::Bool) = A .- x
(-)(x::Bool,A::AbstractArray{Bool}) = x .- A
(+)(A::AbstractArray,x::Number) = A .+ x
(+)(x::Number,A::AbstractArray) = x .+ A
(-)(A::AbstractArray,x::Number) = A .- x
(-)(x::Number,A::AbstractArray) = x .- A

## data movement ##

function flipdim{T}(A::Array{T}, d::Integer)
    nd = ndims(A)
    1 ≤ d ≤ nd || throw(ArgumentError("dimension $d is not 1 ≤ $d ≤ $nd"))
    sd = size(A, d)
    if sd == 1 || isempty(A)
        return copy(A)
    end

    B = similar(A)

    nnd = 0
    for i = 1:nd
        nnd += Int(size(A,i)==1 || i==d)
    end
    if nnd==nd
        # flip along the only non-singleton dimension
        for i = 1:sd
            B[i] = A[sd+1-i]
        end
        return B
    end

    d_in = size(A)
    leading = d_in[1:(d-1)]
    M = prod(leading)
    N = length(A)
    stride = M * sd

    if M==1
        for j = 0:stride:(N-stride)
            for i = 1:sd
                ri = sd+1-i
                B[j + ri] = A[j + i]
            end
        end
    else
        if isbits(T) && M>200
            for i = 1:sd
                ri = sd+1-i
                for j=0:stride:(N-stride)
                    offs = j + 1 + (i-1)*M
                    boffs = j + 1 + (ri-1)*M
                    copy!(B, boffs, A, offs, M)
                end
            end
        else
            for i = 1:sd
                ri = sd+1-i
                for j=0:stride:(N-stride)
                    offs = j + 1 + (i-1)*M
                    boffs = j + 1 + (ri-1)*M
                    for k=0:(M-1)
                        B[boffs + k] = A[offs + k]
                    end
                end
            end
        end
    end
    return B
end

"""
    rotl90(A)

Rotate matrix `A` left 90 degrees.

```jldoctest
julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> rotl90(a)
2×2 Array{Int64,2}:
 2  4
 1  3
```
"""
function rotl90(A::AbstractMatrix)
    ind1, ind2 = indices(A)
    B = similar(A, (ind2,ind1))
    n = first(ind2)+last(ind2)
    for i=indices(A,1), j=ind2
        B[n-j,i] = A[i,j]
    end
    return B
end

"""
    rotr90(A)

Rotate matrix `A` right 90 degrees.

```jldoctest
julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> rotr90(a)
2×2 Array{Int64,2}:
 3  1
 4  2
```
"""
function rotr90(A::AbstractMatrix)
    ind1, ind2 = indices(A)
    B = similar(A, (ind2,ind1))
    m = first(ind1)+last(ind1)
    for i=ind1, j=indices(A,2)
        B[j,m-i] = A[i,j]
    end
    return B
end
"""
    rot180(A)

Rotate matrix `A` 180 degrees.

```jldoctest
julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> rot180(a)
2×2 Array{Int64,2}:
 4  3
 2  1
```
"""
function rot180(A::AbstractMatrix)
    B = similar(A)
    ind1, ind2 = indices(A,1), indices(A,2)
    m, n = first(ind1)+last(ind1), first(ind2)+last(ind2)
    for j=ind2, i=ind1
        B[m-i,n-j] = A[i,j]
    end
    return B
end
"""
    rotl90(A, k)

Rotate matrix `A` left 90 degrees an integer `k` number of times.
If `k` is zero or a multiple of four, this is equivalent to a `copy`.

```jldoctest
julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> rotl90(a,1)
2×2 Array{Int64,2}:
 2  4
 1  3

julia> rotl90(a,2)
2×2 Array{Int64,2}:
 4  3
 2  1

julia> rotl90(a,3)
2×2 Array{Int64,2}:
 3  1
 4  2

julia> rotl90(a,4)
2×2 Array{Int64,2}:
 1  2
 3  4
```
"""
function rotl90(A::AbstractMatrix, k::Integer)
    k = mod(k, 4)
    k == 1 ? rotl90(A) :
    k == 2 ? rot180(A) :
    k == 3 ? rotr90(A) : copy(A)
end
"""
    rotr90(A, k)

Rotate matrix `A` right 90 degrees an integer `k` number of times. If `k` is zero or a
multiple of four, this is equivalent to a `copy`.

```jldoctest
julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> rotr90(a,1)
2×2 Array{Int64,2}:
 3  1
 4  2

julia> rotr90(a,2)
2×2 Array{Int64,2}:
 4  3
 2  1

julia> rotr90(a,3)
2×2 Array{Int64,2}:
 2  4
 1  3

julia> rotr90(a,4)
2×2 Array{Int64,2}:
 1  2
 3  4
```
"""
rotr90(A::AbstractMatrix, k::Integer) = rotl90(A,-k)
"""
    rot180(A, k)

Rotate matrix `A` 180 degrees an integer `k` number of times.
If `k` is even, this is equivalent to a `copy`.

```jldoctest
julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> rot180(a,1)
2×2 Array{Int64,2}:
 4  3
 2  1

julia> rot180(a,2)
2×2 Array{Int64,2}:
 1  2
 3  4
```
"""
rot180(A::AbstractMatrix, k::Integer) = mod(k, 2) == 1 ? rot180(A) : copy(A)

## Transpose ##

"""
    transpose!(dest,src)

Transpose array `src` and store the result in the preallocated array `dest`, which should
have a size corresponding to `(size(src,2),size(src,1))`. No in-place transposition is
supported and unexpected results will happen if `src` and `dest` have overlapping memory
regions.
"""
transpose!(B::AbstractMatrix, A::AbstractMatrix) = transpose_f!(transpose, B, A)

"""
    ctranspose!(dest,src)

Conjugate transpose array `src` and store the result in the preallocated array `dest`, which
should have a size corresponding to `(size(src,2),size(src,1))`. No in-place transposition
is supported and unexpected results will happen if `src` and `dest` have overlapping memory
regions.
"""
ctranspose!(B::AbstractMatrix, A::AbstractMatrix) = transpose_f!(ctranspose, B, A)
function transpose!(B::AbstractVector, A::AbstractMatrix)
    indices(B,1) == indices(A,2) && indices(A,1) == 1:1 || throw(DimensionMismatch("transpose"))
    copy!(B, A)
end
function transpose!(B::AbstractMatrix, A::AbstractVector)
    indices(B,2) == indices(A,1) && indices(B,1) == 1:1 || throw(DimensionMismatch("transpose"))
    copy!(B, A)
end
function ctranspose!(B::AbstractVector, A::AbstractMatrix)
    indices(B,1) == indices(A,2) && indices(A,1) == 1:1 || throw(DimensionMismatch("transpose"))
    ccopy!(B, A)
end
function ctranspose!(B::AbstractMatrix, A::AbstractVector)
    indices(B,2) == indices(A,1) && indices(B,1) == 1:1 || throw(DimensionMismatch("transpose"))
    ccopy!(B, A)
end

const transposebaselength=64
function transpose_f!(f,B::AbstractMatrix,A::AbstractMatrix)
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
function transposeblock!(f,B::AbstractMatrix,A::AbstractMatrix,m::Int,n::Int,offseti::Int,offsetj::Int)
    if m*n<=transposebaselength
        @inbounds begin
            for j = offsetj+(1:n)
                for i = offseti+(1:m)
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
function ccopy!(B, A)
    for (i,j) = zip(eachindex(B),eachindex(A))
        B[i] = ctranspose(A[j])
    end
end

"""
    transpose(A)

The transposition operator (`.'`).
"""
function transpose(A::AbstractMatrix)
    ind1, ind2 = indices(A)
    B = similar(A, (ind2, ind1))
    transpose!(B, A)
end
function ctranspose(A::AbstractMatrix)
    ind1, ind2 = indices(A)
    B = similar(A, (ind2, ind1))
    ctranspose!(B, A)
end
ctranspose{T<:Real}(A::AbstractVecOrMat{T}) = transpose(A)

transpose(x::AbstractVector) = [ transpose(v) for i=of_indices(x, OneTo(1)), v in x ]
ctranspose{T}(x::AbstractVector{T}) = T[ ctranspose(v) for i=of_indices(x, OneTo(1)), v in x ]

_cumsum_type{T<:Number}(v::AbstractArray{T}) = typeof(+zero(T))
_cumsum_type(v) = typeof(v[1]+v[1])

for (f, f!, fp, op) = ((:cumsum, :cumsum!, :cumsum_pairwise!, :+),
                       (:cumprod, :cumprod!, :cumprod_pairwise!, :*) )
    # in-place cumsum of c = s+v[range(i1,n)], using pairwise summation
    @eval function ($fp){T}(v::AbstractVector, c::AbstractVector{T}, s, i1, n)
        local s_::T # for sum(v[range(i1,n)]), i.e. sum without s
        if n < 128
            @inbounds s_ = v[i1]
            @inbounds c[i1] = ($op)(s, s_)
            for i = i1+1:i1+n-1
                @inbounds s_ = $(op)(s_, v[i])
                @inbounds c[i] = $(op)(s, s_)
            end
        else
            n2 = n >> 1
            s_ = ($fp)(v, c, s, i1, n2)
            s_ = $(op)(s_, ($fp)(v, c, ($op)(s, s_), i1+n2, n-n2))
        end
        return s_
    end

    @eval function ($f!)(result::AbstractVector, v::AbstractVector)
        n = length(v)
        if n == 0; return result; end
        ($fp)(v, result, $(op==:+ ? :(zero(first(v))) : :(one(first(v)))), first(indices(v,1)), n)
        return result
    end

    @eval function ($f)(v::AbstractVector)
        c = $(op===:+ ? (:(similar(v,_cumsum_type(v)))) : (:(similar(v))))
        return ($f!)(c, v)
    end
end
