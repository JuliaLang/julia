# This file is a part of Julia. License is MIT: http://julialang.org/license

## Unary operators ##

function conj!{T<:Number}(A::AbstractArray{T})
    for i in eachindex(A)
        A[i] = conj(A[i])
    end
    return A
end

for f in (:-, :~, :conj, :sign)
    @eval begin
        function ($f)(A::StridedArray)
            F = similar(A)
            for i in eachindex(A)
                F[i] = ($f)(A[i])
            end
            return F
        end
    end
end

(-)(A::StridedArray{Bool}) = reshape([ -A[i] for i in eachindex(A) ], size(A))

real(A::StridedArray) = reshape([ real(x) for x in A ], size(A))
imag(A::StridedArray) = reshape([ imag(x) for x in A ], size(A))
real{T<:Real}(x::StridedArray{T}) = x
imag{T<:Real}(x::StridedArray{T}) = zero(x)

function !(A::StridedArray{Bool})
    F = similar(A)
    for i in eachindex(A)
        F[i] = !A[i]
    end
    return F
end

## Binary arithmetic operators ##

promote_array_type{Scalar, Arry}(F, ::Type{Scalar}, ::Type{Arry}) = promote_op(F, Scalar, Arry)
promote_array_type{S<:Real, A<:AbstractFloat}(F, ::Type{S}, ::Type{A}) = A
promote_array_type{S<:Integer, A<:Integer}(F, ::Type{S}, ::Type{A}) = A
promote_array_type{S<:Integer}(F, ::Type{S}, ::Type{Bool}) = S

# Handle operations that return different types
./(x::Number, Y::AbstractArray) =
    reshape([ x ./ y for y in Y ], size(Y))
./(X::AbstractArray, y::Number) =
    reshape([ x ./ y for x in X ], size(X))
.\(x::Number, Y::AbstractArray) =
    reshape([ x .\ y for y in Y ], size(Y))
.\(X::AbstractArray, y::Number) =
    reshape([ x .\ y for x in X ], size(X))
.^(x::Number, Y::AbstractArray) =
    reshape([ x ^ y for y in Y ], size(Y))
.^(X::AbstractArray, y::Number      ) =
    reshape([ x ^ y for x in X ], size(X))

for (f,F) in ((:+,   AddFun()),
              (:-,   SubFun()),
              (:div, IDivFun()),
              (:mod, ModFun()),
              (:&,   AndFun()),
              (:|,   OrFun()),
              (:$,   XorFun()))
    @eval begin
        function ($f){S,T}(A::Range{S}, B::Range{T})
            F = similar(A, promote_op($F,S,T), promote_shape(size(A),size(B)))
            i = 1
            for (a,b) in zip(A,B)
                @inbounds F[i] = ($f)(a, b)
                i += 1
            end
            return F
        end
        function ($f){S,T}(A::AbstractArray{S}, B::Range{T})
            F = similar(A, promote_op($F,S,T), promote_shape(size(A),size(B)))
            i = 1
            for b in B
                @inbounds F[i] = ($f)(A[i], b)
                i += 1
            end
            return F
        end
        function ($f){S,T}(A::Range{S}, B::AbstractArray{T})
            F = similar(B, promote_op($F,S,T), promote_shape(size(A),size(B)))
            i = 1
            for a in A
                @inbounds F[i] = ($f)(a, B[i])
                i += 1
            end
            return F
        end
        function ($f){S,T}(A::AbstractArray{S}, B::AbstractArray{T})
            F = similar(A, promote_op($F,S,T), promote_shape(size(A),size(B)))
            for i in eachindex(A,B)
                @inbounds F[i] = ($f)(A[i], B[i])
            end
            return F
        end
    end
end
for (f,F) in ((:.+,  DotAddFun()),
          (:.-,  DotSubFun()),
          (:.*,  DotMulFun()),
          (:.÷,  DotIDivFun()),
          (:.%,  DotRemFun()),
          (:.<<, DotLSFun()),
          (:.>>, DotRSFun()),
          (:div, IDivFun()),
          (:mod, ModFun()),
          (:rem, RemFun()),
          (:&,   AndFun()),
          (:|,   OrFun()),
          (:$,   XorFun()))
    @eval begin
        function ($f){T}(A::Number, B::AbstractArray{T})
            F = similar(B, promote_array_type($F,typeof(A),T))
            for i in eachindex(B)
                @inbounds F[i] = ($f)(A, B[i])
            end
            return F
        end
        function ($f){T}(A::AbstractArray{T}, B::Number)
            F = similar(A, promote_array_type($F,typeof(B),T))
            for i in eachindex(A)
                @inbounds F[i] = ($f)(A[i], B)
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

# functions that should give an Int result for Bool arrays
for f in (:.+, :.-)
    @eval begin
        function ($f)(A::Bool, B::StridedArray{Bool})
            F = similar(B, Int, size(B))
            for i in eachindex(B)
                @inbounds F[i] = ($f)(A, B[i])
            end
            return F
        end
        function ($f)(A::StridedArray{Bool}, B::Bool)
            F = similar(A, Int, size(A))
            for i in eachindex(A)
                @inbounds F[i] = ($f)(A[i], B)
            end
            return F
        end
    end
end
for f in (:+, :-)
    @eval begin
        function ($f)(A::StridedArray{Bool}, B::StridedArray{Bool})
            F = similar(A, Int, promote_shape(size(A), size(B)))
            for i in eachindex(A,B)
                @inbounds F[i] = ($f)(A[i], B[i])
            end
            return F
        end
    end
end

## data movement ##

function slicedim(A::Array, d::Integer, i::Integer)
    if d < 1
        throw(ArgumentError("dimension must be ≥ 1"))
    end
    d_in = size(A)
    leading = d_in[1:(d-1)]
    d_out = tuple(leading..., 1, d_in[(d+1):end]...)

    M = prod(leading)
    N = length(A)
    stride = M * d_in[d]

    B = similar(A, d_out)
    index_offset = 1 + (i-1)*M

    l = 1

    if M==1
        for j=0:stride:(N-stride)
            B[l] = A[j + index_offset]
            l += 1
        end
    else
        for j=0:stride:(N-stride)
            offs = j + index_offset
            for k=0:(M-1)
                B[l] = A[offs + k]
                l += 1
            end
        end
    end
    return B
end

function flipdim{T}(A::Array{T}, d::Integer)
    if d < 1
        throw(ArgumentError("dimension d must be ≥ 1"))
    end
    nd = ndims(A)
    sd = d > nd ? 1 : size(A, d)
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

function rotl90(A::StridedMatrix)
    m,n = size(A)
    B = similar(A,(n,m))
    for i=1:m, j=1:n
        B[n-j+1,i] = A[i,j]
    end
    return B
end
function rotr90(A::StridedMatrix)
    m,n = size(A)
    B = similar(A,(n,m))
    for i=1:m, j=1:n
        B[j,m-i+1] = A[i,j]
    end
    return B
end
function rot180(A::StridedMatrix)
    m,n = size(A)
    B = similar(A)
    for i=1:m, j=1:n
        B[m-i+1,n-j+1] = A[i,j]
    end
    return B
end
function rotl90(A::AbstractMatrix, k::Integer)
    k = mod(k, 4)
    k == 1 ? rotl90(A) :
    k == 2 ? rot180(A) :
    k == 3 ? rotr90(A) : copy(A)
end
rotr90(A::AbstractMatrix, k::Integer) = rotl90(A,-k)
rot180(A::AbstractMatrix, k::Integer) = mod(k, 2) == 1 ? rot180(A) : copy(A)


## Transpose ##
const transposebaselength=64
function transpose!(B::StridedMatrix,A::StridedMatrix)
    m, n = size(A)
    size(B,1) == n && size(B,2) == m || throw(DimensionMismatch("transpose"))

    if m*n<=4*transposebaselength
        @inbounds begin
            for j = 1:n
                for i = 1:m
                    B[j,i] = transpose(A[i,j])
                end
            end
        end
    else
        transposeblock!(B,A,m,n,0,0)
    end
    return B
end
function transpose!(B::StridedVector, A::StridedMatrix)
    length(B) == length(A) && size(A,1) == 1 || throw(DimensionMismatch("transpose"))
    copy!(B, A)
end
function transpose!(B::StridedMatrix, A::StridedVector)
    length(B) == length(A) && size(B,1) == 1 || throw(DimensionMismatch("transpose"))
    copy!(B, A)
end
function transposeblock!(B::StridedMatrix,A::StridedMatrix,m::Int,n::Int,offseti::Int,offsetj::Int)
    if m*n<=transposebaselength
        @inbounds begin
            for j = offsetj+(1:n)
                for i = offseti+(1:m)
                    B[j,i] = transpose(A[i,j])
                end
            end
        end
    elseif m>n
        newm=m>>1
        transposeblock!(B,A,newm,n,offseti,offsetj)
        transposeblock!(B,A,m-newm,n,offseti+newm,offsetj)
    else
        newn=n>>1
        transposeblock!(B,A,m,newn,offseti,offsetj)
        transposeblock!(B,A,m,n-newn,offseti,offsetj+newn)
    end
    return B
end
function ctranspose!(B::StridedMatrix,A::StridedMatrix)
    m, n = size(A)
    size(B,1) == n && size(B,2) == m || throw(DimensionMismatch("transpose"))

    if m*n<=4*transposebaselength
        @inbounds begin
            for j = 1:n
                for i = 1:m
                    B[j,i] = ctranspose(A[i,j])
                end
            end
        end
    else
        ctransposeblock!(B,A,m,n,0,0)
    end
    return B
end
function ctranspose!(B::StridedVector, A::StridedMatrix)
    length(B) == length(A) && size(A,1) == 1 || throw(DimensionMismatch("transpose"))
    ccopy!(B, A)
end
function ctranspose!(B::StridedMatrix, A::StridedVector)
    length(B) == length(A) && size(B,1) == 1 || throw(DimensionMismatch("transpose"))
    ccopy!(B, A)
end
function ctransposeblock!(B::StridedMatrix,A::StridedMatrix,m::Int,n::Int,offseti::Int,offsetj::Int)
    if m*n<=transposebaselength
        @inbounds begin
            for j = offsetj+(1:n)
                for i = offseti+(1:m)
                    B[j,i] = ctranspose(A[i,j])
                end
            end
        end
    elseif m>n
        newm=m>>1
        ctransposeblock!(B,A,newm,n,offseti,offsetj)
        ctransposeblock!(B,A,m-newm,n,offseti+newm,offsetj)
    else
        newn=n>>1
        ctransposeblock!(B,A,m,newn,offseti,offsetj)
        ctransposeblock!(B,A,m,n-newn,offseti,offsetj+newn)
    end
    return B
end
function ccopy!(B, A)
    for i = 1:length(A)
        B[i] = ctranspose(A[i])
    end
end

function transpose(A::StridedMatrix)
    B = similar(A, size(A, 2), size(A, 1))
    transpose!(B, A)
end
function ctranspose(A::StridedMatrix)
    B = similar(A, size(A, 2), size(A, 1))
    ctranspose!(B, A)
end
ctranspose{T<:Real}(A::StridedVecOrMat{T}) = transpose(A)

transpose(x::StridedVector) = [ transpose(x[j]) for i=1, j=1:size(x,1) ]
ctranspose{T}(x::StridedVector{T}) = T[ ctranspose(x[j]) for i=1, j=1:size(x,1) ]

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
        ($fp)(v, result, $(op==:+ ? :(zero(v[1])) : :(one(v[1]))), 1, n)
        return result
    end

    @eval function ($f)(v::AbstractVector)
        c = $(op===:+ ? (:(similar(v,_cumsum_type(v)))) : (:(similar(v))))
        return ($f!)(c, v)
    end
end

for (f, op) = ((:cummin, :min), (:cummax, :max))
    @eval function ($f)(v::AbstractVector)
        n = length(v)
        cur_val = v[1]
        res = similar(v, n)
        res[1] = cur_val
        for i in 2:n
            cur_val = ($op)(v[i], cur_val)
            res[i] = cur_val
        end
        return res
    end

    @eval function ($f)(A::StridedArray, axis::Integer)
        dimsA = size(A)
        ndimsA = ndims(A)
        axis_size = dimsA[axis]
        axis_stride = 1
        for i = 1:(axis-1)
            axis_stride *= size(A,i)
        end

        if axis_size < 1
            return A
        end

        B = similar(A)

        for i = 1:length(A)
            if div(i-1, axis_stride) % axis_size == 0
               B[i] = A[i]
            else
               B[i] = ($op)(A[i], B[i-axis_stride])
            end
        end

        return B
    end

    @eval ($f)(A::AbstractArray) = ($f)(A, 1)
end
