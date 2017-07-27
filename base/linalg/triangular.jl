# This file is a part of Julia. License is MIT: https://julialang.org/license

## Triangular

# could be renamed to Triangular when that name has been fully deprecated
abstract type AbstractTriangular{T,S<:AbstractMatrix} <: AbstractMatrix{T} end

# First loop through all methods that don't need special care for upper/lower and unit diagonal
for t in (:LowerTriangular, :UnitLowerTriangular, :UpperTriangular,
          :UnitUpperTriangular)
    @eval begin
        struct $t{T,S<:AbstractMatrix} <: AbstractTriangular{T,S}
            data::S
        end
        $t(A::$t) = A
        function $t(A::AbstractMatrix)
            Base.LinAlg.checksquare(A)
            return $t{eltype(A), typeof(A)}(A)
        end

        size(A::$t, d) = size(A.data, d)
        size(A::$t) = size(A.data)

        convert(::Type{$t{T}}, A::$t{T}) where {T} = A
        function convert(::Type{$t{T}}, A::$t) where T
            Anew = convert(AbstractMatrix{T}, A.data)
            $t(Anew)
        end
        convert(::Type{AbstractMatrix{T}}, A::$t{T}) where {T} = A
        convert(::Type{AbstractMatrix{T}}, A::$t) where {T} = convert($t{T}, A)
        convert(::Type{Matrix}, A::$t{T}) where {T} = convert(Matrix{T}, A)

        function similar(A::$t, ::Type{T}) where T
            B = similar(A.data, T)
            return $t(B)
        end

        copy(A::$t) = $t(copy(A.data))

        broadcast(::typeof(big), A::$t) = $t(big.(A.data))

        real(A::$t{<:Real}) = A
        real(A::$t{<:Complex}) = (B = real(A.data); $t(B))
        broadcast(::typeof(abs), A::$t) = $t(abs.(A.data))
    end
end

LowerTriangular(U::UpperTriangular) = throw(ArgumentError(
    "cannot create a LowerTriangular matrix from an UpperTriangular input"))
UpperTriangular(U::LowerTriangular) = throw(ArgumentError(
    "cannot create an UpperTriangular matrix from a LowerTriangular input"))

"""
    LowerTriangular(A::AbstractMatrix)

Construct a `LowerTriangular` view of the the matrix `A`.

# Examples
```jldoctest
julia> A = [1.0 2.0 3.0; 4.0 5.0 6.0; 7.0 8.0 9.0]
3×3 Array{Float64,2}:
 1.0  2.0  3.0
 4.0  5.0  6.0
 7.0  8.0  9.0

julia> LowerTriangular(A)
3×3 LowerTriangular{Float64,Array{Float64,2}}:
 1.0   ⋅    ⋅
 4.0  5.0   ⋅
 7.0  8.0  9.0
```
"""
LowerTriangular
"""
    UpperTriangular(A::AbstractMatrix)

Construct an `UpperTriangular` view of the the matrix `A`.

# Examples
```jldoctest
julia> A = [1.0 2.0 3.0; 4.0 5.0 6.0; 7.0 8.0 9.0]
3×3 Array{Float64,2}:
 1.0  2.0  3.0
 4.0  5.0  6.0
 7.0  8.0  9.0

julia> UpperTriangular(A)
3×3 UpperTriangular{Float64,Array{Float64,2}}:
 1.0  2.0  3.0
  ⋅   5.0  6.0
  ⋅    ⋅   9.0
```
"""
UpperTriangular

imag(A::UpperTriangular) = UpperTriangular(imag(A.data))
imag(A::LowerTriangular) = LowerTriangular(imag(A.data))
imag(A::UnitLowerTriangular) = LowerTriangular(tril!(imag(A.data),-1))
imag(A::UnitUpperTriangular) = UpperTriangular(triu!(imag(A.data),1))

convert(::Type{Array}, A::AbstractTriangular) = convert(Matrix, A)
full(A::AbstractTriangular) = convert(Array, A)
parent(A::AbstractTriangular) = A.data

# then handle all methods that requires specific handling of upper/lower and unit diagonal

function convert(::Type{Matrix{T}}, A::LowerTriangular) where T
    B = Matrix{T}(size(A, 1), size(A, 1))
    copy!(B, A.data)
    tril!(B)
    B
end
function convert(::Type{Matrix{T}}, A::UnitLowerTriangular) where T
    B = Matrix{T}(size(A, 1), size(A, 1))
    copy!(B, A.data)
    tril!(B)
    for i = 1:size(B,1)
        B[i,i] = 1
    end
    B
end
function convert(::Type{Matrix{T}}, A::UpperTriangular) where T
    B = Matrix{T}(size(A, 1), size(A, 1))
    copy!(B, A.data)
    triu!(B)
    B
end
function convert(::Type{Matrix{T}}, A::UnitUpperTriangular) where T
    B = Matrix{T}(size(A, 1), size(A, 1))
    copy!(B, A.data)
    triu!(B)
    for i = 1:size(B,1)
        B[i,i] = 1
    end
    B
end

function full!(A::LowerTriangular)
    B = A.data
    tril!(B)
    B
end
function full!(A::UnitLowerTriangular)
    B = A.data
    tril!(B)
    for i = 1:size(A,1)
        B[i,i] = 1
    end
    B
end
function full!(A::UpperTriangular)
    B = A.data
    triu!(B)
    B
end
function full!(A::UnitUpperTriangular)
    B = A.data
    triu!(B)
    for i = 1:size(A,1)
        B[i,i] = 1
    end
    B
end

getindex(A::UnitLowerTriangular{T}, i::Integer, j::Integer) where {T} =
    i > j ? A.data[i,j] : ifelse(i == j, oneunit(T), zero(T))
getindex(A::LowerTriangular, i::Integer, j::Integer) =
    i >= j ? A.data[i,j] : zero(A.data[j,i])
getindex(A::UnitUpperTriangular{T}, i::Integer, j::Integer) where {T} =
    i < j ? A.data[i,j] : ifelse(i == j, oneunit(T), zero(T))
getindex(A::UpperTriangular, i::Integer, j::Integer) =
    i <= j ? A.data[i,j] : zero(A.data[j,i])

function setindex!(A::UpperTriangular, x, i::Integer, j::Integer)
    if i > j
        x == 0 || throw(ArgumentError("cannot set index in the lower triangular part " *
            "($i, $j) of an UpperTriangular matrix to a nonzero value ($x)"))
    else
        A.data[i,j] = x
    end
    return A
end

function setindex!(A::UnitUpperTriangular, x, i::Integer, j::Integer)
    if i > j
        x == 0 || throw(ArgumentError("cannot set index in the lower triangular part " *
            "($i, $j) of a UnitUpperTriangular matrix to a nonzero value ($x)"))
    elseif i == j
        x == 1 || throw(ArgumentError("cannot set index on the diagonal ($i, $j) " *
            "of a UnitUpperTriangular matrix to a non-unit value ($x)"))
    else
        A.data[i,j] = x
    end
    return A
end

function setindex!(A::LowerTriangular, x, i::Integer, j::Integer)
    if i < j
        x == 0 || throw(ArgumentError("cannot set index in the upper triangular part " *
            "($i, $j) of a LowerTriangular matrix to a nonzero value ($x)"))
    else
        A.data[i,j] = x
    end
    return A
end

function setindex!(A::UnitLowerTriangular, x, i::Integer, j::Integer)
    if i < j
        x == 0 || throw(ArgumentError("cannot set index in the upper triangular part " *
            "($i, $j) of a UnitLowerTriangular matrix to a nonzero value ($x)"))
    elseif i == j
        x == 1 || throw(ArgumentError("cannot set index on the diagonal ($i, $j) " *
            "of a UnitLowerTriangular matrix to a non-unit value ($x)"))
    else
        A.data[i,j] = x
    end
    return A
end


## structured matrix methods ##
function Base.replace_in_print_matrix(A::UpperTriangular,i::Integer,j::Integer,s::AbstractString)
    i<=j ? s : Base.replace_with_centered_mark(s)
end
function Base.replace_in_print_matrix(A::LowerTriangular,i::Integer,j::Integer,s::AbstractString)
    i>=j ? s : Base.replace_with_centered_mark(s)
end


istril(A::LowerTriangular) = true
istril(A::UnitLowerTriangular) = true
istriu(A::UpperTriangular) = true
istriu(A::UnitUpperTriangular) = true

function tril!(A::UpperTriangular, k::Integer=0)
    n = size(A,1)
    if abs(k) > n
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($n,$n)"))
    elseif k < 0
        fill!(A.data,0)
        return A
    elseif k == 0
        for j in 1:n, i in 1:j-1
            A.data[i,j] = 0
        end
        return A
    else
        return UpperTriangular(tril!(A.data,k))
    end
end
triu!(A::UpperTriangular, k::Integer=0) = UpperTriangular(triu!(A.data,k))

function tril!(A::UnitUpperTriangular{T}, k::Integer=0) where T
    n = size(A,1)
    if abs(k) > n
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($n,$n)"))
    elseif k < 0
        fill!(A.data, zero(T))
        return UpperTriangular(A.data)
    elseif k == 0
        fill!(A.data, zero(T))
        for i in diagind(A)
            A.data[i] = oneunit(T)
        end
        return UpperTriangular(A.data)
    else
        for i in diagind(A)
            A.data[i] = oneunit(T)
        end
        return UpperTriangular(tril!(A.data,k))
    end
end

function triu!(A::UnitUpperTriangular, k::Integer=0)
    for i in diagind(A)
        A.data[i] = oneunit(eltype(A))
    end
    return triu!(UpperTriangular(A.data),k)
end

function triu!(A::LowerTriangular, k::Integer=0)
    n = size(A,1)
    if abs(k) > n
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($n,$n)"))
    elseif k > 0
        fill!(A.data,0)
        return A
    elseif k == 0
        for j in 1:n, i in j+1:n
            A.data[i,j] = 0
        end
        return A
    else
        return LowerTriangular(triu!(A.data,k))
    end
end

tril!(A::LowerTriangular, k::Integer=0) = LowerTriangular(tril!(A.data,k))

function triu!(A::UnitLowerTriangular{T}, k::Integer=0) where T
    n = size(A,1)
    if abs(k) > n
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($n,$n)"))
    elseif k > 0
        fill!(A.data, zero(T))
        return LowerTriangular(A.data)
    elseif k == 0
        fill!(A.data, zero(T))
        for i in diagind(A)
            A.data[i] = oneunit(T)
        end
        return LowerTriangular(A.data)
    else
        for i in diagind(A)
            A.data[i] = oneunit(T)
        end
        return LowerTriangular(triu!(A.data,k))
    end
end

function tril!(A::UnitLowerTriangular, k::Integer=0)
    for i in diagind(A)
        A.data[i] = oneunit(eltype(A))
    end
    return tril!(LowerTriangular(A.data),k)
end

transpose(A::LowerTriangular) = UpperTriangular(transpose(A.data))
transpose(A::UnitLowerTriangular) = UnitUpperTriangular(transpose(A.data))
transpose(A::UpperTriangular) = LowerTriangular(transpose(A.data))
transpose(A::UnitUpperTriangular) = UnitLowerTriangular(transpose(A.data))
ctranspose(A::LowerTriangular) = UpperTriangular(ctranspose(A.data))
ctranspose(A::UnitLowerTriangular) = UnitUpperTriangular(ctranspose(A.data))
ctranspose(A::UpperTriangular) = LowerTriangular(ctranspose(A.data))
ctranspose(A::UnitUpperTriangular) = UnitLowerTriangular(ctranspose(A.data))

transpose!(A::LowerTriangular) = UpperTriangular(copytri!(A.data, 'L'))
transpose!(A::UnitLowerTriangular) = UnitUpperTriangular(copytri!(A.data, 'L'))
transpose!(A::UpperTriangular) = LowerTriangular(copytri!(A.data, 'U'))
transpose!(A::UnitUpperTriangular) = UnitLowerTriangular(copytri!(A.data, 'U'))
ctranspose!(A::LowerTriangular) = UpperTriangular(copytri!(A.data, 'L' , true))
ctranspose!(A::UnitLowerTriangular) = UnitUpperTriangular(copytri!(A.data, 'L' , true))
ctranspose!(A::UpperTriangular) = LowerTriangular(copytri!(A.data, 'U' , true))
ctranspose!(A::UnitUpperTriangular) = UnitLowerTriangular(copytri!(A.data, 'U' , true))

diag(A::LowerTriangular) = diag(A.data)
diag(A::UnitLowerTriangular) = ones(eltype(A), size(A,1))
diag(A::UpperTriangular) = diag(A.data)
diag(A::UnitUpperTriangular) = ones(eltype(A), size(A,1))

# Unary operations
-(A::LowerTriangular) = LowerTriangular(-A.data)
-(A::UpperTriangular) = UpperTriangular(-A.data)
function -(A::UnitLowerTriangular)
    Anew = -A.data
    for i = 1:size(A, 1)
        Anew[i, i] = -1
    end
    LowerTriangular(Anew)
end
function -(A::UnitUpperTriangular)
    Anew = -A.data
    for i = 1:size(A, 1)
        Anew[i, i] = -1
    end
    UpperTriangular(Anew)
end

# copy and scale
function copy!(A::T, B::T) where T<:Union{UpperTriangular,UnitUpperTriangular}
    n = size(B,1)
    for j = 1:n
        for i = 1:(isa(B, UnitUpperTriangular) ? j-1 : j)
            @inbounds A[i,j] = B[i,j]
        end
    end
    return A
end
function copy!(A::T, B::T) where T<:Union{LowerTriangular,UnitLowerTriangular}
    n = size(B,1)
    for j = 1:n
        for i = (isa(B, UnitLowerTriangular) ? j+1 : j):n
            @inbounds A[i,j] = B[i,j]
        end
    end
    return A
end

function scale!(A::UpperTriangular, B::Union{UpperTriangular,UnitUpperTriangular}, c::Number)
    n = checksquare(B)
    for j = 1:n
        if isa(B, UnitUpperTriangular)
            @inbounds A[j,j] = c
        end
        for i = 1:(isa(B, UnitUpperTriangular) ? j-1 : j)
            @inbounds A[i,j] = c * B[i,j]
        end
    end
    return A
end
function scale!(A::LowerTriangular, B::Union{LowerTriangular,UnitLowerTriangular}, c::Number)
    n = checksquare(B)
    for j = 1:n
        if isa(B, UnitLowerTriangular)
            @inbounds A[j,j] = c
        end
        for i = (isa(B, UnitLowerTriangular) ? j+1 : j):n
            @inbounds A[i,j] = c * B[i,j]
        end
    end
    return A
end
scale!(A::Union{UpperTriangular,LowerTriangular}, c::Number) = scale!(A,A,c)
scale!(c::Number, A::Union{UpperTriangular,LowerTriangular}) = scale!(A,c)

# Binary operations
+(A::UpperTriangular, B::UpperTriangular) = UpperTriangular(A.data + B.data)
+(A::LowerTriangular, B::LowerTriangular) = LowerTriangular(A.data + B.data)
+(A::UpperTriangular, B::UnitUpperTriangular) = UpperTriangular(A.data + triu(B.data, 1) + I)
+(A::LowerTriangular, B::UnitLowerTriangular) = LowerTriangular(A.data + tril(B.data, -1) + I)
+(A::UnitUpperTriangular, B::UpperTriangular) = UpperTriangular(triu(A.data, 1) + B.data + I)
+(A::UnitLowerTriangular, B::LowerTriangular) = LowerTriangular(tril(A.data, -1) + B.data + I)
+(A::UnitUpperTriangular, B::UnitUpperTriangular) = UpperTriangular(triu(A.data, 1) + triu(B.data, 1) + 2I)
+(A::UnitLowerTriangular, B::UnitLowerTriangular) = LowerTriangular(tril(A.data, -1) + tril(B.data, -1) + 2I)
+(A::AbstractTriangular, B::AbstractTriangular) = full(A) + full(B)

-(A::UpperTriangular, B::UpperTriangular) = UpperTriangular(A.data - B.data)
-(A::LowerTriangular, B::LowerTriangular) = LowerTriangular(A.data - B.data)
-(A::UpperTriangular, B::UnitUpperTriangular) = UpperTriangular(A.data - triu(B.data, 1) - I)
-(A::LowerTriangular, B::UnitLowerTriangular) = LowerTriangular(A.data - tril(B.data, -1) - I)
-(A::UnitUpperTriangular, B::UpperTriangular) = UpperTriangular(triu(A.data, 1) - B.data + I)
-(A::UnitLowerTriangular, B::LowerTriangular) = LowerTriangular(tril(A.data, -1) - B.data + I)
-(A::UnitUpperTriangular, B::UnitUpperTriangular) = UpperTriangular(triu(A.data, 1) - triu(B.data, 1))
-(A::UnitLowerTriangular, B::UnitLowerTriangular) = LowerTriangular(tril(A.data, -1) - tril(B.data, -1))
-(A::AbstractTriangular, B::AbstractTriangular) = full(A) - full(B)

######################
# BlasFloat routines #
######################

A_mul_B!(A::Tridiagonal, B::AbstractTriangular) = A*full!(B)
A_mul_B!(C::AbstractMatrix, A::AbstractTriangular, B::Tridiagonal) = A_mul_B!(C, full(A), B)
A_mul_B!(C::AbstractMatrix, A::Tridiagonal, B::AbstractTriangular) = A_mul_B!(C, A, full(B))
A_mul_B!(C::AbstractVector, A::AbstractTriangular, B::AbstractVector) = A_mul_B!(A, copy!(C, B))
A_mul_B!(C::AbstractMatrix, A::AbstractTriangular, B::AbstractVecOrMat) = A_mul_B!(A, copy!(C, B))
A_mul_B!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat) = A_mul_B!(A, copy!(C, B))
A_mul_Bt!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat) = A_mul_B!(A, transpose!(C, B))
A_mul_Bc!(C::AbstractMatrix, A::AbstractTriangular, B::AbstractVecOrMat) = A_mul_B!(A, ctranspose!(C, B))
A_mul_Bc!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat) = A_mul_B!(A, ctranspose!(C, B))

for (t, uploc, isunitc) in ((:LowerTriangular, 'L', 'N'),
                            (:UnitLowerTriangular, 'L', 'U'),
                            (:UpperTriangular, 'U', 'N'),
                            (:UnitUpperTriangular, 'U', 'U'))
    @eval begin
        # Vector multiplication
        A_mul_B!(A::$t{T,<:StridedMatrix}, b::StridedVector{T}) where {T<:BlasFloat} =
            BLAS.trmv!($uploc, 'N', $isunitc, A.data, b)
        At_mul_B!(A::$t{T,<:StridedMatrix}, b::StridedVector{T}) where {T<:BlasFloat} =
            BLAS.trmv!($uploc, 'T', $isunitc, A.data, b)
        Ac_mul_B!(A::$t{T,<:StridedMatrix}, b::StridedVector{T}) where {T<:BlasReal} =
            BLAS.trmv!($uploc, 'T', $isunitc, A.data, b)
        Ac_mul_B!(A::$t{T,<:StridedMatrix}, b::StridedVector{T}) where {T<:BlasComplex} =
            BLAS.trmv!($uploc, 'C', $isunitc, A.data, b)

        # Matrix multiplication
        A_mul_B!(A::$t{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasFloat} =
            BLAS.trmm!('L', $uploc, 'N', $isunitc, one(T), A.data, B)
        A_mul_B!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat} =
            BLAS.trmm!('R', $uploc, 'N', $isunitc, one(T), B.data, A)

        At_mul_B!(A::$t{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasFloat} =
            BLAS.trmm!('L', $uploc, 'T', $isunitc, one(T), A.data, B)
        Ac_mul_B!(A::$t{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasComplex} =
            BLAS.trmm!('L', $uploc, 'C', $isunitc, one(T), A.data, B)
        Ac_mul_B!(A::$t{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasReal} =
            BLAS.trmm!('L', $uploc, 'T', $isunitc, one(T), A.data, B)

        A_mul_Bt!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat} =
            BLAS.trmm!('R', $uploc, 'T', $isunitc, one(T), B.data, A)
        A_mul_Bc!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasComplex} =
            BLAS.trmm!('R', $uploc, 'C', $isunitc, one(T), B.data, A)
        A_mul_Bc!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasReal} =
            BLAS.trmm!('R', $uploc, 'T', $isunitc, one(T), B.data, A)

        # Left division
        A_ldiv_B!(A::$t{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat} =
            LAPACK.trtrs!($uploc, 'N', $isunitc, A.data, B)
        At_ldiv_B!(A::$t{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat} =
            LAPACK.trtrs!($uploc, 'T', $isunitc, A.data, B)
        Ac_ldiv_B!(A::$t{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasReal} =
            LAPACK.trtrs!($uploc, 'T', $isunitc, A.data, B)
        Ac_ldiv_B!(A::$t{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasComplex} =
            LAPACK.trtrs!($uploc, 'C', $isunitc, A.data, B)

        # Right division
        A_rdiv_B!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat} =
            BLAS.trsm!('R', $uploc, 'N', $isunitc, one(T), B.data, A)
        A_rdiv_Bt!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat} =
            BLAS.trsm!('R', $uploc, 'T', $isunitc, one(T), B.data, A)
        A_rdiv_Bc!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasReal} =
            BLAS.trsm!('R', $uploc, 'T', $isunitc, one(T), B.data, A)
        A_rdiv_Bc!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasComplex} =
            BLAS.trsm!('R', $uploc, 'C', $isunitc, one(T), B.data, A)

        # Matrix inverse
        inv!(A::$t{T,S}) where {T<:BlasFloat,S<:StridedMatrix} =
            $t{T,S}(LAPACK.trtri!($uploc, $isunitc, A.data))

        # Error bounds for triangular solve
        errorbounds(A::$t{T,<:StridedMatrix}, X::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat} =
            LAPACK.trrfs!($uploc, 'N', $isunitc, A.data, B, X)

        # Condition numbers
        function cond(A::$t{<:BlasFloat}, p::Real=2)
            checksquare(A)
            if p == 1
                return inv(LAPACK.trcon!('O', $uploc, $isunitc, A.data))
            elseif p == Inf
                return inv(LAPACK.trcon!('I', $uploc, $isunitc, A.data))
            else # use fallback
                return cond(full(A), p)
            end
        end
    end
end

function inv(A::LowerTriangular{T}) where T
    S = typeof((zero(T)*one(T) + zero(T))/one(T))
    LowerTriangular(A_ldiv_B!(convert(AbstractArray{S}, A), eye(S, size(A, 1))))
end
function inv(A::UpperTriangular{T}) where T
    S = typeof((zero(T)*one(T) + zero(T))/one(T))
    UpperTriangular(A_ldiv_B!(convert(AbstractArray{S}, A), eye(S, size(A, 1))))
end
inv(A::UnitUpperTriangular{T}) where {T} = UnitUpperTriangular(A_ldiv_B!(A, eye(T, size(A, 1))))
inv(A::UnitLowerTriangular{T}) where {T} = UnitLowerTriangular(A_ldiv_B!(A, eye(T, size(A, 1))))

errorbounds(A::AbstractTriangular{T,<:StridedMatrix}, X::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:Union{BigFloat,Complex{BigFloat}}} =
    error("not implemented yet! Please submit a pull request.")
function errorbounds(A::AbstractTriangular{TA,<:StridedMatrix}, X::StridedVecOrMat{TX}, B::StridedVecOrMat{TB}) where {TA<:Number,TX<:Number,TB<:Number}
    TAXB = promote_type(TA, TB, TX, Float32)
    errorbounds(convert(AbstractMatrix{TAXB}, A), convert(AbstractArray{TAXB}, X), convert(AbstractArray{TAXB}, B))
end

# Eigensystems
## Notice that trecv works for quasi-triangular matrices and therefore the lower sub diagonal must be zeroed before calling the subroutine
function eigvecs(A::UpperTriangular{<:BlasFloat,<:StridedMatrix})
    LAPACK.trevc!('R', 'A', BlasInt[], triu!(A.data))
end
function eigvecs(A::UnitUpperTriangular{<:BlasFloat,<:StridedMatrix})
    for i = 1:size(A, 1)
        A.data[i,i] = 1
    end
    LAPACK.trevc!('R', 'A', BlasInt[], triu!(A.data))
end
function eigvecs(A::LowerTriangular{<:BlasFloat,<:StridedMatrix})
    LAPACK.trevc!('L', 'A', BlasInt[], tril!(A.data)')
end
function eigvecs(A::UnitLowerTriangular{<:BlasFloat,<:StridedMatrix})
    for i = 1:size(A, 1)
        A.data[i,i] = 1
    end
    LAPACK.trevc!('L', 'A', BlasInt[], tril!(A.data)')
end

####################
# Generic routines #
####################

for (t, unitt) in ((UpperTriangular, UnitUpperTriangular),
                   (LowerTriangular, UnitLowerTriangular))
    @eval begin
        (*)(A::$t, x::Number) = $t(A.data*x)

        function (*)(A::$unitt, x::Number)
            B = A.data*x
            for i = 1:size(A, 1)
                B[i,i] = x
            end
            $t(B)
        end

        (*)(x::Number, A::$t) = $t(x*A.data)

        function (*)(x::Number, A::$unitt)
            B = x*A.data
            for i = 1:size(A, 1)
                B[i,i] = x
            end
            $t(B)
        end

        (/)(A::$t, x::Number) = $t(A.data/x)

        function (/)(A::$unitt, x::Number)
            B = A.data/x
            invx = inv(x)
            for i = 1:size(A, 1)
                B[i,i] = invx
            end
            $t(B)
        end

        (\)(x::Number, A::$t) = $t(x\A.data)

        function (\)(x::Number, A::$unitt)
            B = x\A.data
            invx = inv(x)
            for i = 1:size(A, 1)
                B[i,i] = invx
            end
            $t(B)
        end
    end
end

## Generic triangular multiplication
function A_mul_B!(A::UpperTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = 1:m
            Bij = A.data[i,i]*B[i,j]
            for k = i + 1:m
                Bij += A.data[i,k]*B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end
function A_mul_B!(A::UnitUpperTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = 1:m
            Bij = B[i,j]
            for k = i + 1:m
                Bij += A.data[i,k]*B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function A_mul_B!(A::LowerTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = m:-1:1
            Bij = A.data[i,i]*B[i,j]
            for k = 1:i - 1
                Bij += A.data[i,k]*B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end
function A_mul_B!(A::UnitLowerTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = m:-1:1
            Bij = B[i,j]
            for k = 1:i - 1
                Bij += A.data[i,k]*B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function Ac_mul_B!(A::UpperTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = m:-1:1
            Bij = A.data[i,i]'B[i,j]
            for k = 1:i - 1
                Bij += A.data[k,i]'B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end
function Ac_mul_B!(A::UnitUpperTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = m:-1:1
            Bij = B[i,j]
            for k = 1:i - 1
                Bij += A.data[k,i]'B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function Ac_mul_B!(A::LowerTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = 1:m
            Bij = A.data[i,i]'B[i,j]
            for k = i + 1:m
                Bij += A.data[k,i]'B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end
function Ac_mul_B!(A::UnitLowerTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = 1:m
            Bij = B[i,j]
            for k = i + 1:m
                Bij += A.data[k,i]'B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function At_mul_B!(A::UpperTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = m:-1:1
            Bij = A.data[i,i].'B[i,j]
            for k = 1:i - 1
                Bij += A.data[k,i].'B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end
function At_mul_B!(A::UnitUpperTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = m:-1:1
            Bij = B[i,j]
            for k = 1:i - 1
                Bij += A.data[k,i].'B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function At_mul_B!(A::LowerTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = 1:m
            Bij = A.data[i,i].'B[i,j]
            for k = i + 1:m
                Bij += A.data[k,i].'B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end
function At_mul_B!(A::UnitLowerTriangular, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != size(A, 1)
        throw(DimensionMismatch("right hand side B needs first dimension of size $(size(A,1)), has size $m"))
    end
    for j = 1:n
        for i = 1:m
            Bij = B[i,j]
            for k = i + 1:m
                Bij += A.data[k,i].'B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function A_mul_B!(A::StridedMatrix, B::UpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]*B[j,j]
            for k = 1:j - 1
                Aij += A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij
        end
    end
    A
end
function A_mul_B!(A::StridedMatrix, B::UnitUpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = 1:j - 1
                Aij += A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij
        end
    end
    A
end

function A_mul_B!(A::StridedMatrix, B::LowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]*B[j,j]
            for k = j + 1:n
                Aij += A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij
        end
    end
    A
end
function A_mul_B!(A::StridedMatrix, B::UnitLowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = j + 1:n
                Aij += A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij
        end
    end
    A
end

function A_mul_Bc!(A::StridedMatrix, B::UpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]*B.data[j,j]'
            for k = j + 1:n
                Aij += A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij
        end
    end
    A
end
function A_mul_Bc!(A::StridedMatrix, B::UnitUpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = j + 1:n
                Aij += A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij
        end
    end
    A
end

function A_mul_Bc!(A::StridedMatrix, B::LowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]*B.data[j,j]'
            for k = 1:j - 1
                Aij += A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij
        end
    end
    A
end
function A_mul_Bc!(A::StridedMatrix, B::UnitLowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = 1:j - 1
                Aij += A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij
        end
    end
    A
end

function A_mul_Bt!(A::StridedMatrix, B::UpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]*B.data[j,j].'
            for k = j + 1:n
                Aij += A[i,k]*B.data[j,k].'
            end
            A[i,j] = Aij
        end
    end
    A
end
function A_mul_Bt!(A::StridedMatrix, B::UnitUpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = j + 1:n
                Aij += A[i,k]*B.data[j,k].'
            end
            A[i,j] = Aij
        end
    end
    A
end

function A_mul_Bt!(A::StridedMatrix, B::LowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]*B.data[j,j].'
            for k = 1:j - 1
                Aij += A[i,k]*B.data[j,k].'
            end
            A[i,j] = Aij
        end
    end
    A
end
function A_mul_Bt!(A::StridedMatrix, B::UnitLowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = 1:j - 1
                Aij += A[i,k]*B.data[j,k].'
            end
            A[i,j] = Aij
        end
    end
    A
end

#Generic solver using naive substitution
# manually hoisting x[j] significantly improves performance as of Dec 2015
# manually eliding bounds checking significantly improves performance as of Dec 2015
# directly indexing A.data rather than A significantly improves performance as of Dec 2015
# replacing repeated references to A.data with [Adata = A.data and references to Adata]
# does not significantly impact performance as of Dec 2015
# replacing repeated references to A.data[j,j] with [Ajj = A.data[j,j] and references to Ajj]
# does not significantly impact performance as of Dec 2015
function naivesub!(A::UpperTriangular, b::AbstractVector, x::AbstractVector = b)
    n = size(A, 2)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("second dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in n:-1:1
        A.data[j,j] == zero(A.data[j,j]) && throw(SingularException(j))
        xj = x[j] = A.data[j,j] \ b[j]
        for i in j-1:-1:1 # counterintuitively 1:j-1 performs slightly better
            b[i] -= A.data[i,j] * xj
        end
    end
    x
end
function naivesub!(A::UnitUpperTriangular, b::AbstractVector, x::AbstractVector = b)
    n = size(A, 2)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("second dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in n:-1:1
        xj = x[j] = b[j]
        for i in j-1:-1:1 # counterintuitively 1:j-1 performs slightly better
            b[i] -= A.data[i,j] * xj
        end
    end
    x
end
function naivesub!(A::LowerTriangular, b::AbstractVector, x::AbstractVector = b)
    n = size(A, 2)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("second dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in 1:n
        A.data[j,j] == zero(A.data[j,j]) && throw(SingularException(j))
        xj = x[j] = A.data[j,j] \ b[j]
        for i in j+1:n
            b[i] -= A.data[i,j] * xj
        end
    end
    x
end
function naivesub!(A::UnitLowerTriangular, b::AbstractVector, x::AbstractVector = b)
    n = size(A, 2)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("second dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in 1:n
        xj = x[j] = b[j]
        for i in j+1:n
            b[i] -= A.data[i,j] * xj
        end
    end
    x
end
# in the following transpose and conjugate transpose naive substitution variants,
# accumulating in z rather than b[j] significantly improves performance as of Dec 2015
function At_ldiv_B!(A::LowerTriangular, b::AbstractVector, x::AbstractVector = b)
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in n:-1:1
        z = b[j]
        for i in n:-1:j+1
            z -= A.data[i,j] * x[i]
        end
        A.data[j,j] == zero(A.data[j,j]) && throw(SingularException(j))
        x[j] = A.data[j,j] \ z
    end
    x
end
function At_ldiv_B!(A::UnitLowerTriangular, b::AbstractVector, x::AbstractVector = b)
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in n:-1:1
        z = b[j]
        for i in n:-1:j+1
            z -= A.data[i,j] * x[i]
        end
        x[j] = z
    end
    x
end
function At_ldiv_B!(A::UpperTriangular, b::AbstractVector, x::AbstractVector = b)
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in 1:n
        z = b[j]
        for i in 1:j-1
            z -= A.data[i,j] * x[i]
        end
        A.data[j,j] == zero(A.data[j,j]) && throw(SingularException(j))
        x[j] = A.data[j,j] \ z
    end
    x
end
function At_ldiv_B!(A::UnitUpperTriangular, b::AbstractVector, x::AbstractVector = b)
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in 1:n
        z = b[j]
        for i in 1:j-1
            z -= A.data[i,j] * x[i]
        end
        x[j] = z
    end
    x
end
function Ac_ldiv_B!(A::LowerTriangular, b::AbstractVector, x::AbstractVector = b)
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in n:-1:1
        z = b[j]
        for i in n:-1:j+1
            z -= A.data[i,j]' * x[i]
        end
        A.data[j,j] == zero(A.data[j,j]) && throw(SingularException(j))
        x[j] = A.data[j,j]' \ z
    end
    x
end
function Ac_ldiv_B!(A::UnitLowerTriangular, b::AbstractVector, x::AbstractVector = b)
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in n:-1:1
        z = b[j]
        for i in n:-1:j+1
            z -= A.data[i,j]' * x[i]
        end
        x[j] = z
    end
    x
end
function Ac_ldiv_B!(A::UpperTriangular, b::AbstractVector, x::AbstractVector = b)
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in 1:n
        z = b[j]
        for i in 1:j-1
            z -= A.data[i,j]' * x[i]
        end
        A.data[j,j] == zero(A.data[j,j]) && throw(SingularException(j))
        x[j] = A.data[j,j]' \ z
    end
    x
end
function Ac_ldiv_B!(A::UnitUpperTriangular, b::AbstractVector, x::AbstractVector = b)
    n = size(A, 1)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("first dimension of left hand side A, $n, length of output x, $(length(x)), and length of right hand side b, $(length(b)), must be equal"))
    end
    @inbounds for j in 1:n
        z = b[j]
        for i in 1:j-1
            z -= A.data[i,j]' * x[i]
        end
        x[j] = z
    end
    x
end

function A_rdiv_B!(A::StridedMatrix, B::UpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = 1:j - 1
                Aij -= A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij/B[j,j]
        end
    end
    A
end
function A_rdiv_B!(A::StridedMatrix, B::UnitUpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = 1:j - 1
                Aij -= A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij
        end
    end
    A
end

function A_rdiv_B!(A::StridedMatrix, B::LowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = j + 1:n
                Aij -= A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij/B[j,j]
        end
    end
    A
end
function A_rdiv_B!(A::StridedMatrix, B::UnitLowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = j + 1:n
                Aij -= A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij
        end
    end
    A
end

function A_rdiv_Bc!(A::StridedMatrix, B::UpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = j + 1:n
                Aij -= A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij/B.data[j,j]'
        end
    end
    A
end
function A_rdiv_Bc!(A::StridedMatrix, B::UnitUpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = j + 1:n
                Aij -= A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij
        end
    end
    A
end

function A_rdiv_Bc!(A::StridedMatrix, B::LowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = 1:j - 1
                Aij -= A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij/B.data[j,j]'
        end
    end
    A
end
function A_rdiv_Bc!(A::StridedMatrix, B::UnitLowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = 1:j - 1
                Aij -= A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij
        end
    end
    A
end

function A_rdiv_Bt!(A::StridedMatrix, B::UpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = j + 1:n
                Aij -= A[i,k]*B.data[j,k].'
            end
            A[i,j] = Aij/B.data[j,j].'
        end
    end
    A
end
function A_rdiv_Bt!(A::StridedMatrix, B::UnitUpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = j + 1:n
                Aij -= A[i,k]*B.data[j,k].'
            end
            A[i,j] = Aij
        end
    end
    A
end

function A_rdiv_Bt!(A::StridedMatrix, B::LowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = 1:j - 1
                Aij -= A[i,k]*B.data[j,k].'
            end
            A[i,j] = Aij/B.data[j,j].'
        end
    end
    A
end
function A_rdiv_Bt!(A::StridedMatrix, B::UnitLowerTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = 1:j - 1
                Aij -= A[i,k]*B.data[j,k].'
            end
            A[i,j] = Aij
        end
    end
    A
end

for f in (:Ac_mul_B!, :At_mul_B!, :Ac_ldiv_B!, :At_ldiv_B!)
    @eval begin
        $f(A::Union{LowerTriangular,UnitLowerTriangular}, B::UpperTriangular) =
            UpperTriangular($f(A, triu!(B.data)))
        $f(A::Union{UpperTriangular,UnitUpperTriangular}, B::LowerTriangular) =
            LowerTriangular($f(A, tril!(B.data)))
    end
end

A_rdiv_B!(A::UpperTriangular, B::Union{UpperTriangular,UnitUpperTriangular}) =
    UpperTriangular(A_rdiv_B!(triu!(A.data), B))
A_rdiv_B!(A::LowerTriangular, B::Union{LowerTriangular,UnitLowerTriangular}) =
    LowerTriangular(A_rdiv_B!(tril!(A.data), B))

for f in (:A_mul_Bc!, :A_mul_Bt!, :A_rdiv_Bc!, :A_rdiv_Bt!)
    @eval begin
        $f(A::UpperTriangular, B::Union{LowerTriangular,UnitLowerTriangular}) =
            UpperTriangular($f(triu!(A.data), B))
        $f(A::LowerTriangular, B::Union{UpperTriangular,UnitUpperTriangular}) =
            LowerTriangular($f(tril!(A.data), B))
    end
end

# Promotion
## Promotion methods in matmul don't apply to triangular multiplication since
## it is inplace. Hence we have to make very similar definitions, but without
## allocation of a result array. For multiplication and unit diagonal division
## the element type doesn't have to be stable under division whereas that is
## necessary in the general triangular solve problem.

## Some Triangular-Triangular cases. We might want to write taylored methods
## for these cases, but I'm not sure it is worth it.
for t in (UpperTriangular, UnitUpperTriangular, LowerTriangular, UnitLowerTriangular)
    @eval begin
        (*)(A::Tridiagonal, B::$t) = A_mul_B!(full(A), B)
    end
end

for (f1, f2) in ((:*, :A_mul_B!), (:\, :A_ldiv_B!))
    @eval begin
        function ($f1)(A::LowerTriangular, B::LowerTriangular)
            TAB = typeof(($f1)(zero(eltype(A)), zero(eltype(B))) +
                         ($f1)(zero(eltype(A)), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copy!(BB, B)
            return LowerTriangular($f2(convert(AbstractMatrix{TAB}, A), BB))
        end

        function $(f1)(A::UnitLowerTriangular, B::LowerTriangular)
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copy!(BB, B)
            return LowerTriangular($f2(convert(AbstractMatrix{TAB}, A), BB))
        end

        function ($f1)(A::UpperTriangular, B::UpperTriangular)
            TAB = typeof(($f1)(zero(eltype(A)), zero(eltype(B))) +
                         ($f1)(zero(eltype(A)), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copy!(BB, B)
            return UpperTriangular($f2(convert(AbstractMatrix{TAB}, A), BB))
        end

        function ($f1)(A::UnitUpperTriangular, B::UpperTriangular)
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copy!(BB, B)
            return UpperTriangular($f2(convert(AbstractMatrix{TAB}, A), BB))
        end
    end
end

for (f1, f2) in ((:Ac_mul_B, :Ac_mul_B!), (:At_mul_B, :At_mul_B!),
                 (:Ac_ldiv_B, Ac_ldiv_B!), (:At_ldiv_B, :At_ldiv_B!))
    @eval begin
        function ($f1)(A::UpperTriangular, B::LowerTriangular)
            TAB = typeof(($f1)(zero(eltype(A)), zero(eltype(B))) +
                         ($f1)(zero(eltype(A)), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copy!(BB, B)
            return LowerTriangular($f2(convert(AbstractMatrix{TAB}, A), BB))
        end

        function ($f1)(A::UnitUpperTriangular, B::LowerTriangular)
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copy!(BB, B)
            return LowerTriangular($f2(convert(AbstractMatrix{TAB}, A), BB))
        end

        function ($f1)(A::LowerTriangular, B::UpperTriangular)
            TAB = typeof(($f1)(zero(eltype(A)), zero(eltype(B))) +
                         ($f1)(zero(eltype(A)), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copy!(BB, B)
            return UpperTriangular($f2(convert(AbstractMatrix{TAB}, A), BB))
        end

        function ($f1)(A::UnitLowerTriangular, B::UpperTriangular)
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
            BB = similar(B, TAB, size(B))
            copy!(BB, B)
            return UpperTriangular($f2(convert(AbstractMatrix{TAB}, A), BB))
        end
    end
end

function (/)(A::LowerTriangular, B::LowerTriangular)
    TAB = typeof((/)(zero(eltype(A)), zero(eltype(B))) +
                 (/)(zero(eltype(A)), zero(eltype(B))))
    AA = similar(A, TAB, size(A))
    copy!(AA, A)
    return LowerTriangular(A_rdiv_B!(AA, convert(AbstractMatrix{TAB}, B)))
end
function (/)(A::LowerTriangular, B::UnitLowerTriangular)
    TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                 (*)(zero(eltype(A)), zero(eltype(B))))
    AA = similar(A, TAB, size(A))
    copy!(AA, A)
    return LowerTriangular(A_rdiv_B!(AA, convert(AbstractMatrix{TAB}, B)))
end
function (/)(A::UpperTriangular, B::UpperTriangular)
    TAB = typeof((/)(zero(eltype(A)), zero(eltype(B))) +
                 (/)(zero(eltype(A)), zero(eltype(B))))
    AA = similar(A, TAB, size(A))
    copy!(AA, A)
    return UpperTriangular(A_rdiv_B!(AA, convert(AbstractMatrix{TAB}, B)))
end
function (/)(A::UpperTriangular, B::UnitUpperTriangular)
    TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                 (*)(zero(eltype(A)), zero(eltype(B))))
    AA = similar(A, TAB, size(A))
    copy!(AA, A)
    return UpperTriangular(A_rdiv_B!(AA, convert(AbstractMatrix{TAB}, B)))
end

for (f1, f2) in ((:A_mul_Bc, :A_mul_Bc!), (:A_mul_Bt, :A_mul_Bt!),
                 (:A_rdiv_Bc, :A_rdiv_Bc!), (:A_rdiv_Bt, :A_rdiv_Bt!))
    @eval begin
        function $f1(A::LowerTriangular, B::UpperTriangular)
            TAB = typeof(($f1)(zero(eltype(A)), zero(eltype(B))) +
                         ($f1)(zero(eltype(A)), zero(eltype(B))))
            AA = similar(A, TAB, size(A))
            copy!(AA, A)
            return LowerTriangular($f2(AA, convert(AbstractMatrix{TAB}, B)))
        end

        function $f1(A::LowerTriangular, B::UnitUpperTriangular)
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
            AA = similar(A, TAB, size(A))
            copy!(AA, A)
            return LowerTriangular($f2(AA, convert(AbstractMatrix{TAB}, B)))
        end

        function $f1(A::UpperTriangular, B::LowerTriangular)
            TAB = typeof(($f1)(zero(eltype(A)), zero(eltype(B))) +
                         ($f1)(zero(eltype(A)), zero(eltype(B))))
            AA = similar(A, TAB, size(A))
            copy!(AA, A)
            return UpperTriangular($f2(AA, convert(AbstractMatrix{TAB}, B)))
        end

        function $f1(A::UpperTriangular, B::UnitLowerTriangular)
            TAB = typeof((*)(zero(eltype(A)), zero(eltype(B))) +
                         (*)(zero(eltype(A)), zero(eltype(B))))
            AA = similar(A, TAB, size(A))
            copy!(AA, A)
            return UpperTriangular($f2(AA, convert(AbstractMatrix{TAB}, B)))
        end
    end
end

## The general promotion methods

for (f, g) in ((:*, :A_mul_B!), (:Ac_mul_B, :Ac_mul_B!), (:At_mul_B, :At_mul_B!))
    @eval begin
        function ($f)(A::AbstractTriangular, B::AbstractTriangular)
            TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
            BB = similar(B, TAB, size(B))
            copy!(BB, B)
            ($g)(convert(AbstractArray{TAB}, A), BB)
        end
    end
end
for (f, g) in ((:A_mul_Bc, :A_mul_Bc!), (:A_mul_Bt, :A_mul_Bt!))
    @eval begin
        function ($f)(A::AbstractTriangular, B::AbstractTriangular)
            TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
            AA = similar(A, TAB, size(A))
            copy!(AA, A)
            ($g)(AA, convert(AbstractArray{TAB}, B))
        end
    end
end

for mat in (:AbstractVector, :AbstractMatrix)

### Multiplication with triangle to the left and hence rhs cannot be transposed.
for (f, g) in ((:*, :A_mul_B!), (:Ac_mul_B, :Ac_mul_B!), (:At_mul_B, :At_mul_B!))
    @eval begin
        function ($f)(A::AbstractTriangular, B::$mat)
            TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
            BB = similar(B, TAB, size(B))
            copy!(BB, B)
            ($g)(convert(AbstractArray{TAB}, A), BB)
        end
    end
end
### Left division with triangle to the left hence rhs cannot be transposed. No quotients.
for (f, g) in ((:\, :A_ldiv_B!), (:Ac_ldiv_B, :Ac_ldiv_B!), (:At_ldiv_B, :At_ldiv_B!))
    @eval begin
        function ($f)(A::Union{UnitUpperTriangular,UnitLowerTriangular}, B::$mat)
            TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
            BB = similar(B, TAB, size(B))
            copy!(BB, B)
            ($g)(convert(AbstractArray{TAB}, A), BB)
        end
    end
end
### Left division with triangle to the left hence rhs cannot be transposed. Quotients.
for (f, g) in ((:\, :A_ldiv_B!), (:Ac_ldiv_B, :Ac_ldiv_B!), (:At_ldiv_B, :At_ldiv_B!))
    @eval begin
        function ($f)(A::Union{UpperTriangular,LowerTriangular}, B::$mat)
            TAB = typeof((zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))/one(eltype(A)))
            BB = similar(B, TAB, size(B))
            copy!(BB, B)
            ($g)(convert(AbstractArray{TAB}, A), BB)
        end
    end
end
### Multiplication with triangle to the right and hence lhs cannot be transposed.
for (f, g) in ((:*, :A_mul_B!), (:A_mul_Bc, :A_mul_Bc!), (:A_mul_Bt, :A_mul_Bt!))
    mat != :AbstractVector && @eval begin
        function ($f)(A::$mat, B::AbstractTriangular)
            TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
            AA = similar(A, TAB, size(A))
            copy!(AA, A)
            ($g)(AA, convert(AbstractArray{TAB}, B))
        end
    end
end
### Right division with triangle to the right hence lhs cannot be transposed. No quotients.
for (f, g) in ((:/, :A_rdiv_B!), (:A_rdiv_Bc, :A_rdiv_Bc!), (:A_rdiv_Bt, :A_rdiv_Bt!))
    @eval begin
        function ($f)(A::$mat, B::Union{UnitUpperTriangular, UnitLowerTriangular})
            TAB = typeof(zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))
            AA = similar(A, TAB, size(A))
            copy!(AA, A)
            ($g)(AA, convert(AbstractArray{TAB}, B))
        end
    end
end

### Right division with triangle to the right hence lhs cannot be transposed. Quotients.
for (f, g) in ((:/, :A_rdiv_B!), (:A_rdiv_Bc, :A_rdiv_Bc!), (:A_rdiv_Bt, :A_rdiv_Bt!))
    @eval begin
        function ($f)(A::$mat, B::Union{UpperTriangular,LowerTriangular})
            TAB = typeof((zero(eltype(A))*zero(eltype(B)) + zero(eltype(A))*zero(eltype(B)))/one(eltype(A)))
            AA = similar(A, TAB, size(A))
            copy!(AA, A)
            ($g)(AA, convert(AbstractArray{TAB}, B))
        end
    end
end
end

# If these are not defined, they will fallback to the versions in matmul.jl
# and dispatch to generic_matmatmul! which is very costly to compile. The methods
# below might compute an unnecessary copy. Eliminating the copy requires adding
# all the promotion logic here once again. Since these methods are probably relatively
# rare, we chose not to bother for now.
Ac_mul_B(A::AbstractMatrix, B::AbstractTriangular) = (*)(ctranspose(A), B)
At_mul_B(A::AbstractMatrix, B::AbstractTriangular) = (*)(transpose(A), B)
A_mul_Bc(A::AbstractTriangular, B::AbstractMatrix) = (*)(A, ctranspose(B))
A_mul_Bt(A::AbstractTriangular, B::AbstractMatrix) = (*)(A, transpose(B))
Ac_mul_Bc(A::AbstractTriangular, B::AbstractTriangular) = Ac_mul_B(A, B')
Ac_mul_Bc(A::AbstractTriangular, B::AbstractMatrix) = Ac_mul_B(A, B')
Ac_mul_Bc(A::AbstractMatrix, B::AbstractTriangular) = A_mul_Bc(A', B)
At_mul_Bt(A::AbstractTriangular, B::AbstractTriangular) = At_mul_B(A, B.')
At_mul_Bt(A::AbstractTriangular, B::AbstractMatrix) = At_mul_B(A, B.')
At_mul_Bt(A::AbstractMatrix, B::AbstractTriangular) = A_mul_Bt(A.', B)

# Specializations for RowVector
@inline *(rowvec::RowVector, A::AbstractTriangular) = transpose(A * transpose(rowvec))
@inline A_mul_Bt(rowvec::RowVector, A::AbstractTriangular) = transpose(A * transpose(rowvec))
@inline A_mul_Bt(A::AbstractTriangular, rowvec::RowVector) = A * transpose(rowvec)
@inline At_mul_Bt(A::AbstractTriangular, rowvec::RowVector) = A.' * transpose(rowvec)
@inline A_mul_Bc(rowvec::RowVector, A::AbstractTriangular) = ctranspose(A * ctranspose(rowvec))
@inline A_mul_Bc(A::AbstractTriangular, rowvec::RowVector) = A * ctranspose(rowvec)
@inline Ac_mul_Bc(A::AbstractTriangular, rowvec::RowVector) = A' * ctranspose(rowvec)

@inline /(rowvec::RowVector, A::Union{UpperTriangular,LowerTriangular}) = transpose(transpose(A) \ transpose(rowvec))
@inline /(rowvec::RowVector, A::Union{UnitUpperTriangular,UnitLowerTriangular}) = transpose(transpose(A) \ transpose(rowvec))

@inline A_rdiv_Bt(rowvec::RowVector, A::Union{UpperTriangular,LowerTriangular}) = transpose(A \ transpose(rowvec))
@inline A_rdiv_Bt(rowvec::RowVector, A::Union{UnitUpperTriangular,UnitLowerTriangular}) = transpose(A \ transpose(rowvec))

@inline A_rdiv_Bc(rowvec::RowVector, A::Union{UpperTriangular,LowerTriangular}) = ctranspose(A \ ctranspose(rowvec))
@inline A_rdiv_Bc(rowvec::RowVector, A::Union{UnitUpperTriangular,UnitLowerTriangular}) = ctranspose(A \ ctranspose(rowvec))

\(::Union{UpperTriangular,LowerTriangular}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
\(::Union{UnitUpperTriangular,UnitLowerTriangular}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))

At_ldiv_B(::Union{UpperTriangular,LowerTriangular}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
At_ldiv_B(::Union{UnitUpperTriangular,UnitLowerTriangular}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))

Ac_ldiv_B(::Union{UpperTriangular,LowerTriangular}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
Ac_ldiv_B(::Union{UnitUpperTriangular,UnitLowerTriangular}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))

# Complex matrix power for upper triangular factor, see:
#   Higham and Lin, "A Schur-Padé algorithm for fractional powers of a Matrix",
#     SIAM J. Matrix Anal. & Appl., 32 (3), (2011) 1056–1078.
#   Higham and Lin, "An improved Schur-Padé algorithm for fractional powers of
#     a matrix and their Fréchet derivatives", SIAM. J. Matrix Anal. & Appl.,
#     34(3), (2013) 1341–1360.
function powm!(A0::UpperTriangular{<:BlasFloat}, p::Real)
    if abs(p) >= 1
        ArgumentError("p must be a real number in (-1,1), got $p")
    end

    normA0 = norm(A0, 1)
    scale!(A0, 1/normA0)

    theta = [1.53e-5, 2.25e-3, 1.92e-2, 6.08e-2, 1.25e-1, 2.03e-1, 2.84e-1]
    n = checksquare(A0)

    A, m, s = invsquaring(A0, theta)
    A = I - A

    # Compute accurate diagonal of I - T
    sqrt_diag!(A0, A, s)
    for i = 1:n
        A[i, i] = -A[i, i]
    end
    # Compute the Padé approximant
    c = 0.5 * (p - m) / (2 * m - 1)
    triu!(A)
    S = c * A
    Stmp = similar(S)
    for j = m-1:-1:1
        j4 = 4 * j
        c = (-p - j) / (j4 + 2)
        for i = 1:n
            @inbounds S[i, i] = S[i, i] + 1
        end
        copy!(Stmp, S)
        scale!(S, A, c)
        A_ldiv_B!(Stmp, S.data)

        c = (p - j) / (j4 - 2)
        for i = 1:n
            @inbounds S[i, i] = S[i, i] + 1
        end
        copy!(Stmp, S)
        scale!(S, A, c)
        A_ldiv_B!(Stmp, S.data)
    end
    for i = 1:n
        S[i, i] = S[i, i] + 1
    end
    copy!(Stmp, S)
    scale!(S, A, -p)
    A_ldiv_B!(Stmp, S.data)
    for i = 1:n
        @inbounds S[i, i] = S[i, i] + 1
    end

    blockpower!(A0, S, p/(2^s))
    for m = 1:s
        A_mul_B!(Stmp.data, S, S)
        copy!(S, Stmp)
        blockpower!(A0, S, p/(2^(s-m)))
    end
    scale!(S, normA0^p)
    return S
end
powm(A::LowerTriangular, p::Real) = powm(A.', p::Real).'

# Complex matrix logarithm for the upper triangular factor, see:
#   Al-Mohy and Higham, "Improved inverse  scaling and squaring algorithms for
#     the matrix logarithm", SIAM J. Sci. Comput., 34(4), (2012), pp. C153–C169.
#   Al-Mohy, Higham and Relton, "Computing the Frechet derivative of the matrix
#     logarithm and estimating the condition number", SIAM J. Sci. Comput.,
#     35(4), (2013), C394–C410.
#
# Based on the code available at http://eprints.ma.man.ac.uk/1851/02/logm.zip,
# Copyright (c) 2011, Awad H. Al-Mohy and Nicholas J. Higham
# Julia version relicensed with permission from original authors
function logm(A0::UpperTriangular{T}) where T<:Union{Float64,Complex{Float64}}
    maxsqrt = 100
    theta = [1.586970738772063e-005,
         2.313807884242979e-003,
         1.938179313533253e-002,
         6.209171588994762e-002,
         1.276404810806775e-001,
         2.060962623452836e-001,
         2.879093714241194e-001]
    tmax = size(theta, 1)
    n = size(A0, 1)
    A = copy(A0)
    p = 0
    m = 0

    # Compute repeated roots
    d = complex(diag(A))
    dm1 = d .- 1
    s = 0
    while norm(dm1, Inf) > theta[tmax] && s < maxsqrt
        d .= sqrt.(d)
        dm1 .= d .- 1
        s = s + 1
    end
    s0 = s
    for k = 1:min(s, maxsqrt)
        A = sqrtm(A)
    end

    AmI = A - I
    d2 = sqrt(norm(AmI^2, 1))
    d3 = cbrt(norm(AmI^3, 1))
    alpha2 = max(d2, d3)
    foundm = false
    if alpha2 <= theta[2]
        m = alpha2 <= theta[1] ? 1 : 2
        foundm = true
    end

    while !foundm
        more = false
        if s > s0
            d3 = cbrt(norm(AmI^3, 1))
        end
        d4 = norm(AmI^4, 1)^(1/4)
        alpha3 = max(d3, d4)
        if alpha3 <= theta[tmax]
            for j = 3:tmax
                if alpha3 <= theta[j]
                    break
                end
            end
            if j <= 6
                m = j
                break
            elseif alpha3 / 2 <= theta[5] && p < 2
                more = true
                p = p + 1
           end
        end

        if !more
            d5 = norm(AmI^5, 1)^(1/5)
            alpha4 = max(d4, d5)
            eta = min(alpha3, alpha4)
            if eta <= theta[tmax]
                j = 0
                for j = 6:tmax
                    if eta <= theta[j]
                        m = j
                        break
                    end
                end
                break
            end
        end

        if s == maxsqrt
            m = tmax
            break
        end
        A = sqrtm(A)
        AmI = A - I
        s = s + 1
    end

    # Compute accurate superdiagonal of T
    p = 1 / 2^s
    for k = 1:n-1
        Ak = A0[k,k]
        Akp1 = A0[k+1,k+1]
        Akp = Ak^p
        Akp1p = Akp1^p
        A[k,k] = Akp
        A[k+1,k+1] = Akp1p
        if Ak == Akp1
            A[k,k+1] = p * A0[k,k+1] * Ak^(p-1)
        elseif 2 * abs(Ak) < abs(Akp1) || 2 * abs(Akp1) < abs(Ak)
            A[k,k+1] = A0[k,k+1] * (Akp1p - Akp) / (Akp1 - Ak)
        else
            logAk = log(Ak)
            logAkp1 = log(Akp1)
            w = atanh((Akp1 - Ak)/(Akp1 + Ak)) + im*pi*ceil((imag(logAkp1-logAk)-pi)/(2*pi))
            dd = 2 * exp(p*(logAk+logAkp1)/2) * sinh(p*w) / (Akp1 - Ak)
            A[k,k+1] = A0[k,k+1] * dd
        end
    end

    # Compute accurate diagonal of T
    for i = 1:n
        a = A0[i,i]
        if s == 0
            r = a - 1
        end
        s0 = s
        if angle(a) >= pi / 2
            a = sqrt(a)
            s0 = s - 1
        end
        z0 = a - 1
        a = sqrt(a)
        r = 1 + a
        for j = 1:s0-1
            a = sqrt(a)
            r = r * (1 + a)
        end
        A[i,i] = z0 / r
    end

    # Get the Gauss-Legendre quadrature points and weights
    R = zeros(Float64, m, m)
    for i = 1:m - 1
        R[i,i+1] = i / sqrt((2 * i)^2 - 1)
        R[i+1,i] = R[i,i+1]
    end
    x,V = eig(R)
    w = Vector{Float64}(m)
    for i = 1:m
        x[i] = (x[i] + 1) / 2
        w[i] = V[1,i]^2
    end

    # Compute the Padé approximation
    Y = zeros(T, n, n)
    for k = 1:m
        Y = Y + w[k] * (A / (x[k] * A + I))
    end

    # Scale back
    scale!(2^s, Y)

    # Compute accurate diagonal and superdiagonal of log(T)
    for k = 1:n-1
        Ak = A0[k,k]
        Akp1 = A0[k+1,k+1]
        logAk = log(Ak)
        logAkp1 = log(Akp1)
        Y[k,k] = logAk
        Y[k+1,k+1] = logAkp1
        if Ak == Akp1
            Y[k,k+1] = A0[k,k+1] / Ak
        elseif 2 * abs(Ak) < abs(Akp1) || 2 * abs(Akp1) < abs(Ak)
            Y[k,k+1] = A0[k,k+1] * (logAkp1 - logAk) / (Akp1 - Ak)
        else
            w = atanh((Akp1 - Ak)/(Akp1 + Ak) + im*pi*(ceil((imag(logAkp1-logAk) - pi)/(2*pi))))
            Y[k,k+1] = 2 * A0[k,k+1] * w / (Akp1 - Ak)
        end
    end

    return UpperTriangular(Y)
end
logm(A::LowerTriangular) = logm(A.').'

# Auxiliary functions for logm and matrix power

# Compute accurate diagonal of A = A0^s - I
#   Al-Mohy, "A more accurate Briggs method for the logarithm",
#      Numer. Algorithms, 59, (2012), 393–402.
function sqrt_diag!(A0::UpperTriangular, A::UpperTriangular, s)
    n = checksquare(A0)
    @inbounds for i = 1:n
        a = complex(A0[i,i])
        if s == 0
            A[i,i] = a - 1
        else
            s0 = s
            if imag(a) >= 0 && real(a) <= 0 && a != 0
                a = sqrt(a)
                s0 = s - 1
            end
            z0 = a - 1
            a = sqrt(a)
            r = 1 + a
            for j = 1:s0-1
                a = sqrt(a)
                r = r * (1 + a)
            end
            A[i,i] = z0 / r
        end
    end
end

# Used only by powm at the moment
# Repeatedly compute the square roots of A so that in the end its
# eigenvalues are close enough to the positive real line
function invsquaring(A0::UpperTriangular, theta)
    # assumes theta is in ascending order
    maxsqrt = 100
    tmax = size(theta, 1)
    n = checksquare(A0)
    A = complex(copy(A0))
    p = 0
    m = 0

    # Compute repeated roots
    d = complex(diag(A))
    dm1 = d .- 1
    s = 0
    while norm(dm1, Inf) > theta[tmax] && s < maxsqrt
        d .= sqrt.(d)
        dm1 .= d .- 1
        s = s + 1
    end
    s0 = s
    for k = 1:min(s, maxsqrt)
        A = sqrtm(A)
    end

    AmI = A - I
    d2 = sqrt(norm(AmI^2, 1))
    d3 = cbrt(norm(AmI^3, 1))
    alpha2 = max(d2, d3)
    foundm = false
    if alpha2 <= theta[2]
        m = alpha2 <= theta[1] ? 1 : 2
        foundm = true
    end

    while !foundm
        more = false
        if s > s0
            d3 = cbrt(norm(AmI^3, 1))
        end
        d4 = norm(AmI^4, 1)^(1/4)
        alpha3 = max(d3, d4)
        if alpha3 <= theta[tmax]
            for j = 3:tmax
                if alpha3 <= theta[j]
                    break
                elseif alpha3 / 2 <= theta[5] && p < 2
                    more = true
                    p = p + 1
                end
            end
            if j <= 6
                m = j
                foundm = true
                break
            elseif alpha3 / 2 <= theta[5] && p < 2
                more = true
                p = p + 1
           end
        end

        if !more
            d5 = norm(AmI^5, 1)^(1/5)
            alpha4 = max(d4, d5)
            eta = min(alpha3, alpha4)
            if eta <= theta[tmax]
                j = 0
                for j = 6:tmax
                    if eta <= theta[j]
                        m = j
                        break
                    end
                    break
                end
            end
            if s == maxsqrt
                m = tmax
                break
            end
            A = sqrtm(A)
            AmI = A - I
            s = s + 1
        end
    end

    # Compute accurate superdiagonal of T
    p = 1 / 2^s
    A = complex(A)
    blockpower!(A, A0, p)
    return A,m,s
end

# Compute accurate diagonal and superdiagonal of A = A0^p
function blockpower!(A::UpperTriangular, A0::UpperTriangular, p)
    n = checksquare(A0)
    @inbounds for k = 1:n-1
        Ak = complex(A0[k,k])
        Akp1 = complex(A0[k+1,k+1])

        Akp = Ak^p
        Akp1p = Akp1^p

        A[k,k] = Akp
        A[k+1,k+1] = Akp1p

        if Ak == Akp1
            A[k,k+1] = p * A0[k,k+1] * Ak^(p-1)
        elseif 2 * abs(Ak) < abs(Akp1) || 2 * abs(Akp1) < abs(Ak)
            A[k,k+1] = A0[k,k+1] * (Akp1p - Akp) / (Akp1 - Ak)
        else
            logAk = log(Ak)
            logAkp1 = log(Akp1)
            w = atanh((Akp1 - Ak)/(Akp1 + Ak)) + im * pi * unw(logAkp1-logAk)
            dd = 2 * exp(p*(logAk+logAkp1)/2) * sinh(p*w) / (Akp1 - Ak);
            A[k,k+1] = A0[k,k+1] * dd
        end
    end
end

# Unwinding number
unw(x::Real) = 0
unw(x::Number) = ceil((imag(x) - pi) / (2 * pi))

# End of auxiliary functions for logm and matrix power

function sqrtm(A::UpperTriangular)
    realmatrix = false
    if isreal(A)
        realmatrix = true
        for i = 1:checksquare(A)
            x = real(A[i,i])
            if x < zero(x)
                realmatrix = false
                break
            end
        end
    end
    sqrtm(A,Val(realmatrix))
end
function sqrtm(A::UpperTriangular{T},::Val{realmatrix}) where {T,realmatrix}
    B = A.data
    n = checksquare(B)
    t = realmatrix ? typeof(sqrt(zero(T))) : typeof(sqrt(complex(zero(T))))
    R = zeros(t, n, n)
    tt = typeof(zero(t)*zero(t))
    @inbounds for j = 1:n
        R[j,j] = realmatrix ? sqrt(B[j,j]) : sqrt(complex(B[j,j]))
        for i = j-1:-1:1
            r::tt = B[i,j]
            @simd for k = i+1:j-1
                r -= R[i,k]*R[k,j]
            end
            iszero(r) || (R[i,j] = sylvester(R[i,i],R[j,j],-r))
        end
    end
    return UpperTriangular(R)
end
function sqrtm(A::UnitUpperTriangular{T}) where T
    B = A.data
    n = checksquare(B)
    t = typeof(sqrt(zero(T)))
    R = eye(t, n, n)
    tt = typeof(zero(t)*zero(t))
    half = inv(R[1,1]+R[1,1]) # for general, algebraic cases. PR#20214
    @inbounds for j = 1:n
        for i = j-1:-1:1
            r::tt = B[i,j]
            @simd for k = i+1:j-1
                r -= R[i,k]*R[k,j]
            end
            r==0 || (R[i,j] = half*r)
        end
    end
    return UnitUpperTriangular(R)
end
sqrtm(A::LowerTriangular) = sqrtm(A.').'
sqrtm(A::UnitLowerTriangular) = sqrtm(A.').'

# Generic eigensystems
eigvals(A::AbstractTriangular) = diag(A)
function eigvecs(A::AbstractTriangular{T}) where T
    TT = promote_type(T, Float32)
    if TT <: BlasFloat
        return eigvecs(convert(AbstractMatrix{TT}, A))
    else
        throw(ArgumentError("eigvecs type $(typeof(A)) not supported. Please submit a pull request."))
    end
end
det(A::UnitUpperTriangular{T}) where {T} = one(T)
det(A::UnitLowerTriangular{T}) where {T} = one(T)
logdet(A::UnitUpperTriangular{T}) where {T} = zero(T)
logdet(A::UnitLowerTriangular{T}) where {T} = zero(T)
logabsdet(A::UnitUpperTriangular{T}) where {T} = zero(T), one(T)
logabsdet(A::UnitLowerTriangular{T}) where {T} = zero(T), one(T)
det(A::UpperTriangular) = prod(diag(A.data))
det(A::LowerTriangular) = prod(diag(A.data))
function logabsdet(A::Union{UpperTriangular{T},LowerTriangular{T}}) where T
    sgn = one(T)
    abs_det = zero(real(T))
    @inbounds for i in 1:size(A,1)
        diag_i = A.data[i,i]
        sgn *= sign(diag_i)
        abs_det += log(abs(diag_i))
    end
    return abs_det, sgn
end

eigfact(A::AbstractTriangular) = Eigen(eigvals(A), eigvecs(A))

# Generic singular systems
for func in (:svd, :svdfact, :svdfact!, :svdvals)
    @eval begin
        ($func)(A::AbstractTriangular) = ($func)(full(A))
    end
end

factorize(A::AbstractTriangular) = A
