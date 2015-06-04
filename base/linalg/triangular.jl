# This file is a part of Julia. License is MIT: http://julialang.org/license

## Triangular

abstract AbstractTriangular{T,S<:AbstractMatrix} <: AbstractMatrix{T} # could be renamed to Triangular when than name has been fully deprecated

# First loop through all methods that don't need special care for upper/lower and unit diagonal
for t in (:LowerTriangular, :UnitLowerTriangular, :UpperTriangular, :UnitUpperTriangular)
    @eval begin
        immutable $t{T,S<:AbstractMatrix} <: AbstractTriangular{T,S}
            data::S
        end
        function $t(A::AbstractMatrix)
            Base.LinAlg.chksquare(A)
            return $t{eltype(A), typeof(A)}(A)
        end

        size(A::$t, args...) = size(A.data, args...)

        convert{T,S}(::Type{$t{T}}, A::$t{T,S}) = A
        convert{Tnew,Told,S}(::Type{$t{Tnew}}, A::$t{Told,S}) = (Anew = convert(AbstractMatrix{Tnew}, A.data); $t(Anew))
        convert{Tnew,Told,S}(::Type{AbstractMatrix{Tnew}}, A::$t{Told,S}) = convert($t{Tnew}, A)
        convert{T,S}(::Type{Matrix}, A::$t{T,S}) = convert(Matrix{T}, A)

        function similar{T,S,Tnew}(A::$t{T,S}, ::Type{Tnew}, dims::Dims)
            if dims[1] != dims[2]
                throw(ArgumentError("Triangular matrix must be square"))
            end
            if length(dims) != 2
                throw(ArgumentError("Triangular matrix must have two dimensions"))
            end
            B = similar(A.data, Tnew, dims)
            return $t(B)
        end

        copy{T,S}(A::$t{T,S}) = $t{T,S}(copy(A.data))

        big(A::$t) = $t(big(A.data))

        real{T<:Real}(A::$t{T}) = A
        real{T<:Complex}(A::$t{T}) = (B = real(A.data); $t(B))

    end
end

full(A::AbstractTriangular) = convert(Matrix, A)

fill!(A::AbstractTriangular, x) = (fill!(A.data, x); A)

# then handle all methods that requires specific handling of upper/lower and unit diagonal

function convert{Tret,T,S}(::Type{Matrix{Tret}}, A::LowerTriangular{T,S})
    B = Array(Tret, size(A, 1), size(A, 1))
    copy!(B, A.data)
    tril!(B)
    B
end
function convert{Tret,T,S}(::Type{Matrix{Tret}}, A::UnitLowerTriangular{T,S})
    B = Array(Tret, size(A, 1), size(A, 1))
    copy!(B, A.data)
    tril!(B)
    for i = 1:size(B,1)
        B[i,i] = 1
    end
    B
end
function convert{Tret,T,S}(::Type{Matrix{Tret}}, A::UpperTriangular{T,S})
    B = Array(Tret, size(A, 1), size(A, 1))
    copy!(B, A.data)
    triu!(B)
    B
end
function convert{Tret,T,S}(::Type{Matrix{Tret}}, A::UnitUpperTriangular{T,S})
    B = Array(Tret, size(A, 1), size(A, 1))
    copy!(B, A.data)
    triu!(B)
    for i = 1:size(B,1)
        B[i,i] = 1
    end
    B
end

function full!{T,S}(A::LowerTriangular{T,S})
    B = A.data
    tril!(B)
    B
end
function full!{T,S}(A::UnitLowerTriangular{T,S})
    B = A.data
    tril!(B)
    for i = 1:size(A,1)
        B[i,i] = 1
    end
    B
end
function full!{T,S}(A::UpperTriangular{T,S})
    B = A.data
    triu!(B)
    B
end
function full!{T,S}(A::UnitUpperTriangular{T,S})
    B = A.data
    triu!(B)
    for i = 1:size(A,1)
        B[i,i] = 1
    end
    B
end

getindex{T,S}(A::UnitLowerTriangular{T,S}, i::Integer, j::Integer) = i == j ? one(T) : (i > j ? A.data[i,j] : zero(A.data[i,j]))
getindex{T,S}(A::LowerTriangular{T,S}, i::Integer, j::Integer) = i >= j ? A.data[i,j] : zero(A.data[i,j])
getindex{T,S}(A::UnitUpperTriangular{T,S}, i::Integer, j::Integer) = i == j ? one(T) : (i < j ? A.data[i,j] : zero(A.data[i,j]))
getindex{T,S}(A::UpperTriangular{T,S}, i::Integer, j::Integer) = i <= j ? A.data[i,j] : zero(A.data[i,j])

setindex!(A::UpperTriangular, x, i::Integer, j::Integer) = i <= j ? (A.data[i,j] = x; A) : throw(BoundsError())
setindex!(A::UnitUpperTriangular, x, i::Integer, j::Integer) = i < j ? (A.data[i,j] = x; A) : throw(BoundsError())
setindex!(A::LowerTriangular, x, i::Integer, j::Integer) = i >= j ? (A.data[i,j] = x; A) : throw(BoundsError())
setindex!(A::UnitLowerTriangular, x, i::Integer, j::Integer) = i > j ? (A.data[i,j] = x; A) : throw(BoundsError())

istril(A::LowerTriangular) = true
istril(A::UnitLowerTriangular) = true
istriu(A::UpperTriangular) = true
istriu(A::UnitUpperTriangular) = true

transpose{T,S}(A::LowerTriangular{T,S}) = UpperTriangular{T, S}(transpose(A.data))
transpose{T,S}(A::UnitLowerTriangular{T,S}) = UnitUpperTriangular{T, S}(transpose(A.data))
transpose{T,S}(A::UpperTriangular{T,S}) = LowerTriangular{T, S}(transpose(A.data))
transpose{T,S}(A::UnitUpperTriangular{T,S}) = UnitLowerTriangular{T, S}(transpose(A.data))
ctranspose{T,S}(A::LowerTriangular{T,S}) = UpperTriangular{T, S}(ctranspose(A.data))
ctranspose{T,S}(A::UnitLowerTriangular{T,S}) = UnitUpperTriangular{T, S}(ctranspose(A.data))
ctranspose{T,S}(A::UpperTriangular{T,S}) = LowerTriangular{T, S}(ctranspose(A.data))
ctranspose{T,S}(A::UnitUpperTriangular{T,S}) = UnitLowerTriangular{T, S}(ctranspose(A.data))

transpose!{T,S}(A::LowerTriangular{T,S}) = UpperTriangular{T, S}(copytri!(A.data, 'L'))
transpose!{T,S}(A::UnitLowerTriangular{T,S}) = UnitUpperTriangular{T, S}(copytri!(A.data, 'L'))
transpose!{T,S}(A::UpperTriangular{T,S}) = LowerTriangular{T, S}(copytri!(A.data, 'U'))
transpose!{T,S}(A::UnitUpperTriangular{T,S}) = UnitLowerTriangular{T, S}(copytri!(A.data, 'U'))
ctranspose!{T,S}(A::LowerTriangular{T,S}) = UpperTriangular{T, S}(copytri!(A.data, 'L' , true))
ctranspose!{T,S}(A::UnitLowerTriangular{T,S}) = UnitUpperTriangular{T, S}(copytri!(A.data, 'L' , true))
ctranspose!{T,S}(A::UpperTriangular{T,S}) = LowerTriangular{T, S}(copytri!(A.data, 'U' , true))
ctranspose!{T,S}(A::UnitUpperTriangular{T,S}) = UnitLowerTriangular{T, S}(copytri!(A.data, 'U' , true))

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
A_mul_B!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat) = A_mul_B!(A, copy!(C, B))
A_mul_Bc!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat) = A_mul_Bc!(A, copy!(C, B))

for (t, uploc, isunitc) in ((:LowerTriangular, 'L', 'N'),
                            (:UnitLowerTriangular, 'L', 'U'),
                            (:UpperTriangular, 'U', 'N'),
                            (:UnitUpperTriangular, 'U', 'U'))
    @eval begin
        # Vector multiplication
        A_mul_B!{T<:BlasFloat,S<:StridedMatrix}(A::$t{T,S}, b::StridedVector{T}) = BLAS.trmv!($uploc, 'N', $isunitc, A.data, b)

        # Matrix multiplication
        A_mul_B!{T<:BlasFloat,S<:StridedMatrix}(A::$t{T,S}, B::StridedMatrix{T}) = BLAS.trmm!('L', $uploc, 'N', $isunitc, one(T), A.data, B)
        A_mul_B!{T<:BlasFloat,S<:StridedMatrix}(A::StridedMatrix{T}, B::$t{T,S}) = BLAS.trmm!('R', $uploc, 'N', $isunitc, one(T), B.data, A)

        Ac_mul_B!{T<:BlasComplex,S<:StridedMatrix}(A::$t{T,S}, B::StridedMatrix{T}) = BLAS.trmm!('L', $uploc, 'C', $isunitc, one(T), A.data, B)
        Ac_mul_B!{T<:BlasReal,S<:StridedMatrix}(A::$t{T,S}, B::StridedMatrix{T}) = BLAS.trmm!('L', $uploc, 'T', $isunitc, one(T), A.data, B)

        A_mul_Bc!{T<:BlasComplex,S<:StridedMatrix}(A::StridedMatrix{T}, B::$t{T,S}) = BLAS.trmm!('R', $uploc, 'C', $isunitc, one(T), B.data, A)
        A_mul_Bc!{T<:BlasReal,S<:StridedMatrix}(A::StridedMatrix{T}, B::$t{T,S}) = BLAS.trmm!('R', $uploc, 'T', $isunitc, one(T), B.data, A)

        # Left division
        A_ldiv_B!{T<:BlasFloat,S<:StridedMatrix}(A::$t{T,S}, B::StridedVecOrMat{T}) = LAPACK.trtrs!($uploc, 'N', $isunitc, A.data, B)
        Ac_ldiv_B!{T<:BlasReal,S<:StridedMatrix}(A::$t{T,S}, B::StridedVecOrMat{T}) = LAPACK.trtrs!($uploc, 'T', $isunitc, A.data, B)
        Ac_ldiv_B!{T<:BlasComplex,S<:StridedMatrix}(A::$t{T,S}, B::StridedVecOrMat{T}) = LAPACK.trtrs!($uploc, 'C', $isunitc, A.data, B)

        # Right division
        A_rdiv_B!{T<:BlasFloat,S<:StridedMatrix}(A::StridedVecOrMat{T}, B::$t{T,S}) = BLAS.trsm!('R', $uploc, 'N', $isunitc, one(T), B.data, A)
        A_rdiv_Bc!{T<:BlasReal,S<:StridedMatrix}(A::StridedMatrix{T}, B::$t{T,S}) = BLAS.trsm!('R', $uploc, 'T', $isunitc, one(T), B.data, A)
        A_rdiv_Bc!{T<:BlasComplex,S<:StridedMatrix}(A::StridedMatrix{T}, B::$t{T,S}) = BLAS.trsm!('R', $uploc, 'C', $isunitc, one(T), B.data, A)

        # Matrix inverse
        inv!{T<:BlasFloat,S<:StridedMatrix}(A::$t{T,S}) = $t{T,S}(LAPACK.trtri!($uploc, $isunitc, A.data))

        # Error bounds for triangular solve
        errorbounds{T<:BlasFloat,S<:StridedMatrix}(A::$t{T,S}, X::StridedVecOrMat{T}, B::StridedVecOrMat{T}) = LAPACK.trrfs!($uploc, 'N', $isunitc, A.data, B, X)

        # Condition numbers
        function cond{T<:BlasFloat,S}(A::$t{T,S}, p::Real=2)
            chksquare(A)
            if p == 1
                return inv(LAPACK.trcon!('O', $uploc, $isunitc, A.data))
            elseif p == Inf
                return inv(LAPACK.trcon!('I', $uploc, $isunitc, A.data))
            else #use fallback
                return cond(full(A), p)
            end
        end
    end
end

function inv{T}(A::LowerTriangular{T})
    S = typeof((zero(T)*one(T) + zero(T))/one(T))
    LowerTriangular(A_ldiv_B!(convert(AbstractArray{S}, A), eye(S, size(A, 1))))
end
function inv{T}(A::UpperTriangular{T})
    S = typeof((zero(T)*one(T) + zero(T))/one(T))
    UpperTriangular(A_ldiv_B!(convert(AbstractArray{S}, A), eye(S, size(A, 1))))
end
inv{T}(A::UnitUpperTriangular{T}) = UnitUpperTriangular(A_ldiv_B!(A, eye(T, size(A, 1))))
inv{T}(A::UnitLowerTriangular{T}) = UnitLowerTriangular(A_ldiv_B!(A, eye(T, size(A, 1))))

errorbounds{T<:Union(BigFloat, Complex{BigFloat}),S<:StridedMatrix}(A::AbstractTriangular{T,S}, X::StridedVecOrMat{T}, B::StridedVecOrMat{T}) = error("not implemented yet! Please submit a pull request.")
function errorbounds{TA<:Number,S<:StridedMatrix,TX<:Number,TB<:Number}(A::AbstractTriangular{TA,S}, X::StridedVecOrMat{TX}, B::StridedVecOrMat{TB})
    TAXB = promote_type(TA, TB, TX, Float32)
    errorbounds(convert(AbstractMatrix{TAXB}, A), convert(AbstractArray{TAXB}, X), convert(AbstractArray{TAXB}, B))
end

# Eigensystems
## Notice that trecv works for quasi-triangular matrices and therefore the lower sub diagonal must be zeroed before calling the subroutine
eigvecs{T<:BlasFloat,S<:StridedMatrix}(A::UpperTriangular{T,S}) = LAPACK.trevc!('R', 'A', BlasInt[], triu!(A.data))
eigvecs{T<:BlasFloat,S<:StridedMatrix}(A::UnitUpperTriangular{T,S}) = (for i = 1:size(A, 1); A.data[i,i] = 1;end;LAPACK.trevc!('R', 'A', BlasInt[], triu!(A.data)))
eigvecs{T<:BlasFloat,S<:StridedMatrix}(A::LowerTriangular{T,S}) = LAPACK.trevc!('L', 'A', BlasInt[], tril!(A.data)')
eigvecs{T<:BlasFloat,S<:StridedMatrix}(A::UnitLowerTriangular{T,S}) = (for i = 1:size(A, 1); A.data[i,i] = 1;end;LAPACK.trevc!('L', 'A', BlasInt[], tril!(A.data)'))

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
        throw(DimensionMismatch("left and right hand side does not fit"))
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
        throw(DimensionMismatch("left and right hand side does not fit"))
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
        throw(DimensionMismatch("left and right hand side does not fit"))
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
        throw(DimensionMismatch("left and right hand side does not fit"))
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
        throw(DimensionMismatch("left and right hand side does not fit"))
    end
    for j = 1:n
        for i = m:-1:1
            Bij = A.data[i,i]*B[i,j]
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
        throw(DimensionMismatch("left and right hand side does not fit"))
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
        throw(DimensionMismatch("left and right hand side does not fit"))
    end
    for j = 1:n
        for i = 1:m
            Bij = A.data[i,i]*B[i,j]
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
        throw(DimensionMismatch("left and right hand side does not fit"))
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

function A_mul_B!(A::StridedMatrix, B::UpperTriangular)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch("left and right hand side does not fit"))
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
        throw(DimensionMismatch("left and right hand side does not fit"))
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
        throw(DimensionMismatch("left and right hand side does not fit"))
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
        throw(DimensionMismatch("left and right hand side does not fit"))
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
        throw(DimensionMismatch("left and right hand side does not fit"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]*B[j,j]
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
        throw(DimensionMismatch("left and right hand side does not fit"))
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
        throw(DimensionMismatch("left and right hand side does not fit"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]*B[j,j]
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
        throw(DimensionMismatch("left and right hand side does not fit"))
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

#Generic solver using naive substitution
function naivesub!(A::UpperTriangular, b::AbstractVector, x::AbstractVector=b)
    n = size(A, 2)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("Second dimension of A, $n, length of x, $(length(x)), and length of b, $(length(b)) must be equal"))
    end
    for j = n:-1:1
        xj = b[j]
        for k = j+1:1:n
            xj -= A[j,k] * x[k]
        end
        Ajj = A[j,j]
        if Ajj == zero(Ajj)
            throw(SingularException(j))
        else
            x[j] = Ajj\xj
        end
    end
    x
end
function naivesub!(A::UnitUpperTriangular, b::AbstractVector, x::AbstractVector=b)
    n = size(A, 2)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("Second dimension of A, $n, length of x, $(length(x)), and length of b, $(length(b)) must be equal"))
    end
    for j = n:-1:1
        xj = b[j]
        for k = j+1:1:n
            xj -= A[j,k] * x[k]
        end
        x[j] = xj
    end
    x
end
function naivesub!(A::LowerTriangular, b::AbstractVector, x::AbstractVector=b)
    n = size(A, 2)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("Second dimension of A, $n, length of x, $(length(x)), and length of b, $(length(b)) must be equal"))
    end
    for j = 1:n
        xj = b[j]
        for k = 1:j-1
            xj -= A[j,k] * x[k]
        end
        Ajj = A[j,j]
        if Ajj == zero(Ajj)
            throw(SingularException(j))
        else
            x[j] = Ajj\xj
        end
    end
    x
end
function naivesub!(A::UnitLowerTriangular, b::AbstractVector, x::AbstractVector=b)
    n = size(A, 2)
    if !(n == length(b) == length(x))
        throw(DimensionMismatch("Second dimension of A, $n, length of x, $(length(x)), and length of b, $(length(b)) must be equal"))
    end
    for j = 1:n
        xj = b[j]
        for k = 1:j-1
            xj -= A[j,k] * x[k]
        end
        x[j] = xj
    end
    x
end

function A_rdiv_B!(A::StridedMatrix, B::UpperTriangular)
    m, n = size(A)
    if size(A, 1) != n
        throw(DimensionMismatch("left and right hand side does not fit"))
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
    if size(A, 1) != n
        throw(DimensionMismatch("left and right hand side does not fit"))
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
    if size(A, 1) != n
        throw(DimensionMismatch("left and right hand side does not fit"))
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
    if size(A, 1) != n
        throw(DimensionMismatch("left and right hand side does not fit"))
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
    if size(A, 1) != n
        throw(DimensionMismatch("left and right hand side does not fit"))
    end
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = j + 1:n
                Aij -= A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij/B[j,j]
        end
    end
    A
end
function A_rdiv_Bc!(A::StridedMatrix, B::UnitUpperTriangular)
    m, n = size(A)
    if size(A, 1) != n
        throw(DimensionMismatch("left and right hand side does not fit"))
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
    if size(A, 1) != n
        throw(DimensionMismatch("left and right hand side does not fit"))
    end
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = 1:j - 1
                Aij -= A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij/B[j,j]
        end
    end
    A
end
function A_rdiv_Bc!(A::StridedMatrix, B::UnitLowerTriangular)
    m, n = size(A)
    if size(A, 1) != n
        throw(DimensionMismatch("left and right hand side does not fit"))
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

# Promotion
## Promotion methods in matmul don't apply to triangular multiplication since it is inplace. Hence we have to make very similar definitions, but without allocation of a result array. For multiplication and unit diagonal division the element type doesn't have to be stable under division whereas that is necessary in the general triangular solve problem.

## Some Triangular-Triangular cases. We might want to write taylored methods for these cases, but I'm not sure it is worth it.
for t in (UpperTriangular, UnitUpperTriangular, LowerTriangular, UnitLowerTriangular)
    @eval begin
        *(A::Tridiagonal, B::$t) = A_mul_B!(full(A), B)
    end
end

for f in (:*, :Ac_mul_B, :At_mul_B, :A_mul_Bc, :A_mul_Bt, :Ac_mul_Bc, :At_mul_Bt, :\, :Ac_ldiv_B, :At_ldiv_B)
    @eval begin
        ($f)(A::AbstractTriangular, B::AbstractTriangular) = ($f)(A, full(B))
    end
end
for f in (:A_mul_Bc, :A_mul_Bt, :Ac_mul_Bc, :At_mul_Bt, :/, :A_rdiv_Bc, :A_rdiv_Bt)
    @eval begin
        ($f)(A::AbstractTriangular, B::AbstractTriangular) = ($f)(full(A), B)
    end
end

## The general promotion methods
### Multiplication with triangle to the left and hence rhs cannot be transposed.
for (f, g) in ((:*, :A_mul_B!), (:Ac_mul_B, :Ac_mul_B!), (:At_mul_B, :At_mul_B!))
    @eval begin
        function ($f){TA,TB}(A::AbstractTriangular{TA}, B::StridedVecOrMat{TB})
            TAB = typeof(zero(TA)*zero(TB) + zero(TA)*zero(TB))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end
### Left division with triangle to the left hence rhs cannot be transposed. No quotients.
for (f, g) in ((:\, :A_ldiv_B!), (:Ac_ldiv_B, :Ac_ldiv_B!), (:At_ldiv_B, :At_ldiv_B!))
    @eval begin
        function ($f){TA,TB,S}(A::UnitUpperTriangular{TA,S}, B::StridedVecOrMat{TB})
            TAB = typeof(zero(TA)*zero(TB) + zero(TA)*zero(TB))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end
for (f, g) in ((:\, :A_ldiv_B!), (:Ac_ldiv_B, :Ac_ldiv_B!), (:At_ldiv_B, :At_ldiv_B!))
    @eval begin
        function ($f){TA,TB,S}(A::UnitLowerTriangular{TA,S}, B::StridedVecOrMat{TB})
            TAB = typeof(zero(TA)*zero(TB) + zero(TA)*zero(TB))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end
### Left division with triangle to the left hence rhs cannot be transposed. Quotients.
for (f, g) in ((:\, :A_ldiv_B!), (:Ac_ldiv_B, :Ac_ldiv_B!), (:At_ldiv_B, :At_ldiv_B!))
    @eval begin
        function ($f){TA,TB,S}(A::UpperTriangular{TA,S}, B::StridedVecOrMat{TB})
            TAB = typeof((zero(TA)*zero(TB) + zero(TA)*zero(TB))/one(TA))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end
for (f, g) in ((:\, :A_ldiv_B!), (:Ac_ldiv_B, :Ac_ldiv_B!), (:At_ldiv_B, :At_ldiv_B!))
    @eval begin
        function ($f){TA,TB,S}(A::LowerTriangular{TA,S}, B::StridedVecOrMat{TB})
            TAB = typeof((zero(TA)*zero(TB) + zero(TA)*zero(TB))/one(TA))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end
### Multiplication with triangle to the rigth and hence lhs cannot be transposed.
for (f, g) in ((:*, :A_mul_B!), (:A_mul_Bc, :A_mul_Bc!), (:A_mul_Bt, :A_mul_Bt!))
    @eval begin
        function ($f){TA,TB}(A::StridedVecOrMat{TA}, B::AbstractTriangular{TB})
            TAB = typeof(zero(TA)*zero(TB) + zero(TA)*zero(TB))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end
### Right division with triangle to the right hence lhs cannot be transposed. No quotients.
for (f, g) in ((:/, :A_rdiv_B!), (:A_rdiv_Bc, :A_rdiv_Bc!), (:A_rdiv_Bt, :A_rdiv_Bt!))
    @eval begin
        function ($f){TA,TB,S}(A::StridedVecOrMat{TA}, B::UnitUpperTriangular{TB,S})
            TAB = typeof(zero(TA)*zero(TB) + zero(TA)*zero(TB))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end
for (f, g) in ((:/, :A_rdiv_B!), (:A_rdiv_Bc, :A_rdiv_Bc!), (:A_rdiv_Bt, :A_rdiv_Bt!))
    @eval begin
        function ($f){TA,TB,S}(A::StridedVecOrMat{TA}, B::UnitLowerTriangular{TB,S})
            TAB = typeof(zero(TA)*zero(TB) + zero(TA)*zero(TB))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end
### Right division with triangle to the right hence lhs cannot be transposed. Quotients.
for (f, g) in ((:/, :A_rdiv_B!), (:A_rdiv_Bc, :A_rdiv_Bc!), (:A_rdiv_Bt, :A_rdiv_Bt!))
    @eval begin
        function ($f){TA,TB,S}(A::StridedVecOrMat{TA}, B::UpperTriangular{TB,S})
            TAB = typeof((zero(TA)*zero(TB) + zero(TA)*zero(TB))/one(TA))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end
for (f, g) in ((:/, :A_rdiv_B!), (:A_rdiv_Bc, :A_rdiv_Bc!), (:A_rdiv_Bt, :A_rdiv_Bt!))
    @eval begin
        function ($f){TA,TB,S}(A::StridedVecOrMat{TA}, B::LowerTriangular{TB,S})
            TAB = typeof((zero(TA)*zero(TB) + zero(TA)*zero(TB))/one(TA))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end

function sqrtm{T}(A::UpperTriangular{T})
    n = size(A, 1)
    TT = typeof(sqrt(zero(T)))
    R = zeros(TT, n, n)
    for j = 1:n
        R[j,j] = sqrt(A[j,j])
        for i = j-1:-1:1
            r = A[i,j]
            for k = i+1:j-1
                r -= R[i,k]*R[k,j]
            end
            r==0 || (R[i,j] = r / (R[i,i] + R[j,j]))
        end
    end
    return UpperTriangular(R)
end
function sqrtm{T}(A::UnitUpperTriangular{T})
    n = size(A, 1)
    TT = typeof(sqrt(zero(T)))
    R = zeros(TT, n, n)
    for j = 1:n
        R[j,j] = one(T)
        for i = j-1:-1:1
            r = A[i,j]
            for k = i+1:j-1
                r -= R[i,k]*R[k,j]
            end
            r==0 || (R[i,j] = r / (R[i,i] + R[j,j]))
        end
    end
    return UnitUpperTriangular(R)
end
sqrtm(A::LowerTriangular) = sqrtm(A.').'
sqrtm(A::UnitLowerTriangular) = sqrtm(A.').'

#Generic eigensystems
eigvals(A::AbstractTriangular) = diag(A)
function eigvecs{T}(A::AbstractTriangular{T})
    TT = promote_type(T, Float32)
    if TT <: BlasFloat
        return eigvecs(convert(AbstractMatrix{TT}, A))
    else
        throw(ArgumentError("eigvecs type $(typeof(A)) not supported. Please submit a pull request."))
    end
end
det{T}(A::UnitUpperTriangular{T}) = one(T)*one(T)
det{T}(A::UnitLowerTriangular{T}) = one(T)*one(T)
det{T}(A::UpperTriangular{T}) = prod(diag(A.data))
det{T}(A::LowerTriangular{T}) = prod(diag(A.data))

eigfact(A::AbstractTriangular) = Eigen(eigvals(A), eigvecs(A))

#Generic singular systems
for func in (:svd, :svdfact, :svdfact!, :svdvals)
    @eval begin
        ($func)(A::AbstractTriangular) = ($func)(full(A))
    end
end

factorize(A::AbstractTriangular) = A
