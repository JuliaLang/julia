## Triangular
immutable Triangular{T,S<:AbstractMatrix,UpLo,IsUnit} <: AbstractMatrix{T}
    data::S
end
function Triangular{T}(A::AbstractMatrix{T}, uplo::Symbol, isunit::Bool=false)
    chksquare(A)
    Triangular{T,typeof(A),uplo,isunit}(A)
end

size(A::Triangular, args...) = size(A.data, args...)

convert{T,S,UpLo,IsUnit}(::Type{Triangular{T}}, A::Triangular{T,S,UpLo,IsUnit}) = A
convert{Tnew,Told,S,UpLo,IsUnit}(::Type{Triangular{Tnew}}, A::Triangular{Told,S,UpLo,IsUnit}) = (Anew = convert(AbstractMatrix{Tnew}, A.data); Triangular(Anew, UpLo, IsUnit))
convert{Tnew,Told,S,UpLo,IsUnit}(::Type{AbstractMatrix{Tnew}}, A::Triangular{Told,S,UpLo,IsUnit}) = convert(Triangular{Tnew}, A)
function convert{Tret,T,S,UpLo,IsUnit}(::Type{Matrix{Tret}}, A::Triangular{T,S,UpLo,IsUnit})
    B = Array(Tret, size(A, 1), size(A, 1))
    copy!(B, A.data)
    (UpLo == :L ? tril! : triu!)(B)
    if IsUnit
        for i = 1:size(B,1)
            B[i,i] = 1
        end
    end
    B
end
convert{T,S,UpLo,IsUnit}(::Type{Matrix}, A::Triangular{T,S,UpLo,IsUnit}) = convert(Matrix{T}, A)

function full!{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit})
    B = A.data
    (UpLo == :L ? tril! : triu!)(B)
    if IsUnit
        for i = 1:size(A,1)
            B[i,i] = 1
        end
    end
    B
end
full{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = convert(Matrix, A)

fill!(A::Triangular, x) = (fill!(A.data, x); A)

function similar{T,S,UpLo,IsUnit,Tnew}(A::Triangular{T,S,UpLo,IsUnit}, ::Type{Tnew}, dims::Dims)
    dims[1] == dims[2] || throw(ArgumentError("a Triangular matrix must be square"))
    length(dims) == 2 || throw(ArgumentError("a Triangular matrix must have two dimensions"))
    A = similar(A.data, Tnew, dims)
    return Triangular{Tnew, typeof(A), UpLo, IsUnit}(A)
end

copy{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = Triangular{T,S,UpLo,IsUnit}(copy(A.data))

getindex(A::Triangular, i::Integer) = ((m, n) = divrem(i - 1, size(A, 1)); A[n + 1, m + 1])
getindex{T,S}(A::Triangular{T,S,:L,true}, i::Integer, j::Integer) = i == j ? one(T) : (i > j ? A.data[i,j] : zero(A.data[i,j]))
getindex{T,S}(A::Triangular{T,S,:L,false}, i::Integer, j::Integer) = i >= j ? A.data[i,j] : zero(A.data[i,j])
getindex{T,S}(A::Triangular{T,S,:U,true}, i::Integer, j::Integer) = i == j ? one(T) : (i < j ? A.data[i,j] : zero(A.data[i,j]))
getindex{T,S}(A::Triangular{T,S,:U,false}, i::Integer, j::Integer) = i <= j ? A.data[i,j] : zero(A.data[i,j])

istril{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = UpLo == :L
istriu{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = UpLo == :U

transpose{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = Triangular{T, S, UpLo == :U ? :L : :U, IsUnit}(transpose(A.data))
ctranspose{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = Triangular{T, S, UpLo == :U ? :L : :U, IsUnit}(ctranspose(A.data))
transpose!{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = Triangular{T, S, UpLo == :U ? :L : :U, IsUnit}(copytri!(A.data, UpLo == :L ? 'L' : 'U'))
ctranspose!{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = Triangular{T, S, UpLo == :U ? :L : :U, IsUnit}(copytri!(A.data, UpLo == :L ? 'L' : 'U', true))
diag{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = IsUnit ? ones(T, size(A,1)) : diag(A.data)
function big{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit})
    M = big(A.data)
    Triangular{eltype(M),typeof(M),UpLo,IsUnit}(M)
end

real{T<:Real}(A::Triangular{T}) = A
real{T<:Complex,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = (B = real(A.data); Triangular{eltype(B), typeof(B), UpLo, IsUnit}(B))

# Unary operations
-{T, S, UpLo}(A::Triangular{T, S, UpLo, false}) = Triangular{T, S, UpLo, false}(-A.data)
function -{T, S, UpLo}(A::Triangular{T, S, UpLo, true})
    Anew = -A.data
    for i = 1:size(A, 1)
        Anew[i, i] = -1
    end
    Triangular{T, S, UpLo, false}(Anew)
end

# Binary operations
+{TA, TB, SA, SB, uplo}(A::Triangular{TA, SA, uplo, false}, B::Triangular{TB, SB, uplo, false}) = Triangular(A.data + B.data, uplo)
+{TA, SA, TB, SB}(A::Triangular{TA, SA, :U, false}, B::Triangular{TB, SB, :U, true}) = Triangular(A.data + triu(B.data, 1) + I, :U)
+{TA, SA, TB, SB}(A::Triangular{TA, SA, :L, false}, B::Triangular{TB, SB, :L, true}) = Triangular(A.data + tril(B.data, -1) + I, :L)
+{TA, SA, TB, SB}(A::Triangular{TA, SA, :U, true}, B::Triangular{TB, SB, :U, false}) = Triangular(triu(A.data, 1) + B.data + I, :U)
+{TA, SA, TB, SB}(A::Triangular{TA, SA, :L, true}, B::Triangular{TB, SB, :L, false}) = Triangular(tril(A.data, -1) + B.data + I, :L)
+{TA, SA, TB, SB}(A::Triangular{TA, SA, :U, true}, B::Triangular{TB, SB, :U, true}) = Triangular(triu(A.data, 1) + triu(B.data, 1) + 2I, :U)
+{TA, SA, TB, SB}(A::Triangular{TA, SA, :L, true}, B::Triangular{TB, SB, :L, true}) = Triangular(tril(A.data, -1) + tril(B.data, -1) + 2I, :L)
+{TA, SA, TB, SB, uplo1, uplo2, IsUnit1, IsUnit2}(A::Triangular{TA, SA, uplo1, IsUnit1}, B::Triangular{TB, SB, uplo2, IsUnit2}) = full(A) + full(B)
-{TA, SA, TB, SB, uplo}(A::Triangular{TA, SA, uplo, false}, B::Triangular{TB, SB, uplo, false}) = Triangular(A.data - B.data, uplo)
-{TA, SA, TB, SB}(A::Triangular{TA, SA, :U, false}, B::Triangular{TB, SB, :U, true}) = Triangular(A.data - triu(B.data, 1) - I, :U)
-{TA, SA, TB, SB}(A::Triangular{TA, SA, :L, false}, B::Triangular{TB, SB, :L, true}) = Triangular(A.data - tril(B.data, -1) - I, :L)
-{TA, SA, TB, SB}(A::Triangular{TA, SA, :U, true}, B::Triangular{TB, SB, :U, false}) = Triangular(triu(A.data, 1) - B.data + I, :U)
-{TA, SA, TB, SB}(A::Triangular{TA, SA, :L, true}, B::Triangular{TB, SB, :L, false}) = Triangular(tril(A.data, -1) - B.data + I, :L)
-{TA, SA, TB, SB}(A::Triangular{TA, SA, :U, true}, B::Triangular{TB, SB, :U, true}) = Triangular(triu(A.data, 1) - triu(B.data, 1), :U)
-{TA, SA, TB, SB}(A::Triangular{TA, SA, :L, true}, B::Triangular{TB, SB, :L, true}) = Triangular(tril(A.data, -1) - tril(B.data, -1), :L)
-{TA, SA, TB, SB, uplo1, uplo2, IsUnit1, IsUnit2}(A::Triangular{TA, SA, uplo1, IsUnit1}, B::Triangular{TB, SB, uplo2, IsUnit2}) = full(A) - full(B)

######################
# BlasFloat routines #
######################

for (uplos, uploc) in ((:(:U), 'U'), (:(:L), 'L'))
    for (isunitb, isunitc) in ((true, 'U'), (false, 'N'))
        @eval begin
        # Vector multiplication
        A_mul_B!{T<:BlasFloat,S<:StridedMatrix}(A::Triangular{T,S,$uplos,$isunitb}, b::StridedVector{T}) = BLAS.trmv!($uploc, 'N', $isunitc, A.data, b)

        # Matrix multiplication
        A_mul_B!{T<:BlasFloat,S<:StridedMatrix}(A::Triangular{T,S,$uplos,$isunitb}, B::StridedMatrix{T}) = BLAS.trmm!('L', $uploc, 'N', $isunitc, one(T), A.data, B)
        A_mul_B!{T<:BlasFloat,S<:StridedMatrix}(A::StridedMatrix{T}, B::Triangular{T,S,$uplos,$isunitb}) = BLAS.trmm!('R', $uploc, 'N', $isunitc, one(T), B.data, A)

        Ac_mul_B!{T<:BlasComplex,S<:StridedMatrix}(A::Triangular{T,S,$uplos,$isunitb}, B::StridedMatrix{T}) = BLAS.trmm!('L', $uploc, 'C', $isunitc, one(T), A.data, B)
        Ac_mul_B!{T<:BlasReal,S<:StridedMatrix}(A::Triangular{T,S,$uplos,$isunitb}, B::StridedMatrix{T}) = BLAS.trmm!('L', $uploc, 'T', $isunitc, one(T), A.data, B)

        A_mul_Bc!{T<:BlasComplex,S<:StridedMatrix}(A::StridedMatrix{T}, B::Triangular{T,S,$uplos,$isunitb}) = BLAS.trmm!('R', $uploc, 'C', $isunitc, one(T), B.data, A)
        A_mul_Bc!{T<:BlasReal,S<:StridedMatrix}(A::StridedMatrix{T}, B::Triangular{T,S,$uplos,$isunitb}) = BLAS.trmm!('R', $uploc, 'T', $isunitc, one(T), B.data, A)

        # Left division
        A_ldiv_B!{T<:BlasFloat,S<:StridedMatrix}(A::Triangular{T,S,$uplos,$isunitb}, B::StridedVecOrMat{T}) = LAPACK.trtrs!($uploc, 'N', $isunitc, A.data, B)
        Ac_ldiv_B!{T<:BlasReal,S<:StridedMatrix}(A::Triangular{T,S,$uplos,$isunitb}, B::StridedVecOrMat{T}) = LAPACK.trtrs!($uploc, 'T', $isunitc, A.data, B)
        Ac_ldiv_B!{T<:BlasComplex,S<:StridedMatrix}(A::Triangular{T,S,$uplos,$isunitb}, B::StridedVecOrMat{T}) = LAPACK.trtrs!($uploc, 'C', $isunitc, A.data, B)

        # Right division
        A_rdiv_B!{T<:BlasFloat,S<:StridedMatrix}(A::StridedVecOrMat{T}, B::Triangular{T,S,$uplos,$isunitb}) = BLAS.trsm!('R', $uploc, 'N', $isunitc, one(T), B.data, A)
        A_rdiv_Bc!{T<:BlasReal,S<:StridedMatrix}(A::StridedMatrix{T}, B::Triangular{T,S,$uplos,$isunitb}) = BLAS.trsm!('R', $uploc, 'T', $isunitc, one(T), B.data, A)
        A_rdiv_Bc!{T<:BlasComplex,S<:StridedMatrix}(A::StridedMatrix{T}, B::Triangular{T,S,$uplos,$isunitb}) = BLAS.trsm!('R', $uploc, 'C', $isunitc, one(T), B.data, A)

        # Matrix inverse
        inv{T<:BlasFloat,S<:StridedMatrix}(A::Triangular{T,S,$uplos,$isunitb}) = Triangular{T,S,$uplos,$isunitb}(LAPACK.trtri!($uploc, $isunitc, copy(A.data)))

        # Error bounds for triangular solve
        errorbounds{T<:BlasFloat,S<:StridedMatrix}(A::Triangular{T,S,$uplos,$isunitb}, X::StridedVecOrMat{T}, B::StridedVecOrMat{T}) = LAPACK.trrfs!($uploc, 'N', $isunitc, A.data, B, X)

        # Condition numbers
        function cond{T<:BlasFloat,S}(A::Triangular{T,S,$uplos,$isunitb}, p::Real=2)
            chksquare(A)
            if p==1
                return inv(LAPACK.trcon!('O', $uploc, $isunitc, A.data))
            elseif p==Inf
                return inv(LAPACK.trcon!('I', $uploc, $isunitc, A.data))
            else #use fallback
                return cond(full(A), p)
            end
        end
    end
    end
end

errorbounds{T<:Union(BigFloat, Complex{BigFloat}),S<:StridedMatrix}(A::Triangular{T,S}, X::StridedVecOrMat{T}, B::StridedVecOrMat{T}) = error("not implemented yet! Please submit a pull request.")
function errorbounds{TA<:Number,S<:StridedMatrix,UpLo,IsUnit,TX<:Number,TB<:Number}(A::Triangular{TA,S,UpLo,IsUnit}, X::StridedVecOrMat{TX}, B::StridedVecOrMat{TB})
    TAXB = promote_type(TA, TB, TX, Float32)
    errorbounds(convert(AbstractMatrix{TAXB}, A), convert(AbstractArray{TAXB}, X), convert(AbstractArray{TAXB}, B))
end

# Eigensystems
## Notice that trecv works for quasi-triangular matrices and therefore the lower sub diagonal must be zeroed before calling the subroutine
eigvecs{T<:BlasFloat,S<:StridedMatrix}(A::Triangular{T,S,:U,false}) = LAPACK.trevc!('R', 'A', BlasInt[], triu!(A.data))
eigvecs{T<:BlasFloat,S<:StridedMatrix}(A::Triangular{T,S,:U,true}) = (for i = 1:size(A, 1); A.data[i,i] = 1;end;LAPACK.trevc!('R', 'A', BlasInt[], triu!(A.data)))
eigvecs{T<:BlasFloat,S<:StridedMatrix}(A::Triangular{T,S,:L,false}) = LAPACK.trevc!('L', 'A', BlasInt[], tril!(A.data)')
eigvecs{T<:BlasFloat,S<:StridedMatrix}(A::Triangular{T,S,:L,true}) = (for i = 1:size(A, 1); A.data[i,i] = 1;end;LAPACK.trevc!('L', 'A', BlasInt[], tril!(A.data)'))

####################
# Generic routines #
####################

(*){T,S,UpLo}(A::Triangular{T,S,UpLo,false}, x::Number) = Triangular(A.data*x, UpLo)
function (*){T,S,UpLo}(A::Triangular{T,S,UpLo,true}, x::Number)
    B = A.data*x
    for i = 1:size(A, 1)
        B[i,i] = x
    end
    Triangular(B, UpLo)
end
(*){T,S,UpLo}(x::Number, A::Triangular{T,S,UpLo,false}) = Triangular(x*A.data, UpLo)
function (*){T,S,UpLo}(x::Number, A::Triangular{T,S,UpLo,true})
    B = x*A.data
    for i = 1:size(A, 1)
        B[i,i] = x
    end
    Triangular(B, UpLo)
end
(/){T,S,UpLo}(A::Triangular{T,S,UpLo,false}, x::Number) = Triangular(A.data/x, UpLo)
function (/){T,S,UpLo}(A::Triangular{T,S,UpLo,true}, x::Number)
    B = A.data*x
    invx = inv(x)
    for i = 1:size(A, 1)
        B[i,i] = x
    end
    Triangular(B, UpLo)
end
(\){T,S,UpLo}(x::Number, A::Triangular{T,S,UpLo,false}) = Triangular(x\A.data, UpLo)
function (\){T,S,UpLo}(x::Number, A::Triangular{T,S,UpLo,true})
    B = x\A.data
    invx = inv(x)
    for i = 1:size(A, 1)
        B[i,i] = invx
    end
    Triangular(B, UpLo)
end

## Generic triangular multiplication
function A_mul_B!{T,S<:AbstractMatrix,IsUnit}(A::Triangular{T,S,:U,IsUnit}, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    m == size(A, 1) || throw(DimensionMismatch("left and right hand side doesn't fit"))
    for j = 1:n
        for i = 1:m
            Bij = ifelse(IsUnit, B[i,j], A.data[i,i]*B[i,j])
            for k = i + 1:m
                Bij += A.data[i,k]*B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function A_mul_B!{T,S<:AbstractMatrix,IsUnit}(A::Triangular{T,S,:L,IsUnit}, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    m == size(A, 1) || throw(DimensionMismatch("left and right hand side doesn't fit"))
    for j = 1:n
        for i = m:-1:1
            Bij = ifelse(IsUnit, B[i,j], A.data[i,i]*B[i,j])
            for k = 1:i - 1
                Bij += A.data[i,k]*B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function Ac_mul_B!{T,S<:AbstractMatrix,IsUnit}(A::Triangular{T,S,:U,IsUnit}, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    m == size(A, 1) || throw(DimensionMismatch("left and right hand side doesn't fit"))
    for j = 1:n
        for i = m:-1:1
            Bij = ifelse(IsUnit, B[i,j], A.data[i,i]*B[i,j])
            for k = 1:i - 1
                Bij += A.data[k,i]'B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function Ac_mul_B!{T,S<:AbstractMatrix,IsUnit}(A::Triangular{T,S,:L,IsUnit}, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    m == size(A, 1) || throw(DimensionMismatch("left and right hand side doesn't fit"))
    for j = 1:n
        for i = 1:m
            Bij = ifelse(IsUnit, B[i,j], A.data[i,i]*B[i,j])
            for k = i + 1:m
                Bij += A.data[k,i]'B[k,j]
            end
            B[i,j] = Bij
        end
    end
    B
end

function A_mul_B!{T,S<:AbstractMatrix,IsUnit}(A::StridedMatrix, B::Triangular{T,S,:U,IsUnit})
    m, n = size(A)
    size(B, 1) == n || throw(DimensionMismatch("left and right hand side doesn't fit"))
    for i = 1:m
        for j = n:-1:1
            Aij = ifelse(IsUnit, A[i,j], A[i,j]*B[j,j])
            for k = 1:j - 1
                Aij += A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij
        end
    end
    A
end

function A_mul_B!{T,S<:AbstractMatrix,IsUnit}(A::StridedMatrix, B::Triangular{T,S,:L,IsUnit})
    m, n = size(A)
    size(B, 1) == n || throw(DimensionMismatch("left and right hand side doesn't fit"))
    for i = 1:m
        for j = 1:n
            Aij = ifelse(IsUnit, A[i,j], A[i,j]*B[j,j])
            for k = j + 1:n
                Aij += A[i,k]*B.data[k,j]
            end
            A[i,j] = Aij
        end
    end
    A
end

function A_mul_Bc!{T,S<:AbstractMatrix,IsUnit}(A::StridedMatrix, B::Triangular{T,S,:U,IsUnit})
    m, n = size(A)
    size(B, 1) == n || throw(DimensionMismatch("left and right hand side doesn't fit"))
    for i = 1:m
        for j = 1:n
            Aij = ifelse(IsUnit, A[i,j], A[i,j]*B[j,j])
            for k = j + 1:n
                Aij += A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij
        end
    end
    A
end

function A_mul_Bc!{T,S<:AbstractMatrix,IsUnit}(A::StridedMatrix, B::Triangular{T,S,:L,IsUnit})
    m, n = size(A)
    size(B, 1) == n || throw(DimensionMismatch("left and right hand side doesn't fit"))
    for i = 1:m
        for j = n:-1:1
            Aij = ifelse(IsUnit, A[i,j], A[i,j]*B[j,j])
            for k = 1:j - 1
                Aij += A[i,k]*B.data[j,k]'
            end
            A[i,j] = Aij
        end
    end
    A
end

A_mul_B!(A::Tridiagonal, B::Triangular) = A*full!(B)
A_mul_B!(C::AbstractVecOrMat, A::Triangular, B::AbstractVecOrMat) = A_mul_B!(A, copy!(C, B))
A_mul_Bc!(C::AbstractVecOrMat, A::Triangular, B::AbstractVecOrMat) = A_mul_Bc!(A, copy!(C, B))

#Generic solver using naive substitution
function naivesub!{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, b::AbstractVector, x::AbstractVector=b)
    N = size(A, 2)
    N==length(b)==length(x) || throw(DimensionMismatch())

    if UpLo == :L #do forward substitution
        for j = 1:N
            x[j] = b[j]
            for k = 1:j-1
                x[j] -= A[j,k] * x[k]
            end
            if !IsUnit
                x[j] = A[j,j]==0 ? throw(SingularException(j)) : A[j,j]\x[j]
            end
        end
    elseif UpLo == :U #do backward substitution
        for j = N:-1:1
            x[j] = b[j]
            for k = j+1:1:N
                x[j] -= A[j,k] * x[k]
            end
            if !IsUnit
                x[j] = A[j,j]==0 ? throw(SingularException(j)) : A[j,j]\x[j]
            end
        end
    else
        throw(ArgumentError("Unknown UpLo=$(UpLo)"))
    end
    x
end

function A_rdiv_B!{T,S,IsUnit}(A::StridedMatrix, B::Triangular{T,S,:U,IsUnit})
    m, n = size(A)
    size(A, 1) == n || throw(DimensionMismatch("left and right hand side doesn't fit"))
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = 1:j - 1
                Aij -= A[i,k]*B.data[k,j]
            end
            A[i,j] = ifelse(IsUnit, Aij, Aij/B[j,j])
        end
    end
    A
end

function A_rdiv_B!{T,S,IsUnit}(A::StridedMatrix, B::Triangular{T,S,:L,IsUnit})
    m, n = size(A)
    size(A, 1) == n || throw(DimensionMismatch("left and right hand side doesn't fit"))
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = j + 1:n
                Aij -= A[i,k]*B.data[k,j]
            end
            A[i,j] = ifelse(IsUnit, Aij, Aij/B[j,j])
        end
    end
    A
end

function A_rdiv_Bc!{T,S,IsUnit}(A::StridedMatrix, B::Triangular{T,S,:U,IsUnit})
    m, n = size(A)
    size(A, 1) == n || throw(DimensionMismatch("left and right hand side doesn't fit"))
    for i = 1:m
        for j = n:-1:1
            Aij = A[i,j]
            for k = j + 1:n
                Aij -= A[i,k]*B.data[j,k]'
            end
            A[i,j] = ifelse(IsUnit, Aij, Aij/B[j,j])
        end
    end
    A
end

function A_rdiv_Bc!{T,S,IsUnit}(A::StridedMatrix, B::Triangular{T,S,:L,IsUnit})
    m, n = size(A)
    size(A, 1) == n || throw(DimensionMismatch("left and right hand side doesn't fit"))
    for i = 1:m
        for j = 1:n
            Aij = A[i,j]
            for k = 1:j - 1
                Aij -= A[i,k]*B.data[j,k]'
            end
            A[i,j] = ifelse(IsUnit, Aij, Aij/B[j,j])
        end
    end
    A
end

# Promotion
## Promotion methods in matmul doesn't apply to triangular multiplication since it is inplace. Hence we have to make very similar definitions, but without allocation of a result array. For multiplication and unit diagonal division the element type doesn't have to be stable under division whereas that is necessary in the general triangular solve problem.

## Some Triangular-Triangular cases. We might want to write taylored methods for these cases, but I'm not sure it is worth it.
*(A::Tridiagonal, B::Triangular) = A_mul_B!(full(A), B)
for f in (:*, :Ac_mul_B, :At_mul_B, :A_mul_Bc, :A_mul_Bt, :Ac_mul_Bc, :At_mul_Bt, :\, :Ac_ldiv_B, :At_ldiv_B)
    @eval begin
        ($f)(A::Triangular, B::Triangular) = ($f)(A, full(B))
    end
end
for f in (:A_mul_Bc, :A_mul_Bt, :Ac_mul_Bc, :At_mul_Bt, :/, :A_rdiv_Bc, :A_rdiv_Bt)
    @eval begin
        ($f)(A::Triangular, B::Triangular) = ($f)(full(A), B)
    end
end

## The general promotion methods
### Multiplication with triangle to the left and hence rhs cannot be transposed.
for (f, g) in ((:*, :A_mul_B!), (:Ac_mul_B, :Ac_mul_B!), (:At_mul_B, :At_mul_B!))
    @eval begin
        function ($f){TA,TB}(A::Triangular{TA}, B::StridedVecOrMat{TB})
            TAB = typeof(zero(TA)*zero(TB) + zero(TA)*zero(TB))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end
### Left division with triangle to the left hence rhs cannot be transposed. No quotients.
for (f, g) in ((:\, :A_ldiv_B!), (:Ac_ldiv_B, :Ac_ldiv_B!), (:At_ldiv_B, :At_ldiv_B!))
    @eval begin
        function ($f){TA,TB,S,UpLo}(A::Triangular{TA,S,UpLo,true}, B::StridedVecOrMat{TB})
            TAB = typeof(zero(TA)*zero(TB) + zero(TA)*zero(TB))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end
### Left division with triangle to the left hence rhs cannot be transposed. Quotients.
for (f, g) in ((:\, :A_ldiv_B!), (:Ac_ldiv_B, :Ac_ldiv_B!), (:At_ldiv_B, :At_ldiv_B!))
    @eval begin
        function ($f){TA,TB,S,UpLo}(A::Triangular{TA,S,UpLo,false}, B::StridedVecOrMat{TB})
            TAB = typeof((zero(TA)*zero(TB) + zero(TA)*zero(TB))/one(TA))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end
### Multiplication with triangle to the rigth and hence lhs cannot be transposed.
for (f, g) in ((:*, :A_mul_B!), (:A_mul_Bc, :A_mul_Bc!), (:A_mul_Bt, :A_mul_Bt!))
    @eval begin
        function ($f){TA,TB}(A::StridedVecOrMat{TA}, B::Triangular{TB})
            TAB = typeof(zero(TA)*zero(TB) + zero(TA)*zero(TB))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end
### Right division with triangle to the right hence lhs cannot be transposed. No quotients.
for (f, g) in ((:/, :A_rdiv_B!), (:A_rdiv_Bc, :A_rdiv_Bc!), (:A_rdiv_Bt, :A_rdiv_Bt!))
    @eval begin
        function ($f){TA,TB,S,UpLo}(A::StridedVecOrMat{TA}, B::Triangular{TB,S,UpLo,true})
            TAB = typeof(zero(TA)*zero(TB) + zero(TA)*zero(TB))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end
### Right division with triangle to the right hence lhs cannot be transposed. Quotients.
for (f, g) in ((:/, :A_rdiv_B!), (:A_rdiv_Bc, :A_rdiv_Bc!), (:A_rdiv_Bt, :A_rdiv_Bt!))
    @eval begin
        function ($f){TA,TB,S,UpLo}(A::StridedVecOrMat{TA}, B::Triangular{TB,S,UpLo,false})
            TAB = typeof((zero(TA)*zero(TB) + zero(TA)*zero(TB))/one(TA))
            ($g)(TA == TAB ? copy(A) : convert(AbstractArray{TAB}, A), TB == TAB ? copy(B) : convert(AbstractArray{TAB}, B))
        end
    end
end

function sqrtm{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit})
    n = size(A, 1)
    TT = typeof(sqrt(zero(T)))
    R = zeros(TT, n, n)
    if UpLo == :U
        for j = 1:n
            (T<:Complex || A[j,j]>=0) ? (R[j,j] = IsUnit ? one(T) : sqrt(A[j,j])) : throw(SingularException(j))
            for i = j-1:-1:1
                r = A[i,j]
                for k = i+1:j-1
                    r -= R[i,k]*R[k,j]
                end
                r==0 || (R[i,j] = r / (R[i,i] + R[j,j]))
            end
        end
        return Triangular(R, :U, IsUnit)
    else # UpLo == :L #Not the usual case
        return sqrtm(A.').'
    end
end

#Generic eigensystems
eigvals(A::Triangular) = diag(A)
function eigvecs{T}(A::Triangular{T})
    TT = promote_type(T, Float32)
    TT <: BlasFloat && return eigvecs(convert(AbstractMatrix{TT}, A))
    error("type not handled yet. Please submit a pull request.")
end
det{T,S,UpLo}(A::Triangular{T,S,UpLo,true}) = one(T)*one(T)
det{T,S,UpLo}(A::Triangular{T,S,UpLo,false}) = prod(diag(A.data))

eigfact(A::Triangular) = Eigen(eigvals(A), eigvecs(A))

#Generic singular systems
for func in (:svd, :svdfact, :svdfact!, :svdvals)
    @eval begin
        ($func)(A::Triangular) = ($func)(full(A))
    end
end

factorize(A::Triangular) = A

