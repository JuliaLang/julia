## Triangular
immutable Triangular{T,S<:AbstractMatrix{T},UpLo,IsUnit} <: AbstractMatrix{T}
    data::S
end
function Triangular{T}(A::AbstractMatrix{T}, uplo::Symbol, isunit::Bool=false)
    uplo != :L && uplo != :U && throw(ArgumentError("uplo argument must be either :U or :L"))
    return Triangular{T,typeof(A),uplo,isunit}(A)
end

+{T, MT, uplo}(A::Triangular{T, MT, uplo}, B::Triangular{T, MT, uplo}) = Triangular(A.data + B.data, uplo)
+{T, MT, uplo1, uplo2}(A::Triangular{T, MT, uplo1}, B::Triangular{T, MT, uplo2}) = full(A) + full(B)
-{T, MT, uplo}(A::Triangular{T, MT, uplo}, B::Triangular{T, MT, uplo}) = Triangular(A.data - B.data, uplo)
-{T, MT, uplo1, uplo2}(A::Triangular{T, MT, uplo1}, B::Triangular{T, MT, uplo2}) = full(A) - full(B)

######################
# BlasFloat routines #
######################

### Note! the BlasFloat restriction can be removed if generic triangular multiplication methods are written.
for (func1, func2) in ((:*, :A_mul_B!), (:Ac_mul_B, :Ac_mul_B!), (:/, :A_rdiv_B!))
    @eval begin
        ($func1){T<:BlasFloat,S<:StridedMatrix,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, B::Triangular{T,S,UpLo,IsUnit}) = ($func2)(A, full(B))
        ($func1){T<:BlasFloat,S<:StridedMatrix,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, B::StridedVecOrMat) = ($func2)(A, copy(B))
    end
end
for (func1, func2) in ((:A_mul_Bc, :A_mul_Bc!), (:A_rdiv_Bc, :A_rdiv_Bc!))
    @eval begin
        ($func1){T<:BlasFloat,S<:StridedMatrix,UpLo,IsUnit}(A::StridedMatrix, B::Triangular{T,S,UpLo,IsUnit}) = ($func2)(copy(A), B)
    end
end

# Vector multiplication
A_mul_B!{T<:BlasFloat,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, b::StridedVector{T}) = BLAS.trmv!(UpLo == :L ? 'L' : 'U', 'N', IsUnit ? 'U' : 'N', A.data, b)
Ac_mul_B!{T<:BlasReal,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, b::StridedVector{T}) = BLAS.trmv!(UpLo == :L ? 'L' : 'U', 'T', IsUnit ? 'U' : 'N', A.data, b)
Ac_mul_B!{T<:BlasComplex,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, b::StridedVector{T}) = BLAS.trmv!(UpLo == :L ? 'L' : 'U', 'C', IsUnit ? 'U' : 'N', A.data, b)
At_mul_B!{T<:BlasFloat,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, b::StridedVector{T}) = BLAS.trmv!(UpLo == :L ? 'L' : 'U', 'T', IsUnit ? 'U' : 'N', A.data, b)

# Matrix multiplication
A_mul_B!{T<:BlasFloat,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, B::StridedMatrix{T}) = BLAS.trmm!('L', UpLo == :L ? 'L' : 'U', 'N', IsUnit ? 'U' : 'N', one(T), A.data, B)
A_mul_B!{T<:BlasFloat,S,UpLo,IsUnit}(A::StridedMatrix{T}, B::Triangular{T,S,UpLo,IsUnit}) = BLAS.trmm!('R', UpLo == :L ? 'L' : 'U', 'N', IsUnit ? 'U' : 'N', one(T), B.data, A)
Ac_mul_B!{T<:BlasComplex,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, B::StridedMatrix{T}) = BLAS.trmm!('L', UpLo == :L ? 'L' : 'U', 'C', IsUnit ? 'U' : 'N', one(T), A.data, B)
Ac_mul_B!{T<:BlasReal,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, B::StridedMatrix{T}) = BLAS.trmm!('L', UpLo == :L ? 'L' : 'U', 'T', IsUnit ? 'U' : 'N', one(T), A.data, B)
A_mul_Bc!{T<:BlasComplex,S,UpLo,IsUnit}(A::StridedMatrix{T}, B::Triangular{T,S,UpLo,IsUnit}) = BLAS.trmm!('R', UpLo == :L ? 'L' : 'U', 'C', IsUnit ? 'U' : 'N', one(T), B.data, A)
A_mul_Bc!{T<:BlasReal,S,UpLo,IsUnit}(A::StridedMatrix{T}, B::Triangular{T,S,UpLo,IsUnit}) = BLAS.trmm!('R', UpLo == :L ? 'L' : 'U', 'T', IsUnit ? 'U' : 'N', one(T), B.data, A)

A_ldiv_B!{T<:BlasFloat,S<:AbstractMatrix,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, B::StridedVecOrMat{T}) = LAPACK.trtrs!(UpLo == :L ? 'L' : 'U', 'N', IsUnit ? 'U' : 'N', A.data, B)
function \{T<:BlasFloat,S<:AbstractMatrix,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, B::StridedVecOrMat{T})
    x = A_ldiv_B!(A, copy(B))
    errors = LAPACK.trrfs!(UpLo == :L ? 'L' : 'U', 'N', IsUnit ? 'U' : 'N', A.data, B, x)
    all(isfinite, vcat(errors...)) || all(vcat(errors...) .< one(T)/eps(T)) || warn("""Unreasonably large error in computed solution:
forward errors:
$(errors[1])
backward errors:
$(errors[2])""")
    x
end
Ac_ldiv_B!{T<:BlasReal,S<:AbstractMatrix,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, B::StridedVecOrMat{T}) = LAPACK.trtrs!(UpLo == :L ? 'L' : 'U', 'T', IsUnit ? 'U' : 'N', A.data, B)
Ac_ldiv_B!{T<:BlasComplex,S<:AbstractMatrix,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, B::StridedVecOrMat{T}) = LAPACK.trtrs!(UpLo == :L ? 'L' : 'U', 'C', IsUnit ? 'U' : 'N', A.data, B)

A_rdiv_B!{T<:BlasFloat,S<:AbstractMatrix,UpLo,IsUnit}(A::StridedVecOrMat{T}, B::Triangular{T,S,UpLo,IsUnit}) = BLAS.trsm!('R', UpLo == :L ? 'L' : 'U', 'N', IsUnit ? 'U' : 'N', one(T), B.data, A)
A_rdiv_Bc!{T<:BlasReal,S<:AbstractMatrix,UpLo,IsUnit}(A::StridedMatrix{T}, B::Triangular{T,S,UpLo,IsUnit}) = BLAS.trsm!('R', UpLo == :L ? 'L' : 'U', 'T', IsUnit ? 'U' : 'N', one(T), B.data, A)
A_rdiv_Bc!{T<:BlasComplex,S<:AbstractMatrix,UpLo,IsUnit}(A::StridedMatrix{T}, B::Triangular{T,S,UpLo,IsUnit}) = BLAS.trsm!('R', UpLo == :L ? 'L' : 'U', 'C', IsUnit ? 'U' : 'N', one(T), B.data, A)

inv{T<:BlasFloat,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = Triangular{T,S,UpLo,IsUnit}(LAPACK.trtri!(UpLo == :L ? 'L' : 'U', IsUnit ? 'U' : 'N', copy(A.data)))

#Eigensystems
function eigvecs{T<:BlasFloat,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit})
    if UpLo == :U
        V = LAPACK.trevc!('R', 'A', Array(Bool,1), A.data)
    else # Uplo == :L
        V = LAPACK.trevc!('L', 'A', Array(Bool,1), A.data')
    end
    for i=1:size(V,2) #Normalize
        V[:,i] /= norm(V[:,i])
    end
    V
end

function cond{T<:BlasFloat,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, p::Real=2)
    chksquare(A)
    if p==1
        return inv(LAPACK.trcon!('O', UpLo == :L ? 'L' : 'U', IsUnit ? 'U' : 'N', A.data))
    elseif p==Inf
        return inv(LAPACK.trcon!('I', UpLo == :L ? 'L' : 'U', IsUnit ? 'U' : 'N', A.data))
    else #use fallback
        return cond(full(A), p)
    end
end

####################
# Generic routines #
####################

size(A::Triangular, args...) = size(A.data, args...)

convert{T,S<:AbstractMatrix,UpLo,IsUnit}(::Type{Triangular{T,S,UpLo,IsUnit}}, A::Triangular{T,S,UpLo,IsUnit}) = A
convert{T,S<:AbstractMatrix,UpLo,IsUnit}(::Type{Triangular{T,S,UpLo,IsUnit}}, A::Triangular) = Triangular{T,S,UpLo,IsUnit}(convert(AbstractMatrix{T}, A.data))
function convert{T,TA,S,UpLo,IsUnit}(::Type{AbstractMatrix{T}}, A::Triangular{TA,S,UpLo,IsUnit})
    M = convert(AbstractMatrix{T}, A.data)
    Triangular{T,typeof(M),UpLo,IsUnit}(M)
end
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
    length(dims) == 2 || throw(ArgumentError("a Traigular matrix must have two dimensions"))
    A = similar(A.data, Tnew, dims)
    return Triangular{Tnew, typeof(A), UpLo, IsUnit}(A)
end

getindex{T,S}(A::Triangular{T,S,:L,true}, i::Integer, j::Integer) = i == j ? one(T) : (i > j ? A.data[i,j] : zero(T))
getindex{T,S}(A::Triangular{T,S,:L,false}, i::Integer, j::Integer) = i >= j ? A.data[i,j] : zero(T)
getindex{T,S}(A::Triangular{T,S,:U,true}, i::Integer, j::Integer) = i == j ? one(T) : (i < j ? A.data[i,j] : zero(T))
getindex{T,S}(A::Triangular{T,S,:U,false}, i::Integer, j::Integer) = i <= j ? A.data[i,j] : zero(T)
getindex{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, i::Integer) = ((m, n) = divrem(i - 1, size(A,1)); A[m + 1, n + 1])

istril{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = UpLo == :L
istriu{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = UpLo == :U

transpose{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = Triangular{T, S, UpLo == :U ? :L : :U, IsUnit}(transpose(A.data))
ctranspose{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = Triangular{T, S, UpLo == :U ? :L : :U, IsUnit}(ctranspose(A.data))
transpose!{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = Triangular{T, S, UpLo == :U ? :L : :U, IsUnit}(copytri!(A.data, UpLo == :L ? 'L' : 'U'))
ctranspose!{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = Triangular{T, S, UpLo == :U ? :L : :U, IsUnit}(copytri!(A.data, UpLo == :L ? 'L' : 'U', true))
diag{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}) = IsUnit ? ones(T, size(A,1)) : diag(A.data)
function big{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit})
    M = big(A.data)
    Triangular{T,typeof(M),UpLo,IsUnit}(M)
end

function (*){T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, x::Number)
    n = size(A,1)
    for j = 1:n
        for i = UpLo == :L ? j:n : 1:j
            A.data[i,j] = i == j & IsUnit ? x : A.data[i,j]*x
        end
    end
    A
end
function (*){T,S,UpLo,IsUnit}(x::Number, A::Triangular{T,S,UpLo,IsUnit})
    n = size(A,1)
    for j = 1:n
        for i = UpLo == :L ? j:n : 1:j
            A.data[i,j] = i == j & IsUnit ? x : x*A.data[i,j]
        end
    end
    A
end

A_mul_B!{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, B::Triangular{T,S,UpLo,IsUnit}) = Triangular{T,S,UpLo,IsUnit}(A*full!(B))
A_mul_B!(A::Tridiagonal, B::Triangular) = A*full!(B)
A_mul_B!(C::AbstractVecOrMat, A::Triangular, B::AbstractVecOrMat) = A_mul_B!(A, copy!(C, B))

A_mul_Bc!(C::AbstractVecOrMat, A::Triangular, B::AbstractVecOrMat) = A_mul_Bc!(A, copy!(C, B))

#Generic multiplication
*(A::Tridiagonal, B::Triangular) = A_mul_B!(full(A), B)
for func in (:*, :Ac_mul_B, :A_mul_Bc, :/, :A_rdiv_Bc)
    @eval begin
        ($func){TA,TB,SA<:AbstractMatrix,SB<:AbstractMatrix,UpLoA,UpLoB,IsUnitA,IsUnitB}(A::Triangular{TA,SA,UpLoA,IsUnitA}, B::Triangular{TB,SB,UpLoB,IsUnitB}) = ($func)(A, full(B))
        ($func){T,S<:AbstractMatrix,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, B::AbstractVecOrMat) = ($func)(full(A), B)
        ($func){T,S<:AbstractMatrix,UpLo,IsUnit}(A::AbstractMatrix, B::Triangular{T,S,UpLo,IsUnit}) = ($func)(A, full(B))
    end
end

function sqrtm{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit})
    n = size(A, 1)
    R = zeros(T, n, n)
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
        return Triangular{T,S,UpLo,IsUnit}(R)
    else # UpLo == :L #Not the usual case
        return sqrtm(A.').'
    end
end

#Generic solver using naive substitution
function naivesub!{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit}, b::AbstractVector, x::AbstractVector=b)
    N = size(A, 2)
    N==length(b)==length(x) || throw(DimensionMismatch(""))

    if UpLo == :L #do forward substitution
        for j = 1:N
            x[j] = b[j]
            for k = 1:j-1
                x[j] -= A[j,k] * x[k]
            end
            if !IsUnit
                x[j]/= A[j,j]==0 ? throw(SingularException(j)) : A[j,j]
            end
        end
    elseif UpLo == :U #do backward substitution
        for j = N:-1:1
            x[j] = b[j]
            for k = j+1:1:N
                x[j] -= A[j,k] * x[k]
            end
            if !IsUnit
                x[j]/= A[j,j]==0 ? throw(SingularException(j)) : A[j,j]
            end
        end
    else
        throw(ArgumentError("Unknown UpLo=$(UpLo)"))
    end
    x
end

#Generic eigensystems
eigvals(A::Triangular) = diag(A.data)
det(A::Triangular) = prod(eigvals(A))

function eigvecs{T,S,UpLo,IsUnit}(A::Triangular{T,S,UpLo,IsUnit})
    N = size(A,1)
    evecs = zeros(T, N, N)
    if IsUnit #Trivial
        return eye(A)
    elseif UpLo == :L #do forward substitution
        for i=1:N
            evecs[i,i] = one(T)
            for j = i+1:N
                for k = i:j-1
                    evecs[j,i] -= A[j,k] * evecs[k,i]
                end
                evecs[j,i] /= A[j,j] - A[i,i]
            end
            evecs[i:N, i] /= norm(evecs[i:N, i])
        end
        evecs
    elseif UpLo == :U #do backward substitution
        for i=1:N
            evecs[i,i] = one(T)
            for j = i-1:-1:1
                for k = j+1:i
                    evecs[j,i] -= A[j,k] * evecs[k,i]
                end
                evecs[j,i] /= A[j,j]-A[i,i]
            end
            evecs[1:i, i] /= norm(evecs[1:i, i])
        end
    else
        throw(ArgumentError("Unknown uplo=$(UpLo)"))
    end
    evecs
end

eigfact(A::Triangular) = Eigen(eigvals(A), eigvecs(A))

#Generic singular systems
for func in (:svd, :svdfact, :svdfact!, :svdvals, :svdvecs)
    @eval begin
        ($func)(A::Triangular) = ($func)(full(A))
    end
end
