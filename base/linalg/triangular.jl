## Triangular
immutable Triangular{T<:Number} <: AbstractMatrix{T}
    UL::Matrix{T}
    uplo::Char
    unitdiag::Char
end
function Triangular{T<:Number}(A::Matrix{T}, uplo::Symbol, unitdiag::Bool)
    if size(A, 1) != size(A, 2) throw(DimensionMismatch("matrix must be square")) end
    return Triangular(A, string(uplo)[1], unitdiag ? 'U' : 'N')
end
Triangular(A::Matrix, uplo::Symbol) = Triangular(A, uplo, all(diag(A) .== 1) ? true : false)
function Triangular(A::Matrix)
    if istriu(A) return Triangular(A, :U) end
    if istril(A) return Triangular(A, :L) end
    throw(ArgumentError("matrix is not triangular"))
end

######################
# BlasFloat routines #
######################

# Vector multiplication
*{T<:BlasFloat}(A::Triangular{T}, b::Vector{T}) = BLAS.trmv(A.uplo, 'N', A.unitdiag, A.UL, b)
Ac_mul_B{T<:BlasComplex}(A::Triangular{T}, b::Vector{T}) = BLAS.trmv(A.uplo, 'C', A.unitdiag, A.UL, b)
At_mul_B{T<:BlasReal}(A::Triangular{T}, b::Vector{T}) = BLAS.trmv(A.uplo, 'T', A.unitdiag, A.UL, b)

# Matrix multiplication
*{T<:BlasFloat}(A::Triangular{T}, B::StridedMatrix{T}) = BLAS.trmm('L', A.uplo, 'N', A.unitdiag, one(T), A.UL, B)
*{T<:BlasFloat}(A::StridedMatrix{T}, B::Triangular{T}) = BLAS.trmm('R', B.uplo, 'N', B.unitdiag, one(T), A, B.UL)
A_mul_B!{T<:BlasFloat}(A::Triangular{T},B::Matrix{T}) = BLAS.trmm!('L',A.uplo,'N',A.unitdiag,one(eltype(A)),A.UL,B)
Ac_mul_B!{T<:BlasComplex}(A::Triangular{T}, B::StridedMatrix{T}) = BLAS.trmm('L', A.uplo, 'C', A.unitdiag, one(T), A.UL, B)
Ac_mul_B!{T<:BlasReal}(A::Triangular{T}, B::StridedMatrix{T}) = BLAS.trmm('L', A.uplo, 'T', A.unitdiag, one(T), A.UL, B)
A_mul_Bc!{T<:BlasComplex}(A::StridedMatrix{T}, B::Triangular{T}) = BLAS.trmm('R', B.uplo, 'C', B.unitdiag, one(T), B.UL, A)
A_mul_Bc!{T<:BlasReal}(A::StridedMatrix{T}, B::Triangular{T}) = BLAS.trmm('R', B.uplo, 'T', B.unitdiag, one(T), B.UL, A)


function \{T<:BlasFloat}(A::Triangular{T}, B::StridedVecOrMat{T})
    x = LAPACK.trtrs!(A.uplo, 'N', A.unitdiag, A.UL, copy(B))
    for errors in LAPACK.trrfs!(A.uplo, 'N', A.unitdiag, A.UL, B, x)
        all(isfinite(errors)) || all(errors.<one(T)/eps(T)) || warn("""Unreasonably large error in computed solution:
forward error: $ferr
backward error: $berr""")
    end
    x
end
Ac_ldiv_B{T<:BlasReal}(A::Triangular{T}, B::StridedVecOrMat{T}) = LAPACK.trtrs!(A.uplo, 'T', A.unitdiag, A.UL, copy(B))
Ac_ldiv_B{T<:BlasComplex}(A::Triangular{T}, B::StridedVecOrMat{T}) = LAPACK.trtrs!(A.uplo, 'C', A.unitdiag, A.UL, copy(B))

/{T<:BlasFloat}(A::StridedVecOrMat{T}, B::Triangular{T}) = BLAS.trsm!('R', B.uplo, 'N', B.unitdiag, one(T), B.UL, copy(A))
A_rdiv_Bc{T<:BlasReal}(A::StridedVecOrMat{T}, B::Triangular{T}) = BLAS.trsm!('R', B.uplo, 'T', B.unitdiag, one(T), B.UL, copy(A))
A_rdiv_Bc{T<:BlasComplex}(A::StridedVecOrMat{T}, B::Triangular{T}) = BLAS.trsm!('R', B.uplo, 'C', B.unitdiag, one(T), B.UL, copy(A))

inv{T<:BlasFloat}(A::Triangular{T}) = LAPACK.trtri!(A.uplo, A.unitdiag, copy(A.UL))

#Eigensystems
function eigvecs{T<:BlasFloat}(A::Triangular{T})
    if A.uplo=='U'
        V = LAPACK.trevc!('R', 'A', Array(Bool,1), A.UL)
    else #A.uplo=='L'
        V = LAPACK.trevc!('L', 'A', Array(Bool,1), A.UL')
    end
    for i=1:size(V,2) #Normalize
        V[:,i] /= norm(V[:,i])
    end
    V
end

function cond{T<:BlasFloat}(A::Triangular{T}, p::Real=2)
    chksquare(A)
    if p==1
        return inv(LAPACK.trcon!('O', A.uplo, A.unitdiag, A.UL))
    elseif p==Inf
        return inv(LAPACK.trcon!('I', A.uplo, A.unitdiag, A.UL))
    else #use fallback
        return cond(full(A), p)
    end
end

####################
# Generic routines #
####################

size(A::Triangular, args...) = size(A.UL, args...)
convert(::Type{Matrix}, A::Triangular) = full(A)
full(A::Triangular) = A.uplo == 'U' ? triu!(A.UL) : tril!(A.UL)

getindex{T}(A::Triangular{T}, i::Integer, j::Integer) = i == j ? (A.unitdiag == 'U' ? one(T) : A.UL[i,j]) : ((A.uplo == 'U') == (i < j) ? getindex(A.UL, i, j) : zero(T))

istril(A::Triangular) = A.uplo == 'L' || istriu(A.UL)
istriu(A::Triangular) = A.uplo == 'U' || istril(A.UL)

transpose(A::Triangular) = Triangular(symmetrize!(A.UL, A.uplo), A.uplo=='U'?'L':'U', A.unitdiag)
ctranspose(A::Triangular) = Triangular(symmetrize_conj!(A.UL, A.uplo), A.uplo=='U'?'L':'U', A.unitdiag)
diag(A::Triangular) = diag(A.UL)
big(A::Triangular) = Triangular(big(A.UL), A.uplo, A.unitdiag)

#Generic multiplication
for func in (:*, :Ac_mul_B, :A_mul_Bc, :/, :A_rdiv_Bc)
    @eval begin
        ($func){T}(A::Triangular{T}, B::AbstractVector{T}) = ($func)(full(A), B)
        #($func){T}(A::AbstractArray{T}, B::Triangular{T}) = ($func)(full(A), B)
    end
end

#Generic solver using naive substitution
function naivesub!(A::Triangular, b::AbstractVector, x::AbstractVector=b)
    N = size(A, 2)
    N==length(b)==length(x) || throw(DimensionMismatch(""))

    if A.uplo == 'L' #do forward substitution
        for j = 1:N
            x[j] = b[j]
            for k = 1:j-1
                x[j] -= A[j,k] * x[k]
            end
            if A.unitdiag=='N'
                x[j]/= A[j,j]==0 ? throw(SingularException(j)) : A[j,j]
            end
        end
    elseif A.uplo == 'U' #do backward substitution
        for j = N:-1:1
            x[j] = b[j]
            for k = j+1:1:N
                x[j] -= A[j,k] * x[k]
            end
            if A.unitdiag=='N'
                x[j]/= A[j,j]==0 ? throw(SingularException(j)) : A[j,j]
            end
        end
    else
        throw(ArgumentError("Unknown uplo=$(A.uplo)"))
    end
    x
end

#Generic eigensystems
eigvals(A::Triangular) = diag(A.UL)
det(A::Triangular) = prod(eigvals(A))

function eigvecs{T}(A::Triangular{T})
    evecs = zeros(A)
    N = size(A,1)
    if A.unitdiag == 'U' #Trivial
        return eye(A)
    elseif A.uplo == 'L' #do forward substitution
        for i=1:N
            evecs[i,i] = one(T)
            for j = i+1:N
                for k = i:j-1
                    evecs[j,i] -= A[j,k] * evecs[k,i]
                end
                evecs[j,i] /= A[j,j]-A[i,i]
            end
            evecs[i:N, i] /= norm(evecs[i:N, i])
        end
        evecs
    elseif A.uplo == 'U' #do backward substitution
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
        throw(ArgumentError("Unknown uplo=$(A.uplo)"))
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
