## Hermitian matrices

type Hermitian{T<:Number} <: AbstractMatrix{T}
    S::Matrix{T}
    uplo::Char
end
function Hermitian(S::Matrix, uplo::Symbol)
    if size(S, 1) != size(S, 2) throw(DimensionMismatch("Matrix must be square")); end
    return Hermitian(S, string(uplo)[1])
end
Hermitian(A::Matrix) = Hermitian(A, :U)

copy(A::Hermitian) = Hermitian(copy(A.S), A.uplo)
size(A::Hermitian, args...) = size(A.S, args...)
print_matrix(io::IO, A::Hermitian) = print_matrix(io, full(A))
full(A::Hermitian) = A.S
ishermitian(A::Hermitian) = true
issym{T<:Real}(A::Hermitian{T}) = true
issym{T<:Complex}(A::Hermitian{T}) = all(imag(A.S) .== 0)
ctranspose(A::Hermitian) = A

*(A::Hermitian, B::Hermitian) = *(full(A), full(B))
*(A::Hermitian, B::StridedMatrix) = *(full(A), B)
*(A::StridedMatrix, B::Hermitian) = *(A, full(B))

factorize!(A::Hermitian) = bkfact!(A.S, symbol(A.uplo), issym(A))
\(A::Hermitian, B::StridedVecOrMat) = \(bkfact(A.S, symbol(A.uplo), issym(A)), B)

eigfact!{T<:BlasFloat}(A::Hermitian{T}) = Eigen(LAPACK.syevr!('V', 'A', A.uplo, A.S, 0.0, 0.0, 0, 0, -1.0)...)
eigfact(A::Hermitian) = eigfact!(copy(A))
eigvals{T<:BlasFloat}(A::Hermitian{T}, il::Int, ih::Int) = LAPACK.syevr!('N', 'I', A.uplo, copy(A.S), 0.0, 0.0, il, ih, -1.0)[1]
eigvals{T<:BlasFloat}(A::Hermitian{T}, vl::Real, vh::Real) = LAPACK.syevr!('N', 'V', A.uplo, copy(A.S), vl, vh, 0, 0, -1.0)[1]
eigvals(A::Hermitian) = eigvals(A, 1, size(A, 1))
eigmax(A::Hermitian) = eigvals(A, size(A, 1), size(A, 1))[1]
eigmin(A::Hermitian) = eigvals(A, 1, 1)[1]

function eigfact!{T<:BlasFloat}(A::Hermitian{T}, B::Hermitian{T})
    vals, vecs, _ = LAPACK.sygvd!(1, 'V', A.uplo, A.S, B.uplo == A.uplo ? B.S : B.S')
    return GeneralizedEigen(vals, vecs)
end
eigfact(A::Hermitian, B::Hermitian) = eigfact!(copy(A), copy(B))
eigvals!{T<:BlasFloat}(A::Hermitian{T}, B::Hermitian{T}) = LAPACK.sygvd!(1, 'N', A.uplo, A.S, B.uplo == A.uplo ? B.S : B.S')[1]

function expm(A::Hermitian)
    F = eigfact(A)
    scale(F[:vectors], exp(F[:values])) * F[:vectors]'
end

function sqrtm(A::Hermitian, cond::Bool=false)
    F = eigfact(A)
    if length(F[:values]) == 0 return A end
    vsqrt = sqrt(complex(F[:values]))
    if all(imag(vsqrt) .== 0)
        retmat = symmetrize!(scale(F[:vectors], real(vsqrt)) * F[:vectors]')
    else
        zc = complex(F[:vectors])
        retmat = symmetrize!(scale(zc, vsqrt) * zc')
    end
    if cond
        return retmat, norm(vsqrt, Inf)^2/norm(F[:values], Inf)
    else
        return retmat
    end
end
