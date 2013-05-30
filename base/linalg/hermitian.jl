## Hermitian matrices

type Hermitian{T<:BlasFloat} <: AbstractMatrix{T}
    S::Matrix{T}
    uplo::Char
    function Hermitian(S::Matrix{T}, uplo::Char)
        if size(S, 1) != size(S, 2) throw(DimensionMismatch("Matrix must be square")); end
        return new(S, uplo)
    end
end

Hermitian{T<:BlasFloat}(S::Matrix{T}, uplo::Char) = Hermitian{T}(S, uplo)
Hermitian(A::StridedMatrix) = Hermitian(A, 'U')

copy(A::Hermitian) = Hermitian(copy(A.S), A.uplo)
size(A::Hermitian, args...) = size(A.S, args...)
print_matrix(io::IO, A::Hermitian) = print_matrix(io, full(A))
full(A::Hermitian) = A.S
ishermitian(A::Hermitian) = true
issym{T<:Union(Float64, Float32)}(A::Hermitian{T}) = true
ctranspose(A::Hermitian) = A

*(A::Hermitian, B::Hermitian) = *(full(A), full(B))
*(A::Hermitian, B::StridedMatrix) = *(full(A), B)
*(A::StridedMatrix, B::Hermitian) = *(A, full(B))

function \(A::Hermitian, B::StridedVecOrMat)
    r, _, _, info = LAPACK.sysv!(A.uplo, copy(A.S), copy(B))
    if info > 0 throw(SingularException(info)) end
    return r
end

inv(A::Hermitian) = inv(BunchKaufman(copy(A.S), A.uplo))

eigfact!(A::Hermitian) = Eigen(LAPACK.syevr!('V', 'A', A.uplo, A.S, 0.0, 0.0, 0, 0, -1.0)...)
eigfact(A::Hermitian) = eigfact!(copy(A))
eigvals(A::Hermitian, il::Int, ih::Int) = LAPACK.syevr!('N', 'I', A.uplo, copy(A.S), 0.0, 0.0, il, ih, -1.0)[1]
eigvals(A::Hermitian, vl::Real, vh::Real) = LAPACK.syevr!('N', 'V', A.uplo, copy(A.S), vl, vh, 0, 0, -1.0)[1]
eigvals(A::Hermitian) = eigvals(A, 1, size(A, 1))
eigmax(A::Hermitian) = eigvals(A, size(A, 1), size(A, 1))[1]
eigmin(A::Hermitian) = eigvals(A, 1, 1)[1]

function eigfact!(A::Hermitian, B::Hermitian)
    vals, vecs, _ = LAPACK.sygvd!(1, 'V', A.uplo, A.S, B.uplo == A.uplo ? B.S : B.S')
    return GeneralizedEigen(vals, vecs)
end
eigfact(A::Hermitian, B::Hermitian) = eigfact!(copy(A), copy(B))
eigvals!(A::Hermitian, B::Hermitian) = LAPACK.sygvd!(1, 'N', A.uplo, A.S, B.uplo == A.uplo ? B.S : B.S')[1]

function expm(A::Hermitian)
    F = eigfact(A)
    scale(F[:vectors], exp(F[:values])) * F[:vectors]'
end

function sqrtm(A::Hermitian, cond::Bool)
    F = eigfact(A)
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
