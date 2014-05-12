#Symmetric and Hermitian matrices
immutable Symmetric{T} <: AbstractMatrix{T}
    S::Matrix{T}
    uplo::Char
end
Symmetric(A::Matrix, uplo::Symbol=:U) = (chksquare(A);Symmetric(A, string(uplo)[1]))
immutable Hermitian{T} <: AbstractMatrix{T}
    S::Matrix{T}
    uplo::Char
end
Hermitian(A::Matrix, uplo::Symbol=:U) = (chksquare(A);Hermitian(A, string(uplo)[1]))
typealias HermOrSym{T} Union(Hermitian{T}, Symmetric{T})
typealias RealHermSymComplexHerm{T<:Real} Union(Hermitian{T}, Symmetric{T}, Hermitian{Complex{T}})

size(A::HermOrSym, args...) = size(A.S, args...)
getindex(A::Symmetric, i::Integer, j::Integer) = (A.uplo == 'U') == (i < j) ? getindex(A.S, i, j) : getindex(A.S, j, i)
getindex(A::Hermitian, i::Integer, j::Integer) = (A.uplo == 'U') == (i < j) ? getindex(A.S, i, j) : conj(getindex(A.S, j, i))
full(A::Symmetric) = copytri!(A.S, A.uplo)
full(A::Hermitian) = copytri!(A.S, A.uplo, true)
convert{T}(::Type{Symmetric{T}},A::Symmetric) = Symmetric(convert(Matrix{T},A.S), A.uplo)
convert{T}(::Type{AbstractMatrix{T}}, A::Symmetric) = Symmetric(convert(AbstractMatrix{T}, A.S), A.uplo)
convert{T}(::Type{Hermitian{T}},A::Hermitian) = Hermitian(convert(Matrix{T},A.S), A.uplo)
convert{T}(::Type{AbstractMatrix{T}}, A::Hermitian) = Hermitian(convert(AbstractMatrix{T}, A.S), A.uplo)
copy(A::Symmetric) = Symmetric(copy(A.S),A.uplo)
copy(A::Hermitian) = Hermitian(copy(A.S),A.uplo)
ishermitian(A::Hermitian) = true
ishermitian{T<:Real}(A::Symmetric{T}) = true
ishermitian{T<:Complex}(A::Symmetric{T}) = all(imag(A.S) .== 0)
issym{T<:Real}(A::Hermitian{T}) = true
issym{T<:Complex}(A::Hermitian{T}) = all(imag(A.S) .== 0)
issym(A::Symmetric) = true
transpose(A::Symmetric) = A
ctranspose(A::Hermitian) = A

*(A::HermOrSym, B::HermOrSym) = full(A)*full(B)
*(A::HermOrSym, B::StridedMatrix) = full(A)*B
*(A::StridedMatrix, B::HermOrSym) = A*full(B)

factorize(A::HermOrSym) = bkfact(A.S, symbol(A.uplo), issym(A))
\(A::HermOrSym, B::StridedVecOrMat) = \(bkfact(A.S, symbol(A.uplo), issym(A)), B)

eigfact!{T<:BlasReal}(A::RealHermSymComplexHerm{T}) = Eigen(LAPACK.syevr!('V', 'A', A.uplo, A.S, 0.0, 0.0, 0, 0, -1.0)...)
eigfact!{T<:BlasReal}(A::RealHermSymComplexHerm{T}, irange::UnitRange) = Eigen(LAPACK.syevr!('V', 'I', A.uplo, A.S, 0.0, 0.0, irange.start, irange.stop, -1.0)...)
eigfact!{T<:BlasReal}(A::RealHermSymComplexHerm{T}, vl::Real, vh::Real) = Eigen(LAPACK.syevr!('V', 'V', A.uplo, A.S, convert(T, vl), convert(T, vh), 0, 0, -1.0)...)
eigvals!{T<:BlasReal}(A::RealHermSymComplexHerm{T}) = LAPACK.syevr!('N', 'A', A.uplo, A.S, 0.0, 0.0, 0, 0, -1.0)[1]
eigvals!{T<:BlasReal}(A::RealHermSymComplexHerm{T}, irange::UnitRange) = LAPACK.syevr!('N', 'I', A.uplo, A.S, 0.0, 0.0, irange.start, irange.stop, -1.0)[1]
eigvals!{T<:BlasReal}(A::RealHermSymComplexHerm{T}, vl::Real, vh::Real) = LAPACK.syevr!('N', 'V', A.uplo, A.S, convert(T, vl), convert(T, vh), 0, 0, -1.0)[1]
eigmax{T<:Real}(A::RealHermSymComplexHerm{T}) = eigvals(A, size(A, 1), size(A, 1))[1]
eigmin{T<:Real}(A::RealHermSymComplexHerm{T}) = eigvals(A, 1, 1)[1]

function eigfact!{T<:BlasReal}(A::HermOrSym{T}, B::HermOrSym{T})
    vals, vecs, _ = LAPACK.sygvd!(1, 'V', A.uplo, A.S, B.uplo == A.uplo ? B.S : B.S')
    GeneralizedEigen(vals, vecs)
end
function eigfact!{T<:BlasComplex}(A::Hermitian{T}, B::Hermitian{T})
    vals, vecs, _ = LAPACK.sygvd!(1, 'V', A.uplo, A.S, B.uplo == A.uplo ? B.S : B.S')
    GeneralizedEigen(vals, vecs)
end
eigvals!{T<:BlasReal}(A::HermOrSym{T}, B::HermOrSym{T}) = LAPACK.sygvd!(1, 'N', A.uplo, A.S, B.uplo == A.uplo ? B.S : B.S')[1]
eigvals!{T<:BlasComplex}(A::Hermitian{T}, B::Hermitian{T}) = LAPACK.sygvd!(1, 'N', A.uplo, A.S, B.uplo == A.uplo ? B.S : B.S')[1]

#Matrix-valued functions
expm{T<:Real}(A::RealHermSymComplexHerm{T}) = (F = eigfact(A); F.vectors*Diagonal(exp(F.values))*F.vectors')
function sqrtm{T<:Real}(A::RealHermSymComplexHerm{T})
    F = eigfact(A)
    isposdef(F) && return F.vectors*Diagonal(sqrt(F.values))*F.vectors'
    return F.vectors*Diagonal(sqrt(complex(F.values)))*F.vectors'
end