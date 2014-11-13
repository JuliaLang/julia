#Symmetric and Hermitian matrices
immutable Symmetric{T,S<:AbstractMatrix} <: AbstractMatrix{T}
    data::S
    uplo::Char
end
Symmetric(A::AbstractMatrix, uplo::Symbol=:U) = (chksquare(A);Symmetric{eltype(A),typeof(A)}(A, char_uplo(uplo)))
immutable Hermitian{T,S<:AbstractMatrix} <: AbstractMatrix{T}
    data::S
    uplo::Char
end
Hermitian(A::AbstractMatrix, uplo::Symbol=:U) = (chksquare(A);Hermitian{eltype(A),typeof(A)}(A, char_uplo(uplo)))
typealias HermOrSym{T,S} Union(Hermitian{T,S}, Symmetric{T,S})
typealias RealHermSymComplexHerm{T<:Real,S} Union(Hermitian{T,S}, Symmetric{T,S}, Hermitian{Complex{T},S})

size(A::HermOrSym, args...) = size(A.data, args...)
getindex(A::Symmetric, i::Integer, j::Integer) = (A.uplo == 'U') == (i < j) ? getindex(A.data, i, j) : getindex(A.data, j, i)
getindex(A::Hermitian, i::Integer, j::Integer) = (A.uplo == 'U') == (i < j) ? getindex(A.data, i, j) : conj(getindex(A.data, j, i))
full(A::Symmetric) = copytri!(copy(A.data), A.uplo)
full(A::Hermitian) = copytri!(copy(A.data), A.uplo, true)
convert{T,S<:AbstractMatrix}(::Type{Symmetric{T,S}},A::Symmetric{T,S}) = A
convert{T,S<:AbstractMatrix}(::Type{Symmetric{T,S}},A::Symmetric) = Symmetric{T,S}(convert(S,A.data),A.uplo)
convert{T}(::Type{AbstractMatrix{T}}, A::Symmetric) = Symmetric(convert(AbstractMatrix{T}, A.data), symbol(A.uplo))
convert{T,S<:AbstractMatrix}(::Type{Hermitian{T,S}},A::Hermitian{T,S}) = A
convert{T,S<:AbstractMatrix}(::Type{Hermitian{T,S}},A::Hermitian) = Hermitian{T,S}(convert(S,A.data),A.uplo)
convert{T}(::Type{AbstractMatrix{T}}, A::Hermitian) = Hermitian(convert(AbstractMatrix{T}, A.data), symbol(A.uplo))
copy{T,S}(A::Symmetric{T,S}) = Symmetric{T,S}(copy(A.data),A.uplo)
copy{T,S}(A::Hermitian{T,S}) = Hermitian{T,S}(copy(A.data),A.uplo)
ishermitian(A::Hermitian) = true
ishermitian{T<:Real,S}(A::Symmetric{T,S}) = true
ishermitian{T<:Complex,S}(A::Symmetric{T,S}) = all(imag(A.data) .== 0)
issym{T<:Real,S}(A::Hermitian{T,S}) = true
issym{T<:Complex,S}(A::Hermitian{T,S}) = all(imag(A.data) .== 0)
issym(A::Symmetric) = true
transpose(A::Symmetric) = A
ctranspose(A::Hermitian) = A

## Matvec
A_mul_B!{T<:BlasFloat,S<:AbstractMatrix}(y::StridedVector{T}, A::Symmetric{T,S}, x::StridedVector{T}) = BLAS.symv!(A.uplo, one(T), A.data, x, zero(T), y)
A_mul_B!{T<:BlasComplex,S<:AbstractMatrix}(y::StridedVector{T}, A::Hermitian{T,S}, x::StridedVector{T}) = BLAS.hemv!(A.uplo, one(T), A.data, x, zero(T), y)
##Matmat
A_mul_B!{T<:BlasFloat,S<:AbstractMatrix}(C::StridedMatrix{T}, A::Symmetric{T,S}, B::StridedMatrix{T}) = BLAS.symm!(A.uplo, one(T), A.data, B, zero(T), C)
A_mul_B!{T<:BlasComplex,S<:AbstractMatrix}(y::StridedMatrix{T}, A::Hermitian{T,S}, x::StridedMatrix{T}) = BLAS.hemm!(A.uplo, one(T), A.data, B, zero(T), C)

*(A::HermOrSym, B::HermOrSym) = full(A)*full(B)
*(A::StridedMatrix, B::HermOrSym) = A*full(B)

factorize(A::HermOrSym) = bkfact(A.data, symbol(A.uplo), issym(A))
\(A::HermOrSym, B::StridedVecOrMat) = \(bkfact(A.data, symbol(A.uplo), issym(A)), B)
inv{T<:BlasFloat,S<:StridedMatrix}(A::Hermitian{T,S}) = Hermitian{T,S}(inv(bkfact(A.data, symbol(A.uplo))), A.uplo)
inv{T<:BlasFloat,S<:StridedMatrix}(A::Symmetric{T,S}) = Symmetric{T,S}(inv(bkfact(A.data, symbol(A.uplo), true)), A.uplo)

eigfact!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}) = Eigen(LAPACK.syevr!('V', 'A', A.uplo, A.data, 0.0, 0.0, 0, 0, -1.0)...)
eigfact!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}, irange::UnitRange) = Eigen(LAPACK.syevr!('V', 'I', A.uplo, A.data, 0.0, 0.0, irange.start, irange.stop, -1.0)...)
eigfact!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}, vl::Real, vh::Real) = Eigen(LAPACK.syevr!('V', 'V', A.uplo, A.data, convert(T, vl), convert(T, vh), 0, 0, -1.0)...)
eigvals!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}) = LAPACK.syevr!('N', 'A', A.uplo, A.data, 0.0, 0.0, 0, 0, -1.0)[1]
eigvals!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}, irange::UnitRange) = LAPACK.syevr!('N', 'I', A.uplo, A.data, 0.0, 0.0, irange.start, irange.stop, -1.0)[1]
eigvals!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}, vl::Real, vh::Real) = LAPACK.syevr!('N', 'V', A.uplo, A.data, convert(T, vl), convert(T, vh), 0, 0, -1.0)[1]
eigmax{T<:Real,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}) = eigvals(A, size(A, 1):size(A, 1))[1]
eigmin{T<:Real,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}) = eigvals(A, 1:1)[1]

function eigfact!{T<:BlasReal,S<:StridedMatrix}(A::HermOrSym{T,S}, B::HermOrSym{T,S})
    vals, vecs, _ = LAPACK.sygvd!(1, 'V', A.uplo, A.data, B.uplo == A.uplo ? B.data : B.data')
    GeneralizedEigen(vals, vecs)
end
function eigfact!{T<:BlasComplex,S<:StridedMatrix}(A::Hermitian{T,S}, B::Hermitian{T,S})
    vals, vecs, _ = LAPACK.sygvd!(1, 'V', A.uplo, A.data, B.uplo == A.uplo ? B.data : B.data')
    GeneralizedEigen(vals, vecs)
end
eigvals!{T<:BlasReal,S<:StridedMatrix}(A::HermOrSym{T,S}, B::HermOrSym{T,S}) = LAPACK.sygvd!(1, 'N', A.uplo, A.data, B.uplo == A.uplo ? B.data : B.data')[1]
eigvals!{T<:BlasComplex,S<:StridedMatrix}(A::Hermitian{T,S}, B::Hermitian{T,S}) = LAPACK.sygvd!(1, 'N', A.uplo, A.data, B.uplo == A.uplo ? B.data : B.data')[1]

#Matrix-valued functions
expm{T<:Real}(A::RealHermSymComplexHerm{T}) = (F = eigfact(A); F.vectors*Diagonal(exp(F.values))*F.vectors')
function sqrtm{T<:Real}(A::RealHermSymComplexHerm{T})
    F = eigfact(A)
    isposdef(F) && return F.vectors*Diagonal(sqrt(F.values))*F.vectors'
    return F.vectors*Diagonal(sqrt(complex(F.values)))*F.vectors'
end
