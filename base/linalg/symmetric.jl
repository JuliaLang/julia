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

size(A::HermOrSym, args...) = size(A.S, args...)
getindex(A::HermOrSym, i::Integer, j::Integer) = (A.uplo == 'U') == (i < j) ? getindex(A.S, i, j) : conj(getindex(A.S, j, i))
full(A::Symmetric) = copytri!(A.S, A.uplo)
full(A::Hermitian) = copytri!(A.S, A.uplo, true)
convert{T}(::Type{Symmetric{T}},A::Symmetric) = Symmetric(convert(Matrix{T},A.S),A.uplo)
convert{T}(::Type{Hermitian{T}},A::Hermitian) = Hermitian(convert(Matrix{T},A.S),A.uplo)
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

eigfact!{T<:BlasReal}(A::Symmetric{T}) = Eigen(LAPACK.syevr!('V', 'A', A.uplo, A.S, 0.0, 0.0, 0, 0, -1.0)...)
eigfact!{T<:BlasComplex}(A::Hermitian{T}) = Eigen(LAPACK.syevr!('V', 'A', A.uplo, A.S, 0.0, 0.0, 0, 0, -1.0)...)
eigfact{T<:BlasFloat}(A::HermOrSym{T}) = eigfact!(copy(A))
eigfact{T}(A::HermOrSym{T}) = (S = promote_type(Float32,typeof(sqrt(one(T)))); S != T ? eigfact!(convert(typeof(A).name.primary{S}, A)) : eigfact!(copy(A)))
eigvals!{T<:BlasReal}(A::Symmetric{T}, il::Int=1, ih::Int=size(A,1)) = LAPACK.syevr!('N', 'I', A.uplo, A.S, 0.0, 0.0, il, ih, -1.0)[1]
eigvals!{T<:BlasReal}(A::Symmetric{T}, vl::Real, vh::Real) = LAPACK.syevr!('N', 'V', A.uplo, A.S, vl, vh, 0, 0, -1.0)[1]
eigvals!{T<:BlasComplex}(A::Hermitian{T}, il::Int=1, ih::Int=size(A,1)) = LAPACK.syevr!('N', 'I', A.uplo, A.S, 0.0, 0.0, il, ih, -1.0)[1]
eigvals!{T<:BlasComplex}(A::Hermitian{T}, vl::Real, vh::Real) = LAPACK.syevr!('N', 'V', A.uplo, A.S, vl, vh, 0, 0, -1.0)[1]
eigvals{T<:BlasFloat}(A::HermOrSym{T},l::Real=1,h::Real=size(A,1)) = eigvals!(copy(A),l,h)
eigvals{T}(A::HermOrSym{T},l::Real=1,h::Real=size(A,1)) = (S = promote_type(Float32,typeof(sqrt(one(T)))); S != T ? eigvals!(convert(typeof(A).name.primary{S}, A, l, h)) : eigvals!(copy(A), l, h))
eigmax(A::HermOrSym) = eigvals(A, size(A, 1), size(A, 1))[1]
eigmin(A::HermOrSym) = eigvals(A, 1, 1)[1]

function eigfact!{T<:BlasReal}(A::Symmetric{T}, B::Symmetric{T})
    vals, vecs, _ = LAPACK.sygvd!(1, 'V', A.uplo, A.S, B.uplo == A.uplo ? B.S : B.S')
    GeneralizedEigen(vals, vecs)
end
function eigfact!{T<:BlasComplex}(A::Hermitian{T}, B::Hermitian{T})
    vals, vecs, _ = LAPACK.sygvd!(1, 'V', A.uplo, A.S, B.uplo == A.uplo ? B.S : B.S')
    GeneralizedEigen(vals, vecs)
end
eigvals!{T<:BlasReal}(A::Symmetric{T}, B::Symmetric{T}) = LAPACK.sygvd!(1, 'N', A.uplo, A.S, B.uplo == A.uplo ? B.S : B.S')[1]
eigvals!{T<:BlasComplex}(A::Hermitian{T}, B::Hermitian{T}) = LAPACK.sygvd!(1, 'N', A.uplo, A.S, B.uplo == A.uplo ? B.S : B.S')[1]

#Matrix-valued functions
expm(A::HermOrSym) = (F = eigfact(A); F.vectors*Diagonal(exp(F.values))*F.vectors')
function sqrtm(A::HermOrSym)
    F = eigfact(A)
    isposdef(F) && return F.vectors*Diagonal(sqrt(F.values))*F.vectors'
    return F.vectors*Diagonal(sqrt(complex(F.values)))*F.vectors'
end
