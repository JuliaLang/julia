# This file is a part of Julia. License is MIT: http://julialang.org/license

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
function Hermitian(A::AbstractMatrix, uplo::Symbol=:U)
    n = chksquare(A)
    for i=1:n
        isreal(A[i, i]) || throw(ArgumentError(
            "Cannot construct Hermitian from matrix with nonreal diagonals"))
    end
    Hermitian{eltype(A),typeof(A)}(A, char_uplo(uplo))
end

typealias HermOrSym{T,S} Union{Hermitian{T,S}, Symmetric{T,S}}
typealias RealHermSymComplexHerm{T<:Real,S} Union{Hermitian{T,S}, Symmetric{T,S}, Hermitian{Complex{T},S}}

size(A::HermOrSym, args...) = size(A.data, args...)
getindex(A::Symmetric, i::Integer, j::Integer) = (A.uplo == 'U') == (i < j) ? getindex(A.data, i, j) : getindex(A.data, j, i)
getindex(A::Hermitian, i::Integer, j::Integer) = (A.uplo == 'U') == (i < j) ? getindex(A.data, i, j) : conj(getindex(A.data, j, i))
unsafe_getindex(A::Symmetric, i::Integer, j::Integer) = (A.uplo == 'U') == (i < j) ? unsafe_getindex(A.data, i, j) : unsafe_getindex(A.data, j, i)
unsafe_getindex(A::Hermitian, i::Integer, j::Integer) = (A.uplo == 'U') == (i < j) ? unsafe_getindex(A.data, i, j) : conj(unsafe_getindex(A.data, j, i))
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
ctranspose{T<:Real}(A::Symmetric{T}) = A
function ctranspose(A::Symmetric)
    AC = ctranspose(A.data)
    return Symmetric(AC, ifelse(A.uplo == 'U', :L, :U))
end
function transpose(A::Hermitian)
    AT = transpose(A.data)
    return Hermitian(AT, ifelse(A.uplo == 'U', :L, :U))
end
ctranspose(A::Hermitian) = A
trace(A::Hermitian) = real(trace(A.data))

#tril/triu
function tril(A::Hermitian, k::Integer=0)
    if A.uplo == 'U' && k <= 0
        return tril!(A.data',k)
    elseif A.uplo == 'U' && k > 0
        return tril!(A.data',-1) + tril!(triu(A.data),k)
    elseif A.uplo == 'L' && k <= 0
        return tril(A.data,k)
    else
        return tril(A.data,-1) + tril!(triu!(A.data'),k)
    end
end

function tril(A::Symmetric, k::Integer=0)
    if A.uplo == 'U' && k <= 0
        return tril!(A.data.',k)
    elseif A.uplo == 'U' && k > 0
        return tril!(A.data.',-1) + tril!(triu(A.data),k)
    elseif A.uplo == 'L' && k <= 0
        return tril(A.data,k)
    else
        return tril(A.data,-1) + tril!(triu!(A.data.'),k)
    end
end

function triu(A::Hermitian, k::Integer=0)
    if A.uplo == 'U' && k >= 0
        return triu(A.data,k)
    elseif A.uplo == 'U' && k < 0
        return triu(A.data,1) + triu!(tril!(A.data'),k)
    elseif A.uplo == 'L' && k >= 0
        return triu!(A.data',k)
    else
        return triu!(A.data',1) + triu!(tril(A.data),k)
    end
end

function triu(A::Symmetric, k::Integer=0)
    if A.uplo == 'U' && k >= 0
        return triu(A.data,k)
    elseif A.uplo == 'U' && k < 0
        return triu(A.data,1) + triu!(tril!(A.data.'),k)
    elseif A.uplo == 'L' && k >= 0
        return triu!(A.data.',k)
    else
        return triu!(A.data.',1) + triu!(tril(A.data),k)
    end
end

#Test whether a matrix is positive-definite
isposdef!(A::HermOrSym) = isposdef!(A.data, symbol(A.uplo))
isposdef{T}(A::HermOrSym{T}) = (S = typeof(sqrt(one(T))); isposdef!(S == T ? copy(A.data) : convert(AbstractMatrix{S}, A.data), symbol(A.uplo)))

## Matvec
A_mul_B!{T<:BlasFloat,S<:StridedMatrix}(y::StridedVector{T}, A::Symmetric{T,S}, x::StridedVector{T}) = BLAS.symv!(A.uplo, one(T), A.data, x, zero(T), y)
A_mul_B!{T<:BlasComplex,S<:StridedMatrix}(y::StridedVector{T}, A::Hermitian{T,S}, x::StridedVector{T}) = BLAS.hemv!(A.uplo, one(T), A.data, x, zero(T), y)
##Matmat
A_mul_B!{T<:BlasFloat,S<:StridedMatrix}(C::StridedMatrix{T}, A::Symmetric{T,S}, B::StridedMatrix{T}) = BLAS.symm!('L', A.uplo, one(T), A.data, B, zero(T), C)
A_mul_B!{T<:BlasFloat,S<:StridedMatrix}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Symmetric{T,S}) = BLAS.symm!('R', B.uplo, one(T), B.data, A, zero(T), C)
A_mul_B!{T<:BlasComplex,S<:StridedMatrix}(C::StridedMatrix{T}, A::Hermitian{T,S}, B::StridedMatrix{T}) = BLAS.hemm!('L', A.uplo, one(T), A.data, B, zero(T), C)
A_mul_B!{T<:BlasComplex,S<:StridedMatrix}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Hermitian{T,S}) = BLAS.hemm!('R', B.uplo, one(T), B.data, A, zero(T), C)

*(A::HermOrSym, B::HermOrSym) = full(A)*full(B)
*(A::StridedMatrix, B::HermOrSym) = A*full(B)

bkfact(A::HermOrSym) = bkfact(A.data, symbol(A.uplo), issym(A))
factorize(A::HermOrSym) = bkfact(A)

# Is just RealHermSymComplexHerm, but type alias seems to be broken
det{T<:Real,S}(A::Union{Hermitian{T,S}, Symmetric{T,S}, Hermitian{Complex{T},S}}) = real(det(bkfact(A)))
det{T<:Real}(A::Symmetric{T}) = det(bkfact(A))
det(A::Symmetric) = det(bkfact(A))

\{T,S<:StridedMatrix}(A::HermOrSym{T,S}, B::StridedVecOrMat) = \(bkfact(A.data, symbol(A.uplo), issym(A)), B)

inv{T<:BlasFloat,S<:StridedMatrix}(A::Hermitian{T,S}) = Hermitian{T,S}(inv(bkfact(A.data, symbol(A.uplo))), A.uplo)
inv{T<:BlasFloat,S<:StridedMatrix}(A::Symmetric{T,S}) = Symmetric{T,S}(inv(bkfact(A.data, symbol(A.uplo), true)), A.uplo)

eigfact!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}) = Eigen(LAPACK.syevr!('V', 'A', A.uplo, A.data, 0.0, 0.0, 0, 0, -1.0)...)
# Because of #6721 it is necessary to specify the parameters explicitly here.
eigfact{T1<:Real,T2}(A::RealHermSymComplexHerm{T1,T2}) = (T = eltype(A); S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigfact!(S != T ? convert(AbstractMatrix{S}, A) : copy(A)))

eigfact!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}, irange::UnitRange) = Eigen(LAPACK.syevr!('V', 'I', A.uplo, A.data, 0.0, 0.0, irange.start, irange.stop, -1.0)...)
# Because of #6721 it is necessary to specify the parameters explicitly here.
eigfact{T1<:Real,T2}(A::RealHermSymComplexHerm{T1,T2}, irange::UnitRange) = (T = eltype(A); S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigfact!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), irange))

eigfact!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}, vl::Real, vh::Real) = Eigen(LAPACK.syevr!('V', 'V', A.uplo, A.data, convert(T, vl), convert(T, vh), 0, 0, -1.0)...)
# Because of #6721 it is necessary to specify the parameters explicitly here.
eigfact{T1<:Real,T2}(A::RealHermSymComplexHerm{T1,T2}, vl::Real, vh::Real) = (T = eltype(A); S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigfact!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), vl, vh))

function eig{T<:Real,S}(A::Union{Hermitian{T,S}, Symmetric{T,S}, Hermitian{Complex{T},S}}, args...)
    F = eigfact(A, args...)
    return F.values, F.vectors
end

eigvals!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}) = LAPACK.syevr!('N', 'A', A.uplo, A.data, 0.0, 0.0, 0, 0, -1.0)[1]
# Because of #6721 it is necessary to specify the parameters explicitly here.
eigvals{T1<:Real,T2}(A::RealHermSymComplexHerm{T1,T2}) = (T = eltype(A); S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigvals!(S != T ? convert(AbstractMatrix{S}, A) : copy(A)))

eigvals!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}, irange::UnitRange) = LAPACK.syevr!('N', 'I', A.uplo, A.data, 0.0, 0.0, irange.start, irange.stop, -1.0)[1]
# Because of #6721 it is necessary to specify the parameters explicitly here.
eigvals{T1<:Real,T2}(A::RealHermSymComplexHerm{T1,T2}, irange::UnitRange) = (T = eltype(A); S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigvals!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), irange))

eigvals!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}, vl::Real, vh::Real) = LAPACK.syevr!('N', 'V', A.uplo, A.data, convert(T, vl), convert(T, vh), 0, 0, -1.0)[1]
# Because of #6721 it is necessary to specify the parameters explicitly here.
eigvals{T1<:Real,T2}(A::RealHermSymComplexHerm{T1,T2}, vl::Real, vh::Real) = (T = eltype(A); S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigvals!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), vl, vh))

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

function svdvals!{T<:Real,S}(A::Union{Hermitian{T,S}, Symmetric{T,S}, Hermitian{Complex{T},S}}) #  the union is the same as RealHermSymComplexHerm, but right now parametric typealiases are broken
    vals = eigvals!(A)
    for i = 1:length(vals)
        vals[i] = abs(vals[i])
    end
    return sort!(vals, rev = true)
end

#Matrix-valued functions
function expm(A::Symmetric)
    F = eigfact(A)
    return Symmetric((F.vectors * Diagonal(exp(F.values))) * F.vectors')
end
function expm{T}(A::Hermitian{T})
    n = chksquare(A)
    F = eigfact(A)
    retmat = (F.vectors * Diagonal(exp(F.values))) * F.vectors'
    if T <: Real
        return real(Hermitian(retmat))
    else
        for i = 1:n
            retmat[i,i] = real(retmat[i,i])
        end
        return Hermitian(retmat)
    end
end

for (funm, func) in ([:logm,:log], [:sqrtm,:sqrt])

    @eval begin

        function ($funm)(A::Symmetric)
            F = eigfact(A)
            if isposdef(F)
                retmat = (F.vectors * Diagonal(($func)(F.values))) * F.vectors'
            else
                retmat = (F.vectors * Diagonal(($func)(complex(F.values)))) * F.vectors'
            end
            return Symmetric(retmat)
        end

        function ($funm){T}(A::Hermitian{T})
            n = chksquare(A)
            F = eigfact(A)
            if isposdef(F)
                retmat = (F.vectors * Diagonal(($func)(F.values))) * F.vectors'
                if T <: Real
                    return Hermitian(retmat)
                else
                    for i = 1:n
                        retmat[i,i] = real(retmat[i,i])
                    end
                    return Hermitian(retmat)
                end
            else
                retmat = (F.vectors * Diagonal(($func)(complex(F.values)))) * F.vectors'
                return retmat
            end
        end

    end

end
