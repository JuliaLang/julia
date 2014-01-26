#Symmetric and Hermitian matrices

for ty in (:Hermitian, :Symmetric)
    @eval begin
        type $ty{T} <: AbstractMatrix{T}
            S::Matrix{T}
            uplo::Char
        end
        function $ty(S::Matrix, uplo::Symbol=:U)
            chksquare(S)
            $ty(S, string(uplo)[1])
        end

        function copy!(A::$ty, B::$ty)
            copy!(A.S, B.S)
            A.uplo = B.uplo
            A
        end
        similar(A::$ty, args...) = $ty(similar(A.S, args...), A.uplo)
    end
end

typealias HermOrSym Union(Hermitian, Symmetric)

size(A::HermOrSym, args...) = size(A.S, args...)
getindex(A::HermOrSym, i::Integer, j::Integer) = (A.uplo == 'U') == (i < j) ? getindex(A.S, i, j) : conj(getindex(A.S, j, i))
full(A::Hermitian) = copytri!(A.S, A.uplo, true)
full(A::Symmetric) = copytri!(A.S, A.uplo)
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

factorize!(A::HermOrSym) = bkfact!(A.S, symbol(A.uplo), issym(A))
\(A::HermOrSym, B::StridedVecOrMat) = \(bkfact(A.S, symbol(A.uplo), issym(A)), B)

# eigvals!(A::HermOrSym, args...) = eigvals!(float(A), args...)
eigvals!(A::HermOrSym) = eigvals!(A, 1, size(A, 1))
eigmax(A::HermOrSym) = eigvals(A, size(A, 1), size(A, 1))[1]
eigmin(A::HermOrSym) = eigvals(A, 1, 1)[1]
eigfact(A::HermOrSym) = eigfact!(copy(A))

for (elty, ty) in ((:BlasFloat, :Hermitian), (:BlasReal, :Symmetric))
    @eval begin
        eigfact!{T<:$elty }(A::$ty{T}) = Eigen(LAPACK.syevr!('V', 'A', A.uplo, A.S, 0.0, 0.0, 0, 0, -1.0)...)
        eigvals!{T<:$elty}(A::$ty{T}, il::Int, ih::Int) = LAPACK.syevr!('N', 'I', A.uplo, A.S, 0.0, 0.0, il, ih, -1.0)[1]
        eigvals!{T<:$elty}(A::$ty{T}, vl::Real, vh::Real) = LAPACK.syevr!('N', 'V', A.uplo, A.S, vl, vh, 0, 0, -1.0)[1]
        function eigfact!{T<:$elty}(A::$ty{T}, B::$ty{T})
            vals, vecs, _ = LAPACK.sygvd!(1, 'V', A.uplo, A.S, B.uplo == A.uplo ? B.S : B.S')
            GeneralizedEigen(vals, vecs)
        end
        eigfact(A::$ty, B::$ty) = eigfact!(copy(A), copy(B))
        eigvals!{T<:$elty}(A::$ty{T}, B::$ty{T}) = LAPACK.sygvd!(1, 'N', A.uplo, A.S, B.uplo == A.uplo ? B.S : B.S')[1]
    end
end

#Matrix-valued functions
for (elty, ty) in ((:Any, :Hermitian), (:Real, :Symmetric))
    @eval begin
        function expm{T<:$elty}(A::$ty{T})
            F = eigfact(A)
            F.vectors * Diagonal(exp(F.values)) * F.vectors'
        end

        function sqrtm{T<:$elty}(A::$ty{T}, cond::Bool=false)
            F = eigfact(A)
            retmat = F.vectors*Diagonal((isposdef(F) ? sqrt : x->sqrt(complex(x)))(F.values))*F.vectors'
            return cond ? (retmat, norm(vsqrt, Inf)^2/norm(F[:values], Inf)) : retmat
        end
    end
end