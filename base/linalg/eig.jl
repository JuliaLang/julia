# This file is a part of Julia. License is MIT: http://julialang.org/license

#######################
# Eigendecompositions #
#######################

# Types
## Normal Eigenvalue problem
immutable Eigen{T,V,S<:Union(Number,AbstractMatrix),U<:Union(Number,Diagonal)} <: Factorization{T}
    values::U
    vectors::S
end
Eigen{T,V}(values::AbstractVector{V}, vectors::AbstractMatrix{T}) = Eigen{T,V,typeof(vectors),Diagonal{eltype(values)}}(Diagonal(values), vectors)
Eigen(value::Number, vector::Number) = Eigen{typeof(vector),typeof(value),typeof(vector),typeof(value)}(value, vector)

## Generalized eigenvalue problem.
immutable GeneralizedEigen{T,V,S<:Union(Number,AbstractMatrix),U<:Union(Number,Diagonal)} <: Factorization{T}
    values::U
    vectors::S
end
GeneralizedEigen{T,V}(values::AbstractVector{V}, vectors::AbstractMatrix{T}) = GeneralizedEigen{T,V,typeof(vectors),Diagonal{eltype(values)}}(Diagonal(values), vectors)
GeneralizedEigen(value::Number, vector::Number) = GeneralizedEigen{typeof(vector),typeof(value),typeof(vector),typeof(value)}(value, vector)

function getindex(A::Union(Eigen,GeneralizedEigen), d::Symbol)
    d == :values && return A.values
    d == :vectors && return A.vectors
    throw(KeyError(d))
end

# Should be made more efficient for self adjoint problems
full(F::Eigen) = F.vectors * F.values / F.vectors

isposdef(A::Union(Eigen,GeneralizedEigen)) = all(A.values .> 0)

inv(A::Eigen) = A.vectors/A.values*A.vectors'
det(A::Eigen) = det(A.values)

## Methods
### StridedMatrix and Number
function eigfact!{T<:BlasReal}(A::StridedMatrix{T}; permute::Bool=true, scale::Bool=true)
    n = size(A, 2)
    n==0 && return Eigen(zeros(T, 0), zeros(T, 0, 0))
    issym(A) && return eigfact!(Symmetric(A))
    A, WR, WI, VL, VR, _ = LAPACK.geevx!(permute ? (scale ? 'B' : 'P') : (scale ? 'S' : 'N'), 'N', 'V', 'N', A)
    all(WI .== 0.) && return Eigen(WR, VR)
    evec = zeros(Complex{T}, n, n)
    j = 1
    while j <= n
        if WI[j] == 0.0
            evec[:,j] = VR[:,j]
        else
            evec[:,j]   = VR[:,j] + im*VR[:,j+1]
            evec[:,j+1] = VR[:,j] - im*VR[:,j+1]
            j += 1
        end
        j += 1
    end
    return Eigen(complex(WR, WI), evec)
end

function eigfact!{T<:BlasComplex}(A::StridedMatrix{T}; permute::Bool=true, scale::Bool=true)
    n = size(A, 2)
    n == 0 && return Eigen(zeros(T, 0), zeros(T, 0, 0))
    ishermitian(A) && return eigfact!(Hermitian(A))
    return Eigen(LAPACK.geevx!(permute ? (scale ? 'B' : 'P') : (scale ? 'S' : 'N'), 'N', 'V', 'N', A)[[2,4]]...)
end
function eigfact{T}(A::StridedMatrix{T}; permute::Bool=true, scale::Bool=true)
    S = promote_type(Float32, typeof(one(T)/norm(one(T))))
    eigfact!(copy_oftype(A, S), permute = permute, scale = scale)
end
eigfact(x::Number) = Eigen(x, one(x))

# function eig(A::Union(Number, AbstractMatrix); permute::Bool=true, scale::Bool=true)
#     F = eigfact(A, permute=permute, scale=scale)
#     F[:values], F[:vectors]
# end
function eig(A::Union(Number, AbstractMatrix), args...; kwargs...)
    F = eigfact(A, args...; kwargs...)
    F[:values], F[:vectors]
end

eigvecs(A::Union(Number, AbstractMatrix), args...; kwargs...) = eigfact(A, args...; kwargs...)[:vectors]

function eigvals!{T<:BlasReal}(A::StridedMatrix{T}; permute::Bool=true, scale::Bool=true)
    issym(A) && return eigvals!(Symmetric(A))
    _, valsre, valsim, _ = LAPACK.geevx!(permute ? (scale ? 'B' : 'P') : (scale ? 'S' : 'N'), 'N', 'N', 'N', A)
    return all(valsim .== 0) ? valsre : complex(valsre, valsim)
end
function eigvals!{T<:BlasComplex}(A::StridedMatrix{T}; permute::Bool=true, scale::Bool=true)
    ishermitian(A) && return eigvals(Hermitian(A))
    return LAPACK.geevx!(permute ? (scale ? 'B' : 'P') : (scale ? 'S' : 'N'), 'N', 'N', 'N', A)[2]
end
function eigvals{T}(A::StridedMatrix{T}; permute::Bool=true, scale::Bool=true)
    S = promote_type(Float32, typeof(one(T)/norm(one(T))))
    return eigvals!(copy_oftype(A, S), permute = permute, scale = scale)
end
function eigvals{T<:Number}(x::T; kwargs...)
    val = convert(promote_type(Float32, typeof(one(T)/norm(one(T)))), x)
    return imag(val) == 0 ? [real(val)] : [val]
end

function eigmax(A::Union(Number, StridedMatrix); permute::Bool=true, scale::Bool=true)
    v = eigvals(A, permute = permute, scale = scale)
    iseltype(v,Complex) ? error("DomainError: complex eigenvalues cannot be ordered") : maximum(v)
end
function eigmin(A::Union(Number, StridedMatrix); permute::Bool=true, scale::Bool=true)
    v = eigvals(A, permute = permute, scale = scale)
    iseltype(v,Complex) ? error("DomainError: complex eigenvalues cannot be ordered") : minimum(v)
end

# Generalized eigenproblem
function eigfact!{T<:BlasReal}(A::StridedMatrix{T}, B::StridedMatrix{T})
    issym(A) && isposdef(B) && return eigfact!(Symmetric(A), Symmetric(B))
    n = size(A, 1)
    alphar, alphai, beta, _, vr = LAPACK.ggev!('N', 'V', A, B)
    all(alphai .== 0) && return GeneralizedEigen(alphar ./ beta, vr)

    vecs = zeros(Complex{T}, n, n)
    j = 1
    while j <= n
        if alphai[j] == 0.0
            vecs[:,j] = vr[:,j]
        else
            vecs[:,j  ] = vr[:,j] + im*vr[:,j+1]
            vecs[:,j+1] = vr[:,j] - im*vr[:,j+1]
            j += 1
        end
        j += 1
    end
    return GeneralizedEigen(complex(alphar, alphai)./beta, vecs)
end

function eigfact!{T<:BlasComplex}(A::StridedMatrix{T}, B::StridedMatrix{T})
    ishermitian(A) && isposdef(B) && return eigfact!(Hermitian(A), Hermitian(B))
    alpha, beta, _, vr = LAPACK.ggev!('N', 'V', A, B)
    return GeneralizedEigen(alpha./beta, vr)
end
function eigfact{TA,TB}(A::AbstractMatrix{TA}, B::AbstractMatrix{TB})
    S = promote_type(Float32, typeof(one(TA)/norm(one(TA))),TB)
    return eigfact!(copy_oftype(A, S), copy_oftype(B, S))
end

function eigvals!{T<:BlasReal}(A::StridedMatrix{T}, B::StridedMatrix{T})
    issym(A) && isposdef(B) && return eigvals!(Symmetric(A), Symmetric(B))
    alphar, alphai, beta, vl, vr = LAPACK.ggev!('N', 'N', A, B)
    return (all(alphai .== 0) ? alphar : complex(alphar, alphai))./beta
end
function eigvals!{T<:BlasComplex}(A::StridedMatrix{T}, B::StridedMatrix{T})
    ishermitian(A) && isposdef(B) && return eigvals!(Hermitian(A), Hermitian(B))
    alpha, beta, vl, vr = LAPACK.ggev!('N', 'N', A, B)
    alpha./beta
end
function eigvals{TA,TB}(A::AbstractMatrix{TA}, B::AbstractMatrix{TB})
    S = promote_type(Float32, typeof(one(TA)/norm(one(TA))),TB)
    return eigvals!(copy_oftype(A, S), copy_oftype(B, S))
end

### Triangular Matrix
eigvals(A::AbstractTriangular) = diag(A)
function eigvecs{T}(A::AbstractTriangular{T})
    TT = promote_type(T, Float32)
    TT <: BlasFloat && return eigvecs(convert(AbstractMatrix{TT}, A))
    throw(ArgumentError("eigvecs type $(typeof(A)) not supported. Please submit a pull request."))
end

#### Notice that trecv works for quasi-triangular matrices and therefore the lower sub diagonal must be zeroed before calling the subroutine
eigvecs{T<:BlasFloat,S<:StridedMatrix}(A::UpperTriangular{T,S}) = LAPACK.trevc!('R', 'A', BlasInt[], triu!(A.data))
eigvecs{T<:BlasFloat,S<:StridedMatrix}(A::UnitUpperTriangular{T,S}) = (for i = 1:size(A, 1); A.data[i,i] = 1;end;LAPACK.trevc!('R', 'A', BlasInt[], triu!(A.data)))
eigvecs{T<:BlasFloat,S<:StridedMatrix}(A::LowerTriangular{T,S}) = LAPACK.trevc!('L', 'A', BlasInt[], tril!(A.data)')
eigvecs{T<:BlasFloat,S<:StridedMatrix}(A::UnitLowerTriangular{T,S}) = (for i = 1:size(A, 1); A.data[i,i] = 1;end;LAPACK.trevc!('L', 'A', BlasInt[], tril!(A.data)'))

eigfact(A::AbstractTriangular) = Eigen(eigvals(A), eigvecs(A))

### Diagonal Matrix
eigvals{T<:Number}(D::Diagonal{T}) = sort(D.diag)
eigvals{T}(D::Diagonal{T}) = sort!(T[T[eigvals(x) for x in D.diag]...;]) #For block matrices, etc.
function eig{T<:Real}(D::Diagonal{T})
    p = sortperm(D.diag)
    Diagonal(D.diag[p]), eye(length(D.diag))[:,p]
end
eig{T<:Number}(D::Diagonal{T}) = Diagonal(D.diag), eye(length(D.diag))
function eig{T<:AbstractMatrix}(D::Diagonal{T})
    e = NTuple{2,T}[eig(d) for d in D.diag]
    return Diagonal(T[ee[1] for ee in e]), Diagonal(T[ee[2] for ee in e])
end
function eigfact(D::Diagonal)
    Λ, V = eig(D)
    return Eigen(diag(Λ), V)
end

### Tridiagonal Matrix
eigfact!{T<:BlasReal}(A::SymTridiagonal{T}) = Eigen(LAPACK.stegr!('V', A.dv, A.ev)...)
function eigfact{T}(A::SymTridiagonal{T})
    S = promote_type(Float32, typeof(zero(T)/norm(one(T))))
    eigfact!(copy_oftype(A, S))
end

eigfact!{T<:BlasReal}(A::SymTridiagonal{T}, irange::UnitRange) = Eigen(LAPACK.stegr!('V', 'I', A.dv, A.ev, 0.0, 0.0, irange.start, irange.stop)...)
eigfact{T}(A::SymTridiagonal{T}, irange::UnitRange) = (S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigfact!(S != T ? convert(SymTridiagonal{S}, A) : copy(A), irange))

eigfact!{T<:BlasReal}(A::SymTridiagonal{T}, vl::Real, vu::Real) = Eigen(LAPACK.stegr!('V', A.dv, A.ev, convert(T, vl), convert(T, vu), 0, 0)...)
eigfact{T}(A::SymTridiagonal{T}, vl::Real, vu::Real) = (S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigfact!(S != T ? convert(SymTridiagonal{S}, A) : copy(A), vl, vu))

eigvals!{T<:BlasReal}(A::SymTridiagonal{T}) = LAPACK.stev!('N', A.dv, A.ev)[1]
eigvals{T}(A::SymTridiagonal{T}) = (S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigvals!(S != T ? convert(SymTridiagonal{S}, A) : copy(A)))

eigvals!{T<:BlasReal}(A::SymTridiagonal{T}, irange::UnitRange) = LAPACK.stegr!('N', 'I', A.dv, A.ev, 0.0, 0.0, irange.start, irange.stop)[1]
eigvals{T}(A::SymTridiagonal{T}, irange::UnitRange) = (S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigvals!(S != T ? convert(SymTridiagonal{S}, A) : copy(A), irange))

eigvals!{T<:BlasReal}(A::SymTridiagonal{T}, vl::Real, vu::Real) = LAPACK.stegr!('N', 'V', A.dv, A.ev, vl, vu, 0, 0)[1]
eigvals{T}(A::SymTridiagonal{T}, vl::Real, vu::Real) = (S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigvals!(S != T ? convert(SymTridiagonal{S}, A) : copy(A), vl, vu))

eigmax(A::SymTridiagonal) = eigvals(A, size(A, 1):size(A, 1))[1]
eigmin(A::SymTridiagonal) = eigvals(A, 1:1)[1]

eigvecs(A::SymTridiagonal) = eigfact(A)[:vectors]
eigvecs{T<:BlasFloat,Eigenvalue<:Real}(A::SymTridiagonal{T}, eigvals::Vector{Eigenvalue}) = LAPACK.stein!(A.dv, A.ev, eigvals)


### Bidiagonal Matrix
# Eigensystems
eigvals(M::Bidiagonal) = M.dv
function eigvecs{T}(M::Bidiagonal{T})
    n = length(M.dv)
    Q = Array(T, n, n)
    blks = [0; find(x -> x == 0, M.ev); n]
    if M.isupper
        for idx_block = 1:length(blks) - 1, i = blks[idx_block] + 1:blks[idx_block + 1] #index of eigenvector
            v=zeros(T, n)
            v[blks[idx_block] + 1] = one(T)
            for j = blks[idx_block] + 1:i - 1 #Starting from j=i, eigenvector elements will be 0
                v[j+1] = (M.dv[i] - M.dv[j])/M.ev[j] * v[j]
            end
            Q[:, i] = v/norm(v)
        end
    else
        for idx_block = 1:length(blks) - 1, i = blks[idx_block + 1]:-1:blks[idx_block] + 1 #index of eigenvector
            v = zeros(T, n)
            v[blks[idx_block+1]] = one(T)
            for j = (blks[idx_block+1] - 1):-1:max(1, (i - 1)) #Starting from j=i, eigenvector elements will be 0
                v[j] = (M.dv[i] - M.dv[j+1])/M.ev[j] * v[j+1]
            end
            Q[:,i] = v/norm(v)
        end
    end
    Q #Actually Triangular
end
eigfact(M::Bidiagonal) = Eigen(eigvals(M), eigvecs(M))



### Symmetric/Hermitian
eigfact!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}) = Eigen(LAPACK.syevr!('V', 'A', A.uplo, A.data, 0.0, 0.0, 0, 0, -1.0)...)
# Because of #6721 it is necessary to specify the parameters explicitly here.
eigfact{T1<:Real,T2}(A::RealHermSymComplexHerm{T1,T2}) = (T = eltype(A); S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigfact!(S != T ? convert(AbstractMatrix{S}, A) : copy(A)))

eigfact!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}, irange::UnitRange) = Eigen(LAPACK.syevr!('V', 'I', A.uplo, A.data, 0.0, 0.0, irange.start, irange.stop, -1.0)...)
# Because of #6721 it is necessary to specify the parameters explicitly here.
eigfact{T1<:Real,T2}(A::RealHermSymComplexHerm{T1,T2}, irange::UnitRange) = (T = eltype(A); S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigfact!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), irange))

eigfact!{T<:BlasReal,S<:StridedMatrix}(A::RealHermSymComplexHerm{T,S}, vl::Real, vh::Real) = Eigen(LAPACK.syevr!('V', 'V', A.uplo, A.data, convert(T, vl), convert(T, vh), 0, 0, -1.0)...)
# Because of #6721 it is necessary to specify the parameters explicitly here.
eigfact{T1<:Real,T2}(A::RealHermSymComplexHerm{T1,T2}, vl::Real, vh::Real) = (T = eltype(A); S = promote_type(Float32, typeof(zero(T)/norm(one(T)))); eigfact!(S != T ? convert(AbstractMatrix{S}, A) : copy(A), vl, vh))

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
