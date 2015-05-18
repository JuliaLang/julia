# This file is a part of Julia. License is MIT: http://julialang.org/license

# Eigendecomposition
immutable Eigen{T,V,S<:AbstractMatrix,U<:AbstractVector} <: Factorization{T}
    values::U
    vectors::S
    Eigen(values::AbstractVector{V}, vectors::AbstractMatrix{T}) = new(values, vectors)
end
Eigen{T,V}(values::AbstractVector{V}, vectors::AbstractMatrix{T}) = Eigen{T,V,typeof(vectors),typeof(values)}(values, vectors)

# Generalized eigenvalue problem.
immutable GeneralizedEigen{T,V,S<:AbstractMatrix,U<:AbstractVector} <: Factorization{T}
    values::U
    vectors::S
    GeneralizedEigen(values::AbstractVector{V}, vectors::AbstractMatrix{T}) = new(values, vectors)
end
GeneralizedEigen{T,V}(values::AbstractVector{V}, vectors::AbstractMatrix{T}) = GeneralizedEigen{T,V,typeof(vectors),typeof(values)}(values, vectors)


function getindex(A::Union(Eigen,GeneralizedEigen), d::Symbol)
    d == :values && return A.values
    d == :vectors && return A.vectors
    throw(KeyError(d))
end

isposdef(A::Union(Eigen,GeneralizedEigen)) = all(A.values .> 0)

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
eigfact(x::Number) = Eigen([x], fill(one(x), 1, 1))

# function eig(A::Union(Number, AbstractMatrix); permute::Bool=true, scale::Bool=true)
#     F = eigfact(A, permute=permute, scale=scale)
#     F[:values], F[:vectors]
# end
function eig(A::Union(Number, AbstractMatrix), args...; kwargs...)
    F = eigfact(A, args...; kwargs...)
    F[:values], F[:vectors]
end
#Calculates eigenvectors
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

#Computes maximum and minimum eigenvalue
function eigmax(A::Union(Number, StridedMatrix); permute::Bool=true, scale::Bool=true)
    v = eigvals(A, permute = permute, scale = scale)
    iseltype(v,Complex) ? error("DomainError: complex eigenvalues cannot be ordered") : maximum(v)
end
function eigmin(A::Union(Number, StridedMatrix); permute::Bool=true, scale::Bool=true)
    v = eigvals(A, permute = permute, scale = scale)
    iseltype(v,Complex) ? error("DomainError: complex eigenvalues cannot be ordered") : minimum(v)
end

inv(A::Eigen) = A.vectors/Diagonal(A.values)*A.vectors'
det(A::Eigen) = prod(A.values)

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

## Can we determine the source/result is Real?  This is not stored in the type Eigen
full(F::Eigen) = F.vectors * Diagonal(F.values) / F.vectors
