# This file is a part of Julia. License is MIT: http://julialang.org/license

#################################
# Singular Value Decompositions #
#################################
immutable SVD{ET,UVT<:Union(Number,AbstractMatrix),ST<:Union(Number,Diagonal)} <: Factorization{ET}
    U::UVT
    S::ST
    Vt::UVT
end
SVD{T}(U::AbstractMatrix{T}, S::Vector, Vt::AbstractMatrix{T}) = SVD{T,typeof(U),Diagonal{eltype(S)}}(U, Diagonal(S), Vt)
function SVD(u::Number, s::Number, v::Number)
    EL = promote_type(typeof(u), typeof(v))
    SVD{EL,EL,typeof(s)}(convert(EL, u), s, convert(EL, v))
end

full(F::SVD) = (F.U * F.S) * F.Vt

# Strided Matrix
function svdfact!{T<:BlasFloat}(A::StridedMatrix{T}; thin::Bool=true)
    m,n = size(A)
    if m == 0 || n == 0
        u, s, vt = (eye(T, m, thin ? n : m), real(zeros(T,0)), eye(T,n,n))
    else
        u, s, vt = LAPACK.gesdd!(thin ? 'S' : 'A', A)
    end
    SVD(u, s, vt)
end
function svdfact{T}(A::StridedVecOrMat{T};thin = true)
    S = promote_type(Float32, typeof(one(T)/norm(one(T))))
    svdfact!(copy_oftype(A, S), thin = thin)
end
svdfact(x::Number; thin::Bool=true) = SVD(one(x), abs(x), x == 0 ? one(x)/one(x) : x/abs(x))

function svd(A::Union(Number, AbstractArray); thin::Bool=true)
    F = svdfact(A, thin=thin)
    F.U, Diagonal(F.S), F.Vt'
end

function getindex(F::SVD, d::Symbol)
    d == :U && return F.U
    d == :S && return F.S
    d == :Vt && return F.Vt
    d == :V && return F.Vt'
    throw(KeyError(d))
end

svdvals!{T<:BlasFloat}(A::StridedMatrix{T}) = any([size(A)...].==0) ? zeros(T, 0) : LAPACK.gesdd!('N', A)[2]
svdvals{T<:BlasFloat}(A::AbstractMatrix{T}) = svdvals!(copy(A))
function svdvals{T}(A::AbstractMatrix{T})
    S = promote_type(Float32, typeof(one(T)/norm(one(T))))
    svdvals!(copy_oftype(A, S))
end
svdvals(x::Number) = abs(x)

# SVD least squares
function \{T<:BlasFloat}(A::SVD{T}, B::StridedVecOrMat{T})
    s = A.S.diag
    n = length(s)
    sinv = zeros(T, n)
    k = length(find(s .> eps(real(float(one(T))))*maximum(s)))
    sinv[1:k] = one(T) ./ s[1:k]
    A.Vt[1:k,:]' * (sinv[1:k] .* (A.U[:,1:k]' * B))
end

# Generalized svd
immutable GeneralizedSVD{T,S<:AbstractMatrix} <: Factorization{T}
    U::S
    V::S
    Q::S
    a::Vector
    b::Vector
    k::Int
    l::Int
    R::S
end
GeneralizedSVD{T}(U::AbstractMatrix{T}, V::AbstractMatrix{T}, Q::AbstractMatrix{T}, a::Vector, b::Vector, k::Int, l::Int, R::AbstractMatrix{T}) = GeneralizedSVD{T,typeof(U)}(U, V, Q, a, b, k, l, R)

function svdfact!{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T})
    U, V, Q, a, b, k, l, R = LAPACK.ggsvd!('U', 'V', 'Q', A, B)
    GeneralizedSVD(U, V, Q, a, b, Int(k), Int(l), R)
end
svdfact{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T}) = svdfact!(copy(A),copy(B))
function svdfact{TA,TB}(A::StridedMatrix{TA}, B::StridedMatrix{TB})
    S = promote_type(Float32, typeof(one(TA)/norm(one(TA))),TB)
    return svdfact!(copy_oftype(A, S), copy_oftype(B, S))
end

function svd(A::AbstractMatrix, B::AbstractMatrix)
    F = svdfact(A, B)
    F[:U], F[:V], F[:Q], F[:D1], F[:D2], F[:R0]
end

function getindex{T}(obj::GeneralizedSVD{T}, d::Symbol)
    d == :U && return obj.U
    d == :V && return obj.V
    d == :Q && return obj.Q
    (d == :alpha || d == :a) && return obj.a
    (d == :beta || d == :b) && return obj.b
    (d == :vals || d == :S) && return obj.a.diag[1:obj.k + obj.l] ./ obj.b.diag[1:obj.k + obj.l]
    if d == :D1
        m = size(obj.U, 1)
        if m - obj.k - obj.l >= 0
            return [eye(T, obj.k) zeros(T, obj.k, obj.l); zeros(T, obj.l, obj.k) diagm(obj.a[obj.k + 1:obj.k + obj.l]); zeros(T, m - obj.k - obj.l, obj.k + obj.l)]
        else
            return [eye(T, m, obj.k) [zeros(T, obj.k, m - obj.k); diagm(obj.a[obj.k + 1:m])] zeros(T, m, obj.k + obj.l - m)]
        end
    end
    if d == :D2
        m = size(obj.U, 1)
        p = size(obj.V, 1)
        if m - obj.k - obj.l >= 0
            return [zeros(T, obj.l, obj.k) diagm(obj.b[obj.k + 1:obj.k + obj.l]); zeros(T, p - obj.l, obj.k + obj.l)]
        else
            return [zeros(T, p, obj.k) [diagm(obj.b[obj.k + 1:m]); zeros(T, obj.k + p - m, m - obj.k)] [zeros(T, m - obj.k, obj.k + obj.l - m); eye(T, obj.k + p - m, obj.k + obj.l - m)]]
        end
    end
    d == :R && return obj.R
    if d == :R0
        n = size(obj.Q, 1)
        return [zeros(T, obj.k + obj.l, n - obj.k - obj.l) obj.R]
    end
    throw(KeyError(d))
end

function svdvals!{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T})
    _, _, _, a, b, k, l, _ = LAPACK.ggsvd!('N', 'N', 'N', A, B)
    a[1:k + l] ./ b[1:k + l]
end
svdvals{T<:BlasFloat}(A::StridedMatrix{T},B::StridedMatrix{T}) = svdvals!(copy(A),copy(B))
function svdvals{TA,TB}(A::StridedMatrix{TA}, B::StridedMatrix{TB})
    S = promote_type(Float32, typeof(one(TA)/norm(one(TA))), TB)
    return svdvals!(copy_oftype(A, S), copy_oftype(B, S))
end

## Bidiagonal matrix
svdvals!{T<:BlasReal}(M::Bidiagonal{T}) = LAPACK.bdsdc!(M.isupper ? 'U' : 'L', 'N', M.dv, M.ev)[1]
function svd{T<:BlasReal}(M::Bidiagonal{T})
    d, e, U, Vt, Q, iQ = LAPACK.bdsdc!(M.isupper ? 'U' : 'L', 'I', copy(M.dv), copy(M.ev))
    return U, Diagonal(d), Vt'
end
function svdfact!(M::Bidiagonal, thin::Bool=true)
    d, e, U, Vt, Q, iQ = LAPACK.bdsdc!(M.isupper ? 'U' : 'L', 'I', M.dv, M.ev)
    SVD(U, d, Vt)
end

# Diagonal matrix
svdvals{T<:Number}(D::Diagonal{T}) = sort(typeof(real(D.diag[1]))[svdvals(d) for d in D.diag], rev = true)
svdvals(D::Diagonal) = typeof(real(D.diag[1]))[svdvals(d) for d in D.diag] # We can only assume ordering when the elements are numbers
function svdfact{T<:Number}(D::Diagonal{T}, thin=true)
    n    = size(D, 1)
    vals = [svdvals(d) for d in D.diag]
    p    = sortperm(vals, rev = true)
    ip   = invperm(p)
    U    = eye(typeof(D.diag[1]/vals[1]), n)[:,invperm(p)]
    Vt   = [ip[i] == j ? vals[p[i]]\D.diag[p[i]] : zero(D.diag[1]) for i = 1:n, j = 1:n]
    SVD(U, vals[p], Vt)
end

# Triangular matrix
#Generic singular systems
for func in (:svd, :svdfact, :svdfact!, :svdvals)
    @eval begin
        ($func)(A::AbstractTriangular) = ($func)(full(A))
    end
end
