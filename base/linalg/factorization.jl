## Matrix factorizations and decompositions

abstract Factorization{T}

type CholeskyDense{T<:BlasFloat} <: Factorization{T}
    UL::Matrix{T}
    uplo::Char
    function CholeskyDense(A::Matrix{T}, uplo::Char)
        A, info = LAPACK.potrf!(uplo, A)
        if info > 0; throw(LAPACK.PosDefException(info)); end
        return new(uplo == 'U' ? triu!(A) : tril!(A), uplo)
    end
end
CholeskyDense{T<:BlasFloat}(A::Matrix{T}, uplo::Char) = CholeskyDense{T}(A, uplo)

chol(A::Matrix, uplo::Symbol) = CholeskyDense(copy(A), string(uplo)[1])
chol(A::Matrix) = chol(A, :U)
chol{T<:Integer}(A::Matrix{T}, args...) = chol(float64(A), args...)
chol(x::Number) = imag(x) == 0 && real(x) > 0 ? sqrt(x) : error("Argument not positive-definite")

size(C::CholeskyDense) = size(C.UL)
size(C::CholeskyDense,d::Integer) = size(C.UL,d)

function ref(C::CholeskyDense, d::Symbol)
    if d == :U || d == :L
        return symbol(C.uplo) == d ? C.UL : C.UL'
    elseif d == :UL
        return Triangular(C.UL, C.uplo)
    end
    error("No such property")
end

\{T<:BlasFloat}(C::CholeskyDense{T}, B::StridedVecOrMat{T}) =
    LAPACK.potrs!(C.uplo, C.UL, copy(B))

function det{T}(C::CholeskyDense{T})
    dd = one(T)
    for i in 1:size(C.UL,1) dd *= abs2(C.UL[i,i]) end
    dd
end
    
function inv(C::CholeskyDense)
    Ci, info = LAPACK.potri!(C.uplo, copy(C.UL))
    if info != 0; throw(LAPACK.SingularException(info)); end 
    symmetrize!(Ci, C.uplo)
end

## Pivoted Cholesky
type CholeskyPivotedDense{T<:BlasFloat} <: Factorization{T}
    UL::Matrix{T}
    uplo::Char
    piv::Vector{BlasInt}
    rank::BlasInt
    tol::Real
    info::BlasInt
end
function CholeskyPivotedDense{T<:BlasFloat}(A::Matrix{T}, uplo::Char, tol::Real)
    A, piv, rank, info = LAPACK.pstrf!(uplo, A, tol)
    CholeskyPivotedDense{T}(uplo == 'U' ? triu!(A) : tril!(A), uplo, piv, rank, tol, info)
end

cholp(A::Matrix, uplo::Symbol, tol::Real) = CholeskyPivotedDense(copy(A), string(uplo)[1], tol)
cholp(A::Matrix, tol::Real) = cholp(A, :U, tol)
cholp(A::Matrix) = cholp(A, -1.)
cholp{T<:Int}(A::Matrix{T}, args...) = cholp(float64(A), args...)

size(C::CholeskyPivotedDense) = size(C.UL)
size(C::CholeskyPivotedDense,d::Integer) = size(C.UL,d)

ref(C::CholeskyPivotedDense) = C.UL, C.piv
function ref{T<:BlasFloat}(C::CholeskyPivotedDense{T}, d::Symbol)
    if d == :U || d == :L
        return symbol(C.uplo) == d ? C.UL : C.UL'
    end
    if d == :p return C.piv end
    if d == :P
        n = size(C, 1)
        P = zeros(T, n, n)
        for i in 1:n
            P[C.piv[i],i] = one(T)
        end
        return P
    end
    error("No such property")
end

function \{T<:BlasFloat}(C::CholeskyPivotedDense{T}, B::StridedVector{T})
    if C.rank < size(C.UL, 1); throw(LAPACK.RankDeficientException(C.info)); end
    LAPACK.potrs!(C.uplo, C.UL, copy(B)[C.piv])[invperm(C.piv)]
end

function \{T<:BlasFloat}(C::CholeskyPivotedDense{T}, B::StridedMatrix{T})
    if C.rank < size(C.UL, 1); throw(LAPACK.RankDeficientException(C.info)); end
    LAPACK.potrs!(C.uplo, C.UL, copy(B)[C.piv,:])[invperm(C.piv),:]
end

rank(C::CholeskyPivotedDense) = C.rank

function det{T}(C::CholeskyPivotedDense{T})
    if C.rank < size(C.UL, 1) 
        return real(zero(T))
    else 
        return prod(abs2(diag(C.UL)))
    end
end
    
function inv(C::CholeskyPivotedDense)
    if C.rank < size(C.UL, 1) throw(LAPACK.RankDeficientException(C.info)) end
    Ci, info = LAPACK.potri!(C.uplo, copy(C.UL))
    if info != 0 throw(LAPACK.RankDeficientException(info)) end
    ipiv = invperm(C.piv)
    (symmetrize!(Ci, C.uplo))[ipiv, ipiv]
end

## LU
type LUDense{T} <: Factorization{T}
    LU::Matrix{T}
    ipiv::Vector{BlasInt}
    info::BlasInt
    function LUDense(LU::Matrix{T}, ipiv::Vector{BlasInt}, info::BlasInt)
        m, n = size(LU)
        m == n ? new(LU, ipiv, info) : throw(LAPACK.DimensionMismatch("LUDense only defined for square matrices"))
    end
end
function LUDense{T<:BlasFloat}(A::Matrix{T})
    LU, ipiv, info = LAPACK.getrf!(A)
    LUDense{T}(LU, ipiv, info)
end

lu(A::Matrix) = LUDense(copy(A))
lu{T<:Integer}(A::Matrix{T}) = lu(float(A))
lu(x::Number) = (one(x), x, [1])

size(A::LUDense) = size(A.LU)
size(A::LUDense,n) = size(A.LU,n)

function ref{T}(A::LUDense{T}, d::Symbol)
    if d == :L; return tril(A.LU, -1) + eye(T, size(A, 1)); end;
    if d == :U; return triu(A.LU); end;
    if d == :p
        n = size(A, 1)
        p = [1:n]
        for i in 1:n
            tmp = p[i]
            p[i] = p[A.ipiv[i]]
            p[A.ipiv[i]] = tmp
        end
        return p
    end
    if d == :P
        p = A[:p]
        n = length(p)
        P = zeros(T, n, n)
        for i in 1:n
            P[i,p[i]] = one(T)
        end
        return P
    end
    error("No such property")
end

function det{T}(A::LUDense{T})
    m, n = size(A)
    if A.info > 0; return zero(typeof(A.LU[1])); end
    prod(diag(A.LU)) * (bool(sum(A.ipiv .!= 1:n) % 2) ? -one(T) : one(T))
end

function (\)(A::LUDense, B::StridedVecOrMat)
    if A.info > 0; throw(LAPACK.SingularException(A.info)); end
    LAPACK.getrs!('N', A.LU, A.ipiv, copy(B))
end

function inv(A::LUDense)
    if A.info > 0; return throw(LAPACK.SingularException(A.info)); end
    LAPACK.getri!(copy(A.LU), A.ipiv)
end

## QR decomposition without column pivots. By the faster geqrt3
type QRDense{S} <: Factorization{S}
    vs::Matrix{S}                     # the elements on and above the diagonal contain the N-by-N upper triangular matrix R; the elements below the diagonal are the columns of V
    T::Matrix{S}                      # upper triangular factor of the block reflector.
end
QRDense(A::Matrix) = QRDense(LAPACK.geqrt3!(A)...)

qr(A::Matrix) = QRDense(copy(A))
qr{T<:Integer}(A::Matrix{T}) = qr(float(A))
qr(x::Number) = (one(x), x)

size(A::QRDense, args::Integer...) = size(A.vs, args...)

function ref(A::QRDense, d::Symbol)
    if d == :R; return triu(A.vs[1:min(size(A)),:]); end;
    if d == :Q; return QRDenseQ(A); end
    error("No such property")
end

type QRDenseQ{S}  <: AbstractMatrix{S} 
    vs::Matrix{S}                      
    T::Matrix{S}                       
end
QRDenseQ(A::QRDense) = QRDenseQ(A.vs, A.T)

size(A::QRDenseQ, args::Integer...) = size(A.vs, args...)

function full{T<:BlasFloat}(A::QRDenseQ{T}, thin::Bool)
    if thin return A * eye(T, size(A.T, 1)) end
    return A * eye(T, size(A, 1))
end
full(A::QRDenseQ) = full(A, true)

print_matrix(io::IO, A::QRDenseQ) = print_matrix(io, full(A))

## Multiplication by Q from the QR decomposition
function *{T<:BlasFloat}(A::QRDenseQ{T}, B::StridedVecOrMat{T})
    m = size(B, 1)
    n = size(B, 2)
    if m == size(A.vs, 1)
        Bc = copy(B)
    elseif m == size(A.vs, 2)
        Bc = [B; zeros(T, size(A.vs, 1) - m, n)]
    else
        throw(LAPACK.DimensionMismatch(""))
    end
    LAPACK.gemqrt!('L', 'N', A.vs, A.T, Bc)
end
Ac_mul_B(A::QRDenseQ, B::StridedVecOrMat) = LAPACK.gemqrt!('L', iscomplex(A.vs[1]) ? 'C' : 'T', A.vs, A.T, copy(B))
*(A::StridedVecOrMat, B::QRDenseQ) = LAPACK.gemqrt!('R', 'N', B.vs, B.T, copy(A))
function A_mul_Bc{T<:BlasFloat}(A::StridedVecOrMat{T}, B::QRDenseQ{T})
    m = size(A, 1)
    n = size(A, 2)
    if n == size(B.vs, 1)
        Ac = copy(A)
    elseif n == size(B.vs, 2)
        Ac = [B zeros(T, m, size(B.vs, 1) - n)]
    else
        throw(LAPACK.DimensionMismatch(""))
    end
    LAPACK.gemqrt!('R', iscomplex(B.vs[1]) ? 'C' : 'T', B.vs, B.T, Ac)
end
## Least squares solution.  Should be more careful about cases with m < n
(\)(A::QRDense, B::StridedVector) = Triangular(A[:R], 'U')\(A[:Q]'B)[1:size(A, 2)]
(\)(A::QRDense, B::StridedMatrix) = Triangular(A[:R], 'U')\(A[:Q]'B)[1:size(A, 2),:]

type QRPivotedDense{T} <: Factorization{T}
    hh::Matrix{T}
    tau::Vector{T}
    jpvt::Vector{BlasInt}
    function QRPivotedDense(hh::Matrix{T}, tau::Vector{T}, jpvt::Vector{BlasInt})
        m, n = size(hh)
        if length(tau) != min(m,n) || length(jpvt) != n
            throw(LAPACK.DimensionMismatch(""))
        end
        new(hh,tau,jpvt)
    end
end
QRPivotedDense{T<:BlasFloat}(A::Matrix{T}) = QRPivotedDense{T}(LAPACK.geqp3!(A)...)
qrp(A::Matrix) = QRPivotedDense(copy(A))
# QRDenseQ(A::QRPivotedDense) = QRDenseQ(A.hh, A.tau)

size(A::QRPivotedDense, args::Integer...) = size(A.hh, args...)

function ref{T<:BlasFloat}(A::QRPivotedDense{T}, d::Symbol)
    if d == :R; return triu(A.hh[1:min(size(A)),:]); end;
    if d == :Q; return QRDensePivotedQ(A); end
    if d == :p; return A.jpvt; end
    if d == :P
        p = A[:p]
        n = length(p)
        P = zeros(T, n, n)
        for i in 1:n
            P[p[i],i] = one(T)
        end
        return P
    end
    error("No such property")
end

(\)(A::QRPivotedDense, B::StridedVector) = (Triangular(A[:R])\(A[:Q]'B)[1:size(A, 2)])[invperm(A.jpvt)]
(\)(A::QRPivotedDense, B::StridedMatrix) = (Triangular(A[:R])\(A[:Q]'B)[1:size(A, 2),:])[invperm(A.jpvt),:]

type QRDensePivotedQ{T}  <: AbstractMatrix{T}
    hh::Matrix{T}                       # Householder transformations and R
    tau::Vector{T}                      # Scalar factors of transformations
end
QRDensePivotedQ(A::QRPivotedDense) = QRDensePivotedQ(A.hh, A.tau)

size(A::QRDensePivotedQ, args...) = size(A.hh, args...)

function full{T<:BlasFloat}(A::QRDensePivotedQ{T}, thin::Bool)
    if !thin
        Q = Array(T, size(A, 1), size(A, 1))
        Q[:,1:size(A, 2)] = copy(A.hh)
        return LAPACK.orgqr!(Q, A.tau)
    else
        return LAPACK.orgqr!(copy(A.hh), A.tau)
    end
end
full(A::QRDensePivotedQ) = full(A, true)

## Multiplication by Q from the Pivoted QR decomposition
function *{T<:BlasFloat}(A::QRDensePivotedQ{T}, B::StridedVecOrMat{T})
    m = size(B, 1)
    n = size(B, 2)
    if m == size(A.hh, 1)
        Bc = copy(B)
    elseif m == size(A.hh, 2)
        Bc = [B; zeros(T, size(A.hh, 1) - m, n)]
    else
        throw(LAPACK.DimensionMismatch(""))
    end
    LAPACK.ormqr!('L', 'N', A.hh, A.tau, Bc)
end
Ac_mul_B(A::QRDensePivotedQ, B::StridedVecOrMat) = LAPACK.ormqr!('L', iscomplex(A.hh[1]) ? 'C' : 'T', A.hh, A.tau, copy(B))
*(A::StridedVecOrMat, B::QRDensePivotedQ) = LAPACK.ormqr!('R', 'N', B.hh, B.tau, copy(A))
function A_mul_Bc{T<:BlasFloat}(A::StridedVecOrMat{T}, B::QRDensePivotedQ{T})
    m = size(A, 1)
    n = size(A, 2)
    if n == size(B.hh, 1)
        Ac = copy(A)
    elseif n == size(B.hh, 2)
        Ac = [B zeros(T, m, size(B.hh, 1) - n)]
    else
        throw(LAPACK.DimensionMismatch(""))
    end
    LAPACK.ormqr!('R', iscomplex(B.hh[1]) ? 'C' : 'T', B.hh, B.tau, Ac)
end

##TODO:  Add methods for rank(A::QRP{T}) and adjust the (\) method accordingly
##       Add rcond methods for Cholesky, LU, QR and QRP types
## Lower priority: Add LQ, QL and RQ factorizations

# FIXME! Should add balancing option through xgebal
type Hessenberg{T} <: Factorization{T}
    hh::Matrix{T}
    tau::Vector{T}
    function Hessenberg(hh::Matrix{T}, tau::Vector{T})
        if size(hh, 1) != size(hh, 2) throw(LAPACK.DimensionMismatch("")) end
        return new(hh, tau)
    end
end
Hessenberg{T<:BlasFloat}(hh::Matrix{T}, tau::Vector{T}) = Hessenberg{T}(hh, tau)
Hessenberg(A::StridedMatrix) = Hessenberg(LAPACK.gehrd!(A)...)

hess(A::StridedMatrix) = Hessenberg(copy(A))

type HessenbergQ{T} <: AbstractMatrix{T}
    hh::Matrix{T}
    tau::Vector{T}
end
HessenbergQ(A::Hessenberg) = HessenbergQ(A.hh, A.tau)
size(A::HessenbergQ, args...) = size(A.hh, args...)
ref(A::HessenbergQ, args...) = ref(full(A), args...)

function ref(A::Hessenberg, d::Symbol)
    if d == :Q; return HessenbergQ(A); end
    if d == :H; return triu(A.hh, -1); end
    error("No such property")
end

full(A::HessenbergQ) = LAPACK.orghr!(1, size(A.hh, 1), copy(A.hh), A.tau)

### Linear algebra for general matrices

function det(A::Matrix)
    m, n = size(A)
    if m != n; throw(LAPACK.DimensionMismatch("det only defined for square matrices")); end
    if istriu(A) | istril(A); return det(Triangular(A, 'U', false)); end
    return det(LUDense(copy(A)))
end
det(x::Number) = x

logdet(A::Matrix) = 2.0 * sum(log(diag(chol(A)[:U])))

function inv(A::StridedMatrix)
    if istriu(A) return inv(Triangular(A, 'U')) end
    if istril(A) return inv(Triangular(A, 'L')) end
    if ishermitian(A) return inv(Hermitian(A)) end
    return inv(LUDense(copy(A)))
end

function eig{T<:BlasFloat}(A::StridedMatrix{T})
    n = size(A, 2)
    if n == 0; return (zeros(T, 0), zeros(T, 0, 0)) end
    if ishermitian(A) return eig(Hermitian(A)) end
    if iscomplex(A) return LAPACK.geev!('N', 'V', copy(A))[[1,3]] end

    WR, WI, VL, VR = LAPACK.geev!('N', 'V', copy(A))
    if all(WI .== 0.) return WR, VR end
    evec = complex(zeros(T, n, n))
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
    return complex(WR, WI), evec
end

eig{T<:Integer}(x::StridedMatrix{T}) = eig(float64(x))
eig(x::Number) = (x, one(x))

function eigvals(A::StridedMatrix)
    if ishermitian(A) return eigvals(Hermitian(A)) end
    if iscomplex(A) return LAPACK.geev!('N', 'N', copy(A))[1] end
    valsre, valsim, _, _ = LAPACK.geev!('N', 'N', copy(A))
    if all(valsim .== 0) return valsre end
    return complex(valsre, valsim)
end

eigvals(x::Number) = 1.0

# SVD
type SVDDense{T,Tr} <: Factorization{T}
    U::Matrix{T}
    S::Vector{Tr}
    Vt::Matrix{T}
end
function SVDDense(A::StridedMatrix, thin::Bool)
    m,n = size(A)
    if m == 0 || n == 0
        u,s,vt = (eye(m, thin ? n : m), zeros(0), eye(n,n))
    else
        u,s,vt = LAPACK.gesdd!(thin ? 'S' : 'A', A)
    end
    return SVDDense(u,s,vt)
end
SVDDense(A::StridedMatrix) = SVDDense(A, false)
svd(A::StridedMatrix, args...) = SVDDense(copy(A), args...)
svd(x::Number, thin::Bool) = (x==0?one(x):x/abs(x),abs(x),one(x))

function ref(F::SVDDense, d::Symbol)
    if d == :U return F.U end
    if d == :S return F.S end
    if d == :Vt return F.Vt end
    if d == :V return F.Vt' end
    error("No such property")
end

function svdvals!{T<:BlasFloat}(A::StridedMatrix{T})
    m,n = size(A)
    if m == 0 || n == 0 return zeros(T, 0) end
    return LAPACK.gesdd!('N', A)[2]
end

svdvals(A) = svdvals!(copy(A))

# SVD least squares
function \{T<:BlasFloat}(A::SVDDense{T}, B::StridedVecOrMat{T})
    n = length(A[:S])
    Sinv = zeros(T, n)
    Sinv[A[:S] .> sqrt(eps())] = 1.0 ./ A[:S]
    return diagmm(A[:V], Sinv) * A[:U][:,1:n]'B
end

# Generalized svd
type GSVDDense{T} <: Factorization{T}
    U::Matrix{T}
    V::Matrix{T}
    Q::Matrix{T}
    a::Vector
    b::Vector
    k::Int
    l::Int
    R::Matrix{T}
end

function GSVDDense(A::StridedMatrix, B::StridedMatrix)
    U, V, Q, a, b, k, l, R = LAPACK.ggsvd!('U', 'V', 'Q', A, B)
    return GSVDDense(U, V, Q, a, b, int(k), int(l), R)
end

svd(A::StridedMatrix, B::StridedMatrix) = GSVDDense(copy(A), copy(B))

function ref{T}(obj::GSVDDense{T}, d::Symbol)
    if d == :U return obj.U end
    if d == :V return obj.V end
    if d == :Q return obj.Q end
    if d == :alpha || d == :a return obj.a end
    if d == :beta || d == :b return obj.b end
    if d == :vals || d == :S return obj.a[1:obj.k + obj.l] ./ obj.b[1:obj.k + obj.l] end
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
    if d == :R return obj.R end
    if d == :R0
        m = size(obj.U, 1)
        n = size(obj.Q, 1)
        if m - obj.k - obj.l >= 0
            return [zeros(T, obj.k + obj.l, n - obj.k - obj.l) obj.R]
        else
            return [zeros(T, obj.k + obj.l, n - obj.k - obj.l) obj.R]
        end
    end
    error("No such property")
end

function svdvals(A::StridedMatrix, B::StridedMatrix)
    _, _, _, a, b, k, l, _ = LAPACK.ggsvd!('N', 'N', 'N', copy(A), copy(B))
    return a[1:k + l] ./ b[1:k + l]
end

schur{T<:BlasFloat}(A::StridedMatrix{T}) = LAPACK.gees!('V', copy(A))

function (\){T<:BlasFloat}(A::StridedMatrix{T}, B::StridedVecOrMat{T})
    if size(A, 1) == size(A, 2) # Square
        if istriu(A) return Triangular(A, 'U')\B end
        if istril(A) return Triangular(A, 'L')\B end
        if ishermitian(A) return Hermitian(A)\B end
    end
    LAPACK.gelsd!(copy(A), copy(B))[1]
end

(\){T1<:BlasFloat, T2<:BlasFloat}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) =
    (\)(convert(Array{promote_type(T1,T2)},A), convert(Array{promote_type(T1,T2)},B))
(\){T1<:BlasFloat, T2<:Real}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(A, convert(Array{T1}, B))
(\){T1<:Real, T2<:BlasFloat}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(convert(Array{T2}, A), B)
(\){T1<:Real, T2<:Real}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(float64(A), float64(B))
(\){T1<:Number, T2<:Number}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(complex128(A), complex128(B))

(/)(A::StridedVecOrMat, B::StridedVecOrMat) = (B' \ A')'

## Moore-Penrose inverse
function pinv{T<:BlasFloat}(A::StridedMatrix{T})
    SVD         = SVDDense(copy(A), true)
    Sinv        = zeros(T, length(SVD[:S]))
    index       = SVD[:S] .> eps(real(one(T)))*max(size(A))*max(SVD[:S])
    Sinv[index] = 1.0 ./ SVD[:S][index]
    SVD[:Vt]'diagmm(Sinv, SVD[:U]')
end
pinv{T<:Integer}(A::StridedMatrix{T}) = pinv(float(A))
pinv(a::StridedVector) = pinv(reshape(a, length(a), 1))
pinv(x::Number) = one(x)/x

## Basis for null space
function null{T<:BlasFloat}(A::StridedMatrix{T})
    m,n = size(A)
    SVD = SVDDense(copy(A))
    if m == 0; return eye(T, n); end
    indstart = sum(SVD[:S] .> max(m,n)*max(SVD[:S])*eps(eltype(SVD[:S]))) + 1
    SVD[:V][:,indstart:]
end
null{T<:Integer}(A::StridedMatrix{T}) = null(float(A))
null(a::StridedVector) = null(reshape(a, length(a), 1))

function cond(A::StridedMatrix, p) 
    if p == 2
        v = svdvals(A)
        maxv = max(v)
        cnd = maxv == 0.0 ? Inf : maxv / min(v)
    elseif p == 1 || p == Inf
        m, n = size(A)
        if m != n; error("Use 2-norm for non-square matrices"); end
        cnd = 1 / LAPACK.gecon!(p == 1 ? '1' : 'I', LUDense(copy(A)).LU, norm(A, p))
    else
        error("Norm type must be 1, 2 or Inf")
    end
    return cnd
end
cond(A::StridedMatrix) = cond(A, 2)
