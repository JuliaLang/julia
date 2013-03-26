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

cholfact!(A::StridedMatrix, uplo::Symbol) = CholeskyDense(A, string(uplo)[1])
cholfact(A::StridedMatrix, uplo::Symbol) = cholfact!(copy(A), uplo)
cholfact!(A::StridedMatrix) = cholfact!(A, :U)
cholfact(A::StridedMatrix) = cholfact(A, :U)
cholfact{T<:Integer}(A::StridedMatrix{T}, args...) = cholfact(float(A), args...)
cholfact(x::Number) = imag(x) == 0 && real(x) > 0 ? sqrt(x) : error("Argument not positive-definite")

chol(A::Union(Number, StridedMatrix), uplo::Symbol) = cholfact(A, uplo)[uplo]
chol(A::Union(Number, StridedMatrix)) = cholfact(A, :U)[:U]

size(C::CholeskyDense) = size(C.UL)
size(C::CholeskyDense,d::Integer) = size(C.UL,d)

function getindex(C::CholeskyDense, d::Symbol)
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
function CholeskyPivotedDense{T<:BlasFloat}(A::StridedMatrix{T}, uplo::Char, tol::Real)
    A, piv, rank, info = LAPACK.pstrf!(uplo, A, tol)
    CholeskyPivotedDense{T}(uplo == 'U' ? triu!(A) : tril!(A), uplo, piv, rank, tol, info)
end

cholpfact!(A::StridedMatrix, uplo::Symbol, tol::Real) = CholeskyPivotedDense(A, string(uplo)[1], tol)
cholpfact(A::StridedMatrix, uplo::Symbol, tol::Real) = cholpfact!(copy(A), uplo, tol)
cholpfact!(A::StridedMatrix, tol::Real) = cholpfact!(A, :U, tol)
cholpfact(A::StridedMatrix, tol::Real) = cholpfact(A, :U, tol)
cholpfact!(A::StridedMatrix) = cholpfact!(A, -1.)
cholpfact(A::StridedMatrix) = cholpfact(A, -1.)
cholpfact{T<:Int}(A::StridedMatrix{T}, args...) = cholpfact(float(A), args...)

size(C::CholeskyPivotedDense) = size(C.UL)
size(C::CholeskyPivotedDense,d::Integer) = size(C.UL,d)

getindex(C::CholeskyPivotedDense) = C.UL, C.piv
function getindex{T<:BlasFloat}(C::CholeskyPivotedDense{T}, d::Symbol)
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
end
function LUDense{T<:BlasFloat}(A::StridedMatrix{T})
    LU, ipiv, info = LAPACK.getrf!(A)
    LUDense{T}(LU, ipiv, info)
end

lufact!(A::StridedMatrix) = LUDense(A)
lufact(A::StridedMatrix) = lufact!(copy(A))
lufact!{T<:Integer}(A::StridedMatrix{T}) = lufact!(float(A))
lufact{T<:Integer}(A::StridedMatrix{T}) = lufact(float(A))
lufact(x::Number) = (one(x), x, [1])

function lu(A::Union(Number, StridedMatrix))
    F = lufact(A)
    return (F[:L], F[:U], F[:P])
end

size(A::LUDense) = size(A.LU)
size(A::LUDense,n) = size(A.LU,n)

function getindex{T}(A::LUDense{T}, d::Symbol)
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
QRDense(A::StridedMatrix) = QRDense(LAPACK.geqrt3!(A)...)

qrfact!(A::StridedMatrix) = QRDense(A)
qrfact(A::StridedMatrix) = qrfact!(copy(A))
qrfact{T<:Integer}(A::StridedMatrix{T}) = qrfact(float(A))
qrfact(x::Number) = (one(x), x)

function qr(A::Union(Number, StridedMatrix), thin::Bool)
    F = qrfact(A)
    return (full(F[:Q], thin), F[:R])
end
qr(A::Union(Number, StridedMatrix)) = qr(A, true)

size(A::QRDense, args::Integer...) = size(A.vs, args...)

function getindex(A::QRDense, d::Symbol)
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
qrpfact!{T<:BlasFloat}(A::StridedMatrix{T}) = QRPivotedDense{T}(LAPACK.geqp3!(A)...)

qrpfact(A::StridedMatrix) = qrpfact!(copy(A))

function qrp(A::Union(Number, StridedMatrix), thin::Bool)
    F = qrpfact(A)
    return full(F[:Q], thin), F[:R], F[:P]
end
qrp(A::StridedMatrix) = qrp(A, false)

size(A::QRPivotedDense, args::Integer...) = size(A.hh, args...)

function getindex{T<:BlasFloat}(A::QRPivotedDense{T}, d::Symbol)
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
print_matrix(io::IO, A::QRDensePivotedQ) = print_matrix(io, full(A))

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
type HessenbergDense{T} <: Factorization{T}
    hh::Matrix{T}
    tau::Vector{T}
    function HessenbergDense(hh::Matrix{T}, tau::Vector{T})
        if size(hh, 1) != size(hh, 2) throw(LAPACK.DimensionMismatch("")) end
        return new(hh, tau)
    end
end
HessenbergDense{T<:BlasFloat}(hh::Matrix{T}, tau::Vector{T}) = HessenbergDense{T}(hh, tau)
HessenbergDense(A::StridedMatrix) = HessenbergDense(LAPACK.gehrd!(A)...)

hessfact!(A::StridedMatrix) = HessenbergDense(A)
hessfact(A::StridedMatrix)  = hessfact!(copy(A))

type HessenbergDenseQ{T} <: AbstractMatrix{T}
    hh::Matrix{T}
    tau::Vector{T}
end
HessenbergDenseQ(A::HessenbergDense) = HessenbergDenseQ(A.hh, A.tau)
size(A::HessenbergDenseQ, args...) = size(A.hh, args...)
getindex(A::HessenbergDenseQ, args...) = getindex(full(A), args...)

function getindex(A::HessenbergDense, d::Symbol)
    if d == :Q; return HessenbergDenseQ(A); end
    if d == :H; return triu(A.hh, -1); end
    error("No such property")
end

full(A::HessenbergDenseQ) = LAPACK.orghr!(1, size(A.hh, 1), copy(A.hh), A.tau)

# EigenDensevalues
type EigenDense{T} <: Factorization{T}
    values::Vector
    vectors::Matrix{T}
end

function getindex(A::EigenDense, d::Symbol)
    if d == :values return A.values end
    if d == :vectors return A.vectors end
    error("No such property")
end

function eigfact!{T<:BlasFloat}(A::StridedMatrix{T})
    n = size(A, 2)
    if n == 0; return EigenDense(zeros(T, 0), zeros(T, 0, 0)) end
    if ishermitian(A) return eigfact!(Hermitian(A)) end
    if iscomplex(A) return EigenDense(LAPACK.geev!('N', 'V', A)[[1,3]]...) end

    WR, WI, VL, VR = LAPACK.geev!('N', 'V', A)
    if all(WI .== 0.) return EigenDense(WR, VR) end
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
    return EigenDense(complex(WR, WI), evec)
end

eigfact(A::StridedMatrix) = eigfact!(copy(A))
eigfact{T<:Integer}(x::StridedMatrix{T}) = eigfact(float64(x))
eigfact(x::Number) = (x, one(x))

function eig(A::Union(Number, StridedMatrix))
    F = eigfact(A)
    return F[:values], F[:vectors]
end

function eigvals(A::StridedMatrix)
    if ishermitian(A) return eigvals(Hermitian(A)) end
    if iscomplex(A) return LAPACK.geev!('N', 'N', copy(A))[1] end
    valsre, valsim, _, _ = LAPACK.geev!('N', 'N', copy(A))
    if all(valsim .== 0) return valsre end
    return complex(valsre, valsim)
end

eigvals(x::Number) = 1.0

inv(A::EigenDense) = diagmm(A[:vectors], 1.0/A[:values])*A[:vectors]'
det(A::EigenDense) = prod(A[:values])

# SVD
type SVDDense{T,Tr} <: Factorization{T}
    U::Matrix{T}
    S::Vector{Tr}
    Vt::Matrix{T}
end
function svdfact!(A::StridedMatrix, thin::Bool)
    m,n = size(A)
    if m == 0 || n == 0
        u,s,vt = (eye(m, thin ? n : m), zeros(0), eye(n,n))
    else
        u,s,vt = LAPACK.gesdd!(thin ? 'S' : 'A', A)
    end
    return SVDDense(u,s,vt)
end
svdfact(A::StridedMatrix, thin::Bool) = svdfact!(copy(A), thin)
svdfact(a::Vector, thin::Bool) = svdfact(reshape(a, length(a), 1), thin)
svdfact(x::Number, thin::Bool) = (x==0?one(x):x/abs(x),abs(x),one(x))
svdfact(A::Union(Number, StridedVecOrMat)) = svdfact(A, false)

function svd(A::Union(Number, StridedVecOrMat), thin::Bool)
    F = svdfact(A, thin)
    return F[:U], F[:S], F[:V]
end
svd(A::Union(Number, StridedVecOrMat)) = svd(A, true)

function getindex(F::SVDDense, d::Symbol)
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

function svdfact!(A::StridedMatrix, B::StridedMatrix)
    U, V, Q, a, b, k, l, R = LAPACK.ggsvd!('U', 'V', 'Q', A, B)
    return GSVDDense(U, V, Q, a, b, int(k), int(l), R)
end

svdfact(A::StridedMatrix, B::StridedMatrix) = svdfact!(copy(A), copy(B))

function svd(A::StridedMatrix, B::StridedMatrix)
    F = svdfact(A, B)
    return F[:U], F[:V], F[:Q]*F[:R0]', F[:D1], F[:D2]
end

function getindex{T}(obj::GSVDDense{T}, d::Symbol)
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
