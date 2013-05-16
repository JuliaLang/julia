## Matrix factorizations and decompositions

abstract Factorization{T}

type Cholesky{T<:BlasFloat} <: Factorization{T}
    UL::Matrix{T}
    uplo::Char
end

function cholfact!{T<:BlasFloat}(A::StridedMatrix{T}, uplo::Symbol)
    uplochar = string(uplo)[1]
    C, info = LAPACK.potrf!(uplochar, A)
    if info > 0 throw(PosDefException(info)) end
    Cholesky(uplochar == 'L' ? tril(C) : triu(C), uplochar)
end
cholfact!(A::StridedMatrix, args...) = cholfact!(float(A), args...)
cholfact!{T<:BlasFloat}(A::StridedMatrix{T}) = cholfact!(A, :U)
cholfact{T<:BlasFloat}(A::StridedMatrix{T}, args...) = cholfact!(copy(A), args...)
cholfact(A::StridedMatrix, args...) = cholfact!(float(A), args...)
cholfact(x::Number) = imag(x) == 0 && real(x) > 0 ? Cholesky(fill(sqrt(x), 1, 1), 'U') : throw(PosDefException(1))

chol(A::Union(Number, StridedMatrix), uplo::Symbol) = cholfact(A, uplo)[uplo]
chol(A::Union(Number, StridedMatrix)) = cholfact(A, :U)[:U]

size(C::Cholesky) = size(C.UL)
size(C::Cholesky,d::Integer) = size(C.UL,d)

function getindex(C::Cholesky, d::Symbol)
    if d == :U || d == :L
        return symbol(C.uplo) == d ? C.UL : C.UL'
    elseif d == :UL
        return Triangular(C.UL, C.uplo)
    end
    error("No such type field")
end

\{T<:BlasFloat}(C::Cholesky{T}, B::StridedVecOrMat{T}) =
    LAPACK.potrs!(C.uplo, C.UL, copy(B))

function det{T}(C::Cholesky{T})
    dd = one(T)
    for i in 1:size(C.UL,1) dd *= abs2(C.UL[i,i]) end
    dd
end

function logdet{T}(C::Cholesky{T})
    dd = zero(T)
    for i in 1:size(C.UL,1) dd += log(C.UL[i,i]) end
    dd + dd # instead of 2.0dd which can change the type
end

function inv(C::Cholesky)
    Ci, info = LAPACK.potri!(C.uplo, copy(C.UL))
    if info != 0; throw(SingularException(info)); end 
    symmetrize_conj!(Ci, C.uplo)
end

## Pivoted Cholesky
type CholeskyPivoted{T<:BlasFloat} <: Factorization{T}
    UL::Matrix{T}
    uplo::Char
    piv::Vector{BlasInt}
    rank::BlasInt
    tol::Real
    info::BlasInt
end
function CholeskyPivoted{T<:BlasFloat}(A::StridedMatrix{T}, uplo::Char, tol::Real)
    A, piv, rank, info = LAPACK.pstrf!(uplo, A, tol)
    CholeskyPivoted{T}(uplo == 'U' ? triu!(A) : tril!(A), uplo, piv, rank, tol, info)
end
cholpfact!(A::StridedMatrix, args...) = cholpfact!(float(A), args...)
cholpfact!{T<:BlasFloat}(A::StridedMatrix{T}, uplo::Symbol, tol::Real) = CholeskyPivoted(A, string(uplo)[1], tol)
cholpfact!{T<:BlasFloat}(A::StridedMatrix{T}, tol::Real) = cholpfact!(A, :U, tol)
cholpfact!{T<:BlasFloat}(A::StridedMatrix{T}) = cholpfact!(A, -1.)
cholpfact{T<:BlasFloat}(A::StridedMatrix{T}, args...) = cholpfact!(copy(A), args...)
cholpfact(A::StridedMatrix, args...) = cholpfact!(float(A), args...)

size(C::CholeskyPivoted) = size(C.UL)
size(C::CholeskyPivoted,d::Integer) = size(C.UL,d)

getindex(C::CholeskyPivoted) = C.UL, C.piv
function getindex{T<:BlasFloat}(C::CholeskyPivoted{T}, d::Symbol)
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
    error("No such type field")
end

function \{T<:BlasFloat}(C::CholeskyPivoted{T}, B::StridedVector{T})
    if C.rank < size(C.UL, 1); throw(RankDeficientException(C.info)); end
    LAPACK.potrs!(C.uplo, C.UL, copy(B)[C.piv])[invperm(C.piv)]
end

function \{T<:BlasFloat}(C::CholeskyPivoted{T}, B::StridedMatrix{T})
    if C.rank < size(C.UL, 1); throw(RankDeficientException(C.info)); end
    LAPACK.potrs!(C.uplo, C.UL, copy(B)[C.piv,:])[invperm(C.piv),:]
end

rank(C::CholeskyPivoted) = C.rank

function det{T}(C::CholeskyPivoted{T})
    if C.rank < size(C.UL, 1) 
        return real(zero(T))
    else 
        return prod(abs2(diag(C.UL)))
    end
end
    
function inv(C::CholeskyPivoted)
    if C.rank < size(C.UL, 1) throw(RankDeficientException(C.info)) end
    Ci, info = LAPACK.potri!(C.uplo, copy(C.UL))
    if info != 0 throw(RankDeficientException(info)) end
    ipiv = invperm(C.piv)
    (symmetrize!(Ci, C.uplo))[ipiv, ipiv]
end

## LU
type LU{T<:BlasFloat} <: Factorization{T}
    factors::Matrix{T}
    ipiv::Vector{BlasInt}
    info::BlasInt
end
function LU{T<:BlasFloat}(A::StridedMatrix{T})
    factors, ipiv, info = LAPACK.getrf!(A)
    LU{T}(factors, ipiv, info)
end
lufact!(A::StridedMatrix) = lufact!(float(A))
lufact!{T<:BlasFloat}(A::StridedMatrix{T}) = LU(A)
lufact{T<:BlasFloat}(A::StridedMatrix{T}) = lufact!(copy(A))
lufact(A::StridedMatrix) = lufact!(float(A))
lufact(x::Number) = LU(fill(x, 1, 1), [1], x == 0 ? 1 : 0)

function lu(A::Union(Number, StridedMatrix))
    F = lufact(A)
    return (F[:L], F[:U], F[:P])
end

size(A::LU) = size(A.factors)
size(A::LU,n) = size(A.factors,n)

function getindex{T}(A::LU{T}, d::Symbol)
    if d == :L; return tril(A.factors, -1) + eye(T, size(A, 1)); end;
    if d == :U; return triu(A.factors); end;
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
    error("No such type field")
end

function det{T}(A::LU{T})
    m, n = size(A)
    if A.info > 0; return zero(typeof(A.factors[1])); end
    prod(diag(A.factors)) * (bool(sum(A.ipiv .!= 1:n) % 2) ? -one(T) : one(T))
end

function (\){T<:BlasFloat}(A::LU{T}, B::StridedVecOrMat{T})
    if A.info > 0; throw(SingularException(A.info)); end
    LAPACK.getrs!('N', A.factors, A.ipiv, copy(B))
end

function inv(A::LU)
    if A.info > 0; return throw(SingularException(A.info)); end
    LAPACK.getri!(copy(A.factors), A.ipiv)
end

cond(A::LU, p) = 1.0/LinAlg.LAPACK.gecon!(p == 1 ? '1' : 'I', A.factors, norm(A[:L][A[:p],:]*A[:U], p))

## QR decomposition without column pivots. By the faster geqrt3
type QR{S<:BlasFloat} <: Factorization{S}
    vs::Matrix{S}                     # the elements on and above the diagonal contain the N-by-N upper triangular matrix R; the elements below the diagonal are the columns of V
    T::Matrix{S}                      # upper triangular factor of the block reflector.
end
QR{T<:BlasFloat}(A::StridedMatrix{T}) = QR(LAPACK.geqrt3!(A)...)

qrfact!{T<:BlasFloat}(A::StridedMatrix{T}) = QR(A)
qrfact!(A::StridedMatrix) = qrfact!(float(A))
qrfact{T<:BlasFloat}(A::StridedMatrix{T}) = qrfact!(copy(A))
qrfact(A::StridedMatrix) = qrfact!(float(A))
qrfact(x::Integer) = qrfact(float(x))
qrfact(x::Number) = QR(fill(one(x), 1, 1), fill(x, 1, 1))

function qr(A::Union(Number, StridedMatrix), thin::Bool)
    F = qrfact(A)
    return (full(F[:Q], thin), F[:R])
end
qr(A::Union(Number, StridedMatrix)) = qr(A, true)

size(A::QR, args::Integer...) = size(A.vs, args...)

function getindex(A::QR, d::Symbol)
    if d == :R; return triu(A.vs[1:min(size(A)),:]); end;
    if d == :Q; return QRPackedQ(A); end
    error("No such type field")
end

type QRPackedQ{S}  <: AbstractMatrix{S} 
    vs::Matrix{S}                      
    T::Matrix{S}                       
end
QRPackedQ(A::QR) = QRPackedQ(A.vs, A.T)

size(A::QRPackedQ, args::Integer...) = size(A.vs, args...)

function full{T<:BlasFloat}(A::QRPackedQ{T}, thin::Bool)
    if thin return A * eye(T, size(A.T, 1)) end
    return A * eye(T, size(A, 1))
end
full(A::QRPackedQ) = full(A, true)

print_matrix(io::IO, A::QRPackedQ) = print_matrix(io, full(A))

## Multiplication by Q from the QR decomposition
function *{T<:BlasFloat}(A::QRPackedQ{T}, B::StridedVecOrMat{T})
    m = size(B, 1)
    n = size(B, 2)
    if m == size(A.vs, 1)
        Bc = copy(B)
    elseif m == size(A.vs, 2)
        Bc = [B; zeros(T, size(A.vs, 1) - m, n)]
    else
        throw(DimensionMismatch(""))
    end
    LAPACK.gemqrt!('L', 'N', A.vs, A.T, Bc)
end
Ac_mul_B{T<:BlasReal}(A::QRPackedQ{T}, B::StridedVecOrMat) = LAPACK.gemqrt!('L','T',A.vs,A.T,copy(B))
Ac_mul_B{T<:BlasComplex}(A::QRPackedQ{T}, B::StridedVecOrMat) = LAPACK.gemqrt!('L','C',A.vs,A.T,copy(B))
*{T<:BlasFloat}(A::StridedVecOrMat{T}, B::QRPackedQ{T}) = LAPACK.gemqrt!('R', 'N', B.vs, B.T, copy(A))
function A_mul_Bc{T<:BlasFloat}(A::StridedVecOrMat{T}, B::QRPackedQ{T})
    m = size(A, 1)
    n = size(A, 2)
    if n == size(B.vs, 1)
        Ac = copy(A)
    elseif n == size(B.vs, 2)
        Ac = [B zeros(T, m, size(B.vs, 1) - n)]
    else
        throw(DimensionMismatch(""))
    end
    LAPACK.gemqrt!('R', iseltype(B.vs,Complex) ? 'C' : 'T', B.vs, B.T, Ac)
end
## Least squares solution.  Should be more careful about cases with m < n
(\)(A::QR, B::StridedVector) = Triangular(A[:R], 'U')\(A[:Q]'B)[1:size(A, 2)]
(\)(A::QR, B::StridedMatrix) = Triangular(A[:R], 'U')\(A[:Q]'B)[1:size(A, 2),:]

type QRPivoted{T} <: Factorization{T}
    hh::Matrix{T}
    tau::Vector{T}
    jpvt::Vector{BlasInt}
    function QRPivoted(hh::Matrix{T}, tau::Vector{T}, jpvt::Vector{BlasInt})
        m, n = size(hh)
        if length(tau) != min(m,n) || length(jpvt) != n
            throw(DimensionMismatch(""))
        end
        new(hh,tau,jpvt)
    end
end

qrpfact!{T<:BlasFloat}(A::StridedMatrix{T}) = QRPivoted{T}(LAPACK.geqp3!(A)...)
qrpfact!(A::StridedMatrix) = qrpfact!(float(A))
qrpfact{T<:BlasFloat}(A::StridedMatrix{T}) = qrpfact!(copy(A))
qrpfact(A::StridedMatrix) = qrpfact!(float(A))

function qrp(A::StridedMatrix, thin::Bool)
    F = qrpfact(A)
    return full(F[:Q], thin), F[:R], F[:P]
end
qrp(A::StridedMatrix) = qrp(A, false)

size(A::QRPivoted, args::Integer...) = size(A.hh, args...)

function getindex{T<:BlasFloat}(A::QRPivoted{T}, d::Symbol)
    if d == :R; return triu(A.hh[1:min(size(A)),:]); end;
    if d == :Q; return QRPivotedQ(A); end
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
    error("No such type field")
end

(\){T<:BlasFloat}(A::QRPivoted{T}, B::StridedVector{T}) = (Triangular(A[:R])\(A[:Q]'B)[1:size(A, 2)])[invperm(A.jpvt)]
(\){T<:BlasFloat}(A::QRPivoted{T}, B::StridedMatrix{T}) = (Triangular(A[:R])\(A[:Q]'B)[1:size(A, 2),:])[invperm(A.jpvt),:]

type QRPivotedQ{T}  <: AbstractMatrix{T}
    hh::Matrix{T}                       # Householder transformations and R
    tau::Vector{T}                      # Scalar factors of transformations
end
QRPivotedQ(A::QRPivoted) = QRPivotedQ(A.hh, A.tau)

size(A::QRPivotedQ, args...) = size(A.hh, args...)

function full{T<:BlasFloat}(A::QRPivotedQ{T}, thin::Bool)
    if !thin
        Q = Array(T, size(A, 1), size(A, 1))
        Q[:,1:size(A, 2)] = copy(A.hh)
        return LAPACK.orgqr!(Q, A.tau)
    else
        return LAPACK.orgqr!(copy(A.hh), A.tau)
    end
end
full(A::QRPivotedQ) = full(A, true)
print_matrix(io::IO, A::QRPivotedQ) = print_matrix(io, full(A))

## Multiplication by Q from the Pivoted QR decomposition
function *{T<:BlasFloat}(A::QRPivotedQ{T}, B::StridedVecOrMat{T})
    m = size(B, 1)
    n = size(B, 2)
    if m == size(A.hh, 1)
        Bc = copy(B)
    elseif m == size(A.hh, 2)
        Bc = [B; zeros(T, size(A.hh, 1) - m, n)]
    else
        throw(DimensionMismatch(""))
    end
    LAPACK.ormqr!('L', 'N', A.hh, A.tau, Bc)
end
Ac_mul_B{T<:BlasReal}(A::QRPivotedQ{T}, B::StridedVecOrMat) = LAPACK.ormqr!('L','T',A.hh,A.tau,copy(B))
Ac_mul_B{T<:BlasComplex}(A::QRPivotedQ{T}, B::StridedVecOrMat) = LAPACK.ormqr!('L','C',A.hh,A.tau,copy(B))
*(A::StridedVecOrMat, B::QRPivotedQ) = LAPACK.ormqr!('R', 'N', B.hh, B.tau, copy(A))
function A_mul_Bc{T<:BlasFloat}(A::StridedVecOrMat{T}, B::QRPivotedQ{T})
    m = size(A, 1)
    n = size(A, 2)
    if n == size(B.hh, 1)
        Ac = copy(A)
    elseif n == size(B.hh, 2)
        Ac = [B zeros(T, m, size(B.hh, 1) - n)]
    else
        throw(DimensionMismatch(""))
    end
    LAPACK.ormqr!('R', iseltype(B.hh,Complex) ? 'C' : 'T', B.hh, B.tau, Ac)
end

##TODO:  Add methods for rank(A::QRP{T}) and adjust the (\) method accordingly
##       Add rcond methods for Cholesky, LU, QR and QRP types
## Lower priority: Add LQ, QL and RQ factorizations

# FIXME! Should add balancing option through xgebal
type Hessenberg{T} <: Factorization{T}
    hh::Matrix{T}
    tau::Vector{T}
    function Hessenberg(hh::Matrix{T}, tau::Vector{T})
        if size(hh, 1) != size(hh, 2) throw(DimensionMismatch("")) end
        return new(hh, tau)
    end
end
Hessenberg{T<:BlasFloat}(hh::Matrix{T}, tau::Vector{T}) = Hessenberg{T}(hh, tau)
Hessenberg(A::StridedMatrix) = Hessenberg(LAPACK.gehrd!(A)...)

hessfact!{T<:BlasFloat}(A::StridedMatrix{T}) = Hessenberg(A)
hessfact!(A::StridedMatrix) = hessfact!(float(A))
hessfact{T<:BlasFloat}(A::StridedMatrix{T}) = hessfact!(copy(A))
hessfact(A::StridedMatrix) = hessfact!(float(A))

type HessenbergQ{T} <: AbstractMatrix{T}
    hh::Matrix{T}
    tau::Vector{T}
end
HessenbergQ(A::Hessenberg) = HessenbergQ(A.hh, A.tau)
size(A::HessenbergQ, args...) = size(A.hh, args...)
getindex(A::HessenbergQ, args...) = getindex(full(A), args...)

function getindex(A::Hessenberg, d::Symbol)
    if d == :Q; return HessenbergQ(A); end
    if d == :H; return triu(A.hh, -1); end
    error("No such type field")
end

full(A::HessenbergQ) = LAPACK.orghr!(1, size(A.hh, 1), copy(A.hh), A.tau)

# Eigenvalues
type Eigen{T,V} <: Factorization{T}
    values::Vector{V}
    vectors::Matrix{T}
end

function getindex(A::Eigen, d::Symbol)
    if d == :values return A.values end
    if d == :vectors return A.vectors end
    error("No such type field")
end

function eigfact!{T<:BlasReal}(A::StridedMatrix{T})
    n = size(A, 2)
    if n == 0; return Eigen(zeros(T, 0), zeros(T, 0, 0)) end
    if ishermitian(A) return eigfact!(Hermitian(A)) end

    WR, WI, VL, VR = LAPACK.geev!('N', 'V', A)
    if all(WI .== 0.) return Eigen(WR, VR) end
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
    return Eigen(complex(WR, WI), evec)
end

function eigfact!{T<:BlasComplex}(A::StridedMatrix{T})
    n = size(A, 2)
    if n == 0; return Eigen(zeros(T, 0), zeros(T, 0, 0)) end
    if ishermitian(A) return eigfact!(Hermitian(A)) end
    Eigen(LAPACK.geev!('N', 'V', A)[[1,3]]...)
end
eigfact!(A::StridedMatrix) = eigfact!(float(A))
eigfact{T<:BlasFloat}(x::StridedMatrix{T}) = eigfact!(copy(x))
eigfact(A::StridedMatrix) = eigfact!(float(A))
eigfact(x::Number) = Eigen([x], fill(one(x), 1, 1))

function eig(A::Union(Number, StridedMatrix))
    F = eigfact(A)
    return F[:values], F[:vectors]
end

#Calculates eigenvectors
eigvecs(A::Union(Number, StridedMatrix)) = eigfact(A)[:vectors]

function eigvals{T<:BlasReal}(A::StridedMatrix{T})
    if ishermitian(A) return eigvals(Hermitian(A)) end
    valsre, valsim, _, _ = LAPACK.geev!('N', 'N', copy(A))
    if all(valsim .== 0) return valsre end
    return complex(valsre, valsim)
end
function eigvals{T<:BlasReal}(A::StridedMatrix{T})
    if ishermitian(A) return eigvals(Hermitian(A)) end
    LAPACK.geev!('N', 'N', copy(A))[1]
end

eigvals(x::Number) = [one(x)]

#Computes maximum and minimum eigenvalue
function eigmax(A::Union(Number, StridedMatrix))
    v = eigvals(A)
    iseltype(v,Complex) ? error("Complex eigenvalues cannot be ordered") : max(v)
end
function eigmin(A::Union(Number, StridedMatrix))
    v = eigvals(A)
    iseltype(v,Complex) ? error("Complex eigenvalues cannot be ordered") : min(v)
end

inv(A::Eigen) = scale(A.vectors, 1.0/A.values)*A.vectors'
det(A::Eigen) = prod(A.values)

# SVD
type SVD{T<:BlasFloat,Tr} <: Factorization{T}
    U::Matrix{T}
    S::Vector{Tr}
    Vt::Matrix{T}
end
function svdfact!{T<:BlasFloat}(A::StridedMatrix{T}, thin::Bool)
    m,n = size(A)
    if m == 0 || n == 0
        u,s,vt = (eye(m, thin ? n : m), zeros(0), eye(n,n))
    else
        u,s,vt = LAPACK.gesdd!(thin ? 'S' : 'A', A)
    end
    return SVD(u,s,vt)
end
svdfact!(A::StridedVecOrMat, args...) = svdfact!(float(A), args...)
svdfact!{T<:BlasFloat}(a::Vector, thin::Bool) = svdfact!(reshape(a, length(a), 1), thin)
svdfact!{T<:BlasFloat}(A::StridedVecOrMat{T}) = svdfact!(A, true)
svdfact{T<:BlasFloat}(A::StridedVecOrMat{T}, args...) = svdfact!(copy(A), args...)
svdfact(A::StridedVecOrMat, args...) = svdfact!(float(A), args...)
svdfact(x::Number, thin::Bool) = SVD(x == 0 ? fill(one(x), 1, 1) : fill(x/abs(x), 1, 1), [abs(x)], fill(one(x), 1, 1))
svdfact(x::Integer, thin::Bool) = svdfact(float(x), thin)
svdfact(x::Number) = svdfact(x, true)

function svd(A::Union(Number, StridedVecOrMat), thin::Bool)
    F = svdfact(A, thin)
    return F.U, F.S, F.Vt'
end
svd(A::Union(Number, StridedVecOrMat)) = svd(A, true)

function getindex(F::SVD, d::Symbol)
    if d == :U return F.U end
    if d == :S return F.S end
    if d == :Vt return F.Vt end
    if d == :V return F.Vt' end
    error("No such type field")
end

function svdvals!{T<:BlasFloat}(A::StridedMatrix{T})
    m,n = size(A)
    if m == 0 || n == 0 return zeros(T, 0) end
    return LAPACK.gesdd!('N', A)[2]
end
svdvals{T<:BlasFloat}(A::StridedMatrix{T}) = svdvals!(copy(A))
svdvals(A::StridedMatrix) = svdvals!(float(A))
svdvals(x::Number) = [x]

# SVD least squares
function \{T<:BlasFloat}(A::SVD{T}, B::StridedVecOrMat{T})
    n = length(A.S)
    Sinv = zeros(T, n)
    Sinv[A.S .> sqrt(eps())] = 1.0 ./ A.S
    scale(A.Vt', Sinv) * A.U[:,1:n]'B
end

# Generalized svd
type GeneralizedSVD{T} <: Factorization{T}
    U::Matrix{T}
    V::Matrix{T}
    Q::Matrix{T}
    a::Vector
    b::Vector
    k::Int
    l::Int
    R::Matrix{T}
end

function svdfact!{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T})
    U, V, Q, a, b, k, l, R = LAPACK.ggsvd!('U', 'V', 'Q', A, B)
    return GeneralizedSVD(U, V, Q, a, b, int(k), int(l), R)
end
svdfact!(A::StridedMatrix, B::StridedMatrix) = svdfact!(float(A), float(B))
svdfact{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T}) = svdfact!(copy(A), copy(B))
svdfact(A::StridedMatrix, B::StridedMatrix) = svdfact!(float(A), float(B))

function svd(A::StridedMatrix, B::StridedMatrix)
    F = svdfact(A, B)
    return F[:U], F[:V], F[:Q]*F[:R0]', F[:D1], F[:D2]
end

function getindex{T}(obj::GeneralizedSVD{T}, d::Symbol)
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
    error("No such type field")
end

function svdvals(A::StridedMatrix, B::StridedMatrix)
    _, _, _, a, b, k, l, _ = LAPACK.ggsvd!('N', 'N', 'N', copy(A), copy(B))
    return a[1:k + l] ./ b[1:k + l]
end

type Schur{Ty<:BlasFloat} <: Factorization{Ty}
    T::Matrix{Ty}
    Z::Matrix{Ty}
    values::Vector
end

schurfact!{T<:BlasFloat}(A::StridedMatrix{T}) = Schur(LinAlg.LAPACK.gees!('V', A)...)
schurfact!(A::StridedMatrix) = schurfact!(float(A))
schurfact{T<:BlasFloat}(A::StridedMatrix{T}) = schurfact!(copy(A))
schurfact(A::StridedMatrix) = schurfact!(float(A))

function getindex(F::Schur, d::Symbol)
    if d == :T || d == :Schur return F.T end
    if d == :Z || d == :vectors return F.Z end
    if d == :values return F.values end
    error("No such type field")
end

function schur(A::StridedMatrix)
    SchurF = schurfact(A)
    return SchurF[:T], SchurF[:Z], SchurF[:values]
end

type GeneralizedSchur{Ty<:BlasFloat} <: Factorization{Ty}
    S::Matrix{Ty}
    T::Matrix{Ty}
    alpha::Vector
    beta::Vector{Ty}
    Q::Matrix{Ty}
    Z::Matrix{Ty}
end

schurfact!{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T}) = GeneralizedSchur(LinAlg.LAPACK.gges!('V', 'V', A, B)...)
schurfact!(A::StridedMatrix, B::StridedMatrix) = schurfact!(float(A), float(B))
schurfact{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T}) = schurfact!(copy(A), copy(B))
schurfact(A::StridedMatrix, B::StridedMatrix) = schurfact!(float(A), float(B))

function getindex(F::GeneralizedSchur, d::Symbol)
    if d == :S return F.S end
    if d == :T return F.T end
    if d == :alpha return F.alpha end
    if d == :beta return F.beta end
    if d == :values return F.alpha./F.beta end
    if d == :Q || d == :left return F.Q end
    if d == :Z || d == :right return F.Z end
    error("No such type field")
end

function schur(A::StridedMatrix, B::StridedMatrix)
    SchurF = schurfact(A, B)
    return SchurF[:S], SchurF[:T], SchurF[:Q], SchurF[:Z]
end
