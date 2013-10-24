## Matrix factorizations and decompositions

abstract Factorization{T}

(\)(F::Factorization, b::Union(AbstractVector, AbstractMatrix)) = A_ldiv_B!(F, copy(b))

##########################
# Cholesky factorization #
##########################

type Cholesky{T<:BlasFloat} <: Factorization{T}
    UL::Matrix{T}
    uplo::Char
end

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
    CholeskyPivoted{T}(uplo=='U' ? triu!(A) : tril!(A), uplo, piv, rank, tol, info)
end

function cholfact!{T<:BlasFloat}(A::StridedMatrix{T}, uplo::Symbol=:U, pivoted::Bool=false,
      tol::Real=-1.0)
  if pivoted
    CholeskyPivoted(A, string(uplo)[1], tol)
  else
    uplochar = string(uplo)[1]
    C, info = LAPACK.potrf!(uplochar, A)
    info==0 ? Cholesky(C, uplochar) : throw(PosDefException(info))
  end
end

cholfact!(A::StridedMatrix, uplo::Symbol=:U, pivoted::Bool=false, args...) = cholfact!(float(A), uplo, pivoted, args...)
cholfact{T<:BlasFloat}(A::StridedMatrix{T}, uplo::Symbol=:U, pivoted::Bool=false, args...) = cholfact!(copy(A), uplo, pivoted, args...)
cholfact(A::StridedMatrix, uplo::Symbol=:U, pivoted::Bool=false, args...) = cholfact!(float(A), uplo, pivoted, args...)
cholfact(A::StridedMatrix, pivoted::Bool=false, args...) = cholfact(float(A), :U, pivoted, args...)
cholfact(x::Number, pivoted::Bool=false) = imag(x) == 0 && real(x) > 0 ? Cholesky(fill(sqrt(x), 1, 1), :U) : throw(PosDefException(1))

chol(A::Union(Number, AbstractMatrix), uplo::Symbol=:U) = cholfact(A, uplo)[uplo]

function getindex(C::Cholesky, d::Symbol)
    C.uplo == 'U' ? triu!(C.UL) : tril!(C.UL)
    if d == :U || d == :L return symbol(C.uplo) == d ? C.UL : C.UL'
    elseif d == :UL return Triangular(C.UL, C.uplo)
    else error("No such type field")
    end
end
getindex(C::CholeskyPivoted) = C.UL, C.piv
function getindex{T<:BlasFloat}(C::CholeskyPivoted{T}, d::Symbol)
    if d == :U || d == :L return symbol(C.uplo) == d ? C.UL : C.UL'
    elseif d == :p return C.piv 
    elseif d == :P return perm_matrix(C.piv, T)
    else error("No such type field")
    end
end

#Generate permutation matrix from a permutation
function perm_matrix{T<:Integer}(A::Vector{T}, output_type::Type=T)
    n = length(A)
    P = zeros(output_type, n, n)
    for i=1:n
        P[A[i], i] = one(output_type)
    end
    P
end

size(C::Union(Cholesky,CholeskyPivoted)) = size(C.UL)
size(C::Union(Cholesky,CholeskyPivoted),d::Integer) = size(C.UL,d)

rank(C::CholeskyPivoted) = C.rank
rank(C::Cholesky) = size(C.UL, 1) #full rank

A_ldiv_B!{T<:BlasFloat}(C::Cholesky{T}, B::StridedVecOrMat{T}) = LAPACK.potrs!(C.uplo, C.UL, B)
function A_ldiv_B!{T<:BlasFloat}(C::CholeskyPivoted{T}, B::StridedVector{T})
    C.rank < size(C.UL, 1) ? throw(RankDeficientException(C.info)) :
    ipermute!(LAPACK.potrs!(C.uplo, C.UL, permute!(B, C.piv)), C.piv)
end
function A_ldiv_B!{T<:BlasFloat}(C::CholeskyPivoted{T}, B::StridedMatrix{T})
    C.rank < size(C.UL, 1) ? throw(RankDeficientException(C.info)) : nothing
    n = size(C, 1)
    for i = 1:size(B, 2)
        permute!(sub(B, 1:n, i), C.piv)
    end
    LAPACK.potrs!(C.uplo, C.UL, B)
    for i = 1:size(B, 2)
        ipermute!(sub(B, 1:n, i), C.piv)
    end
    B
end

det{T}(C::Cholesky{T}) = prod(abs2(diag(C.UL)))
det{T}(C::CholeskyPivoted{T}) = C.rank < size(C.UL, 1) ? real(zero(T)) : prod(abs2(diag(C.UL)))

logdet{T}(C::Cholesky{T}) = 2sum(log(diag(C.UL)))
logdet{T}(C::CholeskyPivoted{T}) = C.rank < size(C.UL, 1) ? real(convert(T, -Inf)) : 2sum(log(diag(C.UL)))

function inv(C::Cholesky)
    Ci, info = LAPACK.potri!(C.uplo, copy(C.UL))
    info == 0 ? symmetrize_conj!(Ci, C.uplo) : throw(SingularException(info))
end
function inv(C::CholeskyPivoted)
    if C.rank < size(C.UL, 1) throw(RankDeficientException(C.info)) end
    Ci, info = LAPACK.potri!(C.uplo, copy(C.UL))
    if info != 0 throw(RankDeficientException(info)) end
    ipiv = invperm(C.piv)
    (symmetrize!(Ci, C.uplo))[ipiv, ipiv]
end

####################
# LU factorization #
####################

type LU{T<:BlasFloat} <: Factorization{T}
    factors::Matrix{T}
    pivots::Vector{BlasInt}
    info::BlasInt
end
function LU{T<:BlasFloat}(A::StridedMatrix{T})
    factors, pivots, info = LAPACK.getrf!(A)
    LU{T}(factors, pivots, info)
end
lufact!(A::StridedMatrix) = lufact!(float(A))
lufact!{T<:BlasFloat}(A::StridedMatrix{T}) = LU(A)
lufact{T<:BlasFloat}(A::StridedMatrix{T}) = lufact!(copy(A))
lufact(A::StridedMatrix) = lufact!(float(A))
lufact(x::Number) = LU(fill(x, 1, 1), [1], x == 0 ? 1 : 0)

function lu(A::Union(Number, AbstractMatrix))
    F = lufact(A)
    return (F[:L], F[:U], F[:p])
end

size(A::LU) = size(A.factors)
size(A::LU,n) = size(A.factors,n)

function getindex{T}(A::LU{T}, d::Symbol)
    m, n = size(A)
    if d == :L return tril(A.factors[1:m, 1:min(m,n)], -1) + eye(T, m, min(m,n))
    elseif d == :U return triu(A.factors[1:min(m,n),1:n])
    elseif d == :p
        p = [1:m]
        for i in 1:length(A.pivots)
            tmp = p[i]
            p[i] = p[A.pivots[i]]
            p[A.pivots[i]] = tmp
        end
        return p
    elseif d == :P
        p = A[:p]
        P = zeros(T, m, m)
        for i in 1:m
            P[i,p[i]] = one(T)
        end
        return P
    end
    error("No such type field")
end

function det{T}(A::LU{T})
    m, n = size(A)
    if m != n throw(DimensionMismatch("Matrix must be square")) end
    if A.info > 0; return zero(typeof(A.factors[1])); end
    prod(diag(A.factors)) * (bool(sum(A.pivots .!= 1:n) % 2) ? -one(T) : one(T))
end

function logdet2{T<:Real}(A::LU{T})  # return log(abs(det)) and sign(det)
    m, n = size(A); if m!=n error("matrix must be square") end
    dg = diag(A.factors)
    s = (bool(sum(A.pivots .!= 1:n) % 2) ? -one(T) : one(T)) * prod(sign(dg))
    return sum(log(abs(dg))) , s 
end

function logdet{T<:Real}(A::LU{T})
    d,s = logdet2(A)
    if s<0 error("DomainError: determinant is negative") end
    return d
end

function logdet{T<:Complex}(A::LU{T})
    m, n = size(A); if m!=n error("matrix must be square") end
    s = sum(log(diag(A.factors))) + (bool(sum(A.pivots .!= 1:n) % 2) ? complex(0,pi) : 0) 
    r,a = reim(s); a = a % 2pi; if a>pi a -=2pi elseif a<=-pi a+=2pi end
    return complex(r,a)    
end


function (\){T<:BlasFloat}(A::LU{T}, B::StridedVecOrMat{T})
    if A.info > 0; throw(SingularException(A.info)); end
    LAPACK.getrs!('N', A.factors, A.pivots, copy(B))
end

function At_ldiv_B{T<:BlasFloat}(A::LU{T}, B::StridedVecOrMat{T})
    if A.info > 0; throw(SingularException(A.info)); end
    LAPACK.getrs!('T', A.factors, A.pivots, copy(B))
end

function Ac_ldiv_B{T<:BlasComplex}(A::LU{T}, B::StridedVecOrMat{T})
    if A.info > 0; throw(SingularException(A.info)); end
    LAPACK.getrs!('C', A.factors, A.pivots, copy(B))
end

function At_ldiv_Bt{T<:BlasFloat}(A::LU{T}, B::StridedVecOrMat{T})
    if A.info > 0; throw(SingularException(A.info)); end
    LAPACK.getrs!('T', A.factors, A.pivots, transpose(B))
end

function Ac_ldiv_Bc{T<:BlasComplex}(A::LU{T}, B::StridedVecOrMat{T})
    if A.info > 0; throw(SingularException(A.info)); end
    LAPACK.getrs!('C', A.factors, A.pivots, ctranspose(B))
end

(/){T}(B::Matrix{T},A::LU{T}) = At_ldiv_Bt(A,B).'

function inv(A::LU)
    if A.info > 0; return throw(SingularException(A.info)); end
    LAPACK.getri!(copy(A.factors), A.pivots)
end

cond(A::LU, p) = 1.0/LinAlg.LAPACK.gecon!(p == 1 ? '1' : 'I', A.factors, norm(A[:L][A[:p],:]*A[:U], p))

####################
# QR factorization #
####################

type QR{S<:BlasFloat} <: Factorization{S}
    vs::Matrix{S}                     # the elements on and above the diagonal contain the N-by-N upper triangular matrix R; the elements below the diagonal are the columns of V
    T::Matrix{S}                      # upper triangular factor of the block reflector.
end

# QR decomposition without column pivots. By the faster geqrt3
QR{T<:BlasFloat}(A::StridedMatrix{T}, nb::Integer = min(minimum(size(A)), 36)) = QR(LAPACK.geqrt!(A, nb)...)

type QRPackedQ{S} <: AbstractMatrix{S} 
    vs::Matrix{S}
    T::Matrix{S}
end
QRPackedQ(A::QR) = QRPackedQ(A.vs, A.T)

type QRPivoted{T} <: Factorization{T}
    hh::Matrix{T}
    tau::Vector{T}
    pivots::Vector{BlasInt}
end

type QRPivotedQ{T} <: AbstractMatrix{T}
    hh::Matrix{T}                      # Householder transformations and R
    tau::Vector{T}                     # Scalar factors of transformations
end
QRPivotedQ(A::QRPivoted) = QRPivotedQ(A.hh, A.tau)

function qr(A::Union(Number, AbstractMatrix), thin::Bool=true, pivoted::Bool=false)
  F = qrfact(A, pivoted)
  return pivoted ? (full(F[:Q], thin), F[:R], F[:p]) : (full(F[:Q], thin), F[:R])
end

qrfact!{T<:BlasFloat}(A::StridedMatrix{T}, pivoted::Bool=false, args::Integer...) = pivoted ? QRPivoted{T}(LAPACK.geqp3!(A)...) : QR(A, args...) 
qrfact!(A::StridedMatrix, pivoted::Bool=false) = qrfact!(float(A), pivoted)
qrfact{T<:BlasFloat}(A::StridedMatrix{T}, pivoted::Bool=false, args::Integer...) = qrfact!(copy(A), pivoted, args...)
qrfact(A::StridedMatrix, pivoted::Bool=false, args::Integer...) = qrfact!(float(A), pivoted, args...)
qrfact(x::Integer, pivoted::Bool=false) = qrfact(float(x), pivoted)
qrfact(x::Number, pivoted::Bool=false) = pivoted ? error("Not implemented") : QR(fill(one(x), 1, 1), fill(x, 1, 1))
size(A::Union(QR,        QRPackedQ ), args::Integer...) = size(A.vs, args...)
size(A::Union(QRPivoted, QRPivotedQ), args::Integer...) = size(A.hh, args...)

function getindex(A::QR, d::Symbol)
    if d == :R return triu(A.vs[1:minimum(size(A)),:])
    elseif d == :Q return QRPackedQ(A)
    else error("No such type field") end
end

function getindex{T<:BlasFloat}(A::QRPivoted{T}, d::Symbol)
    if d == :R return triu(A.hh[1:minimum(size(A)),:])
    elseif d == :Q return QRPivotedQ(A)
    elseif d == :p return A.pivots
    elseif d == :P return perm_matrix(A.pivots, T) #The permutation matrix for the pivot
    end
    error("No such type field")
end

full{T<:BlasFloat}(A::QRPackedQ{T}, thin::Bool) = A * eye(T, thin ? size(A.T, 2) : size(A, 1))
function full{T<:BlasFloat}(A::QRPivotedQ{T}, thin::Bool=true)
    m, n = size(A.hh)
    return LAPACK.orgqr!(thin ? copy(A.hh) : [A.hh zeros(T, m, max(0, m-n))], A.tau)
end

print_matrix(io::IO, A::Union(QRPackedQ, QRPivotedQ), rows::Integer, cols::Integer) = print_matrix(io, full(A), rows, cols)

## Multiplication by Q from the QR decomposition
function *{T<:BlasFloat}(A::QRPackedQ{T}, B::StridedVecOrMat{T})
    m, n = size(B)
    if m == size(A.vs, 1)
        Bc = copy(B)
    elseif m == size(A.vs, 2)
        Bc = [B; zeros(T, size(A.vs,1)-m, n)]
    else
        throw(DimensionMismatch(""))
    end
    LAPACK.gemqrt!('L', 'N', A.vs, A.T, Bc)
end
*{T<:BlasFloat}(A::StridedVecOrMat{T}, B::QRPackedQ{T}) = LAPACK.gemqrt!('R', 'N', B.vs, B.T, copy(A))
function *{T<:BlasFloat}(A::QRPivotedQ{T}, B::StridedVecOrMat{T})
    m, n = size(B)
    if m == size(A.hh, 1)
        Bc = copy(B)
    elseif m == size(A.hh, 2)
        Bc = [B; zeros(T, size(A.hh,1)-m, n)]
    else
        throw(DimensionMismatch(""))
    end
    LAPACK.ormqr!('L', 'N', A.hh, A.tau, Bc)
end
*(A::StridedVecOrMat, B::QRPivotedQ) = LAPACK.ormqr!('R', 'N', B.hh, B.tau, copy(A))

Ac_mul_B{T<:BlasReal   }(A::QRPackedQ{T},  B::StridedVecOrMat) = LAPACK.gemqrt!('L','T',A.vs,A.T,  copy(B))
Ac_mul_B{T<:BlasComplex}(A::QRPackedQ{T},  B::StridedVecOrMat) = LAPACK.gemqrt!('L','C',A.vs,A.T,  copy(B))
Ac_mul_B{T<:BlasReal   }(A::QRPivotedQ{T}, B::StridedVecOrMat) = LAPACK.ormqr! ('L','T',A.hh,A.tau,copy(B))
Ac_mul_B{T<:BlasComplex}(A::QRPivotedQ{T}, B::StridedVecOrMat) = LAPACK.ormqr! ('L','C',A.hh,A.tau,copy(B))
function A_mul_Bc{T<:BlasFloat}(A::StridedVecOrMat{T}, B::QRPackedQ{T})
    m, n = size(A)
    if n == size(B.vs, 1)
        Ac = copy(A)
    elseif n == size(B.vs, 2)
        Ac = [B zeros(T, m, size(B.vs, 1) - n)]
    else
        throw(DimensionMismatch(""))
    end
    LAPACK.gemqrt!('R', iseltype(B.vs,Complex) ? 'C' : 'T', B.vs, B.T, Ac)
end
function A_mul_Bc{T<:BlasFloat}(A::StridedVecOrMat{T}, B::QRPivotedQ{T})
    m, n = size(A)
    if n == size(B.hh, 1)
        Ac = copy(A)
    elseif n == size(B.hh, 2)
        Ac = [B zeros(T, m, size(B.hh, 1) - n)]
    else
        throw(DimensionMismatch(""))
    end
    LAPACK.ormqr!('R', iseltype(B.hh,Complex) ? 'C' : 'T', B.hh, B.tau, Ac)
end

## Least squares solution.
#XXX Should be more careful about cases with m < n
(\)(A::QR, B::StridedVector) = Triangular(A[:R], :U)\(A[:Q]'B)[1:size(A, 2)]
(\)(A::QR, B::StridedMatrix) = Triangular(A[:R], :U)\(A[:Q]'B)[1:size(A, 2),:]
# Julia implementation similarly to xgelsy
function (\){T<:BlasFloat}(A::QRPivoted{T}, B::StridedMatrix{T}, rcond::Real)
    nr, nrhs = minimum(size(A.hh)), size(B, 2)
    if nr == 0 return zeros(0, nrhs), 0 end
    ar = abs(A.hh[1])
    if ar < eps(typeof(ar)) return zeros(nr, nrhs), 0 end
    rnk = 1
    xmin, xmax = ones(T, nr), ones(T, nr)
    tmin, tmax = ar, ar
    while rnk < nr
        Ahh = sub(A.hh, 1:rnk, rnk+1), A.hh[rnk+1, rnk+1]
        tmin, smin, cmin = LAPACK.laic1!(2, sub(xmin, 1:rnk), tmin, Ahh...)
        tmax, smax, cmax = LAPACK.laic1!(1, sub(xmax, 1:rnk), tmax, Ahh...)
        if tmax*rcond > tmin break end
        xmin[1:rnk+1] = [smin*sub(xmin, 1:rnk), cmin]
        xmax[1:rnk+1] = [smax*sub(xmin, 1:rnk), cmax] #XXX should this be xmax?
        rnk += 1
        # if cond(r[1:rnk, 1:rnk])*rcond < 1 break end
    end
    C, tau = LAPACK.tzrzf!(A.hh[1:rnk,:])
    X = [Triangular(C[1:rnk,1:rnk],:U)\(A[:Q]'B)[1:rnk,:]; zeros(T, size(A.hh,2)-rnk, nrhs)]
    LAPACK.ormrz!('L', iseltype(B, Complex) ? 'C' : 'T', C, tau, X)
    return X[invperm(A[:p]),:], rnk
end
(\)(A::QRPivoted, B::StridedMatrix) = (\)(A, B, sqrt(eps(typeof(real(B[1])))))[1]
(\)(A::QRPivoted, B::StridedVector) = (\)(A, reshape(B, length(B), 1))[:]

##TODO:  Add methods for rank(A::QRP{T}) and adjust the (\) method accordingly
##       Add rcond methods for Cholesky, LU, QR and QRP types
## Lower priority: Add LQ, QL and RQ factorizations

############################
# Hessenberg factorization #
############################

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
getindex(A::HessenbergQ, i::Real) = getindex(full(A), i)
getindex(A::HessenbergQ, i::AbstractArray) = getindex(full(A), i)
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
    if issym(A) return eigfact!(Symmetric(A)) end

    WR, WI, VL, VR = LAPACK.geev!('N', 'V', A)
    if all(WI .== 0.) return Eigen(WR, VR) end
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

function eig(A::Union(Number, AbstractMatrix))
    F = eigfact(A)
    return F[:values], F[:vectors]
end

#Calculates eigenvectors
eigvecs(A::Union(Number, AbstractMatrix)) = eigfact(A)[:vectors]

function eigvals{T<:BlasReal}(A::StridedMatrix{T})
    if issym(A) return eigvals(Symmetric(A)) end
    valsre, valsim, _, _ = LAPACK.geev!('N', 'N', copy(A))
    if all(valsim .== 0) return valsre end
    return complex(valsre, valsim)
end
function eigvals{T<:BlasComplex}(A::StridedMatrix{T})
    if ishermitian(A) return eigvals(Hermitian(A)) end
    LAPACK.geev!('N', 'N', copy(A))[1]
end

eigvals(x::Number) = [one(x)]

#Computes maximum and minimum eigenvalue
function eigmax(A::Union(Number, AbstractMatrix))
    v = eigvals(A)
    iseltype(v,Complex) ? error("Complex eigenvalues cannot be ordered") : maximum(v)
end
function eigmin(A::Union(Number, AbstractMatrix))
    v = eigvals(A)
    iseltype(v,Complex) ? error("Complex eigenvalues cannot be ordered") : minimum(v)
end

inv(A::Eigen) = scale(A.vectors, 1.0/A.values)*A.vectors'
det(A::Eigen) = prod(A.values)

# Generalized eigenvalue problem.
type GeneralizedEigen{T,V}
    values::Vector{V}
    vectors::Matrix{T}
end

function getindex(A::GeneralizedEigen, d::Symbol)
    if d == :values return A.values end
    if d == :vectors return A.vectors end
    error("No such type field")
end

function eigfact!{T<:BlasReal}(A::StridedMatrix{T}, B::StridedMatrix{T})
    if issym(A) & issym(B) return eigfact!(Symmetric(A), Symmetric(B)) end
    n = size(A, 1)
    alphar, alphai, beta, ~, vr = LAPACK.ggev!('N', 'V', A, B)
    if all(alphai .== 0) 
        return GeneralizedEigen(alphar ./ beta, vr)
    else
        vecs = zeros(Complex{T}, n, n)
        j = 1
        while j <= n
            if alphai[j] == 0.0
                vecs[:,j] = vr[:,j]
            else
                vecs[:,j] = vr[:,j] + im*vr[:,j+1]
                vecs[:,j+1] = vr[:,j] - im*vr[:,j+1]
                j += 1
            end
            j += 1
        end
        return GeneralizedEigen(complex(alphar, alphai)./beta, vecs)
    end
end
function eigfact!{T<:BlasComplex}(A::StridedMatrix{T}, B::StridedMatrix{T})
    if ishermitian(A) & ishermitian(B) return eigfact!(Hermitian(A), Hermitian(B)) end
    alpha, beta, ~, vr = LAPACK.ggev!('N', 'V', A, B)
    return GeneralizedEigen(alpha./beta, vr)
end
eigfact!(A::StridedMatrix, B::StridedMatrix) = eigfact!(float(A), float(B))
eigfact{T<:BlasFloat}(A::StridedMatrix{T}, B::StridedMatrix{T}) = eigfact!(copy(A), copy(B))
eigfact(A::StridedMatrix, B::StridedMatrix) = eigfact!(float(A), float(B))

function eig(A::AbstractMatrix, B::AbstractMatrix)
    F = eigfact(A, B)
    return F[:values], F[:vectors]
end

function eigvals!{T<:BlasReal}(A::StridedMatrix{T}, B::StridedMatrix{T})
    if issym(A) & issym(B) return eigvals!(Symmetric(A), Symmetric(B)) end
    alphar, alphai, beta, vl, vr = LAPACK.ggev!('N', 'N', A, B)
    if all(alphai .== 0)
        return alphar./beta
    else
        return complex(alphar, alphai)./beta
    end
end
function eigvals!{T<:BlasComplex}(A::StridedMatrix{T}, B::StridedMatrix{T})
    if ishermitian(A) & ishermitian(B) return eigvals!(Hermitian(A), Hermitian(B)) end
    alpha, beta, vl, vr = LAPACK.ggev!('N', 'N', A, B)
    return alpha./beta
end
eigvals!(A::AbstractMatrix, B::AbstractMatrix) = eigvals!(float(A), float(B))
eigvals{T<:BlasFloat}(A::AbstractMatrix{T}, B::AbstractMatrix{T}) = eigvals!(copy(A), copy(B))
eigvals(A::AbstractMatrix, B::AbstractMatrix) = eigvals!(float(A), float(B))

# SVD
type SVD{T<:BlasFloat,Tr} <: Factorization{T}
    U::Matrix{T}
    S::Vector{Tr}
    Vt::Matrix{T}
end
function svdfact!{T<:BlasFloat}(A::StridedMatrix{T}, thin::Bool)
    m,n = size(A)
    if m == 0 || n == 0
        u,s,vt = (eye(T, m, thin ? n : m), real(zeros(T,0)), eye(T,n,n))
    else
        u,s,vt = LAPACK.gesdd!(thin ? 'S' : 'A', A)
    end
    return SVD(u,s,vt)
end
svdfact!(A::StridedVecOrMat, args...) = svdfact!(float(A), args...)
svdfact!{T<:BlasFloat}(a::Vector{T}, thin::Bool) = svdfact!(reshape(a, length(a), 1), thin)
svdfact!{T<:BlasFloat}(A::StridedVecOrMat{T}) = svdfact!(A, true)
function svdfact(A::StridedVecOrMat, args...)
    if eltype(A) <: BlasFloat
        A = copy(A)
    else
        A = float(A)
    end
    svdfact!(A, args...)
end
svdfact(x::Number, thin::Bool) = SVD(x == 0 ? fill(one(x), 1, 1) : fill(x/abs(x), 1, 1), [abs(x)], fill(one(x), 1, 1))
svdfact(x::Integer, thin::Bool) = svdfact(float(x), thin)
svdfact(x::Number) = svdfact(x, true)

function svd(A::Union(Number, AbstractArray), thin::Bool)
    F = svdfact(A, thin)
    return F.U, F.S, F.Vt'
end
svd(A::Union(Number, AbstractArray)) = svd(A, true)

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
svdvals(x::Number) = [abs(x)]

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

function svd(A::AbstractMatrix, B::AbstractMatrix)
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

function schur(A::AbstractMatrix)
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

function schur(A::AbstractMatrix, B::AbstractMatrix)
    SchurF = schurfact(A, B)
    return SchurF[:S], SchurF[:T], SchurF[:Q], SchurF[:Z]
end
