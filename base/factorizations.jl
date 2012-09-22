abstract Factorization{T}
## Create an extractor that extracts the modified original matrix, e.g.
## LD for BunchKaufman, LR for CholeskyDense, LU for LUDense and
## define size methods for Factorization types using it.

## merge symmetrize with _jl_copy_upper_to_lower in blas.jl
## maybe another function makehermitian and use conj for complex
function symmetrize!(A::AbstractMatrix, upper::Bool)
    m, n = size(A)
    if m != n error("symmetrize: Matrix must be square") end
    for i = 1:(n-1)
        for j = (i+1):n
            if upper A[j,i] = A[i,j] else A[i,j] = A[j,i] end
        end
    end
    A
end

type BunchKaufman{T<:LapackScalar} <: Factorization{T}
    LD::Matrix{T}
    ipiv::Vector{Int32}
    upper::Bool
    function BunchKaufman(A::Matrix{T}, upper::Bool)
        LD, ipiv = Lapack.sytrf!(upper ? 'U' : 'L' , copy(A))
        new(LD, ipiv, upper)
    end
end

BunchKaufman{T<:LapackScalar}(A::StridedMatrix{T}, upper::Bool) = BunchKaufman{T}(A, upper)
BunchKaufman{T<:Real}(A::StridedMatrix{T}, upper::Bool) = BunchKaufman(float64(A), upper)
BunchKaufman{T<:Number}(A::StridedMatrix{T}) = BunchKaufman(A, true)

size(B::BunchKaufman) = size(B.LD)
size(B::BunchKaufman,d::Integer) = size(B.LD,d)
## need to work out how to extract the factors.
#factors(B::BunchKaufman) = Lapack.syconv!(B.upper ? 'U' : 'L', copy(B.LD), B.ipiv)

function inv(B::BunchKaufman)
    symmetrize!(Lapack.sytri!(B.upper ? 'U' : 'L', copy(B.LD), B.ipiv), B.upper)
end

\{T<:LapackScalar}(B::BunchKaufman{T}, R::StridedVecOrMat{T}) =
    Lapack.sytrs!(B.upper ? 'U' : 'L', B.LD, B.ipiv, copy(R))
    
type CholeskyDense{T<:LapackScalar} <: Factorization{T}
    LR::Matrix{T}
    upper::Bool
    function CholeskyDense(A::Matrix{T}, upper::Bool)
        A, info = Lapack.potrf!(upper ? 'U' : 'L' , A)
        if info != 0 error("Matrix A not positive-definite") end
        new(upper? triu(A) : tril(A), upper)
    end
end

size(C::CholeskyDense) = size(C.LR)
size(C::CholeskyDense,d::Integer) = size(C.LR,d)

factors(C::CholeskyDense) = C.LR

\{T<:LapackScalar}(C::CholeskyDense{T}, B::StridedVecOrMat{T}) =
    Lapack.potrs!(C.upper ? 'U' : 'L', C.LR, copy(B))

function det(C::CholeskyDense)
    ff = C.LR
    dd = 0.
    for i in 1:size(ff,1) dd += ff[i,i]^2 end
    dd
end
    
function inv(C::CholeskyDense)
    Ci, info = Lapack.potri!(C.upper ? 'U' : 'L', copy(C.LR))
    if info != 0 error("Matrix singular") end 
    symmetrize!(Ci, C.upper)
end

## Should these functions check that the matrix is Hermitian?
chold!{T<:LapackScalar}(A::Matrix{T}, upper::Bool) = CholeskyDense{T}(A, upper)
chold{T<:LapackScalar}(A::Matrix{T}, upper::Bool) = chold!(copy(A), upper)
chold{T<:Number}(A::Matrix{T}, upper::Bool) = chold(float64(A), upper)
chold{T<:Number}(A::Matrix{T}) = chold(A, true)

## Matlab (and R) compatible
chol{T<:Number}(A::Matrix{T}) = factors(chold(A))
 
type LUDense{T} <: Factorization{T}
    lu::Matrix{T}
    ipiv::Vector{Int32}
    function LUDense(lu::Matrix{T}, ipiv::Vector{Int32})
        m, n = size(lu)
        m == numel(ipiv) ? new(lu, ipiv) : error("LUDense: dimension mismatch")
    end
end

size(A::LUDense) = size(A.lu)
size(A::LUDense,n) = size(A.lu,n)

function factors{T<:LapackScalar}(lu::LUDense{T}) 
    LU, ipiv = lu.lu, lu.ipiv
    m, n = size(LU)

    L = m >= n ? tril(LU, -1) + eye(m,n) : tril(LU, -1)[:, 1:m] + eye(m,m)
    U = m <= n ? triu(LU) : triu(LU)[1:n, :]
    P = [1:m]
    for i = 1:min(m,n)
        t = P[i]
        P[i] = P[ipiv[i]]
        P[ipiv[i]] = t
    end
    L, U, P
end

function lud!{T<:LapackScalar}(A::Matrix{T})
    lu, ipiv = Lapack.getrf!(A)
    LUDense{T}(lu, ipiv)
end

lud{T<:LapackScalar}(A::Matrix{T}) = lud!(copy(A))
lud{T<:Number}(A::Matrix{T}) = lud(float64(A))

## Matlab-compatible
lu{T<:Number}(A::Matrix{T}) = factors(lud(A))

function det(lu::LUDense)
    m, n = size(lu.lu)
    if m != n error("det only defined for square matrices") end
    prod(diag(lu.lu)) * (bool(sum(lu.ipiv .!= 1:n) % 2) ? -1 : 1)
end

det(A::Matrix) = det(lud(A))

(\){T<:LapackScalar}(lu::LUDense{T}, B::StridedVecOrMat{T}) =
    Lapack.getrs!('N', lu.lu, lu.ipiv, copy(B))

inv{T<:LapackScalar}(lu::LUDense{T}) = Lapack.getri!(copy(lu.lu), lu.ipiv)

## QR decomposition without column pivots
type QRDense{T} <: Factorization{T}
    hh::Matrix{T}                       # Householder transformations and R
    tau::Vector{T}                      # Scalar factors of transformations
    function QRDense(hh::Matrix{T}, tau::Vector{T})
        numel(tau) == min(size(hh)) ? new(hh, tau) : error("QR: mismatched dimensions")
    end
end
size(A::QRDense) = size(A.hh)
size(A::QRDense,n) = size(A.hh,n)

qrd!{T<:LapackScalar}(A::StridedMatrix{T}) = QRDense{T}(Lapack.geqrf!(A)...)
qrd{T<:LapackScalar}(A::StridedMatrix{T}) = qrd!(copy(A))
qrd{T<:Real}(A::StridedMatrix{T}) = qrd(float64(A))

function factors{T<:LapackScalar}(qrd::QRDense{T})
    aa  = copy(qrd.hh)
    R   = triu(aa[1:min(size(aa)),:])   # must be *before* call to orgqr!
    Lapack.orgqr!(aa, qrd.tau, size(aa,2)), R
end

qr{T<:Number}(x::StridedMatrix{T}) = factors(qrd(x))

## Multiplication by Q from the QR decomposition
(*){T<:LapackScalar}(A::QRDense{T}, B::StridedVecOrMat{T}) =
    Lapack.ormqr!('L', 'N', A.hh, size(A.hh,2), A.tau, copy(B))

## Multiplication by Q' from the QR decomposition
Ac_mul_B{T<:LapackScalar}(A::QRDense{T}, B::StridedVecOrMat{T}) =
    Lapack.ormqr!('L', iscomplex(A.tau)?'C':'T', A.hh, size(A.hh,2), A.tau, copy(B))

## Least squares solution.  Should be more careful about cases with m < n
function (\){T<:LapackScalar}(A::QRDense{T}, B::StridedVecOrMat{T})
    n   = length(A.tau)
    Lapack.trtrs!('U','N','N',A.hh[1:n,:],(A'*B)[1:n,:])
end

type QRPDense{T} <: Factorization{T}
    hh::Matrix{T}
    tau::Vector{T}
    jpvt::Vector{Int32}
    function QRPDense(hh::Matrix{T}, tau::Vector{T}, jpvt::Vector{Int32})
        m, n = size(hh)
        if length(tau) != min(m,n) || length(jpvt) != n
            error("QRPDense: mismatched dimensions")
        end
        new(hh,tau,jpvt)
    end
end
size(x::QRPDense)   = size(x.hh)
size(x::QRPDense,d) = size(x.hh,d)
## Multiplication by Q from the QR decomposition
(*){T<:LapackScalar}(A::QRPDense{T}, B::StridedVecOrMat{T}) =
    Lapack.ormqr!('L', 'N', A.hh, size(A,2), A.tau, copy(B))
## Multiplication by Q' from the QR decomposition
Ac_mul_B{T<:LapackScalar}(A::QRPDense{T}, B::StridedVecOrMat{T}) =
    Lapack.ormqr!('L', iscomplex(A.tau)?'C':'T', A.hh, size(A,2), A.tau, copy(B))

qrpd!{T<:LapackScalar}(A::StridedMatrix{T}) = QRPDense{T}(Lapack.geqp3!(A)...)
qrpd{T<:LapackScalar}(A::StridedMatrix{T}) = qrpd!(copy(A))
qrpd{T<:Real}(x::StridedMatrix{T}) = qrpd(float64(x))

function factors{T<:LapackScalar}(x::QRPDense{T})
    aa = copy(x.hh)
    R  = triu(aa[1:min(size(aa)),:])
    Lapack.orgqr!(aa, x.tau, size(aa,2)), R, x.jpvt
end

qrp{T<:LapackScalar}(x::StridedMatrix{T}) = factors(qrpd(x))
qrp{T<:Real}(x::StridedMatrix{T}) = qrp(float64(x))

function (\){T<:LapackScalar}(A::QRPDense{T}, B::StridedVecOrMat{T})
    n = length(A.tau)
    x = Lapack.trtrs!('U','N','N',A.hh[1:n,:],(A'*B)[1:n,:])
    isa(B, Vector) ? x[invperm(A.jpvt)] : x[:,invperm(A.jpvt)]
end

##ToDo:  Add methods for rank(A::QRP{T}) and adjust the (\) method accordingly
##       Add rcond methods for Cholesky, LU, QR and QRP types
## Lower priority: Add LQ, QL and RQ factorizations

#### Factorizations for Tridiagonal ####
type LDLTTridiagonal{T} <: Factorization{T}
    D::Vector{T}
    E::Vector{T}
end

ldltd!{T<:LapackScalar}(A::SymTridiagonal{T}) = LDLTTridiagonal{T}(Lapack.pttrf!(A.dv,A.ev)...)
ldltd{T<:LapackScalar}(A::SymTridiagonal{T}) = ldltd!(copy(A))

(\){T<:LapackScalar}(C::LDLTTridiagonal{T}, B::StridedVecOrMat{T}) =
    Lapack.pttrs!(C.D, C.E, copy(B))

type LUTridiagonal{T} <: Factorization{T}
    dl::Vector{T}
    d::Vector{T}
    du::Vector{T}
    du2::Vector{T}
    ipiv::Vector{Int32}
    function LUTridiagonal(dl::Vector{T}, d::Vector{T}, du::Vector{T},
                           du2::Vector{T}, ipiv::Vector{Int32})
        n = length(d)
        if length(dl) != n - 1 || length(du) != n - 1 || length(ipiv) != n || length(du2) != n-2
            error("LUTridiagonal: dimension mismatch")
        end
        new(dl, d, du, du2, ipiv)
    end
end
#show(io, lu::LUTridiagonal) = print(io, "LU decomposition of ", summary(lu.lu))

lud!{T}(A::Tridiagonal{T}) = LUTridiagonal{T}(Lapack.gttrf!(A.dl,A.d,A.du)...)
lud{T}(A::Tridiagonal{T}) =
    LUTridiagonal{T}(Lapack.gttrf!(copy(A.dl),copy(A.d),copy(A.du))...)
lu(A::Tridiagonal) = factors(lud(A))

function det(lu::LUTridiagonal)
    prod(lu.d) * (bool(sum(lu.ipiv .!= 1:n) % 2) ? -1 : 1)
end

det(A::Tridiagonal) = det(lud(A))

(\){T<:LapackScalar}(lu::LUTridiagonal{T}, B::StridedVecOrMat{T}) =
    Lapack.gttrs!('N', lu.dl, lu.d, lu.du, lu.du2, lu.ipiv, copy(B))
