## linalg_dense.jl: Linear Algebra functions for dense representations ##

function issym(A::Matrix)
    m, n = size(A)
    if m != n; error("matrix must be square, got $(m)x$(n)"); end
    for i = 1:(n-1), j = (i+1):n
        if A[i,j] != A[j,i]
            return false
        end
    end
    return true
end

function ishermitian(A::Matrix)
    m, n = size(A)
    if m != n; error("matrix must be square, got $(m)x$(n)"); end
    for i = 1:n, j = i:n
        if A[i,j] != conj(A[j,i])
            return false
        end
    end
    return true
end

function istriu(A::Matrix)
    m, n = size(A)
    for j = 1:min(n,m-1), i = j+1:m
        if A[i,j] != 0
            return false
        end
    end
    return true
end

function istril(A::Matrix)
    m, n = size(A)
    for j = 2:n, i = 1:min(j-1,m)
        if A[i,j] != 0
            return false
        end
    end
    return true
end

norm{T<:LapackType}(x::Vector{T}) = BLAS.nrm2(length(x), x, 1)

function norm{T<:LapackType, TI<:Integer}(x::Vector{T}, rx::Union(Range1{TI},Range{TI}))
    if min(rx) < 1 || max(rx) > length(x)
        throw(BoundsError())
    end
    BLAS.nrm2(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx))
end

function triu!{T}(M::Matrix{T}, k::Integer)
    m, n = size(M)
    idx = 1
    for j = 0:n-1
        ii = min(max(0, j+1-k), m)
        for i = (idx+ii):(idx+m-1)
            M[i] = zero(T)
        end
        idx += m
    end
    return M
end

triu(M::Matrix, k::Integer) = triu!(copy(M), k)

function tril!{T}(M::Matrix{T}, k::Integer)
    m, n = size(M)
    idx = 1
    for j = 0:n-1
        ii = min(max(0, j-k), m)
        for i = idx:(idx+ii-1)
            M[i] = zero(T)
            end
        idx += m
    end
    return M
end

tril(M::Matrix, k::Integer) = tril!(copy(M), k)

diff(a::Vector) = [ a[i+1] - a[i] for i=1:length(a)-1 ]

function diff(a::Matrix, dim::Integer)
    if dim == 1
        [ a[i+1,j] - a[i,j] for i=1:size(a,1)-1, j=1:size(a,2) ]
    else
        [ a[i,j+1] - a[i,j] for i=1:size(a,1), j=1:size(a,2)-1 ]
    end
end

function gradient(F::Vector, h::Vector)
    n = length(F)
    g = similar(F)
    if n > 0
        g[1] = 0
    end
    if n > 1
        g[1] = (F[2] - F[1]) / (h[2] - h[1])
        g[n] = (F[n] - F[n-1]) / (h[end] - h[end-1])
    end
    if n > 2
        h = h[3:n] - h[1:n-2]
        g[2:n-1] = (F[3:n] - F[1:n-2]) ./ h
    end
    return g
end

diag(A::Matrix) = [ A[i,i] for i=1:min(size(A,1),size(A,2)) ]

function diagm{T}(v::Union(Vector{T},Matrix{T}))
    if isa(v, Matrix)
        if (size(v,1) != 1 && size(v,2) != 1)
            error("Input should be nx1 or 1xn")
        end
    end

    n = numel(v)
    a = zeros(T, n, n)
    for i=1:n
        a[i,i] = v[i]
    end

    return a
end

function trace{T}(A::Matrix{T})
    t = zero(T)
    for i=1:min(size(A))
        t += A[i,i]
    end
    return t
end

kron(a::Vector, b::Vector) = [ a[i]*b[j] for i=1:length(a), j=1:length(b) ]

function kron{T,S}(a::Matrix{T}, b::Matrix{S})
    R = Array(promote_type(T,S), size(a,1)*size(b,1), size(a,2)*size(b,2))

    m = 1
    for j = 1:size(a,2)
        for l = 1:size(b,2)
            for i = 1:size(a,1)
                aij = a[i,j]
                for k = 1:size(b,1)
                    R[m] = aij*b[k,l]
                    m += 1
                end
            end
        end
    end
    R
end

function randsym(n)
    a = randn(n,n)
    for j=1:n-1, i=j+1:n
        x = (a[i,j]+a[j,i])/2
        a[i,j] = x
        a[j,i] = x
    end
    a
end

^(A::Matrix, p::Integer) = p < 0 ? inv(A^-p) : power_by_squaring(A,p)

function ^(A::Matrix, p::Number)
    if integer_valued(p)
        ip = integer(real(p))
        if ip < 0
            return inv(power_by_squaring(A, -ip))
        else
            return power_by_squaring(A, ip)
        end
    end
    if size(A,1) != size(A,2)
        error("matrix must be square")
    end
    (v, X) = eig(A)
    if isreal(v) && any(v.<0)
        v = complex(v)
    end
    if ishermitian(A)
        Xinv = X'
    else
        Xinv = inv(X)
    end
    diagmm(X, v.^p)*Xinv
end

function rref{T}(A::Matrix{T})
    nr, nc = size(A)
    U = copy_to(similar(A,Float64), A)
    e = eps(norm(U,Inf))
    i = j = 1
    while i <= nr && j <= nc
        (m, mi) = findmax(abs(U[i:nr,j]))
        mi = mi+i - 1
        if m <= e
            U[i:nr,j] = 0
            j += 1
        else
            for k=j:nc
                U[i, k], U[mi, k] = U[mi, k], U[i, k]
            end
            d = U[i,j]
            for k = j:nc
                U[i,k] /= d
            end
            for k = 1:nr
                if k != i
                    d = U[k,j]
                    for l = j:nc
                        U[k,l] -= d*U[i,l]
                    end
                end
            end
            i += 1
            j += 1
        end
    end
    return U
end

## Destructive matrix exponential using algorithm from Higham, 2008,
## "Functions of Matrices: Theory and Computation", SIAM
function expm!{T<:LapackType}(A::StridedMatrix{T})
    m, n = size(A)
    if m != n error("expm!: Matrix A must be square") end
    if m < 2 return exp(A) end
    ilo, ihi, scale = LAPACK.gebal!('B', A)    # modifies A
    nA   = norm(A, 1)
    I    = convert(Array{T,2}, eye(n))
    ## For sufficiently small nA, use lower order Padé-Approximations
    if (nA <= 2.1)
        if nA > 0.95
            C = [17643225600.,8821612800.,2075673600.,302702400.,
                    30270240.,   2162160.,    110880.,     3960.,
                          90.,         1.]
        elseif nA > 0.25
            C = [17297280.,8648640.,1995840.,277200.,
                    25200.,   1512.,     56.,     1.]
        elseif nA > 0.015
            C = [30240.,15120.,3360.,
                   420.,   30.,   1.]
        else
            C = [120.,60.,12.,1.]
        end
        A2 = A * A
        P  = copy(I)
        U  = C[2] * P
        V  = C[1] * P
        for k in 1:(div(size(C, 1), 2) - 1)
            k2 = 2 * k
            P *= A2
            U += C[k2 + 2] * P
            V += C[k2 + 1] * P
        end
        U  = A * U
        X  = (V - U)\(V + U)
    else
        s  = log2(nA/5.4)               # power of 2 later reversed by squaring
        if s > 0
            si = iceil(s)
            A /= 2^si
        end
        CC = [64764752532480000.,32382376266240000.,7771770303897600.,
               1187353796428800.,  129060195264000.,  10559470521600.,
                   670442572800.,      33522128640.,      1323241920.,
                       40840800.,           960960.,           16380.,
                            182.,                1.]
        A2 = A * A
        A4 = A2 * A2
        A6 = A2 * A4
        U  = A * (A6 * (CC[14]*A6 + CC[12]*A4 + CC[10]*A2) +
                  CC[8]*A6 + CC[6]*A4 + CC[4]*A2 + CC[2]*I)
        V  = A6 * (CC[13]*A6 + CC[11]*A4 + CC[9]*A2) +
                  CC[7]*A6 + CC[5]*A4 + CC[3]*A2 + CC[1]*I
        X  = (V-U)\(V+U)
                         
        if s > 0            # squaring to reverse dividing by power of 2
            for t in 1:si X *= X end
        end
    end
                                        # Undo the balancing
    doscale = false                     # check if rescaling is needed
    for i = ilo:ihi
        if scale[i] != 1.
            doscale = true
            break
        end
    end
    if doscale
        for j = ilo:ihi
            scj = scale[j]
            if scj != 1.                # is this overkill?
                for i = ilo:ihi
                    X[i,j] *= scale[i]/scj
                end
            else
                for i = ilo:ihi
                    X[i,j] *= scale[i]
                end
            end
        end
    end
    if ilo > 1       # apply lower permutations in reverse order
        for j in (ilo-1):1:-1 rcswap!(j, int(scale[j]), X) end
    end
    if ihi < n       # apply upper permutations in forward order
        for j in (ihi+1):n    rcswap!(j, int(scale[j]), X) end
    end
    X
end

## Swap rows j and jp and columns j and jp in X
function rcswap!{T<:Number}(j::Int, jp::Int, X::StridedMatrix{T})
    for k in 1:size(X, 2)
        tmp     = X[k,j]
        X[k,j]  = X[k,jp]
        X[k,jp] = tmp
        tmp     = X[j,k]
        X[j,k]  = X[jp,k]
        X[jp,k] = tmp
    end
end

# Matrix exponential
expm{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T}) = expm!(copy(A))
expm{T<:Integer}(A::StridedMatrix{T}) = expm!(float(A))

## Matrix factorizations and decompositions

abstract Factorization{T}
## Create an extractor that extracts the modified original matrix, e.g.
## LD for BunchKaufman, LR for CholeskyDense, LU for LUDense and
## define size methods for Factorization types using it.

type BunchKaufman{T<:LapackType} <: Factorization{T}
    LD::Matrix{T}
    ipiv::Vector{Int32}
    upper::Bool
    function BunchKaufman(A::Matrix{T}, upper::Bool)
        LD, ipiv = LAPACK.sytrf!(upper ? 'U' : 'L' , copy(A))
        new(LD, ipiv, upper)
    end
end

BunchKaufman{T<:LapackType}(A::StridedMatrix{T}, upper::Bool) = BunchKaufman{T}(A, upper)
BunchKaufman{T<:Real}(A::StridedMatrix{T}, upper::Bool) = BunchKaufman(float64(A), upper)
BunchKaufman{T<:Number}(A::StridedMatrix{T}) = BunchKaufman(A, true)

size(B::BunchKaufman) = size(B.LD)
size(B::BunchKaufman,d::Integer) = size(B.LD,d)
## need to work out how to extract the factors.
#factors(B::BunchKaufman) = LAPACK.syconv!(B.upper ? 'U' : 'L', copy(B.LD), B.ipiv)

function inv(B::BunchKaufman)
    symmetrize!(LAPACK.sytri!(B.upper ? 'U' : 'L', copy(B.LD), B.ipiv), B.upper)
end

\{T<:LapackType}(B::BunchKaufman{T}, R::StridedVecOrMat{T}) =
    LAPACK.sytrs!(B.upper ? 'U' : 'L', B.LD, B.ipiv, copy(R))
    
type CholeskyDense{T<:LapackType} <: Factorization{T}
    LR::Matrix{T}
    upper::Bool
    function CholeskyDense(A::Matrix{T}, upper::Bool)
        A, info = LAPACK.potrf!(upper ? 'U' : 'L' , A)
        if info != 0 error("Matrix A not positive-definite") end
        new(upper? triu!(A) : tril!(A), upper)
    end
end

size(C::CholeskyDense) = size(C.LR)
size(C::CholeskyDense,d::Integer) = size(C.LR,d)

factors(C::CholeskyDense) = C.LR

\{T<:LapackType}(C::CholeskyDense{T}, B::StridedVecOrMat{T}) =
    LAPACK.potrs!(C.upper ? 'U' : 'L', C.LR, copy(B))

function det(C::CholeskyDense)
    ff = C.LR
    dd = 1.
    for i in 1:size(ff,1) dd *= abs2(ff[i,i]) end
    dd
end
    
function inv(C::CholeskyDense)
    Ci, info = LAPACK.potri!(C.upper ? 'U' : 'L', copy(C.LR))
    if info != 0 error("Matrix singular") end 
    symmetrize!(Ci, C.upper)
end

## Should these functions check that the matrix is Hermitian?
chold!{T<:LapackType}(A::Matrix{T}, upper::Bool) = CholeskyDense{T}(A, upper)
chold!{T<:LapackType}(A::Matrix{T}) = chold!(A, true)
chold{T<:LapackType}(A::Matrix{T}, upper::Bool) = chold!(copy(A), upper)
chold{T<:Number}(A::Matrix{T}, upper::Bool) = chold(float64(A), upper)
chold{T<:Number}(A::Matrix{T}) = chold(A, true)

## Matlab (and R) compatible
chol{T<:Number}(A::Matrix{T}) = factors(chold(A))

type CholeskyDensePivoted{T<:LapackType} <: Factorization{T}
    LR::Matrix{T}
    upper::Bool
    piv::Vector{Int32}
    rank::Int32
    tol::Real
    function CholeskyDensePivoted(A::Matrix{T}, upper::Bool, tol::Real)
        A, piv, rank, info = LAPACK.pstrf!(upper ? 'U' : 'L' , A, tol)
        if info != 0 error("Matrix A not positive-definite") end
        new(upper? triu!(A) : tril!(A), upper, piv, rank, tol)
    end
end

size(C::CholeskyDensePivoted) = size(C.LR)
size(C::CholeskyDensePivoted,d::Integer) = size(C.LR,d)

factors(C::CholeskyDensePivoted) = C.LR, C.piv

\{T<:LapackType}(C::CholeskyDensePivoted{T}, B::StridedVecOrMat{T}) =
    LAPACK.potrs!(C.upper ? 'U' : 'L', C.LR, copy(B)[C.piv])[invperm(C.piv)]

rank(C::CholeskyDensePivoted) = C.rank

det(C::CholeskyDensePivoted) = prod(abs2(diag(C.LR)))
    
function inv(C::CholeskyDensePivoted)
    if C.rank < size(C.LR, 1) error("Matrix singular") end
    Ci, info = LAPACK.potri!(C.upper ? 'U' : 'L', copy(C.LR))
    if info != 0 error("Matrix singular") end
    ipiv = invperm(C.piv)
    (symmetrize!(Ci, C.upper))[ipiv, ipiv]
end

## Should these functions check that the matrix is Hermitian?
cholpd!{T<:LapackType}(A::Matrix{T}, upper::Bool, tol::Real) = CholeskyDensePivoted{T}(A, upper, tol)
cholpd!{T<:LapackType}(A::Matrix{T}, upper::Bool) = cholpd!(A, upper, -1.)
cholpd!{T<:LapackType}(A::Matrix{T}, tol::Real) = cholpd!(A, true, tol)
cholpd!{T<:LapackType}(A::Matrix{T}) = cholpd!(A, true, -1.)
cholpd{T<:Number}(A::Matrix{T}, upper::Bool, tol::Real) = cholpd(float64(A), upper, tol)
cholpd{T<:Number}(A::Matrix{T}, upper::Bool) = cholpd(float64(A), upper, -1.)
cholpd{T<:Number}(A::Matrix{T}, tol::Real) = cholpd(float64(A), true, tol)
cholpd{T<:Number}(A::Matrix{T}) = cholpd(float64(A), true, -1.)
cholpd{T<:LapackType}(A::Matrix{T}, upper::Bool, tol::Real) = cholpd!(copy(A), upper, tol)
cholpd{T<:LapackType}(A::Matrix{T}, upper::Bool) = cholpd!(copy(A), upper, -1.)
cholpd{T<:LapackType}(A::Matrix{T}, tol::Real) = cholpd!(copy(A), true, tol)
cholpd{T<:LapackType}(A::Matrix{T}) = cholpd!(copy(A), true, -1.)

type LUDense{T} <: Factorization{T}
    lu::Matrix{T}
    ipiv::Vector{Int32}
    info::Int32
    function LUDense(lu::Matrix{T}, ipiv::Vector{Int32}, info::Int32)
        m, n = size(lu)
        m == n ? new(lu, ipiv, info) : error("LUDense only defined for square matrices")
    end
end

size(A::LUDense) = size(A.lu)
size(A::LUDense,n) = size(A.lu,n)

function factors{T<:LapackType}(lu::LUDense{T}) 
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

function lud!{T<:LapackType}(A::Matrix{T})
    lu, ipiv, info = LAPACK.getrf!(A)
    LUDense{T}(lu, ipiv, info)
end

lud{T<:LapackType}(A::Matrix{T}) = lud!(copy(A))
lud{T<:Number}(A::Matrix{T}) = lud(float64(A))

## Matlab-compatible
lu{T<:Number}(A::Matrix{T}) = factors(lud(A))

function det(lu::LUDense)
    m, n = size(lu)
    if lu.info > 0; return zero(typeof(lu.lu[1])); end
    prod(diag(lu.lu)) * (bool(sum(lu.ipiv .!= 1:n) % 2) ? -1 : 1)
end

function det(A::Matrix)
    m, n = size(A)
    if m != n; error("det only defined for square matrices"); end
    if istriu(A) | istril(A); return prod(diag(A)); end
    return det(lud(A))
end

function (\){T<:LapackType}(lu::LUDense{T}, B::StridedVecOrMat{T})
    if lu.info > 0; error("Singular system"); end
    LAPACK.getrs!('N', lu.lu, lu.ipiv, copy(B))
end

function inv{T<:LapackType}(lu::LUDense{T})
    m, n = size(lu.lu)
    if m != n; error("inv only defined for square matrices"); end
    if lu.info > 0; return error("Singular system"); end
    LAPACK.getri!(copy(lu.lu), lu.ipiv)
end

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

qrd!{T<:LapackType}(A::StridedMatrix{T}) = QRDense{T}(LAPACK.geqrf!(A)...)
qrd{T<:LapackType}(A::StridedMatrix{T}) = qrd!(copy(A))
qrd{T<:Real}(A::StridedMatrix{T}) = qrd(float64(A))

function factors{T<:LapackType}(qrd::QRDense{T})
    aa  = copy(qrd.hh)
    R   = triu(aa[1:min(size(aa)),:])   # must be *before* call to orgqr!
    LAPACK.orgqr!(aa, qrd.tau, size(aa,2)), R
end

qr{T<:Number}(x::StridedMatrix{T}) = factors(qrd(x))

## Multiplication by Q from the QR decomposition
(*){T<:LapackType}(A::QRDense{T}, B::StridedVecOrMat{T}) =
    LAPACK.ormqr!('L', 'N', A.hh, size(A.hh,2), A.tau, copy(B))

## Multiplication by Q' from the QR decomposition
Ac_mul_B{T<:LapackType}(A::QRDense{T}, B::StridedVecOrMat{T}) =
    LAPACK.ormqr!('L', iscomplex(A.tau)?'C':'T', A.hh, size(A.hh,2), A.tau, copy(B))

## Least squares solution.  Should be more careful about cases with m < n
function (\){T<:LapackType}(A::QRDense{T}, B::StridedVecOrMat{T})
    n   = length(A.tau)
    ans, info = LAPACK.trtrs!('U','N','N',A.hh[1:n,:],(A'*B)[1:n,:])
    if info > 0; error("Singular system"); end
    return ans
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
(*){T<:LapackType}(A::QRPDense{T}, B::StridedVecOrMat{T}) =
    LAPACK.ormqr!('L', 'N', A.hh, size(A,2), A.tau, copy(B))
## Multiplication by Q' from the QR decomposition
Ac_mul_B{T<:LapackType}(A::QRPDense{T}, B::StridedVecOrMat{T}) =
    LAPACK.ormqr!('L', iscomplex(A.tau)?'C':'T', A.hh, size(A,2), A.tau, copy(B))

qrpd!{T<:LapackType}(A::StridedMatrix{T}) = QRPDense{T}(LAPACK.geqp3!(A)...)
qrpd{T<:LapackType}(A::StridedMatrix{T}) = qrpd!(copy(A))
qrpd{T<:Real}(x::StridedMatrix{T}) = qrpd(float64(x))

function factors{T<:LapackType}(x::QRPDense{T})
    aa = copy(x.hh)
    R  = triu(aa[1:min(size(aa)),:])
    LAPACK.orgqr!(aa, x.tau, size(aa,2)), R, x.jpvt
end

qrp{T<:LapackType}(x::StridedMatrix{T}) = factors(qrpd(x))
qrp{T<:Real}(x::StridedMatrix{T}) = qrp(float64(x))

function (\){T<:LapackType}(A::QRPDense{T}, B::StridedVecOrMat{T})
    n = length(A.tau)
    x, info = LAPACK.trtrs!('U','N','N',A.hh[1:n,:],(A'*B)[1:n,:])
    if info > 0; error("Singular system"); end
    isa(B, Vector) ? x[invperm(A.jpvt)] : x[:,invperm(A.jpvt)]
end

function eig{T<:LapackType}(A::StridedMatrix{T}, vecs::Bool)
    n = size(A, 2)
    if n == 0; return vecs ? (zeros(T, 0), zeros(T, 0, 0)) : zeros(T, 0, 0); end

    if ishermitian(A); return LAPACK.syev!(vecs ? 'V' : 'N', 'U', copy(A)); end

    if iscomplex(A)
        W, VR = LAPACK.geev!('N', vecs ? 'V' : 'N', copy(A))[2:3]
        if vecs; return W, VR; end
        return W
    end

    VL, WR, WI, VR = LAPACK.geev!('N', vecs ? 'V' : 'N', copy(A))
    if all(WI .== 0.) 
        if vecs; return WR, VR; end
        return WR
    end
    if vecs    
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
    complex(WR, WI)
end

eig{T<:Integer}(x::StridedMatrix{T}, vecs::Bool) = eig(float64(x), vecs)
eig(x::StridedMatrix) = eig(x, true)
eigvals(x::StridedMatrix) = eig(x, false)

# This is the svd based on the LAPACK GESVD, which is slower, but takes
# lesser memory. It should be made available through a keyword argument
# at a later date.
#
# function svd{T<:LapackType}(A::StridedMatrix{T},vecs::Bool,thin::Bool)
#     m,n = size(A)
#     if m == 0 || n == 0
#         if vecs; return (eye(m, thin ? n : m), zeros(0), eye(n,n)); end
#         return (zeros(T, 0, 0), zeros(T, 0), zeros(T, 0, 0))
#     end
#     if vecs; return LAPACK.gesvd!(thin ? 'S' : 'A', thin ? 'S' : 'A', copy(A)); end
#     LAPACK.gesvd!('N', 'N', copy(A))
# end
#
# svd{T<:Integer}(x::StridedMatrix{T},vecs,thin) = svd(float64(x),vecs,thin)
# svd(A::StridedMatrix) = svd(A,true,false)
# svd(A::StridedMatrix, thin::Bool) = svd(A,true,thin)
# svdvals(A) = svd(A,false,true)[2]

function svd{T<:LapackType}(A::StridedMatrix{T},vecs::Bool,thin::Bool)
    m,n = size(A)
    if m == 0 || n == 0
        if vecs; return (eye(m, thin ? n : m), zeros(0), eye(n,n)); end
        return (zeros(T, 0, 0), zeros(T, 0), zeros(T, 0, 0))
    end
    if vecs; return LAPACK.gesdd!(thin ? 'S' : 'A', copy(A)); end
    LAPACK.gesdd!('N', copy(A))
end

svd{T<:Integer}(x::StridedMatrix{T},vecs,thin) = svd(float64(x),vecs,thin)
svd(A::StridedMatrix) = svd(A,true,false)
svd(A::StridedMatrix, thin::Bool) = svd(A,true,thin)
svdvals(A) = svd(A,false,true)[2]

function (\){T<:LapackType}(A::StridedMatrix{T}, B::StridedVecOrMat{T})
    Acopy = copy(A)
    m, n  = size(Acopy)
    X     = copy(B)

    if m == n # Square
        if istriu(A) 
            ans, info = LAPACK.trtrs!('U', 'N', 'N', Acopy, X)
            if info > 0; error("Singular system"); end
            return ans
        end
        if istril(A) 
            ans, info = LAPACK.trtrs!('L', 'N', 'N', Acopy, X) 
            if info > 0; error("Singular system"); end
            return ans
        end
        if ishermitian(A) 
            ans, _, _, info = LAPACK.sysv!('U', Acopy, X)
            if info > 0; error("Singular system"); end
            return ans
        end
        ans, _, _, info = LAPACK.gesv!(Acopy, X)
        if info > 0; error("Singular system"); end
        return ans
    end
    LAPACK.gelsd!(Acopy, X)[1]
end

(\){T1<:LapackType, T2<:LapackType}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) =
    (\)(convert(Array{promote_type(T1,T2)},A), convert(Array{promote_type(T1,T2)},B))
(\){T1<:LapackType, T2<:Real}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(A, convert(Array{T1}, B))
(\){T1<:Real, T2<:LapackType}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(convert(Array{T2}, A), B)
(\){T1<:Real, T2<:Real}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(float64(A), float64(B))
(\){T1<:Number, T2<:Number}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(complex128(A), complex128(B))

(/)(A::StridedVecOrMat, B::StridedVecOrMat) = (B' \ A')'

##TODO:  Add methods for rank(A::QRP{T}) and adjust the (\) method accordingly
##       Add rcond methods for Cholesky, LU, QR and QRP types
## Lower priority: Add LQ, QL and RQ factorizations

## Moore-Penrose inverse
function pinv{T<:LapackType}(A::StridedMatrix{T})
    u,s,vt      = svd(A, true)
    sinv        = zeros(T, length(s))
    index       = s .> eps(real(one(T)))*max(size(A))*max(s)
    sinv[index] = 1 ./ s[index]
    vt'diagmm(sinv, u')
end
pinv(A::StridedMatrix{Int}) = pinv(float(A))
pinv(a::StridedVector) = pinv(reshape(a, length(a), 1))

## Basis for null space
function null{T<:LapackType}(A::StridedMatrix{T})
    m,n = size(A)
    if m >= n; return zeros(T, n, 0); end;
    u,s,vt = svd(A)
    vt[m+1:,:]'
end
null(A::StridedMatrix{Int}) = null(float(A))
null(a::StridedVector) = null(reshape(a, length(a), 1))

#### Specialized matrix types ####

## Symmetric tridiagonal matrices
type SymTridiagonal{T<:LapackType} <: AbstractMatrix{T}
    dv::Vector{T}                        # diagonal
    ev::Vector{T}                        # sub/super diagonal
    function SymTridiagonal(dv::Vector{T}, ev::Vector{T})
        if length(ev) != length(dv) - 1 error("dimension mismatch") end
        new(dv,ev)
    end
end

SymTridiagonal{T<:LapackType}(dv::Vector{T}, ev::Vector{T}) = SymTridiagonal{T}(copy(dv), copy(ev))
function SymTridiagonal{T<:Real}(dv::Vector{T}, ev::Vector{T})
    SymTridiagonal{Float64}(float64(dv),float64(ev))
end
function SymTridiagonal{Td<:Number,Te<:Number}(dv::Vector{Td}, ev::Vector{Te})
    T = promote(Td,Tv)
    SymTridiagonal(convert(Vector{T}, dv), convert(Vector{T}, ev))
end
copy(S::SymTridiagonal) = SymTridiagonal(S.dv,S.ev)
function full(S::SymTridiagonal)
    M = diagm(S.dv)
    for i in 1:length(S.ev)
        j = i + 1
        M[i,j] = M[j,i] = S.ev[i]
    end
    M
end

function show(io, S::SymTridiagonal)
    println(io, summary(S), ":")
    print(io, "diag: ")
    print_matrix(io, (S.dv)')
    print(io, "\n sup: ")
    print_matrix(io, (S.ev)')
end

size(m::SymTridiagonal) = (length(m.dv), length(m.dv))
size(m::SymTridiagonal,d::Integer) = d<1 ? error("dimension out of range") : (d<2 ? length(m.dv) : 1)

eig(m::SymTridiagonal, vecs::Bool) = LAPACK.stev!(vecs ? 'V' : 'N', copy(m.dv), copy(m.ev))
eig(m::SymTridiagonal) = eig(m::SymTridiagonal, true)
## This function has been in Julia for some time.  Could probably be dropped.
trideig{T<:LapackType}(d::Vector{T}, e::Vector{T}) = LAPACK.stev!('N', copy(d), copy(e))[1]

## Tridiagonal matrices ##
type Tridiagonal{T} <: AbstractMatrix{T}
    dl::Vector{T}    # sub-diagonal
    d::Vector{T}     # diagonal
    du::Vector{T}    # sup-diagonal
    dutmp::Vector{T} # scratch space for vector RHS solver, sup-diagonal
    rhstmp::Vector{T}# scratch space, rhs

    function Tridiagonal(N::Int)
        dutmp = Array(T, N-1)
        rhstmp = Array(T, N)
        new(dutmp, rhstmp, dutmp, dutmp, rhstmp)  # first three will be overwritten
    end
end

function Tridiagonal{T<:Number}(dl::Vector{T}, d::Vector{T}, du::Vector{T})
    N = length(d)
    if length(dl) != N-1 || length(du) != N-1
        error("The sub- and super-diagonals must have length N-1")
    end
    M = Tridiagonal{T}(N)
    M.dl = copy(dl)
    M.d = copy(d)
    M.du = copy(du)
    return M
end
function Tridiagonal{Tl<:Number, Td<:Number, Tu<:Number}(dl::Vector{Tl}, d::Vector{Td}, du::Vector{Tu})
    R = promote(Tl, Td, Tu)
    Tridiagonal(convert(Vector{R}, dl), convert(Vector{R}, d), convert(Vector{R}, du))
end

size(M::Tridiagonal) = (length(M.d), length(M.d))
function show(io, M::Tridiagonal)
    println(io, summary(M), ":")
    print(io, " sub: ")
    print_matrix(io, (M.dl)')
    print(io, "\ndiag: ")
    print_matrix(io, (M.d)')
    print(io, "\n sup: ")
    print_matrix(io, (M.du)')
end
full{T}(M::Tridiagonal{T}) = convert(Matrix{T}, M)
function convert{T}(::Type{Matrix{T}}, M::Tridiagonal{T})
    A = zeros(T, size(M))
    for i = 1:length(M.d)
        A[i,i] = M.d[i]
    end
    for i = 1:length(M.d)-1
        A[i+1,i] = M.dl[i]
        A[i,i+1] = M.du[i]
    end
    return A
end
function similar(M::Tridiagonal, T, dims::Dims)
    if length(dims) != 2 || dims[1] != dims[2]
        error("Tridiagonal matrices must be square")
    end
    return Tridiagonal{T}(dims[1])
end
copy(M::Tridiagonal) = Tridiagonal(M.dl, M.d, M.du)

# Operations on Tridiagonal matrices
round(M::Tridiagonal) = Tridiagonal(round(M.dl), round(M.d), round(M.du))
iround(M::Tridiagonal) = Tridiagonal(iround(M.dl), iround(M.d), iround(M.du))

## Solvers

#### Tridiagonal matrix routines ####
function \{T<:LapackType}(M::Tridiagonal{T}, rhs::StridedVecOrMat{T})
    if stride(rhs, 1) == 1
        return LAPACK.gtsv!(copy(M.dl), copy(M.d), copy(M.du), copy(rhs))
    end
    solve(M, rhs)  # use the Julia "fallback"
end

# This is definitely not going to work
#eig(M::Tridiagonal) = LAPACK.stev!('V', copy(M))

# Allocation-free variants
# Note that solve is non-aliasing, so you can use the same array for
# input and output
function solve(x::AbstractArray, xrng::Ranges{Int}, M::Tridiagonal, rhs::AbstractArray, rhsrng::Ranges{Int})
    d = M.d
    N = length(d)
    if length(xrng) != N || length(rhsrng) != N
        error("dimension mismatch")
    end
    dl = M.dl
    du = M.du
    dutmp = M.dutmp
    rhstmp = M.rhstmp
    xstart = first(xrng)
    xstride = step(xrng)
    rhsstart = first(rhsrng)
    rhsstride = step(rhsrng)
    # Forward sweep
    denom = d[1]
    dulast = du[1] / denom
    dutmp[1] = dulast
    rhslast = rhs[rhsstart] / denom
    rhstmp[1] = rhslast
    irhs = rhsstart+rhsstride
    for i in 2:N-1
        dltmp = dl[i-1]
        denom = d[i] - dltmp*dulast
        dulast = du[i] / denom
        dutmp[i] = dulast
        rhslast = (rhs[irhs] - dltmp*rhslast)/denom
        rhstmp[i] = rhslast
        irhs += rhsstride
    end
    dltmp = dl[N-1]
    denom = d[N] - dltmp*dulast
    xlast = (rhs[irhs] - dltmp*rhslast)/denom
    # Backward sweep
    ix = xstart + (N-2)*xstride
    x[ix+xstride] = xlast
    for i in N-1:-1:1
        xlast = rhstmp[i] - dutmp[i]*xlast
        x[ix] = xlast
        ix -= xstride
    end
    return x
end
solve(x::StridedVector, M::Tridiagonal, rhs::StridedVector) = solve(x, 1:length(x), M, rhs, 1:length(rhs))
function solve(M::Tridiagonal, rhs::StridedVector)
    x = similar(rhs)
    solve(x, M, rhs)
end
function solve(X::StridedMatrix, M::Tridiagonal, B::StridedMatrix)
    if size(B, 1) != size(M, 1)
        error("dimension mismatch")
    end
    if size(X) != size(B)
        error("dimension mismatch in output")
    end
    m, n = size(B)
    r = 1:m
    for j = 1:n
        r.start = (j-1)*m+1
        solve(X, r, M, B, r)
    end
    return X
end
function solve(M::Tridiagonal, B::StridedMatrix)
    X = similar(B)
    solve(X, M, B)
end

# User-friendly solver
\(M::Tridiagonal, rhs::Union(StridedVector,StridedMatrix)) = solve(M, rhs)

# Tridiagonal multiplication
function mult(x::AbstractArray, xrng::Ranges{Int}, M::Tridiagonal, v::AbstractArray, vrng::Ranges{Int})
    dl = M.dl
    d = M.d
    du = M.du
    N = length(d)
    xi = first(xrng)
    xstride = step(xrng)
    vi = first(vrng)
    vstride = step(vrng)
    x[xi] = d[1]*v[vi] + du[1]*v[vi+vstride]
    xi += xstride
    for i = 2:N-1
        x[xi] = dl[i-1]*v[vi] + d[i]*v[vi+vstride] + du[i]*v[vi+2*vstride]
        xi += xstride
        vi += vstride
    end
    x[xi] = dl[N-1]*v[vi] + d[N]*v[vi+vstride]
    return x
end
mult(x::StridedVector, M::Tridiagonal, v::StridedVector) = mult(x, 1:length(x), M, v, 1:length(v))
function mult(X::StridedMatrix, M::Tridiagonal, B::StridedMatrix)
    if size(B, 1) != size(M, 1)
        error("dimension mismatch")
    end
    if size(X) != size(B)
        error("dimension mismatch in output")
    end
    m, n = size(B)
    r = 1:m
    for j = 1:n
        r.start = (j-1)*m+1
        mult(X, r, M, B, r)
    end
    return X
end
mult(X::StridedMatrix, M1::Tridiagonal, M2::Tridiagonal) = mult(X, M1, full(M2))
function *(M::Tridiagonal, B::Union(StridedVector,StridedMatrix))
    X = similar(B)
    mult(X, M, B)
end
*(A::Tridiagonal, B::Tridiagonal) = A*full(B)

#### Factorizations for Tridiagonal ####
type LDLTTridiagonal{T} <: Factorization{T}
    D::Vector{T}
    E::Vector{T}
end

ldltd!{T<:LapackType}(A::SymTridiagonal{T}) = LDLTTridiagonal{T}(LAPACK.pttrf!(A.dv,A.ev)...)
ldltd{T<:LapackType}(A::SymTridiagonal{T}) = ldltd!(copy(A))

(\){T<:LapackType}(C::LDLTTridiagonal{T}, B::StridedVecOrMat{T}) =
    LAPACK.pttrs!(C.D, C.E, copy(B))

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

lud!{T}(A::Tridiagonal{T}) = LUTridiagonal{T}(LAPACK.gttrf!(A.dl,A.d,A.du)...)
lud{T}(A::Tridiagonal{T}) = 
    LUTridiagonal{T}(LAPACK.gttrf!(copy(A.dl),copy(A.d),copy(A.du))...)
lu(A::Tridiagonal) = factors(lud(A))

function det(lu::LUTridiagonal)
    n = length(lu.d)
    prod(lu.d) * (bool(sum(lu.ipiv .!= 1:n) % 2) ? -1 : 1)
end

det(A::Tridiagonal) = det(lud(A))

(\){T<:LapackType}(lu::LUTridiagonal{T}, B::StridedVecOrMat{T}) =
    LAPACK.gttrs!('N', lu.dl, lu.d, lu.du, lu.du2, lu.ipiv, copy(B))


#### Woodbury matrices ####
# This type provides support for the Woodbury matrix identity
type Woodbury{T} <: AbstractMatrix{T}
    A
    U::Matrix{T}
    C
    Cp
    V::Matrix{T}
    tmpN1::Vector{T}
    tmpN2::Vector{T}
    tmpk1::Vector{T}
    tmpk2::Vector{T}

    function Woodbury(A::AbstractMatrix{T}, U::Matrix{T}, C, V::Matrix{T})
        N = size(A, 1)
        k = size(U, 2)
        if size(A, 2) != N || size(U, 1) != N || size(V, 1) != k || size(V, 2) != N
            error("Sizes do not match")
        end
        if k > 1
            if size(C, 1) != k || size(C, 2) != k
                error("Size of C is incorrect")
            end
        end
        Cp = inv(inv(C) + V*(A\U))
        # temporary space for allocation-free solver
        tmpN1 = Array(T, N)
        tmpN2 = Array(T, N)
        tmpk1 = Array(T, k)
        tmpk2 = Array(T, k)
        # don't copy A, it could be huge
        new(A, copy(U), copy(C), Cp, copy(V), tmpN1, tmpN2, tmpk1, tmpk2)
    end
end
Woodbury{T}(A::AbstractMatrix{T}, U::Matrix{T}, C, V::Matrix{T}) = Woodbury{T}(A, U, C, V)
Woodbury{T}(A::AbstractMatrix{T}, U::Vector{T}, C, V::Matrix{T}) = Woodbury{T}(A, reshape(U, length(U), 1), C, V)

size(W::Woodbury) = size(W.A)
function show(io, W::Woodbury)
    println(io, summary(W), ":")
    print(io, "A: ", W.A)
    print(io, "\nU:\n")
    print_matrix(io, W.U)
    if isa(W.C, Matrix)
        print(io, "\nC:\n")
        print_matrix(io, W.C)
    else
        print(io, "\nC: ", W.C)
    end
    print(io, "\nV:\n")
    print_matrix(io, W.V)
end
full{T}(W::Woodbury{T}) = convert(Matrix{T}, W)
convert{T}(::Type{Matrix{T}}, W::Woodbury{T}) = full(W.A) + W.U*W.C*W.V
function similar(W::Woodbury, T, dims::Dims)
    if length(dims) != 2 || dims[1] != dims[2]
        error("Woodbury matrices must be square")
    end
    n = size(W, 1)
    k = size(W.U, 2)
    return Woodbury{T}(similar(W.A), Array(T, n, k), Array(T, k, k), Array(T, k, n))
end
copy(W::Woodbury) = Woodbury(W.A, W.U, W.C, W.V)

## Woodbury matrix routines ##

function *(W::Woodbury, B::StridedVecOrMat)
    return W.A*B + W.U*(W.C*(W.V*B))
end
function \(W::Woodbury, R::StridedVecOrMat)
    AinvR = W.A\R
    return AinvR - W.A\(W.U*(W.Cp*(W.V*AinvR)))
end
function det(W::Woodbury)
    det(W.A)*det(W.C)/det(W.Cp)
end

# Allocation-free solver for arbitrary strides (requires that W.A has a
# non-aliasing "solve" routine, e.g., is Tridiagonal)
function solve(x::AbstractArray, xrng::Ranges{Int}, W::Woodbury, rhs::AbstractArray, rhsrng::Ranges{Int})
    solve(W.tmpN1, 1:length(W.tmpN1), W.A, rhs, rhsrng)
    A_mul_B(W.tmpk1, W.V, W.tmpN1)
    A_mul_B(W.tmpk2, W.Cp, W.tmpk1)
    A_mul_B(W.tmpN2, W.U, W.tmpk2)
    solve(W.tmpN2, W.A, W.tmpN2)
    indx = first(xrng)
    xinc = step(xrng)
    for i = 1:length(W.tmpN2)
        x[indx] = W.tmpN1[i] - W.tmpN2[i]
        indx += xinc
    end
end
solve(x::AbstractVector, W::Woodbury, rhs::AbstractVector) = solve(x, 1:length(x), W, rhs, 1:length(rhs))
function solve(W::Woodbury, rhs::AbstractVector)
    x = similar(rhs)
    solve(x, W, rhs)
end
function solve(X::StridedMatrix, W::Woodbury, B::StridedMatrix)
    if size(B, 1) != size(W, 1)
        error("dimension mismatch")
    end
    if size(X) != size(B)
        error("dimension mismatch in output")
    end
    m, n = size(B)
    r = 1:m
    for j = 1:n
        r.start = (j-1)*m+1
        solve(X, r, W, B, r)
    end
    return X
end
function solve(W::Woodbury, B::StridedMatrix)
    X = similar(B)
    solve(X, W, B)
end
