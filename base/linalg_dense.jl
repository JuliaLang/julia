# Linear algebra functions for dense matrices in column major format

scale!(X::Array{Float32}, s::Real) = BLAS.scal!(length(X), float32(s), X, 1)
scale!(X::Array{Float64}, s::Real) = BLAS.scal!(length(X), float64(s), X, 1)
scale!(X::Array{Complex64}, s::Real) = (ccall(("sscal_",Base.libblas_name), Void, (Ptr{BlasInt}, Ptr{Float32}, Ptr{Complex64}, Ptr{BlasInt}), &(2*length(X)), &s, X, &1); X)
scale!(X::Array{Complex128}, s::Real) = (ccall(("dscal_",Base.libblas_name), Void, (Ptr{BlasInt}, Ptr{Float64}, Ptr{Complex128}, Ptr{BlasInt}), &(2*length(X)), &s, X, &1); X)

#Test whether a matrix is positive-definite

isposdef!{T<:BlasFloat}(A::Matrix{T}, UL::BlasChar) = LAPACK.potrf!(UL, A)[2] == 0
isposdef!(A::Matrix) = ishermitian(A) && isposdef!(A, 'U')

isposdef{T<:BlasFloat}(A::Matrix{T}, UL::BlasChar) = isposdef!(copy(A), UL)
isposdef{T<:BlasFloat}(A::Matrix{T}) = isposdef!(copy(A))
isposdef{T<:Number}(A::Matrix{T}, UL::BlasChar) = isposdef!(float64(A), UL)
isposdef{T<:Number}(A::Matrix{T}) = isposdef!(float64(A))

isposdef(x::Number) = imag(x)==0 && real(x) > 0

norm{T<:BlasFloat}(x::Vector{T}) = BLAS.nrm2(length(x), x, 1)

function norm{T<:BlasFloat, TI<:Integer}(x::Vector{T}, rx::Union(Range1{TI},Range{TI}))
    if min(rx) < 1 || max(rx) > length(x)
        throw(BoundsError())
    end
    BLAS.nrm2(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx))
end

function norm{T<:BlasFloat}(x::Vector{T}, p::Number)
    n = length(x)
    if n == 0
        a = zero(T)
    elseif p == 2
        BLAS.nrm2(n, x, 1)
    elseif p == 1
        BLAS.asum(n, x, 1)
    elseif p == Inf
        max(abs(x))  
    elseif p == -Inf
        min(abs(x))
    elseif p == 0
        convert(T, nnz(x))
    else
        absx = abs(x)
        dx = max(absx)
        if dx != zero(T)
            scale!(absx, 1/dx)
            a = dx * (sum(absx.^p).^(1/p))
        else
            zero(T)
        end
    end
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

function diag{T}(A::Matrix{T}, k::Integer)
    m, n = size(A)
    if k >= 0 && k < n
        nV = min(m, n-k)
    elseif k < 0 && -k < m
        nV = min(m+k, n)
    else
        throw(BoundsError())
    end

    V = zeros(T, nV)

    if k > 0
        for i=1:nV
            V[i] = A[i, i+k]
        end
    else
        for i=1:nV
            V[i] = A[i-k, i]
        end
    end

    return V
end

diag(A) = diag(A, 0)

function diagm{T}(v::VecOrMat{T}, k::Integer)
    if isa(v, Matrix)
        if (size(v,1) != 1 && size(v,2) != 1)
            error("Input should be nx1 or 1xn")
        end
    end

    n = length(v)
    if k >= 0 
        a = zeros(T, n+k, n+k)
        for i=1:n
            a[i,i+k] = v[i]
        end
    else
        a = zeros(T, n-k, n-k)
        for i=1:n
            a[i-k,i] = v[i]
        end
    end

    return a
end  

diagm(v) = diagm(v, 0)

diagm(x::Number) = (X = Array(typeof(x),1,1); X[1,1] = x; X)

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

kron(a::Number, b::Number) = a * b
kron(a::Vector, b::Number) = a * b
kron(a::Number, b::Vector) = a * b
kron(a::Matrix, b::Number) = a * b
kron(a::Number, b::Matrix) = a * b

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
    if T <: Rational
        U = copy(A)
        e = 0
    else
        U = copy!(similar(A, T <: Complex ? Complex128 : Float64), A)
        e = eps(norm(U,Inf))
    end
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

rref(x::Number) = one(x)

## Destructive matrix exponential using algorithm from Higham, 2008,
## "Functions of Matrices: Theory and Computation", SIAM
function expm!{T<:BlasFloat}(A::StridedMatrix{T})
    m, n = size(A)
    if m != n error("expm!: Matrix A must be square") end
    if m < 2 return exp(A) end
    ilo, ihi, scale = LAPACK.gebal!('B', A)    # modifies A
    nA   = norm(A, 1)
    I    = eye(T,n)
    ## For sufficiently small nA, use lower order Padé-Approximations
    if (nA <= 2.1)
        if nA > 0.95
            C = T[17643225600.,8821612800.,2075673600.,302702400.,
                     30270240.,   2162160.,    110880.,     3960.,
                           90.,         1.]
        elseif nA > 0.25
            C = T[17297280.,8648640.,1995840.,277200.,
                     25200.,   1512.,     56.,     1.]
        elseif nA > 0.015
            C = T[30240.,15120.,3360.,
                    420.,   30.,   1.]
        else
            C = T[120.,60.,12.,1.]
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
        U = A * U
        X = V + U
        LAPACK.gesv!(V-U, X)
    else
        s  = log2(nA/5.4)               # power of 2 later reversed by squaring
        if s > 0
            si = iceil(s)
            A /= oftype(T,2^si)
        end
        CC = T[64764752532480000.,32382376266240000.,7771770303897600.,
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

        X = V + U
        LAPACK.gesv!(V-U, X)
    
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
function rcswap!{T<:Number}(j::Integer, jp::Integer, X::StridedMatrix{T})
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
expm(x::Number) = exp(x)

## Matrix factorizations and decompositions

abstract Factorization{T}
## Create an extractor that extracts the modified original matrix, e.g.
## LD for BunchKaufman, LR for CholeskyDense, LU for LUDense and
## define size methods for Factorization types using it.

type BunchKaufman{T<:BlasFloat} <: Factorization{T}
    LD::Matrix{T}
    ipiv::Vector{BlasInt}
    UL::BlasChar
    function BunchKaufman(A::Matrix{T}, UL::BlasChar)
        LD, ipiv = LAPACK.sytrf!(UL , copy(A))
        new(LD, ipiv, UL)
    end
end

BunchKaufman{T<:BlasFloat}(A::StridedMatrix{T}, UL::BlasChar) = BunchKaufman{T}(A, UL)
BunchKaufman{T<:Real}(A::StridedMatrix{T}, UL::BlasChar) = BunchKaufman(float64(A), UL)
BunchKaufman{T<:Number}(A::StridedMatrix{T}) = BunchKaufman(A, 'U')

size(B::BunchKaufman) = size(B.LD)
size(B::BunchKaufman,d::Integer) = size(B.LD,d)
## need to work out how to extract the factors.
#factors(B::BunchKaufman) = LAPACK.syconv!(B.UL, copy(B.LD), B.ipiv)

function inv(B::BunchKaufman)
    symmetrize!(LAPACK.sytri!(B.UL, copy(B.LD), B.ipiv), B.UL)
end

\{T<:BlasFloat}(B::BunchKaufman{T}, R::StridedVecOrMat{T}) =
    LAPACK.sytrs!(B.UL, B.LD, B.ipiv, copy(R))

type CholeskyDense{T<:BlasFloat} <: Factorization{T}
    LR::Matrix{T}
    UL::BlasChar
    function CholeskyDense(A::Matrix{T}, UL::BlasChar)
        A, info = LAPACK.potrf!(UL, A)
        if info != 0; throw(LAPACK.PosDefException(info)); end
        if UL == 'U'
            new(triu!(A), UL)
        elseif UL == 'L'
            new(tril!(A), UL)
        else
            error("Second argument UL should be 'U' or 'L'")
        end
    end
end

size(C::CholeskyDense) = size(C.LR)
size(C::CholeskyDense,d::Integer) = size(C.LR,d)

factors(C::CholeskyDense) = C.LR

\{T<:BlasFloat}(C::CholeskyDense{T}, B::StridedVecOrMat{T}) =
    LAPACK.potrs!(C.UL, C.LR, copy(B))

function det{T}(C::CholeskyDense{T})
    ff = C.LR
    dd = one(T)
    for i in 1:size(ff,1) dd *= abs2(ff[i,i]) end
    dd
end
    
function inv(C::CholeskyDense)
    Ci, info = LAPACK.potri!(C.UL, copy(C.LR))
    if info != 0; throw(LAPACK.SingularException(info)); end 
    symmetrize!(Ci, C.UL)
end

## Should these functions check that the matrix is Hermitian?
cholfact!{T<:BlasFloat}(A::Matrix{T}, UL::BlasChar) = CholeskyDense{T}(A, UL)
cholfact!{T<:BlasFloat}(A::Matrix{T}) = cholfact!(A, 'U')
cholfact{T<:BlasFloat}(A::Matrix{T}, UL::BlasChar) = cholfact!(copy(A), UL)
cholfact{T<:Number}(A::Matrix{T}, UL::BlasChar) = cholfact(float64(A), UL)
cholfact{T<:Number}(A::Matrix{T}) = cholfact(A, 'U')

## Matlab (and R) compatible
chol(A::Matrix, UL::BlasChar) = factors(cholfact(A, UL))
chol(A::Matrix) = chol(A, 'U')
chol(x::Number) = imag(x) == 0 && real(x) > 0 ? sqrt(x) : error("Argument not positive-definite")

type CholeskyPivotedDense{T<:BlasFloat} <: Factorization{T}
    LR::Matrix{T}
    UL::BlasChar
    piv::Vector{BlasInt}
    rank::BlasInt
    tol::Real
    function CholeskyPivotedDense(A::Matrix{T}, UL::BlasChar, tol::Real)
        A, piv, rank, info = LAPACK.pstrf!(UL, A, tol)
        if info != 0; throw(LAPACK.RankDeficientException(info)); end
        if UL == 'U'
            new(triu!(A), UL, piv, rank, tol)
        elseif UL == 'L'
            new(tril!(A), UL, piv, rank, tol)
        else
            error("Second argument UL should be 'U' or 'L'")
        end
    end
end

size(C::CholeskyPivotedDense) = size(C.LR)
size(C::CholeskyPivotedDense,d::Integer) = size(C.LR,d)

factors(C::CholeskyPivotedDense) = C.LR, C.piv

function \{T<:BlasFloat}(C::CholeskyPivotedDense{T}, B::StridedVector{T})
    if C.rank < size(C.LR, 1); throw(LAPACK.RankDeficientException(info)); end
    LAPACK.potrs!(C.UL, C.LR, copy(B)[C.piv])[invperm(C.piv)]
end

function \{T<:BlasFloat}(C::CholeskyPivotedDense{T}, B::StridedMatrix{T})
    if C.rank < size(C.LR, 1); throw(LAPACK.RankDeficientException(info)); end
    LAPACK.potrs!(C.UL, C.LR, copy(B)[C.piv,:])[invperm(C.piv),:]
end

rank(C::CholeskyPivotedDense) = C.rank

function det{T}(C::CholeskyPivotedDense{T})
    if C.rank < size(C.LR, 1) 
        return real(zero(T))
    else 
        return prod(abs2(diag(C.LR)))
    end
end
    
function inv(C::CholeskyPivotedDense)
    if C.rank < size(C.LR, 1) error("Matrix singular") end
    Ci, info = LAPACK.potri!(C.UL, copy(C.LR))
    if info != 0 error("Matrix is singular") end
    ipiv = invperm(C.piv)
    (symmetrize!(Ci, C.UL))[ipiv, ipiv]
end

## Should these functions check that the matrix is Hermitian?
cholpfact!{T<:BlasFloat}(A::Matrix{T}, UL::BlasChar, tol::Real) = CholeskyPivotedDense{T}(A, UL, tol)
cholpfact!{T<:BlasFloat}(A::Matrix{T}, UL::BlasChar) = cholpfact!(A, UL, -1.)
cholpfact!{T<:BlasFloat}(A::Matrix{T}, tol::Real) = cholpfact!(A, 'U', tol)
cholpfact!{T<:BlasFloat}(A::Matrix{T}) = cholpfact!(A, 'U', -1.)
cholpfact{T<:Number}(A::Matrix{T}, UL::BlasChar, tol::Real) = cholpfact(float64(A), UL, tol)
cholpfact{T<:Number}(A::Matrix{T}, UL::BlasChar) = cholpfact(float64(A), UL, -1.)
cholpfact{T<:Number}(A::Matrix{T}, tol::Real) = cholpfact(float64(A), true, tol)
cholpfact{T<:Number}(A::Matrix{T}) = cholpfact(float64(A), 'U', -1.)
cholpfact{T<:BlasFloat}(A::Matrix{T}, UL::BlasChar, tol::Real) = cholpfact!(copy(A), UL, tol)
cholpfact{T<:BlasFloat}(A::Matrix{T}, UL::BlasChar) = cholpfact!(copy(A), UL, -1.)
cholpfact{T<:BlasFloat}(A::Matrix{T}, tol::Real) = cholpfact!(copy(A), 'U', tol)
cholpfact{T<:BlasFloat}(A::Matrix{T}) = cholpfact!(copy(A), 'U', -1.)

type LUDense{T} <: Factorization{T}
    lu::Matrix{T}
    ipiv::Vector{BlasInt}
    info::BlasInt
    function LUDense(lu::Matrix{T}, ipiv::Vector{BlasInt}, info::BlasInt)
        m, n = size(lu)
        m == n ? new(lu, ipiv, info) : error("LUDense only defined for square matrices")
    end
end

size(A::LUDense) = size(A.lu)
size(A::LUDense,n) = size(A.lu,n)

function factors{T}(lu::LUDense{T}) 
    LU, ipiv = lu.lu, lu.ipiv
    m, n = size(LU)

    L = m >= n ? tril(LU, -1) + eye(T,m,n) : tril(LU, -1)[:, 1:m] + eye(T,m)
    U = m <= n ? triu(LU) : triu(LU)[1:n, :]
    P = [1:m]
    for i = 1:min(m,n)
        t = P[i]
        P[i] = P[ipiv[i]]
        P[ipiv[i]] = t
    end
    L, U, P
end

function lufact!{T<:BlasFloat}(A::Matrix{T})
    lu, ipiv, info = LAPACK.getrf!(A)
    LUDense{T}(lu, ipiv, info)
end

lufact{T<:BlasFloat}(A::Matrix{T}) = lufact!(copy(A))
lufact{T<:Number}(A::Matrix{T}) = lufact(float64(A))

## Matlab-compatible
lu{T<:Number}(A::Matrix{T}) = factors(lufact(A))
lu(x::Number) = (one(x), x, [1])

function det{T}(lu::LUDense{T})
    m, n = size(lu)
    if lu.info > 0; return zero(typeof(lu.lu[1])); end
    prod(diag(lu.lu)) * (bool(sum(lu.ipiv .!= 1:n) % 2) ? -one(T) : one(T))
end

function (\){T<:BlasFloat}(lu::LUDense{T}, B::StridedVecOrMat{T})
    if lu.info > 0; throw(LAPACK.SingularException(info)); end
    LAPACK.getrs!('N', lu.lu, lu.ipiv, copy(B))
end

function inv{T<:BlasFloat}(lu::LUDense{T})
    m, n = size(lu.lu)
    if m != n; error("inv only defined for square matrices"); end
    if lu.info > 0; return throw(LAPACK.SingularException(info)); end
    LAPACK.getri!(copy(lu.lu), lu.ipiv)
end

## QR decomposition without column pivots
type QRDense{T} <: Factorization{T}
    hh::Matrix{T}                       # Householder transformations and R
    tau::Vector{T}                      # Scalar factors of transformations
    function QRDense(hh::Matrix{T}, tau::Vector{T})
        length(tau) == min(size(hh)) ? new(hh, tau) : error("QR: mismatched dimensions")
    end
end
size(A::QRDense) = size(A.hh)
size(A::QRDense,n) = size(A.hh,n)

qrfact!{T<:BlasFloat}(A::StridedMatrix{T}) = QRDense{T}(LAPACK.geqrf!(A)...)
qrfact{T<:BlasFloat}(A::StridedMatrix{T}) = qrfact!(copy(A))
qrfact{T<:Real}(A::StridedMatrix{T}) = qrfact(float64(A))

function factors{T<:BlasFloat}(qrfact::QRDense{T})
    aa  = copy(qrfact.hh)
    R   = triu(aa[1:min(size(aa)),:])   # must be *before* call to orgqr!
    LAPACK.orgqr!(aa, qrfact.tau, size(aa,2)), R
end

qr{T<:Number}(x::StridedMatrix{T}) = factors(qrfact(x))
qr(x::Number) = (one(x), x)

## Multiplication by Q from the QR decomposition
qmulQR{T<:BlasFloat}(A::QRDense{T}, B::StridedVecOrMat{T}) =
    LAPACK.ormqr!('L', 'N', A.hh, size(A.hh,2), A.tau, copy(B))

## Multiplication by Q' from the QR decomposition
qTmulQR{T<:BlasFloat}(A::QRDense{T}, B::StridedVecOrMat{T}) =
    LAPACK.ormqr!('L', iscomplex(A.tau)?'C':'T', A.hh, size(A.hh,2), A.tau, copy(B))

## Least squares solution.  Should be more careful about cases with m < n
function (\){T<:BlasFloat}(A::QRDense{T}, B::StridedVecOrMat{T})
    n   = length(A.tau)
    ans, info = LAPACK.trtrs!('U','N','N',A.hh[1:n,:],(qTmulQR(A,B))[1:n,:])
    if info > 0; throw(LAPACK.SingularException(info)); end
    return ans
end

type QRPivotedDense{T} <: Factorization{T}
    hh::Matrix{T}
    tau::Vector{T}
    jpvt::Vector{BlasInt}
    function QRPivotedDense(hh::Matrix{T}, tau::Vector{T}, jpvt::Vector{BlasInt})
        m, n = size(hh)
        if length(tau) != min(m,n) || length(jpvt) != n
            error("QRPivotedDense: mismatched dimensions")
        end
        new(hh,tau,jpvt)
    end
end
size(x::QRPivotedDense)   = size(x.hh)
size(x::QRPivotedDense,d) = size(x.hh,d)
## Multiplication by Q from the QR decomposition
qmulQR{T<:BlasFloat}(A::QRPivotedDense{T}, B::StridedVecOrMat{T}) =
    LAPACK.ormqr!('L', 'N', A.hh, size(A,2), A.tau, copy(B))
## Multiplication by Q' from the QR decomposition
qTmulQR{T<:BlasFloat}(A::QRPivotedDense{T}, B::StridedVecOrMat{T}) =
    LAPACK.ormqr!('L', iscomplex(A.tau)?'C':'T', A.hh, size(A,2), A.tau, copy(B))

qrpfact!{T<:BlasFloat}(A::StridedMatrix{T}) = QRPivotedDense{T}(LAPACK.geqp3!(A)...)
qrpfact{T<:BlasFloat}(A::StridedMatrix{T}) = qrpfact!(copy(A))
qrpfact{T<:Real}(x::StridedMatrix{T}) = qrpfact(float64(x))

function factors{T<:BlasFloat}(x::QRPivotedDense{T})
    aa = copy(x.hh)
    R  = triu(aa[1:min(size(aa)),:])
    LAPACK.orgqr!(aa, x.tau, size(aa,2)), R, x.jpvt
end

qrp{T<:BlasFloat}(x::StridedMatrix{T}) = factors(qrpfact(x))
qrp{T<:Real}(x::StridedMatrix{T}) = qrp(float64(x))

function (\){T<:BlasFloat}(A::QRPivotedDense{T}, B::StridedVecOrMat{T})
    n = length(A.tau)
    x, info = LAPACK.trtrs!('U','N','N',A.hh[1:n,:],(qTmulQR(A,B))[1:n,:])
    if info > 0; throw(LAPACK.SingularException(info)); end
    isa(B, Vector) ? x[invperm(A.jpvt)] : x[:,invperm(A.jpvt)]
end

##TODO:  Add methods for rank(A::QRP{T}) and adjust the (\) method accordingly
##       Add rcond methods for Cholesky, LU, QR and QRP types
## Lower priority: Add LQ, QL and RQ factorizations

# FIXME! Should add balancing option through xgebal
type Hessenberg{T} <: Factorization{T}
    H::Matrix{T}
    tau::Vector{T}
    ilo::Int
    ihi::Int
end
function hessfact(A::StridedMatrix)
    tmp = LAPACK.gehrd!(copy(A))
    return Hessenberg(tmp[1], tmp[2], 1, size(A, 1))
end
function factors(H::Hessenberg) 
    A = copy(H.H)
    n = size(A, 1)
    for j = 1:n-2
        for i = j+2:n
            A[i,j] = zero(A[1])
        end
    end
    return (A, LAPACK.orghr!(BLAS.blas_int(H.ilo), BLAS.blas_int(H.ihi), H.H, H.tau))
end
hess(A::StridedMatrix) = factors(hessfact(A))[1]

### Linear algebra for general matrices

function det(A::Matrix)
    m, n = size(A)
    if m != n; error("det only defined for square matrices"); end
    if istriu(A) | istril(A); return prod(diag(A)); end
    return det(lufact(A))
end
det(x::Number) = x

logdet(A::Matrix) = 2.0 * sum(log(diag(chol(A))))

function eig{T<:BlasFloat}(A::StridedMatrix{T}, vecs::Bool)
    n = size(A, 2)
    if n == 0; return vecs ? (zeros(T, 0), zeros(T, 0, 0)) : zeros(T, 0, 0); end

    if ishermitian(A)
        if vecs
            Z = similar(A)
            W = LAPACK.syevr!(copy(A), Z)
            return W, Z
        else
            W = LAPACK.syevr!(copy(A))
            return W
        end
    end

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
eig(x::Number, vecs::Bool) = vecs ? (x, one(x)) : x
eig(x) = eig(x, true)
eigvals(x) = eig(x, false)

# SVD
type SVDDense{T,Tr} <: Factorization{T}
    U::Matrix{T}
    S::Vector{Tr}
    V::Matrix{T}
end

factors(F::SVDDense) = (F.U, F.S, F.V)

function svdfact!{T<:BlasFloat}(A::StridedMatrix{T}, thin::Bool)
    m,n = size(A)
    if m == 0 || n == 0
        u,s,v = (eye(m, thin ? n : m), zeros(0), eye(n,n))
    else
        u,s,v = LAPACK.gesdd!(thin ? 'S' : 'A', A)
    end
    return SVDDense(u,s,v)
end

svdfact!(A::StridedMatrix) = svdfact(A, false)

svdfact(A::StridedMatrix, thin::Bool) = svdfact!(copy(A), thin)
svdfact(A::StridedMatrix) = svdfact(A, false)

function svdvals!(A::StridedMatrix)
    m,n = size(A)
    if m == 0 || n == 0
        return (zeros(T, 0, 0), zeros(T, 0), zeros(T, 0, 0))
    end
    U, S, V = LAPACK.gesdd!('N', A)
    return S
end

svdvals(A) = svdvals!(copy(A))

svdt(A::StridedMatrix, thin::Bool) = factors(svdfact(A, thin))
svdt(A::StridedMatrix) = svdt(A, false)
svdt(x::Number, thin::Bool) = (x==0?one(x):x/abs(x),abs(x),one(x))

function svd(A::StridedMatrix, thin::Bool)
    u,s,v = factors(svdfact(A, thin))
    return (u,s,v')
end

svd(A::StridedMatrix) = svd(A, false)
svd(x::Number, thin::Bool) = (x==0?one(x):x/abs(x),abs(x),one(x))


# Generalized svd
type GSVDDense{T} <: Factorization{T}
    U::Matrix{T}
    V::Matrix{T}
    Q::Matrix{T}
    a::Vector #{eltype(real(one(T)))}
    b::Vector #{eltype(real(one(T)))}
    k::Int
    l::Int
    R::Matrix{T}
end

function svdfact(A::StridedMatrix, B::StridedMatrix)
    U, V, Q, a, b, k, l, R = LAPACK.ggsvd!('U', 'V', 'Q', copy(A), copy(B))
    return GSVDDense(U, V, Q, a, b, int(k), int(l), R)
end

svd(A::StridedMatrix, B::StridedMatrix) = factors(svdfact(A, B))

function factors{T}(obj::GSVDDense{T})
    m = size(obj.U, 1)
    p = size(obj.V, 1)
    n = size(obj.Q, 1)
    if m - obj.k - obj.l >= 0
        D1 = [eye(T, obj.k) zeros(T, obj.k, obj.l); zeros(T, obj.l, obj.k) diagm(obj.a[obj.k + 1:obj.k + obj.l]); zeros(T, m - obj.k - obj.l, obj.k + obj.l)]
        D2 = [zeros(T, obj.l, obj.k) diagm(obj.b[obj.k + 1:obj.k + obj.l]); zeros(T, p - obj.l, obj.k + obj.l)]
        R0 = [zeros(T, obj.k + obj.l, n - obj.k - obj.l) obj.R]
    else
        D1 = [eye(T, m, obj.k) [zeros(T, obj.k, m - obj.k); diagm(obj.a[obj.k + 1:m])] zeros(T, m, obj.k + obj.l - m)]
        D2 = [zeros(T, p, obj.k) [diagm(obj.b[obj.k + 1:m]); zeros(T, obj.k + p - m, m - obj.k)] [zeros(T, m - obj.k, obj.k + obj.l - m); eye(T, obj.k + p - m, obj.k + obj.l - m)]]
        R0 = [zeros(T, obj.k + obj.l, n - obj.k - obj.l) obj.R]
    end
    return obj.U, obj.V, obj.Q, D1, D2, R0
end

function svdvals(A::StridedMatrix, B::StridedMatrix)
    _, _, _, a, b, k, l, _ = LAPACK.ggsvd!('N', 'N', 'N', copy(A), copy(B))
    return a[1:k + l] ./ b[1:k + l]
end

schur{T<:BlasFloat}(A::StridedMatrix{T}) = LAPACK.gees!('V', copy(A))

function sqrtm(A::StridedMatrix, cond::Bool)
    m, n = size(A)
    if m != n error("DimentionMismatch") end
    if ishermitian(A)
        z = similar(A)
        v = LAPACK.syevr!(copy(A),z)
        vsqrt = sqrt(complex(v))
        if all(imag(vsqrt) .== 0)
            retmat = symmetrize!(diagmm(z, real(vsqrt)) * z')
        else
            zc = complex(z)
            retmat = symmetrize!(diagmm(zc, vsqrt) * zc')
        end
        if cond
            return retmat, norm(vsqrt, Inf)^2/norm(v, Inf)
        else
            return retmat
        end
    else
        T,Q,_ = schur(complex(A))
        R = zeros(eltype(T), n, n)
        for j = 1:n
            R[j,j] = sqrt(T[j,j])
            for i = j - 1:-1:1
                r = T[i,j]
                for k = i + 1:j - 1
                    r -= R[i,k]*R[k,j]
                end
                if r != 0
                    R[i,j] = r / (R[i,i] + R[j,j])
                end
            end
        end
        retmat = Q*R*Q'
        if cond
            alpha = norm(R)^2/norm(T)
            return (all(imag(retmat) .== 0) ? real(retmat) : retmat), alpha
        else
            return (all(imag(retmat) .== 0) ? real(retmat) : retmat)
        end
    end
end
sqrtm{T<:Integer}(A::StridedMatrix{T}, cond::Bool) = sqrtm(float(A), cond)
sqrtm{T<:Integer}(A::StridedMatrix{ComplexPair{T}}, cond::Bool) = sqrtm(complex128(A), cond)
sqrtm(A::StridedMatrix) = sqrtm(A, false)
sqrtm(a::Number) = isreal(a) ? (b = sqrt(complex(a)); imag(b) == 0 ? real(b) : b)  : sqrt(a)

function (\){T<:BlasFloat}(A::StridedMatrix{T}, B::StridedVecOrMat{T})
    Acopy = copy(A)
    m, n  = size(Acopy)
    X     = copy(B)

    if m == n # Square
        if istriu(A) 
            ans, info = LAPACK.trtrs!('U', 'N', 'N', Acopy, X)
            if info > 0; throw(LAPACK.SingularException(info)); end
            return ans
        end
        if istril(A) 
            ans, info = LAPACK.trtrs!('L', 'N', 'N', Acopy, X) 
            if info > 0; throw(LAPACK.SingularException(info)); end
            return ans
        end
        if ishermitian(A) 
            ans, _, _, info = LAPACK.sysv!('U', Acopy, X)
            if info > 0; throw(LAPACK.SingularException(info)); end
            return ans
        end
        ans, _, _, info = LAPACK.gesv!(Acopy, X)
        if info > 0; throw(LAPACK.SingularException(info)); end
        return ans
    end
    LAPACK.gelsd!(Acopy, X)[1]
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
    u,s,vt      = svdt(A, true)
    sinv        = zeros(T, length(s))
    index       = s .> eps(real(one(T)))*max(size(A))*max(s)
    sinv[index] = 1 ./ s[index]
    vt'diagmm(sinv, u')
end
pinv{T<:Integer}(A::StridedMatrix{T}) = pinv(float(A))
pinv(a::StridedVector) = pinv(reshape(a, length(a), 1))
pinv(x::Number) = one(x)/x

## Basis for null space
function null{T<:BlasFloat}(A::StridedMatrix{T})
    m,n = size(A)
    _,s,vt = svdt(A)
    if m == 0; return eye(T, n); end
    indstart = sum(s .> max(m,n)*max(s)*eps(eltype(s))) + 1
    vt[indstart:,:]'
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
        cnd = 1 / LAPACK.gecon!(p == 1 ? '1' : 'I', lufact(A).lu, norm(A, p))
    else
        error("Norm type must be 1, 2 or Inf")
    end
    return cnd
end
cond(A::StridedMatrix) = cond(A, 2)

#### Specialized matrix types ####

## Symmetric tridiagonal matrices
type SymTridiagonal{T<:BlasFloat} <: AbstractMatrix{T}
    dv::Vector{T}                        # diagonal
    ev::Vector{T}                        # sub/super diagonal
    function SymTridiagonal(dv::Vector{T}, ev::Vector{T})
        if length(ev) != length(dv) - 1 error("dimension mismatch") end
        new(dv,ev)
    end
end

SymTridiagonal{T<:BlasFloat}(dv::Vector{T}, ev::Vector{T}) = SymTridiagonal{T}(copy(dv), copy(ev))

function SymTridiagonal{T<:Real}(dv::Vector{T}, ev::Vector{T})
    SymTridiagonal{Float64}(float64(dv),float64(ev))
end

function SymTridiagonal{Td<:Number,Te<:Number}(dv::Vector{Td}, ev::Vector{Te})
    T = promote(Td,Te)
    SymTridiagonal(convert(Vector{T}, dv), convert(Vector{T}, ev))
end

SymTridiagonal(A::AbstractMatrix) = SymTridiagonal(diag(A), diag(A,1))

copy(S::SymTridiagonal) = SymTridiagonal(S.dv,S.ev)

function full(S::SymTridiagonal)
    M = diagm(S.dv)
    for i in 1:length(S.ev)
        j = i + 1
        M[i,j] = M[j,i] = S.ev[i]
    end
    M
end

function show(io::IO, S::SymTridiagonal)
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
eigvals(m::SymTridiagonal) = eig(m::SymTridiagonal, false)[1]

## Tridiagonal matrices ##
type Tridiagonal{T} <: AbstractMatrix{T}
    dl::Vector{T}    # sub-diagonal
    d::Vector{T}     # diagonal
    du::Vector{T}    # sup-diagonal
    dutmp::Vector{T} # scratch space for vector RHS solver, sup-diagonal
    rhstmp::Vector{T}# scratch space, rhs

    function Tridiagonal(N::Integer)
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
function show(io::IO, M::Tridiagonal)
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
function \{T<:BlasFloat}(M::Tridiagonal{T}, rhs::StridedVecOrMat{T})
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
    for j = 1:n
        r = Range1((j-1)*m+1,m)
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
    for j = 1:n
        r = Range1((j-1)*m+1,m)
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
type LDLTTridiagonal{T<:BlasFloat,S<:BlasFloat} <: Factorization{T}
    D::Vector{S}
    E::Vector{T}
    function LDLTTridiagonal(D::Vector{S}, E::Vector{T})
        if typeof(real(E[1])) != eltype(D) error("Wrong eltype") end
        new(D, E)
    end
end

LDLTTridiagonal{S<:BlasFloat,T<:BlasFloat}(D::Vector{S}, E::Vector{T}) = LDLTTridiagonal{T,S}(D, E)

ldltd!{T<:BlasFloat}(A::SymTridiagonal{T}) = LDLTTridiagonal(LAPACK.pttrf!(real(A.dv),A.ev)...)
ldltd{T<:BlasFloat}(A::SymTridiagonal{T}) = ldltd!(copy(A))

function (\){T<:BlasFloat}(C::LDLTTridiagonal{T}, B::StridedVecOrMat{T})
    if iscomplex(B) return LAPACK.pttrs!('L', C.D, C.E, copy(B)) end
    LAPACK.pttrs!(C.D, C.E, copy(B))
end

type LUTridiagonal{T} <: Factorization{T}
    dl::Vector{T}
    d::Vector{T}
    du::Vector{T}
    du2::Vector{T}
    ipiv::Vector{BlasInt}
    function LUTridiagonal(dl::Vector{T}, d::Vector{T}, du::Vector{T},
                           du2::Vector{T}, ipiv::Vector{BlasInt})
        n = length(d)
        if length(dl) != n - 1 || length(du) != n - 1 || length(ipiv) != n || length(du2) != n-2
            error("LUTridiagonal: dimension mismatch")
        end
        new(dl, d, du, du2, ipiv)
    end
end

#show(io, lu::LUTridiagonal) = print(io, "LU decomposition of ", summary(lu.lu))

lufact!{T}(A::Tridiagonal{T}) = LUTridiagonal{T}(LAPACK.gttrf!(A.dl,A.d,A.du)...)
lufact{T}(A::Tridiagonal{T}) = LUTridiagonal{T}(LAPACK.gttrf!(copy(A.dl),copy(A.d),copy(A.du))...)
lu(A::Tridiagonal) = factors(lufact(A))

function det{T}(lu::LUTridiagonal{T})
    n = length(lu.d)
    prod(lu.d) * (bool(sum(lu.ipiv .!= 1:n) % 2) ? -one(T) : one(T))
end

det(A::Tridiagonal) = det(lufact(A))

(\){T<:BlasFloat}(lu::LUTridiagonal{T}, B::StridedVecOrMat{T}) =
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
function show(io::IO, W::Woodbury)
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
    for j = 1:n
        r = Range1((j-1)*m+1,m)
        solve(X, r, W, B, r)
    end
    return X
end
function solve(W::Woodbury, B::StridedMatrix)
    X = similar(B)
    solve(X, W, B)
end
