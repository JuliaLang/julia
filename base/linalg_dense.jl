# Should probably go someweher else
symbol(x::Char) = symbol(string(x))

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

randsym(n) = symmetrize!(randn(n,n))

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
    U = copy!(similar(A, T <: Complex ? Complex128 : Float64), A)
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
## LD for BunchKaufman, UL for CholeskyDense, LU for LUDense and
## define size methods for Factorization types using it.

type BunchKaufman{T<:BlasFloat} <: Factorization{T}
    LD::Matrix{T}
    ipiv::Vector{BlasInt}
    uplo::BlasChar
    function BunchKaufman(A::Matrix{T}, uplo::BlasChar)
        LD, ipiv = LAPACK.sytrf!(uplo , copy(A))
        new(LD, ipiv, uplo)
    end
end

BunchKaufman{T<:BlasFloat}(A::StridedMatrix{T}, uplo::BlasChar) = BunchKaufman{T}(A, uplo)
BunchKaufman{T<:Real}(A::StridedMatrix{T}, uplo::BlasChar) = BunchKaufman(float64(A), uplo)
BunchKaufman{T<:Number}(A::StridedMatrix{T}) = BunchKaufman(A, 'U')

size(B::BunchKaufman) = size(B.LD)
size(B::BunchKaufman,d::Integer) = size(B.LD,d)

function inv(B::BunchKaufman)
    symmetrize!(LAPACK.sytri!(B.uplo, copy(B.LD), B.ipiv), B.uplo)
end

\{T<:BlasFloat}(B::BunchKaufman{T}, R::StridedVecOrMat{T}) =
    LAPACK.sytrs!(B.uplo, B.LD, B.ipiv, copy(R))

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
CholeskyDense(A::Matrix, uplo::Symbol) = CholeskyDense(A, string(uplo)[1])
CholeskyDense(A::Matrix) = CholeskyDense(A, :U)
CholeskyDense{T<:Integer}(A::Matrix{T}, args...) = CholeskyDense(float64(A), args...)

size(C::CholeskyDense) = size(C.UL)
size(C::CholeskyDense,d::Integer) = size(C.UL,d)

function ref(C::CholeskyDense, d::Symbol)
    if d == :U || d == :L
        return symbol(C.uplo) == d ? C.UL : C.UL'
    end
    error("No such property")
end

## Matlab (and R) compatible
chol(A::Matrix, uplo::Symbol) = CholeskyDense(copy(A), uplo)[uplo]
chol(A::Matrix) = chol(A, :U)
chol(x::Number) = imag(x) == 0 && real(x) > 0 ? sqrt(x) : error("Argument not positive-definite")

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
    uplo::BlasChar
    piv::Vector{BlasInt}
    rank::BlasInt
    tol::Real
    info::BlasInt
end
function CholeskyPivotedDense{T<:BlasFloat}(A::Matrix{T}, uplo::BlasChar, tol::Real)
    A, piv, rank, info = LAPACK.pstrf!(uplo, A, tol)
    CholeskyPivotedDense{T}(uplo == 'U' ? triu!(A) : tril!(A), uplo, piv, rank, tol, info)
end
CholeskyPivotedDense(A::Matrix, uplo::Symbol, tol::Real) = CholeskyPivotedDense(A, string(uplo)[1], tol)
CholeskyPivotedDense(A::Matrix, tol::Real) = CholeskyPivotedDense(A, 'U', tol)
CholeskyPivotedDense(A::Matrix) = CholeskyPivotedDense(A, 'U', -1.)
CholeskyPivotedDense{T<:Int}(A::Matrix{T}, args...) = CholeskyPivotedDense(float64(A), args...)

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
LUDense{T<:Real}(A::Matrix{T}) = LUDense(float(A))

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

## Matlab-compatible
function lu(A::Matrix)
    LU = LUDense(copy(A))
    return LU[:L], LU[:U], LU[:p]
end
lu(x::Number) = (one(x), x, [1])

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

## QR decomposition without column pivots
type QRDense{T} <: Factorization{T}
    hh::Matrix{T}                       # Householder transformations and R
    tau::Vector{T}                      # Scalar factors of transformations
end
QRDense(A::StridedMatrix) = QRDense(LAPACK.geqrf!(A)...)
QRDense{T<:Integer}(A::StridedMatrix{T}) = QRDense(float(A))

type QRDenseQ{T}  <: AbstractMatrix{T}
    hh::Matrix{T}                       # Householder transformations and R
    tau::Vector{T}                      # Scalar factors of transformations
end
QRDenseQ(A::QRDense) = QRDenseQ(A.hh, A.tau)

size(A::QRDense, args::Integer...) = size(A.hh, args...)
size(A::QRDenseQ, args::Integer...) = size(A.hh, args...)

function ref(A::QRDense, d::Symbol)
    if d == :R; return triu(A.hh[1:min(size(A)),:]); end;
    if d == :Q; return QRDenseQ(A); end
    error("No such property")
end
function full{T<:BlasFloat}(A::QRDenseQ{T}, thin::Bool)
    if !thin
        Q = Array(T, size(A, 1), size(A, 1))
        Q[:,1:size(A, 2)] = copy(A.hh)
        return LAPACK.orgqr!(Q, A.tau)
    else
        return LAPACK.orgqr!(copy(A.hh), A.tau)
    end
end
full(A::QRDenseQ) = full(A, true)

function qr(A::StridedMatrix)
    QR = QRDense(copy(A))
    return full(QR[:Q]), QR[:R]
end
qr(x::Number) = (one(x), x)

## Multiplication by Q from the QR decomposition
function *{T<:BlasFloat}(A::QRDenseQ{T}, B::StridedVecOrMat{T})
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
Ac_mul_B(A::QRDenseQ, B::StridedVecOrMat) = LAPACK.ormqr!('L', iscomplex(A.hh[1]) ? 'C' : 'T', A.hh, A.tau, copy(B))
*(A::StridedVecOrMat, B::QRDenseQ) = LAPACK.ormqr!('R', 'N', B.hh, B.tau, copy(A))
function A_mul_Bc{T<:BlasFloat}(A::StridedVecOrMat{T}, B::QRDenseQ{T})
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
## Least squares solution.  Should be more careful about cases with m < n
(\)(A::QRDense, B::StridedVector) = A[:R]\(A[:Q]'B)[1:size(A, 2)]
(\)(A::QRDense, B::StridedMatrix) = A[:R]\(A[:Q]'B)[1:size(A, 2),:]

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
QRDenseQ(A::QRPivotedDense) = QRDenseQ(A.hh, A.tau)

size(A::QRPivotedDense, args::Integer...) = size(A.hh, args...)

function ref{T<:BlasFloat}(A::QRPivotedDense{T}, d::Symbol)
    if d == :R; return triu(A.hh[1:min(size(A)),:]); end;
    if d == :Q; return QRDenseQ(A); end
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

(\)(A::QRPivotedDense, B::StridedVector) = (A[:R]\(A[:Q]'B)[1:size(A, 2)])[invperm(A.jpvt)]
(\)(A::QRPivotedDense, B::StridedMatrix) = A[:R]\(A[:Q]'B)[1:size(A, 2),:][invperm(A.jpvt),:]

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

hess(A::StridedMatrix) = Hessenberg(copy(A))[:H]

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

function svd(A::StridedMatrix, thin::Bool)
    SVD = SVDDense(copy(A), thin)
    return SVD[:U], SVD[:S], SVD[:V]
end
svd(A::StridedMatrix) = svd(A, false)
svd(x::Number, thin::Bool) = (x==0?one(x):x/abs(x),abs(x),one(x))

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

function svd(A::StridedMatrix, B::StridedMatrix) 
    G = GSVDDense(copy(A), copy(B))
    return G[:U], G[:V], G[:Q], G[:D1], G[:D2], G[:R0]
end

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
            R[i,j] = (T[i,j] - r) / (R[i,i] + R[j,j])
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

sqrtm{T<:Integer}(A::StridedMatrix{T}, cond::Bool) = sqrtm(float(A), cond)
sqrtm{T<:Integer}(A::StridedMatrix{ComplexPair{T}}, cond::Bool) = sqrtm(complex128(A), cond)
sqrtm(A::StridedMatrix) = sqrtm(A, false)
sqrtm(a::Number) = isreal(a) ? (b = sqrt(complex(a)); imag(b) == 0 ? real(b) : b)  : sqrt(a)

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

#### Specialized matrix types ####

## Hermitian tridiagonal matrices
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
size(m::SymTridiagonal, d::Integer) = d<1 ? error("dimension out of range") : (d<2 ? length(m.dv) : 1)

eig(m::SymTridiagonal) = LAPACK.stegr!('V', copy(m.dv), copy(m.ev))
eigvals(m::SymTridiagonal, il::Int, ih::Int) = LAPACK.stebz!('I', 'E', 0.0, 0.0, il, iu, -1.0, copy(m.dv), copy(m.ev))[1]
eigvals(m::SymTridiagonal, vl::Int, iv::Int) = LAPACK.stebz!('V', 'E', vl, vh, 0, 0, -1.0, copy(m.dv), copy(m.ev))[1]
eigvals(m::SymTridiagonal) = eigvals(m, 1, size(m, 1))

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

copy(A::Tridiagonal) = Tridiagonal(copy(A.dl), copy(A.d), copy(A.du))

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
LUTridiagonal{T}(A::Tridiagonal{T}) = LUTridiagonal{T}(LAPACK.gttrf!(A.dl,A.d,A.du)...)

#show(io, lu::LUTridiagonal) = print(io, "LU decomposition of ", summary(lu.lu))

function det{T}(lu::LUTridiagonal{T})
    n = length(lu.d)
    prod(lu.d) * (bool(sum(lu.ipiv .!= 1:n) % 2) ? -one(T) : one(T))
end

det(A::Tridiagonal) = det(LUTridiagonal(copy(A)))

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

### Special types used for dispatch
## Triangular
type Triangular{T<:BlasFloat} <: AbstractMatrix{T}
    UL::Matrix{T}
    uplo::Char
    unitdiag::Char
    function Triangular(A::Matrix{T}, uplo::Char, unitdiag::Char)
        if size(A, 1) != size(A, 2) throw(LAPACK.DimensionMismatch("Matrix must be square")) end
        return new(A, uplo, unitdiag)
    end
end
Triangular{T<:BlasFloat}(A::Matrix{T}, uplo::Char, unitdiag::Char) = Triangular{T}(A, uplo, unitdiag)
Triangular(A::Matrix, uplo::Char, unitdiag::Bool) = Triangular(A, uplo, unitdiag ? 'U' : 'N')
Triangular(A::Matrix, uplo::Char) = Triangular(A, uplo, all(diag(A) .== 1) ? true : false)
function Triangular(A::Matrix)
    if istriu(A) return Triangular(A, 'U') end
    if istril(A) return Triangular(A, 'L') end
    error("Matrix is not triangular")
end

istril(A::Triangular) = A.uplo == 'L'
istriu(A::Triangular) = A.uplo == 'U'

function \(A::Triangular, B::StridedVecOrMat)
    r, info = LAPACK.trtrs!(A.uplo, 'N', A.unitdiag, A.UL, copy(B))
    if info > 0 throw(LAPACK.SingularException(info)) end
    return r
end
function Ac_ldiv_B{T<:Union(Float64, Float32)}(A::Triangular{T}, B::StridedVecOrMat{T}) 
    r, info = LAPACK.trtrs!(A.uplo, 'T', A.unitdiag, A.UL, copy(B))
    if info > 0 throw(LAPACK.SingularException(info)) end
    return r
end
function Ac_ldiv_B{T<:Union(Complex128, Complex64)}(A::Triangular{T}, B::StridedVecOrMat{T})
    r, info = LAPACK.trtrs!(A.uplo, 'C', A.unitdiag, A.UL, copy(B))
    if info > 0 throw(LAPACK.SingularException(info)) end
    return r
end

det(A::Triangular) = prod(diag(A.UL))

inv(A::Triangular) = LAPACK.trtri!(A.uplo, A.unitdiag, copy(A.UL))[1]

## Hermitian
type Hermitian{T<:BlasFloat} <: AbstractMatrix{T}
    S::Matrix{T}
    uplo::Char
    function Hermitian(S::Matrix{T}, uplo::Char)
        if size(S, 1) != size(S, 2) throw(LAPACK.DimensionMismatch("Matrix must be square")); end
        return new(S, uplo)
    end
end
Hermitian{T<:BlasFloat}(S::Matrix{T}, uplo::Char) = Hermitian{T}(S, uplo)
Hermitian(A::StridedMatrix) = Hermitian(A, 'U')

size(A::Hermitian, args...) = size(A.S, args...)
ishermitian(A::Hermitian) = true
issym{T<:Union(Float64, Float32)}(A::Hermitian{T}) = true

function \(A::Hermitian, B::StridedVecOrMat)
    r, _, _, info = LAPACK.sysv!(A.uplo, copy(A.S), copy(B))
    if info > 0 throw(LAPACK.SingularException(info)) end
    return r
end

inv(A::Hermitian) = inv(BunchKaufman(copy(A.S), A.uplo))

eig(A::Hermitian) = LAPACK.syevr!('V', 'A', A.uplo, copy(A.S), 0.0, 0.0, 0, 0, -1.0)
eigvals(A::Hermitian, il::Int, ih::Int) = LAPACK.syevr!('N', 'I', A.uplo, copy(A.S), 0.0, 0.0, il, ih, -1.0)[1]
eigvals(A::Hermitian, vl::Real, vh::Real) = LAPACK.syevr!('N', 'V', A.uplo, copy(A.S), vl, vh, 0, 0, -1.0)[1]
eigvals(A::Hermitian) = eigvals(A, 1, size(A, 1))
eigmax(A::Hermitian) = eigvals(A, size(A, 1), size(A, 1))[1]

function sqrtm(A::Hermitian, cond::Bool)
    v, z = eig(A)
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
end
