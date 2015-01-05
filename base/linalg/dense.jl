# Linear algebra functions for dense matrices in column major format

## BLAS cutoff threshold constants

const SCAL_CUTOFF = 2048
const DOT_CUTOFF = 128
const ASUM_CUTOFF = 32
const NRM2_CUTOFF = 32

function scale!{T<:BlasFloat}(X::Array{T}, s::T)
    if length(X) < SCAL_CUTOFF
        generic_scale!(X, s)
    else
        BLAS.scal!(length(X), s, X, 1)
    end
    X
end

scale!{T<:BlasFloat}(X::Array{T}, s::Number) = scale!(X, convert(T, s))
function scale!{T<:BlasComplex}(X::Array{T}, s::Real)
    R = typeof(real(zero(T)))
    BLAS.scal!(2*length(X), convert(R,s), convert(Ptr{R},pointer(X)), 1)
    X
end

#Test whether a matrix is positive-definite
isposdef!{T<:BlasFloat}(A::StridedMatrix{T}, UL::Symbol) = LAPACK.potrf!(char_uplo(UL), A)[2] == 0
isposdef!(A::StridedMatrix) = ishermitian(A) && isposdef!(A, :U)

isposdef{T}(A::AbstractMatrix{T}, UL::Symbol) = (S = typeof(sqrt(one(T))); isposdef!(S == T ? copy(A) : convert(AbstractMatrix{S}, A), UL))
isposdef{T}(A::AbstractMatrix{T}) = (S = typeof(sqrt(one(T))); isposdef!(S == T ? copy(A) : convert(AbstractMatrix{S}, A)))
isposdef(x::Number) = imag(x)==0 && real(x) > 0

stride1(x::Array) = 1
stride1(x::StridedVector) = stride(x, 1)::Int

import Base: mapreduce_seq_impl, AbsFun, Abs2Fun, AddFun

mapreduce_seq_impl{T<:BlasReal}(::AbsFun, ::AddFun, a::Union(Array{T},StridedVector{T}), ifirst::Int, ilast::Int) =
    BLAS.asum(ilast-ifirst+1, pointer(a, ifirst), stride1(a))

function mapreduce_seq_impl{T<:BlasReal}(::Abs2Fun, ::AddFun, a::Union(Array{T},StridedVector{T}), ifirst::Int, ilast::Int)
    n = ilast-ifirst+1
    px = pointer(a, ifirst)
    incx = stride1(a)
    BLAS.dot(n, px, incx, px, incx)
end

function mapreduce_seq_impl{T<:BlasComplex}(::Abs2Fun, ::AddFun, a::Union(Array{T},StridedVector{T}), ifirst::Int, ilast::Int)
    n = ilast-ifirst+1
    px = pointer(a, ifirst)
    incx = stride1(a)
    real(BLAS.dotc(n, px, incx, px, incx))
end

function norm{T<:BlasFloat, TI<:Integer}(x::StridedVector{T}, rx::Union(UnitRange{TI},Range{TI}))
    (minimum(rx) < 1 || maximum(rx) > length(x)) && throw(BoundsError())
    BLAS.nrm2(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx))
end

vecnorm1{T<:BlasReal}(x::Union(Array{T},StridedVector{T})) =
    length(x) < ASUM_CUTOFF ? generic_vecnorm1(x) : BLAS.asum(x)

vecnorm2{T<:BlasFloat}(x::Union(Array{T},StridedVector{T})) =
    length(x) < NRM2_CUTOFF ? generic_vecnorm2(x) : BLAS.nrm2(x)

function triu!(M::AbstractMatrix, k::Integer)
    m, n = size(M)
    idx = 1
    for j = 0:n-1
        ii = min(max(0, j+1-k), m)
        for i = (idx+ii):(idx+m-1)
            M[i] = zero(M[i])
        end
        idx += m
    end
    M
end

triu(M::Matrix, k::Integer) = triu!(copy(M), k)

function tril!(M::AbstractMatrix, k::Integer)
    m, n = size(M)
    idx = 1
    for j = 0:n-1
        ii = min(max(0, j-k), m)
        for i = idx:(idx+ii-1)
            M[i] = zero(M[i])
        end
        idx += m
    end
    M
end

tril(M::Matrix, k::Integer) = tril!(copy(M), k)

function gradient(F::Vector, h::Vector)
    n = length(F)
    T = typeof(one(eltype(F))/one(eltype(h)))
    g = Array(T,n)
    if n == 1
        g[1] = zero(T)
    elseif n > 1
        g[1] = (F[2] - F[1]) / (h[2] - h[1])
        g[n] = (F[n] - F[n-1]) / (h[end] - h[end-1])
        if n > 2
            h = h[3:n] - h[1:n-2]
            g[2:n-1] = (F[3:n] - F[1:n-2]) ./ h
        end
    end
    g
end

function diagind(m::Integer, n::Integer, k::Integer=0)
    -m <= k <= n || throw(BoundsError())
    k <= 0 ? range(1-k, m+1, min(m+k, n)) : range(k*m+1, m+1, min(m, n-k))
end

diagind(A::AbstractMatrix, k::Integer=0) = diagind(size(A,1), size(A,2), k)

diag(A::Matrix, k::Integer=0) = A[diagind(A,k)]

function diagm{T}(v::AbstractVector{T}, k::Integer=0)
    n = length(v) + abs(k)
    A = zeros(T,n,n)
    A[diagind(A,k)] = v
    A
end

diagm(x::Number) = (X = Array(typeof(x),1,1); X[1,1] = x; X)

function trace{T}(A::Matrix{T})
    n = chksquare(A)
    t = zero(T)
    for i=1:n
        t += A[i,i]
    end
    t
end

function kron{T,S}(a::Matrix{T}, b::Matrix{S})
    R = Array(promote_type(T,S), size(a,1)*size(b,1), size(a,2)*size(b,2))
    m = 1
    for j = 1:size(a,2), l = 1:size(b,2), i = 1:size(a,1)
        aij = a[i,j]
        for k = 1:size(b,1)
            R[m] = aij*b[k,l]
            m += 1
        end
    end
    R
end

kron(a::Number, b::Union(Number, Vector, Matrix)) = a * b
kron(a::Union(Vector, Matrix), b::Number) = a * b
kron(a::Vector, b::Vector)=vec(kron(reshape(a,length(a),1),reshape(b,length(b),1)))
kron(a::Matrix, b::Vector)=kron(a,reshape(b,length(b),1))
kron(a::Vector, b::Matrix)=kron(reshape(a,length(a),1),b)

^(A::Matrix, p::Integer) = p < 0 ? inv(A^-p) : Base.power_by_squaring(A,p)

function ^(A::Matrix, p::Number)
    isinteger(p) && return A^integer(real(p))

    chksquare(A)
    v, X = eig(A)
    any(v.<0) && (v = complex(v))
    Xinv = ishermitian(A) ? X' : inv(X)
    scale(X, v.^p)*Xinv
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
    U
end

rref(x::Number) = one(x)

# Matrix exponential
expm{T<:BlasFloat}(A::StridedMatrix{T}) = expm!(copy(A))
expm{T<:Integer}(A::StridedMatrix{T}) = expm!(float(A))
expm(x::Number) = exp(x)

## Destructive matrix exponential using algorithm from Higham, 2008,
## "Functions of Matrices: Theory and Computation", SIAM
function expm!{T<:BlasFloat}(A::StridedMatrix{T})
    n = chksquare(A)
    n<2 && return exp(A)
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
            si = ceil(Int,s)
            A /= convert(T,2^si)
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
            for t=1:si X *= X end
        end
    end

    # Undo the balancing
    for j = ilo:ihi
        scj = scale[j]
        for i = 1:n
            X[j,i] *= scj
        end
        for i = 1:n
            X[i,j] /= scj
        end
    end

    if ilo > 1       # apply lower permutations in reverse order
        for j in (ilo-1):-1:1 rcswap!(j, int(scale[j]), X) end
    end
    if ihi < n       # apply upper permutations in forward order
        for j in (ihi+1):n    rcswap!(j, int(scale[j]), X) end
    end
    X
end

## Swap rows i and j and columns i and j in X
function rcswap!{T<:Number}(i::Integer, j::Integer, X::StridedMatrix{T})
    for k = 1:size(X,1)
        X[k,i], X[k,j] = X[k,j], X[k,i]
    end
    for k = 1:size(X,2)
        X[i,k], X[j,k] = X[j,k], X[i,k]
    end
end

function sqrtm{T<:Real}(A::StridedMatrix{T})
    issym(A) && return sqrtm(Symmetric(A))
    n = chksquare(A)
    SchurF = schurfact(complex(A))
    R = full(sqrtm(Triangular(SchurF[:T], :U, false)))
    retmat = SchurF[:vectors]*R*SchurF[:vectors]'
    all(imag(retmat) .== 0) ? real(retmat) : retmat
end
function sqrtm{T<:Complex}(A::StridedMatrix{T})
    ishermitian(A) && return sqrtm(Hermitian(A))
    n = chksquare(A)
    SchurF = schurfact(A)
    R = full(sqrtm(Triangular(SchurF[:T], :U, false)))
    SchurF[:vectors]*R*SchurF[:vectors]'
end
sqrtm(a::Number) = (b = sqrt(complex(a)); imag(b) == 0 ? real(b) : b)
sqrtm(a::Complex) = sqrt(a)

function inv{S}(A::StridedMatrix{S})
    T = typeof(one(S)/one(S))
    Ac = convert(AbstractMatrix{T}, A)
    if istriu(Ac)
        Ai = inv(Triangular(A, :U, false))
    elseif istril(Ac)
        Ai = inv(Triangular(A, :L, false))
    else
        Ai = inv(lufact(Ac))
    end
    return convert(typeof(Ac), Ai)
end

function factorize{T}(A::Matrix{T})
    m, n = size(A)
    if m == n
        if m == 1 return A[1] end
        utri    = true
        utri1   = true
        herm    = true
        sym     = true
        for j = 1:n-1, i = j+1:m
            if utri1
                if A[i,j] != 0
                    utri1 = i == j + 1
                    utri = false
                end
            end
            if sym
                sym &= A[i,j] == A[j,i]
            end
            if herm
                herm &= A[i,j] == conj(A[j,i])
            end
            if !(utri1|herm|sym) break end
        end
        ltri = true
        ltri1 = true
        for j = 3:n, i = 1:j-2
            ltri1 &= A[i,j] == 0
            if !ltri1 break end
        end
        if ltri1
            for i = 1:n-1
                if A[i,i+1] != 0
                    ltri &= false
                    break
                end
            end
            if ltri
                if utri
                    return Diagonal(A)
                end
                if utri1
                    return Bidiagonal(diag(A), diag(A, -1), false)
                end
                return Triangular(A, :L)
            end
            if utri
                return Bidiagonal(diag(A), diag(A, 1), true)
            end
            if utri1
                if (herm & (T <: Complex)) | sym
                    try
                        return ldltfact!(SymTridiagonal(diag(A), diag(A, -1)))
                    end
                end
                return lufact(Tridiagonal(diag(A, -1), diag(A), diag(A, 1)))
            end
        end
        if utri
            return Triangular(A, :U)
        end
        if herm
            try
                return cholfact(A)
            end
            return factorize(Hermitian(A))
        end
        if sym
            return factorize(Symmetric(A))
        end
        return lufact(A)
    end
    qrfact(A,pivot=typeof(zero(T)/sqrt(zero(T) + zero(T)))<:BlasFloat) # Generic pivoted QR not implemented yet
end

(\)(a::Vector, B::StridedVecOrMat) = (\)(reshape(a, length(a), 1), B)
function (\)(A::StridedMatrix, B::StridedVecOrMat)
    m, n = size(A)
    if m == n
        if istril(A)
            return istriu(A) ? \(Diagonal(A),B) : \(Triangular(A, :L),B)
        end
        istriu(A) && return \(Triangular(A, :U),B)
        return \(lufact(A),B)
    end
    return qrfact(A,pivot=eltype(A)<:BlasFloat)\B
end

## Moore-Penrose inverse
function pinv{T}(A::StridedMatrix{T}, tol::Real)
## for __dense__ ill-conditioned matrices, please use
## the threshold tol = sqrt(eps(real(float(one(T)))))
    m, n        = size(A)
    (m == 0 || n == 0) && return Array(T, n, m)
    if istril(A)
       if( istriu(A) )
          maxabsA = maximum(abs(diag(A)))
          B = zeros(T,n,m);
          for i = 1:min(m,n)
             if( abs(A[i,i]) > tol*maxabsA && isfinite(one(T)/A[i,i]) )
                 B[i,i] = one(T)/A[i,i]
             end
          end
          return B;
       end
    end
    SVD         = svdfact(A, thin=true)
    S           = eltype(SVD[:S])
    Sinv        = zeros(S, length(SVD[:S]))
    index       = SVD[:S] .> tol*maximum(SVD[:S])
    Sinv[index] = one(S) ./ SVD[:S][index]
    Sinv[find(!isfinite(Sinv))] = zero(S)
    return SVD[:Vt]'scale(Sinv, SVD[:U]')
end
function pinv{T}(A::StridedMatrix{T})
    tol = eps(real(float(one(T))))*maximum(size(A))
    return pinv(A, tol)
end
pinv(a::StridedVector) = pinv(reshape(a, length(a), 1))
pinv(x::Number) = isfinite(one(x)/x) ? one(x)/x : zero(x)

## Basis for null space
function null{T}(A::StridedMatrix{T})
    m, n = size(A)
    (m == 0 || n == 0) && return eye(T, n)
    SVD = svdfact(A, thin=false)
    indstart = sum(SVD[:S] .> max(m,n)*maximum(SVD[:S])*eps(eltype(SVD[:S]))) + 1
    return SVD[:V][:,indstart:end]
end
null(a::StridedVector) = null(reshape(a, length(a), 1))

function cond(A::StridedMatrix, p::Real=2)
    if p == 2
        v = svdvals(A)
        maxv = maximum(v)
        return maxv == 0.0 ? oftype(real(A[1,1]),Inf) : maxv / minimum(v)
    elseif p == 1 || p == Inf
        chksquare(A)
        return cond(lufact(A), p)
    end
    throw(ArgumentError("invalid p-norm p=$p. Valid: 1, 2 or Inf"))
end

## Lyapunov and Sylvester equation

# AX + XB + C = 0
function sylvester{T<:BlasFloat}(A::StridedMatrix{T},B::StridedMatrix{T},C::StridedMatrix{T})
    RA, QA = schur(A)
    RB, QB = schur(B)

    D = -Ac_mul_B(QA,C*QB)
    Y, scale = LAPACK.trsyl!('N','N', RA, RB, D)
    scale!(QA*A_mul_Bc(Y,QB), inv(scale))
end
sylvester{T<:Integer}(A::StridedMatrix{T},B::StridedMatrix{T},C::StridedMatrix{T}) = sylvester(float(A), float(B), float(C))

# AX + XA' + C = 0
function lyap{T<:BlasFloat}(A::StridedMatrix{T},C::StridedMatrix{T})
    R, Q = schur(A)

    D = -Ac_mul_B(Q,C*Q)
    Y, scale = LAPACK.trsyl!('N', T <: Complex ? 'C' : 'T', R, R, D)
    scale!(Q*A_mul_Bc(Y,Q), inv(scale))
end
lyap{T<:Integer}(A::StridedMatrix{T},C::StridedMatrix{T}) = lyap(float(A), float(C))
lyap{T<:Number}(a::T, c::T) = -c/(2a)

