# Linear algebra functions for dense matrices in column major format

scale!{T<:BlasFloat}(X::Array{T}, s::Number) = BLAS.scal!(length(X), convert(T,s), X, 1)
scale!{T<:BlasReal}(X::Array{T}, s::Complex) = scale!(complex(X), s)
scale!{T<:BlasComplex}(X::Array{T}, s::Real) = BLAS.scal!(length(X), oftype(real(zero(T)),s), X, 1)

#Test whether a matrix is positive-definite
isposdef!{T<:BlasFloat}(A::Matrix{T}, UL::Char) = LAPACK.potrf!(UL, A)[2] == 0
isposdef!(A::Matrix) = ishermitian(A) && isposdef!(A, 'U')

isposdef{T<:BlasFloat}(A::Matrix{T}, UL::Char) = isposdef!(copy(A), UL)
isposdef{T<:BlasFloat}(A::Matrix{T}) = isposdef!(copy(A))
isposdef{T<:Number}(A::Matrix{T}, UL::Char) = isposdef!(float64(A), UL)
isposdef{T<:Number}(A::Matrix{T}) = isposdef!(float64(A))
isposdef(x::Number) = imag(x)==0 && real(x) > 0

norm{T<:BlasFloat}(x::Vector{T}) = BLAS.nrm2(length(x), x, 1)

function norm{T<:BlasFloat, TI<:Integer}(x::Vector{T}, rx::Union(Range1{TI},Range{TI}))
    (minimum(rx) < 1 || maximum(rx) > length(x)) && throw(BoundsError())
    BLAS.nrm2(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx))
end

function norm{T<:BlasFloat}(x::Vector{T}, p::Number)
    n = length(x)
    if n == 0
        return zero(T)
    elseif p == 2
        return BLAS.nrm2(n, x, 1)
    elseif p == 1
        return BLAS.asum(n, x, 1)
    elseif p == Inf
        return maximum(abs(x))
    elseif p == -Inf
        return minimum(abs(x))
    elseif p == 0
        return convert(T, nnz(x))
    else
        absx = abs(x)
        dx = maximum(absx)
        dx==zero(T) && return zero(T)
        scale!(absx, 1/dx)
        return dx * (sum(absx.^p).^(1/p))
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
    M
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
    if 0 < k < n
        return Range(k*m+1,m+1,min(m,n-k))
    elseif 0 <= -k <= m
        return Range(1-k,m+1,min(m+k,n))
    end
    throw(BoundsError())
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

randsym(n) = symmetrize!(randn(n,n))

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

function sqrtm{T<:Real}(A::StridedMatrix{T}, cond::Bool)
    issym(A) && return sqrtm(Symmetric(A), cond)
    
    n = chksquare(A)
    SchurF = schurfact!(complex(A))
    R = zeros(eltype(SchurF[:T]), n, n)
    for j = 1:n
        R[j,j] = sqrt(SchurF[:T][j,j])
        for i = j - 1:-1:1
            r = SchurF[:T][i,j]
            for k = i + 1:j - 1
                r -= R[i,k]*R[k,j]
            end
            if r != 0
                R[i,j] = r / (R[i,i] + R[j,j])
            end
        end
    end
    retmat = SchurF[:vectors]*R*SchurF[:vectors]'
    retmat2= all(imag(retmat) .== 0) ? real(retmat) : retmat
    cond ? (retmat2, alpha) : retmat2
end
function sqrtm{T<:Complex}(A::StridedMatrix{T}, cond::Bool)
    ishermitian(A) && return sqrtm(Hermitian(A), cond)
    
    n = chksquare(A)
    SchurF = schurfact(A)
    R = zeros(eltype(SchurF[:T]), n, n)
    for j = 1:n
        R[j,j] = sqrt(SchurF[:T][j,j])
        for i = j - 1:-1:1
            r = SchurF[:T][i,j]
            for k = i + 1:j - 1
                r -= R[i,k]*R[k,j]
            end
            if r != 0
                R[i,j] = r / (R[i,i] + R[j,j])
            end
        end
    end
    retmat = SchurF[:vectors]*R*SchurF[:vectors]'
    cond ? (retmat, norm(R)^2/norm(SchurF[:T])) : retmat
end

sqrtm{T<:Integer}(A::StridedMatrix{T}, cond::Bool) = sqrtm(float(A), cond)
sqrtm{T<:Integer}(A::StridedMatrix{Complex{T}}, cond::Bool) = sqrtm(complex128(A), cond)
sqrtm(A::StridedMatrix) = sqrtm(A, false)
sqrtm(a::Number) = (b = sqrt(complex(a)); imag(b) == 0 ? real(b) : b)
sqrtm(a::Complex) = sqrt(a)

function det(A::Matrix)
    (istriu(A) || istril(A)) && return det(Triangular(A, :U, false))
    return det(lufact(A))
end
det(x::Number) = x

logdet(A::Matrix) = logdet(lufact(A))

function inv(A::Matrix)
    if istriu(A) return inv(Triangular(A, :U, false)) end
    if istril(A) return inv(Triangular(A, :L, false)) end
    return inv(lufact(A))
end

function factorize!{T}(A::Matrix{T})
    m, n = size(A)
    if m == n
        if m == 1 return A[1] end
        utri    = true
        utri1   = true
        herm    = T <: Complex
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
            if (T <: Complex) & herm
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
                    return ldltd!(SymTridiagonal(diag(A), diag(A, -1)))
                end
                return lufact!(Tridiagonal(diag(A, -1), diag(A), diag(A, 1)))
            end
        end
        if utri
            return Triangular(A, :U)
        end
        if herm
            if T <: BlasFloat
                C, info = LAPACK.potrf!('U', copy(A))
            elseif typeof(one(T)/one(T)) <: BlasFloat
                C, info = LAPACK.potrf!('U', float(A))
            else
                error("Unable to factorize hermitian $(typeof(A)). Try converting to other element type or use explicit factorization.")
            end
            if info == 0 return Cholesky(C, 'U') end
            return factorize!(Hermitian(A))
        end
        if sym
            if T <: BlasFloat
                C, info = LAPACK.potrf!('U', copy(A))
            elseif eltype(one(T)/one(T)) <: BlasFloat
                C, info = LAPACK.potrf!('U', float(A))
            else
                error("Unable to factorize symmetric $(typeof(A)). Try converting to other element type or use explicit factorization.")
            end
            if info == 0 return Cholesky(C, 'U') end
            return factorize!(Symmetric(A))
        end
        return lufact!(A)
    end
    return qrfact!(A,pivot=true)
end

factorize(A::AbstractMatrix) = factorize!(copy(A))

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
    return qrfact(A,pivot=true)\B
end

## Moore-Penrose inverse
function pinv{T<:BlasFloat}(A::StridedMatrix{T})
    m, n = size(A)
    (m == 0 || n == 0) && return Array(T, n, m)
    SVD         = svdfact(A, true)
    Sinv        = zeros(T, length(SVD[:S]))
    index       = SVD[:S] .> eps(real(one(T)))*max(m,n)*maximum(SVD[:S])
    Sinv[index] = 1.0 ./ SVD[:S][index]
    return SVD[:Vt]'scale(Sinv, SVD[:U]')
end
pinv{T<:Integer}(A::StridedMatrix{T}) = pinv(float(A))
pinv(a::StridedVector) = pinv(reshape(a, length(a), 1))
pinv(x::Number) = one(x)/x

## Basis for null space
function null{T<:BlasFloat}(A::StridedMatrix{T})
    m, n = size(A)
    (m == 0 || n == 0) && return eye(T, n)
    SVD = svdfact(A, false)
    indstart = sum(SVD[:S] .> max(m,n)*maximum(SVD[:S])*eps(eltype(SVD[:S]))) + 1
    return SVD[:V][:,indstart:end]
end
null{T<:Integer}(A::StridedMatrix{T}) = null(float(A))
null(a::StridedVector) = null(reshape(a, length(a), 1))

function cond(A::StridedMatrix, p::Real=2) 
    if p == 2
        v = svdvals(A)
        maxv = maximum(v)
        return maxv == 0.0 ? inf(typeof(real(A[1,1]))) : maxv / minimum(v)
    elseif p == 1 || p == Inf
        chksquare(A)
        return cond(lufact(A), p)
    end
    throw(ArgumentError("invalid p-norm p=$p. Valid: 1, 2 or Inf"))
end
