# Linear algebra functions for dense matrices in column major format

scale!(X::Array{Float32}, s::Real) = BLAS.scal!(length(X), float32(s), X, 1)
scale!(X::Array{Float64}, s::Real) = BLAS.scal!(length(X), float64(s), X, 1)
scale!(X::Array{Complex64}, s::Real) = (ccall(("sscal_",Base.libblas_name), Void, (Ptr{BlasInt}, Ptr{Float32}, Ptr{Complex64}, Ptr{BlasInt}), &(2*length(X)), &s, X, &1); X)
scale!(X::Array{Complex128}, s::Real) = (ccall(("dscal_",Base.libblas_name), Void, (Ptr{BlasInt}, Ptr{Float64}, Ptr{Complex128}, Ptr{BlasInt}), &(2*length(X)), &s, X, &1); X)

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

kron(a::Vector, b::Vector) = vec([ a[i]*b[j] for j=1:length(b), i=1:length(a) ])

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

^(A::Matrix, p::Integer) = p < 0 ? inv(A^-p) : Base.power_by_squaring(A,p)

function ^(A::Matrix, p::Number)
    if integer_valued(p)
        ip = integer(real(p))
        if ip < 0
            return inv(Base.power_by_squaring(A, -ip))
        else
            return Base.power_by_squaring(A, ip)
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

function sqrtm(A::StridedMatrix, cond::Bool)
    m, n = size(A)
    if m != n error("DimentionMismatch") end
    if ishermitian(A) 
        return sqrtm(Hermitian(A), cond)
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

function det(A::Matrix)
    m, n = size(A)
    if m != n; throw(LAPACK.DimensionMismatch("det only defined for square matrices")); end
    if istriu(A) | istril(A); return det(Triangular(A, 'U', false)); end
    return det(lufact(A))
end
det(x::Number) = x

logdet(A::Matrix) = 2.0 * sum(log(diag(cholfact(A)[:U])))

function inv(A::StridedMatrix)
    if istriu(A) return inv(Triangular(A, 'U')) end
    if istril(A) return inv(Triangular(A, 'L')) end
    if ishermitian(A) return inv(Hermitian(A)) end
    return inv(lufact(A))
end

schur{T<:BlasFloat}(A::StridedMatrix{T}) = LAPACK.gees!('V', copy(A))

function (\){T<:BlasFloat}(A::StridedMatrix{T}, B::StridedVecOrMat{T})
    if size(A, 1) == size(A, 2) # Square
        if istriu(A) return Triangular(A, 'U')\B end
        if istril(A) return Triangular(A, 'L')\B end
        if ishermitian(A) return Hermitian(A)\B end
        ans, _, _, info = LAPACK.gesv!(copy(A), copy(B))
        if info > 0; throw(LinAlg.LAPACK.SingularException(info)); end
        return ans
    end
    LAPACK.gelsd!(copy(A), copy(B))[1]
end

(\){T1<:BlasFloat, T2<:BlasFloat}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) =
    (\)(convert(Array{promote_type(T1,T2)},A), convert(Array{promote_type(T1,T2)},B))
(\){T1<:BlasFloat, T2<:Real}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(A, convert(Array{T1}, B))
(\){T1<:Real, T2<:BlasFloat}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(convert(Array{T2}, A), B)
(\){T1<:Real, T2<:Real}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(float64(A), float64(B))
(\){T1<:Number, T2<:Number}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(complex128(A), complex128(B))
(\)(a::Vector, B::StridedVecOrMat) = (\)(reshape(a, length(a), 1), B)

(/)(A::StridedVecOrMat, B::StridedVecOrMat) = (B' \ A')'

## Moore-Penrose inverse
function pinv{T<:BlasFloat}(A::StridedMatrix{T})
    SVD         = svdfact(A, true)
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
    SVD = svdfact(A)
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
        cnd = 1 / LAPACK.gecon!(p == 1 ? '1' : 'I', lufact(A).LU, norm(A, p))
    else
        error("Norm type must be 1, 2 or Inf")
    end
    return cnd
end
cond(A::StridedMatrix) = cond(A, 2)
