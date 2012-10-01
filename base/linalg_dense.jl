## linalg_dense.jl: Basic Linear Algebra functions for dense representations ##
#
# note that many functions have specific versions for FloatingPoint/Complex arguments
# which use BLAS instead

triu{T}(M::Matrix{T}, k::Integer) = [ j-i >= k ? M[i,j] : zero(T) for
                                     i=1:size(M,1), j=1:size(M,2) ]
tril{T}(M::Matrix{T}, k::Integer) = [ j-i <= k ? M[i,j] : zero(T) for
                                     i=1:size(M,1), j=1:size(M,2) ]
function triu!{T}(M::Matrix{T}, k::Integer)
    m, n = size(M)
    for i = 1:m
        for j = 1:n
            if j-i < k
                M[i,j] = zero(T)
            end
        end
    end
end
function tril!{T}(M::Matrix{T}, k::Integer)
    m, n = size(M)
    for i = 1:m
        for j = 1:n
            if j-i > k
                M[i,j] = zero(T)
            end
        end
    end
end

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

# Randomized matrix symmetry test
# Theorem: Matrix is symmetric iff x'*A*y == y'*A*x, for randomly chosen x and y.
function issym_rnd(A::Matrix)
    m, n = size(A)
    if m != n; return false; end
    ntrials = 5

    for i=1:ntrials
        x = randn(n)
        y = randn(n)
        if (x'*A*y - y'*A*x)[1] > 1e-6; return false; end
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

# multiply by diagonal matrix as vector
function diagmm!(C::Matrix, A::Matrix, b::Vector)
    m, n = size(A)
    if n != length(b)
        error("argument dimensions do not match")
    end
    for j = 1:n
        bj = b[j]
        for i = 1:m
            C[i,j] = A[i,j]*bj
        end
    end
    return C
end

function diagmm!(C::Matrix, b::Vector, A::Matrix)
    m, n = size(A)
    if m != length(b)
        error("argument dimensions do not match")
    end
    for j=1:n
        for i=1:m
            C[i,j] = A[i,j]*b[i]
        end
    end
    return C
end

diagmm(A::Matrix, b::Vector) =
    diagmm!(Array(promote_type(eltype(A),eltype(b)),size(A)), A, b)
diagmm(b::Vector, A::Matrix) =
    diagmm!(Array(promote_type(eltype(A),eltype(b)),size(A)), b, A)

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

function findmax(a::Array)
    m = typemin(eltype(a))
    mi = 0
    for i=1:length(a)
        if a[i] > m
            m = a[i]
            mi = i
        end
    end
    return (m, mi)
end

function findmin(a::Array)
    m = typemax(eltype(a))
    mi = 0
    for i=1:length(a)
        if a[i] < m
            m = a[i]
            mi = i
        end
    end
    return (m, mi)
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
