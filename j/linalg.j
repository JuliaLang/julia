## linalg.j: Basic Linear Algebra functions ##

dot(x::AbstractVector, y::AbstractVector) = sum(x.*conj(y))

# blas.j defines these for floats; this handles other cases
#(*)(A::Matrix, B::Vector) = [ sum(A[i,:].*B) | i=1:size(A,1) ]
function (*){T,S}(A::AbstractMatrix{T}, B::AbstractVector{S})
    R = promote_type(T,S)
    m = size(A,1)
    l = size(B,1)
    C = zeros(R, m)
    for k = 1:l
        b = B[k]
        for i = 1:m
            C[i] += b * A[i, k]
        end
    end
    C
end

#(*)(A::Matrix, B::Matrix) = [ sum(A[i,:].*B[:,j]) | i=1:size(A,1), j=1:size(B,2) ]
function (*){T,S}(A::AbstractMatrix{T}, B::AbstractMatrix{S})
    R = promote_type(T,S)
    m = size(A,1)
    n = size(B,2)
    l = size(B,1)
    C = zeros(R, m, n)
    for j = 1:n
        coffs = (j-1)*m
        for k = 1:l
            b = B[k, j]
            aoffs = (k-1)*m
            for i = 1:m
                C[coffs+i] += b * A[aoffs+i]
            end
        end
    end
    C
end

triu(M) = triu(M,0)
tril(M) = tril(M,0)
triu{T}(M::Matrix{T}, k) = [ j-i >= k ? M[i,j] : zero(T) |
                            i=1:size(M,1), j=1:size(M,2) ]
tril{T}(M::Matrix{T}, k) = [ j-i <= k ? M[i,j] : zero(T) |
                            i=1:size(M,1), j=1:size(M,2) ]

diff(a::Vector) = [ a[i+1] - a[i] | i=1:length(a)-1 ]

function diff(a::Matrix, dim)
    if dim == 1
        [ a[i+1,j] - a[i,j] | i=1:size(a,1)-1, j=1:size(a,2) ]
    else
        [ a[i,j+1] - a[i,j] | i=1:size(a,1), j=1:size(a,2)-1 ]
    end
end

diff(a::Matrix) = diff(a, 1)

diag(A::Matrix) = [ A[i,i] | i=1:min(size(A,1),size(A,2)) ]

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

function norm(x::Vector, p::Number)
    if p == Inf
        return max(abs(x))
    elseif p == -Inf
        return min(abs(x))
    else
        return sum(abs(x).^p).^(1/p)
    end
end

norm(x::Vector) = sqrt(sum(x.*x))

function norm(A::Matrix, p)
    if size(A,1) == 1 || size(A,2) == 1
        return norm(reshape(A, numel(A)), p)
    elseif p == 1
        return max(sum(abs(A),1))
    elseif p == 2
        return max(diag(svd(A)[2]))
    elseif p == Inf
        max(sum(abs(A),2))
    elseif p == "fro"
        return sqrt(sum(diag(A'*A)))
    end
end

norm(A::Matrix) = norm(A, 2)
rank(A::Matrix, tol::Real) = count(diag(svd(A)[2]) > tol)
rank(A::Matrix) = rank(A, 0)

# trace(A::Matrix) = sum(diag(A))

function trace{T}(A::Matrix{T})
    t = zero(T)
    for i=1:min(size(A))
        t += A[i,i]
    end
    return t
end

mean(V::Vector) = sum(V) / length(V)

function std(V::Vector)
    n = numel(V)
    m = mean(V)
    s = 0.0
    for i=1:n
        s += (V[i] - m)^2
    end
    return sqrt(s/(n-1))
end

kron(a::Vector, b::Vector) = [ a[i]*b[j] | i=1:length(a), j=1:length(b) ]

function kron{T,S}(a::Matrix{T}, b::Matrix{S})
    R = Array(promote_type(T,S), size(a,1)*size(b,1), size(a,2)*size(b,2))

    m = 1
    for j = 1:size(a,2)
        for l = 1:size(b,2)
            for i = 1:size(a,1)
                for k = 1:size(b,1)
                    R[m] = a[i,j]*b[k,l]
                    m += 1
                end
            end
        end
    end
    R
end

det(a::Matrix) = prod(diag(qr(a)[2]))
inv(a::Matrix) = a \ eye(size(a)[1])
cond(a::Matrix, p) = norm(a, p) * norm(inv(a), p)
cond(a::Matrix) = cond(a, 2)

function issymmetric(A::Matrix)
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

function isuppertriangular(A::Matrix)
    m, n = size(A)
    if m != n; error("matrix must be square, got $(m)x$(n)"); end
    for i = 1:n, j = 1:n
        if A[i,j] != 0 && j < i
            return false
        end
    end
    return true
end

function islowertriangular(A::Matrix)
    m, n = size(A)
    if m != n; error("matrix must be square, got $(m)x$(n)"); end
    for i = 1:n, j = n:-1:1
        if A[i,j] != 0 && j > i
            return false
        end
    end
    return true
end
