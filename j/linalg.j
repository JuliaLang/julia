## linalg.j: Basic Linear Algebra functions ##

dot(x::Vector, y::Vector) = sum(x.*conj(y))

# blas.j defines these for floats; this handles other cases
(*)(A::Matrix, B::Vector) = [ sum(A[i,:].*B) | i=1:size(A,1) ]
(*)(A::Matrix, B::Matrix) = [ sum(A[i,:].*B[:,j]) | i=1:size(A,1), j=1:size(B,2) ]

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

diag(A::Matrix) = [ A[i,i] | i=1:min(size(A)) ]

function diagm{T}(v::Union(Vector{T},Matrix{T}))
    if isa(v, Matrix)
        assert(size(v,1) == 1 || size(v,2) == 1)
    end

    n = max(size(v))
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
        return max(sum(abs(A)))
    elseif p == 2
        return max(diag(svd(A)[2]))
    elseif p == Inf
        max(sum(abs(A')))
    elseif p == "fro"
        return sqrt(sum(diag(A'*A)))
    end
end

norm(A::Matrix) = norm(A, 2)
rank(A::Matrix, tol::Real) = count(diag(svd(A)[2]) > tol)
rank(A::Matrix) = rank(A, 0)

# trace(A::Matrix) = sum(diag(A))

function trace(A::Matrix)
    t = 0
    for i=1:min(size(A))
        t += A[i,i]
    end
    return t
end

mean(V::Vector) = sum(V) / length(V)
std(V::Vector) = (m = mean(V);
                  sqrt( sum([ (V[i] - m)^2 | i=1:length(V) ]) / (length(V)-1) ))

kron(a::Vector, b::Vector) = [ a[i]*b[j] | i=1:length(a), j=1:length(b) ]
kron(a::Matrix, b::Matrix) = reshape([ a[i,j]*b[k,l] | k=1:size(b,1),
                                                       i=1:size(a,1),
                                                       l=1:size(b,2),
                                                       j=1:size(a,2)],
                                     size(a,1)*size(b,1),
                                     size(a,2)*size(b,2))

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
