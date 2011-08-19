## linalg.j: Basic Linear Algebra functions ##

dot(x::AbstractVector, y::AbstractVector) = sum(x.*conj(y))

# blas.j defines these for floats; this handles other cases
# TODO: Also need the vector*matrix case
function (*){T,S}(A::AbstractMatrix{T}, B::AbstractVector{S})
    R = promote_type(T,S)
    mA = size(A, 1)
    mB = size(B, 1)
    C = zeros(R, mA)
    for k = 1:mB
        b = B[k]
        for i = 1:mA
            C[i] += b * A[i, k]
        end
    end
    C
end

function (*){T,S}(A::AbstractMatrix{T}, B::AbstractMatrix{S})
    R = promote_type(T,S)
    (mA, nA) = size(A)
    (mB, nB) = size(B)
    if mA == 2 && nA == 2 && nB == 2; return matmul2x2(A,B); end
    if mA == 3 && nA == 3 && nB == 3; return matmul3x3(A,B); end
    C = zeros(R, mA, nB)
    for j = 1:nB
        coffs = (j-1)*mA
        for k = 1:mB
            b = B[k, j]
            aoffs = (k-1)*mA
            for i = 1:mA
                C[coffs+i] += b * A[aoffs+i]
            end
        end
    end
    C
end

# multiply 2x2 matrices
function matmul2x2{T,S}(A::AbstractMatrix{T}, B::AbstractMatrix{S})
    R = promote_type(T,S)
    C = Array(R, 2, 2)

    A11 = A[1,1]; A12 = A[1,2]; A21 = A[2,1]; A22 = A[2,2]
    B11 = B[1,1]; B12 = B[1,2]; B21 = B[2,1]; B22 = B[2,2]

    C[1,1] = A11*B11 + A12*B21
    C[1,2] = A11*B12 + A12*B22
    C[2,1] = A21*B11 + A22*B21
    C[2,2] = A21*B12 + A22*B22

    return C
end

function matmul3x3{T,S}(A::AbstractMatrix{T}, B::AbstractMatrix{S})
    R = promote_type(T,S)
    C = Array(R, 3, 3)

    A11 = A[1,1]; A12 = A[1,2]; A13 = A[1,3];
    A21 = A[2,1]; A22 = A[2,2]; A23 = A[2,3];
    A31 = A[3,1]; A32 = A[3,2]; A33 = A[3,3];

    B11 = B[1,1]; B12 = B[1,2]; B13 = B[1,3];
    B21 = B[2,1]; B22 = B[2,2]; B23 = B[2,3];
    B31 = B[3,1]; B32 = B[3,2]; B33 = B[3,3];

    C[1,1] = A11*B11 + A12*B21 + A13*B31
    C[1,2] = A11*B12 + A12*B22 + A13*B32
    C[1,3] = A11*B13 + A12*B23 + A13*B33

    C[2,1] = A21*B11 + A22*B21 + A23*B31
    C[2,2] = A21*B12 + A22*B22 + A23*B32
    C[2,3] = A21*B13 + A22*B23 + A23*B33

    C[3,1] = A31*B11 + A32*B21 + A33*B31
    C[3,2] = A31*B12 + A32*B22 + A33*B32
    C[3,3] = A31*B13 + A32*B23 + A33*B33

    return C
end





triu(M) = triu(M,0)
tril(M) = tril(M,0)
triu{T}(M::AbstractMatrix{T}, k) = [ j-i >= k ? M[i,j] : zero(T) |
                                    i=1:size(M,1), j=1:size(M,2) ]
tril{T}(M::AbstractMatrix{T}, k) = [ j-i <= k ? M[i,j] : zero(T) |
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
