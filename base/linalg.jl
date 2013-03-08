## linalg.jl: Some generic Linear Algebra definitions

function scale!{T<:Number}(X::AbstractArray{T}, s::Real)
    # FIXME: could use BLAS in more cases
    for i in 1:length(X)
        X[i] *= s;
    end
    return X
end

cross(a::AbstractVector, b::AbstractVector) =
    [a[2]*b[3]-a[3]*b[2], a[3]*b[1]-a[1]*b[3], a[1]*b[2]-a[2]*b[1]]

triu(M::AbstractMatrix) = triu(M,0)
tril(M::AbstractMatrix) = tril(M,0)
#triu{T}(M::AbstractMatrix{T}, k::Integer)
#tril{T}(M::AbstractMatrix{T}, k::Integer)
triu!(M::AbstractMatrix) = triu!(M,0)
tril!(M::AbstractMatrix) = tril!(M,0)

#diff(a::AbstractVector)
#diff(a::AbstractMatrix, dim::Integer)
diff(a::AbstractMatrix) = diff(a, 1)

gradient(F::AbstractVector) = gradient(F, [1:length(F)])
gradient(F::AbstractVector, h::Real) = gradient(F, [h*(1:length(F))])
#gradient(F::AbstractVector, h::AbstractVector)

diag(A::AbstractVector) = error("Perhaps you meant to use diagm().")
#diag(A::AbstractMatrix)

#diagm{T}(v::Union(AbstractVector{T},AbstractMatrix{T}))

function norm{T}(x::AbstractVector{T}, p::Number)
    if length(x) == 0
        a = zero(T)
    elseif p == Inf
        a = max(abs(x))
    elseif p == -Inf
        a = min(abs(x))
    else
        absx = abs(x)
        dx = max(absx)
        if dx != zero(T)
            scale!(absx, 1/dx)
            a = dx * (sum(absx.^p).^(1/p))
        else
            a = sum(absx.^p).^(1/p)
        end
    end
    return float(a)
end
norm{T<:Integer}(x::AbstractVector{T}, p::Number) = norm(float(x), p)
norm(x::AbstractVector) = norm(x, 2)

function norm(A::AbstractMatrix, p::Number)
    m, n = size(A)
    if m == 0 || n == 0
        a = zero(eltype(A))
    elseif m == 1 || n == 1
        a = norm(reshape(A, length(A)), p)
    elseif p == 1
        a = max(sum(abs(A),1))
    elseif p == 2
        a = max(svdvals(A))
    elseif p == Inf
        a = max(sum(abs(A),2))
    else
        error("invalid parameter p given to compute matrix norm")
    end
    return float(a)
end

norm(A::AbstractMatrix) = norm(A, 2)

norm(x::Number) = abs(x)
norm(x::Number, p) = abs(x)

normfro(A::AbstractMatrix) = norm(reshape(A, length(A)))
normfro(x::Number) = abs(x)

rank(A::AbstractMatrix, tol::Real) = sum(svdvals(A) .> tol)
function rank(A::AbstractMatrix)
    m,n = size(A)
    if m == 0 || n == 0; return 0; end
    sv = svdvals(A)
    sum(sv .> max(size(A))*eps(sv[1]))
end
rank(x::Number) = x == 0 ? 0 : 1

trace(A::AbstractMatrix) = sum(diag(A))
trace(x::Number) = x

#kron(a::AbstractVector, b::AbstractVector)
#kron{T,S}(a::AbstractMatrix{T}, b::AbstractMatrix{S})

#det(a::AbstractMatrix)
inv(a::AbstractMatrix) = a \ one(a)

inv(a::AbstractVector) = inv(reshape(a, length(a), 1))

\(a::AbstractVector, b::AbstractArray) = reshape(a, length(a), 1) \ b

cond(x::Number) = x == 0 ? Inf : 1.0
cond(x::Number, p) = cond(x)

function issym(A::AbstractMatrix)
    m, n = size(A)
    if m != n; return false; end
    for i = 1:(n-1), j = (i+1):n
        if A[i,j] != A[j,i]
            return false
        end
    end
    return true
end

issym(x::Number) = true

function ishermitian(A::AbstractMatrix)
    m, n = size(A)
    if m != n; return false; end
    for i = 1:n, j = i:n
        if A[i,j] != conj(A[j,i])
            return false
        end
    end
    return true
end

ishermitian(x::Number) = (x == conj(x))

function istriu(A::AbstractMatrix)
    m, n = size(A)
    for j = 1:min(n,m-1), i = j+1:m
        if A[i,j] != 0
            return false
        end
    end
    return true
end

function istril(A::AbstractMatrix)
    m, n = size(A)
    for j = 2:n, i = 1:min(j-1,m)
        if A[i,j] != 0
            return false
        end
    end
    return true
end

istriu(x::Number) = true
istril(x::Number) = true

function linreg{T<:Number}(X::StridedVecOrMat{T}, y::Vector{T})
    [ones(T, size(X,1)) X] \ y
end

# weighted least squares
function linreg(x::AbstractVector, y::AbstractVector, w::AbstractVector)
    w = sqrt(w)
    [w w.*x] \ (w.*y)
end

# multiply by diagonal matrix as vector
#diagmm!(C::AbstractMatrix, A::AbstractMatrix, b::AbstractVector)

#diagmm!(C::AbstractMatrix, b::AbstractVector, A::AbstractMatrix)

diagmm!(A::AbstractMatrix, b::AbstractVector) = diagmm!(A,A,b)
diagmm!(b::AbstractVector, A::AbstractMatrix) = diagmm!(A,b,A)

#diagmm(A::AbstractMatrix, b::AbstractVector)
#diagmm(b::AbstractVector, A::AbstractMatrix)

#^(A::AbstractMatrix, p::Number)

#findmax(a::AbstractArray)
#findmin(a::AbstractArray)

#rref{T}(A::AbstractMatrix{T})
