## linalg.jl: Some generic Linear Algebra definitions

scale{T<:Number}(X::AbstractArray{T}, s::Number) = scale!(copy(X), s)

function scale!{T<:Number}(X::AbstractArray{T}, s::Number)
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

function trace(A::AbstractMatrix)
    if size(A,1) != size(A,2)
        error("expected square matrix")
    end
    sum(diag(A))
end
trace(x::Number) = x

#kron(a::AbstractVector, b::AbstractVector)
#kron{T,S}(a::AbstractMatrix{T}, b::AbstractMatrix{S})

#det(a::AbstractMatrix)

inv(a::AbstractVector) = error("Input must be a square matrix")

\(a::AbstractVector, b::AbstractArray) = reshape(a, length(a), 1) \ b
(/)(A::AbstractVector, B::AbstractVector) = (B' \ A')'
(/)(A::AbstractVector, B::AbstractMatrix) = (B' \ A')'
(/)(A::AbstractMatrix, B::AbstractVector) = (B' \ A')'
(/)(A::AbstractMatrix, B::AbstractMatrix) = (B' \ A')'


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

scale!(A::AbstractMatrix, b::AbstractVector) = scale!(A,A,b)
scale!(b::AbstractVector, A::AbstractMatrix) = scale!(A,b,A)

#diagmm(A::AbstractMatrix, b::AbstractVector)
#diagmm(b::AbstractVector, A::AbstractMatrix)

#^(A::AbstractMatrix, p::Number)

#findmax(a::AbstractArray)
#findmin(a::AbstractArray)

#rref{T}(A::AbstractMatrix{T})

function peakflops(n::Integer=2000; parallel::Bool=false)
    a = rand(100,100)
    t = @elapsed a*a
    a = rand(n,n)
    t = @elapsed a*a
    if parallel
        floprate = sum(pmap(peakflops, [ n for i in 1:nworkers()]) )
    else
        floprate = (2.0*float64(n)^3/t)
    end
    floprate
end

# Symmetric Toeplitz solver
function durbin!{T<:BlasReal}(r::AbstractVector{T}, y::AbstractVector{T})
    n = length(r)
    n <= length(y) || throw(DimensionMismatch("Auxiliary vector cannot be shorter than data vector"))
    y[1] = -r[1]
    β = one(T)
    α = -r[1]
    for k = 1:n-1
        β *= one(T) - α*α
        α = -r[k+1]
        for j = 1:k
            α -= r[k-j+1]*y[j]
        end
        α /= β
        for j = 1:div(k,2)
            tmp = y[j]
            y[j] += α*y[k-j+1]
            y[k-j+1] += α*tmp
        end
        if isodd(k) y[div(k,2)+1] *= one(T) + α end
        y[k+1] = α
    end
    return y
end
durbin{T<:BlasReal}(r::AbstractVector{T}) = durbin!(r, zeros(T, length(r)))

function levinson!{T<:BlasReal}(r::AbstractVector{T}, b::AbstractVector{T}, x::AbstractVector{T})
    n = length(b)
    n == length(r) || throw(DimensionMismatch("Vectors must have same length"))
    n <= length(x) || throw(DimensionMismatch("Auxiliary vector cannot be shorter than data vector"))
    x[1] = b[1]
    b[1] = -r[2]/r[1]
    β = one(T)
    α = -r[2]/r[1]
    for k = 1:n-1
        β *= one(T) - α*α
        μ = b[k+1]
        for j = 2:k+1
            μ -= r[j]/r[1]*x[k-j+2]
        end
        μ /= β
        for j = 1:k
            x[j] += μ*b[k-j+1]
        end
        x[k+1] = μ
        if k < n - 1
            α = -r[k+2]
            for j = 2:k+1
                α -= r[j]*b[k-j+2]
            end
            α /= β*r[1]
            for j = 1:div(k,2)
                tmp = b[j]
                b[j] += α*b[k-j+1]
                b[k-j+1] += α*tmp
            end
            if isodd(k) b[div(k,2)+1] *= one(T) + α end
            b[k+1] = α
        end
    end
    for i = 1:n
        x[i] /= r[1]
    end
    return x
end
levinson{T<:BlasReal}(r::AbstractVector{T}, b::AbstractVector{T}) = levinson!(r, copy(b), zeros(T, length(b)))