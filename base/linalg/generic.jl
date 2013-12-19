## linalg.jl: Some generic Linear Algebra definitions

scale(X::AbstractArray, s::Number) = scale!(copy(X), s)
scale(s::Number, X::AbstractArray) = scale!(copy(X), s)

function scale!(X::AbstractArray, s::Number)
    for i in 1:length(X)
        @inbounds X[i] *= s
    end
    X
end
scale!(s::Number, X::AbstractArray) = scale!(X, s)

cross(a::AbstractVector, b::AbstractVector) = [a[2]*b[3]-a[3]*b[2], a[3]*b[1]-a[1]*b[3], a[1]*b[2]-a[2]*b[1]]

triu(M::AbstractMatrix) = triu(M,0)
tril(M::AbstractMatrix) = tril(M,0)
#triu{T}(M::AbstractMatrix{T}, k::Integer)
#tril{T}(M::AbstractMatrix{T}, k::Integer)
triu!(M::AbstractMatrix) = triu!(M,0)
tril!(M::AbstractMatrix) = tril!(M,0)

#diff(a::AbstractVector)
#diff(a::AbstractMatrix, dim::Integer)
diff(a::AbstractMatrix) = diff(a, 1)
diff(a::AbstractVector) = [ a[i+1] - a[i] for i=1:length(a)-1 ]

function diff(A::AbstractMatrix, dim::Integer)
    if dim == 1
        [A[i+1,j] - A[i,j] for i=1:size(A,1)-1, j=1:size(A,2)]
    else
        [A[i,j+1] - A[i,j] for i=1:size(A,1), j=1:size(A,2)-1]
    end
end


gradient(F::AbstractVector) = gradient(F, [1:length(F)])
gradient(F::AbstractVector, h::Real) = gradient(F, [h*(1:length(F))])
#gradient(F::AbstractVector, h::AbstractVector)

diag(A::AbstractVector) = error("use diagm instead of diag to construct a diagonal matrix")
#diag(A::AbstractMatrix)

#diagm{T}(v::AbstractVecOrMat{T})

function norm{T}(x::AbstractVector{T}, p::Number)
    if length(x) == 0
        a = zero(T)
    elseif p == Inf
        a = maximum(abs(x))
    elseif p == -Inf
        a = minimum(abs(x))
    else
        absx = abs(x)
        dx = maximum(absx)
        if dx != zero(T)
            scale!(absx, 1/dx)
            a = dx * (sum(absx.^p).^(1/p))
        else
            a = sum(absx.^p).^(1/p)
        end
    end
    float(a)
end
norm{T<:Integer}(x::AbstractVector{T}, p::Number) = norm(float(x), p)
norm(x::AbstractVector) = norm(x, 2)

function norm(A::AbstractMatrix, p::Number=2)
    m, n = size(A)
    if m == 0 || n == 0
        a = zero(eltype(A))
    elseif m == 1 || n == 1
        a = norm(reshape(A, length(A)), p)
    elseif p == 1
        a = maximum(sum(abs(A),1))
    elseif p == 2
        a = maximum(svdvals(A))
    elseif p == Inf
        a = maximum(sum(abs(A),2))
    else
        throw(ArgumentError("invalid p-norm p=$p. Valid: 1, 2, Inf"))
    end
    float(a)
end

norm(x::Number, p=nothing) = abs(x)

normfro(A::AbstractMatrix) = norm(reshape(A, length(A)))
normfro(x::Number) = abs(x)

rank(A::AbstractMatrix, tol::Real) = sum(svdvals(A) .> tol)
function rank(A::AbstractMatrix)
    m,n = size(A)
    (m == 0 || n == 0) && return 0
    sv = svdvals(A)
    return sum(sv .> maximum(size(A))*eps(sv[1]))
end
rank(x::Number) = x==0 ? 0 : 1

function trace(A::AbstractMatrix)
    chksquare(A)
    sum(diag(A))
end
trace(x::Number) = x

#kron(a::AbstractVector, b::AbstractVector)
#kron{T,S}(a::AbstractMatrix{T}, b::AbstractMatrix{S})

#det(a::AbstractMatrix)

inv(a::AbstractVector) = error("argument must be a square matrix")

function \{TA<:Number,TB<:Number}(A::AbstractMatrix{TA}, B::AbstractVecOrMat{TB})
    TC = typeof(one(TA)/one(TB))
    return TB == TC ? A_ldiv_B!(A, copy(B)) : A_ldiv_B!(A, convert(Array{TC}, B))
end
\(a::AbstractVector, b::AbstractArray) = reshape(a, length(a), 1) \ b
/(A::AbstractVecOrMat, B::AbstractVecOrMat) = (B' \ A')'

cond(x::Number) = x == 0 ? Inf : 1.0
cond(x::Number, p) = cond(x)

function issym(A::AbstractMatrix)
    m, n = size(A)
    m==n || return false
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
    m==n || return false
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

linreg{T<:Number}(X::StridedVecOrMat{T}, y::Vector{T}) = [ones(T, size(X,1)) X] \ y

# weighted least squares
function linreg(x::AbstractVector, y::AbstractVector, w::AbstractVector)
    sw = sqrt(w)
    [sw sw.*x] \ (sw.*y)
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
    parallel ? sum(pmap(peakflops, [ n for i in 1:nworkers()])) : (2*n^3/t)
end
