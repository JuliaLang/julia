## linalg.jl: Basic Linear Algebra interface specifications and
## specialized matrix types

#
# This file mostly contains commented functions which are supposed
# to be defined in type-specific linalg_<type>.jl files.
#
# It defines functions in cases where sufficiently few assumptions about
# storage can be made.

#Ac_mul_B(x::AbstractVector, y::AbstractVector)
#At_mul_B{T<:Real}(x::AbstractVector{T}, y::AbstractVector{T})

#dot(x::AbstractVector, y::AbstractVector)

#cross(a::AbstractVector, b::AbstractVector)

#(*){T,S}(A::AbstractMatrix{T}, B::AbstractVector{S})
#(*){T,S}(A::AbstractVector{S}, B::AbstractMatrix{T})
#(*){T,S}(A::AbstractMatrix{T}, B::AbstractMatrix{S})

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

function norm(x::AbstractVector, p::Number)
    if p == Inf
        return max(abs(x))
    elseif p == -Inf
        return min(abs(x))
    else
        return sum(abs(x).^p).^(1/p)
    end
end

norm(x::AbstractVector) = sqrt(real(dot(x,x)))

function norm(A::AbstractMatrix, p)
    if size(A,1) == 1 || size(A,2) == 1
        return norm(reshape(A, numel(A)), p)
    elseif p == 1
        return max(sum(abs(A),1))
    elseif p == 2
        return max(svd(A)[2])
    elseif p == Inf
        max(sum(abs(A),2))
    elseif p == "fro"
        return sqrt(sum(diag(A'*A)))
    else
        error("invalid parameter to matrix norm")
    end
end

norm(A::AbstractMatrix) = norm(A, 2)
rank(A::AbstractMatrix, tol::Real) = sum(svdvals(A) .> tol)
function rank(A::AbstractMatrix)
    sv = svdvals(A)
    sum(sv .> max(size(A,1),size(A,2))*eps(sv[1]))
end

trace(A::AbstractMatrix) = sum(diag(A))

#kron(a::AbstractVector, b::AbstractVector)
#kron{T,S}(a::AbstractMatrix{T}, b::AbstractMatrix{S})

#det(a::AbstractMatrix)
inv(a::AbstractMatrix) = a \ one(a)
cond(a::AbstractMatrix, p) = norm(a, p) * norm(inv(a), p)
cond(a::AbstractMatrix) = cond(a, 2)

#issym(A::AbstractMatrix)
#ishermitian(A::AbstractMatrix)
#istriu(A::AbstractMatrix)
#istril(A::AbstractMatrix)

function linreg(x::AbstractVector, y::AbstractVector)
    M = [ones(length(x)) x]
    Mt = M'
    ((Mt*M)\Mt)*y
end

# weighted least squares
function linreg(x::AbstractVector, y::AbstractVector, w::AbstractVector)
    w = sqrt(w)
    M = [w w.*x]
    Mt = M'
    ((Mt*M)\Mt)*(w.*y)
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
