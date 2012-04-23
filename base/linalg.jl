## linalg.jl: Basic Linear Algebra interface specifications ##
#
# This file mostly contains commented functions which are supposed
# to be defined in type-specific linalg_<type>.jl files.
#
# It only actually defines default argument versions which will
# fail without a proper implementation anyway.


#aCb(x::AbstractVector, y::AbstractVector)
#aTb{T<:Real}(x::AbstractVector{T}, y::AbstractVector{T})

#dot(x::AbstractVector, y::AbstractVector)

#cross(a::AbstractVector, b::AbstractVector)

#(*){T,S}(A::AbstractMatrix{T}, B::AbstractVector{S})
#(*){T,S}(A::AbstractVector{S}, B::AbstractMatrix{T})
#(*){T,S}(A::AbstractMatrix{T}, B::AbstractMatrix{S})

triu(M::AbstractMatrix) = triu(M,0)
tril(M::AbstractMatrix) = tril(M,0)
#triu{T}(M::AbstractMatrix{T}, k::Integer)
#tril{T}(M::AbstractMatrix{T}, k::Integer)

#diff(a::AbstractVector)

#diff(a::AbstractMatrix, dim::Integer)
diff(a::AbstractMatrix) = diff(a, 1)

gradient(F::AbstractVector) = gradient(F, [1:length(F)])
gradient(F::AbstractVector, h::Real) = gradient(F, [h*(1:length(F))])
#gradient(F::AbstractVector, h::AbstractVector)

diag(A::AbstractVector) = error("Perhaps you meant to use diagm().")
#diag(A::AbstractMatrix)

#diagm{T}(v::Union(AbstractVector{T},AbstractMatrix{T}))

#norm(x::AbstractVector, p::Number)
#norm(x::AbstractVector)

#norm(A::AbstractMatrix, p)
norm(A::AbstractMatrix) = norm(A, 2)
rank(A::AbstractMatrix, tol::Real) = sum(svd(A)[2] > tol)
rank(A::AbstractMatrix) = rank(A, 0)

#trace{T}(A::AbstractMatrix{T})

#kron(a::AbstractVector, b::AbstractVector)
#kron{T,S}(a::AbstractMatrix{T}, b::AbstractMatrix{S})

#det(a::AbstractMatrix)
#inv(a::AbstractMatrix)
#cond(a::AbstractMatrix, p)
cond(a::AbstractMatrix) = cond(a, 2)

#XXX this doens't seem to belong here
function randsym(n)
    a = randn(n,n)
    for j=1:n-1, i=j+1:n
        x = (a[i,j]+a[j,i])/2
        a[i,j] = x
        a[j,i] = x
    end
    a
end

#issym(A::AbstractMatrix)

# Randomized matrix symmetry test
# Theorem: AbstractMatrix is symmetric iff x'*A*y == y'*A*x, for randomly chosen x and y.
#issym_rnd(A::AbstractArray)

#ishermitian(A::AbstractMatrix)
#istriu(A::AbstractMatrix)
#istril(A::AbstractMatrix)

# XXX needs type annotation
#linreg(x, y)

# weighted least squares
# XXX needs type annotation
#linreg(x, y, w)

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
