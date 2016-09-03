# This file is a part of Julia. License is MIT: http://julialang.org/license

## linalg.jl: Some generic Linear Algebra definitions

# For better performance when input and output are the same array
# See https://github.com/JuliaLang/julia/issues/8415#issuecomment-56608729
function generic_scale!(X::AbstractArray, s::Number)
    @simd for I in eachindex(X)
        @inbounds X[I] *= s
    end
    X
end

function generic_scale!(s::Number, X::AbstractArray)
    @simd for I in eachindex(X)
        @inbounds X[I] = s*X[I]
    end
    X
end

function generic_scale!(C::AbstractArray, X::AbstractArray, s::Number)
    if _length(C) != _length(X)
        throw(DimensionMismatch("first array has length $(_length(C)) which does not match the length of the second, $(_length(X))."))
    end
    for (IC, IX) in zip(eachindex(C), eachindex(X))
        @inbounds C[IC] = X[IX]*s
    end
    C
end

function generic_scale!(C::AbstractArray, s::Number, X::AbstractArray)
    if _length(C) != _length(X)
        throw(DimensionMismatch("first array has length $(_length(C)) which does not
match the length of the second, $(_length(X))."))
    end
    for (IC, IX) in zip(eachindex(C), eachindex(X))
        @inbounds C[IC] = s*X[IX]
    end
    C
end

scale!(C::AbstractArray, s::Number, X::AbstractArray) = generic_scale!(C, X, s)
scale!(C::AbstractArray, X::AbstractArray, s::Number) = generic_scale!(C, s, X)

"""
    scale!(A, b)
    scale!(b, A)

Scale an array `A` by a scalar `b` overwriting `A` in-place.

If `A` is a matrix and `b` is a vector, then `scale!(A,b)` scales each column `i` of `A` by
`b[i]` (similar to `A*Diagonal(b)`), while `scale!(b,A)` scales each row `i` of `A` by `b[i]`
(similar to `Diagonal(b)*A`), again operating in-place on `A`. An `InexactError` exception is
thrown if the scaling produces a number not representable by the element type of `A`,
e.g. for integer types.

```jldoctest
julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> b = [1; 2]
2-element Array{Int64,1}:
 1
 2

julia> scale!(a,b)
2×2 Array{Int64,2}:
 1  4
 3  8

julia> a = [1 2; 3 4];

julia> b = [1; 2];

julia> scale!(b,a)
2×2 Array{Int64,2}:
 1  2
 6  8
```
"""
scale!(X::AbstractArray, s::Number) = generic_scale!(X, s)
scale!(s::Number, X::AbstractArray) = generic_scale!(s, X)

"""
    cross(x, y)
    ×(x,y)

Compute the cross product of two 3-vectors.

```jldoctest
julia> a = [0;1;0]
3-element Array{Int64,1}:
 0
 1
 0

julia> b = [0;0;1]
3-element Array{Int64,1}:
 0
 0
 1

julia> cross(a,b)
3-element Array{Int64,1}:
 1
 0
 0
```
"""
cross(a::AbstractVector, b::AbstractVector) = [a[2]*b[3]-a[3]*b[2], a[3]*b[1]-a[1]*b[3], a[1]*b[2]-a[2]*b[1]]

"""
    triu(M)

Upper triangle of a matrix.

```jldoctest
julia> a = ones(4,4)
4×4 Array{Float64,2}:
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0

julia> triu(a)
4×4 Array{Float64,2}:
 1.0  1.0  1.0  1.0
 0.0  1.0  1.0  1.0
 0.0  0.0  1.0  1.0
 0.0  0.0  0.0  1.0
```
"""
triu(M::AbstractMatrix) = triu!(copy(M))

"""
    tril(M)

Lower triangle of a matrix.

```jldoctest
julia> a = ones(4,4)
4×4 Array{Float64,2}:
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0

julia> tril(a)
4×4 Array{Float64,2}:
 1.0  0.0  0.0  0.0
 1.0  1.0  0.0  0.0
 1.0  1.0  1.0  0.0
 1.0  1.0  1.0  1.0
```
"""
tril(M::AbstractMatrix) = tril!(copy(M))

"""
    triu(M, k::Integer)

Returns the upper triangle of `M` starting from the `k`th superdiagonal.

```jldoctest
julia> a = ones(4,4)
4×4 Array{Float64,2}:
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0

julia> triu(a,3)
4×4 Array{Float64,2}:
 0.0  0.0  0.0  1.0
 0.0  0.0  0.0  0.0
 0.0  0.0  0.0  0.0
 0.0  0.0  0.0  0.0

julia> triu(a,-3)
4×4 Array{Float64,2}:
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
```
"""
triu(M::AbstractMatrix,k::Integer) = triu!(copy(M),k)

"""
    tril(M, k::Integer)

Returns the lower triangle of `M` starting from the `k`th superdiagonal.

```jldoctest
julia> a = ones(4,4)
4×4 Array{Float64,2}:
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0

julia> tril(a,3)
4×4 Array{Float64,2}:
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0

julia> tril(a,-3)
4×4 Array{Float64,2}:
 0.0  0.0  0.0  0.0
 0.0  0.0  0.0  0.0
 0.0  0.0  0.0  0.0
 1.0  0.0  0.0  0.0
```
"""
tril(M::AbstractMatrix,k::Integer) = tril!(copy(M),k)

"""
    triu!(M)

Upper triangle of a matrix, overwriting `M` in the process.
See also [`triu`](:func:`triu`).
"""
triu!(M::AbstractMatrix) = triu!(M,0)

"""
    tril!(M)

Lower triangle of a matrix, overwriting `M` in the process.
See also [`tril`](:func:`tril`).
"""
tril!(M::AbstractMatrix) = tril!(M,0)

diff(a::AbstractVector) = [ a[i+1] - a[i] for i=1:length(a)-1 ]

"""
    diff(A, [dim::Integer=1])

Finite difference operator of matrix or vector `A`. If `A` is a matrix,
compute the finite difference over a dimension `dim` (default `1`).

```jldoctest
julia> a = [2 4; 6 16]
2×2 Array{Int64,2}:
 2   4
 6  16

julia> diff(a,2)
2×1 Array{Int64,2}:
  2
 10
```
"""
function diff(A::AbstractMatrix, dim::Integer=1)
    if dim == 1
        [A[i+1,j] - A[i,j] for i=1:size(A,1)-1, j=1:size(A,2)]
    elseif dim == 2
        [A[i,j+1] - A[i,j] for i=1:size(A,1), j=1:size(A,2)-1]
    else
        throw(ArgumentError("dimension dim must be 1 or 2, got $dim"))
    end
end


gradient(F::AbstractVector) = gradient(F, [1:length(F);])

"""
    gradient(F::AbstractVector, [h::Real])

Compute differences along vector `F`, using `h` as the spacing between points. The default
spacing is one.

```jldoctest
julia> a = [2,4,6,8];

julia> gradient(a)
4-element Array{Float64,1}:
 2.0
 2.0
 2.0
 2.0
```
"""
gradient(F::AbstractVector, h::Real) = gradient(F, [h*(1:length(F));])

diag(A::AbstractVector) = throw(ArgumentError("use diagm instead of diag to construct a diagonal matrix"))

#diagm{T}(v::AbstractVecOrMat{T})

###########################################################################################
# Inner products and norms

# special cases of vecnorm; note that they don't need to handle isempty(x)
function generic_vecnormMinusInf(x)
    s = start(x)
    (v, s) = next(x, s)
    minabs = norm(v)
    while !done(x, s)
        (v, s) = next(x, s)
        vnorm = norm(v)
        minabs = ifelse(isnan(minabs) | (minabs < vnorm), minabs, vnorm)
    end
    return float(minabs)
end

function generic_vecnormInf(x)
    s = start(x)
    (v, s) = next(x, s)
    maxabs = norm(v)
    while !done(x, s)
        (v, s) = next(x, s)
        vnorm = norm(v)
        maxabs = ifelse(isnan(maxabs) | (maxabs > vnorm), maxabs, vnorm)
    end
    return float(maxabs)
end

function generic_vecnorm1(x)
    s = start(x)
    (v, s) = next(x, s)
    av = float(norm(v))
    T = typeof(av)
    sum::promote_type(Float64, T) = av
    while !done(x, s)
        (v, s) = next(x, s)
        sum += norm(v)
    end
    return convert(T, sum)
end

# faster computation of norm(x)^2, avoiding overflow for integers
norm_sqr(x) = norm(x)^2
norm_sqr(x::Number) = abs2(x)
norm_sqr{T<:Integer}(x::Union{T,Complex{T},Rational{T}}) = abs2(float(x))

function generic_vecnorm2(x)
    maxabs = vecnormInf(x)
    (maxabs == 0 || isinf(maxabs)) && return maxabs
    s = start(x)
    (v, s) = next(x, s)
    T = typeof(maxabs)
    if isfinite(_length(x)*maxabs*maxabs) && maxabs*maxabs != 0 # Scaling not necessary
        sum::promote_type(Float64, T) = norm_sqr(v)
        while !done(x, s)
            (v, s) = next(x, s)
            sum += norm_sqr(v)
        end
        return convert(T, sqrt(sum))
    else
        sum = abs2(norm(v)/maxabs)
        while !done(x, s)
            (v, s) = next(x, s)
            sum += (norm(v)/maxabs)^2
        end
        return convert(T, maxabs*sqrt(sum))
    end
end

# Compute L_p norm ‖x‖ₚ = sum(abs(x).^p)^(1/p)
# (Not technically a "norm" for p < 1.)
function generic_vecnormp(x, p)
    s = start(x)
    (v, s) = next(x, s)
    if p > 1 || p < -1 # might need to rescale to avoid overflow
        maxabs = p > 1 ? vecnormInf(x) : vecnormMinusInf(x)
        (maxabs == 0 || isinf(maxabs)) && return maxabs
        T = typeof(maxabs)
    else
        T = typeof(float(norm(v)))
    end
    spp::promote_type(Float64, T) = p
    if -1 <= p <= 1 || (isfinite(_length(x)*maxabs^spp) && maxabs^spp != 0) # scaling not necessary
        sum::promote_type(Float64, T) = norm(v)^spp
        while !done(x, s)
            (v, s) = next(x, s)
            sum += norm(v)^spp
        end
        return convert(T, sum^inv(spp))
    else # rescaling
        sum = (norm(v)/maxabs)^spp
        while !done(x, s)
            (v, s) = next(x, s)
            sum += (norm(v)/maxabs)^spp
        end
        return convert(T, maxabs*sum^inv(spp))
    end
end

vecnormMinusInf(x) = generic_vecnormMinusInf(x)
vecnormInf(x) = generic_vecnormInf(x)
vecnorm1(x) = generic_vecnorm1(x)
vecnorm2(x) = generic_vecnorm2(x)
vecnormp(x, p) = generic_vecnormp(x, p)

"""
    vecnorm(A, [p::Real=2])

For any iterable container `A` (including arrays of any dimension) of numbers (or any
element type for which `norm` is defined), compute the `p`-norm (defaulting to `p=2`) as if
`A` were a vector of the corresponding length.

For example, if `A` is a matrix and `p=2`, then this is equivalent to the Frobenius norm.
"""
function vecnorm(itr, p::Real=2)
    isempty(itr) && return float(real(zero(eltype(itr))))
    if p == 2
        return vecnorm2(itr)
    elseif p == 1
        return vecnorm1(itr)
    elseif p == Inf
        return vecnormInf(itr)
    elseif p == 0
        return convert(typeof(float(real(zero(eltype(itr))))),
               countnz(itr))
    elseif p == -Inf
        return vecnormMinusInf(itr)
    else
        vecnormp(itr,p)
    end
end
@inline vecnorm(x::Number, p::Real=2) = p == 0 ? real(x==0 ? zero(x) : one(x)) : abs(x)

norm(x::AbstractVector, p::Real=2) = vecnorm(x, p)

function norm1{T}(A::AbstractMatrix{T})
    m, n = size(A)
    Tnorm = typeof(float(real(zero(T))))
    Tsum = promote_type(Float64,Tnorm)
    nrm::Tsum = 0
    @inbounds begin
        for j = 1:n
            nrmj::Tsum = 0
            for i = 1:m
                nrmj += norm(A[i,j])
            end
            nrm = max(nrm,nrmj)
        end
    end
    return convert(Tnorm, nrm)
end
function norm2{T}(A::AbstractMatrix{T})
    m,n = size(A)
    if m == 1 || n == 1 return vecnorm2(A) end
    Tnorm = typeof(float(real(zero(T))))
    (m == 0 || n == 0) ? zero(Tnorm) : convert(Tnorm, svdvals(A)[1])
end
function normInf{T}(A::AbstractMatrix{T})
    m,n = size(A)
    Tnorm = typeof(float(real(zero(T))))
    Tsum = promote_type(Float64,Tnorm)
    nrm::Tsum = 0
    @inbounds begin
        for i = 1:m
            nrmi::Tsum = 0
            for j = 1:n
                nrmi += norm(A[i,j])
            end
            nrm = max(nrm,nrmi)
        end
    end
    return convert(Tnorm, nrm)
end

"""
    norm(A, [p::Real=2])

Compute the `p`-norm of a vector or the operator norm of a matrix `A`, defaulting to the `p=2`-norm.

For vectors, `p` can assume any numeric value (even though not all values produce a
mathematically valid vector norm). In particular, `norm(A, Inf)` returns the largest value
in `abs(A)`, whereas `norm(A, -Inf)` returns the smallest.

For matrices, the matrix norm induced by the vector `p`-norm is used, where valid values of
`p` are `1`, `2`, or `Inf`. (Note that for sparse matrices, `p=2` is currently not
implemented.) Use [`vecnorm`](:func:`vecnorm`) to compute the Frobenius norm.
"""
function norm{T}(A::AbstractMatrix{T}, p::Real=2)
    if p == 2
        return norm2(A)
    elseif p == 1
        return norm1(A)
    elseif p == Inf
        return normInf(A)
    else
        throw(ArgumentError("invalid p-norm p=$p. Valid: 1, 2, Inf"))
    end
end

@inline norm(x::Number, p::Real=2) = vecnorm(x, p)

function vecdot(x::AbstractArray, y::AbstractArray)
    lx = _length(x)
    if lx != _length(y)
        throw(DimensionMismatch("first array has length $(lx) which does not match the length of the second, $(_length(y))."))
    end
    if lx == 0
        return dot(zero(eltype(x)), zero(eltype(y)))
    end
    s = zero(dot(first(x), first(y)))
    for (Ix, Iy) in zip(eachindex(x), eachindex(y))
        @inbounds  s += dot(x[Ix], y[Iy])
    end
    s
end

"""
    vecdot(x, y)

For any iterable containers `x` and `y` (including arrays of any dimension) of numbers (or
any element type for which `dot` is defined), compute the Euclidean dot product (the sum of
`dot(x[i],y[i])`) as if they were vectors.
"""
function vecdot(x, y) # arbitrary iterables
    ix = start(x)
    if done(x, ix)
        if !isempty(y)
            throw(DimensionMismatch("x and y are of different lengths!"))
        end
        return dot(zero(eltype(x)), zero(eltype(y)))
    end
    iy = start(y)
    if done(y, iy)
        throw(DimensionMismatch("x and y are of different lengths!"))
    end
    (vx, ix) = next(x, ix)
    (vy, iy) = next(y, iy)
    s = dot(vx, vy)
    while !done(x, ix)
        if done(y, iy)
            throw(DimensionMismatch("x and y are of different lengths!"))
        end
        (vx, ix) = next(x, ix)
        (vy, iy) = next(y, iy)
        s += dot(vx, vy)
    end
    if !done(y, iy)
            throw(DimensionMismatch("x and y are of different lengths!"))
    end
    return s
end

vecdot(x::Number, y::Number) = conj(x) * y

dot(x::Number, y::Number) = vecdot(x, y)

"""
    dot(x, y)
    ⋅(x,y)

Compute the dot product. For complex vectors, the first vector is conjugated.
"""
dot(x::AbstractVector, y::AbstractVector) = vecdot(x, y)

###########################################################################################

"""
    rank(M[, tol::Real])

Compute the rank of a matrix.
"""
rank(A::AbstractMatrix, tol::Real) = sum(svdvals(A) .> tol)
function rank(A::AbstractMatrix)
    m,n = size(A)
    (m == 0 || n == 0) && return 0
    sv = svdvals(A)
    return sum(sv .> maximum(size(A))*eps(sv[1]))
end
rank(x::Number) = x==0 ? 0 : 1

"""
    trace(M)

Matrix trace.
"""
function trace(A::AbstractMatrix)
    checksquare(A)
    sum(diag(A))
end
trace(x::Number) = x

#kron(a::AbstractVector, b::AbstractVector)
#kron{T,S}(a::AbstractMatrix{T}, b::AbstractMatrix{S})

#det(a::AbstractMatrix)

inv(a::StridedMatrix) = throw(ArgumentError("argument must be a square matrix"))

"""
    inv(M)

Matrix inverse.
"""
function inv{T}(A::AbstractMatrix{T})
    S = typeof(zero(T)/one(T))
    A_ldiv_B!(factorize(convert(AbstractMatrix{S}, A)), eye(S, checksquare(A)))
end

"""
    \\(A, B)

Matrix division using a polyalgorithm. For input matrices `A` and `B`, the result `X` is
such that `A*X == B` when `A` is square. The solver that is used depends upon the structure
of `A`.  If `A` is upper or lower triangular (or diagonal), no factorization of `A` is
required and the system is solved with either forward or backward substitution.
For non-triangular square matrices, an LU factorization is used.

For rectangular `A` the result is the minimum-norm least squares solution computed by a
pivoted QR factorization of `A` and a rank estimate of `A` based on the R factor.

When `A` is sparse, a similar polyalgorithm is used. For indefinite matrices, the `LDLt`
factorization does not use pivoting during the numerical factorization and therefore the
procedure can fail even for invertible matrices.
"""
function (\)(A::AbstractMatrix, B::AbstractVecOrMat)
    m, n = size(A)
    if m == n
        if istril(A)
            if istriu(A)
                return Diagonal(A) \ B
            else
                return LowerTriangular(A) \ B
            end
        end
        if istriu(A)
            return UpperTriangular(A) \ B
        end
        return lufact(A) \ B
    end
    return qrfact(A,Val{true}) \ B
end

(\)(a::AbstractVector, b::AbstractArray) = reshape(a, length(a), 1) \ b
(/)(A::AbstractVecOrMat, B::AbstractVecOrMat) = (B' \ A')'
# \(A::StridedMatrix,x::Number) = inv(A)*x Should be added at some point when the old elementwise version has been deprecated long enough
# /(x::Number,A::StridedMatrix) = x*inv(A)

cond(x::Number) = x == 0 ? Inf : 1.0
cond(x::Number, p) = cond(x)

#Skeel condition numbers
condskeel(A::AbstractMatrix, p::Real=Inf) = norm(abs(inv(A))*abs(A), p)
condskeel{T<:Integer}(A::AbstractMatrix{T}, p::Real=Inf) = norm(abs(inv(float(A)))*abs(A), p)

"""
    condskeel(M, [x, p::Real=Inf])

```math
\\kappa_S(M, p) & = \\left\\Vert \\left\\vert M \\right\\vert \\left\\vert M^{-1} \\right\\vert  \\right\\Vert_p \\\\
\\kappa_S(M, x, p) & = \\left\\Vert \\left\\vert M \\right\\vert \\left\\vert M^{-1} \\right\\vert \\left\\vert x \\right\\vert \\right\\Vert_p
```

Skeel condition number ``\\kappa_S`` of the matrix `M`, optionally with respect to the
vector `x`, as computed using the operator `p`-norm.
`p` is `Inf` by default, if not provided. Valid values for `p` are `1`, `2`, or `Inf`.

This quantity is also known in the literature as the Bauer condition number, relative
condition number, or componentwise relative condition number.
"""
condskeel(A::AbstractMatrix, x::AbstractVector, p::Real=Inf) = norm(abs(inv(A))*abs(A)*abs(x), p)
condskeel{T<:Integer}(A::AbstractMatrix{T}, x::AbstractVector, p::Real=Inf) = norm(abs(inv(float(A)))*abs(A)*abs(x), p)

"""
    issymmetric(A) -> Bool

Test whether a matrix is symmetric.

```jldoctest
julia> a = [1 2; 2 -1]
2×2 Array{Int64,2}:
 1   2
 2  -1

julia> issymmetric(a)
true

julia> b = [1 im; -im 1]
2×2 Array{Complex{Int64},2}:
 1+0im  0+1im
 0-1im  1+0im

julia> issymmetric(b)
false
```
"""
function issymmetric(A::AbstractMatrix)
    indsm, indsn = indices(A)
    if indsm != indsn
        return false
    end
    for i = first(indsn):last(indsn)-1, j = (i+1):last(indsn)
        if A[i,j] != transpose(A[j,i])
            return false
        end
    end
    return true
end

issymmetric(x::Number) = true

"""
    ishermitian(A) -> Bool

Test whether a matrix is Hermitian.

```jldoctest
julia> a = [1 2; 2 -1]
2×2 Array{Int64,2}:
 1   2
 2  -1

julia> ishermitian(a)
true

julia> b = [1 im; -im 1]
2×2 Array{Complex{Int64},2}:
 1+0im  0+1im
 0-1im  1+0im

julia> ishermitian(b)
true
```
"""
function ishermitian(A::AbstractMatrix)
    indsm, indsn = indices(A)
    if indsm != indsn
        return false
    end
    for i = indsn, j = i:last(indsn)
        if A[i,j] != ctranspose(A[j,i])
            return false
        end
    end
    return true
end

ishermitian(x::Number) = (x == conj(x))

"""
    istriu(A) -> Bool

Test whether a matrix is upper triangular.

```jldoctest
julia> a = [1 2; 2 -1]
2×2 Array{Int64,2}:
 1   2
 2  -1

julia> istriu(a)
false

julia> b = [1 im; 0 -1]
2×2 Array{Complex{Int64},2}:
 1+0im   0+1im
 0+0im  -1+0im

julia> istriu(b)
true
```
"""
function istriu(A::AbstractMatrix)
    m, n = size(A)
    for j = 1:min(n,m-1), i = j+1:m
        if A[i,j] != 0
            return false
        end
    end
    return true
end

"""
    istril(A) -> Bool

Test whether a matrix is lower triangular.

```jldoctest
julia> a = [1 2; 2 -1]
2×2 Array{Int64,2}:
 1   2
 2  -1

julia> istril(a)
false

julia> b = [1 0; -im -1]
2×2 Array{Complex{Int64},2}:
 1+0im   0+0im
 0-1im  -1+0im

julia> istril(b)
true
```
"""
function istril(A::AbstractMatrix)
    m, n = size(A)
    for j = 2:n, i = 1:min(j-1,m)
        if A[i,j] != 0
            return false
        end
    end
    return true
end

"""
    isdiag(A) -> Bool

Test whether a matrix is diagonal.

```jldoctest
julia> a = [1 2; 2 -1]
2×2 Array{Int64,2}:
 1   2
 2  -1

julia> isdiag(a)
false

julia> b = [im 0; 0 -im]
2×2 Array{Complex{Int64},2}:
 0+1im  0+0im
 0+0im  0-1im

julia> isdiag(b)
true
```
"""
isdiag(A::AbstractMatrix) = istril(A) && istriu(A)

istriu(x::Number) = true
istril(x::Number) = true
isdiag(x::Number) = true

"""
    linreg(x, y)

Perform simple linear regression using Ordinary Least Squares. Returns `a` and `b` such
that `a + b*x` is the closest straight line to the given points `(x, y)`, i.e., such that
the squared error between `y` and `a + b*x` is minimized.

**Examples:**

    using PyPlot
    x = 1.0:12.0
    y = [5.5, 6.3, 7.6, 8.8, 10.9, 11.79, 13.48, 15.02, 17.77, 20.81, 22.0, 22.99]
    a, b = linreg(x, y)          # Linear regression
    plot(x, y, "o")              # Plot (x, y) points
    plot(x, a + b*x)             # Plot line determined by linear regression

See also:

`\\`, [`cov`](:func:`cov`), [`std`](:func:`std`), [`mean`](:func:`mean`).

"""
function linreg(x::AbstractVector, y::AbstractVector)
    # Least squares given
    # Y = a + b*X
    # where
    # b = cov(X, Y)/var(X)
    # a = mean(Y) - b*mean(X)
    size(x) == size(y) || throw(DimensionMismatch("x and y must be the same size"))
    mx = mean(x)
    my = mean(y)
    # don't need to worry about the scaling (n vs n - 1) since they cancel in the ratio
    b = Base.covm(x, mx, y, my)/Base.varm(x, mx)
    a = my - b*mx
    return (a, b)
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

"""
    peakflops(n::Integer=2000; parallel::Bool=false)

`peakflops` computes the peak flop rate of the computer by using double precision
[`gemm!`](:func:`Base.LinAlg.BLAS.gemm!`). By default, if no arguments are specified, it
multiplies a matrix of size `n x n`, where `n = 2000`. If the underlying BLAS is using
multiple threads, higher flop rates are realized. The number of BLAS threads can be set with
[`BLAS.set_num_threads(n)`](:func:`Base.LinAlg.BLAS.set_num_threads`).

If the keyword argument `parallel` is set to `true`, `peakflops` is run in parallel on all
the worker processors. The flop rate of the entire parallel computer is returned. When
running in parallel, only 1 BLAS thread is used. The argument `n` still refers to the size
of the problem that is solved on each processor.
"""
function peakflops(n::Integer=2000; parallel::Bool=false)
    a = ones(Float64,100,100)
    t = @elapsed a2 = a*a
    a = ones(Float64,n,n)
    t = @elapsed a2 = a*a
    @assert a2[1,1] == n
    parallel ? sum(pmap(peakflops, [ n for i in 1:nworkers()])) : (2*Float64(n)^3/t)
end

# BLAS-like in-place y = x*α+y function (see also the version in blas.jl
#                                          for BlasFloat Arrays)
function axpy!(α, x::AbstractArray, y::AbstractArray)
    n = _length(x)
    if n != _length(y)
        throw(DimensionMismatch("x has length $n, but y has length $(_length(y))"))
    end
    for (IY, IX) in zip(eachindex(y), eachindex(x))
        @inbounds y[IY] += x[IX]*α
    end
    y
end

function axpy!{Ti<:Integer,Tj<:Integer}(α, x::AbstractArray, rx::AbstractArray{Ti}, y::AbstractArray, ry::AbstractArray{Tj})
    if _length(rx) != _length(ry)
        throw(DimensionMismatch("rx has length $(_length(rx)), but ry has length $(_length(ry))"))
    elseif !checkindex(Bool, linearindices(x), rx)
        throw(BoundsError(x, rx))
    elseif !checkindex(Bool, linearindices(y), ry)
        throw(BoundsError(y, ry))
    end
    for (IY, IX) in zip(eachindex(ry), eachindex(rx))
        @inbounds y[ry[IY]] += x[rx[IX]]*α
    end
    y
end


# Elementary reflection similar to LAPACK. The reflector is not Hermitian but ensures that tridiagonalization of Hermitian matrices become real. See lawn72
@inline function reflector!(x::AbstractVector)
    n = length(x)
    @inbounds begin
        ξ1 = x[1]
        normu = abs2(ξ1)
        for i = 2:n
            normu += abs2(x[i])
        end
        if normu == zero(normu)
            return zero(ξ1/normu)
        end
        normu = sqrt(normu)
        ν = copysign(normu, real(ξ1))
        ξ1 += ν
        x[1] = -ν
        for i = 2:n
            x[i] /= ξ1
        end
    end
    ξ1/ν
end

@inline function reflectorApply!(x::AbstractVector, τ::Number, A::StridedMatrix) # apply reflector from left
    m, n = size(A)
    if length(x) != m
        throw(DimensionMismatch("reflector must have same length as first dimension of matrix"))
    end
    @inbounds begin
        for j = 1:n
            # dot
            vAj = A[1, j]
            for i = 2:m
                vAj += x[i]'*A[i, j]
            end

            vAj = τ'*vAj

            # ger
            A[1, j] -= vAj
            for i = 2:m
                A[i, j] -= x[i]*vAj
            end
        end
    end
    return A
end

"""
    det(M)

Matrix determinant.
"""
function det{T}(A::AbstractMatrix{T})
    if istriu(A) || istril(A)
        S = typeof((one(T)*zero(T) + zero(T))/one(T))
        return convert(S, det(UpperTriangular(A)))
    end
    return det(lufact(A))
end
det(x::Number) = x

"""
    logabsdet(M)

Log of absolute value of determinant of real matrix. Equivalent to `(log(abs(det(M))), sign(det(M)))`,
but may provide increased accuracy and/or speed.
"""
logabsdet(A::AbstractMatrix) = logabsdet(lufact(A))

"""
    logdet(M)

Log of matrix determinant. Equivalent to `log(det(M))`, but may provide increased accuracy
and/or speed.
"""
function logdet(A::AbstractMatrix)
    d,s = logabsdet(A)
    return d + log(s)
end

# isapprox: approximate equality of arrays [like isapprox(Number,Number)]
function isapprox{T<:Number,S<:Number}(x::AbstractArray{T}, y::AbstractArray{S}; rtol::Real=Base.rtoldefault(T,S), atol::Real=0, norm::Function=vecnorm)
    d = norm(x - y)
    if isfinite(d)
        return d <= atol + rtol*max(norm(x), norm(y))
    else
        # Fall back to a component-wise approximate comparison
        return all(ab -> isapprox(ab[1], ab[2]; rtol=rtol, atol=atol), zip(x, y))
    end
end

"""
    normalize!(v, [p::Real=2])

Normalize the vector `v` in-place with respect to the `p`-norm.
See also [`vecnorm`](:func:`vecnorm`) and [`normalize`](:func:`normalize`).
"""
function normalize!(v::AbstractVector, p::Real=2)
    nrm = norm(v, p)
    __normalize!(v, nrm)
end

@inline function __normalize!(v::AbstractVector, nrm::AbstractFloat)
    #The largest positive floating point number whose inverse is less than
    #infinity
    δ = inv(prevfloat(typemax(nrm)))

    if nrm ≥ δ #Safe to multiply with inverse
        invnrm = inv(nrm)
        scale!(v, invnrm)

    else # scale elements to avoid overflow
        εδ = eps(one(nrm))/δ
        scale!(v, εδ)
        scale!(v, inv(nrm*εδ))
    end

    v
end

"""
    normalize(v, [p::Real=2])

Normalize the vector `v` with respect to the `p`-norm.
See also [`normalize!`](:func:`normalize!`) and [`vecnorm`](:func:`vecnorm`).

```jldoctest
julia> a = [1,2,4];

julia> normalize(a)
3-element Array{Float64,1}:
 0.218218
 0.436436
 0.872872

julia> normalize(a,1)
3-element Array{Float64,1}:
 0.142857
 0.285714
 0.571429
```
"""
function normalize(v::AbstractVector, p::Real = 2)
    nrm = norm(v, p)
    if !isempty(v)
        vv = copy_oftype(v, typeof(v[1]/nrm))
        return __normalize!(vv, nrm)
    else
        T = typeof(zero(eltype(v))/nrm)
        return T[]
    end
end
