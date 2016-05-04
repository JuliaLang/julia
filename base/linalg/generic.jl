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
    if length(C) != length(X)
        throw(DimensionMismatch("first array has length $(length(C)) which does not match the length of the second, $(length(X))."))
    end
    for (IC, IX) in zip(eachindex(C), eachindex(X))
        @inbounds C[IC] = X[IX]*s
    end
    C
end

function generic_scale!(C::AbstractArray, s::Number, X::AbstractArray)
    if length(C) != length(X)
        throw(DimensionMismatch("first array has length $(length(C)) which does not
match the length of the second, $(length(X))."))
    end
    for (IC, IX) in zip(eachindex(C), eachindex(X))
        @inbounds C[IC] = s*X[IX]
    end
    C
end

scale!(C::AbstractArray, s::Number, X::AbstractArray) = generic_scale!(C, X, s)
scale!(C::AbstractArray, X::AbstractArray, s::Number) = generic_scale!(C, s, X)
scale!(X::AbstractArray, s::Number) = generic_scale!(X, s)
scale!(s::Number, X::AbstractArray) = generic_scale!(s, X)

cross(a::AbstractVector, b::AbstractVector) = [a[2]*b[3]-a[3]*b[2], a[3]*b[1]-a[1]*b[3], a[1]*b[2]-a[2]*b[1]]

triu(M::AbstractMatrix) = triu!(copy(M))
tril(M::AbstractMatrix) = tril!(copy(M))
triu(M::AbstractMatrix,k::Integer) = triu!(copy(M),k)
tril(M::AbstractMatrix,k::Integer) = tril!(copy(M),k)
triu!(M::AbstractMatrix) = triu!(M,0)
tril!(M::AbstractMatrix) = tril!(M,0)

diff(a::AbstractMatrix) = diff(a, 1)
diff(a::AbstractVector) = [ a[i+1] - a[i] for i=1:length(a)-1 ]

function diff(A::AbstractMatrix, dim::Integer)
    if dim == 1
        [A[i+1,j] - A[i,j] for i=1:size(A,1)-1, j=1:size(A,2)]
    elseif dim == 2
        [A[i,j+1] - A[i,j] for i=1:size(A,1), j=1:size(A,2)-1]
    else
        throw(ArgumentError("dimension dim must be 1 or 2, got $dim"))
    end
end


gradient(F::AbstractVector) = gradient(F, [1:length(F);])
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
    if isfinite(length(x)*maxabs*maxabs) && maxabs*maxabs != 0 # Scaling not necessary
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
    if -1 <= p <= 1 || (isfinite(length(x)*maxabs^spp) && maxabs^spp != 0) # scaling not necessary
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
    lx = length(x)
    if lx != length(y)
        throw(DimensionMismatch("first array has length $(lx) which does not match the length of the second, $(length(y))."))
    end
    if lx == 0
        return dot(zero(eltype(x)), zero(eltype(y)))
    end
    s = zero(dot(x[1], y[1]))
    for (Ix, Iy) in zip(eachindex(x), eachindex(y))
        @inbounds  s += dot(x[Ix], y[Iy])
    end
    s
end

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
dot(x::AbstractVector, y::AbstractVector) = vecdot(x, y)

###########################################################################################

rank(A::AbstractMatrix, tol::Real) = sum(svdvals(A) .> tol)
function rank(A::AbstractMatrix)
    m,n = size(A)
    (m == 0 || n == 0) && return 0
    sv = svdvals(A)
    return sum(sv .> maximum(size(A))*eps(sv[1]))
end
rank(x::Number) = x==0 ? 0 : 1

function trace(A::AbstractMatrix)
    checksquare(A)
    sum(diag(A))
end
trace(x::Number) = x

#kron(a::AbstractVector, b::AbstractVector)
#kron{T,S}(a::AbstractMatrix{T}, b::AbstractMatrix{S})

#det(a::AbstractMatrix)

inv(a::StridedMatrix) = throw(ArgumentError("argument must be a square matrix"))
function inv{T}(A::AbstractMatrix{T})
    S = typeof(zero(T)/one(T))
    A_ldiv_B!(factorize(convert(AbstractMatrix{S}, A)), eye(S, checksquare(A)))
end

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
condskeel(A::AbstractMatrix, x::AbstractVector, p::Real=Inf) = norm(abs(inv(A))*abs(A)*abs(x), p)
condskeel{T<:Integer}(A::AbstractMatrix{T}, x::AbstractVector, p::Real=Inf) = norm(abs(inv(float(A)))*abs(A)*abs(x), p)

function issymmetric(A::AbstractMatrix)
    m, n = size(A)
    if m != n
        return false
    end
    for i = 1:(n-1), j = (i+1):n
        if A[i,j] != transpose(A[j,i])
            return false
        end
    end
    return true
end

issymmetric(x::Number) = true

function ishermitian(A::AbstractMatrix)
    m, n = size(A)
    if m != n
        return false
    end
    for i = 1:n, j = i:n
        if A[i,j] != ctranspose(A[j,i])
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

isdiag(A::AbstractMatrix) = istril(A) && istriu(A)

istriu(x::Number) = true
istril(x::Number) = true
isdiag(x::Number) = true

linreg{T<:Number}(X::AbstractVector{T}, y::AbstractVector{T}) = [ones(T, size(X,1)) X] \ y

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
    n = length(x)
    if n != length(y)
        throw(DimensionMismatch("x has length $n, but y has length $(length(y))"))
    end
    for i = 1:n
        @inbounds y[i] += x[i]*α
    end
    y
end

function axpy!{Ti<:Integer,Tj<:Integer}(α, x::AbstractArray, rx::AbstractArray{Ti}, y::AbstractArray, ry::AbstractArray{Tj})
    if length(rx) != length(ry)
        throw(DimensionMismatch("rx has length $(length(rx)), but ry has length $(length(ry))"))
    elseif minimum(rx) < 1 || maximum(rx) > length(x)
        throw(BoundsError(x, rx))
    elseif minimum(ry) < 1 || maximum(ry) > length(y)
        throw(BoundsError(y, ry))
    end
    for i = 1:length(rx)
        @inbounds y[ry[i]] += x[rx[i]]*α
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

function det{T}(A::AbstractMatrix{T})
    if istriu(A) || istril(A)
        S = typeof((one(T)*zero(T) + zero(T))/one(T))
        return convert(S, det(UpperTriangular(A)))
    end
    return det(lufact(A))
end
det(x::Number) = x

logdet(A::AbstractMatrix) = logdet(lufact(A))
logabsdet(A::AbstractMatrix) = logabsdet(lufact(A))

# isapprox: approximate equality of arrays [like isapprox(Number,Number)]
function isapprox{T<:Number,S<:Number}(x::AbstractArray{T}, y::AbstractArray{S}; rtol::Real=Base.rtoldefault(T,S), atol::Real=0, norm::Function=vecnorm)
    d = norm(x - y)
    return isfinite(d) ? d <= atol + rtol*max(norm(x), norm(y)) : x == y
end

"""
    normalize!(v, [p=2])

Normalize the vector `v` in-place with respect to the `p`-norm.

Inputs:

- `v::AbstractVector` - vector to be normalized
- `p::Real` - The `p`-norm to normalize with respect to. Default: 2

Output:

- `v` - A unit vector being the input vector, rescaled to have norm 1.
        The input vector is modified in-place.

See also:

`normalize`, `qr`

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
    normalize(v, [p=2])

Normalize the vector `v` with respect to the `p`-norm.

Inputs:

- `v::AbstractVector` - vector to be normalized
- `p::Real` - The `p`-norm to normalize with respect to. Default: 2

Output:

- `v` - A unit vector being a copy of the input vector, scaled to have norm 1

See also:

`normalize!`, `qr`
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
