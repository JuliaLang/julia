# This file is a part of Julia. License is MIT: https://julialang.org/license

## linalg.jl: Some generic Linear Algebra definitions

"""
    MulAddMul(alpha, beta)

A callable for operating short-circuiting version of `x * alpha + y * beta`.

# Examples
```jldoctest
julia> using LinearAlgebra: MulAddMul

julia> _add = MulAddMul(1, 0);

julia> _add(123, nothing)
123

julia> MulAddMul(12, 34)(56, 78) == 56 * 12 + 78 * 34
true
```
"""
struct MulAddMul{ais1, bis0, TA, TB}
    alpha::TA
    beta::TB
end

@inline function MulAddMul(alpha::TA, beta::TB) where {TA,TB}
    if isone(alpha)
        if iszero(beta)
            return MulAddMul{true,true,TA,TB}(alpha, beta)
        else
            return MulAddMul{true,false,TA,TB}(alpha, beta)
        end
    else
        if iszero(beta)
            return MulAddMul{false,true,TA,TB}(alpha, beta)
        else
            return MulAddMul{false,false,TA,TB}(alpha, beta)
        end
    end
end

MulAddMul() = MulAddMul{true,true,Bool,Bool}(true, false)

@inline (::MulAddMul{true})(x) = x
@inline (p::MulAddMul{false})(x) = x * p.alpha
@inline (::MulAddMul{true, true})(x, _) = x
@inline (p::MulAddMul{false, true})(x, _) = x * p.alpha
@inline (p::MulAddMul{true, false})(x, y) = x + y * p.beta
@inline (p::MulAddMul{false, false})(x, y) = x * p.alpha + y * p.beta

"""
    _modify!(_add::MulAddMul, x, C, idx)

Short-circuiting version of `C[idx] = _add(x, C[idx])`.

Short-circuiting the indexing `C[idx]` is necessary for avoiding `UndefRefError`
when mutating an array of non-primitive numbers such as `BigFloat`.

# Examples
```jldoctest
julia> using LinearAlgebra: MulAddMul, _modify!

julia> _add = MulAddMul(1, 0);
       C = Vector{BigFloat}(undef, 1);

julia> _modify!(_add, 123, C, 1)

julia> C
1-element Vector{BigFloat}:
 123.0
```
"""
@inline @propagate_inbounds function _modify!(p::MulAddMul{ais1, bis0},
                                              x, C, idx′) where {ais1, bis0}
    # `idx′` may be an integer, a tuple of integer, or a `CartesianIndex`.
    #  Let `CartesianIndex` constructor normalize them so that it can be
    # used uniformly.  It also acts as a workaround for performance penalty
    # of splatting a number (#29114):
    idx = CartesianIndex(idx′)
    if bis0
        C[idx] = p(x)
    else
        C[idx] = p(x, C[idx])
    end
    return
end

@inline function _rmul_or_fill!(C::AbstractArray, beta::Number)
    if isempty(C)
        return C
    end
    if iszero(beta)
        fill!(C, zero(eltype(C)))
    else
        rmul!(C, beta)
    end
    return C
end


function generic_mul!(C::AbstractArray, X::AbstractArray, s::Number, _add::MulAddMul)
    if length(C) != length(X)
        throw(DimensionMismatch("first array has length $(length(C)) which does not match the length of the second, $(length(X))."))
    end
    for (IC, IX) in zip(eachindex(C), eachindex(X))
        @inbounds _modify!(_add, X[IX] * s, C, IC)
    end
    C
end

function generic_mul!(C::AbstractArray, s::Number, X::AbstractArray, _add::MulAddMul)
    if length(C) != length(X)
        throw(DimensionMismatch("first array has length $(length(C)) which does not
match the length of the second, $(length(X))."))
    end
    for (IC, IX) in zip(eachindex(C), eachindex(X))
        @inbounds _modify!(_add, s * X[IX], C, IC)
    end
    C
end

@inline mul!(C::AbstractArray, s::Number, X::AbstractArray, alpha::Number, beta::Number) =
    generic_mul!(C, s, X, MulAddMul(alpha, beta))
@inline mul!(C::AbstractArray, X::AbstractArray, s::Number, alpha::Number, beta::Number) =
    generic_mul!(C, X, s, MulAddMul(alpha, beta))

# For better performance when input and output are the same array
# See https://github.com/JuliaLang/julia/issues/8415#issuecomment-56608729
"""
    rmul!(A::AbstractArray, b::Number)

Scale an array `A` by a scalar `b` overwriting `A` in-place.  Use
[`lmul!`](@ref) to multiply scalar from left.  The scaling operation
respects the semantics of the multiplication [`*`](@ref) between an
element of `A` and `b`.  In particular, this also applies to
multiplication involving non-finite numbers such as `NaN` and `±Inf`.

!!! compat "Julia 1.1"
    Prior to Julia 1.1, `NaN` and `±Inf` entries in `A` were treated
    inconsistently.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> rmul!(A, 2)
2×2 Matrix{Int64}:
 2  4
 6  8

julia> rmul!([NaN], 0.0)
1-element Vector{Float64}:
 NaN
```
"""
function rmul!(X::AbstractArray, s::Number)
    @simd for I in eachindex(X)
        @inbounds X[I] *= s
    end
    X
end


"""
    lmul!(a::Number, B::AbstractArray)

Scale an array `B` by a scalar `a` overwriting `B` in-place.  Use
[`rmul!`](@ref) to multiply scalar from right.  The scaling operation
respects the semantics of the multiplication [`*`](@ref) between `a`
and an element of `B`.  In particular, this also applies to
multiplication involving non-finite numbers such as `NaN` and `±Inf`.

!!! compat "Julia 1.1"
    Prior to Julia 1.1, `NaN` and `±Inf` entries in `B` were treated
    inconsistently.

# Examples
```jldoctest
julia> B = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> lmul!(2, B)
2×2 Matrix{Int64}:
 2  4
 6  8

julia> lmul!(0.0, [Inf])
1-element Vector{Float64}:
 NaN
```
"""
function lmul!(s::Number, X::AbstractArray)
    @simd for I in eachindex(X)
        @inbounds X[I] = s*X[I]
    end
    X
end

"""
    rdiv!(A::AbstractArray, b::Number)

Divide each entry in an array `A` by a scalar `b` overwriting `A`
in-place.  Use [`ldiv!`](@ref) to divide scalar from left.

# Examples
```jldoctest
julia> A = [1.0 2.0; 3.0 4.0]
2×2 Matrix{Float64}:
 1.0  2.0
 3.0  4.0

julia> rdiv!(A, 2.0)
2×2 Matrix{Float64}:
 0.5  1.0
 1.5  2.0
```
"""
function rdiv!(X::AbstractArray, s::Number)
    @simd for I in eachindex(X)
        @inbounds X[I] /= s
    end
    X
end

"""
    ldiv!(a::Number, B::AbstractArray)

Divide each entry in an array `B` by a scalar `a` overwriting `B`
in-place.  Use [`rdiv!`](@ref) to divide scalar from right.

# Examples
```jldoctest
julia> B = [1.0 2.0; 3.0 4.0]
2×2 Matrix{Float64}:
 1.0  2.0
 3.0  4.0

julia> ldiv!(2.0, B)
2×2 Matrix{Float64}:
 0.5  1.0
 1.5  2.0
```
"""
function ldiv!(s::Number, X::AbstractArray)
    @simd for I in eachindex(X)
        @inbounds X[I] = s\X[I]
    end
    X
end
ldiv!(Y::AbstractArray, s::Number, X::AbstractArray) = Y .= s .\ X

# Generic fallback. This assumes that B and Y have the same sizes.
ldiv!(Y::AbstractArray, A::AbstractMatrix, B::AbstractArray) = ldiv!(A, copyto!(Y, B))


"""
    cross(x, y)
    ×(x,y)

Compute the cross product of two 3-vectors.

# Examples
```jldoctest
julia> a = [0;1;0]
3-element Vector{Int64}:
 0
 1
 0

julia> b = [0;0;1]
3-element Vector{Int64}:
 0
 0
 1

julia> cross(a,b)
3-element Vector{Int64}:
 1
 0
 0
```
"""
function cross(a::AbstractVector, b::AbstractVector)
    if !(length(a) == length(b) == 3)
        throw(DimensionMismatch("cross product is only defined for vectors of length 3"))
    end
    a1, a2, a3 = a
    b1, b2, b3 = b
    [a2*b3-a3*b2, a3*b1-a1*b3, a1*b2-a2*b1]
end

"""
    triu(M)

Upper triangle of a matrix.

# Examples
```jldoctest
julia> a = fill(1.0, (4,4))
4×4 Matrix{Float64}:
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0

julia> triu(a)
4×4 Matrix{Float64}:
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

# Examples
```jldoctest
julia> a = fill(1.0, (4,4))
4×4 Matrix{Float64}:
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0

julia> tril(a)
4×4 Matrix{Float64}:
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

# Examples
```jldoctest
julia> a = fill(1.0, (4,4))
4×4 Matrix{Float64}:
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0

julia> triu(a,3)
4×4 Matrix{Float64}:
 0.0  0.0  0.0  1.0
 0.0  0.0  0.0  0.0
 0.0  0.0  0.0  0.0
 0.0  0.0  0.0  0.0

julia> triu(a,-3)
4×4 Matrix{Float64}:
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

# Examples
```jldoctest
julia> a = fill(1.0, (4,4))
4×4 Matrix{Float64}:
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0

julia> tril(a,3)
4×4 Matrix{Float64}:
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0
 1.0  1.0  1.0  1.0

julia> tril(a,-3)
4×4 Matrix{Float64}:
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
See also [`triu`](@ref).
"""
triu!(M::AbstractMatrix) = triu!(M,0)

"""
    tril!(M)

Lower triangle of a matrix, overwriting `M` in the process.
See also [`tril`](@ref).
"""
tril!(M::AbstractMatrix) = tril!(M,0)

diag(A::AbstractVector) = throw(ArgumentError("use diagm instead of diag to construct a diagonal matrix"))

###########################################################################################
# Dot products and norms

# special cases of norm; note that they don't need to handle isempty(x)
function generic_normMinusInf(x)
    (v, s) = iterate(x)::Tuple
    minabs = norm(v)
    while true
        y = iterate(x, s)
        y === nothing && break
        (v, s) = y
        vnorm = norm(v)
        minabs = ifelse(isnan(minabs) | (minabs < vnorm), minabs, vnorm)
    end
    return float(minabs)
end

function generic_normInf(x)
    (v, s) = iterate(x)::Tuple
    maxabs = norm(v)
    while true
        y = iterate(x, s)
        y === nothing && break
        (v, s) = y
        vnorm = norm(v)
        maxabs = ifelse(isnan(maxabs) | (maxabs > vnorm), maxabs, vnorm)
    end
    return float(maxabs)
end

function generic_norm1(x)
    (v, s) = iterate(x)::Tuple
    av = float(norm(v))
    T = typeof(av)
    sum::promote_type(Float64, T) = av
    while true
        y = iterate(x, s)
        y === nothing && break
        (v, s) = y
        sum += norm(v)
    end
    return convert(T, sum)
end

# faster computation of norm(x)^2, avoiding overflow for integers
norm_sqr(x) = norm(x)^2
norm_sqr(x::Number) = abs2(x)
norm_sqr(x::Union{T,Complex{T},Rational{T}}) where {T<:Integer} = abs2(float(x))

function generic_norm2(x)
    maxabs = normInf(x)
    (maxabs == 0 || isinf(maxabs)) && return maxabs
    (v, s) = iterate(x)::Tuple
    T = typeof(maxabs)
    if isfinite(length(x)*maxabs*maxabs) && maxabs*maxabs != 0 # Scaling not necessary
        sum::promote_type(Float64, T) = norm_sqr(v)
        while true
            y = iterate(x, s)
            y === nothing && break
            (v, s) = y
            sum += norm_sqr(v)
        end
        return convert(T, sqrt(sum))
    else
        sum = abs2(norm(v)/maxabs)
        while true
            y = iterate(x, s)
            y === nothing && break
            (v, s) = y
            sum += (norm(v)/maxabs)^2
        end
        return convert(T, maxabs*sqrt(sum))
    end
end

# Compute L_p norm ‖x‖ₚ = sum(abs(x).^p)^(1/p)
# (Not technically a "norm" for p < 1.)
function generic_normp(x, p)
    (v, s) = iterate(x)::Tuple
    if p > 1 || p < -1 # might need to rescale to avoid overflow
        maxabs = p > 1 ? normInf(x) : normMinusInf(x)
        (maxabs == 0 || isinf(maxabs)) && return maxabs
        T = typeof(maxabs)
    else
        T = typeof(float(norm(v)))
    end
    spp::promote_type(Float64, T) = p
    if -1 <= p <= 1 || (isfinite(length(x)*maxabs^spp) && maxabs^spp != 0) # scaling not necessary
        sum::promote_type(Float64, T) = norm(v)^spp
        while true
            y = iterate(x, s)
            y === nothing && break
            (v, s) = y
            sum += norm(v)^spp
        end
        return convert(T, sum^inv(spp))
    else # rescaling
        sum = (norm(v)/maxabs)^spp
        while true
            y = iterate(x, s)
            y === nothing && break
            (v, s) = y
            sum += (norm(v)/maxabs)^spp
        end
        return convert(T, maxabs*sum^inv(spp))
    end
end


normMinusInf(x) = generic_normMinusInf(x)
normInf(x) = generic_normInf(x)
norm1(x) = generic_norm1(x)
norm2(x) = generic_norm2(x)
normp(x, p) = generic_normp(x, p)
function norm0(itr)
    T = typeofnorm(itr)
    return T(count(!iszero, itr))
end

@inline strip_val(p) = p
@inline strip_val(::Val{p}) where {p} = p
@inline abspow(x, p) = norm(x)^p
@inline abspow(x, ::Val) = abspow(x, strip_val(p))
@inline abspow(x, ::Val{2}) = norm_sqr(x)
invpow(x, p) = x^(1/strip_val(p))

_issubnormal(x) = false
_issubnormal(x::Union{Float16, Float32, Float64}) = issubnormal(x)
function typeofnorm(arr)
    return typeof(float(norm(first(arr))))
end
function abstract_array_normp(arr, p)
    T = typeofnorm(arr)
    psum = zero(T)/oneunit(T) # accumulates Σ(x/normcrude)^p
    # normcrude is a crude approximation of norm(arr, p) used to
    # scale down to prevent overflows
    normcrude = if strip_val(p) < 0
        normMinusInf(arr)
    else
        normInf(arr)
    end
    if isinf(normcrude) || iszero(normcrude)
        return T(normcrude)::T
    end
    if _issubnormal(normcrude)
        @simd for x in arr
            # oneunit(T)/normcrude will overflow
            # so we need to do the division in every iteration
            # for subnormal normcrude
            xp = abspow(x/normcrude, p)
            psum += xp
        end
    else
        invnormcrude = one(T)/normcrude
        @simd for x in arr
            xp = abspow(x*invnormcrude, p)
            psum += xp
        end
    end
    return T(invpow(psum, p) * normcrude)::T
end

norm1(  arr::AbstractArray) = typeofnorm(arr)(sum(norm, arr))
norm2(  arr::AbstractArray) = abstract_array_normp(arr, Val(2))
normp(  arr::AbstractArray) = abstract_array_normp(arr, p)
normInf(arr::AbstractArray) = typeofnorm(arr)(maximum(norm, arr))
normMinusInf(arr::AbstractArray) = typeofnorm(arr)(generic_normMinusInf(arr))

"""
    norm(A, p::Real=2)

For any iterable container `A` (including arrays of any dimension) of numbers (or any
element type for which `norm` is defined), compute the `p`-norm (defaulting to `p=2`) as if
`A` were a vector of the corresponding length.

The `p`-norm is defined as
```math
\\|A\\|_p = \\left( \\sum_{i=1}^n | a_i | ^p \\right)^{1/p}
```
with ``a_i`` the entries of ``A``, ``| a_i |`` the [`norm`](@ref) of ``a_i``, and
``n`` the length of ``A``. Since the `p`-norm is computed using the [`norm`](@ref)s
of the entries of `A`, the `p`-norm of a vector of vectors is not compatible with
the interpretation of it as a block vector in general if `p != 2`.

`p` can assume any numeric value (even though not all values produce a
mathematically valid vector norm). In particular, `norm(A, Inf)` returns the largest value
in `abs.(A)`, whereas `norm(A, -Inf)` returns the smallest. If `A` is a matrix and `p=2`,
then this is equivalent to the Frobenius norm.

The second argument `p` is not necessarily a part of the interface for `norm`, i.e. a custom
type may only implement `norm(A)` without second argument.

Use [`opnorm`](@ref) to compute the operator norm of a matrix.

# Examples
```jldoctest
julia> v = [3, -2, 6]
3-element Vector{Int64}:
  3
 -2
  6

julia> norm(v)
7.0

julia> norm(v, 1)
11.0

julia> norm(v, Inf)
6.0

julia> norm([1 2 3; 4 5 6; 7 8 9]) ≈ sqrt(sum(abs2, 1:9))
true

julia> norm([1 2 3 4 5 6 7 8 9]) ≈ sqrt(sum(abs2, 1:9))
true

julia> norm(1:9) ≈ sqrt(sum(abs2, 1:9))
true

julia> norm(hcat(v,v), 1) == norm(vcat(v,v), 1)
true

julia> !(norm(vcat(v,v), 1) ≈ norm([v,v], 1))
true

julia> norm(hcat(v,v), 2) ≈ norm(vcat(v,v), 2) ≈ norm([v,v], 2)
true

julia> norm(hcat(v,v), Inf) == norm(vcat(v,v), Inf)
true

julia> !(norm(vcat(v,v), Inf) ≈ norm([v,v], Inf))
true
```
"""
function norm(itr, p::Real=2)
    isempty(itr) && return float(norm(zero(eltype(itr))))
    if p == 2
        return norm2(itr)
    elseif p == 1
        return norm1(itr)
    elseif p == Inf
        return normInf(itr)
    elseif p == 0
        return norm0(itr)
    elseif p == -Inf
        return normMinusInf(itr)
    else
        normp(itr, p)
    end
end


"""
    norm(x::Number, p::Real=2)

For numbers, return ``\\left( |x|^p \\right)^{1/p}``.

# Examples
```jldoctest
julia> norm(2, 1)
2.0

julia> norm(-2, 1)
2.0

julia> norm(2, 2)
2.0

julia> norm(-2, 2)
2.0

julia> norm(2, Inf)
2.0

julia> norm(-2, Inf)
2.0
```
"""
@inline function norm(x::Number, p::Real=2)
    afx = abs(float(x))
    if p == 0
        if x == 0
            return zero(afx)
        elseif !isnan(x)
            return oneunit(afx)
        else
            return afx
        end
    else
        return afx
    end
end
norm(::Missing, p::Real=2) = missing

# special cases of opnorm
function opnorm1(A::AbstractMatrix{T}) where T
    require_one_based_indexing(A)
    m, n = size(A)
    Tnorm = typeof(float(real(zero(T))))
    Tsum = promote_type(Float64, Tnorm)
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

function opnorm2(A::AbstractMatrix{T}) where T
    require_one_based_indexing(A)
    m,n = size(A)
    if m == 1 || n == 1 return norm2(A) end
    Tnorm = typeof(float(real(zero(T))))
    (m == 0 || n == 0) ? zero(Tnorm) : convert(Tnorm, svdvals(A)[1])
end

function opnormInf(A::AbstractMatrix{T}) where T
    require_one_based_indexing(A)
    m,n = size(A)
    Tnorm = typeof(float(real(zero(T))))
    Tsum = promote_type(Float64, Tnorm)
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
    opnorm(A::AbstractMatrix, p::Real=2)

Compute the operator norm (or matrix norm) induced by the vector `p`-norm,
where valid values of `p` are `1`, `2`, or `Inf`. (Note that for sparse matrices,
`p=2` is currently not implemented.) Use [`norm`](@ref) to compute the Frobenius
norm.

When `p=1`, the operator norm is the maximum absolute column sum of `A`:
```math
\\|A\\|_1 = \\max_{1 ≤ j ≤ n} \\sum_{i=1}^m | a_{ij} |
```
with ``a_{ij}`` the entries of ``A``, and ``m`` and ``n`` its dimensions.

When `p=2`, the operator norm is the spectral norm, equal to the largest
singular value of `A`.

When `p=Inf`, the operator norm is the maximum absolute row sum of `A`:
```math
\\|A\\|_\\infty = \\max_{1 ≤ i ≤ m} \\sum _{j=1}^n | a_{ij} |
```

# Examples
```jldoctest
julia> A = [1 -2 -3; 2 3 -1]
2×3 Matrix{Int64}:
 1  -2  -3
 2   3  -1

julia> opnorm(A, Inf)
6.0

julia> opnorm(A, 1)
5.0
```
"""
function opnorm(A::AbstractMatrix, p::Real=2)
    if p == 2
        return opnorm2(A)
    elseif p == 1
        return opnorm1(A)
    elseif p == Inf
        return opnormInf(A)
    else
        throw(ArgumentError("invalid p-norm p=$p. Valid: 1, 2, Inf"))
    end
end

"""
    opnorm(x::Number, p::Real=2)

For numbers, return ``\\left( |x|^p \\right)^{1/p}``.
This is equivalent to [`norm`](@ref).
"""
@inline opnorm(x::Number, p::Real=2) = norm(x, p)

"""
    opnorm(A::Adjoint{<:Any,<:AbstracVector}, q::Real=2)
    opnorm(A::Transpose{<:Any,<:AbstracVector}, q::Real=2)

For Adjoint/Transpose-wrapped vectors, return the operator ``q``-norm of `A`, which is
equivalent to the `p`-norm with value `p = q/(q-1)`. They coincide at `p = q = 2`.
Use [`norm`](@ref) to compute the `p` norm of `A` as a vector.

The difference in norm between a vector space and its dual arises to preserve
the relationship between duality and the dot product, and the result is
consistent with the operator `p`-norm of a `1 × n` matrix.

# Examples
```jldoctest
julia> v = [1; im];

julia> vc = v';

julia> opnorm(vc, 1)
1.0

julia> norm(vc, 1)
2.0

julia> norm(v, 1)
2.0

julia> opnorm(vc, 2)
1.4142135623730951

julia> norm(vc, 2)
1.4142135623730951

julia> norm(v, 2)
1.4142135623730951

julia> opnorm(vc, Inf)
2.0

julia> norm(vc, Inf)
1.0

julia> norm(v, Inf)
1.0
```
"""
opnorm(v::TransposeAbsVec, q::Real) = q == Inf ? norm(v.parent, 1) : norm(v.parent, q/(q-1))
opnorm(v::AdjointAbsVec, q::Real) = q == Inf ? norm(conj(v.parent), 1) : norm(conj(v.parent), q/(q-1))
opnorm(v::AdjointAbsVec) = norm(conj(v.parent))
opnorm(v::TransposeAbsVec) = norm(v.parent)

norm(v::Union{TransposeAbsVec,AdjointAbsVec}, p::Real) = norm(v.parent, p)

"""
    dot(x, y)
    x ⋅ y

Compute the dot product between two vectors. For complex vectors, the first
vector is conjugated.

`dot` also works on arbitrary iterable objects, including arrays of any dimension,
as long as `dot` is defined on the elements.

`dot` is semantically equivalent to `sum(dot(vx,vy) for (vx,vy) in zip(x, y))`,
with the added restriction that the arguments must have equal lengths.

`x ⋅ y` (where `⋅` can be typed by tab-completing `\\cdot` in the REPL) is a synonym for
`dot(x, y)`.

# Examples
```jldoctest
julia> dot([1; 1], [2; 3])
5

julia> dot([im; im], [1; 1])
0 - 2im

julia> dot(1:5, 2:6)
70

julia> x = fill(2., (5,5));

julia> y = fill(3., (5,5));

julia> dot(x, y)
150.0
```
"""
function dot end

function dot(x, y) # arbitrary iterables
    ix = iterate(x)
    iy = iterate(y)
    if ix === nothing
        if iy !== nothing
            throw(DimensionMismatch("x and y are of different lengths!"))
        end
        return dot(zero(eltype(x)), zero(eltype(y)))
    end
    if iy === nothing
        throw(DimensionMismatch("x and y are of different lengths!"))
    end
    (vx, xs) = ix
    (vy, ys) = iy
    s = dot(vx, vy)
    while true
        ix = iterate(x, xs)
        iy = iterate(y, ys)
        ix === nothing && break
        iy === nothing && break
        (vx, xs), (vy, ys) = ix, iy
        s += dot(vx, vy)
    end
    if !(iy === nothing && ix === nothing)
        throw(DimensionMismatch("x and y are of different lengths!"))
    end
    return s
end

dot(x::Number, y::Number) = conj(x) * y

function dot(x::AbstractArray, y::AbstractArray)
    lx = length(x)
    if lx != length(y)
        throw(DimensionMismatch("first array has length $(lx) which does not match the length of the second, $(length(y))."))
    end
    if lx == 0
        return dot(zero(eltype(x)), zero(eltype(y)))
    end
    s = zero(dot(first(x), first(y)))
    for (Ix, Iy) in zip(eachindex(x), eachindex(y))
        @inbounds s += dot(x[Ix], y[Iy])
    end
    s
end

"""
    dot(x, A, y)

Compute the generalized dot product `dot(x, A*y)` between two vectors `x` and `y`,
without storing the intermediate result of `A*y`. As for the two-argument
[`dot(_,_)`](@ref), this acts recursively. Moreover, for complex vectors, the
first vector is conjugated.

!!! compat "Julia 1.4"
    Three-argument `dot` requires at least Julia 1.4.

# Examples
```jldoctest
julia> dot([1; 1], [1 2; 3 4], [2; 3])
26

julia> dot(1:5, reshape(1:25, 5, 5), 2:6)
4850

julia> ⋅(1:5, reshape(1:25, 5, 5), 2:6) == dot(1:5, reshape(1:25, 5, 5), 2:6)
true
```
"""
dot(x, A, y) = dot(x, A*y) # generic fallback for cases that are not covered by specialized methods

function dot(x::AbstractVector, A::AbstractMatrix, y::AbstractVector)
    (axes(x)..., axes(y)...) == axes(A) || throw(DimensionMismatch())
    T = typeof(dot(first(x), first(A), first(y)))
    s = zero(T)
    i₁ = first(eachindex(x))
    x₁ = first(x)
    @inbounds for j in eachindex(y)
        yj = y[j]
        if !iszero(yj)
            temp = zero(adjoint(A[i₁,j]) * x₁)
            @simd for i in eachindex(x)
                temp += adjoint(A[i,j]) * x[i]
            end
            s += dot(temp, yj)
        end
    end
    return s
end
dot(x::AbstractVector, adjA::Adjoint, y::AbstractVector) = adjoint(dot(y, adjA.parent, x))
dot(x::AbstractVector, transA::Transpose{<:Real}, y::AbstractVector) = adjoint(dot(y, transA.parent, x))

###########################################################################################

"""
    rank(A::AbstractMatrix; atol::Real=0, rtol::Real=atol>0 ? 0 : n*ϵ)
    rank(A::AbstractMatrix, rtol::Real)

Compute the rank of a matrix by counting how many singular
values of `A` have magnitude greater than `max(atol, rtol*σ₁)` where `σ₁` is
`A`'s largest singular value. `atol` and `rtol` are the absolute and relative
tolerances, respectively. The default relative tolerance is `n*ϵ`, where `n`
is the size of the smallest dimension of `A`, and `ϵ` is the [`eps`](@ref) of
the element type of `A`.

!!! compat "Julia 1.1"
    The `atol` and `rtol` keyword arguments requires at least Julia 1.1.
    In Julia 1.0 `rtol` is available as a positional argument, but this
    will be deprecated in Julia 2.0.

# Examples
```jldoctest
julia> rank(Matrix(I, 3, 3))
3

julia> rank(diagm(0 => [1, 0, 2]))
2

julia> rank(diagm(0 => [1, 0.001, 2]), rtol=0.1)
2

julia> rank(diagm(0 => [1, 0.001, 2]), rtol=0.00001)
3

julia> rank(diagm(0 => [1, 0.001, 2]), atol=1.5)
1
```
"""
function rank(A::AbstractMatrix; atol::Real = 0.0, rtol::Real = (min(size(A)...)*eps(real(float(one(eltype(A))))))*iszero(atol))
    isempty(A) && return 0 # 0-dimensional case
    s = svdvals(A)
    tol = max(atol, rtol*s[1])
    count(x -> x > tol, s)
end
rank(x::Number) = x == 0 ? 0 : 1

"""
    tr(M)

Matrix trace. Sums the diagonal elements of `M`.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> tr(A)
5
```
"""
function tr(A::AbstractMatrix)
    checksquare(A)
    sum(diag(A))
end
tr(x::Number) = x

#kron(a::AbstractVector, b::AbstractVector)
#kron(a::AbstractMatrix{T}, b::AbstractMatrix{S}) where {T,S}

#det(a::AbstractMatrix)

"""
    inv(M)

Matrix inverse. Computes matrix `N` such that
`M * N = I`, where `I` is the identity matrix.
Computed by solving the left-division
`N = M \\ I`.

# Examples
```jldoctest
julia> M = [2 5; 1 3]
2×2 Matrix{Int64}:
 2  5
 1  3

julia> N = inv(M)
2×2 Matrix{Float64}:
  3.0  -5.0
 -1.0   2.0

julia> M*N == N*M == Matrix(I, 2, 2)
true
```
"""
function inv(A::AbstractMatrix{T}) where T
    n = checksquare(A)
    S = typeof(zero(T)/one(T))      # dimensionful
    S0 = typeof(zero(T)/oneunit(T)) # dimensionless
    dest = Matrix{S0}(I, n, n)
    ldiv!(factorize(convert(AbstractMatrix{S}, A)), dest)
end
inv(A::Adjoint) = adjoint(inv(parent(A)))
inv(A::Transpose) = transpose(inv(parent(A)))

pinv(v::AbstractVector{T}, tol::Real = real(zero(T))) where {T<:Real} = _vectorpinv(transpose, v, tol)
pinv(v::AbstractVector{T}, tol::Real = real(zero(T))) where {T<:Complex} = _vectorpinv(adjoint, v, tol)
pinv(v::AbstractVector{T}, tol::Real = real(zero(T))) where {T} = _vectorpinv(adjoint, v, tol)
function _vectorpinv(dualfn::Tf, v::AbstractVector{Tv}, tol) where {Tv,Tf}
    res = dualfn(similar(v, typeof(zero(Tv) / (abs2(one(Tv)) + abs2(one(Tv))))))
    den = sum(abs2, v)
    # as tol is the threshold relative to the maximum singular value, for a vector with
    # single singular value σ=√den, σ ≦ tol*σ is equivalent to den=0 ∨ tol≥1
    if iszero(den) || tol >= one(tol)
        fill!(res, zero(eltype(res)))
    else
        res .= dualfn(v) ./ den
    end
    return res
end

# this method is just an optimization: literal negative powers of A are
# already turned by literal_pow into powers of inv(A), but for A^-1 this
# would turn into inv(A)^1 = copy(inv(A)), which makes an extra copy.
@inline Base.literal_pow(::typeof(^), A::AbstractMatrix, ::Val{-1}) = inv(A)

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

# Examples
```jldoctest
julia> A = [1 0; 1 -2]; B = [32; -4];

julia> X = A \\ B
2-element Vector{Float64}:
 32.0
 18.0

julia> A * X == B
true
```
"""
function (\)(A::AbstractMatrix, B::AbstractVecOrMat)
    require_one_based_indexing(A, B)
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
        return lu(A) \ B
    end
    return qr(A,Val(true)) \ B
end

(\)(a::AbstractVector, b::AbstractArray) = pinv(a) * b
function (/)(A::AbstractVecOrMat, B::AbstractVecOrMat)
    size(A,2) != size(B,2) && throw(DimensionMismatch("Both inputs should have the same number of columns"))
    return copy(adjoint(adjoint(B) \ adjoint(A)))
end
# \(A::StridedMatrix,x::Number) = inv(A)*x Should be added at some point when the old elementwise version has been deprecated long enough
# /(x::Number,A::StridedMatrix) = x*inv(A)
/(x::Number, v::AbstractVector) = x*pinv(v)

cond(x::Number) = x == 0 ? Inf : 1.0
cond(x::Number, p) = cond(x)

#Skeel condition numbers
condskeel(A::AbstractMatrix, p::Real=Inf) = opnorm(abs.(inv(A))*abs.(A), p)

"""
    condskeel(M, [x, p::Real=Inf])

```math
\\kappa_S(M, p) = \\left\\Vert \\left\\vert M \\right\\vert \\left\\vert M^{-1} \\right\\vert \\right\\Vert_p \\\\
\\kappa_S(M, x, p) = \\frac{\\left\\Vert \\left\\vert M \\right\\vert \\left\\vert M^{-1} \\right\\vert \\left\\vert x \\right\\vert \\right\\Vert_p}{\\left \\Vert x \\right \\Vert_p}
```

Skeel condition number ``\\kappa_S`` of the matrix `M`, optionally with respect to the
vector `x`, as computed using the operator `p`-norm. ``\\left\\vert M \\right\\vert``
denotes the matrix of (entry wise) absolute values of ``M``;
``\\left\\vert M \\right\\vert_{ij} = \\left\\vert M_{ij} \\right\\vert``.
Valid values for `p` are `1`, `2` and `Inf` (default).

This quantity is also known in the literature as the Bauer condition number, relative
condition number, or componentwise relative condition number.
"""
function condskeel(A::AbstractMatrix, x::AbstractVector, p::Real=Inf)
    norm(abs.(inv(A))*(abs.(A)*abs.(x)), p) / norm(x, p)
end

issymmetric(A::AbstractMatrix{<:Real}) = ishermitian(A)

"""
    issymmetric(A) -> Bool

Test whether a matrix is symmetric.

# Examples
```jldoctest
julia> a = [1 2; 2 -1]
2×2 Matrix{Int64}:
 1   2
 2  -1

julia> issymmetric(a)
true

julia> b = [1 im; -im 1]
2×2 Matrix{Complex{Int64}}:
 1+0im  0+1im
 0-1im  1+0im

julia> issymmetric(b)
false
```
"""
function issymmetric(A::AbstractMatrix)
    indsm, indsn = axes(A)
    if indsm != indsn
        return false
    end
    for i = first(indsn):last(indsn), j = (i):last(indsn)
        if A[i,j] != transpose(A[j,i])
            return false
        end
    end
    return true
end

issymmetric(x::Number) = x == x

"""
    ishermitian(A) -> Bool

Test whether a matrix is Hermitian.

# Examples
```jldoctest
julia> a = [1 2; 2 -1]
2×2 Matrix{Int64}:
 1   2
 2  -1

julia> ishermitian(a)
true

julia> b = [1 im; -im 1]
2×2 Matrix{Complex{Int64}}:
 1+0im  0+1im
 0-1im  1+0im

julia> ishermitian(b)
true
```
"""
function ishermitian(A::AbstractMatrix)
    indsm, indsn = axes(A)
    if indsm != indsn
        return false
    end
    for i = indsn, j = i:last(indsn)
        if A[i,j] != adjoint(A[j,i])
            return false
        end
    end
    return true
end

ishermitian(x::Number) = (x == conj(x))

"""
    istriu(A::AbstractMatrix, k::Integer = 0) -> Bool

Test whether `A` is upper triangular starting from the `k`th superdiagonal.

# Examples
```jldoctest
julia> a = [1 2; 2 -1]
2×2 Matrix{Int64}:
 1   2
 2  -1

julia> istriu(a)
false

julia> istriu(a, -1)
true

julia> b = [1 im; 0 -1]
2×2 Matrix{Complex{Int64}}:
 1+0im   0+1im
 0+0im  -1+0im

julia> istriu(b)
true

julia> istriu(b, 1)
false
```
"""
function istriu(A::AbstractMatrix, k::Integer = 0)
    require_one_based_indexing(A)
    m, n = size(A)
    for j in 1:min(n, m + k - 1)
        for i in max(1, j - k + 1):m
            iszero(A[i, j]) || return false
        end
    end
    return true
end
istriu(x::Number) = true

"""
    istril(A::AbstractMatrix, k::Integer = 0) -> Bool

Test whether `A` is lower triangular starting from the `k`th superdiagonal.

# Examples
```jldoctest
julia> a = [1 2; 2 -1]
2×2 Matrix{Int64}:
 1   2
 2  -1

julia> istril(a)
false

julia> istril(a, 1)
true

julia> b = [1 0; -im -1]
2×2 Matrix{Complex{Int64}}:
 1+0im   0+0im
 0-1im  -1+0im

julia> istril(b)
true

julia> istril(b, -1)
false
```
"""
function istril(A::AbstractMatrix, k::Integer = 0)
    require_one_based_indexing(A)
    m, n = size(A)
    for j in max(1, k + 2):n
        for i in 1:min(j - k - 1, m)
            iszero(A[i, j]) || return false
        end
    end
    return true
end
istril(x::Number) = true

"""
    isbanded(A::AbstractMatrix, kl::Integer, ku::Integer) -> Bool

Test whether `A` is banded with lower bandwidth starting from the `kl`th superdiagonal
and upper bandwidth extending through the `ku`th superdiagonal.

# Examples
```jldoctest
julia> a = [1 2; 2 -1]
2×2 Matrix{Int64}:
 1   2
 2  -1

julia> LinearAlgebra.isbanded(a, 0, 0)
false

julia> LinearAlgebra.isbanded(a, -1, 1)
true

julia> b = [1 0; -im -1] # lower bidiagonal
2×2 Matrix{Complex{Int64}}:
 1+0im   0+0im
 0-1im  -1+0im

julia> LinearAlgebra.isbanded(b, 0, 0)
false

julia> LinearAlgebra.isbanded(b, -1, 0)
true
```
"""
isbanded(A::AbstractMatrix, kl::Integer, ku::Integer) = istriu(A, kl) && istril(A, ku)

"""
    isdiag(A) -> Bool

Test whether a matrix is diagonal.

# Examples
```jldoctest
julia> a = [1 2; 2 -1]
2×2 Matrix{Int64}:
 1   2
 2  -1

julia> isdiag(a)
false

julia> b = [im 0; 0 -im]
2×2 Matrix{Complex{Int64}}:
 0+1im  0+0im
 0+0im  0-1im

julia> isdiag(b)
true
```
"""
isdiag(A::AbstractMatrix) = isbanded(A, 0, 0)
isdiag(x::Number) = true


# BLAS-like in-place y = x*α+y function (see also the version in blas.jl
#                                          for BlasFloat Arrays)
function axpy!(α, x::AbstractArray, y::AbstractArray)
    n = length(x)
    if n != length(y)
        throw(DimensionMismatch("x has length $n, but y has length $(length(y))"))
    end
    for (IY, IX) in zip(eachindex(y), eachindex(x))
        @inbounds y[IY] += x[IX]*α
    end
    y
end

function axpy!(α, x::AbstractArray, rx::AbstractArray{<:Integer}, y::AbstractArray, ry::AbstractArray{<:Integer})
    if length(rx) != length(ry)
        throw(DimensionMismatch("rx has length $(length(rx)), but ry has length $(length(ry))"))
    elseif !checkindex(Bool, eachindex(IndexLinear(), x), rx)
        throw(BoundsError(x, rx))
    elseif !checkindex(Bool, eachindex(IndexLinear(), y), ry)
        throw(BoundsError(y, ry))
    end
    for (IY, IX) in zip(eachindex(ry), eachindex(rx))
        @inbounds y[ry[IY]] += x[rx[IX]]*α
    end
    y
end

function axpby!(α, x::AbstractArray, β, y::AbstractArray)
    if length(x) != length(y)
        throw(DimensionMismatch("x has length $(length(x)), but y has length $(length(y))"))
    end
    for (IX, IY) in zip(eachindex(x), eachindex(y))
        @inbounds y[IY] = x[IX]*α + y[IY]*β
    end
    y
end

"""
    rotate!(x, y, c, s)

Overwrite `x` with `c*x + s*y` and `y` with `-conj(s)*x + c*y`.
Returns `x` and `y`.

!!! compat "Julia 1.5"
    `rotate!` requires at least Julia 1.5.
"""
function rotate!(x::AbstractVector, y::AbstractVector, c, s)
    require_one_based_indexing(x, y)
    n = length(x)
    if n != length(y)
        throw(DimensionMismatch("x has length $(length(x)), but y has length $(length(y))"))
    end
    @inbounds for i = 1:n
        xi, yi = x[i], y[i]
        x[i] =       c *xi + s*yi
        y[i] = -conj(s)*xi + c*yi
    end
    return x, y
end

"""
    reflect!(x, y, c, s)

Overwrite `x` with `c*x + s*y` and `y` with `conj(s)*x - c*y`.
Returns `x` and `y`.

!!! compat "Julia 1.5"
    `reflect!` requires at least Julia 1.5.
"""
function reflect!(x::AbstractVector, y::AbstractVector, c, s)
    require_one_based_indexing(x, y)
    n = length(x)
    if n != length(y)
        throw(DimensionMismatch("x has length $(length(x)), but y has length $(length(y))"))
    end
    @inbounds for i = 1:n
        xi, yi = x[i], y[i]
        x[i] =      c *xi + s*yi
        y[i] = conj(s)*xi - c*yi
    end
    return x, y
end

# Elementary reflection similar to LAPACK. The reflector is not Hermitian but
# ensures that tridiagonalization of Hermitian matrices become real. See lawn72
@inline function reflector!(x::AbstractVector)
    require_one_based_indexing(x)
    n = length(x)
    @inbounds begin
        ξ1 = x[1]
        normu = abs2(ξ1)
        for i = 2:n
            normu += abs2(x[i])
        end
        if iszero(normu)
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

# apply reflector from left
@inline function reflectorApply!(x::AbstractVector, τ::Number, A::StridedMatrix)
    require_one_based_indexing(x)
    m, n = size(A)
    if length(x) != m
        throw(DimensionMismatch("reflector has length $(length(x)), which must match the first dimension of matrix A, $m"))
    end
    @inbounds begin
        for j = 1:n
            # dot
            vAj = A[1, j]
            for i = 2:m
                vAj += x[i]'*A[i, j]
            end

            vAj = conj(τ)*vAj

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

# Examples
```jldoctest
julia> M = [1 0; 2 2]
2×2 Matrix{Int64}:
 1  0
 2  2

julia> det(M)
2.0
```
"""
function det(A::AbstractMatrix{T}) where T
    if istriu(A) || istril(A)
        S = typeof((one(T)*zero(T) + zero(T))/one(T))
        return convert(S, det(UpperTriangular(A)))
    end
    return det(lu(A; check = false))
end
det(x::Number) = x

"""
    logabsdet(M)

Log of absolute value of matrix determinant. Equivalent to
`(log(abs(det(M))), sign(det(M)))`, but may provide increased accuracy and/or speed.

# Examples
```jldoctest
julia> A = [-1. 0.; 0. 1.]
2×2 Matrix{Float64}:
 -1.0  0.0
  0.0  1.0

julia> det(A)
-1.0

julia> logabsdet(A)
(0.0, -1.0)

julia> B = [2. 0.; 0. 1.]
2×2 Matrix{Float64}:
 2.0  0.0
 0.0  1.0

julia> det(B)
2.0

julia> logabsdet(B)
(0.6931471805599453, 1.0)
```
"""
logabsdet(A::AbstractMatrix) = logabsdet(lu(A, check=false))

"""
    logdet(M)

Log of matrix determinant. Equivalent to `log(det(M))`, but may provide
increased accuracy and/or speed.

# Examples
```jldoctest
julia> M = [1 0; 2 2]
2×2 Matrix{Int64}:
 1  0
 2  2

julia> logdet(M)
0.6931471805599453

julia> logdet(Matrix(I, 3, 3))
0.0
```
"""
function logdet(A::AbstractMatrix)
    d,s = logabsdet(A)
    return d + log(s)
end

logdet(A) = log(det(A))

const NumberArray{T<:Number} = AbstractArray{T}

"""
    promote_leaf_eltypes(itr)

For an (possibly nested) iterable object `itr`, promote the types of leaf
elements.  Equivalent to `promote_type(typeof(leaf1), typeof(leaf2), ...)`.
Currently supports only numeric leaf elements.

# Examples
```jldoctest
julia> a = [[1,2, [3,4]], 5.0, [6im, [7.0, 8.0]]]
3-element Vector{Any}:
  Any[1, 2, [3, 4]]
 5.0
  Any[0 + 6im, [7.0, 8.0]]

julia> LinearAlgebra.promote_leaf_eltypes(a)
ComplexF64 = Complex{Float64}
```
"""
promote_leaf_eltypes(x::Union{AbstractArray{T},Tuple{T,Vararg{T}}}) where {T<:Number} = T
promote_leaf_eltypes(x::Union{AbstractArray{T},Tuple{T,Vararg{T}}}) where {T<:NumberArray} = eltype(T)
promote_leaf_eltypes(x::T) where {T} = T
promote_leaf_eltypes(x::Union{AbstractArray,Tuple}) = mapreduce(promote_leaf_eltypes, promote_type, x; init=Bool)

# isapprox: approximate equality of arrays [like isapprox(Number,Number)]
# Supports nested arrays; e.g., for `a = [[1,2, [3,4]], 5.0, [6im, [7.0, 8.0]]]`
# `a ≈ a` is `true`.
function isapprox(x::AbstractArray, y::AbstractArray;
    atol::Real=0,
    rtol::Real=Base.rtoldefault(promote_leaf_eltypes(x),promote_leaf_eltypes(y),atol),
    nans::Bool=false, norm::Function=norm)
    d = norm(x - y)
    if isfinite(d)
        return d <= max(atol, rtol*max(norm(x), norm(y)))
    else
        # Fall back to a component-wise approximate comparison
        return all(ab -> isapprox(ab[1], ab[2]; rtol=rtol, atol=atol, nans=nans), zip(x, y))
    end
end

"""
    normalize!(a::AbstractArray, p::Real=2)

Normalize the array `a` in-place so that its `p`-norm equals unity,
i.e. `norm(a, p) == 1`.
See also [`normalize`](@ref) and [`norm`](@ref).
"""
function normalize!(a::AbstractArray, p::Real=2)
    nrm = norm(a, p)
    __normalize!(a, nrm)
end

@inline function __normalize!(a::AbstractArray, nrm::AbstractFloat)
    # The largest positive floating point number whose inverse is less than infinity
    δ = inv(prevfloat(typemax(nrm)))

    if nrm ≥ δ # Safe to multiply with inverse
        invnrm = inv(nrm)
        rmul!(a, invnrm)

    else # scale elements to avoid overflow
        εδ = eps(one(nrm))/δ
        rmul!(a, εδ)
        rmul!(a, inv(nrm*εδ))
    end

    a
end

"""
    normalize(a::AbstractArray, p::Real=2)

Normalize the array `a` so that its `p`-norm equals unity,
i.e. `norm(a, p) == 1`.
See also [`normalize!`](@ref) and [`norm`](@ref).

# Examples
```jldoctest
julia> a = [1,2,4];

julia> b = normalize(a)
3-element Vector{Float64}:
 0.2182178902359924
 0.4364357804719848
 0.8728715609439696

julia> norm(b)
1.0

julia> c = normalize(a, 1)
3-element Vector{Float64}:
 0.14285714285714285
 0.2857142857142857
 0.5714285714285714

julia> norm(c, 1)
1.0

julia> a = [1 2 4 ; 1 2 4]
2×3 Matrix{Int64}:
 1  2  4
 1  2  4

julia> norm(a)
6.48074069840786

julia> normalize(a)
2×3 Matrix{Float64}:
 0.154303  0.308607  0.617213
 0.154303  0.308607  0.617213

```
"""
function normalize(a::AbstractArray, p::Real = 2)
    nrm = norm(a, p)
    if !isempty(a)
        aa = copy_oftype(a, typeof(first(a)/nrm))
        return __normalize!(aa, nrm)
    else
        T = typeof(zero(eltype(a))/nrm)
        return T[]
    end
end
