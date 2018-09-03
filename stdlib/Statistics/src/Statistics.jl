# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Statistics

Standard library module for basic statistics functionality.
"""
module Statistics

using LinearAlgebra, SparseArrays

using Base: has_offset_axes

export cor, cov, std, stdm, var, varm, mean!, mean,
    median!, median, middle, quantile!, quantile

##### mean #####

"""
    mean(itr)

Compute the mean of all elements in a collection.

!!! note
    If `itr` contains `NaN` or [`missing`](@ref) values, the result is also
    `NaN` or `missing` (`missing` takes precedence if array contains both).
    Use the [`skipmissing`](@ref) function to omit `missing` entries and compute the
    mean of non-missing values.

# Examples
```jldoctest
julia> mean(1:20)
10.5

julia> mean([1, missing, 3])
missing

julia> mean(skipmissing([1, missing, 3]))
2.0
```
"""
mean(itr) = mean(identity, itr)

"""
    mean(f::Function, itr)

Apply the function `f` to each element of collection `itr` and take the mean.

```jldoctest
julia> mean(√, [1, 2, 3])
1.3820881233139908

julia> mean([√1, √2, √3])
1.3820881233139908
```
"""
function mean(f::Base.Callable, itr)
    y = iterate(itr)
    if y === nothing
        throw(ArgumentError("mean of empty collection undefined: $(repr(itr))"))
    end
    count = 1
    value, state = y
    f_value = f(value)
    total = Base.reduce_first(Base.add_sum, f_value)
    y = iterate(itr, state)
    while y !== nothing
        value, state = y
        total += f(value)
        count += 1
        y = iterate(itr, state)
    end
    return total/count
end
mean(f::Base.Callable, A::AbstractArray) = sum(f, A) / length(A)

"""
    mean!(r, v)

Compute the mean of `v` over the singleton dimensions of `r`, and write results to `r`.

# Examples
```jldoctest
julia> v = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> mean!([1., 1.], v)
2-element Array{Float64,1}:
 1.5
 3.5

julia> mean!([1. 1.], v)
1×2 Array{Float64,2}:
 2.0  3.0
```
"""
function mean!(R::AbstractArray, A::AbstractArray)
    sum!(R, A; init=true)
    x = max(1, length(R)) // length(A)
    R .= R .* x
    return R
end

"""
    mean(A::AbstractArray; dims)

Compute the mean of an array over the given dimensions.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> mean(A, dims=1)
1×2 Array{Float64,2}:
 2.0  3.0

julia> mean(A, dims=2)
2×1 Array{Float64,2}:
 1.5
 3.5
```
"""
mean(A::AbstractArray; dims=:) = _mean(A, dims)

_mean(A::AbstractArray{T}, region) where {T} = mean!(Base.reducedim_init(t -> t/2, +, A, region), A)
_mean(A::AbstractArray, ::Colon) = sum(A) / length(A)

function mean(r::AbstractRange{<:Real})
    isempty(r) && throw(ArgumentError("mean of an empty range is undefined"))
    (first(r) + last(r)) / 2
end

median(r::AbstractRange{<:Real}) = mean(r)

##### variances #####

# faster computation of real(conj(x)*y)
realXcY(x::Real, y::Real) = x*y
realXcY(x::Complex, y::Complex) = real(x)*real(y) + imag(x)*imag(y)

var(iterable; corrected::Bool=true, mean=nothing) = _var(iterable, corrected, mean)

function _var(iterable, corrected::Bool, mean)
    y = iterate(iterable)
    if y === nothing
        throw(ArgumentError("variance of empty collection undefined: $(repr(iterable))"))
    end
    count = 1
    value, state = y
    y = iterate(iterable, state)
    if mean === nothing
        # Use Welford algorithm as seen in (among other places)
        # Knuth's TAOCP, Vol 2, page 232, 3rd edition.
        M = value / 1
        S = real(zero(M))
        while y !== nothing
            value, state = y
            y = iterate(iterable, state)
            count += 1
            new_M = M + (value - M) / count
            S = S + realXcY(value - M, value - new_M)
            M = new_M
        end
        return S / (count - Int(corrected))
    elseif isa(mean, Number) # mean provided
        # Cannot use a compensated version, e.g. the one from
        # "Updating Formulae and a Pairwise Algorithm for Computing Sample Variances."
        # by Chan, Golub, and LeVeque, Technical Report STAN-CS-79-773,
        # Department of Computer Science, Stanford University,
        # because user can provide mean value that is different to mean(iterable)
        sum2 = abs2(value - mean::Number)
        while y !== nothing
            value, state = y
            y = iterate(iterable, state)
            count += 1
            sum2 += abs2(value - mean)
        end
        return sum2 / (count - Int(corrected))
    else
        throw(ArgumentError("invalid value of mean, $(mean)::$(typeof(mean))"))
    end
end

centralizedabs2fun(m) = x -> abs2.(x - m)
centralize_sumabs2(A::AbstractArray, m) =
    mapreduce(centralizedabs2fun(m), +, A)
centralize_sumabs2(A::AbstractArray, m, ifirst::Int, ilast::Int) =
    Base.mapreduce_impl(centralizedabs2fun(m), +, A, ifirst, ilast)

function centralize_sumabs2!(R::AbstractArray{S}, A::AbstractArray, means::AbstractArray) where S
    # following the implementation of _mapreducedim! at base/reducedim.jl
    lsiz = Base.check_reducedims(R,A)
    isempty(R) || fill!(R, zero(S))
    isempty(A) && return R

    if Base.has_fast_linear_indexing(A) && lsiz > 16 && !has_offset_axes(R, means)
        nslices = div(length(A), lsiz)
        ibase = first(LinearIndices(A))-1
        for i = 1:nslices
            @inbounds R[i] = centralize_sumabs2(A, means[i], ibase+1, ibase+lsiz)
            ibase += lsiz
        end
        return R
    end
    indsAt, indsRt = Base.safe_tail(axes(A)), Base.safe_tail(axes(R)) # handle d=1 manually
    keep, Idefault = Broadcast.shapeindexer(indsRt)
    if Base.reducedim1(R, A)
        i1 = first(Base.axes1(R))
        @inbounds for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            r = R[i1,IR]
            m = means[i1,IR]
            @simd for i in axes(A, 1)
                r += abs2(A[i,IA] - m)
            end
            R[i1,IR] = r
        end
    else
        @inbounds for IA in CartesianIndices(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            @simd for i in axes(A, 1)
                R[i,IR] += abs2(A[i,IA] - means[i,IR])
            end
        end
    end
    return R
end

function varm!(R::AbstractArray{S}, A::AbstractArray, m::AbstractArray; corrected::Bool=true) where S
    if isempty(A)
        fill!(R, convert(S, NaN))
    else
        rn = div(length(A), length(R)) - Int(corrected)
        centralize_sumabs2!(R, A, m)
        R .= R .* (1 // rn)
    end
    return R
end

"""
    varm(v, m; dims, corrected::Bool=true)

Compute the sample variance of a collection `v` with known mean(s) `m`,
optionally over the given dimensions. `m` may contain means for each dimension of
`v`. If `corrected` is `true`, then the sum is scaled with `n-1`,
whereas the sum is scaled with `n` if `corrected` is `false` where `n = length(v)`.

!!! note
    If array contains `NaN` or [`missing`](@ref) values, the result is also
    `NaN` or `missing` (`missing` takes precedence if array contains both).
    Use the [`skipmissing`](@ref) function to omit `missing` entries and compute the
    variance of non-missing values.
"""
varm(A::AbstractArray, m::AbstractArray; corrected::Bool=true, dims=:) = _varm(A, m, corrected, dims)

_varm(A::AbstractArray{T}, m, corrected::Bool, region) where {T} =
    varm!(Base.reducedim_init(t -> abs2(t)/2, +, A, region), A, m; corrected=corrected)

varm(A::AbstractArray, m; corrected::Bool=true) = _varm(A, m, corrected, :)

function _varm(A::AbstractArray{T}, m, corrected::Bool, ::Colon) where T
    n = length(A)
    n == 0 && return typeof((abs2(zero(T)) + abs2(zero(T)))/2)(NaN)
    return centralize_sumabs2(A, m) / (n - Int(corrected))
end


"""
    var(v; dims, corrected::Bool=true, mean=nothing)

Compute the sample variance of a vector or array `v`, optionally along the given dimensions.
The algorithm will return an estimator of the generative distribution's variance
under the assumption that each entry of `v` is an IID drawn from that generative
distribution. This computation is equivalent to calculating `sum(abs2, v - mean(v)) /
(length(v) - 1)`. If `corrected` is `true`, then the sum is scaled with `n-1`,
whereas the sum is scaled with `n` if `corrected` is `false` where `n = length(v)`.
The mean `mean` over the region may be provided.

!!! note
    If array contains `NaN` or [`missing`](@ref) values, the result is also
    `NaN` or `missing` (`missing` takes precedence if array contains both).
    Use the [`skipmissing`](@ref) function to omit `missing` entries and compute the
    variance of non-missing values.
"""
var(A::AbstractArray; corrected::Bool=true, mean=nothing, dims=:) = _var(A, corrected, mean, dims)

_var(A::AbstractArray, corrected::Bool, mean, dims) =
    varm(A, something(mean, Statistics.mean(A, dims=dims)); corrected=corrected, dims=dims)

_var(A::AbstractArray, corrected::Bool, mean, ::Colon) =
    real(varm(A, something(mean, Statistics.mean(A)); corrected=corrected))

varm(iterable, m; corrected::Bool=true) = _var(iterable, corrected, m)

## variances over ranges

varm(v::AbstractRange, m::AbstractArray) = range_varm(v, m)
varm(v::AbstractRange, m) = range_varm(v, m)

function range_varm(v::AbstractRange, m)
    f  = first(v) - m
    s  = step(v)
    l  = length(v)
    vv = f^2 * l / (l - 1) + f * s * l + s^2 * l * (2 * l - 1) / 6
    if l == 0 || l == 1
        return typeof(vv)(NaN)
    end
    return vv
end

function var(v::AbstractRange)
    s  = step(v)
    l  = length(v)
    vv = abs2(s) * (l + 1) * l / 12
    if l == 0 || l == 1
        return typeof(vv)(NaN)
    end
    return vv
end


##### standard deviation #####

function sqrt!(A::AbstractArray)
    for i in eachindex(A)
        @inbounds A[i] = sqrt(A[i])
    end
    A
end

stdm(A::AbstractArray, m; corrected::Bool=true) =
    sqrt.(varm(A, m; corrected=corrected))

"""
    std(v; corrected::Bool=true, mean=nothing, dims)

Compute the sample standard deviation of a vector or array `v`, optionally along the given
dimensions. The algorithm returns an estimator of the generative distribution's standard
deviation under the assumption that each entry of `v` is an IID drawn from that generative
distribution. This computation is equivalent to calculating `sqrt(sum((v - mean(v)).^2) /
(length(v) - 1))`. A pre-computed `mean` may be provided. If `corrected` is `true`,
then the sum is scaled with `n-1`, whereas the sum is scaled with `n` if `corrected` is
`false` where `n = length(v)`.

!!! note
    If array contains `NaN` or [`missing`](@ref) values, the result is also
    `NaN` or `missing` (`missing` takes precedence if array contains both).
    Use the [`skipmissing`](@ref) function to omit `missing` entries and compute the
    standard deviation of non-missing values.
"""
std(A::AbstractArray; corrected::Bool=true, mean=nothing, dims=:) = _std(A, corrected, mean, dims)

_std(A::AbstractArray, corrected::Bool, mean, dims) =
    sqrt.(var(A; corrected=corrected, mean=mean, dims=dims))

_std(A::AbstractArray, corrected::Bool, mean, ::Colon) =
    sqrt.(var(A; corrected=corrected, mean=mean))

_std(A::AbstractArray{<:AbstractFloat}, corrected::Bool, mean, dims) =
    sqrt!(var(A; corrected=corrected, mean=mean, dims=dims))

_std(A::AbstractArray{<:AbstractFloat}, corrected::Bool, mean, ::Colon) =
    sqrt.(var(A; corrected=corrected, mean=mean))

std(iterable; corrected::Bool=true, mean=nothing) =
    sqrt(var(iterable, corrected=corrected, mean=mean))

"""
    stdm(v, m; corrected::Bool=true)

Compute the sample standard deviation of a vector `v`
with known mean `m`. If `corrected` is `true`,
then the sum is scaled with `n-1`, whereas the sum is
scaled with `n` if `corrected` is `false` where `n = length(v)`.

!!! note
    If array contains `NaN` or [`missing`](@ref) values, the result is also
    `NaN` or `missing` (`missing` takes precedence if array contains both).
    Use the [`skipmissing`](@ref) function to omit `missing` entries and compute the
    standard deviation of non-missing values.
"""
stdm(iterable, m; corrected::Bool=true) =
    std(iterable, corrected=corrected, mean=m)


###### covariance ######

# auxiliary functions

_conj(x::AbstractArray{<:Real}) = x
_conj(x::AbstractArray) = conj(x)

_getnobs(x::AbstractVector, vardim::Int) = length(x)
_getnobs(x::AbstractMatrix, vardim::Int) = size(x, vardim)

function _getnobs(x::AbstractVecOrMat, y::AbstractVecOrMat, vardim::Int)
    n = _getnobs(x, vardim)
    _getnobs(y, vardim) == n || throw(DimensionMismatch("dimensions of x and y mismatch"))
    return n
end

_vmean(x::AbstractVector, vardim::Int) = mean(x)
_vmean(x::AbstractMatrix, vardim::Int) = mean(x, dims=vardim)

# core functions

unscaled_covzm(x::AbstractVector{<:Number})    = sum(abs2, x)
unscaled_covzm(x::AbstractVector)              = sum(t -> t*t', x)
unscaled_covzm(x::AbstractMatrix, vardim::Int) = (vardim == 1 ? _conj(x'x) : x * x')

unscaled_covzm(x::AbstractVector, y::AbstractVector) = sum(conj(y[i])*x[i] for i in eachindex(y, x))
unscaled_covzm(x::AbstractVector, y::AbstractMatrix, vardim::Int) =
    (vardim == 1 ? *(transpose(x), _conj(y)) : *(transpose(x), transpose(_conj(y))))
unscaled_covzm(x::AbstractMatrix, y::AbstractVector, vardim::Int) =
    (c = vardim == 1 ? *(transpose(x), _conj(y)) :  x * _conj(y); reshape(c, length(c), 1))
unscaled_covzm(x::AbstractMatrix, y::AbstractMatrix, vardim::Int) =
    (vardim == 1 ? *(transpose(x), _conj(y)) : *(x, adjoint(y)))

# covzm (with centered data)

covzm(x::AbstractVector; corrected::Bool=true) = unscaled_covzm(x) / (length(x) - Int(corrected))
function covzm(x::AbstractMatrix, vardim::Int=1; corrected::Bool=true)
    C = unscaled_covzm(x, vardim)
    T = promote_type(typeof(first(C) / 1), eltype(C))
    A = convert(AbstractMatrix{T}, C)
    b = 1//(size(x, vardim) - corrected)
    A .= A .* b
    return A
end
covzm(x::AbstractVector, y::AbstractVector; corrected::Bool=true) =
    unscaled_covzm(x, y) / (length(x) - Int(corrected))
function covzm(x::AbstractVecOrMat, y::AbstractVecOrMat, vardim::Int=1; corrected::Bool=true)
    C = unscaled_covzm(x, y, vardim)
    T = promote_type(typeof(first(C) / 1), eltype(C))
    A = convert(AbstractArray{T}, C)
    b = 1//(_getnobs(x, y, vardim) - corrected)
    A .= A .* b
    return A
end

# covm (with provided mean)
## Use map(t -> t - xmean, x) instead of x .- xmean to allow for Vector{Vector}
## which can't be handled by broadcast
covm(x::AbstractVector, xmean; corrected::Bool=true) =
    covzm(map(t -> t - xmean, x); corrected=corrected)
covm(x::AbstractMatrix, xmean, vardim::Int=1; corrected::Bool=true) =
    covzm(x .- xmean, vardim; corrected=corrected)
covm(x::AbstractVector, xmean, y::AbstractVector, ymean; corrected::Bool=true) =
    covzm(map(t -> t - xmean, x), map(t -> t - ymean, y); corrected=corrected)
covm(x::AbstractVecOrMat, xmean, y::AbstractVecOrMat, ymean, vardim::Int=1; corrected::Bool=true) =
    covzm(x .- xmean, y .- ymean, vardim; corrected=corrected)

# cov (API)
"""
    cov(x::AbstractVector; corrected::Bool=true)

Compute the variance of the vector `x`. If `corrected` is `true` (the default) then the sum
is scaled with `n-1`, whereas the sum is scaled with `n` if `corrected` is `false` where `n = length(x)`.
"""
cov(x::AbstractVector; corrected::Bool=true) = covm(x, mean(x); corrected=corrected)

"""
    cov(X::AbstractMatrix; dims::Int=1, corrected::Bool=true)

Compute the covariance matrix of the matrix `X` along the dimension `dims`. If `corrected`
is `true` (the default) then the sum is scaled with `n-1`, whereas the sum is scaled with `n`
if `corrected` is `false` where `n = size(X, dims)`.
"""
cov(X::AbstractMatrix; dims::Int=1, corrected::Bool=true) =
    covm(X, _vmean(X, dims), dims; corrected=corrected)

"""
    cov(x::AbstractVector, y::AbstractVector; corrected::Bool=true)

Compute the covariance between the vectors `x` and `y`. If `corrected` is `true` (the
default), computes ``\\frac{1}{n-1}\\sum_{i=1}^n (x_i-\\bar x) (y_i-\\bar y)^*`` where
``*`` denotes the complex conjugate and `n = length(x) = length(y)`. If `corrected` is
`false`, computes ``\\frac{1}{n}\\sum_{i=1}^n (x_i-\\bar x) (y_i-\\bar y)^*``.
"""
cov(x::AbstractVector, y::AbstractVector; corrected::Bool=true) =
    covm(x, mean(x), y, mean(y); corrected=corrected)

"""
    cov(X::AbstractVecOrMat, Y::AbstractVecOrMat; dims::Int=1, corrected::Bool=true)

Compute the covariance between the vectors or matrices `X` and `Y` along the dimension
`dims`. If `corrected` is `true` (the default) then the sum is scaled with `n-1`, whereas
the sum is scaled with `n` if `corrected` is `false` where `n = size(X, dims) = size(Y, dims)`.
"""
cov(X::AbstractVecOrMat, Y::AbstractVecOrMat; dims::Int=1, corrected::Bool=true) =
    covm(X, _vmean(X, dims), Y, _vmean(Y, dims), dims; corrected=corrected)

##### correlation #####

"""
    clampcor(x)

Clamp a real correlation to between -1 and 1, leaving complex correlations unchanged
"""
clampcor(x::Real) = clamp(x, -1, 1)
clampcor(x) = x

# cov2cor!

function cov2cor!(C::AbstractMatrix{T}, xsd::AbstractArray) where T
    @assert !has_offset_axes(C, xsd)
    nx = length(xsd)
    size(C) == (nx, nx) || throw(DimensionMismatch("inconsistent dimensions"))
    for j = 1:nx
        for i = 1:j-1
            C[i,j] = adjoint(C[j,i])
        end
        C[j,j] = oneunit(T)
        for i = j+1:nx
            C[i,j] = clampcor(C[i,j] / (xsd[i] * xsd[j]))
        end
    end
    return C
end
function cov2cor!(C::AbstractMatrix, xsd, ysd::AbstractArray)
    @assert !has_offset_axes(C, ysd)
    nx, ny = size(C)
    length(ysd) == ny || throw(DimensionMismatch("inconsistent dimensions"))
    for (j, y) in enumerate(ysd)   # fixme (iter): here and in all `cov2cor!` we assume that `C` is efficiently indexed by integers
        for i in 1:nx
            C[i,j] = clampcor(C[i, j] / (xsd * y))
        end
    end
    return C
end
function cov2cor!(C::AbstractMatrix, xsd::AbstractArray, ysd)
    @assert !has_offset_axes(C, xsd)
    nx, ny = size(C)
    length(xsd) == nx || throw(DimensionMismatch("inconsistent dimensions"))
    for j in 1:ny
        for (i, x) in enumerate(xsd)
            C[i,j] = clampcor(C[i,j] / (x * ysd))
        end
    end
    return C
end
function cov2cor!(C::AbstractMatrix, xsd::AbstractArray, ysd::AbstractArray)
    @assert !has_offset_axes(C, xsd, ysd)
    nx, ny = size(C)
    (length(xsd) == nx && length(ysd) == ny) ||
        throw(DimensionMismatch("inconsistent dimensions"))
    for (i, x) in enumerate(xsd)
        for (j, y) in enumerate(ysd)
            C[i,j] = clampcor(C[i,j] / (x * y))
        end
    end
    return C
end

# corzm (non-exported, with centered data)

corzm(x::AbstractVector{T}) where {T} = one(real(T))
function corzm(x::AbstractMatrix, vardim::Int=1)
    c = unscaled_covzm(x, vardim)
    return cov2cor!(c, collect(sqrt(c[i,i]) for i in 1:min(size(c)...)))
end
corzm(x::AbstractVector, y::AbstractMatrix, vardim::Int=1) =
    cov2cor!(unscaled_covzm(x, y, vardim), sqrt(sum(abs2, x)), sqrt!(sum(abs2, y, dims=vardim)))
corzm(x::AbstractMatrix, y::AbstractVector, vardim::Int=1) =
    cov2cor!(unscaled_covzm(x, y, vardim), sqrt!(sum(abs2, x, dims=vardim)), sqrt(sum(abs2, y)))
corzm(x::AbstractMatrix, y::AbstractMatrix, vardim::Int=1) =
    cov2cor!(unscaled_covzm(x, y, vardim), sqrt!(sum(abs2, x, dims=vardim)), sqrt!(sum(abs2, y, dims=vardim)))

# corm

corm(x::AbstractVector{T}, xmean) where {T} = one(real(T))
corm(x::AbstractMatrix, xmean, vardim::Int=1) = corzm(x .- xmean, vardim)
function corm(x::AbstractVector, mx, y::AbstractVector, my)
    @assert !has_offset_axes(x, y)
    n = length(x)
    length(y) == n || throw(DimensionMismatch("inconsistent lengths"))
    n > 0 || throw(ArgumentError("correlation only defined for non-empty vectors"))

    @inbounds begin
        # Initialize the accumulators
        xx = zero(sqrt(abs2(x[1])))
        yy = zero(sqrt(abs2(y[1])))
        xy = zero(x[1] * y[1]')

        @simd for i in eachindex(x, y)
            xi = x[i] - mx
            yi = y[i] - my
            xx += abs2(xi)
            yy += abs2(yi)
            xy += xi * yi'
        end
    end
    return clampcor(xy / max(xx, yy) / sqrt(min(xx, yy) / max(xx, yy)))
end

corm(x::AbstractVecOrMat, xmean, y::AbstractVecOrMat, ymean, vardim::Int=1) =
    corzm(x .- xmean, y .- ymean, vardim)

# cor
"""
    cor(x::AbstractVector)

Return the number one.
"""
cor(x::AbstractVector) = one(real(eltype(x)))

"""
    cor(X::AbstractMatrix; dims::Int=1)

Compute the Pearson correlation matrix of the matrix `X` along the dimension `dims`.
"""
cor(X::AbstractMatrix; dims::Int=1) = corm(X, _vmean(X, dims), dims)

"""
    cor(x::AbstractVector, y::AbstractVector)

Compute the Pearson correlation between the vectors `x` and `y`.
"""
cor(x::AbstractVector, y::AbstractVector) = corm(x, mean(x), y, mean(y))

"""
    cor(X::AbstractVecOrMat, Y::AbstractVecOrMat; dims=1)

Compute the Pearson correlation between the vectors or matrices `X` and `Y` along the dimension `dims`.
"""
cor(x::AbstractVecOrMat, y::AbstractVecOrMat; dims::Int=1) =
    corm(x, _vmean(x, dims), y, _vmean(y, dims), dims)

##### median & quantiles #####

"""
    middle(x)

Compute the middle of a scalar value, which is equivalent to `x` itself, but of the type of `middle(x, x)` for consistency.
"""
middle(x::Union{Bool,Int8,Int16,Int32,Int64,Int128,UInt8,UInt16,UInt32,UInt64,UInt128}) = Float64(x)
# Specialized functions for real types allow for improved performance
middle(x::AbstractFloat) = x
middle(x::Real) = (x + zero(x)) / 1

"""
    middle(x, y)

Compute the middle of two reals `x` and `y`, which is
equivalent in both value and type to computing their mean (`(x + y) / 2`).
"""
middle(x::Real, y::Real) = x/2 + y/2

"""
    middle(range)

Compute the middle of a range, which consists of computing the mean of its extrema.
Since a range is sorted, the mean is performed with the first and last element.

```jldoctest
julia> middle(1:10)
5.5
```
"""
middle(a::AbstractRange) = middle(a[1], a[end])

"""
    middle(a)

Compute the middle of an array `a`, which consists of finding its
extrema and then computing their mean.

```jldoctest
julia> a = [1,2,3.6,10.9]
4-element Array{Float64,1}:
  1.0
  2.0
  3.6
 10.9

julia> middle(a)
5.95
```
"""
middle(a::AbstractArray) = ((v1, v2) = extrema(a); middle(v1, v2))

"""
    median!(v)

Like [`median`](@ref), but may overwrite the input vector.
"""
function median!(v::AbstractVector)
    isempty(v) && throw(ArgumentError("median of an empty array is undefined, $(repr(v))"))
    eltype(v)>:Missing && any(ismissing, v) && return missing
    (eltype(v)<:AbstractFloat || eltype(v)>:AbstractFloat) && any(isnan, v) && return NaN
    inds = axes(v, 1)
    n = length(inds)
    mid = div(first(inds)+last(inds),2)
    if isodd(n)
        return middle(partialsort!(v,mid))
    else
        m = partialsort!(v, mid:mid+1)
        return middle(m[1], m[2])
    end
end
median!(v::AbstractArray) = median!(vec(v))

"""
    median(itr)

Compute the median of all elements in a collection.
For an even number of elements no exact median element exists, so the result is
equivalent to calculating mean of two median elements.

!!! note
    If `itr` contains `NaN` or [`missing`](@ref) values, the result is also
    `NaN` or `missing` (`missing` takes precedence if `itr` contains both).
    Use the [`skipmissing`](@ref) function to omit `missing` entries and compute the
    median of non-missing values.

# Examples
```jldoctest
julia> median([1, 2, 3])
2.0

julia> median([1, 2, 3, 4])
2.5

julia> median([1, 2, missing, 4])
missing

julia> median(skipmissing([1, 2, missing, 4]))
2.0
```
"""
median(itr) = median!(collect(itr))

"""
    median(A::AbstractArray; dims)

Compute the median of an array along the given dimensions.

# Examples
```jldoctest
julia> median([1 2; 3 4], dims=1)
1×2 Array{Float64,2}:
 2.0  3.0
```
"""
median(v::AbstractArray; dims=:) = _median(v, dims)

_median(v::AbstractArray, dims) = mapslices(median!, v, dims = dims)

_median(v::AbstractArray{T}, ::Colon) where {T} = median!(copyto!(Array{T,1}(undef, length(v)), v))

# for now, use the R/S definition of quantile; may want variants later
# see ?quantile in R -- this is type 7
"""
    quantile!([q::AbstractArray, ] v::AbstractVector, p; sorted=false)

Compute the quantile(s) of a vector `v` at a specified probability or vector or tuple of
probabilities `p` on the interval [0,1]. If `p` is a vector, an optional
output array `q` may also be specified. (If not provided, a new output array is created.)
The keyword argument `sorted` indicates whether `v` can be assumed to be sorted; if
`false` (the default), then the elements of `v` will be partially sorted in-place.

Quantiles are computed via linear interpolation between the points `((k-1)/(n-1), v[k])`,
for `k = 1:n` where `n = length(v)`. This corresponds to Definition 7 of Hyndman and Fan
(1996), and is the same as the R default.

!!! note
    An `ArgumentError` is thrown if `v` contains `NaN` or [`missing`](@ref) values.

* Hyndman, R.J and Fan, Y. (1996) "Sample Quantiles in Statistical Packages",
  *The American Statistician*, Vol. 50, No. 4, pp. 361-365

# Examples
```jldoctest
julia> x = [3, 2, 1];

julia> quantile!(x, 0.5)
2.0

julia> x
3-element Array{Int64,1}:
 1
 2
 3

julia> y = zeros(3);

julia> quantile!(y, x, [0.1, 0.5, 0.9]) === y
true

julia> y
3-element Array{Float64,1}:
 1.2
 2.0
 2.8
```
"""
function quantile!(q::AbstractArray, v::AbstractVector, p::AbstractArray;
                   sorted::Bool=false)
    @assert !has_offset_axes(q, v, p)
    if size(p) != size(q)
        throw(DimensionMismatch("size of p, $(size(p)), must equal size of q, $(size(q))"))
    end
    isempty(q) && return q

    minp, maxp = extrema(p)
    _quantilesort!(v, sorted, minp, maxp)

    for (i, j) in zip(eachindex(p), eachindex(q))
        @inbounds q[j] = _quantile(v,p[i])
    end
    return q
end

quantile!(v::AbstractVector, p::AbstractArray; sorted::Bool=false) =
    quantile!(similar(p,float(eltype(v))), v, p; sorted=sorted)

quantile!(v::AbstractVector, p::Real; sorted::Bool=false) =
    _quantile(_quantilesort!(v, sorted, p, p), p)

function quantile!(v::AbstractVector, p::Tuple{Vararg{Real}}; sorted::Bool=false)
    isempty(p) && return ()
    minp, maxp = extrema(p)
    _quantilesort!(v, sorted, minp, maxp)
    return map(x->_quantile(v, x), p)
end

# Function to perform partial sort of v for quantiles in given range
function _quantilesort!(v::AbstractArray, sorted::Bool, minp::Real, maxp::Real)
    isempty(v) && throw(ArgumentError("empty data vector"))
    @assert !has_offset_axes(v)

    if !sorted
        lv = length(v)
        lo = floor(Int,1+minp*(lv-1))
        hi = ceil(Int,1+maxp*(lv-1))

        # only need to perform partial sort
        sort!(v, 1, lv, Base.Sort.PartialQuickSort(lo:hi), Base.Sort.Forward)
    end
    ismissing(v[end]) && throw(ArgumentError("quantiles are undefined in presence of missing values"))
    isnan(v[end]) && throw(ArgumentError("quantiles are undefined in presence of NaNs"))
    return v
end

# Core quantile lookup function: assumes `v` sorted
@inline function _quantile(v::AbstractVector, p::Real)
    0 <= p <= 1 || throw(ArgumentError("input probability out of [0,1] range"))
    @assert !has_offset_axes(v)

    lv = length(v)
    f0 = (lv - 1)*p # 0-based interpolated index
    t0 = trunc(f0)
    h  = f0 - t0
    i  = trunc(Int,t0) + 1

    T  = promote_type(eltype(v), typeof(v[1]*h))

    if h == 0
        return convert(T, v[i])
    else
        a = v[i]
        b = v[i+1]
        if isfinite(a) && isfinite(b)
            return convert(T, a + h*(b-a))
        else
            return convert(T, (1-h)*a + h*b)
        end
    end
end


"""
    quantile(itr, p; sorted=false)

Compute the quantile(s) of a collection `itr` at a specified probability or vector or tuple of
probabilities `p` on the interval [0,1]. The keyword argument `sorted` indicates whether
`itr` can be assumed to be sorted.

Quantiles are computed via linear interpolation between the points `((k-1)/(n-1), v[k])`,
for `k = 1:n` where `n = length(itr)`. This corresponds to Definition 7 of Hyndman and Fan
(1996), and is the same as the R default.

!!! note
    An `ArgumentError` is thrown if `itr` contains `NaN` or [`missing`](@ref) values.
    Use the [`skipmissing`](@ref) function to omit `missing` entries and compute the
    quantiles of non-missing values.

- Hyndman, R.J and Fan, Y. (1996) "Sample Quantiles in Statistical Packages",
  *The American Statistician*, Vol. 50, No. 4, pp. 361-365

# Examples
```jldoctest
julia> quantile(0:20, 0.5)
10.0

julia> quantile(0:20, [0.1, 0.5, 0.9])
3-element Array{Float64,1}:
  2.0
 10.0
 18.0

julia> quantile(skipmissing([1, 10, missing]), 0.5)
5.5
 ```
"""
quantile(itr, p; sorted::Bool=false) = quantile!(collect(itr), p, sorted=sorted)

quantile(v::AbstractVector, p; sorted::Bool=false) =
    quantile!(sorted ? v : Base.copymutable(v), p; sorted=sorted)


##### SparseArrays optimizations #####

function cov(X::SparseMatrixCSC; dims::Int=1, corrected::Bool=true)
    vardim = dims
    a, b = size(X)
    n, p = vardim == 1 ? (a, b) : (b, a)

    # The covariance can be decomposed into two terms
    # 1/(n - 1) ∑ (x_i - x̄)*(x_i - x̄)' = 1/(n - 1) (∑ x_i*x_i' - n*x̄*x̄')
    # which can be evaluated via a sparse matrix-matrix product

    # Compute ∑ x_i*x_i' = X'X using sparse matrix-matrix product
    out = Matrix(unscaled_covzm(X, vardim))

    # Compute x̄
    x̄ᵀ = mean(X, dims=vardim)

    # Subtract n*x̄*x̄' from X'X
    @inbounds for j in 1:p, i in 1:p
        out[i,j] -= x̄ᵀ[i] * x̄ᵀ[j]' * n
    end

    # scale with the sample size n or the corrected sample size n - 1
    return rmul!(out, inv(n - corrected))
end

# This is the function that does the reduction underlying var/std
function centralize_sumabs2!(R::AbstractArray{S}, A::SparseMatrixCSC{Tv,Ti}, means::AbstractArray) where {S,Tv,Ti}
    @assert !has_offset_axes(R, A, means)
    lsiz = Base.check_reducedims(R,A)
    size(means) == size(R) || error("size of means must match size of R")
    isempty(R) || fill!(R, zero(S))
    isempty(A) && return R

    colptr = A.colptr
    rowval = A.rowval
    nzval = A.nzval
    m = size(A, 1)
    n = size(A, 2)

    if size(R, 1) == size(R, 2) == 1
        # Reduction along both columns and rows
        R[1, 1] = centralize_sumabs2(A, means[1])
    elseif size(R, 1) == 1
        # Reduction along rows
        @inbounds for col = 1:n
            mu = means[col]
            r = convert(S, (m-colptr[col+1]+colptr[col])*abs2(mu))
            @simd for j = colptr[col]:colptr[col+1]-1
                r += abs2(nzval[j] - mu)
            end
            R[1, col] = r
        end
    elseif size(R, 2) == 1
        # Reduction along columns
        rownz = fill(convert(Ti, n), m)
        @inbounds for col = 1:n
            @simd for j = colptr[col]:colptr[col+1]-1
                row = rowval[j]
                R[row, 1] += abs2(nzval[j] - means[row])
                rownz[row] -= 1
            end
        end
        for i = 1:m
            R[i, 1] += rownz[i]*abs2(means[i])
        end
    else
        # Reduction along a dimension > 2
        @inbounds for col = 1:n
            lastrow = 0
            @simd for j = colptr[col]:colptr[col+1]-1
                row = rowval[j]
                for i = lastrow+1:row-1
                    R[i, col] = abs2(means[i, col])
                end
                R[row, col] = abs2(nzval[j] - means[row, col])
                lastrow = row
            end
            for i = lastrow+1:m
                R[i, col] = abs2(means[i, col])
            end
        end
    end
    return R
end

end # module
