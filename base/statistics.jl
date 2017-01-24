# This file is a part of Julia. License is MIT: http://julialang.org/license

##### mean #####

"""
    mean(f::Function, v)

Apply the function `f` to each element of `v` and take the mean.
"""
function mean(f::Callable, iterable)
    state = start(iterable)
    if done(iterable, state)
        throw(ArgumentError("mean of empty collection undefined: $(repr(iterable))"))
    end
    count = 1
    value, state = next(iterable, state)
    f_value = f(value)
    total = f_value + zero(f_value)
    while !done(iterable, state)
        value, state = next(iterable, state)
        total += f(value)
        count += 1
    end
    return total/count
end
mean(iterable) = mean(identity, iterable)
mean(f::Callable, A::AbstractArray) = sum(f, A) / _length(A)
mean(A::AbstractArray) = sum(A) / _length(A)

function mean!{T}(R::AbstractArray{T}, A::AbstractArray)
    sum!(R, A; init=true)
    scale!(R, _length(R) / _length(A))
    return R
end

momenttype{T}(::Type{T}) = typeof((zero(T) + zero(T)) / 2)
momenttype(::Type{Float32}) = Float32
momenttype{T<:Union{Float64,Int32,Int64,UInt32,UInt64}}(::Type{T}) = Float64

"""
    mean(v[, region])

Compute the mean of whole array `v`, or optionally along the dimensions in `region`.

!!! note
    Julia does not ignore `NaN` values in the computation. For applications requiring the
    handling of missing data, the `DataArrays.jl` package is recommended.
"""
mean{T}(A::AbstractArray{T}, region) =
    mean!(reducedim_initarray(A, region, 0, momenttype(T)), A)


##### variances #####

# faster computation of real(conj(x)*y)
realXcY(x::Real, y::Real) = x*y
realXcY(x::Complex, y::Complex) = real(x)*real(y) + imag(x)*imag(y)

function var(iterable; corrected::Bool=true, mean=nothing)
    state = start(iterable)
    if done(iterable, state)
        throw(ArgumentError("variance of empty collection undefined: $(repr(iterable))"))
    end
    count = 1
    value, state = next(iterable, state)
    if mean === nothing
        # Use Welford algorithm as seen in (among other places)
        # Knuth's TAOCP, Vol 2, page 232, 3rd edition.
        M = value / 1
        S = real(zero(M))
        while !done(iterable, state)
            value, state = next(iterable, state)
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
        while !done(iterable, state)
            value, state = next(iterable, state)
            count += 1
            sum2 += abs2(value - mean)
        end
        return sum2 / (count - Int(corrected))
    else
        throw(ArgumentError("invalid value of mean, $(mean)::$(typeof(mean))"))
    end
end

centralizedabs2fun(m::Number) = x -> abs2(x - m)
centralize_sumabs2(A::AbstractArray, m::Number) =
    mapreduce(centralizedabs2fun(m), +, A)
centralize_sumabs2(A::AbstractArray, m::Number, ifirst::Int, ilast::Int) =
    mapreduce_impl(centralizedabs2fun(m), +, A, ifirst, ilast)

function centralize_sumabs2!{S,T,N}(R::AbstractArray{S}, A::AbstractArray{T,N}, means::AbstractArray)
    # following the implementation of _mapreducedim! at base/reducedim.jl
    lsiz = check_reducedims(R,A)
    isempty(R) || fill!(R, zero(S))
    isempty(A) && return R

    if has_fast_linear_indexing(A) && lsiz > 16
        nslices = div(_length(A), lsiz)
        ibase = first(linearindices(A))-1
        for i = 1:nslices
            @inbounds R[i] = centralize_sumabs2(A, means[i], ibase+1, ibase+lsiz)
            ibase += lsiz
        end
        return R
    end
    indsAt, indsRt = safe_tail(indices(A)), safe_tail(indices(R)) # handle d=1 manually
    keep, Idefault = Broadcast.shapeindexer(indsAt, indsRt)
    if reducedim1(R, A)
        i1 = first(indices1(R))
        @inbounds for IA in CartesianRange(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            r = R[i1,IR]
            m = means[i1,IR]
            @simd for i in indices(A, 1)
                r += abs2(A[i,IA] - m)
            end
            R[i1,IR] = r
        end
    else
        @inbounds for IA in CartesianRange(indsAt)
            IR = Broadcast.newindex(IA, keep, Idefault)
            @simd for i in indices(A, 1)
                R[i,IR] += abs2(A[i,IA] - means[i,IR])
            end
        end
    end
    return R
end

function varm{T}(A::AbstractArray{T}, m::Number; corrected::Bool=true)
    n = _length(A)
    n == 0 && return convert(real(momenttype(T)), NaN)
    n == 1 && return convert(real(momenttype(T)), abs2(A[1] - m)/(1 - Int(corrected)))
    return centralize_sumabs2(A, m) / (n - Int(corrected))
end

function varm!{S}(R::AbstractArray{S}, A::AbstractArray, m::AbstractArray; corrected::Bool=true)
    if isempty(A)
        fill!(R, convert(S, NaN))
    else
        rn = div(_length(A), _length(R)) - Int(corrected)
        scale!(centralize_sumabs2!(R, A, m), convert(S, 1/rn))
    end
    return R
end

"""
    varm(v, m[, region]; corrected::Bool=true)

Compute the sample variance of a collection `v` with known mean(s) `m`,
optionally over `region`. `m` may contain means for each dimension of
`v`. If `corrected` is `true`, then the sum is scaled with `n-1`,
whereas the sum is scaled with `n` if `corrected` is `false` where `n = length(x)`.

!!! note
    Julia does not ignore `NaN` values in the computation. For
    applications requiring the handling of missing data, the
    `DataArrays.jl` package is recommended.
"""
varm{T}(A::AbstractArray{T}, m::AbstractArray, region; corrected::Bool=true) =
    varm!(reducedim_initarray(A, region, 0, real(momenttype(T))), A, m; corrected=corrected)


var{T}(A::AbstractArray{T}; corrected::Bool=true, mean=nothing) =
    convert(real(momenttype(T)),
            varm(A, mean === nothing ? Base.mean(A) : mean; corrected=corrected))

"""
    var(v[, region]; corrected::Bool=true, mean=nothing)

Compute the sample variance of a vector or array `v`, optionally along dimensions in
`region`. The algorithm will return an estimator of the generative distribution's variance
under the assumption that each entry of `v` is an IID drawn from that generative
distribution. This computation is equivalent to calculating `sum(abs2, v - mean(v)) /
(length(v) - 1)`. If `corrected` is `true`, then the sum is scaled with `n-1`,
whereas the sum is scaled with `n` if `corrected` is `false` where `n = length(x)`.
The mean `mean` over the region may be provided.

!!! note
    Julia does not ignore `NaN` values in the computation. For
    applications requiring the handling of missing data, the
    `DataArrays.jl` package is recommended.
"""
var(A::AbstractArray, region; corrected::Bool=true, mean=nothing) =
    varm(A, mean === nothing ? Base.mean(A, region) : mean, region; corrected=corrected)

varm(iterable, m::Number; corrected::Bool=true) =
    var(iterable, corrected=corrected, mean=m)

## variances over ranges

function varm(v::Range, m::Number)
    f = first(v) - m
    s = step(v)
    l = length(v)
    if l == 0 || l == 1
           return NaN
    end
    return f^2 * l / (l - 1) + f * s * l + s^2 * l * (2 * l - 1) / 6
end

function var(v::Range)
    s = step(v)
    l = length(v)
    if l == 0 || l == 1
        return NaN
    end
    return abs2(s) * (l + 1) * l / 12
end


##### standard deviation #####

function sqrt!(A::AbstractArray)
    for i in eachindex(A)
        @inbounds A[i] = sqrt(A[i])
    end
    A
end

stdm(A::AbstractArray, m::Number; corrected::Bool=true) =
    sqrt(varm(A, m; corrected=corrected))

std(A::AbstractArray; corrected::Bool=true, mean=nothing) =
    sqrt(var(A; corrected=corrected, mean=mean))

"""
    std(v[, region]; corrected::Bool=true, mean=nothing)

Compute the sample standard deviation of a vector or array `v`, optionally along dimensions
in `region`. The algorithm returns an estimator of the generative distribution's standard
deviation under the assumption that each entry of `v` is an IID drawn from that generative
distribution. This computation is equivalent to calculating `sqrt(sum((v - mean(v)).^2) /
(length(v) - 1))`. A pre-computed `mean` may be provided. If `corrected` is `true`,
then the sum is scaled with `n-1`, whereas the sum is scaled with `n` if `corrected` is
`false` where `n = length(x)`.

!!! note
    Julia does not ignore `NaN` values in the computation. For
    applications requiring the handling of missing data, the
    `DataArrays.jl` package is recommended.
"""
std(A::AbstractArray, region; corrected::Bool=true, mean=nothing) =
    sqrt!(var(A, region; corrected=corrected, mean=mean))

std(iterable; corrected::Bool=true, mean=nothing) =
    sqrt(var(iterable, corrected=corrected, mean=mean))

"""
    stdm(v, m::Number; corrected::Bool=true)

Compute the sample standard deviation of a vector `v`
with known mean `m`. If `corrected` is `true`,
then the sum is scaled with `n-1`, whereas the sum is
scaled with `n` if `corrected` is `false` where `n = length(x)`.

!!! note
    Julia does not ignore `NaN` values in the computation. For
    applications requiring the handling of missing data, the
    `DataArrays.jl` package is recommended.
"""
stdm(iterable, m::Number; corrected::Bool=true) =
    std(iterable, corrected=corrected, mean=m)


###### covariance ######

# auxiliary functions

_conj{T<:Real}(x::AbstractArray{T}) = x
_conj(x::AbstractArray) = conj(x)

_getnobs(x::AbstractVector, vardim::Int) = _length(x)
_getnobs(x::AbstractMatrix, vardim::Int) = size(x, vardim)

function _getnobs(x::AbstractVecOrMat, y::AbstractVecOrMat, vardim::Int)
    n = _getnobs(x, vardim)
    _getnobs(y, vardim) == n || throw(DimensionMismatch("dimensions of x and y mismatch"))
    return n
end

_vmean(x::AbstractVector, vardim::Int) = mean(x)
_vmean(x::AbstractMatrix, vardim::Int) = mean(x, vardim)

# core functions

unscaled_covzm(x::AbstractVector) = sum(abs2, x)
unscaled_covzm(x::AbstractMatrix, vardim::Int) = (vardim == 1 ? _conj(x'x) : x * x')

unscaled_covzm(x::AbstractVector, y::AbstractVector) = dot(x, y)
unscaled_covzm(x::AbstractVector, y::AbstractMatrix, vardim::Int) =
    (vardim == 1 ? At_mul_B(x, _conj(y)) : At_mul_Bt(x, _conj(y)))
unscaled_covzm(x::AbstractMatrix, y::AbstractVector, vardim::Int) =
    (c = vardim == 1 ? At_mul_B(x, _conj(y)) :  x * _conj(y); reshape(c, length(c), 1))
unscaled_covzm(x::AbstractMatrix, y::AbstractMatrix, vardim::Int) =
    (vardim == 1 ? At_mul_B(x, _conj(y)) : A_mul_Bc(x, y))

# covzm (with centered data)

covzm(x::AbstractVector, corrected::Bool=true) = unscaled_covzm(x) / (_length(x) - Int(corrected))
covzm(x::AbstractMatrix, vardim::Int=1, corrected::Bool=true) =
    scale!(unscaled_covzm(x, vardim), inv(size(x,vardim) - Int(corrected)))
covzm(x::AbstractVector, y::AbstractVector, corrected::Bool=true) =
    unscaled_covzm(x, y) / (_length(x) - Int(corrected))
covzm(x::AbstractVecOrMat, y::AbstractVecOrMat, vardim::Int=1, corrected::Bool=true) =
    scale!(unscaled_covzm(x, y, vardim), inv(_getnobs(x, y, vardim) - Int(corrected)))

# covm (with provided mean)

covm(x::AbstractVector, xmean, corrected::Bool=true) =
    covzm(x .- xmean, corrected)
covm(x::AbstractMatrix, xmean, vardim::Int=1, corrected::Bool=true) =
    covzm(x .- xmean, vardim, corrected)
covm(x::AbstractVector, xmean, y::AbstractVector, ymean, corrected::Bool=true) =
    covzm(x .- xmean, y .- ymean, corrected)
covm(x::AbstractVecOrMat, xmean, y::AbstractVecOrMat, ymean, vardim::Int=1, corrected::Bool=true) =
    covzm(x .- xmean, y .- ymean, vardim, corrected)

# cov (API)
"""
    cov(x[, corrected=true])

Compute the variance of the vector `x`. If `corrected` is `true` (the default) then the sum
is scaled with `n-1`, whereas the sum is scaled with `n` if `corrected` is `false` where `n = length(x)`.
"""
cov(x::AbstractVector, corrected::Bool) = covm(x, Base.mean(x), corrected)
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these two methods can be merged
cov{T<:AbstractVector}(x::T) = covm(x, Base.mean(x), true)

"""
    cov(X[, vardim=1, corrected=true])

Compute the covariance matrix of the matrix `X` along the dimension `vardim`. If `corrected`
is `true` (the default) then the sum is scaled with `n-1`, whereas the sum is scaled with `n`
if `corrected` is `false` where `n = size(X, vardim)`.
"""
cov(X::AbstractMatrix, vardim::Int, corrected::Bool=true) =
    covm(X, _vmean(X, vardim), vardim, corrected)
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these two methods can be merged
cov{T<:AbstractMatrix}(X::T) = cov(X, 1, true)

"""
    cov(x, y[, corrected=true])

Compute the covariance between the vectors `x` and `y`. If `corrected` is `true` (the default)
then the sum is scaled with `n-1`, whereas the sum is scaled with `n` if `corrected` is `false`
where `n = length(x) = length(y)`.
"""
cov(x::AbstractVector, y::AbstractVector, corrected::Bool) =
    covm(x, Base.mean(x), y, Base.mean(y), corrected)
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these two methods can be merged
cov{T<:AbstractVector,S<:AbstractVector}(x::T, y::S) =
    covm(x, Base.mean(x), y, Base.mean(y), true)

"""
    cov(X, Y[, vardim=1, corrected=true])

Compute the covariance between the vectors or matrices `X` and `Y` along the dimension
`vardim`. If `corrected` is `true` (the default) then the sum is scaled with `n-1`, whereas
the sum is scaled with `n` if `corrected` is `false` where `n = size(X, vardim) = size(Y, vardim)`.
"""
cov(X::AbstractVecOrMat, Y::AbstractVecOrMat, vardim::Int, corrected::Bool=true) =
    covm(X, _vmean(X, vardim), Y, _vmean(Y, vardim), vardim, corrected)
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these methods can be merged
cov(x::AbstractVector, Y::AbstractMatrix) = cov(x, Y, 1, true)
cov(X::AbstractMatrix, y::AbstractVector) = cov(X, y, 1, true)
cov(X::AbstractMatrix, Y::AbstractMatrix) = cov(X, Y, 1, true)

##### correlation #####

# cov2cor!

function cov2cor!{T}(C::AbstractMatrix{T}, xsd::AbstractArray)
    nx = length(xsd)
    size(C) == (nx, nx) || throw(DimensionMismatch("inconsistent dimensions"))
    for j = 1:nx
        for i = 1:j-1
            C[i,j] = C[j,i]
        end
        C[j,j] = oneunit(T)
        for i = j+1:nx
            C[i,j] = clamp(C[i,j] / (xsd[i] * xsd[j]), -1, 1)
        end
    end
    return C
end
function cov2cor!(C::AbstractMatrix, xsd::Number, ysd::AbstractArray)
    nx, ny = size(C)
    length(ysd) == ny || throw(DimensionMismatch("inconsistent dimensions"))
    for (j, y) in enumerate(ysd)   # fixme (iter): here and in all `cov2cor!` we assume that `C` is efficiently indexed by integers
        for i in 1:nx
            C[i,j] = clamp(C[i, j] / (xsd * y), -1, 1)
        end
    end
    return C
end
function cov2cor!(C::AbstractMatrix, xsd::AbstractArray, ysd::Number)
    nx, ny = size(C)
    length(xsd) == nx || throw(DimensionMismatch("inconsistent dimensions"))
    for j in 1:ny
        for (i, x) in enumerate(xsd)
            C[i,j] = clamp(C[i,j] / (x * ysd), -1, 1)
        end
    end
    return C
end
function cov2cor!(C::AbstractMatrix, xsd::AbstractArray, ysd::AbstractArray)
    nx, ny = size(C)
    (length(xsd) == nx && length(ysd) == ny) ||
        throw(DimensionMismatch("inconsistent dimensions"))
    for (i, x) in enumerate(xsd)
        for (j, y) in enumerate(ysd)
            C[i,j] = clamp(C[i,j] / (x * y), -1, 1)
        end
    end
    return C
end

# corzm (non-exported, with centered data)

corzm{T}(x::AbstractVector{T}) = one(real(T))
function corzm(x::AbstractMatrix, vardim::Int=1)
    c = unscaled_covzm(x, vardim)
    return cov2cor!(c, sqrt!(diag(c)))
end
corzm(x::AbstractVector, y::AbstractMatrix, vardim::Int=1) =
    cov2cor!(unscaled_covzm(x, y, vardim), sqrt(sum(abs2, x)), sqrt!(sum(abs2, y, vardim)))
corzm(x::AbstractMatrix, y::AbstractVector, vardim::Int=1) =
    cov2cor!(unscaled_covzm(x, y, vardim), sqrt!(sum(abs2, x, vardim)), sqrt(sum(abs2, y)))
corzm(x::AbstractMatrix, y::AbstractMatrix, vardim::Int=1) =
    cov2cor!(unscaled_covzm(x, y, vardim), sqrt!(sum(abs2, x, vardim)), sqrt!(sum(abs2, y, vardim)))

# corm

corm{T}(x::AbstractVector{T}, xmean) = one(real(T))
corm(x::AbstractMatrix, xmean, vardim::Int=1) = corzm(x .- xmean, vardim)
function corm(x::AbstractVector, mx::Number, y::AbstractVector, my::Number)
    n = length(x)
    length(y) == n || throw(DimensionMismatch("inconsistent lengths"))
    n > 0 || throw(ArgumentError("correlation only defined for non-empty vectors"))

    @inbounds begin
        # Initialize the accumulators
        xx = zero(sqrt(x[1] * x[1]))
        yy = zero(sqrt(y[1] * y[1]))
        xy = zero(xx * yy)

        @simd for i = 1:n
            xi = x[i] - mx
            yi = y[i] - my
            xx += abs2(xi)
            yy += abs2(yi)
            xy += xi * yi'
        end
    end
    return clamp(xy / max(xx, yy) / sqrt(min(xx, yy) / max(xx, yy)), -1, 1)
end
corm(x::AbstractVecOrMat, xmean, y::AbstractVecOrMat, ymean, vardim::Int=1) =
    corzm(x .- xmean, y .- ymean, vardim)

# cor
"""
    cor(x)

Return the number one.
"""
cor{T<:AbstractVector}(x::T) = one(real(eltype(x)))
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these two methods can be merged

"""
    cor(X[, vardim=1])

Compute the Pearson correlation matrix of the matrix `X` along the dimension `vardim`.
"""
cor(X::AbstractMatrix, vardim::Int) = corm(X, _vmean(X, vardim), vardim)
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these two methods can be merged
cor{T<:AbstractMatrix}(X::T) = cor(X, 1)

"""
    cor(x, y)

Compute the Pearson correlation between the vectors `x` and `y`.
"""
cor{T<:AbstractVector,S<:AbstractVector}(x::T, y::S) = corm(x, Base.mean(x), y, Base.mean(y))
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these two methods can be merged

"""
    cor(X, Y[, vardim=1])

Compute the Pearson correlation between the vectors or matrices `X` and `Y` along the dimension `vardim`.
"""
cor(x::AbstractVecOrMat, y::AbstractVecOrMat, vardim::Int) =
    corm(x, _vmean(x, vardim), y, _vmean(y, vardim), vardim)
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these methods can be merged
cor(x::AbstractVector, Y::AbstractMatrix) = cor(x, Y, 1)
cor(X::AbstractMatrix, y::AbstractVector) = cor(X, y, 1)
cor(X::AbstractMatrix, Y::AbstractMatrix) = cor(X, Y, 1)

##### median & quantiles #####

"""
    middle(x)

Compute the middle of a scalar value, which is equivalent to `x` itself, but of the type of `middle(x, x)` for consistency.
"""
# Specialized functions for real types allow for improved performance
middle(x::Union{Bool,Int8,Int16,Int32,Int64,Int128,UInt8,UInt16,UInt32,UInt64,UInt128}) = Float64(x)
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
middle(a::Range) = middle(a[1], a[end])

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
function median!{T}(v::AbstractVector{T})
    isempty(v) && throw(ArgumentError("median of an empty array is undefined, $(repr(v))"))
    if T<:AbstractFloat
        @inbounds for x in v
            isnan(x) && return x
        end
    end
    inds = indices(v, 1)
    n = length(inds)
    mid = div(first(inds)+last(inds),2)
    if isodd(n)
        return middle(select!(v,mid))
    else
        m = select!(v, mid:mid+1)
        return middle(m[1], m[2])
    end
end
median!{T}(v::AbstractArray{T}) = median!(vec(v))
median{T}(v::AbstractArray{T}) = median!(copy!(Array{T,1}(_length(v)), v))

"""
    median(v[, region])

Compute the median of an entire array `v`, or, optionally,
along the dimensions in `region`. For an even number of
elements no exact median element exists, so the result is
equivalent to calculating mean of two median elements.

!!! note
    Julia does not ignore `NaN` values in the computation. For applications requiring the
    handling of missing data, the `DataArrays.jl` package is recommended.
"""
median{T}(v::AbstractArray{T}, region) = mapslices(median!, v, region)

# for now, use the R/S definition of quantile; may want variants later
# see ?quantile in R -- this is type 7
"""
    quantile!([q, ] v, p; sorted=false)

Compute the quantile(s) of a vector `v` at the probabilities `p`, with optional output into
array `q` (if not provided, a new output array is created). The keyword argument `sorted`
indicates whether `v` can be assumed to be sorted; if `false` (the default), then the
elements of `v` may be partially sorted.

The elements of `p` should be on the interval [0,1], and `v` should not have any `NaN`
values.

Quantiles are computed via linear interpolation between the points `((k-1)/(n-1), v[k])`,
for `k = 1:n` where `n = length(v)`. This corresponds to Definition 7 of Hyndman and Fan
(1996), and is the same as the R default.

!!! note
    Julia does not ignore `NaN` values in the computation. For applications requiring the
    handling of missing data, the `DataArrays.jl` package is recommended. `quantile!` will
    throw an `ArgumentError` in the presence of `NaN` values in the data array.

* Hyndman, R.J and Fan, Y. (1996) "Sample Quantiles in Statistical Packages",
  *The American Statistician*, Vol. 50, No. 4, pp. 361-365
"""
function quantile!(q::AbstractArray, v::AbstractVector, p::AbstractArray;
                   sorted::Bool=false)
    if size(p) != size(q)
        throw(DimensionMismatch("size of p, $(size(p)), must equal size of q, $(size(q))"))
    end

    isempty(v) && throw(ArgumentError("empty data vector"))

    lv = length(v)
    if !sorted
        minp, maxp = extrema(p)
        lo = floor(Int,1+minp*(lv-1))
        hi = ceil(Int,1+maxp*(lv-1))

        # only need to perform partial sort
        sort!(v, 1, lv, PartialQuickSort(lo:hi), Base.Sort.Forward)
    end
    isnan(v[end]) && throw(ArgumentError("quantiles are undefined in presence of NaNs"))

    for (i, j) in zip(eachindex(p), eachindex(q))
        @inbounds q[j] = _quantile(v,p[i])
    end
    return q
end

quantile!(v::AbstractVector, p::AbstractArray; sorted::Bool=false) =
    quantile!(similar(p,float(eltype(v))), v, p; sorted=sorted)

function quantile!(v::AbstractVector, p::Real;
                   sorted::Bool=false)
    isempty(v) && throw(ArgumentError("empty data vector"))

    lv = length(v)
    if !sorted
        lo = floor(Int,1+p*(lv-1))
        hi = ceil(Int,1+p*(lv-1))

        # only need to perform partial sort
        sort!(v, 1, lv, PartialQuickSort(lo:hi), Base.Sort.Forward)
    end
    isnan(v[end]) && throw(ArgumentError("quantiles are undefined in presence of NaNs"))

    return _quantile(v,p)
end

# Core quantile lookup function: assumes `v` sorted
@inline function _quantile(v::AbstractVector, p::Real)
    T = float(eltype(v))
    isnan(p) && return T(NaN)
    0 <= p <= 1 || throw(ArgumentError("input probability out of [0,1] range"))

    lv = length(v)
    f0 = (lv-1)*p # 0-based interpolated index
    t0 = trunc(f0)
    h = f0 - t0
    i = trunc(Int,t0) + 1

    if h == 0
        return T(v[i])
    else
        a = T(v[i])
        b = T(v[i+1])
        if isfinite(a) && isfinite(b)
            return a + h*(b-a)
        else
            return (1-h)*a + h*b
        end
    end
end


"""
    quantile(v, p; sorted=false)

Compute the quantile(s) of a vector `v` at a specified probability or vector `p`. The
keyword argument `sorted` indicates whether `v` can be assumed to be sorted.

The `p` should be on the interval [0,1], and `v` should not have any `NaN` values.

Quantiles are computed via linear interpolation between the points `((k-1)/(n-1), v[k])`,
for `k = 1:n` where `n = length(v)`. This corresponds to Definition 7 of Hyndman and Fan
(1996), and is the same as the R default.

!!! note
    Julia does not ignore `NaN` values in the computation. For applications requiring the
    handling of missing data, the `DataArrays.jl` package is recommended. `quantile` will
    throw an `ArgumentError` in the presence of `NaN` values in the data array.

* Hyndman, R.J and Fan, Y. (1996) "Sample Quantiles in Statistical Packages",
  *The American Statistician*, Vol. 50, No. 4, pp. 361-365
"""
quantile(v::AbstractVector, p; sorted::Bool=false) =
    quantile!(sorted ? v : copymutable(v), p; sorted=sorted)
