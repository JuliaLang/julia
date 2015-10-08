# This file is a part of Julia. License is MIT: http://julialang.org/license

##### mean #####

function mean(iterable)
    state = start(iterable)
    if done(iterable, state)
        throw(ArgumentError("mean of empty collection undefined: $(repr(iterable))"))
    end
    count = 1
    total, state = next(iterable, state)
    while !done(iterable, state)
        value, state = next(iterable, state)
        total += value
        count += 1
    end
    return total/count
end
mean(A::AbstractArray) = sum(A) / length(A)

function mean!{T}(R::AbstractArray{T}, A::AbstractArray)
    sum!(R, A; init=true)
    scale!(R, length(R) / length(A))
    return R
end

momenttype{T}(::Type{T}) = typeof((zero(T) + zero(T)) / 2)
momenttype(::Type{Float32}) = Float32
momenttype{T<:Union{Float64,Int32,Int64,UInt32,UInt64}}(::Type{T}) = Float64

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

function varzm{T}(A::AbstractArray{T}; corrected::Bool=true)
    n = length(A)
    n == 0 && return convert(real(momenttype(T)), NaN)
    return sumabs2(A) / (n - Int(corrected))
end

function varzm!{S}(R::AbstractArray{S}, A::AbstractArray; corrected::Bool=true)
    if isempty(A)
        fill!(R, convert(S, NaN))
    else
        rn = div(length(A), length(r)) - Int(corrected)
        scale!(sumabs2!(R, A; init=true), convert(S, 1/rn))
    end
    return R
end

varzm{T}(A::AbstractArray{T}, region; corrected::Bool=true) =
    varzm!(reducedim_initarray(A, region, 0, real(momenttype(T))), A; corrected=corrected)

immutable CentralizedAbs2Fun{T<:Number} <: Func{1}
    m::T
end
call(f::CentralizedAbs2Fun, x) = abs2(x - f.m)
centralize_sumabs2(A::AbstractArray, m::Number) =
    mapreduce(CentralizedAbs2Fun(m), AddFun(), A)
centralize_sumabs2(A::AbstractArray, m::Number, ifirst::Int, ilast::Int) =
    mapreduce_impl(CentralizedAbs2Fun(m), AddFun(), A, ifirst, ilast)

@generated function centralize_sumabs2!{S,T,N}(R::AbstractArray{S}, A::AbstractArray{T,N}, means::AbstractArray)
    quote
        # following the implementation of _mapreducedim! at base/reducedim.jl
        lsiz = check_reducedims(R,A)
        isempty(R) || fill!(R, zero(S))
        isempty(A) && return R
        @nextract $N sizeR d->size(R,d)
        sizA1 = size(A, 1)

        if has_fast_linear_indexing(A) && lsiz > 16
            # use centralize_sumabs2, which is probably better tuned to achieve higher performance
            nslices = div(length(A), lsiz)
            ibase = 0
            for i = 1:nslices
                @inbounds R[i] = centralize_sumabs2(A, means[i], ibase+1, ibase+lsiz)
                ibase += lsiz
            end
        elseif size(R, 1) == 1 && sizA1 > 1
            # keep the accumulator as a local variable when reducing along the first dimension
            @nloops $N i d->(d>1? (1:size(A,d)) : (1:1)) d->(j_d = sizeR_d==1 ? 1 : i_d) begin
                @inbounds r = (@nref $N R j)
                @inbounds m = (@nref $N means j)
                for i_1 = 1:sizA1
                    @inbounds r += abs2((@nref $N A i) - m)
                end
                @inbounds (@nref $N R j) = r
            end
        else
            # general implementation
            @nloops $N i A d->(j_d = sizeR_d==1 ? 1 : i_d) begin
                @inbounds (@nref $N R j) += abs2((@nref $N A i) - (@nref $N means j))
            end
        end
        return R
    end
end

function varm{T}(A::AbstractArray{T}, m::Number; corrected::Bool=true)
    n = length(A)
    n == 0 && return convert(real(momenttype(T)), NaN)
    n == 1 && return convert(real(momenttype(T)), abs2(A[1] - m)/(1 - Int(corrected)))
    return centralize_sumabs2(A, m) / (n - Int(corrected))
end

function varm!{S}(R::AbstractArray{S}, A::AbstractArray, m::AbstractArray; corrected::Bool=true)
    if isempty(A)
        fill!(R, convert(S, NaN))
    else
        rn = div(length(A), length(R)) - Int(corrected)
        scale!(centralize_sumabs2!(R, A, m), convert(S, 1/rn))
    end
    return R
end

varm{T}(A::AbstractArray{T}, m::AbstractArray, region; corrected::Bool=true) =
    varm!(reducedim_initarray(A, region, 0, real(momenttype(T))), A, m; corrected=corrected)


function var{T}(A::AbstractArray{T}; corrected::Bool=true, mean=nothing)
    convert(real(momenttype(T)),
            mean == 0 ? varzm(A; corrected=corrected) :
            mean === nothing ? varm(A, Base.mean(A); corrected=corrected) :
            isa(mean, Number) ? varm(A, mean::Number; corrected=corrected) :
            throw(ArgumentError("invalid value of mean, $(mean)::$(typeof(mean))")))::real(momenttype(T))
end

function var(A::AbstractArray, region; corrected::Bool=true, mean=nothing)
    mean == 0 ? varzm(A, region; corrected=corrected) :
    mean === nothing ? varm(A, Base.mean(A, region), region; corrected=corrected) :
    isa(mean, AbstractArray) ? varm(A, mean::AbstractArray, region; corrected=corrected) :
    throw(ArgumentError("invalid value of mean, $(mean)::$(typeof(mean))"))
end

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

std(A::AbstractArray, region; corrected::Bool=true, mean=nothing) =
    sqrt!(var(A, region; corrected=corrected, mean=mean))

std(iterable; corrected::Bool=true, mean=nothing) =
    sqrt(var(iterable, corrected=corrected, mean=mean))

stdm(iterable, m::Number; corrected::Bool=true) =
    std(iterable, corrected=corrected, mean=m)


###### covariance ######

# auxiliary functions

_conj{T<:Real}(x::AbstractArray{T}) = x
_conj(x::AbstractArray) = conj(x)

_getnobs(x::AbstractVector, vardim::Int) = length(x)
_getnobs(x::AbstractMatrix, vardim::Int) = size(x, vardim)

function _getnobs(x::AbstractVecOrMat, y::AbstractVecOrMat, vardim::Int)
    n = _getnobs(x, vardim)
    _getnobs(y, vardim) == n || throw(DimensionMismatch("dimensions of x and y mismatch"))
    return n
end

_vmean(x::AbstractVector, vardim::Int) = mean(x)
_vmean(x::AbstractMatrix, vardim::Int) = mean(x, vardim)

# core functions

unscaled_covzm(x::AbstractVector) = sumabs2(x)
unscaled_covzm(x::AbstractMatrix, vardim::Int) = (vardim == 1 ? _conj(x'x) : x * x')

unscaled_covzm(x::AbstractVector, y::AbstractVector) = dot(x, y)
unscaled_covzm(x::AbstractVector, y::AbstractMatrix, vardim::Int) =
    (vardim == 1 ? At_mul_B(x, _conj(y)) : At_mul_Bt(x, _conj(y)))
unscaled_covzm(x::AbstractMatrix, y::AbstractVector, vardim::Int) =
    (c = vardim == 1 ? At_mul_B(x, _conj(y)) :  x * _conj(y); reshape(c, length(c), 1))
unscaled_covzm(x::AbstractMatrix, y::AbstractMatrix, vardim::Int) =
    (vardim == 1 ? At_mul_B(x, _conj(y)) : A_mul_Bc(x, y))

# covzm (with centered data)

covzm(x::AbstractVector, corrected::Bool=true) = unscaled_covzm(x) / (length(x) - Int(corrected))
covzm(x::AbstractMatrix, vardim::Int=1, corrected::Bool=true) =
    scale!(unscaled_covzm(x, vardim), inv(size(x,vardim) - Int(corrected)))
covzm(x::AbstractVector, y::AbstractVector, corrected::Bool=true) =
    unscaled_covzm(x, y) / (length(x) - Int(corrected))
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
doc"""
    cov(x[, corrected=true])

Compute the variance of the vector `x`. If `corrected` is `true` (the default) then the sum is scaled with `n-1` wheares the sum is scaled with `n` if `corrected` is `false` where `n = length(x)`.
"""
cov(x::AbstractVector, corrected::Bool) = covm(x, Base.mean(x), corrected)
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these two methods can be merged
cov{T<:AbstractVector}(x::T) = covm(x, Base.mean(x), true)

doc"""
    cov(X[, vardim=1, corrected=true])

Compute the covariance matrix of the matrix `X` along the dimension `vardim`. If `corrected` is `true` (the default) then the sum is scaled with `n-1` wheares the sum is scaled with `n` if `corrected` is `false` where `n = size(X, vardim)`.
"""
cov(X::AbstractMatrix, vardim::Int, corrected::Bool=true) =
    covm(X, _vmean(X, vardim), vardim, corrected)
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these two methods can be merged
cov{T<:AbstractMatrix}(X::T) = cov(X, 1, true)

doc"""
    cov(x, y[, corrected=true])

Compute the covariance between the vectors `x` and `y`. If `corrected` is `true` (the default) then the sum is scaled with `n-1` wheares the sum is scaled with `n` if `corrected` is `false` where `n = length(x) = length(y)`.
"""
cov(x::AbstractVector, y::AbstractVector, corrected::Bool) =
    covm(x, Base.mean(x), y, Base.mean(y), corrected)
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these two methods can be merged
cov{T<:AbstractVector,S<:AbstractVector}(x::T, y::S) =
    covm(x, Base.mean(x), y, Base.mean(y), true)

doc"""
    cov(X, Y[, vardim=1, corrected=true])

Compute the covariance between the vectors or matrices `X` and `Y` along the dimension `vardim`. If `corrected` is `true` (the default) then the sum is scaled with `n-1` wheares the sum is scaled with `n` if `corrected` is `false` where `n = size(X, vardim) = size(Y, vardim)`.

"""
cov(X::AbstractVecOrMat, Y::AbstractVecOrMat, vardim::Int, corrected::Bool=true) =
    covm(X, _vmean(X, vardim), Y, _vmean(Y, vardim), vardim, corrected)
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these two methods can be merged
cov{T<:AbstractVecOrMat,S<:AbstractVecOrMat}(X::T, Y::S) =
    covm(X, _vmean(X, vardim), Y, _vmean(Y, vardim), 1, true)

##### correlation #####

# cov2cor!

function cov2cor!{T}(C::AbstractMatrix{T}, xsd::AbstractArray)
    nx = length(xsd)
    size(C) == (nx, nx) || throw(DimensionMismatch("inconsistent dimensions"))
    for j = 1:nx
        for i = 1:j-1
            C[i,j] = C[j,i]
        end
        C[j,j] = one(T)
        for i = j+1:nx
            C[i,j] /= (xsd[i] * xsd[j])
        end
    end
    return C
end
function cov2cor!(C::AbstractMatrix, xsd::Number, ysd::AbstractArray)
    nx, ny = size(C)
    length(ysd) == ny || throw(DimensionMismatch("inconsistent dimensions"))
    for j = 1:ny
        for i = 1:nx
            C[i,j] /= (xsd * ysd[j])
        end
    end
    return C
end
function cov2cor!(C::AbstractMatrix, xsd::AbstractArray, ysd::Number)
    nx, ny = size(C)
    length(xsd) == nx || throw(DimensionMismatch("inconsistent dimensions"))
    for j = 1:ny
        for i = 1:nx
            C[i,j] /= (xsd[i] * ysd)
        end
    end
    return C
end
function cov2cor!(C::AbstractMatrix, xsd::AbstractArray, ysd::AbstractArray)
    nx, ny = size(C)
    (length(xsd) == nx && length(ysd) == ny) ||
        throw(DimensionMismatch("inconsistent dimensions"))
    for j = 1:ny
        for i = 1:nx
            C[i,j] /= (xsd[i] * ysd[j])
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
function corzm(x::AbstractVector, y::AbstractVector)
    n = length(x)
    length(y) == n || throw(DimensionMismatch("inconsistent lengths"))
    x1 = x[1]
    y1 = y[1]
    xx = abs2(x1)
    yy = abs2(y1)
    xy = x1 * conj(y1)
    i = 1
    while i < n
        i += 1
        @inbounds xi = x[i]
        @inbounds yi = y[i]
        xx += abs2(xi)
        yy += abs2(yi)
        xy += xi * conj(yi)
    end
    return xy / (sqrt(xx) * sqrt(yy))
end
corzm(x::AbstractVector, y::AbstractMatrix, vardim::Int=1) =
    cov2cor!(unscaled_covzm(x, y, vardim), sqrt(sumabs2(x)), sqrt!(sumabs2(y, vardim)))
corzm(x::AbstractMatrix, y::AbstractVector, vardim::Int=1) =
    cov2cor!(unscaled_covzm(x, y, vardim), sqrt!(sumabs2(x, vardim)), sqrt(sumabs2(y)))
corzm(x::AbstractMatrix, y::AbstractMatrix, vardim::Int=1) =
    cov2cor!(unscaled_covzm(x, y, vardim), sqrt!(sumabs2(x, vardim)), sqrt!(sumabs2(y, vardim)))

# corm

corm{T}(x::AbstractVector{T}, xmean) = one(real(T))
corm(x::AbstractMatrix, xmean, vardim::Int=1) = corzm(x .- xmean, vardim)
corm(x::AbstractVector, xmean, y::AbstractVector, ymean) = corzm(x .- xmean, y .- ymean)
corm(x::AbstractVecOrMat, xmean, y::AbstractVecOrMat, ymean, vardim::Int=1) =
    corzm(x .- xmean, y .- ymean, vardim)

# cor
doc"""
    cor(x)

Return the number one.
"""
cor{T<:AbstractVector}(x::T) = one(real(eltype(x)))
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these two methods can be merged

doc"""
    cor(X[, vardim=1])

Compute the Pearson correlation matrix of the matrix `X` along the dimension `vardim`.
"""
cor(X::AbstractMatrix, vardim::Int) = corm(X, _vmean(X, vardim), vardim)
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these two methods can be merged
cor{T<:AbstractMatrix}(X::T) = corm(X, _vmean(X, vardim), 1)

doc"""
    cor(x, y)

Compute the Pearson correlation between the vectors `x` and `y`.
"""
cor{T<:AbstractVector,S<:AbstractVector}(x::T, y::S) = corm(x, Base.mean(x), y, Base.mean(y))
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these two methods can be merged

doc"""
    cor(X, Y[, vardim=1])

Compute the Pearson correlation between the vectors or matrices `X` and `Y` along the dimension `vardim`.

"""
cor(x::AbstractVecOrMat, y::AbstractVecOrMat, vardim::Int) =
    corm(x, _vmean(x, vardim), y, _vmean(y, vardim), vardim)
# This ugly hack is necessary to make the method below considered more specific than the deprecated method. When the old keyword version has been completely deprecated, these two methods can be merged
cor(x::AbstractVecOrMat, y::AbstractVecOrMat) =
    corm(x, _vmean(x, vardim), y, _vmean(y, vardim), 1)

##### median & quantiles #####

"""
    middle(x)

Compute the middle of a scalar value, which is equivalent to `x` itself, but of the type of `middle(x, x)` for consistency.
"""
# Specialized functions for real types allow for improved performance
middle(x::Union{Bool,Int8,Int16,Int32,Int64,Int128,UInt8,UInt16,UInt32,UInt64,UInt128}) = Float64(x)
middle(x::AbstractFloat) = x
middle(x::Float16) = Float32(x)
middle(x::Real) = (x + zero(x)) / 1

"""
    middle(x, y)

Compute the middle of two reals `x` and `y`, which is equivalent in both value and type to computing their mean (`(x + y) / 2`).
"""
middle(x::Real, y::Real) = x/2 + y/2

"""
    middle(range)

Compute the middle of a range, which consists in computing the mean of its extrema. Since a range is sorted, the mean is performed with the first and last element.
"""
middle(a::Range) = middle(a[1], a[end])

"""
    middle(array)

Compute the middle of an array, which consists in finding its extrema and then computing their mean.
"""
middle(a::AbstractArray) = ((v1, v2) = extrema(a); middle(v1, v2))

function median!{T}(v::AbstractVector{T})
    isempty(v) && throw(ArgumentError("median of an empty array is undefined, $(repr(v))"))
    if T<:AbstractFloat
        @inbounds for x in v
            isnan(x) && return x
        end
    end
    n = length(v)
    if isodd(n)
        return middle(select!(v,div(n+1,2)))
    else
        m = select!(v, div(n,2):div(n,2)+1)
        return middle(m[1], m[2])
    end
end
median!{T}(v::AbstractArray{T}) = median!(vec(v))

median{T}(v::AbstractArray{T}) = median!(vec(copy(v)))
median{T}(v::AbstractArray{T}, region) = mapslices(median!, v, region)

# for now, use the R/S definition of quantile; may want variants later
# see ?quantile in R -- this is type 7
# TODO: need faster implementation (use select!?)
#
function quantile!(v::AbstractVector, q::AbstractVector)
    isempty(v) && throw(ArgumentError("empty data array"))
    isempty(q) && throw(ArgumentError("empty quantile array"))

    # make sure the quantiles are in [0,1]
    q = bound_quantiles(q)

    lv = length(v)
    lq = length(q)

    index = 1 .+ (lv-1)*q
    lo = floor(Int,index)
    hi = ceil(Int,index)
    sort!(v)
    isnan(v[end]) && throw(ArgumentError("quantiles are undefined in presence of NaNs"))
    i = find(index .> lo)
    r = float(v[lo])
    h = (index.-lo)[i]
    r[i] = (1.-h).*r[i] + h.*v[hi[i]]
    return r
end

"""
    quantile(v, ps)

Compute the quantiles of a vector `v` at a specified set of probability values `ps`. Note: Julia does not ignore `NaN` values in the computation.
"""
quantile(v::AbstractVector, q::AbstractVector) = quantile!(copy(v),q)
"""
    quantile(v, p)

Compute the quantile of a vector `v` at the probability `p`. Note: Julia does not ignore `NaN` values in the computation.
"""
quantile(v::AbstractVector, q::Number) = quantile(v,[q])[1]

function bound_quantiles(qs::AbstractVector)
    epsilon = 100*eps()
    if (any(qs .< -epsilon) || any(qs .> 1+epsilon))
        throw(ArgumentError("quantiles out of [0,1] range"))
    end
    [min(1,max(0,q)) for q = qs]
end



##### histogram #####

## nice-valued ranges for histograms

function histrange{T<:AbstractFloat,N}(v::AbstractArray{T,N}, n::Integer)
    nv = length(v)
    if nv == 0 && n < 0
        throw(ArgumentError("number of bins must be ≥ 0 for an empty array, got $n"))
    elseif nv > 0 && n < 1
        throw(ArgumentError("number of bins must be ≥ 1 for a non-empty array, got $n"))
    end
    if nv == 0
        return 0.0:1.0:0.0
    end
    lo, hi = extrema(v)
    if hi == lo
        step = 1.0
    else
        bw = (hi - lo) / n
        e = 10.0^floor(log10(bw))
        r = bw / e
        if r <= 2
            step = 2*e
        elseif r <= 5
            step = 5*e
        else
            step = 10*e
        end
    end
    start = step*(ceil(lo/step)-1)
    nm1 = ceil(Int,(hi - start)/step)
    start:step:(start + nm1*step)
end

function histrange{T<:Integer,N}(v::AbstractArray{T,N}, n::Integer)
    nv = length(v)
    if nv == 0 && n < 0
        throw(ArgumentError("number of bins must be ≥ 0 for an empty array, got $n"))
    elseif nv > 0 && n < 1
        throw(ArgumentError("number of bins must be ≥ 1 for a non-empty array, got $n"))
    end
    if nv == 0
        return 0:1:0
    end
    if n <= 0
        throw(ArgumentError("number of bins n=$n must be positive"))
    end
    lo, hi = extrema(v)
    if hi == lo
        step = 1
    else
        bw = (Float64(hi) - Float64(lo)) / n
        e = 10.0^max(0,floor(log10(bw)))
        r = bw / e
        if r <= 1
            step = e
        elseif r <= 2
            step = 2*e
        elseif r <= 5
            step = 5*e
        else
            step = 10*e
        end
    end
    start = step*(ceil(lo/step)-1)
    nm1 = ceil(Int,(hi - start)/step)
    start:step:(start + nm1*step)
end

## midpoints of intervals
midpoints(r::Range) = r[1:length(r)-1] + 0.5*step(r)
midpoints(v::AbstractVector) = [0.5*(v[i] + v[i+1]) for i in 1:length(v)-1]

## hist ##
function sturges(n)  # Sturges' formula
    n==0 && return one(n)
    ceil(Int,log2(n))+1
end

function hist!{HT}(h::AbstractArray{HT}, v::AbstractVector, edg::AbstractVector; init::Bool=true)
    n = length(edg) - 1
    length(h) == n || throw(DimensionMismatch("length(histogram) must equal length(edges) - 1"))
    if init
        fill!(h, zero(HT))
    end
    for x in v
        i = searchsortedfirst(edg, x)-1
        if 1 <= i <= n
            h[i] += 1
        end
    end
    edg, h
end

hist(v::AbstractVector, edg::AbstractVector) = hist!(Array(Int, length(edg)-1), v, edg)
hist(v::AbstractVector, n::Integer) = hist(v,histrange(v,n))
hist(v::AbstractVector) = hist(v,sturges(length(v)))

function hist!{HT}(H::AbstractArray{HT,2}, A::AbstractMatrix, edg::AbstractVector; init::Bool=true)
    m, n = size(A)
    sH = size(H)
    sE = (length(edg)-1,n)
    sH == sE || throw(DimensionMismatch("incorrect size of histogram"))
    if init
        fill!(H, zero(HT))
    end
    for j = 1:n
        hist!(sub(H, :, j), sub(A, :, j), edg)
    end
    edg, H
end

hist(A::AbstractMatrix, edg::AbstractVector) = hist!(Array(Int, length(edg)-1, size(A,2)), A, edg)
hist(A::AbstractMatrix, n::Integer) = hist(A,histrange(A,n))
hist(A::AbstractMatrix) = hist(A,sturges(size(A,1)))


## hist2d
function hist2d!{HT}(H::AbstractArray{HT,2}, v::AbstractMatrix,
                     edg1::AbstractVector, edg2::AbstractVector; init::Bool=true)
    size(v,2) == 2 || throw(DimensionMismatch("hist2d requires an Nx2 matrix"))
    n = length(edg1) - 1
    m = length(edg2) - 1
    size(H) == (n, m) || throw(DimensionMismatch("incorrect size of histogram"))
    if init
        fill!(H, zero(HT))
    end
    for i = 1:size(v,1)
        x = searchsortedfirst(edg1, v[i,1]) - 1
        y = searchsortedfirst(edg2, v[i,2]) - 1
        if 1 <= x <= n && 1 <= y <= m
            @inbounds H[x,y] += 1
        end
    end
    edg1, edg2, H
end

hist2d(v::AbstractMatrix, edg1::AbstractVector, edg2::AbstractVector) =
    hist2d!(Array(Int, length(edg1)-1, length(edg2)-1), v, edg1, edg2)

hist2d(v::AbstractMatrix, edg::AbstractVector) = hist2d(v, edg, edg)

hist2d(v::AbstractMatrix, n1::Integer, n2::Integer) =
    hist2d(v, histrange(sub(v,:,1),n1), histrange(sub(v,:,2),n2))
hist2d(v::AbstractMatrix, n::Integer) = hist2d(v, n, n)
hist2d(v::AbstractMatrix) = hist2d(v, sturges(size(v,1)))
