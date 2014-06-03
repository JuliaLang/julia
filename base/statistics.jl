function mean(iterable)
    state = start(iterable)
    if done(iterable, state)
        error("mean of empty collection undefined: $(repr(iterable))")
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
mean(v::AbstractArray) = sum(v) / length(v)

function mean!{T}(r::AbstractArray{T}, v::AbstractArray)
    sum!(r, v; init=true)
    rs = convert(T, length(v) / length(r))
    if rs != 1
        for i = 1:length(r)
            @inbounds r[i] /= rs
        end
    end
    return r
end

meantype{T}(::Type{T}) = typeof((zero(T) + zero(T)) / 2)
mean{T}(v::AbstractArray{T}, region) = 
    mean!(Array(meantype(T), reduced_dims(size(v), region)), v)

function median!{T<:Real}(v::AbstractVector{T}; checknan::Bool=true)
    isempty(v) && error("median of an empty array is undefined")
    if checknan
        for x in v
            isnan(x) && return x
        end
    end
    n = length(v)
    if isodd(n)
        return select!(v,div(n+1,2))
    else
        m = select!(v, div(n,2):div(n,2)+1)
        return (m[1] + m[2])/2
    end
end
median{T<:Real}(v::AbstractArray{T}; checknan::Bool=true) =
    median!(vec(copy(v)), checknan=checknan)
median{T}(v::AbstractArray{T}, region; checknan::Bool=true) = 
    mapslices( x->median(x; checknan=checknan), v, region )


## variances

function varzm(v::AbstractArray; corrected::Bool=true)
    n = length(v)
    n == 0 && return NaN
    return sumabs2(v) / (n - int(corrected))
end

function varzm(v::AbstractArray, region; corrected::Bool=true)
    cn = regionsize(v, region) - int(corrected)
    sumabs2(v, region) / cn    
end

function varm_pairwise(A::AbstractArray, m::Number, i1::Int, n::Int) # see sum_pairwise
    if n < 256
        @inbounds s = abs2(A[i1] - m)
        for i = i1+1:i1+n-1
            @inbounds s += abs2(A[i] - m)
        end
        return s
    else
        n2 = div(n,2)
        return varm_pairwise(A, m, i1, n2) + varm_pairwise(A, m, i1+n2, n-n2)
    end
end

function varm(v::AbstractArray, m::Number; corrected::Bool=true)
    n = length(v)
    n == 0 && return NaN
    return varm_pairwise(v, m, 1, n) / (n - int(corrected))
end

@ngenerate N Array{typeof((abs2(zero(T))+abs2(zero(T)))/1), N} function _varm{S,T,N}(v::AbstractArray{S,N}, m::AbstractArray{T,N}, region, corrected::Bool)
    rdims = reduced_dims(v, region)
    rdims == size(m) || error(DimensionMismatch("size of mean does not match reduced dimensions"))

    R = fill!(similar(v, typeof((abs2(zero(T))+abs2(zero(T)))/1), rdims), 0)
    @nextract N sizeR d->size(R,d)
    @nloops N i v d->(j_d = sizeR_d==1 ? 1 : i_d) begin
        @inbounds (@nref N R j) += abs2((@nref N v i) - (@nref N m j))
    end
    scale!(R, 1/(regionsize(v, region) - int(corrected)))
end
varm{S,T,N}(v::AbstractArray{S,N}, m::AbstractArray{T,N}, region; corrected::Bool=true) = _varm(v, m, region, corrected)

function var(v::AbstractArray; corrected::Bool=true, mean=nothing)
    mean == 0 ? varzm(v; corrected=corrected) :
    mean == nothing ? varm(v, Base.mean(v); corrected=corrected) :
    isa(mean, Number) ? varm(v, mean; corrected=corrected) :
    error("Invalid value of mean.")
end

function var(v::AbstractArray, region; corrected::Bool=true, mean=nothing)
    mean == 0 ? varzm(v, region; corrected=corrected) :
    mean == nothing ? varm(v, Base.mean(v, region), region; corrected=corrected) :
    isa(mean, AbstractArray) ? varm(v, mean, region; corrected=corrected) :
    error("Invalid value of mean.")
end

function var(iterable; corrected::Bool=true, mean=nothing)
    state = start(iterable)
    if done(iterable, state)
        error("variance of empty collection undefined: $(repr(iterable))")
    end
    count = 1
    value, state = next(iterable, state)
    if mean == nothing
        # Use Welford algorithm as seen in (among other places) 
        # Knuth's TAOCP, Vol 2, page 232, 3rd edition. 
        M = value / 1
        S = zero(M)
        while !done(iterable, state)
            value, state = next(iterable, state)
            count += 1
            new_M = M + (value - M) / count
            S = S + (value - M) * (value - new_M)
            M = new_M
        end
        return S / (count - int(corrected))
    else # mean provided
        # Cannot use a compensated version, e.g. the one from
        # "Updating Formulae and a Pairwise Algorithm for Computing Sample Variances."
        # by Chan, Golub, and LeVeque, Technical Report STAN-CS-79-773, 
        # Department of Computer Science, Stanford University,
        # because user can provide mean value that is different to mean(iterable)
        sum2 = (value - mean)^2
        while !done(iterable, state)
            value, state = next(iterable, state)
            count += 1
            sum2 += (value - mean)^2
        end
        return sum2 / (count - int(corrected))
    end
end

varm(iterable, m::Number; corrected::Bool=true) =
    var(iterable, corrected=corrected, mean=m)

## variances over ranges

varm(v::Range, m::Number) = var(v)

function var(v::Range)
    s = step(v)
    l = length(v)
    if l == 0 || l == 1
        return NaN
    end
    return abs2(s) * (l + 1) * l / 12
end

## standard deviation

function sqrt!(v::AbstractArray) 
    for i = 1:length(v)
        v[i] = sqrt(v[i])
    end
    v
end

stdm(v::AbstractArray, m::Number; corrected::Bool=true) = 
    sqrt(varm(v, m; corrected=corrected))

std(v::AbstractArray; corrected::Bool=true, mean=nothing) = 
    sqrt(var(v; corrected=corrected, mean=mean))

std(v::AbstractArray, region; corrected::Bool=true, mean=nothing) = 
    sqrt!(var(v, region; corrected=corrected, mean=mean))

std(iterable; corrected::Bool=true, mean=nothing) =
    sqrt(var(iterable, corrected=corrected, mean=mean))

stdm(iterable, m::Number; corrected::Bool=true) =
    std(iterable, corrected=corrected, mean=m)

## pearson covariance functions ##

# auxiliary functions

_conj{T<:Real}(x::AbstractArray{T}) = x
_conj(x::AbstractArray) = conj(x)

_getnobs(x::AbstractVector, vardim::Int) = length(x)
_getnobs(x::AbstractMatrix, vardim::Int) = size(x, vardim)

function _getnobs(x::AbstractVecOrMat, y::AbstractVecOrMat, vardim::Int)
    n = _getnobs(x, vardim)
    _getnobs(y, vardim) == n || throw(DimensionMismatch("Dimensions of x and y mismatch."))
    return n
end

_vmean(x::AbstractVector, vardim::Int) = mean(x)
_vmean(x::AbstractMatrix, vardim::Int) = mean(x, vardim)


# core functions

unscaled_covzm(x::AbstractVector) = dot(x, x)
unscaled_covzm(x::AbstractMatrix, vardim::Int) = (vardim == 1 ? _conj(x'x) : x * x')

unscaled_covzm(x::AbstractVector, y::AbstractVector) = dot(x, y)
unscaled_covzm(x::AbstractVector, y::AbstractMatrix, vardim::Int) = 
    (vardim == 1 ? At_mul_B(x, _conj(y)) : At_mul_Bt(x, _conj(y)))
unscaled_covzm(x::AbstractMatrix, y::AbstractVector, vardim::Int) = 
    (c = vardim == 1 ? At_mul_B(x, _conj(y)) :  x * _conj(y); reshape(c, length(c), 1))
unscaled_covzm(x::AbstractMatrix, y::AbstractMatrix, vardim::Int) = 
    (vardim == 1 ? At_mul_B(x, _conj(y)) : A_mul_Bc(x, y))

# covzm (with centered data)

covzm(x::AbstractVector; corrected::Bool=true) = unscaled_covzm(x, x) / (length(x) - int(corrected))

covzm(x::AbstractMatrix; vardim::Int=1, corrected::Bool=true) = 
    scale!(unscaled_covzm(x, vardim), inv(size(x,vardim) - int(corrected)))

covzm(x::AbstractVector, y::AbstractVector; corrected::Bool=true) = 
    unscaled_covzm(x, y) / (length(x) - int(corrected))

covzm(x::AbstractVecOrMat, y::AbstractVecOrMat; vardim::Int=1, corrected::Bool=true) = 
    scale!(unscaled_covzm(x, y, vardim), inv(_getnobs(x, y, vardim) - int(corrected)))

# covm (with provided mean)

covm(x::AbstractVector, xmean; corrected::Bool=true) = 
    covzm(x .- xmean; corrected=corrected)

covm(x::AbstractMatrix, xmean; vardim::Int=1, corrected::Bool=true) = 
    covzm(x .- xmean; vardim=vardim, corrected=corrected)

covm(x::AbstractVector, xmean, y::AbstractVector, ymean; corrected::Bool=true) = 
    covzm(x .- xmean, y .- ymean; corrected=corrected)

covm(x::AbstractVecOrMat, xmean, y::AbstractVecOrMat, ymean; vardim::Int=1, corrected::Bool=true) = 
    covzm(x .- xmean, y .- ymean; vardim=vardim, corrected=corrected)

# cov (API)

function cov(x::AbstractVector; corrected::Bool=true, mean=nothing)
    mean == 0 ? covzm(x; corrected=corrected) :
    mean == nothing ? covm(x, Base.mean(x); corrected=corrected) :
    isa(mean, Number) ? covm(x, mean; corrected=corrected) :
    error("Invalid value of mean.")
end

function cov(x::AbstractMatrix; vardim::Int=1, corrected::Bool=true, mean=nothing)
    mean == 0 ? covzm(x; vardim=vardim, corrected=corrected) :
    mean == nothing ? covm(x, _vmean(x, vardim); vardim=vardim, corrected=corrected) :
    isa(mean, AbstractArray) ? covm(x, mean; vardim=vardim, corrected=corrected) :
    error("Invalid value of mean.")
end

function cov(x::AbstractVector, y::AbstractVector; corrected::Bool=true, mean=nothing)
    mean == 0 ? covzm(x, y; corrected=corrected) :
    mean == nothing ? covm(x, Base.mean(x), y, Base.mean(y); corrected=corrected) :
    isa(mean, (Number,Number)) ? covm(x, mean[1], y, mean[2]; corrected=corrected) :
    error("Invalid value of mean.")
end

function cov(x::AbstractVecOrMat, y::AbstractVecOrMat; vardim::Int=1, corrected::Bool=true, mean=nothing)
    if mean == 0
        covzm(x, y; vardim=vardim, corrected=corrected)
    elseif mean == nothing
        covm(x, _vmean(x, vardim), y, _vmean(y, vardim); vardim=vardim, corrected=corrected)
    elseif isa(mean, (Any,Any))
        covm(x, mean[1], y, mean[2]; vardim=vardim, corrected=corrected)
    else
        error("Invalid value of mean.")
    end
end

# cov2cor!

function cov2cor!{T}(C::AbstractMatrix{T}, xsd::AbstractArray)
    nx = length(xsd)
    size(C) == (nx, nx) || throw(DimensionMismatch("Inconsistent dimensions."))
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
    length(ysd) == ny || throw(DimensionMismatch("Inconsistent dimensions."))
    for j = 1:ny
        for i = 1:nx
            C[i,j] /= (xsd * ysd[j])
        end
    end
    return C
end

function cov2cor!(C::AbstractMatrix, xsd::AbstractArray, ysd::Number)
    nx, ny = size(C)
    length(xsd) == nx || throw(DimensionMismatch("Inconsistent dimensions."))
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
        throw(DimensionMismatch("Inconsistent dimensions."))
    for j = 1:ny
        for i = 1:nx
            C[i,j] /= (xsd[i] * ysd[j])
        end
    end
    return C
end


# # corzm (non-exported, with centered data)

corzm{T}(x::AbstractVector{T}) = float(one(T) * one(T))

corzm(x::AbstractMatrix; vardim::Int=1) = 
    (c = unscaled_covzm(x, vardim); cov2cor!(c, sqrt!(diag(c))))

function corzm(x::AbstractVector, y::AbstractVector)
    n = length(x)
    length(y) == n || throw(DimensionMismatch("Inconsistent lengths."))
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

corzm(x::AbstractVector, y::AbstractMatrix; vardim::Int=1) = 
    cov2cor!(unscaled_covzm(x, y, vardim), sqrt(sumabs2(x)), sqrt!(sumabs2(y, vardim)))

corzm(x::AbstractMatrix, y::AbstractVector; vardim::Int=1) = 
    cov2cor!(unscaled_covzm(x, y, vardim), sqrt!(sumabs2(x, vardim)), sqrt(sumabs2(y)))

corzm(x::AbstractMatrix, y::AbstractMatrix; vardim::Int=1) = 
    cov2cor!(unscaled_covzm(x, y, vardim), sqrt!(sumabs2(x, vardim)), sqrt!(sumabs2(y, vardim)))

# corm

corm(x::AbstractVector, xmean) = corzm(x .- xmean)

corm(x::AbstractMatrix, xmean; vardim::Int=1) = corzm(x .- xmean; vardim=vardim)

corm(x::AbstractVector, xmean, y::AbstractVector, ymean) = corzm(x .- xmean, y .- ymean)

corm(x::AbstractVecOrMat, xmean, y::AbstractVecOrMat, ymean; vardim::Int=1) = 
    corzm(x .- xmean, y .- ymean; vardim=vardim)

# cor

function cor(x::AbstractVector; mean=nothing)
    mean == 0 ? corzm(x) :
    mean == nothing ? corm(x, Base.mean(x)) :
    isa(mean, Number) ? corm(x, mean) :
    error("Invalid value of mean.")
end

function cor(x::AbstractMatrix; vardim::Int=1, mean=nothing)
    mean == 0 ? corzm(x; vardim=vardim) :
    mean == nothing ? corm(x, _vmean(x, vardim); vardim=vardim) :
    isa(mean, AbstractArray) ? corm(x, mean; vardim=vardim) :
    error("Invalid value of mean.")
end

function cor(x::AbstractVector, y::AbstractVector; mean=nothing)
    mean == 0 ? corzm(x, y) : 
    mean == nothing ? corm(x, Base.mean(x), y, Base.mean(y)) :
    isa(mean, (Number,Number)) ? corm(x, mean[1], y, mean[2]) :
    error("Invalid value of mean.")
end

function cor(x::AbstractVecOrMat, y::AbstractVecOrMat; vardim::Int=1, mean=nothing)
    if mean == 0
        corzm(x, y; vardim=vardim)
    elseif mean == nothing 
        corm(x, _vmean(x, vardim), y, _vmean(y, vardim); vardim=vardim)
    elseif isa(mean, (Any,Any))
        corm(x, mean[1], y, mean[2]; vardim=vardim)
    else
        error("Invalid value of mean.")
    end
end

## nice-valued ranges for histograms

function histrange{T<:FloatingPoint,N}(v::AbstractArray{T,N}, n::Integer)
    if length(v) == 0
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
    nm1 = iceil((hi - start)/step)
    start:step:(start + nm1*step)
end

function histrange{T<:Integer,N}(v::AbstractArray{T,N}, n::Integer)
    if length(v) == 0
        return 0:1:0
    end
    lo, hi = extrema(v)
    if hi == lo
        step = 1
    else
        bw = (hi - lo) / n
        e = 10^max(0,ifloor(log10(bw)))
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
    nm1 = iceil((hi - start)/step)
    start:step:(start + nm1*step)
end

## midpoints of intervals
midpoints(r::Range) = r[1:length(r)-1] + 0.5*step(r)
midpoints(v::AbstractVector) = [0.5*(v[i] + v[i+1]) for i in 1:length(v)-1]

## hist ##
function sturges(n)  # Sturges' formula
    n==0 && return one(n)
    iceil(log2(n))+1
end

function hist!{HT}(h::AbstractArray{HT}, v::AbstractVector, edg::AbstractVector; init::Bool=true)
    n = length(edg) - 1
    length(h) == n || error("length(h) must equal length(edg) - 1.")
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
    size(H) == (length(edg)-1, n) || error("Incorrect size of H.")
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
    size(v,2) == 2 || error("hist2d requires an Nx2 matrix.")
    n = length(edg1) - 1
    m = length(edg2) - 1
    size(H) == (n, m) || error("Incorrect size of H.")
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


## quantiles ##

# for now, use the R/S definition of quantile; may want variants later
# see ?quantile in R -- this is type 7
function quantile!(v::AbstractVector, q::AbstractVector)
    isempty(v) && error("empty data array")
    isempty(q) && error("empty quantile array")

    # make sure the quantiles are in [0,1]
    q = bound_quantiles(q)

    lv = length(v)
    lq = length(q)

    index = 1 .+ (lv-1)*q
    lo = ifloor(index)
    hi = iceil(index)
    sort!(v)
    isnan(v[end]) && error("quantiles are undefined in presence of NaNs")
    i = find(index .> lo)
    r = float(v[lo])
    h = (index.-lo)[i]
    r[i] = (1.-h).*r[i] + h.*v[hi[i]]
    return r
end
quantile(v::AbstractVector, q::AbstractVector) = quantile!(copy(v),q)
quantile(v::AbstractVector, q::Number) = quantile(v,[q])[1]

function bound_quantiles(qs::AbstractVector)
    epsilon = 100*eps()
    if (any(qs .< -epsilon) || any(qs .> 1+epsilon))
        error("quantiles out of [0,1] range")
    end
    [min(1,max(0,q)) for q = qs]
end
