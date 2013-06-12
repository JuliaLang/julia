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
mean(v::AbstractArray, region) = sum(v, region) / prod(size(v)[region])

function median!{T<:Real}(v::AbstractVector{T})
    isempty(v) && error("median of an empty array is undefined")
    any(isnan,v) && error("median of an array with NaNs is undefined")
    n = length(v)
    isodd(n) ? select!(v,div(n+1,2)) : (select!(v,div(n,2))+select!(v,div(n,2)+1))/2
end
median{T<:Real}(v::AbstractArray{T}) = median!(copy(vec(v)))

## variance with known mean
function varm(v::AbstractVector, m::Number)
    n = length(v)
    if n == 0 || n == 1
        return NaN
    end
    x = v - m
    return dot(x, x) / (n - 1)
end
varm(v::AbstractArray, m::Number) = varm(vec(v), m)
varm(v::Ranges, m::Number) = var(v)

## variance
function var(v::Ranges)
    s = step(v)
    l = length(v)
    if l == 0 || l == 1
        return NaN
    end
    return abs2(s) * (l + 1) * l / 12
end
var(v::AbstractArray) = varm(v, mean(v))
function var(v::AbstractArray, region)
    x = bsxfun(-, v, mean(v, region))
    return sum(x.^2, region) / (prod(size(v)[region]) - 1)
end

## standard deviation with known mean
stdm(v, m::Number) = sqrt(varm(v, m))

## standard deviation
std(v) = sqrt(var(v))
std(v, region) = sqrt(var(v, region))

## nice-valued ranges for histograms
function histrange{T<:FloatingPoint,N}(v::AbstractArray{T,N}, n::Integer)
    if length(v) == 0
        return Range(0.0,1.0,1)
    end
    lo, hi = min(v), max(v)
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
    Range(start,step,1+iceil((hi - start)/step))
end

function histrange{T<:Integer,N}(v::AbstractArray{T,N}, n::Integer)
    if length(v) == 0
        return Range(0,1,1)
    end
    lo, hi = min(v), max(v)
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
    start = step*(iceil(lo/step)-1)
    Range(start,step,1+iceil((hi - start)/step))
end

## midpoints of intervals
midpoints(r::Ranges) = r[1:length(r)-1] + 0.5*step(r)
midpoints(v::AbstractVector) = [0.5*(v[i] + v[i+1]) for i in 1:length(v)-1]

## hist ##
function sturges(n)  # Sturges' formula
    n==0 && return one(n)
    iceil(log2(n))+1
end

hist(v::AbstractVector, n::Integer) = hist(v,histrange(v,n))
hist(v::AbstractVector) = hist(v,sturges(length(v)))

function hist(v::AbstractVector, edg::AbstractVector)
    n = length(edg)-1
    h = zeros(Int, n)
    for x in v
        i = searchsortedfirst(edg, x)-1
        if 1 <= i <= n
            h[i] += 1
        end
    end
    edg,h
end

function hist(A::AbstractMatrix, edg::AbstractVector)
    m, n = size(A)
    H = Array(Int, length(edg)-1, n)
    for j = 1:n
        _,H[:,j] = hist(sub(A, 1:m, j), edg)
    end
    edg,H
end
hist(A::AbstractMatrix, n::Integer) = hist(A,histrange(A,n))
hist(A::AbstractMatrix) = hist(A,sturges(size(A,1)))

function hist2d(v::AbstractMatrix, edg1::AbstractVector, edg2::AbstractVector)
    @assert size(v,2) == 2
    n = length(edg1)-1
    m = length(edg2)-1
    h = zeros(Int, n, m)
    for i = 1:size(v,1)
        x = searchsortedfirst(edg1, v[i, 1])-1
        y = searchsortedfirst(edg2, v[i, 2])-1
        if 1 <= x <= n && 1 <= y <= m
            h[x,y] += 1
        end
    end
    edg1,edg2,h
end
hist2d(v::AbstractMatrix, edg::AbstractVector) = hist2d(v, edg, edg)
function hist2d(v::AbstractMatrix, n::Integer)
    m = size(v,1)
    hist2d(v, histrange(sub(v, 1:m, 1),n), histrange(sub(v, 1:m, 2),n))
end
function hist2d(v::AbstractMatrix, n1::Integer, n2::Integer)
    m = size(v,1)
    hist2d(v, histrange(sub(v, 1:m,1),n1), histrange(sub(v, 1:m,2),n2))
end
hist2d(v::AbstractMatrix) = hist2d(v, sturges(size(v,1)))

## pearson covariance functions ##

typealias AbstractVecOrMat{T} Union(AbstractVector{T}, AbstractMatrix{T})

function center(x::AbstractMatrix)
    m,n = size(x)
    res = Array(promote_type(eltype(x),Float64), size(x))
    for j in 1:n
        colmean = mean(x[:,j])
        for i in 1:m
            res[i,j] = x[i,j] - colmean 
        end
    end
    res
end

function center(x::AbstractVector)
    colmean = mean(x)
    res = Array(promote_type(eltype(x),Float64), size(x))
    for i in 1:length(x)
        res[i] = x[i] - colmean 
    end
    res
end

function cov(x::AbstractVecOrMat, y::AbstractVecOrMat)
    if size(x, 1) != size(y, 1)
        error("incompatible matrices")
    end
    n = size(x, 1)
    xc = center(x)
    yc = center(y)
    conj(xc' * yc / (n - 1))
end
cov(x::AbstractVector, y::AbstractVector) = cov(x'', y)[1]

function cov(x::AbstractVecOrMat)
    n = size(x, 1)
    xc = center(x)
    conj(xc' * xc / (n - 1))
end
cov(x::AbstractVector) = cov(x'')[1]

function cor(x::AbstractVecOrMat, y::AbstractVecOrMat)
    z = cov(x, y)
    scale = Base.amap(std, x, 2) * Base.amap(std, y, 2)'
    z ./ scale
end
cor(x::AbstractVector, y::AbstractVector) =
    cov(x, y) / std(x) / std(y)
    

function cor(x::AbstractVecOrMat)
    res = cov(x)
    n = size(res, 1)
    scale = 1 / sqrt(diag(res))
    for j in 1:n
        for i in 1 : j - 1
            res[i,j] *= scale[i] * scale[j] 
            res[j,i] = res[i,j]
        end
        res[j,j] = 1.0
    end
    res 
end
cor(x::AbstractVector) = cor(x'')[1]

## quantiles ##

# for now, use the R/S definition of quantile; may want variants later
# see ?quantile in R -- this is type 7
function quantile!(v::AbstractVector, q::AbstractVector)
    isempty(v) && error("quantile: empty data array")
    isempty(q) && error("quantile: empty quantile array")

    # make sure the quantiles are in [0,1]
    q = bound_quantiles(q)

    lv = length(v)
    lq = length(q)

    index = 1 + (lv-1)*q
    lo = ifloor(index)
    hi = iceil(index)
    sort!(v)
    isnan(v[end]) && error("quantiles are undefined in presence of NaNs")
    i = find(index .> lo)
    r = float(v[lo])
    h = (index-lo)[i]
    r[i] = (1-h).*r[i] + h.*v[hi[i]]
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
