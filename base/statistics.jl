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

function median!{T<:Real}(v::AbstractVector{T})
    isempty(v) && error("median of an empty array is undefined")
    sort!(v) # TODO: do something more efficient, e.g. select but detect NaNs
    isnan(v[end]) && error("median is undefined in presence of NaNs")
    isodd(length(v)) ? float(v[div(end+1,2)]) : (v[div(end,2)]+v[div(end,2)+1])/2
end
median{T<:Real}(v::AbstractArray{T}) = median!(copy(v))

## variance with known mean
function var(v::AbstractVector, m::Number, corrected::Bool)
    n = length(v)
    if n == 0 || (n == 1 && corrected)
        return NaN
    end
    x = v - m
    return dot(x, x) / (n - (corrected ? 1 : 0))
end
var(v::AbstractVector, m::Number) = var(v, m, true)
var(v::AbstractArray, m::Number, corrected::Bool) = var(reshape(v, length(v)), m, corrected)
var(v::AbstractArray, m::Number) = var(v, m, true)
function var(v::Ranges, m::Number, corrected::Bool)
    f = first(v) - m
    s = step(v)
    l = length(v)
    if l == 0 || (l == 1 && corrected)
        return NaN
    end
    if corrected
        return f^2 * l / (l - 1) + f * s * l + s^2 * l * (2 * l - 1) / 6
    else
        return f^2 + f * s * (l - 1) + s^2 * (l - 1) * (2 * l - 1) / 6
    end
end
var(v::Ranges, m::Number) = var(v, m, true)

## variance
function var(v::Ranges, corrected::Bool)
    s = step(v)
    l = length(v)
    if l == 0 || (l == 1 && corrected)
        return NaN
    end
    return abs2(s) * (l + 1) * (corrected ? l : (l - 1)) / 12
end
var(v::AbstractVector, corrected::Bool) = var(v, mean(v), corrected)
var(v::AbstractArray, corrected::Bool) = var(reshape(v, length(v)), corrected)
var(v::AbstractArray) = var(v, true)

## standard deviation with known mean
std(v::AbstractArray, m::Number, corrected::Bool) = sqrt(var(v, m, corrected))
std(v::AbstractArray, m::Number) = std(v, m, true)

## standard deviation
std(v::AbstractArray, corrected::Bool) = std(v, mean(v), corrected)
std(v::AbstractArray) = std(v, true)
std(v::Ranges, corrected::Bool) = sqrt(var(v, corrected))
std(v::Ranges) = std(v, true)

## hist ##

function hist(v::StridedVector, nbins::Integer)
    h = zeros(Int, nbins)
    if nbins == 0
        return h
    end
    lo, hi = min(v), max(v)
    if lo == hi
        lo -= div(nbins,2)
        hi += div(nbins,2) + int(isodd(nbins))
    end
    binsz = (hi - lo) / nbins
    for x in v
        if isfinite(x)
            i = iround((x - lo) / binsz + 0.5)
            h[i > nbins ? nbins : i] += 1
        end
    end
    h
end

hist(x) = hist(x, 10)

function hist(A::StridedMatrix, nbins::Integer)
    m, n = size(A)
    h = Array(Int, nbins, n)
    for j=1:n
        h[:,j] = hist(sub(A, 1:m, j), nbins)
    end
    h
end

function hist(v::StridedVector, edg::AbstractVector)
    n = length(edg)
    h = zeros(Int, n)
    if n == 0
        return h
    end
    first = edg[1]
    last = edg[n]
    for x in v
        if !isless(last, x) && !isless(x, first)
            i = searchsortedlast(edg, x)
            h[i] += 1
        end
    end
    h
end

function hist(A::StridedMatrix, edg::AbstractVector)
    m, n = size(A)
    h = Array(Int, length(edg), n)
    for j=1:n
        h[:,j] = histc(sub(A, 1:m, j), edg)
    end
    h
end

## pearson covariance functions ##

function cov(x::AbstractVector, y::AbstractVector, corrected::Bool)
    n = length(x)
    if n != length(y); error("vectors must have same length"); end
    meanx = x[1]
    meany = y[1]
    C = zero(float(x[1]))
    for i = 2:n
        meanx += (x[i] - meanx) / i
        C += (x[i] - meanx)*(y[i] - meany)
        if i < n; meany += (y[i] - meany) / i; end
    end
    return C / (n - (corrected ? 1 : 0))
end
cov(X::AbstractMatrix, Y::AbstractMatrix, corrected::Bool) = [cov(X[:,i], Y[:,j], corrected) for i = 1:size(X, 2), j = 1:size(Y,2)]
cov(x::AbstractVector, Y::AbstractMatrix, corrected::Bool) = [cov(x, Y[:,i], corrected) for i = 1:size(Y, 2)]
cov(X::AbstractMatrix, y::AbstractVector, corrected::Bool) = [cov(X[:,i], y, corrected) for i = 1:size(X, 2)]
function cov(X::AbstractMatrix, corrected::Bool)
    n = size(X, 2)
    C = Array(typeof(float(X[1])), n, n)
    for i = 1:n
        for j = i:n
            if i == j
                C[i,i] = var(X[:,i], corrected)
            else
                C[i,j] = cov(X[:,i], X[:,j], corrected)
                C[j,i] = C[i,j]
            end
        end
    end
    return C
end
cov(x) = cov(x, true)
cov(x, y) = cov(x, y, true)

## pearson correlation functions ##

# pearson correlation between two vectors
cor(x::AbstractVector, y::AbstractVector, corrected::Bool) = cov(x, y, corrected) / (std(x, corrected)*std(y, corrected))

# pearson correlation over all pairs of columns of two matrices
cor(X::AbstractMatrix, Y::AbstractMatrix, corrected::Bool) = [cor(X[:,i], Y[:,j], corrected) for i = 1:size(X, 2), j = 1:size(Y,2)]
cor(x::AbstractVector, Y::AbstractMatrix, corrected::Bool) = [cor(x, Y[:,i], corrected) for i = 1:size(Y, 2)]
cor(X::AbstractMatrix, y::AbstractVector, corrected::Bool) = [cor(X[:,i], y, corrected) for i = 1:size(X, 2)]

# pearson correlation over all pairs of columns of a matrix
function cor(X::AbstractMatrix, corrected::Bool) 
    vsd = amap(x -> std(x, corrected), X, 2)
    return cov(X, corrected) ./ (vsd*vsd')
end

cor(x) = cor(x, true)
cor(x, y) = cor(x, y, true)

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
