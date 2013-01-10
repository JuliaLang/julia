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
mean(v::AbstractArray, dim::Int) = sum(v,dim)/size(v,dim)

weighted_mean(v::AbstractArray, w::AbstractArray) = sum(v.*w)/sum(w)

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

## median absolute deviation with known center with consistency adjustment
mad(v::AbstractArray, center::Number) = 1.4826 * median(abs(v - center))

## median absolute deviation
mad(v::AbstractArray) = mad(v, median(v))

## maximum likelihood estimate of skewness with known mean m
function skewness(v::AbstractVector, m::Number)
    n = length(v)
    empirical_third_centered_moment = 0.0
    empirical_variance = 0.0
    for x_i in v
        empirical_third_centered_moment += (x_i - m)^3
        empirical_variance += (x_i - m)^2
    end
    empirical_third_centered_moment /= n
    empirical_variance /= n
    return empirical_third_centered_moment / (empirical_variance^1.5)
end

## maximum likelihood estimate of skewness
skewness(v::AbstractVector) = skewness(v, mean(v))

## maximum likelihood estimate of kurtosis with known mean m
function kurtosis(v::AbstractVector, m::Number)
    n = length(v)
    empirical_fourth_centered_moment = 0.0
    empirical_variance = 0.0
    for x_i in v
        empirical_fourth_centered_moment += (x_i - m)^4
        empirical_variance += (x_i - m)^2
    end
    empirical_fourth_centered_moment /= n
    empirical_variance /= n
    return (empirical_fourth_centered_moment / (empirical_variance^2)) - 3.0
end

## maximum likelihood estimate of kurtosis
kurtosis(v::AbstractVector) = kurtosis(v, mean(v))

## distance matrix
function dist(m::AbstractMatrix)
    n = size(m, 1)
    d = Array(Float64, n, n)
    for i in 1:n
        d[i, i] = 0.0
        for j in (i + 1):n
            x = norm(m[i, :] - m[j, :])
            d[i, j] = x
            d[j, i] = x
        end
    end
    return d
end

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

function histc(v::StridedVector, edg)
    n = length(edg)
    h = zeros(Int, n)
    if n == 0
        return h
    end
    first = edg[1]
    last = edg[n]
    for x in v
        if !isless(last, x) && !isless(x, first)
            i = search_sorted(edg, x)
            while isless(x, edg[i])
                i -= 1
            end
            h[i] += 1
        end
    end
    h
end

function histc(A::StridedMatrix, edg)
    m, n = size(A)
    h = Array(Int, length(edg), n)
    for j=1:n
        h[:,j] = histc(sub(A, 1:m, j), edg)
    end
    h
end

## order (aka, rank), resolving ties using the mean rank
function tiedrank(v::AbstractArray)
    n     = length(v)
    place = order(v)
    ord   = Array(Float64, n)

    i = 1
    while i <= n
        j = i
        while j + 1 <= n && v[place[i]] == v[place[j + 1]]
            j += 1
        end

        if j > i
            m = sum(i:j) / (j - i + 1)
            for k = i:j
                ord[place[k]] = m
            end
        else
            ord[place[i]] = i
        end

        i = j + 1
    end

    return ord
end

## pearson covariance functions ##

# pearson covariance between two vectors, with known means
function cov_pearson1(x::AbstractArray, y::AbstractArray, mx::Number, my::Number, corrected::Bool)
    n = length(x)
    if n == 0 || (n == 1 && corrected)
        return NaN
    end
    x0 = x - mx
    y0 = y - my
    return (x0'*y0)[1] / (n - (corrected ? 1 : 0))
end

# pearson covariance between two vectors
function cov_pearson(x::AbstractVector, y::AbstractVector, corrected::Bool)
    if length(x) != length(y) 
        error("cov_pearson: incompatible dimensions")
    end

    mx = mean(x)
    my = mean(y)
    cov_pearson1(x, y, mx, my, corrected)
end
cov_pearson(x::AbstractVector, y::AbstractVector) = cov_pearson(x, y, true)

# pearson covariance over all pairs of columns of a matrix
function cov_pearson(x::AbstractMatrix, mxs::AbstractMatrix, corrected::Bool)
    n = size(x, 1)
    if n == 0 || (n == 1 && corrected)
        return NaN
    end
    x0 = x - repmat(mxs, n, 1)
    return (x0'*x0) / (n - (corrected ? 1 : 0))
end
cov_pearson(x::AbstractMatrix, corrected::Bool) = cov_pearson(x, mean(x, 1), corrected)
cov_pearson(x::AbstractMatrix) = cov_pearson(x, true)

# pearson covariance over all pairs of columns of two matrices
function cov_pearson(x::AbstractMatrix, y::AbstractMatrix,
                     mxs::AbstractMatrix, mys::AbstractMatrix,
                     corrected::Bool)
    n = size(x, 1)
    if n == 0 || (n == 1 && corrected)
        return NaN
    end
    x0 = x - repmat(mxs, n, 1)
    y0 = y - repmat(mys, n, 1)
    return (x0'*y0) / (n - (corrected ? 1 : 0))
end
function cov_pearson(x::AbstractMatrix, y::AbstractMatrix, corrected::Bool)
    if size(x) != size(y)
        error("cov_pearson: incompatible dimensions")
    end

    if is(x, y)
        return cov_pearson(x, corrected)
    end

    n = size(x, 1)
    mxs = mean(x, 1)
    mys = mean(y, 1)
    return cov_pearson(x, y, mxs, mys, corrected)
end
cov_pearson(x::AbstractMatrix, y::AbstractMatrix) = cov_pearson(x, y, true)

## spearman covariance functions ##

# spearman covariance between two vectors
function cov_spearman(x::AbstractVector, y::AbstractVector, corrected::Bool)
    cov_pearson(tiedrank(x), tiedrank(y), corrected)
end
cov_spearman(x::AbstractVector, y::AbstractVector) = cov_spearman(x, y, true)

# spearman covariance over all pairs of columns of a matrix
function cov_spearman(x::AbstractMatrix, corrected::Bool)
    cov_pearson(apply(hcat, amap(tiedrank, x, 2)), corrected)
end
cov_spearman(x::AbstractMatrix) = cov_spearman(x, true)

# spearman covariance over all pairs of columns of two matrices
function cov_spearman(x::AbstractMatrix, y::AbstractMatrix, corrected::Bool)
    if is(x, y)
        return cov_spearman(x, corrected)
    end

    cov_pearson(
        apply(hcat, amap(tiedrank, x, 2)),
        apply(hcat, amap(tiedrank, y, 2)),
        corrected)
end
cov_spearman(x::AbstractMatrix, y::AbstractMatrix) = cov_spearman(x, y, true)

const cov = cov_pearson

## pearson correlation functions ##

# pearson correlation between two vectors
function cor_pearson(x::AbstractVector, y::AbstractVector, corrected::Bool)
    if length(x) != length(y)
        error("cor_pearson: incompatible dimensions")
    end

    mx = mean(x)
    my = mean(y)
    sx = std(x, mx, corrected)
    sy = std(y, my, corrected)

    return cov_pearson1(x, y, mx, my, corrected) / (sx * sy)
end
cor_pearson(x::AbstractVector, y::AbstractVector) = cor_pearson(x, y, true)

# pearson correlation over all pairs of columns of a matrix
function cor_pearson{T}(x::AbstractMatrix{T}, corrected::Bool)
    (n,m) = size(x)
    mxs = mean(x, 1)
    sxs = similar(mxs)
    for i = 1:m
        sxs[i] = std(sub(x, (1:n, i)), mxs[i], corrected)
    end
    R = cov_pearson(x, mxs, corrected) ./ (sxs' * sxs)

    R[1:m+1:end] = one(T) # fix diagonal for numerical errors

    return R
end
cor_pearson(x::AbstractMatrix) = cor_pearson(x, true)

# pearson correlation over all pairs of columns of two matrices
function cor_pearson(x::AbstractMatrix, y::AbstractMatrix, corrected::Bool)
    if size(x) != size(y)
        error("cor_pearson: incompatible dimensions")
    end

    if is(x, y)
        return cor_pearson(x, corrected)
    end

    (n,m) = size(x)
    mxs = mean(x, 1)
    mys = mean(y, 1)
    sxs = similar(mxs)
    sys = similar(mys)
    for i = 1:m
        sxs[i] = std(sub(x, (1:n, i)), mxs[i], corrected)
        sys[i] = std(sub(y, (1:n, i)), mys[i], corrected)
    end

    return cov_pearson(x, y, mxs, mys, corrected) ./ (sxs' * sys)
end
cor_pearson(x::AbstractMatrix, y::AbstractMatrix) = cor_pearson(x, y, true)

## spearman correlation functions ##

# spearman correlation between two vectors
function cor_spearman(x::AbstractVector, y::AbstractVector, corrected::Bool)
    cor_pearson(tiedrank(x), tiedrank(y), corrected)
end
cor_spearman(x::AbstractVector, y::AbstractVector) = cor_spearman(x, y, true)

# spearman correlation over all pairs of columns of a matrix
function cor_spearman(x::AbstractMatrix, corrected::Bool)
    cor_pearson(apply(hcat, amap(tiedrank, x, 2)), corrected)
end
cor_spearman(x::AbstractMatrix) = cor_spearman(x, true)

# spearman correlation over all pairs of columns of two matrices
function cor_spearman(x::AbstractMatrix, y::AbstractMatrix, corrected::Bool)
    if is(x, y)
        return cor_spearman(x, corrected)
    end

    cor_pearson(
        apply(hcat, amap(tiedrank, x, 2)),
        apply(hcat, amap(tiedrank, y, 2)),
        corrected)
end
cor_spearman(x::AbstractMatrix, y::AbstractMatrix) = cor_spearman(x, y, true)

const cor = cor_pearson

## autocorrelation at a specific lag
autocor(v::AbstractVector, lag::Int) = cor(v[1:(end-lag)], v[(1 + lag):end])

## autocorrelation at a default lag of 1
autocor(v::AbstractVector) = autocor(v, 1)

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
quantile(v::AbstractVector, qs::AbstractVector) = quantile!(copy(v),qs)
quantile(v::AbstractVector, q::Number) = quantile(v,[q])[1]

  quantile(v::AbstractVector) = quantile(v,[.0,.25,.5,.75,1.0])
percentile(v::AbstractVector) = quantile(v,[1:99]/100)
  quartile(v::AbstractVector) = quantile(v,[.25,.5,.75])
  quintile(v::AbstractVector) = quantile(v,[.2,.4,.6,.8])
    decile(v::AbstractVector) = quantile(v,[.1,.2,.3,.4,.5,.6,.7,.8,.9])
       iqr(v::AbstractVector) = quantile(v,[0.25,0.75])

function bound_quantiles(qs::AbstractVector)
    epsilon = 100*eps()
    if (any(qs .< -epsilon) || any(qs .> 1+epsilon))
        error("quantiles out of [0,1] range!")
    end
    [min(1,max(0,q)) for q = qs]
end

## run-length encoding
function rle{T}(v::Vector{T})
    n = length(v)
    current_value = v[1]
    current_length = 1
    values = Array(T, n)
    total_values = 1
    lengths = Array(Int, n)
    total_lengths = 1
    for i in 2:n
        if v[i] == current_value
            current_length += 1
        else
            values[total_values] = current_value
            total_values += 1
            lengths[total_lengths] = current_length
            total_lengths += 1
            current_value = v[i]
            current_length = 1
        end
    end
    values[total_values] = current_value
    lengths[total_lengths] = current_length
    return (values[1:total_values], lengths[1:total_lengths])
end

## inverse run-length encoding
function inverse_rle{T}(values::Vector{T}, lengths::Vector{Int})
    total_n = sum(lengths)
    pos = 0
    res = Array(T, total_n)
    n = length(values)
    for i in 1:n
        v = values[i]
        l = lengths[i]
        for j in 1:l
            pos += 1
            res[pos] = v
        end
    end
    return res
end
