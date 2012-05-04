mean(v::AbstractArray) = sum(v)/numel(v)
mean(v::AbstractArray, dim::Int) = sum(v,dim)/size(v,dim)
weighted_mean(v::AbstractArray, w::AbstractArray) = sum(v.*w)/sum(w)

function median(v::AbstractArray)
    n = numel(v)
    if isodd(n)
        return select(v, div(n, 2))
    else
        vs = sort(v)
        return (vs[div(n, 2)] + vs[div(n, 2) + 1]) / 2
    end
end

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
var(v::AbstractArray, m::Number, corrected::Bool) = var(reshape(v, numel(v)), m, corrected)
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
var(v::AbstractArray, corrected::Bool) = var(reshape(v, numel(v)), corrected)
var(v::AbstractArray) = var(v, true)

## standard deviation with known mean
std(v::AbstractArray, m::Number, corrected::Bool) = sqrt(var(v, m, corrected))
std(v::AbstractArray, m::Number) = std(v, m, true)

## standard deviation
std(v::AbstractArray, corrected::Bool) = std(v, mean(v), corrected)
std(v::AbstractArray) = std(v, true)
std(v::Ranges, corrected::Bool) = sqrt(var(v, corrected))
std(v::Ranges) = std(v, true)

## median absolute deviation with known center
mad(v::AbstractArray, center::Number) = median(abs(v - center))

## median absolute deviation
mad(v::AbstractArray) = mad(v, median(v))

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
            i = searchsorted(edg, x)
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
function _jl_cov_pearson1(x::AbstractArray, y::AbstractArray, mx::Number, my::Number, corrected::Bool)
    n = numel(x)
    if n == 0 || (n == 1 && corrected)
        return NaN
    end
    x0 = x - mx
    y0 = y - my
    return (x0'*y0)[1] / (n - (corrected ? 1 : 0))
end

# pearson covariance between two vectors
function cov_pearson(x::AbstractVector, y::AbstractVector, corrected::Bool)
    if numel(x) != numel(y) 
        error("cov_pearson: incompatible dimensions")
    end

    mx = mean(x)
    my = mean(y)
    _jl_cov_pearson1(x, y, mx, my, corrected)
end
cov_pearson(x::AbstractVector, y::AbstractVector) = cov_pearson(x, y, true)

# pearson covariance over all pairs of columns of a matrix
function _jl_cov_pearson(x::AbstractMatrix, mxs::AbstractMatrix, corrected::Bool)
    n = size(x, 1)
    if n == 0 || (n == 1 && corrected)
        return NaN
    end
    x0 = x - repmat(mxs, n, 1)
    return (x0'*x0) / (n - (corrected ? 1 : 0))
end
cov_pearson(x::AbstractMatrix, corrected::Bool) = _jl_cov_pearson(x, mean(x, 1), corrected)
cov_pearson(x::AbstractMatrix) = cov_pearson(x, true)

# pearson covariance over all pairs of columns of two matrices
function _jl_cov_pearson(x::AbstractMatrix, y::AbstractMatrix,
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
    return _jl_cov_pearson(x, y, mxs, mys, corrected)
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
    if numel(x) != numel(y)
        error("cor_pearson: incompatible dimensions")
    end

    mx = mean(x)
    my = mean(y)
    sx = std(x, mx, corrected)
    sy = std(y, my, corrected)

    return _jl_cov_pearson1(x, y, mx, my, corrected) / (sx * sy)
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
    R = _jl_cov_pearson(x, mxs, corrected) ./ (sxs' * sxs)

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

    return _jl_cov_pearson(x, y, mxs, mys, corrected) ./ (sxs' * sys)
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

## quantiles ##

# for now, use the R/S definition of quantile; may want variants later
# see ?quantile in R -- this is type 7
function quantile(x, qs)
    # make sure the quantiles are in [0,1]
    bqs = _bound_quantiles(qs)
    
    lx = length(x)
    lqs = length(bqs)
    
    if lx > 0 && lqs > 0
        index = 1 + (lx-1) * bqs
        lo = int(floor(index))
        hi = int(ceil(index))
        sortedX = sort(x)
        i = index > lo
        ret = sortedX[lo]
        i = [1:length(i)][i]
        h = (index - lo)[i]
        ret[i] = (1-h) .* ret[i] + h .* sortedX[hi[i]]
    else
        ret = zeros(lqs) * NaN
    end
    
    ret
end
quantile(x, q::Number) = quantile(x, [q])[1]
quartile(x) = quantile(x, [.25, .5, .75])
quintile(x) = quantile(x, [.2:.2:.8])
decile(x) = quantile(x, [.1:.1:.9])

function _bound_quantiles(qs)
    epsilon = 100 * eps()
    if (any(qs < -epsilon) || any(qs > 1 + epsilon))
        error("quantiles out of [0,1] range!")
    end
    [min(1, max(0, q)) for q = qs]
end
