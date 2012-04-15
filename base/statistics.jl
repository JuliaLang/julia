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
# generic version: only found to be faster for ranges
function var(v::Ranges, m::Number)
    n = numel(v)
    d = 0.0
    for x in v
        d += abs2(x - m)
    end
    return d / (n - 1)
end
# vectorized version
function var(v::AbstractVector, m::Number)
    n = length(v)
    x = v - m
    return (x'*x)[1] / (n - 1)
end
var(v::AbstractArray, m::Number) = var(reshape(v, numel(v)), m)

# variance
var(v::AbstractArray) = var(v, mean(v))

# standard deviation with known mean
function std(v::AbstractArray, m::Number)
    sqrt(var(v, m))
end

# standard deviation
std(v::AbstractArray) = std(v, mean(v))

# median absolute deviation with known center
mad(v::AbstractArray, center::Number) = median(abs(v - center))

#median absolute deviation
mad(v::AbstractArray) = mad(v, median(v))

## hist ##

function hist(v::StridedVector, nbins::Integer)
    h = zeros(Int, nbins)
    if nbins == 0
        return h
    end
    lo, hi = min(v), max(v)
    if lo == hi
        lo = lo - div(nbins,2)
        hi = hi + div(nbins,2)
    end
    binsz = (hi-lo)/nbins
    for x in v
        if isfinite(x)
            i = iround((x-lo+binsz/2)/binsz)
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
        i = 1+(j-1)*m
        h[:,j] = hist(sub(A, i:(i+m-1)), nbins)
    end
    h
end

function histc(v::StridedVector, edg)
    n = length(edg)
    h = zeros(Int, n)
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
        i = 1+(j-1)*m
        h[:,j] = histc(sub(A, i:(i+m-1)), edg)
    end
    h
end

# order (aka, rank), resolving ties using the mean rank
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

# pearson covariance with known means
function _jl_cov_pearson1(x::AbstractArray, y::AbstractArray, mx::Number, my::Number)
    n = numel(x)
    x0 = x - mx
    y0 = y - my
    return (x0'*y0)[1] / (n - 1)
end

# pearson covariance
function cov_pearson(x::AbstractVector, y::AbstractVector)
    if numel(x) != numel(y) 
        error("cov_pearson: incompatible dimensions")
    end

    mx = mean(x)
    my = mean(y)
    _jl_cov_pearson1(x, y, mx, my)
end

# pearson covariance over all pairs of columns
function _jl_cov_pearson(x::AbstractMatrix, mxs::AbstractMatrix)
    n = size(x, 1)
    x0 = x - repmat(mxs, n, 1)
    return (x0'*x0) / (n - 1)
end
cov_pearson(x::AbstractMatrix) = cov_pearson(x, mean(x, 1))

# pearson covariance over all pairs of columns with known means
function _jl_cov_pearson(x::AbstractMatrix, y::AbstractMatrix,
                     mxs::AbstractMatrix, mys::AbstractMatrix)
    n = size(x, 1)
    x0 = x - repmat(mxs, n, 1)
    y0 = y - repmat(mys, n, 1)
    return (x0'*y0) / (n - 1)
end

# pearson covariance over all pairs of columns
function cov_pearson(x::AbstractMatrix, y::AbstractMatrix)
    if size(x) != size(y)
        error("cov_pearson: incompatible dimensions")
    end

    if is(x, y)
        return cov_pearson(x)
    end

    n = size(x, 1)
    mx = mean(x, 1)
    my = mean(y, 1)
    return _jl_cov_pearson(x, y, mxs, mys)
end

# spearman covariance
function cov_spearman(x::AbstractVector, y::AbstractVector)
    cov_pearson(tiedrank(x), tiedrank(y))
end

# spearman covariance over all pairs of columns
function cov_spearman(x::AbstractMatrix)
    cov_pearson(apply(hcat, amap(tiedrank, x, 2)))
end

# spearman covariance over all pairs of columns
function cov_spearman(x::AbstractMatrix, y::AbstractMatrix)
    if is(x, y)
        return cov_spearman(x)
    end

    cov_pearson(
        apply(hcat, amap(tiedrank, x, 2)),
        apply(hcat, amap(tiedrank, y, 2)))
end

const cov = cov_pearson

# pearson correlation
function cor_pearson(x::AbstractVector, y::AbstractVector)
    if numel(x) != numel(y)
        error("cor_pearson: incompatible dimensions")
    end

    mx = mean(x)
    my = mean(y)
    sx = std(x, mx)
    sy = std(y, my)

    r = _jl_cov_pearson1(x, y, mx, my)
    r / (sx * sy)
end

# pearson correlation over all pairs of columns
function cor_pearson{T}(x::AbstractMatrix{T})
    (n,m) = size(x)
    mxs = mean(x, 1)
    sxs = similar(mxs)
    for i = 1:m
        sxs[i] = std(sub(x, (1:n, i)), mxs[i])
    end
    R = _jl_cov_pearson(x, mxs) ./ (sxs' * sxs)

    R[1:m+1:end] = one(T)

    return R
end

# pearson correlation over all pairs of columns
function cor_pearson{T}(x::AbstractMatrix{T}, y::AbstractMatrix{T})
    if size(x) != size(y)
        error("cor_pearson: incompatible dimensions")
    end

    if is(x, y)
        return cor_pearson(x)
    end

    (n,m) = size(x)
    mxs = mean(x, 1)
    mys = mean(y, 1)
    sxs = similar(mxs)
    sys = similar(mys)
    for i = 1:m
        sxs[i] = std(sub(x, (1:n, i)), mxs[i])
        sys[i] = std(sub(y, (1:n, i)), mys[i])
    end
    R = _jl_cov_pearson(x, y, mxs, mys) ./ (sxs' * sys)

    return R
end

# spearman correlation
function cor_spearman(x::AbstractVector, y::AbstractVector)
    cor_pearson(tiedrank(x), tiedrank(y))
end

# spearman correlation over all pairs of columns
function cor_spearman(x::AbstractMatrix)
    cor_pearson(apply(hcat, amap(tiedrank, x, 2)))
end

# spearman correlation over all pairs of columns
function cor_spearman(x::AbstractMatrix, y::AbstractMatrix)
    if is(x, y)
        return cor_spearman(x)
    end

    cor_pearson(
        apply(hcat, amap(tiedrank, x, 2)),
        apply(hcat, amap(tiedrank, y, 2)))
end

const cor = cor_pearson

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
    [min(1, max(0, q)) | q = qs]
end
