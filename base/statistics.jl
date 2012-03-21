mean(v::AbstractArray) = sum(v) / numel(v)
mean(v::AbstractArray, dim::Int) = sum(v,dim) / size(v,dim)

weighted_mean(v::AbstractArray, w::AbstractArray) =
    sum(v .* w) / sum(w)

function median(v::AbstractArray)
    n = numel(v)
    if isodd(n)
        return select(v, div(n, 2))
    else
        vs = sort(v)
        return (vs[div(n, 2)] + vs[div(n, 2) + 1]) / 2
    end
end

# variance with known mean
function var(v::AbstractArray, m::Number)
    n = numel(v)
    d = 0.0
    for i = 1:n
        d += (v[i] - m) ^ 2
    end
    return d / (n - 1)
end

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
    place = sort_by(i -> v[i], 1:n)
    ord   = Array(Float, n)

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
function _jl_cov_pearson1(x::AbstractVector, y::AbstractVector, mx::Number, my::Number)
    n = numel(x)
    r = 0.0
    for i = 1:n
        r += (x[i] - mx) * (y[i] - my)
    end
    r / (n - 1)
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
function _jl_cov_pearson{T}(x::AbstractMatrix, mxs::AbstractVector{T})
    (n,m) = size(x)
    R = Array(T, (m,m))
    for i = 1:m
        R[i,i] = _jl_cov_pearson1(sub(x, (1:n, i)),
                                  sub(x, (1:n, i)),
                                  mxs[i], mxs[i])

        for j = (i+1):m
            R[i,j] = _jl_cov_pearson1(sub(x, (1:n, i)),
                                      sub(x, (1:n, j)),
                                      mxs[i], mxs[j])
            R[j,i] = R[i,j]
        end
    end
    return R
end
cov_pearson(x::AbstractMatrix) = _jl_cov_pearson(x, amap(mean, x, 2))

# pearson covariance over all pairs of columns with known means
function _jl_cov_pearson{T}(x::AbstractMatrix, y::AbstractMatrix, 
                            mxs::AbstractVector{T}, mys::AbstractVector{T})
    (n,m) = size(x)
    R = Array(T, (m,m))
    for i = 1:m
        for j = 1:m
            R[i,j] = _jl_cov_pearson1(sub(x, (1:n, i)),
                                      sub(y, (1:n, j)),
                                      mxs[i], mys[j])
        end
    end
    return R
end

# pearson covariance over all pairs of columns
function cov_pearson(x::AbstractMatrix, y::AbstractMatrix)
    if size(x) != size(y)
        error("cov_pearson: incompatible dimensions")
    end

    if is(x, y)
        return cov_pearson(x)
    end

    _jl_cov_pearson(x, y, amap(mean, x, 2), amap(mean, y, 2))
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
function cor_pearson(x::AbstractMatrix)
    (n,m) = size(x)
    mxs = amap(mean, x, 2)
    sxs = similar(mxs)
    for i = 1:m
        sxs[i] = std(x[:,i], mxs[i])
    end
    R = _jl_cov_pearson(x, mxs)

    for i = 1:m
        R[i,i] = 1.0
        for j = (i+1):m
            R[i,j] /= sxs[i] * sxs[j]
            R[j,i] = R[i,j]
        end
    end
    return R
end

# pearson correlation over all pairs of columns
function cor_pearson(x::AbstractMatrix, y::AbstractMatrix)
    if size(x) != size(y)
        error("cor_pearson: incompatible dimensions")
    end

    if is(x, y)
        return cor_pearson(x)
    end

    (n,m) = size(x)
    mxs = amap(mean, x, 2)
    mys = amap(mean, y, 2)

    sxs = similar(mxs)
    sys = similar(mys)
    for i = 1:m
        sxs[i] = std(x[:,i], mxs[i])
        sys[i] = std(y[:,i], mys[i])
    end
    R = _jl_cov_pearson(x, y, mxs, mys)

    for i = 1:m
        for j = 1:m
            R[i,j] /= sxs[i] * sys[j]
        end
    end
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

