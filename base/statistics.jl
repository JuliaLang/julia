
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
    checknan && any(isnan,v) && error("median of an array with NaNs is undefined")
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


## variances

function varzm_pairwise(A::AbstractArray, i1::Int, n::Int)
    if n < 256
        @inbounds s = abs2(A[i1])
        for i=i1+1:i1+n-1
            @inbounds s += abs2(A[i])
        end
        return s
    else
        n2 = div(n,2)
        return varzm_pairwise(A, i1, n2) + varzm_pairwise(A, i1+n2, n-n2)
    end
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

function varzm(v::AbstractArray; corrected::Bool=true)
    n = length(v)
    n == 0 && return NaN
    return varzm_pairwise(v, 1, n) / (n - int(corrected))
end

function varm(v::AbstractArray, m::Number; corrected::Bool=true)
    n = length(v)
    n == 0 && return NaN
    return varm_pairwise(v, m, 1, n) / (n - int(corrected))
end

var(v::AbstractArray; corrected::Bool=true, zeromean::Bool=false) = 
    zeromean ? varzm(v; corrected=corrected) : varm(v, mean(v); corrected=corrected)

function var(v::AbstractArray, region; corrected::Bool=true, zeromean::Bool=false)
    cn = regionsize(v, region) - int(corrected)
    if zeromean
        return sum(abs2(v), region) / cn
    else
        return sum(abs2(v .- mean(v, region)), region) / cn
    end
end


## variances over ranges

varm(v::Ranges, m::Number) = var(v)

function var(v::Ranges)
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

std(v::AbstractArray; corrected::Bool=true, zeromean::Bool=false) = 
    sqrt(var(v; corrected=corrected, zeromean=zeromean))

std(v::AbstractArray, region; corrected::Bool=true, zeromean::Bool=false) = 
    sqrt!(var(v, region; corrected=corrected, zeromean=zeromean))


## nice-valued ranges for histograms

function histrange{T<:FloatingPoint,N}(v::AbstractArray{T,N}, n::Integer)
    if length(v) == 0
        return Range(0.0,1.0,1)
    end
    lo, hi = minimum(v), maximum(v)
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
    lo, hi = minimum(v), maximum(v)
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

function hist!{HT}(h::StoredArray{HT}, v::AbstractVector, edg::AbstractVector; init::Bool=true)
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

function hist!{HT}(H::StoredArray{HT,2}, A::AbstractMatrix, edg::AbstractVector; init::Bool=true)
    m, n = size(A)
    size(H) == (length(edg)-1, n) || error("Incorrect size of H.")
    if init
        fill!(H, zero(HT))
    end
    for j = 1:n
        hist!(sub(H(H, :, j), sub(A, :, j), edg))
    end
    edg, H
end

hist(A::AbstractMatrix, edg::AbstractVector) = hist!(Array(Int, length(edg-1), size(A,2)), A, edg)
hist(A::AbstractMatrix, n::Integer) = hist(A,histrange(A,n))
hist(A::AbstractMatrix) = hist(A,sturges(size(A,1)))


## hist2d
function hist2d!{HT}(H::StoredArray{HT,2}, v::AbstractMatrix, 
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

## pearson covariance functions ##

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
    size(x, 1)==size(y, 1) || throw(DimensionMismatch())
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
    scale = mapslices(std, x, 1)'*mapslices(std, y, 1)
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
