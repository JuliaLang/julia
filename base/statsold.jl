mean(v::AbstractArray, dim::Int) = sum(v,dim)/size(v,dim)

weighted_mean(v::AbstractArray, w::AbstractArray) = sum(v.*w)/sum(w)

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

## order (aka, rank), resolving ties using the mean rank
function tiedrank(v::AbstractArray)
    n     = length(v)
    place = sortperm(v)
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
tiedrank(X::AbstractMatrix) = tiedrank(reshape(X, length(X)))
function tiedrank(X::AbstractMatrix, dim::Int)
    retmat = apply(hcat, amap(tiedrank, X, 3 - dim))
    return dim == 1 ? retmat : retmat'
end

## spearman covariance functions ##

# spearman covariance between two vectors
cov_spearman(x::AbstractVector, y::AbstractVector, corrected::Bool) = cov(tiedrank(x), tiedrank(y), corrected)

# spearman covariance over all pairs of columns of two matrices
cov_spearman(X::AbstractMatrix, Y::AbstractMatrix, corrected::Bool) = [cov_spearman(X[:,i], Y[:,j], corrected) for i = 1:size(X, 2), j = 1:size(Y,2)]
cov_spearman(x::AbstractVector, Y::AbstractMatrix, corrected::Bool) = [cov_spearman(x, Y[:,i], corrected) for i = 1:size(Y, 2)]
cov_spearman(X::AbstractMatrix, y::AbstractVector, corrected::Bool) = [cov_spearman(X[:,i], y, corrected) for i = 1:size(X, 2)]

# spearman covariance over all pairs of columns of a matrix
cov_spearman(X::AbstractMatrix, corrected::Bool) = cov(tiedrank(X, 1), corrected)

cov_spearman(x) = cov_spearman(x, true)
cov_spearman(x, y) = cov_spearman(x, y, true)

## spearman correlation functions ##

# spearman correlation between two vectors
cor_spearman(x::AbstractVector, y::AbstractVector, corrected::Bool) = cor(tiedrank(x), tiedrank(y), corrected)

# spearman correlation over all pairs of columns of two matrices
cor_spearman(X::AbstractMatrix, Y::AbstractMatrix, corrected::Bool) = cor(tiedrank(X, 1), tiedrank(Y, 1))
cor_spearman(X::AbstractMatrix, y::AbstractVector, corrected::Bool) = cor(tiedrank(X, 1), tiedrank(y))
cor_spearman(x::AbstractVector, Y::AbstractMatrix, corrected::Bool) = cor(tiedrank(x), tiedrank(Y, 1))

# spearman correlation over all pairs of columns of a matrix
cor_spearman(X::AbstractMatrix, corrected::Bool) = cor(tiedrank(X, 1), corrected)

cor_spearman(x) = cor_spearman(x, true)
cor_spearman(x, y) = cor_spearman(x, y, true)

## autocorrelation at a specific lag
autocor(v::AbstractVector, lag::Int) = cor(v[1:end-lag], v[1+lag:end])

## autocorrelation at a default lag of 1
autocor(v::AbstractVector) = autocor(v, 1)

  quantile(v::AbstractVector) = quantile(v,[.0,.25,.5,.75,1.0])
percentile(v::AbstractVector) = quantile(v,[1:99]/100)
  quartile(v::AbstractVector) = quantile(v,[.25,.5,.75])
  quintile(v::AbstractVector) = quantile(v,[.2,.4,.6,.8])
    decile(v::AbstractVector) = quantile(v,[.1,.2,.3,.4,.5,.6,.7,.8,.9])
       iqr(v::AbstractVector) = quantile(v,[0.25,0.75])

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

## old stats tests ##

@test abs(autocor([1, 2, 3, 4, 5]) - 1.0) < 10e-8

@test iqr([1, 2, 3, 4, 5]) == [2.0, 4.0]

z = [true, true, false, false, true, false, true, true, true]
values, lengths = rle(z)
@test values == [true, false, true, false, true]
@test lengths == [2, 2, 1, 1, 3]
@test inverse_rle(values, lengths) == z

z = [true, true, false, false, true, false, true, true, true, false]
values, lengths = rle(z)
@test values == [true, false, true, false, true, false]
@test lengths == [2, 2, 1, 1, 3, 1]
@test inverse_rle(values, lengths) == z

m = [1 0; 0 1]
d = [0.0 sqrt(2); sqrt(2) 0.0]
@test norm(dist(m) - d) < 10e-8

m = [3.0 1.0; 5.0 1.0]
d = [0.0 2.0; 2.0 0.0]
@test norm(dist(m) - d) < 10e-8

m = [1 0 0; 0 1 0 ; 1 0 1]
d = [0.0 sqrt(2) 1.0; sqrt(2) 0.0 sqrt(3); 1.0 sqrt(3) 0.0]
@test norm(dist(m) - d) < 10e-8

@assert_approx_eq cov_spearman(X, y)[1] cov_spearman(X[:,1],y)
@assert_approx_eq cov_spearman(X) cov_spearman(X, X)
@assert_approx_eq cov_spearman(X, y) [-0.25, -0.1875]
