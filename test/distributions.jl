load("../base/distributions.jl")

# n probability points, i.e. the midpoints of the intervals [0, 1/n],...,[1-1/n, 1]
probpts(n::Int) = ((1:n) - 0.5)/n  
pp = float(probpts(1000))
lpp = log(pp)

tol = sqrt(eps())

function absdiff{T<:Real}(current::AbstractArray{T}, target::AbstractArray{T})
    @assert all(size(current) == size(target))
    max(abs(current - target))
end

function reldiff{T<:Real}(current::T, target::T)
    abs((current - target)/(bool(target) ? target : 1))
end

function reldiff{T<:Real}(current::AbstractArray{T}, target::AbstractArray{T})
    @assert all(size(current) == size(target))
    max([reldiff(current[i], target[i]) for i in 1:numel(target)])
end

## Checks on ContinuousDistribution instances
for d in (Beta(), Cauchy(), Chisq(12), Exponential(), Exponential(23.1),
          FDist(2, 21), Gamma(3), Gamma(), Logistic(), logNormal(),
          Normal(), TDist(1), TDist(28), Uniform(), Weibull(2.3))
##    println(d)  # uncomment if an assertion fails
    qq = quantile(d, pp)
    @assert absdiff(cdf(d, qq), pp) < tol
    @assert absdiff(ccdf(d, qq), 1 - pp) < tol
    @assert reldiff(cquantile(d, 1 - pp), qq) < tol
    @assert reldiff(logpdf(d, qq), log(pdf(d, qq))) < tol
    @assert reldiff(logcdf(d, qq), lpp) < tol
    @assert reldiff(logccdf(d, qq), lpp[end:-1:1]) < tol
    @assert reldiff(invlogcdf(d, lpp), qq) < tol
    @assert reldiff(invlogccdf(d, lpp), qq[end:-1:1]) < tol
## These tests are not suitable for routine use as they can fail due to sampling
## variability.
#    ss = rand(d, int(1e6))  
#    if isfinite(mean(d)) @assert reldiff(mean(ss), mean(d)) < 1e-3 end
#    if isfinite(std(d)) @assert reldiff(std(ss), std(d)) < 0.1 end
end

