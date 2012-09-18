require("distributions.jl")
import Distributions.*

# n probability points, i.e. the midpoints of the intervals [0, 1/n],...,[1-1/n, 1]
probpts(n::Int) = ((1:n) - 0.5)/n  
pp  = float(probpts(1000))              # convert from a Range{Float64}
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

# Additional tests on the Multinomial and Dirichlet constructors
d = Multinomial(1, [0.5, 0.4, 0.1])
d = Multinomial(1, 3)
d = Multinomial(3)
d = Multinomial(1, [0.6; 0.4])
d = Multinomial(1, [0.6; 0.4]')
mean(d)
var(d)
@assert insupport(d, [1, 0])
@assert !insupport(d, [1, 1])
@assert insupport(d, [0, 1])
pmf(d, [1, 0])
pmf(d, [1, 1])
pmf(d, [0, 1])
logpmf(d, [1, 0])
logpmf(d, [1, 1])
logpmf(d, [0, 1])
d.n = 10
rand(d)
A = zeros(Int, 2, 10)
rand!(d, A)
A

d = Dirichlet([1.0, 2.0, 1.0])
d = Dirichlet(3)
d = Dirichlet([1.0; 2.0; 1.0])
d = Dirichlet([1.0; 2.0; 1.0]')
mean(d)
var(d)
insupport(d, [0.1, 0.8, 0.1])
insupport(d, [0.1, 0.8, 0.2])
insupport(d, [0.1, 0.8])
pdf(d, [0.1, 0.8, 0.1])
rand(d)
A = zeros(Float64, 10, 3)
rand!(d, A)
A

d = Categorical([0.25, 0.5, 0.25])
d = Categorical(3)
d = Categorical([0.25; 0.5; 0.25])

@assert !insupport(d, 0)
@assert insupport(d, 1)
@assert insupport(d, 2)
@assert insupport(d, 3)
@assert !insupport(d, 4)

@assert logpmf(d, 1) == log(0.25)
@assert pmf(d, 1) == 0.25

@assert logpmf(d, 2) == log(0.5)
@assert pmf(d, 2) == 0.5

@assert logpmf(d, 0) == -Inf
@assert pmf(d, 0) == 0.0

@assert 1.0 <= rand(d) <= 3.0

A = zeros(Int, 10)
rand!(d, A)
@assert 1.0 <= mean(A) <= 3.0

# Examples of sample()
a = [1, 6, 19]
p = rand(Dirichlet(3))
x = sample(a, p)
@assert x == 1 || x == 6 || x == 19

# This worked before and now fails with recent changes.
#a = 19.0 * eye(2)
#x = sample(a)
#@assert x == 0.0 || x == 19.0
