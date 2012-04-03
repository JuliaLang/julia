type Link
    name::String          # name of the link function
    linkFun::Function     # link function mu -> eta
    linkInv::Function     # inverse link  eta -> mu
    muEta::Function       # derivative d mu/d eta as a function of eta
end

logitLink =
    Link("logit",
         mu  -> log(mu ./ (1 - mu)),
         eta -> 1 ./ (1. + exp(-eta)),
         eta -> (x = abs(eta); e = exp(-x); f = 1. + e; e ./ (f .* f)))

logLink =
    Link("log",
         mu  -> log(mu),
         eta -> exp(eta),
         eta -> exp(eta))

identityLink =
    Link("identity",
         mu  -> mu,
         eta -> eta,
         eta -> ones(eltype(eta), size(eta)))

inverseLink =
    Link("inverse",
         mu  -> 1 ./ mu,
         eta -> 1 ./ eta,
         eta -> -1. ./ (eta .* eta))

type Dist
    name::String             # the name of the distribution
    canonical::Link          # the canonical link for the distribution
    variance::Function       # variance function (mu)-> var
    devResid::Function       # vector of squared deviance residuals
    deviance::Function       # the scalar deviance
    mustart::Function        # derive a starting estimate for mu
    validmu::Function        # check validity of the mu vector
    valideta::Function       # check validity of the eta vector
end

## utilities used in some distributions
logN0(x::Number) = x == 0 ? x : log(x)
logN0{T<:Number}(x::AbstractArray{T}) = reshape([ logN0(x[i]) | i=1:numel(x) ], size(x))
y_log_y(y, mu) = y .* logN0(y ./ mu)

BernoulliDist =
    Dist("Bernoulli",
         logitLink,
         mu  -> max(eps(Float64), mu .* (1. - mu)),
         (y, mu, wt)-> 2 * wt .* (y_log_y(y, mu) +  y_log_y(1. - y, 1. - mu)),
         (y, mu, wt)-> -2. * sum(y .* log(mu) + (1. - y) .* log(1. - mu)),
         (y, wt)-> (wt .* y + 0.5) ./ (wt + 1.),
         mu  -> all((0 < mu) & (mu < 1)),
         eta -> true)


GaussianDist =
    Dist("Gaussian",
         identityLink,
         mu  -> ones(typeof(mu), size(mu)),
         (y, mu, wt)-> (r = y - mu; wt .* r .* r),
         (y, mu, wt)-> (n = length(mu); r = y - mu; n * (log(2*pi*sum(wt .* r .* r)/n) + 1) + 2 - sum(log(wt))),
         (y, wt)-> y,
         mu  -> true,
         eta -> true)


PoissonDist =
    Dist("Poisson",
         logLink,
         mu  -> mu,
         (y, mu, wt)-> 2 * wt .* (y .* logN0(y ./ mu) - (y - mu)),
         (y, mu, wt)-> -2 * sum(dpois(y, mu, true) * wt),
         (y, mu)-> y + 0.1,
         mu  -> all(mu > 0),
         eta -> true)

         
type GlmResp
    dist::Dist                  
    link::Link
    eta::Vector{Float64}        # linear predictor
    mu::Vector{Float64}         # mean response
    offset::Vector{Float64}     # offset added to linear predictor (usually 0)
    wts::Vector{Float64}        # prior weights
    y::Vector{Float64}          # response
end

## outer constructor - the most common way of creating the object
function GlmResp(dist::Dist, link::Link, y::Vector{Float64})
    n  = length(y)
    wt = ones(Float64, (n,))
    mu = dist.mustart(y, wt)
    GlmResp(dist, link, link.linkFun(mu), mu, zeros(Float64, (n,)), wt, y)
end

## another outer constructor using the canonical link for the distribution
GlmResp(dist::Dist, y::Vector{Float64}) = GlmResp(dist, dist.canonical, y)

updateMu{T<:Number}(r::GlmResp, linPr::AbstractArray{T}) = (r.eta = linPr + r.offset; r.mu = r.link.linkInv(r.eta); nothing)
deviance( r::GlmResp) = r.dist.deviance(r.y, r.mu, r.wts)
devResid( r::GlmResp) = r.dist.devResid(r.y, r.mu, r.wts)
drsum(    r::GlmResp) = sum(devResid(r))
muEta(    r::GlmResp) = r.link.muEta(r.eta)
sqrtWrkWt(r::GlmResp) = muEta(r) .* sqrt(r.wts ./ variance(r))
variance( r::GlmResp) = r.dist.variance(r.mu)
wrkResid( r::GlmResp) = (r.y - r.mu) ./ r.link.muEta(r.eta)
wrkResp(  r::GlmResp) = (r.eta - r.offset) + wrkResid(r)

function accumulate(rr, X)
    w   = sqrtWrkWt(rr)
    wX  = diagmm(w, X)
    (wX' * (w .* wrkResp(rr))), (wX' * wX)
end

iterate(rr, X) = ((wXz, wXtwX) = accumulate(rr, X); bb = wXtwX \ wXz; updateMu(rr, X * bb))

## put at the end of the file because it screws up the indentation for what follows

gammaDist =
    Dist("gamma",
         inverseLink,
         mu -> mu .* mu,
         (y, mu, wt)-> -2 * wt .* (logN0(y ./ mu) - (y - mu) ./ mu),
         (y, mu, wt, drsum)-> (n=sum(wt); disp=sum(-2 * wt .* (logN0(y ./ mu) - (y - mu) ./ mu))/n; invdisp(1/disp); sum(wt .* dgamma(y, invdisp, mu * disp, true))),
         (y, wt)-> all(y > 0) ? y : error("non-positive response values not allowed for gammaDist"),
         mu  -> all(mu > 0.),
         eta -> all(eta > 0.))
