# This file is a part of Julia. License is MIT: http://julialang.org/license

# Benchmark European option
# https://groups.google.com/forum/?hl=en&fromgroups=#!topic/julia-dev/ImhGsqX_IHc

function bench_eu_devec(numPaths)
    steps = 250
    r = 0.05
    sigma = .4
    T = 1
    dt = T/(steps)
    K = 100

    S = 100 * ones(numPaths,1)

    t1 = (r-0.5*sigma.^2)*dt
    t2 = sigma*sqrt(dt)
    for i=1:steps
        for j=1:numPaths
            S[j] .*= exp(t1 + t2*randn())
        end
    end

    V = mean( exp(-r*T)*max(K.-S,0) )
end

function bench_eu_vec(numPaths)
    steps = 250
    r = 0.05
    sigma = .4
    T = 1
    dt = T/(steps)
    K = 100

    S = 100 * ones(numPaths,1)

    t1 = (r-0.5*sigma.^2)*dt
    t2 = sigma*sqrt(dt)
    for i=1:steps
        S = S .* exp(t1.+t2*randn(numPaths))
    end

    V = mean( exp(-r*T)*max(K.-S,0) )
end
