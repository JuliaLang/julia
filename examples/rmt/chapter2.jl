# code 2.5
function patiencesort(p)
    piles = similar(p, 0)
    for i = 1:length(p)
        idx = 1+sum(p[i]>piles)
        if idx > length(piles)
            d = idx-length(piles)
            grow(piles, d)
            piles[end-d+1:end] = 0
        end
        piles[idx] = p[i]
    end
    return length(piles)
end

# code 2.6
function unitarylis()
    t = 200000
    n = 4
    k = 2
    v = zeros(t)
    for i = 1:t
        (X, _) = qr(complex(randn(k,k), randn(k,k)))
        X = X*diagm(sign(complex(randn(k),randn(k))))
        v[i] = abs(trace(X)) ^ (2n)
    end
    z = mean(v)
    c = 0
    for i=1:factorial(n)
        c = c + int(patiencesort(nthperm([1:n],i))<=k)
    end
    return [z c]
end

# code 2.7
function tracywidomlis()
    t = 10000
    n = 6^5
    dx = 1/6
    v = zeros(t)
    for i=1:t
        v[i] = patiencesort(randperm(n))
    end
    w = (v-2sqrt(n))/n^(1/6)
    return hist(w, 42)/(t*dx)
end
