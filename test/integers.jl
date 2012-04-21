for n = 1:100
    m = 1
    for (p,k) in factor(n)
        m *= p^k
    end
    @assert n == m
end
