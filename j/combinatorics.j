function factorial(n::Integer)
    if n < 0
        return zero(n)
    end
    f = one(n)
    for i = 2:n
        f *= i
    end
    return f
end

# computes n!/k!
function factorial{T<:Integer}(n::T, k::T)
    if k < 0 || n < 0 || k > n
        return zero(T)
    end
    f = one(T)
    while n > k
        f *= n
        n -= 1
    end
    return f
end

nPr(n, r) = factorial(n, n-r)

function binomial{T<:Integer}(n::T, k::T)
    if k < 0
        return zero(T)
    end
    sgn = one(T)
    if n < 0
        n = -n + k -1
        if isodd(k)
            sgn = -sgn
        end
    end
    if k > n # TODO: is this definitely right?
        return zero(T)
    end
    if k == 0 || k == n
        return sgn
    end
    if k == 1
        return sgn*n
    end
    if k > (n>>1)
        k = (n - k)
    end
    x = nn = n - k + 1.0
    nn += 1.0
    rr = 2.0
    while rr <= k
        x *= (nn/rr)
        rr += 1
        nn += 1
    end
    return sgn*convert(T,x)
end

pascal(n) = [binomial(i+j-2,i-1) | i=1:n,j=1:n]

## other ordering related functions ##

function shuffle!(a::AbstractVector)
    for i = length(a):-1:2
        j = randi(i)
        a[i], a[j] = a[j], a[i]
    end
    return a
end

@in_place_matrix_op shuffle

function randperm(n::Integer)
    a = Array(typeof(n), n)
    a[1] = 1
    for i = 2:n
        j = randi(i)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function randcycle(n::Integer)
    a = Array(typeof(n), n)
    a[1] = 1
    for i = 2:n
        j = randi(i-1)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function nthperm!(a::AbstractVector, k::Integer)
    n = length(a)
    k -= 1   # make k 1-indexed
    f = factorial(oftype(k, n-1))
    for i=1:n-1
        j = div(k, f) + 1
        k = k % f
        f = div(f, n-i)

        j = j+i-1
        elt = a[j]
        for d = j:-1:i+1
            a[d] = a[d-1]
        end
        a[i] = elt
    end
    a
end
nthperm(a::AbstractVector, k::Integer) = nthperm!(copy(a),k)
