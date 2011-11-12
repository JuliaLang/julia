function factorial(n::Int)
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
function factorial{T<:Int}(n::T, k::T)
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

function binomial{T<:Int}(n::T, k::T)
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

## other ordering related functions ##

function shuffle!(a::AbstractVector)
    for i = length(a):-1:2
        j = randi(i)
        a[i], a[j] = a[j], a[i]
    end
    return a
end

@in_place_matrix_op shuffle

function randperm(n::Int)
    a = Array(typeof(n), n)
    a[1] = 1
    for i = 2:n
        j = randi(i)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function randcycle(n::Int)
    a = Array(typeof(n), n)
    a[1] = 1
    for i = 2:n
        j = randi(i-1)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function nthperm!(k::Int, a::AbstractVector)
    fac = one(k)
    for i = 2:length(a)
        fac *= (i-1)
        j = i - rem(div(k,fac),i)
        a[i], a[j] = a[j], a[i]
    end
    a
end

@in_place_matrix_op nthperm k::Int
