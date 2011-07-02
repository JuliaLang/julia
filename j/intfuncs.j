## integer functions ##

isodd(n::Int) = bool(rem(n,2))
iseven(n::Int) = !isodd(n)

copysign(x::Int, y::Real) = y < 0 ? -abs(x) : abs(x) # TODO: make more efficient

function gcd(a::Int, b::Int)
    neg = a < 0
    while b != 0
        t = b
        b = rem(a, b)
        a = t
    end
    g = abs(a)
    neg ? -g : g
end

lcm(a::Int, b::Int) = div(a*b, gcd(b,a))

gcd(a::Int) = a
lcm(a::Int) = a

gcd(a::Int, b::Int...) = gcd(a, gcd(b...))
lcm(a::Int, b::Int...) = lcm(a, lcm(b...))

# return (gcd(a,b),x,y) such that ax+by == gcd(a,b)
function gcdx(a, b)
    if b == 0
        (a, 1, 0)
    else
        m = rem(a, b)
        k = div((a-m), b)
        (g, x, y) = gcdx(b, m)
        (g, y, x-k*y)
    end
end

# multiplicative inverse of x mod m, false if none
function invmod(n, m)
    g, x, y = gcdx(n, m)
    g != 1 ? false : (x < 0 ? m + x : x)
end

# ^ for any x supporting *
function ^(x, p::Int)
    if p == 1
        return x
    elseif p == 0
        return one(x)
    elseif p < 0
        return inv(x^(-p))
    elseif p == 2
        return x*x
    end
    t = 1
    while t <= p
        t *= 2
    end
    t = div(t,2)
    p -= t
    a = x
    while true
        t = div(t,2)
        if t > 0
            x = x*x
        else
            break
        end

        if p >= t
            x = x*a
            p -= t
        end
    end
    return x
end

# x^p mod m
function powermod(x::Int, p::Int, m::Int)
    if p == 0
        return one(x)
    elseif p < 0
        error("powermod: exponent must be >= 0, got $p")
    end
    t = 1
    while t <= p
        t *= 2
    end
    t = div(t,2)
    r = 1
    while true
        if p >= t
            r = mod(r*x, m)
            p -= t
        end
        t = div(t,2)
        if t > 0
            r = mod(r*r, m)
        else
            break
        end
    end
    return r
end
