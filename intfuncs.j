## integer functions ##

isodd(n::Int) = bool(rem(n,2))
iseven(n::Int) = !isodd(n)

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
        return 1/(x^(-p))
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
        error("powermod: exponent must be >= 0, got ", p)
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

randui64() = boxui64(or_int(zext64(unbox32(randui32())),
                            shl_int(zext64(unbox32(randui32())),unbox32(32))))

randint(::Type{Int32}) = int32(randui32())&typemax(Int32)
randint(::Type{Uint32}) = randui32()
randint(::Type{Int64}) = int64(randui64())&typemax(Int64)
randint(::Type{Uint64}) = randui64()
randint() = randint(Int32)

# random integer from lo to hi inclusive
function randint{T<:Int}(lo::T, hi::T)
    m = typemax(T)
    s = randint(T)
    if (hi-lo == m)
        return s + lo
    end
    r = hi-lo+1
    if (r&(r-1))==0
        # power of 2 range
        return s&(r-1) + lo
    end
    # note: m>=0 && r>=0
    lim = m - rem(rem(m,r)+1, r)
    while s > lim
        s = randint(T)
    end
    return rem(s,r) + lo
end

# random integer from 1 to n
randint(n::Int) = randint(one(n), n)
