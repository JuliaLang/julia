## integer functions ##

abs(x::Uint ) = x
abs(x::Int8 ) = (y=x>>7;  (x+y)$y)
abs(x::Int16) = (y=x>>15; (x+y)$y)
abs(x::Int32) = (y=x>>31; (x+y)$y)
abs(x::Int64) = (y=x>>63; (x+y)$y)

isodd(n::Int) = bool(rem(n,2))
iseven(n::Int) = !isodd(n)

sign{T<:Int}(x::T) = convert(T,convert(Int8,(x > 0))-convert(Int8,(x < 0)))
sign{T<:Uint}(x::T) = convert(T,(x > 0))

signbit(x::Uint ) = one(x)
signbit(x::Int8 ) = one(x)-((x>>>7) <<1)
signbit(x::Int16) = one(x)-((x>>>15)<<1)
signbit(x::Int32) = one(x)-((x>>>31)<<1)
signbit(x::Int64) = one(x)-((x>>>63)<<1)

copysign(x::Int, y::Real) = y < 0 ? -abs(x) : abs(x)
copysign(x::Int, y::Int) = copysign(promote(x,y)...)
copysign(x::Int8 , y::Int8 ) = (t=(x$y)>>7;  (x+t)$t)
copysign(x::Int16, y::Int16) = (t=(x$y)>>15; (x+t)$t)
copysign(x::Int32, y::Int32) = (t=(x$y)>>31; (x+t)$t)
copysign(x::Int64, y::Int64) = (t=(x$y)>>63; (x+t)$t)
copysign(x::Int, y::Float32) = copysign(x,boxsi32(unbox32(y)))
copysign(x::Int, y::Float64) = copysign(x,boxsi64(unbox64(y)))

## number-theoretic functions ##

function gcd{T<:Int}(a::T, b::T)
    neg = a < 0
    while b != 0
        t = b
        b = rem(a, b)
        a = t
    end
    g = abs(a)
    neg ? -g : g
end
lcm{T<:Int}(a::T, b::T) = div(a*b, gcd(b,a))

gcd(a::Int) = a
lcm(a::Int) = a
gcd(a::Int, b::Int) = gcd(promote(a,b)...)
lcm(a::Int, b::Int) = lcm(promote(a,b)...)
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

# multiplicative inverse of x mod m, error if none
function invmod(n, m)
    g, x, y = gcdx(n, m)
    g != 1 ? error("no inverse exists") : (x < 0 ? m + x : x)
end

# ^ for any x supporting *
function power_by_squaring(x, p::Int)
    if p == 1
        return x
    elseif p == 0
        return one(x)
    elseif p < 0
        error("power_by_squaring: exponent must be >= 0, got $p")
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

^(x, p::Int)          = power_by_squaring(x,p)
^(x::Int, p::Int)     = p < 0 ? inv(x^(-p)) : power_by_squaring(float64(x),p)
^{T<:Int}(x::T, p::T) = p < 0 ? inv(x^(-p)) : power_by_squaring(float64(x),p)

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
