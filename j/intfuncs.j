## integer functions ##

abs(x::Unsigned ) = x
abs(x::Int8 ) = (y=x>>7;  boxsi8 (add_int(unbox8 (x),unbox8 (y)))$y)
abs(x::Int16) = (y=x>>15; boxsi16(add_int(unbox16(x),unbox16(y)))$y)
abs(x::Int32) = (y=x>>31; boxsi32(add_int(unbox32(x),unbox32(y)))$y)
abs(x::Int64) = (y=x>>63; boxsi64(add_int(unbox64(x),unbox64(y)))$y)

isodd(n::Integer) = bool(rem(n,2))
iseven(n::Integer) = !isodd(n)

sign{T<:Integer}(x::T) = convert(T,(x > 0)-(x < 0))
sign{T<:Unsigned}(x::T) = convert(T,(x > 0))

signbit(x::Unsigned) = 0
signbit(x::Int8 ) = int(x>>>7)
signbit(x::Int16) = int(x>>>15)
signbit(x::Int32) = int(x>>>31)
signbit(x::Int64) = int(x>>>63)

copysign(x::Int8 , y::Int8 ) = (t=(x$y)>>7;  boxsi8 (add_int(unbox8 (x),unbox8 (t)))$t)
copysign(x::Int16, y::Int16) = (t=(x$y)>>15; boxsi16(add_int(unbox16(x),unbox16(t)))$t)
copysign(x::Int32, y::Int32) = (t=(x$y)>>31; boxsi32(add_int(unbox32(x),unbox32(t)))$t)
copysign(x::Int64, y::Int64) = (t=(x$y)>>63; boxsi64(add_int(unbox64(x),unbox64(t)))$t)

copysign(x::Signed, y::Real)    = copysign(x, -oftype(x,signbit(y)))
copysign(x::Signed, y::Float32) = copysign(x, reinterpret(Int32,y))
copysign(x::Signed, y::Float64) = copysign(x, reinterpret(Int64,y))

## number-theoretic functions ##

function gcd{T<:Integer}(a::T, b::T)
    neg = a < 0
    while b != 0
        t = b
        b = rem(a, b)
        a = t
    end
    g = abs(a)
    neg ? -g : g
end
lcm{T<:Integer}(a::T, b::T) = div(a*b, gcd(b,a))

gcd(a::Integer) = a
lcm(a::Integer) = a
gcd(a::Integer, b::Integer) = gcd(promote(a,b)...)
lcm(a::Integer, b::Integer) = lcm(promote(a,b)...)
gcd(a::Integer, b::Integer...) = gcd(a, gcd(b...))
lcm(a::Integer, b::Integer...) = lcm(a, lcm(b...))

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
function power_by_squaring(x, p::Integer)
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

^{T<:Integer}(x::T, p::T) = power_by_squaring(x,p)
^(x::Number, p::Integer)  = power_by_squaring(x,p)
^(x, p::Integer)          = power_by_squaring(x,p)

# x^p mod m
function powermod(x::Integer, p::Integer, m::Integer)
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

# smallest power of 2 >= i
function nextpow2(i::Integer)
    if i&(i-1) == 0
        return i
    end
    while (i&(i-1) != 0)
        i = i&(i-1)
    end
    return i<<1
end
