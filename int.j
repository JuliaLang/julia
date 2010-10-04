## traits ##

typemin(::Type{Int8  }) = int8(-128)
typemax(::Type{Int8  }) = int8(127)
typemin(::Type{Uint8 }) = uint8(0)
typemax(::Type{Uint8 }) = uint8(255)
typemin(::Type{Int16 }) = int16(-32768)
typemax(::Type{Int16 }) = int16(32767)
typemin(::Type{Uint16}) = uint16(0)
typemax(::Type{Uint16}) = uint16(65535)
typemin(::Type{Int32 }) = (-2147483647-1)
typemax(::Type{Int32 }) = 2147483647
typemin(::Type{Uint32}) = uint32(0)
typemax(::Type{Uint32}) = uint32(4294967295)
typemin(::Type{Int64 }) = (-9223372036854775807-1)
typemax(::Type{Int64 }) = 9223372036854775807
typemin(::Type{Uint64}) = uint64(0)
typemax(::Type{Uint64}) = 18446744073709551615

sizeof(::Type{Union(Int8,Uint8,Bool)})   = 1
sizeof(::Type{Union(Int16,Uint16)})      = 2
sizeof(::Type{Union(Char,Int32,Uint32)}) = 4
sizeof(::Type{Union(Int64,Uint64)})      = 8

## functions ##

isodd(n::Int)  = ((n%2)==1)
iseven(n::Int) = ((n%2)==0)

function gcd(a::Int, b::Int)
    while b != 0
        t = b
        b = a % b
        a = t
    end
    return a
end

lcm(a::Int, b::Int) = div(a*b,gcd(a,b))

# return (gcd(a,b),x,y) such that ax+by == gcd(a,b)
function gcdx(a, b)
    if b == 0
        (a, 1, 0)
    else
        m = a % b
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
        error("powermod: exponent must be >= 0")
    end
    t = 1
    while t <= p
        t *= 2
    end
    t = div(t,2)
    r = 1
    while true
        if p >= t
            r = (r*x) % m
            p -= t
        end
        t = div(t,2)
        if t > 0
            r = (r*r) % m
        else
            break
        end
    end
    return r
end

function fact(n::Int)
    if n < 0
        return zero(n)
    end
    p = one(n)
    for i=2:n
        p*=i
    end
    p
end

function nPr{T <: Int}(n::T, r::T)
    if r < 0 || n < 0 || r > n
        return zero(T)
    end

    ans = one(T)
    while (r > 0)
        ans *= n
        n -= 1
        r -= 1
    end
    return ans
end

function nCr{T <: Int}(n::T, r::T)
    if r < 0
        return zero(T)
    end

    neg = false
    if n < 0
        n = (-n)+r-1
        if isodd(r)
            neg = true
        end
    end

    if r > n
        return zero(T)
    end
    if r == 0 || r == n
        return one(T)
    end

    if r > div(n,2)
        r = (n - r)
    end

    ans = nn = n - r + 1.0
    nn += 1.0
    rr = 2.0
    while (rr <= r)
        ans *= (nn/rr)
        rr += 1
        nn += 1
    end
    if neg
        return oftype(T,-ans)
    end
    return oftype(T,ans)
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
    lim = m - (m%r+1)%r  # m - (m+1)%r
    while s > lim
        s = randint(T)
    end
    return s%r + lo
end

# random integer from 1 to n
randint(n::Int) = randint(one(n), n)
