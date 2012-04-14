## integer functions ##

isodd(n::Integer) = bool(rem(n,2))
iseven(n::Integer) = !isodd(n)

sign{T<:Integer}(x::T) = convert(T,(x>0)-(x<0))
sign{T<:Unsigned}(x::T) = convert(T,(x>0))

signbit(x::Unsigned) = 0
signbit(x::Int8 ) = int(x>>>7)
signbit(x::Int16) = int(x>>>15)
signbit(x::Int32) = int(x>>>31)
signbit(x::Int64) = int(x>>>63)

flipsign(x::Int32, y::Int32) = boxsi32(flipsign_int32(unbox32(x),unbox32(y)))
flipsign(x::Int64, y::Int64) = boxsi64(flipsign_int64(unbox64(x),unbox64(y)))

flipsign{T<:Signed}(x::T,y::T)  = flipsign(int(x),int(y))
flipsign(x::Signed, y::Signed)  = flipsign(promote(x,y)...)
flipsign(x::Signed, y::Float32) = flipsign(x, reinterpret(Int32,y))
flipsign(x::Signed, y::Float64) = flipsign(x, reinterpret(Int64,y))
flipsign(x::Signed, y::Real)    = flipsign(x, -oftype(x,signbit(y)))

copysign(x::Signed, y::Signed)  = flipsign(x, x$y)
copysign(x::Signed, y::Float32) = copysign(x, reinterpret(Int32,y))
copysign(x::Signed, y::Float64) = copysign(x, reinterpret(Int64,y))
copysign(x::Signed, y::Real)    = copysign(x, -oftype(x,signbit(y)))

abs(x::Unsigned) = x
abs(x::Signed) = flipsign(x,x)

## byte-order mark, ntoh & hton ##

const ENDIAN_BOM = reinterpret(Uint32,uint8([1:4]))[1]

if ENDIAN_BOM == 0x01020304
    ntoh(x) = identity(x)
    hton(x) = identity(x)
    ltoh(x) = bswap(x)
    htol(x) = bswap(x)
elseif ENDIAN_BOM == 0x04030201
    ntoh(x) = bswap(x)
    hton(x) = bswap(x)
    ltoh(x) = identity(x)
    htol(x) = identity(x)
else
    error("seriously? what is this machine?")
end

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
        error("power_by_squaring: exponent must be non-negative")
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

^{T<:Integer}(x::T, p::T) = p < 0 ? x^float(p) : power_by_squaring(x,p)
^(x::Number, p::Integer)  = p < 0 ? x^float(p) : power_by_squaring(x,p)
^(x, p::Integer)          = power_by_squaring(x,p)

# x^p mod m
function powermod(x::Integer, p::Integer, m::Integer)
    if p == 0
        return one(x)
    elseif p < 0
        error("powermod: exponent must be non-negative")
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
nextpow2(x::Unsigned) = one(x)<<((sizeof(x)<<3)-leading_zeros(x-1))
nextpow2(x::Integer) = oftype(x,x < 0 ? -nextpow2(unsigned(-x)) : nextpow2(unsigned(x)))

ispow2(x::Integer) = (x&(x-1))==0

# decimal digits in an unsigned integer
global const _jl_powers_of_ten = [
    0x0000000000000001, 0x000000000000000a, 0x0000000000000064, 0x00000000000003e8,
    0x0000000000002710, 0x00000000000186a0, 0x00000000000f4240, 0x0000000000989680,
    0x0000000005f5e100, 0x000000003b9aca00, 0x00000002540be400, 0x000000174876e800,
    0x000000e8d4a51000, 0x000009184e72a000, 0x00005af3107a4000, 0x00038d7ea4c68000,
    0x002386f26fc10000, 0x016345785d8a0000, 0x0de0b6b3a7640000, 0x8ac7230489e80000,
]
function ndigits0z(x::Unsigned)
    lz = (sizeof(x)<<3)-leading_zeros(x)
    nd = (1233*lz)>>12+1
    nd -= x < _jl_powers_of_ten[nd]
end
ndigits0z(x::Integer) = ndigits0z(unsigned(abs(x)))

function ndigits0z(n::Unsigned, b::Integer)
    if b == 2  return (sizeof(n)<<3-leading_zeros(n)); end
    if b == 8  return div((sizeof(n)<<3)-leading_zeros(n)+2,3); end
    if b == 16 return (sizeof(n)<<1)-(leading_zeros(n)>>2); end
    if b == 10 return ndigits0z(n); end
    nd = 1
    if n <= 500000000000000000
        # multiplication method is faster, but doesn't work for extreme values
        d = b
        while n >= d
            nd += 1
            d *= b
        end
    else
        while true
            n = div(n, b)
            if n == 0
                break
            end
            nd += 1
        end
    end
    return nd
end
ndigits0z(x::Integer, b::Integer) = ndigits0z(unsigned(abs(x)),b)

ndigits(x::Unsigned, b::Integer) = x==0 ? 1 : ndigits0z(x,b)
ndigits(x::Unsigned)             = x==0 ? 1 : ndigits0z(x)

ndigits(x::Integer, b::Integer) = ndigits(unsigned(abs(x)),b)
ndigits(x::Integer) = ndigits(unsigned(abs(x)))

## integer to string functions ##

macro _jl_int_stringifier(sym)
    quote
        ($sym)(x::Unsigned, p::Int) = ($sym)(x,p,false)
        ($sym)(x::Unsigned)         = ($sym)(x,1,false)
        ($sym)(x::Integer, p::Int)  = ($sym)(unsigned(abs(x)),p,x<0)
        ($sym)(x::Integer)          = ($sym)(unsigned(abs(x)),1,x<0)
    end
end

function bin(x::Unsigned, pad::Int, neg::Bool)
    i = neg + max(pad,sizeof(x)<<3-leading_zeros(x))
    a = Array(Uint8,i)
    while i > neg
        a[i] = '0'+(x&0x1)
        x >>= 1
        i -= 1
    end
    if neg; a[1]='-'; end
    ASCIIString(a)
end

function oct(x::Unsigned, pad::Int, neg::Bool)
    i = neg + max(pad,div((sizeof(x)<<3)-leading_zeros(x)+2,3))
    a = Array(Uint8,i)
    while i > neg
        a[i] = '0'+(x&0x7)
        x >>= 3
        i -= 1
    end
    if neg; a[1]='-'; end
    ASCIIString(a)
end

function dec(x::Unsigned, pad::Int, neg::Bool)
    i = neg + max(pad,ndigits0z(x))
    a = Array(Uint8,i)
    while i > neg
        a[i] = '0'+mod(x,10)
        x = div(x,10)
        i -= 1
    end
    if neg; a[1]='-'; end
    ASCIIString(a)
end

function hex(x::Unsigned, pad::Int, neg::Bool)
    i = neg + max(pad,(sizeof(x)<<1)-(leading_zeros(x)>>2))
    a = Array(Uint8,i)
    while i > neg
        a[i] = _jl_hex_symbols[(x&0xf)+1]
        x >>= 4
        i -= 1
    end
    if neg; a[1]='-'; end
    ASCIIString(a)
end

@_jl_int_stringifier bin
@_jl_int_stringifier oct
@_jl_int_stringifier dec
@_jl_int_stringifier hex

bits(x::Union(Bool,Int8,Uint8))           = bin(reinterpret(Uint8 ,x),  8)
bits(x::Union(Int16,Uint16))              = bin(reinterpret(Uint16,x), 16)
bits(x::Union(Char,Int32,Uint32,Float32)) = bin(reinterpret(Uint32,x), 32)
bits(x::Union(Int64,Uint64,Float64))      = bin(reinterpret(Uint64,x), 64)
