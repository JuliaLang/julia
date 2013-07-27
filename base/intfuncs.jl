## integer functions ##

isodd(n::Integer) = bool(rem(n,2))
iseven(n::Integer) = !isodd(n)

signbit(x::Unsigned) = 0
signbit(x::Int8) = int(x>>>7)
signbit(x::Int16) = int(x>>>15)
signbit(x::Int32) = int(x>>>31)
signbit(x::Int64) = int(x>>>63)
signbit(x::Int128) = int(x>>>127)

flipsign(x::Int,    y::Int)    = box(Int,flipsign_int(unbox(Int,x),unbox(Int,y)))
flipsign(x::Int64,  y::Int64)  = box(Int64,flipsign_int(unbox(Int64,x),unbox(Int64,y)))
flipsign(x::Int128, y::Int128) = box(Int128,flipsign_int(unbox(Int128,x),unbox(Int128,y)))

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

~(n::Integer) = -n-1

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
lcm{T<:Integer}(a::T, b::T) = a * div(b, gcd(b,a))

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
to_power_type(x::Number) = oftype(x*x, x)
to_power_type(x) = x
function power_by_squaring(x, p::Integer)
    x = to_power_type(x)
    if p == 1
        return copy(x)
    elseif p == 0
        return one(x)
    elseif p == 2
        return x*x
    elseif p < 0
        throw(DomainError())
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

# b^p mod m
function powermod{T}(b::Integer, p::Integer, m::T)
    p < 0 && throw(DomainError())
    p == 0 && return one(b)
    b = oftype(m,mod(b,m))
    t = prevpow2(p)
    local r::T
    r = 1
    while true
        if p >= t
            r = mod(widemul(r,b),m)
            p -= t
        end
        t >>>= 1
        t <= 0 && break
        r = mod(widemul(r,r),m)
    end
    return r
end

# smallest power of 2 >= x
nextpow2(x::Unsigned) = one(x)<<((sizeof(x)<<3)-leading_zeros(x-1))
nextpow2(x::Integer) = oftype(x,x < 0 ? -nextpow2(unsigned(-x)) : nextpow2(unsigned(x)))

prevpow2(x::Unsigned) = (one(x)>>(x==0)) << ((sizeof(x)<<3)-leading_zeros(x)-1)
prevpow2(x::Integer) = oftype(x,x < 0 ? -prevpow2(unsigned(-x)) : prevpow2(unsigned(x)))

ispow2(x::Integer) = ((x<=0) == (x&(x-1)))

# smallest integer n for which a^n >= x
function nextpow(a, x)
    n = iceil(log(x) ./ log(a))
    return n - int(a.^(n-1) .>= x) # guard against roundoff error, e.g., with a=5 and x=125
end
# largest integer n for which a^n <= x
function prevpow(a, x)
    n = ifloor(log(x) ./ log(a))
    return n + int(a.^(n+1) .<= x)
end

# decimal digits in an unsigned integer
const powers_of_ten = [
    0x0000000000000001, 0x000000000000000a, 0x0000000000000064, 0x00000000000003e8,
    0x0000000000002710, 0x00000000000186a0, 0x00000000000f4240, 0x0000000000989680,
    0x0000000005f5e100, 0x000000003b9aca00, 0x00000002540be400, 0x000000174876e800,
    0x000000e8d4a51000, 0x000009184e72a000, 0x00005af3107a4000, 0x00038d7ea4c68000,
    0x002386f26fc10000, 0x016345785d8a0000, 0x0de0b6b3a7640000, 0x8ac7230489e80000,
]
function ndigits0z(x::Union(Uint8,Uint16,Uint32,Uint64))
    lz = (sizeof(x)<<3)-leading_zeros(x)
    nd = (1233*lz)>>12+1
    nd -= x < powers_of_ten[nd]
end
function ndigits0z(x::Uint128)
    n = 0
    while x > 0x8ac7230489e80000
        x = div(x,0x8ac7230489e80000)
        n += 19
    end
    return n + ndigits0z(uint64(x))
end
ndigits0z(x::Integer) = ndigits0z(unsigned(abs(x)))

const ndigits_max_mul = WORD_SIZE==32 ? 69000000 : 290000000000000000

function ndigits0z(n::Unsigned, b::Int)
    b == 2  && return (sizeof(n)<<3-leading_zeros(n))
    b == 8  && return div((sizeof(n)<<3)-leading_zeros(n)+2,3)
    b == 16 && return (sizeof(n)<<1)-(leading_zeros(n)>>2)
    b == 10 && return ndigits0z(n)
    d = 0
    while ndigits_max_mul < n
        n = div(n,b)
        d += 1
    end
    m = 1
    while m <= n
        m *= b
        d += 1
    end
    return d
end
ndigits0z(x::Integer, b::Integer) = ndigits0z(unsigned(abs(x)),b)

ndigits(x::Unsigned, b::Integer) = x==0 ? 1 : ndigits0z(x,b)
ndigits(x::Unsigned)             = x==0 ? 1 : ndigits0z(x)

ndigits(x::Integer, b::Integer) = ndigits(unsigned(abs(x)),b)
ndigits(x::Integer) = ndigits(unsigned(abs(x)))

## integer to string functions ##

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
        a[i] = '0'+rem(x,10)
        x = oftype(x,div(x,10))
        i -= 1
    end
    if neg; a[1]='-'; end
    ASCIIString(a)
end

digit(x::Integer) = '0'+x+39*(x>9)

function hex(x::Unsigned, pad::Int, neg::Bool)
    i = neg + max(pad,(sizeof(x)<<1)-(leading_zeros(x)>>2))
    a = Array(Uint8,i)
    while i > neg
        a[i] = digit(x&0xf)
        x >>= 4
        i -= 1
    end
    if neg; a[1]='-'; end
    ASCIIString(a)
end

num2hex(n::Integer) = hex(n, sizeof(n)*2)

function base(b::Int, x::Unsigned, pad::Int, neg::Bool)
    if !(2 <= b <= 62) error("invalid base: $b") end
    i = neg + max(pad,ndigits0z(x,b))
    a = Array(Uint8,i)
    while i > neg
        a[i] = digit(rem(x,b))
        x = div(x,b)
        i -= 1
    end
    if neg; a[1]='-'; end
    ASCIIString(a)
end
base(b::Integer, n::Integer, pad::Integer=1) =
    base(int(b), unsigned(abs(n)), pad, n<0)

for sym in (:bin, :oct, :dec, :hex)
    @eval begin
        ($sym)(x::Unsigned, p::Int) = ($sym)(x,p,false)
        ($sym)(x::Unsigned)         = ($sym)(x,1,false)
        ($sym)(x::Integer, p::Int)  = ($sym)(unsigned(abs(x)),p,x<0)
        ($sym)(x::Integer)          = ($sym)(unsigned(abs(x)),1,x<0)
    end
end

bits(x::Union(Bool,Int8,Uint8))           = bin(reinterpret(Uint8,x),8)
bits(x::Union(Int16,Uint16))              = bin(reinterpret(Uint16,x),16)
bits(x::Union(Char,Int32,Uint32,Float32)) = bin(reinterpret(Uint32,x),32)
bits(x::Union(Int64,Uint64,Float64))      = bin(reinterpret(Uint64,x),64)
bits(x::Union(Int128,Uint128))            = bin(reinterpret(Uint128,x),128)

function digits{T<:Integer}(n::Integer, base::T=10, pad::Int=1)
    2 <= base || error("invalid base: $base")
    m = max(pad,ndigits0z(n,base))
    a = zeros(T,m)
    for i = 1:m
        a[i] = rem(n,base)
        n = div(n,base)
    end
    return a
end

isqrt(x::Integer) = oftype(x, trunc(sqrt(x)))
