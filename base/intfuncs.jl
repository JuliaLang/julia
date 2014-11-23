## number-theoretic functions ##

function gcd{T<:Integer}(a::T, b::T)
    while b != 0
        t = b
        b = rem(a, b)
        a = t
    end
    abs(a)
end

# binary GCD (aka Stein's) algorithm
# about 1.7x (2.1x) faster for random Int64s (Int128s)
function gcd{T<:Union(Int64,UInt64,Int128,UInt128)}(a::T, b::T)
    a == 0 && return abs(b)
    b == 0 && return abs(a)
    za = trailing_zeros(a)
    zb = trailing_zeros(b)
    k = min(za, zb)
    u = abs(a >> za)
    v = abs(b >> zb)
    while u != v
        if u > v
            u, v = v, u
        end
        v -= u
        v >>= trailing_zeros(v)
    end
    u << k
end

# explicit a==0 test is to handle case of lcm(0,0) correctly
lcm{T<:Integer}(a::T, b::T) = a == 0 ? a : abs(a * div(b, gcd(b,a)))

gcd(a::Integer) = a
lcm(a::Integer) = a
gcd(a::Integer, b::Integer) = gcd(promote(a,b)...)
lcm(a::Integer, b::Integer) = lcm(promote(a,b)...)
gcd(a::Integer, b::Integer...) = gcd(a, gcd(b...))
lcm(a::Integer, b::Integer...) = lcm(a, lcm(b...))

gcd{T<:Integer}(abc::AbstractArray{T}) = reduce(gcd,abc)
lcm{T<:Integer}(abc::AbstractArray{T}) = reduce(lcm,abc)

# return (gcd(a,b),x,y) such that ax+by == gcd(a,b)
function gcdx{T<:Integer}(a::T, b::T)
    s0, s1 = one(T), zero(T)
    t0, t1 = s1, s0
    while b != 0
        q = div(a, b)
        a, b = b, rem(a, b)
        s0, s1 = s1, s0 - q*s1
        t0, t1 = t1, t0 - q*t1
    end
    a < 0 ? (-a, -s0, -t0) : (a, s0, t0)
end
gcdx(a::Integer, b::Integer) = gcdx(promote(a,b)...)

# multiplicative inverse of n mod m, error if none
function invmod(n, m)
    g, x, y = gcdx(n, m)
    if g != 1 || m == 0
        error("no inverse exists")
    end
    x < 0 ? abs(m) + x : x
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
    t = trailing_zeros(p) + 1
    p >>= t
    while (t -= 1) > 0
        x *= x
    end
    y = x
    while p > 0
        t = trailing_zeros(p) + 1
        p >>= t
        while (t -= 1) >= 0
            x *= x
        end
        y *= x
    end
    return y
end
power_by_squaring(x::Bool, p::Unsigned) = ((p==0) | x)
function power_by_squaring(x::Bool, p::Integer)
    p < 0 && throw(DomainError())
    return (p==0) | x
end

^{T<:Integer}(x::T, p::T) = power_by_squaring(x,p)
^(x::Number, p::Integer)  = power_by_squaring(x,p)
^(x, p::Integer)          = power_by_squaring(x,p)

# b^p mod m
function powermod{T}(b::Integer, p::Integer, m::T)
    p < 0 && throw(DomainError())
    b = oftype(m,mod(b,m))  # this also checks for divide by zero
    p == 0 && return mod(one(b),m)
    (m == 1 || m == -1) && return zero(m)

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
nextpow2(x::Integer) = reinterpret(typeof(x),x < 0 ? -nextpow2(unsigned(-x)) : nextpow2(unsigned(x)))

prevpow2(x::Unsigned) = (one(x)>>(x==0)) << ((sizeof(x)<<3)-leading_zeros(x)-1)
prevpow2(x::Integer) = reinterpret(typeof(x),x < 0 ? -prevpow2(unsigned(-x)) : prevpow2(unsigned(x)))

ispow2(x::Integer) = count_ones(x)==1

# smallest a^n >= x, with integer n
function nextpow(a::Real, x::Real)
    (a <= 1 || x <= 0) && throw(DomainError())
    x <= 1 && return one(a)
    n = ceil(Integer,log(a, x))
    p = a^(n-1)
    # guard against roundoff error, e.g., with a=5 and x=125
    p >= x ? p : a^n
end
# largest a^n <= x, with integer n
function prevpow(a::Real, x::Real)
    (a <= 1 || x < 1) && throw(DomainError())
    n = floor(Integer,log(a, x))
    p = a^(n+1)
    p <= x ? p : a^n
end

# decimal digits in an unsigned integer
const powers_of_ten = [
    0x0000000000000001, 0x000000000000000a, 0x0000000000000064, 0x00000000000003e8,
    0x0000000000002710, 0x00000000000186a0, 0x00000000000f4240, 0x0000000000989680,
    0x0000000005f5e100, 0x000000003b9aca00, 0x00000002540be400, 0x000000174876e800,
    0x000000e8d4a51000, 0x000009184e72a000, 0x00005af3107a4000, 0x00038d7ea4c68000,
    0x002386f26fc10000, 0x016345785d8a0000, 0x0de0b6b3a7640000, 0x8ac7230489e80000,
]
function ndigits0z(x::Union(UInt8,UInt16,UInt32,UInt64))
    lz = (sizeof(x)<<3)-leading_zeros(x)
    nd = (1233*lz)>>12+1
    nd -= x < powers_of_ten[nd]
end
function ndigits0z(x::UInt128)
    n = 0
    while x > 0x8ac7230489e80000
        x = div(x,0x8ac7230489e80000)
        n += 19
    end
    return n + ndigits0z(uint64(x))
end
ndigits0z(x::Integer) = ndigits0z(unsigned(abs(x)))

const ndigits_max_mul = WORD_SIZE==32 ? 69000000 : 290000000000000000

function ndigits0znb(n::Int, b::Int)
    d = 0
    while n != 0
        n = cld(n,b)
        d += 1
    end
    return d
end

function ndigits0z(n::Unsigned, b::Int)
    d = 0
    if b < 0
        d = ndigits0znb(signed(n), b)
    else
        b == 2  && return (sizeof(n)<<3-leading_zeros(n))
        b == 8  && return div((sizeof(n)<<3)-leading_zeros(n)+2,3)
        b == 16 && return (sizeof(n)<<1)-(leading_zeros(n)>>2)
        b == 10 && return ndigits0z(n)
        while ndigits_max_mul < n
            n = div(n,b)
            d += 1
        end
        m = 1
        while m <= n
            m *= b
            d += 1
        end
    end
    return d
end
ndigits0z(x::Integer, b::Integer) = ndigits0z(unsigned(abs(x)),int(b))

ndigitsnb(x::Integer, b::Integer) = x==0 ? 1 : ndigits0znb(x, b)

ndigits(x::Unsigned, b::Integer) = x==0 ? 1 : ndigits0z(x,int(b))
ndigits(x::Unsigned)             = x==0 ? 1 : ndigits0z(x)

ndigits(x::Integer, b::Integer) = b >= 0 ? ndigits(unsigned(abs(x)),int(b)) : ndigitsnb(x, b)
ndigits(x::Integer) = ndigits(unsigned(abs(x)))

## integer to string functions ##

function bin(x::Unsigned, pad::Int, neg::Bool)
    i = neg + max(pad,sizeof(x)<<3-leading_zeros(x))
    a = Array(UInt8,i)
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
    a = Array(UInt8,i)
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
    a = Array(UInt8,i)
    while i > neg
        a[i] = '0'+rem(x,10)
        x = oftype(x,div(x,10))
        i -= 1
    end
    if neg; a[1]='-'; end
    ASCIIString(a)
end

function hex(x::Unsigned, pad::Int, neg::Bool)
    i = neg + max(pad,(sizeof(x)<<1)-(leading_zeros(x)>>2))
    a = Array(UInt8,i)
    while i > neg
        d = x & 0xf
        a[i] = '0'+d+39*(d>9)
        x >>= 4
        i -= 1
    end
    if neg; a[1]='-'; end
    ASCIIString(a)
end

num2hex(n::Integer) = hex(n, sizeof(n)*2)

const base36digits = ['0':'9','a':'z']
const base62digits = ['0':'9','A':'Z','a':'z']

function base(b::Int, x::Unsigned, pad::Int, neg::Bool)
    if !(2 <= b <= 62) error("invalid base: $b") end
    digits = b <= 36 ? base36digits : base62digits
    i = neg + max(pad,ndigits0z(x,b))
    a = Array(UInt8,i)
    while i > neg
        a[i] = digits[1+rem(x,b)]
        x = div(x,b)
        i -= 1
    end
    if neg; a[1]='-'; end
    ASCIIString(a)
end
base(b::Integer, n::Integer, pad::Integer=1) = base(int(b), unsigned(abs(n)), pad, n<0)

for sym in (:bin, :oct, :dec, :hex)
    @eval begin
        ($sym)(x::Unsigned, p::Int) = ($sym)(x,p,false)
        ($sym)(x::Unsigned)         = ($sym)(x,1,false)
        ($sym)(x::Char, p::Int)     = ($sym)(unsigned(x),p,false)
        ($sym)(x::Char)             = ($sym)(unsigned(x),1,false)
        ($sym)(x::Integer, p::Int)  = ($sym)(unsigned(abs(x)),p,x<0)
        ($sym)(x::Integer)          = ($sym)(unsigned(abs(x)),1,x<0)
    end
end

bits(x::Union(Bool,Int8,UInt8))           = bin(reinterpret(UInt8,x),8)
bits(x::Union(Int16,UInt16,Float16))      = bin(reinterpret(UInt16,x),16)
bits(x::Union(Char,Int32,UInt32,Float32)) = bin(reinterpret(UInt32,x),32)
bits(x::Union(Int64,UInt64,Float64))      = bin(reinterpret(UInt64,x),64)
bits(x::Union(Int128,UInt128))            = bin(reinterpret(UInt128,x),128)

function digits{T<:Integer}(n::Integer, base::T=10, pad::Integer=1)
    2 <= base || error("invalid base: $base")
    m = max(pad,ndigits0z(n,base))
    a = zeros(T,m)
    digits!(a, n, base)
    return a
end

function digits!{T<:Integer}(a::AbstractArray{T,1}, n::Integer, base::T=10)
    2 <= base || error("invalid base: $base")
    for i = 1:length(a)
        a[i] = rem(n, base)
        n = div(n, base)
    end
    return a
end

isqrt(x::Integer) = oftype(x, trunc(sqrt(x)))

function isqrt(x::Union(Int64,UInt64,Int128,UInt128))
    x==0 && return x
    s = oftype(x, trunc(sqrt(x)))
    # fix with a Newton iteration, since conversion to float discards
    # too many bits.
    s = (s + div(x,s)) >> 1
    s*s > x ? s-1 : s
end
