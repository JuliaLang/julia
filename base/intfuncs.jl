# This file is a part of Julia. License is MIT: http://julialang.org/license

## number-theoretic functions ##

"""
    gcd(x,y)

Greatest common (positive) divisor (or zero if `x` and `y` are both zero).

```jldoctest
julia> gcd(6,9)
3

julia> gcd(6,-9)
3
```
"""
function gcd{T<:Integer}(a::T, b::T)
    while b != 0
        t = b
        b = rem(a, b)
        a = t
    end
    checked_abs(a)
end

# binary GCD (aka Stein's) algorithm
# about 1.7x (2.1x) faster for random Int64s (Int128s)
function gcd{T<:Union{Int64,UInt64,Int128,UInt128}}(a::T, b::T)
    a == 0 && return abs(b)
    b == 0 && return abs(a)
    za = trailing_zeros(a)
    zb = trailing_zeros(b)
    k = min(za, zb)
    u = unsigned(abs(a >> za))
    v = unsigned(abs(b >> zb))
    while u != v
        if u > v
            u, v = v, u
        end
        v -= u
        v >>= trailing_zeros(v)
    end
    r = u << k
    # T(r) would throw InexactError; we want OverflowError instead
    r > typemax(T) && throw(OverflowError())
    r % T
end

"""
    lcm(x,y)

Least common (non-negative) multiple.
```jldoctest
julia> lcm(2,3)
6

julia> lcm(-2,3)
6
```
"""
function lcm{T<:Integer}(a::T, b::T)
    # explicit a==0 test is to handle case of lcm(0,0) correctly
    if a == 0
        return a
    else
        return checked_abs(a * div(b, gcd(b,a)))
    end
end

gcd(a::Integer) = a
lcm(a::Integer) = a
gcd(a::Integer, b::Integer) = gcd(promote(a,b)...)
lcm(a::Integer, b::Integer) = lcm(promote(a,b)...)
gcd(a::Integer, b::Integer...) = gcd(a, gcd(b...))
lcm(a::Integer, b::Integer...) = lcm(a, lcm(b...))

gcd{T<:Integer}(abc::AbstractArray{T}) = reduce(gcd,abc)
lcm{T<:Integer}(abc::AbstractArray{T}) = reduce(lcm,abc)

# return (gcd(a,b),x,y) such that ax+by == gcd(a,b)
"""
    gcdx(x,y)

Computes the greatest common (positive) divisor of `x` and `y` and their Bézout
coefficients, i.e. the integer coefficients `u` and `v` that satisfy
``ux+vy = d = gcd(x,y)``. ``gcdx(x,y)`` returns ``(d,u,v)``.

```jldoctest
julia> gcdx(12, 42)
(6, -3, 1)
```

```jldoctest
julia> gcdx(240, 46)
(2, -9, 47)
```

!!! note
    Bézout coefficients are *not* uniquely defined. `gcdx` returns the minimal
    Bézout coefficients that are computed by the extended Euclidean algorithm.
    (Ref: D. Knuth, TAoCP, 2/e, p. 325, Algorithm X.)
    For signed integers, these coefficients `u` and `v` are minimal in
    the sense that ``|u| < |y/d|`` and ``|v| < |x/d|``. Furthermore,
    the signs of `u` and `v` are chosen so that `d` is positive.
    For unsigned integers, the coefficients `u` and `v` might be near
    their `typemax`, and the identity then holds only via the unsigned
    integers' modulo arithmetic.
"""
function gcdx{T<:Integer}(a::T, b::T)
    # a0, b0 = a, b
    s0, s1 = oneunit(T), zero(T)
    t0, t1 = s1, s0
    # The loop invariant is: s0*a0 + t0*b0 == a
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

"""
    invmod(x,m)

Take the inverse of `x` modulo `m`: `y` such that ``x y = 1 \\pmod m``,
with ``div(x,y) = 0``. This is undefined for ``m = 0``, or if
``gcd(x,m) \\neq 1``.

```jldoctest
julia> invmod(2,5)
3

julia> invmod(2,3)
2

julia> invmod(5,6)
5
```
"""
function invmod{T<:Integer}(n::T, m::T)
    g, x, y = gcdx(n, m)
    (g != 1 || m == 0) && throw(DomainError())
    # Note that m might be negative here.
    # For unsigned T, x might be close to typemax; add m to force a wrap-around.
    r = mod(x + m, m)
    # The postcondition is: mod(r * n, m) == mod(T(1), m) && div(r, m) == 0
    r
end
invmod(n::Integer, m::Integer) = invmod(promote(n,m)...)

# ^ for any x supporting *
to_power_type(x::Number) = oftype(x*x, x)
to_power_type(x) = x
function power_by_squaring(x_, p::Integer)
    x = to_power_type(x_)
    if p == 1
        return copy(x)
    elseif p == 0
        return one(x)
    elseif p == 2
        return x*x
    elseif p < 0
        x == 1 && return copy(x)
        x == -1 && return iseven(p) ? one(x) : copy(x)
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
    p < 0 && !x && throw(DomainError())
    return (p==0) | x
end

^{T<:Integer}(x::T, p::T) = power_by_squaring(x,p)
^(x::Number, p::Integer)  = power_by_squaring(x,p)
^(x, p::Integer)          = power_by_squaring(x,p)

# b^p mod m

"""
    powermod(x::Integer, p::Integer, m)

Compute ``x^p \\pmod m``.
"""
function powermod{T<:Integer}(x::Integer, p::Integer, m::T)
    p < 0 && return powermod(invmod(x, m), -p, m)
    p == 0 && return mod(one(m),m)
    (m == 1 || m == -1) && return zero(m)
    b = oftype(m,mod(x,m))  # this also checks for divide by zero

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

# optimization: promote the modulus m to BigInt only once (cf. widemul in generic powermod above)
powermod(x::Integer, p::Integer, m::Union{Int128,UInt128}) = oftype(m, powermod(x, p, big(m)))

# smallest power of 2 >= x

"""
    nextpow2(n::Integer)

The smallest power of two not less than `n`. Returns 0 for `n==0`, and returns
`-nextpow2(-n)` for negative arguments.

```jldoctest
julia> nextpow2(16)
16

julia> nextpow2(17)
32
```
"""
nextpow2(x::Unsigned) = oneunit(x)<<((sizeof(x)<<3)-leading_zeros(x-oneunit(x)))
nextpow2(x::Integer) = reinterpret(typeof(x),x < 0 ? -nextpow2(unsigned(-x)) : nextpow2(unsigned(x)))

"""
    prevpow2(n::Integer)

The largest power of two not greater than `n`. Returns 0 for `n==0`, and returns
`-prevpow2(-n)` for negative arguments.

```jldoctest
julia> prevpow2(5)
4
```
"""
prevpow2(x::Unsigned) = one(x) << unsigned((sizeof(x)<<3)-leading_zeros(x)-1)
prevpow2(x::Integer) = reinterpret(typeof(x),x < 0 ? -prevpow2(unsigned(-x)) : prevpow2(unsigned(x)))

"""
    ispow2(n::Integer) -> Bool

Test whether `n` is a power of two.

```jldoctest
julia> ispow2(4)
true

julia> ispow2(5)
false
```
"""
ispow2(x::Integer) = x > 0 && count_ones(x) == 1

"""
    nextpow(a, x)

The smallest `a^n` not less than `x`, where `n` is a non-negative integer. `a` must be
greater than 1, and `x` must be greater than 0.
"""
function nextpow(a::Real, x::Real)
    (a <= 1 || x <= 0) && throw(DomainError())
    x <= 1 && return one(a)
    n = ceil(Integer,log(a, x))
    p = a^(n-1)
    # guard against roundoff error, e.g., with a=5 and x=125
    p >= x ? p : a^n
end

"""
    prevpow(a, x)

The largest `a^n` not greater than `x`, where `n` is a non-negative integer.
`a` must be greater than 1, and `x` must not be less than 1.
"""
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
function ndigits0z(x::Union{UInt8,UInt16,UInt32,UInt64})
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
    return n + ndigits0z(UInt64(x))
end
ndigits0z(x::Integer) = ndigits0z(unsigned(abs(x)))

function ndigits0znb(n::Signed, b::Int)
    d = 0
    while n != 0
        n = cld(n,b)
        d += 1
    end
    return d
end

function ndigits0z(n::Unsigned, b::Int)
    b < 0   && return ndigits0znb(signed(n), b)
    b == 2  && return sizeof(n)<<3 - leading_zeros(n)
    b == 8  && return (sizeof(n)<<3 - leading_zeros(n) + 2) ÷ 3
    b == 16 && return sizeof(n)<<1 - leading_zeros(n)>>2
    b == 10 && return ndigits0z(n)

    d = 0
    while n > typemax(Int)
        n = div(n,b)
        d += 1
    end
    n = div(n,b)
    d += 1

    m = 1
    while m <= n
        m *= b
        d += 1
    end
    return d
end
ndigits0z(x::Integer, b::Integer) = ndigits0z(unsigned(abs(x)),Int(b))

ndigitsnb(x::Integer, b::Integer) = x==0 ? 1 : ndigits0znb(x, b)

ndigits(x::Unsigned, b::Integer) = x==0 ? 1 : ndigits0z(x,Int(b))
ndigits(x::Unsigned)             = x==0 ? 1 : ndigits0z(x)

"""
    ndigits(n::Integer, b::Integer=10)

Compute the number of digits in integer `n` written in base `b`.
"""
ndigits(x::Integer, b::Integer) = b >= 0 ? ndigits(unsigned(abs(x)),Int(b)) : ndigitsnb(x, b)
ndigits(x::Integer) = ndigits(unsigned(abs(x)))

## integer to string functions ##

string(x::Union{Int8,Int16,Int32,Int64,Int128}) = dec(x)

function bin(x::Unsigned, pad::Int, neg::Bool)
    i = neg + max(pad,sizeof(x)<<3-leading_zeros(x))
    a = StringVector(i)
    while i > neg
        a[i] = '0'+(x&0x1)
        x >>= 1
        i -= 1
    end
    if neg; a[1]='-'; end
    String(a)
end

function oct(x::Unsigned, pad::Int, neg::Bool)
    i = neg + max(pad,div((sizeof(x)<<3)-leading_zeros(x)+2,3))
    a = StringVector(i)
    while i > neg
        a[i] = '0'+(x&0x7)
        x >>= 3
        i -= 1
    end
    if neg; a[1]='-'; end
    String(a)
end

function dec(x::Unsigned, pad::Int, neg::Bool)
    i = neg + max(pad,ndigits0z(x))
    a = StringVector(i)
    while i > neg
        a[i] = '0'+rem(x,10)
        x = oftype(x,div(x,10))
        i -= 1
    end
    if neg; a[1]='-'; end
    String(a)
end

function hex(x::Unsigned, pad::Int, neg::Bool)
    i = neg + max(pad,(sizeof(x)<<1)-(leading_zeros(x)>>2))
    a = StringVector(i)
    while i > neg
        d = x & 0xf
        a[i] = '0'+d+39*(d>9)
        x >>= 4
        i -= 1
    end
    if neg; a[1]='-'; end
    String(a)
end

num2hex(n::Integer) = hex(n, sizeof(n)*2)

const base36digits = ['0':'9';'a':'z']
const base62digits = ['0':'9';'A':'Z';'a':'z']

function base(b::Int, x::Unsigned, pad::Int, neg::Bool)
    2 <= b <= 62 || throw(ArgumentError("base must be 2 ≤ base ≤ 62, got $b"))
    digits = b <= 36 ? base36digits : base62digits
    i = neg + max(pad,ndigits0z(x,b))
    a = StringVector(i)
    while i > neg
        a[i] = digits[1+rem(x,b)]
        x = div(x,b)
        i -= 1
    end
    if neg; a[1]='-'; end
    String(a)
end

"""
    base(base::Integer, n::Integer, pad::Integer=1)

Convert an integer `n` to a string in the given `base`,
optionally specifying a number of digits to pad to.

```jldoctest
julia> base(13,5,4)
"0005"

julia> base(5,13,4)
"0023"
```
"""
base(b::Integer, n::Integer, pad::Integer=1) = base(Int(b), unsigned(abs(n)), pad, n<0)

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

"""
    bin(n, pad::Int=1)

Convert an integer to a binary string, optionally specifying a number of digits to pad to.

```jldoctest
julia> bin(10,2)
"1010"

julia> bin(10,8)
"00001010"
```
"""
bin

"""
    hex(n, pad::Int=1)

Convert an integer to a hexadecimal string, optionally specifying a number of digits to pad to.
"""
hex

"""
    oct(n, pad::Int=1)

Convert an integer to an octal string, optionally specifying a number of digits to pad to.
"""
oct

"""
    dec(n, pad::Int=1)

Convert an integer to a decimal string, optionally specifying a number of digits to pad to.
"""
dec

bits(x::Union{Bool,Int8,UInt8})           = bin(reinterpret(UInt8,x),8)
bits(x::Union{Int16,UInt16,Float16})      = bin(reinterpret(UInt16,x),16)
bits(x::Union{Char,Int32,UInt32,Float32}) = bin(reinterpret(UInt32,x),32)
bits(x::Union{Int64,UInt64,Float64})      = bin(reinterpret(UInt64,x),64)
bits(x::Union{Int128,UInt128})            = bin(reinterpret(UInt128,x),128)

"""
    digits([T<:Integer], n::Integer, base::T=10, pad::Integer=1)

Returns an array with element type `T` (default `Int`) of the digits of `n` in the given
base, optionally padded with zeros to a specified size. More significant digits are at
higher indexes, such that `n == sum([digits[k]*base^(k-1) for k=1:length(digits)])`.
"""
digits{T<:Integer}(n::Integer, base::T=10, pad::Integer=1) = digits(T, n, base, pad)

function digits{T<:Integer}(::Type{T}, n::Integer, base::Integer=10, pad::Integer=1)
    2 <= base || throw(ArgumentError("base must be ≥ 2, got $base"))
    digits!(zeros(T, max(pad, ndigits0z(n,base))), n, base)
end

"""
    digits!(array, n::Integer, base::Integer=10)

Fills an array of the digits of `n` in the given base. More significant digits are at higher
indexes. If the array length is insufficient, the least significant digits are filled up to
the array length. If the array length is excessive, the excess portion is filled with zeros.
"""
function digits!{T<:Integer}(a::AbstractArray{T,1}, n::Integer, base::Integer=10)
    2 <= base || throw(ArgumentError("base must be ≥ 2, got $base"))
    base - 1 <= typemax(T) || throw(ArgumentError("type $T too small for base $base"))
    for i in eachindex(a)
        a[i] = rem(n, base)
        n = div(n, base)
    end
    return a
end

"""
    isqrt(n::Integer)

Integer square root: the largest integer `m` such that `m*m <= n`.

```jldoctest
julia> isqrt(5)
2
```
"""
isqrt(x::Integer) = oftype(x, trunc(sqrt(x)))

function isqrt(x::Union{Int64,UInt64,Int128,UInt128})
    x==0 && return x
    s = oftype(x, trunc(sqrt(x)))
    # fix with a Newton iteration, since conversion to float discards
    # too many bits.
    s = (s + div(x,s)) >> 1
    s*s > x ? s-1 : s
end

function factorial(n::Integer)
    n < 0 && throw(DomainError())
    local f::typeof(n*n), i::typeof(n*n)
    f = 1
    for i = 2:n
        f *= i
    end
    return f
end

"""
    binomial(n,k)

Number of ways to choose `k` out of `n` items.
"""
function binomial{T<:Integer}(n::T, k::T)
    k < 0 && return zero(T)
    sgn = one(T)
    if n < 0
        n = -n + k -1
        if isodd(k)
            sgn = -sgn
        end
    end
    k > n && return zero(T)
    (k == 0 || k == n) && return sgn
    k == 1 && return sgn*n
    if k > (n>>1)
        k = (n - k)
    end
    x::T = nn = n - k + 1
    nn += 1
    rr = 2
    while rr <= k
        xt = div(widemul(x, nn), rr)
        x = xt
        x == xt || throw(OverflowError())
        rr += 1
        nn += 1
    end
    convert(T, copysign(x, sgn))
end
