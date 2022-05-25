# This file is a part of Julia. License is MIT: https://julialang.org/license

## number-theoretic functions ##

"""
    gcd(x, y...)

Greatest common (positive) divisor (or zero if all arguments are zero).
The arguments may be integer and rational numbers.

!!! compat "Julia 1.4"
    Rational arguments require Julia 1.4 or later.

# Examples
```jldoctest
julia> gcd(6, 9)
3

julia> gcd(6, -9)
3

julia> gcd(6, 0)
6

julia> gcd(0, 0)
0

julia> gcd(1//3, 2//3)
1//3

julia> gcd(1//3, -2//3)
1//3

julia> gcd(1//3, 2)
1//3

julia> gcd(0, 0, 10, 15)
5
```
"""
function gcd(a::T, b::T) where T<:Integer
    while b != 0
        t = b
        b = rem(a, b)
        a = t
    end
    checked_abs(a)
end

function gcd(a::T, b::T) where T<:BitInteger
    a == 0 && return checked_abs(b)
    b == 0 && return checked_abs(a)
    r = _gcd(a, b)
    signbit(r) && __throw_gcd_overflow(a, b)
    return r
end
@noinline __throw_gcd_overflow(a, b) =
    throw(OverflowError(LazyString("gcd(", a, ", ", b, ") overflows")))

# binary GCD (aka Stein's) algorithm
# about 1.7x (2.1x) faster for random Int64s (Int128s)
# Unfortunately, we need to manually annotate this as `@assume_effects :terminates_locally` to work around #41694.
# Since this is used in the Rational constructor, constant folding is something we do care about here.
@assume_effects :terminates_locally function _gcd(a::T, b::T) where T<:BitInteger
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
    return r % T
end

"""
    lcm(x, y...)

Least common (positive) multiple (or zero if any argument is zero).
The arguments may be integer and rational numbers.

!!! compat "Julia 1.4"
    Rational arguments require Julia 1.4 or later.

# Examples
```jldoctest
julia> lcm(2, 3)
6

julia> lcm(-2, 3)
6

julia> lcm(0, 3)
0

julia> lcm(0, 0)
0

julia> lcm(1//3, 2//3)
2//3

julia> lcm(1//3, -2//3)
2//3

julia> lcm(1//3, 2)
2//1

julia> lcm(1, 3, 5, 7)
105
```
"""
function lcm(a::T, b::T) where T<:Integer
    # explicit a==0 test is to handle case of lcm(0, 0) correctly
    # explicit b==0 test is to handle case of lcm(typemin(T),0) correctly
    if a == 0 || b == 0
        return zero(a)
    else
        return checked_abs(checked_mul(a, div(b, gcd(b,a))))
    end
end

gcd(a::Integer) = checked_abs(a)
gcd(a::Rational) = checked_abs(a.num) // a.den
lcm(a::Union{Integer,Rational}) = gcd(a)
gcd(a::Unsigned, b::Signed) = gcd(promote(a, abs(b))...)
gcd(a::Signed, b::Unsigned) = gcd(promote(abs(a), b)...)
gcd(a::Real, b::Real) = gcd(promote(a,b)...)
lcm(a::Real, b::Real) = lcm(promote(a,b)...)
gcd(a::Real, b::Real, c::Real...) = gcd(a, gcd(b, c...))
lcm(a::Real, b::Real, c::Real...) = lcm(a, lcm(b, c...))
gcd(a::T, b::T) where T<:Real = throw(MethodError(gcd, (a,b)))
lcm(a::T, b::T) where T<:Real = throw(MethodError(lcm, (a,b)))

gcd(abc::AbstractArray{<:Real}) = reduce(gcd, abc; init=zero(eltype(abc)))
lcm(abc::AbstractArray{<:Real}) = reduce(lcm, abc; init=one(eltype(abc)))

function gcd(abc::AbstractArray{<:Integer})
    a = zero(eltype(abc))
    for b in abc
        a = gcd(a, b)
        if a == 1
            return a
        end
    end
    return a
end

# return (gcd(a, b), x, y) such that ax+by == gcd(a, b)
"""
    gcdx(a, b)

Computes the greatest common (positive) divisor of `a` and `b` and their Bézout
coefficients, i.e. the integer coefficients `u` and `v` that satisfy
``ua+vb = d = gcd(a, b)``. ``gcdx(a, b)`` returns ``(d, u, v)``.

The arguments may be integer and rational numbers.

!!! compat "Julia 1.4"
    Rational arguments require Julia 1.4 or later.

# Examples
```jldoctest
julia> gcdx(12, 42)
(6, -3, 1)

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
function gcdx(a::Integer, b::Integer)
    T = promote_type(typeof(a), typeof(b))
    # a0, b0 = a, b
    s0, s1 = oneunit(T), zero(T)
    t0, t1 = s1, s0
    # The loop invariant is: s0*a0 + t0*b0 == a
    x = a % T
    y = b % T
    while y != 0
        q, r = divrem(x, y)
        x, y = y, r
        s0, s1 = s1, s0 - q*s1
        t0, t1 = t1, t0 - q*t1
    end
    x < 0 ? (-x, -s0, -t0) : (x, s0, t0)
end
gcdx(a::Real, b::Real) = gcdx(promote(a,b)...)
gcdx(a::T, b::T) where T<:Real = throw(MethodError(gcdx, (a,b)))

# multiplicative inverse of n mod m, error if none

"""
    invmod(n, m)

Take the inverse of `n` modulo `m`: `y` such that ``n y = 1 \\pmod m``,
and ``div(y,m) = 0``. This will throw an error if ``m = 0``, or if
``gcd(n,m) \\neq 1``.

# Examples
```jldoctest
julia> invmod(2, 5)
3

julia> invmod(2, 3)
2

julia> invmod(5, 6)
5
```
"""
function invmod(n::Integer, m::Integer)
    iszero(m) && throw(DomainError(m, "`m` must not be 0."))
    if n isa Signed && hastypemax(typeof(n))
        # work around inconsistencies in gcdx
        # https://github.com/JuliaLang/julia/issues/33781
        T = promote_type(typeof(n), typeof(m))
        n == typemin(typeof(n)) && m == typeof(n)(-1) && return T(0)
        n == typeof(n)(-1) && m == typemin(typeof(n)) && return T(-1)
    end
    g, x, y = gcdx(n, m)
    g != 1 && throw(DomainError((n, m), "Greatest common divisor is $g."))
    # Note that m might be negative here.
    if n isa Unsigned && hastypemax(typeof(n)) && x > typemax(n)>>1
        # x might have wrapped if it would have been negative
        # adding back m forces a correction
        x += m
    end
    # The postcondition is: mod(result * n, m) == mod(T(1), m) && div(result, m) == 0
    return mod(x, m)
end

# ^ for any x supporting *
to_power_type(x) = convert(Base._return_type(*, Tuple{typeof(x), typeof(x)}), x)
@noinline throw_domerr_powbysq(::Any, p) = throw(DomainError(p,
    string("Cannot raise an integer x to a negative power ", p, '.',
           "\nConvert input to float.")))
@noinline throw_domerr_powbysq(::Integer, p) = throw(DomainError(p,
   string("Cannot raise an integer x to a negative power ", p, '.',
          "\nMake x or $p a float by adding a zero decimal ",
          "(e.g., 2.0^$p or 2^$(float(p)) instead of 2^$p), ",
          "or write 1/x^$(-p), float(x)^$p, x^float($p) or (x//1)^$p")))
@noinline throw_domerr_powbysq(::AbstractMatrix, p) = throw(DomainError(p,
   string("Cannot raise an integer matrix x to a negative power ", p, '.',
          "\nMake x a float matrix by adding a zero decimal ",
          "(e.g., [2.0 1.0;1.0 0.0]^$p instead ",
          "of [2 1;1 0]^$p), or write float(x)^$p or Rational.(x)^$p")))
function power_by_squaring(x_, p::Integer)
    x = to_power_type(x_)
    if p == 1
        return copy(x)
    elseif p == 0
        return one(x)
    elseif p == 2
        return x*x
    elseif p < 0
        isone(x) && return copy(x)
        isone(-x) && return iseven(p) ? one(x) : copy(x)
        throw_domerr_powbysq(x, p)
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
    p < 0 && !x && throw_domerr_powbysq(x, p)
    return (p==0) | x
end

^(x::T, p::T) where {T<:Integer} = power_by_squaring(x,p)
^(x::Number, p::Integer)  = power_by_squaring(x,p)

# x^p for any literal integer p is lowered to Base.literal_pow(^, x, Val(p))
# to enable compile-time optimizations specialized to p.
# However, we still need a fallback that calls the function ^ which may either
# mean Base.^ or something else, depending on context.
# We mark these @inline since if the target is marked @inline,
# we want to make sure that gets propagated,
# even if it is over the inlining threshold.
@inline literal_pow(f, x, ::Val{p}) where {p} = f(x,p)

# Restrict inlining to hardware-supported arithmetic types, which
# are fast enough to benefit from inlining.
const HWReal = Union{Int8,Int16,Int32,Int64,UInt8,UInt16,UInt32,UInt64,Float32,Float64}
const HWNumber = Union{HWReal, Complex{<:HWReal}, Rational{<:HWReal}}

# Inline x^2 and x^3 for Val
# (The first argument prevents unexpected behavior if a function ^
# is defined that is not equal to Base.^)
@inline literal_pow(::typeof(^), x::HWNumber, ::Val{0}) = one(x)
@inline literal_pow(::typeof(^), x::HWNumber, ::Val{1}) = x
@inline literal_pow(::typeof(^), x::HWNumber, ::Val{2}) = x*x
@inline literal_pow(::typeof(^), x::HWNumber, ::Val{3}) = x*x*x
@inline literal_pow(::typeof(^), x::HWNumber, ::Val{-1}) = inv(x)
@inline literal_pow(::typeof(^), x::HWNumber, ::Val{-2}) = (i=inv(x); i*i)

# don't use the inv(x) transformation here since float^p is slightly more accurate
@inline literal_pow(::typeof(^), x::AbstractFloat, ::Val{p}) where {p} = x^p
@inline literal_pow(::typeof(^), x::AbstractFloat, ::Val{-1}) = inv(x)

# for other types, define x^-n as inv(x)^n so that negative literal powers can
# be computed in a type-stable way even for e.g. integers.
@inline function literal_pow(f::typeof(^), x, ::Val{p}) where {p}
    if p < 0
        if x isa BitInteger64
            f(Float64(x), p) # inv would cause rounding, while Float64^Integer is able to compensate the inverse
        else
            f(inv(x), -p)
        end
    else
        f(x, p)
    end
end

# note: it is tempting to add optimized literal_pow(::typeof(^), x, ::Val{n})
#       methods here for various n, but this easily leads to method ambiguities
#       if anyone has defined literal_pow(::typeof(^), x::T, ::Val).

# b^p mod m

"""
    powermod(x::Integer, p::Integer, m)

Compute ``x^p \\pmod m``.

# Examples
```jldoctest
julia> powermod(2, 6, 5)
4

julia> mod(2^6, 5)
4

julia> powermod(5, 2, 20)
5

julia> powermod(5, 2, 19)
6

julia> powermod(5, 3, 19)
11
```
"""
function powermod(x::Integer, p::Integer, m::T) where T<:Integer
    p < 0 && return powermod(invmod(x, m), -p, m)
    p == 0 && return mod(one(m),m)
    (m == 1 || m == -1) && return zero(m)
    b = oftype(m,mod(x,m))  # this also checks for divide by zero

    t = prevpow(2, p)
    r::T = 1
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

_nextpow2(x::Unsigned) = oneunit(x)<<((sizeof(x)<<3)-leading_zeros(x-oneunit(x)))
_nextpow2(x::Integer) = reinterpret(typeof(x),x < 0 ? -_nextpow2(unsigned(-x)) : _nextpow2(unsigned(x)))
_prevpow2(x::Unsigned) = one(x) << unsigned((sizeof(x)<<3)-leading_zeros(x)-1)
_prevpow2(x::Integer) = reinterpret(typeof(x),x < 0 ? -_prevpow2(unsigned(-x)) : _prevpow2(unsigned(x)))

"""
    ispow2(n::Number) -> Bool

Test whether `n` is an integer power of two.

See also [`count_ones`](@ref), [`prevpow`](@ref), [`nextpow`](@ref).

# Examples
```jldoctest
julia> ispow2(4)
true

julia> ispow2(5)
false

julia> ispow2(4.5)
false

julia> ispow2(0.25)
true

julia> ispow2(1//8)
true
```

!!! compat "Julia 1.6"
    Support for non-`Integer` arguments was added in Julia 1.6.
"""
ispow2(x::Number) = isreal(x) && ispow2(real(x))

ispow2(x::Integer) = x > 0 && count_ones(x) == 1

"""
    nextpow(a, x)

The smallest `a^n` not less than `x`, where `n` is a non-negative integer. `a` must be
greater than 1, and `x` must be greater than 0.

See also [`prevpow`](@ref).

# Examples
```jldoctest
julia> nextpow(2, 7)
8

julia> nextpow(2, 9)
16

julia> nextpow(5, 20)
25

julia> nextpow(4, 16)
16
```
"""
function nextpow(a::Real, x::Real)
    x <= 0 && throw(DomainError(x, "`x` must be positive."))
    # Special case fast path for x::Integer, a == 2.
    # This is a very common case. Constant prop will make sure that a call site
    # specified as `nextpow(2, x)` will get this special case inlined.
    a == 2 && isa(x, Integer) && return _nextpow2(x)
    a <= 1 && throw(DomainError(a, "`a` must be greater than 1."))
    x <= 1 && return one(a)
    n = ceil(Integer,log(a, x))
    # round-off error of log can go either direction, so need some checks
    p = a^(n-1)
    x > typemax(p) && throw(DomainError(x,"argument is beyond the range of type of the base"))
    p >= x && return p
    wp = a^n
    wp > p || throw(OverflowError("result is beyond the range of type of the base"))
    wp >= x && return wp
    wwp = a^(n+1)
    wwp > wp || throw(OverflowError("result is beyond the range of type of the base"))
    return wwp
end

"""
    prevpow(a, x)

The largest `a^n` not greater than `x`, where `n` is a non-negative integer.
`a` must be greater than 1, and `x` must not be less than 1.

See also [`nextpow`](@ref), [`isqrt`](@ref).

# Examples
```jldoctest
julia> prevpow(2, 7)
4

julia> prevpow(2, 9)
8

julia> prevpow(5, 20)
5

julia> prevpow(4, 16)
16
```
"""
function prevpow(a::T, x::Real) where T <: Real
    x < 1 && throw(DomainError(x, "`x` must be ≥ 1."))
    # See comment in nextpos() for a == special case.
    a == 2 && isa(x, Integer) && return _prevpow2(x)
    a <= 1 && throw(DomainError(a, "`a` must be greater than 1."))
    n = floor(Integer,log(a, x))
    # round-off error of log can go either direction, so need some checks
    p = a^n
    x > typemax(p) && throw(DomainError(x,"argument is beyond the range of type of the base"))
    if a isa Integer
        wp, overflow = mul_with_overflow(a, p)
        wp <= x && !overflow && return wp
    else
        wp = a^(n+1)
        wp <= x && return wp
    end
    p <= x && return p
    return a^(n-1)
end

## ndigits (number of digits) in base 10 ##

# decimal digits in an unsigned integer
const powers_of_ten = [
    0x0000000000000001, 0x000000000000000a, 0x0000000000000064, 0x00000000000003e8,
    0x0000000000002710, 0x00000000000186a0, 0x00000000000f4240, 0x0000000000989680,
    0x0000000005f5e100, 0x000000003b9aca00, 0x00000002540be400, 0x000000174876e800,
    0x000000e8d4a51000, 0x000009184e72a000, 0x00005af3107a4000, 0x00038d7ea4c68000,
    0x002386f26fc10000, 0x016345785d8a0000, 0x0de0b6b3a7640000, 0x8ac7230489e80000,
]
function bit_ndigits0z(x::Base.BitUnsigned64)
    lz = (sizeof(x)<<3)-leading_zeros(x)
    nd = (1233*lz)>>12+1
    nd -= x < powers_of_ten[nd]
end
function bit_ndigits0z(x::UInt128)
    n = 0
    while x > 0x8ac7230489e80000
        x = div(x,0x8ac7230489e80000)
        n += 19
    end
    return n + ndigits0z(UInt64(x))
end

ndigits0z(x::BitSigned) = bit_ndigits0z(unsigned(abs(x)))
ndigits0z(x::BitUnsigned) = bit_ndigits0z(x)
ndigits0z(x::Integer) = ndigits0zpb(x, 10)

## ndigits with specified base ##

# The suffix "nb" stands for "negative base"
function ndigits0znb(x::Integer, b::Integer)
    d = 0
    if x isa Unsigned
        d += (x != 0)::Bool
        x = -signed(fld(x, -b))
    end
    # precondition: b < -1 && !(typeof(x) <: Unsigned)
    while x != 0
        x = cld(x,b)
        d += 1
    end
    return d
end

# do first division before conversion with signed here, which can otherwise overflow
ndigits0znb(x::Bool, b::Integer) = x % Int

# The suffix "pb" stands for "positive base"
function ndigits0zpb(x::Integer, b::Integer)
    # precondition: b > 1
    x == 0 && return 0
    b = Int(b)
    x = abs(x)
    if x isa Base.BitInteger
        x = unsigned(x)::Unsigned
        b == 2  && return sizeof(x)<<3 - leading_zeros(x)
        b == 8  && return (sizeof(x)<<3 - leading_zeros(x) + 2) ÷ 3
        b == 16 && return sizeof(x)<<1 - leading_zeros(x)>>2
        b == 10 && return bit_ndigits0z(x)
        if ispow2(b)
            dv, rm = divrem(sizeof(x)<<3 - leading_zeros(x), trailing_zeros(b))
            return iszero(rm) ? dv : dv + 1
        end
    end

    d = 0
    while x > typemax(Int)
        x = div(x,b)
        d += 1
    end
    x = div(x,b)
    d += 1

    m = 1
    while m <= x
        m *= b
        d += 1
    end
    return d
end

ndigits0zpb(x::Bool, b::Integer) = x % Int

# The suffix "0z" means that the output is 0 on input zero (cf. #16841)
"""
    ndigits0z(n::Integer, b::Integer=10)

Return 0 if `n == 0`, otherwise compute the number of digits in
integer `n` written in base `b` (i.e. equal to `ndigits(n, base=b)`
in this case).
The base `b` must not be in `[-1, 0, 1]`.

# Examples
```jldoctest
julia> Base.ndigits0z(0, 16)
0

julia> Base.ndigits(0, base=16)
1

julia> Base.ndigits0z(0)
0

julia> Base.ndigits0z(10, 2)
4

julia> Base.ndigits0z(10)
2
```

See also [`ndigits`](@ref).
"""
function ndigits0z(x::Integer, b::Integer)
    if b < -1
        ndigits0znb(x, b)
    elseif b > 1
        ndigits0zpb(x, b)
    else
        throw(DomainError(b, "The base must not be in `[-1, 0, 1]`."))
    end
end

"""
    ndigits(n::Integer; base::Integer=10, pad::Integer=1)

Compute the number of digits in integer `n` written in base `base`
(`base` must not be in `[-1, 0, 1]`), optionally padded with zeros
to a specified size (the result will never be less than `pad`).

See also [`digits`](@ref), [`count_ones`](@ref).

# Examples
```jldoctest
julia> ndigits(12345)
5

julia> ndigits(1022, base=16)
3

julia> string(1022, base=16)
"3fe"

julia> ndigits(123, pad=5)
5

julia> ndigits(-123)
3
```
"""
ndigits(x::Integer; base::Integer=10, pad::Integer=1) = max(pad, ndigits0z(x, base))

## integer to string functions ##

function bin(x::Unsigned, pad::Int, neg::Bool)
    m = 8 * sizeof(x) - leading_zeros(x)
    n = neg + max(pad, m)
    a = StringVector(n)
    # for i in 0x0:UInt(n-1) # automatic vectorization produces redundant codes
    #     @inbounds a[n - i] = 0x30 + (((x >> i) % UInt8)::UInt8 & 0x1)
    # end
    i = n
    @inbounds while i >= 4
        b = UInt32((x % UInt8)::UInt8)
        d = 0x30303030 + ((b * 0x08040201) >> 0x3) & 0x01010101
        a[i-3] = (d >> 0x00) % UInt8
        a[i-2] = (d >> 0x08) % UInt8
        a[i-1] = (d >> 0x10) % UInt8
        a[i]   = (d >> 0x18) % UInt8
        x >>= 0x4
        i -= 4
    end
    while i > neg
        @inbounds a[i] = 0x30 + ((x % UInt8)::UInt8 & 0x1)
        x >>= 0x1
        i -= 1
    end
    if neg; @inbounds a[1]=0x2d; end
    String(a)
end

function oct(x::Unsigned, pad::Int, neg::Bool)
    m = div(8 * sizeof(x) - leading_zeros(x) + 2, 3)
    n = neg + max(pad, m)
    a = StringVector(n)
    i = n
    while i > neg
        @inbounds a[i] = 0x30 + ((x % UInt8)::UInt8 & 0x7)
        x >>= 0x3
        i -= 1
    end
    if neg; @inbounds a[1]=0x2d; end
    String(a)
end

# 2-digit decimal characters ("00":"99")
const _dec_d100 = UInt16[(0x30 + i % 10) << 0x8 + (0x30 + i ÷ 10) for i = 0:99]

function dec(x::Unsigned, pad::Int, neg::Bool)
    n = neg + ndigits(x, pad=pad)
    a = StringVector(n)
    i = n
    @inbounds while i >= 2
        d, r = divrem(x, 0x64)
        d100 = _dec_d100[(r % Int)::Int + 1]
        a[i-1] = d100 % UInt8
        a[i] = (d100 >> 0x8) % UInt8
        x = oftype(x, d)
        i -= 2
    end
    if i > neg
        @inbounds a[i] = 0x30 + (rem(x, 0xa) % UInt8)::UInt8
    end
    if neg; @inbounds a[1]=0x2d; end
    String(a)
end

function hex(x::Unsigned, pad::Int, neg::Bool)
    m = 2 * sizeof(x) - (leading_zeros(x) >> 2)
    n = neg + max(pad, m)
    a = StringVector(n)
    i = n
    while i >= 2
        b = (x % UInt8)::UInt8
        d1, d2 = b >> 0x4, b & 0xf
        @inbounds a[i-1] = d1 + ifelse(d1 > 0x9, 0x57, 0x30)
        @inbounds a[i]   = d2 + ifelse(d2 > 0x9, 0x57, 0x30)
        x >>= 0x8
        i -= 2
    end
    if i > neg
        d = (x % UInt8)::UInt8 & 0xf
        @inbounds a[i] = d + ifelse(d > 0x9, 0x57, 0x30)
    end
    if neg; @inbounds a[1]=0x2d; end
    String(a)
end

const base36digits = UInt8['0':'9';'a':'z']
const base62digits = UInt8['0':'9';'A':'Z';'a':'z']

function _base(base::Integer, x::Integer, pad::Int, neg::Bool)
    (x >= 0) | (base < 0) || throw(DomainError(x, "For negative `x`, `base` must be negative."))
    2 <= abs(base) <= 62 || throw(DomainError(base, "base must satisfy 2 ≤ abs(base) ≤ 62"))
    b = (base % Int)::Int
    digits = abs(b) <= 36 ? base36digits : base62digits
    n = neg + ndigits(x, base=b, pad=pad)
    a = StringVector(n)
    i = n
    @inbounds while i > neg
        if b > 0
            a[i] = digits[1 + (rem(x, b) % Int)::Int]
            x = div(x,b)
        else
            a[i] = digits[1 + (mod(x, -b) % Int)::Int]
            x = cld(x,b)
        end
        i -= 1
    end
    if neg; @inbounds a[1]=0x2d; end
    String(a)
end

split_sign(n::Integer) = unsigned(abs(n)), n < 0
split_sign(n::Unsigned) = n, false

"""
    string(n::Integer; base::Integer = 10, pad::Integer = 1)

Convert an integer `n` to a string in the given `base`,
optionally specifying a number of digits to pad to.

See also [`digits`](@ref), [`bitstring`](@ref), [`count_zeros`](@ref).

# Examples
```jldoctest
julia> string(5, base = 13, pad = 4)
"0005"

julia> string(-13, base = 5, pad = 4)
"-0023"
```
"""
function string(n::Integer; base::Integer = 10, pad::Integer = 1)
    pad = (min(max(pad, typemin(Int)), typemax(Int)) % Int)::Int
    if base == 2
        (n_positive, neg) = split_sign(n)
        bin(n_positive, pad, neg)
    elseif base == 8
        (n_positive, neg) = split_sign(n)
        oct(n_positive, pad, neg)
    elseif base == 10
        (n_positive, neg) = split_sign(n)
        dec(n_positive, pad, neg)
    elseif base == 16
        (n_positive, neg) = split_sign(n)
        hex(n_positive, pad, neg)
    else
        _base(base, base > 0 ? unsigned(abs(n)) : convert(Signed, n), pad, (base>0) & (n<0))
    end
end

string(b::Bool) = b ? "true" : "false"

"""
    bitstring(n)

A string giving the literal bit representation of a primitive type.

See also [`count_ones`](@ref), [`count_zeros`](@ref), [`digits`](@ref).

# Examples
```jldoctest
julia> bitstring(Int32(4))
"00000000000000000000000000000100"

julia> bitstring(2.2)
"0100000000000001100110011001100110011001100110011001100110011010"
```
"""
function bitstring(x::T) where {T}
    isprimitivetype(T) || throw(ArgumentError("$T not a primitive type"))
    sz = sizeof(T) * 8
    str = StringVector(sz)
    i = sz
    @inbounds while i >= 4
        b = UInt32(sizeof(T) == 1 ? bitcast(UInt8, x) : trunc_int(UInt8, x))
        d = 0x30303030 + ((b * 0x08040201) >> 0x3) & 0x01010101
        str[i-3] = (d >> 0x00) % UInt8
        str[i-2] = (d >> 0x08) % UInt8
        str[i-1] = (d >> 0x10) % UInt8
        str[i]   = (d >> 0x18) % UInt8
        x = lshr_int(x, 4)
        i -= 4
    end
    return String(str)
end

"""
    digits([T<:Integer], n::Integer; base::T = 10, pad::Integer = 1)

Return an array with element type `T` (default `Int`) of the digits of `n` in the given
base, optionally padded with zeros to a specified size. More significant digits are at
higher indices, such that `n == sum(digits[k]*base^(k-1) for k=1:length(digits))`.

See also [`ndigits`](@ref), [`digits!`](@ref),
and for base 2 also [`bitstring`](@ref), [`count_ones`](@ref).

# Examples
```jldoctest
julia> digits(10)
2-element Vector{Int64}:
 0
 1

julia> digits(10, base = 2)
4-element Vector{Int64}:
 0
 1
 0
 1

julia> digits(-256, base = 10, pad = 5)
5-element Vector{Int64}:
 -6
 -5
 -2
  0
  0

julia> n = rand(-999:999);

julia> n == evalpoly(13, digits(n, base = 13))
true
```
"""
digits(n::Integer; base::Integer = 10, pad::Integer = 1) =
    digits(typeof(base), n, base = base, pad = pad)

function digits(T::Type{<:Integer}, n::Integer; base::Integer = 10, pad::Integer = 1)
    digits!(zeros(T, ndigits(n, base=base, pad=pad)), n, base=base)
end

"""
    hastypemax(T::Type) -> Bool

Return true if and only if the extrema `typemax(T)` and `typemin(T)` are defined.
"""
hastypemax(::Base.BitIntegerType) = true
hastypemax(::Type{Bool}) = true
hastypemax(::Type{T}) where {T} = applicable(typemax, T) && applicable(typemin, T)

"""
    digits!(array, n::Integer; base::Integer = 10)

Fills an array of the digits of `n` in the given base. More significant digits are at higher
indices. If the array length is insufficient, the least significant digits are filled up to
the array length. If the array length is excessive, the excess portion is filled with zeros.

# Examples
```jldoctest
julia> digits!([2, 2, 2, 2], 10, base = 2)
4-element Vector{Int64}:
 0
 1
 0
 1

julia> digits!([2, 2, 2, 2, 2, 2], 10, base = 2)
6-element Vector{Int64}:
 0
 1
 0
 1
 0
 0
```
"""
function digits!(a::AbstractVector{T}, n::Integer; base::Integer = 10) where T<:Integer
    2 <= abs(base) || throw(DomainError(base, "base must be ≥ 2 or ≤ -2"))
    hastypemax(T) && abs(base) - 1 > typemax(T) &&
        throw(ArgumentError("type $T too small for base $base"))
    isempty(a) && return a

    if base > 0
        if ispow2(base) && n >= 0 && n isa Base.BitInteger && base <= typemax(Int)
            base = Int(base)
            k = trailing_zeros(base)
            c = base - 1
            for i in eachindex(a)
                a[i] = (n >> (k * (i - firstindex(a)))) & c
            end
        else
            for i in eachindex(a)
                n, d = divrem(n, base)
                a[i] = d
            end
        end
    else
        # manually peel one loop iteration for type stability
        n, d = fldmod(n, -base)
        a[firstindex(a)] = d
        n = -signed(n)
        for i in firstindex(a)+1:lastindex(a)
            n, d = fldmod(n, -base)
            a[i] = d
            n = -n
        end
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

"""
    factorial(n::Integer)

Factorial of `n`. If `n` is an [`Integer`](@ref), the factorial is computed as an
integer (promoted to at least 64 bits). Note that this may overflow if `n` is not small,
but you can use `factorial(big(n))` to compute the result exactly in arbitrary precision.

See also [`binomial`](@ref).

# Examples
```jldoctest
julia> factorial(6)
720

julia> factorial(21)
ERROR: OverflowError: 21 is too large to look up in the table; consider using `factorial(big(21))` instead
Stacktrace:
[...]

julia> factorial(big(21))
51090942171709440000
```

# External links
* [Factorial](https://en.wikipedia.org/wiki/Factorial) on Wikipedia.
"""
function factorial(n::Integer)
    n < 0 && throw(DomainError(n, "`n` must be nonnegative."))
    f::typeof(n*n) = 1
    for i::typeof(n*n) = 2:n
        f *= i
    end
    return f
end

"""
    binomial(n::Integer, k::Integer)

The _binomial coefficient_ ``\\binom{n}{k}``, being the coefficient of the ``k``th term in
the polynomial expansion of ``(1+x)^n``.

If ``n`` is non-negative, then it is the number of ways to choose `k` out of `n` items:
```math
\\binom{n}{k} = \\frac{n!}{k! (n-k)!}
```
where ``n!`` is the [`factorial`](@ref) function.

If ``n`` is negative, then it is defined in terms of the identity
```math
\\binom{n}{k} = (-1)^k \\binom{k-n-1}{k}
```

See also [`factorial`](@ref).

# Examples
```jldoctest
julia> binomial(5, 3)
10

julia> factorial(5) ÷ (factorial(5-3) * factorial(3))
10

julia> binomial(-5, 3)
-35
```

# External links
* [Binomial coefficient](https://en.wikipedia.org/wiki/Binomial_coefficient) on Wikipedia.
"""
function binomial(n::T, k::T) where T<:Integer
    n0, k0 = n, k
    k < 0 && return zero(T)
    sgn = one(T)
    if n < 0
        n = -n + k - one(T)
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
    x = nn = n - k + one(T)
    nn += one(T)
    rr = T(2)
    while rr <= k
        xt = div(widemul(x, nn), rr)
        x = xt % T
        x == xt || throw(OverflowError("binomial($n0, $k0) overflows"))
        rr += one(T)
        nn += one(T)
    end
    copysign(x, sgn)
end
