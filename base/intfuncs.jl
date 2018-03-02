# This file is a part of Julia. License is MIT: https://julialang.org/license

## number-theoretic functions ##

"""
    gcd(x,y)

Greatest common (positive) divisor (or zero if `x` and `y` are both zero).

# Examples
```jldoctest
julia> gcd(6,9)
3

julia> gcd(6,-9)
3
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

# binary GCD (aka Stein's) algorithm
# about 1.7x (2.1x) faster for random Int64s (Int128s)
function gcd(a::T, b::T) where T<:Union{Int64,UInt64,Int128,UInt128}
    @noinline throw1(a, b) = throw(OverflowError("gcd($a, $b) overflows"))
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
    r > typemax(T) && throw1(a, b)
    r % T
end

"""
    lcm(x,y)

Least common (non-negative) multiple.

# Examples
```jldoctest
julia> lcm(2,3)
6

julia> lcm(-2,3)
6
```
"""
function lcm(a::T, b::T) where T<:Integer
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

lcm(abc::AbstractArray{<:Integer}) = reduce(lcm,one(eltype(abc)),abc)

function gcd(abc::AbstractArray{<:Integer})
    a = zero(eltype(abc))
    for b in abc
        a = gcd(a,b)
        if a == 1
            return a
        end
    end
    return a
end

# return (gcd(a,b),x,y) such that ax+by == gcd(a,b)
"""
    gcdx(x,y)

Computes the greatest common (positive) divisor of `x` and `y` and their Bézout
coefficients, i.e. the integer coefficients `u` and `v` that satisfy
``ux+vy = d = gcd(x,y)``. ``gcdx(x,y)`` returns ``(d,u,v)``.

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
function gcdx(a::T, b::T) where T<:Integer
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

# Examples
```jldoctest
julia> invmod(2,5)
3

julia> invmod(2,3)
2

julia> invmod(5,6)
5
```
"""
function invmod(n::T, m::T) where T<:Integer
    g, x, y = gcdx(n, m)
    g != 1 && throw(DomainError((n, m), "Greatest common divisor is $g."))
    m == 0 && throw(DomainError(m, "`m` must not be 0."))
    # Note that m might be negative here.
    # For unsigned T, x might be close to typemax; add m to force a wrap-around.
    r = mod(x + m, m)
    # The postcondition is: mod(r * n, m) == mod(T(1), m) && div(r, m) == 0
    r
end
invmod(n::Integer, m::Integer) = invmod(promote(n,m)...)

# ^ for any x supporting *
to_power_type(x) = convert(promote_op(*, typeof(x), typeof(x)), x)
@noinline throw_domerr_powbysq(::Any, p) = throw(DomainError(p,
    string("Cannot raise an integer x to a negative power ", p, '.',
           "\nConvert input to float.")))
@noinline throw_domerr_powbysq(::Integer, p) = throw(DomainError(p,
   string("Cannot raise an integer x to a negative power ", p, '.',
          "\nMake x a float by adding a zero decimal (e.g., 2.0^$p instead ",
          "of 2^$p), or write 1/x^$(-p), float(x)^$p, or (x//1)^$p")))
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

# Core.Compiler has complicated logic to inline x^2 and x^3 for
# numeric types.  In terms of Val we can do it much more simply.
# (The first argument prevents unexpected behavior if a function ^
# is defined that is not equal to Base.^)
@inline literal_pow(::typeof(^), x::HWNumber, ::Val{0}) = one(x)
@inline literal_pow(::typeof(^), x::HWNumber, ::Val{1}) = x
@inline literal_pow(::typeof(^), x::HWNumber, ::Val{2}) = x*x
@inline literal_pow(::typeof(^), x::HWNumber, ::Val{3}) = x*x*x

# don't use the inv(x) transformation here since float^p is slightly more accurate
@inline literal_pow(::typeof(^), x::AbstractFloat, ::Val{p}) where {p} = x^p
@inline literal_pow(::typeof(^), x::AbstractFloat, ::Val{-1}) = inv(x)

# for other types, define x^-n as inv(x)^n so that negative literal powers can
# be computed in a type-stable way even for e.g. integers.
@inline @generated function literal_pow(f::typeof(^), x, ::Val{p}) where {p}
    if p < 0
        :(literal_pow(^, inv(x), $(Val{-p}())))
    else
        :(f(x,$p))
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

    t = prevpow2(p)
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

# smallest power of 2 >= x

"""
    nextpow2(n::Integer)

The smallest power of two not less than `n`. Returns 0 for `n==0`, and returns
`-nextpow2(-n)` for negative arguments.

# Examples
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

# Examples
```jldoctest
julia> prevpow2(5)
4

julia> prevpow2(0)
0
```
"""
prevpow2(x::Unsigned) = one(x) << unsigned((sizeof(x)<<3)-leading_zeros(x)-1)
prevpow2(x::Integer) = reinterpret(typeof(x),x < 0 ? -prevpow2(unsigned(-x)) : prevpow2(unsigned(x)))

"""
    ispow2(n::Integer) -> Bool

Test whether `n` is a power of two.

# Examples
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

See also [`prevpow`](@ref).
"""
function nextpow(a::Real, x::Real)
    a <= 1 && throw(DomainError(a, "`a` must be greater than 1."))
    x <= 0 && throw(DomainError(x, "`x` must be positive."))
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
See also [`nextpow`](@ref).
"""
function prevpow(a::Real, x::Real)
    a <= 1 && throw(DomainError(a, "`a` must be greater than 1."))
    x < 1 && throw(DomainError(x, "`x` must be ≥ 1."))
    n = floor(Integer,log(a, x))
    p = a^(n+1)
    p <= x ? p : a^n
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
function ndigits0z(x::Base.BitUnsigned64)
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

ndigits0z(x::BitSigned) = ndigits0z(unsigned(abs(x)))

ndigits0z(x::Integer) = ndigits0zpb(x, 10)

# TODO (when keywords args are fast): rename to ndigits and make pad a keyword
ndigits10(x::Integer, pad::Int=1) = max(pad, ndigits0z(x))
ndigits(x::Integer) = iszero(x) ? 1 : ndigits0z(x)

## ndigits with specified base ##

# The suffix "nb" stands for "negative base"
function ndigits0znb(x::Integer, b::Integer)
    # precondition: b < -1 && !(typeof(x) <: Unsigned)
    d = 0
    while x != 0
        x = cld(x,b)
        d += 1
    end
    return d
end

ndigits0znb(x::Unsigned, b::Integer) = ndigits0znb(signed(x), b)
ndigits0znb(x::Bool, b::Integer) = x % Int

# The suffix "pb" stands for "positive base"
# TODO: allow b::Integer
function ndigits0zpb(x::Base.BitUnsigned, b::Int)
    # precondition: b > 1
    x == 0 && return 0
    b < 0   && return ndigits0znb(signed(x), b)
    b == 2  && return sizeof(x)<<3 - leading_zeros(x)
    b == 8  && return (sizeof(x)<<3 - leading_zeros(x) + 2) ÷ 3
    b == 16 && return sizeof(x)<<1 - leading_zeros(x)>>2
    b == 10 && return ndigits0z(x)

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

ndigits0zpb(x::Base.BitSigned, b::Integer) = ndigits0zpb(unsigned(abs(x)), Int(b))
ndigits0zpb(x::Base.BitUnsigned, b::Integer) = ndigits0zpb(x, Int(b))
ndigits0zpb(x::Bool, b::Integer) = x % Int

# The suffix "0z" means that the output is 0 on input zero (cf. #16841)
"""
    ndigits0z(n::Integer, b::Integer=10)

Return 0 if `n == 0`, otherwise compute the number of digits in
integer `n` written in base `b` (i.e. equal to `ndigits(n, b)`
in this case).
The base `b` must not be in `[-1, 0, 1]`.

# Examples
```jldoctest
julia> Base.ndigits0z(0, 16)
0

julia> Base.ndigits(0, 16)
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
        throw(DomainError(b, "The base `b` must not be in `[-1, 0, 1]`."))
    end
end

"""
    ndigits(n::Integer, b::Integer=10)

Compute the number of digits in integer `n` written in base `b`.
The base `b` must not be in `[-1, 0, 1]`.

# Examples
```jldoctest
julia> ndigits(12345)
5

julia> ndigits(1022, 16)
3

julia> string(1022, base = 16)
"3fe"
```
"""
ndigits(x::Integer, b::Integer, pad::Int=1) = max(pad, ndigits0z(x, b))

## integer to string functions ##

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
    i = neg + ndigits10(x, pad)
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

const base36digits = ['0':'9';'a':'z']
const base62digits = ['0':'9';'A':'Z';'a':'z']

function _base(b::Int, x::Integer, pad::Int, neg::Bool)
    (x >= 0) | (b < 0) || throw(DomainError(x, "For negative `x`, `b` must be negative."))
    2 <= abs(b) <= 62 || throw(ArgumentError("base must satisfy 2 ≤ abs(base) ≤ 62, got $b"))
    digits = abs(b) <= 36 ? base36digits : base62digits
    i = neg + ndigits(x, b, pad)
    a = StringVector(i)
    @inbounds while i > neg
        if b > 0
            a[i] = digits[1+rem(x,b)]
            x = div(x,b)
        else
            a[i] = digits[1+mod(x,-b)]
            x = cld(x,b)
        end
        i -= 1
    end
    if neg; a[1]='-'; end
    String(a)
end

split_sign(n::Integer) = unsigned(abs(n)), n < 0
split_sign(n::Unsigned) = n, false

"""
    string(n::Integer; base::Integer = 10, pad::Integer = 1)

Convert an integer `n` to a string in the given `base`,
optionally specifying a number of digits to pad to.

```jldoctest
julia> string(5, base = 13, pad = 4)
"0005"

julia> string(13, base = 5, pad = 4)
"0023"
```
"""
function string(n::Integer; base::Integer = 10, pad::Integer = 1)
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
        _base(Int(base), base > 0 ? unsigned(abs(n)) : convert(Signed, n), Int(pad), (base>0) & (n<0))
    end
end

string(b::Bool) = b ? "true" : "false"

"""
    bitstring(n)

A string giving the literal bit representation of a number.

# Examples
```jldoctest
julia> bitstring(4)
"0000000000000000000000000000000000000000000000000000000000000100"

julia> bitstring(2.2)
"0100000000000001100110011001100110011001100110011001100110011010"
```
"""
function bitstring end

bitstring(x::Union{Bool,Int8,UInt8})           = string(reinterpret(UInt8,x), pad = 8, base = 2)
bitstring(x::Union{Int16,UInt16,Float16})      = string(reinterpret(UInt16,x), pad = 16, base = 2)
bitstring(x::Union{Char,Int32,UInt32,Float32}) = string(reinterpret(UInt32,x), pad = 32, base = 2)
bitstring(x::Union{Int64,UInt64,Float64})      = string(reinterpret(UInt64,x), pad = 64, base = 2)
bitstring(x::Union{Int128,UInt128})            = string(reinterpret(UInt128,x), pad = 128, base = 2)

"""
    digits([T<:Integer], n::Integer; base::T = 10, pad::Integer = 1)

Return an array with element type `T` (default `Int`) of the digits of `n` in the given
base, optionally padded with zeros to a specified size. More significant digits are at
higher indices, such that `n == sum([digits[k]*base^(k-1) for k=1:length(digits)])`.

# Examples
```jldoctest
julia> digits(10, base = 10)
2-element Array{Int64,1}:
 0
 1

julia> digits(10, base = 2)
4-element Array{Int64,1}:
 0
 1
 0
 1

julia> digits(10, base = 2, pad = 6)
6-element Array{Int64,1}:
 0
 1
 0
 1
 0
 0
```
"""
digits(n::Integer; base::Integer = 10, pad::Integer = 1) =
    digits(typeof(base), n, base = base, pad = pad)

function digits(T::Type{<:Integer}, n::Integer; base::Integer = 10, pad::Integer = 1)
    digits!(zeros(T, ndigits(n, base, pad)), n, base = base)
end

"""
    hastypemax(T::Type) -> Bool

Return `true` if and only if `typemax(T)` is defined.
"""
hastypemax(::Base.BitIntegerType) = true
hastypemax(::Type{T}) where {T} = applicable(typemax, T)

"""
    digits!(array, n::Integer; base::Integer = 10)

Fills an array of the digits of `n` in the given base. More significant digits are at higher
indices. If the array length is insufficient, the least significant digits are filled up to
the array length. If the array length is excessive, the excess portion is filled with zeros.

# Examples
```jldoctest
julia> digits!([2,2,2,2], 10, base = 2)
4-element Array{Int64,1}:
 0
 1
 0
 1

julia> digits!([2,2,2,2,2,2], 10, base = 2)
6-element Array{Int64,1}:
 0
 1
 0
 1
 0
 0
```
"""
function digits!(a::AbstractVector{T}, n::Integer; base::Integer = 10) where T<:Integer
    base < 0 && isa(n, Unsigned) && return digits!(a, convert(Signed, n), base = base)
    2 <= abs(base) || throw(ArgumentError("base must be ≥ 2 or ≤ -2, got $base"))
    hastypemax(T) && abs(base) - 1 > typemax(T) &&
        throw(ArgumentError("type $T too small for base $base"))
    for i in eachindex(a)
        if base > 0
            a[i] = rem(n, base)
            n = div(n, base)
        else
            a[i] = mod(n, -base)
            n = cld(n, base)
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

function factorial(n::Integer)
    n < 0 && throw(DomainError(n, "`n` must be nonnegative."))
    f::typeof(n*n) = 1
    for i::typeof(n*n) = 2:n
        f *= i
    end
    return f
end

"""
    binomial(n, k)

Number of ways to choose `k` out of `n` items.

# Examples
```jldoctest
julia> binomial(5, 3)
10

julia> factorial(5) ÷ (factorial(5-3) * factorial(3))
10
```
"""
function binomial(n::T, k::T) where T<:Integer
    n0, k0 = n, k
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
        x = xt % T
        x == xt || throw(OverflowError("binomial($n0, $k0) overflows"))
        rr += 1
        nn += 1
    end
    convert(T, copysign(x, sgn))
end
