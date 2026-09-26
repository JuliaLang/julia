# This file is a part of Julia. License is MIT: https://julialang.org/license

# `BitInt{N}` and `BitUInt{N}`: two's-complement integers of arbitrary width,
# built on primitive types whose bit size is a type parameter. Besides being a
# worked example of that feature, this exercises conversion, promotion and the
# integer intrinsics at widths that are not multiples of eight.

using Test

using Core.Intrinsics:
    bitcast, trunc_int, sext_int, zext_int,
    add_int, sub_int, mul_int, neg_int,
    checked_sdiv_int, checked_srem_int, checked_udiv_int, checked_urem_int,
    and_int, or_int, xor_int, not_int,
    shl_int, lshr_int, ashr_int, flipsign_int,
    eq_int, slt_int, sle_int, ult_int, ule_int,
    ctpop_int, ctlz_int, cttz_int

primitive type BitInt{N}  <: Signed   N end
primitive type BitUInt{N} <: Unsigned N end

const NBitInteger = Union{BitInt, BitUInt}

## width and signedness, both statically known ##

nbits(::Type{BitInt{N}})  where {N} = N
nbits(::Type{BitUInt{N}}) where {N} = N
nbits(::Type{T}) where {T<:Base.BitInteger} = 8 * sizeof(T)
nbits(x::Integer) = nbits(typeof(x))

issigned(::Type{<:Signed})   = true
issigned(::Type{<:Unsigned}) = false

## conversion ##

# Reinterpret `x` at the width of `To`: truncate when narrowing, and when
# widening replicate the sign bit or zero-fill according to `From`. This is the
# raw bit-level move; `_convert` adds the range check on top of it.
@inline function _extend(::Type{To}, x::From) where {To,From}
    n, m = nbits(To), nbits(From)
    n == m ? bitcast(To, x) :
    n <  m ? trunc_int(To, x) :
    issigned(From) ? sext_int(To, x) : zext_int(To, x)
end

@inline _isneg(x::T) where {T} = issigned(T) && slt_int(x, _extend(T, 0x00))

# every value of `From` fits in `To`, so no check is needed
@inline _always_exact(::Type{To}, ::Type{From}) where {To,From} =
    issigned(To) == issigned(From) ? nbits(To) >= nbits(From) :
    issigned(To) ? nbits(To) > nbits(From) : false

# `x` is representable in `To` exactly when the bits survive the round trip and
# the sign does not change. The sign test is what rules out e.g. `Int8(0xff)`,
# where the round trip alone would be lossless.
@inline function _convert(::Type{To}, x::From) where {To,From}
    y = _extend(To, x)
    _always_exact(To, From) && return y
    (_isneg(x) === _isneg(y) && eq_int(_extend(From, y), x)) ||
        throw(InexactError(:convert, To, x))
    y
end

BitInt{N}(x::BitInt{N})   where {N} = x
BitUInt{N}(x::BitUInt{N}) where {N} = x

(::Type{T})(x::NBitInteger)      where {T<:NBitInteger}     = _convert(T, x)
(::Type{T})(x::Base.BitInteger)  where {T<:NBitInteger}     = _convert(T, x)
(::Type{T})(x::NBitInteger)      where {T<:Base.BitInteger} = _convert(T, x)
(::Type{T})(x::Bool)             where {T<:NBitInteger}     = ifelse(x, one(T), zero(T))

# `%`: truncating conversion, never throws
Base.rem(x::BitInt{N},  ::Type{BitInt{N}})  where {N} = x
Base.rem(x::BitUInt{N}, ::Type{BitUInt{N}}) where {N} = x
Base.rem(x::NBitInteger,     ::Type{T}) where {T<:NBitInteger}     = _extend(T, x)
Base.rem(x::Base.BitInteger, ::Type{T}) where {T<:NBitInteger}     = _extend(T, x)
Base.rem(x::NBitInteger,     ::Type{T}) where {T<:Base.BitInteger} = _extend(T, x)

Base.signed(::Type{BitInt{N}})    where {N} = BitInt{N}
Base.signed(::Type{BitUInt{N}})   where {N} = BitInt{N}
Base.unsigned(::Type{BitInt{N}})  where {N} = BitUInt{N}
Base.unsigned(::Type{BitUInt{N}}) where {N} = BitUInt{N}

Base.signed(x::BitUInt{N})   where {N} = bitcast(BitInt{N}, x)
Base.unsigned(x::BitInt{N})  where {N} = bitcast(BitUInt{N}, x)

Base.Signed(x::BitUInt{N})  where {N} = BitInt{N}(x)
Base.Unsigned(x::BitInt{N}) where {N} = BitUInt{N}(x)

## constants and limits ##

Base.zero(::Type{T}) where {T<:NBitInteger} = _extend(T, 0x00)
Base.one(::Type{T})  where {T<:NBitInteger} = _extend(T, 0x01)

Base.typemin(::Type{BitUInt{N}}) where {N} = zero(BitUInt{N})
Base.typemax(::Type{BitUInt{N}}) where {N} = not_int(zero(BitUInt{N}))
Base.typemin(::Type{BitInt{N}})  where {N} = shl_int(one(BitInt{N}), N - 1)
Base.typemax(::Type{BitInt{N}})  where {N} = not_int(typemin(BitInt{N}))

# a doubled width always holds a product, which is what `widemul` needs
Base.widen(::Type{BitInt{N}})  where {N} = BitInt{2N}
Base.widen(::Type{BitUInt{N}}) where {N} = BitUInt{2N}

## arithmetic ##

Base.:(-)(x::NBitInteger)                    = neg_int(x)
Base.:(+)(x::T, y::T) where {T<:NBitInteger} = add_int(x, y)
Base.:(-)(x::T, y::T) where {T<:NBitInteger} = sub_int(x, y)
Base.:(*)(x::T, y::T) where {T<:NBitInteger} = mul_int(x, y)

Base.:(-%)(x::NBitInteger)                    = neg_int(x)
Base.:(+%)(x::T, y::T) where {T<:NBitInteger} = add_int(x, y)
Base.:(-%)(x::T, y::T) where {T<:NBitInteger} = sub_int(x, y)
Base.:(*%)(x::T, y::T) where {T<:NBitInteger} = mul_int(x, y)

Base.div(x::T, y::T) where {T<:BitInt}  = checked_sdiv_int(x, y)
Base.rem(x::T, y::T) where {T<:BitInt}  = checked_srem_int(x, y)
Base.div(x::T, y::T) where {T<:BitUInt} = checked_udiv_int(x, y)
Base.rem(x::T, y::T) where {T<:BitUInt} = checked_urem_int(x, y)

# `fld`, `mod` and friends reach the two-argument form through this
Base.div(x::T, y::T, ::typeof(RoundToZero)) where {T<:NBitInteger} = div(x, y)

Base.flipsign(x::T, y::T) where {T<:BitInt} = flipsign_int(x, y)

## bitwise ##

Base.:(~)(x::NBitInteger)                      = not_int(x)
Base.:(&)(x::T, y::T)   where {T<:NBitInteger} = and_int(x, y)
Base.:(|)(x::T, y::T)   where {T<:NBitInteger} = or_int(x, y)
Base.xor(x::T, y::T)    where {T<:NBitInteger} = xor_int(x, y)

# the generic operators in Base reduce every shift count to a `UInt`
Base.:(>>)(x::BitInt,       y::UInt) = ashr_int(x, y)
Base.:(>>)(x::BitUInt,      y::UInt) = lshr_int(x, y)
Base.:(<<)(x::NBitInteger,  y::UInt) = shl_int(x, y)
Base.:(>>>)(x::NBitInteger, y::UInt) = lshr_int(x, y)

# a count of up to `N` need not fit in `BitInt{N}` (e.g. `N == 2`), so read the
# result back through the unsigned companion, where it always does
@inline _count(x::T) where {T<:NBitInteger} = Int(bitcast(unsigned(T), x) % UInt)

Base.count_ones(x::NBitInteger)     = _count(ctpop_int(x))
Base.leading_zeros(x::NBitInteger)  = _count(ctlz_int(x))
Base.trailing_zeros(x::NBitInteger) = _count(cttz_int(x))
Base.top_set_bit(x::NBitInteger)    = nbits(x) - leading_zeros(x)

## comparison ##

Base.:(==)(x::T, y::T) where {T<:NBitInteger} = eq_int(x, y)
Base.:(<)(x::T, y::T)  where {T<:BitInt}      = slt_int(x, y)
Base.:(<)(x::T, y::T)  where {T<:BitUInt}     = ult_int(x, y)
Base.:(<=)(x::T, y::T) where {T<:BitInt}      = sle_int(x, y)
Base.:(<=)(x::T, y::T) where {T<:BitUInt}     = ule_int(x, y)

## promotion ##

# The result must hold every value of both operands: a signed type wins only
# when it is strictly wider than the unsigned one, matching `Int32`/`UInt32`.
_promote_nbits(N, M) = N > M ? N : M

Base.promote_rule(::Type{BitInt{N}},  ::Type{BitInt{M}})  where {N,M} = BitInt{_promote_nbits(N, M)}
Base.promote_rule(::Type{BitUInt{N}}, ::Type{BitUInt{M}}) where {N,M} = BitUInt{_promote_nbits(N, M)}
Base.promote_rule(::Type{BitInt{N}},  ::Type{BitUInt{M}}) where {N,M} = N > M ? BitInt{N} : BitUInt{M}

Base.promote_rule(::Type{BitInt{N}},  ::Type{T}) where {N,T<:Base.BitSigned}   = BitInt{_promote_nbits(N, 8sizeof(T))}
Base.promote_rule(::Type{BitUInt{N}}, ::Type{T}) where {N,T<:Base.BitUnsigned} = BitUInt{_promote_nbits(N, 8sizeof(T))}
Base.promote_rule(::Type{BitInt{N}},  ::Type{T}) where {N,T<:Base.BitUnsigned} = N > 8sizeof(T) ? BitInt{N} : BitUInt{8sizeof(T)}
Base.promote_rule(::Type{BitUInt{N}}, ::Type{T}) where {N,T<:Base.BitSigned}   = 8sizeof(T) > N ? BitInt{8sizeof(T)} : BitUInt{N}

## exact value, for arbitrary width ##

_wide(x::BitInt{N})  where {N} = Int64(x)
_wide(x::BitUInt{N}) where {N} = UInt64(x)

function _bigint(x::T) where {T<:NBitInteger}
    n, u, r = nbits(T), unsigned(x), big(0)
    for k in 0:32:n-1
        r |= big(u >>> UInt(k) % BitUInt{32} % UInt32) << k
    end
    _isneg(x) ? r - (big(1) << n) : r
end

Base.BigInt(x::T) where {T<:NBitInteger} = nbits(T) <= 64 ? big(_wide(x)) : _bigint(x)

(::Type{T})(x::NBitInteger) where {T<:AbstractFloat} = T(BigInt(x))
Base.AbstractFloat(x::NBitInteger) = Float64(x)
Base.:(/)(x::T, y::T) where {T<:NBitInteger} = float(x) / float(y)

## tests ##

using Random

# reference semantics: an N-bit two's-complement window onto the integers
_wrap(::Type{BitUInt{N}}, v) where {N} = mod(big(v), big(1) << N)
_wrap(::Type{BitInt{N}},  v) where {N} = mod(big(v) + (big(1) << (N-1)), big(1) << N) - (big(1) << (N-1))

function _make(::Type{T}, v::Integer) where {T<:NBitInteger}
    n = nbits(T)
    u = mod(big(v), big(1) << n)
    r = zero(T)
    for k in 0:32:n-1
        r |= (UInt32((u >> k) & 0xffffffff) % T) << UInt(k)
    end
    r
end

function _randval(rng, n)
    v = big(0)
    for k in 0:64:n-1
        v |= big(rand(rng, UInt64)) << k
    end
    v & ((big(1) << n) - 1)
end

const WIDTHS = (2, 3, 8, 13, 16, 21, 32, 47, 64, 100)

@testset "parametric primitive integers" begin

@testset "layout" begin
    @test Core.bitsizeof(BitInt{21}) == 21
    @test Core.bitsizeof(BitUInt{21}) == 21
    @test isbitstype(BitInt{21}) && isbitstype(BitUInt{21})
    @test BitInt{21} <: Signed && BitUInt{21} <: Unsigned
    @test !isconcretetype(BitInt) && !isconcretetype(BitUInt)
end

@testset "values round-trip through BigInt" begin
    rng = Xoshiro(0x51ce)
    for n in WIDTHS, T in (BitInt{n}, BitUInt{n}), _ in 1:20
        v = _randval(rng, n)
        @test BigInt(_make(T, v)) == _wrap(T, v)
    end
    @test BigInt(zero(BitInt{100})) == 0
    @test BigInt(one(BitUInt{100})) == 1
end

@testset "arithmetic matches arbitrary precision" begin
    rng = Xoshiro(0xa11e)
    for n in WIDTHS, T in (BitInt{n}, BitUInt{n}), _ in 1:20
        x, y = _make(T, _randval(rng, n)), _make(T, _randval(rng, n))
        a, b = BigInt(x), BigInt(y)

        @test BigInt(x + y) == _wrap(T, a + b)
        @test BigInt(x - y) == _wrap(T, a - b)
        @test BigInt(x * y) == _wrap(T, a * b)
        @test BigInt(-x)    == _wrap(T, -a)
        @test BigInt(x +% y) == BigInt(x + y)
        @test BigInt(x -% y) == BigInt(x - y)
        @test BigInt(x *% y) == BigInt(x * y)

        @test BigInt(~x)     == _wrap(T, ~a)
        @test BigInt(x & y)  == _wrap(T, a & b)
        @test BigInt(x | y)  == _wrap(T, a | b)
        @test BigInt(xor(x, y)) == _wrap(T, xor(a, b))

        @test (x <  y) == (a <  b)
        @test (x <= y) == (a <= b)
        @test (x == y) == (a == b)
        @test isequal(x, y) == (a == b)

        # `typemin ÷ -1` is the one signed quotient that does not exist
        if !iszero(y) && !(x === typemin(T) && b == -1)
            @test BigInt(div(x, y)) == _wrap(T, div(a, b))
            @test BigInt(rem(x, y)) == _wrap(T, rem(a, b))
            @test BigInt(fld(x, y)) == _wrap(T, fld(a, b))
            @test BigInt(mod(x, y)) == _wrap(T, mod(a, b))
        end

        for k in (0, 1, n ÷ 2, n - 1, n, n + 7)
            @test BigInt(x << k)  == _wrap(T, a * big(2)^k)
            @test BigInt(x >> k)  == _wrap(T, fld(a, big(2)^k))
            @test BigInt(x >>> k) == _wrap(T, BigInt(unsigned(x)) >> k)
        end

        u = BigInt(unsigned(x))
        @test count_ones(x)     == count_ones(u)
        @test Base.top_set_bit(x) == (iszero(u) ? 0 : ndigits(u, base=2))
        @test leading_zeros(x)  == n - Base.top_set_bit(x)
        @test trailing_zeros(x) == (iszero(u) ? n : trailing_zeros(u))
    end
end

@testset "matches Base at matching widths" begin
    rng = Xoshiro(0xba5e)
    for B in (Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64)
        T = B <: Signed ? BitInt{8sizeof(B)} : BitUInt{8sizeof(B)}
        for _ in 1:50
            a, b = rand(rng, B), rand(rng, B)
            x, y = T(a), T(b)
            @test T(a +% b) === x + y
            @test T(a -% b) === x - y
            @test T(a *% b) === x * y
            @test T(~a)     === ~x
            @test T(a & b)  === x & y
            @test T(a | b)  === x | y
            @test T(xor(a, b)) === xor(x, y)
            @test (a < b)   === (x < y)
            @test (a <= b)  === (x <= y)
            @test count_ones(a) === count_ones(x)
            @test leading_zeros(a) === leading_zeros(x)
            @test trailing_zeros(a) === trailing_zeros(x)
            for k in (0, 3, 8sizeof(B) - 1, 8sizeof(B) + 1)
                @test T(a << k)  === x << k
                @test T(a >> k)  === x >> k
                @test T(a >>> k) === x >>> k
            end
            iszero(b) || (a === typemin(B) && b == -1) || begin
                @test T(div(a, b)) === div(x, y)
                @test T(rem(a, b)) === rem(x, y)
            end
        end
        @test T(typemin(B)) === typemin(T)
        @test T(typemax(B)) === typemax(T)
    end
    @test_throws DivideError div(typemin(BitInt{21}), BitInt{21}(-1))
    @test_throws DivideError div(BitInt{21}(1), zero(BitInt{21}))
end

@testset "conversion" begin
    @test BitInt{47}(BitInt{21}(-5)) === BitInt{47}(-5)
    @test BitInt{5}(BitInt{21}(-5))  === BitInt{5}(-5)
    @test BitUInt{47}(BitUInt{21}(5)) === BitUInt{47}(5)
    @test BitInt{22}(typemax(BitUInt{21})) === BitInt{22}(2^21 - 1)
    @test BitUInt{21}(BitInt{21}(5)) === BitUInt{21}(5)
    @test Int64(BitInt{21}(-1)) === -1
    @test Int64(typemax(BitUInt{21})) === Int64(2^21 - 1)
    @test BitInt{21}(Int64(-1)) === BitInt{21}(-1)
    @test BitInt{100}(typemin(Int64)) === _make(BitInt{100}, typemin(Int64))
    @test BitInt{21}(true) === one(BitInt{21})
    @test BitInt{21}(false) === zero(BitInt{21})
    @test convert(BitInt{47}, BitInt{21}(-5)) === BitInt{47}(-5)

    @test_throws InexactError BitInt{5}(BitInt{21}(100))
    @test_throws InexactError BitUInt{21}(BitInt{21}(-1))
    @test_throws InexactError BitInt{21}(typemax(BitUInt{21}))
    @test_throws InexactError BitUInt{21}(Int64(-1))
    @test_throws InexactError BitInt{21}(Int64(1) << 21)
    @test_throws InexactError Int8(BitInt{21}(1000))
    @test_throws InexactError UInt64(BitInt{21}(-1))

    # `%` truncates instead of throwing
    rng = Xoshiro(0xc0de)
    for n in WIDTHS, m in WIDTHS, S in (BitInt{n}, BitUInt{n}), T in (BitInt{m}, BitUInt{m})
        x = _make(S, _randval(rng, n))
        @test BigInt(x % T) == _wrap(T, BigInt(x))
    end
    @test BitInt{21}(-1) % Int64 === -1
    @test Int64(-1) % BitInt{21} === BitInt{21}(-1)
    @test BitInt{21}(-1) % BitInt{21} === BitInt{21}(-1)
end

@testset "signedness" begin
    @test signed(BitUInt{21}) === BitInt{21}
    @test signed(BitInt{21}) === BitInt{21}
    @test unsigned(BitInt{21}) === BitUInt{21}
    @test unsigned(BitUInt{21}) === BitUInt{21}
    @test signed(typemax(BitUInt{21})) === BitInt{21}(-1)
    @test unsigned(BitInt{21}(-1)) === typemax(BitUInt{21})
    @test Signed(BitUInt{21}(5)) === BitInt{21}(5)
    @test Unsigned(BitInt{21}(5)) === BitUInt{21}(5)
    @test_throws InexactError Signed(typemax(BitUInt{21}))
    @test_throws InexactError Unsigned(BitInt{21}(-1))
end

@testset "limits and widening" begin
    for n in WIDTHS
        @test BigInt(typemin(BitInt{n})) == -big(2)^(n-1)
        @test BigInt(typemax(BitInt{n})) == big(2)^(n-1) - 1
        @test typemin(BitUInt{n}) === zero(BitUInt{n})
        @test BigInt(typemax(BitUInt{n})) == big(2)^n - 1
        @test typemax(BitInt{n}) + one(BitInt{n}) === typemin(BitInt{n})
        @test typemax(BitUInt{n}) + one(BitUInt{n}) === zero(BitUInt{n})
        @test widen(BitInt{n}) === BitInt{2n}
        @test widen(BitUInt{n}) === BitUInt{2n}
        @test widen(typemin(BitInt{n})) === BitInt{2n}(typemin(BitInt{n}))
    end
    let x = typemin(BitInt{21})
        @test BigInt(widemul(x, x)) == big(2)^40
    end
    @test BigInt(widemul(typemax(BitUInt{13}), typemax(BitUInt{13}))) == (big(2)^13 - 1)^2
end

@testset "promotion" begin
    @test promote_type(BitInt{21}, BitInt{47}) === BitInt{47}
    @test promote_type(BitUInt{21}, BitUInt{47}) === BitUInt{47}
    @test promote_type(BitInt{21}, BitUInt{21}) === BitUInt{21}
    @test promote_type(BitInt{47}, BitUInt{21}) === BitInt{47}
    @test promote_type(BitInt{21}, Int32) === BitInt{32}
    @test promote_type(BitInt{47}, Int32) === BitInt{47}
    @test promote_type(BitUInt{21}, UInt64) === BitUInt{64}
    @test promote_type(BitInt{21}, UInt64) === BitUInt{64}
    @test promote_type(BitUInt{21}, Int64) === BitInt{64}
    @test promote_type(BitUInt{64}, Int32) === BitUInt{64}

    @test BitInt{21}(3) + Int32(4) === BitInt{32}(7)
    @test BitInt{21}(3) < Int64(4)
    @test BitInt{21}(3) == 3
    @test BitUInt{21}(3) * BitInt{47}(-2) === BitInt{47}(-6)
end

@testset "printing" begin
    @test string(BitInt{21}(-1000)) == "-1000"
    @test string(BitInt{21}(-1000), base=16) == "-3e8"
    @test string(BitUInt{21}(0x1234), base=16) == "1234"
    @test repr(BitUInt{21}(1)) == "0x000001"
    @test sprint(show, BitInt{21}(-1000)) == "-1000"
    @test string(typemax(BitInt{100})) == string(big(2)^99 - 1)
    @test string(typemin(BitInt{100})) == string(-big(2)^99)
end

@testset "inference and unboxing" begin
    f(x, y) = (x + y) * x - div(x, y)
    @test @inferred(f(BitInt{21}(7), BitInt{21}(3))) === BitInt{21}(68)
    @test @inferred(BitInt{47}(BitInt{21}(-1))) === BitInt{47}(-1)
    @test @inferred(typemax(BitInt{21})) === BitInt{21}(2^20 - 1)
    @test @inferred(BitInt{21}(-1) % BitUInt{13}) === typemax(BitUInt{13})
    @test @inferred(widen(BitInt{21}(-1))) === BitInt{42}(-1)
    @test Base.return_types(+, (BitInt{21}, BitInt{21})) == [BitInt{21}]

    # a concrete width is an ordinary isbits type: it stays unboxed
    g(x, n) = (s = zero(x); for _ in 1:n; s += x; end; s)
    g(BitInt{21}(3), 10)
    @test @allocated(g(BitInt{21}(3), 1000)) == 0
    a = fill(BitInt{21}(7), 4)
    @test Base.elsize(a) == Base.aligned_sizeof(BitInt{21})
    @test sum(a) === BitInt{21}(28)
end

end
