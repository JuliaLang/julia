# This file is a part of Julia. License is MIT: http://julialang.org/license

# Test integer conversion routines from int.jl


for y in (-4, Float32(-4), -4.0, big(-4.0))
    @test flipsign(3, y)  == -3
    @test flipsign(-3, y) == 3
    @test copysign(3, y)  == -3
    @test copysign(-3, y) == -3
end

for y in (4, Float32(4), 4.0, big(4.0))
    @test flipsign(3, y)  == 3
    @test flipsign(-3, y) == -3
    @test copysign(3, y)  == 3
    @test copysign(-3, y) == 3
end

@test signed(3) == 3
@test signed(UInt(3)) == 3
@test isa(signed(UInt(3)), Int)
@test signed(UInt(0) - 1) == -1
@test_throws InexactError signed(UInt(-3))

@test bswap(Int8(3)) == 3
@test bswap(UInt8(3)) == 3
@test bswap(Int16(3)) == 256*3
@test bswap(Int16(256)) == 1
@test bswap(Int16(257)) == 257
@test bswap(Int32(1)) == 2^(3*8)
@test bswap(Int32(2)^(3*8)) == 1
@test bswap(Int64(1)) == Int64(2)^(7*8)
@test bswap(Int64(2)^(7*8)) == 1
@test bswap(Int128(1)) == Int128(2)^(15*8)
@test bswap(Int128(2)^(15*8)) == Int128(1)
@test bswap(UInt128(2)^(15*8)) == UInt128(1)

@test count_zeros(10) == WORD_SIZE - 2
@test count_zeros(UInt8(10)) == 6

@test convert(Signed, UInt128(3)) === Int128(3)
@test convert(Signed, false) === 0
@test convert(Signed, true) === 1

for (II, UU) in ((Int8,UInt8), (Int16,UInt16),
                 (Int32,UInt32), (Int64,UInt64),
                 (Int128,UInt128))
    @test convert(Unsigned, II(3)) === UU(3)
end

for T in (Float32, Float64)
    @test convert(Unsigned, T(3.0)) === UInt(3)
end

@test trunc(3) == 3
@test trunc(Integer, 3) == 3

@test floor(3) == 3
@test ceil(3) == 3

@test big"2"^100 == BigInt(2)^100
@test isa(big"2", BigInt)
@test big"1.0" == BigFloat(1.0)
@test_throws ArgumentError big"1.0.3"
@test_throws ArgumentError big"pi"

@test round(UInt8, 123) == 123
@test mod(123, UInt8) == 0x7b

bitstype 8 MyBitsType <: Integer
@test_throws MethodError ~reinterpret(MyBitsType, 0x7b)

UItypes = (UInt8, UInt16, UInt32, UInt64, UInt128)
SItypes = (Int8, Int16, Int32, Int64, Int128)

for T in UItypes, S in UItypes
    @test promote(S(3), T(3)) === (sizeof(T) < sizeof(S) ? (S(3), S(3)) : (T(3), T(3)))
end

for T in SItypes, S in SItypes
    @test promote(S(3), T(3)) === (sizeof(T) < sizeof(S) ? (S(3), S(3)) : (T(3), T(3)))
end

for T in SItypes, S in UItypes
    R = sizeof(S) < sizeof(Int) ? Int : S
    @test promote(R(3), T(3)) === (sizeof(R) < sizeof(T) ? (T(3), T(3)) : (R(3), R(3)))
end

# Test limiting conversions
for T in (Int8, Int16, Int32, Int64)
    max_val = Int128(typemax(T))
    @test convert(T, max_val) == max_val
    @test_throws InexactError convert(T, max_val+1)

    m = Int128(typemin(T))
    @test convert(T, m) == m
    @test_throws InexactError convert(T, m-1)
end

for T in (UInt8, UInt16, UInt32, UInt64)
    max_val = Int128(typemax(T))
    @test convert(T, max_val) == max_val
    @test_throws InexactError convert(T, max_val+1)
    @test_throws InexactError convert(T, -1)
end

for i in 1:length(UItypes)-1
    T = UItypes[i]
    S = UItypes[i+1]
    R = sizeof(S) < sizeof(UInt) ? S : UInt
    @test widen(T(3)) == R(3)
end

for i in 1:length(SItypes)-1
    T = SItypes[i]
    S = SItypes[i+1]
    R = sizeof(S) < sizeof(Int) ? S : Int
    @test widen(T(3)) == R(3)
end

@test widemul(false, false) == false
@test widemul(false, 3) == 0
@test widemul(3, true) == widemul(true, 3) == 3

# checked operations

#=
import Base: checked_abs, checked_neg, checked_add, checked_sub, checked_mul,
             checked_div, checked_rem, checked_fld, checked_mod
for T in (Int8, Int16, Int32, Int64, Int128)
    # regular cases
    for s in (-1, +1)
        @test checked_abs(T(0s)) === T(abs(0s))
        @test checked_neg(T(0s)) === T(-(0s))
        @test checked_abs(T(3s)) === T(abs(3s))
        @test checked_neg(T(3s)) === T(-(3s))
        @test checked_abs(T(s*typemax(T))) === typemax(T)
        @test checked_neg(T(s*typemax(T))) === T(-s*typemax(T))
    end
    # corner cases
    @test_throws OverflowError checked_abs(typemin(T))
    @test_throws OverflowError checked_neg(typemin(T))
    # regular cases
    for s1 in (-1, +1), s2 in (-1,+1)
        @test checked_add(T(4s1), T(3s2)) === T(4s1 + 3s2)
        @test checked_sub(T(4s1), T(3s2)) === T(4s1 - 3s2)
        @test checked_mul(T(4s1), T(3s2)) === T(4s1 * 3s2)
        @test checked_div(T(4s1), T(3s2)) === T(div(4s1, 3s2))
        @test checked_rem(T(4s1), T(3s2)) === T(rem(4s1, 3s2))
        @test checked_fld(T(4s1), T(3s2)) === T(fld(4s1, 3s2))
        @test checked_mod(T(4s1), T(3s2)) === T(mod(4s1, 3s2))
    end
    # corner cases
    max2 = T(typemax(T)÷2)
    max21 = T(max2+1)
    min2 = T(typemin(T)÷2)
    min21 = T(min2-1)
    sqrt2 = T(1) << T(sizeof(T)*4)

    @test checked_add(typemax(T), T(-1)) === T(typemax(T) - 1)
    @test_throws OverflowError checked_add(typemin(T), T(-1))
    @test checked_add(typemax(T), T(0)) === typemax(T)
    @test checked_add(typemin(T), T(0)) === typemin(T)
    @test_throws OverflowError checked_add(typemax(T), T(1))
    @test checked_add(typemin(T), T(1)) === T(typemin(T) + 1)
    @test checked_add(T(-1), typemax(T)) === T(typemax(T) - 1)
    @test_throws OverflowError checked_add(T(-1), typemin(T))
    @test checked_add(T(0), typemax(T)) === typemax(T)
    @test checked_add(T(0), typemin(T)) === typemin(T)
    @test_throws OverflowError checked_add(T(1), typemax(T))
    @test checked_add(T(1), typemin(T)) === T(typemin(T) + 1)
    @test checked_add(typemax(T), typemin(T)) === T(-1)
    @test checked_add(typemin(T), typemax(T)) === T(-1)
    @test_throws OverflowError checked_add(max21, max21)
    @test_throws OverflowError checked_add(min2, min21)

    @test_throws OverflowError checked_sub(typemax(T), T(-1))
    @test checked_sub(typemax(T), T(0)) === typemax(T)
    @test checked_sub(typemax(T), T(1)) === T(typemax(T) - 1)
    @test checked_sub(typemin(T), T(-1)) === T(typemin(T) + 1)
    @test checked_sub(typemin(T), T(0)) === typemin(T)
    @test_throws OverflowError checked_sub(typemin(T), T(1))
    @test checked_sub(T(0), typemax(T)) === T(typemin(T) + 1)
    @test checked_sub(T(1), typemax(T)) === T(typemin(T) + 2)
    @test checked_sub(T(-1), typemin(T)) === typemax(T)
    @test_throws OverflowError checked_sub(T(0), typemin(T))
    @test checked_sub(typemax(T), typemax(T)) === T(0)
    @test checked_sub(typemin(T), typemin(T)) === T(0)
    @test checked_sub(max2, T(-min2)) === T(-1)
    @test_throws OverflowError checked_sub(min2, T(-min21))

    @test checked_mul(typemax(T), T(0)) === T(0)
    @test checked_mul(typemin(T), T(0)) === T(0)
    @test checked_mul(typemax(T), T(1)) === typemax(T)
    @test checked_mul(typemin(T), T(1)) === typemin(T)
    @test_throws OverflowError checked_mul(sqrt2, sqrt2)
    @test_throws OverflowError checked_mul(sqrt2, -sqrt2)
    @test_throws OverflowError checked_mul(-sqrt2, sqrt2)
    @test_throws OverflowError checked_mul(-sqrt2, -sqrt2)

    @test checked_div(typemax(T), T(1)) === typemax(T)
    @test_throws DivideError checked_div(typemax(T), T(0))
    @test checked_div(typemax(T), T(-1)) === T(-typemax(T))
    @test checked_div(typemin(T), T(1)) === typemin(T)
    @test_throws DivideError checked_div(typemin(T), T(0))
    @test_throws DivideError checked_div(typemin(T), T(-1))
    @test checked_rem(typemax(T), T(1)) === T(0)
    @test_throws DivideError checked_rem(typemax(T), T(0))
    @test checked_rem(typemax(T), T(-1)) === T(0)
    @test checked_rem(typemin(T), T(1)) === T(0)
    @test_throws DivideError checked_rem(typemin(T), T(0))
    @test checked_rem(typemin(T), T(-1)) === T(0)
    @test checked_fld(typemax(T), T(1)) === typemax(T)
    @test_throws DivideError checked_fld(typemax(T), T(0))
    @test checked_fld(typemax(T), T(-1)) === T(-typemax(T))
    @test checked_fld(typemin(T), T(1)) === typemin(T)
    @test_throws DivideError checked_fld(typemin(T), T(0))
    @test_throws DivideError checked_fld(typemin(T), T(-1))
    @test checked_mod(typemax(T), T(1)) === T(0)
    @test_throws DivideError checked_mod(typemax(T), T(0))
    @test checked_mod(typemax(T), T(-1)) === T(0)
    @test checked_mod(typemin(T), T(1)) === T(0)
    @test_throws DivideError checked_mod(typemin(T), T(0))
    @test checked_mod(typemin(T), T(-1)) === T(0)
end

for T in (UInt8, UInt16, UInt32, UInt64, UInt128)
    # regular cases
    @test checked_abs(T(0)) === T(0)
    @test checked_neg(T(0)) === T(0)
    @test checked_abs(T(3)) === T(3)
    @test_throws OverflowError checked_neg(T(3))
    # regular cases
    @test checked_add(T(4), T(3)) === T(7)
    @test checked_sub(T(4), T(3)) === T(1)
    @test checked_mul(T(4), T(3)) === T(12)
    # corner cases
    max2 = T(typemax(T)÷2)
    max21 = T(max2+1)
    sqrt2 = T(1) << T(sizeof(T)*4)

    @test checked_add(typemax(T), T(0)) === typemax(T)
    @test checked_add(T(0), T(0)) === T(0)
    @test_throws OverflowError checked_add(typemax(T), T(1))
    @test checked_add(T(0), T(1)) === T(T(0) + 1)
    @test checked_add(T(0), typemax(T)) === typemax(T)
    @test checked_add(T(0), T(0)) === T(0)
    @test_throws OverflowError checked_add(T(1), typemax(T))
    @test checked_add(T(1), T(0)) === T(T(0) + 1)
    @test checked_add(typemax(T), T(0)) === typemax(T)
    @test checked_add(T(0), typemax(T)) === typemax(T)
    @test_throws OverflowError checked_add(max21, max21)

    @test checked_sub(typemax(T), T(0)) === typemax(T)
    @test checked_sub(typemax(T), T(1)) === T(typemax(T) - 1)
    @test checked_sub(T(0), T(0)) === T(0)
    @test_throws OverflowError checked_sub(T(0), T(1))
    @test_throws OverflowError checked_sub(T(0), typemax(T))
    @test_throws OverflowError checked_sub(T(1), typemax(T))
    @test checked_sub(T(0), T(0)) === T(0)
    @test checked_sub(typemax(T), typemax(T)) === T(0)

    @test checked_mul(typemax(T), T(0)) === T(0)
    @test checked_mul(T(0), T(0)) === T(0)
    @test checked_mul(typemax(T), T(1)) === typemax(T)
    @test checked_mul(T(0), T(1)) === T(0)
    @test_throws OverflowError checked_mul(sqrt2, sqrt2)

    @test checked_div(typemax(T), T(1)) === typemax(T)
    @test_throws DivideError checked_div(typemax(T), T(0))
    @test checked_rem(typemax(T), T(1)) === T(0)
    @test_throws DivideError checked_rem(typemax(T), T(0))
    @test checked_fld(typemax(T), T(1)) === typemax(T)
    @test_throws DivideError checked_fld(typemax(T), T(0))
    @test checked_mod(typemax(T), T(1)) === T(0)
    @test_throws DivideError checked_mod(typemax(T), T(0))
end

@test checked_abs(BigInt(-1)) == BigInt(1)
@test checked_abs(BigInt(1)) == BigInt(1)
@test checked_neg(BigInt(-1)) == BigInt(1)
@test checked_neg(BigInt(1)) == BigInt(-1)

# Additional tests

@test checked_sub(UInt(4), UInt(3)) === UInt(1)
@test_throws OverflowError checked_sub(UInt(5), UInt(6))
@test checked_mul(UInt(4), UInt(3)) === UInt(12)

@test checked_sub(Int128(-1),Int128(-2)) === Int128(1)

if WORD_SIZE == 32
    @test_throws OverflowError checked_mul(UInt(2)^30, UInt(2)^2)
else
    @test_throws OverflowError checked_mul(UInt(2)^62, UInt(2)^2)
end

# Checked operations on UInt128 are currently broken
# FIXME: #4905

@test checked_add(UInt128(1), UInt128(2)) === UInt128(3)
@test_throws OverflowError checked_add(UInt128(2)^127, UInt128(2)^127)

@test checked_sub(UInt128(2), UInt128(1)) === UInt128(1)
@test_throws OverflowError checked_sub(UInt128(3), UInt128(4))

@test checked_mul(UInt128(3), UInt128(4)) === UInt128(12)
@test_throws OverflowError checked_mul(UInt128(2)^127, UInt128(2))
=#

# unchecked operations

#=
import Base: unchecked_abs, unchecked_neg,
             unchecked_add, unchecked_sub, unchecked_mul,
             unchecked_div, unchecked_rem, unchecked_fld, unchecked_mod
for T in (Int8, Int16, Int32, Int64, Int128)
    # regular cases
    for s in (-1, +1)
        @test unchecked_abs(T(0s)) === T(abs(0s))
        @test unchecked_neg(T(0s)) === T(-(0s))
        @test unchecked_abs(T(3s)) === T(abs(3s))
        @test unchecked_neg(T(3s)) === T(-(3s))
        @test unchecked_abs(T(s*typemax(T))) === typemax(T)
        @test unchecked_neg(T(s*typemax(T))) === T(-s*typemax(T))
    end
    # regular cases
    for s1 in (-1, +1), s2 in (-1,+1)
        @test unchecked_add(T(4s1), T(3s2)) === T(4s1 + 3s2)
        @test unchecked_sub(T(4s1), T(3s2)) === T(4s1 - 3s2)
        @test unchecked_mul(T(4s1), T(3s2)) === T(4s1 * 3s2)
        @test unchecked_div(T(4s1), T(3s2)) === T(div(4s1, 3s2))
        @test unchecked_rem(T(4s1), T(3s2)) === T(rem(4s1, 3s2))
        @test unchecked_fld(T(4s1), T(3s2)) === T(fld(4s1, 3s2))
        @test unchecked_mod(T(4s1), T(3s2)) === T(mod(4s1, 3s2))
    end
    # corner cases
    max2 = T(typemax(T)÷2)
    max21 = T(max2+1)
    min2 = T(typemin(T)÷2)
    min21 = T(min2-1)
    sqrt2 = T(1) << T(sizeof(T)*4)

    @test unchecked_add(typemax(T), T(-1)) === T(typemax(T) - 1)
    @test unchecked_add(typemax(T), T(0)) === typemax(T)
    @test unchecked_add(typemin(T), T(0)) === typemin(T)
    @test unchecked_add(typemin(T), T(1)) === T(typemin(T) + 1)
    @test unchecked_add(T(-1), typemax(T)) === T(typemax(T) - 1)
    @test unchecked_add(T(0), typemax(T)) === typemax(T)
    @test unchecked_add(T(0), typemin(T)) === typemin(T)
    @test unchecked_add(T(1), typemin(T)) === T(typemin(T) + 1)
    @test unchecked_add(typemax(T), typemin(T)) === T(-1)
    @test unchecked_add(typemin(T), typemax(T)) === T(-1)

    @test unchecked_sub(typemax(T), T(0)) === typemax(T)
    @test unchecked_sub(typemax(T), T(1)) === T(typemax(T) - 1)
    @test unchecked_sub(typemin(T), T(-1)) === T(typemin(T) + 1)
    @test unchecked_sub(typemin(T), T(0)) === typemin(T)
    @test unchecked_sub(T(0), typemax(T)) === T(typemin(T) + 1)
    @test unchecked_sub(T(1), typemax(T)) === T(typemin(T) + 2)
    @test unchecked_sub(T(-1), typemin(T)) === typemax(T)
    @test unchecked_sub(typemax(T), typemax(T)) === T(0)
    @test unchecked_sub(typemin(T), typemin(T)) === T(0)
    @test unchecked_sub(max2, T(-min2)) === T(-1)

    @test unchecked_mul(typemax(T), T(0)) === T(0)
    @test unchecked_mul(typemin(T), T(0)) === T(0)
    @test unchecked_mul(typemax(T), T(1)) === typemax(T)
    @test unchecked_mul(typemin(T), T(1)) === typemin(T)

    @test unchecked_div(typemax(T), T(1)) === typemax(T)
    @test unchecked_div(typemax(T), T(-1)) === T(-typemax(T))
    @test unchecked_div(typemin(T), T(1)) === typemin(T)
    @test unchecked_rem(typemax(T), T(1)) === T(0)
    @test unchecked_rem(typemax(T), T(-1)) === T(0)
    @test unchecked_rem(typemin(T), T(1)) === T(0)
    @test unchecked_rem(typemin(T), T(-1)) === T(0)
    @test unchecked_fld(typemax(T), T(1)) === typemax(T)
    @test unchecked_fld(typemax(T), T(-1)) === T(-typemax(T))
    @test unchecked_fld(typemin(T), T(1)) === typemin(T)
    @test unchecked_mod(typemax(T), T(1)) === T(0)
    @test unchecked_mod(typemax(T), T(-1)) === T(0)
    @test unchecked_mod(typemin(T), T(1)) === T(0)
    @test unchecked_mod(typemin(T), T(-1)) === T(0)
end

for T in (UInt8, UInt16, UInt32, UInt64, UInt128)
    # regular cases
    @test unchecked_abs(T(0)) === T(0)
    @test unchecked_neg(T(0)) === T(0)
    @test unchecked_abs(T(3)) === T(3)
    # regular cases
    @test unchecked_add(T(4), T(3)) === T(7)
    @test unchecked_sub(T(4), T(3)) === T(1)
    @test unchecked_mul(T(4), T(3)) === T(12)
    # corner cases
    max2 = T(typemax(T)÷2)
    max21 = T(max2+1)
    sqrt2 = T(1) << T(sizeof(T)*4)

    @test unchecked_add(typemax(T), T(0)) === typemax(T)
    @test unchecked_add(T(0), T(0)) === T(0)
    @test unchecked_add(T(0), T(1)) === T(T(0) + 1)
    @test unchecked_add(T(0), typemax(T)) === typemax(T)
    @test unchecked_add(T(0), T(0)) === T(0)
    @test unchecked_add(T(1), T(0)) === T(T(0) + 1)
    @test unchecked_add(typemax(T), T(0)) === typemax(T)
    @test unchecked_add(T(0), typemax(T)) === typemax(T)

    @test unchecked_sub(typemax(T), T(0)) === typemax(T)
    @test unchecked_sub(typemax(T), T(1)) === T(typemax(T) - 1)
    @test unchecked_sub(T(0), T(0)) === T(0)
    @test unchecked_sub(T(0), T(0)) === T(0)
    @test unchecked_sub(typemax(T), typemax(T)) === T(0)

    @test unchecked_mul(typemax(T), T(0)) === T(0)
    @test unchecked_mul(T(0), T(0)) === T(0)
    @test unchecked_mul(typemax(T), T(1)) === typemax(T)
    @test unchecked_mul(T(0), T(1)) === T(0)

    @test unchecked_div(typemax(T), T(1)) === typemax(T)
    @test unchecked_rem(typemax(T), T(1)) === T(0)
    @test unchecked_fld(typemax(T), T(1)) === typemax(T)
    @test unchecked_mod(typemax(T), T(1)) === T(0)
end
=#
