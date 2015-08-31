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

for T in (Int8, Int16, Int32, Int64, Int128)
    @test promote(Int8(3), T(3)) === (T(3), T(3))
end

for T in (Int16, Int32, Int64, Int128)
    @test promote(Int16(3), T(3)) === (T(3), T(3))
end

for T in (Int32, Int64, Int128)
    @test promote(Int32(3), T(3)) === (T(3), T(3))
end

for T in (Int64, Int128)
    @test promote(Int64(3), T(3)) === (T(3), T(3))
end

@test promote(Int128(3), Int128(3)) === (Int128(3), Int128(3))


for T in (UInt8, UInt16, UInt32, UInt64, UInt128)
    @test promote(UInt8(3), T(3)) === (T(3), T(3))
end

for T in (UInt16, UInt32, UInt64, UInt128)
    @test promote(UInt16(3), T(3)) === (T(3), T(3))
end

for T in (UInt32, UInt64, UInt128)
    @test promote(UInt32(3), T(3)) === (T(3), T(3))
end

for T in (UInt64, UInt128)
    @test promote(UInt64(3), T(3)) === (T(3), T(3))
end

for T in (UInt8, UInt16, UInt32)
    @test promote(T(3), Int64(3)) === (Int64(3), Int64(3))
    @test promote(T(3), Int128(3)) === (Int128(3), Int128(3))
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

@test widen(Int16(3)) == Int(3)
@test widen(Int32(3)) == Int64(3)
@test widen(UInt8(3)) == UInt(3)
@test widen(UInt16(3)) == UInt(3)
@test widen(UInt32(3)) == UInt64(3)

@test widemul(false, false) == false
@test widemul(false, 3) == 0
@test widemul(3, true) == widemul(true, 3) == 3


# checked operations

import Base: checked_add, checked_sub, checked_mul
@test checked_sub(UInt(4), UInt(3)) === UInt(1)
@test_throws OverflowError checked_sub(UInt(5), UInt(6))
@test checked_mul(UInt(4), UInt(3)) === UInt(12)

if WORD_SIZE == 32
    @test_throws OverflowError checked_mul(UInt(2)^30, UInt(2)^2)
else
    @test_throws OverflowError checked_mul(UInt(2)^62, UInt(2)^2)
end

# Checked operations on UInt128 are currently broken
# FIXME: #4905

@test checked_add(UInt128(1), UInt128(2)) === UInt128(3)
#@test_throws OverflowError checked_add(UInt128(2)^127, UInt128(2)^127)
@test checked_add(UInt128(2)^127, UInt128(2)^127) === UInt128(0)  # broken

@test checked_sub(UInt128(2), UInt128(1)) === UInt128(1)
#@test_throws OverflowError checked_sub(UInt128(3), UInt128(4))
@test checked_sub(UInt128(3), UInt128(4)) === UInt128(0) - 1  # broken

@test checked_mul(UInt128(3), UInt128(4)) === UInt128(12)
#@test_throws OverflowError checked_mul(UInt128(2)^127, UInt128(2))
@test checked_mul(UInt128(2)^127, UInt128(2)) === UInt128(0)
# broken












