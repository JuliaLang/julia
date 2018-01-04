# This file is a part of Julia. License is MIT: https://julialang.org/license

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

# Result type must be type of first argument, except for Bool
for U in (Base.BitInteger_types..., BigInt,
          Rational{Int}, Rational{BigInt},
          Float16, Float32, Float64)
    for T in (Base.BitInteger_types..., BigInt,
              Rational{Int}, Rational{BigInt},
              Float16, Float32, Float64)
        @test typeof(copysign(T(3), U(4))) === T
        @test typeof(flipsign(T(3), U(4))) === T
    end
    # Bool promotes to Int
    U <: Unsigned && continue
    for x in [true, false]
        @test flipsign(x, U(4)) === Int(x)
        @test flipsign(x, U(-1)) === -Int(x)
        @test copysign(x, U(4)) === Int(x)
        @test copysign(x, U(-1)) === -Int(x)
    end
end

@testset "flipsign/copysign(typemin($T), -1)" for T in Base.BitInteger_types
    for U in (Base.BitSigned_types..., BigInt, Float16, Float32, Float64)
        @test flipsign(typemin(T), U(-1)) == typemin(T)
        @test copysign(typemin(T), U(-1)) == typemin(T)
    end
end

for s1 in (-1,+1), s2 in (-1,+1)
    @test flipsign(Int16(3s1), Float16(3s2)) === Int16(3s1*s2)
    @test flipsign(Int32(3s1), Float32(3s2)) === Int32(3s1*s2)
    @test flipsign(Int64(3s1), Float64(3s2)) === Int64(3s1*s2)
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

@test count_zeros(10) == Sys.WORD_SIZE - 2
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

primitive type MyBitsType <: Integer 8 end
@test_throws MethodError ~reinterpret(MyBitsType, 0x7b)

UItypes = Base.BitUnsigned_types
SItypes = Base.BitSigned_types

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

    min_val = Int128(typemin(T))
    @test convert(T, min_val) == min_val
    @test_throws InexactError convert(T, min_val-1)
end

for T in (UInt8, UInt16, UInt32, UInt64)
    max_val = Int128(typemax(T))
    @test convert(T, max_val) == max_val
    @test_throws InexactError convert(T, max_val+1)
    @test_throws InexactError convert(T, -1)
end

# Test bit shifts
for T in Base.BitInteger_types
    nbits = 8*sizeof(T)
    issigned = typemin(T) < 0
    highbit = T(2) ^ (nbits-1)
    val1 = 0x1234567890abcdef % T % highbit
    val2 = val1 + highbit
    for val in (val1, val2)
        for count in 0:nbits+1
            ucount, scount = unsigned(count), signed(count)
            # Note: We assume modulo semantics for the arithmetic operations
            # used here
            if count < nbits
                @test val << ucount === val * T(2)^count
                @test val >>> ucount ===
                    fld(unsigned(val), unsigned(T(2))^count) % T
            else
                @test val << ucount === T(0)
                @test val >>> ucount === T(0)
            end
            @test val << scount === val << ucount
            @test val << -scount === val >> ucount
            @test val >>> scount === val >>> ucount
            @test val >>> -scount === val << ucount
            if count < (issigned ? nbits-1 : nbits)
                @test val >> ucount === fld(val, T(2)^count)
            else
                @test val >> ucount === T(val<0 ? -1 : 0)
            end
            @test val >> scount === val >> ucount
            @test val >> -scount === val << ucount
        end
    end
end

@test widen(UInt8(3)) === UInt32(3)
@test widen(UInt16(3)) === UInt32(3)
@test widen(UInt32(3)) === UInt64(3)
@test widen(UInt64(3)) === UInt128(3)
@test widen(UInt128(3)) == 3
@test typeof(widen(UInt128(3))) == BigInt

@test widen(Int8(-3)) === Int32(-3)
@test widen(Int16(-3)) === Int32(-3)
@test widen(Int32(-3)) === Int64(-3)
@test widen(Int64(-3)) === Int128(-3)
@test widen(Int128(-3)) == -3
@test typeof(widen(Int128(-3))) == BigInt

@test widemul(false, false) == false
@test widemul(false, 3) == 0
@test widemul(3, true) == widemul(true, 3) == 3

let i=Int64(2)^63-1, k=widemul(i,i)
    @test widemul(i,i)==85070591730234615847396907784232501249
    j=div(k,2)
    @test div(k,j)==2
    j=div(k,5)
    @test rem(k,j)==4
end

# issue #3596
@test Int128(1)<<0 == 1
@test repr(Int128(1)<<1) == "2"

# issue #16700
@test_throws MethodError 1.0 >> 8

# PR #16988
@test true << 2 === 1 << 2
@test true >> 2 === 1 >> 2
@test true >>> 2 === 1 >>> 2

@test @inferred(unsafe_trunc(Int8, 127)) === Int8(127)
@test unsafe_trunc(Int8, 128) === Int8(-128)
@test unsafe_trunc(Int8, -127) === Int8(-127)
@test unsafe_trunc(Int8, -128) === Int8(-128)
@test unsafe_trunc(Int8, -129) === Int8(127)

# Test x % T returns a T
for T in [Base.BitInteger_types..., BigInt],
    U in [Base.BitInteger_types..., BigInt]
    @test typeof(rand(U(0):U(127)) % T) === T
end

# issue #15489
@test 0x00007ffea27edaa0 + (-40) === (-40) + 0x00007ffea27edaa0 === 0x00007ffea27eda78
@test UInt64(1) * Int64(-1) === typemax(UInt64)
@test UInt(1) - (-1) == 2
@test UInt64(15) & -4 === UInt64(12)
@test UInt64(15) | -4 === typemax(UInt64)
@test UInt64(15) ⊻ -4 === 0xfffffffffffffff3


@testset "left shift with Vector{Int} on BigInt-scalar #13832" begin
    x = BigInt(1) .<< [1:70;]
    @test x[end] == 1180591620717411303424
    @test eltype(x) == BigInt
end

# issue #9292
@testset "mixed signedness arithmetic" begin
    for T in Base.BitInteger_types
        for S in Base.BitInteger_types
            a, b = one(T), one(S)
            for c in (a+b, a-b, a*b)
                if T === S
                    @test c isa T
                elseif sizeof(T) > sizeof(S)
                    # larger type wins
                    @test c isa T
                elseif sizeof(S) > sizeof(T)
                    @test c isa S
                else
                    # otherwise Unsigned wins
                    @test c isa (T <: Unsigned ? T : S)
                end
            end
        end
    end
end

# issue #21092
@test big"1_0_0_0" == BigInt(1000)
@test_throws ArgumentError big"1_0_0_0_"
@test_throws ArgumentError big"_1_0_0_0"
