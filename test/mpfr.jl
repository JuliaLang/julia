# constructors
with_bigfloat_precision(53) do
    x = BigFloat()
    x = BigFloat(12)
end
x = BigFloat(12)
y = BigFloat(x)
@test_approx_eq x y
y = BigFloat(0xc)
@test_approx_eq x y
y = BigFloat(12.)
@test_approx_eq x y
y = BigFloat(BigInt(12))
@test_approx_eq x y
y = BigFloat(BigFloat(12))
@test_approx_eq x y
y = BigFloat("12")
@test_approx_eq x y
y = BigFloat(float32(12.))
@test_approx_eq x y
y = BigFloat(12//1)
@test_approx_eq x y

# +
x = BigFloat(12)
y = BigFloat(30)
@test x + y == BigFloat(42)

# -
x = BigFloat(12)
y = BigFloat(-30)
@test x - y == BigFloat(42)

# *
x = BigFloat(6)
y = BigFloat(9)
@test x * y != BigFloat(42)
@test x * y == BigFloat(54)

# /
x = BigFloat(9)
y = BigFloat(6)
@test x / y == BigFloat(9/6)

# iterated arithmetic
a = BigFloat(12.25)
b = BigFloat(23.125)
c = BigFloat(-7)
d = BigFloat(-12.75)
f = BigFloat(2.0625)
g = BigFloat(0.03125)
@test +(a, b) == BigFloat(35.375)
@test +(a, b, c) == BigFloat(28.375)
@test +(a, b, c, d) == BigFloat(15.625)
@test +(a, b, c, d, f) == BigFloat(17.6875)
@test +(a, b, c, d, f, g) == BigFloat(17.71875)

@test *(a, b) == BigFloat("2.8328125e+02")
@test *(a, b, c) == BigFloat("-1.98296875e+03")
@test *(a, b, c, d) == BigFloat("2.52828515625e+04")
@test *(a, b, c, d, f) == BigFloat("5.214588134765625e+04")
@test *(a, b, c, d, f, g) == BigFloat("1.6295587921142578125e+03")

# < / > / <= / >=
x = BigFloat(12)
y = BigFloat(42)
z = BigFloat(30)
@test y > x
@test y >= x
@test y > z
@test y >= z
@test x < y
@test x <= y
@test z < y
@test z <= y
@test y - x >= z
@test y - x <= z
@test !(x >= z)
@test !(y <= z)

# rounding modes
with_bigfloat_precision(4) do
    # default mode is round to nearest
    down, up =  with_bigfloat_rounding(MPFR.RoundToNearest) do
        BigFloat("0.0938"), BigFloat("0.102")
    end
    with_bigfloat_rounding(MPFR.RoundDown) do
        @test BigFloat(0.1) == down
        @test BigFloat(0.1) != up
    end
    with_bigfloat_rounding(MPFR.RoundUp) do
        @test BigFloat(0.1) != down
        @test BigFloat(0.1) == up
    end
end

# ^
x = BigFloat(12)
y = BigFloat(4)
@test x^y == BigFloat(20736)

# ceil
x = BigFloat(12.042)
@test BigFloat(13) == ceil(x)

# copysign
x = BigFloat(1)
y = BigFloat(-1)
@test copysign(x, y) == y
@test copysign(y, x) == x

# isfinite / isinf
x = BigFloat(Inf)
y = BigFloat(1)
@test isinf(x) == true
@test isinf(y) == false
@test isfinite(x) == false
@test isinf(x) == true

# isnan
x = BigFloat(NaN)
y = BigFloat(1)
@test isnan(x) == true
@test isnan(y) == false

# convert to
@test convert(BigFloat, 1//2) == BigFloat("0.5")
@test typeof(convert(BigFloat, 1//2)) == BigFloat
@test convert(BigFloat, 0.5) == BigFloat("0.5")
@test typeof(convert(BigFloat, 0.5)) == BigFloat
@test convert(BigFloat, 40) == BigFloat("40")
@test typeof(convert(BigFloat, 40)) == BigFloat
@test convert(BigFloat, float32(0.5)) == BigFloat("0.5")
@test typeof(convert(BigFloat, float32(0.5))) == BigFloat
@test convert(BigFloat, BigInt("9223372036854775808")) == BigFloat("9223372036854775808")
@test typeof(convert(BigFloat, BigInt("9223372036854775808"))) == BigFloat
@test convert(FloatingPoint, BigInt("9223372036854775808")) == BigFloat("9223372036854775808")
@test typeof(convert(FloatingPoint, BigInt("9223372036854775808"))) == BigFloat

# convert from
@test convert(Float64, BigFloat(0.5)) == 0.5
@test convert(Float32, BigFloat(0.5)) == float32(0.5)

# exponent
x = BigFloat(0)
@test_throws exponent(x)
x = BigFloat(Inf)
@test_throws exponent(x)
x = BigFloat(15.674)
@test exponent(x) == exponent(15.674)

# nextfloat/prevfloat should be immutable
x = 12.
y = BigFloat(x)
@test x == y
nextfloat(y)
@test x == y
prevfloat(y)
@test x == y

# sqrt DomainError
@test_throws sqrt(BigFloat(-1))

# precision
old_precision = get_bigfloat_precision()
x = BigFloat(0)
@test get_precision(x) == old_precision
set_bigfloat_precision(256)
x = BigFloat(0)
@test get_precision(x) == 256
set_bigfloat_precision(old_precision)
z = with_bigfloat_precision(240) do
    z = x + 20
    return z
end
@test float(z) == 20.
@test get_precision(z) == 240
x = BigFloat(12)
@test get_precision(x) == old_precision
@test_throws set_bigfloat_precision(1)

# isinteger
@test isinteger(BigFloat(12))
@test !isinteger(BigFloat(12.12))

# nextfloat / prevfloat
with_bigfloat_precision(53) do
    x = BigFloat(12.12)
    @test BigFloat(nextfloat(12.12)) == nextfloat(x)
    @test BigFloat(prevfloat(12.12)) == prevfloat(x)
end
@test isnan(nextfloat(BigFloat(NaN)))
@test isnan(prevfloat(BigFloat(NaN)))

# comparisons
x = BigFloat(1)
y = BigFloat(-1)
z = BigFloat(NaN)
ipl = BigFloat(Inf)
imi = BigFloat(-Inf)
@test x > y
@test x >= y
@test x >= x
@test y < x
@test y <= x
@test y <= y
@test x < ipl
@test x <= ipl
@test x > imi
@test x >= imi
@test imi == imi
@test ipl == ipl
@test imi < ipl
@test z != z
@test !(z == z)
@test !(z <= z)
@test !(z < z)
@test !(z >= z)
@test !(z > z)

@test !isequal(BigFloat(0.0),BigFloat(-0.0))
@test isequal(z, BigFloat(NaN))

# signbit
@test signbit(BigFloat(-1.0)) == 1
@test signbit(BigFloat(1.0)) == 0
@test signbit(BigFloat(-0.0)) == 1

# modf
x = BigFloat(12)
y = BigFloat(0.5)
@test modf(x+y) == (y, x)
x = BigFloat(NaN)
@test map(isnan, modf(x)) == (true, true)
x = BigFloat(Inf)
y = modf(x)
@test (isnan(y[1]), isinf(y[2])) == (true, true)

# rem
with_bigfloat_precision(53) do
    x = BigFloat(2)
    y = BigFloat(1.67)
    @test rem(x,y) == rem(2, 1.67)
    y = BigFloat(NaN)
    @test isnan(rem(x,y))
    @test isnan(rem(y,x))
    y = BigFloat(Inf)
    @test rem(x,y) == x
    @test isnan(rem(y,x))
end

# min/max
x = BigFloat(4)
y = BigFloat(2)
@test max(x,y) == x
@test min(x,y) == y
y = BigFloat(NaN)
@test max(x,y) == x
@test min(x,y) == x
@test isnan(max(y,y))
@test isnan(min(y,y))

# sum
x = BigFloat(1)
y = BigFloat(2)
z = BigFloat(3)
w = BigFloat(4)
@test sum([x,y,z,w]) == BigFloat(10)
big_array = ones(BigFloat, 100)
@test sum(big_array) == BigFloat(100)
@test sum(BigFloat[]) == BigFloat(0)

# promotion
# the array converts everyone to the DEFAULT_PRECISION!
x = BigFloat(12)
y = with_bigfloat_precision(60) do
    BigFloat(42)
end
@test [x,y] == [BigFloat(12), BigFloat(42)]

# log / log2 / log10
with_bigfloat_precision(53) do
x = BigFloat(42)
    @test log(x) == log(42)
    @test isinf(log(BigFloat(0)))
    @test_throws log(BigFloat(-1))
    @test log2(x) == log2(42)
    @test isinf(log2(BigFloat(0)))
    @test_throws log2(BigFloat(-1))
    @test log10(x) == log10(42)
    @test isinf(log10(BigFloat(0)))
    @test_throws log10(BigFloat(-1))
end

# exp / exp2 / exp10
with_bigfloat_precision(53) do
    x = BigFloat(10)
    @test exp(x) == exp(10)
    @test exp2(x) == 1024
    @test exp10(x) == 10000000000
end

# convert to integer types
x = BigFloat(12.1)
y = BigFloat(42)
@test_throws convert(Int32, x)
@test_throws convert(Int64, x)
@test_throws convert(BigInt, x)
@test_throws convert(Uint32, x)
@test_throws convert(Uint32, x)
@test convert(Int32, y) == 42
@test convert(Int64, y) == 42
@test convert(BigInt, y) == 42
@test convert(Uint32, y) == 42
@test convert(Uint32, y) == 42

# iround
x = BigFloat(42.42)
y = with_bigfloat_precision(256) do
    BigFloat("9223372036854775809.2324")
end
z = BigInt("9223372036854775809")
@test iround(x) == 42
@test iround(y) == z
@test typeof(iround(Uint8, x)) == Uint8 && iround(Uint8, x) == 0x2a
@test typeof(iround(Uint16, x)) == Uint16 && iround(Uint16, x) == 0x2a
@test typeof(iround(Uint32, x)) == Uint32 && iround(Uint32, x) == 0x2a
@test typeof(iround(Uint64, x)) == Uint64 && iround(Uint64, x) == 0x2a
@test typeof(iround(Int64, x)) == Int64 && iround(Int64, x) == 42
@test typeof(iround(Int, x)) == Int && iround(Int, x) == 42
@test typeof(iround(Uint, x)) == Uint && iround(Uint, x) == 0x2a

# string representation
str = "1.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000012e+00"
with_bigfloat_precision(406) do
    @test string(nextfloat(BigFloat(1))) == str
end

# eps
x = eps(BigFloat)
@test BigFloat(1) + x == BigFloat(1) + prevfloat(x)

# factorial
with_bigfloat_precision(256) do
    x = BigFloat(42)
    @test factorial(x) == factorial(BigInt(42))
    x = BigFloat(10)
    @test factorial(x) == factorial(10)
    @test_throws factorial(BigFloat(-1))
    @test_throws factorial(BigFloat(331.3))
end

# bessel functions
with_bigfloat_precision(53) do
    @test_approx_eq besselj(4, BigFloat(2)) besselj(4, 2.)
    @test_approx_eq besselj0(BigFloat(2))  besselj0(2.)
    @test_approx_eq besselj1(BigFloat(2))  besselj1(2.)
    @test_approx_eq bessely(4, BigFloat(2))  bessely(4, 2.)
    @test_approx_eq bessely0(BigFloat(2))  bessely0(2.)
    @test_approx_eq bessely1(BigFloat(2))  bessely1(2.)
end

# trigonometric functions
with_bigfloat_precision(53) do
    for f in (:sin,:cos,:tan,:sec,:csc,:cot,:acos,:asin,:atan,
            :cosh,:sinh,:tanh,:sech,:csch,:coth,:asinh),
        j in (-1., -0.5, -0.25, .25, .5, 1.)
        @eval begin
            @test_approx_eq ($f)(BigFloat($j)) ($f)($j)
        end
    end
    for f in (:acos,:asin,:acosh,:atanh),
        j in (-2, -1.5)
        @eval begin
            @test_throws ($f)(BigFloat($j))
        end
    end
    for f in (:sin,:cos,:tan,:sec,:csc,:cot,:cosh,:sinh,:tanh,
            :sech,:csch,:coth,:acosh,:asinh),
        j in (1., 1.5, 1.9)
        @eval begin
            @test_approx_eq ($f)(BigFloat($j)) ($f)($j)
        end
    end
    for j in (.25, .5)
        @test_approx_eq atanh(BigFloat(j)) atanh(j)
    end
end

# hypot
@test hypot(BigFloat(3), BigFloat(4)) == 5

# atan2
with_bigfloat_precision(53) do
    @test isequal(atan2(12,2), atan2(BigFloat(12), BigFloat(2)))
end

# ldexp
with_bigfloat_precision(53) do
    @test ldexp(BigFloat(24.5), 72) == ldexp(24.5, 72)
    @test ldexp(BigFloat(24.5), int16(72)) == ldexp(24.5, 72)
    @test ldexp(BigFloat(24.5), -72) == ldexp(24.5, -72)
    @test ldexp(BigFloat(24.5), int16(-72)) == ldexp(24.5, -72)
    @test ldexp(BigFloat(24.5), uint(72)) == ldexp(24.5, 72)
    @test ldexp(BigFloat(24.5), 0x48) == ldexp(24.5, 72)
end

# ceil / iceil / floor / ifloor / trunc / itrunc
x = BigFloat("28273.2312487489135135135")
y = BigInt(28273)
z = BigInt(28274)
a = BigFloat("123456789012345678901234567890.2414")
b = BigInt("123456789012345678901234567890")
c = BigInt("123456789012345678901234567891")
@test ceil(x) == z
@test typeof(ceil(x)) == BigFloat
@test floor(x) == y
@test typeof(floor(x)) == BigFloat
@test trunc(x) == y
@test typeof(trunc(x)) == BigFloat

@test iceil(x) == z
@test typeof(iceil(x)) == BigInt
@test ifloor(x) == y
@test typeof(ifloor(x)) == BigInt
@test itrunc(x) == y
@test typeof(itrunc(x)) == BigInt

@test iceil(Int64, x) == int64(z)
@test typeof(iceil(Int64, x)) == Int64
@test ifloor(Int64, x) == int64(y)
@test typeof(ifloor(Int64, x)) == Int64
@test itrunc(Int64, x) == int64(y)
@test typeof(itrunc(Int64, x)) == Int64

@test iceil(Int32, x) == int32(z)
@test typeof(iceil(Int32, x)) == Int32
@test ifloor(Int32, x) == int32(y)
@test typeof(ifloor(Int32, x)) == Int32
@test itrunc(Int32, x) == int32(y)
@test typeof(itrunc(Int32, x)) == Int32

@test iceil(Int16, x) == int16(z)
@test typeof(iceil(Int16, x)) == Int16
@test ifloor(Int16, x) == int16(y)
@test typeof(ifloor(Int16, x)) == Int16
@test itrunc(Int16, x) == int16(y)
@test typeof(itrunc(Int16, x)) == Int16

#@test iceil(Int8, x) == int8(z)
#@test typeof(iceil(Int8, x)) == Int8
#@test ifloor(Int8, x) == int8(y)
#@test typeof(ifloor(Int8, x)) == Int8
#@test itrunc(Int8, x) == int8(y)
#@test typeof(itrunc(Int8, x)) == Int8

@test iceil(Uint64, x) == uint64(z)
@test typeof(iceil(Uint64, x)) == Uint64
@test ifloor(Uint64, x) == uint64(y)
@test typeof(ifloor(Uint64, x)) == Uint64
@test itrunc(Uint64, x) == uint64(y)
@test typeof(itrunc(Uint64, x)) == Uint64

@test iceil(Uint32, x) == uint32(z)
@test typeof(iceil(Uint32, x)) == Uint32
@test ifloor(Uint32, x) == uint32(y)
@test typeof(ifloor(Uint32, x)) == Uint32
@test itrunc(Uint32, x) == uint32(y)
@test typeof(itrunc(Uint32, x)) == Uint32

@test iceil(Uint16, x) == uint16(z)
@test typeof(iceil(Uint16, x)) == Uint16
@test ifloor(Uint16, x) == uint16(y)
@test typeof(ifloor(Uint16, x)) == Uint16
@test itrunc(Uint16, x) == uint16(y)
@test typeof(itrunc(Uint16, x)) == Uint16

#@test iceil(Uint8, x) == uint8(z)
#@test typeof(iceil(Uint8, x)) == Uint8
#@test ifloor(Uint8, x) == uint8(y)
#@test typeof(ifloor(Uint8, x)) == Uint8
#@test itrunc(Uint8, x) == uint8(y)
#@test typeof(itrunc(Uint8, x)) == Uint8

@test iceil(a) == c
@test typeof(iceil(a)) == BigInt
@test ifloor(a) == b
@test typeof(ifloor(a)) == BigInt
@test itrunc(a) == b
@test typeof(itrunc(a)) == BigInt

# basic arithmetic
# Signed addition
a = BigFloat("123456789012345678901234567890")
b = BigFloat("123456789012345678901234567891")
@test a+int8(1) == b
@test a+int16(1) == b
@test a+int32(1) == b
@test a+int64(1) == b
@test int8(1)+ a == b
@test int16(1)+a == b
@test int32(1)+a == b
@test int64(1)+a == b
@test b+int8(-1) == a
@test b+int16(-1) == a
@test b+int32(-1) == a
@test b+int64(-1) == a
@test int8(-1)+ b == a
@test int16(-1)+b == a
@test int32(-1)+b == a
@test int64(-1)+b == a

# Unsigned addition
@test a+true == b
@test a+uint8(1) == b
@test a+uint16(1) == b
@test a+uint32(1) == b
@test a+uint64(1) == b
@test true+a == b
@test uint8(1)+ a == b
@test uint16(1)+a == b
@test uint32(1)+a == b
@test uint64(1)+a == b

# Float64 addition
@test a + 1.0f0 == b
@test 1.0f0 + a == b
@test a + 1.0 == b
@test 1.0 + a == b

# BigInt addition
@test a + BigInt(1) == b
@test BigInt(1) + a == b

# Signed subtraction
@test b-int8(1) == a
@test b-int16(1) == a
@test b-int32(1) == a
@test b-int64(1) == a
@test int8(1)- b == -a
@test int16(1)-b == -a
@test int32(1)-b == -a
@test int64(1)-b == -a
@test a-int8(-1) == b
@test a-int16(-1) == b
@test a-int32(-1) == b
@test a-int64(-1) == b
@test int8(-1)- a == -b
@test int16(-1)-a == -b
@test int32(-1)-a == -b
@test int64(-1)-a == -b

# Unsigned subtraction
@test b-true == a
@test b-uint8(1) == a
@test b-uint16(1) == a
@test b-uint32(1) == a
@test b-uint64(1) == a
@test true-b == -a
@test uint8(1)- b == -a
@test uint16(1)-b == -a
@test uint32(1)-b == -a
@test uint64(1)-b == -a

# Float64 subtraction
@test b - 1.0f0 == a
@test 1.0f0 - b == -a
@test b - 1.0 == a
@test 1.0 - b == -a

# BigInt subtraction
@test b - BigInt(1) == a
@test BigInt(1) - b == -a

# Signed multiplication
@test a*int8(1) == a
@test a*int16(1) == a
@test a*int32(1) == a
@test a*int64(1) == a
@test int8(1)* a == a
@test int16(1)*a == a
@test int32(1)*a == a
@test int64(1)*a == a
@test a*int8(-1) == -a
@test a*int16(-1) == -a
@test a*int32(-1) == -a
@test a*int64(-1) == -a
@test int8(-1)* a == -a
@test int16(-1)*a == -a
@test int32(-1)*a == -a
@test int64(-1)*a == -a

# Unsigned multiplication
@test a*true == a
@test a*uint8(1) == a
@test a*uint16(1) == a
@test a*uint32(1) == a
@test a*uint64(1) == a
@test true*a == a
@test uint8(1)* a == a
@test uint16(1)*a == a
@test uint32(1)*a == a
@test uint64(1)*a == a

# Float64 multiplication
@test a * 1.0f0 == a
@test 1.0f0 * a == a
@test a * 1.0 == a
@test 1.0 * a == a

# BigInt multiplication
@test a * BigInt(1) == a
@test BigInt(1) * a == a

# Signed division
c = BigInt("61728394506172839450617283945")
# d = 2^200
d = BigFloat("1606938044258990275541962092341162602522202993782792835301376")
f = BigFloat("6.223015277861141707144064053780124240590252168721167133101116614789698834035383e-61")

@test a/int8(2) == c
@test a/int16(2) == c
@test a/int32(2) == c
@test a/int64(2) == c
@test int8(1)/ d == f
@test int16(1)/d == f
@test int32(1)/d == f
@test int64(1)/d == f
@test a/int8(-2) == -c
@test a/int16(-2) == -c
@test a/int32(-2) == -c
@test a/int64(-2) == -c
@test int8(-1)/ d == -f
@test int16(-1)/d == -f
@test int32(-1)/d == -f
@test int64(-1)/d == -f

# Unsigned division
@test a/true == a
@test a/uint8(2) == c
@test a/uint16(2) == c
@test a/uint32(2) == c
@test a/uint64(2) == c
@test true/d == f
@test uint8(1)/ d == f
@test uint16(1)/d == f
@test uint32(1)/d == f
@test uint64(1)/d == f

# Float64 division
@test a / 2.0f0 == c
@test 1.0f0 / d == f
@test a / 2.0 == c
@test 1.0 / d == f

# BigInt division
@test a / BigInt(2) == c

# old tests
tol = 1e-12

a = BigFloat("12.34567890121")
b = BigFloat("12.34567890122")

@test_approx_eq_eps a+1e-11 b tol
@test !(b == a)
@test b > a
@test b >= a
@test !(b < a)
@test !(b <= a)

c = BigFloat("24.69135780242")
@test typeof(a * 2) == BigFloat
@test_approx_eq_eps a*2 c tol
@test_approx_eq_eps (c-a) a tol


d = BigFloat("-24.69135780242")
@test typeof(d) == BigFloat
@test_approx_eq_eps d+c 0 tol

@test_approx_eq_eps (BigFloat(3)/BigFloat(2)) BigFloat(1.5) tol

@test typeof(BigFloat(typemax(Int8))) == BigFloat
@test typeof(BigFloat(typemax(Int16))) == BigFloat
@test typeof(BigFloat(typemax(Int32))) == BigFloat
@test typeof(BigFloat(typemax(Int64))) == BigFloat
@test typeof(BigFloat(typemax(Int128))) == BigFloat

@test typeof(BigFloat(true)) == BigFloat
@test typeof(BigFloat(typemax(Uint8))) == BigFloat
@test typeof(BigFloat(typemax(Uint16))) == BigFloat
@test typeof(BigFloat(typemax(Uint32))) == BigFloat
@test typeof(BigFloat(typemax(Uint64))) == BigFloat
@test typeof(BigFloat(typemax(Uint128))) == BigFloat

@test typeof(BigFloat(realmax(Float32))) == BigFloat
@test typeof(BigFloat(realmax(Float64))) == BigFloat

@test typeof(BigFloat(BigInt(1))) == BigFloat
@test typeof(BigFloat(BigFloat(1))) == BigFloat

@test typeof(BigFloat(1//1)) == BigFloat
@test typeof(BigFloat(one(Rational{BigInt}))) == BigFloat

f = BigFloat("1234567890.123")
g = BigFloat("1234567891.123")

tol = 1e-3

@test_approx_eq_eps f+int8(1) g tol
@test_approx_eq_eps f+int16(1) g tol
@test_approx_eq_eps f+int32(1) g tol
@test_approx_eq_eps f+int64(1) g tol
@test_approx_eq_eps f+int128(1) g tol

@test_approx_eq_eps f+true g tol
@test_approx_eq_eps f+uint8(1) g tol
@test_approx_eq_eps f+uint16(1) g tol
@test_approx_eq_eps f+uint32(1) g tol
@test_approx_eq_eps f+uint64(1) g tol
@test_approx_eq_eps f+uint128(1) g tol

@test_approx_eq_eps f+BigInt(1) g tol

@test_approx_eq_eps f+1f0 g tol
@test_approx_eq_eps f+1e0 g tol

@test_approx_eq_eps f+BigFloat(1) g tol

@test_approx_eq_eps f+(1//1) g tol

@test_approx_eq_eps f+one(Rational{BigInt}) g tol

# new tests
