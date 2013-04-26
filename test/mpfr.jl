# constructors
with_bigfloat_precision(53) do
    x = MPFRFloat()
    x = MPFRFloat(12)
end
x = MPFRFloat(12)
y = MPFRFloat(x)
@test_approx_eq x y
y = MPFRFloat(0xc)
@test_approx_eq x y
y = MPFRFloat(12.)
@test_approx_eq x y
y = MPFRFloat(BigInt(12))
@test_approx_eq x y
y = MPFRFloat(BigFloat(12))
@test_approx_eq x y
y = MPFRFloat("12")
@test_approx_eq x y
y = MPFRFloat(float32(12.))
@test_approx_eq x y
y = MPFRFloat(12//1)
@test_approx_eq x y

# +
x = MPFRFloat(12)
y = MPFRFloat(30)
@test x + y == MPFRFloat(42)

# -
x = MPFRFloat(12)
y = MPFRFloat(-30)
@test x - y == MPFRFloat(42)

# *
x = MPFRFloat(6)
y = MPFRFloat(9)
@test x * y != MPFRFloat(42)
@test x * y == MPFRFloat(54)

# /
x = MPFRFloat(9)
y = MPFRFloat(6)
@test x / y == MPFRFloat(9/6)

# < / > / <= / >=
x = MPFRFloat(12)
y = MPFRFloat(42)
z = MPFRFloat(30)
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

# ^
x = MPFRFloat(12)
y = MPFRFloat(4)
@test x^y == MPFRFloat(20736)

# ceil
x = MPFRFloat(12.042)
@test MPFRFloat(13) == ceil(x)

# copysign
x = MPFRFloat(1)
y = MPFRFloat(-1)
@test copysign(x, y) == y
@test copysign(y, x) == x

# isfinite / isinf
x = MPFRFloat(Inf)
y = MPFRFloat(1)
@test isinf(x) == true
@test isinf(y) == false
@test isfinite(x) == false
@test isinf(x) == true

# isnan
x = MPFRFloat(NaN)
y = MPFRFloat(1)
@test isnan(x) == true
@test isnan(y) == false

# convert to
@test convert(MPFRFloat, 1//2) == MPFRFloat("0.5")
@test convert(MPFRFloat, 0.5) == MPFRFloat("0.5")
@test convert(MPFRFloat, 40) == MPFRFloat("40")
@test convert(MPFRFloat, float32(0.5)) == MPFRFloat("0.5")

# convert from
@test convert(Float64, MPFRFloat(0.5)) == 0.5
@test convert(Float32, MPFRFloat(0.5)) == float32(0.5)

# exponent
x = MPFRFloat(0)
@test_fails exponent(x)
x = MPFRFloat(Inf)
@test_fails exponent(x)
x = MPFRFloat(15.674)
@test exponent(x) == exponent(15.674)

# nextfloat/prevfloat should be immutable
x = 12.
y = MPFRFloat(x)
@test x == y
nextfloat(y)
@test x == y
prevfloat(y)
@test x == y

# sqrt DomainError
@test_fails sqrt(MPFRFloat(-1))

# precision
old_precision = get_bigfloat_precision()
x = MPFRFloat(0)
@test get_precision(x) == old_precision
set_bigfloat_precision(256)
x = MPFRFloat(0)
@test get_precision(x) == 256
set_bigfloat_precision(old_precision)
z = with_bigfloat_precision(240) do
    z = x + 20
    return z
end
@test float(z) == 20.
@test get_precision(z) == 240
x = MPFRFloat(12)
@test get_precision(x) == old_precision
@test_fails set_bigfloat_precision(1)

# integer_valued
@test integer_valued(MPFRFloat(12))
@test !integer_valued(MPFRFloat(12.12))

# nextfloat / prevfloat
with_bigfloat_precision(53) do
    x = MPFRFloat(12.12)
    @test MPFRFloat(nextfloat(12.12)) == nextfloat(x)
    @test MPFRFloat(prevfloat(12.12)) == prevfloat(x)
end
@test isnan(nextfloat(MPFRFloat(NaN)))
@test isnan(prevfloat(MPFRFloat(NaN)))

# comparisons
x = MPFRFloat(1)
y = MPFRFloat(-1)
z = MPFRFloat(NaN)
ipl = MPFRFloat(Inf)
imi = MPFRFloat(-Inf)
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

# modf
x = MPFRFloat(12)
y = MPFRFloat(0.5)
@test modf(x+y) == (y, x)
x = MPFRFloat(NaN)
@test map(isnan, modf(x)) == (true, true)
x = MPFRFloat(Inf)
y = modf(x)
@test (isnan(y[1]), isinf(y[2])) == (true, true)

# rem
with_bigfloat_precision(53) do
    x = MPFRFloat(2)
    y = MPFRFloat(1.67)
    @test rem(x,y) == rem(2, 1.67)
    y = MPFRFloat(NaN)
    @test isnan(rem(x,y))
    @test isnan(rem(y,x))
    y = MPFRFloat(Inf)
    @test rem(x,y) == x
    @test isnan(rem(y,x))
end

# min/max
x = MPFRFloat(4)
y = MPFRFloat(2)
@test max(x,y) == x
@test min(x,y) == y
y = MPFRFloat(NaN)
@test max(x,y) == x
@test min(x,y) == x
@test isnan(max(y,y))
@test isnan(min(y,y))

# sum
x = MPFRFloat(1)
y = MPFRFloat(2)
z = MPFRFloat(3)
w = MPFRFloat(4)
@test sum([x,y,z,w]) == MPFRFloat(10)
big_array = ones(MPFRFloat, 100)
@test sum(big_array) == MPFRFloat(100)

# promotion
# the array converts everyone to the DEFAULT_PRECISION!
x = MPFRFloat(12)
y = with_bigfloat_precision(60) do
    MPFRFloat(42)
end
@test [x,y] == [MPFRFloat(12), MPFRFloat(42)]

# log / log2 / log10
with_bigfloat_precision(53) do
x = MPFRFloat(42)
    @test log(x) == log(42)
    @test isinf(log(MPFRFloat(0)))
    @test_fails log(MPFRFloat(-1))
    @test log2(x) == log2(42)
    @test isinf(log2(MPFRFloat(0)))
    @test_fails log2(MPFRFloat(-1))
    @test log10(x) == log10(42)
    @test isinf(log10(MPFRFloat(0)))
    @test_fails log10(MPFRFloat(-1))
end

# exp / exp2 / exp10
with_bigfloat_precision(53) do
    x = MPFRFloat(10)
    @test exp(x) == exp(10)
    @test exp2(x) == 1024
    @test exp10(x) == 10000000000
end

# convert to integer types
x = MPFRFloat(12.1)
y = MPFRFloat(42)
@test_fails convert(Int32, x)
@test_fails convert(Int64, x)
@test_fails convert(BigInt, x)
@test_fails convert(Uint32, x)
@test_fails convert(Uint32, x)
@test convert(Int32, y) == 42
@test convert(Int64, y) == 42
@test convert(BigInt, y) == 42
@test convert(Uint32, y) == 42
@test convert(Uint32, y) == 42

# iround
x = MPFRFloat(42.42)
y = with_bigfloat_precision(256) do
    MPFRFloat("9223372036854775809.2324")
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

# factorial
with_bigfloat_precision(256) do
    x = MPFRFloat(42)
    @test factorial(x) == factorial(BigInt(42))
    x = MPFRFloat(10)
    @test factorial(x) == factorial(10)
    @test_fails factorial(MPFRFloat(-1))
    @test_fails factorial(MPFRFloat(331.3))
end

# bessel functions
with_bigfloat_precision(53) do
    @test_approx_eq besselj(4, MPFRFloat(2)) besselj(4, 2.)
    @test_approx_eq besselj0(MPFRFloat(2))  besselj0(2.)
    @test_approx_eq besselj1(MPFRFloat(2))  besselj1(2.)
    @test_approx_eq bessely(4, MPFRFloat(2))  bessely(4, 2.)
    @test_approx_eq bessely0(MPFRFloat(2))  bessely0(2.)
    @test_approx_eq bessely1(MPFRFloat(2))  bessely1(2.)
end

# trigonometric functions
with_bigfloat_precision(53) do
    for f in (:sin,:cos,:tan,:sec,:csc,:cot,:acos,:asin,:atan,
            :cosh,:sinh,:tanh,:sech,:csch,:coth,:asinh),
        j in (-1., -0.5, -0.25, .25, .5, 1.)
        @eval begin
            @test_approx_eq ($f)(MPFRFloat($j)) ($f)($j)
        end
    end
    for f in (:acos,:asin,:acosh,:atanh),
        j in (-2, -1.5)
        @eval begin
            @test_fails ($f)(MPFRFloat($j))
        end
    end
    for f in (:sin,:cos,:tan,:sec,:csc,:cot,:cosh,:sinh,:tanh,
            :sech,:csch,:coth,:acosh,:asinh),
        j in (1., 1.5, 1.9)
        @eval begin
            @test_approx_eq ($f)(MPFRFloat($j)) ($f)($j)
        end
    end
    for j in (.25, .5)
        @test_approx_eq atanh(MPFRFloat(j)) atanh(j)
    end
end

# hypot
@test hypot(MPFRFloat(3), MPFRFloat(4)) == 5

# atan2
with_bigfloat_precision(53) do
    @test isequal(atan2(12,2), atan2(MPFRFloat(12), MPFRFloat(2)))
end

# ldexp
with_bigfloat_precision(53) do
    @test ldexp(MPFRFloat(24.5), 72) == ldexp(24.5, 72)
    @test ldexp(MPFRFloat(24.5), int16(72)) == ldexp(24.5, 72)
    @test ldexp(MPFRFloat(24.5), -72) == ldexp(24.5, -72)
    @test ldexp(MPFRFloat(24.5), int16(-72)) == ldexp(24.5, -72)
    @test ldexp(MPFRFloat(24.5), uint(72)) == ldexp(24.5, 72)
    @test ldexp(MPFRFloat(24.5), 0x48) == ldexp(24.5, 72)
end

# basic arithmetic
# Signed addition
a = MPFRFloat("123456789012345678901234567890")
b = MPFRFloat("123456789012345678901234567891")
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
d = MPFRFloat("1606938044258990275541962092341162602522202993782792835301376")
f = MPFRFloat("6.223015277861141707144064053780124240590252168721167133101116614789698834035383e-61")

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
