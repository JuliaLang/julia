# This file is a part of Julia. License is MIT: https://julialang.org/license

using Serialization

import Base.MPFR
@testset "constructors" begin
    setprecision(53) do
        x = BigFloat()
        x = BigFloat(12)
    end
    x = BigFloat(12)
    @test x == BigFloat(x) == BigFloat(0xc) == BigFloat(12.) ==
          BigFloat(BigInt(12)) == BigFloat(BigFloat(12)) == parse(BigFloat,"12") ==
          parse(BigFloat,"12 ") == parse(BigFloat," 12") == parse(BigFloat," 12 ") ==
          BigFloat(Float32(12.)) == BigFloat(12//1) == BigFloat(SubString("12"))

    @test typeof(BigFloat(typemax(Int8))) == BigFloat
    @test typeof(BigFloat(typemax(Int16))) == BigFloat
    @test typeof(BigFloat(typemax(Int32))) == BigFloat
    @test typeof(BigFloat(typemax(Int64))) == BigFloat
    @test typeof(BigFloat(typemax(Int128))) == BigFloat

    @test typeof(BigFloat(true)) == BigFloat
    @test typeof(BigFloat(typemax(UInt8))) == BigFloat
    @test typeof(BigFloat(typemax(UInt16))) == BigFloat
    @test typeof(BigFloat(typemax(UInt32))) == BigFloat
    @test typeof(BigFloat(typemax(UInt64))) == BigFloat
    @test typeof(BigFloat(typemax(UInt128))) == BigFloat

    @test typeof(BigFloat(floatmax(Float32))) == BigFloat
    @test typeof(BigFloat(floatmax(Float64))) == BigFloat

    @test typeof(BigFloat(BigInt(1))) == BigFloat
    @test typeof(BigFloat(BigFloat(1))) == BigFloat

    @test typeof(BigFloat(1//1)) == BigFloat
    @test typeof(BigFloat(one(Rational{BigInt}))) == BigFloat

    # BigFloat constructor respects global precision when not specified
    let prec = precision(BigFloat) < 16 ? 256 : precision(BigFloat) ÷ 2
        xs = Real[T(1) for T in (Int, BigInt, Float32, Float64, BigFloat)]
        f = xs[end]
        @test BigFloat(f) === f # no-op when precision of operand is the same as global's one
        setprecision(prec) do
            @test precision(xs[end]) != prec
            for x in xs
                @test precision(BigFloat(x)) == prec
                @test precision(BigFloat(x; precision=prec ÷ 2)) == prec ÷ 2
            end
        end
    end
end
@testset "basic arithmetic" begin
    tol = 1e-12

    a = parse(BigFloat,"12.34567890121")
    b = parse(BigFloat,"12.34567890122")

    @test a+1e-11 ≈ b atol=tol
    @test !(b == a)
    @test b > a
    @test b >= a
    @test !(b < a)
    @test !(b <= a)

    c = parse(BigFloat,"24.69135780242")
    @test typeof(a * 2) == BigFloat
    @test a*2 ≈ c atol=tol
    @test (c-a) ≈ a atol=tol


    d = parse(BigFloat,"-24.69135780242")
    @test typeof(d) == BigFloat
    @test d+c ≈ 0 atol=tol

    @test (BigFloat(3)/BigFloat(2)) ≈ BigFloat(1.5) atol=tol
    @testset "+" begin
        x = BigFloat(12)
        y = BigFloat(30)
        @test x + y == BigFloat(42)
        @test x + typemax(UInt128) == x + BigInt(typemax(UInt128))
        @test x + typemax(Int128) == x + BigInt(typemax(Int128))
        f = parse(BigFloat,"1234567890.123")
        g = parse(BigFloat,"1234567891.123")

        tol = 1e-3

        @test f+Int8(1) ≈ g atol=tol
        @test f+Int16(1) ≈ g atol=tol
        @test f+Int32(1) ≈ g atol=tol
        @test f+Int64(1) ≈ g atol=tol
        @test f+Int128(1) ≈ g atol=tol

        @test f+true ≈ g atol=tol
        @test f+UInt8(1) ≈ g atol=tol
        @test f+UInt16(1) ≈ g atol=tol
        @test f+UInt32(1) ≈ g atol=tol
        @test f+UInt64(1) ≈ g atol=tol
        @test f+UInt128(1) ≈ g atol=tol

        @test f+BigInt(1) ≈ g atol=tol

        @test f+1f0 ≈ g atol=tol
        @test f+1e0 ≈ g atol=tol

        @test f+BigFloat(1) ≈ g atol=tol

        @test f+(1//1) ≈ g atol=tol

        @test f+one(Rational{BigInt}) ≈ g atol=tol

        a = parse(BigFloat,"123456789012345678901234567890")
        b = parse(BigFloat,"123456789012345678901234567891")
        @testset "Signed addition" begin
            @test a+Int8(1) == b
            @test a+Int16(1) == b
            @test a+Int32(1) == b
            @test a+Int64(1) == b
            @test Int8(1)+ a == b
            @test Int16(1)+a == b
            @test Int32(1)+a == b
            @test Int64(1)+a == b
            @test b+Int8(-1) == a
            @test b+Int16(-1) == a
            @test b+Int32(-1) == a
            @test b+Int64(-1) == a
            @test Int8(-1)+ b == a
            @test Int16(-1)+b == a
            @test Int32(-1)+b == a
            @test Int64(-1)+b == a
        end
        @testset "Unsigned addition" begin
            @test a+true == b
            @test a+UInt8(1) == b
            @test a+UInt16(1) == b
            @test a+UInt32(1) == b
            @test a+UInt64(1) == b
            @test true+a == b
            @test UInt8(1)+ a == b
            @test UInt16(1)+a == b
            @test UInt32(1)+a == b
            @test UInt64(1)+a == b
        end
        @testset "Float64 addition" begin
            @test a + 1.0f0 == b
            @test 1.0f0 + a == b
            @test a + 1.0 == b
            @test 1.0 + a == b
        end
        @testset "BigInt addition" begin
            @test a + BigInt(1) == b
            @test BigInt(1) + a == b
        end
    end
    @testset "-" begin
        x = BigFloat(12)
        y = BigFloat(-30)
        @test x - y == BigFloat(42)
        @test x - typemax(UInt128) == x - BigInt(typemax(UInt128))
        @test x - typemax(Int128) == x - BigInt(typemax(Int128))
        a = parse(BigFloat,"123456789012345678901234567890")
        b = parse(BigFloat,"123456789012345678901234567891")

        @testset "Signed subtraction" begin
            @test b-Int8(1) == a
            @test b-Int16(1) == a
            @test b-Int32(1) == a
            @test b-Int64(1) == a
            @test Int8(1)- b == -a
            @test Int16(1)-b == -a
            @test Int32(1)-b == -a
            @test Int64(1)-b == -a
            @test a-Int8(-1) == b
            @test a-Int16(-1) == b
            @test a-Int32(-1) == b
            @test a-Int64(-1) == b
            @test Int8(-1)- a == -b
            @test Int16(-1)-a == -b
            @test Int32(-1)-a == -b
            @test Int64(-1)-a == -b
        end
        @testset "Unsigned subtraction" begin
            @test b-true == a
            @test b-UInt8(1) == a
            @test b-UInt16(1) == a
            @test b-UInt32(1) == a
            @test b-UInt64(1) == a
            @test true-b == -a
            @test UInt8(1)- b == -a
            @test UInt16(1)-b == -a
            @test UInt32(1)-b == -a
            @test UInt64(1)-b == -a
        end
        @testset "Float64 subtraction" begin
            @test b - 1.0f0 == a
            @test 1.0f0 - b == -a
            @test b - 1.0 == a
            @test 1.0 - b == -a
        end
        @testset "BigInt subtraction" begin
            @test b - BigInt(1) == a
            @test BigInt(1) - b == -a
        end
    end
    @testset "*" begin
        x = BigFloat(6)
        y = BigFloat(9)
        @test x * y != BigFloat(42)
        @test x * y == BigFloat(54)
        @test x * typemax(UInt128) == x * BigInt(typemax(UInt128))
        @test x * typemax(Int128) == x * BigInt(typemax(Int128))

        a = parse(BigFloat,"123456789012345678901234567890")
        b = parse(BigFloat,"123456789012345678901234567891")
        @testset "Signed multiplication" begin
            @test a*Int8(1) == a
            @test a*Int16(1) == a
            @test a*Int32(1) == a
            @test a*Int64(1) == a
            @test Int8(1)* a == a
            @test Int16(1)*a == a
            @test Int32(1)*a == a
            @test Int64(1)*a == a
            @test a*Int8(-1) == -a
            @test a*Int16(-1) == -a
            @test a*Int32(-1) == -a
            @test a*Int64(-1) == -a
            @test Int8(-1)* a == -a
            @test Int16(-1)*a == -a
            @test Int32(-1)*a == -a
            @test Int64(-1)*a == -a
        end
        @testset "Unsigned multiplication" begin
            @test a*true == a
            @test a*UInt8(1) == a
            @test a*UInt16(1) == a
            @test a*UInt32(1) == a
            @test a*UInt64(1) == a
            @test true*a == a
            @test UInt8(1)* a == a
            @test UInt16(1)*a == a
            @test UInt32(1)*a == a
            @test UInt64(1)*a == a
        end
        @testset "Float64 multiplication" begin
            @test a * 1.0f0 == a
            @test 1.0f0 * a == a
            @test a * 1.0 == a
            @test 1.0 * a == a
        end
        @testset "BigInt multiplication" begin
            @test a * BigInt(1) == a
            @test BigInt(1) * a == a
        end
    end
    @testset "/" begin
        x = BigFloat(9)
        y = BigFloat(6)
        @test x / y == BigFloat(9/6)
        @test x / typemax(UInt128) == x / BigInt(typemax(UInt128))
        @test x / typemax(Int128) == x / BigInt(typemax(Int128))

        a = parse(BigFloat,"123456789012345678901234567890")
        b = parse(BigFloat,"123456789012345678901234567891")
        c = parse(BigInt,"61728394506172839450617283945")
        # d = 2^200
        d = parse(BigFloat,"1606938044258990275541962092341162602522202993782792835301376")
        f = parse(BigFloat,"6.223015277861141707144064053780124240590252168721167133101116614789698834035383e-61")

        @testset "Signed division" begin
            @test a/Int8(2) == c
            @test a/Int16(2) == c
            @test a/Int32(2) == c
            @test a/Int64(2) == c
            @test Int8(1)/ d == f
            @test Int16(1)/d == f
            @test Int32(1)/d == f
            @test Int64(1)/d == f
            @test a/Int8(-2) == -c
            @test a/Int16(-2) == -c
            @test a/Int32(-2) == -c
            @test a/Int64(-2) == -c
            @test Int8(-1)/ d == -f
            @test Int16(-1)/d == -f
            @test Int32(-1)/d == -f
            @test Int64(-1)/d == -f
        end
        @testset "Unsigned division" begin
            @test a/true == a
            @test a/UInt8(2) == c
            @test a/UInt16(2) == c
            @test a/UInt32(2) == c
            @test a/UInt64(2) == c
            @test true/d == f
            @test UInt8(1)/ d == f
            @test UInt16(1)/d == f
            @test UInt32(1)/d == f
            @test UInt64(1)/d == f
        end
        @testset "Float64 division" begin
            @test a / 2.0f0 == c
            @test 1.0f0 / d == f
            @test a / 2.0 == c
            @test 1.0 / d == f
        end
        # BigInt division
        @test a / BigInt(2) == c
        # inv
        @test inv(x) == one(x)/x == 1/x == x^-1 == Clong(1)/x
    end
    #^
    x = BigFloat(12)
    y = BigFloat(4)
    @test x^y == BigFloat(20736)

    @test big(2.0)^big(3) == 8
    for T in [Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, BigInt]
        @test T(2)^big(3.0) == 8
        @test big(2.0)^T(3) == 8
    end
end
@testset "iterated arithmetic" begin
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

    @test *(a, b) == parse(BigFloat,"2.8328125e+02")
    @test *(a, b, c) == parse(BigFloat,"-1.98296875e+03")
    @test *(a, b, c, d) == parse(BigFloat,"2.52828515625e+04")
    @test *(a, b, c, d, f) == parse(BigFloat,"5.214588134765625e+04")
    @test *(a, b, c, d, f, g) == parse(BigFloat,"1.6295587921142578125e+03")
end
@testset "< / > / <= / >=" begin
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
end
@testset "rounding modes" begin
    setprecision(4) do
        # default mode is round to nearest
        down, up =  setrounding(BigFloat,RoundNearest) do
            parse(BigFloat,"0.0938"), parse(BigFloat,"0.102")
        end
        setrounding(BigFloat,RoundDown) do
            @test BigFloat(0.1) == down
            @test BigFloat(0.1) != up
        end
        setrounding(BigFloat,RoundUp) do
            @test BigFloat(0.1) != down
            @test BigFloat(0.1) == up
        end
    end
end

@testset "copysign / sign" begin
    x = BigFloat(1)
    y = BigFloat(-1)
    @test copysign(x, y) == y
    @test copysign(y, x) == x
    @test copysign(1.0, BigFloat(NaN)) == 1.0

    @test sign(BigFloat(-3.0)) == -1.0
    @test sign(BigFloat( 3.0)) == 1.0
    @test isequal(sign(BigFloat(-0.0)), BigFloat(-0.0))
    @test isequal(sign(BigFloat( 0.0)), BigFloat( 0.0))
    @test isnan(sign(BigFloat(NaN)))
end
@testset "isfinite / isinf / isnan" begin
    x = BigFloat(Inf)
    y = BigFloat(1)
    @test isinf(x) == true
    @test isinf(y) == false
    @test isfinite(x) == false
    @test isinf(x) == true

    x = BigFloat(NaN)
    y = BigFloat(1)
    @test isnan(x) == true
    @test isnan(y) == false
end
@testset "convert to BigFloat" begin
    @test convert(BigFloat, 1//2) == parse(BigFloat,"0.5")
    @test typeof(convert(BigFloat, 1//2)) == BigFloat
    @test convert(BigFloat, 0.5) == parse(BigFloat,"0.5")
    @test typeof(convert(BigFloat, 0.5)) == BigFloat
    @test convert(BigFloat, 40) == parse(BigFloat,"40")
    @test typeof(convert(BigFloat, 40)) == BigFloat
    @test convert(BigFloat, Float32(0.5)) == parse(BigFloat,"0.5")
    @test typeof(convert(BigFloat, Float32(0.5))) == BigFloat
    @test convert(BigFloat, parse(BigInt,"9223372036854775808")) == parse(BigFloat,"9223372036854775808")
    @test typeof(convert(BigFloat, parse(BigInt,"9223372036854775808"))) == BigFloat
    @test convert(AbstractFloat, parse(BigInt,"9223372036854775808")) == parse(BigFloat,"9223372036854775808")
    @test typeof(convert(AbstractFloat, parse(BigInt,"9223372036854775808"))) == BigFloat

    @test signbit(BigFloat(NaN)) == 0
    @test signbit(BigFloat(-NaN)) == 1
end
@testset "convert from BigFloat" begin
    @test convert(Float64, BigFloat(0.5)) == 0.5
    @test convert(Float32, BigFloat(0.5)) == Float32(0.5)
    @test convert(Float16, BigFloat(0.5)) == Float16(0.5)
    @test convert(Bool, BigFloat(0.0)) == false
    @test convert(Bool, BigFloat(1.0)) == true
    @test_throws InexactError convert(Bool, BigFloat(0.1))

    @test signbit(Float64(BigFloat(NaN))) == 0
    @test signbit(Float64(-BigFloat(NaN))) == 1
end
@testset "exponent, frexp, significand" begin
    x = BigFloat(0)
    @test_throws DomainError exponent(x)
    x = BigFloat(Inf)
    @test_throws DomainError exponent(x)
    x = BigFloat(15.674)
    @test exponent(x) == exponent(15.674)

    for i in [big(0.2), big(1.2), big(1220.0), big(23414.123)]
        mantissa, ex = frexp(i)
        @test i == mantissa * 2. ^ ex
    end

    for i in [big(0.2), big(1.2), big(1220.0), big(23414.123)]
        @test i == significand(i) * 2. ^ exponent(i)
    end
end
@testset "nextfloat/prevfloat" begin
    #should be immutable
    x = 12.
    y = BigFloat(x)
    @test x == y
    nextfloat(y)
    @test x == y
    prevfloat(y)
    @test x == y
    setprecision(53) do
        x = BigFloat(12.12)
        @test BigFloat(nextfloat(12.12)) == nextfloat(x)
        @test BigFloat(prevfloat(12.12)) == prevfloat(x)
    end
    x = BigFloat(12.12, precision = 100)
    @test nextfloat(x, 0) === x
    @test prevfloat(x, 0) === x
    @test nextfloat(x).prec == x.prec
    @test prevfloat(x).prec == x.prec
    @test nextfloat(x) == nextfloat(x, 1)
    @test prevfloat(x) == prevfloat(x, 1)
    @test nextfloat(x, -1) == prevfloat(x, 1)
    @test nextfloat(x, -2) == prevfloat(x, 2)
    @test prevfloat(x, -1) == nextfloat(x, 1)
    @test prevfloat(x, -2) == nextfloat(x, 2)
    @test isnan(nextfloat(BigFloat(NaN)))
    @test isnan(prevfloat(BigFloat(NaN)))
    @test isnan(nextfloat(BigFloat(NaN), 1))
    @test isnan(prevfloat(BigFloat(NaN), 1))
end
# sqrt DomainError
@test_throws DomainError sqrt(BigFloat(-1))

@testset "precision" begin
    old_precision = precision(BigFloat)
    x = BigFloat(0)
    @test precision(x) == old_precision
    setprecision(256)
    x = BigFloat(0)
    @test precision(x) == 256
    setprecision(old_precision)
    z = setprecision(240) do
        z = x + 20
        return z
    end
    @test float(z) == 20.
    @test precision(z) == 240
    x = BigFloat(12)
    @test precision(x) == old_precision
    @test_throws DomainError setprecision(1)
    @test_throws DomainError BigFloat(1, precision = 0)
    @test_throws DomainError BigFloat(big(1.1), precision = 0)
    @test_throws DomainError BigFloat(2.5, precision = -900)
    # issue 15659
    @test (setprecision(53) do; big(1/3); end) < 1//3
end
@testset "isinteger" begin
    @test !isinteger(BigFloat(1.2))
    @test isinteger(BigFloat(12))
    @test isinteger(zero(BigFloat))
    @test isinteger(-zero(BigFloat))
    @test !isinteger(nextfloat(zero(BigFloat)))
    @test !isinteger(prevfloat(zero(BigFloat)))
    @test isinteger(maxintfloat(BigFloat))
    @test isinteger(-maxintfloat(BigFloat))
    @test !isinteger(BigFloat(Inf))
    @test !isinteger(-BigFloat(Inf))
    @test !isinteger(BigFloat(NaN))
end

@testset "comparisons" begin
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

    # total ordering
    @test isless(big(-0.0), big(0.0))
    @test isless(big(1.0), big(NaN))
    @test isless(big(Inf), big(NaN))
    @test isless(big(Inf), -big(NaN))
    @test !isless(big(NaN), big(NaN))
    @test !isless(big(-NaN), big(NaN))
    @test !isless(-big(NaN), big(NaN))
    @test !isless(-big(NaN), big(1.0))
    @test !isless(-big(NaN), 1.0)

    # cmp
    @test cmp(big(-0.0), big(0.0)) == -1
    @test cmp(big(0.0), big(-0.0)) == 1
    @test cmp(big(1.0), big(NaN)) == -1
    @test cmp(big(NaN), big(NaN)) == 0
    @test cmp(big(NaN), big(1.0)) == 1
end
@testset "signbit" begin
    @test signbit(BigFloat(-1.0)) == 1
    @test signbit(BigFloat(1.0)) == 0
    @test signbit(BigFloat(-0.0)) == 1
end
@testset "modf" begin
    x = BigFloat(12)
    y = BigFloat(0.5)
    @test modf(x+y) == (y, x)
    x = BigFloat(NaN)
    @test map(isnan, modf(x)) == (true, true)
    @test isequal(modf(BigFloat(-Inf)), (BigFloat(-0.0), BigFloat(-Inf)))
    @test isequal(modf(BigFloat(Inf)), (BigFloat(0.0), BigFloat(Inf)))
end
@testset "rem" begin
    setprecision(53) do
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
end
@testset "min/max" begin
    x = BigFloat(4)
    y = BigFloat(2)
    @test max(x,y) == x
    @test min(x,y) == y
    y = BigFloat(NaN)
    @test isnan(max(x,y))
    @test isnan(min(x,y))
    @test isnan(max(y,y))
    @test isnan(min(y,y))
end
@testset "sum" begin
    x = BigFloat(1)
    y = BigFloat(2)
    z = BigFloat(3)
    w = BigFloat(4)
    @test sum([x,y,z,w]) == BigFloat(10)
    big_array = fill(BigFloat(1), 100)
    @test sum(big_array) == BigFloat(100)
    @test sum(BigFloat[]) == BigFloat(0)
end
@testset "promotion" begin
    # the array converts everyone to the DEFAULT_PRECISION!
    x = BigFloat(12)
    y = setprecision(60) do
        BigFloat(42)
    end
    @test [x,y] == [BigFloat(12), BigFloat(42)]
end
@testset "log / log2 / log10" begin
    setprecision(53) do
    x = BigFloat(42)
        @test log(x) == log(42)
        @test isinf(log(BigFloat(0)))
        @test_throws DomainError log(BigFloat(-1))
        @test log2(x) == log2(42)
        @test isinf(log2(BigFloat(0)))
        @test_throws DomainError log2(BigFloat(-1))
        @test log10(x) == log10(42)
        @test isinf(log10(BigFloat(0)))
        @test_throws DomainError log10(BigFloat(-1))
    end
end
@testset "exp / exp2 / exp10" begin
    setprecision(53) do
        x = BigFloat(10)
        @test exp(x) == exp(10)
        @test exp2(x) == 1024
        @test exp10(x) == 10000000000
    end
end
@testset "convert to integer types" begin
    x = BigFloat(12.1)
    y = BigFloat(42)
    @test_throws InexactError convert(Int32, x)
    @test_throws InexactError convert(Int64, x)
    @test_throws InexactError convert(BigInt, x)
    @test_throws InexactError convert(UInt32, x)
    @test_throws InexactError convert(UInt32, x)
    @test convert(Int32, y) == 42
    @test convert(Int64, y) == 42
    @test convert(BigInt, y) == 42
    @test convert(UInt32, y) == 42
    @test convert(UInt32, y) == 42
end
@testset "round" begin
    x = BigFloat(42.42)
    y = setprecision(256) do
        parse(BigFloat,"9223372036854775809.2324")
    end
    z = parse(BigInt,"9223372036854775809")
    @test round(Integer,x) == 42
    @test round(Integer,y) == z
    @test typeof(round(UInt8, x)) == UInt8 && round(UInt8, x) == 0x2a
    @test typeof(round(UInt16, x)) == UInt16 && round(UInt16, x) == 0x2a
    @test typeof(round(UInt32, x)) == UInt32 && round(UInt32, x) == 0x2a
    @test typeof(round(UInt64, x)) == UInt64 && round(UInt64, x) == 0x2a
    @test typeof(round(Int64, x)) == Int64 && round(Int64, x) == 42
    @test typeof(round(Int, x)) == Int && round(Int, x) == 42
    @test typeof(round(UInt, x)) == UInt && round(UInt, x) == 0x2a
end
@testset "string representation" begin
    str = "1.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000012"
    setprecision(406) do
        @test string(nextfloat(BigFloat(1))) == str
    end
    setprecision(21) do
        @test string(parse(BigFloat, "0.1")) == "0.10000002"
        @test string(parse(BigFloat, "-9.9")) == "-9.9000015"
    end
    setprecision(40) do
        @test string(parse(BigFloat, "0.1")) == "0.10000000000002"
        @test string(parse(BigFloat, "-9.9")) == "-9.8999999999942"
    end
    setprecision(123) do
        @test string(parse(BigFloat, "0.1")) == "0.0999999999999999999999999999999999999953"
        @test string(parse(BigFloat, "-9.9")) == "-9.8999999999999999999999999999999999997"
    end
end
@testset "eps" begin
    x = eps(BigFloat)
    @test BigFloat(1) + x == BigFloat(1) + prevfloat(x)
    @test eps(BigFloat) == eps(BigFloat(1))
end
@testset "floatmin/floatmax" begin
    x = floatmin(BigFloat)
    @test x > 0
    @test prevfloat(x) == 0
    x = floatmax(BigFloat)
    @test !isinf(x)
    @test isinf(nextfloat(x))
end
@testset "factorial" begin
    setprecision(256) do
        x = BigFloat(42)
        @test factorial(x) == factorial(BigInt(42))
        x = BigFloat(10)
        @test factorial(x) == factorial(10)
        @test_throws DomainError factorial(BigFloat(-1))
        @test_throws DomainError factorial(BigFloat(331.3))
    end
end
@testset "trigonometric functions" begin
    setprecision(53) do
        for f in (:sin,:cos,:tan,:sec,:csc,:cot,:acos,:asin,:atan,
                :cosh,:sinh,:tanh,:sech,:csch,:coth,:asinh),
            j in (-1., -0.5, -0.25, .25, .5, 1.)
            @eval begin
                @test ($f)(BigFloat($j)) ≈ ($f)($j)
            end
        end
        for f in (:acos,:asin,:acosh,:atanh),
            j in (-2, -1.5)
            @eval begin
                @test_throws DomainError ($f)(BigFloat($j))
            end
        end
        for f in (:sin,:cos,:tan,:sec,:csc,:cot,:cosh,:sinh,:tanh,
                  :sech,:csch,:coth,:acosh,:asinh),
            j in (1., 1.5, 1.9)
            @eval begin
                @test ($f)(BigFloat($j)) ≈ ($f)($j)
            end
        end
        for j in (.25, .5)
            @test atanh(BigFloat(j)) ≈ atanh(j)
        end
    end

    # hypot
    @test hypot(BigFloat(3), BigFloat(4)) == 5

    # atan
    setprecision(53) do
        @test atan(12,2) == atan(BigFloat(12), BigFloat(2))
    end
end
@testset "ldexp" begin
    setprecision(53) do
        @test ldexp(BigFloat(24.5), 72) == ldexp(24.5, 72)
        @test ldexp(BigFloat(24.5), Int16(72)) == ldexp(24.5, 72)
        @test ldexp(BigFloat(24.5), -72) == ldexp(24.5, -72)
        @test ldexp(BigFloat(24.5), Int16(-72)) == ldexp(24.5, -72)
        @test ldexp(BigFloat(24.5), UInt(72)) == ldexp(24.5, 72)
        @test ldexp(BigFloat(24.5), 0x48) == ldexp(24.5, 72)
    end
end
@testset "ceil / floor / trunc" begin
    x = BigFloat(12.042)
    @test BigFloat(13) == ceil(x)
    x = parse(BigFloat,"28273.7312487489135135135")
    y = BigInt(28273)
    z = BigInt(28274)
    a = parse(BigFloat,"123456789012345678901234567890.2414")
    b = parse(BigInt,"123456789012345678901234567890")
    c = parse(BigInt,"123456789012345678901234567891")
    @test ceil(x) == z
    @test typeof(ceil(x)) == BigFloat
    @test floor(x) == y
    @test typeof(floor(x)) == BigFloat
    @test trunc(x) == y
    @test typeof(trunc(x)) == BigFloat

    @test ceil(Integer,x) == z
    @test typeof(ceil(Integer,x)) == BigInt
    @test floor(Integer,x) == y
    @test typeof(floor(Integer,x)) == BigInt
    @test trunc(Integer,x) == y
    @test typeof(trunc(Integer,x)) == BigInt

    @test ceil(Int64, x) == Int64(z)
    @test typeof(ceil(Int64, x)) == Int64
    @test floor(Int64, x) == Int64(y)
    @test typeof(floor(Int64, x)) == Int64
    @test trunc(Int64, x) == Int64(y)
    @test typeof(trunc(Int64, x)) == Int64

    @test ceil(Int32, x) == Int32(z)
    @test typeof(ceil(Int32, x)) == Int32
    @test floor(Int32, x) == Int32(y)
    @test typeof(floor(Int32, x)) == Int32
    @test trunc(Int32, x) == Int32(y)
    @test typeof(trunc(Int32, x)) == Int32

    @test ceil(Int16, x) == Int16(z)
    @test typeof(ceil(Int16, x)) == Int16
    @test floor(Int16, x) == Int16(y)
    @test typeof(floor(Int16, x)) == Int16
    @test trunc(Int16, x) == Int16(y)
    @test typeof(trunc(Int16, x)) == Int16

    #@test ceil(Int8, x) == Int8(z)
    #@test typeof(ceil(Int8, x)) == Int8
    #@test floor(Int8, x) == Int8(y)
    #@test typeof(floor(Int8, x)) == Int8
    #@test trunc(Int8, x) == Int8(y)
    #@test typeof(trunc(Int8, x)) == Int8

    @test ceil(UInt64, x) == UInt64(z)
    @test typeof(ceil(UInt64, x)) == UInt64
    @test floor(UInt64, x) == UInt64(y)
    @test typeof(floor(UInt64, x)) == UInt64
    @test trunc(UInt64, x) == UInt64(y)
    @test typeof(trunc(UInt64, x)) == UInt64

    @test ceil(UInt32, x) == UInt32(z)
    @test typeof(ceil(UInt32, x)) == UInt32
    @test floor(UInt32, x) == UInt32(y)
    @test typeof(floor(UInt32, x)) == UInt32
    @test trunc(UInt32, x) == UInt32(y)
    @test typeof(trunc(UInt32, x)) == UInt32

    @test ceil(UInt16, x) == UInt16(z)
    @test typeof(ceil(UInt16, x)) == UInt16
    @test floor(UInt16, x) == UInt16(y)
    @test typeof(floor(UInt16, x)) == UInt16
    @test trunc(UInt16, x) == UInt16(y)
    @test typeof(trunc(UInt16, x)) == UInt16

    #@test ceil(UInt8, x) == UInt8(z)
    #@test typeof(ceil(UInt8, x)) == UInt8
    #@test floor(UInt8, x) == UInt8(y)
    #@test typeof(floor(UInt8, x)) == UInt8
    #@test trunc(UInt8, x) == UInt8(y)
    #@test typeof(trunc(UInt8, x)) == UInt8

    @test ceil(Integer,a) == c
    @test typeof(ceil(Integer,a)) == BigInt
    @test floor(Integer,a) == b
    @test typeof(floor(Integer,a)) == BigInt
    @test trunc(Integer,a) == b
    @test typeof(trunc(Integer,a)) == BigInt

    @test ceil(Int128,a) == c
    @test typeof(ceil(Int128,a)) == Int128
    @test floor(Int128,a) == b
    @test typeof(floor(Int128,a)) == Int128
    @test trunc(Int128,a) == b
    @test typeof(trunc(Int128,a)) == Int128

    @test ceil(UInt128,a) == c
    @test typeof(ceil(UInt128,a)) == UInt128
    @test floor(UInt128,a) == b
    @test typeof(floor(UInt128,a)) == UInt128
    @test trunc(UInt128,a) == b
    @test typeof(trunc(UInt128,a)) == UInt128

    # Issue #33676
    @test trunc(UInt8, parse(BigFloat,"255.1")) == UInt8(255)
    @test_throws InexactError trunc(UInt8, parse(BigFloat,"256.1"))

    @testset "inexact limits ($T)" for T in Base.BitInteger_types
        typemin_and_half = BigFloat(typemin(T)) - 0.5
        typemax_and_half = BigFloat(typemax(T)) + 0.5
        typemin_and_one = BigFloat(typemin(T)) - 1
        typemax_and_one = BigFloat(typemax(T)) + 1

        @test trunc(T, typemin_and_half) == typemin(T)
        @test trunc(T, typemax_and_half) == typemax(T)
        @test_throws InexactError trunc(T, typemin_and_one)
        @test_throws InexactError trunc(T, typemax_and_one)

        @test_throws InexactError floor(T, typemin_and_half)
        @test floor(T, typemax_and_half) == typemax(T)
        @test_throws InexactError floor(T, typemin_and_one)
        @test_throws InexactError floor(T, typemax_and_one)

        @test ceil(T, typemin_and_half) == typemin(T)
        @test_throws InexactError ceil(T, typemax_and_half)
        @test_throws InexactError ceil(T, typemin_and_one)
        @test_throws InexactError ceil(T, typemax_and_one)

        if iseven(typemin(T))
            @test round(T, typemin_and_half) == typemin(T)
        else
            @test_throws InexactError round(T, typemin_and_half)
        end

        if iseven(typemax(T))
            @test round(T, typemax_and_half) == typemax(T)
        else
            @test_throws InexactError round(T, typemax_and_half)
        end

        @test round(T, BigFloat(typemin(T)) - 0.4) == typemin(T)
        @test round(T, BigFloat(typemax(T)) + 0.4) == typemax(T)
        @test_throws InexactError round(T, typemin_and_one)
        @test_throws InexactError round(T, typemax_and_one)
    end
end
@testset "div" begin
    @test div(big"1.0",big"0.1") == 9
    @test div(1,big"0.1") == 9
    @test div(1.0,big"0.1") == 9
    @test div(big"1.0",0.1) == 9
    @test div(big"1",big"0.1") == 9
    @test div(big"1",0.1) == 9
end
@testset "issue #5963" begin
    @test typemax(Int128) == convert(BigFloat, typemax(Int128))
    @test typemax(Int128)  * big(1.0) == convert(BigFloat, typemax(Int128))
    @test typemax(UInt64)  * big(1.0) == big(typemax(UInt64))
    @test typemax(UInt128) * big(1.0) == big(typemax(UInt128))
end
@testset "issue #3399" begin
    i1 = BigInt(10)^Int32(1000)
    i2 = parse(BigInt,"10000000000000000000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
    f = BigFloat(10)^Int32(1000)
    @test i1 != i2
    @test i1 != f
    @test i2 != f

    @test f > i1
    @test f > i2

    i3 = trunc(Integer,f)
    @test i3 == f
    @test i3+1 > f
    @test i3+1 >= f
end
# issue #8318
@test convert(Int64,big(500_000_000_000_000.)) == 500_000_000_000_000

@testset "issue #9816" begin
    # check exponent range is set to max possible
    @test MPFR.get_emin() == MPFR.get_emin_min()
    @test MPFR.get_emax() == MPFR.get_emax_max()
end
# issue #10994: handle embedded NUL chars for string parsing
@test_throws ArgumentError parse(BigFloat, "1\0")

@testset "serialization (issue #12386)" begin
    b = PipeBuffer()
    let x = setprecision(53) do
            return 2.1 * big(pi)
        end
        serialize(b, x)
        @test deserialize(b) == x
    end
    let x = BigFloat(Inf, precision = 46)
        serialize(b, x)
        @test deserialize(b) == x == BigFloat(Inf, precision = 2)
    end
end
@test isnan(sqrt(BigFloat(NaN)))

@testset "PR 17217" begin
    # PR 17217 -- BigFloat constructors with given precision and rounding mode
    # test constructors and `big` with additional precision and rounding mode:
    for prec in (10, 100, 1000)
        for val in ("3.1", pi, "-1.3", 3.1, 1//10)
            let a = BigFloat(val),
                b = BigFloat(val, precision = prec),
                c = BigFloat(val, RoundUp),
                d = BigFloat(val, RoundDown, precision = prec),
                e = BigFloat(val, RoundUp, precision = prec)

                @test precision(a) == precision(BigFloat)
                @test precision(b) == prec
                @test precision(c) == precision(BigFloat)
                @test precision(d) == prec
                @test precision(e) == prec
                (val != 3.1) && @test e > d     # rounding has no effect when constructing from Float64
            end
        end
    end
end

# issue #22758
if MPFR.version() > v"3.1.5" || "r11590" in MPFR.patches()
    setprecision(2_000_000) do
        @test abs(sin(big(pi)/6) - 0.5) < ldexp(big(1.0),-1_999_000)
    end
end

@testset "show BigFloat" begin
    function test_show_bigfloat(x::BigFloat; contains_e::Bool=true,
        ends::String="",
        starts::String="")
        sx = sprint(show, x)
        scx = sprint(show, x, context=:compact => true)
        strx = string(x)
        @test sx == strx
        @test length(scx) < 20
        @test length(scx) <= length(sx)
        @test x == parse(BigFloat, sx)
        @test ≈(x, parse(BigFloat, scx), rtol=1e-4)
        for s in (sx, scx)
            @test occursin('e', s) == contains_e
            @test startswith(s, starts)
            @test endswith(s, ends)
        end
    end

    test_show_bigfloat(big"1.23456789", contains_e=false, starts="1.23")
    test_show_bigfloat(big"-1.23456789", contains_e=false, starts="-1.23")
    test_show_bigfloat(big"2.3457645687563543266576889678956787e10000", starts="2.345", ends="e+10000")
    test_show_bigfloat(big"-2.3457645687563543266576889678956787e-10000", starts="-2.345", ends="e-10000")
    test_show_bigfloat(big"42.0", contains_e=false, starts="42.0")
    test_show_bigfloat(big"420.0", contains_e=false, starts="420.0") # '0's have to be added on the right before point
    test_show_bigfloat(big"-420.0", contains_e=false, starts="-420.0")
    test_show_bigfloat(big"420000.0", contains_e=false, starts="420000.0")
    test_show_bigfloat(big"654321.0", contains_e=false, starts="654321.0")
    test_show_bigfloat(big"-654321.0", contains_e=false, starts="-654321.0")
    test_show_bigfloat(big"6543210.0", contains_e=true, starts="6.5", ends="e+06")
    test_show_bigfloat(big"0.000123", contains_e=false, starts="0.000123")
    test_show_bigfloat(big"-0.000123", contains_e=false, starts="-0.000123")
    test_show_bigfloat(big"0.00001234", contains_e=true, starts="1.23", ends="e-05")

    for to_string in [string,
                      x->sprint(show, x),
                      x->sprint(show, x, context=:compact => true)]
        @test to_string(big"0.0") == "0.0"
        @test to_string(big"-0.0") == "-0.0"
        @test to_string(big"1.0") == "1.0"
        @test to_string(big"-1.0") == "-1.0"
    end
end

@testset "big(::Type)" begin
    for x in (2f0, pi, 7.8, big(ℯ))
        @test big(typeof(x)) == typeof(big(x))
        @test big(typeof(complex(x, x))) == typeof(big(complex(x, x)))
    end
end
