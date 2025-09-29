# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Random

# test the basic floating point functions

@testset "flipsign" begin
    for elty in (Float32,Float64)
        x = convert(elty,-2.0)
        x = flipsign(x,-1.0)
        @test flipsign(x,big(-1.0)) == convert(elty,-2.0)
    end
end

@testset "maxintfloat" begin
    @test maxintfloat(Float16) === Float16(2048f0)
    for elty in (Float16,Float32,Float64)
        @test maxintfloat(rand(elty)) === maxintfloat(elty)
    end
    @test maxintfloat() === maxintfloat(Float64)
    @test maxintfloat(Float64, Int32) === 2147483647.0
    @test maxintfloat(Float32, Int32) === maxintfloat(Float32)
    @test maxintfloat(Float64, Int16) === 32767.0
    @test maxintfloat(Float64, Int64) === maxintfloat(Float64)
end

@testset "isinteger" begin
    for elty in (Float16, Float32, Float64)
        @test !isinteger(elty(1.2))
        @test isinteger(elty(12))
        @test isinteger(zero(elty))
        @test isinteger(-zero(elty))
        @test !isinteger(nextfloat(zero(elty)))
        @test !isinteger(prevfloat(zero(elty)))
        @test isinteger(maxintfloat(elty))
        @test isinteger(-maxintfloat(elty))
        @test !isinteger(elty(Inf))
        @test !isinteger(-elty(Inf))
        @test !isinteger(elty(NaN))
    end
end

@testset "ispow2 and iseven/isodd" begin
    for T in (Float16,Float32,Float64,BigFloat)
        for x in (0.25, 1.0, 4.0, exp2(T(exponent(floatmax(T)))), exp2(T(exponent(floatmin(T)))))
            @test ispow2(T(x))
        end
        for x in (1.5, 0.0, 7.0, NaN, Inf)
            @test !ispow2(T(x))
        end
        for x in (0, 134)
            @test iseven(T(x)) && iseven(T(-x))
            @test isodd(T(x+1)) && isodd(T(-x-1))
        end
        let x = maxintfloat(T) * π
            @test iseven(x) && iseven(-x)
            @test !isodd(x) && !isodd(-x)
        end
        @test !iseven(0.5) && !isodd(0.5)
    end
end

@testset "round" begin
    for elty in (Float32, Float64)
        x = rand(elty)
        A = fill(x,(10,10))
        @test round.(A,RoundToZero) == fill(trunc(x),(10,10))
        @test round.(A,RoundUp) == fill(ceil(x),(10,10))
        @test round.(A,RoundDown) == fill(floor(x),(10,10))
        A = fill(x,(10,10,10))
        @test round.(A,RoundToZero) == fill(trunc(x),(10,10,10))
        @test round.(A,RoundUp) == fill(ceil(x),(10,10,10))
        @test round.(A,RoundDown) == fill(floor(x),(10,10,10))
        for elty2 in (Int32,Int64)
            A = fill(x,(10,))
            @test round.(elty2,A,RoundToZero) == fill(trunc(elty2,x),(10,))
            @test round.(elty2,A,RoundUp) == fill(ceil(elty2,x),(10,))
            @test round.(elty2,A,RoundDown) == fill(floor(elty2,x),(10,))
            A = fill(x,(10,10))
            @test round.(elty2,A,RoundToZero) == fill(trunc(elty2,x),(10,10))
            @test round.(elty2,A,RoundUp) == fill(ceil(elty2,x),(10,10))
            @test round.(elty2,A,RoundDown) == fill(floor(elty2,x),(10,10))
            A = fill(x,(10,10,10))
            @test round.(elty2,A,RoundToZero) == fill(trunc(elty2,x),(10,10,10))
            @test round.(elty2,A,RoundUp) == fill(ceil(elty2,x),(10,10,10))
            @test round.(elty2,A,RoundDown) == fill(floor(elty2,x),(10,10,10))
            @test round.(elty2,A) == fill(round(elty2,x),(10,10,10))
        end
    end
end

@testset "Types" begin
    for x in (Int16(0), 1, 2f0, pi, 3//4, big(5//6), 7.8, big(9), big(ℯ))
        @test float(typeof(x)) == typeof(float(x))
        @test float(typeof(complex(x, x))) == typeof(float(complex(x, x)))
    end
end

@testset "significant digits" begin
    # (would be nice to have a smart vectorized
    # version of signif)
    @test round(123.456, sigdigits=1) ≈ 100.
    @test round(123.456, sigdigits=3) ≈ 123.
    @test round(123.456, sigdigits=5) ≈ 123.46
    @test round(123.456, sigdigits=8, base = 2) ≈ 123.5
    @test round(123.456, sigdigits=2, base = 4) ≈ 128.0
    @test round(0.0, sigdigits=1) === 0.0
    @test round(-0.0, sigdigits=1) === -0.0
    @test round(1.2, sigdigits=2) === 1.2
    @test round(1.0, sigdigits=6) === 1.0
    @test round(0.6, sigdigits=1) === 0.6
    @test round(7.262839104539736, sigdigits=2) === 7.3
    @test isinf(round(Inf, sigdigits=3))
    @test isnan(round(NaN, sigdigits=3))
    @test round(1.12312, sigdigits=1000) === 1.12312
    @test round(Float32(7.262839104539736), sigdigits=3) === Float32(7.26)
    @test round(Float32(7.262839104539736), sigdigits=4) === Float32(7.263)
    @test round(Float32(1.2), sigdigits=3) === Float32(1.2)
    @test round(Float32(1.2), sigdigits=5) === Float32(1.2)
    @test round(Float16(0.6), sigdigits=2) === Float16(0.6)
    @test round(Float16(1.1), sigdigits=70) === Float16(1.1)

    # issue 37171
    @test round(9.87654321e-308, sigdigits = 1) ≈ 1.0e-307
    @test round(9.87654321e-308, sigdigits = 2) ≈ 9.9e-308
    @test round(9.87654321e-308, sigdigits = 3) ≈ 9.88e-308
    @test round(9.87654321e-308, sigdigits = 4) ≈ 9.877e-308
    @test round(9.87654321e-308, sigdigits = 5) ≈ 9.8765e-308
    @test round(9.87654321e-308, sigdigits = 6) ≈ 9.87654e-308
    @test round(9.87654321e-308, sigdigits = 7) ≈ 9.876543e-308
    @test round(9.87654321e-308, sigdigits = 8) ≈ 9.8765432e-308
    @test round(9.87654321e-308, sigdigits = 9) ≈ 9.87654321e-308
    @test round(9.87654321e-308, sigdigits = 10) ≈ 9.87654321e-308
    @test round(9.87654321e-308, sigdigits = 11) ≈ 9.87654321e-308

    @inferred round(Float16(1.), sigdigits=2)
    @inferred round(Float32(1.), sigdigits=2)
    @inferred round(Float64(1.), sigdigits=2)
end

@testset "literal pow matches runtime pow matches optimized pow" begin
    let two = 2
        @test 1.0000000105367122^2 == 1.0000000105367122^two
        @test 1.0041504f0^2 == 1.0041504f0^two
    end

    function g2(start, two, N)
        x = start
        n = 0
        for _ in 1:N
           n += (x^2 !== x^two)
           x = nextfloat(x)
        end
        return n
    end
    @test g2(1.0, 2, 100_000_000) == 0
    @test g2(1.0f0, 2, 100_000_000) == 0
    g2′(start, N) = g2(start, 2, N)
    @test g2′(1.0, 100_000_000) == 0
    @test g2′(1.0f0, 100_000_000) == 0

    function g3(start, three, N)
        x = start
        n = 0
        for _ in 1:N
           n += (x^3 !== x^three)
           x = nextfloat(x)
        end
        return n
    end
    @test g3(1.0, 3, 100_000_000) == 0
    @test g3(1.0f0, 3, 100_000_000) == 0
    g3′(start, N) = g3(start, 3, N)
    @test g3′(1.0, 100_000_000) == 0
    @test g3′(1.0f0, 100_000_000) == 0

    function ginv(start, inv, N)
        x = start
        n = 0
        for _ in 1:N
           n += (x^-1 !== x^inv)
           x = nextfloat(x)
        end
        return n
    end
    @test ginv(1.0, -1, 100_000_000) == 0
    @test ginv(1.0f0, -1, 100_000_000) == 0
    ginv′(start, N) = ginv(start, -1, N)
    @test ginv′(1.0, 100_000_000) == 0
    @test ginv′(1.0f0, 100_000_000) == 0

    f(x, p) = x^p
    finv(x) = f(x, -1)
    f2(x) = f(x, 2)
    f3(x) = f(x, 3)
    let x = 1.0000000105367122
        @test x^2 == f(x, 2) == f2(x) == x*x == Float64(big(x)*big(x))
        @test x^3 == f(x, 3) == f3(x) == x*x*x == Float64(big(x)*big(x)*big(x))
    end
    let x = 1.000000007393669
        @test x^-1 == f(x, -1) == finv(x) == 1/x == inv(x) == Float64(1/big(x)) == Float64(inv(big(x)))
    end
end

@testset "curried approximation" begin

    @test ≈(1.0; atol=1).(1.0:3.0) == [true, true, false]

end

@testset "isnan for Number" begin
    struct CustomNumber <: Number end
    @test !isnan(CustomNumber())
end

@testset "isapprox and integer overflow" begin
    for T in (Int8, Int16, Int32)
        T === Int && continue
        @test !isapprox(typemin(T), T(0))
        @test !isapprox(typemin(T), unsigned(T)(0))
        @test !isapprox(typemin(T), 0)
        @test !isapprox(typemin(T), T(0), atol=0.99)
        @test !isapprox(typemin(T), unsigned(T)(0), atol=0.99)
        @test !isapprox(typemin(T), 0, atol=0.99)
        @test_broken !isapprox(typemin(T), T(0), atol=1)
        @test_broken !isapprox(typemin(T), unsigned(T)(0), atol=1)
        @test !isapprox(typemin(T), 0, atol=1)

        @test !isapprox(typemin(T)+T(10), T(10))
        @test !isapprox(typemin(T)+T(10), unsigned(T)(10))
        @test !isapprox(typemin(T)+T(10), 10)
        @test !isapprox(typemin(T)+T(10), T(10), atol=0.99)
        @test !isapprox(typemin(T)+T(10), unsigned(T)(10), atol=0.99)
        @test !isapprox(typemin(T)+T(10), 10, atol=0.99)
        @test_broken !isapprox(typemin(T)+T(10), T(10), atol=1)
        @test !isapprox(typemin(T)+T(10), unsigned(T)(10), atol=1)
        @test !isapprox(typemin(T)+T(10), 10, atol=1)

        @test isapprox(typemin(T), 0.0, rtol=1)
    end
    for T in (Int, Int64, Int128)
        @test !isapprox(typemin(T), T(0))
        @test !isapprox(typemin(T), unsigned(T)(0))
        @test !isapprox(typemin(T), T(0), atol=0.99)
        @test !isapprox(typemin(T), unsigned(T)(0), atol=0.99)
        @test_broken !isapprox(typemin(T), T(0), atol=1)
        @test_broken !isapprox(typemin(T), unsigned(T)(0), atol=1)

        @test !isapprox(typemin(T)+T(10), T(10))
        @test !isapprox(typemin(T)+T(10), unsigned(T)(10))
        @test !isapprox(typemin(T)+T(10), T(10), atol=0.99)
        @test !isapprox(typemin(T)+T(10), unsigned(T)(10), atol=0.99)
        @test_broken !isapprox(typemin(T)+T(10), T(10), atol=1)
        @test !isapprox(typemin(T)+T(10), unsigned(T)(10), atol=1)

        @test isapprox(typemin(T), 0.0, rtol=1)
    end
end

@testset "isapprox and unsigned integers" begin
    for T in Base.BitUnsigned_types
        # Test also combinations of different integer types
        W = widen(T)
        # The order of the operands for difference between unsigned integers is
        # very important, test both combinations.
        @test isapprox(T(42), T(42); rtol=T(0), atol=0.5)
        @test isapprox(T(42), W(42); rtol=T(0), atol=0.5)
        @test !isapprox(T(0), T(1); rtol=T(0), atol=0.5)
        @test !isapprox(T(1), T(0); rtol=T(0), atol=0.5)
        @test isapprox(T(1), T(3); atol=T(2))
        @test isapprox(T(4), T(2); atol=T(2))
        @test isapprox(T(1), W(3); atol=T(2))
        @test isapprox(T(4), W(2); atol=T(2))
        @test isapprox(T(5), T(7); atol=typemax(T))
        @test isapprox(T(8), T(6); atol=typemax(T))
        @test isapprox(T(1), T(2); rtol=1)
        @test isapprox(T(6), T(3); rtol=1)
        @test isapprox(T(1), W(2); rtol=1)
        @test isapprox(T(6), W(3); rtol=1)
        @test !isapprox(typemin(T), typemax(T))
        @test !isapprox(typemax(T), typemin(T))
        @test !isapprox(typemin(T), typemax(T); atol=typemax(T)-T(1))
        @test !isapprox(typemax(T), typemin(T); atol=typemax(T)-T(1))
        @test isapprox(typemin(T), typemax(T); atol=typemax(T))
        @test isapprox(typemax(T), typemin(T); atol=typemax(T))
    end
end

@testset "Conversion from floating point to unsigned integer near extremes (#51063)" begin
    @test_throws InexactError UInt32(4.2949673f9)
    @test_throws InexactError UInt64(1.8446744f19)
    @test_throws InexactError UInt64(1.8446744073709552e19)
    @test_throws InexactError UInt128(3.402823669209385e38)
end

@testset "Conversion from floating point to integer near extremes (exhaustive)" begin
    for Ti in Base.BitInteger_types, Tf in (Float16, Float32, Float64), x in (typemin(Ti), typemax(Ti))
        y = Tf(x)
        for i in -3:3
            z = nextfloat(y, i)

            result = isfinite(z) ? round(BigInt, z) : error
            result = result !== error && typemin(Ti) <= result <= typemax(Ti) ? result : error

            if result === error
                @test_throws InexactError round(Ti, z)
                @test_throws InexactError Ti(z)
            else
                @test result == round(Ti, z)
                if isinteger(z)
                    @test result == Ti(z)
                else
                    @test_throws InexactError Ti(z)
                end
            end
        end
    end
end
