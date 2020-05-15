# This file is a part of Julia. License is MIT: https://julialang.org/license

using LinearAlgebra

@test reim(2 + 3im) == (2, 3)

import Base: zero, real, checked_mul, checked_abs2

struct TestQuaternion{T} <: Number
    scalar::T
    i_component::T
    j_component::T
    k_component::T
end
zero(::Type{TestQuaternion{T}}) where {T} =
    TestQuaternion(zero(T), zero(T), zero(T), zero(T))
real(quaternion::TestQuaternion) = quaternion.scalar

for T in (Int64, Float64)
    @test real(T) == T
    @test real(Complex{T}) == T
    # TestQuaternions are neither real or complex
    @test real(TestQuaternion{T}) == T
    @test complex(T) == Complex{T}
    @test complex(Complex{T}) == Complex{T}
end

#show
@test sprint(show, complex(1, 0), context=:compact => true) == "1+0im"
@test sprint(show, complex(true, true)) == "Complex(true,true)"
@test sprint(show, Complex{Int8}(0, typemin(Int8))) == "0 - 128im"

@testset "unary operator on complex boolean" begin
    @test +Complex(true, true) === Complex(1, 1)
    @test +Complex(true, false) === Complex(1, 0)
    @test +Complex(false, true) === Complex(0, 1)
    @test +Complex(false, false) === Complex(0, 0)
    @test -Complex(true, true) === Complex(-1, -1)
    @test -Complex(true, false) === Complex(-1, 0)
    @test -Complex(false, true) === Complex(0, -1)
    @test -Complex(false, false) === Complex(0, 0)
end

@testset "arithmetic" begin
    @testset for T in (Float16, Float32, Float64, BigFloat)
        t = true
        f = false

        @testset "add and subtract" begin
            @test isequal(T(+0.0) + im, Complex(T(+0.0), T(+1.0)))
            @test isequal(T(-0.0) + im, Complex(T(-0.0), T(+1.0)))
            @test isequal(T(+0.0) - im, Complex(T(+0.0), T(-1.0)))
            @test isequal(T(-0.0) - im, Complex(T(-0.0), T(-1.0)))
            @test isequal(T(+1.0) + im, Complex(T(+1.0), T(+1.0)))
            @test isequal(T(-1.0) + im, Complex(T(-1.0), T(+1.0)))
            @test isequal(T(+1.0) - im, Complex(T(+1.0), T(-1.0)))
            @test isequal(T(-1.0) - im, Complex(T(-1.0), T(-1.0)))
            @test isequal(im + T(+0.0), Complex(T(+0.0), T(+1.0)))
            @test isequal(im + T(-0.0), Complex(T(-0.0), T(+1.0)))
            @test isequal(im - T(+0.0), Complex(T(+0.0), T(+1.0)))
            @test isequal(im - T(-0.0), Complex(T(+0.0), T(+1.0)))
            @test isequal(im + T(+1.0), Complex(T(+1.0), T(+1.0)))
            @test isequal(im + T(-1.0), Complex(T(-1.0), T(+1.0)))
            @test isequal(im - T(+1.0), Complex(T(-1.0), T(+1.0)))
            @test isequal(im - T(-1.0), Complex(T(+1.0), T(+1.0)))
            @test isequal(T(f) + im, Complex(T(+0.0), T(+1.0)))
            @test isequal(T(t) + im, Complex(T(+1.0), T(+1.0)))
            @test isequal(T(f) - im, Complex(T(+0.0), T(-1.0)))
            @test isequal(T(t) - im, Complex(T(+1.0), T(-1.0)))
            @test isequal(im + T(f), Complex(T(+0.0), T(+1.0)))
            @test isequal(im + T(t), Complex(T(+1.0), T(+1.0)))
            @test isequal(im - T(f), Complex(T(+0.0), T(+1.0)))
            @test isequal(im - T(t), Complex(T(-1.0), T(+1.0)))
        end

        @testset "multiply" begin
            @test isequal(T(+0.0) * im, Complex(T(+0.0), T(+0.0)))
            @test isequal(T(-0.0) * im, Complex(T(-0.0), T(-0.0)))
            @test isequal(T(+1.0) * im, Complex(T(+0.0), T(+1.0)))
            @test isequal(T(-1.0) * im, Complex(T(-0.0), T(-1.0)))
            @test isequal(im * T(+0.0), Complex(T(+0.0), T(+0.0)))
            @test isequal(im * T(-0.0), Complex(T(-0.0), T(-0.0)))
            @test isequal(im * T(+1.0), Complex(T(+0.0), T(+1.0)))
            @test isequal(im * T(-1.0), Complex(T(-0.0), T(-1.0)))
        end

        @testset "divide" begin
            @test isequal(T(+0.0) / im, Complex(T(+0.0), T(-0.0)))
            @test isequal(T(-0.0) / im, Complex(T(-0.0), T(+0.0)))
            @test isequal(T(+1.0) / im, Complex(T(+0.0), T(-1.0)))
            @test isequal(T(-1.0) / im, Complex(T(-0.0), T(+1.0)))
        end
    end
    @test isequal(true + complex(true,false), complex(true,false) + complex(true,false))
    @test isequal(complex(true,false) + true, complex(true,false) + complex(true,false))
    @test isequal(true - complex(true,false), complex(true,false) - complex(true,false))
    @test isequal(complex(true,false) - true, complex(true,false) - complex(true,false))
    @test isequal(true * complex(true,false), complex(true,false) * complex(true,false))
    @test isequal(complex(true,false) * true, complex(true,false) * complex(true,false))

    # This is mainly useful when x and y can have mutable types too.
    # This is similar to === of immutable types, but for mutable types.
    ≟(x,y) = (typeof(x)==typeof(y)) && (x==y)

    # Multiply complex Integers
    @testset for T in (Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, BigInt)
        @test (complex(T(0), T(0)) * complex(T(0), T(1))) ≟ complex(T(0), T(0))
        @test (complex(T(0), T(1)) * complex(T(0), T(0))) ≟ complex(T(0), T(0))
        @test (complex(T(1), T(0)) * complex(T(0), T(1))) ≟ complex(T(0), T(1))
        @test (complex(T(0), T(1)) * complex(T(1), T(0))) ≟ complex(T(0), T(1))
        @test (complex(T(2), T(3)) * complex(T(2), T(0))) ≟ complex(T(4), T(6))
        if T <: Signed
            @test (complex(T(-1), T(0)) * complex(T(0), T(1))) ≟ complex(T(0), T(-1))
            @test (complex(T(0), T(1)) * complex(T(-1), T(0))) ≟ complex(T(0), T(-1))

            @test (complex(T(2), T(3)) * complex(T(-2), T(0))) ≟ complex(T(-4), T(-6))
            @test (complex(T(2), T(3)) * complex(T(0), T(2))) ≟ complex(T(-6), T(4))
            @test (complex(T(2), T(3)) * complex(T(0), T(-2))) ≟ complex(T(6), T(-4))

            @test (complex(T(2), T(3)) * complex(T(2), T(3))) ≟ complex(T(-5), T(12))
            @test (complex(T(2), T(3)) * complex(T(-2), T(3))) ≟ complex(T(-13), T(0))
            @test (complex(T(2), T(3)) * complex(T(2), T(-3))) ≟ complex(T(13), T(0))
            @test (complex(T(2), T(3)) * complex(T(-2), T(-3))) ≟ complex(T(5), T(-12))
        end
    end

    # Multiply complex integers using checked arithmetic (checked_mul)
    @testset for T in (Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, BigInt)
        # Single argument checked_mul method.
        for s in (-1, 0, +1), s2 in (-1, 0, +1)
            if (s>=0 && s2>=0) || T <: Signed
                @test checked_mul(complex(T(3s), T(4s2))) ≟ complex(T(3s), T(4s2))
                if T != BigInt
                    @test checked_mul(complex(T(s*typemax(T)), T(s2*typemax(T)))) ≟ complex(T(s*typemax(T)), T(s2*typemax(T)))
                end
            end
            if T != BigInt
                # -typemin(T) overflows for signed integers. -typemin(T) === typemin(T).
                if T <: Signed
                    @test checked_mul(complex(T(s)*typemax(T), T(s2)*typemin(T))) === complex(T(s)*typemax(T), T(s2)*typemin(T))
                    @test checked_mul(complex(T(s2)*typemin(T), T(s)*typemax(T))) === complex(T(s2)*typemin(T), T(s)*typemax(T))
                    @test checked_mul(complex(T(s)*typemin(T), T(s2)*typemin(T))) === complex(T(s)*typemin(T), T(s2)*typemin(T))
                else # For unsigned integer types, typemin(T) === T(0). Hence -typemin(T) === typemin(T).
                    if s>=0
                        @test checked_mul(complex(T(s*typemax(T)), T(s2*typemin(T)))) === complex(T(s*typemax(T)), T(s2*typemin(T)))
                        @test checked_mul(complex(T(s2*typemin(T)), T(s*typemax(T)))) === complex(T(s2*typemin(T)), T(s*typemax(T)))
                        @test checked_mul(complex(T(s*typemin(T)), T(s2*typemin(T)))) === complex(T(s*typemin(T)), T(s2*typemin(T)))
                    end
                end
            end
        end

        # Two arguments checked_mul method.
        # regular cases
        for s1 in (-1, 0, +1), s2 in (-1, 0, +1), s3 in (-1, 0, +1)
            if (s1>=0 && s2>=0 && s3>=0) || T <: Signed
                @test checked_mul(complex(T(5s1), T(7s3)), complex(T(6s2), T(0))) ≟ complex(T(5s1 * 6s2), T(7s3 * 6s2))
                @test checked_mul(complex(T(5s1), T(0)), complex(T(6s2), T(8s3))) ≟ complex(T(5s1 * 6s2), T(5s1 * 8s3))
            end
            # Special cases, due to subtraction.
            if ((s1==0 || s3==0) && s1>=0 && s2>=0 && s3>=0) || T <: Signed
                @test checked_mul(complex(T(6s2), T(5s1)), complex(T(0), T(7s3))) ≟ complex(T(-5s1 * 7s3), T(6s2 * 7s3))
                @test checked_mul(complex(T(0), T(7s3)), complex(T(6s2), T(5s1))) ≟ complex(T(-7s3 * 5s1), T(7s3 * 6s2))
            end
            for s4 in (-1, 0, +1)
                if ((s3==0 || s4==0) && s1>=0 && s2>=0 && s3>=0 && s4>=0) || T <: Signed
                    @test checked_mul(complex(T(5s1), T(7s3)), complex(T(6s2), T(8s4))) ≟ complex(T(5s1 * 6s2 - 7s3 * 8s4), T(7s3 * 6s2 + 5s1 * 8s4))
                end
            end
        end
        if T <: Unsigned
            # Special case, due to subtraction.
            @test_throws OverflowError checked_mul(complex(T(0), T(1)), complex(T(0), T(1)))
        end

        if T != BigInt
            for s in (-1, 0, +1)
                if s>=0 || T <: Signed
                    @test checked_mul(complex(typemax(T), T(0)), complex(T(1s), T(0))) === complex(T(s*typemax(T)), T(0))
                    @test checked_mul(complex(T(0), typemax(T)), complex(T(1s), T(0))) === complex(T(0), T(s*typemax(T)))
                    @test checked_mul(complex(T(1s), T(0)), complex(typemax(T), T(0))) === complex(T(s*typemax(T)), T(0))
                    @test checked_mul(complex(T(1s), T(0)), complex(T(0), typemax(T))) === complex(T(0), T(s*typemax(T)))

                    @test checked_mul(complex(typemax(T), T(0)), complex(T(0), T(1s))) === complex(T(0), T(s*typemax(T)))
                    @test checked_mul(complex(T(0), T(1s)), complex(typemax(T), T(0))) === complex(T(0), T(s*typemax(T)))
                end
                # Special cases, due to subtraction.
                if s==0 || T <: Signed
                    @test checked_mul(complex(T(0), typemax(T)), complex(T(0), T(1s))) === complex(T(-s*typemax(T)), T(0))
                    @test checked_mul(complex(T(0), T(1s)), complex(T(0), typemax(T))) === complex(T(-s*typemax(T)), T(0))
                end

                if T <: Unsigned
                    if s>=0
                        # typemin(T) === 0 for T <: Unsigned
                        @test checked_mul(complex(typemin(T), T(0)), complex(T(1s), T(0))) === complex(T(s*typemin(T)), T(0))
                        @test checked_mul(complex(T(0), typemin(T)), complex(T(1s), T(0))) === complex(T(0), T(s*typemin(T)))
                        @test checked_mul(complex(T(1s), T(0)), complex(typemin(T), T(0))) === complex(T(s*typemin(T)), T(0))
                        @test checked_mul(complex(T(1s), T(0)), complex(T(0), typemin(T))) === complex(T(0), T(s*typemin(T)))

                        @test checked_mul(complex(typemin(T), T(0)), complex(T(0), T(1s))) === complex(T(0), T(s*typemin(T)))
                        @test checked_mul(complex(T(0), T(1s)), complex(typemin(T), T(0))) === complex(T(0), T(s*typemin(T)))
                    end
                else # T is Signed
                    if s>=0
                        @test checked_mul(complex(typemin(T), T(0)), complex(T(1s), T(0))) === complex(T(s*typemin(T)), T(0))
                        @test checked_mul(complex(T(0), typemin(T)), complex(T(1s), T(0))) === complex(T(0), T(s*typemin(T)))
                        @test checked_mul(complex(T(1s), T(0)), complex(typemin(T), T(0))) === complex(T(s*typemin(T)), T(0))
                        @test checked_mul(complex(T(1s), T(0)), complex(T(0), typemin(T))) === complex(T(0), T(s*typemin(T)))

                        @test checked_mul(complex(typemin(T), T(0)), complex(T(0), T(1s))) === complex(T(0), T(s*typemin(T)))
                        @test checked_mul(complex(T(0), T(1s)), complex(typemin(T), T(0))) === complex(T(0), T(s*typemin(T)))
                    else
                        # -typemin(T) overflows for T <: Signed
                        @test_throws OverflowError checked_mul(complex(typemin(T), T(0)), complex(T(1s), T(0)))
                        @test_throws OverflowError checked_mul(complex(T(0), typemin(T)), complex(T(1s), T(0)))
                        @test_throws OverflowError checked_mul(complex(T(1s), T(0)), complex(typemin(T), T(0)))
                        @test_throws OverflowError checked_mul(complex(T(1s), T(0)), complex(T(0), typemin(T)))

                        @test_throws OverflowError checked_mul(complex(typemin(T), T(0)), complex(T(0), T(1s)))
                        @test_throws OverflowError checked_mul(complex(T(0), T(1s)), complex(typemin(T), T(0)))
                    end
                end
                # Special cases, due to subtraction.
                if T <: Unsigned
                    # typemin(T) === 0 for T <: Unsigned
                    if s >= 0
                        @test checked_mul(complex(T(0), typemin(T)), complex(T(0), T(1s))) === complex(T(-s*typemin(T)), T(0))
                        @test checked_mul(complex(T(0), T(1s)), complex(T(0), typemin(T))) === complex(T(-s*typemin(T)), T(0))
                    end
                else
                    # -typemin(T) overflows for T <: Signed
                    if s == 0
                        @test checked_mul(complex(T(0), typemin(T)), complex(T(0), T(1s))) === complex(T(-s*typemin(T)), T(0))
                        @test checked_mul(complex(T(0), T(1s)), complex(T(0), typemin(T))) === complex(T(-s*typemin(T)), T(0))
                    else
                        @test_throws OverflowError checked_mul(complex(T(0), typemin(T)), complex(T(0), T(1s)))
                        @test_throws OverflowError checked_mul(complex(T(0), T(1s)), complex(T(0), typemin(T)))
                    end
                end
            end

            # corner cases
            sqrtmax = T(1) << T(sizeof(T)*4)
            half_sqrtmax = sqrtmax >> T(1)
            half_sqrtmax_plus1 = half_sqrtmax + T(1)

            @test_throws OverflowError checked_mul(complex(typemax(T), typemax(T)), complex(typemax(T), T(0)))
            @test_throws OverflowError checked_mul(complex(typemax(T), typemax(T)), complex(T(0), typemax(T)))
            @test_throws OverflowError checked_mul(complex(typemax(T), T(0)), complex(typemax(T), typemax(T)))
            @test_throws OverflowError checked_mul(complex(T(0), typemax(T)), complex(typemax(T), typemax(T)))

            @test_throws OverflowError checked_mul(complex(sqrtmax, T(0)), complex(sqrtmax, T(0)))
            @test_throws OverflowError checked_mul(complex(sqrtmax, T(0)), complex(T(0), sqrtmax))
            @test_throws OverflowError checked_mul(complex(T(0), sqrtmax), complex(sqrtmax, T(0)))
            @test_throws OverflowError checked_mul(complex(T(0), sqrtmax), complex(T(0), sqrtmax))

            if T<:Signed
                @test_throws OverflowError checked_mul(complex(sqrtmax, T(0)), complex(half_sqrtmax, T(0)))
                @test_throws OverflowError checked_mul(complex(sqrtmax, T(0)), complex(T(0), half_sqrtmax))
                @test_throws OverflowError checked_mul(complex(T(0), half_sqrtmax), complex(sqrtmax, T(0)))
                @test_throws OverflowError checked_mul(complex(T(0), sqrtmax), complex(T(0), half_sqrtmax))

                @test checked_mul(complex(sqrtmax, T(0)), complex(-half_sqrtmax, T(0))) === complex(T(sqrtmax * -half_sqrtmax), T(0))
                @test checked_mul(complex(sqrtmax, T(0)), complex(T(0), -half_sqrtmax)) === complex(T(0), T(sqrtmax * -half_sqrtmax))
                @test checked_mul(complex(T(0), sqrtmax), complex(-half_sqrtmax, T(0))) === complex(T(0), T(sqrtmax * -half_sqrtmax))
                @test_throws OverflowError checked_mul(complex(T(0), sqrtmax), complex(T(0), -half_sqrtmax))

                @test_throws OverflowError checked_mul(complex(sqrtmax, T(0)), complex(-half_sqrtmax_plus1, T(0)))
                @test_throws OverflowError checked_mul(complex(sqrtmax, T(0)), complex(T(0), -half_sqrtmax_plus1))
                @test_throws OverflowError checked_mul(complex(T(0), -half_sqrtmax_plus1), complex(sqrtmax, T(0)))
                @test_throws OverflowError checked_mul(complex(T(0), sqrtmax), complex(T(0), -half_sqrtmax_plus1))

                @test checked_mul(complex(-sqrtmax, T(0)), complex(half_sqrtmax, T(0))) === complex(T(-sqrtmax * half_sqrtmax), T(0))
                @test checked_mul(complex(-sqrtmax, T(0)), complex(T(0), half_sqrtmax)) === complex(T(0), T(-sqrtmax * half_sqrtmax))
                @test checked_mul(complex(T(0), half_sqrtmax), complex(-sqrtmax, T(0))) === complex(T(0), T(-sqrtmax * half_sqrtmax))
                @test_throws OverflowError checked_mul(complex(T(0), -sqrtmax), complex(T(0), half_sqrtmax))

                @test_throws OverflowError checked_mul(complex(-sqrtmax, T(0)), complex(half_sqrtmax_plus1, T(0)))
                @test_throws OverflowError checked_mul(complex(-sqrtmax, T(0)), complex(T(0), half_sqrtmax_plus1))
                @test_throws OverflowError checked_mul(complex(T(0), half_sqrtmax_plus1), complex(-sqrtmax, T(0)))
                @test_throws OverflowError checked_mul(complex(T(0), -sqrtmax), complex(T(0), half_sqrtmax_plus1))

                @test_throws OverflowError checked_mul(complex(-sqrtmax, T(0)), complex(-half_sqrtmax, T(0)))
                @test_throws OverflowError checked_mul(complex(-sqrtmax, T(0)), complex(T(0), -half_sqrtmax))
                @test_throws OverflowError checked_mul(complex(T(0), -half_sqrtmax), complex(-sqrtmax, T(0)))
                @test_throws OverflowError checked_mul(complex(T(0), -sqrtmax), complex(T(0), half_sqrtmax))
            end
        end
    end

    for s1 in (true, false), s2 in (true, false), s3 in (true, false), s4 in (true, false)
        @test checked_mul(complex(s1, s2), complex(s3, s4)) === complex(s1 * s3 - s2 * s4, s2 * s3 + s1 * s4)
    end

    @testset "Additional tests for checked_mul" begin
        # test promotions
        @test checked_mul(complex(UInt(4), UInt(0)), complex(UInt8(3), UInt8(0))) === complex(UInt(12), UInt(0))
        @test checked_mul(complex(UInt(4), UInt(0)), complex(UInt8(0), UInt8(3))) === complex(UInt(0), UInt(12))
        @test checked_mul(complex(UInt(0), UInt(4)), complex(UInt8(3), UInt8(0))) === complex(UInt(0), UInt(12))
        # Special case, due to subtraction.
        @test_throws OverflowError checked_mul(complex(UInt(0), UInt(4)), complex(UInt8(0), UInt8(3)))

        for T in (UInt8, UInt16, UInt32, UInt64, UInt128)
            @test checked_mul(complex(T(4), T(0)), complex(T(3), T(0))) === complex(T(12), T(0))
            @test checked_mul(complex(T(4), T(0)), complex(T(0), T(3))) === complex(T(0), T(12))
            @test checked_mul(complex(T(0), T(4)), complex(T(3), T(0))) === complex(T(0), T(12))
            # Special case, due to subtraction.
            @test_throws OverflowError checked_mul(complex(T(0), T(4)), complex(T(0), T(3)))

            pow2 = sizeof(T)*8 - 2
            @test_throws OverflowError checked_mul(complex(T(2)^pow2, T(0)), complex(T(2)^2, T(0)))
            @test_throws OverflowError checked_mul(complex(T(2)^pow2, T(0)), complex(T(0), T(2)^2))
            @test_throws OverflowError checked_mul(complex(T(0), T(2)^pow2), complex(T(2)^2, T(0)))
            @test_throws OverflowError checked_mul(complex(T(0), T(2)^pow2), complex(T(0), T(2)^2))

            @test_throws OverflowError checked_mul(complex(T(2)^2, T(0)), complex(T(2)^pow2, T(0)))
            @test_throws OverflowError checked_mul(complex(T(2)^2, T(0)), complex(T(0), T(2)^pow2))
            @test_throws OverflowError checked_mul(complex(T(0), T(2)^2), complex(T(2)^pow2, T(0)))
            @test_throws OverflowError checked_mul(complex(T(0), T(2)^2), complex(T(0), T(2)^pow2))

            pow2 += 1
            @test_throws OverflowError checked_mul(complex(T(2)^pow2, T(0)), complex(T(2), T(0)))
            @test_throws OverflowError checked_mul(complex(T(2)^pow2, T(0)), complex(T(0), T(2)))
            @test_throws OverflowError checked_mul(complex(T(0), T(2)^pow2), complex(T(2), T(0)))
            @test_throws OverflowError checked_mul(complex(T(0), T(2)^pow2), complex(T(0), T(2)))

            @test_throws OverflowError checked_mul(complex(T(2), T(0)), complex(T(2)^pow2, T(0)))
            @test_throws OverflowError checked_mul(complex(T(2), T(0)), complex(T(0), T(2)^pow2))
            @test_throws OverflowError checked_mul(complex(T(0), T(2)), complex(T(2)^pow2, T(0)))
            @test_throws OverflowError checked_mul(complex(T(0), T(2)), complex(T(0), T(2)^pow2))
        end
    end
end

@testset "basic math functions" begin
    # We compare to BigFloat instead of hard-coding
    # values, assuming that BigFloat has an independent and independently
    # tested implementation.
    @testset for T in (Float32, Float64)
        x = Complex{T}(1//3 + 1//4*im)
        y = Complex{T}(1//2 + 1//5*im)
        yi = 4
        @testset "Random values" begin
            @test x^y ≈ big(x)^big(y)
            @test x^yi ≈ big(x)^yi
            @test x^true ≈ big(x)^true
            @test x^false ≈ big(x)^false
            @test x^1 ≈ big(x)^1
            @test abs(x) ≈ abs(big(x))
            @test abs2(x) ≈ abs2(big(x))
            @test acos(x) ≈ acos(big(x))
            @test acosh(1+x) ≈ acosh(1+big(x))
            @test angle(x) ≈ angle(big(x))
            @test asin(x) ≈ asin(big(x))
            @test asinh(x) ≈ asinh(big(x))
            @test atan(x) ≈ atan(big(x))
            @test atanh(x) ≈ atanh(big(x))
            @test cis(real(x)) ≈ cis(real(big(x)))
            @test cis(x) ≈ cis(big(x))
            @test cos(x) ≈ cos(big(x))
            @test cosh(x) ≈ cosh(big(x))
            @test exp(x) ≈ exp(big(x))
            @test exp10(x) ≈ exp10(big(x))
            @test exp2(x) ≈ exp2(big(x))
            @test expm1(x) ≈ expm1(big(x)) atol=eps(T)
            @test log(x) ≈ log(big(x))
            @test log10(x) ≈ log10(big(x))
            @test log1p(x) ≈ log1p(big(x))
            @test log2(x) ≈ log2(big(x))
            @test sin(x) ≈ sin(big(x))
            @test sinh(x) ≈ sinh(big(x))
            @test sqrt(x) ≈ sqrt(big(x))
            @test tan(x) ≈ tan(big(x))
            @test tanh(x) ≈ tanh(big(x))
            @test sec(x) ≈ sec(big(x))
            @test csc(x) ≈ csc(big(x))
            @test secd(x) ≈ secd(big(x))
            @test cscd(x) ≈ cscd(big(x))
            @test sech(x) ≈ sech(big(x))
            @test csch(x) ≈ csch(big(x))
        end
        @testset "Inverses" begin
            @test acos(cos(x)) ≈ x
            @test acosh(cosh(x)) ≈ x
            @test asin(sin(x)) ≈ x
            @test asinh(sinh(x)) ≈ x
            @test atan(tan(x)) ≈ x
            @test atanh(tanh(x)) ≈ x
            @test cos(acos(x)) ≈ x
            @test cosh(acosh(1+x)) ≈ 1+x
            @test exp(log(x)) ≈ x
            @test exp10(log10(x)) ≈ x
            @test exp2(log2(x)) ≈ x
            @test expm1(log1p(x)) ≈ x
            @test log(exp(x)) ≈ x
            @test log10(exp10(x)) ≈ x
            @test log1p(expm1(x)) ≈ x
            @test log2(exp2(x)) ≈ x
            @test sin(asin(x)) ≈ x
            @test sinh(asinh(x)) ≈ x
            @test sqrt(x)^2 ≈ x
            @test sqrt(x^2) ≈ x
            @test tan(atan(x)) ≈ x
            @test tanh(atanh(x)) ≈ x
        end
        @testset "Relations between functions" begin
            @test cosh(x) ≈ (exp(x)+exp(-x))/2
            @test cosh(x)^2-sinh(x)^2 ≈ 1
            @test sin(x)^2+cos(x)^2 ≈ 1
            @test sinh(x) ≈ (exp(x)-exp(-x))/2
            @test tan(x) ≈ sin(x)/cos(x)
            @test tanh(x) ≈ sinh(x)/cosh(x)
            @test sec(x) ≈ inv(cos(x))
            @test csc(x) ≈ inv(sin(x))
            @test secd(x) ≈ inv(cosd(x))
            @test cscd(x) ≈ inv(sind(x))
            @test sech(x) ≈ inv(cosh(x))
            @test csch(x) ≈ inv(sinh(x))
        end
    end
end

@testset "isinf" begin
    @test iszero(real(complex(0.0,1.0))) # isimag deprecated
    @test !iszero(real(complex(1.0,1.0))) # isimag deprecated
    @test isinf(complex(Inf,0))
    @test isinf(complex(-Inf,0))
    @test isinf(complex(0,Inf))
    @test isinf(complex(0,-Inf))
    @test !isinf(complex(0,0))
end

@testset "flipsign" begin
    @test isequal(complex( 0.0, 0.0 ), flipsign(complex( 0.0, 0.0 ), 1))
    @test isequal(complex( -0.0, -0.0 ), flipsign(complex( 0.0, 0.0 ), -1))
    @test isequal(complex( Inf, 0.0 ), flipsign(complex( Inf, 0.0 ), 1))
    @test isequal(complex( -Inf, -0.0 ), flipsign(complex( Inf, 0.0 ), -1))
    @test isequal(complex( 0.0, NaN ), flipsign(complex( 0.0, NaN ), 1.0))
    @test isequal(complex( -0.0, NaN ), flipsign(complex( 0.0, NaN ), -1.0))

    @test isequal(complex( 5.0, 4.0 ), flipsign(complex(-5.0, -4.0), -1))
    @test isequal(complex( 0.5, -0.5 ), flipsign(complex(-0.5, 0.5), -2))
end

@testset "sqrt" begin
    # tests special values from csqrt man page
    # as well as conj(sqrt(z)) = sqrt(conj(z))
    @test isequal(sqrt(complex( 0.0, 0.0)), complex( 0.0, 0.0))
    @test isequal(sqrt(complex( 0.0,-0.0)), complex( 0.0,-0.0))
    @test isequal(sqrt(complex( 0.0, Inf)), complex( Inf, Inf))
    @test isequal(sqrt(complex( 0.0,-Inf)), complex( Inf,-Inf))
    @test isequal(sqrt(complex( 0.0, NaN)), complex( NaN, NaN))
    @test isequal(sqrt(complex(-0.0, 0.0)), complex( 0.0, 0.0))
    @test isequal(sqrt(complex(-0.0,-0.0)), complex( 0.0,-0.0))

    @test isequal(sqrt(complex( 5.0, 0.0)), complex(sqrt(5.0), 0.0))
    @test isequal(sqrt(complex( 5.0,-0.0)), complex(sqrt(5.0),-0.0))
    @test isequal(sqrt(complex(-5.0, 0.0)), complex( 0.0, sqrt(5.0)))
    @test isequal(sqrt(complex(-5.0,-0.0)), complex( 0.0,-sqrt(5.0)))

    @test isequal(sqrt(complex( Inf, 0.0)), complex( Inf, 0.0))
    @test isequal(sqrt(complex( Inf,-0.0)), complex( Inf,-0.0))
    @test isequal(sqrt(complex( Inf, 5.0)), complex( Inf, 0.0))
    @test isequal(sqrt(complex( Inf,-5.0)), complex( Inf,-0.0))
    @test isequal(sqrt(complex( Inf, Inf)), complex( Inf, Inf))
    @test isequal(sqrt(complex( Inf,-Inf)), complex( Inf,-Inf))
    @test isequal(sqrt(complex( Inf, NaN)), complex( Inf, NaN))

    @test isequal(sqrt(complex(-Inf, 0.0)), complex( 0.0, Inf))
    @test isequal(sqrt(complex(-Inf,-0.0)), complex( 0.0,-Inf))
    @test isequal(sqrt(complex(-Inf, 5.0)), complex( 0.0, Inf))
    @test isequal(sqrt(complex(-Inf,-5.0)), complex( 0.0,-Inf))
    @test isequal(sqrt(complex(-Inf, Inf)), complex( Inf, Inf))
    @test isequal(sqrt(complex(-Inf,-Inf)), complex( Inf,-Inf))
    @test isequal(sqrt(complex(-Inf, NaN)), complex( NaN, Inf))

    @test isequal(sqrt(complex( NaN, 0.0)), complex( NaN, NaN))
    @test isequal(sqrt(complex( NaN, 0.0)), complex( NaN, NaN))
    @test isequal(sqrt(complex( NaN, Inf)), complex( Inf, Inf))
    @test isequal(sqrt(complex( NaN,-Inf)), complex( Inf,-Inf))

    # odd binary exponent
    @test isequal(sqrt(complex(1.0e160, 0.0)), complex(1.0e80, 0.0))
end

@testset "log(conj(z)) == conj(log(z))" begin
    @test isequal(log(complex( 0.0, 0.0)), complex(-Inf, 0.0))
    @test isequal(log(complex( 0.0,-0.0)), complex(-Inf,-0.0))
    @test isequal(log(complex( 0.0, 1.0)), complex( 0.0, pi/2))
    @test isequal(log(complex( 0.0,-1.0)), complex( 0.0,-pi/2))
    @test isequal(log(complex( 0.0, Inf)), complex( Inf, pi/2))
    @test isequal(log(complex( 0.0,-Inf)), complex( Inf,-pi/2))
    @test isequal(log(complex( 0.0, NaN)), complex( NaN, NaN))
    @test isequal(log(complex(-0.0, 0.0)), complex(-Inf, pi))
    @test isequal(log(complex(-0.0,-0.0)), complex(-Inf,-pi))

    @test isequal(log(complex( 5.0, 0.0)),complex(log(5.0), 0.0))
    @test isequal(log(complex( 5.0,-0.0)),complex(log(5.0),-0.0))

    @test isequal(log(complex( Inf, 5.0)), complex( Inf, 0.0))
    @test isequal(log(complex( Inf,-5.0)), complex( Inf,-0.0))
    @test isequal(log(complex( Inf, Inf)), complex( Inf, pi/4))
    @test isequal(log(complex( Inf,-Inf)), complex( Inf,-pi/4))
    @test isequal(log(complex( Inf, NaN)), complex( Inf, NaN))

    @test isequal(log(complex(-Inf, 5.0)), complex( Inf, pi))
    @test isequal(log(complex(-Inf,-5.0)), complex( Inf,-pi))
    @test isequal(log(complex(-Inf, Inf)), complex( Inf, 3*pi/4))
    @test isequal(log(complex(-Inf,-Inf)), complex( Inf,-3*pi/4))
    @test isequal(log(complex(-Inf, NaN)), complex( Inf, NaN))

    @test isequal(log(complex( NaN, 0.0)), complex( NaN, NaN))
    @test isequal(log(complex( NaN, Inf)), complex( Inf, NaN))
    @test isequal(log(complex( NaN,-Inf)), complex( Inf, NaN))
    @test isequal(log(complex( NaN, NaN)), complex( NaN, NaN))
end

@testset "exp(conj(z)) == conj(exp(z))" begin
    @test isequal(exp(complex( 0.0, 0.0)), complex(1.0, 0.0))
    @test isequal(exp(complex( 0.0,-0.0)), complex(1.0,-0.0))
    @test isequal(exp(complex( 0.0, Inf)), complex(NaN, NaN))
    @test isequal(exp(complex( 0.0,-Inf)), complex(NaN, NaN))
    @test isequal(exp(complex( 0.0, NaN)), complex(NaN, NaN))

    @test isequal(exp(complex(-0.0, 0.0)), complex(1.0, 0.0))
    @test isequal(exp(complex(-0.0,-0.0)), complex(1.0,-0.0))

    @test isequal(exp(complex( 5.0, Inf)), complex(NaN, NaN))

    @test isequal(exp(complex( Inf, 0.0)), complex(Inf, 0.0))
    @test isequal(exp(complex( Inf,-0.0)), complex(Inf,-0.0))
    @test isequal(exp(complex( Inf, 5.0)), complex(cos(5.0)*Inf,sin(5.0)* Inf))
    @test isequal(exp(complex( Inf,-5.0)), complex(cos(5.0)*Inf,sin(5.0)*-Inf))
    @test isequal(exp(complex( Inf, NaN)), complex(-Inf, NaN))
    @test isequal(exp(complex( Inf, Inf)), complex(-Inf, NaN))
    @test isequal(exp(complex( Inf,-Inf)), complex(-Inf, NaN))

    @test isequal(exp(complex(-Inf, 0.0)), complex(0.0, 0.0))
    @test isequal(exp(complex(-Inf,-0.0)), complex(0.0,-0.0))
    @test isequal(exp(complex(-Inf, 5.0)), complex(cos(5.0)*0.0,sin(5.0)* 0.0))
    @test isequal(exp(complex(-Inf,-5.0)), complex(cos(5.0)*0.0,sin(5.0)*-0.0))
    @test isequal(exp(complex(-Inf, Inf)), complex(-0.0, 0.0))
    @test isequal(exp(complex(-Inf,-Inf)), complex(-0.0,-0.0))
    @test isequal(exp(complex(-Inf, NaN)), complex(-0.0, 0.0))

    @test isequal(exp(complex( NaN, 0.0)), complex( NaN, 0.0))
    @test isequal(exp(complex( NaN,-0.0)), complex( NaN,-0.0))
    @test isequal(exp(complex( NaN, 5.0)), complex( NaN, NaN))
    @test isequal(exp(complex( NaN, NaN)), complex( NaN, NaN))
end

@testset "expm1(conj(z)) == conj(expm1(z))" begin
    @test isequal(expm1(complex( 0.0, 0.0)), complex(0.0, 0.0))
    @test isequal(expm1(complex( 0.0,-0.0)), complex(0.0,-0.0))
    @test isequal(expm1(complex( 0.0, Inf)), complex(NaN, NaN))
    @test isequal(expm1(complex( 0.0,-Inf)), complex(NaN, NaN))
    @test isequal(expm1(complex( 0.0, NaN)), complex(NaN, NaN))

    @test isequal(expm1(complex(-0.0, 0.0)), complex(-0.0, 0.0))
    @test isequal(expm1(complex(-0.0,-0.0)), complex(-0.0,-0.0))

    @test isequal(expm1(complex( 5.0, Inf)), complex(NaN, NaN))

    @test isequal(expm1(complex( Inf, 0.0)), complex(Inf, 0.0))
    @test isequal(expm1(complex( Inf,-0.0)), complex(Inf,-0.0))
    @test isequal(expm1(complex( Inf, 5.0)), complex(cos(5.0)*Inf,sin(5.0)* Inf))
    @test isequal(expm1(complex( Inf,-5.0)), complex(cos(5.0)*Inf,sin(5.0)*-Inf))
    @test isequal(expm1(complex( Inf, NaN)), complex(-Inf, NaN))
    @test isequal(expm1(complex( Inf, Inf)), complex(-Inf, NaN))
    @test isequal(expm1(complex( Inf,-Inf)), complex(-Inf, NaN))

    @test isequal(expm1(complex(-Inf, 0.0)), complex(-1.0, 0.0))
    @test isequal(expm1(complex(-Inf,-0.0)), complex(-1.0,-0.0))
    @test isequal(expm1(complex(-Inf, 5.0)), complex(-1.0,sin(5.0)* 0.0))
    @test isequal(expm1(complex(-Inf,-5.0)), complex(-1.0,sin(5.0)*-0.0))
    @test isequal(expm1(complex(-Inf, Inf)), complex(-1.0, 0.0))
    @test isequal(expm1(complex(-Inf,-Inf)), complex(-1.0,-0.0))
    @test isequal(expm1(complex(-Inf, NaN)), complex(-1.0, 0.0))

    @test isequal(expm1(complex( NaN, 0.0)), complex( NaN, 0.0))
    @test isequal(expm1(complex( NaN,-0.0)), complex( NaN,-0.0))
    @test isequal(expm1(complex( NaN, 5.0)), complex( NaN, NaN))
    @test isequal(expm1(complex( NaN, NaN)), complex( NaN, NaN))

    @test isequal(expm1(complex( 1e-20, 0.0)), complex(expm1( 1e-20), 0.0))
    @test isequal(expm1(complex(-1e-20, 0.0)), complex(expm1(-1e-20), 0.0))

    @test expm1(complex( 1e-20, 1e-10)) ≈ complex( 5e-21, 1e-10)
    @test expm1(complex( 1e-20,-1e-10)) ≈ complex( 5e-21,-1e-10)
    @test expm1(complex(-1e-20, 1e-10)) ≈ complex(-1.5e-20, 1e-10)
    @test expm1(complex(-1e-20,-1e-10)) ≈ complex(-1.5e-20,-1e-10)

    @test expm1(complex( 10.0, 10.0)) ≈ exp(complex( 10.0, 10.0))-1
    @test expm1(complex( 10.0,-10.0)) ≈ exp(complex( 10.0,-10.0))-1
    @test expm1(complex(-10.0, 10.0)) ≈ exp(complex(-10.0, 10.0))-1
    @test expm1(complex(-10.0,-10.0)) ≈ exp(complex(-10.0,-10.0))-1
end

import Base.Math.@horner
@testset "log1p" begin
    @test isequal(log1p(complex(Inf, 3)), complex(Inf, +0.))
    @test isequal(log1p(complex(Inf, -3)), complex(Inf, -0.))
    @test isequal(log1p(complex(-Inf, 3)), complex(Inf, +pi))
    @test isequal(log1p(complex(-Inf, -3)), complex(Inf, -pi))
    @test isequal(log1p(complex(Inf, NaN)), complex(Inf, NaN))
    @test isequal(log1p(complex(NaN, 0)), complex(NaN, NaN))
    @test isequal(log1p(complex(0, NaN)), complex(NaN, NaN))
    @test isequal(log1p(complex(-1, +0.)), complex(-Inf, +0.))
    @test isequal(log1p(complex(-1, -0.)), complex(-Inf, -0.))
    @test isequal(log1p(complex(-2, 1e-10)), log(1 + complex(-2, 1e-10)))
    @test isequal(log1p(complex(1, Inf)), complex(Inf, pi/2))
    @test isequal(log1p(complex(1, -Inf)), complex(Inf, -pi/2))

    for z in (1e-10+1e-9im, 1e-10-1e-9im, -1e-10+1e-9im, -1e-10-1e-9im)
        @test log1p(z) ≈ @horner(z, 0, 1, -0.5, 1/3, -0.25, 0.2)
    end
    for z in (15+4im, 0.2+3im, 0.08-0.9im)
        @test log1p(z) ≈ log(1+z)
    end
end


@testset "^ (cpow)" begin
    # equivalent to exp(y*log(x))
    #   except for 0^0?
    # conj(x)^conj(y) = conj(x^y)
    @test isequal(complex( 0.0, 0.0)^complex( 0.0, 0.0), complex(1.0, 0.0))
    @test isequal(complex( 0.0, 0.0)^complex( 0.0,-0.0), complex(1.0, 0.0))
    @test isequal(complex( 0.0, 0.0)^complex(-0.0, 0.0), complex(1.0,-0.0))
    @test isequal(complex( 0.0, 0.0)^complex(-0.0,-0.0), complex(1.0,-0.0))

    @test isequal(complex( 0.0,-0.0)^complex( 0.0, 0.0), complex(1.0,-0.0))
    @test isequal(complex( 0.0,-0.0)^complex( 0.0,-0.0), complex(1.0,-0.0))
    @test isequal(complex( 0.0,-0.0)^complex(-0.0, 0.0), complex(1.0, 0.0))
    @test isequal(complex( 0.0,-0.0)^complex(-0.0,-0.0), complex(1.0, 0.0))

    @test isequal(complex(-0.0, 0.0)^complex( 0.0, 0.0), complex(1.0, 0.0))
    @test isequal(complex(-0.0, 0.0)^complex( 0.0,-0.0), complex(1.0, 0.0))
    @test isequal(complex(-0.0, 0.0)^complex(-0.0, 0.0), complex(1.0,-0.0))
    @test isequal(complex(-0.0, 0.0)^complex(-0.0,-0.0), complex(1.0,-0.0))

    @test isequal(complex(-0.0,-0.0)^complex( 0.0, 0.0), complex(1.0,-0.0))
    @test isequal(complex(-0.0,-0.0)^complex( 0.0,-0.0), complex(1.0,-0.0))
    @test isequal(complex(-0.0,-0.0)^complex(-0.0, 0.0), complex(1.0, 0.0))
    @test isequal(complex(-0.0,-0.0)^complex(-0.0,-0.0), complex(1.0, 0.0))

    @test complex(0.0,1.0)^complex(2.0,0) ≈ complex(-1.0, 0.0)
    @test complex(1.0,2.0)^complex(3.0,0) ≈ complex(-11.0, -2.0)

    @test isequal(complex(0.0,0.0)^false, complex(1.0,0.0))
    @test isequal(complex(0.0,0.0)^0, complex(1.0,0.0))
end

@testset "sinh and sin" begin
    # sinh: has properties
    #  sinh(conj(z)) = conj(sinh(z))
    #  sinh(-z) = -sinh(z)

    # sin: defined in terms of sinh
    #  sin(z) = -i*sinh(i*z)
    #  i.e. if sinh(a+ib) = x+iy
    #    then  sin(b-ia) = y-ix
    #  sin(conj(z)) = conj(sin(z))
    #  sin(-z) = -sin(z)

    # @test isequal(sin(complex( 0, 10000)),complex( 0.0, Inf))
    # @test isequal(sin(complex( 0,-10000)),complex( 0.0,-Inf))
    for (x,y) in [(complex( 0.0, 0.0), complex( 0.0, 0.0)),
                  (complex( 0.0, Inf), complex( 0.0, NaN)),
                  (complex( 0.0, NaN), complex( 0.0, NaN)),
                  (complex( 7.2, Inf), complex( NaN, NaN)),
                  (complex( 7.2, NaN), complex( NaN, NaN)),
                  (complex( 7.2, 0.0), complex( sinh(7.2), 0.0)),
                  (complex( 1e5, 0.0), complex( sinh(1e5), 0.0)),
                  (complex( Inf, 0.0), complex( Inf, 0.0)),
                  (complex( Inf, 7.2), Inf*cis(7.2)),
                  (complex( Inf, Inf), complex( Inf, NaN)),
                  (complex( Inf, NaN), complex( Inf, NaN)),
                  (complex( NaN, 0.0), complex( NaN, 0.0)),
                  (complex( NaN, 7.2), complex( NaN, NaN)),
                  (complex( NaN, NaN), complex( NaN, NaN)),
                  ]
        @test isequal(sinh(x), y)
        @test isequal(sinh(conj(x)), conj(y))
        @test isequal(sinh(-x), -y)
        @test isequal(sinh(-conj(x)), -conj(y))

        xx = complex(imag(x),-real(x))
        yy = complex(imag(y),-real(y))

        @test isequal(sin(xx),yy)
        @test isequal(sin(conj(xx)), conj(yy))
        @test isequal(sin(-xx), -yy)
        @test isequal(sin(-conj(xx)), -conj(yy))

        yyy = sin(pi*xx)
        @test isequal(sinpi(xx), yyy)
        @test isequal(sinpi(conj(xx)),conj(yyy))
        @test isequal(sinpi(-xx),-yyy)
        @test isequal(sinpi(-conj(xx)),-conj(yyy))
    end
end

@testset "cosh and cos" begin
    # cosh: has properties
    #  cosh(conj(z)) = conj(cosh(z))
    #  coshh(-z) = cosh(z)

    # cos
    #  cos(z) = cosh(iz)
    #   i.e cos(b-ia) = cosh(a+ib)
    #   and cos(b+ia) = cosh(a-ib)
    #  cos(conj(z)) = conj(cos(z))
    #  cos(-z) = cos(z)
    for (x,y) in [(complex( 0.0, 0.0), complex( 1.0, 0.0)),
                  (complex( 0.0, Inf), complex( NaN, 0.0)),
                  (complex( 0.0, NaN), complex( NaN, 0.0)),
                  (complex( 7.2, Inf), complex( NaN, NaN)),
                  (complex( 7.2, NaN), complex( NaN, NaN)),
                  (complex( 7.2, 0.0), complex( cosh(7.2), 0.0)),
                  (complex( 1e5, 0.0), complex( Inf, 0.0)),
                  (complex( Inf, 0.0), complex( Inf, 0.0)),
                  (complex( Inf, 7.2), Inf*cis(7.2)),
                  (complex( Inf, Inf), complex( Inf, NaN)),
                  (complex( Inf, NaN), complex( Inf, NaN)),
                  (complex( NaN, 0.0), complex( NaN, 0.0)),
                  (complex( NaN, 7.2), complex( NaN, NaN)),
                  (complex( NaN, NaN), complex( NaN, NaN)),
                  ]
        undef_sign = isequal(x,complex( NaN, 0.0)) || isequal(x,complex( 0.0, NaN))

        @test isequal(cosh(x), y)
        if !undef_sign
            @test isequal(cosh(conj(x)), conj(y))
            @test isequal(cosh(-x), y)
            @test isequal(cosh(-conj(x)), conj(y))
        end

        xx = complex(imag(x),-real(x))
        yy = y
        @test isequal(cos(xx),yy)
        if !undef_sign
            @test isequal(cos(conj(xx)), conj(yy))
            @test isequal(cos(-xx), yy)
            @test isequal(cos(-conj(xx)), conj(yy))
        end

        yyy = cos(pi*xx)
        @test isequal(cospi(xx), yyy)
        if !undef_sign
            @test isequal(cospi(conj(xx)), conj(yyy))
            @test isequal(cospi(-xx), yyy)
            @test isequal(cospi(-conj(xx)), conj(yyy))
        end
    end
end

@testset "tanh(op(z)) == op(tanh(z)) for op in (conj, -)" begin
    @test isequal(tanh(complex( 0, 0)),complex(0.0,0.0)) #integer fallback
    @test isequal(tanh(complex( 0.0, 0.0)),complex(0.0,0.0))
    @test isequal(tanh(complex( 0.0,-0.0)),complex(0.0,-0.0))
    @test_throws DomainError  tanh(complex( 0.0, Inf))
    @test_throws DomainError  tanh(complex( 0.0,-Inf))
    @test isequal(tanh(complex( 0.0, NaN)),complex(NaN,NaN))

    @test isequal(tanh(complex(-0.0, 0.0)),complex(-0.0,0.0))
    @test isequal(tanh(complex(-0.0,-0.0)),complex(-0.0,-0.0))

    @test_throws DomainError  tanh(complex( 5.0, Inf))
    @test isequal(tanh(complex( 5.0, NaN)),complex(NaN,NaN))

    @test isequal(tanh(complex( Inf, 0.0)),complex(1.0, 0.0))
    @test isequal(tanh(complex( Inf,-0.0)),complex(1.0,-0.0))
    @test isequal(tanh(complex( Inf, 5.0)),complex(1.0,sin(2*5.0)* 0.0))
    @test isequal(tanh(complex( Inf,-5.0)),complex(1.0,sin(2*5.0)*-0.0))
    @test isequal(tanh(complex( Inf, Inf)),complex(1.0, 0.0))
    @test isequal(tanh(complex( Inf,-Inf)),complex(1.0,-0.0))
    @test isequal(tanh(complex( Inf, NaN)),complex(1.0, 0.0))

    @test isequal(tanh(complex(-Inf, 0.0)),complex(-1.0, 0.0))
    @test isequal(tanh(complex(-Inf,-0.0)),complex(-1.0,-0.0))
    @test isequal(tanh(complex(-Inf, 5.0)),complex(-1.0,sin(2*5.0)* 0.0))
    @test isequal(tanh(complex(-Inf,-5.0)),complex(-1.0,sin(2*5.0)*-0.0))
    @test isequal(tanh(complex(-Inf, Inf)),complex(-1.0, 0.0))
    @test isequal(tanh(complex(-Inf,-Inf)),complex(-1.0,-0.0))
    @test isequal(tanh(complex(-Inf, NaN)),complex(-1.0, 0.0))

    @test isequal(tanh(complex( NaN, 0.0)),complex(NaN, 0.0))
    @test isequal(tanh(complex( NaN,-0.0)),complex(NaN,-0.0))
    @test isequal(tanh(complex( NaN, 5.0)),complex(NaN, NaN))
    @test isequal(tanh(complex( NaN,-5.0)),complex(NaN, NaN))
    @test isequal(tanh(complex( NaN, NaN)),complex(NaN, NaN))
end

@testset "tan(z) == -i tanh(iz)" begin
    @test isequal(tan(complex( 0.0, Inf)),complex( 0.0, 1.0))
    @test isequal(tan(complex( 0.0,-Inf)),complex( 0.0,-1.0))
    @test isequal(tan(complex( 0.0, NaN)),complex( 0.0, NaN))
    @test isequal(tan(complex(-0.0,-Inf)),complex(-0.0,-1.0))
    @test isequal(tan(complex(-0.0, Inf)),complex(-0.0, 1.0))
    @test isequal(tan(complex(-0.0, NaN)),complex(-0.0, NaN))

    @test isequal(tan(complex( 5.0, Inf)),complex(sin(2*5.0)* 0.0, 1.0))
    @test isequal(tan(complex( 5.0,-Inf)),complex(sin(2*5.0)* 0.0,-1.0))
    @test isequal(tan(complex( 5.0, NaN)),complex( NaN, NaN))
    @test isequal(tan(complex(-5.0, Inf)),complex(sin(2*5.0)*-0.0, 1.0))
    @test isequal(tan(complex(-5.0,-Inf)),complex(sin(2*5.0)*-0.0,-1.0))
    @test isequal(tan(complex(-5.0, NaN)),complex( NaN, NaN))

    @test_throws DomainError  tan(complex( Inf, 5.0))
    @test isequal(tan(complex( Inf, Inf)),complex( 0.0, 1.0))
    @test isequal(tan(complex( Inf,-Inf)),complex( 0.0,-1.0))
    @test isequal(tan(complex(-Inf, Inf)),complex(-0.0, 1.0))
    @test isequal(tan(complex(-Inf,-Inf)),complex(-0.0,-1.0))

    @test isequal(tan(complex( NaN, 5.0)),complex( NaN, NaN))
    @test isequal(tan(complex( NaN, Inf)),complex( 0.0, 1.0))
    @test isequal(tan(complex( NaN,-Inf)),complex( 0.0,-1.0))
    @test isequal(tan(complex( NaN, NaN)),complex( NaN, NaN))
end

@testset "acosh(conj(z)) == conj(acosh(z))" begin
    @test isequal(acosh(complex( 0.0, 0.0)), complex( 0.0, pi/2))
    @test isequal(acosh(complex( 0.0,-0.0)), complex( 0.0,-pi/2))
    @test isequal(acosh(complex( 0.0, Inf)), complex( Inf, pi/2))
    @test isequal(acosh(complex( 0.0,-Inf)), complex( Inf,-pi/2))
    @test isequal(acosh(complex(-0.0, 0.0)), complex( 0.0, pi/2))
    @test isequal(acosh(complex(-0.0,-0.0)), complex( 0.0,-pi/2))
    @test isequal(acosh(complex( 5.0, Inf)), complex( Inf, pi/2))
    @test isequal(acosh(complex( 5.0,-Inf)), complex( Inf,-pi/2))
    @test isequal(acosh(complex( 5.0, NaN)), complex( NaN, NaN))

    @test isequal(acosh(complex( Inf, 0.0)), complex( Inf, 0.0))
    @test isequal(acosh(complex( Inf,-0.0)), complex( Inf,-0.0))
    @test isequal(acosh(complex( Inf, 5.0)), complex( Inf, 0.0))
    @test isequal(acosh(complex( Inf,-5.0)), complex( Inf,-0.0))
    @test isequal(acosh(complex( Inf, Inf)), complex( Inf, pi/4))
    @test isequal(acosh(complex( Inf,-Inf)), complex( Inf,-pi/4))
    @test isequal(acosh(complex( Inf, NaN)), complex( Inf, NaN))

    @test isequal(acosh(complex(-Inf, 0.0)), complex( Inf, pi))
    @test isequal(acosh(complex(-Inf,-0.0)), complex( Inf,-pi))
    @test isequal(acosh(complex(-Inf, 5.0)), complex( Inf, pi))
    @test isequal(acosh(complex(-Inf,-5.0)), complex( Inf,-pi))
    @test isequal(acosh(complex(-Inf, Inf)), complex( Inf, 3*pi/4))
    @test isequal(acosh(complex(-Inf,-Inf)), complex( Inf,-3*pi/4))
    @test isequal(acosh(complex(-Inf, NaN)), complex( Inf, NaN))

    @test isequal(acosh(complex( NaN, Inf)), complex( Inf, NaN))
    @test isequal(acosh(complex( NaN,-Inf)), complex( Inf, NaN))
    @test isequal(acosh(complex( NaN, NaN)), complex( NaN, NaN))
end

@testset "acos(conj(z)) == conj(acos(z))" begin
    @test isequal(acos(complex( 0, 0)),complex(pi/2,-0.0)) #integer fallback
    @test isequal(acos(complex( 0.0, 0.0)),complex(pi/2,-0.0))
    @test isequal(acos(complex( 0.0,-0.0)),complex(pi/2, 0.0))
    @test isequal(acos(complex( 0.0, Inf)),complex(pi/2,-Inf))
    @test isequal(acos(complex( 0.0,-Inf)),complex(pi/2, Inf))
    @test isequal(acos(complex( 0.0, NaN)),complex(pi/2, NaN))

    @test isequal(acos(complex(-0.0, 0.0)),complex(pi/2,-0.0))
    @test isequal(acos(complex(-0.0,-0.0)),complex(pi/2, 0.0))
    @test isequal(acos(complex(-0.0, NaN)),complex(pi/2, NaN))

    @test isequal(acos(complex( 5.0, Inf)),complex(pi/2,-Inf))
    @test isequal(acos(complex( 5.0,-Inf)),complex(pi/2, Inf))
    @test isequal(acos(complex( 5.0, NaN)),complex( NaN, NaN))

    @test isequal(acos(complex( Inf, 0.0)),complex( 0.0,-Inf))
    @test isequal(acos(complex( Inf,-0.0)),complex( 0.0, Inf))
    @test isequal(acos(complex( Inf, 5.0)),complex( 0.0,-Inf))
    @test isequal(acos(complex( Inf,-5.0)),complex( 0.0, Inf))
    @test isequal(acos(complex( Inf, Inf)),complex(pi/4,-Inf))
    @test isequal(acos(complex( Inf,-Inf)),complex(pi/4, Inf))
    @test isequal(acos(complex( Inf, NaN)),complex( NaN, Inf))

    @test isequal(acos(complex(-Inf, 0.0)),complex(pi,-Inf))
    @test isequal(acos(complex(-Inf,-0.0)),complex(pi, Inf))
    @test isequal(acos(complex(-Inf, 5.0)),complex(pi,-Inf))
    @test isequal(acos(complex(-Inf,-5.0)),complex(pi, Inf))
    @test isequal(acos(complex(-Inf, Inf)),complex(3*pi/4,-Inf))
    @test isequal(acos(complex(-Inf,-Inf)),complex(3*pi/4, Inf))
    @test isequal(acos(complex(-Inf, NaN)),complex( NaN, Inf))

    @test isequal(acos(complex( NaN, 0.0)),complex( NaN, NaN))
    @test isequal(acos(complex( NaN, 5.0)),complex( NaN, NaN))
    @test isequal(acos(complex( NaN, Inf)),complex( NaN,-Inf))
    @test isequal(acos(complex( NaN,-Inf)),complex( NaN, Inf))
    @test isequal(acos(complex( NaN, NaN)),complex( NaN, NaN))
end

@testset "asinh(op(z)) == op(asinh(z)) for op in (conj, -)" begin
    @test isequal(asinh(complex( 0.0, 0.0)),complex( 0.0, 0.0))
    @test isequal(asinh(complex( 0.0,-0.0)),complex( 0.0,-0.0))
    @test isequal(asinh(complex( 0.0, Inf)),complex( Inf, pi/2))
    @test isequal(asinh(complex( 0.0,-Inf)),complex( Inf,-pi/2))
    @test isequal(asinh(complex( 0.0, NaN)),complex( NaN, NaN))

    @test isequal(asinh(complex(-0.0, 0.0)),complex(-0.0, 0.0))
    @test isequal(asinh(complex(-0.0,-0.0)),complex(-0.0,-0.0))
    @test isequal(asinh(complex(-0.0, Inf)),complex(-Inf, pi/2))
    @test isequal(asinh(complex(-0.0,-Inf)),complex(-Inf,-pi/2))

    @test isequal(asinh(complex( 5.0, Inf)),complex( Inf, pi/2))
    @test isequal(asinh(complex( 5.0,-Inf)),complex( Inf,-pi/2))
    @test isequal(asinh(complex( 5.0, NaN)),complex( NaN, NaN))
    @test isequal(asinh(complex(-5.0, Inf)),complex(-Inf, pi/2))
    @test isequal(asinh(complex(-5.0,-Inf)),complex(-Inf,-pi/2))

    @test isequal(asinh(complex( Inf, Inf)),complex( Inf, pi/4))
    @test isequal(asinh(complex( Inf,-Inf)),complex( Inf,-pi/4))
    @test isequal(asinh(complex( Inf, NaN)),complex( Inf, NaN))
    @test isequal(asinh(complex(-Inf, Inf)),complex(-Inf, pi/4))
    @test isequal(asinh(complex(-Inf,-Inf)),complex(-Inf,-pi/4))
    @test isequal(asinh(complex(-Inf, NaN)),complex(-Inf, NaN))

    @test isequal(asinh(complex( NaN, 0.0)),complex( NaN, 0.0))
    @test isequal(asinh(complex( NaN,-0.0)),complex( NaN,-0.0))
    @test isequal(asinh(complex( NaN, 5.0)),complex( NaN, NaN))
    @test isequal(asinh(complex( NaN, Inf)),complex( Inf, NaN))
    @test isequal(asinh(complex( NaN,-Inf)),complex( Inf, NaN))
    @test isequal(asinh(complex( NaN, NaN)),complex( NaN, NaN))
end

@testset "asin(z) == -i*asinh(iz)" begin
    @test isequal(asin(complex( 0.0, 0.0)),complex( 0.0, 0.0))
    @test isequal(asin(complex( 0.0,-0.0)),complex( 0.0,-0.0))
    @test isequal(asin(complex(-0.0, 0.0)),complex(-0.0, 0.0))
    @test isequal(asin(complex( 0.0, NaN)),complex( 0.0, NaN))
    @test isequal(asin(complex(-0.0,-0.0)),complex(-0.0,-0.0))
    @test isequal(asin(complex(-0.0, NaN)),complex(-0.0, NaN))
    @test isequal(asin(complex( 5.0, NaN)),complex( NaN, NaN))

    @test isequal(asin(complex( Inf, 0.0)),complex( pi/2, Inf))
    @test isequal(asin(complex( Inf,-0.0)),complex( pi/2,-Inf))
    @test isequal(asin(complex( Inf, 5.0)),complex( pi/2, Inf))
    @test isequal(asin(complex( Inf,-5.0)),complex( pi/2,-Inf))
    @test isequal(asin(complex( Inf, Inf)),complex( pi/4, Inf))
    @test isequal(asin(complex( Inf,-Inf)),complex( pi/4,-Inf))
    @test isequal(asin(complex( Inf, NaN)),complex( NaN, Inf))

    @test isequal(asin(complex(-Inf, 0.0)),complex(-pi/2, Inf))
    @test isequal(asin(complex(-Inf,-0.0)),complex(-pi/2,-Inf))
    @test isequal(asin(complex(-Inf, 5.0)),complex(-pi/2, Inf))
    @test isequal(asin(complex(-Inf,-5.0)),complex(-pi/2,-Inf))
    @test isequal(asin(complex(-Inf, Inf)),complex(-pi/4, Inf))
    @test isequal(asin(complex(-Inf,-Inf)),complex(-pi/4,-Inf))
    @test isequal(asin(complex(-Inf, NaN)),complex( NaN, Inf))

    @test isequal(asin(complex( NaN, 0.0)),complex( NaN, NaN))
    @test isequal(asin(complex( NaN, 5.0)),complex( NaN, NaN))
    @test isequal(asin(complex( NaN, Inf)),complex( NaN, Inf))
    @test isequal(asin(complex( NaN,-Inf)),complex( NaN,-Inf))
    @test isequal(asin(complex( NaN, NaN)),complex( NaN, NaN))
end

@testset "atanh(op(z)) == op(atanh(z)) for op in (conj, -)" begin
    @test isequal(atanh(complex( 0, 0)),complex( 0.0, 0.0)) #integer fallback
    @test isequal(atanh(complex( 0.0, 0.0)),complex( 0.0, 0.0))
    @test isequal(atanh(complex( 0.0,-0.0)),complex( 0.0,-0.0))
    @test isequal(atanh(complex( 0.0, NaN)),complex( 0.0, NaN))
    @test isequal(atanh(complex( 0.0, Inf)),complex( 0.0, pi/2))
    @test isequal(atanh(complex( 0.0,-Inf)),complex( 0.0,-pi/2))

    @test isequal(atanh(complex(-0.0, NaN)),complex(-0.0, NaN))
    @test isequal(atanh(complex(-0.0, 0.0)),complex(-0.0, 0.0))
    @test isequal(atanh(complex(-0.0,-0.0)),complex(-0.0,-0.0))
    @test isequal(atanh(complex(-0.0, Inf)),complex(-0.0, pi/2))
    @test isequal(atanh(complex(-0.0,-Inf)),complex(-0.0,-pi/2))

    @test isequal(atanh(complex( 1.0, 0.0)),complex( Inf, 0.0))
    @test isequal(atanh(complex( 1.0,-0.0)),complex( Inf,-0.0))
    @test isequal(atanh(complex(-1.0, 0.0)),complex(-Inf, 0.0))
    @test isequal(atanh(complex(-1.0,-0.0)),complex(-Inf,-0.0))
    @test isequal(atanh(complex( 5.0, Inf)),complex( 0.0, pi/2))
    @test isequal(atanh(complex( 5.0,-Inf)),complex( 0.0,-pi/2))
    @test isequal(atanh(complex( 5.0, NaN)),complex( NaN, NaN))
    @test isequal(atanh(complex(-5.0, Inf)),complex(-0.0, pi/2))
    @test isequal(atanh(complex(-5.0,-Inf)),complex(-0.0,-pi/2))
    @test isequal(atanh(complex(-5.0, NaN)),complex( NaN, NaN))

    @test isequal(atanh(complex( Inf, 0.0)),complex(0.0, pi/2))
    @test isequal(atanh(complex( Inf,-0.0)),complex(0.0,-pi/2))
    @test isequal(atanh(complex( Inf, 5.0)),complex(0.0, pi/2))
    @test isequal(atanh(complex( Inf,-5.0)),complex(0.0,-pi/2))
    @test isequal(atanh(complex( Inf, Inf)),complex(0.0, pi/2))
    @test isequal(atanh(complex( Inf,-Inf)),complex(0.0,-pi/2))
    @test isequal(atanh(complex( Inf, NaN)),complex(0.0, NaN))
    # very big but not infinite
    @test isequal(atanh(complex(4e200, NaN)),complex(NaN, NaN))

    @test isequal(atanh(complex(-Inf, 0.0)),complex(-0.0, pi/2))
    @test isequal(atanh(complex(-Inf,-0.0)),complex(-0.0,-pi/2))
    @test isequal(atanh(complex(-Inf, 5.0)),complex(-0.0, pi/2))
    @test isequal(atanh(complex(-Inf,-5.0)),complex(-0.0,-pi/2))
    @test isequal(atanh(complex(-Inf, Inf)),complex(-0.0, pi/2))
    @test isequal(atanh(complex(-Inf,-Inf)),complex(-0.0,-pi/2))
    @test isequal(atanh(complex(-Inf, NaN)),complex(-0.0, NaN))

    @test isequal(atanh(complex( NaN, 0.0)),complex( NaN, NaN))
    @test isequal(atanh(complex( NaN,-0.0)),complex( NaN, NaN))
    @test isequal(atanh(complex( NaN, 5.0)),complex( NaN, NaN))
    @test isequal(atanh(complex( NaN,-5.0)),complex( NaN, NaN))
    @test isequal(atanh(complex( NaN, Inf)),complex( 0.0, pi/2))
    @test isequal(atanh(complex( NaN,-Inf)),complex( 0.0,-pi/2))
    @test isequal(atanh(complex( NaN, NaN)),complex( NaN, NaN))
end

@testset "atan(z) == -i*atanh(iz)" begin
    @test isequal(atan(complex( 0.0, 0.0)),complex( 0.0, 0.0))
    @test isequal(atan(complex( 0.0,-0.0)),complex( 0.0,-0.0))
    @test isequal(atan(complex( 0.0, 1.0)),complex( 0.0, Inf))
    @test isequal(atan(complex( 0.0, Inf)),complex( pi/2, 0.0))
    @test isequal(atan(complex( 0.0,-Inf)),complex( pi/2,-0.0))
    @test isequal(atan(complex( 0.0, NaN)),complex( NaN, NaN))

    @test isequal(atan(complex(-0.0, 0.0)),complex(-0.0, 0.0))
    @test isequal(atan(complex(-0.0,-0.0)),complex(-0.0,-0.0))
    @test isequal(atan(complex(-0.0, Inf)),complex(-pi/2, 0.0))
    @test isequal(atan(complex(-0.0,-Inf)),complex(-pi/2,-0.0))
    @test isequal(atan(complex(-0.0, NaN)),complex( NaN, NaN))

    @test isequal(atan(complex( 5.0, Inf)),complex( pi/2, 0.0))
    @test isequal(atan(complex( 5.0,-Inf)),complex( pi/2,-0.0))
    @test isequal(atan(complex( 5.0, NaN)),complex( NaN, NaN))

    @test isequal(atan(complex(-5.0, Inf)),complex(-pi/2, 0.0))
    @test isequal(atan(complex(-5.0,-Inf)),complex(-pi/2,-0.0))
    @test isequal(atan(complex(-5.0, NaN)),complex( NaN, NaN))

    @test isequal(atan(complex( Inf, 0.0)),complex( pi/2, 0.0))
    @test isequal(atan(complex( Inf,-0.0)),complex( pi/2,-0.0))
    @test isequal(atan(complex( Inf, 5.0)),complex( pi/2, 0.0))
    @test isequal(atan(complex( Inf,-5.0)),complex( pi/2,-0.0))
    @test isequal(atan(complex( Inf, Inf)),complex( pi/2, 0.0))
    @test isequal(atan(complex( Inf,-Inf)),complex( pi/2,-0.0))
    @test isequal(atan(complex( Inf, NaN)),complex( pi/2, 0.0))

    @test isequal(atan(complex(-Inf, 0.0)),complex(-pi/2, 0.0))
    @test isequal(atan(complex(-Inf,-0.0)),complex(-pi/2,-0.0))
    @test isequal(atan(complex(-Inf, 5.0)),complex(-pi/2, 0.0))
    @test isequal(atan(complex(-Inf,-5.0)),complex(-pi/2,-0.0))
    @test isequal(atan(complex(-Inf, Inf)),complex(-pi/2, 0.0))
    @test isequal(atan(complex(-Inf,-Inf)),complex(-pi/2,-0.0))
    @test isequal(atan(complex(-Inf, NaN)),complex(-pi/2, 0.0))

    @test isequal(atan(complex( NaN, 0.0)),complex( NaN, 0.0))
    @test isequal(atan(complex( NaN,-0.0)),complex( NaN,-0.0))
    @test isequal(atan(complex( NaN, 5.0)),complex( NaN, NaN))
    @test isequal(atan(complex( NaN,-5.0)),complex( NaN, NaN))
    @test isequal(atan(complex( NaN, Inf)),complex( NaN, 0.0))
    @test isequal(atan(complex( NaN,-Inf)),complex( NaN,-0.0))
    @test isequal(atan(complex( NaN, NaN)),complex( NaN, NaN))
end

# misc.

@test complex(1//2,1//3)^2 === complex(5//36, 1//3)
@test complex(2,2)^2 === complex(0,8)
let p = -2
    @test_throws DomainError complex(2,2)^p
end
@test complex(2,2)^(-2) === complex(2.0,2.0)^(-2) === complex(0.0, -0.125)

@test complex.(1.0, [1.0, 1.0]) == [complex(1.0, 1.0), complex(1.0, 1.0)]
@test complex.([1.0, 1.0], 1.0) == [complex(1.0, 1.0), complex(1.0, 1.0)]
# robust division of Float64
# hard complex divisions from Fig 6 of arxiv.1210.4539
z7 = Complex{Float64}(3.898125604559113300e289, 8.174961907852353577e295)
z9 = Complex{Float64}(0.001953125, -0.001953125)
z10 = Complex{Float64}( 1.02951151789360578e-84, 6.97145987515076231e-220)
harddivs = ((1.0+im*1.0, 1.0+im*2^1023.0, 2^-1023.0-im*2^-1023.0), #1
      (1.0+im*1.0, 2^-1023.0+im*2^-1023.0, 2^1023.0+im*0.0), #2
      (2^1023.0+im*2^-1023.0, 2^677.0+im*2^-677.0, 2^346.0-im*2^-1008.0), #3
      (2^1023.0+im*2^1023.0, 1.0+im*1.0, 2^1023.0+im*0.0), #4
      (2^1020.0+im*2^-844., 2^656.0+im*2^-780.0, 2^364.0-im*2^-1072.0), #5
      (2^-71.0+im*2^1021., 2^1001.0+im*2^-323.0, 2^-1072.0+im*2^20.0), #6
      (2^-347.0+im*2^-54., 2^-1037.0+im*2^-1058.0, z7), #7
      (2^-1074.0+im*2^-1074., 2^-1073.0+im*2^-1074., 0.6+im*0.2), #8
      (2^1015.0+im*2^-989., 2^1023.0+im*2^1023.0, z9), #9
      (2^-622.0+im*2^-1071., 2^-343.0+im*2^-798.0, z10) #10
      )

# calculate "accurate bits" in range 0:53 by algorithm given in arxiv.1210.4539
function sb_accuracy(x,expected)
    min(logacc(real(x),real(expected)),
        logacc(imag(x),imag(expected)))
end
relacc(x,expected) = abs(x-expected)/abs(expected)
function logacc(x::Float64,expected::Float64)
    x == expected && (return 53)
    expected == 0 && (return 0)
    (x == Inf || x == -Inf) && (return 0)
    isnan(x) && (return 0)
    ra = relacc(BigFloat(x),BigFloat(expected))
    max(floor(Int,-log2(ra)),0)
end
# the robust division algorithm should have 53 or 52
# bits accuracy for each of the hard divisions
@test 52 <= minimum([sb_accuracy(h[1]/h[2],h[3]) for h in harddivs])

# division of non-Float64
function cdiv_test(a,b)
    c=convert(Complex{Float64},a)/convert(Complex{Float64},b)
    50 <= sb_accuracy(c,convert(Complex{Float64},a/b))
end
@test cdiv_test(complex(1//2, 3//4), complex(17//13, 4//5))
@test cdiv_test(complex(1,2), complex(8997,2432))

@testset "inv" begin
    @test inv(1e300+0im) == 1e-300 - 0.0im
    @test inv(0+1e300im) == 0.0 - 1e-300im
end

@testset "issue #7904" begin
    @test log10(10+0im) === 1.0 + 0.0im
    @test log2(2+0im) === 1.0 + 0.0im
end

@testset "sign" begin
    for T in (Float32, Float64)
        z = Complex{T}(1)
        @test typeof(sign(z)) == typeof(z)
        z = Complex{T}(0)
        @test typeof(sign(z)) == typeof(z)
    end
    for T in (Int32, Int64)
        z = Complex{T}(1)
        @test typeof(sign(z)) == typeof(float(z))
        z = Complex{T}(0)
        @test typeof(sign(z)) == typeof(float(z))
    end

    @test sign(0 + 0im) == 0
    @test sign(2 + 0im) == 1
    @test sign(-2 + 0im) == -1
    @test sign(1 + im) ≈ (1 + im) / sqrt(2)
    @test sign(1 - im) ≈ (1 - im) / sqrt(2)

    for T in (Float16, Float32, Float64)
        z = Complex(zero(T), zero(T))
        @test sign(z) === z
        @test sign(-z) === -z
        @test sign(conj(z)) === conj(z)
        @test sign(-conj(z)) === -conj(z)
    end
end

@testset "cis" begin
    @test cis(0.0+1.0im) ≈ 0.367879441171442321595523770161460867445811131031767834507836+0.0im
    @test cis(1.0+0.0im) ≈ 0.54030230586813971740093660744297660373231042061+0.84147098480789650665250232163029899962256306079im
    @test cis(pi) ≈ -1.0+0.0im
    @test cis(pi/2) ≈ 0.0+1.0im
end

@testset "exp2" begin
    @test exp2(0.0+0.0im) == 1.0+0.0im
    @test exp2(1.0+0.0im) == 2.0+0.0im
    #wolframalpha
    @test exp2(1.0+3.0im) ≈ -0.9739888359315627962096198412+1.74681016354974281701922im
    @test exp2(im) ≈ 0.7692389013639721 + 0.6389612763136348im
end

@testset "exp10" begin
    @test exp10(0.0+0.0im) == 1.0+0.0im
    @test exp10(1.0+0.0im) == 10.0+0.0im
    #wolframalpha
    @test exp10(1.0+2.0im) ≈ -1.0701348355877020772086517528518239460495529361-9.9425756941378968736161937190915602112878340717im
    @test exp10(im) ≈ -0.6682015101903132 + 0.7439803369574931im
end

@testset "round and float, PR #8291" begin
    @test round(Complex(1.125, 0.875), digits=2) == Complex(1.12, 0.88)
    @test round(Complex(1.5, 0.5), RoundDown, RoundUp) == Complex(1.0, 1.0)
    @test round.([1:5;] .+ im) == [1:5;] .+ im
    @test round.([1:5;] .+ 0.5im) == [1.0:5.0;]

    @test float(Complex(1, 2)) == Complex(1.0, 2.0)
    @test round(float(Complex(π, ℯ)), digits=3) == Complex(3.142, 2.718)
end

@testset "ComplexF16 arithmetic, PR #10003" begin
    @test Float16(1)+Float16(1)im === ComplexF16(1, 1)
    @test Float16(1)-Float16(1)im === Float16(1)+Float16(-1)im === ComplexF16(1, -1)
    @test Float16(1)*im === ComplexF16(im)
    @test Float16(1)/im === ComplexF16(0,-1)
    @test Float16(1)^im === ComplexF16(1) === Float16(1)+Float16(0)im
end

# issue/PR #10148
@test typeof(Int8(1) - im) == Complex{Int8}

# issue #10926
@test typeof(π - 1im) == Complex{Float64}

@testset "issue #15969" begin
    # specialized muladd for complex types
    for x in (3, 3+13im), y in (2, 2+7im), z in (5, 5+11im)
        @test muladd(x,y,z) === x*y + z
    end
end

@testset "issue #11839" begin
    # type stability for Complex{Int64}
    let x = 1+im
        @inferred sin(x)
        @inferred cos(x)
        @inferred norm(x)
        @inferred opnorm(x)
    end
end

@testset "issue #18785" begin
    # type stability for exp, expm1 for Complex{Int64}
    let x = 2*im
        @inferred exp(x)
        @inferred expm1(x)
    end
end

# issue #19240
@test big(1)/(10+10im) ≈ (5-5im)/big(100) ≈ big"0.05" - big"0.05"*im

@testset "Complex Irrationals, issue #21204" begin
    for x in (pi, ℯ, Base.MathConstants.catalan) # No need to test all of them
        z = Complex(x, x)
        @test typeof(z) == Complex{typeof(x)}
        @test exp(z) ≈ exp(x) * cis(x)
        @test log1p(z) ≈ log(1 + z)
        @test exp2(z) ≈ exp(z * log(2))
        @test exp10(z) ≈ exp(z * log(10))
        @test isequal(z^true, z)
        @test isequal(z^0, complex(1.0,0.0))
    end
end

@testset "expm1 type stability" begin
    x = @inferred expm1(0.1im)
    @test x isa ComplexF64
    x = @inferred expm1(0.1f0im)
    @test x isa ComplexF32
end

@testset "array printing with exponent format" begin
    a = [1.0 + 1e-10im, 2.0e-15 - 2.0e-5im, 1.0e-15 + 2im, 1.0 + 2e-15im]
    @test sprint((io, x) -> show(io, MIME("text/plain"), x), a) ==
        join([
            "4-element Array{Complex{Float64},1}:",
            "     1.0 + 1.0e-10im",
            " 2.0e-15 - 2.0e-5im",
            " 1.0e-15 + 2.0im",
            "     1.0 + 2.0e-15im"], "\n")
end

@testset "corner cases of division, issue #22983" begin
    # These results abide by ISO/IEC 10967-3:2006(E) and
    # mathematical definition of division of complex numbers.
    for T in (Float32, Float64, BigFloat)
        @test isequal(one(T) / zero(Complex{T}), one(Complex{T}) / zero(Complex{T}))
        @test isequal(one(T) / zero(Complex{T}), Complex{T}(NaN, NaN))
        @test isequal(one(Complex{T}) / zero(T), Complex{T}(Inf, NaN))
        @test isequal(one(Complex{T}) / one(Complex{T}), one(Complex{T}))
        @test isequal(one(T) / complex(one(T),  zero(T)), Complex(one(T), -zero(T)))
        @test isequal(one(T) / complex(one(T), -zero(T)), Complex(one(T),  zero(T)))
    end
end

@testset "division by Inf, issue#23134" begin
    @testset "$T" for T in (Float32, Float64, BigFloat)
        @test isequal(one(T) / complex(T(Inf)),         complex(zero(T), -zero(T)))
        @test isequal(one(T) / complex(T(Inf), one(T)), complex(zero(T), -zero(T)))
        @test isequal(one(T) / complex(T(Inf), T(NaN)), complex(zero(T), -zero(T)))
        @test isequal(one(T) / complex(T(Inf), T(Inf)), complex(zero(T), -zero(T)))

        @test isequal(one(T) / complex(T(-Inf)),         complex(-zero(T), -zero(T)))
        @test isequal(one(T) / complex(T(-Inf), one(T)), complex(-zero(T), -zero(T)))
        @test isequal(one(T) / complex(T(-Inf), T(NaN)), complex(-zero(T), -zero(T)))
        @test isequal(one(T) / complex(T(-Inf), T(Inf)), complex(-zero(T), -zero(T)))

        @test isequal(one(T) / complex(T(Inf),-zero(T)), complex(zero(T), zero(T)))
        @test isequal(one(T) / complex(T(Inf),-one(T)),  complex(zero(T), zero(T)))
        @test isequal(one(T) / complex(T(Inf),T(-NaN)),  complex(zero(T), zero(T)))
        @test isequal(one(T) / complex(T(Inf),T(-Inf)),  complex(zero(T), zero(T)))

        @test isequal(one(T) / complex(T(-Inf),-zero(T)),complex(-zero(T), zero(T)))
        @test isequal(one(T) / complex(T(-Inf),-one(T)), complex(-zero(T), zero(T)))
        @test isequal(one(T) / complex(T(-Inf),T(-NaN)), complex(-zero(T), zero(T)))
        @test isequal(one(T) / complex(T(-Inf),T(-Inf)), complex(-zero(T), zero(T)))

        @test isequal(one(T) / complex(zero(T), T(Inf)), complex(zero(T), -zero(T)))
        @test isequal(one(T) / complex(one(T),  T(Inf)), complex(zero(T), -zero(T)))
        @test isequal(one(T) / complex(T(NaN),  T(Inf)), complex(zero(T), -zero(T)))

        @test isequal(one(T) / complex(zero(T), T(-Inf)), complex(zero(T), zero(T)))
        @test isequal(one(T) / complex(one(T),  T(-Inf)), complex(zero(T), zero(T)))
        @test isequal(one(T) / complex(T(NaN),  T(-Inf)), complex(zero(T), zero(T)))

        @test isequal(one(T) / complex(-zero(T), T(Inf)), complex(-zero(T), -zero(T)))
        @test isequal(one(T) / complex(-one(T),  T(Inf)), complex(-zero(T), -zero(T)))
        @test isequal(one(T) / complex(T(-NaN),  T(Inf)), complex(-zero(T), -zero(T)))

        @test isequal(one(T) / complex(-zero(T), T(-Inf)), complex(-zero(T), zero(T)))
        @test isequal(one(T) / complex(-one(T),  T(-Inf)), complex(-zero(T), zero(T)))
        @test isequal(one(T) / complex(T(-NaN),  T(-Inf)), complex(-zero(T), zero(T)))

        # divide complex by complex Inf
        if T == Float64
            @test_broken isequal(complex(one(T)) / complex(T(Inf), T(-Inf)), complex(zero(T), zero(T)))
            @test_broken isequal(complex(one(T)) / complex(T(-Inf), T(Inf)), complex(-zero(T), -zero(T)))
        elseif T == Float32
            @test isequal(complex(one(T)) / complex(T(Inf), T(-Inf)), complex(zero(T), zero(T)))
            @test_broken isequal(complex(one(T)) / complex(T(-Inf), T(Inf)), complex(-zero(T), -zero(T)))
        else
            @test isequal(complex(one(T)) / complex(T(Inf), T(-Inf)), complex(zero(T), zero(T)))
            @test isequal(complex(one(T)) / complex(T(-Inf), T(Inf)), complex(-zero(T), -zero(T)))
        end
    end
end

@testset "complex^real, issue #14342" begin
    for T in (Float32, Float64, BigFloat), p in (T(-21//10), -21//10)
        z = T(2)+0im
        @test real(z^p) ≈ 2^p
        @test signbit(imag(z^p))
    end
    @test (2+0im)^(-21//10) === (2//1+0im)^(-21//10) === 2^-2.1 - 0.0im
end

@testset "more cpow" begin
    # for testing signs of zeros, it is useful to convert ±0.0 to ±1e-15
    zero2small(r::Real) = iszero(r) ? copysign(1e-15, r) : r
    zero2small(z::Complex) = complex(zero2small(real(z)), zero2small(imag(z)))
    ≋(x::Real, y::Real) = x*y == 0 ? abs(x) < 1e-8 && abs(y) < 1e-8 && signbit(x)==signbit(y) : isfinite(x) ? x ≈ y : isequal(x, y)
    ≋(x::Complex, y::Complex) = real(x) ≋ real(y) && imag(x) ≋ imag(y)
    ≟(x,y) = isequal(x,y)

    # test z^p for positive/negative/zero real and imaginary parts of z and p:
    v=(-2.7,-3.0,-2.0,-0.0,+0.0,2.0,3.0,2.7)
    for zr=v, zi=v, pr=v, pi=v
        z = complex(zr,zi)
        p = iszero(pi) ? pr : complex(pr,pi)
        if isinteger(p)
            c = zero2small(z)^Integer(pr)
        else
            c = exp(zero2small(p) * log(zero2small(z)))
        end
        if !iszero(z*p) # z==0 or p==0 is tricky, check it separately
            @test z^p ≋ c
            if isreal(p)
                @test z^(p + 1e-15im) ≈ z^(p - 1e-15im) ≈ c
                if isinteger(p)
                    @test isequal(z^Integer(pr), z^p)
                end
            elseif (zr != 0 || !signbit(zr)) && (zi != 0 || !signbit(zi))
                @test isequal((Complex{Int}(z*10)//10)^p, z^p)
            end
        end
    end

    @test 2 ^ (0.3 + 0.0im) === 2.0 ^ (0.3 + 0.0im) === conj(2.0 ^ (0.3 - 0.0im)) ≋  2.0 ^ (0.3 + 1e-15im)
    @test 0.2 ^ (0.3 + 0.0im) === conj(0.2 ^ (0.3 - 0.0im)) ≋  0.2 ^ (0.3 + 1e-15im)
    @test (0.0 - 0.0im)^2.0 === (0.0 - 0.0im)^2 === (0.0 - 0.0im)^1.1 === (0.0 - 0.0im) ^ (1.1 + 2.3im) === 0.0 - 0.0im
    @test (0.0 - 0.0im)^-2.0 ≟ (0.0 - 0.0im)^-2 ≟ (0.0 - 0.0im)^-1.1 ≟ (0.0 - 0.0im) ^ (-1.1 + 2.3im) ≟ NaN + NaN*im
    @test (1.0+0.0)^(1.2+0.7im) === 1.0 + 0.0im
    @test (-1.0+0.0)^(2.0+0.7im) ≈ exp(-0.7π)
    @test (-4.0+0.0im)^1.5 === (-4.0)^(1.5+0.0im) === (-4)^(1.5+0.0im) === (-4)^(3//2+0im) === 0.0 - 8.0im

    # issue #24515:
    @test (Inf + Inf*im)^2.0 ≟ (Inf + Inf*im)^2 ≟ NaN + Inf*im
    @test (0+0im)^-3.0 ≟ (0+0im)^-3 ≟ NaN + NaN*im
    @test (1.0+0.0im)^1e300 === 1.0 + 0.0im
    @test Inf^(-Inf + 0.0im) == (Inf + 0.0im)^(-Inf - 0.0im) == (Inf - 0.0im)^(-Inf - 0.0im) == (Inf - 0.0im)^-Inf == 0

    # NaN propagation
    @test (0 + NaN*im)^1 ≟ (0 + NaN*im)^1.0 ≟ (0 + NaN*im)^(1.0+0im) ≟ 0.0 + NaN*im
    @test (0 + NaN*im)^2 ≟ (0 + NaN*im)^2.0 ≟ (0 + NaN*im)^(2.0+0im) ≟ NaN + NaN*im
    @test (NaN + 0im)^2.0 ≟ (NaN + 0im)^(2.0+0im) ≟ (2+0im)^NaN ≟ NaN + 0im
    @test (NaN + 0im)^2.5 ≟ NaN^(2.5+0im) ≟ (NaN + NaN*im)^2.5 ≟ (-2+0im)^NaN ≟ (2+0im)^(1+NaN*im) ≟ NaN + NaN*im

    # more Inf cases:
    @test (Inf + 0im)^Inf === Inf^(Inf + 0im) === (Inf + 0im)^(Inf + 0im) == Inf + 0im
    @test (-Inf + 0im)^(0.7 + 0im) === (-Inf + 1im)^(0.7 + 0im) === conj((-Inf - 1im)^(0.7 + 0im)) === -Inf + Inf*im
    @test (-Inf + 0.0im) ^ 3.1 === conj((-Inf - 0.0im) ^ 3.1) === -Inf - Inf*im
    @test (3.0+0.0im)^(Inf + 1im) === (3.0-0.0im)^(Inf + 1im) === conj((3.0+0.0im)^(Inf - 1im)) === Inf + Inf*im

    # The following cases should arguably give Inf + Inf*im, but currently
    # give partial NaNs instead.  Marking as broken for now (since Julia 0.4 at least),
    # in the hope that someday we can fix these corner cases.  (Python gets them wrong too.)
    @test_broken (Inf + 1im)^3 === (Inf + 1im)^3.0 === (Inf + 1im)^(3+0im) === Inf + Inf*im
    @test_broken (Inf + 1im)^3.1 === (Inf + 1im)^(3.1+0im) === Inf + Inf*im

    # cases where phase angle is non-finite yield NaN + NaN*im:
    @test NaN + NaN*im ≟ Inf ^ (2 + 3im) ≟ (Inf + 1im) ^ (2 + 3im) ≟ (Inf*im) ^ (2 + 3im) ≟
          3^(Inf*im) ≟ (-3)^(Inf + 0im) ≟ (-3)^(Inf + 1im) ≟ (3+1im)^Inf ≟
          (3+1im)^(Inf + 1im) ≟ (1e200+1e-200im)^Inf ≟ (1e200+1e-200im)^(Inf+1im)

    @test @inferred(2.0^(3.0+0im)) === @inferred((2.0+0im)^(3.0+0im)) === @inferred((2.0+0im)^3.0) === 8.0+0.0im
end

@testset "issue #31054" begin
    @test tanh(atanh(complex(1.0,1.0))) == complex(1.0,1.0)
    @test tanh(atanh(complex(1.0,-1.0))) == complex(1.0,-1.0)
    @test tanh(atanh(complex(-1.0,1.0))) == complex(-1.0,1.0)
    @test tanh(atanh(complex(-1.0,-1.0))) == complex(-1.0,-1.0)
end

@testset "issue #29840" begin
    @testset "$T" for T in (ComplexF32, ComplexF64, Complex{BigFloat})
        @test isequal(ComplexF64(sec(T(-10, 1000))), ComplexF64(-0.0, 0.0))
        @test isequal(ComplexF64(csc(T(-10, 1000))), ComplexF64(0.0, 0.0))
        @test isequal(ComplexF64(sech(T(1000, 10))), ComplexF64(-0.0, 0.0))
        @test isequal(ComplexF64(csch(T(1000, 10))), ComplexF64(-0.0, 0.0))
        @test isequal(ComplexF64(secd(T(-1000, 100000))), ComplexF64(0.0, 0.0))
        @test isequal(ComplexF64(cscd(T(-1000, 100000))), ComplexF64(0.0, -0.0))
    end
end

# real(C) with C a Complex Unionall
@test real(Complex{<:AbstractFloat}) == AbstractFloat

# complex with non-concrete eltype
@test_throws ErrorException complex(Union{Complex{Int}, Nothing}[])

@testset "GCD for Gaussian Integers" begin
    # This is mainly useful when x and y can have mutable types too.
    # This is similar to === of immutable types, but for mutable types.
    ≟(x, y) = (typeof(x) == typeof(y)) && (x == y)

    for T in (Int8, Int16, Int32, Int64, Int128, BigInt)
        # Special cases
        @test gcd(complex(T(3), T(1)), complex(T(0), T(0))) ≟ complex(T(3), T(1))
        @test gcd(complex(T(0), T(0)), complex(T(3), T(1))) ≟ complex(T(3), T(1))
        @test gcd(complex(T(0), T(0)), complex(T(0), T(0))) ≟ complex(T(0), T(0))

        # Regular cases
        @test gcd(complex(T(3), T(1)), complex(T(1), T(-1))) ≟ complex(T(1), T(1))
        @test gcd(complex(T(5), T(3)), complex(T(2), T(-8))) ≟ complex(T(1), T(1))
        @test gcd(complex(T(3), T(13)), complex(T(4), T(3))) ≟ complex(T(1), T(0))
        @test gcd(complex(T(5), T(1)), complex(T(3), T(5))) ≟ complex(T(1), T(1))
        @test gcd(complex(T(5), T(3)), complex(T(2), T(8))) ≟ complex(T(5), T(3))
        if T != Int8
            @test gcd(complex(T(11), T(7)), complex(T(18), T(-1))) ≟ complex(T(1), T(0))
            @test gcd(complex(T(135), T(-14)), complex(T(155), T(34))) ≟ complex(T(5), T(12))
            @test gcd(complex(T(4), T(22)), complex(T(17), T(1))) ≟ complex(T(1), T(3))
            @test gcd(complex(T(85), T(0)), complex(T(1), T(13))) ≟ complex(T(7), T(6))
            @test gcd(complex(T(0), T(0)), complex(T(1), T(13))) ≟ complex(T(1), T(13))
        end
        @test gcd(complex(T(7), T(1)), complex(T(5), T(3))) ≟ complex(T(1), T(1))
        @test gcd(complex(T(2), T(2)), complex(T(-1), T(1))) ≟ complex(T(1), T(1))

        @test gcd(complex(T(1), T(3)), complex(T(3), T(9))) ≟ complex(T(1), T(3))
        @test gcd(complex(T(1), T(3)), complex(T(-9), T(3))) ≟ complex(T(1), T(3))
        @test gcd(complex(T(1), T(3)), complex(T(-3), T(-9))) ≟ complex(T(1), T(3))
        @test gcd(complex(T(1), T(3)), complex(T(9), T(-3))) ≟ complex(T(1), T(3))
        @test gcd(complex(T(-3), T(-9)), complex(T(1), T(3))) ≟ complex(T(1), T(3))

        @test gcd(complex(T(3), T(0)), complex(T(5), T(0))) ≟ complex(T(1), T(0))
        @test gcd(complex(T(0), T(3)), complex(T(0), T(5))) ≟ complex(T(1), T(0))
        @test gcd(complex(T(-3), T(0)), complex(T(5), T(0))) ≟ complex(T(1), T(0))
        @test gcd(complex(T(0), T(-3)), complex(T(0), T(-5))) ≟ complex(T(1), T(0))

        if T != BigInt
            for s in (T(-1), T(1))
                let x = typemax(T)
                    @test gcd(complex(s*x, s*x), complex(T(1), T(0))) === complex(T(1), T(0))
                    @test gcd(complex(s*x, s*x), complex(T(-1), T(0))) === complex(T(1), T(0))
                    @test gcd(complex(s*x, s*x), complex(T(0), T(1))) === complex(T(1), T(0))
                    @test gcd(complex(s*x, s*x), complex(T(0), T(-1))) === complex(T(1), T(0))

                    # These tests are failing because in div(z1/z2) = z1*conj(z2)/abs2(z2),
                    # abs2(z2) overflows for these z2.
                    @test_broken gcd(complex(T(1), T(0)), complex(s*x, s*x)) === complex(T(1), T(0))
                    @test_broken gcd(complex(T(-1), T(0)), complex(s*x, s*x)) === complex(T(1), T(0))
                    @test_broken gcd(complex(T(0), T(1)), complex(s*x, s*x)) === complex(T(1), T(0))
                    @test_broken gcd(complex(T(0), T(-1)), complex(s*x, s*x)) === complex(T(1), T(0))

                    # These tests are failing because in div(z1/z2) = z1*conj(z2)/abs2(z2),
                    # z1*conj(z2) overflows for these.
                    @test_broken gcd(complex(s*x, T(0)), complex(T(2), T(0))) === complex(T(1), T(0))
                    @test_broken gcd(complex(T(2), T(0)), complex(s*x, T(0))) === complex(T(1), T(0))
                    @test_broken gcd(complex(s*x, s*x), complex(T(2), T(0))) === complex(T(1), T(1))
                    @test_broken gcd(complex(T(2), T(0)), complex(s*x, s*x)) === complex(T(1), T(1))
                end
                let x = typemin(T)
                    @test gcd(complex(s*x, s*x), complex(T(1), T(0))) === complex(T(1), T(0))

                    # These tests are failing because in div(z1/z2) = z1*conj(z2)/abs2(z2),
                    # z1*conj(z2) overflows.
                    @test_broken gcd(complex(s*x, s*x), complex(T(-1), T(0))) === complex(T(1), T(0))
                    @test_broken gcd(complex(s*x, s*x), complex(T(0), T(1))) === complex(T(1), T(0))
                    @test_broken gcd(complex(s*x, s*x), complex(T(0), T(-1))) === complex(T(1), T(0))

                    # These tests are failing because in div(z1/z2) = z1*conj(z2)/abs2(z2),
                    # abs2(z2) overflows for these z2.
                    @test_broken gcd(complex(T(1), T(0)), complex(s*x, s*x)) === complex(T(1), T(0))
                    @test_broken gcd(complex(T(-1), T(0)), complex(s*x, s*x)) === complex(T(1), T(0))
                    @test_broken gcd(complex(T(0), T(1)), complex(s*x, s*x)) === complex(T(1), T(0))
                    @test_broken gcd(complex(T(0), T(-1)), complex(s*x, s*x)) === complex(T(1), T(0))

                    # These tests are failing because in div(z1/z2) = z1*conj(z2)/abs2(z2),
                    # z1*conj(z2) overflows.
                    @test_broken gcd(complex(s*x, T(0)), complex(T(2), T(0))) === complex(T(2), T(0))
                    @test_broken gcd(complex(T(2), T(0)), complex(s*x, T(0))) === complex(T(2), T(0))
                    @test_broken gcd(complex(s*x, s*x), complex(T(2), T(0))) === complex(T(1), T(1))
                    @test_broken gcd(complex(T(2), T(0)), complex(s*x, s*x)) === complex(T(1), T(1))
                end
            end
        end

        # Multi argument gcd method
        @test gcd(complex(T(2), T(0)), complex(T(4), T(0)), complex(T(6), T(0))) ≟ complex(T(2), T(0))
        @test gcd(  complex(T(2), T(0)), complex(T(4), T(0)), complex(T(6), T(0)),
                    complex(T(1), T(0))) ≟ complex(T(1), T(0))
        @test gcd(complex(T(6), T(0)), complex(T(4), T(0)), complex(T(2), T(0))) ≟ complex(T(2), T(0))
        @test gcd(complex(T(1), T(0)), complex(T(4), T(0)), complex(T(6), T(0))) ≟ complex(T(1), T(0))
        @test gcd(complex(T(1), T(0)), complex(T(0), T(1)), complex(T(0), T(-1))) ≟ complex(T(1), T(0))
        @test gcd(complex(T(0), T(-1)), complex(T(-1), T(0)), complex(T(0), T(-1))) ≟ complex(T(1), T(0))
    end

    @test_broken gcd(complex(Int32(10)^5, Int32(10)^5), complex(-Int32(10)^5, -Int32(10)^5)) === complex(Int32(10)^5, Int32(10)^5)
    @test_broken gcd(complex(Int32(10)^5, Int32(0)), complex(Int32(10)^5, Int32(0))) === complex(Int32(10)^5, Int32(0))

    @test_broken gcd(complex(Int64(10)^10, Int64(10)^10), complex(-Int64(10)^10, -Int64(10)^10)) === complex(Int64(10)^10, Int64(10)^10)
    @test_broken gcd(complex(Int64(10)^10, Int64(0)), complex(Int64(10)^10, Int64(0))) === complex(Int64(10)^10, Int64(0))
end

@testset "division and remainder for Gaussian Integers" begin
    # This is mainly useful when x and y can have mutable types too.
    # This is similar to === of immutable types, but for mutable types.
    ≟(x, y) = (typeof(x) == typeof(y)) && (x == y)

    for T in (Int8, Int16, Int32, Int64, Int128, BigInt)
        # Regular cases
        @test divrem(complex(T(3), T(1)), complex(T(1), T(-1))) ≟ (complex(T(1), T(2)), complex(T(0), T(0)))
        @test divrem(complex(T(5), T(3)), complex(T(2), T(-8))) ≟ (complex(T(0), T(1)), complex(T(-3), T(1)))
        @test divrem(complex(T(3), T(13)), complex(T(4), T(3))) ≟ (complex(T(2), T(2)), complex(T(1), T(-1)))
        @test divrem(complex(T(5), T(1)), complex(T(3), T(5))) ≟ (complex(T(1), T(-1)), complex(T(-3), T(-1)))
        @test divrem(complex(T(5), T(3)), complex(T(2), T(8))) ≟ (complex(T(0), T(0)), complex(T(5), T(3)))
        if T != Int8
            @test divrem(complex(T(11), T(7)), complex(T(18), T(-1))) ≟ (complex(T(1), T(0)), complex(T(-7), T(8)))
            @test divrem(complex(T(135), T(-14)), complex(T(155), T(34))) ≟ (complex(T(1), T(0)), complex(T(-20), T(-48)))
            @test divrem(complex(T(4), T(22)), complex(T(17), T(1))) ≟ (complex(T(0), T(1)), complex(T(5), T(5)))
            @test divrem(complex(T(85), T(0)), complex(T(1), T(13))) ≟ (complex(T(0), T(-6)), complex(T(7), T(6)))
        end
        @test divrem(complex(T(7), T(1)), complex(T(5), T(3))) ≟ (complex(T(1), T(0)), complex(T(2), T(-2)))
        @test divrem(complex(T(2), T(2)), complex(T(-1), T(1))) ≟ (complex(T(0), T(-2)), complex(T(0), T(0)))

        @test divrem(complex(T(1), T(3)), complex(T(3), T(9))) ≟ (complex(T(0), T(0)), complex(T(1), T(3)))
        @test divrem(complex(T(1), T(3)), complex(T(-9), T(3))) ≟ (complex(T(0), T(0)), complex(T(1), T(3)))
        @test divrem(complex(T(1), T(3)), complex(T(-3), T(-9))) ≟ (complex(T(0), T(0)), complex(T(1), T(3)))
        @test divrem(complex(T(1), T(3)), complex(T(9), T(-3))) ≟ (complex(T(0), T(0)), complex(T(1), T(3)))
        @test divrem(complex(T(-3), T(-9)), complex(T(1), T(3))) ≟ (complex(T(-3), T(0)), complex(T(0), T(0)))

        # Special cases
        @test divrem(complex(T(0), T(0)), complex(T(5), T(1))) ≟ (complex(T(0), T(0)), complex(T(0), T(0)))
        @test_throws DivideError divrem(complex(T(5), T(1)), complex(T(0), T(0)))
        @test_throws DivideError divrem(complex(T(0), T(0)), complex(T(0), T(0)))

        @test divrem(complex(T(3), T(0)), complex(T(5), T(0))) ≟ (complex(T(1), T(0)), complex(T(-2), T(0)))
        @test divrem(complex(T(0), T(3)), complex(T(0), T(5))) ≟ (complex(T(1), T(0)), complex(T(0), T(-2)))
        @test divrem(complex(T(-3), T(0)), complex(T(5), T(0))) ≟ (complex(T(-1), T(0)), complex(T(2), T(0)))
        @test divrem(complex(T(0), T(-3)), complex(T(0), T(-5))) ≟ (complex(T(1), T(0)), complex(T(0), T(2)))

        if T != BigInt
            for s in (T(-1), T(1))
                let x = typemax(T)
                    @test divrem(complex(s*x, s*x), complex(T(1), T(0))) === (complex(T(s*x), T(s*x)), complex(T(0), T(0)))
                    @test divrem(complex(s*x, s*x), complex(T(-1), T(0))) === (complex(T(-s*x), T(-s*x)), complex(T(0), T(0)))
                    @test divrem(complex(s*x, s*x), complex(T(0), T(1))) === (complex(T(s*x), T(-s*x)), complex(T(0), T(0)))
                    @test divrem(complex(s*x, s*x), complex(T(0), T(-1))) === (complex(T(-s*x), T(s*x)), complex(T(0), T(0)))

                    # These tests are failing because in div(z1/z2) = z1*conj(z2)/abs2(z2),
                    # abs2(z2) overflows for these z2.
                    @test_broken divrem(complex(T(1), T(0)), complex(s*x, s*x)) === (complex(T(0), T(0)), complex(T(1), T(0)))
                    @test_broken divrem(complex(T(-1), T(0)), complex(s*x, s*x)) === (complex(T(0), T(0)), complex(T(-1), T(0)))
                    @test_broken divrem(complex(T(0), T(1)), complex(s*x, s*x)) === (complex(T(0), T(0)), complex(T(0), T(1)))
                    @test_broken divrem(complex(T(0), T(-1)), complex(s*x, s*x)) === (complex(T(0), T(0)), complex(T(0), T(-1)))

                    # These tests are failing because in div(z1/z2) = z1*conj(z2)/abs2(z2),
                    # z1*conj(z2) overflows for these.
                    @test_broken divrem(complex(s*x, T(0)), complex(T(2), T(0))) === (complex(T((s*x)÷2), T(0)), complex(T(s), T(0)))
                    @test_broken divrem(complex(T(2), T(0)), complex(s*x, T(0))) === (complex(T(0), T(0)), complex(T(2), T(0)))
                    @test_broken divrem(complex(s*x, s*x), complex(T(2), T(0))) === (complex(T((s*x)÷2), T((s*x)÷2)), complex(T(s), T(s)))
                    @test_broken divrem(complex(T(2), T(0)), complex(s*x, s*x)) === (complex(T(0), T(0)), complex(T(2), T(0)))
                end
                let x = typemin(T)
                    @test divrem(complex(s*x, s*x), complex(T(1), T(0))) === (complex(T(s*x), T(s*x)), complex(T(0), T(0)))
                    # -typemin(T) overflows for T <: Signed
                    @test_throws OverflowError divrem(complex(s*x, s*x), complex(T(-1), T(0))) === (complex(T(-s*x), T(-s*x)), complex(T(0), T(0)))
                    @test_throws OverflowError divrem(complex(s*x, s*x), complex(T(0), T(1))) === (complex(T(s*x), T(-s*x)), complex(T(0), T(0)))
                    @test_throws OverflowError divrem(complex(s*x, s*x), complex(T(0), T(-1))) === (complex(T(-s*x), T(s*x)), complex(T(0), T(0)))

                    # These tests are failing because in div(z1/z2) = z1*conj(z2)/abs2(z2),
                    # abs2(z2) overflows for these z2.
                    @test_broken divrem(complex(T(1), T(0)), complex(s*x, s*x)) === (complex(T(0), T(0)), complex(T(1), T(0)))
                    @test_broken divrem(complex(T(-1), T(0)), complex(s*x, s*x)) === (complex(T(0), T(0)), complex(T(-1), T(0)))
                    @test_broken divrem(complex(T(0), T(1)), complex(s*x, s*x)) === (complex(T(0), T(0)), complex(T(0), T(1)))
                    @test_broken divrem(complex(T(0), T(-1)), complex(s*x, s*x)) === (complex(T(0), T(0)), complex(T(0), T(-1)))

                    # These tests are failing because in div(z1/z2) = z1*conj(z2)/abs2(z2),
                    # z1*conj(z2) overflows for these.
                    @test_broken divrem(complex(s*x, T(0)), complex(T(2), T(0))) === (complex(T((s*x)÷2), T(0)), complex(T(0), T(0)))
                    @test_broken divrem(complex(s*x, s*x), complex(T(2), T(0))) === (complex(T((s*x)÷2), T((s*x)÷2)), complex(T(0), T(0)))
                    @test_broken divrem(complex(T(2), T(0)), complex(s*x, T(0))) === (complex(T(0), T(0)), complex(T(2), T(0)))
                    @test_broken divrem(complex(T(2), T(0)), complex(s*x, s*x)) === (complex(T(0), T(0)), complex(T(2), T(0)))
                end
            end
        end
    end

    @test_broken divrem(complex(Int32(10)^5, Int32(10)^5), complex(-Int32(10)^5, -Int32(10)^5)) ≟ (complex(Int32(-1), Int32(0)), complex(Int32(0), Int32(0)))
    @test_broken divrem(complex(Int32(10)^5, Int32(0)), complex(Int32(10)^5, Int32(0))) ≟ (complex(Int32(1), Int32(0)), complex(Int32(0), Int32(0)))

    @test_broken divrem(complex(Int64(10)^10, Int64(10)^10), complex(-Int64(10)^10, -Int64(10)^10)) ≟ (complex(Int64(-1), Int64(0)), complex(Int64(0), Int64(0)))
    @test_broken divrem(complex(Int64(10)^10, Int64(0)), complex(Int64(10)^10, Int64(0))) ≟ (complex(Int32(1), Int32(0)), complex(Int32(0), Int32(0)))
end

@testset "abs2 and checked_abs2 for Gaussian integers" begin
    # This is mainly useful when x and y can have mutable types too.
    # This is similar to === of immutable types, but for mutable types.
    ≟(x, y) = (typeof(x) == typeof(y)) && (x == y)

    for T in (Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, BigInt)
        @test abs2(complex(T(0), T(0))) ≟ T(0)
        @test checked_abs2(complex(T(0), T(0))) ≟ T(0)
        for s in (-1, 0, +1), s2 in (-1, 0, +1)
            if s==0 && s2==0
                continue
            end
            if (s>=0 && s2>=0) || T <: Signed
                @test abs2(complex(T(3s), T(4s2))) ≟ T( (s!=0)*T(9) + (s2!=0)*T(16) )
                @test checked_abs2(complex(T(3s), T(4s2))) ≟ T( (s!=0)*T(9) + (s2!=0)*T(16) )
                @test abs2(complex(T(2s), T(5s2))) ≟ T( (s!=0)*T(4) + (s2!=0)*T(25) )
                @test checked_abs2(complex(T(2s), T(5s2))) ≟ T( (s!=0)*T(4) + (s2!=0)*T(25) )
                if T != BigInt
                    @test_throws OverflowError checked_abs2(complex(T(s*typemax(T)), T(s2*typemax(T))))
                end
            end
            if T != BigInt
                if T <: Signed
                    @test_throws OverflowError checked_abs2(complex(T(s)*typemax(T), T(s2)*typemin(T)))
                    @test_throws OverflowError checked_abs2(complex(T(s2)*typemin(T), T(s)*typemax(T)))
                    @test_throws OverflowError checked_abs2(complex(T(s)*typemin(T), T(s2)*typemin(T)))
                else # typemin(T) === T(0) for T <: Unsigned
                    if s>0
                        @test_throws OverflowError checked_abs2(complex(T(s*typemax(T)), T(s2*typemin(T))))
                        @test_throws OverflowError checked_abs2(complex(T(s2*typemin(T)), T(s*typemax(T))))
                    elseif s==0
                        @test checked_abs2(complex(T(0), T(s2*typemin(T)))) === T(0)
                        @test checked_abs2(complex(T(s2*typemin(T)), T(0))) === T(0)
                    end
                    @test checked_abs2(complex(T(s*typemin(T)), T(s2*typemin(T)))) === T(0)
                end
            end
        end
    end
end

@testset "gcdx for Gaussian integers" begin
    # This is mainly useful when x and y can have mutable types too.
    # This is similar to === of immutable types, but for mutable types.
    ≟(x, y) = (typeof(x) == typeof(y)) && (x == y)

    for T in (Int8, Int16, Int32, Int64, Int128, BigInt)
        # Special cases
        @test gcdx(complex(T(3), T(1)), complex(T(0), T(0))) ≟
            (complex(T(3), T(1)), complex(T(1), T(0)), complex(T(0), T(0)))
        @test gcdx(complex(T(0), T(0)), complex(T(3), T(1))) ≟
            (complex(T(3), T(1)), complex(T(0), T(0)), complex(T(1), T(0)))
        @test gcdx(complex(T(0), T(0)), complex(T(0), T(0))) ≟
            (complex(T(0), T(0)), complex(T(1), T(0)), complex(T(0), T(0)))

        # Regular cases
        @test gcdx(complex(T(-25), T(0)), complex(T(-4), T(0))) ≟
            (complex(T(1), T(0)), complex(T(-1), T(0)), complex(T(6), T(0)))

        @test gcdx(complex(T(5), T(1)), complex(T(8), T(1))) ≟
            (complex(T(1), T(0)), complex(T(-5), T(-6)), complex(T(3), T(4)))
        @test gcdx(complex(T(1), T(1)), complex(T(2), T(2))) ≟
            (complex(T(1), T(1)), complex(T(1), T(0)), complex(T(0), T(0)))
        @test gcdx(complex(T(1), T(1)), complex(T(-2), T(2))) ≟
            (complex(T(1), T(1)), complex(T(1), T(0)), complex(T(0), T(0)))
        @test gcdx(complex(T(1), T(1)), complex(T(0), T(0))) ≟
            (complex(T(1), T(1)), complex(T(1), T(0)), complex(T(0), T(0)))

        @test gcdx(complex(T(3), T(1)), complex(T(1), T(-1))) ≟
            (complex(T(1), T(1)), complex(T(0), T(0)), complex(T(0), T(1)))
        @test gcdx(complex(T(5), T(3)), complex(T(2), T(-8))) ≟
            (complex(T(1), T(1)), complex(T(2), T(1)), complex(T(1), T(-1)))
        @test gcdx(complex(T(3), T(13)), complex(T(4), T(3))) ≟
            (complex(T(1), T(0)), complex(T(4), T(0)), complex(T(-8), T(-7)))
        @test gcdx(complex(T(5), T(1)), complex(T(3), T(5))) ≟
            (complex(T(1), T(1)), complex(T(1), T(1)), complex(T(-1), T(0)))
        @test gcdx(complex(T(5), T(3)), complex(T(2), T(8))) ≟
            (complex(T(5), T(3)), complex(T(1), T(0)), complex(T(0), T(0)))
        if T != Int8
            @test gcdx(complex(T(5), T(0)), complex(T(12), T(0))) ≟
                (complex(T(1), T(0)), complex(T(5), T(0)), complex(T(-2), T(0)))
            @test gcdx(complex(T(5), T(0)), complex(T(-12), T(0))) ≟
                (complex(T(1), T(0)), complex(T(5), T(0)), complex(T(2), T(0)))

            @test gcdx(complex(T(11), T(7)), complex(T(18), T(-1))) ≟
                (complex(T(1), T(0)), complex(T(-11), T(13)), complex(T(12), T(-3)))
            @test gcdx(complex(T(135), T(-14)), complex(T(155), T(34))) ≟
                (complex(T(5), T(12)), complex(T(3), T(6)), complex(T(-4), T(-4)))
            @test gcdx(complex(T(4), T(22)), complex(T(17), T(1))) ≟
                (complex(T(1), T(3)), complex(T(2), T(2)), complex(T(2), T(-3)))
            @test gcdx(complex(T(85), T(0)), complex(T(1), T(13))) ≟
                (complex(T(7), T(6)), complex(T(1), T(0)), complex(T(0), T(6)))
            @test gcdx(complex(T(0), T(0)), complex(T(1), T(13))) ≟
                (complex(T(1), T(13)), complex(T(0), T(0)), complex(T(1), T(0)))
        end
        @test gcdx(complex(T(7), T(1)), complex(T(5), T(3))) ≟
            (complex(T(1), T(1)), complex(T(2), T(0)), complex(T(-2), T(1)))
        @test gcdx(complex(T(2), T(2)), complex(T(-1), T(1))) ≟
            (complex(T(1), T(1)), complex(T(0), T(0)), complex(T(0), T(-1)))

        @test gcdx(complex(T(1), T(3)), complex(T(3), T(9))) ≟
            (complex(T(1), T(3)), complex(T(1), T(0)), complex(T(0), T(0)))
        @test gcdx(complex(T(1), T(3)), complex(T(-9), T(3))) ≟
            (complex(T(1), T(3)), complex(T(1), T(0)), complex(T(0), T(0)))
        @test gcdx(complex(T(1), T(3)), complex(T(-3), T(-9))) ≟
            (complex(T(1), T(3)), complex(T(1), T(0)), complex(T(0), T(0)))
        @test gcdx(complex(T(1), T(3)), complex(T(9), T(-3))) ≟
            (complex(T(1), T(3)), complex(T(1), T(0)), complex(T(0), T(0)))
        @test gcdx(complex(T(-3), T(-9)), complex(T(1), T(3))) ≟
            (complex(T(1), T(3)), complex(T(0), T(0)), complex(T(1), T(0)))

        @test gcdx(complex(T(3), T(0)), complex(T(5), T(0))) ≟
            (complex(T(1), T(0)), complex(T(2), T(0)), complex(T(-1), T(0)))
        @test gcdx(complex(T(0), T(3)), complex(T(0), T(5))) ≟
            (complex(T(1), T(0)), complex(T(0), T(-2)), complex(T(0), T(1)))
        @test gcdx(complex(T(-3), T(0)), complex(T(5), T(0))) ≟
            (complex(T(1), T(0)), complex(T(-2), T(0)), complex(T(-1), T(0)))
        @test gcdx(complex(T(0), T(-3)), complex(T(0), T(-5))) ≟
            (complex(T(1), T(0)), complex(T(0), T(2)), complex(T(0), T(-1)))

        if T != BigInt
            for s in (T(-1), T(1))
                let x = typemax(T)
                    @test gcdx(complex(s*x, s*x), complex(T(1), T(0))) === (complex(T(1), T(0)), complex(T(0), T(0)), complex(T(1), T(0)))
                    @test gcdx(complex(s*x, s*x), complex(T(-1), T(0))) === (complex(T(1), T(0)), complex(T(0), T(0)), complex(T(-1), T(0)))
                    @test gcdx(complex(s*x, s*x), complex(T(0), T(1))) === (complex(T(1), T(0)), complex(T(0), T(0)), complex(T(0), T(-1)))
                    @test gcdx(complex(s*x, s*x), complex(T(0), T(-1))) === (complex(T(1), T(0)), complex(T(0), T(0)), complex(T(0), T(1)))

                    # These tests are failing because in div(z1/z2) = z1*conj(z2)/abs2(z2),
                    # abs2(z2) overflows for these z2.
                    @test_broken gcdx(complex(T(1), T(0)), complex(s*x, s*x)) === (complex(T(1), T(0)), complex(T(1), T(0)), complex(T(0), T(0)))
                    @test_broken gcdx(complex(T(-1), T(0)), complex(s*x, s*x)) === (complex(T(1), T(0)), complex(T(-1), T(0)), complex(T(0), T(0)))
                    @test_broken gcdx(complex(T(0), T(1)), complex(s*x, s*x)) === (complex(T(1), T(0)), complex(T(0), T(-1)), complex(T(0), T(0)))
                    @test_broken gcdx(complex(T(0), T(-1)), complex(s*x, s*x)) === (complex(T(1), T(0)), complex(T(0), T(1)), complex(T(0), T(0)))

                    # These tests are failing because in div(z1/z2) = z1*conj(z2)/abs2(z2),
                    # z1*conj(z2) overflows for these.
                    @test_broken gcdx(complex(s*x, T(0)), complex(T(2), T(0))) === (complex(T(1), T(0)), complex(T(1), T(0)), complex(T(-x÷2), T(0)))
                    @test_broken gcdx(complex(T(2), T(0)), complex(s*x, T(0))) === (complex(T(1), T(0)), complex(T(-x÷2), T(0)), complex(T(1), T(0)))
                    @test_broken gcdx(complex(s*x, s*x), complex(T(2), T(0))) === (complex(T(1), T(1)), complex(T(1), T(0)), complex(T(-x÷2), T(-x÷2)))
                    @test_broken gcdx(complex(T(2), T(0)), complex(s*x, s*x)) === (complex(T(1), T(1)), complex(T(-x÷2), T(-x÷2)), complex(T(1), T(0)))
                end
                let x = typemin(T)
                    @test gcdx(complex(s*x, s*x), complex(T(1), T(0))) === (complex(T(1), T(0)), complex(T(0), T(0)), complex(T(1), T(0)))

                    # These tests are failing because in div(z1/z2) = z1*conj(z2)/abs2(z2),
                    # z1*conj(z2) overflows.
                    @test_broken gcdx(complex(s*x, s*x), complex(T(-1), T(0))) === (complex(T(1), T(0)), complex(T(0), T(0)), complex(T(-1), T(0)))
                    @test_broken gcdx(complex(s*x, s*x), complex(T(0), T(1))) === (complex(T(1), T(0)), complex(T(0), T(0)), complex(T(0), T(-1)))
                    @test_broken gcdx(complex(s*x, s*x), complex(T(0), T(-1))) === (complex(T(1), T(0)), complex(T(0), T(0)), complex(T(0), T(1)))

                    # These tests are failing because in div(z1/z2) = z1*conj(z2)/abs2(z2),
                    # abs2(z2) overflows for these z2.
                    @test_broken gcdx(complex(T(1), T(0)), complex(s*x, s*x)) === (complex(T(1), T(0)), complex(T(1), T(0)), complex(T(0), T(0)))
                    @test_broken gcdx(complex(T(-1), T(0)), complex(s*x, s*x)) === (complex(T(1), T(0)), complex(T(-1), T(0)), complex(T(0), T(0)))
                    @test_broken gcdx(complex(T(0), T(1)), complex(s*x, s*x)) === (complex(T(1), T(0)), complex(T(0), T(-1)), complex(T(0), T(0)))
                    @test_broken gcdx(complex(T(0), T(-1)), complex(s*x, s*x)) === (complex(T(1), T(0)), complex(T(0), T(1)), complex(T(0), T(0)))

                    # These tests are failing because in div(z1/z2) = z1*conj(z2)/abs2(z2),
                    # z1*conj(z2) overflows.
                    @test_broken gcdx(complex(s*x, T(0)), complex(T(2), T(0))) === (complex(T(2), T(0)), complex(T(0), T(0)), complex(T(1), T(0)))
                    @test_broken gcdx(complex(T(2), T(0)), complex(s*x, T(0))) === (complex(T(2), T(0)), complex(T(1), T(0)), complex(T(0), T(0)))
                    @test_broken gcdx(complex(s*x, s*x), complex(T(2), T(0))) === (complex(T(2), T(0)), complex(T(0), T(0)), complex(T(1), T(0)))
                    @test_broken gcdx(complex(T(2), T(0)), complex(s*x, s*x)) === (complex(T(2), T(0)), complex(T(1), T(0)), complex(T(0), T(0)))
                end
            end
        end
    end

    @test_broken gcdx(complex(Int32(10)^5, Int32(10)^5), complex(-Int32(10)^5, -Int32(10)^5)) === (complex(Int32(10)^5, Int32(10)^5), complex(Int32(0), Int32(0)), complex(Int32(-1), Int32(0)))
    @test_broken gcdx(complex(Int32(10)^5, Int32(0)), complex(Int32(10)^5, Int32(0))) === (complex(Int32(10)^5, Int32(0)), complex(Int32(0), Int32(0)), complex(Int32(1), Int32(0)))

    @test_broken gcdx(complex(Int64(10)^10, Int64(10)^10), complex(-Int64(10)^10, -Int64(10)^10)) === (complex(Int64(10)^10, Int64(10)^10), complex(Int64(0), Int64(0)), complex(Int64(-1), Int64(0)))
    @test_broken gcdx(complex(Int64(10)^10, Int64(0)), complex(Int64(10)^10, Int64(0))) === (complex(Int64(10)^10, Int64(0)), complex(Int64(0), Int64(0)), complex(Int64(1), Int64(0)))
end