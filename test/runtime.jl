# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: Runtime

@testset "truncdfhf2" begin
    test_truncdfhf2(a, expected) =
        @test Runtime.truncdfhf2(Float64(a)) === reinterpret(UInt16, expected)
    # NaN
    test_truncdfhf2(NaN, NaN16)
    # inf
    test_truncdfhf2(Inf, Inf16)
    test_truncdfhf2(-Inf, -Inf16)
    # zero
    test_truncdfhf2(0.0, 0x0000)
    test_truncdfhf2(-0.0, 0x8000)
    test_truncdfhf2(3.1415926535, 0x4248)
    test_truncdfhf2(-3.1415926535, 0xc248)
    test_truncdfhf2(0x1.987124876876324p+1000, 0x7c00)
    test_truncdfhf2(0x1.987124876876324p+12, 0x6e62)
    test_truncdfhf2(0x1.0p+0, 0x3c00)
    test_truncdfhf2(0x1.0p-14, 0x0400)
    # denormal
    test_truncdfhf2(0x1.0p-20, 0x0010)
    test_truncdfhf2(0x1.0p-24, 0x0001)
    test_truncdfhf2(-0x1.0p-24, 0x8001)
    test_truncdfhf2(0x1.5p-25, 0x0001)
    # and back to zero
    test_truncdfhf2(0x1.0p-25, 0x0000)
    test_truncdfhf2(-0x1.0p-25, 0x8000)
    # max (precise)
    test_truncdfhf2(65504.0, 0x7bff)
    # max (rounded)
    test_truncdfhf2(65519.0, 0x7bff)
    # max (to +inf)
    test_truncdfhf2(65520.0, 0x7c00)
    test_truncdfhf2(-65520.0, 0xfc00)
    test_truncdfhf2(65536.0, 0x7c00)
end

@testset "truncsfhf2" begin
    test_truncsfhf2(a, expected) =
        @test Runtime.truncsfhf2(Float32(a)) === reinterpret(UInt16, expected)
    # NaN
    test_truncsfhf2(NaN32, NaN16)
    # inf
    test_truncsfhf2(Inf32, Inf16)
    test_truncsfhf2(-Inf32, -Inf16)
    # zero
    test_truncsfhf2(0.0f0, 0x0000)
    test_truncsfhf2(-0.0f0, 0x8000)
    test_truncsfhf2(3.1415926535f0, 0x4248)
    test_truncsfhf2(-3.1415926535f0, 0xc248)
    test_truncsfhf2(0x1.987124876876324p+100, 0x7c00)
    test_truncsfhf2(0x1.987124876876324p+12, 0x6e62)
    test_truncsfhf2(0x1.0p+0, 0x3c00)
    test_truncsfhf2(0x1.0p-14, 0x0400)
    # denormal
    test_truncsfhf2(0x1.0p-20, 0x0010)
    test_truncsfhf2(0x1.0p-24, 0x0001)
    test_truncsfhf2(-0x1.0p-24, 0x8001)
    test_truncsfhf2(0x1.5p-25, 0x0001)
    # and back to zero
    test_truncsfhf2(0x1.0p-25, 0x0000)
    test_truncsfhf2(-0x1.0p-25, 0x8000)
    # max (precise)
    test_truncsfhf2(65504.0f0, 0x7bff)
    # max (rounded)
    test_truncsfhf2(65519.0f0, 0x7bff)
    # max (to +inf)
    test_truncsfhf2(65520.0f0, 0x7c00)
    test_truncsfhf2(65536.0f0, 0x7c00)
    test_truncsfhf2(-65520.0f0, 0xfc00)
end

@testset "extendhfsf2" begin
    function test_extendhfsf2(a::UInt16, expected)
        b = Runtime.extendhfsf2(a)
        b16 = Float16(b)
        expected16 = Float16(expected)
        @test reinterpret(UInt16, b16) == reinterpret(UInt16, expected16)
    end
    # NaN
    test_extendhfsf2(0x7e00, NaN32)
    # inf
    test_extendhfsf2(0x7c00, Inf32)
    test_extendhfsf2(0xfc00, -Inf32)
    # zero
    test_extendhfsf2(0x0000, 0.0f0)
    test_extendhfsf2(0x8000, -0.0f0)
    test_extendhfsf2(0x4248, π)
    test_extendhfsf2(0xc248, -π)
    test_extendhfsf2(0x7c00, 0x1.987124876876324p+100)
    test_extendhfsf2(0x6e62, 0x1.988p+12)
    test_extendhfsf2(0x3c00, 0x1.0p+0)
    test_extendhfsf2(0x0400, 0x1.0p-14)
    # denormal
    test_extendhfsf2(0x0010, 0x1.0p-20)
    test_extendhfsf2(0x0001, 0x1.0p-24)
    test_extendhfsf2(0x8001, -0x1.0p-24)
    test_extendhfsf2(0x0001, 0x1.5p-25)
    # and back to zero
    test_extendhfsf2(0x0000, 0x1.0p-25)
    test_extendhfsf2(0x8000, -0x1.0p-25)
    # max (precise)
    test_extendhfsf2(0x7bff, 65504.0f0)
    # max (rounded)
    test_extendhfsf2(0x7bff, 65504.0f0)
end

@testset "extendhfdf2" begin
    function test_extendhfdf2(a::UInt16, expected)
        b = Runtime.extendhfdf2(a)
        b16 = Float16(reinterpret(Float64, b))
        expected16 = Float16(expected)
        @test reinterpret(UInt16, b16) == reinterpret(UInt16, expected16)
    end
    # NaN
    test_extendhfdf2(0x7e00, NaN64)
    # inf
    test_extendhfdf2(0x7c00, Inf64)
    test_extendhfdf2(0xfc00, -Inf64)
    # zero
    test_extendhfdf2(0x0000, 0.0)
    test_extendhfdf2(0x8000, -0.0)
    test_extendhfdf2(0x4248, π)
    test_extendhfdf2(0xc248, -π)
    test_extendhfdf2(0x7c00, 0x1.987124876876324p+100)
    test_extendhfdf2(0x6e62, 0x1.988p+12)
    test_extendhfdf2(0x3c00, 0x1.0p+0)
    test_extendhfdf2(0x0400, 0x1.0p-14)
    # denormal
    test_extendhfdf2(0x0010, 0x1.0p-20)
    test_extendhfdf2(0x0001, 0x1.0p-24)
    test_extendhfdf2(0x8001, -0x1.0p-24)
    test_extendhfdf2(0x0001, 0x1.5p-25)
    # and back to zero
    test_extendhfdf2(0x0000, 0x1.0p-25)
    test_extendhfdf2(0x8000, -0x1.0p-25)
    # max (precise)
    test_extendhfdf2(0x7bff, 65504.0)
    # max (rounded)
    test_extendhfdf2(0x7bff, 65504.0)
end
