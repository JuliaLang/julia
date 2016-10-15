# This file is part of Julia. License is MIT: http://julialang.org/license
# Parts of it are copied from llvm's compiler-rt
#
#                     The LLVM Compiler Infrastructure
#
# This file is dual licensed under the MIT and the University of Illinois Open
# Source Licenses. See LICENSE.TXT for details.

using Base.Test
import Base.RTLIB
@testset "RTLIB" begin
@testset "truncdfhf2" begin
    @test RTLIB.truncdfhf2(NaN) === NaN16
    @test RTLIB.truncdfhf2(Inf) === Inf16
    @test RTLIB.truncdfhf2(-Inf) === -Inf16
    @test RTLIB.truncdfhf2(0.0) === reinterpret(Float16, 0x0000)
    @test RTLIB.truncdfhf2(-0.0) === reinterpret(Float16, 0x8000)
    @test RTLIB.truncdfhf2(3.1415926535) === reinterpret(Float16, 0x4248)
    @test RTLIB.truncdfhf2(-3.1415926535) === reinterpret(Float16, 0xc248)
    @test RTLIB.truncdfhf2(0x1.987124876876324p+1000) === reinterpret(Float16, 0x7c00)
    @test RTLIB.truncdfhf2(0x1.987124876876324p+12) === reinterpret(Float16, 0x6e62)
    @test RTLIB.truncdfhf2(0x1.0p+0) === reinterpret(Float16, 0x3c00)
    @test RTLIB.truncdfhf2(0x1.0p-14) === reinterpret(Float16, 0x0400)
    # denormal
    @test RTLIB.truncdfhf2(0x1.0p-20) === reinterpret(Float16, 0x0010)
    @test RTLIB.truncdfhf2(0x1.0p-24) === reinterpret(Float16, 0x0001)
    @test RTLIB.truncdfhf2(-0x1.0p-24) === reinterpret(Float16, 0x8001)
    @test RTLIB.truncdfhf2(0x1.5p-25) === reinterpret(Float16, 0x0001)
    # and back to zero
    @test RTLIB.truncdfhf2(0x1.0p-25) === reinterpret(Float16, 0x0000)
    @test RTLIB.truncdfhf2(-0x1.0p-25) === reinterpret(Float16, 0x8000)
    # max (precise)
    @test RTLIB.truncdfhf2(65504.0) === reinterpret(Float16, 0x7bff)
    # max (rounded)
    @test RTLIB.truncdfhf2(65519.0) === reinterpret(Float16, 0x7bff)
    # max (to +inf)
    @test RTLIB.truncdfhf2(65520.0) === reinterpret(Float16, 0x7c00)
    @test RTLIB.truncdfhf2(-65520.0) === reinterpret(Float16, 0xfc00)
    @test RTLIB.truncdfhf2(65536.0) === reinterpret(Float16, 0x7c00)
end

@testset "truncdfsf2" begin
   @test RTLIB.truncdfsf2(340282366920938463463374607431768211456.0) === Inf32
end

@testset "truncsfhf2" begin
    # NaN
    @test RTLIB.truncsfhf2(NaN32) === reinterpret(Float16, 0x7e00)
    # inf
    @test RTLIB.truncsfhf2(Inf32) === reinterpret(Float16, 0x7c00)
    @test RTLIB.truncsfhf2(-Inf32) === reinterpret(Float16, 0xfc00)
    # zero
    @test RTLIB.truncsfhf2(0.0f0) === reinterpret(Float16, 0x0000)
    @test RTLIB.truncsfhf2(-0.0f0) === reinterpret(Float16, 0x8000)
    @test RTLIB.truncsfhf2(3.1415926535f0) === reinterpret(Float16, 0x4248)
    @test RTLIB.truncsfhf2(-3.1415926535f0) === reinterpret(Float16, 0xc248)
    @test RTLIB.truncsfhf2(Float32(0x1.987124876876324p+100)) === reinterpret(Float16, 0x7c00)
    @test RTLIB.truncsfhf2(Float32(0x1.987124876876324p+12)) === reinterpret(Float16, 0x6e62)
    @test RTLIB.truncsfhf2(Float32(0x1.0p+0)) === reinterpret(Float16, 0x3c00)
    @test RTLIB.truncsfhf2(Float32(0x1.0p-14)) === reinterpret(Float16, 0x0400)
    # denormal
    @test RTLIB.truncsfhf2(Float32(0x1.0p-20)) === reinterpret(Float16, 0x0010)
    @test RTLIB.truncsfhf2(Float32(0x1.0p-24)) === reinterpret(Float16, 0x0001)
    @test RTLIB.truncsfhf2(Float32(-0x1.0p-24)) === reinterpret(Float16, 0x8001)
    @test RTLIB.truncsfhf2(Float32(0x1.5p-25)) === reinterpret(Float16, 0x0001)
    # and back to zero
    @test RTLIB.truncsfhf2(Float32(0x1.0p-25)) === reinterpret(Float16, 0x0000)
    @test RTLIB.truncsfhf2(Float32(-0x1.0p-25)) === reinterpret(Float16, 0x8000)
    # max (precise)
    @test RTLIB.truncsfhf2(65504.0f0) === reinterpret(Float16, 0x7bff)
    # max (rounded)
    @test RTLIB.truncsfhf2(65519.0f0) === reinterpret(Float16, 0x7bff)
    # max (to +inf)
    @test RTLIB.truncsfhf2(65520.0f0) === reinterpret(Float16, 0x7c00)
    @test RTLIB.truncsfhf2(65536.0f0) === reinterpret(Float16, 0x7c00)
    @test RTLIB.truncsfhf2(-65520.0f0) === reinterpret(Float16, 0xfc00)
end

@testset "extendhfsf2" begin
    # NaN
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x7e00)) === NaN32
    # inf
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x7c00)) === Inf32
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0xfc00)) === -Inf32
    # zero
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x0000)) === 0.0f0
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x8000)) === -0.0f0
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x4248)) === 3.1415926535f0
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0xc248)) === -3.1415926535f0
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x7c00)) === Float32(0x1.987124876876324p+100)
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x6e62)) === Float32(0x1.988p+12)
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x3c00)) === Float32(0x1.0p+0)
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x0400)) === Float32(0x1.0p-14)
    # denormal
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x0010)) === Float32(0x1.0p-20)
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x0001)) === Float32(0x1.0p-24)
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x8001)) === Float32(-0x1.0p-24)
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x0001)) === Float32(0x1.5p-25)
    # and back to zero
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x0000)) === Float32(0x1.0p-25)
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x8000)) === Float32(-0x1.0p-25)
    # max (precise)
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x7bff)) === 65504.0f0
    # max (rounded)
    @test RTLIB.extendhfsf2(reinterpret(Float16, 0x7bff)) === 65504.0f0
end
end