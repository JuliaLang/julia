# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: SecretBuffer, SecretBuffer!, shred!, isshredded
using Test

@testset "SecretBuffer" begin
    @testset "original unmodified" begin
        str = "foobar"
        secret = SecretBuffer(str)

        @test read(secret, String) == str
        seekstart(secret)

        @test shred!(secret) === secret
        @test read(secret, String) == ""
        @test str == "foobar"
    end

    @testset "finalizer" begin
        v = UInt8[1, 2]
        secret_a = SecretBuffer!(v)
        secret_b = secret_a

        secret_a = nothing
        GC.gc()

        @test all(iszero, v)
        @test !isshredded(secret_b)

        # TODO: ideally we'd test that the finalizer warns from GC.gc(), but that is harder
        @test_logs (:warn, r".*SecretBuffer was `shred!`ed by the GC.*") finalize(secret_b)
        @test isshredded(secret_b)
        secret_b = nothing
        GC.gc()
    end

    @testset "initializers" begin
        s1 = SecretBuffer("setec astronomy")
        data2 = [0x73, 0x65, 0x74, 0x65, 0x63, 0x20, 0x61, 0x73, 0x74, 0x72, 0x6f, 0x6e, 0x6f, 0x6d, 0x79]
        s2 = SecretBuffer!(data2)
        @test all(==(0x0), data2)
        @test s1 == s2

        ptr3 = Base.unsafe_convert(Cstring, "setec astronomy")
        s3 = Base.unsafe_SecretBuffer!(ptr3)
        @test Base.unsafe_string(ptr3) == ""
        @test s1 == s2 == s3

        shred!(s1); shred!(s2); shred!(s3)
    end
end
