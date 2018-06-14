using Base: SecretBuffer, SecretBuffer!, shred!, isshredded
using Base.Test

@testset "SecretBuffer" begin
    @testset "original unmodified" begin
        str = "foobar"
        secret = SecretBuffer(str)

        @test read(secret, String) == str
        seekstart(secret)

        @test shred!(secret) === secret
        @test read(secret, String) != ""
        @test str != "foobar"
    end

    @testset "finalizer" begin
        v = UInt8[1, 2]
        secret_a = SecretBuffer!(v)
        secret_b = secret_a

        secret_a = nothing
        GC.gc()

        @test all(iszero, v)
        @test !isshredded(secret_b)
    end
end
