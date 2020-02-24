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
        @test_logs (:warn, r".*SecretBuffer was `shred!`ed by the GC.*") begin
            finalize(secret_b)
            # Allow the async task which produces the SecretBuffer warning to run.
            # This is a hack, but we don't have a way to get a handle to that
            # task in order to `wait` on it.
            for i=1:1000
                yield()
            end
        end
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

        s4 = SecretBuffer(split("setec astronomy", " ")[1]) # initialize from SubString
        s5 = convert(SecretBuffer, split("setec astronomy", " ")[1]) # initialize from SubString
        @test s4 == s5
        shred!(s1); shred!(s2); shred!(s3); shred!(s4), shred!(s5);
    end
    @testset "basics" begin
        s1 = SecretBuffer("setec astronomy")
        @test sprint(show, s1) == "SecretBuffer(\"*******\")"
        @test !isempty(s1)
        shred!(s1)
        s2 = SecretBuffer!([0x00])
        @test_throws ArgumentError Base.cconvert(Cstring, s2)
        shred!(s2)
    end
    @testset "write! past data size" begin
        sb = SecretBuffer(sizehint=2)
        # data vector will not grow
        bits = typemax(UInt8)
        write(sb, bits)
        write(sb, bits)
        # data vector must grow
        write(sb, bits)
        seek(sb, 0)
        @test read(sb, String) == "\xff\xff\xff"
        shred!(sb)
    end
    @testset "bytes available" begin
        sb = SecretBuffer("secret")
        @test bytesavailable(sb) == sb.size
        seek(sb, 3)
        @test bytesavailable(sb) == sb.size - 3
        seekend(sb)
        @test bytesavailable(sb) == 0
        shred!(sb)
    end
    @testset "testing the skip function" begin
        sb = SecretBuffer("computer")
        skip(sb, 2)
        @test position(sb) == 2
        seek(sb, 0)
        @test position(sb) == 0
        skip(sb, sb.size)
        @test position(sb) == sb.size
    end
    @testset "position" begin
        sb = SecretBuffer("Julia")
        println("testing position")
        initial_pos = (position(sb))
        seek(sb,2)
        mid_pos = position(sb)
        seekend(sb)
        @test initial_pos == 0 && mid_pos == 2 && position(sb)==sb.size
        shred!(sb)
    end
end
