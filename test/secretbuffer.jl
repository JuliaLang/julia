# This file is a part of Julia. License is MIT: https://julialang.org/license

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :ChallengePrompts) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "ChallengePrompts.jl"))
using .Main.ChallengePrompts: challenge_prompt

using Base: SecretBuffer, SecretBuffer!, shred!, isshredded
using Test, Random

@testset "SecretBuffer" begin
    @testset "original unmodified" begin
        str = "foobar"
        secret = SecretBuffer(str)

        @test read(secret, String) == str
        seekstart(secret)

        @test peek(secret) == 0x66

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
        shred!(sb)
    end
    @testset "seekend" begin
        sb = SecretBuffer("hello")
        seekend(sb)
        @test read(sb, String) == ""
        shred!(sb)
    end
    @testset "position" begin
        sb = SecretBuffer("Julia")
        initial_pos = (position(sb))
        seek(sb,2)
        mid_pos = position(sb)
        seekend(sb)
        @test initial_pos == 0 && mid_pos == 2 && position(sb)==sb.size
        shred!(sb)
    end
    @testset "hashing secret buffers" begin
        sb1 = SecretBuffer("hello")
        sb2 = SecretBuffer("juliaisawesome")
        @test hash(sb1, UInt(5)) === hash(sb2, UInt(5))
        shred!(sb1); shred!(sb2)
    end
    @testset "NULL initialization" begin
        null_ptr = Cstring(C_NULL)
        @test_throws ArgumentError Base.unsafe_SecretBuffer!(null_ptr)
        null_ptr = Ptr{UInt8}(C_NULL)
        @test_throws ArgumentError Base.unsafe_SecretBuffer!(null_ptr)
        @test_throws ArgumentError Base.unsafe_SecretBuffer!(null_ptr, 0)
    end

    @testset "copiers" begin
        s1 = SecretBuffer()
        write(s1, "hello world")
        seekstart(s1)

        s2 = copy(s1)
        write(s2, 'c')
        seekstart(s2)

        @test read(s1) == codeunits("hello world")
        @test read(s2) == codeunits("cello world")

        shred!(s1)
        @test isshredded(s1)
        @test !isshredded(s2)
        shred!(s2)

        # Copying into a bigger destination
        s3 = SecretBuffer()
        s4 = SecretBuffer()
        write(s3, "original")
        seekstart(s3)
        write(s4, randstring(1234))
        s4data = s4.data
        copy!(s4, s3)
        @test s3.data == s4.data
        @test read(s3) == read(s4) == codeunits("original")
        @test all(iszero, s4data)
        shred!(s3); shred!(s4)

        # Copying into a smaller destination
        s5 = SecretBuffer()
        s6 = SecretBuffer("sekrit")
        str = randstring(321)
        write(s5, str)
        seekstart(s5)
        copy!(s6, s5)
        @test read(s5) == read(s6) == codeunits(str)
        shred!(s5); shred!(s6)
    end

    if !Sys.iswindows()
        @testset "getpass" begin
            v1, s1 = challenge_prompt(:(s=Base.getpass("LPAwVZM8D4I"); (read(s), Base.shred!(s))), ["LPAwVZM8D4I: " => "too many secrets\n"])
            s2 = SecretBuffer("too many secrets")
            @test s1 isa SecretBuffer
            @test isshredded(s1)
            @test v1 == read(s2) == codeunits("too many secrets")
            shred!(s1); shred!(s2)

            v3, s3 = challenge_prompt(:(s=Base.getpass("LPAwVZM8D4I> ", with_suffix=false); (read(s), Base.shred!(s))), ["LPAwVZM8D4I> " => "frperg\n"])
            s4 = SecretBuffer("frperg")
            @test s3 isa SecretBuffer
            @test isshredded(s3)
            @test v3 == read(s4) == codeunits("frperg")
            shred!(s3); shred!(s4)

            v5, s5 = challenge_prompt(:(s=Base.getpass("LPAwVZM8D4I> ", with_suffix=true); (read(s), Base.shred!(s))), ["LPAwVZM8D4I> : " => "frperg\n"])
            s6 = SecretBuffer("frperg")
            @test s5 isa SecretBuffer
            @test isshredded(s5)
            @test v5 == read(s6) == codeunits("frperg")
            shred!(s5); shred!(s6)
        end
    end
end
