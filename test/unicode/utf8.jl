# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "cesu8 input" begin
    let ch = 0x10000
        for hi = 0xd800:0xdbff
            for lo = 0xdc00:0xdfff
                @test convert(String, Vector{UInt8}(String(Char[hi, lo]))) == string(Char(ch))
                ch += 1
            end
        end
    end
end

@testset "string indexing" begin
    let str = String(b"this is a test\xed\x80")
        @test next(str, 15) == ('\ufffd', 16)
        @test_throws BoundsError getindex(str, 0:3)
        @test_throws BoundsError getindex(str, 17:18)
        @test_throws BoundsError getindex(str, 2:17)
        @test_throws UnicodeError getindex(str, 16:17)
        @test string(Char(0x110000)) == "\ufffd"
    end
end

@testset "string reverse" begin
    @test reverse("") == ""
    @test reverse("a") == "a"
    @test reverse("abc") == "cba"
    @test reverse("xyz\uff\u800\uffff\U10ffff") == "\U10ffff\uffff\u800\uffzyx"
    for str in [
        b"xyz\xc1",
        b"xyz\xd0",
        b"xyz\xe0",
        b"xyz\xed\x80",
        b"xyz\xf0",
        b"xyz\xf0\x80",
        b"xyz\xf0\x80\x80"
    ]
        @test_throws UnicodeError reverse(String(str))
    end
end

@testset "string convert" begin
    @test convert(String, b"this is a test\xed\x80\x80") == "this is a test\ud000"
    ## Specifically check UTF-8 string whose lead byte is same as a surrogate
    @test convert(String, b"\xed\x9f\xbf") == "\ud7ff"
end
