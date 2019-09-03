# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "string indexing" begin
    let str = "this is a test\xed\x80"
        @test iterate(str, 15) == (reinterpret(Char, 0xed800000), 17)
        @test_throws BoundsError getindex(str, 0:3)
        @test_throws BoundsError getindex(str, 17:18)
        @test_throws BoundsError getindex(str, 2:17)
        @test_throws BoundsError getindex(str, 16:17)
        @test string(Char(0x110000)) == "\xf4\x90\x80\x80"
    end
end

@testset "string reverse" begin
    @test reverse("") == ""
    @test reverse("a") == "a"
    @test reverse("abc") == "cba"
    @test reverse("xyz\uff\u800\uffff\U10ffff") == "\U10ffff\uffff\u800\uffzyx"
    for (s, r) in [
        "xyz\xc1"         => "\xc1zyx",
        "xyz\xd0"         => "\xd0zyx",
        "xyz\xe0"         => "\xe0zyx",
        "xyz\xed\x80"     => "\xed\x80zyx",
        "xyz\xf0"         => "\xf0zyx",
        "xyz\xf0\x80"     => "\xf0\x80zyx",
        "xyz\xf0\x80\x80" => "\xf0\x80\x80zyx",
    ]
        @test reverse(s) == r
    end
end

@testset "string convert" begin
    @test String(b"this is a test\xed\x80\x80") ==
                  "this is a test\xed\x80\x80"  ==
                  "this is a test\ud000"
    # Specifically check UTF-8 string whose lead byte is same as a surrogate
    @test String(b"\xed\x9f\xbf") == "\xed\x9f\xbf" == "\ud7ff"
end
