# This file is a part of Julia. License is MIT: http://julialang.org/license

## Test for CESU-8 sequences

let ch = 0x10000
    for hichar = 0xd800:0xdbff
        for lochar = 0xdc00:0xdfff
            @test convert(UTF8String, utf8(Char[hichar, lochar]).data) == string(Char(ch))
            ch += 1
        end
    end
end

let str = UTF8String(b"this is a test\xed\x80")
    @test next(str, 15) == ('\ufffd', 16)
    @test_throws BoundsError getindex(str, 0:3)
    @test_throws BoundsError getindex(str, 17:18)
    @test_throws BoundsError getindex(str, 2:17)
    @test_throws UnicodeError getindex(str, 16:17)
    @test string(Char(0x110000)) == "\ufffd"
    sa = SubString{ASCIIString}("This is a silly test", 1, 14)
    s8 = convert(SubString{UTF8String}, sa)
    @test typeof(s8) == SubString{UTF8String}
    @test s8 == "This is a sill"
    @test convert(UTF8String, b"this is a test\xed\x80\x80") == "this is a test\ud000"
end
