# This file is a part of Julia. License is MIT: http://julialang.org/license

## Test for CESU-8 sequences

let ch = 0x10000
    for hichar = 0xd800:0xdbff
        for lochar = 0xdc00:0xdfff
            @test convert(String, utf8(Char[hichar, lochar]).data) == string(Char(ch))
            ch += 1
        end
    end
end


let str = String(b"this is a test\xed\x80")
    @test next(str, 15) == ('\ufffd', 16)
    @test_throws BoundsError getindex(str, 0:3)
    @test_throws BoundsError getindex(str, 17:18)
    @test_throws BoundsError getindex(str, 2:17)
    @test_throws UnicodeError getindex(str, 16:17)
    @test string(Char(0x110000)) == "\ufffd"
    @test convert(String, b"this is a test\xed\x80\x80") == "this is a test\ud000"
end

## Reverse of String
@test reverse(String("")) == ""
@test reverse(String("a")) == "a"
@test reverse(String("abc")) == "cba"
@test reverse(String("xyz\uff\u800\uffff\U10ffff")) == "\U10ffff\uffff\u800\uffzyx"
for str in (b"xyz\xc1", b"xyz\xd0", b"xyz\xe0", b"xyz\xed\x80", b"xyz\xf0", b"xyz\xf0\x80",  b"xyz\xf0\x80\x80")
    @test_throws UnicodeError reverse(String(str))
end

## Specifically check UTF-8 string whose lead byte is same as a surrogate
@test convert(String,b"\xed\x9f\xbf") == "\ud7ff"
