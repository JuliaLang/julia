# This file is a part of Julia. License is MIT: http://julialang.org/license

# UTF32
u8 = "\U10ffff\U1d565\U1d7f6\U00066\U2008a"
u32 = utf32(u8)
@test sizeof(u32) == 20
@test length(u32.data) == 6 && u32.data[end] == Char(0)
@test length(u32) == 5
@test utf8(u32) == u8
@test collect(u8) == collect(u32)
@test u8 == utf32(u32.data[1:end-1]) == utf32(copy!(Array(UInt8, 20), 1, reinterpret(UInt8, u32.data), 1, 20))
@test u8 == utf32(pointer(u32)) == utf32(convert(Ptr{Int32}, pointer(u32)))
@test_throws UnicodeError utf32(UInt8[1,2,3])

# issue #11551 (#11004,#10959)
function tstcvt(strUTF8::UTF8String, strUTF16::UTF16String, strUTF32::UTF32String)
    @test utf16(strUTF8) == strUTF16
    @test utf32(strUTF8) == strUTF32
    @test utf8(strUTF16) == strUTF8
    @test utf32(strUTF16) == strUTF32
    @test utf8(strUTF32)  == strUTF8
    @test utf16(strUTF32) == strUTF16
end

# Create some ASCII, UTF8, UTF16, and UTF32 strings

strAscii = "abcdefgh"
strA_UTF8 = ("abcdefgh\uff")[1:8]
strL_UTF8 = "abcdef\uff\uff"
str2_UTF8 = "abcd\uff\uff\u7ff\u7ff"
str3_UTF8 = "abcd\uff\uff\u7fff\u7fff"
str4_UTF8 = "abcd\uff\u7ff\u7fff\U7ffff"
strS_UTF8 = UTF8String(b"abcd\xc3\xbf\xdf\xbf\xe7\xbf\xbf\xed\xa0\x80\xed\xb0\x80")
strC_UTF8 = UTF8String(b"abcd\xc3\xbf\xdf\xbf\xe7\xbf\xbf\U10000")
strZ_UTF8 = UTF8String(b"abcd\xc3\xbf\xdf\xbf\xe7\xbf\xbf\xc0\x80")
strz_UTF8 = UTF8String(b"abcd\xc3\xbf\xdf\xbf\xe7\xbf\xbf\0")

strA_UTF16 = utf16(strA_UTF8)
strL_UTF16 = utf16(strL_UTF8)
str2_UTF16 = utf16(str2_UTF8)
str3_UTF16 = utf16(str3_UTF8)
str4_UTF16 = utf16(str4_UTF8)
strS_UTF16 = utf16(strS_UTF8)

strA_UTF32 = utf32(strA_UTF8)
strL_UTF32 = utf32(strL_UTF8)
str2_UTF32 = utf32(str2_UTF8)
str3_UTF32 = utf32(str3_UTF8)
str4_UTF32 = utf32(str4_UTF8)
strS_UTF32 = utf32(strS_UTF8)

@test utf8(strAscii) == strAscii
@test utf16(strAscii) == strAscii
@test utf32(strAscii) == strAscii

tstcvt(strA_UTF8,strA_UTF16,strA_UTF32)
tstcvt(strL_UTF8,strL_UTF16,strL_UTF32)
tstcvt(str2_UTF8,str2_UTF16,str2_UTF32)
tstcvt(str3_UTF8,str3_UTF16,str3_UTF32)
tstcvt(str4_UTF8,str4_UTF16,str4_UTF32)

# Test converting surrogate pairs
@test utf16(strS_UTF8) == strC_UTF8
@test utf32(strS_UTF8) == strC_UTF8
@test utf8(strS_UTF16) == strC_UTF8
@test utf32(strS_UTF16) == strC_UTF8
@test utf8(strS_UTF32)  == strC_UTF8
@test utf16(strS_UTF32) == strC_UTF8

# Test converting overlong \0
# @test utf8(strZ_UTF8)  == strz_UTF8   # currently broken! (in utf8.jl)
@test utf16(strZ_UTF8) == strz_UTF8
@test utf32(strZ_UTF8) == strz_UTF8

# Test invalid sequences

byt = 0x0
for T in (UTF16String, UTF32String)
    try
    # Continuation byte not after lead
    for byt in 0x80:0xbf
        @test_throws UnicodeError convert(T,  UTF8String(UInt8[byt]))
    end

    # Test lead bytes
    for byt in 0xc0:0xff
        # Single lead byte at end of string
        @test_throws UnicodeError convert(T, UTF8String(UInt8[byt]))
        # Lead followed by non-continuation character < 0x80
        @test_throws UnicodeError convert(T, UTF8String(UInt8[byt,0]))
        # Lead followed by non-continuation character > 0xbf
        @test_throws UnicodeError convert(T, UTF8String(UInt8[byt,0xc0]))
    end

    # Test overlong 2-byte
    for byt in 0x81:0xbf
        @test_throws UnicodeError convert(T, UTF8String(UInt8[0xc0,byt]))
    end
    for byt in 0x80:0xbf
        @test_throws UnicodeError convert(T, UTF8String(UInt8[0xc1,byt]))
    end

    # Test overlong 3-byte
    for byt in 0x80:0x9f
        @test_throws UnicodeError convert(T, UTF8String(UInt8[0xe0,byt,0x80]))
    end

    # Test overlong 4-byte
    for byt in 0x80:0x8f
        @test_throws UnicodeError convert(T, UTF8String(UInt8[0xef,byt,0x80,0x80]))
    end

    # Test 4-byte > 0x10ffff
    for byt in 0x90:0xbf
        @test_throws UnicodeError convert(T, UTF8String(UInt8[0xf4,byt,0x80,0x80]))
    end
    for byt in 0xf5:0xf7
        @test_throws UnicodeError convert(T, UTF8String(UInt8[byt,0x80,0x80,0x80]))
    end

    # Test 5-byte
    for byt in 0xf8:0xfb
        @test_throws UnicodeError convert(T, UTF8String(UInt8[byt,0x80,0x80,0x80,0x80]))
    end

    # Test 6-byte
    for byt in 0xfc:0xfd
        @test_throws UnicodeError convert(T, UTF8String(UInt8[byt,0x80,0x80,0x80,0x80,0x80]))
    end

    # Test 7-byte
    @test_throws UnicodeError convert(T, UTF8String(UInt8[0xfe,0x80,0x80,0x80,0x80,0x80,0x80]))

    # Three and above byte sequences
    for byt in 0xe0:0xef
        # Lead followed by only 1 continuation byte
        @test_throws UnicodeError convert(T, UTF8String(UInt8[byt,0x80]))
        # Lead ended by non-continuation character < 0x80
        @test_throws UnicodeError convert(T, UTF8String(UInt8[byt,0x80,0]))
        # Lead ended by non-continuation character > 0xbf
        @test_throws UnicodeError convert(T, UTF8String(UInt8[byt,0x80,0xc0]))
    end

    # 3-byte encoded surrogate character(s)
    # Single surrogate
    @test_throws UnicodeError convert(T, UTF8String(UInt8[0xed,0xa0,0x80]))
    # Not followed by surrogate
    @test_throws UnicodeError convert(T, UTF8String(UInt8[0xed,0xa0,0x80,0xed,0x80,0x80]))
    # Trailing surrogate first
    @test_throws UnicodeError convert(T, UTF8String(UInt8[0xed,0xb0,0x80,0xed,0xb0,0x80]))
    # Followed by lead surrogate
    @test_throws UnicodeError convert(T, UTF8String(UInt8[0xed,0xa0,0x80,0xed,0xa0,0x80]))

    # Four byte sequences
    for byt in 0xf0:0xf4
        # Lead followed by only 2 continuation bytes
        @test_throws UnicodeError convert(T, UTF8String(UInt8[byt,0x80,0x80]))
        # Lead followed by non-continuation character < 0x80
        @test_throws UnicodeError convert(T, UTF8String(UInt8[byt,0x80,0x80,0]))
        # Lead followed by non-continuation character > 0xbf
        @test_throws UnicodeError convert(T, UTF8String(UInt8[byt,0x80,0x80,0xc0]))
    end
    catch exp ;
        println("Error checking $T: $byt")
        throw(exp)
    end
end

# Wstring
u8 = "\U10ffff\U1d565\U1d7f6\U00066\U2008a"
w = wstring(u8)
@test length(w) == 5 && utf8(w) == u8 && collect(u8) == collect(w)
@test u8 == WString(w.data)
