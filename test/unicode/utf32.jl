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
function tstcvt(strUTF8::String, strUTF16::UTF16String, strUTF32::UTF32String)
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
strS_UTF8 = String(b"abcd\xc3\xbf\xdf\xbf\xe7\xbf\xbf\xed\xa0\x80\xed\xb0\x80")
strC_UTF8 = String(b"abcd\xc3\xbf\xdf\xbf\xe7\xbf\xbf\U10000")
strz_UTF8 = String(b"abcd\xc3\xbf\xdf\xbf\xe7\xbf\xbf\0")
strZ      = b"abcd\xc3\xbf\xdf\xbf\xe7\xbf\xbf\xc0\x80"

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
@test utf8(strZ)  == strz_UTF8
@test utf16(String(strZ)) == strz_UTF8
@test utf32(String(strZ)) == strz_UTF8

# Test invalid sequences

strval(::Type{String}, dat) = dat
strval(::Union{Type{UTF16String},Type{UTF32String}}, dat) = String(dat)

byt = 0x0
for T in (String, UTF16String, UTF32String)
    try
    # Continuation byte not after lead
    for byt in 0x80:0xbf
        @test_throws UnicodeError convert(T,  strval(T, UInt8[byt]))
    end

    # Test lead bytes
    for byt in 0xc0:0xff
        # Single lead byte at end of string
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt]))
        # Lead followed by non-continuation character < 0x80
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0]))
        # Lead followed by non-continuation character > 0xbf
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0xc0]))
    end

    # Test overlong 2-byte
    for byt in 0x81:0xbf
        @test_throws UnicodeError convert(T, strval(T, UInt8[0xc0,byt]))
    end
    for byt in 0x80:0xbf
        @test_throws UnicodeError convert(T, strval(T, UInt8[0xc1,byt]))
    end

    # Test overlong 3-byte
    for byt in 0x80:0x9f
        @test_throws UnicodeError convert(T, strval(T, UInt8[0xe0,byt,0x80]))
    end

    # Test overlong 4-byte
    for byt in 0x80:0x8f
        @test_throws UnicodeError convert(T, strval(T, UInt8[0xef,byt,0x80,0x80]))
    end

    # Test 4-byte > 0x10ffff
    for byt in 0x90:0xbf
        @test_throws UnicodeError convert(T, strval(T, UInt8[0xf4,byt,0x80,0x80]))
    end
    for byt in 0xf5:0xf7
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0x80,0x80]))
    end

    # Test 5-byte
    for byt in 0xf8:0xfb
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0x80,0x80,0x80]))
    end

    # Test 6-byte
    for byt in 0xfc:0xfd
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0x80,0x80,0x80,0x80]))
    end

    # Test 7-byte
    @test_throws UnicodeError convert(T, strval(T, UInt8[0xfe,0x80,0x80,0x80,0x80,0x80,0x80]))

    # Three and above byte sequences
    for byt in 0xe0:0xef
        # Lead followed by only 1 continuation byte
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80]))
        # Lead ended by non-continuation character < 0x80
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0]))
        # Lead ended by non-continuation character > 0xbf
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0xc0]))
    end

    # 3-byte encoded surrogate character(s)
    # Single surrogate
    @test_throws UnicodeError convert(T, strval(T, UInt8[0xed,0xa0,0x80]))
    # Not followed by surrogate
    @test_throws UnicodeError convert(T, strval(T, UInt8[0xed,0xa0,0x80,0xed,0x80,0x80]))
    # Trailing surrogate first
    @test_throws UnicodeError convert(T, strval(T, UInt8[0xed,0xb0,0x80,0xed,0xb0,0x80]))
    # Followed by lead surrogate
    @test_throws UnicodeError convert(T, strval(T, UInt8[0xed,0xa0,0x80,0xed,0xa0,0x80]))

    # Four byte sequences
    for byt in 0xf0:0xf4
        # Lead followed by only 2 continuation bytes
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0x80]))
        # Lead followed by non-continuation character < 0x80
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0x80,0]))
        # Lead followed by non-continuation character > 0xbf
        @test_throws UnicodeError convert(T, strval(T, UInt8[byt,0x80,0x80,0xc0]))
    end
    catch exp
        println("Error checking $T: $byt")
        throw(exp)
    end
end

# Wstring
u8 = "\U10ffff\U1d565\U1d7f6\U00066\U2008a"
w = wstring(u8)
@test length(w) == 5 && utf8(w) == u8 && collect(u8) == collect(w)
@test u8 == WString(w.data)

# 12268
for (fun, S, T) in ((utf16, UInt16, UTF16String), (utf32, UInt32, UTF32String))
    # AbstractString
    str = "abcd\0\uff\u7ff\u7fff\U7ffff"
    tst = SubString(convert(T,str),4)
    cmp = Char['d','\0','\uff','\u7ff','\u7fff','\U7ffff']
    cmpch = Char['d','\0','\uff','\u7ff','\u7fff','\U7ffff','\0']
    cmp16 = UInt16[0x0064,0x0000,0x00ff,0x07ff,0x7fff,0xd9bf,0xdfff,0x0000]
    x = fun(tst)
    cmpx = (S == UInt16 ? cmp16 : cmpch)
    @test typeof(tst) == SubString{T}
    @test convert(T, tst) == str[4:end]
    S != UInt32 && @test convert(Vector{Char}, x) == cmp
    # Vector{T} / Array{T}
    @test convert(Vector{S}, x) == cmpx
    @test convert(Array{S}, x) == cmpx
    # Embedded nul checking
    @test Base.containsnul(x)
    @test Base.containsnul(tst)
    # map
    @test_throws UnicodeError map(islower, x)
    @test_throws ArgumentError map(islower, tst)
    # SubArray conversion
    subarr = sub(cmp, 1:6)
    @test convert(T, subarr) == str[4:end]
end

# Char to UTF32String
@test utf32('\U7ffff') == utf32("\U7ffff")
@test convert(UTF32String, '\U7ffff') == utf32("\U7ffff")

@test isvalid(UTF32String, Char['d','\uff','\u7ff','\u7fff','\U7ffff'])
@test reverse(utf32("abcd \uff\u7ff\u7fff\U7ffff")) == utf32("\U7ffff\u7fff\u7ff\uff dcba")

# Test pointer() functions
let str = ascii("this ")
    u8  = utf8(str)
    u16 = utf16(str)
    u32 = utf32(str)
    pa  = pointer(str)
    p8  = pointer(u8)
    p16 = pointer(u16)
    p32 = pointer(u32)
    @test typeof(pa) == Ptr{UInt8}
    @test unsafe_load(pa,1) == 0x74
    @test typeof(p8) == Ptr{UInt8}
    @test unsafe_load(p8,1) == 0x74
    @test typeof(p16) == Ptr{UInt16}
    @test unsafe_load(p16,1) == 0x0074
    @test typeof(p32) == Ptr{UInt32}
    @test unsafe_load(p32,1) == 't'
    pa  = pointer(str, 2)
    p8  = pointer(u8,  2)
    p16 = pointer(u16, 2)
    p32 = pointer(u32, 2)
    @test typeof(pa) == Ptr{UInt8}
    @test unsafe_load(pa,1) == 0x68
    @test typeof(p8) == Ptr{UInt8}
    @test unsafe_load(p8,1) == 0x68
    @test typeof(p16) == Ptr{UInt16}
    @test unsafe_load(p16,1) == 0x0068
    @test typeof(p32) == Ptr{UInt32}
    @test unsafe_load(p32,1) == 'h'
    s8  = SubString{String}(u8,   3, 5)
    s16 = SubString{UTF16String}(u16, 3, 5)
    s32 = SubString{UTF32String}(u32, 3, 5)
    p8  = pointer(s8)
    p16 = pointer(s16)
    p32 = pointer(s32)
    @test typeof(p8) == Ptr{UInt8}
    @test unsafe_load(p8,1) == 0x69
    @test typeof(p16) == Ptr{UInt16}
    @test unsafe_load(p16,1) == 0x0069
    @test typeof(p32) == Ptr{UInt32}
    @test unsafe_load(p32,1) == 'i'
    p8  = pointer(s8,  2)
    p16 = pointer(s16, 2)
    p32 = pointer(s32, 2)
    @test typeof(p8) == Ptr{UInt8}
    @test unsafe_load(p8,1) == 0x73
    @test typeof(p16) == Ptr{UInt16}
    @test unsafe_load(p16,1) == 0x0073
    @test typeof(p32) == Ptr{UInt32}
    @test unsafe_load(p32,1) == 's'
end

@test isvalid(Char['f','o','o','b','a','r'])
