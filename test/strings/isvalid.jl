# This file is a part of Julia. License is MIT: http://julialang.org/license

# Issue #11140
@test isvalid(utf32("a")) == true
@test isvalid(utf32("\x00")) == true
@test isvalid(UTF32String, UInt32[0xd800,0]) == false

# Issue #11241

@test isvalid(ASCIIString, "is_valid_ascii") == true
@test isvalid(ASCIIString, "Σ_not_valid_ascii") == false

# test all edge conditions
for (val, pass) in (
        (0, true), (0xd7ff, true),
        (0xd800, false), (0xdfff, false),
        (0xe000, true), (0xffff, true),
        (0x10000, true), (0x10ffff, true),
        (0x110000, false)
    )
    @test isvalid(Char, val) == pass
end
for (val, pass) in (
        (b"\x00", true),
        (b"\x7f", true),
        (b"\x80", false),
        (b"\xbf", false),
        (b"\xc0", false),
        (b"\xff", false),
        (b"\xc0\x80", false),
        (b"\xc1\x80", false),
        (b"\xc2\x80", true),
        (b"\xc2\xc0", false),
        (b"\xed\x9f\xbf", true),
        (b"\xed\xa0\x80", false),
        (b"\xed\xbf\xbf", false),
        (b"\xee\x80\x80", true),
        (b"\xef\xbf\xbf", true),
        (b"\xf0\x90\x80\x80", true),
        (b"\xf4\x8f\xbf\xbf", true),
        (b"\xf4\x90\x80\x80", false),
        (b"\xf5\x80\x80\x80", false),
        (b"\ud800\udc00", false),
        (b"\udbff\udfff", false),
        (b"\ud800\u0100", false),
        (b"\udc00\u0100", false),
        (b"\udc00\ud800", false)
        )
    @test isvalid(UTF8String, val) == pass
end
for (val, pass) in (
        (UInt16[0x0000], true),
        (UInt16[0xd7ff,0], true),
        (UInt16[0xd800,0], false),
        (UInt16[0xdfff,0], false),
        (UInt16[0xe000,0], true),
        (UInt16[0xffff,0], true),
        (UInt16[0xd800,0xdc00,0], true),
        (UInt16[0xdbff,0xdfff,0], true),
        (UInt16[0xd800,0x0100,0], false),
        (UInt16[0xdc00,0x0100,0], false),
        (UInt16[0xdc00,0xd800,0], false)
        )
    @test isvalid(UTF16String, val) == pass
end
for (val, pass) in (
        (UInt32[0x0000], true),
        (UInt32[0xd7ff,0], true),
        (UInt32[0xd800,0], false),
        (UInt32[0xdfff,0], false),
        (UInt32[0xe000,0], true),
        (UInt32[0xffff,0], true),
        (UInt32[0x100000,0], true),
        (UInt32[0x10ffff,0], true),
        (UInt32[0x110000,0], false),
        )
    @test isvalid(UTF32String, val) == pass
end

# Issue #11203
@test isvalid(ASCIIString,UInt8[]) == true
@test isvalid(UTF8String, UInt8[]) == true
@test isvalid(UTF16String,UInt16[]) == true
@test isvalid(UTF32String,UInt32[]) == true

# Check UTF-8 characters
# Check ASCII range (true),
# then single continuation bytes and lead bytes with no following continuation bytes (false)
for (rng,flg) in ((0:0x7f, true), (0x80:0xff, false))
    for byt in rng
        @test isvalid(UTF8String, UInt8[byt]) == flg
    end
end
# Check overlong lead bytes for 2-character sequences (false)
for byt = 0xc0:0xc1
    @test isvalid(UTF8String, UInt8[byt,0x80]) == false
end
# Check valid lead-in to two-byte sequences (true)
for byt = 0xc2:0xdf
    for (rng,flg) in ((0x00:0x7f, false), (0x80:0xbf, true), (0xc0:0xff, false))
        for cont in rng
            @test isvalid(UTF8String, UInt8[byt, cont]) == flg
        end
    end
end
# Check three-byte sequences
for r1 in (0xe0:0xec, 0xee:0xef)
    for byt = r1
        # Check for short sequence
        @test isvalid(UTF8String, UInt8[byt]) == false
        for (rng,flg) in ((0x00:0x7f, false), (0x80:0xbf, true), (0xc0:0xff, false))
            for cont in rng
                @test isvalid(UTF8String, UInt8[byt, cont]) == false
                @test isvalid(UTF8String, UInt8[byt, cont, 0x80]) == flg
            end
        end
    end
end
# Check hangul characters (0xd000-0xd7ff) hangul
# Check for short sequence, or start of surrogate pair
for (rng,flg) in ((0x00:0x7f, false), (0x80:0x9f, true), (0xa0:0xff, false))
    for cont in rng
        @test isvalid(UTF8String, UInt8[0xed, cont]) == false
        @test isvalid(UTF8String, UInt8[0xed, cont, 0x80]) == flg
    end
end
# Check valid four-byte sequences
for byt = 0xf0:0xf4
    if (byt == 0xf0)
        r0 = ((0x00:0x8f, false), (0x90:0xbf, true), (0xc0:0xff, false))
    elseif byt == 0xf4
        r0 = ((0x00:0x7f, false), (0x80:0x8f, true), (0x90:0xff, false))
    else
        r0 = ((0x00:0x7f, false), (0x80:0xbf, true), (0xc0:0xff, false))
    end
    for (rng,flg) in r0
        for cont in rng
            @test isvalid(UTF8String, UInt8[byt, cont]) == false
            @test isvalid(UTF8String, UInt8[byt, cont, 0x80]) == false
            @test isvalid(UTF8String, UInt8[byt, cont, 0x80, 0x80]) == flg
        end
    end
end
# Check five-byte sequences, should be invalid
for byt = 0xf8:0xfb
    @test isvalid(UTF8String, UInt8[byt, 0x80, 0x80, 0x80, 0x80]) == false
end
# Check six-byte sequences, should be invalid
for byt = 0xfc:0xfd
    @test isvalid(UTF8String, UInt8[byt, 0x80, 0x80, 0x80, 0x80, 0x80]) == false
end
# Check seven-byte sequences, should be invalid
@test isvalid(UTF8String, UInt8[0xfe, 0x80, 0x80, 0x80, 0x80, 0x80]) == false

# 11482

# isvalid
let s = "abcdef", u8 = "abcdef\uff", u16 = utf16(u8), u32 = utf32(u8),
    bad32 = utf32(UInt32[65,0x110000]), badch = Char[0x110000][1]

    @test !isvalid(bad32)
    @test !isvalid(badch)
    @test isvalid(s)
    @test isvalid(u8)
    @test isvalid(u16)
    @test isvalid(u32)
    @test isvalid(ASCIIString, s)
    @test isvalid(UTF8String,  u8)
    @test isvalid(UTF16String, u16)
    @test isvalid(UTF32String, u32)
end
