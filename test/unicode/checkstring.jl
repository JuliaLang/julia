# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "checkstring" begin
# 11575
# Test invalid sequences

byt = 0x0 # Needs to be defined outside the try block!
try
    # Continuation byte not after lead
    for byt in 0x80:0xbf
        @test_throws UnicodeError Base.checkstring(UInt8[byt])
    end

    # Test lead bytes
    for byt in 0xc0:0xff
        # Single lead byte at end of string
        @test_throws UnicodeError Base.checkstring(UInt8[byt])
        # Lead followed by non-continuation character < 0x80
        @test_throws UnicodeError Base.checkstring(UInt8[byt,0])
        # Lead followed by non-continuation character > 0xbf
        @test_throws UnicodeError Base.checkstring(UInt8[byt,0xc0])
    end

    # Test overlong 2-byte
    for byt in 0x81:0xbf
        @test_throws UnicodeError Base.checkstring(UInt8[0xc0,byt])
    end
    for byt in 0x80:0xbf
        @test_throws UnicodeError Base.checkstring(UInt8[0xc1,byt])
    end

    # Test overlong 3-byte
    for byt in 0x80:0x9f
        @test_throws UnicodeError Base.checkstring(UInt8[0xe0,byt,0x80])
    end

    # Test overlong 4-byte
    for byt in 0x80:0x8f
        @test_throws UnicodeError Base.checkstring(UInt8[0xef,byt,0x80,0x80])
    end

    # Test 4-byte > 0x10ffff
    for byt in 0x90:0xbf
        @test_throws UnicodeError Base.checkstring(UInt8[0xf4,byt,0x80,0x80])
    end
    for byt in 0xf5:0xf7
        @test_throws UnicodeError Base.checkstring(UInt8[byt,0x80,0x80,0x80])
    end

    # Test 5-byte
    for byt in 0xf8:0xfb
        @test_throws UnicodeError Base.checkstring(UInt8[byt,0x80,0x80,0x80,0x80])
    end

    # Test 6-byte
    for byt in 0xfc:0xfd
        @test_throws UnicodeError Base.checkstring(UInt8[byt,0x80,0x80,0x80,0x80,0x80])
    end

    # Test 7-byte
    @test_throws UnicodeError Base.checkstring(UInt8[0xfe,0x80,0x80,0x80,0x80,0x80,0x80])

    # Three and above byte sequences
    for byt in 0xe0:0xef
        # Lead followed by only 1 continuation byte
        @test_throws UnicodeError Base.checkstring(UInt8[byt,0x80])
        # Lead ended by non-continuation character < 0x80
        @test_throws UnicodeError Base.checkstring(UInt8[byt,0x80,0])
        # Lead ended by non-continuation character > 0xbf
        @test_throws UnicodeError Base.checkstring(UInt8[byt,0x80,0xc0])
    end

    # 3-byte encoded surrogate character(s)
    # Single surrogate
    @test_throws UnicodeError Base.checkstring(UInt8[0xed,0xa0,0x80])
    # Not followed by surrogate
    @test_throws UnicodeError Base.checkstring(UInt8[0xed,0xa0,0x80,0xed,0x80,0x80])
    # Trailing surrogate first
    @test_throws UnicodeError Base.checkstring(UInt8[0xed,0xb0,0x80,0xed,0xb0,0x80])
    # Followed by lead surrogate
    @test_throws UnicodeError Base.checkstring(UInt8[0xed,0xa0,0x80,0xed,0xa0,0x80])

    # Four byte sequences
    for byt in 0xf0:0xf4
        # Lead followed by only 2 continuation bytes
        @test_throws UnicodeError Base.checkstring(UInt8[byt,0x80,0x80])
        # Lead followed by non-continuation character < 0x80
        @test_throws UnicodeError Base.checkstring(UInt8[byt,0x80,0x80,0])
        # Lead followed by non-continuation character > 0xbf
        @test_throws UnicodeError Base.checkstring(UInt8[byt,0x80,0x80,0xc0])
    end

    # Long encoding of 0x01
    @test_throws UnicodeError convert(String, b"\xf0\x80\x80\x80")
    # Test ends of long encoded surrogates
    @test_throws UnicodeError convert(String, b"\xf0\x8d\xa0\x80")
    @test_throws UnicodeError convert(String, b"\xf0\x8d\xbf\xbf")
    # Long encodings
    @test_throws UnicodeError Base.checkstring(b"\xf0\x80\x80\x80")
    @test Base.checkstring(b"\xc0\x81"; accept_long_char=true) == (1,0x1,0,0,0)
    @test Base.checkstring(b"\xf0\x80\x80\x80"; accept_long_char=true) == (1,0x1,0,0,0)
catch exp;
    println("Error testing checkstring: $byt, $exp")
    throw(exp)
end

# Surrogates
@test_throws UnicodeError Base.checkstring(UInt16[0xd800])
@test_throws UnicodeError Base.checkstring(UInt16[0xdc00])
@test_throws UnicodeError Base.checkstring(UInt16[0xdc00,0xd800])

# Surrogates in UTF-32
@test_throws UnicodeError Base.checkstring(UInt32[0xd800])
@test_throws UnicodeError Base.checkstring(UInt32[0xdc00])
@test_throws UnicodeError Base.checkstring(UInt32[0xdc00,0xd800])

# Characters > 0x10ffff
@test_throws UnicodeError Base.checkstring(UInt32[0x110000])

# Test starting and different position
@test Base.checkstring(UInt32[0x110000, 0x1f596], 2) == (1,0x10,1,0,0)

# Test valid sequences
for (seq, res) in (
    (UInt8[0x0],                (1,0,0,0,0)),   # Nul byte, beginning of ASCII range
    (UInt8[0x7f],               (1,0,0,0,0)),   # End of ASCII range
    (UInt8[0xc0,0x80],          (1,1,0,0,0)),   # Long encoded Nul byte (Modified UTF-8, Java)
    (UInt8[0xc2,0x80],          (1,2,0,0,1)),   # \u80, beginning of Latin1 range
    (UInt8[0xc3,0xbf],          (1,2,0,0,1)),   # \uff, end of Latin1 range
    (UInt8[0xc4,0x80],          (1,4,0,0,1)),   # \u100, beginning of non-Latin1 2-byte range
    (UInt8[0xdf,0xbf],          (1,4,0,0,1)),   # \u7ff, end of non-Latin1 2-byte range
    (UInt8[0xe0,0xa0,0x80],     (1,8,0,1,0)),   # \u800, beginning of 3-byte range
    (UInt8[0xed,0x9f,0xbf],     (1,8,0,1,0)),   # \ud7ff, end of first part of 3-byte range
    (UInt8[0xee,0x80,0x80],     (1,8,0,1,0)),   # \ue000, beginning of second part of 3-byte range
    (UInt8[0xef,0xbf,0xbf],     (1,8,0,1,0)),   # \uffff, end of 3-byte range
    (UInt8[0xf0,0x90,0x80,0x80],(1,16,1,0,0)),  # \U10000, beginning of 4-byte range
    (UInt8[0xf4,0x8f,0xbf,0xbf],(1,16,1,0,0)),  # \U10ffff, end of 4-byte range
    (UInt8[0xed,0xa0,0x80,0xed,0xb0,0x80], (1,0x30,1,0,0)), # Overlong \U10000, (CESU-8)
    (UInt8[0xed,0xaf,0xbf,0xed,0xbf,0xbf], (1,0x30,1,0,0)), # Overlong \U10ffff, (CESU-8)
    (UInt16[0x0000],            (1,0,0,0,0)),   # Nul byte, beginning of ASCII range
    (UInt16[0x007f],            (1,0,0,0,0)),   # End of ASCII range
    (UInt16[0x0080],            (1,2,0,0,1)),   # Beginning of Latin1 range
    (UInt16[0x00ff],            (1,2,0,0,1)),   # End of Latin1 range
    (UInt16[0x0100],            (1,4,0,0,1)),   # Beginning of non-Latin1 2-byte range
    (UInt16[0x07ff],            (1,4,0,0,1)),   # End of non-Latin1 2-byte range
    (UInt16[0x0800],            (1,8,0,1,0)),   # Beginning of 3-byte range
    (UInt16[0xd7ff],            (1,8,0,1,0)),   # End of first part of 3-byte range
    (UInt16[0xe000],            (1,8,0,1,0)),   # Beginning of second part of 3-byte range
    (UInt16[0xffff],            (1,8,0,1,0)),   # End of 3-byte range
    (UInt16[0xd800,0xdc00],     (1,16,1,0,0)),  # \U10000, beginning of 4-byte range
    (UInt16[0xdbff,0xdfff],     (1,16,1,0,0)),  # \U10ffff, end of 4-byte range
    (UInt32[0x0000],            (1,0,0,0,0)),   # Nul byte, beginning of ASCII range
    (UInt32[0x007f],            (1,0,0,0,0)),   # End of ASCII range
    (UInt32[0x0080],            (1,2,0,0,1)),   # Beginning of Latin1 range
    (UInt32[0x00ff],            (1,2,0,0,1)),   # End of Latin1 range
    (UInt32[0x0100],            (1,4,0,0,1)),   # Beginning of non-Latin1 2-byte range
    (UInt32[0x07ff],            (1,4,0,0,1)),   # End of non-Latin1 2-byte range
    (UInt32[0x0800],            (1,8,0,1,0)),   # Beginning of 3-byte range
    (UInt32[0xd7ff],            (1,8,0,1,0)),   # End of first part of 3-byte range
    (UInt32[0xe000],            (1,8,0,1,0)),   # Beginning of second part of 3-byte range
    (UInt32[0xffff],            (1,8,0,1,0)),   # End of 3-byte range
    (UInt32[0x10000],           (1,16,1,0,0)),  # \U10000, beginning of 4-byte range
    (UInt32[0x10ffff],          (1,16,1,0,0)),  # \U10ffff, end of 4-byte range
    (UInt32[0xd800,0xdc00],     (1,0x30,1,0,0)),# Overlong \U10000, (CESU-8)
    (UInt32[0xdbff,0xdfff],     (1,0x30,1,0,0)))# Overlong \U10ffff, (CESU-8)
    @test Base.checkstring(seq) == res
end

# Test bounds checking
@test_throws BoundsError Base.checkstring(b"abcdef", -10)
@test_throws BoundsError Base.checkstring(b"abcdef", 0)
@test_throws BoundsError Base.checkstring(b"abcdef", 7)
@test_throws BoundsError Base.checkstring(b"abcdef", 3, -10)
@test_throws BoundsError Base.checkstring(b"abcdef", 3, 0)
@test_throws BoundsError Base.checkstring(b"abcdef", 3, 7)
@test_throws ArgumentError Base.checkstring(b"abcdef", 3, 1)

end
