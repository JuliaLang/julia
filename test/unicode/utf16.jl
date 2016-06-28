# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "UTF16" begin
u8 = "\U10ffff\U1d565\U1d7f6\U00066\U2008a"
u16 = utf16(u8)
@test sizeof(u16) == 18
@test length(u16.data) == 10 && u16.data[end] == 0
@test length(u16) == 5
@test String(u16) == u8
@test collect(u8) == collect(u16)
@test u8 == utf16(u16.data[1:end-1]) == utf16(copy!(Array(UInt8, 18), 1, reinterpret(UInt8, u16.data), 1, 18))
@test u8 == utf16(pointer(u16)) == utf16(convert(Ptr{Int16}, pointer(u16)))
@test_throws UnicodeError utf16(utf32(Char(0x120000)))
@test_throws UnicodeError utf16(UInt8[1,2,3])

# Add tests for full coverage
@test convert(UTF16String, "test") == "test"
@test convert(UTF16String, u16) == u16
@test convert(UTF16String, UInt16[[0x65, 0x66] [0x67, 0x68]]) == "efgh"
@test convert(UTF16String, Int16[[0x65, 0x66] [0x67, 0x68]]) == "efgh"
@test map(lowercase, utf16("TEST\U1f596")) == "test\U1f596"
@test typeof(Base.unsafe_convert(Ptr{UInt16}, utf16("test"))) == Ptr{UInt16}
end
