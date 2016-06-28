# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "types" begin
nullstring16 = UInt16[]
badstring16  = UInt16[0x0065]
@test_throws UnicodeError UTF16String(nullstring16)
@test_throws UnicodeError UTF16String(badstring16)

nullstring32 = UInt32[]
badstring32  = UInt32['a']
@test_throws UnicodeError UTF32String(nullstring32)
@test_throws UnicodeError UTF32String(badstring32)

end
