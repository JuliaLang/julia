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
