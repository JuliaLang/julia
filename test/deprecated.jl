# This file is a part of Julia. License is MIT: http://julialang.org/license

if (Base.JLOptions()).depwarn > 1
    @test_throws ErrorException Base.utf16_is_surrogate(0xdc00)
    @test_throws ErrorException Base.utf16_is_lead(0xd800)
    @test_throws ErrorException Base.utf16_is_trail(0xdc00)
    @test_throws ErrorException Base.utf16_get_supplementary(0xd800, 0xdc00)
    @test_throws ErrorException Base.is_utf8_start(0x40)
    @test_throws ErrorException Base.is_utf8_continuation(0x90)
else
    olderr = STDERR
    try
        rd, wr = redirect_stderr()
        @test Base.utf16_is_surrogate(0xdc00)
        @test Base.utf16_is_lead(0xd800)
        @test Base.utf16_is_trail(0xdc00)
        @test Base.utf16_get_supplementary(0xd800, 0xdc00) == 0x10000
        @test Base.is_utf8_start(0x40)
        @test Base.is_utf8_continuation(0x90)
    finally
        redirect_stderr(olderr)
    end
end
