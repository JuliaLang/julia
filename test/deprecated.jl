# This file is a part of Julia. License is MIT: http://julialang.org/license

if (Base.JLOptions()).depwarn > 1
    @test_throws ErrorException Base.utf16_is_surrogate(0xdc00)
    @test_throws ErrorException Base.utf16_is_lead(0xd800)
    @test_throws ErrorException Base.utf16_is_trail(0xdc00)
    @test_throws ErrorException Base.utf16_get_supplementary(0xd800, 0xdc00)
else
    olderr = STDERR
    try
        rd, wr = redirect_stderr()
        @test Base.utf16_is_surrogate(0xdc00) == true
        @test Base.utf16_is_lead(0xd800) == true
        @test Base.utf16_is_trail(0xdc00) == true
        @test Base.utf16_get_supplementary(0xd800, 0xdc00) == 0x10000
    finally
        redirect_stderr(olderr)
    end
end