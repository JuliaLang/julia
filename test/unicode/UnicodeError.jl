# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "invalid utf8" begin
    let io = IOBuffer()
        show(io, UnicodeError(Base.UTF_ERR_SHORT, 1, 10))
        check = "UnicodeError: invalid UTF-8 sequence starting at index 1 (0xa missing one or more continuation bytes)"
        @test String(take!(io)) == check
    end
end
