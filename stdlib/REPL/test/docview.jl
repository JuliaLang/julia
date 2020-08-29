# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
import REPL

@testset "symbol completion" begin
    @test startswith(let buf = IOBuffer()
            Core.eval(Main, REPL.helpmode(buf, "α"))
            String(take!(buf))
        end, "\"α\" can be typed by \\alpha<tab>\n")

    @test startswith(let buf = IOBuffer()
            Core.eval(Main, REPL.helpmode(buf, "🐨"))
            String(take!(buf))
        end, "\"🐨\" can be typed by \\:koala:<tab>\n")
end
