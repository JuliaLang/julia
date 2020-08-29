# This file is a part of Julia. License is MIT: https://julialang.org/license

import REPL

@test startswith(let buf = IOBuffer()
        Core.eval(Main, REPL.helpmode(buf, "α"))
        String(take!(buf))
    end, "\"α\" can be typed by \\alpha<tab>\n")

@test startswith(let buf = IOBuffer()
        Core.eval(Main, REPL.helpmode(buf, "🐨"))
        String(take!(buf))
    end, "\"🐨\" can be typed by \\coala<tab>\n")
