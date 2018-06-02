# This file is a part of Julia. License is MIT: https://julialang.org/license

module TerminalsTest
    using Test
    using REPL.Terminals
    @testset "Terminals.supports_color" begin
        test_supports_color(typ) = supports_color(TTYTerminal(typ, devnull, devnull, devnull))
        @test test_supports_color("ansi") == (Sys.KERNEL !== :FreeBSD) # tput appears to be broken on FreeBSD
        @test test_supports_color("xterm")
        @test test_supports_color("xterm-256color")
        @test !test_supports_color("dumb")
        @test !test_supports_color("")
        @test test_supports_color("vt100") == Sys.iswindows()
        @test test_supports_color("xxx_unknown_xxx") == Sys.iswindows()
    end
end
module REPLTests
    include("repl.jl")
end
module REPLCompletionsTest
    include("replcompletions.jl")
end
module LineEditTest
    include("lineedit.jl")
end
module TerminalMenusTest
    include("TerminalMenus/runtests.jl")
end
