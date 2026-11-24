# This file is a part of Julia. License is MIT: https://julialang.org/license

import REPL
using REPL.TerminalMenus
using Test

function simulate_input(menu::TerminalMenus.AbstractMenu, keys...; kwargs...)
    keydict =  Dict(:up => "\e[A",
                    :down => "\e[B",
                    :enter => "\r")

    new_stdin = Base.BufferStream()
    for key in keys
        if isa(key, Symbol)
            write(new_stdin, keydict[key])
        else
            write(new_stdin, "$key")
        end
    end
    terminal = TerminalMenus.default_terminal(; in=new_stdin, out=devnull)

    return request(terminal, menu; suppress_output=true, kwargs...)
end

@testset "readkey escape sequences" begin
    for (seq, key) in ("\e[1~" => TerminalMenus.HOME_KEY, "\e[7~" => TerminalMenus.HOME_KEY,
                       "\e[H" => TerminalMenus.HOME_KEY, "\e[4~" => TerminalMenus.END_KEY,
                       "\e[8~" => TerminalMenus.END_KEY, "\e[F" => TerminalMenus.END_KEY,
                       "\e[3~" => TerminalMenus.DEL_KEY, "\e[5~" => TerminalMenus.PAGE_UP,
                       "\e[6~" => TerminalMenus.PAGE_DOWN, "\e[C" => TerminalMenus.ARROW_RIGHT,
                       "\e[D" => TerminalMenus.ARROW_LEFT, "\eOA" => TerminalMenus.ARROW_UP,
                       # unknown or incomplete sequences read as a plain escape
                       "\e[9~" => '\e', "\e[Z" => '\e', "\e[2" => '\e', "\eXY" => '\e')
        @test TerminalMenus.readkey(IOBuffer(seq)) == UInt32(key)
    end
end

include("radio_menu.jl")
include("multiselect_menu.jl")
include("dynamic_menu.jl")
include("multiselect_with_skip_menu.jl")
include("pager.jl")

# Legacy tests
include("legacytests/old_radio_menu.jl")
include("legacytests/old_multiselect_menu.jl")
include("legacytests/config.jl")
