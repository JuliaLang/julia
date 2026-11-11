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

include("radio_menu.jl")
include("multiselect_menu.jl")
include("dynamic_menu.jl")
include("multiselect_with_skip_menu.jl")
include("pager.jl")

# Legacy tests
include("legacytests/old_radio_menu.jl")
include("legacytests/old_multiselect_menu.jl")
include("legacytests/config.jl")

@testset "readkey ESC H and ESC F" begin
    for (seq, key) in ("\eH" => TerminalMenus.HOME_KEY, "\eF" => TerminalMenus.END_KEY,
                       "\eOH" => TerminalMenus.HOME_KEY, "\eOF" => TerminalMenus.END_KEY)
        io = IOBuffer(seq * "x")
        @test TerminalMenus.readkey(io) == UInt32(key)
        @test read(io, Char) == 'x'
        @test TerminalMenus.readkey(IOBuffer(seq)) == UInt32(key)
    end
end
