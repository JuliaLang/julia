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
    TerminalMenus.terminal.in_stream = new_stdin

    return request(menu; suppress_output=true, kwargs...)
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
