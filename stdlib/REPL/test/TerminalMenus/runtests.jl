# This file is a part of Julia. License is MIT: https://julialang.org/license

import REPL
using REPL.TerminalMenus
using Test

function simulate_input(expected, menu::TerminalMenus.AbstractMenu, keys...)
    keydict =  Dict(:up => "\e[A",
                    :down => "\e[B",
                    :enter => "\r")

    for key in keys
        if isa(key, Symbol)
            write(stdin.buffer, keydict[key])
        else
            write(stdin.buffer, "$key")
        end
    end

    request(menu; suppress_output=true) == expected
end

include("radio_menu.jl")
include("multiselect_menu.jl")
include("dynamic_menu.jl")

# Legacy tests
include("legacytests/old_radio_menu.jl")
include("legacytests/old_multiselect_menu.jl")
include("legacytests/config.jl")
