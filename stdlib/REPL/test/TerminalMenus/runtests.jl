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

# Other test

# scroll must only accept symbols
@test_throws TypeError TerminalMenus.config(scroll=true)
# :foo is not a valid scroll option
@test_throws ArgumentError TerminalMenus.config(scroll=:foo)
# Test scroll wrap
TerminalMenus.config(scroll=:wrap)
@test TerminalMenus.CONFIG[:scroll_wrap] == true
# Updating some params shouldn't change other ones
TerminalMenus.config(charset=:ascii)
@test TerminalMenus.CONFIG[:scroll_wrap] == true
TerminalMenus.config(scroll=:nowrap)
@test TerminalMenus.CONFIG[:scroll_wrap] == false

# Legacy tests
include("legacytests/old_radio_menu.jl")
include("legacytests/old_multiselect_menu.jl")
