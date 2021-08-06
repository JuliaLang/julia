# This file is a part of Julia. License is MIT: https://julialang.org/license

import REPL
using REPL.TerminalMenus
using Test

function simulate_input(expected, menu::TerminalMenus.AbstractMenu, keys...;
                        kwargs...)
    keydict = Dict(:up => "\e[A",
                   :down => "\e[B",
                   :enter => "\r")
    vimdict = Dict(:up => "k",
                   :down => "j",
                   :enter => " ")
    errs = []
    got = _simulate_input(keydict, deepcopy(menu), keys...; kwargs...)
    got == expected || push!(errs, :arrows => got)
    got = _simulate_input(vimdict, menu, keys...; kwargs...)
    got == expected || push!(errs, :vim => got)
    isempty(errs) || return errs
end

function _simulate_input(keydict, menu::TerminalMenus.AbstractMenu, keys...;
                         kwargs...)
    for key in keys
        if isa(key, Symbol)
            write(stdin.buffer, keydict[key])
        else
            write(stdin.buffer, "$key")
        end
    end

    request(menu; suppress_output=true, kwargs...)
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
