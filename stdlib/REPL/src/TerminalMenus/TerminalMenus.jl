# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    REPL.TerminalMenus

A module that contains code for displaying text mode interactive menus.
Key exported symbols include [`REPL.TerminalMenus.RadioMenu`](@ref) and
[`REPL.TerminalMenus.MultiSelectMenu`](@ref).
"""
module TerminalMenus

using ..REPL: REPL

function default_terminal(; in::IO=stdin, out::IO=stdout, err::IO=stderr)
    return REPL.Terminals.TTYTerminal(
        get(ENV, "TERM", Sys.iswindows() ? "" : "dumb"), in, out, err)
end

include("util.jl")
include("config.jl")

include("AbstractMenu.jl")
include("RadioMenu.jl")
include("MultiSelectMenu.jl")
include("Pager.jl")

export
    RadioMenu,
    MultiSelectMenu,
    Pager,
    request

public Config, config, MultiSelectConfig
public pick, cancel, writeline, options, numoptions, selected, header, keypress

# TODO: remove in Julia 2.0
# While not exported, AbstractMenu documented these as an extension interface
@deprecate printMenu printmenu
function writeLine(buf::IOBuffer, m::AbstractMenu, idx::Int, cursor::Bool)
    Base.depwarn("`writeLine` is deprecated, use `writeline` instead.", :writeLine)
    error("unimplemented")
end

end # module
