# This file is a part of Julia. License is MIT: https://julialang.org/license

module TerminalMenus

terminal = nothing  # The user terminal

import REPL

function __init__()
    global terminal
    terminal = REPL.Terminals.TTYTerminal(get(ENV, "TERM", Sys.iswindows() ? "" : "dumb"), stdin, stdout, stderr)
end

include("util.jl")
include("config.jl")

include("AbstractMenu.jl")
include("RadioMenu.jl")
include("MultiSelectMenu.jl")

export
    RadioMenu,
    MultiSelectMenu,
    request

# TODO: remove in Julia 2.0
# While not exported, AbstractMenu documented these as an extension interface
@deprecate printMenu printmenu
function writeLine(buf::IOBuffer, m::AbstractMenu, idx::Int, cursor::Bool)
    Base.depwarn("`writeLine` is deprecated, use `writeline` instead.", :writeLine)
    error("unimplemented")
end

end # module
