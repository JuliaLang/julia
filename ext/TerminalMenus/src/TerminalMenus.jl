module TerminalMenus

import Pkg3.iswindows

terminal = nothing  # The user terminal

function __init__()
    global terminal
    terminal = Base.Terminals.TTYTerminal(get(ENV, "TERM", iswindows() ? "" : "dumb"), STDIN, STDOUT, STDERR)
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

end # module
