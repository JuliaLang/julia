__precompile__()
module TerminalMenus
using Compat

terminal = nothing  # The user terminal

function __init__()
    global terminal
    terminal = Base.Terminals.TTYTerminal(get(ENV, "TERM", Sys.iswindows() ? "" : "dumb"), Base.stdin, Base.stdout, Base.stderr)
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
