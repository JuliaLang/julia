__precompile__(true)
module Pkg3

import Random
import REPL
using REPL.TerminalMenus

depots() = Base.DEPOT_PATH
logdir() = joinpath(depots()[1], "logs")
devdir() = get(ENV, "JULIA_PKG_DEVDIR", joinpath(homedir(), ".julia", "dev"))

# load snapshotted dependencies
include("../ext/TOML/src/TOML.jl")

include("PlatformEngines.jl")
include("Types.jl")
include("Display.jl")
include("Pkg2/Pkg2.jl")
include("GraphType.jl")
include("Resolve.jl")
include("Operations.jl")
include("API.jl")
include("REPLMode.jl")

import .API: add, rm, up, test, gc, init, build, installed, pin, free, checkout, develop, generate
const update = up
import .REPLMode: @pkg_str
export @pkg_str


function __init__()
    if isdefined(Base, :active_repl)
        REPLMode.repl_init(Base.active_repl)
    else
        atreplinit() do repl
            if isinteractive() && repl isa REPL.LineEditREPL
                repl.interface = REPL.setup_interface(repl)
                REPLMode.repl_init(repl)
            end
        end
    end
end

using .Types
using UUIDs
import LibGit2
import Dates
include("precompile.jl")

end # module
