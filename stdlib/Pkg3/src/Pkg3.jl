__precompile__(true)
module Pkg3

import Random
import REPL
using REPL.TerminalMenus

const _depots = [joinpath(homedir(), ".julia")]
depots() = _depots
logdir() = joinpath(depots()[1], "logs")

# load snapshotted dependencies
include("../ext/TOML/src/TOML.jl")

include("PlatformEngines.jl")
include("Types.jl")
include("GraphType.jl")
include("Resolve.jl")
include("Display.jl")
include("Operations.jl")
include("REPLMode.jl")
include("API.jl")

import .API: add, rm, up, test, gc, init, build, installed
const update = up

function __init__()
    if isdefined(Base, :active_repl)
        REPLMode.repl_init(Base.active_repl)
    else
        atreplinit() do repl
            repl.interface = REPL.setup_interface(repl)
            REPLMode.repl_init(repl)
        end
    end
end

# TODO: Hook this up to the code loading and make it execute
# when in interactive mode and a package is not found
function query_if_interactive(base, name)
    ctx = Types.Context()
    pkgspec = [Types.PackageSpec(base)]

    r = Operations.registry_resolve!(ctx.env, pkgspec)
    Types.has_uuid(r[1]) || return nothing

    ctx.load_error_choice == LOAD_ERROR_INSTALL && @goto install
    ctx.load_error_choice == LOAD_ERROR_ERROR   && return nothing

    choice = TerminalMenus.request("Could not find package \e[1m$(base)\e[22m, do you want to install it?",
                   TerminalMenus.RadioMenu(["yes", "yes (remember)", "no", "no (remember)"]))

    if choice == 3 || choice == 4
        choice == 4 && (ctx.load_error_choice = LOAD_ERROR_ERROR)
        return nothing
    end

    choice == 2 && (ctx.load_error_choice = LOAD_ERROR_INSTALL)
    @label install
    Pkg3.Operations.ensure_resolved(ctx.env, pkgspec, true)
    Pkg3.Operations.add(ctx, pkgspec)
    return _find_package(name)
end

end # module
