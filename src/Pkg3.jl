__precompile__(true)
module Pkg3

import Random

@enum LoadErrorChoice LOAD_ERROR_QUERY LOAD_ERROR_INSTALL LOAD_ERROR_ERROR

Base.@kwdef mutable struct GlobalSettings
    load_error_choice::LoadErrorChoice = LOAD_ERROR_QUERY # query, install, or error, when not finding package on import
    use_libgit2_for_all_downloads::Bool = false
    num_concurrent_downloads::Int = 8
    depots::Vector{String} = [joinpath(homedir(), ".julia")]
end

const GLOBAL_SETTINGS = GlobalSettings()

depots() = GLOBAL_SETTINGS.depots
logdir() = joinpath(depots()[1], "logs")

# load snapshotted dependencies
include("../ext/TOML/src/TOML.jl")
include("../ext/TerminalMenus/src/TerminalMenus.jl")

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
    BinaryProvider.probe_platform_engines!()
    if isdefined(Base, :active_repl)
        REPLMode.repl_init(Base.active_repl)
    else
        atreplinit() do repl
            repl.interface = Base.REPL.setup_interface(repl)
            REPLMode.repl_init(repl)
        end
    end
end

function _query_if_interactive(base, name)
    env = Types.EnvCache()
    pkgspec = [Types.PackageSpec(base)]

    r = Operations.registry_resolve!(env, pkgspec)
    Types.has_uuid(r[1]) || return nothing

    GLOBAL_SETTINGS.load_error_choice == LOAD_ERROR_INSTALL && @goto install
    GLOBAL_SETTINGS.load_error_choice == LOAD_ERROR_ERROR   && return nothing

    choice = TerminalMenus.request("Could not find package \e[1m$(base)\e[22m, do you want to install it?",
                   TerminalMenus.RadioMenu(["yes", "yes (remember)", "no", "no (remember)"]))

    if choice == 3 || choice == 4
        choice == 4 && (GLOBAL_SETTINGS.load_error_choice = LOAD_ERROR_ERROR)
        return nothing
    end

    choice == 2 && (GLOBAL_SETTINGS.load_error_choice = LOAD_ERROR_INSTALL)
    @label install
    Pkg3.Operations.ensure_resolved(env, pkgspec, true)
    Pkg3.Operations.add(env, pkgspec)
    return _find_package(name)
end

const query_if_interactive = Ref{Any}(_query_if_interactive)

end # module
