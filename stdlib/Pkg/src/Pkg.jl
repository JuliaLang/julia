__precompile__(true)
module Pkg

using Dates

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

# Backport of Equalto
if !isdefined(Base, :EqualTo)
    struct EqualTo{T} <: Function
        x::T
        EqualTo(x::T) where {T} = new{T}(x)
    end
    (f::EqualTo)(y) = isequal(f.x, y)
    const equalto = EqualTo
end

# load snapshotted dependencies
include("../ext/SHA/src/SHA.jl")
include("../ext/BinaryProvider/src/BinaryProvider.jl")
include("../ext/TOML/src/TOML.jl")
include("../ext/TerminalMenus/src/TerminalMenus.jl")

include("Types.jl")
include("Query.jl")
include("Resolve.jl")
include("Display.jl")
include("Operations.jl")
include("REPLMode.jl")
include("API.jl")

import .API: add, rm, up, test, gc, init, dir
const update = up

function __init__()
    vers = "v$(VERSION.major).$(VERSION.minor)"
    unshift!(Base.LOAD_CACHE_PATH, abspath(depots()[1], "lib", vers))
    Base.__Pkg.find_package[] = _find_package
    #Base.__Pkg.installed[] = installed
    Base.__Pkg.dir[] = dir

    if isdefined(Base, :active_repl)
        REPLMode.repl_init(Base.active_repl)
    else
        atreplinit() do repl
            repl.interface = Base.REPL.setup_interface(repl)
            REPLMode.repl_init(repl)
        end
    end
end

function _find_package(name::String)
    isabspath(name) && return name
    base = name
    if endswith(name, ".jl")
        base = name[1:end-3]
    else
        name = string(base, ".jl")
    end
    info = Pkg.Operations.package_env_info(base)
    info == nothing && return @goto find_global
    haskey(info, "uuid") || @goto find_global
    haskey(info, "hash-sha1") || @goto find_global
    uuid = Base.Random.UUID(info["uuid"])
    hash = Pkg.Types.SHA1(info["hash-sha1"])
    path = Pkg.Operations.find_installed(uuid, hash)
    ispath(path) && return joinpath(path, "src", name)

    # If we still haven't found the file, look if the package exists in the registry
    # and query the user (if we are interactive) to install it.
    @label find_global
    if isinteractive()
        # Hide this from inference
        return query_if_interactive[](base, name)::Union{Void, String}
    else
        return nothing
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
    Pkg.Operations.ensure_resolved(env, pkgspec, true)
    Pkg.Operations.add(env, pkgspec)
    return _find_package(name)
end

const query_if_interactive = Ref{Any}(_query_if_interactive)

include("precompile_pkg.jl")
_precompile_()
end # module
