__precompile__(true)
module Pkg3

const DEPOTS = [joinpath(homedir(), ".julia")]
depots() = DEPOTS

const USE_LIBGIT2_FOR_ALL_DOWNLOADS = false
const NUM_CONCURRENT_DOWNLOADS      = 8

iswindows() = @static VERSION < v"0.7-" ? Sys.is_windows() : Sys.iswindows()
isapple()   = @static VERSION < v"0.7-" ? Sys.is_apple()   : Sys.isapple()
islinux()   = @static VERSION < v"0.7-" ? Sys.is_linux()   : Sys.islinux()

# load snapshotted dependencies
include("../ext/BinaryProvider/src/BinaryProvider.jl")
include("../ext/TOML/src/TOML.jl")
include("../ext/TerminalMenus/src/TerminalMenus.jl")

include("Pkg2/Pkg2.jl")
include("Types.jl")
include("Display.jl")
include("Operations.jl")
include("REPLMode.jl")
include("API.jl")

@enum LoadErrorChoice LOAD_ERROR_QUERY LOAD_ERROR_INSTALL LOAD_ERROR_ERROR

Base.@kwdef mutable struct GlobalSettings
    load_error_choice::LoadErrorChoice = LOAD_ERROR_QUERY # query, install, or error, when not finding package on import
end

GLOBAL_SETTINGS = GlobalSettings()

function __init__()
    push!(empty!(LOAD_PATH), dirname(dirname(@__DIR__)))
    isdefined(Base, :active_repl) && REPLMode.repl_init(Base.active_repl)
end

function Base.julia_cmd(julia::AbstractString)
    cmd = invoke(Base.julia_cmd, Tuple{Any}, julia)
    push!(cmd.exec, "-L$(abspath(@__DIR__, "require.jl"))")
    return cmd
end

if VERSION < v"0.7.0-DEV.2303"
    Base.find_in_path(name::String, wd::Void)   = _find_package(name, nothing)
    Base.find_in_path(name::String, wd::String) = _find_package(name, nothing)
else
    Base.find_package(name::String) = _find_package(name)
end

function _find_package(name::String, wd::Union{Void,String})
    isabspath(name) && return name
    base = name
    if endswith(name, ".jl")
        base = name[1:end-3]
    else
        name = string(base, ".jl")
    end

    info = Pkg3.Operations.package_env_info(base, verb = "use")
    info == nothing && @goto find_global
    haskey(info, "uuid") || @goto find_global
    haskey(info, "hash-sha1") || @goto find_global
    uuid = Base.Random.UUID(info["uuid"])
    hash = Pkg3.Types.SHA1(info["hash-sha1"])
    path = Pkg3.Operations.find_installed(uuid, hash)
    ispath(path) && return joinpath(path, "src", name)

    # If we still haven't found the file, look if the package exists in the registry
    # and query the user (if we are interactive) to install it.
    @label find_global
    if isinteractive()
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
    return nothing
end


end # module
