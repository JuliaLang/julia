module Pkg3

const DEPOTS = [joinpath(homedir(), ".julia")]
depots() = DEPOTS

# load snapshotted dependencies
include("../ext/TOML/src/TOML.jl")
include("../ext/TerminalMenus/src/TerminalMenus.jl")

include("Types.jl")
include("Display.jl")
include("Operations.jl")
include("REPLMode.jl")

__init__() = isdefined(Base, :active_repl) &&
    REPLMode.repl_init(Base.active_repl)

function Base.julia_cmd(julia::AbstractString)
    cmd = invoke(Base.julia_cmd, Tuple{Any}, julia)
    push!(cmd.exec, "-L$(abspath(@__DIR__, "require.jl"))")
    return cmd
end

function _find_in_path(name::String, wd::Union{Void,String})
    isabspath(name) && return name
    base = name
    if endswith(name, ".jl")
        base = name[1:end-3]
    else
        name = string(base, ".jl")
    end
    if wd !== nothing
        path = joinpath(wd, name)
        Base.isfile_casesensitive(path) && return path
    end
    info = Pkg3.Operations.package_env_info(base, verb = "use")
    info == nothing && return nothing
    haskey(info, "uuid") || return nothing
    haskey(info, "hash-sha1") || return nothing
    uuid = Base.Random.UUID(info["uuid"])
    hash = Pkg3.Types.SHA1(info["hash-sha1"])
    path = Pkg3.Operations.find_installed(uuid, hash)
    ispath(path) ? joinpath(path, "src", name) : nothing
end

Base.find_in_path(name::String, wd::Void) = _find_in_path(name, wd)
Base.find_in_path(name::String, wd::String) = _find_in_path(name, wd)

end # module
