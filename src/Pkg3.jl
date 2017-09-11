module Pkg3

const DEPOTS = [joinpath(homedir(), ".julia")]
depots() = DEPOTS

include("Types.jl")
include("Operations.jl")
include("REPLMode.jl")
isdefined(Base, :active_repl) &&
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
    uuid = String(Base.Random.UUID(info["uuid"]))
    hash = String(Pkg3.Types.SHA1(info["hash-sha1"]))
    path = joinpath("packages", uuid, hash, "src", name)
    for depot in depots()
        file = joinpath(depot, path)
        ispath(file) && return file
    end
    return nothing
end

Base.find_in_path(name::String, wd::Void) = _find_in_path(name, wd)
Base.find_in_path(name::String, wd::String) = _find_in_path(name, wd)

end # module
