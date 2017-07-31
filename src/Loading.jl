module Loading

using TerminalMenus

using Pkg3.Operations: user_depot, package_env_info

abstract type Loader end

struct LoadInstalled <: Loader
    depot::String
end

function Base.load_hook(loader::LoadInstalled, pkg::String, prev)
    # look for the package name in the current environement
    # prompting the user for a choice if necessary
    info = package_env_info(pkg, verb = "use")
    # if package not found in env, pass through prev
    info == nothing && return prev
    # compute a path suffix for the package
    uuid = info["uuid"]
    hash = info["hash-sha1"]
    path = joinpath(uuid, hash, "src", "$pkg.jl")
    # see if previously found matches expected suffix
    # this effectively prefers earlier depots but lets Pkg3
    # loading override package found via LOAD_PATH otherwise
    prev isa String && endswith(prev, path) && return prev
    # see if package and version is installed in the current depot
    path = joinpath(loader.depot, "packages", path)
    # if it is, return it, otherwise pass through
    return isfile(path) ? path : prev
end

empty!(LOAD_PATH)
ENV["JULIA_PKGDIR"] = tempname()
@eval Base module Loading; DEPOTS = []; end
push!(LOAD_PATH, LoadInstalled(user_depot()))
push!(Base.Loading.DEPOTS, user_depot())

end # module
