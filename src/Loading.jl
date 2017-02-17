module Loading

using TOML
using Pkg3.Operations: user_depot, load_manifest

abstract type Loader end

struct LoadInstalled <: Loader
    depot::String
end

function Base.load_hook(loader::LoadInstalled, name::String, found)
    # load and parse the package manifest
    manifest = load_manifest()
    # pull the package UUID and verion hash out of the manifest
    package  = haskey(manifest, name)       ? manifest[name]       : return found
    uuid     = haskey(package, "uuid")      ? package["uuid"]      : return found
    hash     = haskey(package, "hash-sha1") ? package["hash-sha1"] : return found
    # relative path for package and version
    path = joinpath(uuid, hash, "src", "$name.jl")
    # see if previously found matches expected suffix
    found isa String && endswith(found, path) && return found
    # see if package and version is installed in the current depot
    path = joinpath(loader.depot, "packages", path)
    # if it is, return it, otherwise pass through
    return isfile(path) ? path : found
end

empty!(LOAD_PATH)
ENV["JULIA_PKGDIR"] = tempname()
@eval Base module Loading; DEPOTS = []; end
push!(LOAD_PATH, LoadInstalled(user_depot()))
push!(Base.Loading.DEPOTS, user_depot())

end # module
