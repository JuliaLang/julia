module Loading

using TerminalMenus

using Pkg3.Operations:
    parse_toml,
    user_depot,
    load_config,
    load_manifest,
    find_registered

abstract type Loader end

struct LoadInstalled <: Loader
    depot::String
end

function Base.load_hook(loader::LoadInstalled, pkg::String, prev)
    # load and parse the package config & manifest
    config   = load_config()
    manifest = load_manifest()
    infos = haskey(manifest, pkg) ? manifest[pkg] : return prev
    isempty(infos) && return prev # TODO: maybe warn or error?
    # look up package name
    if haskey(config, "deps") && haskey(config["deps"], pkg)
        uuid = config["deps"][pkg]
        filter!(infos) do info
            haskey(info, "uuid") && info["uuid"] == uuid
        end
        length(infos) < 1 &&
            error("manifest has no stanza for $pkg/$uuid")
        length(infos) > 1 &&
            error("manifest has multiple stanzas for $pkg/$uuid")
        info = first(infos)
    elseif length(infos) == 1
        info = first(infos)
    else
        options = String[]
        paths = convert(Dict{String,Vector{String}}, find_registered(pkg))
        for info in infos
            uuid = info["uuid"]
            option = uuid
            if haskey(paths, uuid)
                for path in paths[uuid]
                    info′ = parse_toml(path, "package.toml")
                    option *= " – $(info′["repo"])"
                    break
                end
            else
                option *= " – (unregistred)"
            end
            push!(options, option)
        end
        menu = RadioMenu(options)
        choice = request("Which $pkg package do you want to use:", menu)
        choice == -1 && error("Package load aborted")
        info = infos[choice]
    end
    # load the chosen package UUID at hash version in the manifest
    uuid = info["uuid"]
    hash = info["hash-sha1"]
    path = joinpath(uuid, hash, "src", "$pkg.jl")
    # see if previously found matches expected suffix
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
