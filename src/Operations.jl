module Operations

using Base.Random: UUID
using Base: LibGit2
using Base: Pkg

using TOML
using TerminalMenus

using Pkg3.Types

function parse_toml(path::String...; fakeit::Bool=false)
    p = joinpath(path...)
    !fakeit || isfile(p) ? TOML.parsefile(p) : Dict{Any,Any}()
end

user_depot() = abspath(homedir(), ".julia")
depots() = Base.Loading.DEPOTS

function registries(depot::String)
    d = joinpath(depot, "registries")
    regs = filter!(readdir(d)) do r
        isfile(joinpath(d, r, "registry.toml"))
    end
    return map(reg->joinpath(depot, "registries", reg), regs)
end
registries() = [r for d in depots() for r in registries(d)]

const line_re = r"""
    ^ \s*
    ([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})
    \s* = \s* \{
    \s* name \s* = \s* "([^"]*)" \s*,
    \s* path \s* = \s* "([^"]*)" \s*,?
    \s* \} \s* $
"""x

function find_registered(names::Vector{String})
    # name --> uuid --> paths
    paths = Dict{String,Dict{UUID,Vector{String}}}()
    isempty(names) && return paths
    names_re = let p = "\\bname\\s*=\\s*\"(?:$(names[1])"
        for i in 2:length(names)
            p *= "|$(names[i])"
        end
        p *= ")\""
        Regex(p)
    end
    for registry in registries()
        packages_file = joinpath(registry, "registry.toml")
        open(packages_file) do io
            for line in eachline(io)
                ismatch(r"^\s*\[\s*packages\s*\]\s*$", line) && break
            end
            for line in eachline(io)
                ismatch(names_re, line) || continue
                m = match(line_re, line)
                m == nothing && error("misformated packages.toml file: $line")
                uuid = UUID(m.captures[1])
                name = Base.unescape_string(m.captures[2])
                path = Base.unescape_string(m.captures[3])
                if !haskey(paths, name)
                    paths[name] = Dict{UUID,Vector{String}}()
                end
                if !haskey(paths[name], uuid)
                    paths[name][uuid] = String[]
                end
                push!(paths[name][uuid], abspath(registry, path))
            end
        end
    end
    return paths
end
find_registered(name::String) = find_registered([name])[name]

function find_installed(uuid::UUID, sha1::SHA1)
    for depot in depots()
        path = abspath(depot, "packages", string(uuid), string(sha1))
        ispath(path) && return path
    end
    return abspath(user_depot(), "packages", string(uuid), string(sha1))
end

include("libgit2_discover.jl")

default_env() = get(ENV, "JULIA_ENV", nothing)
find_config() = find_config(default_env())

const config_names = ["JuliaConfig.toml", "Config.toml"]
const manifest_names = ["JuliaManifest.toml", "Manifest.toml"]
const default_envs = [
    "v$(VERSION.major).$(VERSION.minor).$(VERSION.patch)",
    "v$(VERSION.major).$(VERSION.minor)",
    "v$(VERSION.major)",
    "default",
]

function find_project_env(start_path::String = pwd())
    path = LibGit2.discover(start_path, ceiling = homedir())
    repo = LibGit2.GitRepo(path)
    work = LibGit2.workdir(repo)
    for name in config_names
        path = abspath(work, name)
        isfile(path) && return path
    end
    return abspath(work, config_names[end])
end

function find_default_env()
    for depot in depots(), env in default_envs, name in config_names
        path = joinpath(depot, "environments", env, name)
        isfile(path) && return path
    end
    env = VERSION.major == 0 ? default_envs[2] : default_envs[3]
    return joinpath(user_depot(), "environments", env, config_names[end])
end

function find_config(env::String)
    if isempty(env)
        error("invalid environment name: \"\"")
    elseif env == "/"
        return find_default_env()
    elseif env == "."
        return find_project_env()
    elseif startswith(env, "/") || startswith(env, "./")
        # path to config file or project directory
        splitext(env)[2] == ".toml" && return abspath(env)
        for name in config_names
            path = abspath(env, name)
            isfile(path) && return path
        end
        return abspath(env, config_names[end])
    else # named environment
        for depot in depots()
            path = joinpath(depot, "environments", env, "Config.toml")
            isfile(path) && return path
        end
        return joinpath(user_depot(), "environments", env, "Config.toml")
    end
end

function find_config(::Void)
    try
        return find_project_env()
    catch err
        err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
    end
    return find_default_env()
end

function find_manifest(env::Union{Void,String} = default_env())
    config_file = find_config(env)
    if isfile(config_file)
        config = parse_toml(config_file)
        haskey(config, "manifest") &&
        return abspath(config["manifest"])
    end
    names = ["JuliaManifest.toml", "Manifest.toml"]
    dir = dirname(config_file)
    for name in names
        path = joinpath(dir, name)
        isfile(path) && return path
    end
    return joinpath(dir, names[end])
end

load_config(config_file::String = find_config()) =
    parse_toml(config_file, fakeit=true)

function load_manifest(manifest_file::String = find_manifest())
    manifest = parse_toml(manifest_file, fakeit=true)
    for (name, infos) in manifest, info in infos
        haskey(info, "deps") || continue
        info["deps"] isa AbstractVector || continue
        for dep in info["deps"]
            length(manifest[dep]) == 1 ||
                error("ambiguious dependency for $name: $dep")
        end
        info["deps"] = Dict(d => manifest[d][1]["uuid"] for d in info["deps"])
    end
    return manifest
end

function write_manifest(manifest::Dict, manifest_file::String = find_manifest())
    uniques = sort!(collect(keys(manifest)), by=lowercase)
    filter!(p->length(manifest[p]) == 1, uniques)
    for (_, infos) in manifest, info in infos
        haskey(info, "deps") || continue
        all(d in uniques for d in keys(info["deps"])) || continue
        info["deps"] = sort!(collect(keys(info["deps"])))
    end
    open(manifest_file, "w") do io
        TOML.print(io, manifest, sorted=true)
    end
end

function package_env_info(
    pkg::String,
    config::Dict = load_config(),
    manifest::Dict = load_manifest();
    verb::String = "choose",
)
    haskey(manifest, pkg) || return nothing
    infos = manifest[pkg]
    isempty(infos) && return nothing
    if haskey(config, "deps") && haskey(config["deps"], pkg)
        uuid = config["deps"][pkg]
        filter!(infos) do info
            haskey(info, "uuid") && info["uuid"] == uuid
        end
        length(infos) < 1 &&
            error("manifest has no stanza for $pkg/$uuid")
        length(infos) > 1 &&
            error("manifest has multiple stanzas for $pkg/$uuid")
        return first(infos)
    elseif length(infos) == 1
        return first(infos)
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
        return infos[choice]
    end
end

get_or_make(::Type{T}, d::Dict{K}, k::K) where {T,K} =
    haskey(d, k) ? convert(T, d[k]) : T()

function load_versions(path::String)
    toml = parse_toml(path, "versions.toml")
    Dict(VersionNumber(ver) => SHA1(info["hash-sha1"]) for (ver, info) in toml)
end

function load_package_data(f::Base.Callable, path::String, versions)
    toml = parse_toml(path, fakeit=true)
    data = Dict{VersionNumber,Dict{String,Any}}()
    for ver in versions
        ver::VersionNumber
        for (v, d) in toml, (key, value) in d
            vr = VersionRange(v)
            ver in vr || continue
            dict = get!(data, ver, Dict{String,Any}())
            haskey(dict, key) && error("$ver/$key is duplicated in $path")
            dict[key] = f(value)
        end
    end
    return data
end

load_package_data(f::Base.Callable, path::String, version::VersionNumber) =
    get(load_package_data(f, path, [version]), version, nothing)

function update_config(config::Dict, config_file::String = find_config())
    isempty(config) && !ispath(config_file) && return
    mkpath(dirname(config_file))
    info("Updating config file $config_file")
    open(config_file, "w") do io
        TOML.print(io, config, sorted=true)
    end
end

function update_manifest(manifest::Dict, manifest_file::String = find_manifest())
    isempty(manifest) && !ispath(manifest_file) && return
    info("Updating manifest file $manifest_file")
    write_manifest(manifest, manifest_file)
end

function add(names...; kwargs...)
    pkgs = Dict{String,Union{VersionNumber,VersionRange}}()
    for name in names
        pkgs[string(name)] = vr"*"
    end
    for (key, val) in kwargs
        pkgs[string(key)] = val
    end
    return add(pkgs)
end

function add(pkgs::Dict{String})
    orig_pkgs = copy(pkgs)
    names = sort!(collect(keys(pkgs)))
    regs = find_registered(names)
    config = load_config()
    uuids = get_or_make(Dict{String,UUID}, config, "deps")

    # disambiguate package names
    info("Resolving package UUIDs")
    for name in names
        if haskey(uuids, name)
            uuid = uuids[name]
            if haskey(regs, name)
                for uuid′ in collect(keys(regs[name]))
                    uuid′ == uuid || delete!(regs[name], uuid)
                end
            end
            haskey(regs, name) && !isempty(regs[name]) && continue
            error("""
            $name/$uuid found in config but not in registries;
            To install a different $name package `pkg rm $name` and then do `pkg add $name` again.
            """)
        elseif !haskey(regs, name) || length(regs[name]) == 0
            error("No registered package named $(repr(name)) found")
        elseif length(regs[name]) == 1
            uuids[name] = first(first(regs[name]))
        else
            uuids′ = UUID[]
            options = String[]
            for (i, (uuid, paths)) in enumerate(sort!(collect(regs[name]), by=first))
                option = "$uuid"
                for path in paths
                    info = parse_toml(path, "package.toml")
                    option *= " – $(info["repo"])"
                    break
                end
                push!(uuids′, uuid)
                push!(options, option)
            end
            menu = RadioMenu(options)
            choice = request("Which $name package do you want to add:", menu)
            choice == -1 && return warn("Package add aborted")
            uuids[name] = uuids′[choice]
        end
    end
    merge!(regs, find_registered(setdiff(keys(uuids), names)))
    names = sort!(collect(keys(uuids)))
    paths = isempty(regs) ? Dict{UUID,Vector{String}}() :
        Dict(uuids[name] => info[uuids[name]] for (name, info) in regs)

    # copy manifest versions to pkgs
    # if already in specified version set, leave as installed
    manifest_file = find_manifest()
    manifest = load_manifest(manifest_file)
    for (name, infos) in manifest, info in infos
        info["uuid"] == get(uuids, name, "") || continue
        ver = VersionNumber(info["version"])
        if !haskey(pkgs, name) || ver in pkgs[name]
            pkgs[name] = ver
        end
    end

    info("Resolving package versions")
    # compute reqs & deps for Pkg.Resolve.resolve
    #   reqs: what we need to choose versions for
    #     UUID --> VersionSet
    #   deps: relevant portion of dependency graph
    #     UUID --> VersionNumber --> (SHA1, String --> VersionSet)
    reqs = Dict{UUID,VersionSet}(uuids[name] => vers for (name, vers) in pkgs)
    deps = Dict{UUID,Dict{VersionNumber,Tuple{SHA1,Dict{UUID,VersionSet}}}}()
    ruuids = collect(map(reverse, uuids))
    for (uuid, name) in ruuids
        deps[uuid] = valtype(deps)()
        for path in paths[uuid]
            versions = load_versions(path)
            vs = sort!(collect(keys(versions)))
            dependencies = load_package_data(UUID, joinpath(path, "dependencies.toml"), vs)
            compatibility = load_package_data(VersionSet, joinpath(path, "compatibility.toml"), vs)
            for (v, h) in versions
                d = get_or_make(Dict{String,UUID}, dependencies, v)
                r = get_or_make(Dict{String,VersionSet}, compatibility, v)
                q = Dict(u => get_or_make(VersionSet, r, p) for (p, u) in d)
                VERSION in get_or_make(VersionSet, r, "julia") || continue
                deps[uuid][v] = (h, q)
                for (p, u) in d
                    u in first.(ruuids) && continue
                    reg = find_registered(p)
                    haskey(reg, u) ||
                        error("$p = $u found in $name's dependencies but not in registries")
                    push!(ruuids, u => p)
                    paths[u] = reg[u]
                end
            end
        end
    end

    # actually do the version resloution
    vers = let reqs = convert(Dict{String,Pkg.Types.VersionSet}, reqs),
        deps = convert(Dict{String,Dict{VersionNumber,Pkg.Types.Available}}, deps)
        deps = Pkg.Query.prune_dependencies(reqs, deps)
        vers = Pkg.Resolve.resolve(reqs, deps)
        convert(Dict{UUID,VersionNumber}, vers)
    end

    # find repos and hashes for each package & version
    rud = Dict(ruuids)
    repos = Dict{UUID,Vector{String}}()
    hashes = Dict{UUID,SHA1}()
    for (uuid, ver) in vers
        repos[uuid] = String[]
        for path in paths[uuid]
            package = parse_toml(path, "package.toml")
            repo = package["repo"]
            repo in repos[uuid] || push!(repos[uuid], repo)
            versions = load_versions(path)
            if haskey(versions, ver)
                h = versions[ver]
                if haskey(hashes, uuid)
                    h == hashes[uuid] ||
                        warn("$(rud[uuid]): hash mismatch for version $ver!")
                else
                    hashes[uuid] = h
                end
            end
        end
        @assert haskey(hashes, uuid)
    end
    foreach(sort!, values(repos))

    # clone or update repos and find or create source trees
    refspecs = ["+refs/*:refs/remotes/cache/*"]
    for (uuid, hash) in hashes
        name = rud[uuid]
        version_path = find_installed(uuid, hash)
        ispath(version_path) && continue
        repo_path = joinpath(user_depot(), "upstream", string(uuid))
        urls = copy(repos[uuid])
        git_hash = LibGit2.GitHash(hash.bytes)
        repo = ispath(repo_path) ? LibGit2.GitRepo(repo_path) : begin
            info("Cloning [$uuid] $name")
            LibGit2.clone(shift!(urls), repo_path, isbare=true)
        end
        while !isempty(urls)
            try
                LibGit2.GitObject(repo, git_hash)
                break
            catch err
                err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
            end
            info("Updating $name from $(urls[1])")
            LibGit2.fetch(repo, remoteurl=shift!(urls), refspecs=refspecs)
        end
        tree = try
            LibGit2.GitObject(repo, git_hash)
        catch err
            err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
            error("$name: git object $(string(hash)) could not be found")
        end
        tree isa LibGit2.GitTree ||
            error("$name: git object $(string(hash)) should be a tree, not $(typeof(tree))")
        mkpath(version_path)
        opts = LibGit2.CheckoutOptions(
            checkout_strategy = LibGit2.Consts.CHECKOUT_FORCE,
            target_directory = Base.unsafe_convert(Cstring, version_path)
        )
        info("Installing $name at $(string(hash))")
        LibGit2.checkout_tree(repo, tree, options=opts)
    end

    # update config and manifest files
    config_file = find_config()
    config = parse_toml(config_file, fakeit=true)
    if !haskey(config, "deps")
        config["deps"] = Dict{String,String}()
    end
    for (name, uuid) in uuids
        config["deps"][name] = string(uuid)
    end
    isempty(config["deps"]) && delete!(config, "deps")

    for (uuid, ver) in vers
        name = rud[uuid]
        info = nothing
        a = get!(manifest, name, Dict{String,Any}[])
        for i in a
            UUID(i["uuid"]) == uuid || continue
            info = i
            break
        end
        if info == nothing
            info = Dict{String,Any}("uuid" => string(uuid))
            push!(a, info)
        end
        info["version"] = string(vers[uuid])
        info["hash-sha1"] = string(hashes[uuid])
        for path in paths[uuid]
            d = load_package_data(UUID, joinpath(path, "dependencies.toml"), ver)
            d == nothing && continue
            info["deps"] = convert(Dict{String,String}, d)
            break
        end
    end

    update_config(config, config_file)
    update_manifest(manifest, manifest_file)
end

function rm(pkgs::Vector{String})
    config_file = find_config()
    config = load_config(config_file)
    manifest_file = find_manifest()
    manifest = load_manifest(manifest_file)
    # drop named packages
    drop = String[]
    for pkg in pkgs
        info = package_env_info(pkg, config, manifest, verb = "delete")
        info == nothing && error("$pkg not found in environment")
        push!(drop, info["uuid"])
    end
    # also drop reverse dependencies
    while !isempty(drop)
        clean = true
        for (pkg, infos) in manifest, info in infos
            haskey(info, "deps") || continue
            isempty(drop ∩ values(info["deps"])) && continue
            haskey(info, "uuid") && info["uuid"] ∉ drop || continue
            push!(drop, info["uuid"])
            clean = false
        end
        clean && break
    end
    # keep forward dependencies
    keep = setdiff(values(get(config, "deps", Dict())), drop)
    while !isempty(keep)
        clean = true
        for (pkg, infos) in manifest
            for (pkg, infos) in manifest, info in infos
                haskey(info, "uuid") && haskey(info, "deps") || continue
                info["uuid"] in keep || continue
                for dep in values(info["deps"])
                    dep in keep && continue
                    push!(keep, dep)
                    clean = false
                end
            end
        end
        clean && break
    end
    # filter config & manifest
    if haskey(config, "deps")
        filter!(config["deps"]) do _, uuid
            uuid in keep
        end
        isempty(config["deps"]) && delete!(config, "deps")
    end
    filter!(manifest) do pkg, infos
        filter!(infos) do info
            haskey(info, "uuid") && info["uuid"] in keep
        end
        !isempty(infos)
    end
    # update config & manifest files
    update_config(config, config_file)
    update_manifest(manifest, manifest_file)
end
rm(pkgs::String...) = rm(String[pkgs...])

end # module
