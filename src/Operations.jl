module Operations

using TOML
using Base.Random: UUID
using Base: LibGit2
using Base: Pkg
using Base.Pkg.Types: VersionSet
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
    where = Dict{String,Dict{UUID,Vector{String}}}()
    isempty(names) && return where
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
                if !haskey(where, name)
                    where[name] = Dict{UUID,Vector{String}}()
                end
                if !haskey(where[name], uuid)
                    where[name][uuid] = String[]
                end
                push!(where[name][uuid], abspath(registry, path))
            end
        end
    end
    return where
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

function find_manifest(env::Union{Void,String}=default_env())
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

function load_manifest(env::Union{Void,String}=default_env())
    manifest = find_manifest(env)
    T = Dict{String,Dict{String,String}}
    return isfile(manifest) ? convert(T, parse_toml(manifest)) : T()
end

inspec(v::VersionNumber, s::VersionSpec) = v in s
inspec(v::VersionNumber, s::VersionNumber) = v == s

function parse_version_set(s::String)
    parts = split(s, '-')
    length(parts) == 1 && return VersionSet(VersionSpec(parts[1]))
    length(parts) != 2 && error("invalid version spec: ", repr(s))
    lower = VersionSet(VersionSpec(parts[1])).intervals[1].lower
    upper = VersionSet(VersionSpec(parts[2])).intervals[1].upper
    return VersionSet(lower, upper)
end

function add(names...; kwargs...)
    pkgs = Dict{String,Union{VersionNumber,VersionSpec}}()
    for name in names
        pkgs[string(name)] = vs"*"
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
    manifest = load_manifest()
    uuids = Dict{String,UUID}(name => info["uuid"] for (name, info) in manifest)
    # disambiguate package names
    info("Resolving package UUIDs")
    let ambig = false
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
                $name/$uuid found in manifest but not in registries;
                To install a different $name package `pkg rm $name` and then do `pkg add $name` again.
                """)
            elseif length(regs[name]) == 1
                uuids[name] = first(first(regs[name]))
            else
                msg = "$name is ambiguous, it could refer to:\n"
                for (i, (uuid, paths)) in enumerate(sort!(collect(regs[name]), by=first))
                    msg *= " [$i] $uuid"
                    for path in paths
                        info = parse_toml(path, "package.toml")
                        msg *= " – $(info["repo"])"
                        break
                    end
                    msg *= "\n"
                end
                info(msg)
                ambig = true
            end
        end
        ambig && error("interactive package choice not yet implemented")
    end
    merge!(regs, find_registered(setdiff(keys(uuids), names)))
    where = isempty(regs) ? Dict{String,Vector{String}}() :
        Dict(name => paths for (name, info) in regs for (uuid, paths) in info)

    # copy manifest versions to pkgs
    # if already in specified version set, leave as installed
    for (name, info) in manifest
        ver = VersionNumber(info["version"])
        if !haskey(pkgs, name) || inspec(ver, pkgs[name])
            pkgs[name] = ver
        end
    end

    # compute reqs & deps for Pkg.Resolve.resolve
    #  - reqs: what we need to choose versions for
    #  - deps: relevant portion of dependency graph

    # reqs :: String --> VersionSet
    reqs = convert(Pkg.Types.Requires, pkgs)

    info("Resolving package versions")
    # deps :: String --> VersionNumber --> (SHA1, String --> VersionSet)
    deps = Dict{String,Dict{VersionNumber,Pkg.Types.Available}}()
    names = sort!(collect(keys(uuids)))
    for name in names
        spec = get(pkgs, name, VersionSpec())
        uuid, paths = uuids[name], where[name]
        deps[name] = Dict{VersionNumber,Pkg.Types.Available}()
        for path in paths
            versions = parse_toml(path, "versions.toml")
            requires = parse_toml(path, "requirements.toml", fakeit=true)
            for (v, d) in versions
                ver = VersionNumber(v)
                inspec(ver, spec) || continue
                r = haskey(requires, v) ?
                    Dict(dep => parse_version_set(r) for (dep, r) in requires[v]) :
                    Pkg.Types.Requires()
                if haskey(r, "julia")
                    VERSION in r["julia"] || continue
                    delete!(r, "julia")
                end
                x = deps[name][ver] = Pkg.Types.Available(SHA1(d["hash-sha1"]), r)
                for dep in keys(x.requires)
                    (dep in names || dep == "julia") && continue
                    found = find_registered(dep)
                    @assert length(found) == 1 # TODO: use UUIDs to disambiguate
                    uuids[dep] = first(found)[1]
                    where[dep] = first(found)[2]
                    push!(names, dep)
                end
            end
        end
    end
    deps = Pkg.Query.prune_dependencies(reqs, deps)
    vers = Pkg.Resolve.resolve(reqs, deps)

    # find repos and hashes for each package & version
    repos = Dict{String,Vector{String}}()
    hashes = Dict{String,SHA1}()
    for (name, ver) in vers
        repos[name] = String[]
        v = string(ver)
        for path in where[name]
            package = parse_toml(path, "package.toml")
            repo = package["repo"]
            repo in repos[name] || push!(repos[name], repo)
            versions = parse_toml(path, "versions.toml")
            if haskey(versions, v)
                sha1 = SHA1(versions[v]["hash-sha1"])
                if haskey(hashes, name)
                    sha1 == hashes[name] || warn("$name: hash mismatch for version $v!")
                else
                    hashes[name] = sha1
                end
            end
        end
        @assert haskey(hashes, name)
    end
    foreach(sort!, values(repos))

    # clone or update repos and find or create source trees
    for (name, sha1) in hashes
        uuid = uuids[name]
        version_path = find_installed(uuid, sha1)
        ispath(version_path) && continue
        repo_path = joinpath(user_depot(), "upstream", string(uuid))
        urls = copy(repos[name])
        git_hash = LibGit2.GitHash(sha1.bytes)
        repo = ispath(repo_path) ? LibGit2.GitRepo(repo_path) : begin
            info("Cloning [$uuid] $name")
            LibGit2.clone(shift!(urls), repo_path, isbare=true)
        end
        refspecs = ["+refs/*:refs/remotes/cache/*"]
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
        obj = try LibGit2.GitObject(repo, git_hash)
        catch err
            err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
            error("$name: git object $(string(sha1)) could not be found")
        end
        tree = LibGit2.peel(LibGit2.GitTree, obj)
        sha1′ = SHA1(string(LibGit2.GitHash(tree)))
        if sha1 != sha1′
            # SHA1 was a commit instead of tree
            sha1 = hashes[name] = sha1′
            version_path = find_installed(uuid, sha1)
            ispath(version_path) && continue
        end
        mkpath(version_path)
        opts = LibGit2.CheckoutOptions(
            checkout_strategy = LibGit2.Consts.CHECKOUT_FORCE,
            target_directory = Base.unsafe_convert(Cstring, version_path)
        )
        info("Installing $name at $(string(sha1))")
        LibGit2.checkout_tree(repo, tree, options=opts)
    end

    # update config and manifest files
    config_file = find_config()
    config = parse_toml(config_file, fakeit=true)
    if !haskey(config, "deps")
        config["deps"] = Dict{String,String}()
    end
    for (name, sha1) in hashes
        uuid = string(uuids[name])
        version = string(vers[name])
        config["deps"][name] = uuid
        manifest[name] = Dict{String,String}(
            "uuid"      => uuid,
            "hash-sha1" => sha1,
            "version"   => version,
        )
    end
    isempty(config["deps"]) && delete!(config, "deps")

    if !isempty(config) || ispath(config_file)
        mkpath(dirname(config_file))
        info("Updating config file $config_file")
        open(config_file, "w") do io
            TOML.print(io, config, sorted=true)
        end
    end
    manifest_file = find_manifest()
    if !isempty(manifest) || ispath(manifest_file)
        info("Updating manifest file $manifest_file")
        open(manifest_file, "w") do io
            TOML.print(io, manifest, sorted=true)
        end
    end
end

end # module
