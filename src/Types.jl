module Types

using Base.Random: UUID
using Base.Pkg.Types: VersionSet, Available
using Pkg3.TOML
using Pkg3.TerminalMenus
import Pkg3: depots

export SHA1, VersionRange, VersionSpec, Package, PackageVersion, UpgradeLevel,
    EnvCache, has_name, has_uuid, write_env, parse_toml, find_registered!,
    project_resolve!, registry_resolve!, ensure_resolved, manifest_info,
    registered_uuids, registered_paths, registered_uuid, registered_name

## ordering of UUIDs ##

Base.isless(a::UUID, b::UUID) = a.value < b.value
Base.convert(::Type{String}, u::UUID) = string(u)

## SHA1 ##

struct SHA1
    bytes::Vector{UInt8}
    function SHA1(bytes::Vector{UInt8})
        length(bytes) == 20 ||
            throw(ArgumentError("wrong number of bytes for SHA1 hash: $(length(bytes))"))
        return new(bytes)
    end
end

Base.convert(::Type{SHA1}, s::String) = SHA1(hex2bytes(s))
Base.convert(::Type{Vector{UInt8}}, hash::SHA1) = hash.bytes
Base.convert(::Type{String}, hash::SHA1) = bytes2hex(Vector{UInt8}(hash))

Base.string(hash::SHA1) = String(hash)
Base.show(io::IO, hash::SHA1) = print(io, "SHA1(", String(hash), ")")
Base.isless(a::SHA1, b::SHA1) = lexless(a.bytes, b.bytes)
Base.hash(a::SHA1, h::UInt) = hash((SHA1, a.bytes), h)
Base.:(==)(a::SHA1, b::SHA1) = a.bytes == b.bytes

## VersionRange ##

struct VersionBound{n}
    t::NTuple{n,Int}
    function VersionBound{n}(t::NTuple{n,Integer}) where n
        n <= 3 || throw(ArgumentError("VersionBound: you can only specify major, minor and patch versions"))
        return new(t)
    end
end
VersionBound(t::Integer...) = VersionBound{length(t)}(t)

Base.convert(::Type{VersionBound}, v::VersionNumber) =
    VersionBound(v.major, v.minor, v.patch)

Base.getindex(b::VersionBound, i::Int) = b.t[i]

≲(v::VersionNumber, b::VersionBound{0}) = true
≲(v::VersionNumber, b::VersionBound{1}) = v.major <= b[1]
≲(v::VersionNumber, b::VersionBound{2}) = (v.major, v.minor) <= (b[1], b[2])
≲(v::VersionNumber, b::VersionBound{3}) = (v.major, v.minor, v.patch) <= (b[1], b[2], b[3])

≲(b::VersionBound{0}, v::VersionNumber) = true
≲(b::VersionBound{1}, v::VersionNumber) = v.major >= b[1]
≲(b::VersionBound{2}, v::VersionNumber) = (v.major, v.minor) >= (b[1], b[2])
≲(b::VersionBound{3}, v::VersionNumber) = (v.major, v.minor, v.patch) >= (b[1], b[2], b[3])

≳(v::VersionNumber, b::VersionBound) = v ≲ b
≳(b::VersionBound, v::VersionNumber) = b ≲ v

Base.convert(::Type{VersionBound}, s::AbstractString) =
    VersionBound(map(x->parse(Int, x), split(s, '.'))...)

struct VersionRange{m,n}
    lower::VersionBound{m}
    upper::VersionBound{n}
    # TODO: check non-emptiness of range?
end
VersionRange(b::VersionBound=VersionBound()) = VersionRange(b, b)
VersionRange(t::Integer...) = VersionRange(VersionBound(t...))

Base.convert(::Type{VersionRange}, v::VersionNumber) =
    VersionRange(VersionBound(v))

function Base.convert(::Type{VersionRange}, s::AbstractString)
    ismatch(r"^\s*\*\s*$", s) && return VersionRange()
    m = match(r"^\s*(\d+(?:\.\d+)?(?:\.\d+)?)(?:\s*-\s*(\d+(?:\.\d+)?(?:\.\d+)?))?\s*$", s)
    m == nothing && throw(ArgumentError("invalid version range: $(repr(s))"))
    lower = VersionBound(m.captures[1])
    upper = m.captures[2] != nothing ? VersionBound(m.captures[2]) : lower
    return VersionRange(lower, upper)
end

function Base.print(io::IO, r::VersionRange)
    join(io, r.lower.t, '.')
    if r.lower != r.upper
        print(io, '-')
        join(io, r.upper.t, '.')
    end
end
Base.print(io::IO, ::VersionRange{0,0}) = print(io, "*")
Base.show(io::IO, r::VersionRange) = print(io, "VersionRange(\"", r, "\")")

Base.in(v::VersionNumber, r::VersionRange) = r.lower ≲ v ≲ r.upper
Base.in(v::VersionNumber, r::VersionNumber) = v == r

struct VersionSpec
    ranges::Vector{VersionRange}
    VersionSpec(r::Vector{<:VersionRange}) = new(r)
end
VersionSpec() = VersionSpec(VersionRange())

Base.in(v::VersionNumber, s::VersionSpec) = any(v in r for r in s.ranges)

Base.convert(::Type{VersionSpec}, v::VersionNumber) = VersionSpec(VersionRange(v))
Base.convert(::Type{VersionSpec}, r::VersionRange) = VersionSpec(VersionRange[r])
Base.convert(::Type{VersionSpec}, s::AbstractString) = VersionSpec(VersionRange(s))
Base.convert(::Type{VersionSpec}, v::AbstractVector) = VersionSpec(map(VersionRange, v))

function Base.print(io::IO, s::VersionSpec)
    length(s.ranges) == 1 && return print(io, s.ranges[1])
    print(io, '[')
    for i = 1:length(s.ranges)
        1 < i && print(io, ", ")
        print(io, s.ranges[i])
    end
    print(io, ']')
end
Base.show(io::IO, s::VersionSpec) = print(io, "VersionSpec(\"", s, "\")")

Base.convert(::Type{VersionSet}, v::VersionNumber) = VersionSet(v, Base.nextpatch(v))
Base.convert(::Type{VersionSet}, r::VersionRange{0,0}) = VersionSet()
Base.convert(::Type{VersionSet}, r::VersionRange{m,1}) where {m} =
    VersionSet(VersionNumber(r.lower.t...), VersionNumber(r.upper[1]+1))
Base.convert(::Type{VersionSet}, r::VersionRange{m,2}) where {m} =
    VersionSet(VersionNumber(r.lower.t...), VersionNumber(r.upper[1], r.upper[2]+1))
Base.convert(::Type{VersionSet}, r::VersionRange{m,3}) where {m} =
    VersionSet(VersionNumber(r.lower.t...), VersionNumber(r.upper[1], r.upper[2], r.upper[3]+1))
Base.convert(::Type{VersionSet}, s::VersionSpec) = mapreduce(VersionSet, ∪, s.ranges)
Base.convert(::Type{Available}, t::Tuple{SHA1,Dict{UUID,VersionSpec}}) = Available(t...)

## type for expressing operations ##

mutable struct Package
    name::String
    uuid::UUID
end
Package(name::AbstractString) = Package(name, UUID(zero(UInt128)))
Package(uuid::UUID) = Package("", uuid)

has_name(pkg::Package) = !isempty(pkg.name)
has_uuid(pkg::Package) = pkg.uuid != UUID(zero(UInt128))

function Base.show(io::IO, pkg::Package)
    print(io, "Package(")
    has_name(pkg) && show(io, pkg.name)
    has_name(pkg) && has_uuid(pkg) && print(io, ", ")
    has_uuid(pkg) && show(io, pkg.uuid)
    print(io, ")")
end

@enum UpgradeLevel fixed=0 patch=1 minor=2 major=3

function Base.convert(::Type{UpgradeLevel}, s::Symbol)
    s == :fixed ? fixed :
    s == :patch ? patch :
    s == :minor ? minor :
    s == :major ? major :
    throw(ArgumentError("invalid upgrade bound: $s"))
end
Base.convert(::Type{UpgradeLevel}, s::String) = UpgradeLevel(Symbol(s))
Base.convert(::Type{Union{VersionSpec,UpgradeLevel}}, v::VersionRange) = VersionSpec(v)

mutable struct PackageVersion
    package::Package
    version::Union{VersionNumber,VersionSpec,UpgradeLevel}
end
PackageVersion(pkg::Package) = PackageVersion(pkg, VersionSpec("*"))

## environment & registry loading & caching ##

function parse_toml(path::String...; fakeit::Bool=false)
    p = joinpath(path...)
    !fakeit || isfile(p) ? TOML.parsefile(p) : Dict{String,Any}()
end

const project_names = ["JuliaProject.toml", "Project.toml"]
const manifest_names = ["JuliaManifest.toml", "Manifest.toml"]
const default_envs = [
    "v$(VERSION.major).$(VERSION.minor).$(VERSION.patch)",
    "v$(VERSION.major).$(VERSION.minor)",
    "v$(VERSION.major)",
    "default",
]

struct EnvCache
    # environment info:
    env::Union{Void,String}
    git::Union{Void,LibGit2.GitRepo}

    # paths for files:
    project_file::String
    manifest_file::String

    # cache of metadata:
    project::Dict
    manifest::Dict

    # registered package info:
    uuids::Dict{String,Vector{UUID}}
    paths::Dict{UUID,Vector{String}}

    function EnvCache(env::Union{Void,String})
        project_file, git_repo = find_project(env)
        project = read_project(project_file)
        if haskey(project, "manifest")
            manifest_file = abspath(project["manifest"])
        else
            dir = abspath(dirname(project_file))
            for name in manifest_names
                manifest_file = joinpath(dir, name)
                isfile(manifest_file) && break
            end
        end
        manifest = read_manifest(manifest_file)
        uuids = Dict{String,Vector{UUID}}()
        paths = Dict{UUID,Vector{String}}()
        return new(
            env,
            git_repo,
            project_file,
            manifest_file,
            project,
            manifest,
            uuids,
            paths,
        )
    end
end
EnvCache() = EnvCache(get(ENV, "JULIA_ENV", nothing))

function read_project(io::IO)
    project = TOML.parse(io)
    if !haskey(project, "deps")
        project["deps"] = Dict{String,Any}()
    end
    return project
end
function read_project(file::String)
    isfile(file) ? open(read_project, file) : read_project(DevNull)
end

function read_manifest(io::IO)
    manifest = TOML.parse(io)
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
function read_manifest(file::String)
    try isfile(file) ? open(read_manifest, file) : read_manifest(DevNull)
    catch err
        err isa ErrorException && startswith(err.msg, "ambiguious dependency") || rethrow(err)
        err.msg *= "In manifest file: $file"
        rethrow(err)
    end
end

function emit_project(x::Char, name::String, uuid::String)
    color = x == '+' ? :light_green : :light_red
    print_with_color(:light_black, " [$(uuid[1:8])]")
    print_with_color(color, " $x $name\n")
end

function print_project_diff(deps₀::Dict, deps₁::Dict)
    clean = true
    for name in sort!(union(keys(deps₀), keys(deps₁)), by=lowercase)
        uuid₀, uuid₁ = get(deps₀, name, ""), get(deps₁, name, "")
        uuid₀ == uuid₁ && continue
        isempty(uuid₀) || emit_project('-', name, uuid₀)
        isempty(uuid₁) || emit_project('+', name, uuid₁)
        clean = false
    end
    clean && print_with_color(:light_black, " [no changes]\n")
    return nothing
end
print_project_diff(env₀::EnvCache, env₁::EnvCache) =
    print_project_diff(env₀.project["deps"], env₁.project["deps"])

struct ManifestEntry
    name::String
    uuid::UUID
    hash::SHA1
    version::Union{VersionNumber,Void}
end

function manifest_entries(manifest::Dict)
    entries = Dict{UUID,ManifestEntry}()
    for (name, infos) in manifest, info in infos
        uuid = UUID(info["uuid"])
        hash = SHA1(info["hash-sha1"])
        ver = get(info, "version", nothing)
        version = ver != nothing ? VersionNumber(ver) : nothing
        entries[uuid] = ManifestEntry(name, uuid, hash, version)
    end
    return entries
end
manifest_entries(env::EnvCache) = manifest_entries(env.manifest)

const ManifestDiff = Vector{NTuple{2,Union{ManifestEntry,Void}}}

function manifest_diff(
    infos₀::Dict{UUID,ManifestEntry},
    infos₁::Dict{UUID,ManifestEntry},
)::ManifestDiff
    uuids = sort!(union(keys(infos₀), keys(infos₁)), by=uuid->uuid.value)
    diff = eltype(ManifestDiff)[
        (get(infos₀, uuid, nothing), get(infos₁, uuid, nothing))
        for uuid in uuids]
    filter!(diff) do infos
        info₀, info₁ = infos
        info₀ == nothing || info₁ == nothing || info₀.hash != info₁.hash
    end
    sort!(diff, by=pair->lowercase(pair[pair[2]!=nothing ? 2 : 1].name))
end
manifest_diff(env₀::EnvCache, env₁::EnvCache)::ManifestDiff =
    manifest_diff(manifest_entries(env₀), manifest_entries(env₁))

v_str(x::ManifestEntry) =
    x.version == nothing ? "[$(string(x.hash)[1:16])]" : "v$(x.version)"

function emit_manifest_diff(emit::Function, diff::ManifestDiff)
    if isempty(diff)
        print_with_color(:light_black, " [no changes]\n")
        return
    end
    for (info₀, info₁) in diff
        uuid = info₁ != nothing ? info₁.uuid : info₀.uuid
        name = info₁ != nothing ? info₁.name : info₀.name
        u = string(uuid)[1:8]
        if info₀ != nothing && info₁ != nothing
            v₀, v₁ = v_str(info₀), v_str(info₁)
            x = info₀.version == nothing || info₁.version == nothing ? '~' :
                info₀.version < info₁.version ? '↑' : '↓'
            emit(uuid, name, x, "$v₀ ⇒ $v₁")
        elseif info₀ != nothing
            emit(uuid, name, '-', v_str(info₀))
        elseif info₁ != nothing
            emit(uuid, name, '+', v_str(info₁))
        else
            error("this should not happen")
        end
    end
end

function print_manifest_diff(env₀::EnvCache, env₁::EnvCache)
    emit_manifest_diff(manifest_diff(env₀, env₁)) do uuid, name, x, vers
        color = x == '+' ? :light_green   :
                x == '-' ? :light_red     :
                x == '↑' ? :light_yellow  :
                           :light_magenta
        print_with_color(:light_black, " [$(string(uuid)[1:8])] ")
        print_with_color(color, "$x $name $vers\n")
    end
end

const indent = "  "

function print_package_tree(
    io::IO,
    env::EnvCache,
    deps::Dict = env.project["deps"],
    seen::Dict{UUID,Bool} = Dict{UUID,Bool}(),
    depth::Int = 0,
)::Void
    for (name::String, uuid::UUID) in sort!(collect(deps), by=lowercase∘first)
        print(io, indent^depth, name, " [", string(uuid)[1:8], "]")
        if haskey(seen, uuid)
            seen[uuid] && print(io, " ⋯")
            println(io)
        else
            println(io)
            seen[uuid] = false # no deps
            for (name′, infos) in env.manifest, info in infos
                uuid == UUID(info["uuid"]) || continue
                haskey(info, "deps") && !isempty(info["deps"]) || continue
                print_package_tree(io, env, info["deps"], seen, depth+1)
                seen[uuid] = true # has deps
                break # stop searching manifest
            end
        end
    end
end
print_package_tree(env::EnvCache = EnvCache()) =
    print_package_tree(STDOUT, env)

function write_env(env::EnvCache)
    # load old environment for comparison
    old_env = EnvCache(env.env)
    # update the project file
    if !isempty(env.project) || ispath(env.project_file)
        info("Updating project file $(env.project_file)")
        print_project_diff(old_env, env)
        project = deepcopy(env.project)
        isempty(project["deps"]) && delete!(project, "deps")
        mkpath(dirname(env.project_file))
        open(env.project_file, "w") do io
            TOML.print(io, project, sorted=true)
        end
    end
    # update the manifest file
    if !isempty(env.manifest) || ispath(env.manifest_file)
        info("Updating manifest file $(env.manifest_file)")
        print_manifest_diff(old_env, env)
        manifest = deepcopy(env.manifest)
        uniques = sort!(collect(keys(manifest)), by=lowercase)
        filter!(name->length(manifest[name]) == 1, uniques)
        uuids = Dict(name => UUID(manifest[name][1]["uuid"]) for name in uniques)
        for (name, infos) in manifest, info in infos
            haskey(info, "deps") || continue
            deps = convert(Dict{String,UUID}, info["deps"])
            all(d in uniques && uuids[d] == u for (d, u) in deps) || continue
            info["deps"] = sort!(collect(keys(deps)))
        end
        open(env.manifest_file, "w") do io
            TOML.print(io, manifest, sorted=true)
        end
    end
end

# finding the current project file

function LibGit2_discover(
    start_path::AbstractString = pwd();
    ceiling::Union{AbstractString,Vector} = "",
    across_fs::Bool = false,
)
    sep = @static is_windows() ? ";" : ":"
    ceil = ceiling isa AbstractString ? ceiling :
        join(convert(Vector{String}, ceiling), sep)
    buf_ref = Ref(LibGit2.Buffer())
    @LibGit2.check ccall(
        (:git_repository_discover, :libgit2), Cint,
        (Ptr{LibGit2.Buffer}, Cstring, Cint, Cstring),
        buf_ref, start_path, across_fs, ceil)
    buf = buf_ref[]
    str = unsafe_string(buf.ptr, buf.size)
    LibGit2.free(buf_ref)
    return str
end

function find_git_repo(path::String)
    while !ispath(path)
        prev = path
        path = dirname(path)
        path == prev && break
    end
    try return LibGit2.GitRepo(LibGit2_discover(path))
    catch err
        err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
    end
    return nothing
end

function find_local_env(start_path::String = pwd())
    path = LibGit2_discover(start_path, ceiling = homedir())
    repo = LibGit2.GitRepo(path)
    work = LibGit2.workdir(repo)
    for name in project_names
        path = abspath(work, name)
        isfile(path) && return path, repo
    end
    return abspath(work, project_names[end]), repo
end

function find_named_env()
    for depot in depots(), env in default_envs, name in project_names
        path = abspath(depot, "environments", env, name)
        isfile(path) && return path, find_git_repo(path)
    end
    env = VERSION.major == 0 ? default_envs[2] : default_envs[3]
    path = abspath(depots()[1], "environments", env, project_names[end])
    return path, find_git_repo(path)
end

function find_project(env::String)
    if isempty(env)
        error("invalid environment name: \"\"")
    elseif env == "/"
        return find_named_env()
    elseif env == "."
        return find_local_env()
    elseif startswith(env, "/") || startswith(env, "./")
        # path to project file or project directory
        splitext(env)[2] == ".toml" && return abspath(env)
        for name in project_names
            path = abspath(env, name)
            isfile(path) && return path, find_git_repo(path)
        end
        path = abspath(env, project_names[end])
        return path, find_git_repo(path)
    else # named environment
        for depot in depots()
            path = abspath(depot, "environments", env, project_names[end])
            isfile(path) && return path, find_git_repo(path)
        end
        path = abspath(depots()[1], "environments", env, project_names[end])
        return path, find_git_repo(path)
    end
end

function find_project(::Void)
    try return find_local_env()
    catch err
        err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
    end
    return find_named_env()
end

## resolving packages from name or uuid ##

"""
Disambiguate name-only and uuid-only package specifications using only
information in the project file.
"""
function project_resolve!(env::EnvCache, pkgs::AbstractVector{Package})
    deps = env.project["deps"]
    depr = Dict(uuid => name for (uuid, name) in deps)
    length(deps) == length(depr) || # TODO: handle this somehow?
        warn("duplicate UUID found in project file's [deps] section")
    for pkg in pkgs
        if has_name(pkg) && !has_uuid(pkg) && pkg.name in keys(deps)
            pkg.uuid = deps[pkg.name]
        end
        if has_uuid(pkg) && !has_name(pkg) && pkg.uuid in keys(depr)
            pkg.name = depr[pkg.uuid]
        end
    end
    return pkgs
end
project_resolve!(env::EnvCache, pkgs::AbstractVector{PackageVersion}) =
    project_resolve!(env, [v.package for v in pkgs])

"""
Disambiguate name-only and uuid-only package specifications using only
information from registries.
"""
function registry_resolve!(env::EnvCache, pkgs::AbstractVector{Package})
    # if there are no half-specified packages, return early
    any(pkg->has_name(pkg) ⊻ has_uuid(pkg), pkgs) || return
    # collect all names and uuids since we're looking anyway
    names = [pkg.name for pkg in pkgs if has_name(pkg)]
    uuids = [pkg.uuid for pkg in pkgs if has_uuid(pkg)]
    find_registered!(env, names, uuids)
    for pkg in pkgs
        @assert has_name(pkg) || has_uuid(pkg)
        if has_name(pkg) && !has_uuid(pkg)
            pkg.uuid = registered_uuid(env, pkg.name)
        end
        if has_uuid(pkg) && !has_name(pkg)
            pkg.name = registered_name(env, pkg.uuid)
        end
    end
    return pkgs
end
registry_resolve!(env::EnvCache, pkgs::AbstractVector{PackageVersion}) =
    registry_resolve!(env, [v.package for v in pkgs])

"Ensure that all packages are fully resolved"
function ensure_resolved(
    env::EnvCache,
    pkgs::AbstractVector{Package},
    registry::Bool = false,
)::Void
    unresolved = Dict{String,Vector{UUID}}()
    for pkg in pkgs
        has_uuid(pkg) && continue
        uuids = UUID[]
        for (name, infos) in env.manifest, info in infos
            name == pkg.name && haskey(info, "uuid") || continue
            uuid = UUID(info["uuid"])
            uuid in uuids || push!(uuids, uuid)
        end
        sort!(uuids, by=uuid->uuid.value)
        unresolved[pkg.name] = uuids
    end
    isempty(unresolved) && return
    msg = sprint() do io
        println(io, "The following package names could not be resolved:")
        for (name, uuids) in sort!(collect(unresolved), by=lowercase∘first)
            print(io, " * $name (")
            if length(uuids) == 0
                what = ["project", "manifest"]
                registry && push!(what, "registry")
                print(io, "not found in ")
                join(io, what, ", ", " or ")
            else
                join(io, uuids, ", ", " or ")
                print(io, " in manifest but not in project")
            end
            println(io, ")")
        end
        print(io, "Please specify by known `name=uuid`.")
    end
    error(msg)
end
ensure_resolved(
    env::EnvCache,
    pkgs::AbstractVector{PackageVersion},
    registry::Bool = false,
) = ensure_resolved(env, [v.package for v in pkgs], registry)

const DEFAULT_REGISTRIES = Dict(
    "Uncurated" => "https://github.com/JuliaRegistries/Uncurated.git"
)

"Return paths of all registries in a depot"
function registries(depot::String)::Vector{String}
    d = joinpath(depot, "registries")
    ispath(d) || return String[]
    regs = filter!(readdir(d)) do r
        isfile(joinpath(d, r, "registry.toml"))
    end
    String[joinpath(depot, "registries", r) for r in regs]
end

"Return paths of all registries in all depots"
function registries()::Vector{String}
    isempty(depots()) && return String[]
    user_regs = abspath(depots()[1], "registries")
    if !ispath(user_regs)
        mkpath(user_regs)
        info("Cloning default registries into $user_regs")
        for (reg, url) in DEFAULT_REGISTRIES
            info(" [+] $reg = $(repr(url))")
            path = joinpath(user_regs, reg)
            LibGit2.clone(url, path)
        end
    end
    return [r for d in depots() for r in registries(d)]
end

const line_re = r"""
    ^ \s*
    ([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})
    \s* = \s* \{
    \s* name \s* = \s* "([^"]*)" \s*,
    \s* path \s* = \s* "([^"]*)" \s*,?
    \s* \} \s* $
"""x

"""
Lookup package names & uuids in a single pass through registries
"""
function find_registered!(
    env::EnvCache,
    names::Vector{String},
    uuids::Vector{UUID} = UUID[];
    force::Bool = false,
)::Void
    # only look if there's something new to see (or force == true)
    names = filter(name->!haskey(env.uuids, name), names)
    uuids = filter(uuid->!haskey(env.paths, uuid), uuids)
    !force && isempty(names) && isempty(uuids) && return

    # since we're looking anyway, look for everything
    save(name::String) =
        name in names || haskey(env.uuids, name) || push!(names, name)
    save(uuid::UUID) =
        uuid in uuids || haskey(env.paths, uuid) || push!(uuids, uuid)

    # lookup any dependency in the project file
    for (name, uuid) in env.project["deps"]
        save(name); save(UUID(uuid))
    end
    # lookup anything mentioned in the manifest file
    for (name, infos) in env.manifest, info in infos
        save(name)
        haskey(info, "uuid") && save(UUID(info["uuid"]))
        haskey(info, "deps") || continue
        for (n, u) in info["deps"]
            save(n); save(UUID(u))
        end
    end
    # if there's still nothing to look for, return early
    isempty(names) && isempty(uuids) && return

    # build regexs for names and uuids
    uuid_re = sprint() do io
        if !isempty(uuids)
            print(io, raw"^( ")
            for (i, uuid) in enumerate(uuids)
                1 < i && print(io, " | ")
                print(io, raw"\Q", uuid, raw"\E")
            end
            print(io, raw" )\b")
        end
    end
    name_re = sprint() do io
        if !isempty(names)
            print(io, raw"\bname \s* = \s* \"( ")
            for (i, name) in enumerate(names)
                1 < i && print(io, " | ")
                print(io, raw"\Q", name, raw"\E")
            end
            print(io, raw" )\"")
        end
    end
    regex = if !isempty(uuids) && !isempty(names)
        Regex("( $uuid_re | $name_re )", "x")
    elseif !isempty(uuids)
        Regex(uuid_re, "x")
    elseif !isempty(names)
        Regex(name_re, "x")
    else
        error("this sholdn't happen")
    end

    # initialize env entries for names and uuids
    for name in names; env.uuids[name] = UUID[]; end
    for uuid in uuids; env.paths[uuid] = String[]; end
    # note: empty vectors will be left for names & uuids that aren't found

    # search through all registries
    for registry in registries()
        open(joinpath(registry, "registry.toml")) do io
            # skip forward until [packages] section
            for line in eachline(io)
                ismatch(r"^ \s* \[ \s* packages \s* \] \s* $"x, line) && break
            end
            # fine lines with uuid or name we're looking for
            for line in eachline(io)
                ismatch(regex, line) || continue
                m = match(line_re, line)
                m == nothing &&
                    error("misformated registry.toml package entry: $line")
                uuid = UUID(m.captures[1])
                name = Base.unescape_string(m.captures[2])
                path = abspath(registry, Base.unescape_string(m.captures[3]))
                push!(get!(env.uuids, name, typeof(uuid)[]), uuid)
                push!(get!(env.paths, uuid, typeof(path)[]), path)
            end
        end
    end
end
find_registered!(env::EnvCache, uuids::Vector{UUID}; force::Bool=false)::Void =
    find_registered!(env, String[], uuids, force=force)

"Lookup all packages in project & manifest files"
find_registered!(env::EnvCache)::Void =
    find_registered!(env, String[], UUID[], force=true)

"Get registered uuids associated with a package name"
function registered_uuids(env::EnvCache, name::String)::Vector{UUID}
    find_registered!(env, [name], UUID[])
    return env.uuids[name]
end

"Get registered paths associated with a package uuid"
function registered_paths(env::EnvCache, uuid::UUID)::Vector{String}
    find_registered!(env, String[], [uuid])
    return env.paths[uuid]
end

"Get registered names associated with a package uuid"
function registered_names(env::EnvCache, uuid::UUID)::Vector{String}
    find_registered!(env, String[], [uuid])
    String[n for (n, uuids) in env.uuids for u in uuids if u == uuid]
end

"Determine a single UUID for a given name, prompting if needed"
function registered_uuid(env::EnvCache, name::String)::UUID
    uuids = registered_uuids(env, name)
    length(uuids) == 0 && return UUID(zero(UInt128))
    length(uuids) == 1 && return uuids[1]
    # prompt for which UUID was intended:
    choices = ["$uuid – $(registered_info(env, uuid, "repo"))" for uuid in uuids]
    menu = RadioMenu(choices)
    choice = request("There are multiple registered `$name` packages, choose one:", menu)
    choice == -1 && return UUID(zero(UInt128))
    return uuids[choice]
end

"Determine current name for a given package UUID"
function registered_name(env::EnvCache, uuid::UUID)::String
    names = registered_names(env, uuid)
    length(names) == 0 && return ""
    length(names) == 1 && return names[1]
    return registered_info(env, uuid, "name")
end

"Return most current package info for a registered UUID"
function registered_info(env::EnvCache, uuid::UUID, key::String)
    paths = env.paths[uuid]
    isempty(paths) && error("`$uuid` is not registered")
    values = []
    for path in paths
        info = parse_toml(paths[1], "package.toml")
        value = get(info, key, nothing)
        value in values || push!(values, value)
    end
    length(values) > 1 &&
        error("package `$uuid` has multiple registered `$key` values: ", join(values, ", "))
    return values[1]
end

"Find package by UUID in the manifest file"
function manifest_info(env::EnvCache, uuid::UUID)::Union{Dict{String,Any},Void}
    uuid in values(env.uuids) || find_registered!(env, [uuid])
    for (name, infos) in env.manifest, info in infos
        haskey(info, "uuid") && uuid == UUID(info["uuid"]) || continue
        return convert(Dict{String,Any}, info)
    end
    return nothing
end

end # module
