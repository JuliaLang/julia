module Types

using Base.Random: UUID
using Pkg3.Pkg2.Types: VersionSet, Available
using Pkg3.TOML
using Pkg3.TerminalMenus

import Pkg3
import Pkg3: depots, iswindows

export SHA1, VersionRange, VersionSpec, PackageSpec, UpgradeLevel, EnvCache,
    CommandError, cmderror, has_name, has_uuid, write_env, parse_toml, find_registered!,
    project_resolve!, manifest_resolve!, registry_resolve!, ensure_resolved,
    manifest_info, registered_uuids, registered_paths, registered_uuid, registered_name,
    git_file_stream, read_project, read_manifest, pathrepr

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

## command errors (no stacktrace) ##

struct CommandError <: Exception
    msg::String
end
cmderror(msg::String...) = throw(CommandError(join(msg)))

## type for expressing operations ##

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

const VersionTypes = Union{VersionNumber,VersionSpec,UpgradeLevel}

mutable struct PackageSpec
    name::String
    uuid::UUID
    version::VersionTypes
    mode::Symbol
    PackageSpec(name::String, uuid::UUID, version::VersionTypes) =
        new(name, uuid, version, :project)
end
PackageSpec(name::String, uuid::UUID) =
    PackageSpec(name, uuid, VersionSpec())
PackageSpec(name::AbstractString, version::VersionTypes=VersionSpec()) =
    PackageSpec(name, UUID(zero(UInt128)), version)
PackageSpec(uuid::UUID, version::VersionTypes=VersionSpec()) =
    PackageSpec("", uuid, version)

has_name(pkg::PackageSpec) = !isempty(pkg.name)
has_uuid(pkg::PackageSpec) = pkg.uuid != UUID(zero(UInt128))

function Base.show(io::IO, pkg::PackageSpec)
    print(io, "PackageSpec(")
    has_name(pkg) && show(io, pkg.name)
    has_name(pkg) && has_uuid(pkg) && print(io, ", ")
    has_uuid(pkg) && show(io, pkg.uuid)
    vstr = repr(pkg.version)
    if vstr != "VersionSpec(\"*\")"
        (has_name(pkg) || has_uuid(pkg)) && print(io, ", ")
        print(io, vstr)
    end
    print(io, ")")
end

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

function write_env(env::EnvCache)
    # load old environment for comparison
    old_env = EnvCache(env.env)
    # update the project file
    project = deepcopy(env.project)
    isempty(project["deps"]) && delete!(project, "deps")
    if !isempty(project) || ispath(env.project_file)
        info("Updating $(pathrepr(env, env.project_file))")
        Pkg3.Display.print_project_diff(old_env, env)
        mkpath(dirname(env.project_file))
        open(env.project_file, "w") do io
            TOML.print(io, project, sorted=true)
        end
    end
    # update the manifest file
    if !isempty(env.manifest) || ispath(env.manifest_file)
        info("Updating $(pathrepr(env, env.manifest_file))")
        Pkg3.Display.print_manifest_diff(old_env, env)
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

function git_discover(
    start_path::AbstractString = pwd();
    ceiling::Union{AbstractString,Vector} = "",
    across_fs::Bool = false,
)
    sep = @static iswindows() ? ";" : ":"
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
    try return LibGit2.GitRepo(git_discover(path))
    catch err
        err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
    end
    return nothing
end

function git_file_stream(repo::LibGit2.GitRepo, spec::String; fakeit::Bool=false)::IO
    blob = try LibGit2.GitBlob(repo, spec)
    catch err
        err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
        fakeit && return DevNull
    end
    return IOBuffer(LibGit2.rawcontent(blob))
end

function find_local_env(start_path::String = pwd())
    path = git_discover(start_path, ceiling = homedir())
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
        cmderror("invalid environment name: \"\"")
    elseif env == "/"
        return find_named_env()
    elseif env == "."
        return find_local_env()
    elseif startswith(env, "/") || startswith(env, "./")
        # path to project file or project directory
        if splitext(env)[2] == ".toml"
            path = abspath(env)
            return path, find_git_repo(path)
        end
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
Disambiguate name/uuid package specifications using project info.
"""
function project_resolve!(env::EnvCache, pkgs::AbstractVector{PackageSpec})
    uuids = env.project["deps"]
    names = Dict(uuid => name for (uuid, name) in uuids)
    length(uuids) < length(names) && # TODO: handle this somehow?
        warn("duplicate UUID found in project file's [deps] section")
    for pkg in pkgs
        pkg.mode == :project || continue
        if has_name(pkg) && !has_uuid(pkg) && pkg.name in keys(uuids)
            pkg.uuid = uuids[pkg.name]
        end
        if has_uuid(pkg) && !has_name(pkg) && pkg.uuid in keys(names)
            pkg.name = names[pkg.uuid]
        end
    end
    return pkgs
end

"""
Disambiguate name/uuid package specifications using manifest info.
"""
function manifest_resolve!(env::EnvCache, pkgs::AbstractVector{PackageSpec})
    uuids = Dict{String,Vector{String}}()
    names = Dict{String,String}()
    for (name, infos) in env.manifest, info in infos
        haskey(info, "uuid") || continue
        uuid = info["uuid"]
        push!(get!(uuids, name, String[]), uuid)
        names[uuid] = name # can be duplicate but doesn't matter
    end
    for pkg in pkgs
        pkg.mode == :manifest || continue
        if has_name(pkg) && !has_uuid(pkg) && pkg.name in keys(uuids)
            length(uuids[pkg.name]) == 1 && (pkg.uuid = uuids[pkg.name][1])
        end
        if has_uuid(pkg) && !has_name(pkg) && pkg.uuid in keys(names)
            pkg.name = names[pkg.uuid]
        end
    end
    return pkgs
end

"""
Disambiguate name/uuid package specifications using registry info.
"""
function registry_resolve!(env::EnvCache, pkgs::AbstractVector{PackageSpec})
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

"Ensure that all packages are fully resolved"
function ensure_resolved(
    env::EnvCache,
    pkgs::AbstractVector{PackageSpec},
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
    cmderror(msg)
end

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
        error("this should not happen")
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
    isempty(paths) && cmderror("`$uuid` is not registered")
    values = []
    for path in paths
        info = parse_toml(paths[1], "package.toml")
        value = get(info, key, nothing)
        value in values || push!(values, value)
    end
    length(values) > 1 &&
        cmderror("package `$uuid` has multiple registered `$key` values: ", join(values, ", "))
    return values[1]
end

"Find package by UUID in the manifest file"
function manifest_info(env::EnvCache, uuid::UUID)::Union{Dict{String,Any},Void}
    uuid in values(env.uuids) || find_registered!(env, [uuid])
    for (name, infos) in env.manifest, info in infos
        haskey(info, "uuid") && uuid == UUID(info["uuid"]) || continue
        return merge!(Dict{String,Any}("name" => name), info)
    end
    return nothing
end

"Give a short path string representation"
function pathrepr(env::EnvCache, path::String, base::String=pwd())
    path = abspath(base, path)
    if env.git != nothing
        repo = LibGit2.path(env.git)
        if startswith(base, repo)
            # we're in the repo => path relative to pwd()
            path = relpath(path, base)
        elseif startswith(path, repo)
            # we're not in repo but path is => path relative to repo
            path = relpath(path, repo)
        end
    end
    if !iswindows() && isabspath(path)
        home = joinpath(homedir(), "")
        if startswith(path, home)
            path = joinpath("~", path[nextind(path,endof(home)):end])
        end
    end
    return repr(path)
end

end # module
