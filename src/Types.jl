module Types

using Base.Random: UUID
using Base.Pkg.Types: VersionSet, Available
using Pkg3: user_depot, depots
using TOML, TerminalMenus

export SHA1, VersionRange, VersionSpec,
    Package, PackageVersion, UpgradeLevel, EnvCache,
    project_resolve!, registry_resolve!,
    registered_uuids, registered_paths,
    registered_uuid, registered_name

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
    version::Union{VersionSpec,UpgradeLevel}
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
    env::Union{String,Void}
    project_file::String
    manifest_file::String

    # cache of metadata:
    project::Dict
    manifest::Dict

    # registered package info:
    uuids::Dict{String,Vector{UUID}}
    paths::Dict{UUID,Vector{String}}

    function EnvCache(env::Union{Void,String})
        project_file = find_project(env)
        project = parse_toml(project_file, fakeit=true)
        if !haskey(project, "deps")
            project["deps"] = Dict{String,Any}()
        end
        if haskey(project, "manifest")
            manifest_file = abspath(project["manifest"])
        else
            dir = abspath(dirname(project_file))
            for name in manifest_names
                manifest_file = joinpath(dir, name)
                isfile(manifest_file) && break
            end
        end
        manifest = parse_toml(manifest_file, fakeit=true)
        for (name, infos) in manifest, info in infos
            haskey(info, "deps") || continue
            info["deps"] isa AbstractVector || continue
            for dep in info["deps"]
                length(manifest[dep]) == 1 ||
                    error("ambiguious dependency for $name: $dep\nManifest: $manifest_file")
            end
            info["deps"] = Dict(d => manifest[d][1]["uuid"] for d in info["deps"])
        end
        uuids = Dict{String,Vector{UUID}}()
        paths = Dict{UUID,Vector{String}}()
        new(env, project_file, manifest_file, project, manifest, uuids, paths)
    end
end
EnvCache() = EnvCache(get(ENV, "JULIA_ENV", nothing))

# finding the current project file

include("libgit2_discover.jl")

function find_local_env(start_path::String = pwd())
    path = LibGit2.discover(start_path, ceiling = homedir())
    repo = LibGit2.GitRepo(path)
    work = LibGit2.workdir(repo)
    for name in project_names
        path = abspath(work, name)
        isfile(path) && return path
    end
    return abspath(work, project_names[end])
end

function find_named_env()
    for depot in depots(), env in default_envs, name in project_names
        path = abspath(depot, "environments", env, name)
        isfile(path) && return path
    end
    env = VERSION.major == 0 ? default_envs[2] : default_envs[3]
    return abspath(user_depot(), "environments", env, project_names[end])
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
            isfile(path) && return path
        end
        return abspath(env, project_names[end])
    else # named environment
        for depot in depots()
            path = abspath(depot, "environments", env, project_names[end])
            isfile(path) && return path
        end
        return abspath(user_depot(), "environments", env, project_names[end])
    end
end

function find_project(::Void)
    try
        return find_local_env()
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
    # if there are no ambiuous packages, return early
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

"Return paths of all registries in a depot"
function registries(depot::String)
    d = joinpath(depot, "registries")
    regs = filter!(readdir(d)) do r
        isfile(joinpath(d, r, "registry.toml"))
    end
    return map(reg->joinpath(depot, "registries", reg), regs)
end
"Return paths of all registries in all depots"
registries() = [r for d in depots() for r in registries(d)]

const line_re = r"""
    ^ \s*
    ([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})
    \s* = \s* \{
    \s* name \s* = \s* "([^"]*)" \s*,
    \s* path \s* = \s* "([^"]*)" \s*,?
    \s* \} \s* $
"""x

"""
In a single pass through registries, lookup a set of names & uuids
"""
function find_registered!(
    env::EnvCache,
    names::Vector{String},
    uuids::Vector{UUID};
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
                println(line)
                uuid = UUID(m.captures[1])
                name = Base.unescape_string(m.captures[2])
                path = abspath(registry, Base.unescape_string(m.captures[3]))
                push!(get!(env.uuids, name, typeof(uuid)[]), uuid)
                push!(get!(env.paths, uuid, typeof(path)[]), path)
            end
        end
    end
end

"Lookup whatever's in project & manifest files"
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
    error("TODO: UUID prompt for `$name`")
end

"Determine current name for a given package UUID"
function registered_name(env::EnvCache, uuid::UUID)::String
    names = registered_names(env, uuid)
    length(names) == 0 && return ""
    length(names) == 1 && return names[1]
    error("TODO: find current name for `$uuid`")
end

end # module
