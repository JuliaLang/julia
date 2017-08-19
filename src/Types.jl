module Types

using Base.Random: UUID
using Base.Pkg.Types: VersionSet, Available
using Pkg3: user_depot, depots
using TOML

export SHA1, VersionRange, VersionSpec, Package, PackageVersion, UpgradeLevel, EnvCache

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

const config_names = ["JuliaConfig.toml", "Config.toml"]
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
    config_file::String
    manifest_file::String

    # cache of metadata:
    config::Dict
    manifest::Dict

    # registered package info:
    uuids::Dict{String,Vector{UUID}}
    paths::Dict{UUID,Vector{String}}

    function EnvCache(env::Union{Void,String})
        config_file = find_config(env)
        config = parse_toml(config_file, fakeit=true)
        if haskey(config, "manifest")
            manifest_file = abspath(config["manifest"])
        else
            dir = abspath(dirname(config_file))
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
        new(env, config_file, manifest_file, config, manifest, uuids, paths)
    end
end
EnvCache() = EnvCache(get(ENV, "JULIA_ENV", nothing))

# finding the current config file

include("libgit2_discover.jl")

function find_project_config(start_path::String = pwd())
    path = LibGit2.discover(start_path, ceiling = homedir())
    repo = LibGit2.GitRepo(path)
    work = LibGit2.workdir(repo)
    for name in config_names
        path = abspath(work, name)
        isfile(path) && return path
    end
    return abspath(work, config_names[end])
end

function find_default_config()
    for depot in depots(), env in default_envs, name in config_names
        path = abspath(depot, "environments", env, name)
        isfile(path) && return path
    end
    env = VERSION.major == 0 ? default_envs[2] : default_envs[3]
    return abspath(user_depot(), "environments", env, config_names[end])
end

function find_config(env::String)
    if isempty(env)
        error("invalid environment name: \"\"")
    elseif env == "/"
        return find_default_config()
    elseif env == "."
        return find_project_config()
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
            path = abspath(depot, "environments", env, config_names[end])
            isfile(path) && return path
        end
        return abspath(user_depot(), "environments", env, config_names[end])
    end
end

function find_config(::Void)
    try
        return find_project_config()
    catch err
        err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
    end
    return find_default_config()
end

## resolving packages from name or uuid ##

function resolve_packages!(env::EnvCache, pkgs::AbstractVector{Package}; new::Bool=false)
    names = String[]
    uuids = UUID[]
    for pkg in pkgs
        if pkg.uuid != UUID(zero(UInt128))
            pkg.uuid in uuids || push!(uuids, pkg.uuid)
        end
        if !isemtpy(pkg.name)
            pkg.name in names || push!(names, pkg.name)
        end
    end

end

end # module
