module Types

using UUIDs
using Random
using Dates
import LibGit2
import REPL
using REPL.TerminalMenus

using Pkg3.TOML
import Pkg3
import Pkg3: depots, logdir

import Base: SHA1, AbstractEnv
using SHA

export UUID, pkgID, SHA1, VersionRange, VersionSpec, empty_versionspec,
    Requires, Fixed, merge_requires!, satisfies, ResolverError,
    PackageSpec, EnvCache, Context, Context!,
    CommandError, cmderror, has_name, has_uuid, write_env, parse_toml, find_registered!,
    project_resolve!, manifest_resolve!, registry_resolve!, ensure_resolved,
    manifest_info, registered_uuids, registered_paths, registered_uuid, registered_name,
    read_project, read_manifest, pathrepr, registries,
    PackageMode, PKGMODE_MANIFEST, PKGMODE_PROJECT, PKGMODE_COMBINED,
    UpgradeLevel, UPLEVEL_FIXED, UPLEVEL_PATCH, UPLEVEL_MINOR, UPLEVEL_MAJOR,
    PackageSpecialAction, PKGSPEC_NOTHING, PKGSPEC_PINNED, PKGSPEC_FREED, PKGSPEC_CHECKED_OUT


## ordering of UUIDs ##

Base.isless(a::UUID, b::UUID) = a.value < b.value

## Computing UUID5 values from (namespace, key) pairs ##
function uuid5(namespace::UUID, key::String)
    data = [reinterpret(UInt8, [namespace.value]); codeunits(key)]
    u = reinterpret(UInt128, sha1(data)[1:16])[1]
    u &= 0xffffffffffff0fff3fffffffffffffff
    u |= 0x00000000000050008000000000000000
    return UUID(u)
end
uuid5(namespace::UUID, key::AbstractString) = uuid5(namespace, String(key))

const uuid_dns = UUID(0x6ba7b810_9dad_11d1_80b4_00c04fd430c8)
const uuid_julia = uuid5(uuid_dns, "julialang.org")
const uuid_package = uuid5(uuid_julia, "package")
const uuid_registry = uuid5(uuid_julia, "registry")


## user-friendly representation of package IDs ##
function pkgID(p::UUID, uuid_to_name::Dict{UUID,String})
    name = get(uuid_to_name, p, "")
    isempty(name) && (name = "(unknown)")
    uuid_short = string(p)[1:8]
    return "$name [$uuid_short]"
end

################
# VersionBound #
################
struct VersionBound
    t::NTuple{3,Int}
    n::Int
    function VersionBound(tin::NTuple{n,Integer}) where n
        n <= 3 || throw(ArgumentError("VersionBound: you can only specify major, minor and patch versions"))
        n == 0 && return new( (     0,      0,      0), n)
        n == 1 && return new( (tin[1],      0,      0), n)
        n == 2 && return new( (tin[1], tin[2],      0), n)
        n == 3 && return new( (tin[1], tin[2], tin[3]), n)
        error("invalid $n")
    end
end
VersionBound(t::Integer...) = VersionBound(t)
VersionBound(v::VersionNumber) = VersionBound(v.major, v.minor, v.patch)

Base.getindex(b::VersionBound, i::Int) = b.t[i]

function ≲(v::VersionNumber, b::VersionBound)
    b.n == 0 && return true
    b.n == 1 && return v.major <= b[1]
    b.n == 2 && return (v.major, v.minor) <= (b[1], b[2])
    return (v.major, v.minor, v.patch) <= (b[1], b[2], b[3])
end

function ≲(b::VersionBound, v::VersionNumber)
    b.n == 0 && return true
    b.n == 1 && return v.major >= b[1]
    b.n == 2 && return (v.major, v.minor) >= (b[1], b[2])
    return (v.major, v.minor, v.patch) >= (b[1], b[2], b[3])
end

≳(v::VersionNumber, b::VersionBound) = v ≲ b
≳(b::VersionBound, v::VersionNumber) = b ≲ v

function isless_ll(a::VersionBound, b::VersionBound)
    m, n = a.n, b.n
    for i = 1:min(m,n)
        a[i] < b[i] && return true
        a[i] > b[i] && return false
    end
    return m < n
end

stricterlower(a::VersionBound, b::VersionBound) = isless_ll(a, b) ? b : a

# Comparison between two upper bounds
# (could be done with generated functions, or even manually unrolled...)
function isless_uu(a::VersionBound, b::VersionBound)
    m, n = a.n, b.n
    for i = 1:min(m,n)
        a[i] < b[i] && return true
        a[i] > b[i] && return false
    end
    return m > n
end

stricterupper(a::VersionBound, b::VersionBound) = isless_uu(a, b) ? a : b

# `isjoinable` compares an upper bound of a range with the lower bound of the next range
# to determine if they can be joined, as in [1.5-2.8, 2.5-3] -> [1.5-3]. Used by `union!`.
# The equal-length-bounds case is special since e.g. `1.5` can be joined with `1.6`,
# `2.3.4` can be joined with `2.3.5` etc.

function isjoinable(up::VersionBound, lo::VersionBound)
    up.n == 0 && up.lo == 0 && return true
    if up.n == lo.n
        n = up.n
        for i = 1:(n - 1)
            up[i] > lo[i] && return true
            up[i] < lo[i] && return false
        end
        up[n] < lo[n] - 1 && return false
        return true
    else
        l = min(up.n, lo.n)
        for i = 1:l
            up[i] > lo[i] && return true
            up[i] < lo[i] && return false
        end
    end
    return true
end

Base.hash(r::VersionBound, h::UInt) = hash(hash(r.t, h), r.n)

VersionBound(s::AbstractString) =
    s == "*" ? VersionBound() : VersionBound(map(x->parse(Int, x), split(s, '.'))...)

################
# VersionRange #
################
struct VersionRange
    lower::VersionBound
    upper::VersionBound
    # NOTE: ranges are allowed to be empty; they are ignored by VersionSpec anyway
end
VersionRange(b::VersionBound=VersionBound()) = VersionRange(b, b)
VersionRange(t::Integer...)                  = VersionRange(VersionBound(t...))
VersionRange(v::VersionNumber)               = VersionRange(VersionBound(v))
function VersionRange(s::AbstractString)
    m = match(r"^\s*v?((?:\d+(?:\.\d+)?(?:\.\d+)?)|\*)(?:\s*-\s*v?((?:\d+(?:\.\d+)?(?:\.\d+)?)|\*))?\s*$", s)
    m == nothing && throw(ArgumentError("invalid version range: $(repr(s))"))
    lower = VersionBound(m.captures[1])
    upper = m.captures[2] != nothing ? VersionBound(m.captures[2]) : lower
    return VersionRange(lower, upper)
end

function Base.isempty(r::VersionRange)
    for i = 1:min(r.lower.n, r.upper.n)
        r.lower[i] > r.upper[i] && return true
        r.lower[i] < r.upper[i] && return false
    end
    return false
end

function Base.print(io::IO, r::VersionRange)
    m, n = r.lower.n, r.upper.n
    if (m, n) == (0, 0)
        print(io, '*')
    elseif m == 0
        print(io, "0-")
        join(io, r.upper.t, '.')
    elseif n == 0
        join(io, r.lower.t, '.')
        print(io, "-*")
    else
        join(io, r.lower.t, '.')
        if r.lower != r.upper
            print(io, '-')
            join(io, r.upper.t, '.')
        end
    end
end
Base.show(io::IO, r::VersionRange) = print(io, "VersionRange(\"", r, "\")")

Base.in(v::VersionNumber, r::VersionRange) = r.lower ≲ v ≲ r.upper
Base.in(v::VersionNumber, r::VersionNumber) = v == r

Base.intersect(a::VersionRange, b::VersionRange) = VersionRange(stricterlower(a.lower,b.lower), stricterupper(a.upper,b.upper))

function Base.union!(ranges::Vector{<:VersionRange})
    l = length(ranges)
    l == 0 && return ranges

    sort!(ranges, lt=(a,b)->(isless_ll(a.lower, b.lower) || (a.lower == b.lower && isless_uu(a.upper, b.upper))))

    k0 = 1
    ks = findfirst(!isempty, ranges)
    ks == nothing && return empty!(ranges)

    lo, up, k0 = ranges[ks].lower, ranges[ks].upper, 1
    for k = (ks+1):l
        isempty(ranges[k]) && continue
        lo1, up1 = ranges[k].lower, ranges[k].upper
        if isjoinable(up, lo1)
            isless_uu(up, up1) && (up = up1)
            continue
        end
        vr = VersionRange(lo, up)
        @assert !isempty(vr)
        ranges[k0] = vr
        k0 += 1
        lo, up = lo1, up1
    end
    vr = VersionRange(lo, up)
    if !isempty(vr)
        ranges[k0] = vr
        k0 += 1
    end
    resize!(ranges, k0 - 1)
    return ranges
end

###############
# VersionSpec #
###############
struct VersionSpec
    ranges::Vector{VersionRange}
    VersionSpec(r::Vector{<:VersionRange}) = new(union!(r))
    VersionSpec(vs::VersionSpec) = new(copy(vs.ranges))
end

VersionSpec() = VersionSpec(VersionRange())
VersionSpec(v::VersionNumber) = VersionSpec(VersionRange(v))
VersionSpec(r::VersionRange) = VersionSpec(VersionRange[r])
VersionSpec(s::AbstractString) = VersionSpec(VersionRange(s))
VersionSpec(v::AbstractVector) = VersionSpec(map(VersionRange, v))

# Hot code
function Base.in(v::VersionNumber, s::VersionSpec)
    for r in s.ranges
        v in r && return true
    end
    return false
end

Base.copy(vs::VersionSpec) = VersionSpec(vs)

const empty_versionspec = VersionSpec(VersionRange[])
# Windows console doesn't like Unicode
const _empty_symbol = @static Sys.iswindows() ? "empty" : "∅"

Base.isempty(s::VersionSpec) = all(isempty, s.ranges)
@assert isempty(empty_versionspec)
# Hot code, measure performance before changing
function Base.intersect(A::VersionSpec, B::VersionSpec)
    (isempty(A) || isempty(B)) && return copy(empty_versionspec)
    ranges = Vector{VersionRange}(uninitialized, length(A.ranges) * length(B.ranges))
    i = 1
    @inbounds for a in A.ranges, b in B.ranges
        ranges[i] = intersect(a, b)
        i += 1
    end
    VersionSpec(ranges)
end

Base.union(A::VersionSpec, B::VersionSpec) = union!(copy(A), B)
function Base.union!(A::VersionSpec, B::VersionSpec)
    A == B && return A
    append!(A.ranges, B.ranges)
    union!(A.ranges)
    return A
end

Base.:(==)(A::VersionSpec, B::VersionSpec) = A.ranges == B.ranges
Base.hash(s::VersionSpec, h::UInt) = hash(s.ranges, h + (0x2fd2ca6efa023f44 % UInt))
Base.deepcopy_internal(vs::VersionSpec, ::IdDict) = copy(vs)

function Base.print(io::IO, s::VersionSpec)
    isempty(s) && return print(io, _empty_symbol)
    length(s.ranges) == 1 && return print(io, s.ranges[1])
    print(io, '[')
    for i = 1:length(s.ranges)
        1 < i && print(io, ", ")
        print(io, s.ranges[i])
    end
    print(io, ']')
end
Base.show(io::IO, s::VersionSpec) = print(io, "VersionSpec(\"", s, "\")")

####################
# Requires / Fixed #
####################
const Requires = Dict{UUID,VersionSpec}

function merge_requires!(A::Requires, B::Requires)
    for (pkg,vers) in B
        A[pkg] = haskey(A,pkg) ? intersect(A[pkg],vers) : vers
    end
    return A
end

satisfies(pkg::UUID, ver::VersionNumber, reqs::Requires) =
    !haskey(reqs, pkg) || in(ver, reqs[pkg])

struct Fixed
    version::VersionNumber
    requires::Requires
end
Fixed(v::VersionNumber) = Fixed(v,Requires())

Base.:(==)(a::Fixed, b::Fixed) = a.version == b.version && a.requires == b.requires
Base.hash(f::Fixed, h::UInt) = hash((f.version, f.requires), h + (0x68628b809fd417ca % UInt))

Base.show(io::IO, f::Fixed) = isempty(f.requires) ?
    print(io, "Fixed(", repr(f.version), ")") :
    print(io, "Fixed(", repr(f.version), ",", f.requires, ")")


struct ResolverError <: Exception
    msg::AbstractString
    ex::Union{Exception, Nothing}
end
ResolverError(msg::AbstractString) = ResolverError(msg, nothing)

function Base.showerror(io::IO, pkgerr::ResolverError)
    print(io, pkgerr.msg)
    if pkgerr.ex !== nothing
        pkgex = pkgerr.ex
        if isa(pkgex, CompositeException)
            for cex in pkgex
                print(io, "\n=> ")
                showerror(io, cex)
            end
        else
            print(io, "\n")
            showerror(io, pkgex)
        end
    end
end

#################
# Command Error #
#################
struct CommandError <: Exception
    msg::String
end
cmderror(msg::String...) = throw(CommandError(join(msg)))

# No stacktrace shown
Base.showerror(io::IO, ex::CommandError) = showerror(io, ex, [])
function Base.showerror(io::IO, ex::CommandError, bt; backtrace=true)
    printstyled(color = Base.error_color(), io, string(ex.msg))
end

###############
# PackageSpec #
###############
@enum(UpgradeLevel, UPLEVEL_FIXED, UPLEVEL_PATCH, UPLEVEL_MINOR, UPLEVEL_MAJOR)

function UpgradeLevel(s::Symbol)
    s == :fixed ? UPLEVEL_FIXED :
    s == :patch ? UPLEVEL_PATCH :
    s == :minor ? UPLEVEL_MINOR :
    s == :major ? UPLEVEL_MAJOR :
    throw(ArgumentError("invalid upgrade bound: $s"))
end

@enum(PackageMode, PKGMODE_PROJECT, PKGMODE_MANIFEST, PKGMODE_COMBINED)
@enum(PackageSpecialAction, PKGSPEC_NOTHING, PKGSPEC_PINNED, PKGSPEC_FREED, PKGSPEC_CHECKED_OUT)

const VersionTypes = Union{VersionNumber,VersionSpec,UpgradeLevel}

mutable struct PackageSpec
    name::String
    uuid::UUID
    version::VersionTypes
    mode::PackageMode
    path::Union{Nothing, String}
    special_action::PackageSpecialAction # If the package is currently being pinned, freed etc
    PackageSpec(name::AbstractString, uuid::UUID, version::VersionTypes, mode::PackageMode=PKGMODE_PROJECT, path=nothing, special_action=PKGSPEC_NOTHING) =
        new(String(name), uuid, version, mode, path, special_action)
end
PackageSpec(name::AbstractString, uuid::UUID) =
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


############
# EnvCache #
############

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
    env::Union{Nothing,String,AbstractEnv}
    git::Union{Nothing,LibGit2.GitRepo}

    # paths for files:
    project_file::String
    manifest_file::String

    # cache of metadata:
    project::Dict
    manifest::Dict

    # registered package info:
    uuids::Dict{String,Vector{UUID}}
    paths::Dict{UUID,Vector{String}}

    function EnvCache(env::Union{Nothing,String,AbstractEnv}=nothing)
        if env isa Nothing
            project_file = nothing
            for entry in LOAD_PATH
                project_file = Base.find_env(entry)
                project_file isa String && !isdir(project_file) && break
            end
            project_file === nothing && error("No Pkg3 environment found in LOAD_PATH")
        elseif env isa AbstractEnv
            project_file = Base.find_env(env)
            project_file === nothing && error("package environment does not exist: $env")
        elseif env isa String
            isdir(env) && error("environment is a package directory: $env")
            project_file = endswith(env, ".toml") ? abspath(env) :
                                                    abspath(env, Base.project_names[end])
        end
        @assert project_file isa String && (isfile(project_file) || !ispath(project_file))
        project_dir = dirname(project_file)
        git = ispath(joinpath(project_dir, ".git")) ? LibGit2.GitRepo(project_dir) : nothing

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
        write_env_usage(manifest_file)
        manifest = read_manifest(manifest_file)
        uuids = Dict{String,Vector{UUID}}()
        paths = Dict{UUID,Vector{String}}()
        return new(
            env,
            git,
            project_file,
            manifest_file,
            project,
            manifest,
            uuids,
            paths,
        )
    end
end

function write_env_usage(manifest_file::AbstractString)
    !ispath(logdir()) && mkpath(logdir())
    usage_file = joinpath(logdir(), "usage.toml")
    touch(usage_file)
    !isfile(manifest_file) && return
    # Do not rewrite as do syntax (no longer precompilable)
    io = open(usage_file, "a")
    println(io, "[[\"", escape_string(manifest_file), "\"]]")
    print(io, "time = ", now()); println(io, 'Z')
    close(io)
end

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
        new_dict = Dict()
        for d in info["deps"]
            new_dict[d] = manifest[d][1]["uuid"]
        end
        info["deps"] = new_dict
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


########################################
# Resolving packages from name or uuid #
########################################
# Disambiguate name/uuid package specifications using project info.
function project_resolve!(env::EnvCache, pkgs::AbstractVector{PackageSpec})
    uuids = env.project["deps"]
    names = Dict(uuid => name for (uuid, name) in uuids)
    length(uuids) < length(names) && # TODO: handle this somehow?
        @warn "duplicate UUID found in project file's [deps] section"
    for pkg in pkgs
        pkg.mode == PKGMODE_PROJECT || continue
        if has_name(pkg) && !has_uuid(pkg) && pkg.name in keys(uuids)
            pkg.uuid = UUID(uuids[pkg.name])
        end
        if has_uuid(pkg) && !has_name(pkg) && pkg.uuid in keys(names)
            pkg.name = names[pkg.uuid]
        end
    end
    return pkgs
end

# Disambiguate name/uuid package specifications using manifest info.
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
        pkg.mode == PKGMODE_MANIFEST || continue
        if has_name(pkg) && !has_uuid(pkg) && pkg.name in keys(uuids)
            length(uuids[pkg.name]) == 1 && (pkg.uuid = UUID(uuids[pkg.name][1]))
        end
        if has_uuid(pkg) && !has_name(pkg) && pkg.uuid in keys(names)
            pkg.name = names[pkg.uuid]
        end
    end
    return pkgs
end

# Disambiguate name/uuid package specifications using registry info.
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

# Ensure that all packages are fully resolved
function ensure_resolved(
    env::EnvCache,
    pkgs::AbstractVector{PackageSpec},
    registry::Bool = false,
)::Nothing
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

# Return paths of all registries in a depot
function registries(depot::String)::Vector{String}
    d = joinpath(depot, "registries")
    ispath(d) || return String[]
    regs = filter!(readdir(d)) do r
        isfile(joinpath(d, r, "registry.toml"))
    end
    String[joinpath(depot, "registries", r) for r in regs]
end

# Return paths of all registries in all depots
function registries()::Vector{String}
    isempty(depots()) && return String[]
    user_regs = abspath(depots()[1], "registries")
    if !ispath(user_regs)
        mkpath(user_regs)
        @info "Cloning default registries into $user_regs"
        for (reg, url) in DEFAULT_REGISTRIES
            @info " [+] $reg = $(repr(url))"
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

# Lookup package names & uuids in a single pass through registries
function find_registered!(
    env::EnvCache,
    names::Vector{String},
    uuids::Vector{UUID} = UUID[];
    force::Bool = false,
)::Nothing
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
                contains(line, r"^ \s* \[ \s* packages \s* \] \s* $"x) && break
            end
            # find lines with uuid or name we're looking for
            for line in eachline(io)
                contains(line,regex) || continue
                m = match(line_re, line)
                m == nothing &&
                    error("misformatted registry.toml package entry: $line")
                uuid = UUID(m.captures[1])
                name = Base.unescape_string(m.captures[2])
                path = abspath(registry, Base.unescape_string(m.captures[3]))
                push!(get!(env.uuids, name, typeof(uuid)[]), uuid)
                push!(get!(env.paths, uuid, typeof(path)[]), path)
            end
        end
    end
end

find_registered!(env::EnvCache, uuids::Vector{UUID}; force::Bool=false)::Nothing =
    find_registered!(env, String[], uuids, force=force)

# Lookup all packages in project & manifest files
find_registered!(env::EnvCache)::Nothing =
    find_registered!(env, String[], UUID[], force=true)

# Get registered uuids associated with a package name
function registered_uuids(env::EnvCache, name::String)::Vector{UUID}
    find_registered!(env, [name], UUID[])
    return unique(env.uuids[name])
end

# Get registered paths associated with a package uuid
function registered_paths(env::EnvCache, uuid::UUID)::Vector{String}
    find_registered!(env, String[], [uuid])
    return env.paths[uuid]
end

#Get registered names associated with a package uuid
function registered_names(env::EnvCache, uuid::UUID)::Vector{String}
    find_registered!(env, String[], [uuid])
    String[n for (n, uuids) in env.uuids for u in uuids if u == uuid]
end

# Determine a single UUID for a given name, prompting if needed
function registered_uuid(env::EnvCache, name::String)::UUID
    uuids = registered_uuids(env, name)
    length(uuids) == 0 && return UUID(zero(UInt128))
    choices::Vector{String} = []
    choices_cache::Vector{Tuple{UUID, String}} = []
    for uuid in uuids
        values = registered_info(env, uuid, "repo")
        for value in values
            depot = "(unknown)"
            for d in depots()
                r = joinpath(d, "registries")
                startswith(value[1], r) || continue
                depot = split(relpath(value[1], r), Base.Filesystem.path_separator_re)[1]
                break
            end
            push!(choices, "Registry: $depot - Path: $(value[2])")
            push!(choices_cache, (uuid, value[1]))
        end
    end
    length(choices_cache) == 1 && return choices_cache[1][1]
    # prompt for which UUID was intended:
    menu = RadioMenu(choices)
    choice = request("There are multiple registered `$name` packages, choose one:", menu)
    choice == -1 && return UUID(zero(UInt128))
    env.paths[choices_cache[choice][1]] = [choices_cache[choice][2]]
    return choices_cache[choice][1]
end

# Determine current name for a given package UUID
function registered_name(env::EnvCache, uuid::UUID)::String
    names = registered_names(env, uuid)
    length(names) == 0 && return ""
    length(names) == 1 && return names[1]
    values = registered_info(env, uuid, "name")
    name = nothing
    for value in values
        name  == nothing && (name = value[2])
        name != value[2] && cmderror("package `$uuid` has multiple registered name values: $name, $(value[2])")
    end
    return name
end

# Return most current package info for a registered UUID
function registered_info(env::EnvCache, uuid::UUID, key::String)
    paths = env.paths[uuid]
    isempty(paths) && cmderror("`$uuid` is not registered")
    values = []
    for path in paths
        info = parse_toml(path, "package.toml")
        value = get(info, key, nothing)
        push!(values, (path, value))
    end
    return values
end

# Find package by UUID in the manifest file
function manifest_info(env::EnvCache, uuid::UUID)::Union{Dict{String,Any},Nothing}
    uuid in values(env.uuids) || find_registered!(env, [uuid])
    for (name, infos) in env.manifest, info in infos
        haskey(info, "uuid") && uuid == UUID(info["uuid"]) || continue
        return merge!(Dict{String,Any}("name" => name), info)
    end
    return nothing
end

# Give a short path string representation
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
    if !Sys.iswindows() && isabspath(path)
        home = joinpath(homedir(), "")
        if startswith(path, home)
            path = joinpath("~", path[nextind(path, lastindex(home)):end])
        end
    end
    return repr(path)
end


###########
# Context #
###########
function gather_stdlib_uuids()
    stdlib_uuids = UUID[]
    stdlib_dir = joinpath(Sys.BINDIR, "..", "share", "julia", "site", "v$(VERSION.major).$(VERSION.minor)")
    for stdlib in readdir(stdlib_dir)
        projfile = joinpath(stdlib_dir, stdlib, "Project.toml")
        if isfile(projfile)
            proj = TOML.parsefile(joinpath(stdlib_dir, stdlib, "Project.toml"))
            if haskey(proj, "uuid")
                push!(stdlib_uuids, UUID(proj["uuid"]))
            end
        end
    end
    return stdlib_uuids
end

# ENV variables to set some of these defaults?
Base.@kwdef mutable struct Context
    env::EnvCache = EnvCache()
    preview::Bool = false
    use_libgit2_for_all_downloads::Bool = false
    num_concurrent_downloads::Int = 8
    graph_verbose::Bool = false
    stdlib_uuids::Vector{UUID} = gather_stdlib_uuids()
end

function Context!(ctx::Context; kwargs...)
    for (k, v) in kwargs
        setfield!(ctx, k, v)
    end
end

function write_env(ctx::Context)
    env = ctx.env
    # load old environment for comparison
    old_env = EnvCache(env.env)
    # update the project file
    project = deepcopy(env.project)
    isempty(project["deps"]) && delete!(project, "deps")
    if !isempty(project) || ispath(env.project_file)
        @info "Updating $(pathrepr(env, env.project_file))"
        Pkg3.Display.print_project_diff(old_env, env)
        if !ctx.preview
            mkpath(dirname(env.project_file))
            open(env.project_file, "w") do io
                TOML.print(io, project, sorted=true)
            end
        end
    end
    # update the manifest file
    if !isempty(env.manifest) || ispath(env.manifest_file)
        @info "Updating $(pathrepr(env, env.manifest_file))"
        Pkg3.Display.print_manifest_diff(old_env, env)
        manifest = deepcopy(env.manifest)
        uniques = sort!(collect(keys(manifest)), by=lowercase)
        filter!(name->length(manifest[name]) == 1, uniques)
        uuids = Dict(name => UUID(manifest[name][1]["uuid"]) for name in uniques)
        for (name, infos) in manifest, info in infos
            haskey(info, "deps") || continue
            deps = Dict{String,UUID}(n => UUID(u) for (n, u) in info["deps"])
            all(d in uniques && uuids[d] == u for (d, u) in deps) || continue
            info["deps"] = sort!(collect(keys(deps)))
        end
        if !ctx.preview
            open(env.manifest_file, "w") do io
                TOML.print(io, manifest, sorted=true)
            end
        end
    end
end

end # module
