# This file is a part of Julia. License is MIT: https://julialang.org/license

module Types

using UUIDs
using Random
using Dates
import LibGit2
import REPL
using REPL.TerminalMenus

using ..TOML
import ..Pkg, ..UPDATED_REGISTRY_THIS_SESSION
import Pkg: GitTools, depots, depots1, logdir

import Base: SHA1
using SHA

export UUID, pkgID, SHA1, VersionRange, VersionSpec, empty_versionspec,
    Requires, Fixed, merge_requires!, satisfies, ResolverError,
    PackageSpec, EnvCache, Context, Context!,
    PkgError, pkgerror, has_name, has_uuid, write_env, parse_toml, find_registered!,
    project_resolve!, project_deps_resolve!, manifest_resolve!, registry_resolve!, stdlib_resolve!, handle_repos_develop!, handle_repos_add!, ensure_resolved,
    manifest_info, registered_uuids, registered_paths, registered_uuid, registered_name,
    read_project, read_package, read_manifest, pathrepr, registries,
    PackageMode, PKGMODE_MANIFEST, PKGMODE_PROJECT, PKGMODE_COMBINED,
    UpgradeLevel, UPLEVEL_FIXED, UPLEVEL_PATCH, UPLEVEL_MINOR, UPLEVEL_MAJOR,
    PackageSpecialAction, PKGSPEC_NOTHING, PKGSPEC_PINNED, PKGSPEC_FREED, PKGSPEC_DEVELOPED, PKGSPEC_TESTED, PKGSPEC_REPO_ADDED,
    printpkgstyle,
    projectfile_path


include("versions.jl")

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
const uuid_julia_project = uuid5(uuid_dns, "julialang.org")
const uuid_package = uuid5(uuid_julia_project, "package")
const uuid_registry = uuid5(uuid_julia_project, "registry")
const uuid_julia = uuid5(uuid_package, "julia")

## user-friendly representation of package IDs ##
function pkgID(p::UUID, uuid_to_name::Dict{UUID,String})
    name = get(uuid_to_name, p, "")
    isempty(name) && (name = "(unknown)")
    uuid_short = string(p)[1:8]
    return "$name [$uuid_short]"
end

####################
# Requires / Fixed #
####################
const Requires = Dict{UUID,VersionSpec}

function merge_requires!(A::Requires, B::Requires)
    for (pkg, vers) in B
        A[pkg] = haskey(A, pkg) ? intersect(A[pkg], vers) : vers
    end
    return A
end

satisfies(pkg::UUID, ver::VersionNumber, reqs::Requires) =
    !haskey(reqs, pkg) || in(ver, reqs[pkg])

struct Fixed
    version::VersionNumber
    requires::Requires
end
Fixed(v::VersionNumber) = Fixed(v, Requires())

Base.:(==)(a::Fixed, b::Fixed) = a.version == b.version && a.requires == b.requires
Base.hash(f::Fixed, h::UInt) = hash((f.version, f.requires), h + (0x68628b809fd417ca % UInt))

Base.show(io::IO, f::Fixed) = isempty(f.requires) ?
    print(io, "Fixed(", repr(f.version), ")") :
    print(io, "Fixed(", repr(f.version), ",", f.requires, ")")


struct ResolverError <: Exception
    msg::AbstractString
    ex::Union{Exception,Nothing}
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
# Pkg Error #
#################
struct PkgError <: Exception
    msg::String
end
pkgerror(msg::String...) = throw(PkgError(join(msg)))
Base.show(io::IO, err::PkgError) = print(io, err.msg)


###############
# PackageSpec #
###############
@enum(UpgradeLevel, UPLEVEL_FIXED, UPLEVEL_PATCH, UPLEVEL_MINOR, UPLEVEL_MAJOR)
@enum(PackageMode, PKGMODE_PROJECT, PKGMODE_MANIFEST, PKGMODE_COMBINED)
@enum(PackageSpecialAction, PKGSPEC_NOTHING, PKGSPEC_PINNED, PKGSPEC_FREED,
                            PKGSPEC_DEVELOPED, PKGSPEC_TESTED, PKGSPEC_REPO_ADDED)

const VersionTypes = Union{VersionNumber,VersionSpec,UpgradeLevel}

# The url field can also be a local path, rename?
mutable struct GitRepo
    url::String
    rev::String
    git_tree_sha1::Union{Nothing,SHA1}
end

GitRepo(url::String, revspec) = GitRepo(url, revspec, nothing)
GitRepo(url::String) = GitRepo(url, "", nothing)
GitRepo(;url::Union{String, Nothing}=nothing, rev::Union{String, Nothing} =nothing) =
    GitRepo(url == nothing ? "" : url, rev == nothing ? "" : rev, nothing)
Base.:(==)(repo1::GitRepo, repo2::GitRepo) = (repo1.url == repo2.url && repo1.rev == repo2.rev && repo1.git_tree_sha1 == repo2.git_tree_sha1)

mutable struct PackageSpec
    name::String
    uuid::UUID
    version::VersionTypes
    mode::PackageMode
    path::Union{Nothing,String}
    special_action::PackageSpecialAction # If the package is currently being pinned, freed etc
    repo::Union{Nothing,GitRepo}
end
PackageSpec(name::AbstractString, uuid::UUID, version::VersionTypes,
            mode::PackageMode=PKGMODE_PROJECT, path=nothing, special_action=PKGSPEC_NOTHING,
            repo=nothing) = PackageSpec(String(name), uuid, version, mode, path, special_action, repo)
PackageSpec(name::AbstractString, uuid::UUID) =
    PackageSpec(name, uuid, VersionSpec())
PackageSpec(name::AbstractString, version::VersionTypes=VersionSpec()) =
    PackageSpec(name, UUID(zero(UInt128)), version)
PackageSpec(uuid::UUID, version::VersionTypes=VersionSpec()) =
    PackageSpec("", uuid, version)
function PackageSpec(repo::GitRepo)
    pkg = PackageSpec()
    pkg.repo = repo
    return pkg
end

# kwarg constructor
function PackageSpec(;name::AbstractString="", uuid::Union{String, UUID}=UUID(0),
                     version::Union{VersionNumber, String, VersionSpec} = VersionSpec(),
                     url = nothing, rev = nothing, path=nothing, mode::PackageMode = PKGMODE_PROJECT)
    if url !== nothing || path !== nothing || rev !== nothing
        if path !== nothing || url !== nothing
            path !== nothing && url !== nothing && pkgerror("cannot specify both path and url")
            url = url == nothing ? path : url
        end
        repo = GitRepo(url=url, rev=rev)
    else
        repo = nothing
    end

    version = VersionSpec(version)
    uuid isa String && (uuid = UUID(uuid))
    PackageSpec(name, uuid, version, mode, nothing, PKGSPEC_NOTHING, repo)
end

has_name(pkg::PackageSpec) = !isempty(pkg.name)
has_uuid(pkg::PackageSpec) = pkg.uuid != UUID(zero(UInt128))

function Base.show(io::IO, pkg::PackageSpec)
    vstr = repr(pkg.version)
    f = ["name" => pkg.name, "uuid" => has_uuid(pkg) ? pkg.uuid : "", "v" => (vstr == "VersionSpec(\"*\")" ? "" : vstr)]
    if pkg.repo !== nothing
        if !isempty(pkg.repo.url)
            push!(f, "url/path" => string("\"", pkg.repo.url, "\""))
        end
        if !isempty(pkg.repo.rev)
            push!(f, "rev" => pkg.repo.rev)
        end
    end
    print(io, "PackageSpec(")
    first = true
    for (field, value) in f
        value == "" && continue
        first || print(io, ", ")
        print(io, field, "=", value)
        first = false
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

let trynames(names) = begin
    return root_path::AbstractString -> begin
        for x in names
            maybe_file = joinpath(root_path, x)
            if isfile(maybe_file)
                return maybe_file
            end
        end
    end
end # trynames
    global projectfile_path = trynames(Base.project_names)
    global manifestfile_path = trynames(Base.manifest_names)
end # let

function find_project_file(env::Union{Nothing,String}=nothing)
    project_file = nothing
    if env isa Nothing
        project_file = Base.active_project()
        project_file == nothing && pkgerror("no active project")
    elseif startswith(env, '@')
        project_file = Base.load_path_expand(env)
        project_file === nothing && pkgerror("package environment does not exist: $env")
    elseif env isa String
        if isdir(env)
            isempty(readdir(env)) || pkgerror("environment is a package directory: $env")
            project_file = joinpath(env, Base.project_names[end])
        else
            project_file = endswith(env, ".toml") ? abspath(env) :
                abspath(env, Base.project_names[end])
        end
    end
    @assert project_file isa String &&
        (isfile(project_file) || !ispath(project_file) ||
         isdir(project_file) && isempty(readdir(project_file)))
     return project_file
end

mutable struct EnvCache
    # environment info:
    env::Union{Nothing,String}
    git::Union{Nothing,String}

    # paths for files:
    project_file::String
    manifest_file::String

    # name / uuid of the project
    pkg::Union{PackageSpec, Nothing}

    # cache of metadata:
    project::Dict
    manifest::Dict

    # registered package info:
    uuids::Dict{String,Vector{UUID}}
    paths::Dict{UUID,Vector{String}}
    names::Dict{UUID,Vector{String}}
end

function EnvCache(env::Union{Nothing,String}=nothing)
    project_file = find_project_file(env)
    project_dir = dirname(project_file)
    git = ispath(joinpath(project_dir, ".git")) ? project_dir : nothing

    project = read_project(project_file)
    if any(haskey.((project,), ["name", "uuid", "version"]))
        project_package = PackageSpec(
            get(project, "name", ""),
            UUID(get(project, "uuid", 0)),
            VersionNumber(get(project, "version", "0.0")),
        )
    else
        project_package = nothing
    end
    # determine manifest_file name
    dir = abspath(dirname(project_file))
    manifest_file = haskey(project, "manifest") ?
        abspath(project["manifest"]) :
        manifestfile_path(dir)
    # use default name if still not determined
    (manifest_file === nothing) && (manifest_file = joinpath(dir, "Manifest.toml"))
    write_env_usage(manifest_file)
    manifest = read_manifest(manifest_file)
    uuids = Dict{String,Vector{UUID}}()
    paths = Dict{UUID,Vector{String}}()
    names = Dict{UUID,Vector{String}}()
    return EnvCache(env,
        git,
        project_file,
        manifest_file,
        project_package,
        project,
        manifest,
        uuids,
        paths,
        names,)
end

collides_with_project(env::EnvCache, pkg::PackageSpec) =
    is_project_name(env, pkg.name) || is_project_uuid(env, pkg.uuid)
is_project(env::EnvCache, pkg::PackageSpec) = is_project_uuid(env, pkg.uuid)
is_project_name(env::EnvCache, name::String) =
    env.pkg !== nothing && env.pkg.name == name
is_project_uuid(env::EnvCache, uuid::UUID) =
    env.pkg !== nothing && env.pkg.uuid == uuid



###########
# Context #
###########
stdlib_dir() = normpath(joinpath(Sys.BINDIR, "..", "share", "julia", "stdlib", "v$(VERSION.major).$(VERSION.minor)"))
stdlib_path(stdlib::String) = joinpath(stdlib_dir(), stdlib)
function gather_stdlib_uuids()
    stdlibs = Dict{UUID,String}()
    for stdlib in readdir(stdlib_dir())
        projfile = projectfile_path(stdlib_path(stdlib))
        if nothing !== projfile
            proj = TOML.parsefile(projfile)
            if haskey(proj, "uuid")
                stdlibs[UUID(proj["uuid"])] = stdlib
            end
        end
    end
    return stdlibs
end

# ENV variables to set some of these defaults?
Base.@kwdef mutable struct Context
    env::EnvCache = EnvCache()
    preview::Bool = false
    use_libgit2_for_all_downloads::Bool = false
    use_only_tarballs_for_downloads::Bool = false
    num_concurrent_downloads::Int = 8
    graph_verbose::Bool = false
    stdlibs::Dict{UUID,String} = gather_stdlib_uuids()
    # Remove next field when support for Pkg2 CI scripts is removed
    currently_running_target::Bool = false
    old_pkg2_clone_name::String = ""
end

function Context!(kw_context::Vector{Pair{Symbol,Any}})::Context
    ctx = Context()
    for (k, v) in kw_context
        setfield!(ctx, k, v)
    end
    return ctx
end

function Context!(ctx::Context; kwargs...)
    for (k, v) in kwargs
        setfield!(ctx, k, v)
    end
end

# target === nothing : main dependencies
# target === "*"     : main + all extras
# target === "name"  : named target deps

function deps_names(project::Dict, target::Union{Nothing,String}=nothing)::Vector{String}
    deps = sort!(collect(keys(project["deps"])))
    target == "*" && return !haskey(project, "extras") ? deps :
        sort!(union!(deps, collect(keys(project["extras"]))))
    haskey(project, "targets") || return deps
    targets = project["targets"]
    haskey(targets, target) || return deps
    return sort!(union!(deps, targets[target]))
end

function get_deps(project::Dict, target::Union{Nothing,String}=nothing)
    names = deps_names(project, target)
    deps = filter(((dep, _),) -> dep in names, project["deps"])
    extras = get(project, "extras", Dict{String,Any}())
    for name in names
        haskey(deps, name) && continue
        haskey(extras, name) ||
            pkgerror("target `$target` has unlisted dependency `$name`")
        deps[name] = extras[name]
    end
    return deps
end
get_deps(env::EnvCache, target::Union{Nothing,String}=nothing) =
    get_deps(env.project, target)
get_deps(ctx::Context, target::Union{Nothing,String}=nothing) =
    get_deps(ctx.env, target)

function project_compatibility(ctx::Context, name::String)
    v = VersionSpec()
    project = ctx.env.project
    compat = get(project, "compat", Dict())
    if haskey(compat, name)
        v = VersionSpec(semver_spec(compat[name]))
    end
    return v
end

function write_env_usage(manifest_file::AbstractString)
    !ispath(logdir()) && mkpath(logdir())
    usage_file = joinpath(logdir(), "manifest_usage.toml")
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
    isfile(file) ? open(read_project, file) : read_project(devnull)
end

_throw_package_err(x, f) = pkgerror("expected a `$x` entry in project file at $(abspath(f))")
function read_package(f::String)
    project = read_project(f)
    haskey(project, "name") || _throw_package_err("name", f)
    haskey(project, "uuid") || _throw_package_err("uuid", f)
    name = project["name"]
    entry = joinpath(dirname(f), "src", "$name.jl")
    if !isfile(entry)
        pkgerror("expected the file `src/$name.jl` to exist for package $name at $(dirname(f))")
    end
    return project
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
    try isfile(file) ? open(read_manifest, file) : read_manifest(devnull)
    catch err
        err isa ErrorException && startswith(err.msg, "ambiguious dependency") || rethrow(err)
        err.msg *= "In manifest file: $file"
        rethrow(err)
    end
end


const refspecs = ["+refs/*:refs/remotes/cache/*"]
const reg_pkg = r"(?:^|[/\\])(\w+?)(?:\.jl)?(?:\.git)?(?:\/)?$"

# Windows sometimes throw on `isdir`...
function isdir_windows_workaround(path::String)
    try isdir(path)
    catch e
        false
    end
end

# try to call realpath on as much as possible
function safe_realpath(path)
    ispath(path) && return realpath(path)
    a, b = splitdir(path)
    return joinpath(safe_realpath(a), b)
end
function relative_project_path(ctx::Context, path::String)
    # compute path relative the project
    # realpath needed to expand symlinks before taking the relative path
    return relpath(safe_realpath(abspath(path)),
                   safe_realpath(dirname(ctx.env.project_file)))
end

casesensitive_isdir(dir::String) = isdir_windows_workaround(dir) && dir in readdir(joinpath(dir, ".."))

function handle_repos_develop!(ctx::Context, pkgs::AbstractVector{PackageSpec}; shared::Bool)
    Base.shred!(LibGit2.CachedCredentials()) do creds
        env = ctx.env
        new_uuids = UUID[]
        for pkg in pkgs
            pkg.repo == nothing && continue
            pkg.special_action = PKGSPEC_DEVELOPED
            isempty(pkg.repo.url) && set_repo_for_pkg!(env, pkg)


            if isdir_windows_workaround(pkg.repo.url)
                # Developing a local package, just point `pkg.path` to it
                if isabspath(pkg.repo.url)
                    # absolute paths should stay absolute
                    pkg.path = pkg.repo.url
                else
                    # Relative paths are given relative pwd() so we
                    # translate that to be relative the project instead.
                    pkg.path = relative_project_path(ctx, pkg.repo.url)
                end
                folder_already_downloaded = true
                project_path = pkg.repo.url
                parse_package!(ctx, pkg, project_path)
            else
                # Only update the registry in case of developing a non-local package
                UPDATED_REGISTRY_THIS_SESSION[] || Pkg.API.update_registry(ctx)
                # We save the repo in case another environement wants to
                # develop from the same repo, this avoids having to reclone it
                # from scratch.
                clone_path = joinpath(depots1(), "clones")
                mkpath(clone_path)
                repo_path = joinpath(clone_path, string(hash(pkg.repo.url), "_full"))
                repo = nothing
                try
                    repo, just_cloned = ispath(repo_path) ? (LibGit2.GitRepo(repo_path), false) : begin
                        r = GitTools.clone(pkg.repo.url, repo_path)
                        GitTools.fetch(r, pkg.repo.url; refspecs=refspecs, credentials=creds)
                        r, true
                    end
                    if !just_cloned
                        GitTools.fetch(repo, pkg.repo.url; refspecs=refspecs, credentials=creds)
                    end
                finally
                    repo isa LibGit2.GitRepo && LibGit2.close(repo)
                end

                # Copy the repo to a temporary place and check out the rev
                project_path = mktempdir()
                cp(repo_path, project_path; force=true)
                LibGit2.with(LibGit2.GitRepo(project_path)) do repo
                    if LibGit2.isattached(repo)
                        rev = LibGit2.branch(repo)
                    else
                        rev = string(LibGit2.GitHash(LibGit2.head(repo)))
                    end
                    gitobject, isbranch = get_object_branch(repo, rev, creds)
                    try
                        LibGit2.transact(repo) do r
                            if isbranch
                                LibGit2.branch!(r, rev, track=LibGit2.Consts.REMOTE_ORIGIN)
                            else
                                LibGit2.checkout!(r, string(LibGit2.GitHash(gitobject)))
                            end
                        end
                    finally
                        close(gitobject)
                    end
                end

                parse_package!(ctx, pkg, project_path)
                devdir = shared ? Pkg.devdir() : joinpath(dirname(ctx.env.project_file), "dev")
                dev_pkg_path = joinpath(devdir, pkg.name)
                if isdir(dev_pkg_path)
                    if !isfile(joinpath(dev_pkg_path, "src", pkg.name * ".jl"))
                        pkgerror("Path `$(dev_pkg_path)` exists but it does not contain `src/$(pkg.name).jl")
                    else
                        @info "Path `$(dev_pkg_path)` exists and looks like the correct package, using existing path instead of cloning"
                    end
                else
                    mkpath(dev_pkg_path)
                    mv(project_path, dev_pkg_path; force=true)
                    push!(new_uuids, pkg.uuid)
                end
                # Save the path as relative if it is a --local dev,
                # otherwise put in the absolute path.
                pkg.path = shared ? dev_pkg_path : relative_project_path(ctx, dev_pkg_path)
            end
            @assert pkg.path != nothing
            @assert has_uuid(pkg)
        end
        return new_uuids
    end
end

function handle_repos_add!(ctx::Context, pkgs::AbstractVector{PackageSpec};
                           upgrade_or_add::Bool=true, credentials=nothing)
    # Always update the registry when adding
    UPDATED_REGISTRY_THIS_SESSION[] || Pkg.API.update_registry(ctx)
    creds = credentials !== nothing ? credentials : LibGit2.CachedCredentials()
    try
        env = ctx.env
        new_uuids = UUID[]
        for pkg in pkgs
            pkg.repo == nothing && continue
            pkg.special_action = PKGSPEC_REPO_ADDED
            isempty(pkg.repo.url) && set_repo_for_pkg!(env, pkg)
            clones_dir = joinpath(depots1(), "clones")
            mkpath(clones_dir)
            repo_path = joinpath(clones_dir, string(hash(pkg.repo.url)))
            repo = nothing
            do_nothing_more = false
            project_path = nothing
            folder_already_downloaded = false
            try
                repo = if ispath(repo_path)
                    LibGit2.GitRepo(repo_path)
                else
                    GitTools.clone(pkg.repo.url, repo_path, isbare=true, credentials=creds)
                end
                info = manifest_info(env, pkg.uuid)
                pinned = (info != nothing && get(info, "pinned", false))
                upgrading = upgrade_or_add && !pinned
                if upgrading
                    GitTools.fetch(repo; refspecs=refspecs, credentials=creds)
                    rev = pkg.repo.rev
                    # see if we can get rev as a branch
                    if isempty(rev)
                        if LibGit2.isattached(repo)
                            rev = LibGit2.branch(repo)
                        else
                            rev = string(LibGit2.GitHash(LibGit2.head(repo)))
                        end
                    end
                else
                    # Not upgrading so the rev should be the current git-tree-sha
                    rev = info["git-tree-sha1"]
                    pkg.version = VersionNumber(info["version"])
                end

                gitobject, isbranch = get_object_branch(repo, rev, creds)
                # If the user gave a shortened commit SHA, might as well update it to the full one
                try
                    if upgrading
                        pkg.repo.rev = isbranch ? rev : string(LibGit2.GitHash(gitobject))
                    end
                    LibGit2.with(LibGit2.peel(LibGit2.GitTree, gitobject)) do git_tree
                        @assert git_tree isa LibGit2.GitTree
                        pkg.repo.git_tree_sha1 = SHA1(string(LibGit2.GitHash(git_tree)))
                            version_path = nothing
                            folder_already_downloaded = false
                        if has_uuid(pkg) && has_name(pkg)
                            version_path = Pkg.Operations.find_installed(pkg.name, pkg.uuid, pkg.repo.git_tree_sha1)
                            isdir(version_path) && (folder_already_downloaded = true)
                            info = manifest_info(env, pkg.uuid)
                            if info != nothing && get(info, "git-tree-sha1", "") == string(pkg.repo.git_tree_sha1) && folder_already_downloaded
                                # Same tree sha and this version already downloaded, nothing left to do
                                do_nothing_more = true
                            end
                        end
                        if folder_already_downloaded
                            project_path = version_path
                        else
                            project_path = mktempdir()
                            opts = LibGit2.CheckoutOptions(checkout_strategy=LibGit2.Consts.CHECKOUT_FORCE,
                                target_directory=Base.unsafe_convert(Cstring, project_path))
                            LibGit2.checkout_tree(repo, git_tree, options=opts)
                        end
                    end
                finally
                    close(gitobject)
                end
            finally
                repo isa LibGit2.GitRepo && close(repo)
            end
            do_nothing_more && continue
            parse_package!(ctx, pkg, project_path)
            if !folder_already_downloaded
                version_path = Pkg.Operations.find_installed(pkg.name, pkg.uuid, pkg.repo.git_tree_sha1)
                mkpath(version_path)
                mv(project_path, version_path; force=true)
                push!(new_uuids, pkg.uuid)
            end
            @assert has_uuid(pkg)
        end
        return new_uuids
    finally
        creds !== credentials && Base.shred!(creds)
    end
end

function parse_package!(ctx, pkg, project_path)
    env = ctx.env
    project_file = projectfile_path(project_path)
    if project_file !== nothing
        project_data = read_package(project_file)
        pkg.uuid = UUID(project_data["uuid"])
        pkg.name = project_data["name"]
    else
        if !isempty(ctx.old_pkg2_clone_name) # remove when legacy CI script support is removed
            pkg.name = ctx.old_pkg2_clone_name
        else
            # This is an old style package, if not set, get the name from src/PackageName
            if !has_name(pkg)
                if isdir_windows_workaround(pkg.repo.url)
                    m = match(reg_pkg, abspath(pkg.repo.url))
                else
                    m = match(reg_pkg, pkg.repo.url)
                end
                m === nothing && pkgerror("cannot determine package name from URL or path: $(pkg.repo.url), provide a name argument to `PackageSpec`")
                pkg.name = m.captures[1]
            end
        end
        reg_uuids = registered_uuids(env, pkg.name)
        is_registered = !isempty(reg_uuids)
        if !is_registered
            # This is an unregistered old style package, give it a UUID and a version
            if !has_uuid(pkg)
                uuid_unreg_pkg = UUID(0xa9a2672e746f11e833ef119c5b888869)
                pkg.uuid = uuid5(uuid_unreg_pkg, pkg.name)
                @info "Assigning UUID $(pkg.uuid) to $(pkg.name)"
            end
        else
            @assert length(reg_uuids) == 1
            pkg.uuid = reg_uuids[1]
        end
    end
end

function set_repo_for_pkg!(env, pkg)
    if !has_uuid(pkg)
        registry_resolve!(env, [pkg])
        ensure_resolved(env, [pkg]; registry=true)
    end
    # TODO: look into [1]
    _, pkg.repo.url = Types.registered_info(env, pkg.uuid, "repo")[1]
end

function get_object_branch(repo, rev, creds)
    gitobject = nothing
    isbranch = false
    try
        gitobject = LibGit2.GitObject(repo, "remotes/cache/heads/" * rev)
        isbranch = true
    catch err
        err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
    end
    if gitobject == nothing
        try
            gitobject = LibGit2.GitObject(repo, rev)
        catch err
            err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
            GitTools.fetch(repo; refspecs=refspecs, credentials=creds)
            try
                gitobject = LibGit2.GitObject(repo, rev)
            catch err
                err isa LibGit2.GitError && err.code == LibGit2.Error.ENOTFOUND || rethrow(err)
                pkgerror("git object $(rev) could not be found")
            end
        end
    end
    return gitobject, isbranch
end

########################################
# Resolving packages from name or uuid #
########################################

function project_resolve!(env::EnvCache, pkgs::AbstractVector{PackageSpec})
    for pkg in pkgs
        if has_uuid(pkg) && !has_name(pkg) && Types.is_project_uuid(env, pkg.uuid)
            pkg.name = env.pkg.name
        end
        if has_name(pkg) && !has_uuid(pkg) && Types.is_project_name(env, pkg.name)
            pkg.uuid = env.pkg.uuid
        end
    end
end

# Disambiguate name/uuid package specifications using project info.
function project_deps_resolve!(env::EnvCache, pkgs::AbstractVector{PackageSpec})
    uuids = env.project["deps"]
    names = Dict(uuid => name for (name, uuid) in uuids)
    length(uuids) < length(names) && # TODO: handle this somehow?
        pkgerror("duplicate UUID found in project file's [deps] section")
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
    any(pkg -> has_name(pkg) ⊻ has_uuid(pkg), pkgs) || return
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

function stdlib_resolve!(ctx::Context, pkgs::AbstractVector{PackageSpec})
    for pkg in pkgs
        @assert has_name(pkg) || has_uuid(pkg)
        if has_name(pkg) && !has_uuid(pkg)
            for (uuid, name) in ctx.stdlibs
                name == pkg.name && (pkg.uuid = uuid)
            end
        end
        if has_uuid(pkg) && !has_name(pkg)
            haskey(ctx.stdlibs, pkg.uuid) && (pkg.name = ctx.stdlibs[pkg.uuid])
        end
    end
end

# Ensure that all packages are fully resolved
function ensure_resolved(env::EnvCache,
    pkgs::AbstractVector{PackageSpec};
    registry::Bool=false,)::Nothing
    unresolved = Dict{String,Vector{UUID}}()
    for pkg in pkgs
        has_uuid(pkg) && continue
        uuids = UUID[]
        for (name, infos) in env.manifest, info in infos
            name == pkg.name && haskey(info, "uuid") || continue
            uuid = UUID(info["uuid"])
            uuid in uuids || push!(uuids, uuid)
        end
        sort!(uuids, by=uuid -> uuid.value)
        unresolved[pkg.name] = uuids
    end
    isempty(unresolved) && return
    msg = sprint() do io
        println(io, "The following package names could not be resolved:")
        for (name, uuids) in sort!(collect(unresolved), by=lowercase ∘ first)
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
    pkgerror(msg)
end

const DEFAULT_REGISTRIES = Dict("General" => "https://github.com/JuliaRegistries/General.git")

# Return paths of all registries in a depot
function registries(depot::String)::Vector{String}
    d = joinpath(depot, "registries")
    ispath(d) || return String[]
    regs = filter!(readdir(d)) do r
        isfile(joinpath(d, r, "Registry.toml"))
    end
    String[joinpath(depot, "registries", r) for r in regs]
end

# Return paths of all registries in all depots
function registries(; clone_default=true)::Vector{String}
    isempty(depots()) && return String[]
    user_regs = abspath(depots1(), "registries")
    # TODO: delete the following let block in Julia 1.0
    let uncurated = joinpath(user_regs, "Uncurated"),
        general = joinpath(user_regs, "General")
        if ispath(uncurated) && !ispath(general)
            mv(uncurated, general)
            git_config_file = joinpath(general, ".git", "config")
            cfg = read(git_config_file, String)
            cfg = replace(cfg, r"\bUncurated\b" => "General")
            write(git_config_file, cfg)
        end
    end
    if clone_default
        if !ispath(user_regs) || isempty(readdir(user_regs))
            mkpath(user_regs)
            Base.shred!(LibGit2.CachedCredentials()) do creds
                printpkgstyle(stdout, :Cloning, "default registries into $user_regs")
                for (reg, url) in DEFAULT_REGISTRIES
                    path = joinpath(user_regs, reg)
                    LibGit2.with(GitTools.clone(url, path; header = "registry $reg from $(repr(url))", credentials = creds)) do repo
                    end
                end
            end
        end
    end
    return [r for d in depots() for r in registries(d)]
end

# path -> (mtime, TOML Dict)
const REGISTRY_CACHE = Dict{String, Tuple{Float64, Dict{String, Any}}}()

function read_registry(reg_file)
    t = mtime(reg_file)
    if haskey(REGISTRY_CACHE, reg_file)
        prev_t, registry = REGISTRY_CACHE[reg_file]
        t == prev_t && return registry
    end
    registry = TOML.parsefile(reg_file)
    REGISTRY_CACHE[reg_file] = (t, registry)
    return registry
end


# Lookup package names & uuids in a single pass through registries
function find_registered!(env::EnvCache,
    names::Vector{String},
    uuids::Vector{UUID}=UUID[];
    force::Bool=false,
)::Nothing
    # only look if there's something new to see (or force == true)
    names = filter(name -> !haskey(env.uuids, name), names)
    uuids = filter(uuid -> !haskey(env.paths, uuid), uuids)
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
    # initialize env entries for names and uuids
    for name in names; env.uuids[name] = UUID[]; end
    for uuid in uuids; env.paths[uuid] = String[]; end
    for uuid in uuids; env.names[uuid] = String[]; end

    # note: empty vectors will be left for names & uuids that aren't found
    for registry in registries()
        data = read_registry(joinpath(registry, "Registry.toml"))
        for (_uuid, pkgdata) in data["packages"]
              uuid = UUID(_uuid)
              name = pkgdata["name"]
              path = abspath(registry, pkgdata["path"])
              push!(get!(env.uuids, name, UUID[]), uuid)
              push!(get!(env.paths, uuid, String[]), path)
              push!(get!(env.names, uuid, String[]), name)
        end
    end
    for d in (env.uuids, env.paths, env.names)
        for (k, v) in d
            unique!(v)
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
    return env.names[uuid]
end

# Determine a single UUID for a given name, prompting if needed
function registered_uuid(env::EnvCache, name::String)::UUID
    uuids = registered_uuids(env, name)
    length(uuids) == 0 && return UUID(zero(UInt128))
    choices::Vector{String} = []
    choices_cache::Vector{Tuple{UUID,String}} = []
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
    if isinteractive()
        # prompt for which UUID was intended:
        menu = RadioMenu(choices)
        choice = request("There are multiple registered `$name` packages, choose one:", menu)
        choice == -1 && return UUID(zero(UInt128))
        env.paths[choices_cache[choice][1]] = [choices_cache[choice][2]]
        return choices_cache[choice][1]
    else
        pkgerror("there are multiple registered `$name` packages, explicitly set the uuid")
    end
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
        name != value[2] && pkgerror("package `$uuid` has multiple registered name values: $name, $(value[2])")
    end
    return name
end

# Return most current package info for a registered UUID
function registered_info(env::EnvCache, uuid::UUID, key::String)
    paths = env.paths[uuid]
    isempty(paths) && pkgerror("`$uuid` is not registered")
    values = []
    for path in paths
        info = parse_toml(path, "Package.toml")
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

# TODO: redirect to ctx stream
function printpkgstyle(io::IO, cmd::Symbol, text::String, ignore_indent::Bool=false)
    indent = textwidth(string(:Downloaded))
    ignore_indent && (indent = 0)
    printstyled(io, lpad(string(cmd), indent), color=:green, bold=true)
    println(io, " ", text)
end

# TODO: use ctx specific context
function printpkgstyle(ctx::Context, cmd::Symbol, text::String, ignore_indent::Bool=false)
    printpkgstyle(stdout, cmd, text)
end


function pathrepr(path::String)
    # print stdlib paths as @stdlib/Name
    if startswith(path, stdlib_dir())
        path = "@stdlib/" * basename(path)
    end
    return "`" * Base.contractuser(path) * "`"
end

function project_key_order(key::String)
    key == "name"     && return 1
    key == "uuid"     && return 2
    key == "keywords" && return 3
    key == "license"  && return 4
    key == "desc"     && return 5
    key == "deps"     && return 6
    key == "compat"   && return 7
    return 8
end

function write_env(ctx::Context; display_diff=true)
    env = ctx.env
    # load old environment for comparison
    old_env = EnvCache(env.env)
    # update the project file
    project = deepcopy(env.project)
    isempty(project["deps"]) && delete!(project, "deps")
    if !isempty(project) || ispath(env.project_file)
        if display_diff && !(ctx.currently_running_target)
            printpkgstyle(ctx, :Updating, pathrepr(env.project_file))
            Pkg.Display.print_project_diff(ctx, old_env, env)
        end
        if !ctx.preview
            mkpath(dirname(env.project_file))
            open(env.project_file, "w") do io
                TOML.print(io, project, sorted=true, by=key -> (project_key_order(key), key))
            end
        end
    end
    # update the manifest file
    if !isempty(env.manifest) || ispath(env.manifest_file)
        if display_diff && !(ctx.currently_running_target)
            printpkgstyle(ctx, :Updating, pathrepr(env.manifest_file))
            Pkg.Display.print_manifest_diff(ctx, old_env, env)
        end
        manifest = deepcopy(env.manifest)
        uniques = sort!(collect(keys(manifest)), by=lowercase)
        filter!(name -> length(manifest[name]) == 1, uniques)
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
