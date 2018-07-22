# This file is a part of Julia. License is MIT: https://julialang.org/license

module API

using UUIDs
using Printf
import Random
import Dates
import LibGit2

import ..depots, ..logdir, ..devdir
import ..Operations, ..Display, ..GitTools, ..Pkg, ..UPDATED_REGISTRY_THIS_SESSION
using ..Types, ..TOML


preview_info() = printstyled("───── Preview mode ─────\n"; color=Base.info_color(), bold=true)

include("generate.jl")

parse_package(pkg) = Pkg.REPLMode.parse_package(pkg; add_or_develop=true)

add_or_develop(pkg::Union{String, PackageSpec}; kwargs...) = add_or_develop([pkg]; kwargs...)
add_or_develop(pkgs::Vector{String}; kwargs...)            = add_or_develop([parse_package(pkg) for pkg in pkgs]; kwargs...)
add_or_develop(pkgs::Vector{PackageSpec}; kwargs...)       = add_or_develop(Context(), pkgs; kwargs...)

function add_or_develop(ctx::Context, pkgs::Vector{PackageSpec}; mode::Symbol, kwargs...)
    Context!(ctx; kwargs...)

    # if julia is passed as a package the solver gets tricked;
    # this catches the error early on
    any(pkg->(pkg.name == "julia"), pkgs) &&
        cmderror("Trying to $mode julia as a package")

    ctx.preview && preview_info()
    if !UPDATED_REGISTRY_THIS_SESSION[]
        update_registry(ctx)
    end
    if mode == :develop
        new_git = handle_repos_develop!(ctx, pkgs)
    else
        new_git = handle_repos_add!(ctx, pkgs; upgrade_or_add=true)
    end
    project_deps_resolve!(ctx.env, pkgs)
    registry_resolve!(ctx.env, pkgs)
    stdlib_resolve!(ctx, pkgs)
    ensure_resolved(ctx.env, pkgs, registry=true)

    any(pkg -> Types.collides_with_project(ctx.env, pkg), pkgs) &&
        cmderror("Cannot $mode package with the same name or uuid as the project")

    Operations.add_or_develop(ctx, pkgs; new_git=new_git)
    ctx.preview && preview_info()
    return
end

add(args...; kwargs...) = add_or_develop(args...; mode = :add, kwargs...)
develop(args...; kwargs...) = add_or_develop(args...; mode = :develop, kwargs...)
@deprecate checkout develop


rm(pkg::Union{String, PackageSpec}; kwargs...)               = rm([pkg]; kwargs...)
rm(pkgs::Vector{String}; kwargs...)      = rm([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
rm(pkgs::Vector{PackageSpec}; kwargs...) = rm(Context(), pkgs; kwargs...)

function rm(ctx::Context, pkgs::Vector{PackageSpec}; kwargs...)
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    project_deps_resolve!(ctx.env, pkgs)
    manifest_resolve!(ctx.env, pkgs)
    Operations.rm(ctx, pkgs)
    ctx.preview && preview_info()
    return
end


function update_registry(ctx)
    # Update the registry
    errors = Tuple{String, String}[]
    if ctx.preview
        @info("Skipping updating registry in preview mode")
    else
        for reg in registries()
            if isdir(joinpath(reg, ".git"))
                regpath = pathrepr(ctx, reg)
                printpkgstyle(ctx, :Updating, "registry at " * regpath)
                LibGit2.with(LibGit2.GitRepo, reg) do repo
                    if LibGit2.isdirty(repo)
                        push!(errors, (regpath, "registry dirty"))
                        return
                    end
                    if !LibGit2.isattached(repo)
                        push!(errors, (regpath, "registry detached"))
                        return
                    end
                    branch = LibGit2.headname(repo)
                    try
                        GitTools.fetch(repo; refspecs=["+refs/heads/$branch:refs/remotes/origin/$branch"])
                    catch e
                        e isa LibGit2.GitError || rethrow(e)
                        push!(errors, (reg, "failed to fetch from repo"))
                        return
                    end
                    ff_succeeded = try
                        LibGit2.merge!(repo; branch="refs/remotes/origin/$branch", fastforward=true)
                    catch e
                        e isa LibGit2.GitError && e.code == LibGit2.Error.ENOTFOUND || rethrow(e)
                        push!(errors, (reg, "branch origin/$branch not found"))
                        return
                    end

                    if !ff_succeeded
                        try LibGit2.rebase!(repo, "origin/$branch")
                        catch e
                            e isa LibGit2.GitError || rethrow(e)
                            push!(errors, (reg, "registry failed to rebase on origin/$branch"))
                            return
                        end
                    end
                end
            end
        end
    end
    if !isempty(errors)
        warn_str = "Some registries failed to update:"
        for (reg, err) in errors
            warn_str *= "\n    — $reg — $err"
        end
        @warn warn_str
    end
    UPDATED_REGISTRY_THIS_SESSION[] = true
    return
end

up(ctx::Context; kwargs...)                    = up(ctx, PackageSpec[]; kwargs...)
up(; kwargs...)                                = up(PackageSpec[]; kwargs...)
up(pkg::Union{String, PackageSpec}; kwargs...) = up([pkg]; kwargs...)
up(pkgs::Vector{String}; kwargs...)            = up([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
up(pkgs::Vector{PackageSpec}; kwargs...)       = up(Context(), pkgs; kwargs...)

function up(ctx::Context, pkgs::Vector{PackageSpec};
            level::UpgradeLevel=UPLEVEL_MAJOR, mode::PackageMode=PKGMODE_PROJECT, do_update_registry=true, kwargs...)
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    do_update_registry && update_registry(ctx)
    if isempty(pkgs)
        if mode == PKGMODE_PROJECT
            for (name::String, uuidstr::String) in ctx.env.project["deps"]
                uuid = UUID(uuidstr)
                push!(pkgs, PackageSpec(name, uuid, level))
            end
        elseif mode == PKGMODE_MANIFEST
            for (name, infos) in ctx.env.manifest, info in infos
                uuid = UUID(info["uuid"])
                push!(pkgs, PackageSpec(name, uuid, level))
            end
        end
    else
        project_deps_resolve!(ctx.env, pkgs)
        manifest_resolve!(ctx.env, pkgs)
        ensure_resolved(ctx.env, pkgs)
    end
    Operations.up(ctx, pkgs)
    ctx.preview && preview_info()
    return
end

resolve(ctx::Context=Context()) =
    up(ctx, level=UPLEVEL_FIXED, mode=PKGMODE_MANIFEST, do_update_registry=false)

pin(pkg::Union{String, PackageSpec}; kwargs...) = pin([pkg]; kwargs...)
pin(pkgs::Vector{String}; kwargs...)            = pin([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
pin(pkgs::Vector{PackageSpec}; kwargs...)       = pin(Context(), pkgs; kwargs...)

function pin(ctx::Context, pkgs::Vector{PackageSpec}; kwargs...)
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    project_deps_resolve!(ctx.env, pkgs)
    ensure_resolved(ctx.env, pkgs)
    Operations.pin(ctx, pkgs)
    return
end


free(pkg::Union{String, PackageSpec}; kwargs...) = free([pkg]; kwargs...)
free(pkgs::Vector{String}; kwargs...)            = free([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
free(pkgs::Vector{PackageSpec}; kwargs...)       = free(Context(), pkgs; kwargs...)

function free(ctx::Context, pkgs::Vector{PackageSpec}; kwargs...)
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    registry_resolve!(ctx.env, pkgs)
    uuids_in_registry = UUID[]
    for pkg in pkgs
        pkg.mode = PKGMODE_MANIFEST
    end
    for pkg in pkgs
        has_uuid(pkg) && push!(uuids_in_registry, pkg.uuid)
    end
    manifest_resolve!(ctx.env, pkgs)
    ensure_resolved(ctx.env, pkgs)
    # Every non pinned package that is freed need to be in a registry
    for pkg in pkgs
        info = manifest_info(ctx.env, pkg.uuid)
        if !get(info, "pinned", false) && !(pkg.uuid in uuids_in_registry)
            cmderror("cannot free an unpinned package that does not exist in a registry")
        end
    end
    Operations.free(ctx, pkgs)
    return
end



test(;kwargs...)                                  = test(PackageSpec[]; kwargs...)
test(pkg::Union{String, PackageSpec}; kwargs...)  = test([pkg]; kwargs...)
test(pkgs::Vector{String}; kwargs...)             = test([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
test(pkgs::Vector{PackageSpec}; kwargs...)        = test(Context(), pkgs; kwargs...)

function test(ctx::Context, pkgs::Vector{PackageSpec}; coverage=false, kwargs...)
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    if isempty(pkgs)
        # TODO: Allow this?
        ctx.env.pkg == nothing && cmderror("trying to test unnamed project")
        push!(pkgs, ctx.env.pkg)
    end
    project_resolve!(ctx.env, pkgs)
    project_deps_resolve!(ctx.env, pkgs)
    manifest_resolve!(ctx.env, pkgs)
    ensure_resolved(ctx.env, pkgs)
    if !ctx.preview && (Operations.any_package_not_installed(ctx) || !isfile(ctx.env.manifest_file))
        Pkg.instantiate(ctx)
    end
    Operations.test(ctx, pkgs; coverage=coverage)
    return
end


function installed(mode::PackageMode=PKGMODE_MANIFEST)
    diffs = Display.status(Context(), mode, #=use_as_api=# true)
    version_status = Dict{String, Union{VersionNumber,Nothing}}()
    diffs == nothing && return version_status
    for entry in diffs
        version_status[entry.name] = entry.new.ver
    end
    return version_status
end

function gc(ctx::Context=Context(); kwargs...)
    function recursive_dir_size(path)
        size = 0
        for (root, dirs, files) in walkdir(path)
            for file in files
                size += stat(joinpath(root, file)).size
            end
        end
        return size
    end

    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    env = ctx.env

    # If the manifest was not used
    usage_file = joinpath(logdir(), "manifest_usage.toml")

    # Collect only the manifest that is least recently used
    manifest_date = Dict{String, Dates.DateTime}()
    for (manifest_file, infos) in TOML.parse(String(read(usage_file)))
        for info in infos
            date = info["time"]
            manifest_date[manifest_file] = haskey(manifest_date, date) ? max(manifest_date[date], date) : date
        end
    end

    # Find all reachable packages through manifests recently used
    new_usage = Dict{String, Any}()
    paths_to_keep = String[]
    printpkgstyle(ctx, :Active, "manifests at:")
    for (manifestfile, date) in manifest_date
        !isfile(manifestfile) && continue
        println("        `$manifestfile`")
        infos = try
            read_manifest(manifestfile)
        catch e
            @warn "Reading manifest file at $manifestfile failed with error" exception = e
            nothing
        end
        infos == nothing && continue
        new_usage[manifestfile] = [Dict("time" => date)]
        for entry in infos
            entry isa Pair || continue
            name, _stanzas = entry
            @assert length(_stanzas) == 1
            stanzas = _stanzas[1]
            if stanzas isa Dict && haskey(stanzas, "uuid") && haskey(stanzas, "git-tree-sha1")
                push!(paths_to_keep,
                      Operations.find_installed(name, UUID(stanzas["uuid"]), SHA1(stanzas["git-tree-sha1"])))
            end
        end
    end

    # Collect the paths to delete (everything that is not reachable)
    paths_to_delete = String[]
    for depot in depots()
        packagedir = abspath(depot, "packages")
        if isdir(packagedir)
            for name in readdir(packagedir)
                for slug in readdir(joinpath(packagedir, name))
                    versiondir = joinpath(packagedir, name, slug)
                    if !(versiondir in paths_to_keep)
                        push!(paths_to_delete, versiondir)
                    end
                end
            end
        end
    end

    pretty_byte_str = (size) -> begin
        bytes, mb = Base.prettyprint_getunits(size, length(Base._mem_units), Int64(1024))
        return @sprintf("%.3f %s", bytes, Base._mem_units[mb])
    end

    # Delete paths for noreachable package versions and compute size saved
    sz = 0
    for path in paths_to_delete
        sz_pkg = recursive_dir_size(path)
        if !ctx.preview
            try
                Base.rm(path; recursive=true)
            catch
                @warn "Failed to delete $path"
            end
        end
        printpkgstyle(ctx, :Deleted, "$path:" * " " * pretty_byte_str(sz_pkg))
        sz += sz_pkg
    end

    # Delete package paths that are now empty
    for depot in depots()
        packagedir = abspath(depot, "packages")
        if isdir(packagedir)
            for name in readdir(packagedir)
                name_path = joinpath(packagedir, name)
                if isempty(readdir(name_path))
                    !ctx.preview && Base.rm(name_path)
                end
            end
        end
    end

    # Write the new condensed usage file
    if !ctx.preview
        open(usage_file, "w") do io
            TOML.print(io, new_usage, sorted=true)
        end
    end
    byte_save_str = length(paths_to_delete) == 0 ? "" : (": " * pretty_byte_str(sz))
    printpkgstyle(ctx, :Deleted, "$(length(paths_to_delete)) package installations $byte_save_str")

    ctx.preview && preview_info()
    return
end


function _get_deps!(ctx::Context, pkgs::Vector{PackageSpec}, uuids::Vector{UUID})
    for pkg in pkgs
        pkg.uuid in keys(ctx.stdlibs) && continue
        pkg.uuid in uuids && continue
        push!(uuids, pkg.uuid)
        if Types.is_project(ctx.env, pkg)
            pkgs = [PackageSpec(name, UUID(uuid)) for (name, uuid) in ctx.env.project["deps"]]
        else
            info = manifest_info(ctx.env, pkg.uuid)
            if haskey(info, "deps")
                pkgs = [PackageSpec(name, UUID(uuid)) for (name, uuid) in info["deps"]]
            end
        end
        _get_deps!(ctx, pkgs, uuids)
    end
    return
end

build(pkgs...) = build([PackageSpec(pkg) for pkg in pkgs])
build(pkg::Array{Union{}, 1}) = build(PackageSpec[])
build(pkg::PackageSpec) = build([pkg])
build(pkgs::Vector{PackageSpec}) = build(Context(), pkgs)

function build(ctx::Context, pkgs::Vector{PackageSpec}; kwargs...)
    Context!(ctx; kwargs...)

    ctx.preview && preview_info()
    if isempty(pkgs)
        if ctx.env.pkg !== nothing
            push!(pkgs, ctx.env.pkg)
        else
            for (name, infos) in ctx.env.manifest, info in infos
                uuid = UUID(info["uuid"])
                push!(pkgs, PackageSpec(name, uuid))
            end
        end
    end
    for pkg in pkgs
        pkg.mode = PKGMODE_MANIFEST
    end
    project_resolve!(ctx.env, pkgs)
    manifest_resolve!(ctx.env, pkgs)
    ensure_resolved(ctx.env, pkgs)
    if !ctx.preview && (Operations.any_package_not_installed(ctx) || !isfile(ctx.env.manifest_file))
        Pkg.instantiate(ctx)
    end
    uuids = UUID[]
    _get_deps!(ctx, pkgs, uuids)
    length(uuids) == 0 && (@info("no packages to build"); return)
    Operations.build_versions(ctx, uuids; might_need_to_resolve=true)
    ctx.preview && preview_info()
    return
end

#####################################
# Backwards compatibility with Pkg2 #
#####################################
function clone(url::String, name::String = "")
    @warn "Pkg.clone is only kept for legacy CI script reasons, please use `add`" maxlog=1
    ctx = Context()
    if !isempty(name)
        ctx.old_pkg2_clone_name = name
    end
    develop(ctx, [parse_package(url)])
end

function dir(pkg::String, paths::String...)
    @warn "Pkg.dir is only kept for legacy CI script reasons" maxlog=1
    pkgid = Base.identify_package(pkg)
    pkgid == nothing && return nothing
    path = Base.locate_package(pkgid)
    pkgid == nothing && return nothing
    return joinpath(abspath(path, "..", "..", paths...))
end

precompile() = precompile(Context())
function precompile(ctx::Context)
    printpkgstyle(ctx, :Precompiling, "project...")

    pkgids = [Base.PkgId(UUID(uuid), name) for (name, uuid) in ctx.env.project["deps"] if !(UUID(uuid) in  keys(ctx.stdlibs))]
    if ctx.env.pkg !== nothing && isfile( joinpath( dirname(ctx.env.project_file), "src", ctx.env.pkg.name * ".jl"))
        push!(pkgids, Base.PkgId(ctx.env.pkg.uuid, ctx.env.pkg.name))
    end

    needs_to_be_precompiled = String[]
    for pkg in pkgids
        paths = Base.find_all_in_cache_path(pkg)
        sourcepath = Base.locate_package(pkg)
        if sourcepath == nothing
            cmderror("couldn't find path to $(pkg.name) when trying to precompilie project")
        end
        found_matching_precompile = false
        for path_to_try in paths::Vector{String}
            staledeps = Base.stale_cachefile(sourcepath, path_to_try)
            if !(staledeps isa Bool)
                found_matching_precompile = true
            end
        end
        if !found_matching_precompile
            # Only precompile packages that has contains `__precompile__` or `__precompile__(true)`
            source = read(sourcepath, String)
            if occursin(r"__precompile__\(\)|__precompile__\(true\)", source)
                push!(needs_to_be_precompiled, pkg.name)
            end
        end
    end

    # Perhaps running all the imports in the same process would avoid some overheda.
    # Julia starts pretty fast though (0.3 seconds)
    code = join(["import " * pkg for pkg in needs_to_be_precompiled], '\n') * "\nexit(0)"
    for (i, pkg) in enumerate(needs_to_be_precompiled)
        code = """
            $(Base.load_path_setup_code())
            import $pkg
        """
        printpkgstyle(ctx, :Precompiling, pkg * " [$i of $(length(needs_to_be_precompiled))]")
        run(pipeline(ignorestatus(```
        $(Base.julia_cmd()) -O$(Base.JLOptions().opt_level) --color=no --history-file=no
        --startup-file=$(Base.JLOptions().startupfile != 2 ? "yes" : "no")
        --compiled-modules="yes"
        --depwarn=no
        --eval $code
        ```)))
    end
    return nothing
end


function read_package_from_manifest!(pkg::PackageSpec, info::Dict)
    pkg.uuid = UUID(info["uuid"])
    pkg.path = get(info, "path", nothing)
    if haskey(info, "repo-url")
        pkg.repo = Types.GitRepo(info["repo-url"], info["repo-rev"])
    end
    haskey(info, "version") && (pkg.version = VersionNumber(info["version"]))
end

instantiate(; kwargs...) = instantiate(Context(); kwargs...)
function instantiate(ctx::Context; manifest::Union{Bool, Nothing}=nothing, kwargs...)
    Context!(ctx; kwargs...)
    if (!isfile(ctx.env.manifest_file) && manifest == nothing) || manifest == false
        up(ctx)
        return
    end
    if !isfile(ctx.env.manifest_file) && manifest == true
        cmderror("manifest at $(ctx.env.manifest) does not exist")
    end
    update_registry(ctx)
    urls = Dict{}
    hashes = Dict{UUID,SHA1}()
    urls = Dict{UUID,Vector{String}}()
    pkgs = PackageSpec[]
    for (pkg_name, pkg_info) in ctx.env.manifest
        for info in pkg_info
            pkg = PackageSpec(pkg_name)
            read_package_from_manifest!(pkg, info)
            push!(pkgs, pkg)
            pkg.uuid in keys(ctx.stdlibs) && continue
            pkg.path !== nothing && continue
            urls[pkg.uuid] = String[]
            hashes[pkg.uuid] = SHA1(info["git-tree-sha1"])

            if haskey(info, "repo-url")
                pkg.repo = Types.GitRepo(
                    info["repo-url"],
                    info["repo-rev"],
                    SHA1(info["git-tree-sha1"]))
            end
        end
    end
    _, urls_ref = Operations.version_data!(ctx, pkgs)
    for (uuid, url) in urls_ref
        append!(urls[uuid], url)
        urls[uuid] = unique(urls[uuid])
    end
    new_git = handle_repos_add!(ctx, pkgs; upgrade_or_add=false)
    new_apply = Operations.apply_versions(ctx, pkgs, hashes, urls)
    Operations.build_versions(ctx, union(new_apply, new_git))
end

function activate(path::Union{String,Nothing}=nothing)
    Base.ACTIVE_PROJECT[] = Base.load_path_expand(path)
end

"""
    setprotocol!(proto::Union{Nothing, AbstractString}=nothing)

Set the protocol used to access GitHub-hosted packages when `add`ing a url or `develop`ing a package.
Defaults to delegating the choice to the package developer (`proto == nothing`).
Other choices for `proto` are `"https` or `git`.
"""
setprotocol!(proto::Union{Nothing, AbstractString}=nothing) = GitTools.setprotocol!(proto)

end # module
