module API

using UUIDs
using Printf
import Random
import Dates
import LibGit2

import ..depots, ..logdir, ..devdir, ..print_first_command_header
import ..Operations, ..Display
using ..Types, ..TOML


preview_info() = printstyled("───── Preview mode ─────\n"; color=Base.info_color(), bold=true)

include("generate.jl")

add_or_develop(pkg::Union{String, PackageSpec}; kwargs...) = add_or_develop([pkg]; kwargs...)
add_or_develop(pkgs::Vector{String}; kwargs...)            = add_or_develop([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
add_or_develop(pkgs::Vector{PackageSpec}; kwargs...)       = add_or_develop(Context(), pkgs; kwargs...)

function add_or_develop(ctx::Context, pkgs::Vector{PackageSpec}; mode::Symbol, kwargs...)
    print_first_command_header()
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    if mode == :develop
        handle_repos_develop!(ctx, pkgs)
    else
        handle_repos_add!(ctx, pkgs; upgrade_or_add=true)
    end
    project_deps_resolve!(ctx.env, pkgs)
    registry_resolve!(ctx.env, pkgs)
    stdlib_resolve!(ctx, pkgs)
    ensure_resolved(ctx.env, pkgs, registry=true)
    Operations.add_or_develop(ctx, pkgs)
    ctx.preview && preview_info()
end

add(args...; kwargs...) = add_or_develop(args...; mode = :add, kwargs...)
develop(args...; kwargs...) = add_or_develop(args...; mode = :develop, kwargs...)
@deprecate checkout develop


rm(pkg::Union{String, PackageSpec}; kwargs...)               = rm([pkg]; kwargs...)
rm(pkgs::Vector{String}; kwargs...)      = rm([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
rm(pkgs::Vector{PackageSpec}; kwargs...) = rm(Context(), pkgs; kwargs...)

function rm(ctx::Context, pkgs::Vector{PackageSpec}; kwargs...)
    print_first_command_header()
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    project_deps_resolve!(ctx.env, pkgs)
    manifest_resolve!(ctx.env, pkgs)
    Operations.rm(ctx, pkgs)
    ctx.preview && preview_info()
end


up(;kwargs...)                           = up(PackageSpec[]; kwargs...)
up(pkg::Union{String, PackageSpec}; kwargs...)               = up([pkg]; kwargs...)
up(pkgs::Vector{String}; kwargs...)      = up([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
up(pkgs::Vector{PackageSpec}; kwargs...) = up(Context(), pkgs; kwargs...)

function up(ctx::Context, pkgs::Vector{PackageSpec};
            level::UpgradeLevel=UPLEVEL_MAJOR, mode::PackageMode=PKGMODE_PROJECT, kwargs...)
    print_first_command_header()
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()

    # Update the registry
    errors = Tuple{String, String}[]
    if ctx.preview
        info("Skipping updating registry in preview mode")
    else
        for reg in registries()
            if isdir(joinpath(reg, ".git"))
                regpath = pathrepr(ctx, reg)
                printpkgstyle(ctx, :Updating, "registry at ", regpath)
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
                    LibGit2.fetch(repo)
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
end


pin(pkg::Union{String, PackageSpec}; kwargs...) = pin([pkg]; kwargs...)
pin(pkgs::Vector{String}; kwargs...)            = pin([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
pin(pkgs::Vector{PackageSpec}; kwargs...)       = pin(Context(), pkgs; kwargs...)

function pin(ctx::Context, pkgs::Vector{PackageSpec}; kwargs...)
    print_first_command_header()
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    project_deps_resolve!(ctx.env, pkgs)
    ensure_resolved(ctx.env, pkgs)
    Operations.pin(ctx, pkgs)
end


free(pkg::Union{String, PackageSpec}; kwargs...) = free([pkg]; kwargs...)
free(pkgs::Vector{String}; kwargs...)            = free([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
free(pkgs::Vector{PackageSpec}; kwargs...)       = free(Context(), pkgs; kwargs...)

function free(ctx::Context, pkgs::Vector{PackageSpec}; kwargs...)
    print_first_command_header()
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    registry_resolve!(ctx.env, pkgs)
    ensure_resolved(ctx.env, pkgs; registry=true)
    Operations.free(ctx, pkgs)
end



test(;kwargs...)                                  = test(PackageSpec[], kwargs...)
test(pkg::Union{String, PackageSpec}; kwargs...)  = test([pkg]; kwargs...)
test(pkgs::Vector{String}; kwargs...)             = test([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
test(pkgs::Vector{PackageSpec}; kwargs...)        = test(Context(), pkgs; kwargs...)

function test(ctx::Context, pkgs::Vector{PackageSpec}; coverage=false, kwargs...)
    print_first_command_header()
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
    Operations.test(ctx, pkgs; coverage=coverage)
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


function gc(ctx::Context=Context(); period = Dates.Week(6), kwargs...)
    print_first_command_header()
    function recursive_dir_size(path)
        sz = 0
        for (root, dirs, files) in walkdir(path)
            for file in files
                sz += stat(joinpath(root, file)).size
            end
        end
        return sz
    end

    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    env = ctx.env

    # If the manifest was not used
    gc_time = Dates.now() - period
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
    for (manifestfile, date) in manifest_date
        !isfile(manifestfile) && continue
        if date < gc_time
            continue
        end
        infos = read_manifest(manifestfile)
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

    # Delete paths for noreachable package versions and compute size saved
    sz = 0
    for path in paths_to_delete
        sz += recursive_dir_size(path)
        if !ctx.preview
            Base.rm(path; recursive=true)
        end
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
    bytes, mb = Base.prettyprint_getunits(sz, length(Base._mem_units), Int64(1024))
    byte_save_str = length(paths_to_delete) == 0 ? "" : ("saving " * @sprintf("%.3f %s", bytes, Base._mem_units[mb]))
    @info("Deleted $(length(paths_to_delete)) package installations $byte_save_str")
    ctx.preview && preview_info()
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
end

build(pkgs...) = build([PackageSpec(pkg) for pkg in pkgs])
build(pkg::Array{Union{}, 1}) = build(PackageSpec[])
build(pkg::PackageSpec) = build([pkg])
build(pkgs::Vector{PackageSpec}) = build(Context(), pkgs)

function build(ctx::Context, pkgs::Vector{PackageSpec}; kwargs...)
    print_first_command_header()
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
    uuids = UUID[]
    _get_deps!(ctx, pkgs, uuids)
    length(uuids) == 0 && (@info("no packages to build"); return)
    Operations.build_versions(ctx, uuids; might_need_to_resolve=true)
    ctx.preview && preview_info()
end

init() = init(Context())
init(path::String) = init(Context(), path)
function init(ctx::Context, path::String=pwd())
    print_first_command_header()
    Context!(ctx; env = EnvCache(joinpath(path, "Project.toml")))
    Operations.init(ctx)
end

end # module
