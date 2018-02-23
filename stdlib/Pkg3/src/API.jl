module API

using UUIDs
using Printf
import Random
import Dates
import LibGit2

import Pkg3
import Pkg3: depots, logdir, devdir
using Pkg3.Types
using Pkg3.TOML


preview_info() = @info("In preview mode")

add(pkg::Union{String, PackageSpec}; kwargs...)               = add([pkg]; kwargs...)
add(pkgs::Vector{String}; kwargs...)      = add([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
add(pkgs::Vector{PackageSpec}; kwargs...) = add(Context(), pkgs; kwargs...)

function add(ctx::Context, pkgs::Vector{PackageSpec}; kwargs...)
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    project_resolve!(ctx.env, pkgs)
    registry_resolve!(ctx.env, pkgs)
    ensure_resolved(ctx.env, pkgs, true)
    Pkg3.Operations.add(ctx, pkgs)
end


rm(pkg::Union{String, PackageSpec}; kwargs...)               = rm([pkg]; kwargs...)
rm(pkgs::Vector{String}; kwargs...)      = rm([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
rm(pkgs::Vector{PackageSpec}; kwargs...) = rm(Context(), pkgs; kwargs...)

function rm(ctx::Context, pkgs::Vector{PackageSpec}; kwargs...)
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    project_resolve!(ctx.env, pkgs)
    manifest_resolve!(ctx.env, pkgs)
    Pkg3.Operations.rm(ctx, pkgs)
end


up(;kwargs...)                           = up(PackageSpec[]; kwargs...)
up(pkg::Union{String, PackageSpec}; kwargs...)               = up([pkg]; kwargs...)
up(pkgs::Vector{String}; kwargs...)      = up([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
up(pkgs::Vector{PackageSpec}; kwargs...) = up(Context(), pkgs; kwargs...)

function up(ctx::Context, pkgs::Vector{PackageSpec};
            level::UpgradeLevel=UPLEVEL_MAJOR, mode::PackageMode=PKGMODE_PROJECT, kwargs...)
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()

    # Update the registry
    errors = Tuple{String, String}[]
    if ctx.preview
        info("Skipping updating registry in preview mode")
    else
        for reg in registries()
            if !isdir(joinpath(reg, ".git"))
                @info("Registry at $reg is not a git repo, skipping update")
            end
            @info("Updating registry at $reg")
            LibGit2.with(LibGit2.GitRepo, reg) do repo
                if LibGit2.isdirty(repo)
                    push!(errors, (reg, "registry dirty"))
                    return
                end
                if !LibGit2.isattached(repo)
                    push!(errors, (reg, "registry detached"))
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
        project_resolve!(ctx.env, pkgs)
        manifest_resolve!(ctx.env, pkgs)
        ensure_resolved(ctx.env, pkgs)
    end
    Pkg3.Operations.up(ctx, pkgs)
end


pin(pkg::Union{String, PackageSpec}; kwargs...) = pin([pkg]; kwargs...)
pin(pkgs::Vector{String}; kwargs...)            = pin([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
pin(pkgs::Vector{PackageSpec}; kwargs...)       = pin(Context(), pkgs; kwargs...)

function pin(ctx::Context, pkgs::Vector{PackageSpec}; kwargs...)
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    project_resolve!(ctx.env, pkgs)
    ensure_resolved(ctx.env, pkgs)
    Pkg3.Operations.pin(ctx, pkgs)
end


free(pkg::Union{String, PackageSpec}; kwargs...) = free([pkg]; kwargs...)
free(pkgs::Vector{String}; kwargs...)            = free([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
free(pkgs::Vector{PackageSpec}; kwargs...)       = free(Context(), pkgs; kwargs...)

function free(ctx::Context, pkgs::Vector{PackageSpec}; kwargs...)
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    project_resolve!(ctx.env, pkgs)
    ensure_resolved(ctx.env, pkgs)
    Pkg3.Operations.free(ctx, pkgs)
end


checkout(pkg::Union{String, PackageSpec}; kwargs...)  = checkout([pkg]; kwargs...)
checkout(pkg::String, branch::String; kwargs...)      = checkout([(PackageSpec(pkg), branch)]; kwargs...)
checkout(pkg::PackageSpec, branch::String; kwargs...) = checkout([(pkg, branch)]; kwargs...)
checkout(pkgs::Vector{String}; kwargs...)             = checkout([(PackageSpec(pkg), nothing) for pkg in pkgs]; kwargs...)
checkout(pkgs::Vector{PackageSpec}; kwargs...)        = checkout([(pkg, nothing) for pkg in pkgs]; kwargs...)
checkout(pkgs_branches::Vector; kwargs...)            = checkout(Context(), pkgs_branches; kwargs...)

function checkout(ctx::Context, pkgs_branches::Vector; path = devdir(), kwargs...)
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    pkgs = [p[1] for p in pkgs_branches]
    project_resolve!(ctx.env, pkgs)
    registry_resolve!(ctx.env, pkgs)
    ensure_resolved(ctx.env, pkgs)
    Pkg3.Operations.checkout(ctx, pkgs_branches; path = path)
end


test(;kwargs...)                                  = test(PackageSpec[], kwargs...)
test(pkg::Union{String, PackageSpec}; kwargs...)  = test([pkg]; kwargs...)
test(pkgs::Vector{String}; kwargs...)             = test([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
test(pkgs::Vector{PackageSpec}; kwargs...)        = test(Context(), pkgs; kwargs...)

function test(ctx::Context, pkgs::Vector{PackageSpec}; coverage=false, kwargs...)
    Context!(ctx; kwargs...)
    ctx.preview && preview_info()
    project_resolve!(ctx.env, pkgs)
    manifest_resolve!(ctx.env, pkgs)
    ensure_resolved(ctx.env, pkgs)
    Pkg3.Operations.test(ctx, pkgs; coverage=coverage)
end


function installed(mode::PackageMode=PKGMODE_MANIFEST)::Dict{String, VersionNumber}
    diffs = Pkg3.Display.status(Context(), mode, #=use_as_api=# true)
    version_status = Dict{String, VersionNumber}()
    diffs == nothing && return version_status
    for entry in diffs
        version_status[entry.name] = entry.new.ver
    end
    return version_status
end


function gc(ctx::Context=Context(); period = Dates.Week(6), kwargs...)
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
    usage_file = joinpath(logdir(), "usage.toml")

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
                      Pkg3.Operations.find_installed(name, UUID(stanzas["uuid"]), SHA1(stanzas["git-tree-sha1"])))
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
    byte_save_str = length(paths_to_delete) == 0 ? "" : (" saving " * @sprintf("%.3f %s", bytes, Base._mem_units[mb]))
    @info("Deleted $(length(paths_to_delete)) package installations $byte_save_str")
end


function _get_deps!(ctx::Context, pkgs::Vector{PackageSpec}, uuids::Vector{UUID})
    for pkg in pkgs
        pkg.uuid in ctx.stdlib_uuids && continue
        info = manifest_info(ctx.env, pkg.uuid)
        pkg.uuid in uuids && continue
        push!(uuids, pkg.uuid)
        if haskey(info, "deps")
            pkgs = [PackageSpec(name, UUID(uuid)) for (name, uuid) in info["deps"]]
            _get_deps!(ctx, pkgs, uuids)
        end
    end
end

build(pkgs...) = build([PackageSpec(pkg) for pkg in pkgs])
build(pkg::Array{Union{}, 1}) = build(PackageSpec[])
build(pkg::PackageSpec) = build([pkg])
build(pkgs::Vector{PackageSpec}) = build(Context(), pkgs)

function build(ctx::Context, pkgs::Vector{PackageSpec}; kwargs...)
    Context!(ctx; kwargs...)
    if isempty(pkgs)
        for (name, infos) in ctx.env.manifest, info in infos
            uuid = UUID(info["uuid"])
            push!(pkgs, PackageSpec(name, uuid))
        end
    end
    for pkg in pkgs
        pkg.mode = PKGMODE_MANIFEST
    end
    manifest_resolve!(ctx.env, pkgs)
    ensure_resolved(ctx.env, pkgs)
    uuids = UUID[]
    _get_deps!(ctx, pkgs, uuids)
    length(uuids) == 0 && (@info("no packages to build"); return)
    Pkg3.Operations.build_versions(ctx, uuids)
end

function init(path::String)
    ctx = Context(env = EnvCache(joinpath(path, "Project.toml")))
    Pkg3.Operations.init(ctx)
end

end # module
