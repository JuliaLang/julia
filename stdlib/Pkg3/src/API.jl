module API

using UUIDs
using Printf
import Random
import Dates
import LibGit2

import Pkg3
import Pkg3: depots, logdir, Display.DiffEntry
using Pkg3.Types
using Pkg3.TOML


previewmode_info() = @info("In preview mode")

add(pkg::String; kwargs...)               = add([pkg]; kwargs...)
add(pkgs::Vector{String}; kwargs...)      = add([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
add(pkgs::Vector{PackageSpec}; kwargs...) = add(EnvCache(), pkgs; kwargs...)

function add(env::EnvCache, pkgs::Vector{PackageSpec}; preview::Bool=env.preview[])
    env.preview[] = preview
    preview && previewmode_info()
    project_resolve!(env, pkgs)
    registry_resolve!(env, pkgs)
    ensure_resolved(env, pkgs, true)
    Pkg3.Operations.add(env, pkgs)
end


rm(pkg::String; kwargs...)               = rm([pkg]; kwargs...)
rm(pkgs::Vector{String}; kwargs...)      = rm([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
rm(pkgs::Vector{PackageSpec}; kwargs...) = rm(EnvCache(), pkgs; kwargs...)

function rm(env::EnvCache, pkgs::Vector{PackageSpec}; preview::Bool=env.preview[])
    env.preview[] = preview
    preview && previewmode_info()
    project_resolve!(env, pkgs)
    manifest_resolve!(env, pkgs)
    Pkg3.Operations.rm(env, pkgs)
end


up(;kwargs...)                           = up(PackageSpec[], kwargs...)
up(pkg::String; kwargs...)               = up([pkg]; kwargs...)
up(pkgs::Vector{String}; kwargs...)      = up([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
up(pkgs::Vector{PackageSpec}; kwargs...) = up(EnvCache(), pkgs; kwargs...)

function up(env::EnvCache, pkgs::Vector{PackageSpec};
            level::UpgradeLevel=UpgradeLevel(:major), mode::Symbol=:project, preview::Bool=env.preview[])
    env.preview[] = preview
    preview && previewmode_info()

    # Update the registry
    errors = Tuple{String, String}[]
    if env.preview[]
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
        if mode == :project
            for (name::String, uuidstr::String) in env.project["deps"]
                uuid = UUID(uuidstr)
                push!(pkgs, PackageSpec(name, uuid, level))
            end
        elseif mode == :manifest
            for (name, infos) in env.manifest, info in infos
                uuid = UUID(info["uuid"])
                push!(pkgs, PackageSpec(name, uuid, level))
            end
        end
    else
        project_resolve!(env, pkgs)
        manifest_resolve!(env, pkgs)
        ensure_resolved(env, pkgs)
    end
    Pkg3.Operations.up(env, pkgs)
end

test(;kwargs...)                           = test(PackageSpec[], kwargs...)
test(pkg::String; kwargs...)               = test([pkg]; kwargs...)
test(pkgs::Vector{String}; kwargs...)      = test([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
test(pkgs::Vector{PackageSpec}; kwargs...) = test(EnvCache(), pkgs; kwargs...)

function test(env::EnvCache, pkgs::Vector{PackageSpec}; coverage=false, preview=env.preview[])
    env.preview[] = preview
    preview && previewmode_info()
    project_resolve!(env, pkgs)
    manifest_resolve!(env, pkgs)
    ensure_resolved(env, pkgs)
    Pkg3.Operations.test(env, pkgs; coverage=coverage)
end


function convert(::Type{Dict{String, VersionNumber}}, diffs::Union{Array{DiffEntry}, Nothing})
    version_status = Dict{String, VersionNumber}()
    diffs == nothing && return version_status
    for entry in diffs
        version_status[entry.name] = entry.new.ver
    end
    return version_status
end

function installed(mode::Symbol=:manifest)::Dict{String, VersionNumber}
    diff = Pkg3.Display.status(EnvCache(), mode, true)
    convert(Dict{String, VersionNumber}, diff)
end

function recursive_dir_size(path)
    sz = 0
    for (root, dirs, files) in walkdir(path)
      for file in files
          sz += stat(joinpath(root, file)).size
      end
    end
    return sz
end

function gc(env::EnvCache=EnvCache(); period = Week(6), preview=env.preview[])
    env.preview[] = preview
    preview && previewmode_info()

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
                push!(paths_to_keep, Pkg3.Operations.find_installed(UUID(stanzas["uuid"]), SHA1(stanzas["git-tree-sha1"])))
            end
        end
    end

    # Collect the paths to delete (everything that is not reachable)
    paths_to_delete = String[]
    for depot in depots()
        packagedir = abspath(depot, "packages")
        if isdir(packagedir)
            for uuidslug in readdir(packagedir)
                for shaslug in readdir(joinpath(packagedir, uuidslug))
                    versiondir = joinpath(packagedir, uuidslug, shaslug)
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
        if !env.preview[]
            Base.rm(path; recursive=true)
        end
    end

    # Delete package paths that are now empty
    for depot in depots()
        packagedir = abspath(depot, "packages")
        if isdir(packagedir)
            for uuidslug in readdir(packagedir)
                uuidslug_path = joinpath(packagedir, uuidslug)
                if isempty(readdir(uuidslug_path))
                    !env.preview[] && Base.rm(uuidslug_path)
                end
            end
        end
    end

    # Write the new condensed usage file
    if !env.preview[]
        open(usage_file, "w") do io
            TOML.print(io, new_usage, sorted=true)
        end
    end
    bytes, mb = Base.prettyprint_getunits(sz, length(Base._mem_units), Int64(1024))
    byte_save_str = length(paths_to_delete) == 0 ? "" : (" saving " * @sprintf("%.3f %s", bytes, Base._mem_units[mb]))
    info("Deleted $(length(paths_to_delete)) package installations", byte_save_str)
end

function _get_deps!(env::EnvCache, pkgs::Vector{PackageSpec}, uuids::Vector{UUID})
   for pkg in pkgs
       info = manifest_info(env, pkg.uuid)
       pkg.uuid in uuids && continue
       push!(uuids, pkg.uuid)
       if haskey(info, "deps")
           pkgs = [PackageSpec(name, UUID(uuid)) for (name, uuid) in info["deps"]]
           _get_deps!(env, pkgs, uuids)
       end
   end
end


build(pkgs...) = build([PackageSpec(pkg) for pkg in pkgs])
build(pkg::Array{Union{}, 1}) = build(PackageSpec[])
build(pkg::PackageSpec) = build([pkg])
build(pkgs::Vector{PackageSpec}) = build(EnvCache(), pkgs)

function build(env::EnvCache, pkgs::Vector{PackageSpec})
   if isempty(pkgs)
       for (name, infos) in env.manifest, info in infos
           uuid = UUID(info["uuid"])
           push!(pkgs, PackageSpec(name, uuid))
       end
   end
   for pkg in pkgs
       pkg.mode = :manifest
   end
   manifest_resolve!(env, pkgs)
   ensure_resolved(env, pkgs)
   uuids = Random.UUID[]
   _get_deps!(env, pkgs, uuids)
   length(uuids) == 0 && (info("No packages to build!"); return)
   Pkg3.Operations.build_versions(env, uuids)
end


function init(path = pwd())
    Pkg3.Operations.init(path)
end

end # module
