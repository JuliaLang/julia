module API

import Pkg
import Pkg: depots, logdir, TOML
using Pkg: Types, Dates
using Base.Random.UUID

previewmode_info() = info("In preview mode")

add(pkg::String; kwargs...)               = add([pkg]; kwargs...)
add(pkgs::Vector{String}; kwargs...)      = add([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
add(pkgs::Vector{PackageSpec}; kwargs...) = add(EnvCache(), pkgs; kwargs...)

function add(env::EnvCache, pkgs::Vector{PackageSpec}; preview::Bool=env.preview[])
    env.preview[] = preview
    preview && previewmode_info()
    project_resolve!(env, pkgs)
    registry_resolve!(env, pkgs)
    ensure_resolved(env, pkgs, true)
    Pkg.Operations.add(env, pkgs)
end


rm(pkg::String; kwargs...)               = rm([pkg]; kwargs...)
rm(pkgs::Vector{String}; kwargs...)      = rm([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
rm(pkgs::Vector{PackageSpec}; kwargs...) = rm(EnvCache(), pkgs; kwargs...)

function rm(env::EnvCache, pkgs::Vector{PackageSpec}; preview::Bool=env.preview[])
    env.preview[] = preview
    preview && previewmode_info()
    project_resolve!(env, pkgs)
    manifest_resolve!(env, pkgs)
    Pkg.Operations.rm(env, pkgs)
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
                info("Registry at $reg is not a git repo, skipping update")
            end
            info("Updating registry at $reg")
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
        warn(warn_str)
    end

    if isempty(pkgs)
        if mode == :project
            for (name::String, uuid::UUID) in env.project["deps"]
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
    Pkg.Operations.up(env, pkgs)
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
    Pkg.Operations.test(env, pkgs; coverage=coverage)
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
    gc_time = now() - period
    usage_file = joinpath(logdir(), "usage.toml")

    # Collect only the manifest that is least recently used
    manifest_date = Dict{String, DateTime}()
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
            if stanzas isa Dict && haskey(stanzas, "uuid") && haskey(stanzas, "hash-sha1")
                push!(paths_to_keep, Pkg.Operations.find_installed(UUID(stanzas["uuid"]), SHA1(stanzas["hash-sha1"])))
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

function init(path = pwd())
    Pkg.Operations.init(path)
end

dir() = error("`Pkg.dir()` is discontinued, use `Pkg.dir(\"Package\")` or `Pkg.dir([\"PackageA\", ...])`")

dir(pkg::String) = dir([pkg])[1]
function dir(pkgs_str::Vector{String})
    env = EnvCache()
    pkgs = [PackageSpec(pkg, :manifest) for pkg in pkgs_str]
    manifest_resolve!(env, pkgs)

    paths = String[]
    for pkg in pkgs
        if !has_uuid(pkg)
            cmderror("package $pkg not found in the manifest")
        else
            info = manifest_info(env, pkg.uuid)
            sha1 = SHA1(info["hash-sha1"])
            push!(paths, Pkg3.Operations.find_installed(pkg.uuid, sha1))
        end
    end
    return paths
end



end # module

