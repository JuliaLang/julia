module API

import Pkg3
import Pkg3: depots, logdir, TOML
using Pkg3: Types, Dates
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
                push!(paths_to_keep, Pkg3.Operations.find_installed(UUID(stanzas["uuid"]), SHA1(stanzas["hash-sha1"])))
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

end # module

