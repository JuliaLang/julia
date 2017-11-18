module API

import Pkg3
using Pkg3.Types
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

end
