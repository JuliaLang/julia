module API

import Pkg3
using Pkg3.Types
using Base.Random.UUID

add(pkg::String)               = add([pkg])
add(pkgs::Vector{String})      = add([PackageSpec(pkg) for pkg in pkgs])
add(pkgs::Vector{PackageSpec}) = add(EnvCache(), pkgs)

function add(env::EnvCache, pkgs::Vector{PackageSpec})
    project_resolve!(env, pkgs)
    registry_resolve!(env, pkgs)
    ensure_resolved(env, pkgs, true)
    Pkg3.Operations.add(env, pkgs)
end


rm(pkg::String)               = rm([pkg])
rm(pkgs::Vector{String})      = rm([PackageSpec(pkg) for pkg in pkgs])
rm(pkgs::Vector{PackageSpec}) = rm(EnvCache(), pkgs)

function rm(env::EnvCache, pkgs::Vector{PackageSpec})
    project_resolve!(env, pkgs)
    manifest_resolve!(env, pkgs)
    Pkg3.Operations.rm(env, pkgs)
end


up(;kwargs...)                           = up(PackageSpec[], kwargs...)
up(pkg::String; kwargs...)               = up([pkg]; kwargs...)
up(pkgs::Vector{String}; kwargs...)      = up([PackageSpec(pkg) for pkg in pkgs]; kwargs...)
up(pkgs::Vector{PackageSpec}; kwargs...) = up(EnvCache(), pkgs; kwargs...)

function up(env::EnvCache, pkgs::Vector{PackageSpec};
            level::UpgradeLevel=UpgradeLevel(:major), mode::Symbol=:project)
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
