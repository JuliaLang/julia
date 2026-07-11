using Pkg

let old_active_project = Base.active_project()
    try
        # test the in-tree UnifiedIR package
        Base.set_active_project(joinpath(@__DIR__, "..", "Project.toml"))
        Pkg.instantiate(; update_registry=false)
        @eval Main using UnifiedIR
        Core.include(Main, joinpath(@__DIR__, "runtests.jl"))
    finally
        Base.set_active_project(old_active_project)
    end
end
