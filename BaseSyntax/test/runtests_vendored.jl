# Test the BaseSyntax package, which re-exports the copy included into `Base`
let old_active_project = Base.active_project()
    try
        Base.set_active_project(joinpath(@__DIR__, "..", "Project.toml"))
        include("runtests.jl")
    finally
        Base.set_active_project(old_active_project)
    end
end
