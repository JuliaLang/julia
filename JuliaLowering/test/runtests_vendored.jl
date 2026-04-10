old_active_project = Base.active_project()
try
    # test local (dev) copy of JuliaLowering, not yet vendored into Base
    Base.set_active_project(joinpath(@__DIR__, "..", "Project.toml"))
    manifest_path = joinpath(@__DIR__, "..", "Manifest.toml")

    # restore error hints (emptied by `testdefs.jl`) so that errors print as
    # JuliaLowering expects them to
    Base.Experimental.register_error_hint(Base.UndefVarError_hint, UndefVarError)

    # n.b.: these must be run in `Main`, so that type-printing is equivalent
    # when running via Pkg.test() (e.g. "SyntaxGraph" should be printed instead
    # of "JuliaLowering.SyntaxGraph")
    @eval Main using JuliaLowering
    Core.include(Main, joinpath(@__DIR__, "runtests.jl")) # run the actual tests
finally
    # Restore original load path and active project
    Base.set_active_project(old_active_project)
end
