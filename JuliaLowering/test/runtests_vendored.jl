using Pkg

let old_active_project = Base.active_project()
    try
        # test local (dev) copy of JuliaLowering, not yet vendored into Base
        Base.set_active_project(joinpath(@__DIR__, "..", "Project.toml"))

        # `Manifest.toml` is not checked in (see `JuliaLowering/.gitignore`), so
        # resolve the environment before loading. `Project.toml` records the path to
        # the in-tree `JuliaSyntax` in its `[sources]` section, so instantiating
        # picks up that dev copy without needing a committed manifest. Skip the
        # registry update: the only dependencies are path-based / stdlib, so no
        # network access is required (and tests may run with networking disabled).
        Pkg.instantiate(; update_registry=false)

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
end
