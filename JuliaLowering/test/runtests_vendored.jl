using Pkg

function with_output_on_failure(f)
    output = IOBuffer()
    try
        return f(output)
    catch
        seekstart(output)
        write(stderr, read(output))
        rethrow()
    end
end

let old_active_project = Base.active_project()
    try
        # test local (dev) copy of JuliaLowering, not the copy vendored into Base
        Base.set_active_project(joinpath(@__DIR__, "..", "Project.toml"))

        # `Manifest.toml` is not checked in (see `JuliaLowering/.gitignore`), so
        # resolve the environment before loading. `Project.toml` records the path to
        # the in-tree `JuliaSyntax` in its `[sources]` section, so instantiating
        # picks up that dev copy without needing a committed manifest. The only
        # dependencies are path-based / stdlib, so no registry or network access is
        # required (and tests may run with networking disabled). Offline mode also
        # keeps Pkg from installing the default registries into a depot without any.
        offline = Pkg.OFFLINE_MODE[]
        Pkg.offline(true)
        try
            with_output_on_failure() do output
                Pkg.instantiate(; io=output)
            end
        finally
            Pkg.offline(offline)
        end

        # restore error hints (emptied by `testdefs.jl`) so that errors print as
        # JuliaLowering expects them to
        Base.Experimental.register_error_hint(Base.UndefVarError_hint, UndefVarError)

        # n.b.: these must be run in `Main`, so that type-printing is equivalent
        # when running via Pkg.test() (e.g. "SomeType" should be printed instead
        # of "JuliaLowering.SomeType")
        @eval Main using JuliaLowering
        Core.include(Main, joinpath(@__DIR__, "runtests.jl")) # run the actual tests
        # Later tests on this worker (e.g. REPL) need `ans` and `err` in `Main` untouched
        @test all(s -> which(Main, s) === Base.MainInclude, (:ans, :err))
    finally
        # Restore original load path and active project
        Base.set_active_project(old_active_project)
    end
end
