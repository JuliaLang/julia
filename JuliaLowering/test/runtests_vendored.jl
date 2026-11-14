let old_active_project = Base.active_project(),
    dep_paths = [normpath(joinpath(@__DIR__, "..", "..", dep))
                 for dep in ("JuliaSyntax", "BaseSyntax")],
    pushed_load_paths = String[]

    try
        # test local (dev) copy of JuliaLowering, not the copy vendored into Base
        Base.set_active_project(joinpath(@__DIR__, "..", "Project.toml"))

        # There is no manifest to say where the in-tree JuliaSyntax and BaseSyntax (the only
        # non-stdlib dependencies) are, so put them in `LOAD_PATH`. They have to precede the
        # active project, since the search for a dependency that a project names stops at
        # that project.
        for dep_path in dep_paths
            if dep_path ∉ LOAD_PATH
                pushfirst!(LOAD_PATH, dep_path)
                push!(pushed_load_paths, dep_path)
            end
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
        for dep_path in pushed_load_paths
            i = findfirst(==(dep_path), LOAD_PATH)
            i === nothing || deleteat!(LOAD_PATH, i)
        end
        Base.set_active_project(old_active_project)
    end
end
