let old_active_project = Base.active_project(),
    juliasyntax_path = normpath(joinpath(@__DIR__, "..", "..", "JuliaSyntax")),
    pushed_load_path = false

    try
        # test local (dev) copy of JuliaLowering, not the copy vendored into Base
        Base.set_active_project(joinpath(@__DIR__, "..", "Project.toml"))

        # There is no manifest to say where the in-tree JuliaSyntax (the only non-stdlib
        # dependency) is, so put it in `LOAD_PATH`. It has to precede the active project,
        # since the search for a dependency that a project names stops at that project.
        if juliasyntax_path ∉ LOAD_PATH
            pushfirst!(LOAD_PATH, juliasyntax_path)
            pushed_load_path = true
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
        if pushed_load_path
            i = findfirst(==(juliasyntax_path), LOAD_PATH)
            i === nothing || deleteat!(LOAD_PATH, i)
        end
        Base.set_active_project(old_active_project)
    end
end
