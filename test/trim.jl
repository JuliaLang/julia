# Build each project under `test/trim` with JuliaC (pinned via `deps/jlutilities/juliac`)
# and run its `runtests.jl` against the generated executable / library bundle.
#
# By default a project is built as a trimmed executable (`--output-exe`) bundled
# into a temporary directory. A project may instead provide a `build.jl` (see
# `LibSimple`) that is `include`d here to perform a custom build using the
# `run_juliac` and `c_compiler` helpers below; `ARGS[1]` is the bundle directory.
# Each project's `runtests.jl` is then run in a subprocess (activated in the
# project's own environment) with the bundle directory as `ARGS[1]`.
using Test
using Pkg

const TRIM_DIR = joinpath(@__DIR__, "trim")
const BUILDROOT = get(ENV, "JULIA_TEST_BUILDROOT", abspath(joinpath(@__DIR__, "..")))
const JULIAC_PROJECT = joinpath(BUILDROOT, "deps", "jlutilities", "juliac")
const VENDORED_DEPOT = joinpath(BUILDROOT, "deps", "jlutilities", "depot")
const PATHSEP = Sys.iswindows() ? ';' : ':'

# Depot string giving the build/verify subprocesses access to the vendored
# JuliaC checkout plus the normal depots (for stdlibs).
depot_env() = string(VENDORED_DEPOT, PATHSEP, join(DEPOT_PATH, PATHSEP))

# Run `f` with the vendored depot active, for in-process `Pkg` operations.
function with_vendored_depot(f)
    pushfirst!(DEPOT_PATH, VENDORED_DEPOT)
    old_active_project = Base.active_project()
    try
        Base.redirect_stdout(devnull) do
            Base.redirect_stderr(devnull) do
                f()
            end
        end
    finally
        Base.set_active_project(old_active_project)
        popfirst!(DEPOT_PATH)
    end
end

# Invoke the pinned JuliaC (checked out into the vendored depot).
function run_juliac(args::Vector{String})
    cmd = `$(Base.julia_cmd()) --startup-file=no --history-file=no --project=$JULIAC_PROJECT -m JuliaC --quiet $args`
    run(addenv(cmd, "JULIA_DEPOT_PATH" => depot_env()))
    return nothing
end

c_compiler() = something(Sys.which("cc"), Sys.which("clang"), Sys.which("gcc"), nothing)

@testset "trim" begin
    @test isdir(JULIAC_PROJECT)
    # (1) Check out JuliaC at the commit pinned in `deps/jlutilities/juliac`.
    with_vendored_depot() do
        Pkg.activate(JULIAC_PROJECT)
        Pkg.instantiate()
    end

    projects = sort!(filter(readdir(TRIM_DIR)) do name
        isdir(joinpath(TRIM_DIR, name)) &&
            isfile(joinpath(TRIM_DIR, name, "Project.toml")) &&
            isfile(joinpath(TRIM_DIR, name, "runtests.jl"))
    end)
    @test !isempty(projects)

    saved_args = copy(ARGS)
    for name in projects
        @testset "$name" begin
            # Operate on an isolated copy so instantiation/build artifacts (e.g.
            # a generated `Manifest.toml`) never touch the source tree.
            tmpdir = mktempdir()
            projdir = joinpath(tmpdir, name)
            cp(joinpath(TRIM_DIR, name), projdir)
            outdir = mktempdir()
            try
                with_vendored_depot() do
                    Pkg.activate(projdir)
                    Pkg.instantiate()
                end
                # (2) Build the project with JuliaC into `outdir`.
                buildscript = joinpath(projdir, "build.jl")
                if isfile(buildscript)
                    empty!(ARGS); push!(ARGS, outdir)
                    include(buildscript)
                else
                    run_juliac(String["--output-exe", lowercase(name), "--trim=safe",
                                      "--experimental", projdir, "--bundle", outdir])
                end
                # Verify the build with the project's test script.
                #
                # Running it in a subprocess lets test-only deps (e.g. JSON) resolve from
                # the project env, and a failing `@testset` propagates via the exit code.
                verify_cmd = addenv(`$(Base.julia_cmd()) --startup-file=no --history-file=no --depwarn=error --project=$projdir $(joinpath(projdir, "runtests.jl")) $outdir`,
                                    "JULIA_DEPOT_PATH" => depot_env())
                mktemp() do logpath, logio
                    if !success(run(pipeline(ignorestatus(verify_cmd); stdout=logio, stderr=logio)))
                        print(stderr, read(logpath, String))
                        error("trim verification failed for `$name`")
                    end
                end
            finally
                append!(empty!(ARGS), saved_args)
                rm(outdir; recursive=true, force=true)
                rm(tmpdir; recursive=true, force=true)
            end
        end
    end
end
