module OperationsTest

import Random: randstring
using Test

using Pkg3
using Pkg3.Types

function temp_pkg_dir(fn::Function)
    local project_path
    try
        # TODO: Use a temporary depot
        project_path = joinpath(tempdir(), randstring())
        push!(LOAD_PATH, project_path)
        fn(project_path)
    finally
        project_path in LOAD_PATH && (deleteat!(LOAD_PATH, findfirst(equalto(project_path), LOAD_PATH)))
        rm(project_path, recursive=true, force=true)
    end
end

isinstalled(pkg) = Base.find_package(pkg) != nothing

# Tests for Example.jl fail on master,
# so let's use another small package
# in the meantime
const TEST_PKG = "Crayons"

temp_pkg_dir() do project_path
    Pkg3.init(project_path)
    Pkg3.add(TEST_PKG; preview = true)
    # @test_warn "not in project" Pkg3.API.rm("Example")
    Pkg3.add(TEST_PKG)
    println("Going to import")
    @eval import $(Symbol(TEST_PKG))
    println("Imported...")
    Pkg3.up()
    Pkg3.rm(TEST_PKG; preview = true)
    @test isinstalled(TEST_PKG)
    # TODO: Check coverage kwargs
    # TODO: Check that preview = true doesn't actually execute the test
    # by creating a package with a test file that fails.

    @test_broken Pkg3.test(TEST_PKG)
    Pkg3.test(TEST_PKG; preview = true)

    Pkg3.GLOBAL_SETTINGS.use_libgit2_for_all_downloads = true
    Pkg3.add("Example")
    Pkg3.GLOBAL_SETTINGS.use_libgit2_for_all_downloads = false

    try
        Pkg3.add([PackageSpec(TEST_PKG, VersionSpec(v"55"))])
    catch e
        @test contains(sprint(showerror, e), TEST_PKG)
    end

    usage = Pkg3.TOML.parse(String(read(joinpath(Pkg3.logdir(), "usage.toml"))))
    @test any(x -> startswith(x, joinpath(project_path, "Manifest.toml")), keys(usage))

    nonexisting_pkg = randstring(14)
    @test_throws CommandError Pkg3.add(nonexisting_pkg)
    @test_throws CommandError Pkg3.up(nonexisting_pkg)
    # @test_warn "not in project" Pkg3.rm(nonexisting_pkg)

    mktempdir() do tmp
        LibGit2.init(tmp)
        mkdir(joinpath(tmp, "subfolder"))
        cd(joinpath(tmp, "subfolder")) do
            # Haven't initialized here so using the default env
            @test isinstalled(TEST_PKG)
            withenv("JULIA_ENV" => nothing) do
                Pkg3.init()
                @test !isinstalled(TEST_PKG)
                @test isfile(joinpath(tmp, "Project.toml"))
                Pkg3.add(TEST_PKG)
                @test isinstalled(TEST_PKG)
            end
        end
    end

    Pkg3.rm(TEST_PKG)
    @test !isinstalled(TEST_PKG)

end

end # module
