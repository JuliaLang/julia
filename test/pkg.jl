module OperationsTest

import Random: randstring
import LibGit2
using Test
using UUIDs

using Pkg3
using Pkg3.Types

import Random: randstring
import LibGit2

function temp_pkg_dir(fn::Function)
    local env_dir
    local old_load_path
    local old_depot_path
    try
        old_load_path = copy(LOAD_PATH)
        old_depot_path = copy(DEPOT_PATH)
        empty!(LOAD_PATH)
        empty!(DEPOT_PATH)
        mktempdir() do env_dir
            mktempdir() do depot_dir
                pushfirst!(LOAD_PATH, env_dir)
                pushfirst!(DEPOT_PATH, depot_dir)
                # Add the standard library paths back
                vers = "v$(VERSION.major).$(VERSION.minor)"
                push!(LOAD_PATH, abspath(Sys.BINDIR, "..", "local", "share", "julia", "site", vers))
                push!(LOAD_PATH, abspath(Sys.BINDIR, "..", "share", "julia", "site", vers))
                fn(env_dir)
            end
        end
    finally
        append!(LOAD_PATH, old_load_path)
        append!(DEPOT_PATH, old_depot_path)
    end
end

const TEST_PKG = (name = "Example", uuid = UUID("7876af07-990d-54b4-ab0e-23690620f79a"))
isinstalled(pkg) = Base.locate_package(Base.PkgId(pkg.uuid, pkg.name)) !== nothing

temp_pkg_dir() do project_path

    @testset "simple add and remove with preview" begin
        Pkg3.init(project_path)
        Pkg3.add(TEST_PKG.name; preview = true)
        @test !isinstalled(TEST_PKG)
        Pkg3.add(TEST_PKG.name)
        @test isinstalled(TEST_PKG)
        @eval import $(Symbol(TEST_PKG.name))
        Pkg3.rm(TEST_PKG.name; preview = true)
        @test isinstalled(TEST_PKG)
        Pkg3.rm(TEST_PKG.name)
        @test !isinstalled(TEST_PKG)
    end

    @testset "package with wrong UUID" begin
        @test_throws ResolverError Pkg3.add(PackageSpec(TEST_PKG.name, UUID(UInt128(1))))
    end

    @testset "adding and upgrading different versions" begin
        # VersionNumber
        Pkg3.add(PackageSpec(TEST_PKG.name, v"0.3"))
        @test Pkg3.installed()[TEST_PKG.name] == v"0.3"
        Pkg3.add(PackageSpec(TEST_PKG.name, v"0.3.1"))
        @test Pkg3.installed()[TEST_PKG.name] == v"0.3.1"
        Pkg3.rm(TEST_PKG.name)

        # VersionRange
        Pkg3.add(PackageSpec(TEST_PKG.name, VersionSpec(VersionRange("0.3.0-0.3.2"))))
        @test Pkg3.installed()[TEST_PKG.name] == v"0.3.2"
        Pkg3.up(; level = UpgradeLevel(:patch))
        @test Pkg3.installed()[TEST_PKG.name] == v"0.3.3"
        Pkg3.up(; level = UpgradeLevel(:minor))
        @test Pkg3.installed()[TEST_PKG.name].minor != 3
        Pkg3.rm(TEST_PKG.name)
    end

    @testset "testing" begin
        # TODO: Check that preview = true doesn't actually execute the test
        # TODO: Test-only dependencies
        Pkg3.add(TEST_PKG.name)
        Pkg3.test(TEST_PKG.name; coverage=true)
        pkgdir = Base.locate_package(Base.PkgId(TEST_PKG.uuid, TEST_PKG.name))
        # No coverage files being generated?
        @test_broken TEST_PKG.name * ".cov" in readdir(pkgdir)
        Pkg3.rm(TEST_PKG.name)
    end

    @testset "pinning / freeing" begin
        Pkg3.add(TEST_PKG.name)
        old_v = Pkg3.installed()[TEST_PKG.name]
        Pkg3.pin(PackageSpec(TEST_PKG.name, v"0.2"))
        @test Pkg3.installed()[TEST_PKG.name].minor == 2
        Pkg3.up(TEST_PKG.name)
        @test Pkg3.installed()[TEST_PKG.name].minor == 2
        Pkg3.free(TEST_PKG.name)
        Pkg3.up()
        @test Pkg3.installed()[TEST_PKG.name] == old_v
        Pkg3.rm(TEST_PKG.name)
    end

    @testset "checking out / freeing" begin
        Pkg3.add(TEST_PKG.name)
        old_v = Pkg3.installed()[TEST_PKG.name]
        Pkg3.rm(TEST_PKG.name)
        mktempdir() do devdir
            Pkg3.checkout(TEST_PKG.name; path = devdir)
            @test isinstalled(TEST_PKG)
            @test Pkg3.installed()[TEST_PKG.name] > old_v
            @test isfile(joinpath(devdir, TEST_PKG.name, "src", TEST_PKG.name * ".jl"))
            mkpath(joinpath(devdir, TEST_PKG.name, "deps"))
            write(joinpath(devdir, TEST_PKG.name, "deps", "build.jl"),
                """
                touch("I_got_built")
                """
            )
            Pkg3.build(TEST_PKG.name)
            @test isfile(joinpath(devdir, TEST_PKG.name, "deps", "I_got_built"))
            Pkg3.test(TEST_PKG.name)
            Pkg3.free(TEST_PKG.name)
            @test Pkg3.installed()[TEST_PKG.name] == old_v
        end
        mktempdir() do tmp
            withenv("JULIA_PKG_DEVDIR" => tmp) do
                @test_throws CommandError Pkg3.checkout(TEST_PKG.name, "nonexisting_branch",)
            end
        end
    end

    @testset "package name in resolver errors" begin
        try
            Pkg3.add([PackageSpec(TEST_PKG.name, VersionSpec(v"55"))])
        catch e
            @test contains(sprint(showerror, e), TEST_PKG.name)
        end
    end

    @testset "check logging" begin
        usage = Pkg3.TOML.parse(String(read(joinpath(Pkg3.logdir(), "usage.toml"))))
        @test any(x -> startswith(x, joinpath(project_path, "Manifest.toml")), keys(usage))
    end

    @testset "adding nonexisting packages" begin
        nonexisting_pkg = randstring(14)
        @test_throws CommandError Pkg3.add(nonexisting_pkg)
        @test_throws CommandError Pkg3.up(nonexisting_pkg)
    end

    Pkg3.rm(TEST_PKG.name)
end

end # module
