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
        @test Pkg3.installed()[TEST_PKG.name] == v"0.5.0"
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
