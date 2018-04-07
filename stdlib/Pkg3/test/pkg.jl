module OperationsTest

import Random: randstring
import LibGit2
using Test
using UUIDs

using Pkg3
using Pkg3.Types

import Random: randstring
import LibGit2

include("utils.jl")

const TEST_PKG = (name = "Example", uuid = UUID("7876af07-990d-54b4-ab0e-23690620f79a"))

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

    @testset "develop / freeing" begin
        Pkg3.add(TEST_PKG.name)
        old_v = Pkg3.installed()[TEST_PKG.name]
        Pkg3.rm(TEST_PKG.name)
        mktempdir() do devdir
            withenv("JULIA_PKG_DEVDIR" => devdir) do
                Pkg3.REPLMode.pkgstr("develop $(TEST_PKG.name)")
                @test isinstalled(TEST_PKG)
                @test Pkg3.installed()[TEST_PKG.name] > old_v
                test_pkg_main_file = joinpath(devdir, TEST_PKG.name, "src", TEST_PKG.name * ".jl")
                @test isfile(test_pkg_main_file)
                # Pkg3 #152
                write(test_pkg_main_file,
                    """
                    module Example
                        export hello, domath
                        const example2path = joinpath(@__DIR__, "..", "deps", "deps.jl")
                        if !isfile(example2path)
                            error("Example is not installed correctly")
                        end
                        hello(who::String) = "Hello, \$who"
                        domath(x::Number) = x + 5
                    end
                    """)
                mkpath(joinpath(devdir, TEST_PKG.name, "deps"))
                write(joinpath(devdir, TEST_PKG.name, "deps", "build.jl"),
                    """
                    touch("deps.jl")
                    """
                )
                Pkg3.build(TEST_PKG.name)
                @test isfile(joinpath(devdir, TEST_PKG.name, "deps", "deps.jl"))
                Pkg3.test(TEST_PKG.name)
                Pkg3.free(TEST_PKG.name)
                @test Pkg3.installed()[TEST_PKG.name] == old_v
            end
        end
    end

    @testset "stdlibs as direct dependency" begin
        uuid_pkg = (name = "CRC32c", uuid = UUID("8bf52ea8-c179-5cab-976a-9e18b702a9bc"))
        Pkg3.add("CRC32c")
        @test haskey(Pkg3.installed(), uuid_pkg.name)
        Pkg3.up()
        Pkg3.test("CRC32c")
        Pkg3.rm("CRC32c")
    end

    @testset "package name in resolver errors" begin
        try
            Pkg3.add([PackageSpec(TEST_PKG.name, VersionSpec(v"55"))])
        catch e
            @test occursin(TEST_PKG.name, sprint(showerror, e))
        end
    end

    @testset "check logging" begin
        usage = Pkg3.TOML.parse(String(read(joinpath(Pkg3.logdir(), "manifest_usage.toml"))))
        @test any(x -> startswith(x, joinpath(project_path, "Manifest.toml")), keys(usage))
    end

    @testset "adding nonexisting packages" begin
        nonexisting_pkg = randstring(14)
        @test_throws CommandError Pkg3.add(nonexisting_pkg)
        @test_throws CommandError Pkg3.up(nonexisting_pkg)
    end

    Pkg3.rm(TEST_PKG.name)
end

temp_pkg_dir() do project_path
    @testset "libgit2 downloads" begin
        Pkg3.add(TEST_PKG.name; use_libgit2_for_all_downloads=true)
        @test haskey(Pkg3.installed(), TEST_PKG.name)
        Pkg3.rm(TEST_PKG.name)
    end
end

include("repl.jl")

end # module
