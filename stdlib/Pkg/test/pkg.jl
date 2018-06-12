# This file is a part of Julia. License is MIT: https://julialang.org/license

module OperationsTest

import Random: randstring
import LibGit2
using Test
using UUIDs

using Pkg
using Pkg.Types

import Random: randstring
import LibGit2

include("utils.jl")

const TEST_PKG = (name = "Example", uuid = UUID("7876af07-990d-54b4-ab0e-23690620f79a"))

# Make the progress bar less verbose since some CI does not support \r
Pkg.GitTools.PROGRESS_BAR_PERCENTAGE_GRANULARITY[] = 33

temp_pkg_dir() do project_path
    @testset "simple add and remove with preview" begin
        Pkg.init(project_path)
        Pkg.add(TEST_PKG.name; preview = true)
        @test !isinstalled(TEST_PKG)
        Pkg.add(TEST_PKG.name)
        @test isinstalled(TEST_PKG)
        @eval import $(Symbol(TEST_PKG.name))
        Pkg.rm(TEST_PKG.name; preview = true)
        @test isinstalled(TEST_PKG)
        Pkg.rm(TEST_PKG.name)
        @test !isinstalled(TEST_PKG)
    end

    @testset "package with wrong UUID" begin
        @test_throws ResolverError Pkg.add(PackageSpec(TEST_PKG.name, UUID(UInt128(1))))
    end

    @testset "adding and upgrading different versions" begin
        # VersionNumber
        Pkg.add(PackageSpec(TEST_PKG.name, v"0.3"))
        @test Pkg.installed()[TEST_PKG.name] == v"0.3"
        Pkg.add(PackageSpec(TEST_PKG.name, v"0.3.1"))
        @test Pkg.installed()[TEST_PKG.name] == v"0.3.1"
        Pkg.rm(TEST_PKG.name)

        # VersionRange
        Pkg.add(PackageSpec(TEST_PKG.name, VersionSpec(VersionRange("0.3.0-0.3.2"))))
        @test Pkg.installed()[TEST_PKG.name] == v"0.3.2"
        Pkg.up(; level = UpgradeLevel(:patch))
        @test Pkg.installed()[TEST_PKG.name] == v"0.3.3"
        Pkg.up(; level = UpgradeLevel(:minor))
        @test Pkg.installed()[TEST_PKG.name].minor != 3
        Pkg.rm(TEST_PKG.name)
    end

    @testset "testing" begin
        # TODO: Check that preview = true doesn't actually execute the test
        # TODO: Test-only dependencies
        Pkg.add(TEST_PKG.name)
        Pkg.test(TEST_PKG.name; coverage=true)
        pkgdir = Base.locate_package(Base.PkgId(TEST_PKG.uuid, TEST_PKG.name))
        # No coverage files being generated?
        @test_broken TEST_PKG.name * ".cov" in readdir(pkgdir)
        Pkg.rm(TEST_PKG.name)
    end

    @testset "pinning / freeing" begin
        Pkg.add(TEST_PKG.name)
        old_v = Pkg.installed()[TEST_PKG.name]
        Pkg.pin(PackageSpec(TEST_PKG.name, v"0.2"))
        @test Pkg.installed()[TEST_PKG.name].minor == 2
        Pkg.up(TEST_PKG.name)
        @test Pkg.installed()[TEST_PKG.name].minor == 2
        Pkg.free(TEST_PKG.name)
        Pkg.up()
        @test Pkg.installed()[TEST_PKG.name] == old_v
        Pkg.rm(TEST_PKG.name)
    end

    @testset "develop / freeing" begin
        Pkg.add(TEST_PKG.name)
        old_v = Pkg.installed()[TEST_PKG.name]
        Pkg.rm(TEST_PKG.name)
        mktempdir() do devdir
            withenv("JULIA_PKG_DEVDIR" => devdir) do
                Pkg.REPLMode.pkgstr("develop $(TEST_PKG.name)")
                @test isinstalled(TEST_PKG)
                @test Pkg.installed()[TEST_PKG.name] > old_v
                test_pkg_main_file = joinpath(devdir, TEST_PKG.name, "src", TEST_PKG.name * ".jl")
                @test isfile(test_pkg_main_file)
                # Pkg #152
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
                Pkg.build(TEST_PKG.name)
                @test isfile(joinpath(devdir, TEST_PKG.name, "deps", "deps.jl"))
                Pkg.test(TEST_PKG.name)
                Pkg.free(TEST_PKG.name)
                @test Pkg.installed()[TEST_PKG.name] == old_v
            end
        end
    end

    @testset "stdlibs as direct dependency" begin
        uuid_pkg = (name = "CRC32c", uuid = UUID("8bf52ea8-c179-5cab-976a-9e18b702a9bc"))
        Pkg.add("CRC32c")
        @test haskey(Pkg.installed(), uuid_pkg.name)
        Pkg.up()
        Pkg.test("CRC32c")
        Pkg.rm("CRC32c")
    end

    @testset "package name in resolver errors" begin
        try
            Pkg.add([PackageSpec(TEST_PKG.name, VersionSpec(v"55"))])
        catch e
            @test occursin(TEST_PKG.name, sprint(showerror, e))
        end
    end

    @testset "check logging" begin
        usage = Pkg.TOML.parse(String(read(joinpath(Pkg.logdir(), "manifest_usage.toml"))))
        @test any(x -> startswith(x, joinpath(project_path, "Manifest.toml")), keys(usage))
    end

    @testset "adding nonexisting packages" begin
        nonexisting_pkg = randstring(14)
        @test_throws CommandError Pkg.add(nonexisting_pkg)
        @test_throws CommandError Pkg.up(nonexisting_pkg)
    end

    Pkg.rm(TEST_PKG.name)

    @testset "legacy CI script" begin
        mktempdir() do dir
            LibGit2.with(LibGit2.clone("https://github.com/JuliaLang/Example.jl", joinpath(dir, "Example.jl"))) do r
                cd(joinpath(dir, "Example.jl")) do
                    let Pkg = Pkg
                        Pkg.clone(pwd())
                        Pkg.build("Example")
                        Pkg.test("Example"; coverage=true)
                        @test isfile(Pkg.dir("Example", "src", "Example.jl"))
                    end
                end
            end
        end
    end

    @testset "add julia" begin
        @test_throws CommandError Pkg.add("julia")
    end

    @testset "up in Project without manifest" begin
        mktempdir() do dir
            cp(joinpath(@__DIR__, "test_packages", "UnregisteredWithProject"), joinpath(dir, "UnregisteredWithProject"))
            cd(joinpath(dir, "UnregisteredWithProject")) do
                try
                    pushfirst!(LOAD_PATH, Base.parse_load_path("@"))
                    Pkg.up()
                    @test haskey(Pkg.installed(), "Example")
                finally
                    popfirst!(LOAD_PATH)
                end
            end
        end
    end
end

temp_pkg_dir() do project_path
    @testset "libgit2 downloads" begin
        Pkg.add(TEST_PKG.name; use_libgit2_for_all_downloads=true)
        @test haskey(Pkg.installed(), TEST_PKG.name)
        Pkg.rm(TEST_PKG.name)
    end
end


@testset "parse package url win" begin
    @test typeof(Pkg.REPLMode.parse_package("https://github.com/abc/ABC.jl"; context=Pkg.REPLMode.CMD_ADD)) == PackageSpec
end

include("repl.jl")

end # module
