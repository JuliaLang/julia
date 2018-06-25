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

import Pkg.Types: semver_spec, VersionSpec
@testset "semver notation" begin
    @test semver_spec("^1.2.3") == VersionSpec("1.2.3-1")
    @test semver_spec("^1.2")   == VersionSpec("1.2.0-1")
    @test semver_spec("^1")     == VersionSpec("1.0.0-1")
    @test semver_spec("^0.2.3") == VersionSpec("0.2.3-0.2")
    @test semver_spec("^0.0.3") == VersionSpec("0.0.3-0.0.3")
    @test semver_spec("^0.0")   == VersionSpec("0.0.0-0.0")
    @test semver_spec("^0")     == VersionSpec("0.0.0-0")
    @test semver_spec("~1.2.3") == VersionSpec("1.2.3-1.2")
    @test semver_spec("~1.2")   == VersionSpec("1.2.0-1.2")
    @test semver_spec("~1")     == VersionSpec("1.0.0-1")
    @test semver_spec("1.2.3")  == semver_spec("^1.2.3")
    @test semver_spec("1.2")    == semver_spec("^1.2")
    @test semver_spec("1")      == semver_spec("^1")
    @test semver_spec("0.0.3")  == semver_spec("^0.0.3")
    @test semver_spec("0")      == semver_spec("^0")

    @test semver_spec("0.0.3, 1.2") == VersionSpec(["0.0.3-0.0.3", "1.2.0-1"])
    @test semver_spec("~1.2.3, ~v1") == VersionSpec(["1.2.3-1.2", "1.0.0-1"])

    @test   v"1.5.2"  in semver_spec("1.2.3")
    @test   v"1.2.3"  in semver_spec("1.2.3")
    @test !(v"2.0.0"  in semver_spec("1.2.3"))
    @test !(v"1.2.2"  in semver_spec("1.2.3"))
    @test   v"1.2.99" in semver_spec("~1.2.3")
    @test   v"1.2.3"  in semver_spec("~1.2.3")
    @test !(v"1.3"    in semver_spec("~1.2.3"))
    @test  v"1.2.0"   in semver_spec("1.2")
    @test  v"1.9.9"   in semver_spec("1.2")
    @test !(v"2.0.0"  in semver_spec("1.2"))
    @test !(v"1.1.9"  in semver_spec("1.2"))
    @test   v"0.2.3"  in semver_spec("0.2.3")
    @test !(v"0.3.0"  in semver_spec("0.2.3"))
    @test !(v"0.2.2"  in semver_spec("0.2.3"))
    @test   v"0.0.0"  in semver_spec("0")
    @test  v"0.99.0"  in semver_spec("0")
    @test !(v"1.0.0"  in semver_spec("0"))
    @test  v"0.0.0"   in semver_spec("0.0")
    @test  v"0.0.99"  in semver_spec("0.0")
    @test !(v"0.1.0"  in semver_spec("0.0"))
end

# TODO: Should rewrite these tests not to rely on internals like field names
@testset "union, isjoinable" begin
    @test sprint(print, VersionRange("0-0.3.2")) == "0-0.3.2"
    # test missing paths on union! and isjoinable
    # there's no == for VersionBound or VersionRange
    unified_vr = union!([VersionRange("1.5-2.8"), VersionRange("2.5-3")])[1]
    @test unified_vr.lower.t == (UInt32(1), UInt32(5), UInt32(0))
    @test unified_vr.upper.t == (UInt32(3), UInt32(0), UInt32(0))
    unified_vr = union!([VersionRange("2.5-3"), VersionRange("1.5-2.8")])[1]
    @test unified_vr.lower.t == (UInt32(1), UInt32(5), UInt32(0))
    @test unified_vr.upper.t == (UInt32(3), UInt32(0), UInt32(0))
    unified_vr = union!([VersionRange("1.5-2.2"), VersionRange("2.5-3")])[1]
    @test unified_vr.lower.t == (UInt32(1), UInt32(5), UInt32(0))
    @test unified_vr.upper.t == (UInt32(2), UInt32(2), UInt32(0))
    unified_vr = union!([VersionRange("1.5-2.2"), VersionRange("2.5-3")])[2]
    @test unified_vr.lower.t == (UInt32(2), UInt32(5), UInt32(0))
    @test unified_vr.upper.t == (UInt32(3), UInt32(0), UInt32(0))
    unified_vb = Types.VersionBound(union!([v"1.5", v"1.6"])[1])
    @test unified_vb.t == (UInt32(1), UInt32(5), UInt32(0))
    unified_vb = Types.VersionBound(union!([v"1.5", v"1.6"])[2])
    @test unified_vb.t == (UInt32(1), UInt32(6), UInt32(0))
    unified_vb = Types.VersionBound(union!([v"1.5", v"1.5"])[1])
    @test unified_vb.t == (UInt32(1), UInt32(5), UInt32(0))
end

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
                    pushfirst!(LOAD_PATH, Base.current_env())
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

@testset "preview generate" begin
    mktempdir() do tmp
        cd(tmp) do
            withenv("USER" => "Test User") do
                Pkg.generate("Foo"; preview=true)
                @test !isdir(joinpath(tmp, "Foo"))
            end
        end
    end
end

temp_pkg_dir() do project_path
    @testset "test should instantiate" begin
        mktempdir() do dir
            cp(joinpath(@__DIR__, "test_packages", "UnregisteredWithProject"), joinpath(dir, "UnregisteredWithProject"))
            cd(joinpath(dir, "UnregisteredWithProject")) do
                try
                    pushfirst!(LOAD_PATH, Base.current_env())
                    Pkg.add("Test") # test https://github.com/JuliaLang/Pkg.jl/issues/324
                    Pkg.test()
                finally
                    popfirst!(LOAD_PATH)
                end
            end
        end
    end
end

include("repl.jl")

end # module
