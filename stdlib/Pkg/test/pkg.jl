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

    @test semver_spec("<1.2.3") == VersionSpec("0.0.0 - 1.2.2")
    @test semver_spec("<1.2") == VersionSpec("0.0.0 - 1.1")
    @test semver_spec("<1") == VersionSpec("0.0.0 - 0")
    @test semver_spec("<2") == VersionSpec("0.0.0 - 1")
    @test semver_spec("<0.2.3") == VersionSpec("0.0.0 - 0.2.2")
    @test semver_spec("<2.0.3") == VersionSpec("0.0.0 - 2.0.2")
    @test   v"0.2.3" in semver_spec("<0.2.4")
    @test !(v"0.2.4" in semver_spec("<0.2.4"))

    @test semver_spec("=1.2.3") == VersionSpec("1.2.3")
    @test semver_spec("=1.2") == VersionSpec("1.2.0")
    @test semver_spec("  =1") == VersionSpec("1.0.0")
    @test   v"1.2.3" in semver_spec("=1.2.3")
    @test !(v"1.2.4" in semver_spec("=1.2.3"))
    @test !(v"1.2.2" in semver_spec("=1.2.3"))

    @test semver_spec("≥1.3.0") == semver_spec(">=1.3.0")

    @test semver_spec(">=   1.2.3") == VersionSpec("1.2.3-*")
    @test semver_spec(">=1.2  ") == VersionSpec("1.2.0-*")
    @test semver_spec("  >=  1") == VersionSpec("1.0.0-*")
    @test   v"1.0.0" in semver_spec(">=1")
    @test   v"0.0.1" in semver_spec(">=0")
    @test   v"1.2.3" in semver_spec(">=1.2.3")
    @test !(v"1.2.2" in semver_spec(">=1.2.3"))

    @test_throws ErrorException semver_spec("^^0.2.3")
    @test_throws ErrorException semver_spec("^^0.2.3.4")
    @test_throws ErrorException semver_spec("0.0.0")

    @test Pkg.Types.isjoinable(Pkg.Types.VersionBound((1,5)), Pkg.Types.VersionBound((1,6)))
    @test !(Pkg.Types.isjoinable(Pkg.Types.VersionBound((1,5)), Pkg.Types.VersionBound((1,6,0))))
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
        Pkg.activate(project_path)
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
        @test Pkg.API.__installed()[TEST_PKG.name] == v"0.3"
        Pkg.add(PackageSpec(TEST_PKG.name, v"0.3.1"))
        @test Pkg.API.__installed()[TEST_PKG.name] == v"0.3.1"
        Pkg.rm(TEST_PKG.name)

        # VersionRange
        Pkg.add(PackageSpec(TEST_PKG.name, VersionSpec(VersionRange("0.3.0-0.3.2"))))
        @test Pkg.API.__installed()[TEST_PKG.name] == v"0.3.2"
        # Check that adding another packages doesn't upgrade other packages
        Pkg.add("Test")
        @test Pkg.API.__installed()[TEST_PKG.name] == v"0.3.2"
        Pkg.update(; level = UPLEVEL_PATCH)
        @test Pkg.API.__installed()[TEST_PKG.name] == v"0.3.3"
        Pkg.update(; level = UPLEVEL_MINOR)
        @test Pkg.API.__installed()[TEST_PKG.name].minor != 3
        Pkg.rm(TEST_PKG.name)
    end

    @testset "testing" begin
        # TODO: Check that preview = true doesn't actually execute the test
        Pkg.add(TEST_PKG.name)
        Pkg.test(TEST_PKG.name; coverage=true)
        pkgdir = Base.locate_package(Base.PkgId(TEST_PKG.uuid, TEST_PKG.name))
        # No coverage files being generated?
        @test_broken TEST_PKG.name * ".cov" in readdir(pkgdir)
        Pkg.rm(TEST_PKG.name)
    end

    @testset "pinning / freeing" begin
        Pkg.add(TEST_PKG.name)
        old_v = Pkg.API.__installed()[TEST_PKG.name]
        Pkg.pin(PackageSpec(TEST_PKG.name, v"0.2"))
        @test Pkg.API.__installed()[TEST_PKG.name].minor == 2
        Pkg.update(TEST_PKG.name)
        @test Pkg.API.__installed()[TEST_PKG.name].minor == 2
        Pkg.free(TEST_PKG.name)
        Pkg.update()
        @test Pkg.API.__installed()[TEST_PKG.name] == old_v
        Pkg.rm(TEST_PKG.name)
    end

    @testset "develop / freeing" begin
        Pkg.add(TEST_PKG.name)
        old_v = Pkg.API.__installed()[TEST_PKG.name]
        Pkg.rm(TEST_PKG.name)
        mktempdir() do devdir
            withenv("JULIA_PKG_DEVDIR" => devdir) do
                @test_throws PkgError Pkg.develop(PackageSpec(url="bleh", rev="blurg"))
                Pkg.develop(TEST_PKG.name)
                @test isinstalled(TEST_PKG)
                @test Pkg.API.__installed()[TEST_PKG.name] > old_v
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
                @test Pkg.API.__installed()[TEST_PKG.name] == old_v
            end
        end
    end

    @testset "invalid pkg name" begin
        @test_throws PkgError Pkg.add(",sa..,--")
    end

    @testset "stdlibs as direct dependency" begin
        uuid_pkg = (name = "CRC32c", uuid = UUID("8bf52ea8-c179-5cab-976a-9e18b702a9bc"))
        Pkg.add("CRC32c")
        @test haskey(Pkg.API.__installed(), uuid_pkg.name)
        Pkg.update()
        # Disable until fixed in Base
        # Pkg.test("CRC32c")
        Pkg.rm("CRC32c")
    end

    @testset "package name in resolver errors" begin
        try
            Pkg.add(PackageSpec(;name = TEST_PKG.name, version = v"55"))
        catch e
            @test occursin(TEST_PKG.name, sprint(showerror, e))
        end
    end

    @testset "protocols" begin
        mktempdir() do devdir
            withenv("JULIA_PKG_DEVDIR" => devdir) do
                try
                    Pkg.setprotocol!("notarealprotocol")
                    @test_throws PkgError Pkg.develop("Example")
                    Pkg.setprotocol!("https")
                    Pkg.develop("Example")
                    @test isinstalled(TEST_PKG)
                finally
                    Pkg.setprotocol!()
                end
            end
        end
        mktempdir() do devdir
            withenv("JULIA_PKG_DEVDIR" => devdir) do
                try
                    https_url = "https://github.com/JuliaLang/Example.jl.git"
                    ssh_url = "ssh://git@github.com/JuliaLang/Example.jl.git"
                    @test Pkg.GitTools.normalize_url(https_url) == https_url
                    Pkg.setprotocol!("ssh")
                    @test Pkg.GitTools.normalize_url(https_url) == ssh_url
                    # TODO: figure out how to test this without
                    #       having to deploy a ssh key on github
                    #Pkg.develop("Example")
                    #@test isinstalled(TEST_PKG)
                finally
                    Pkg.setprotocol!()
                end
            end
        end
    end

    @testset "check logging" begin
        usage = Pkg.TOML.parse(String(read(joinpath(Pkg.logdir(), "manifest_usage.toml"))))
        @test any(x -> startswith(x, joinpath(project_path, "Manifest.toml")), keys(usage))
    end

    @testset "adding nonexisting packages" begin
        nonexisting_pkg = randstring(14)
        @test_throws PkgError Pkg.add(nonexisting_pkg)
        @test_throws PkgError Pkg.update(nonexisting_pkg)
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
        @test_throws PkgError Pkg.add("julia")
    end
end

temp_pkg_dir() do project_path
    @testset "libgit2 downloads" begin
        Pkg.add(TEST_PKG.name; use_libgit2_for_all_downloads=true)
        @test haskey(Pkg.installed(), TEST_PKG.name)
        Pkg.rm(TEST_PKG.name)
    end

    @testset "up in Project without manifest" begin
        mktempdir() do dir
            cp(joinpath(@__DIR__, "test_packages", "UnregisteredWithProject"), joinpath(dir, "UnregisteredWithProject"))
            cd(joinpath(dir, "UnregisteredWithProject")) do
                with_current_env() do
                    Pkg.update()
                    @test haskey(Pkg.API.__installed(), "Example")
                end
            end
        end
    end
end

temp_pkg_dir() do project_path
    @testset "libgit2 downloads" begin
        Pkg.add(TEST_PKG.name; use_libgit2_for_all_downloads=true)
        @test haskey(Pkg.API.__installed(), TEST_PKG.name)
        Pkg.rm(TEST_PKG.name)
    end
    @testset "tarball downloads" begin
        Pkg.add("JSON"; use_only_tarballs_for_downloads=true)
        @test haskey(Pkg.API.__installed(), "JSON")
        Pkg.rm("JSON")
    end
end

@testset "parse package url win" begin
    @test typeof(Pkg.REPLMode.parse_package("https://github.com/abc/ABC.jl"; add_or_develop=true)) == PackageSpec
end

@testset "preview generate" begin
    mktempdir() do tmp
        cd(tmp) do
            Pkg.generate("Foo"; preview=true)
            @test !isdir(joinpath(tmp, "Foo"))
        end
    end
end

temp_pkg_dir() do project_path
    @testset "test should instantiate" begin
        mktempdir() do dir
            cp(joinpath(@__DIR__, "test_packages", "UnregisteredWithProject"), joinpath(dir, "UnregisteredWithProject"))
            cd(joinpath(dir, "UnregisteredWithProject")) do
                with_current_env() do
                    Pkg.add("Test") # test https://github.com/JuliaLang/Pkg.jl/issues/324
                    Pkg.test()
                end
            end
        end
    end
end

#=
temp_pkg_dir() do project_path
    @testset "valid project file names" begin
        extract_uuid(toml_path) = begin
            uuid = ""
            for line in eachline(toml_path)
                m = match(r"uuid = \"(.+)\"", line)
                if m !== nothing
                    uuid = m.captures[1]
                    break
                end
            end
            return uuid
        end

        cd(project_path) do
            mktempdir() do tmp; cd(tmp) do
                pkg_name = "FooBar"
                # create a project and grab its uuid
                withenv("USER" => "Test User") do
                    Pkg.generate(pkg_name)
                end
                uuid = extract_uuid(joinpath(pkg_name, "Project.toml"))
                # activate project env
                Pkg.activate(abspath(pkg_name))
                # add an example project to populate manifest file
                Pkg.add("Example")
                Pkg.activate()
                # change away from default names
                mv(joinpath(pkg_name, "Project.toml"), joinpath(pkg_name, "JuliaProject.toml"))
                mv(joinpath(pkg_name, "Manifest.toml"), joinpath(pkg_name, "JuliaManifest.toml"))
                # make sure things still work
                Pkg.develop(PackageSpec(url = abspath(pkg_name)))
                @test isinstalled((name=pkg_name, uuid=UUID(uuid)))
                Pkg.rm(pkg_name)
                @test !isinstalled((name=pkg_name, uuid=UUID(uuid)))
            end end
        end # cd project_path
    end # @testset
end
=#

temp_pkg_dir() do project_path
    @testset "invalid repo url" begin
        cd(project_path) do
            @test_throws PkgError Pkg.add("https://github.com")
            Pkg.generate("FooBar")
            @test_throws PkgError Pkg.add("./Foobar")
        end
    end
end


temp_pkg_dir() do project_path
    # pkg assumes `Example.jl` is still a git repo, it will try to fetch on `update`
    # `fetch` should warn that it is no longer a git repo
    with_temp_env() do
        @testset "inconsistent repo state" begin
            package_path = joinpath(project_path, "Example")
            LibGit2.with(LibGit2.clone("https://github.com/JuliaLang/Example.jl", package_path)) do repo
                Pkg.add(PackageSpec(path=package_path))
            end
            rm(joinpath(package_path, ".git"); force=true, recursive=true)
            @test_throws PkgError Pkg.update()
        end
    end
end

temp_pkg_dir() do project_path; cd(project_path) do
    @testset "instantiating updated repo" begin
        tmp = mktempdir()
        cd(tmp)
        depo1 = mktempdir()
        depo2 = mktempdir()

        empty!(DEPOT_PATH)
        pushfirst!(DEPOT_PATH, depo1)
        LibGit2.close(LibGit2.clone("https://github.com/JuliaLang/Example.jl", "Example.jl"))
        mkdir("machine1")
        cd("machine1")
        Pkg.activate(".")
        Pkg.add(PackageSpec(path="../Example.jl"))
        cd("..")
        cp("machine1", "machine2")
        empty!(DEPOT_PATH)
        pushfirst!(DEPOT_PATH, depo2)
        cd("machine2")
        Pkg.activate(".")
        Pkg.instantiate()
        cd("..")
        cd("Example.jl")
        open("README.md", "a") do io
            print(io, "Hello")
        end
        LibGit2.with(LibGit2.GitRepo(".")) do repo
            LibGit2.add!(repo, "*")
            LibGit2.commit(repo, "changes"; author=TEST_SIG, committer=TEST_SIG)
        end
        cd("../machine1")
        empty!(DEPOT_PATH)
        pushfirst!(DEPOT_PATH, depo1)
        Pkg.activate(".")
        Pkg.update()
        cd("..")
        cp("machine1/Manifest.toml", "machine2/Manifest.toml"; force=true)
        cd("machine2")
        empty!(DEPOT_PATH)
        pushfirst!(DEPOT_PATH, depo2)
        Pkg.activate(".")
        Pkg.instantiate()
    end
end end

temp_pkg_dir() do project_path
    cd(project_path) do
        project = """
        [deps]
        UUIDs = "cf7118a7-6976-5b1a-9a39-7adc72f591a4"

        [extras]
        Markdown = "d6f4376e-aef5-505a-96c1-9c027394607a"
        Test = "8dfed614-e22c-5e08-85e1-65c5234f0b40"

        [targets]
        test = ["Markdown", "Test"]
        """
        write("Project.toml", project)
        Pkg.activate(".")
        @testset "resolve ignores extras" begin
            Pkg.resolve()
            @test !(occursin("[[Test]]", read("Manifest.toml", String)))
        end
    end
end

@testset "dependency of test dependency (#567)" begin
    mktempdir() do tmpdir
        temp_pkg_dir() do project_path; cd(tmpdir) do; with_temp_env() do
            for x in ["x1", "x2", "x3"]
                cp(joinpath(@__DIR__, "test_packages/$x"), joinpath(tmpdir, "$x"))
                Pkg.develop(Pkg.PackageSpec(url = joinpath(tmpdir, x)))
            end
            Pkg.test("x3")
        end end end
    end
end

@testset "printing of stdlib paths, issue #605" begin
    path = Pkg.Types.stdlib_path("Test")
    @test Pkg.Types.pathrepr(path) == "`@stdlib/Test`"
end


temp_pkg_dir() do project_path
    @testset "Pkg.add should not mutate" begin
        package_names = ["JSON"]
        packages = PackageSpec.(package_names)
        Pkg.add(packages)
        @test [p.name for p in packages] == package_names
    end
end

include("repl.jl")
include("api.jl")

end # module
