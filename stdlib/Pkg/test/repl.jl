# This file is a part of Julia. License is MIT: https://julialang.org/license

module REPLTests

using Pkg
import Pkg.Types.CommandError
using UUIDs
using Test
import LibGit2

include("utils.jl")

const TEST_SIG = LibGit2.Signature("TEST", "TEST@TEST.COM", round(time()), 0)
const TEST_PKG = (name = "Example", uuid = UUID("7876af07-990d-54b4-ab0e-23690620f79a"))

function git_init_package(tmp, path)
    base = basename(path)
    pkgpath = joinpath(tmp, base)
    cp(path, pkgpath)
    repo = LibGit2.init(pkgpath)
    LibGit2.add!(repo, "*")
    LibGit2.commit(repo, "initial commit"; author=TEST_SIG, committer=TEST_SIG)
    close(repo)
    return pkgpath
end


@testset "generate init args" begin
    @test_throws CommandError pkg"generate"
    @test_throws CommandError pkg"init Beep"
end

mktempdir() do project_path
    cd(project_path) do
        pushfirst!(LOAD_PATH, Base.parse_load_path("@"))
        try
            withenv("USER" => "Test User") do
                pkg"generate HelloWorld"
                cd("HelloWorld")
                LibGit2.init(".")
                pkg"st"
                @eval using HelloWorld
                Base.invokelatest(HelloWorld.greet)
                @test isfile("Project.toml")
                Pkg.REPLMode.pkgstr("develop $(joinpath(@__DIR__, "test_packages", "PackageWithBuildSpecificTestDeps"))")
                Pkg.test("PackageWithBuildSpecificTestDeps")
            end
        finally
            popfirst!(LOAD_PATH)
        end
    end
end

temp_pkg_dir() do project_path; cd(project_path) do; mktempdir() do tmp_pkg_path
    pkg"init"
    pkg"add Example"
    @test isinstalled(TEST_PKG)
    v = Pkg.installed()[TEST_PKG.name]
    pkg"rm Example"
    pkg"add Example#master"
    pkg"test Example"
    @test isinstalled(TEST_PKG)
    @test Pkg.installed()[TEST_PKG.name] > v
    pkg = "UnregisteredWithoutProject"
    p = git_init_package(tmp_pkg_path, joinpath(@__DIR__, "test_packages/$pkg"))
    Pkg.REPLMode.pkgstr("add $p; precompile")
    @eval import $(Symbol(pkg))
    @test Pkg.installed()[pkg] == v"0.0"
    Pkg.test("UnregisteredWithoutProject")

    pkg2 = "UnregisteredWithProject"
    p2 = git_init_package(tmp_pkg_path, joinpath(@__DIR__, "test_packages/$pkg2"))
    Pkg.REPLMode.pkgstr("add $p2")
    Pkg.REPLMode.pkgstr("pin $pkg2")
    @eval import $(Symbol(pkg2))
    @test Pkg.installed()[pkg2] == v"0.1.0"
    Pkg.REPLMode.pkgstr("free $pkg2")
    @test_throws CommandError Pkg.REPLMode.pkgstr("free $pkg2")
    Pkg.test("UnregisteredWithProject")

    write(joinpath(p2, "Project.toml"), """
        name = "UnregisteredWithProject"
        uuid = "58262bb0-2073-11e8-3727-4fe182c12249"
        version = "0.2.0"
        """
    )
    LibGit2.with(LibGit2.GitRepo, p2) do repo
        LibGit2.add!(repo, "*")
        LibGit2.commit(repo, "bump version"; author = TEST_SIG, committer=TEST_SIG)
        pkg"update"
        @test Pkg.installed()[pkg2] == v"0.2.0"
        Pkg.REPLMode.pkgstr("rm $pkg2")

        c = LibGit2.commit(repo, "empty commit"; author = TEST_SIG, committer=TEST_SIG)
        c_hash = LibGit2.GitHash(c)
        Pkg.REPLMode.pkgstr("add $p2#$c")
    end

    mktempdir() do tmp_dev_dir
    withenv("JULIA_PKG_DEVDIR" => tmp_dev_dir) do
        pkg"develop Example"

        # Copy the manifest + project and see that we can resolve it in a new environment
        # and get all the packages installed
        proj = read("Project.toml", String)
        manifest = read("Manifest.toml", String)
        cd_tempdir() do tmp
            old_depot = copy(DEPOT_PATH)
            try
                empty!(DEPOT_PATH)
                write("Project.toml", proj)
                write("Manifest.toml", manifest)
                mktempdir() do depot_dir
                    pushfirst!(DEPOT_PATH, depot_dir)
                    pkg"instantiate"
                    @test Pkg.installed()[pkg2] == v"0.2.0"
                end
            finally
                empty!(DEPOT_PATH)
                append!(DEPOT_PATH, old_depot)
            end
        end # cd_tempdir
    end # withenv
    end # mktempdir
end # mktempdir
end # cd
end # temp_pkg_dir


locate_name(pkg) = Base.locate_package(Base.identify_package(pkg))

temp_pkg_dir() do project_path; cd(project_path) do
    mktempdir() do tmp
        mktempdir() do depot_dir
            old_depot = copy(DEPOT_PATH)
            try
                empty!(DEPOT_PATH)
                pushfirst!(DEPOT_PATH, depot_dir)
                withenv("JULIA_PKG_DEVDIR" => tmp) do
                    pkg"init"

                    # Test an unregistered package
                    p1_path = joinpath(@__DIR__, "test_packages", "UnregisteredWithProject")
                    p2_path = joinpath(@__DIR__, "test_packages", "UnregisteredWithoutProject")
                    p1_new_path = joinpath(tmp, "UnregisteredWithProject")
                    p2_new_path = joinpath(tmp, "UnregisteredWithoutProject")
                    cp(p1_path, p1_new_path)
                    cp(p2_path, p2_new_path)
                    Pkg.REPLMode.pkgstr("develop $(p1_new_path)")
                    Pkg.REPLMode.pkgstr("develop $(p2_new_path)")
                    Pkg.REPLMode.pkgstr("build; precompile")
                    @test locate_name("UnregisteredWithProject") == joinpath(p1_new_path, "src", "UnregisteredWithProject.jl")
                    @test locate_name("UnregisteredWithoutProject") == joinpath(p2_new_path, "src", "UnregisteredWithoutProject.jl")
                    @test Pkg.installed()["UnregisteredWithProject"] == v"0.1.0"
                    @test Pkg.installed()["UnregisteredWithoutProject"] == v"0.0.0"
                    Pkg.test("UnregisteredWithoutProject")
                    Pkg.test("UnregisteredWithProject")

                    pkg"develop Example#c37b675"
                    @test locate_name("Example") ==  joinpath(tmp, "Example", "src", "Example.jl")
                    Pkg.test("Example")
                end
            finally
                empty!(DEPOT_PATH)
                append!(DEPOT_PATH, old_depot)
            end
        end # withenv
    end # mktempdir
    # nested
    try
        pushfirst!(LOAD_PATH, Base.parse_load_path("@"))
        mktempdir() do other_dir
            mktempdir() do tmp; cd(tmp) do
                withenv("USER" => "Test User") do
                    pkg"generate HelloWorld"
                    cd("HelloWorld") do
                        pkg"generate SubModule1"
                        pkg"generate SubModule2"
                        pkg"develop SubModule1"
                        mkdir("tests")
                        cd("tests") do
                            pkg"develop ../SubModule2"
                        end
                        @test Pkg.installed()["SubModule1"] == v"0.1.0"
                        @test Pkg.installed()["SubModule2"] == v"0.1.0"
                    end
                    cp("HelloWorld", joinpath(other_dir, "HelloWorld"))
                end
            end end
            # Check that these didnt generate absolute paths in the Manifest by copying
            # to another directory
            cd(joinpath(other_dir, "HelloWorld")) do
                @test locate_name("SubModule1") == joinpath(pwd(), "SubModule1", "src", "SubModule1.jl")
                @test locate_name("SubModule2") == joinpath(pwd(), "SubModule2", "src", "SubModule2.jl")
            end
        end
    finally
        popfirst!(LOAD_PATH)
    end
end # cd
end # temp_pkg_dir


test_complete(s) = Pkg.REPLMode.completions(s,lastindex(s))
apply_completion(str) = begin
    c, r, s = test_complete(str)
    @test s == true
    str[1:prevind(str, first(r))]*first(c)
end

# Autocompletions
temp_pkg_dir() do project_path; cd(project_path) do
    try
        pushfirst!(LOAD_PATH, Base.parse_load_path("@"))
        Pkg.Types.registries()
        pkg"init"
        c, r = test_complete("add Exam")
        @test "Example" in c
        c, r = test_complete("rm Exam")
        @test isempty(c)
        Pkg.REPLMode.pkgstr("develop $(joinpath(@__DIR__, "test_packages", "RequireDependency"))")

        c, r = test_complete("rm RequireDep")
        @test "RequireDependency" in c
        c, r = test_complete("rm -p RequireDep")
        @test "RequireDependency" in c
        c, r = test_complete("rm --project RequireDep")
        @test "RequireDependency" in c
        c, r = test_complete("rm Exam")
        @test isempty(c)
        c, r = test_complete("rm -p Exam")
        @test isempty(c)
        c, r = test_complete("rm --project Exam")
        @test isempty(c)

        c, r = test_complete("rm -m RequireDep")
        @test "RequireDependency" in c
        c, r = test_complete("rm --manifest RequireDep")
        @test "RequireDependency" in c
        c, r = test_complete("rm -m Exam")
        @test "Example" in c
        c, r = test_complete("rm --manifest Exam")
        @test "Example" in c

        c, r = test_complete("rm RequireDep")
        @test "RequireDependency" in c
        c, r = test_complete("rm Exam")
        @test isempty(c)
        c, r = test_complete("rm -m Exam")
        c, r = test_complete("rm -m Exam")
        @test "Example" in c

        pkg"add Example"
        c, r = test_complete("rm Exam")
        @test "Example" in c
        c, r = test_complete("add --man")
        @test "--manifest" in c
        c, r = test_complete("rem")
        @test "remove" in c
        @test apply_completion("rm E") == "rm Example"
        @test apply_completion("add Exampl") == "add Example"

        c, r = test_complete("preview r")
        @test "remove" in c
        c, r = test_complete("help r")
        @test "remove" in c
        @test !("rm" in c)

    finally
        popfirst!(LOAD_PATH)
    end
end end

temp_pkg_dir() do project_path; cd(project_path) do
    mktempdir() do tmp
        cp(joinpath(@__DIR__, "test_packages", "BigProject"), joinpath(tmp, "BigProject"))
        cd(joinpath(tmp, "BigProject")) do
            try
                pushfirst!(LOAD_PATH, Base.parse_load_path("@"))
                pkg"dev SubModule"
                pkg"dev SubModule2"
                pkg"add Random"
                pkg"add Example"
                pkg"build"
                @eval using BigProject
                pkg"build BigProject"
                @test_throws CommandError pkg"add BigProject"
                pkg"test SubModule"
                pkg"test SubModule2"
                pkg"test BigProject"
                pkg"test"
                current_example = Pkg.API.installed()["Example"]
                old_project = read("Project.toml", String)
                open("Project.toml"; append=true) do io
                    print(io, """

                    [compat]
                    Example = "0.4.0"
                    """
                    )
                end
                pkg"up"
                @test Pkg.API.installed()["Example"].minor == 4
                write("Project.toml", old_project)
                pkg"up"
                @test Pkg.API.installed()["Example"] ==     current_example
            finally
                popfirst!(LOAD_PATH)
            end
        end
    end
end; end

end # module
