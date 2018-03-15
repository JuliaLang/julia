module REPLTests

using Pkg3
using UUIDs
using Test
import LibGit2

include("utils.jl")

const TEST_SIG = LibGit2.Signature("TEST", "TEST@TEST.COM", round(time(), 0), 0)
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


mktempdir() do project_path
    cd(project_path) do
        push!(LOAD_PATH, Base.parse_load_path("@"))
        try
            withenv("USER" => "Test User") do
                pkg"generate HelloWorld"
                cd("HelloWorld")
                LibGit2.init(".")
                pkg"st"
                @eval using HelloWorld
                Base.invokelatest(HelloWorld.greet)
                @test isfile("Project.toml")
            end
        finally
            pop!(LOAD_PATH)
        end
    end
end

temp_pkg_dir() do project_path; cd(project_path) do; mktempdir() do tmp_pkg_path
    pkg"init"
    pkg"add Example"
    @test isinstalled(TEST_PKG)
    v = Pkg3.installed()[TEST_PKG.name]
    pkg"rm Example"
    pkg"add Example#master"
    pkg"test Example"
    @test isinstalled(TEST_PKG)
    @test Pkg3.installed()[TEST_PKG.name] > v
    pkg = "UnregisteredWithoutProject"
    p = git_init_package(tmp_pkg_path, joinpath(@__DIR__, "test_packages/$pkg"))
    Pkg3.REPLMode.pkgstr("add $p")
    @eval import $(Symbol(pkg))
    @test Pkg3.installed()[pkg] == v"0.0"
    Pkg3.test("UnregisteredWithoutProject")

    pkg2 = "UnregisteredWithProject"
    p2 = git_init_package(tmp_pkg_path, joinpath(@__DIR__, "test_packages/$pkg2"))
    Pkg3.REPLMode.pkgstr("add $p2")
    @eval import $(Symbol(pkg2))
    @test Pkg3.installed()[pkg2] == v"0.1.0"
    Pkg3.test("UnregisteredWithProject")

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
        @test Pkg3.installed()[pkg2] == v"0.2.0"
        Pkg3.REPLMode.pkgstr("rm $pkg2")

        c = LibGit2.commit(repo, "empty commit"; author = TEST_SIG, committer=TEST_SIG)
        c_hash = LibGit2.GitHash(c)
        Pkg3.REPLMode.pkgstr("add $p2#$c")
    end

    # TODO cleanup
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
                depot_dir = mktempdir()
                pushfirst!(DEPOT_PATH, depot_dir)
                pkg"up --fixed"
                @test Pkg3.installed()[pkg2] == v"0.2.0"
                try rm(depot_dir; recursive=true) end
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
        withenv("JULIA_PKG_DEVDIR" => tmp) do
            pkg"init"
            pkg"develop Example#c37b675"
            @test locate_name("Example") ==  joinpath(tmp, "Example", "src", "Example.jl")
            Pkg3.test("Example")
            # Test an unregistered package
            p1_path = joinpath(@__DIR__, "test_packages", "UnregisteredWithProject")
            p2_path = joinpath(@__DIR__, "test_packages", "UnregisteredWithoutProject")
            Pkg3.REPLMode.pkgstr("develop $(p1_path)")
            Pkg3.REPLMode.pkgstr("develop $(p2_path)")
            @test locate_name("UnregisteredWithProject") == joinpath(p1_path, "src", "UnregisteredWithProject.jl")
            @test locate_name("UnregisteredWithoutProject") == joinpath(p2_path, "src", "UnregisteredWithoutProject.jl")
            @test Pkg3.installed()["UnregisteredWithProject"] == v"0.1.0"
            @test Pkg3.installed()["UnregisteredWithoutProject"] == v"0.0.0"
            Pkg3.test("UnregisteredWithoutProject")
            Pkg3.test("UnregisteredWithProject")
        end
    end # withenv
end # mktempdir
end # temp_pkg_dir


end # module
