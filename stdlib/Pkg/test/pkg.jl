# This file is a part of Julia. License is MIT: https://julialang.org/license

module PkgTests

using Test
import Pkg
import Pkg.PkgError
using LibGit2
using Random: randstring

function capture_stdout(f::Function)
    let fname = tempname()
        try
            open(fname, "w") do fout
                redirect_stdout(fout) do
                    f()
                end
            end
            return read(fname, String)
        finally
            rm(fname, force=true)
        end
    end
end


function temp_pkg_dir(fn::Function, tmp_dir=joinpath(tempdir(), randstring()),
        remove_tmp_dir::Bool=true; initialize::Bool=true)

    # Used in tests below to set up and tear down a sandboxed package directory
    withenv("JULIA_PKGDIR" => tmp_dir) do
        @test !isdir(Pkg.dir())
        try
            if initialize
                Pkg.init()
                @test isdir(Pkg.dir())
                Pkg.resolve()
            else
                mkpath(Pkg.dir())
            end
            fn()
        finally
            remove_tmp_dir && rm(tmp_dir, recursive=true)
        end
    end
end

function write_build(pkg, content)
    build_filename = Pkg.dir(pkg, "deps", "build.jl")
    mkpath(dirname(build_filename))
    write(build_filename, content)
end

# Test basic operations: adding or removing a package, status, free
# Also test for the existence of REQUIRE and META_BRANCH
temp_pkg_dir() do
    @test isfile(joinpath(Pkg.dir(),"REQUIRE"))
    @test isfile(joinpath(Pkg.dir(),"META_BRANCH"))
    @test isempty(Pkg.installed())
    @test sprint(Pkg.status) == "No packages installed\n"
    @test !isempty(Pkg.available())

    # 18325
    cd(Pkg.dir()) do
        avail = Pkg.Read.available()
        avail_copy = Pkg.Query.availcopy(avail);
        delete!(avail_copy["Example"][v"0.0.1"].requires, "julia")
        @test haskey(avail["Example"][v"0.0.1"].requires, "julia")
    end

    @test_throws PkgError Pkg.installed("MyFakePackage")
    @test_throws PkgError Pkg.status("MyFakePackage")
    @test Pkg.installed("Example") === nothing

    # Check that setprotocol! works.
    begin
        try
            Pkg.setprotocol!("notarealprotocol")
            Pkg.add("Example")
            error("unexpected")
        catch ex
            if isa(ex, CompositeException)
                ex = ex.exceptions[1]

                if isa(ex, CapturedException)
                    ex = ex.ex
                end
            end
            @test isa(ex,Pkg.PkgError)
            @test occursin("Cannot clone Example from notarealprotocol://github.com/JuliaLang/Example.jl.git", ex.msg)
        end
    end

    Pkg.setprotocol!("")
    @test Pkg.Cache.rewrite_url_to === nothing
    Pkg.setprotocol!("https")
    Pkg.add("Example")
    @test [keys(Pkg.installed())...] == ["Example"]
    iob = IOBuffer()
    Pkg.checkout("Example")
    Pkg.status("Example", iob)
    str = chomp(String(take!(iob)))
    @test startswith(str, " - Example")
    @test endswith(str, "master")
    Pkg.free("Example")
    Pkg.status("Example", iob)
    str = chomp(String(take!(iob)))
    @test endswith(str, string(Pkg.installed("Example")))
    Pkg.checkout("Example")
    Pkg.free(("Example",))
    Pkg.status("Example", iob)
    str = chomp(String(take!(iob)))
    @test endswith(str, string(Pkg.installed("Example")))
    Pkg.rm("Example")
    @test isempty(Pkg.installed())
    @test !isempty(Pkg.available("Example"))
    @test !in("Example", keys(Pkg.installed()))
    Pkg.rm("Example")
    @test isempty(Pkg.installed())
    @test !isempty(Pkg.available("Example"))
    @test !in("Example", keys(Pkg.installed()))
    Pkg.clone("https://github.com/JuliaLang/Example.jl.git")
    @test [keys(Pkg.installed())...] == ["Example"]
    Pkg.status("Example", iob)
    str = chomp(String(take!(iob)))
    @test startswith(str, " - Example")
    @test endswith(str, "master")
    Pkg.free("Example")
    Pkg.status("Example", iob)
    str = chomp(String(take!(iob)))
    @test endswith(str, string(Pkg.installed("Example")))
    Pkg.checkout("Example")
    Pkg.free(("Example",))
    Pkg.status("Example", iob)
    str = chomp(String(take!(iob)))
    @test endswith(str, string(Pkg.installed("Example")))

    # 17364 - a, Pkg.checkout with specific local branch
    let branch_name = "test-branch-1",
        branch_commit = "ba3888212e30a7974ac6803a89e64c7098f4865e"

        # create a branch in Example package
        LibGit2.with(LibGit2.GitRepo, Pkg.dir("Example")) do repo
            LibGit2.branch!(repo, branch_name, branch_commit, set_head=false)
        end

        Pkg.clone(Pkg.dir("Example"), "Example2")
        Pkg.clone(Pkg.dir("Example"), "Example3")
        open(Pkg.dir("Example3", "README.md"), "w") do f
            println(f, "overwritten")
        end
        LibGit2.with(LibGit2.GitRepo, Pkg.dir("Example3")) do repo
            LibGit2.add!(repo, "README.md")
            test_sig = LibGit2.Signature("TEST", "TEST@TEST.COM", round(time(), 0), 0)
            LibGit2.commit(repo, "testmsg"; author=test_sig, committer=test_sig)
        end

        Pkg.checkout("Example2", branch_name)
        Pkg.checkout("Example3", branch_name)

        LibGit2.with(LibGit2.GitRepo, Pkg.dir("Example2")) do repo
            @test LibGit2.head_oid(repo) == LibGit2.GitHash(branch_commit)
        end
        LibGit2.with(LibGit2.GitRepo, Pkg.dir("Example3")) do repo
            @test LibGit2.head_oid(repo) == LibGit2.GitHash(branch_commit)
        end
    end

    # 17364 - b, remote off-tree branch
    let branch_name = "test-branch-2",
        branch_commit = "ba3888212e30a7974ac6803a89e64c7098f4865e"

        # create a branch in Example package
        LibGit2.with(LibGit2.GitRepo, Pkg.dir("Example")) do repo
            LibGit2.branch!(repo, branch_name, branch_commit, set_head=true)
        end

        # Make changes to local branch
        open(Pkg.dir("Example", "README.md"), "w") do f
            println(f, "overwritten")
        end

        test_commit = LibGit2.with(LibGit2.GitRepo, Pkg.dir("Example")) do repo
            LibGit2.add!(repo, "README.md")
            test_sig = LibGit2.Signature("TEST", "TEST@TEST.COM", round(time(), 0), 0)
            LibGit2.commit(repo, "testmsg"; author=test_sig, committer=test_sig)
        end
        Pkg.checkout("Example")

        Pkg.clone(Pkg.dir("Example"), "Example4")
        Pkg.checkout("Example4", branch_name)

        LibGit2.with(LibGit2.GitRepo, Pkg.dir("Example4")) do repo
            @test LibGit2.head_oid(repo) == test_commit
        end
    end

    # adding a package with unsatisfiable julia version requirements (REPL.jl) errors
    try
        Pkg.add("REPL")
        error("unexpected")
    catch err
        @test isa(err.exceptions[1].ex, PkgError)
        @test err.exceptions[1].ex.msg == "REPL can't be installed because " *
            "it has no versions that support $VERSION of julia. You may " *
            "need to update METADATA by running `Pkg.update()`"
    end

    # trying to add, check availability, or pin a nonexistent package errors
    try
        Pkg.add("NonexistentPackage")
        error("unexpected")
    catch err
        @test isa(err.exceptions[1].ex, PkgError)
        @test err.exceptions[1].ex.msg == "unknown package NonexistentPackage"
    end
    try
        Pkg.available("NonexistentPackage")
        error("unexpected")
    catch err
        @test isa(err, PkgError)
        @test err.msg == "NonexistentPackage is not a package (not registered or installed)"
    end
    try
        Pkg.pin("NonexistentPackage", v"1.0.0")
        error("unexpected")
    catch err
        @test isa(err, PkgError)
        @test err.msg == "NonexistentPackage is not a git repo"
    end

    # trying to pin a git repo under Pkg.dir that is not an installed package errors
    try
        Pkg.pin("METADATA", v"1.0.0")
        error("unexpected")
    catch err
        @test isa(err, PkgError)
        @test err.msg == "METADATA cannot be pinned – not an installed package"
    end

    # trying to pin an installed, registered package to an unregistered version errors
    try
        Pkg.pin("Example", v"2147483647.0.0")
        error("unexpected")
    catch err
        @test isa(err, PkgError)
        @test err.msg == "Example – 2147483647.0.0 is not a registered version"
    end

    # PR #13572, handling of versions with untagged detached heads
    LibGit2.with(LibGit2.GitRepo, Pkg.dir("Example")) do repo
        LibGit2.checkout!(repo, "72f09c7d0099793378c645929a9961155faae6d2")
    end
    @test Pkg.installed()["Example"] > v"0.0.0"

    # issue #13583
    begin
        try
            Pkg.test("IDoNotExist")
            error("unexpected")
        catch ex
            @test isa(ex,Pkg.Entry.PkgTestError)
            @test ex.msg == "IDoNotExist is not an installed package"
        end

        try
            Pkg.test("IDoNotExist1", "IDoNotExist2")
            error("unexpected")
        catch ex
            @test isa(ex,Pkg.Entry.PkgTestError)
            @test ex.msg == "IDoNotExist1 and IDoNotExist2 are not installed packages"
        end
    end

    # Various pin/free/re-pin/change-pin patterns (issue #17176)
    @test "" == capture_stdout() do
        nothingtodomsg = (:info,"No packages to install, update or remove")

        @test_logs((:info,"Freeing Example"),
                   nothingtodomsg,
                   Pkg.free("Example"))

        @test_logs (:info,r"^Creating Example branch pinned\.[0-9a-f]{8}\.tmp$") Pkg.pin("Example")
        vers = Pkg.installed("Example")
        branch = LibGit2.with(LibGit2.GitRepo, Pkg.dir("Example")) do repo
            LibGit2.branch(repo)
        end

        @test_logs((:info,"Freeing Example"),
                   nothingtodomsg,
                   Pkg.free("Example"))

        @test_logs((:info,"Creating Example branch pinned.b1990792.tmp"),
                   nothingtodomsg, Pkg.pin("Example", v"0.4.0"))
        @test Pkg.installed("Example") == v"0.4.0"

        @test_logs (:info,r"^Package Example is already pinned to the selected commit$") Pkg.pin("Example", v"0.4.0")
        @test Pkg.installed("Example") == v"0.4.0"

        @test_logs (:info,r"^Package Example is already pinned$") Pkg.pin("Example")
        @test Pkg.installed("Example") == v"0.4.0"

        @test_logs (:info,"Package Example: skipping update (pinned)...") match_mode=:any Pkg.update()
        @test Pkg.installed("Example") == v"0.4.0"

        @test_logs((:info,"Creating Example branch pinned.d1ef7b00.tmp"),
                   nothingtodomsg, Pkg.pin("Example", v"0.3.1"))
        @test Pkg.installed("Example") == v"0.3.1"

        @test_logs((:info,"Package Example: checking out existing branch pinned.b1990792.tmp"),
                   nothingtodomsg, Pkg.pin("Example", v"0.4.0"))
        @test Pkg.installed("Example") == v"0.4.0"

        @test_logs((:info,"Freeing Example"),
                   nothingtodomsg, Pkg.free("Example"))
        @test Pkg.installed("Example") == vers

        @test_logs (:info,Regex("^Package Example: checking out existing branch $branch\$")) Pkg.pin("Example")
        @test Pkg.installed("Example") == vers

        @test_logs((:info,"Freeing Example"),
                   nothingtodomsg, Pkg.free("Example"))
        @test Pkg.installed("Example") == vers
    end

    begin
        # bug identified in #16850, Base.url \ vs / for non-Base methods
        include(Pkg.dir("Example","src","Example.jl"))
        meth = first(methods(Example.domath))
        fname = string(meth.file)
        @test ('\\' in fname) == Sys.iswindows()
        @test startswith(Base.url(meth), "https://github.com/JuliaLang/Example.jl/tree")
    end

    # add a directory that is not a git repository
    begin
        mkdir(joinpath(Pkg.dir(), "NOTGIT"))
        Pkg.installed("NOTGIT") == typemin(VersionNumber)
        Pkg.installed()["NOTGIT"] == typemin(VersionNumber)
    end

    begin
        # don't bork when a Pkg repo is bare, issue #13804
        pth = joinpath(Pkg.dir(), "BAREGIT")
        mkdir(pth)
        # create a bare repo (isbare = true)
        repo = LibGit2.init(pth, true)
        @test repo.ptr != C_NULL
        close(repo)
        Pkg.update()
    end

    #test PkgDev redirects
    begin
        try
            Pkg.register("IDoNotExist")
            error("unexpected")
        catch ex
            @test ex.msg == "Pkg.register(pkg,[url]) has been moved to the package PkgDev.jl.\nRun Pkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-"
        end

        try
            Pkg.tag("IDoNotExist")
            error("unexpected")
        catch ex
            @test ex.msg == "Pkg.tag(pkg, [ver, [commit]]) has been moved to the package PkgDev.jl.\nRun Pkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-"
        end

        try
            Pkg.generate("IDoNotExist","MIT")
            error("unexpected")
        catch ex
            @test ex.msg == "Pkg.generate(pkg, license) has been moved to the package PkgDev.jl.\nRun Pkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-"
        end

        try
            Pkg.publish()
            error("unexpected")
        catch ex
            @test ex.msg == "Pkg.publish() has been moved to the package PkgDev.jl.\nRun Pkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-"
        end

        try
            Pkg.license()
            error("unexpected")
        catch ex
            @test ex.msg == "Pkg.license([lic]) has been moved to the package PkgDev.jl.\nRun Pkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-"
        end
        try
            Pkg.submit("IDoNotExist")
            error("unexpected")
        catch ex
            @test ex.msg == "Pkg.submit(pkg[, commit]) has been moved to the package PkgDev.jl.\nRun Pkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-"
        end
        try
            Pkg.submit("IDoNotExist", "nonexistentcommit")
            error("unexpected")
        catch ex
            @test ex.msg == "Pkg.submit(pkg[, commit]) has been moved to the package PkgDev.jl.\nRun Pkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-"
        end
    end

    # Test Pkg.Read.url works
    @test Pkg.Read.url("Example") == "git://github.com/JuliaLang/Example.jl.git"

    # issue #15789, build failure warning are printed correctly.
    # Also makes sure `Pkg.build()` works even for non-git repo
    begin
        pth = joinpath(Pkg.dir(), "BuildFail")
        mkdir(pth)
        depspath = joinpath(pth, "deps")
        mkdir(depspath)
        depsbuild = joinpath(depspath, "build.jl")
        touch(depsbuild)
        # Pkg.build works without the src directory now
        # but it's probably fine to require it.
        msg = read(`$(Base.julia_cmd()) --startup-file=no -e 'redirect_stderr(stdout); using Logging; global_logger(SimpleLogger(stdout)); import Pkg; Pkg.build("BuildFail")'`, String)
        @test occursin("Building BuildFail", msg)
        @test !occursin("Build failed for BuildFail", msg)
        open(depsbuild, "w") do fd
            println(fd, "error(\"Throw build error\")")
        end
        msg = read(`$(Base.julia_cmd()) --startup-file=no -e 'redirect_stderr(stdout); using Logging; global_logger(SimpleLogger(stdout)); import Pkg; Pkg.build("BuildFail")'`, String)
        @test occursin("Building BuildFail", msg)
        @test occursin("Build failed for BuildFail", msg)
        @test occursin("Pkg.build(\"BuildFail\")", msg)
        @test occursin("Throw build error", msg)
    end

    # issue #15948
    let package = "Example"
        Pkg.rm(package)  # Remove package if installed
        @test Pkg.installed(package) === nothing  # Registered with METADATA but not installed
        msg = read(ignorestatus(`$(Base.julia_cmd()) --startup-file=no -e "redirect_stderr(stdout); using Logging; global_logger(SimpleLogger(stdout)); import Pkg; Pkg.build(\"$package\")"`), String)
        @test occursin("$package is not an installed package", msg)
        @test !occursin("signal (15)", msg)
    end

    # issue #20695
    Pkg.cd() do
        @test Pkg.Entry.url_and_pkg("Example") == ("git://github.com/JuliaLang/Example.jl.git", "Example")
        for url = [
            "https://github.com/Org/Nonsense",
            "git@github.com:Org/Nonsense",
            "file:///home/user/Nonsense",
            "/home/user/Nonsense",
        ]
            @test Pkg.Entry.url_and_pkg(url) == (url, "Nonsense")
            @test Pkg.Entry.url_and_pkg("$url.jl") == ("$url.jl", "Nonsense")
            @test Pkg.Entry.url_and_pkg("$url.git") == ("$url.git", "Nonsense")
            @test Pkg.Entry.url_and_pkg("$url.jl.git") == ("$url.jl.git", "Nonsense")
        end
        pkg = randstring(20)
        @test Pkg.Entry.url_and_pkg(pkg) == (pkg, pkg)
    end

    # partial Pkg.update
    @test "" == capture_stdout() do
        nothingtodomsg = (:info,"No packages to install, update or remove")

        @test_logs (:info,r"Installing Example v") match_mode=:any begin
            Pkg.rm("Example")
            Pkg.add("Example")
        end

        @test_logs nothingtodomsg match_mode=:any Pkg.update("Example")

        @test_logs (:info,"Installing Example v0.4.0") match_mode=:any begin
            Pkg.rm("Example")
            Pkg.add("Example", v"0", v"0.4.1-") # force version to be < 0.4.1
        end


        @test_logs((:info,r"""Package Example was set to version 0\.4\.0, but a higher version \d+\.\d+\.\d+\S* exists.
                              The update is prevented by explicit requirements constraints. Edit your REQUIRE file to change this."""),
                   nothingtodomsg, match_mode=:any, Pkg.update("Example"))

        @test_logs (:info,r"Installing Example") match_mode=:any begin
            Pkg.rm("Example")
            Pkg.add("Example")
            Pkg.pin("Example", v"0.4.0")
        end

        @test_logs((:info,"Package Example: skipping update (pinned)..."),
                   (:info,r"""Package Example was set to version 0\.4\.0, but a higher version \d+\.\d+\.\d+\S* exists.
                              The package is fixed. You can try using `Pkg.free\("Example"\)` to update it."""),
                   nothingtodomsg, match_mode=:any, Pkg.update("Example"))

        metadata_dir = Pkg.dir("METADATA")
        old_commit = "313bfaafa301e82d40574a778720e893c559a7e2"

        # Force a METADATA rollback to an old version, so that we will install some
        # outdated versions of some packages and then update some of those
        # (note that the following Pkg.update calls will update METADATA to the
        # latest version even though they don't update all packages)
        LibGit2.with(LibGit2.GitRepo, metadata_dir) do repo
            LibGit2.reset!(repo, LibGit2.GitHash(old_commit), LibGit2.Consts.RESET_HARD)
        end

        # run these at an old metadata commit where it's guaranteed no
        # packages depend on Example.jl
        @test isempty(Pkg.dependents("Example"))
        @test isempty(Pkg.dependents("Example.jl"))

        logs,_ = Test.collect_test_logs() do
            Pkg.add("Iterators")
            Pkg.update("Iterators")
        end
        @test all(!occursin("updated but were already imported", l.message) for l in logs)

        # Do it again, because the above Iterators test will update things prematurely
        LibGit2.with(LibGit2.GitRepo, metadata_dir) do repo
            LibGit2.reset!(repo, LibGit2.GitHash(old_commit), LibGit2.Consts.RESET_HARD)
        end

        @test_logs((:info,"Installing Colors v0.6.4"),
                   (:info,"Installing ColorTypes v0.2.2"),
                   (:info,"Installing FixedPointNumbers v0.1.3"),
                   (:info,"Installing Compat v0.7.18"),
                   (:info,"Installing Reexport v0.0.3"), match_mode=:any, Pkg.add("Colors"))

        logs,_ = Test.collect_test_logs() do
            Pkg.update("ColorTypes")
        end
        @test any(occursin((:info, r"Upgrading ColorTypes: v0\.2\.2 => v\d+\.\d+\.\d+"), l) for l in logs)
        @test any(occursin((:info, r"Upgrading Compat: v0\.7\.18 => v\d+\.\d+\.\d+"), l) for l in logs)
        @test !any(occursin((:info, r"Upgrading Colors"), l) for l in logs)

        @test Pkg.installed("Colors") == v"0.6.4"

        @test_logs nothingtodomsg match_mode=:any Pkg.update("FixedPointNumbers")
    end

    # issue #18239
    let package = "Example"
        Pkg.free(package)
        Pkg.rm(package)  # Remove package if installed

        metadata_dir = Pkg.dir("METADATA")
        old_commit = "83ff7116e51fc9cdbd7e67affbd344b9f5c9dbf2"

        # Reset METADATA to the second to last update of Example.jl
        LibGit2.with(LibGit2.GitRepo, metadata_dir) do repo
            LibGit2.reset!(repo, LibGit2.GitHash(old_commit), LibGit2.Consts.RESET_HARD)
        end

        Pkg.add(package)
        msg = read(ignorestatus(`$(Base.julia_cmd()) --startup-file=no -e
            "redirect_stderr(stdout); using Logging; global_logger(SimpleLogger(stdout)); using Example; import Pkg; Pkg.update(\"$package\")"`), String)
        @test occursin(Regex("- $package.*Restart Julia to use the updated versions","s"), msg)
    end

    # Verify that the --startup-file flag is respected by Pkg.build / Pkg.test
    let package = "StartupFile"
        content = """
            @info "JULIA_RC_LOADED defined \$(isdefined(@__MODULE__, :JULIA_RC_LOADED))"
            @info "Main.JULIA_RC_LOADED defined \$(isdefined(Main, :JULIA_RC_LOADED))"
            """

        write_build(package, content)

        test_filename = Pkg.dir(package, "test", "runtests.jl")
        mkpath(dirname(test_filename))
        write(test_filename, content)

        # Make a ~/.julia/config/startup.jl
        home = Pkg.dir(".home")
        mkpath(joinpath(home, ".julia", "config"))
        write(joinpath(home, ".julia", "config", "startup.jl"),
            "const JULIA_RC_LOADED = true")

        withenv((Sys.iswindows() ? "USERPROFILE" : "HOME") => home) do
            code = "redirect_stderr(stdout); using Logging; global_logger(SimpleLogger(stdout)); import Pkg; Pkg.build(\"$package\")"
            msg = read(`$(Base.julia_cmd()) --startup-file=no -e $code`, String)
            @test occursin("JULIA_RC_LOADED defined false", msg)
            @test occursin("Main.JULIA_RC_LOADED defined false", msg)

            msg = read(`$(Base.julia_cmd()) --startup-file=yes -e $code`, String)
            @test occursin("JULIA_RC_LOADED defined false", msg)
            @test occursin("Main.JULIA_RC_LOADED defined true", msg)

            code = "redirect_stderr(stdout); using Logging; global_logger(SimpleLogger(stdout)); import Pkg; Pkg.test(\"$package\")"

            msg = read(`$(Base.julia_cmd()) --startup-file=no -e $code`, String)
            @test occursin("JULIA_RC_LOADED defined false", msg)
            @test occursin("Main.JULIA_RC_LOADED defined false", msg)

            # Note: Since both the startup-file and "runtests.jl" are run in the Main
            # module any global variables created in the startup file can be referenced.
            msg = read(`$(Base.julia_cmd()) --startup-file=yes -e $code`, String)
            @test occursin("JULIA_RC_LOADED defined true", msg)
            @test occursin("Main.JULIA_RC_LOADED defined true", msg)
        end
    end

    let package = "Output"
        stdout_file = Pkg.dir(package, "stdout.txt")
        stderr_file = Pkg.dir(package, "stderr.txt")
        content = """
            println(stdout, "stdout")
            println(stderr, "stderr")
            """
        write_build(package, content)

        code = "import Pkg; Pkg.build(\"$package\")"
        msg = run(pipeline(
            `$(Base.julia_cmd()) --startup-file=no -e $code`,
            stdout=stdout_file, stderr=stderr_file))
        @test last(readlines(stdout_file)) == "stdout"
        @test last(readlines(stderr_file)) == "stderr"
    end
end

@testset "Pkg functions with .jl extension" begin
    temp_pkg_dir() do
        @test Pkg.installed("Example.jl") === nothing
        Pkg.add("Example.jl")
        @test [keys(Pkg.installed())...] == ["Example"]
        iob = IOBuffer()
        Pkg.update("Example.jl")
        Pkg.checkout("Example.jl")
        Pkg.status("Example.jl", iob)
        str = chomp(String(take!(iob)))
        @test startswith(str, " - Example")
        @test endswith(str, "master")
        Pkg.free("Example.jl")
        Pkg.status("Example.jl", iob)
        str = chomp(String(take!(iob)))
        @test endswith(str, string(Pkg.installed("Example.jl")))
        Pkg.checkout("Example.jl")
        Pkg.free(("Example.jl",))
        Pkg.status("Example.jl", iob)
        str = chomp(String(take!(iob)))
        @test endswith(str, string(Pkg.installed("Example.jl")))
        Pkg.rm("Example.jl")
        @test isempty(Pkg.installed())
        @test !isempty(Pkg.available("Example.jl"))
        @test !in("Example", keys(Pkg.installed()))
        Pkg.rm("Example.jl")
        @test isempty(Pkg.installed())
        @test !isempty(Pkg.available("Example.jl"))
        @test !in("Example", keys(Pkg.installed()))
        Pkg.clone("https://github.com/JuliaLang/Example.jl.git")
        @test [keys(Pkg.installed())...] == ["Example"]
        Pkg.status("Example.jl", iob)
        str = chomp(String(take!(iob)))
        @test startswith(str, " - Example")
        @test endswith(str, "master")
        Pkg.free("Example.jl")
        Pkg.status("Example.jl", iob)
        str = chomp(String(take!(iob)))
        @test endswith(str, string(Pkg.installed("Example.jl")))
        Pkg.checkout("Example.jl")
        Pkg.free(("Example.jl",))
        Pkg.status("Example.jl", iob)
        str = chomp(String(take!(iob)))
        @test endswith(str, string(Pkg.installed("Example.jl")))
    end
end

let io = IOBuffer()
    Base.showerror(io, Pkg.Entry.PkgTestError("ppp"), backtrace())
    @test !occursin("backtrace()", String(take!(io)))
end

@testset "Relative path operations" begin
    cd(tempdir()) do
        temp_pkg_dir(randstring()) do
            Pkg.add("Example")
            @test [keys(Pkg.installed())...] == ["Example"]
        end
    end
end

temp_pkg_dir(initialize=false) do
    write_build("Normal", "")
    write_build("Error", "error(\"An error has occurred while building a package\")")
    write_build("Exit", "exit()")

    cd(Pkg.dir()) do
        errors = Dict()

        # Log forwarding TODO - port these to @test_logs
        empty!(errors)
        @test_warn ("Building Error",
                    "Building Normal") Pkg.Entry.build!(["Error", "Normal"], errors)

        empty!(errors)
        @test_warn ("Building Exit",
                    "Building Normal") Pkg.Entry.build!(["Exit", "Normal"], errors)

        empty!(errors)
        @test_warn ("Building Exit",
                    "Building Normal",
                    "Building Exit",
                    "Building Normal") Pkg.Entry.build!(["Exit", "Normal", "Exit", "Normal"], errors)
    end
end

# VersionSet tests
import Pkg.Types: VersionSet, VersionInterval

function chkint(a::VersionSet)
    ints = a.intervals
    for k = 1:length(ints)
        ints[k].lower < ints[k].upper || return false
        k < length(ints) && (ints[k].upper < ints[k+1].lower || return false)
    end
    return true
end

const empty_versionset = VersionSet(VersionInterval[])
@test isempty(empty_versionset)

# VersionSet intersections and unions
@test empty_versionset ∩ empty_versionset == empty_versionset
@test empty_versionset ∪ empty_versionset == empty_versionset
for t = 1:1_000
    a = VersionSet(sort!(map(v->VersionNumber(v...), [(rand(0:8),rand(0:3)) for i = 1:rand(0:10)]))...)
    b = VersionSet(sort!(map(v->VersionNumber(v...), [(rand(0:8),rand(0:3)) for i = 1:rand(0:10)]))...)
    @assert chkint(a)
    @assert chkint(b)
    u = a ∪ b
    @test chkint(u)
    i = a ∩ b
    @test chkint(i)
    for vM = 0:9, vm = 0:5
        v = VersionNumber(vM, vm)
        @test (v ∈ a || v ∈ b) ? (v ∈ u) : (v ∉ u)
        @test (v ∈ a && v ∈ b) ? (v ∈ i) : (v ∉ i)
    end
end

end # module
