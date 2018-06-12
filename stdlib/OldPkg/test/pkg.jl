# This file is a part of Julia. License is MIT: https://julialang.org/license

module PkgTests

using Test
import OldPkg
import OldPkg.PkgError
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
        @test !isdir(OldPkg.dir())
        try
            if initialize
                OldPkg.init()
                @test isdir(OldPkg.dir())
                OldPkg.resolve()
            else
                mkpath(OldPkg.dir())
            end
            fn()
        finally
            remove_tmp_dir && rm(tmp_dir, recursive=true)
        end
    end
end

function write_build(pkg, content)
    build_filename = OldPkg.dir(pkg, "deps", "build.jl")
    mkpath(dirname(build_filename))
    write(build_filename, content)
end

# Test basic operations: adding or removing a package, status, free
# Also test for the existence of REQUIRE and META_BRANCH
temp_pkg_dir() do
    @test isfile(joinpath(OldPkg.dir(),"REQUIRE"))
    @test isfile(joinpath(OldPkg.dir(),"META_BRANCH"))
    @test isempty(OldPkg.installed())
    @test sprint(OldPkg.status) == "No packages installed\n"
    @test !isempty(OldPkg.available())

    # 18325
    cd(OldPkg.dir()) do
        avail = OldPkg.Read.available()
        avail_copy = OldPkg.Query.availcopy(avail);
        delete!(avail_copy["Example"][v"0.0.1"].requires, "julia")
        @test haskey(avail["Example"][v"0.0.1"].requires, "julia")
    end

    @test_throws PkgError OldPkg.installed("MyFakePackage")
    @test_throws PkgError OldPkg.status("MyFakePackage")
    @test OldPkg.installed("Example") === nothing

    # Check that setprotocol! works.
    begin
        try
            OldPkg.setprotocol!("notarealprotocol")
            OldPkg.add("Example")
            error("unexpected")
        catch ex
            if isa(ex, CompositeException)
                ex = ex.exceptions[1]

                if isa(ex, CapturedException)
                    ex = ex.ex
                end
            end
            @test isa(ex,OldPkg.PkgError)
            @test occursin("Cannot clone Example from notarealprotocol://github.com/JuliaLang/Example.jl.git", ex.msg)
        end
    end

    OldPkg.setprotocol!("")
    @test OldPkg.Cache.rewrite_url_to === nothing
    OldPkg.setprotocol!("https")
    OldPkg.add("Example")
    @test [keys(OldPkg.installed())...] == ["Example"]
    iob = IOBuffer()
    OldPkg.checkout("Example")
    OldPkg.status("Example", iob)
    str = chomp(String(take!(iob)))
    @test startswith(str, " - Example")
    @test endswith(str, "master")
    OldPkg.free("Example")
    OldPkg.status("Example", iob)
    str = chomp(String(take!(iob)))
    @test endswith(str, string(OldPkg.installed("Example")))
    OldPkg.checkout("Example")
    OldPkg.free(("Example",))
    OldPkg.status("Example", iob)
    str = chomp(String(take!(iob)))
    @test endswith(str, string(OldPkg.installed("Example")))
    OldPkg.rm("Example")
    @test isempty(OldPkg.installed())
    @test !isempty(OldPkg.available("Example"))
    @test !in("Example", keys(OldPkg.installed()))
    OldPkg.rm("Example")
    @test isempty(OldPkg.installed())
    @test !isempty(OldPkg.available("Example"))
    @test !in("Example", keys(OldPkg.installed()))
    OldPkg.clone("https://github.com/JuliaLang/Example.jl.git")
    @test [keys(OldPkg.installed())...] == ["Example"]
    OldPkg.status("Example", iob)
    str = chomp(String(take!(iob)))
    @test startswith(str, " - Example")
    @test endswith(str, "master")
    OldPkg.free("Example")
    OldPkg.status("Example", iob)
    str = chomp(String(take!(iob)))
    @test endswith(str, string(OldPkg.installed("Example")))
    OldPkg.checkout("Example")
    OldPkg.free(("Example",))
    OldPkg.status("Example", iob)
    str = chomp(String(take!(iob)))
    @test endswith(str, string(OldPkg.installed("Example")))

    # 17364 - a, OldPkg.checkout with specific local branch
    let branch_name = "test-branch-1",
        branch_commit = "ba3888212e30a7974ac6803a89e64c7098f4865e"

        # create a branch in Example package
        LibGit2.with(LibGit2.GitRepo, OldPkg.dir("Example")) do repo
            LibGit2.branch!(repo, branch_name, branch_commit, set_head=false)
        end

        OldPkg.clone(OldPkg.dir("Example"), "Example2")
        OldPkg.clone(OldPkg.dir("Example"), "Example3")
        open(OldPkg.dir("Example3", "README.md"), "w") do f
            println(f, "overwritten")
        end
        LibGit2.with(LibGit2.GitRepo, OldPkg.dir("Example3")) do repo
            LibGit2.add!(repo, "README.md")
            test_sig = LibGit2.Signature("TEST", "TEST@TEST.COM", round(time()), 0)
            LibGit2.commit(repo, "testmsg"; author=test_sig, committer=test_sig)
        end

        OldPkg.checkout("Example2", branch_name)
        OldPkg.checkout("Example3", branch_name)

        LibGit2.with(LibGit2.GitRepo, OldPkg.dir("Example2")) do repo
            @test LibGit2.head_oid(repo) == LibGit2.GitHash(branch_commit)
        end
        LibGit2.with(LibGit2.GitRepo, OldPkg.dir("Example3")) do repo
            @test LibGit2.head_oid(repo) == LibGit2.GitHash(branch_commit)
        end
    end

    # 17364 - b, remote off-tree branch
    let branch_name = "test-branch-2",
        branch_commit = "ba3888212e30a7974ac6803a89e64c7098f4865e"

        # create a branch in Example package
        LibGit2.with(LibGit2.GitRepo, OldPkg.dir("Example")) do repo
            LibGit2.branch!(repo, branch_name, branch_commit, set_head=true)
        end

        # Make changes to local branch
        open(OldPkg.dir("Example", "README.md"), "w") do f
            println(f, "overwritten")
        end

        test_commit = LibGit2.with(LibGit2.GitRepo, OldPkg.dir("Example")) do repo
            LibGit2.add!(repo, "README.md")
            test_sig = LibGit2.Signature("TEST", "TEST@TEST.COM", round(time()), 0)
            LibGit2.commit(repo, "testmsg"; author=test_sig, committer=test_sig)
        end
        OldPkg.checkout("Example")

        OldPkg.clone(OldPkg.dir("Example"), "Example4")
        OldPkg.checkout("Example4", branch_name)

        LibGit2.with(LibGit2.GitRepo, OldPkg.dir("Example4")) do repo
            @test LibGit2.head_oid(repo) == test_commit
        end
    end

    # adding a package with unsatisfiable julia version requirements (REPL.jl) errors
    try
        OldPkg.add("REPL")
        error("unexpected")
    catch err
        @test isa(err.exceptions[1].ex, PkgError)
        @test err.exceptions[1].ex.msg == "REPL can't be installed because " *
            "it has no versions that support $VERSION of julia. You may " *
            "need to update METADATA by running `OldPkg.update()`"
    end

    # trying to add, check availability, or pin a nonexistent package errors
    try
        OldPkg.add("NonexistentPackage")
        error("unexpected")
    catch err
        @test isa(err.exceptions[1].ex, PkgError)
        @test err.exceptions[1].ex.msg == "unknown package NonexistentPackage"
    end
    try
        OldPkg.available("NonexistentPackage")
        error("unexpected")
    catch err
        @test isa(err, PkgError)
        @test err.msg == "NonexistentPackage is not a package (not registered or installed)"
    end
    try
        OldPkg.pin("NonexistentPackage", v"1.0.0")
        error("unexpected")
    catch err
        @test isa(err, PkgError)
        @test err.msg == "NonexistentPackage is not a git repo"
    end

    # trying to pin a git repo under OldPkg.dir that is not an installed package errors
    try
        OldPkg.pin("METADATA", v"1.0.0")
        error("unexpected")
    catch err
        @test isa(err, PkgError)
        @test err.msg == "METADATA cannot be pinned – not an installed package"
    end

    # trying to pin an installed, registered package to an unregistered version errors
    try
        OldPkg.pin("Example", v"2147483647.0.0")
        error("unexpected")
    catch err
        @test isa(err, PkgError)
        @test err.msg == "Example – 2147483647.0.0 is not a registered version"
    end

    # PR #13572, handling of versions with untagged detached heads
    LibGit2.with(LibGit2.GitRepo, OldPkg.dir("Example")) do repo
        LibGit2.checkout!(repo, "72f09c7d0099793378c645929a9961155faae6d2")
    end
    @test OldPkg.installed()["Example"] > v"0.0.0"

    # issue #13583
    begin
        try
            OldPkg.test("IDoNotExist")
            error("unexpected")
        catch ex
            @test isa(ex,OldPkg.Entry.PkgTestError)
            @test ex.msg == "IDoNotExist is not an installed package"
        end

        try
            OldPkg.test("IDoNotExist1", "IDoNotExist2")
            error("unexpected")
        catch ex
            @test isa(ex,OldPkg.Entry.PkgTestError)
            @test ex.msg == "IDoNotExist1 and IDoNotExist2 are not installed packages"
        end
    end

    # Various pin/free/re-pin/change-pin patterns (issue #17176)
    @test "" == capture_stdout() do
        nothingtodomsg = (:info,"No packages to install, update or remove")

        @test_logs((:info,"Freeing Example"),
                   nothingtodomsg,
                   OldPkg.free("Example"))

        @test_logs (:info,r"^Creating Example branch pinned\.[0-9a-f]{8}\.tmp$") OldPkg.pin("Example")
        vers = OldPkg.installed("Example")
        branch = LibGit2.with(LibGit2.GitRepo, OldPkg.dir("Example")) do repo
            LibGit2.branch(repo)
        end

        @test_logs((:info,"Freeing Example"),
                   nothingtodomsg,
                   OldPkg.free("Example"))

        @test_logs((:info,"Creating Example branch pinned.b1990792.tmp"),
                   nothingtodomsg, OldPkg.pin("Example", v"0.4.0"))
        @test OldPkg.installed("Example") == v"0.4.0"

        @test_logs (:info,r"^Package Example is already pinned to the selected commit$") OldPkg.pin("Example", v"0.4.0")
        @test OldPkg.installed("Example") == v"0.4.0"

        @test_logs (:info,r"^Package Example is already pinned$") OldPkg.pin("Example")
        @test OldPkg.installed("Example") == v"0.4.0"

        @test_logs (:info,"Package Example: skipping update (pinned)...") match_mode=:any OldPkg.update()
        @test OldPkg.installed("Example") == v"0.4.0"

        @test_logs((:info,"Creating Example branch pinned.d1ef7b00.tmp"),
                   nothingtodomsg, OldPkg.pin("Example", v"0.3.1"))
        @test OldPkg.installed("Example") == v"0.3.1"

        @test_logs((:info,"Package Example: checking out existing branch pinned.b1990792.tmp"),
                   nothingtodomsg, OldPkg.pin("Example", v"0.4.0"))
        @test OldPkg.installed("Example") == v"0.4.0"

        @test_logs((:info,"Freeing Example"),
                   nothingtodomsg, OldPkg.free("Example"))
        @test OldPkg.installed("Example") == vers

        @test_logs (:info,Regex("^Package Example: checking out existing branch $branch\$")) OldPkg.pin("Example")
        @test OldPkg.installed("Example") == vers

        @test_logs((:info,"Freeing Example"),
                   nothingtodomsg, OldPkg.free("Example"))
        @test OldPkg.installed("Example") == vers
    end

    begin
        # bug identified in #16850, Base.url \ vs / for non-Base methods
        include(OldPkg.dir("Example","src","Example.jl"))
        meth = first(methods(Example.domath))
        fname = string(meth.file)
        @test ('\\' in fname) == Sys.iswindows()
        @test startswith(Base.url(meth), "https://github.com/JuliaLang/Example.jl/tree")
    end

    # add a directory that is not a git repository
    begin
        mkdir(joinpath(OldPkg.dir(), "NOTGIT"))
        OldPkg.installed("NOTGIT") == typemin(VersionNumber)
        OldPkg.installed()["NOTGIT"] == typemin(VersionNumber)
    end

    begin
        # don't bork when a Pkg repo is bare, issue #13804
        pth = joinpath(OldPkg.dir(), "BAREGIT")
        mkdir(pth)
        # create a bare repo (isbare = true)
        repo = LibGit2.init(pth, true)
        @test repo.ptr != C_NULL
        close(repo)
        OldPkg.update()
    end

    #test PkgDev redirects
    begin
        try
            OldPkg.register("IDoNotExist")
            error("unexpected")
        catch ex
            @test ex.msg == "OldPkg.register(pkg,[url]) has been moved to the package PkgDev.jl.\nRun OldPkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-"
        end

        try
            OldPkg.tag("IDoNotExist")
            error("unexpected")
        catch ex
            @test ex.msg == "OldPkg.tag(pkg, [ver, [commit]]) has been moved to the package PkgDev.jl.\nRun OldPkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-"
        end

        try
            OldPkg.generate("IDoNotExist","MIT")
            error("unexpected")
        catch ex
            @test ex.msg == "OldPkg.generate(pkg, license) has been moved to the package PkgDev.jl.\nRun OldPkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-"
        end

        try
            OldPkg.publish()
            error("unexpected")
        catch ex
            @test ex.msg == "OldPkg.publish() has been moved to the package PkgDev.jl.\nRun OldPkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-"
        end

        try
            OldPkg.license()
            error("unexpected")
        catch ex
            @test ex.msg == "OldPkg.license([lic]) has been moved to the package PkgDev.jl.\nRun OldPkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-"
        end
        try
            OldPkg.submit("IDoNotExist")
            error("unexpected")
        catch ex
            @test ex.msg == "OldPkg.submit(pkg[, commit]) has been moved to the package PkgDev.jl.\nRun OldPkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-"
        end
        try
            OldPkg.submit("IDoNotExist", "nonexistentcommit")
            error("unexpected")
        catch ex
            @test ex.msg == "OldPkg.submit(pkg[, commit]) has been moved to the package PkgDev.jl.\nRun OldPkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-"
        end
    end

    # Test OldPkg.Read.url works
    @test OldPkg.Read.url("Example") == "git://github.com/JuliaLang/Example.jl.git"

    # issue #15789, build failure warning are printed correctly.
    # Also makes sure `OldPkg.build()` works even for non-git repo
    begin
        pth = joinpath(OldPkg.dir(), "BuildFail")
        mkdir(pth)
        depspath = joinpath(pth, "deps")
        mkdir(depspath)
        depsbuild = joinpath(depspath, "build.jl")
        touch(depsbuild)
        # OldPkg.build works without the src directory now
        # but it's probably fine to require it.
        msg = read(`$(Base.julia_cmd()) --startup-file=no -e 'redirect_stderr(stdout); using Logging; global_logger(SimpleLogger(stdout)); import OldPkg; OldPkg.build("BuildFail")'`, String)
        @test occursin("Building BuildFail", msg)
        @test !occursin("Build failed for BuildFail", msg)
        open(depsbuild, "w") do fd
            println(fd, "error(\"Throw build error\")")
        end
        msg = read(`$(Base.julia_cmd()) --startup-file=no -e 'redirect_stderr(stdout); using Logging; global_logger(SimpleLogger(stdout)); import OldPkg; OldPkg.build("BuildFail")'`, String)
        @test occursin("Building BuildFail", msg)
        @test occursin("Build failed for BuildFail", msg)
        @test occursin("OldPkg.build(\"BuildFail\")", msg)
        @test occursin("Throw build error", msg)
    end

    # issue #15948
    let package = "Example"
        OldPkg.rm(package)  # Remove package if installed
        @test OldPkg.installed(package) === nothing  # Registered with METADATA but not installed
        msg = read(ignorestatus(`$(Base.julia_cmd()) --startup-file=no -e "redirect_stderr(stdout); using Logging; global_logger(SimpleLogger(stdout)); import OldPkg; OldPkg.build(\"$package\")"`), String)
        @test occursin("$package is not an installed package", msg)
        @test !occursin("signal (15)", msg)
    end

    # issue #20695
    OldPkg.cd() do
        @test OldPkg.Entry.url_and_pkg("Example") == ("git://github.com/JuliaLang/Example.jl.git", "Example")
        for url = [
            "https://github.com/Org/Nonsense",
            "git@github.com:Org/Nonsense",
            "file:///home/user/Nonsense",
            "/home/user/Nonsense",
        ]
            @test OldPkg.Entry.url_and_pkg(url) == (url, "Nonsense")
            @test OldPkg.Entry.url_and_pkg("$url.jl") == ("$url.jl", "Nonsense")
            @test OldPkg.Entry.url_and_pkg("$url.git") == ("$url.git", "Nonsense")
            @test OldPkg.Entry.url_and_pkg("$url.jl.git") == ("$url.jl.git", "Nonsense")
        end
        pkg = randstring(20)
        @test OldPkg.Entry.url_and_pkg(pkg) == (pkg, pkg)
    end

    # partial OldPkg.update
    @test "" == capture_stdout() do
        nothingtodomsg = (:info,"No packages to install, update or remove")

        @test_logs (:info,r"Installing Example v") match_mode=:any begin
            OldPkg.rm("Example")
            OldPkg.add("Example")
        end

        @test_logs nothingtodomsg match_mode=:any OldPkg.update("Example")

        @test_logs (:info,"Installing Example v0.4.0") match_mode=:any begin
            OldPkg.rm("Example")
            OldPkg.add("Example", v"0", v"0.4.1-") # force version to be < 0.4.1
        end


        @test_logs((:info,r"""Package Example was set to version 0\.4\.0, but a higher version \d+\.\d+\.\d+\S* exists.
                              The update is prevented by explicit requirements constraints. Edit your REQUIRE file to change this."""),
                   nothingtodomsg, match_mode=:any, OldPkg.update("Example"))

        @test_logs (:info,r"Installing Example") match_mode=:any begin
            OldPkg.rm("Example")
            OldPkg.add("Example")
            OldPkg.pin("Example", v"0.4.0")
        end

        @test_logs((:info,"Package Example: skipping update (pinned)..."),
                   (:info,r"""Package Example was set to version 0\.4\.0, but a higher version \d+\.\d+\.\d+\S* exists.
                              The package is fixed. You can try using `OldPkg.free\("Example"\)` to update it."""),
                   nothingtodomsg, match_mode=:any, OldPkg.update("Example"))

        metadata_dir = OldPkg.dir("METADATA")
        old_commit = "313bfaafa301e82d40574a778720e893c559a7e2"

        # Force a METADATA rollback to an old version, so that we will install some
        # outdated versions of some packages and then update some of those
        # (note that the following OldPkg.update calls will update METADATA to the
        # latest version even though they don't update all packages)
        LibGit2.with(LibGit2.GitRepo, metadata_dir) do repo
            LibGit2.reset!(repo, LibGit2.GitHash(old_commit), LibGit2.Consts.RESET_HARD)
        end

        # run these at an old metadata commit where it's guaranteed no
        # packages depend on Example.jl
        @test isempty(OldPkg.dependents("Example"))
        @test isempty(OldPkg.dependents("Example.jl"))

        logs,_ = Test.collect_test_logs() do
            OldPkg.add("Iterators")
            OldPkg.update("Iterators")
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
                   (:info,"Installing Reexport v0.0.3"), match_mode=:any, OldPkg.add("Colors"))

        logs,_ = Test.collect_test_logs() do
            OldPkg.update("ColorTypes")
        end
        @test any(occursin((:info, r"Upgrading ColorTypes: v0\.2\.2 => v\d+\.\d+\.\d+"), l) for l in logs)
        @test any(occursin((:info, r"Upgrading Compat: v0\.7\.18 => v\d+\.\d+\.\d+"), l) for l in logs)
        @test !any(occursin((:info, r"Upgrading Colors"), l) for l in logs)

        @test OldPkg.installed("Colors") == v"0.6.4"

        @test_logs nothingtodomsg match_mode=:any OldPkg.update("FixedPointNumbers")
    end

    # issue #18239
    let package = "Example"
        OldPkg.free(package)
        OldPkg.rm(package)  # Remove package if installed

        metadata_dir = OldPkg.dir("METADATA")
        old_commit = "83ff7116e51fc9cdbd7e67affbd344b9f5c9dbf2"

        # Reset METADATA to the second to last update of Example.jl
        LibGit2.with(LibGit2.GitRepo, metadata_dir) do repo
            LibGit2.reset!(repo, LibGit2.GitHash(old_commit), LibGit2.Consts.RESET_HARD)
        end

        OldPkg.add(package)
        msg = read(ignorestatus(`$(Base.julia_cmd()) --startup-file=no -e
            "import OldPkg; push!(LOAD_PATH, OldPkg.dir); redirect_stderr(stdout); using Logging; global_logger(SimpleLogger(stdout)); using Example; OldPkg.update(\"$package\")"`), String)
        @test occursin(Regex("- $package.*Restart Julia to use the updated versions","s"), msg)
    end

    # Verify that the --startup-file flag is respected by OldPkg.build / OldPkg.test
    let package = "StartupFile"
        content = """
            @info "JULIA_RC_LOADED defined \$(isdefined(@__MODULE__, :JULIA_RC_LOADED))"
            @info "Main.JULIA_RC_LOADED defined \$(isdefined(Main, :JULIA_RC_LOADED))"
            """

        write_build(package, content)

        test_filename = OldPkg.dir(package, "test", "runtests.jl")
        mkpath(dirname(test_filename))
        write(test_filename, content)

        # Make a ~/.julia/config/startup.jl
        home = OldPkg.dir(".home")
        mkpath(joinpath(home, ".julia", "config"))
        write(joinpath(home, ".julia", "config", "startup.jl"),
            "const JULIA_RC_LOADED = true")

        withenv((Sys.iswindows() ? "USERPROFILE" : "HOME") => home) do
            code = "redirect_stderr(stdout); using Logging; global_logger(SimpleLogger(stdout)); import OldPkg; OldPkg.build(\"$package\")"
            msg = read(`$(Base.julia_cmd()) --startup-file=no -e $code`, String)
            @test occursin("JULIA_RC_LOADED defined false", msg)
            @test occursin("Main.JULIA_RC_LOADED defined false", msg)

            msg = read(`$(Base.julia_cmd()) --startup-file=yes -e $code`, String)
            @test occursin("JULIA_RC_LOADED defined false", msg)
            @test occursin("Main.JULIA_RC_LOADED defined true", msg)

            code = "redirect_stderr(stdout); using Logging; global_logger(SimpleLogger(stdout)); import OldPkg; OldPkg.test(\"$package\")"

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
        stdout_file = OldPkg.dir(package, "stdout.txt")
        stderr_file = OldPkg.dir(package, "stderr.txt")
        content = """
            println(stdout, "stdout")
            println(stderr, "stderr")
            """
        write_build(package, content)

        code = "import OldPkg; OldPkg.build(\"$package\")"
        msg = run(pipeline(
            `$(Base.julia_cmd()) --startup-file=no -e $code`,
            stdout=stdout_file, stderr=stderr_file))
        @test last(readlines(stdout_file)) == "stdout"
        @test last(readlines(stderr_file)) == "stderr"
    end
end

@testset "Pkg functions with .jl extension" begin
    temp_pkg_dir() do
        @test OldPkg.installed("Example.jl") === nothing
        OldPkg.add("Example.jl")
        @test [keys(OldPkg.installed())...] == ["Example"]
        iob = IOBuffer()
        OldPkg.update("Example.jl")
        OldPkg.checkout("Example.jl")
        OldPkg.status("Example.jl", iob)
        str = chomp(String(take!(iob)))
        @test startswith(str, " - Example")
        @test endswith(str, "master")
        OldPkg.free("Example.jl")
        OldPkg.status("Example.jl", iob)
        str = chomp(String(take!(iob)))
        @test endswith(str, string(OldPkg.installed("Example.jl")))
        OldPkg.checkout("Example.jl")
        OldPkg.free(("Example.jl",))
        OldPkg.status("Example.jl", iob)
        str = chomp(String(take!(iob)))
        @test endswith(str, string(OldPkg.installed("Example.jl")))
        OldPkg.rm("Example.jl")
        @test isempty(OldPkg.installed())
        @test !isempty(OldPkg.available("Example.jl"))
        @test !in("Example", keys(OldPkg.installed()))
        OldPkg.rm("Example.jl")
        @test isempty(OldPkg.installed())
        @test !isempty(OldPkg.available("Example.jl"))
        @test !in("Example", keys(OldPkg.installed()))
        OldPkg.clone("https://github.com/JuliaLang/Example.jl.git")
        @test [keys(OldPkg.installed())...] == ["Example"]
        OldPkg.status("Example.jl", iob)
        str = chomp(String(take!(iob)))
        @test startswith(str, " - Example")
        @test endswith(str, "master")
        OldPkg.free("Example.jl")
        OldPkg.status("Example.jl", iob)
        str = chomp(String(take!(iob)))
        @test endswith(str, string(OldPkg.installed("Example.jl")))
        OldPkg.checkout("Example.jl")
        OldPkg.free(("Example.jl",))
        OldPkg.status("Example.jl", iob)
        str = chomp(String(take!(iob)))
        @test endswith(str, string(OldPkg.installed("Example.jl")))
    end
end

let io = IOBuffer()
    Base.showerror(io, OldPkg.Entry.PkgTestError("ppp"), backtrace())
    @test !occursin("backtrace()", String(take!(io)))
end

@testset "Relative path operations" begin
    cd(tempdir()) do
        temp_pkg_dir(randstring()) do
            OldPkg.add("Example")
            @test [keys(OldPkg.installed())...] == ["Example"]
        end
    end
end

temp_pkg_dir(initialize=false) do
    write_build("Normal", "")
    write_build("Error", "error(\"An error has occurred while building a package\")")
    write_build("Exit", "exit()")

    cd(OldPkg.dir()) do
        errors = Dict()

        # Log forwarding TODO - port these to @test_logs
        empty!(errors)
        @test_warn ("Building Error",
                    "Building Normal") OldPkg.Entry.build!(["Error", "Normal"], errors)

        empty!(errors)
        @test_warn ("Building Exit",
                    "Building Normal") OldPkg.Entry.build!(["Exit", "Normal"], errors)

        empty!(errors)
        @test_warn ("Building Exit",
                    "Building Normal",
                    "Building Exit",
                    "Building Normal") OldPkg.Entry.build!(["Exit", "Normal", "Exit", "Normal"], errors)
    end
end

# VersionSet tests
import OldPkg.Types: VersionSet, VersionInterval

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
