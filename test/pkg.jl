# This file is a part of Julia. License is MIT: http://julialang.org/license

import Base.Pkg.PkgError

function temp_pkg_dir(fn::Function, remove_tmp_dir::Bool=true)
    # Used in tests below to set up and tear down a sandboxed package directory
    const tmpdir = joinpath(tempdir(),randstring())
    withenv("JULIA_PKGDIR" => tmpdir) do
        @test !isdir(Pkg.dir())
        try
            Pkg.init()
            @test isdir(Pkg.dir())
            Pkg.resolve()
            fn()
        finally
            remove_tmp_dir && rm(tmpdir, recursive=true)
        end
    end
end

macro grab_outputs(ex)
    quote
        local err::String, out::String
        local ferrname = ""
        local foutname = ""
        local ret
        try
            OLD_STDERR = STDERR
            ferrname = tempname()
            ferr = open(ferrname, "w")
            try
                OLD_STDOUT = STDOUT
                foutname = tempname()
                fout = open(foutname, "w")
                try
                    redirect_stderr(ferr)
                    try
                        redirect_stdout(fout)
                        ret = $(esc(ex))
                    finally
                        redirect_stdout(OLD_STDOUT)
                        close(fout)
                    end
                finally
                    redirect_stderr(OLD_STDERR)
                    close(ferr)
                end
                out = readstring(foutname)
                err = readstring(ferrname)
            finally
                isfile(foutname) && rm(foutname)
            end
        finally
            isfile(ferrname) && rm(ferrname)
        end
        ret, out, err
    end
end

# Test basic operations: adding or removing a package, status, free
# Also test for the existence of REQUIRE and META_BRANCH
temp_pkg_dir() do
    @test isfile(joinpath(Pkg.dir(),"REQUIRE"))
    @test isfile(joinpath(Pkg.dir(),"META_BRANCH"))
    @test isempty(Pkg.installed())
    @test sprint(io -> Pkg.status(io)) == "No packages installed\n"
    @test !isempty(Pkg.available())

    # 18325
    cd(Pkg.dir()) do
        avail = Pkg.Read.available()
        avail_copy = Pkg.Query.availcopy(avail);
        delete!(avail_copy["Example"][v"0.0.1"].requires, "julia")
        @test haskey(avail["Example"][v"0.0.1"].requires, "julia")
    end

    @test_throws PkgError Pkg.installed("MyFakePackage")
    @test Pkg.installed("Example") === nothing

    # check that versioninfo(io, true) doesn't error and produces some output
    # (done here since it calls Pkg.status which might error or clone metadata)
    buf = PipeBuffer()
    versioninfo(buf, true)
    ver = readstring(buf)
    @test startswith(ver, "Julia Version $VERSION")
    @test contains(ver, "Environment:")

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
            @test contains(ex.msg, "Cannot clone Example from notarealprotocol://github.com/JuliaLang/Example.jl.git")
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
    @test isempty(Pkg.dependents("Example"))

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
            @test LibGit2.head_oid(repo) == LibGit2.Oid(branch_commit)
        end
        LibGit2.with(LibGit2.GitRepo, Pkg.dir("Example3")) do repo
            @test LibGit2.head_oid(repo) == LibGit2.Oid(branch_commit)
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
            @test isa(ex,Pkg.PkgError)
            @test ex.msg == "IDoNotExist is not an installed package"
        end

        try
            Pkg.test("IDoNotExist1", "IDoNotExist2")
            error("unexpected")
        catch ex
            @test isa(ex,Pkg.PkgError)
            @test ex.msg == "IDoNotExist1 and IDoNotExist2 are not installed packages"
        end
    end

    # Various pin/free/re-pin/change-pin patterns (issue #17176)
    begin
        ret, out, err = @grab_outputs Pkg.free("Example")
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Freeing Example")

        ret, out, err = @grab_outputs Pkg.pin("Example")
        @test ret === nothing && out == ""
        @test ismatch(r"INFO: Creating Example branch pinned\.[0-9a-f]{8}\.tmp", err)
        @test !contains(err, "INFO: No packages to install, update or remove")
        branchid = replace(err, r".*pinned\.([0-9a-f]{8})\.tmp.*"s, s"\1")
        vers = Pkg.installed("Example")

        ret, out, err = @grab_outputs Pkg.free("Example")
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Freeing Example")

        ret, out, err = @grab_outputs Pkg.pin("Example", v"0.4.0")
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Creating Example branch pinned.b1990792.tmp")
        @test contains(err, "INFO: No packages to install, update or remove")
        @test Pkg.installed("Example") == v"0.4.0"

        ret, out, err = @grab_outputs Pkg.pin("Example", v"0.4.0")
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Package Example is already pinned to the selected commit")
        @test !contains(err, "INFO: No packages to install, update or remove")
        @test Pkg.installed("Example") == v"0.4.0"

        ret, out, err = @grab_outputs Pkg.pin("Example")
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Package Example is already pinned")
        @test !contains(err, "INFO: No packages to install, update or remove")
        @test Pkg.installed("Example") == v"0.4.0"

        ret, out, err = @grab_outputs Pkg.update()
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Package Example: skipping update (pinned)...")
        @test Pkg.installed("Example") == v"0.4.0"

        ret, out, err = @grab_outputs Pkg.pin("Example", v"0.3.1")
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Creating Example branch pinned.d1ef7b00.tmp")
        @test contains(err, "INFO: No packages to install, update or remove")
        @test Pkg.installed("Example") == v"0.3.1"

        ret, out, err = @grab_outputs Pkg.pin("Example", v"0.4.0")
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Package Example: checking out existing branch pinned.b1990792.tmp")
        @test contains(err, "INFO: No packages to install, update or remove")
        @test Pkg.installed("Example") == v"0.4.0"

        ret, out, err = @grab_outputs Pkg.free("Example")
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Freeing Example")
        @test contains(err, "INFO: No packages to install, update or remove")
        @test Pkg.installed("Example") == vers

        ret, out, err = @grab_outputs Pkg.pin("Example")
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Package Example: checking out existing branch pinned.$branchid.tmp")
        @test !contains(err, "INFO: No packages to install, update or remove")
        @test Pkg.installed("Example") == vers

        ret, out, err = @grab_outputs Pkg.free("Example")
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Freeing Example")
        @test Pkg.installed("Example") == vers
    end

    begin
        # bug identified in #16850, Base.url \ vs / for non-Base methods
        include(Pkg.dir("Example","src","Example.jl"))
        meth = first(methods(Example.domath))
        fname = string(meth.file)
        @test ('\\' in fname) == is_windows()
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
        finalize(repo)
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
        msg = readstring(`$(Base.julia_cmd()) --startup-file=no -e 'redirect_stderr(STDOUT); Pkg.build("BuildFail")'`)
        @test contains(msg, "Building BuildFail")
        @test !contains(msg, "ERROR")
        open(depsbuild, "w") do fd
            println(fd, "error(\"Throw build error\")")
        end
        msg = readstring(`$(Base.julia_cmd()) --startup-file=no -e 'redirect_stderr(STDOUT); Pkg.build("BuildFail")'`)
        @test contains(msg, "Building BuildFail")
        @test contains(msg, "ERROR")
        @test contains(msg, "Pkg.build(\"BuildFail\")")
        @test contains(msg, "Throw build error")
    end

    # issue #15948
    let package = "Example"
        Pkg.rm(package)  # Remove package if installed
        @test Pkg.installed(package) === nothing  # Registered with METADATA but not installed
        msg = readstring(ignorestatus(`$(Base.julia_cmd()) --startup-file=no -e "redirect_stderr(STDOUT); Pkg.build(\"$package\")"`))
        @test contains(msg, "$package is not an installed package")
        @test !contains(msg, "signal (15)")
    end

    # partial Pkg.update
    begin
        nothingtodomsg = "INFO: No packages to install, update or remove\n"

        ret, out, err = @grab_outputs begin
            Pkg.rm("Example")
            Pkg.add("Example")
        end
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Installing Example v")

        ret, out, err = @grab_outputs Pkg.update("Example")
        @test ret === nothing && out == ""
        @test contains(err, nothingtodomsg)

        ret, out, err = @grab_outputs begin
            Pkg.rm("Example")
            Pkg.add("Example", v"0", v"0.4.1-") # force version to be < 0.4.1
        end
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Installing Example v0.4.0")

        ret, out, err = @grab_outputs Pkg.update("Example")
        @test ret === nothing && out == ""
        @test ismatch(r"INFO: Package Example was set to version 0\.4\.0, but a higher version \d+\.\d+\.\d+\S* exists.\n", err)
        @test contains(err, "The update is prevented by explicit requirements constraints. Edit your REQUIRE file to change this.\n")
        @test contains(err, nothingtodomsg)

        ret, out, err = @grab_outputs begin
            Pkg.rm("Example")
            Pkg.add("Example")
            Pkg.pin("Example", v"0.4.0")
        end
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Installing Example")

        ret, out, err = @grab_outputs Pkg.update("Example")
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Package Example: skipping update (pinned)...\n")
        @test ismatch(r"INFO: Package Example was set to version 0\.4\.0, but a higher version \d+\.\d+\.\d+\S* exists.\n", err)
        @test contains(err, "The package is fixed. You can try using `Pkg.free(\"Example\")` to update it.")
        @test contains(err, nothingtodomsg)

        metadata_dir = Pkg.dir("METADATA")
        const old_commit = "313bfaafa301e82d40574a778720e893c559a7e2"

        # Force a METADATA rollback to an old version, so that we will install some
        # outdated versions of some packages and then update some of those
        # (note that the following Pkg.update calls will update METADATA to the
        # latest version even though they don't update all packages)
        LibGit2.with(LibGit2.GitRepo, metadata_dir) do repo
            LibGit2.reset!(repo, LibGit2.Oid(old_commit), LibGit2.Consts.RESET_HARD)
        end

        ret, out, err = @grab_outputs Pkg.add("Colors")
        @test ret === nothing && out == ""
        @test contains(err, "INFO: Installing Colors v0.6.4")
        @test contains(err, "INFO: Installing ColorTypes v0.2.2")
        @test contains(err, "INFO: Installing FixedPointNumbers v0.1.3")
        @test contains(err, "INFO: Installing Compat v0.7.18")
        @test contains(err, "INFO: Installing Reexport v0.0.3")

        ret, out, err = @grab_outputs Pkg.update("ColorTypes")
        @test ismatch(r"INFO: Upgrading ColorTypes: v0\.2\.2 => v\d+\.\d+\.\d+", err)
        @test ismatch(r"INFO: Upgrading Compat: v0\.7\.18 => v\d+\.\d+\.\d+", err)
        @test !contains(err, "INFO: Upgrading Colors: ")
        @test Pkg.installed("Colors") == v"0.6.4"

        ret, out, err = @grab_outputs Pkg.update("FixedPointNumbers")
        @test ret === nothing && out == ""
        @test contains(err, nothingtodomsg)
    end

    # issue #18239
    let package = "Example"
        Pkg.free(package)
        Pkg.rm(package)  # Remove package if installed

        metadata_dir = Pkg.dir("METADATA")
        const old_commit = "83ff7116e51fc9cdbd7e67affbd344b9f5c9dbf2"

        # Reset METADATA to the second to last update of Example.jl
        LibGit2.with(LibGit2.GitRepo, metadata_dir) do repo
            LibGit2.reset!(repo, LibGit2.Oid(old_commit), LibGit2.Consts.RESET_HARD)
        end

        Pkg.add(package)
        msg = readstring(ignorestatus(`$(Base.julia_cmd()) --startup-file=no -e
            "redirect_stderr(STDOUT); using Example; Pkg.update(\"$package\")"`))
        @test contains(msg, "- $package\nRestart Julia to use the updated versions.")
    end
end
