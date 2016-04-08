# This file is a part of Julia. License is MIT: http://julialang.org/license

import Base.Pkg.PkgError

function temp_pkg_dir(fn::Function, remove_tmp_dir::Bool=true)
    # Used in tests below to setup and teardown a sandboxed package directory
    const tmpdir = ENV["JULIA_PKGDIR"] = joinpath(tempdir(),randstring())
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


# Test basic operations: adding or removing a package, status, free
#Also test for the existence of REQUIRE and META_Branch
temp_pkg_dir() do
    @test isfile(joinpath(Pkg.dir(),"REQUIRE"))
    @test isfile(joinpath(Pkg.dir(),"META_BRANCH"))
    @test isempty(Pkg.installed())
    @test sprint(io -> Pkg.status(io)) == "No packages installed\n"
    @test !isempty(Pkg.available())

    @test_throws PkgError Pkg.installed("MyFakePackage")
    @test Pkg.installed("Example") == nothing

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
    @test Pkg.Cache.rewrite_url_to == nothing
    Pkg.setprotocol!("https")
    Pkg.add("Example")
    @test [keys(Pkg.installed())...] == ["Example"]
    iob = IOBuffer()
    Pkg.checkout("Example")
    Pkg.status("Example", iob)
    str = chomp(takebuf_string(iob))
    @test startswith(str, " - Example")
    @test endswith(str, "master")
    Pkg.free("Example")
    Pkg.status("Example", iob)
    str = chomp(takebuf_string(iob))
    @test endswith(str, string(Pkg.installed("Example")))
    Pkg.checkout("Example")
    Pkg.free(("Example",))
    Pkg.status("Example", iob)
    str = chomp(takebuf_string(iob))
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
    str = chomp(takebuf_string(iob))
    @test startswith(str, " - Example")
    @test endswith(str, "master")
    Pkg.free("Example")
    Pkg.status("Example", iob)
    str = chomp(takebuf_string(iob))
    @test endswith(str, string(Pkg.installed("Example")))
    Pkg.checkout("Example")
    Pkg.free(("Example",))
    Pkg.status("Example", iob)
    str = chomp(takebuf_string(iob))
    @test endswith(str, string(Pkg.installed("Example")))
    @test isempty(Pkg.dependents("Example"))

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

    begin
        Pkg.pin("Example")
        Pkg.free("Example")

        Pkg.pin("Example", v"0.4.0")
        Pkg.update()
        Pkg.installed()["Example"] == v"0.4.0"
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
        msg = readstring(`$(Base.julia_cmd()) -f -e 'redirect_stderr(STDOUT); Pkg.build("BuildFail")'`)
        @test contains(msg, "Building BuildFail")
        @test !contains(msg, "ERROR")
        open(depsbuild, "w") do fd
            println(fd, "error(\"Throw build error\")")
        end
        msg = readstring(`$(Base.julia_cmd()) -f -e 'redirect_stderr(STDOUT); Pkg.build("BuildFail")'`)
        @test contains(msg, "Building BuildFail")
        @test contains(msg, "ERROR")
        @test contains(msg, "Pkg.build(\"BuildFail\")")
        @test contains(msg, "Throw build error")
    end
end
