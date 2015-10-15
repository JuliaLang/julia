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

    Pkg.rm("Example")
    @test isempty(Pkg.installed())

    # issue #13583
    begin
        try
            Pkg.test("IDoNotExist")
        catch ex
            @test isa(ex,Pkg.PkgError)
            @test ex.msg == "IDoNotExist is not an installed package"
        end

        try
            Pkg.test("IDoNotExist1", "IDoNotExist2")
        catch ex
            @test isa(ex,Pkg.PkgError)
            @test ex.msg == "IDoNotExist1 and IDoNotExist2 are not installed packages"
        end
    end
end
