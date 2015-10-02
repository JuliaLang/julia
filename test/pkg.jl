# This file is a part of Julia. License is MIT: http://julialang.org/license

function temp_pkg_dir(fn::Function)
    # Used in tests below to setup and teardown a sandboxed package directory
    const tmpdir = ENV["JULIA_PKGDIR"] = joinpath(tempdir(),randstring())
    @test !isdir(Pkg.dir())
    try
        Pkg.init()
        @test isdir(Pkg.dir())
        Pkg.resolve()

        fn()
    finally
        rm(tmpdir, recursive=true)
    end
end

# Test adding a removing a package
temp_pkg_dir() do
    @test isempty(Pkg.installed())
    Pkg.add("Example")
    @test [keys(Pkg.installed())...] == ["Example"]
    Pkg.rm("Example")
    @test isempty(Pkg.installed())

    # adding a package with unsatisfiable julia version requirements (REPL.jl) errors
    try
        Pkg.add("REPL")
        error("unexpected")
    catch err
        @test err.msg == "REPL can't be installed because " *
            "it has no versions that support $VERSION of julia. You may " *
            "need to update METADATA by running `Pkg.update()`"
    end

    # trying to add, check availability, or pin a nonexistent package errors
    try
        Pkg.add("NonexistentPackage")
        error("unexpected")
    catch err
        @test err.msg == "unknown package NonexistentPackage"
    end
    try
        Pkg.available("NonexistentPackage")
        error("unexpected")
    catch err
        @test err.msg == "NonexistentPackage is not a package (not registered or installed)"
    end
    try
        Pkg.pin("NonexistentPackage", v"1.0.0")
        error("unexpected")
    catch err
        @test err.msg == "NonexistentPackage is not a git repo"
    end
end

# testing a package with test dependencies causes them to be installed for the duration of the test
temp_pkg_dir() do
    Pkg.generate("PackageWithTestDependencies", "MIT", config=["user.name"=>"Julia Test", "user.email"=>"test@julialang.org"])
    @test [keys(Pkg.installed())...] == ["PackageWithTestDependencies"]

    isdir(Pkg.dir("PackageWithTestDependencies","test")) || mkdir(Pkg.dir("PackageWithTestDependencies","test"))
    open(Pkg.dir("PackageWithTestDependencies","test","REQUIRE"),"w") do f
        println(f,"Example")
    end

    open(Pkg.dir("PackageWithTestDependencies","test","runtests.jl"),"w") do f
        println(f,"using Base.Test")
        println(f,"@test haskey(Pkg.installed(), \"Example\")")
    end

    Pkg.resolve()
    @test [keys(Pkg.installed())...] == ["PackageWithTestDependencies"]

    Pkg.test("PackageWithTestDependencies")

    @test [keys(Pkg.installed())...] == ["PackageWithTestDependencies"]
end

# testing a package with no runtests.jl errors
temp_pkg_dir() do
    Pkg.generate("PackageWithNoTests", "MIT", config=["user.name"=>"Julia Test", "user.email"=>"test@julialang.org"])

    if isfile(Pkg.dir("PackageWithNoTests", "test", "runtests.jl"))
        rm(Pkg.dir("PackageWithNoTests", "test", "runtests.jl"))
    end

    try
        Pkg.test("PackageWithNoTests")
        error("unexpected")
    catch err
        @test err.msg == "PackageWithNoTests did not provide a test/runtests.jl file"
    end
end

# testing a package with failing tests errors
temp_pkg_dir() do
    Pkg.generate("PackageWithFailingTests", "MIT", config=["user.name"=>"Julia Test", "user.email"=>"test@julialang.org"])

    isdir(Pkg.dir("PackageWithFailingTests","test")) || mkdir(Pkg.dir("PackageWithFailingTests","test"))
    open(Pkg.dir("PackageWithFailingTests", "test", "runtests.jl"),"w") do f
        println(f,"using Base.Test")
        println(f,"@test false")
    end

    try
        Pkg.test("PackageWithFailingTests")
        error("unexpected")
    catch err
        @test err.msg == "PackageWithFailingTests had test errors"
    end
end

# issue #13374
temp_pkg_dir() do
    Pkg.generate("Foo", "MIT")
    Pkg.tag("Foo")
    Pkg.tag("Foo")
end
