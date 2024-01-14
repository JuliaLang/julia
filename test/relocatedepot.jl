using Test
using Logging


include("testenv.jl")


function test_harness(@nospecialize(fn); empty_load_path=true, empty_depot_path=true)
    load_path = copy(LOAD_PATH)
    depot_path = copy(DEPOT_PATH)
    try
        empty_load_path && empty!(LOAD_PATH)
        empty_depot_path && empty!(DEPOT_PATH)
        fn()
    finally
        copy!(LOAD_PATH, load_path)
        copy!(DEPOT_PATH, depot_path)
    end
end


if !test_relocated_depot

    @testset "insert @depot tag in path" begin

        test_harness() do
            mktempdir() do dir
                pushfirst!(DEPOT_PATH, dir)
                path = dir*dir
                @test Base.replace_depot_path(path) == "@depot"*dir
            end
        end

        test_harness() do
            mktempdir() do dir
                pushfirst!(DEPOT_PATH, dir)
                path = joinpath(dir, "foo")
                if isdirpath(DEPOT_PATH[1])
                    DEPOT_PATH[1] = dirname(DEPOT_PATH[1]) # strip trailing pathsep
                end
                tag = joinpath("@depot", "") # append a pathsep
                @test startswith(Base.replace_depot_path(path), tag)
                DEPOT_PATH[1] = joinpath(DEPOT_PATH[1], "") # append a pathsep
                @test startswith(Base.replace_depot_path(path), tag)
                popfirst!(DEPOT_PATH)
                @test !startswith(Base.replace_depot_path(path), tag)
            end
        end

    end

    @testset "restore path from @depot tag" begin

        tmp = tempdir()

        path = joinpath("@depot", "foo", "bar")
        tmppath = joinpath(tmp, "foo", "bar")
        @test Base.restore_depot_path(path, tmp) == tmppath

        path = joinpath("no@depot", "foo", "bar")
        @test Base.restore_depot_path(path, tmp) == path

        path = joinpath("@depot", "foo", "bar\n", "@depot", "foo")
        tmppath = joinpath(tmp, "foo", "bar\n", "@depot", "foo")
        @test Base.restore_depot_path(path, tmp) == tmppath

    end

    @testset "precompile RelocationTestPkg1" begin
        pkgname = "RelocationTestPkg1"
        test_harness(empty_depot_path=false) do
            push!(LOAD_PATH, @__DIR__)
            push!(DEPOT_PATH, @__DIR__) # required to make relocatable, but cache is written to DEPOT_PATH[1]
            pkg = Base.identify_package(pkgname)
            cachefiles = Base.find_all_in_cache_path(pkg)
            rm.(cachefiles, force=true)
            @test Base.isprecompiled(pkg) == false
            Base.require(pkg) # precompile
            @test Base.isprecompiled(pkg, ignore_loaded=true) == true
        end
    end

    @testset "precompile RelocationTestPkg2 (contains include_dependency)" begin
        pkgname = "RelocationTestPkg2"
        test_harness(empty_depot_path=false) do
            push!(LOAD_PATH, @__DIR__)
            push!(DEPOT_PATH, @__DIR__) # required to make relocatable, but cache is written to DEPOT_PATH[1]
            pkg = Base.identify_package(pkgname)
            cachefiles = Base.find_all_in_cache_path(pkg)
            rm.(cachefiles, force=true)
            @test Base.isprecompiled(pkg) == false
            touch(joinpath(@__DIR__, pkgname, "src", "foo.txt"))
            Base.require(pkg) # precompile
            @test Base.isprecompiled(pkg, ignore_loaded=true) == true
        end
    end

    @testset "#52161" begin
        # Take the src files from two pkgs Example1 and Example2,
        # which are each located in depot1 and depot2, respectively, and
        # add them as include_dependency()s to a new pkg Foo, which will be precompiled into depot3.
        # After loading the include_dependency()s of Foo should refer to depot1 depot2 each.
        test_harness() do
            mktempdir() do depot1
                # precompile Example in depot1
                example1_root = joinpath(depot1, "Example1")
                mkpath(joinpath(example1_root, "src"))
                open(joinpath(example1_root, "src", "Example1.jl"); write=true) do io
                    println(io, """
                    module Example1
                    greet() = println("Hello from Example1!")
                    end
                    """)
                end
                open(joinpath(example1_root, "Project.toml"); write=true) do io
                    println(io, """
                    name = "Example1"
                    uuid = "00000000-0000-0000-0000-000000000001"
                    version = "1.0.0"
                    """)
                end
                pushfirst!(LOAD_PATH, depot1); pushfirst!(DEPOT_PATH, depot1)
                pkg = Base.identify_package("Example1"); Base.require(pkg)
                mktempdir() do depot2
                    # precompile Example in depot2
                    example2_root = joinpath(depot2, "Example2")
                    mkpath(joinpath(example2_root, "src"))
                    open(joinpath(example2_root, "src", "Example2.jl"); write=true) do io
                        println(io, """
                        module Example2
                        greet() = println("Hello from Example2!")
                        end
                        """)
                    end
                    open(joinpath(example2_root, "Project.toml"); write=true) do io
                        println(io, """
                        name = "Example2"
                        uuid = "00000000-0000-0000-0000-000000000002"
                        version = "1.0.0"
                        """)
                    end
                    pushfirst!(LOAD_PATH, depot2)
                    pushfirst!(DEPOT_PATH, depot2)
                    pkg = Base.identify_package("Example2")
                    Base.require(pkg)
                    mktempdir() do depot3
                        # precompile Foo in depot3
                        open(joinpath(depot3, "Foo.jl"), write=true) do io
                            println(io, """
                            module Foo
                            using Example1
                            using Example2
                            srcfile1 = joinpath(pkgdir(Example1), "src", "Example1.jl")
                            srcfile2 = joinpath(pkgdir(Example2), "src", "Example2.jl")
                            @show srcfile1
                            @show srcfile2
                            include_dependency(srcfile1)
                            include_dependency(srcfile2)
                            end
                            """)
                        end
                        pushfirst!(LOAD_PATH, depot3)
                        pushfirst!(DEPOT_PATH, depot3)
                        pkg = Base.identify_package("Foo")
                        Base.require(pkg)
                        cachefile = joinpath(depot3, "compiled",
                                             "v$(VERSION.major).$(VERSION.minor)", "Foo.ji")
                        _, (deps, _, _), _... = Base.parse_cache_header(cachefile)
                        @test map(x -> x.filename, deps) ==
                            [ joinpath(depot3, "Foo.jl"),
                              joinpath(depot1, "Example1", "src", "Example1.jl"),
                              joinpath(depot2, "Example2", "src", "Example2.jl") ]
                    end
                end
            end
        end
    end


else

    @testset "load stdlib from test/relocatedepot" begin
        test_harness() do
            push!(LOAD_PATH, "@stdlib")
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot", "julia"))
            # stdlib should be already precompiled
            pkg = Base.identify_package("DelimitedFiles")
            @test Base.isprecompiled(pkg) == true
        end
    end

    @testset "load RelocationTestPkg1 from test/relocatedepot" begin
        pkgname = "RelocationTestPkg1"
        test_harness() do
            push!(LOAD_PATH, joinpath(@__DIR__, "relocatedepot"))
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot")) # required to find src files
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot", "julia")) # contains cache file
            pkg = Base.identify_package(pkgname)
            @test Base.isprecompiled(pkg) == true
            Base.require(pkg) # re-precompile
            @test Base.isprecompiled(pkg) == true
        end
    end

    @testset "load RelocationTestPkg2 (contains include_dependency) from test/relocatedepot" begin
        pkgname = "RelocationTestPkg2"
        test_harness() do
            push!(LOAD_PATH, joinpath(@__DIR__, "relocatedepot"))
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot")) # required to find src files
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot", "julia")) # contains cache file
            pkg = Base.identify_package(pkgname)
            @test Base.isprecompiled(pkg) == false # moving depot changes mtime of include_dependency
            Base.require(pkg) # re-precompile
            @test Base.isprecompiled(pkg) == true
        end
    end

end
