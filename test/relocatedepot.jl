using Test
using Logging


include("testenv.jl")


function test_harness(@nospecialize(fn))
    load_path = copy(LOAD_PATH)
    depot_path = copy(DEPOT_PATH)
    try
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
        test_harness() do
            push!(LOAD_PATH, @__DIR__)
            push!(DEPOT_PATH, @__DIR__)
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
        test_harness() do
            push!(LOAD_PATH, @__DIR__)
            push!(DEPOT_PATH, string(@__DIR__, "/"))
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
        # Take the src files from two RelocationTestPkg1 and RelocationTestPkg2,
        # which are each located in depot1 and depot2, respectively, and
        # add them as include_dependency()s to a new pkg Foo, which will be precompiled into depot3.
        # After loading the include_dependency()s of Foo should refer to depot1 depot2 each.
        test_harness() do
            mktempdir() do depot1
                # precompile RelocationTestPkg1 in depot1
                cp(joinpath(@__DIR__,"RelocationTestPkg1"),joinpath(depot1,"RelocationTestPkg1"))
                pushfirst!(LOAD_PATH, depot1)
                pushfirst!(DEPOT_PATH, depot1)
                pkg = Base.identify_package("RelocationTestPkg1")
                Base.require(pkg)
                mktempdir() do depot2
                    cp(joinpath(@__DIR__,"RelocationTestPkg2"),joinpath(depot2,"RelocationTestPkg2"))
                    # precompile RelocationTestPkg2 in depot2
                    pushfirst!(LOAD_PATH, depot2)
                    pushfirst!(DEPOT_PATH, depot2)
                    pkg = Base.identify_package("RelocationTestPkg2")
                    Base.require(pkg)
                    # precompile Foo into in depot3
                    mktempdir() do depot3
                        pushfirst!(LOAD_PATH, depot3)
                        pushfirst!(DEPOT_PATH, depot3)
                        foofile = joinpath(depot3, "Foo.jl")
                        write(foofile, """
                            module Foo
                            using RelocationTestPkg1
                            using RelocationTestPkg2
                            srcfile1 = joinpath(pkgdir(RelocationTestPkg1), "src", "RelocationTestPkg1.jl")
                            srcfile2 = joinpath(pkgdir(RelocationTestPkg2), "src", "RelocationTestPkg2.jl")
                            @show srcfile1
                            @show srcfile2
                            include_dependency(srcfile1)
                            include_dependency(srcfile2)
                            end
                        """)
                        pkg = Base.identify_package("Foo")
                        Base.require(pkg)
                        cachefile = joinpath(depot3, "compiled", "v1.11", "Foo.ji")
                        _, (deps, _, _), _... = Base.parse_cache_header(cachefile)
                        @test map(x -> x.filename, deps) ==
                            [ joinpath(depot3, "Foo.jl"),
                              joinpath(depot1, "RelocationTestPkg1", "src", "RelocationTestPkg1.jl"),
                              joinpath(depot2, "RelocationTestPkg2", "src", "RelocationTestPkg2.jl") ]
                    end
                end
            end
        end
    end


else

    @testset "load stdlib from test/relocatedepot" begin
        test_harness() do
            push!(LOAD_PATH, joinpath(@__DIR__, "relocatedepot"))
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot"))
            # stdlib should be already precompiled
            pkg = Base.identify_package("DelimitedFiles")
            @test Base.isprecompiled(pkg) == true
        end
    end

    @testset "load RelocationTestPkg1 from test/relocatedepot" begin
        pkgname = "RelocationTestPkg1"
        test_harness() do
            push!(LOAD_PATH, joinpath(@__DIR__, "relocatedepot"))
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot"))
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
            push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot"))
            pkg = Base.identify_package(pkgname)
            @test Base.isprecompiled(pkg) == false # moving depot changes mtime of include_dependency
            Base.require(pkg) # re-precompile
            @test Base.isprecompiled(pkg) == true
        end
    end

end
