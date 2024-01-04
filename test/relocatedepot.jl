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
