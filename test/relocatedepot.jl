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
                @test startswith(Base.replace_depot_path(path), "@depot/")
                DEPOT_PATH[1] = joinpath(DEPOT_PATH[1], "") # append a pathsep
                @test startswith(Base.replace_depot_path(path), "@depot/")
                popfirst!(DEPOT_PATH)
                @test !startswith(Base.replace_depot_path(path), "@depot/")
            end
        end

    end

    @testset "restore path from @depot tag" begin

        path = "@depot/foo/bar"
        @test Base.restore_depot_path(path, "/tmp") == "/tmp/foo/bar"

        path = "blabla/foo/bar"
        @test Base.restore_depot_path(path, "/tmp") == "blabla/foo/bar"

        path = "@depot/foo/bar\n@depot/mypkg/src/file.jl"
        @test Base.restore_depot_path(path, "/tmp") == "/tmp/foo/bar\n@depot/mypkg/src/file.jl"

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
            @info "SERS OIDA"
            @test Base.isprecompiled(pkg, ignore_loaded=true) == true
        end
    end

else

    # must come before any of the load tests, because the will recompile and generate new cache files
    @testset "attempt loading precompiled pkgs when depot is missing" begin
        test_harness() do
            empty!(LOAD_PATH)
            push!(LOAD_PATH, joinpath(@__DIR__, "relocatedepot"))
            for pkgname in ("RelocationTestPkg1", "RelocationTestPkg2")
                pkg = Base.identify_package(pkgname)
                cachefile = only(Base.find_all_in_cache_path(pkg))
                @info cachefile
                @test_throws ArgumentError("""
                  Failed to determine depot from srctext files in cache file $cachefile.
                  - Make sure you have adjusted DEPOT_PATH in case you relocated depots.""") Base.isprecompiled(pkg)
            end
        end
    end

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
