using Test
using Logging


include("testenv.jl")


if !test_relocated_depot

    @testset "precompile RelocationTestPkg" begin
        load_path = copy(LOAD_PATH)
        depot_path = copy(DEPOT_PATH)
        push!(LOAD_PATH, @__DIR__)
        push!(DEPOT_PATH, @__DIR__)
        try
            pkg = Base.identify_package("RelocationTestPkg")
            cachefiles = Base.find_all_in_cache_path(pkg)
            rm.(cachefiles, force=true)
            rm(joinpath(@__DIR__, "RelocationTestPkg", "src", "foo.txt"), force=true)
            @test Base.isprecompiled(pkg) == false # include_dependency foo.txt is missing
            Base.require(pkg) # precompile
            @test Base.isprecompiled(pkg, ignore_loaded=true) == false # foo.txt still missing
            touch(joinpath(@__DIR__, "RelocationTestPkg", "src", "foo.txt"))
            @test Base.isprecompiled(pkg, ignore_loaded=true) == true
        finally
            copy!(LOAD_PATH, load_path)
            copy!(DEPOT_PATH, depot_path)
        end
    end

else

    @testset "load stdlib from test/relocatedepot" begin
        # stdlib should be already precompiled
        pkg = Base.identify_package("DelimitedFiles")
        @test Base.isprecompiled(pkg, ignore_loaded=true) == true
    end

    @testset "load RelocationTestPkg from test/relocatedepot" begin
        load_path = copy(LOAD_PATH)
        depot_path = copy(DEPOT_PATH)
        # moved source and depot into test/relocatedepot
        push!(LOAD_PATH, joinpath(@__DIR__, "relocatedepot"))
        push!(DEPOT_PATH, joinpath(@__DIR__, "relocatedepot"))
        try
            pkg = Base.identify_package("RelocationTestPkg")
            rm(joinpath(@__DIR__, "relocatedepot", "RelocationTestPkg", "src", "foo.txt"), force=true)
            @test Base.isprecompiled(pkg, ignore_loaded=true) == false # foo.txt missing
            touch(joinpath(@__DIR__, "relocatedepot", "RelocationTestPkg", "src", "foo.txt"))
            @test Base.isprecompiled(pkg) == true
        finally
            copy!(LOAD_PATH, load_path)
            copy!(DEPOT_PATH, depot_path)
        end
    end


    @testset "throw when failed to find a depot for RelocationTestPkg" begin
        load_path = copy(LOAD_PATH)
        push!(LOAD_PATH, joinpath(@__DIR__, "relocatedepot"))
        try
            pkg = Base.identify_package("RelocationTestPkg")
            touch(joinpath(@__DIR__, "relocatedepot", "RelocationTestPkg", "src", "foo.txt"))
            cachefile = only(Base.find_all_in_cache_path(pkg))
            @test_throws ArgumentError("""
              Failed to determine depot from srctext files in cache file $cachefile.
              - Make sure you have adjusted DEPOT_PATH in case you relocated depots.""") Base.isprecompiled(pkg)
        finally
            copy!(LOAD_PATH, load_path)
        end
    end

end
