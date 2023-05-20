using Test


include("testenv.jl")


@testset "compile and load relocated pkg" begin
    pkg = Base.identify_package("DelimitedFiles")
    if !test_relocated_depot
        # precompile
        Base.require(Main, :DelimitedFiles)
        @test Base.root_module_exists(pkg) == true
    else
        path = Base.locate_package(pkg)
        iscached = @lock Base.require_lock begin
            m = Base._require_search_from_serialized(pkg, path, UInt128(0))
            m isa Module
        end
        @test iscached == true # can load from cache
        Base.require(Main, :DelimitedFiles)
        @test Base.root_module_exists(pkg) == true
    end
end
