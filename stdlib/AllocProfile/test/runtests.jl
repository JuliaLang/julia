# TODO: register AllocProfile in the stdlib
using Pkg; Pkg.activate("stdlib/AllocProfile")

using Test

using AllocProfile

@testset "alloc profiler doesn't segfault" begin
    res, profile = AllocProfile.@profile 0 begin
        # test the allocations during compilation
        using Base64
        # make sure some frees show up in the profile
        GC.gc()
    end

    @test length(profile.allocs) > 0
    first_alloc = profile.allocs[1]
    @test first_alloc.size > 0
    @test length(first_alloc.stacktrace) > 0
    @test length(string(first_alloc.type)) > 0
    
    @test length(profile.frees) > 0
end
