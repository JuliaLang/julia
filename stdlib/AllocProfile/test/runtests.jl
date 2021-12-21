# TODO: register AllocProfile in the stdlib
using Pkg; Pkg.activate("stdlib/AllocProfile")

using Test

using AllocProfile

@testset "alloc profiler doesn't segfault" begin
    AllocProfile.start()

    # test the allocations during compilation
    using Base64
    # make sure some frees show up in the profile
    GC.gc()

    results = AllocProfile.stop()
    AllocProfile.clear()

    @test length(results.allocs) > 0
    first_alloc = results.allocs[1]
    @test first_alloc.size > 0
    @test length(first_alloc.stacktrace) > 0
    @test length(string(first_alloc.type)) > 0
    
    @test length(results.frees) > 0
end
