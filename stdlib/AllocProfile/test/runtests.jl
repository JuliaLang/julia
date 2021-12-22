# TODO: register AllocProfile in the stdlib
using Pkg; Pkg.activate("stdlib/AllocProfile")

using Test

using AllocProfile

@testset "alloc profiler doesn't segfault" begin
    res, profile = AllocProfile.@profile skip_every=0 begin
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

@testset "works when there are multiple tasks on multiple threads" begin
    NUM_TASKS = 1000

    # TODO: is this always true in CI?
    @test Threads.nthreads() > 1

    function do_work()
        ch = Channel{Vector{Int}}(Inf)
        @sync for i in 1:NUM_TASKS
            Threads.@spawn begin
                # generate garbage
                put!(ch, zeros(100))
            end
        end
        close(ch)
        # for obj in ch
        #     # ...
        # end
        GC.gc()
    end

    # call once to make sure it's compiled
    do_work()

    res, profile = AllocProfile.@profile skip_every=0 begin
        do_work()
    end

    # expecting at least 3 allocations per task:
    # 1. the task
    # 2. the vector
    # 3. the buffer inside the vector
    @test length(profile.allocs) >= 3*NUM_TASKS
    println(length(profile.allocs))
    first_alloc = profile.allocs[1]
    @test first_alloc.size > 0
    @test length(first_alloc.stacktrace) > 0
    @test length(string(first_alloc.type)) > 0

    @test length(profile.frees) > 0

    # TODO: it would be nice to assert that these tasks
    # were actually scheduled onto multiple threads,
    # and we see allocs from all threads in the profile
end
