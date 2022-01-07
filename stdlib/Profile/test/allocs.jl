using Test
using Profile: Allocs

@testset "alloc profiler doesn't segfault" begin
    res = Allocs.@profile sample_rate=1.0 begin
        # test the allocations during compilation
        using Base64
    end
    profile = Allocs.fetch()

    @test length(profile.allocs) > 0
    first_alloc = profile.allocs[1]
    @test first_alloc.size > 0
    @test length(first_alloc.stacktrace) > 0
    @test length(string(first_alloc.type)) > 0
end

@testset "alloc profiler works when there are multiple tasks on multiple threads" begin
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
    end

    # call once to make sure it's compiled
    do_work()

    res = Allocs.@profile sample_rate=1 begin
        do_work()
    end
    profile = Allocs.fetch()

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

    # TODO: it would be nice to assert that these tasks
    # were actually scheduled onto multiple threads,
    # and we see allocs from all threads in the profile
end

@testset "alloc profiler start stop fetch clear" begin
    function do_work()
        # Compiling allocates a lot
        for f in (gensym() for _ in 1:10)
            @eval begin
                $f() = 10
                $f()
            end
        end
    end

    Allocs.@profile sample_rate=1 do_work()
    @test length(Allocs.fetch().allocs) > 10

    Allocs.clear()
    @test length(Allocs.fetch().allocs) == 0
    Allocs.clear()
    @test length(Allocs.fetch().allocs) == 0

    Allocs.@profile sample_rate=1 do_work()
    curr_allocs = length(Allocs.fetch().allocs)
    @test curr_allocs > 10

    # Do _more_ work, adding into the same profile
    Allocs.@profile sample_rate=1 do_work()
    @test length(Allocs.fetch().allocs) > curr_allocs

    Allocs.clear()
    @test length(Allocs.fetch().allocs) == 0
end
