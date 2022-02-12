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

    # This test is only really meaningful if we're running on
    # multiple threads, but this isn't true on the windows tests,
    # causing them to fail. So, commenting this assertion out.
    # @test Threads.nthreads() > 1

    function do_work()
        ch = Channel{Vector{Float64}}(Inf)
        @sync for i in 1:NUM_TASKS
            Threads.@spawn begin
                # generate garbage
                put!(ch, zeros(100))
            end
        end
        close(ch)
    end

    # call once to make sure it's compiled
    precompile(do_work, ())
    do_work()

    res = Allocs.@profile sample_rate=1 begin
        do_work()
    end
    profile = Allocs.fetch()

    # expecting at least 2 allocations per task:
    # 1. the task
    # 2. the vector
    @test length(profile.allocs) >= 2*NUM_TASKS
    first_alloc = profile.allocs[1]
    @test first_alloc.size > 0
    @test length(first_alloc.stacktrace) > 0
    @test length(string(first_alloc.type)) > 0

    @testset for type in (Task, Vector{Float64},)
        @test length(filter(a->a.type <: type, profile.allocs)) >= NUM_TASKS
    end

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

    # Clear without fetching

    Allocs.@profile sample_rate=1 do_work()
    Allocs.clear()
    @test length(Allocs.fetch().allocs) == 0

    # And things still work like normal afterwards

    Allocs.@profile sample_rate=1 do_work()
    Allocs.@profile sample_rate=1 do_work()
    Allocs.@profile sample_rate=1 do_work()
    @test length(Allocs.fetch().allocs) > 10

    Allocs.@profile sample_rate=1 do_work()
    Allocs.@profile sample_rate=1 do_work()
    @test length(Allocs.fetch().allocs) > 10

    Allocs.clear()
end

@testset "alloc profiler catches strings" begin
    Allocs.@profile sample_rate=1 "$(rand())"

    prof = Allocs.fetch()
    Allocs.clear()

    @test length(prof.allocs) >= 1
    @test length([a for a in prof.allocs if a.type == String]) >= 1
end
