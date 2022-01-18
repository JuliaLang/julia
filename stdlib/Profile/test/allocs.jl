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

function capture_stderr(f::Function)
    new = Base.redirect_stderr()
    try
        f()
    finally
        Base.redirect_stderr(stderr)
        flush(new)
        close(new)
        return read(new, String)
    end
end

@testset "alloc profiler warning message" begin
    @testset "no allocs" begin
        Profile.Allocs.clear()
        Profile.Allocs.fetch()
    end
    io = IOBuffer()
    @testset "catches all allocations" begin
        foo() = []
        precompile(foo, ())
        Profile.Allocs.clear()
        Profile.Allocs.@profile sample_rate=1 foo()
        # Fake that we expected exactly 1 alloc, since we should have recorded >= 1
        Profile.Allocs._g_expected_sampled_allocs[] = 1
        Base.redirect_stderr(io) do
            @assert length(Profile.Allocs.fetch().allocs) >= 1
        end
        warning = String(take!(io))
        @test occursin("may have missed some of the allocs", warning)
        @test occursin("https://github.com/JuliaLang/julia/issues/43688", warning)
    end
    @testset "misses some allocations" begin
        foo() = []
        precompile(foo, ())
        Profile.Allocs.clear()
        Profile.Allocs.@profile sample_rate=1 foo()
        # Fake some allocs that we missed, to force the print statement
        Profile.Allocs._g_expected_sampled_allocs[] += 10
        Base.redirect_stderr(io) do
            @assert 1 <= length(Profile.Allocs.fetch().allocs) < 10
        end
        warning = String(take!(io))
        @test occursin(r"missed approximately [0-9]+%", warning)
        @test occursin("https://github.com/JuliaLang/julia/issues/43688", warning)
    end
end

@testset "alloc profiler catches strings" begin
    Allocs.@profile sample_rate=1 "$(rand())"

    prof = Allocs.fetch()
    Allocs.clear()

    @test length(prof.allocs) >= 1
    @test length([a for a in prof.allocs if a.type == String]) >= 1
end
