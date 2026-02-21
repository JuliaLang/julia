using Test
using Profile: Allocs

Allocs.clear()
let iobuf = IOBuffer()
    for format in (:tree, :flat)
        Test.@test_logs (:warn, r"^There were no samples collected\.") Allocs.print(iobuf; format, C=true)
    end
end

# Issue #57103: This test does not work with MMTk because of fastpath
# allocation which never calls the allocation profiler.
# TODO: We should port these observability tools (e.g. allocation
# profiler and heap snapshot) to MMTk
@static if Base.USING_STOCK_GC
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

    # test printing options
    for options in ((format=:tree, C=true),
                    (format=:tree, maxdepth=2),
                    (format=:flat, C=true),
                    (),
                    (format=:flat, sortedby=:count),
                    (format=:tree, recur=:flat),
                   )
        iobuf = IOBuffer()
        Allocs.print(iobuf; options...)
        str = String(take!(iobuf))
        @test !isempty(str)
    end
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
        nsyms = @static Sys.WORD_SIZE == 32 ? 1 : 10
        for f in (gensym() for _ in 1:nsyms)
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

@testset "alloc profiler catches allocs from codegen" begin
    @eval begin
        struct MyType x::Int; y::Int end
        Base.:(+)(n::Number, x::MyType) = n + x.x + x.y
        foo(a, x) = a[1] + x
        wrapper(a) = foo(a, MyType(0,1))
    end
    a = Any[1,2,3]
    # warmup
    wrapper(a)

    @eval Allocs.@profile sample_rate=1 wrapper($a)

    prof = Allocs.fetch()
    Allocs.clear()

    @test length(prof.allocs) >= 1
    @test length([a for a in prof.allocs if a.type == MyType]) >= 1
end

@testset "alloc profiler catches allocs from buffer resize" begin
    f(a) = for _ in 1:100; push!(a, 1); end
    f(Int[])
    resize!(Int[], 1)
    a = Int[]
    Allocs.clear()
    Allocs.@profile sample_rate=1 f(a)
    Allocs.@profile sample_rate=1 resize!(a, 1_000_000) # 4MB
    prof = Allocs.fetch()
    Allocs.clear()

    @test 3 <= length(prof.allocs) <= 10
    @test length([a for a in prof.allocs if a.type === Allocs.BufferType]) == 1
    @test length([a for a in prof.allocs if a.type === Memory{Int}]) >= 2
end
end
