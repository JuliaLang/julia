using Test
using Pkg


@testset "GC thread-safety" begin
    # Test 1: Basic thread-safety (no crashes)
    threads = [Threads.@spawn begin
        for _ in 1:1000
            a = rand(1000)  # Allocate memory
            GC.gc()        # Trigger GC
        end
    end for _ in 1:Threads.nthreads()]
    fetch.(threads)  # Will throw if any thread crashes
    @test true  # Only reaches here if no crashes

    # Test 2: Verify no data corruption (using proper Atomic)
    counter = Threads.Atomic{Int}(0)
    Threads.@threads for i in 1:1000
        Threads.atomic_add!(counter, 1)
        GC.gc()  # Trigger GC during atomic ops
    end
    @test counter[] == 1000  # Verify all increments completed
end
