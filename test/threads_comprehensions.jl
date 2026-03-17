# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for @threads with array comprehensions
using Test
using Base.Threads

isdefined(Main, :OffsetArrays) || @eval Main include("testhelpers/OffsetArrays.jl")
using .Main.OffsetArrays

@testset "@threads comprehensions" begin
    # Test simple array comprehensions
    @testset "simple comprehensions" begin
        n = 1000
        # Test default scheduling
        result = @threads [i^2 for i in 1:n]
        @test length(result) == n
        @test all(result[i] == i^2 for i in 1:n)
        @test issorted(result)  # should be ordered for default scheduling

        # Test static scheduling
        result_static = @threads :static [i^2 for i in 1:n]
        @test length(result_static) == n
        @test all(result_static[i] == i^2 for i in 1:n)
        @test issorted(result_static)  # should be ordered for static scheduling

        # Test greedy scheduling (does not guarantee order)
        result_greedy = @threads :greedy [i^2 for i in 1:n]
        @test length(result_greedy) == n
        @test sort(result_greedy) == [i^2 for i in 1:n]
    end

    # Test filtered comprehensions
    @testset "filtered comprehensions" begin
        n = 100

        # Test default scheduling with filter
        result = @threads [i^2 for i in 1:n if iseven(i)]
        expected = [i^2 for i in 1:n if iseven(i)]
        @test result == expected

        # Test static scheduling with filter
        result_static = @threads :static [i^2 for i in 1:n if iseven(i)]
        @test result_static == expected

        # Test greedy scheduling with filter (does not guarantee order)
        result_greedy = @threads :greedy [i^2 for i in 1:n if iseven(i)]
        @test sort(result_greedy) == expected

        # Test with more complex filter
        result_complex = @threads [i for i in 1:100 if i % 3 == 0 && i > 20]
        expected_complex = [i for i in 1:100 if i % 3 == 0 && i > 20]
        @test result_complex == expected_complex
    end

    # Test edge cases
    @testset "edge cases" begin
        # Empty range
        result_empty = @threads [i for i in 1:0]
        @test result_empty == []

        # Single element
        result_single = @threads [i^2 for i in 1:1]
        @test result_single == [1]

        # Filter that excludes all elements
        result_none = @threads [i for i in 1:10 if i > 20]
        @test result_none == []

        # Large range to test thread distribution
        n = 10000
        result_large = @threads [i for i in 1:n]
        @test result_large == collect(1:n)
    end

    # Test with side effects (should work but order may vary with greedy)
    @testset "side effects" begin
        # Test with atomic operations
        counter = Threads.Atomic{Int}(0)
        result = @threads [begin
            Threads.atomic_add!(counter, 1)
            i
        end for i in 1:100]
        @test counter[] == 100
        @test result == collect(1:100)  # default scheduling preserves order

        # Test with thread-local computation
        result_tid = @threads [Threads.threadid() for i in 1:100]
        @test length(result_tid) == 100
        @test all(1 <= tid <= Threads.nthreads() + Threads.nthreads(:interactive) for tid in result_tid)
    end

    # Test multiple loops comprehensions
    @testset "multiple loops comprehensions" begin
        # Test simple multiple loops
        result = @threads [i + j for i in 1:3, j in 1:3]
        expected = [i + j for i in 1:3, j in 1:3]
        @test size(result) == size(expected)
        @test result == expected  # default scheduling preserves order and dimensions

        # Test multiple loops with different scheduling
        result_static = @threads :static [i * j for i in 1:3, j in 1:3]
        expected_static = [i * j for i in 1:3, j in 1:3]
        @test size(result_static) == size(expected_static)
        @test result_static == expected_static  # static scheduling preserves order and dimensions

        result_greedy = @threads :greedy [i * j for i in 1:3, j in 1:3]
        @test sort(vec(result_greedy)) == sort(vec(expected_static))

        # Test with more than 2 loops
        result_3d = @threads [i + j + k for i in 1:2, j in 1:2, k in 1:2]
        expected_3d = [i + j + k for i in 1:2, j in 1:2, k in 1:2]
        @test size(result_3d) == size(expected_3d)
        @test result_3d == expected_3d  # default scheduling preserves order and dimensions
        # Test 2D with filter
        result_2d_filt = @threads [i * j for i in 1:4, j in 1:4 if i + j > 5]
        expected_2d_filt = [i * j for i in 1:4, j in 1:4 if i + j > 5]
        @test result_2d_filt == expected_2d_filt

        # Test 3D with filter
        result_3d_filt = @threads [i + j + k for i in 1:3, j in 1:3, k in 1:3 if (i + j + k) % 2 == 0]
        expected_3d_filt = [i + j + k for i in 1:3, j in 1:3, k in 1:3 if (i + j + k) % 2 == 0]
        @test result_3d_filt == expected_3d_filt    end

    # Test non-indexable iterators
    @testset "non-indexable iterators" begin
        # Test with Iterators.flatten
        flat_iter = Iterators.flatten([1:3, 4:6])
        result = @threads [i^2 for i in flat_iter]
        expected = [i^2 for i in 1:6]
        @test result == expected

        # Test with greedy scheduling for non-indexable (does not guarantee order)
        result_greedy = @threads :greedy [i^2 for i in Iterators.flatten([1:3, 4:6])]
        @test sort(result_greedy) == expected

        # Test with filter on non-indexable iterator
        result_filter = @threads [i for i in Iterators.flatten([1:5, 6:10]) if iseven(i)]
        expected_filter = [i for i in 1:10 if iseven(i)]
        @test result_filter == expected_filter

        # Test with Iterators.repeated (but limited)
        repeated_iter = Iterators.take(Iterators.repeated(42), 5)
        result_repeated = @threads [x for x in repeated_iter]
        @test result_repeated == fill(42, 5)
    end

    # Test Tuple iterators (used directly without collect)
    @testset "tuple iterators" begin
        t = (10, 20, 30, 40, 50)
        result = @threads [x^2 for x in t]
        @test result == [x^2 for x in t]

        result_typed = @threads Int[x + 1 for x in t]
        @test result_typed == Int[x + 1 for x in t]
        @test result_typed isa Vector{Int}

        result_greedy = @threads :greedy [x for x in t]
        @test sort(result_greedy) == sort(collect(t))

        result_filter = @threads [x for x in t if x > 25]
        @test result_filter == [x for x in t if x > 25]
    end

    # Test Channel-based iterators
    @testset "channel iterators" begin
        # Test with Channel
        ch = Channel{Int}(10)
        foreach(i -> put!(ch, i), 1:10)
        close(ch)
        result_ch = @threads :greedy [i^2 for i in ch]
        @test sort(result_ch) == [i^2 for i in 1:10]

        # Test Channel with filter
        ch2 = Channel{Int}(10)
        foreach(i -> put!(ch2, i), 1:10)
        close(ch2)
        result_ch_filter = @threads :greedy [i for i in ch2 if iseven(i)]
        @test sort(result_ch_filter) == [2, 4, 6, 8, 10]
    end

    # Test mixed element types
    @testset "mixed element types" begin
        # Test that mixed types return Vector{Any} like normal comprehensions
        result = @threads [x for x in [1, 2.0, "3"]]
        expected = [x for x in [1, 2.0, "3"]]
        @test result == expected
        @test eltype(result) == eltype(expected)

        # Test with :greedy scheduler (does not guarantee order)
        result_greedy = @threads :greedy [x for x in [1, 2.0, "3"]]
        @test sort(result_greedy, by=string) == sort(expected, by=string)
        @test result_greedy isa Vector{Any}

        # Test with :static scheduler
        result_static = @threads :static [x for x in [1, 2.0, "3"]]
        @test result_static == expected
        @test eltype(result_static) == eltype(expected)
    end

    # Test type widening when body expression produces heterogeneous types
    @testset "body expression type widening" begin
        result = @threads [i == 50 ? 1.0 : i for i in 1:100]
        expected = [i == 50 ? 1.0 : i for i in 1:100]
        @test result == expected
        @test eltype(result) == eltype(expected)

        # Verify widening allocations aren't significantly worse than serial
        widen_threaded() = @threads [i == 100 ? 1.0 : i for i in 1:100_000]
        widen_serial() = [i == 100 ? 1.0 : i for i in 1:100_000]
        widen_threaded(); widen_serial()
        @test @allocations(widen_threaded()) < @allocations(widen_serial()) + 200
    end

    # Test typed comprehensions
    @testset "typed comprehensions" begin
        n = 100

        # Basic typed comprehension
        result = @threads Float64[i^2 for i in 1:n]
        @test result isa Vector{Float64}
        @test length(result) == n
        @test all(result[i] == Float64(i^2) for i in 1:n)

        # Typed comprehension with different schedulers
        result_static = @threads :static Float64[i for i in 1:n]
        @test result_static isa Vector{Float64}
        @test result_static == Float64.(1:n)

        result_greedy = @threads :greedy Float64[i for i in 1:n]
        @test result_greedy isa Vector{Float64}
        @test sort(result_greedy) == Float64.(1:n)

        # Typed comprehension with filter
        result_filtered = @threads Int[i^2 for i in 1:20 if iseven(i)]
        expected_filtered = Int[i^2 for i in 1:20 if iseven(i)]
        @test result_filtered isa Vector{Int}
        @test result_filtered == expected_filtered

        # Typed comprehension with filter and greedy (does not guarantee order)
        result_greedy_filt = @threads :greedy Float64[i for i in 1:20 if i > 10]
        @test result_greedy_filt isa Vector{Float64}
        @test sort(result_greedy_filt) == Float64.(11:20)

        # Typed multi-dimensional comprehension
        result_2d = @threads Float64[i + j for i in 1:3, j in 1:4]
        expected_2d = Float64[i + j for i in 1:3, j in 1:4]
        @test result_2d isa Matrix{Float64}
        @test size(result_2d) == (3, 4)
        @test result_2d == expected_2d

        # Empty typed comprehension
        result_empty = @threads Float64[i for i in 1:0]
        @test result_empty isa Vector{Float64}
        @test isempty(result_empty)

        # Type conversion: body produces Int, but result is Float64
        result_conv = @threads Float64[i for i in 1:5]
        @test result_conv isa Vector{Float64}
        @test result_conv == [1.0, 2.0, 3.0, 4.0, 5.0]

        # Typed threaded comprehension should have O(nthreads) allocs, not O(n)
        t1() = @threads Int[i for i in 1:1]
        t1()
        t2() = @threads :static Int[i for i in 1:1]
        t2()
        t3() = @threads :dynamic Int[i for i in 1:1]
        t3()
        @test @allocations(t1()) < 100  # ~35 allocs in practice
        @test @allocations(t2()) < 100
        @test @allocations(t3()) < 100
    end

    # Test non-1-based indexing preserves axes
    @testset "non-1-based indexing" begin
        r = OffsetArray(1:5, -2:2)
        result = @threads [x^2 for x in r]
        expected = [x^2 for x in r]
        @test result == expected
        @test axes(result) == axes(expected)

        result_typed = @threads Int[x^2 for x in r]
        @test result_typed == expected
        @test axes(result_typed) == axes(expected)

        result_static = @threads :static [x^2 for x in r]
        @test result_static == expected
        @test axes(result_static) == axes(expected)

        # Multi-loop with OffsetArrays preserves offset axes in each dimension
        r2 = OffsetArray(1:3, 0:2)
        result_multi = @threads [x+y for x in r2, y in r2]
        expected_multi = [x+y for x in r2, y in r2]
        @test result_multi == expected_multi
        @test axes(result_multi) == axes(expected_multi)
    end
end
