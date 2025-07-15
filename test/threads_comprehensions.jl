# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for @threads with array comprehensions
using Test
using Base.Threads

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

        # Test dynamic scheduling
        result_dynamic = @threads :dynamic [i^2 for i in 1:n]
        @test length(result_dynamic) == n
        @test all(result_dynamic[i] == i^2 for i in 1:n)
        @test issorted(result_dynamic)  # should be ordered for dynamic scheduling

        # Test greedy scheduling (may not preserve order)
        result_greedy = @threads :greedy [i^2 for i in 1:n]
        @test length(result_greedy) == n
        @test sort(result_greedy) == [i^2 for i in 1:n]  # same elements but potentially different order
    end

    # Test filtered comprehensions
    @testset "filtered comprehensions" begin
        n = 100

        # Test default scheduling with filter
        result = @threads [i^2 for i in 1:n if iseven(i)]
        expected = [i^2 for i in 1:n if iseven(i)]
        @test length(result) == length(expected)
        @test result == expected  # should preserve order

        # Test static scheduling with filter
        result_static = @threads :static [i^2 for i in 1:n if iseven(i)]
        @test length(result_static) == length(expected)
        @test result_static == expected  # should preserve order

        # Test dynamic scheduling with filter
        result_dynamic = @threads :dynamic [i^2 for i in 1:n if iseven(i)]
        @test length(result_dynamic) == length(expected)
        @test result_dynamic == expected  # should preserve order

        # Test greedy scheduling with filter
        result_greedy = @threads :greedy [i^2 for i in 1:n if iseven(i)]
        @test length(result_greedy) == length(expected)
        @test sort(result_greedy) == sort(expected)  # same elements but potentially different order

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
        @test length(result_large) == n
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

        result_dynamic = @threads :dynamic [i * j for i in 1:3, j in 1:3]
        @test size(result_dynamic) == size(expected_static)
        @test result_dynamic == expected_static  # dynamic scheduling preserves order and dimensions

        result_greedy = @threads :greedy [i * j for i in 1:3, j in 1:3]
        @test size(result_greedy) == size(expected_static)
        @test sort(vec(result_greedy)) == sort(vec(expected_static))  # greedy may reorder but preserves shape

        # Test with more than 2 loops
        result_3d = @threads [i + j + k for i in 1:2, j in 1:2, k in 1:2]
        expected_3d = [i + j + k for i in 1:2, j in 1:2, k in 1:2]
        @test size(result_3d) == size(expected_3d)
        @test result_3d == expected_3d  # default scheduling preserves order and dimensions
    end

    # Test non-indexable iterators
    @testset "non-indexable iterators" begin
        # Test with Iterators.flatten
        flat_iter = Iterators.flatten([1:3, 4:6])
        result = @threads [i^2 for i in flat_iter]
        expected = [i^2 for i in 1:6]
        @test length(result) == 6
        @test result == expected  # default scheduling preserves order

        # Test with greedy scheduling for non-indexable
        result_greedy = @threads :greedy [i^2 for i in Iterators.flatten([1:3, 4:6])]
        @test length(result_greedy) == 6
        @test sort(result_greedy) == sort(expected)  # greedy may reorder

        # Test with filter on non-indexable iterator
        result_filter = @threads [i for i in Iterators.flatten([1:5, 6:10]) if iseven(i)]
        expected_filter = [i for i in 1:10 if iseven(i)]
        @test length(result_filter) == 5
        @test result_filter == expected_filter  # default scheduling preserves order

        # Test with Iterators.repeated (but limited)
        repeated_iter = Iterators.take(Iterators.repeated(42), 5)
        result_repeated = @threads [x for x in repeated_iter]
        @test length(result_repeated) == 5
        @test all(x == 42 for x in result_repeated)

        # Test non-indexable with static and dynamic scheduling
        result_static = @threads :static [i^2 for i in Iterators.flatten([1:3, 4:6])]
        @test length(result_static) == 6
        @test result_static == expected  # static scheduling preserves order

        result_dynamic = @threads :dynamic [i^2 for i in Iterators.flatten([1:3, 4:6])]
        @test length(result_dynamic) == 6
        @test result_dynamic == expected  # dynamic scheduling preserves order
    end

    # Test type inference
    @testset "type inference" begin
        # Test that return types are properly inferred, not Vector{Any}
        result_int = @threads [i for i in 1:10]
        @test result_int isa Vector{Int}
        @test !(result_int isa Vector{Any})

        result_float = @threads [Float64(i) for i in 1:10]
        @test result_float isa Vector{Float64}
        @test !(result_float isa Vector{Any})

        # Test with filtering
        result_filtered = @threads [i^2 for i in 1:10 if iseven(i)]
        @test result_filtered isa Vector{Int}

        # Test multi-dimensional
        result_2d = @threads [i + j for i in 1:3, j in 1:3]
        @test result_2d isa Matrix{Int}
        @test !(result_2d isa Array{Any})

        # Test non-indexable iterators
        result_flatten = @threads [i for i in Iterators.flatten([1:3, 4:6])]
        @test result_flatten isa Vector{Int}

        # Test with String type
        result_string = @threads [string(i) for i in 1:5]
        @test result_string isa Vector{String}
        @test result_string == ["1", "2", "3", "4", "5"]
    end

    # Test Channel-based iterators
    @testset "channel iterators" begin
        # Test with Channel
        ch = Channel{Int}(10)
        foreach(i -> put!(ch, i), 1:10)
        close(ch)
        result_ch = @threads :greedy [i^2 for i in ch]
        @test length(result_ch) == 10
        @test sort(result_ch) == [i^2 for i in 1:10]

        # Test Channel with filter
        ch2 = Channel{Int}(10)
        foreach(i -> put!(ch2, i), 1:10)
        close(ch2)
        result_ch_filter = @threads :greedy [i for i in ch2 if iseven(i)]
        @test length(result_ch_filter) == 5
        @test sort(result_ch_filter) == [2, 4, 6, 8, 10]
    end

    # Test multi-dimensional with filters
    @testset "multi-dimensional with filters" begin
        # Test 2D with filter
        result = @threads [i * j for i in 1:4, j in 1:4 if i + j > 5]
        expected = [i * j for i in 1:4, j in 1:4 if i + j > 5]
        @test length(result) == length(expected)
        @test result == expected  # default scheduling preserves order

        # Test 3D with filter
        result_3d = @threads [i + j + k for i in 1:3, j in 1:3, k in 1:3 if (i + j + k) % 2 == 0]
        expected_3d = [i + j + k for i in 1:3, j in 1:3, k in 1:3 if (i + j + k) % 2 == 0]
        @test length(result_3d) == length(expected_3d)
        @test result_3d == expected_3d  # default scheduling preserves order
    end

    # Test mixed element types
    @testset "mixed element types" begin
        # Test that mixed types return Vector{Any} like normal comprehensions
        result = @threads [x for x in [1, 2.0, "3"]]
        expected = [x for x in [1, 2.0, "3"]]
        @test result == expected
        @test result isa Vector{Any}
        @test typeof(result) == typeof(expected)

        # Test with :greedy scheduler
        result_greedy = @threads :greedy [x for x in [1, 2.0, "3"]]
        @test sort(result_greedy, by=string) == sort(expected, by=string)  # greedy may reorder
        @test result_greedy isa Vector{Any}

        # Test with :static scheduler
        result_static = @threads :static [x for x in [1, 2.0, "3"]]
        @test result_static == expected
        @test result_static isa Vector{Any}
    end
end
