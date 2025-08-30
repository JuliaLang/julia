# Test file that exercises all testset filtering functionality
using Test

# ============================================================================
# SHARED TEST FILES (used by multiple include patterns)
# ============================================================================

# Test 1: Basic static include
include("shared_tests.jl")

# Test 2: Const declaration with path function
const TESTDIR = joinpath(@__DIR__, "modules")

# Test 3: Dynamic include with const variable (reuses shared_tests.jl)
include(joinpath(TESTDIR, "shared_tests.jl"))

# Test 4: Variable-based includes (reuse the same files)
test_files = ["shared_tests.jl", "advanced_tests.jl"]
for file in test_files
    include(file)
end

# Test 5: String concatenation includes (create variants that reuse content)
test_variants = ["math", "string", "logic"]
for variant in test_variants
    include(variant * "_tests.jl")
end

# Test 6: Readlines-based includes (reuse shared files)
for file in readlines(joinpath(@__DIR__, "file_list.txt"))
    include(file)
end

# Test 7: Path function includes with deep nesting
include(joinpath(@__DIR__, "nested", "deep_tests.jl"))

# ============================================================================
# INLINE TESTSETS (demonstrate direct testset definition)
# ============================================================================

@testset "Main Test Suite" begin
    @testset "Basic functionality" begin
        @test 1 + 1 == 2
        @test 5 > 3
    end

    @testset "Nested tests" begin
        @testset "Level 1" begin
            @test true
            @testset "Level 2" begin
                @test 2 * 2 == 4
                @testset "Level 3" begin
                    @test length("test") == 4
                end
            end
        end
    end
end

# ============================================================================
# LOOP TESTSETS (demonstrate parameterized testsets)
# ============================================================================

@testset "Loop testsets" for n in 1:3
    @test n > 0
    @test n <= 3
end

# Complex loop testsets with string interpolation
test_cases = ["case_a", "case_b", "case_c"]
@testset "Parameterized $case" for case in test_cases
    @test !isempty(case)
    @test startswith(case, "case_")
end
