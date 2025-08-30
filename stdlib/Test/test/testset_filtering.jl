# Test for testset filtering functionality
using Test

# Helper function to collect all testset names recursively
function collect_all_testset_names(testsets::Vector{Test.TestSetNode})
    names = String[]
    for testset in testsets
        push!(names, testset.name)
        append!(names, collect_all_testset_names(testset.children))
    end
    return names
end

@testset "TestSet Filtering Tests" begin

    @testset "testset discovery" begin
        # Test discovery of the rationalized test package
        test_file = joinpath(@__DIR__, "testset_filtering", "runtests.jl")
        @test isfile(test_file)

        # Discover testsets
        discovered = Test.discover_testsets(test_file)
        @test isa(discovered, Vector{Test.TestSetNode})
        @test length(discovered) > 0

        # Collect all testset names including nested ones
        all_names = collect_all_testset_names(discovered)

        # Check for shared testsets (included multiple times via different patterns)
        @test count(contains("Shared Tests"), all_names) >= 2  # Should appear multiple times
        @test any(contains("basic math"), all_names)
        @test any(contains("string operations"), all_names)

        # Check for advanced testsets (included via variable pattern)
        @test count(contains("Advanced Tests"), all_names) >= 1
        @test any(contains("boolean logic"), all_names)
        @test any(contains("comparison operators"), all_names)
        @test any(contains("array operations"), all_names)

        # Check for variant testsets (string concatenation includes)
        @test any(contains("Math Variant Tests"), all_names)
        @test any(contains("String Variant Tests"), all_names)
        @test any(contains("Logic Variant Tests"), all_names)
        @test any(contains("arithmetic operations"), all_names)
        @test any(contains("string manipulation"), all_names)
        @test any(contains("logical operations"), all_names)

        # Check for deep path includes
        @test any(contains("Deep Path Tests"), all_names)
        @test any(contains("path operations"), all_names)
        @test any(contains("nested path tests"), all_names)
        @test any(contains("very deep nesting"), all_names)

        # Check for main testsets (defined inline)
        @test any(contains("Main Test Suite"), all_names)
        @test any(contains("Basic functionality"), all_names)
        @test any(contains("Nested tests"), all_names)
        @test any(contains("Level 1"), all_names)
        @test any(contains("Level 2"), all_names)
        @test any(contains("Level 3"), all_names)

        # Check for loop testsets
        @test any(contains("Loop testsets"), all_names)
        @test any(startswith("Parameterized case_"), all_names)

        println("Total testsets discovered (including nested): ", length(all_names))
        println("Rationalized structure demonstrates:")
        println("  - Shared files used by multiple include patterns: $(count(contains("Shared Tests"), all_names)) times")
        println("  - Advanced tests used by variable includes: $(count(contains("Advanced Tests"), all_names)) times")
        println("  - Variant tests via string concatenation: $(count(x -> contains("Variant Tests")(x), all_names)) variants")
    end

    @testset "testset filtering" begin
        test_file = joinpath(@__DIR__, "testset_filtering", "runtests.jl")
        discovered = Test.discover_testsets(test_file)

        # Test single pattern filtering
        filter1 = Test.create_testset_filter(discovered, ["Math Variant Tests"])
        @test length(filter1.enabled_testset_ids) >= 1

        # Test multiple pattern filtering
        filter2 = Test.create_testset_filter(discovered, ["Shared Tests", "Advanced Tests"])
        @test length(filter2.enabled_testset_ids) >= 2

        # Test regex pattern filtering
        filter3 = Test.create_testset_filter(discovered, [r"Variant Tests"])
        @test length(filter3.enabled_testset_ids) >= 3  # Should match Math, String, Logic Variant Tests

        # Test no matches
        filter4 = Test.create_testset_filter(discovered, ["nonexistent test"])
        @test length(filter4.enabled_testset_ids) == 0
    end

    @testset "tree structure" begin
        test_file = joinpath(@__DIR__, "testset_filtering", "runtests.jl")
        discovered = Test.discover_testsets(test_file)

        # Find a nested testset and verify structure
        main_suite = nothing
        for node in discovered
            if node.name == "Main Test Suite"
                main_suite = node
                break
            end
        end

        @test main_suite !== nothing
        @test length(main_suite.children) >= 2  # Should have "Basic functionality" and "Nested tests"

        # Check nested structure
        nested_tests = nothing
        for child in main_suite.children
            if child.name == "Nested tests"
                nested_tests = child
                break
            end
        end

        @test nested_tests !== nothing
        @test length(nested_tests.children) >= 1  # Should have "Level 1"

        # Check deep nesting
        level1 = nested_tests.children[1]
        @test level1.name == "Level 1"
        @test length(level1.children) >= 1  # Should have "Level 2"

        level2 = level1.children[1]
        @test level2.name == "Level 2"
    end

    @testset "error handling" begin
        # Test with non-existent file - should return empty list with warning, not throw
        discovered = Test.discover_testsets("/nonexistent/file.jl")
        @test isa(discovered, Vector{Test.TestSetNode})
        @test length(discovered) == 0

        # Test with empty file (should not error)
        empty_file = tempname()
        touch(empty_file)
        try
            discovered = Test.discover_testsets(empty_file)
            @test isa(discovered, Vector{Test.TestSetNode})
            @test length(discovered) == 0
        finally
            rm(empty_file, force=true)
        end
    end
end
