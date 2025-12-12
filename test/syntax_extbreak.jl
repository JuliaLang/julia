# This file is a part of Julia. License is MIT: https://julialang.org/license
# Extended break syntax tests (1.14+)
# This file should be included with Base.Experimental.@set_syntax_version v"1.14"

using Test

@testset "extended break syntax" begin
    # Valued break - basic functionality
    @test (for i in 1:10
        if i == 5
            break 42
        end
    end) == 42

    @test (for i in 1:10; end) === nothing

    # Valued break from while loop
    @test (let i = 0
        while i < 10
            i += 1
            if i == 5
                break i * 2
            end
        end
    end) == 10

    # Multi-level break
    @test (for i in 1:3
        for j in 1:3
            break break (i, j)
        end
    end) == (1, 1)

    # Multi-level break with value
    @test (for i in 1:5
        for j in 1:5
            if i + j == 5
                break break (i, j)
            end
        end
    end) == (1, 4)

    # Break continue (continue outer loop)
    let results = Int[]
        for i in 1:3
            for j in 1:5
                push!(results, i * 10 + j)
                if j == 2
                    break continue  # Skip rest of inner loop, continue outer
                end
            end
        end
        @test results == [11, 12, 21, 22, 31, 32]
    end

    # Three-level nesting with break break continue
    let results = Int[]
        for i in 1:2
            for j in 1:3
                for k in 1:4
                    push!(results, i * 100 + j * 10 + k)
                    if k == 2
                        break break continue  # Continue the i loop
                    end
                end
            end
        end
        @test results == [111, 112, 211, 212]
    end

    # Labeled loops - basic break
    @test (@label outer for i in 1:10
        for j in 1:10
            if i * j > 20
                @goto outer break (i, j)
            end
        end
    end) == (3, 7)

    # Labeled loops - continue
    let results = Int[]
        @label outer for i in 1:3
            for j in 1:5
                push!(results, i * 10 + j)
                if j == 2
                    @goto outer continue
                end
            end
        end
        @test results == [11, 12, 21, 22, 31, 32]
    end

    # Error: break level exceeds nesting
    @test Meta.lower(@__MODULE__, :(for i in 1:3; break break break; end)) == Expr(:error, "break level exceeds loop nesting depth")

    # Error: continue level exceeds nesting
    @test Meta.lower(@__MODULE__, :(for i in 1:3; break break continue; end)) == Expr(:error, "continue level exceeds loop nesting depth")
end

@testset "cartesian loops" begin
    # Cartesian loop with valued break
    @test (for i in 1:3, j in 1:4
        if i == 2 && j == 3
            break (i, j)
        end
    end) == (2, 3)

    # Cartesian loop completes normally returns nothing
    @test (for i in 1:2, j in 1:2
    end) === nothing

    # Cartesian loop with then clause - then runs after full completion
    @test (begin
        local completed = false
        for i in 1:2, j in 1:2
        then
            completed = true
        end
        completed
    end) == true

    # Cartesian loop with break skips then
    @test (begin
        local completed = false
        for i in 1:2, j in 1:2
            if i == 1 && j == 2
                break
            end
        then
            completed = true
        end
        completed
    end) == false

    # Cartesian loop with valued break and then
    @test (for i in 1:3, j in 1:3
        if i + j == 4
            break :found
        end
    then
        :not_found
    end) == :found

    # Cartesian loop completes - then clause returns its value
    @test (for i in 1:2, j in 1:2
        if i + j == 100  # never true
            break :found
        end
    then
        :not_found
    end) == :not_found

    # Multi-level break in cartesian loop is an error (cartesian is single loop)
    @test Meta.lower(@__MODULE__, :(for i in 1:3, j in 1:3; break break; end)) == Expr(:error, "break level exceeds loop nesting depth")

    # Cartesian inside regular loop - multi-level break works
    @test (for outer in 1:2
        for i in 1:3, j in 1:3
            if i == 2 && j == 2
                break break (outer, i, j)
            end
        end
    end) == (1, 2, 2)

    # break continue with cartesian loop inside
    let results = Tuple{Int,Int,Int}[]
        for outer in 1:3
            for i in 1:2, j in 1:2
                push!(results, (outer, i, j))
                if i == 1 && j == 2
                    break continue  # exit cartesian, continue outer
                end
            end
        end
        @test results == [(1,1,1), (1,1,2), (2,1,1), (2,1,2), (3,1,1), (3,1,2)]
    end
end

@testset "labeled loop errors" begin
    # Error: @goto to non-loop label with break - treated as break outside loop
    @test Meta.lower(@__MODULE__, quote
        @label target
        for i in 1:3
            @goto target break
        end
    end) == Expr(:error, "break or continue outside loop")

    # Error: @goto to non-loop label with continue - treated as continue outside loop
    @test Meta.lower(@__MODULE__, quote
        @label target
        for i in 1:3
            @goto target continue
        end
    end) == Expr(:error, "break or continue outside loop")

    # Error: @goto break to undefined label - treated as break outside loop
    @test Meta.lower(@__MODULE__, quote
        for i in 1:3
            @goto nonexistent break
        end
    end) == Expr(:error, "break or continue outside loop")

    # Error: @goto continue to undefined label - treated as continue outside loop
    @test Meta.lower(@__MODULE__, quote
        for i in 1:3
            @goto nonexistent continue
        end
    end) == Expr(:error, "break or continue outside loop")

    # Valid: labeled loop with then clause - break returns value
    @test (@label outer for i in 1:3
        for j in 1:3
            if i == 2 && j == 2
                @goto outer break :found
            end
        end
    then
        :not_found
    end) == :found

    # Labeled loop then clause runs on normal completion and returns its value
    @test (@label outer for i in 1:2
        for j in 1:2
            if i + j == 100  # never true
                @goto outer break :found
            end
        end
    then
        :completed
    end) == :completed
end

@testset "loop then clause" begin
    # for loop completes normally - then clause runs
    @test (begin
        local found = false
        for i in 1:3
            if i == 10
                found = true
                break
            end
        then
            found = :completed
        end
        found
    end) == :completed

    # for loop with break - then clause is skipped
    @test (begin
        local found = false
        for i in 1:10
            if i == 5
                found = :found_it
                break
            end
        then
            found = :completed
        end
        found
    end) == :found_it

    # while loop completes normally - then clause runs
    @test (begin
        local i = 0
        local status = :running
        while i < 3
            i += 1
        then
            status = :completed
        end
        status
    end) == :completed

    # while loop with break - then clause is skipped
    @test (begin
        local i = 0
        local status = :running
        while i < 10
            i += 1
            if i == 5
                status = :found_at_5
                break
            end
        then
            status = :completed
        end
        status
    end) == :found_at_5

    # Empty loop body - then still runs
    @test (begin
        local ran = false
        for i in 1:0  # Empty iteration
        then
            ran = true
        end
        ran
    end) == true

    # Nested for loops - only innermost then is affected by break
    @test (begin
        local outer_then = false
        local inner_then = false
        for i in 1:2
            for j in 1:3
                if j == 2
                    break
                end
            then
                inner_then = true  # Should NOT run (break executed)
            end
        then
            outer_then = true  # Should run (outer loop completes)
        end
        (outer_then, inner_then)
    end) == (true, false)

    # then clause can access loop variables
    @test (begin
        local last_i = 0
        for i in 1:5
            last_i = i
        then
            last_i = last_i * 10
        end
        last_i
    end) == 50

    # Valued break bypasses then clause
    @test (for i in 1:10
        if i == 3
            break :early_exit
        end
    then
        :completed
    end) == :early_exit

    # Loop that never enters body still runs then
    @test (begin
        local x = 0
        for i in Int[]
            x = 1
        then
            x = 2
        end
        x
    end) == 2
end
