# This file is a part of Julia. License is MIT: https://julialang.org/license

# Basic goto tests

function goto_test1()
    @goto a
    return false
    @label a
    return true
end
@test goto_test1()


@test eval(:(@label a)) === nothing

@test Expr(:error, "label \"a\" referenced but not defined") ==
    Meta.lower(@__MODULE__, :(@goto a))

@test Expr(:error, "label \"a\" defined multiple times") ==
    Meta.lower(@__MODULE__, quote
        function goto_test2()
            @goto a
            @label a
            @label a
            return
        end
    end)


@test Expr(:error, "label \"a\" referenced but not defined") ==
    Meta.lower(@__MODULE__, quote
        function goto_test3()
            @goto a
            return
        end
    end)

@test Expr(:error, "misplaced label") ==
    Meta.lower(@__MODULE__, quote
        function goto_test4()
            @goto a
            try
                @label a
            catch
            end
        end
    end)


# test that labels in macros are reassigned if unescaped
macro goto_test5_macro1()
    return :(@label a)
end
macro goto_test5_macro2()
    return :(@goto a)
end
macro goto_test5_macro3()
    return esc(:(@label a))
end

@test Expr(:error, "label \"a\" referenced but not defined") ==
    Meta.lower(@__MODULE__, quote
        function goto_test5_1()
            @goto a
            @goto_test5_macro1
            return
        end
    end)

let e = Meta.lower(@__MODULE__, quote
        function goto_test5_2()
            @goto_test5_macro2
            @label a
            return
        end
    end)
    @test (e::Expr).head === :error
    @test occursin(r"label \"#\d+#a\" referenced but not defined", e.args[1])
end

function goto_test5_3()
    @goto a
    return false
    @goto_test5_macro3
    return true
end
@test goto_test5_3()


@test Expr(:error, "goto from a try/finally block is not permitted around $(@__FILE__):$(3 + @__LINE__)") ==
    Meta.lower(@__MODULE__, quote
        function goto_test6()
            try
                @goto a
            finally
            end
            @label a
            return
        end
    end)


function goto_test6()
    @goto a
    @label a
end

@test goto_test6() === nothing


function goto_test7(x)
    @label a
    if x
        @goto a
    end
end

@test goto_test7(false) === nothing

module GotoMacroTest
    macro goto_test8_macro()
        quote
            function $(esc(:goto_test8))()
                @label a
                @goto a
            end
        end
    end
end

GotoMacroTest.@goto_test8_macro

# issue #15600
function t0_15600(flag)
    flag && @goto return2
    return 1
    @label return2
    return 2
end
@test t0_15600(true) == 2
@test t0_15600(false) == 1
function t1_15600(flag)
    flag || @goto return2
    return 1
    @label return2
    return 2
end
@test t1_15600(true) == 1
@test t1_15600(false) == 2

# issue #15561
function f15561()
    a = @goto crater
    @label crater
end
@test f15561() === nothing

# issue #28077
function foo28077()
    s = 0
    i = 0
    @label L
    i += 1
    s += i
    if i < 10
        @goto L
    end
    return s
end
@test foo28077() == 55

# Block break tests (@label name expr with break name val)

# Basic block break with value
function block_break_test1()
    @label myblock begin
        for i in 1:10
            if i > 5
                break myblock i * 2
            end
        end
        0
    end
end
@test block_break_test1() == 12

# Block break without value (returns nothing)
function block_break_test2()
    result = @label myblock begin
        for i in 1:10
            if i > 5
                break myblock
            end
        end
        42
    end
    result
end
@test block_break_test2() === nothing

# Block break from nested loops
function block_break_test3()
    @label outer begin
        for i in 1:5
            for j in 1:5
                if i * j > 10
                    local result = (i, j)
                    break outer result
                end
            end
        end
        (0, 0)
    end
end
@test block_break_test3() == (3, 4)

# Block break with no break executed (returns body value)
function block_break_test4()
    @label myblock begin
        x = 1 + 2
        x * 3
    end
end
@test block_break_test4() == 9

# Block break in tail position
function block_break_test5(n)
    @label myblock begin
        if n > 0
            break myblock n * 2
        end
        -1
    end
end
@test block_break_test5(5) == 10
@test block_break_test5(-1) == -1

# Error: cannot use @goto to jump to @label block
@test Expr(:error, "cannot use @goto to jump to @label block \"myblock\"") ==
    Meta.lower(@__MODULE__, quote
        function block_break_test_error1()
            @goto myblock
            @label myblock begin
                42
            end
        end
    end)

# Break with value DOES work for labeled loops (returns the value)
function block_break_value_from_loop()
    @label loop_exit while true
        break loop_exit 42
    end
end
@test block_break_value_from_loop() == 42

# Error: duplicate label (symbolicblock and symboliclabel)
@test Expr(:error, "label \"foo\" defined multiple times") ==
    Meta.lower(@__MODULE__, quote
        function block_break_test_error3()
            @label foo begin
                42
            end
            @label foo
        end
    end)

# Nested block breaks
function block_break_nested()
    @label outer begin
        x = @label inner begin
            if true
                break inner 10
            end
            0
        end
        x + 5
    end
end
@test block_break_nested() == 15

# Block break with computed value
function block_break_computed(arr)
    @label search begin
        for (i, v) in enumerate(arr)
            if v > 100
                break search i => v
            end
        end
        nothing
    end
end
@test block_break_computed([1, 50, 150, 200]) == (3 => 150)
@test block_break_computed([1, 2, 3]) === nothing

# Labeled continue tests
function labeled_continue_test1()
    result = Int[]
    @label outer for i in 1:5
        for j in 1:5
            if j > 2
                continue outer  # skip to next i
            end
            push!(result, i * 10 + j)
        end
    end
    result
end
@test labeled_continue_test1() == [11, 12, 21, 22, 31, 32, 41, 42, 51, 52]

# Labeled break from outer loop
function labeled_break_loop_test()
    result = Int[]
    @label outer for i in 1:5
        for j in 1:5
            if i == 3 && j == 2
                break outer  # exit both loops
            end
            push!(result, i * 10 + j)
        end
    end
    result
end
@test labeled_break_loop_test() == [11, 12, 13, 14, 15, 21, 22, 23, 24, 25, 31]

# Labeled break with value from loop
function labeled_break_value_test()
    @label outer for i in 1:10
        for j in 1:10
            if i * j > 50
                local result = (i, j, i * j)
                break outer result
            end
        end
    end
end
@test labeled_break_value_test() == (6, 9, 54)

# While loop with labeled continue
function while_labeled_continue()
    result = Int[]
    i = 0
    @label outer while i < 5
        i += 1
        j = 0
        while j < 5
            j += 1
            if j > 2
                continue outer
            end
            push!(result, i * 10 + j)
        end
    end
    result
end
@test while_labeled_continue() == [11, 12, 21, 22, 31, 32, 41, 42, 51, 52]

# Nested labeled blocks with loops
function nested_labeled_loops()
    @label outer for i in 1:3
        x = @label inner for j in 1:3
            if j == 2
                break inner j * 100
            end
        end
        if x > 100
            break outer x + i
        end
    end
end
@test nested_labeled_loops() == 201

# Ternary operator disambiguation: `cond ? break : val` should work
# The space after `:` indicates ternary, not labeled break
function ternary_break_test()
    for i in 1:10
        x = i > 5 ? break : i * 2
        @test x == i * 2
    end
end
ternary_break_test()

function ternary_continue_test()
    result = Int[]
    for i in 1:10
        i > 5 ? continue : push!(result, i)
    end
    result
end
@test ternary_continue_test() == [1, 2, 3, 4, 5]

# Also test labeled break/continue in ternary still works
function ternary_labeled_break()
    @label outer for i in 1:5
        for j in 1:5
            # labeled break in ternary condition
            i == 3 && j == 2 ? break outer : nothing
        end
    end
    42
end
@test ternary_labeled_break() == 42

function ternary_labeled_continue()
    result = Int[]
    @label outer for i in 1:5
        for j in 1:5
            # labeled continue in ternary condition
            j > 2 ? continue outer : push!(result, i * 10 + j)
        end
    end
    result
end
@test ternary_labeled_continue() == [11, 12, 21, 22, 31, 32, 41, 42, 51, 52]

# Tests for combined labeled and ordinary break/continue
# Ensure that ordinary break/continue still work inside labeled loops

# Ordinary continue inside a labeled loop
function combined_ordinary_continue()
    result = Int[]
    @label outer for i in 1:5
        if i == 3
            continue  # ordinary continue - should skip to next i
        end
        push!(result, i)
    end
    result
end
@test combined_ordinary_continue() == [1, 2, 4, 5]

# Ordinary break inside a labeled loop
function combined_ordinary_break()
    result = Int[]
    @label outer for i in 1:5
        if i == 3
            break  # ordinary break - should exit the loop
        end
        push!(result, i)
    end
    result
end
@test combined_ordinary_break() == [1, 2]

# Both labeled and ordinary continue in the same loop
function combined_labeled_and_ordinary_continue()
    result = Int[]
    @label outer for i in 1:4
        for j in 1:4
            if j == 1
                continue  # ordinary continue - skip to next j
            end
            if j == 2
                continue outer  # labeled continue - skip to next i
            end
            push!(result, i * 10 + j)
        end
        push!(result, i * 100)  # should not be reached due to continue outer at j==2
    end
    result
end
@test combined_labeled_and_ordinary_continue() == Int64[]

# Both labeled and ordinary break in the same nested loop
function combined_labeled_and_ordinary_break()
    result = Int[]
    @label outer for i in 1:4
        for j in 1:4
            push!(result, i * 10 + j)
            if j == 2 && i == 1
                break  # ordinary break - exit inner loop only
            end
            if j == 1 && i == 2
                break outer  # labeled break - exit outer loop
            end
        end
        push!(result, i * 100)
    end
    result
end
# i=1: j=1->11, j=2->12, break inner, 100
# i=2: j=1->21, break outer
@test combined_labeled_and_ordinary_break() == [11, 12, 100, 21]

# Multiple nested loops with mixed break/continue
function combined_triple_nested()
    result = Int[]
    @label outer for i in 1:3
        @label middle for j in 1:3
            for k in 1:3
                if k == 2
                    continue  # skip to next k
                end
                if k == 3 && j == 1
                    continue middle  # skip to next j
                end
                if k == 3 && j == 2
                    continue outer  # skip to next i
                end
                push!(result, i * 100 + j * 10 + k)
            end
            push!(result, -(i * 10 + j))  # middle loop epilogue
        end
        push!(result, i * 1000)  # outer loop epilogue
    end
    result
end
# i=1, j=1: k=1->111, k=2 skipped, k=3 continue middle
# i=1, j=2: k=1->121, k=2 skipped, k=3 continue outer
# i=2, j=1: k=1->211, k=2 skipped, k=3 continue middle
# i=2, j=2: k=1->221, k=2 skipped, k=3 continue outer
# i=3, j=1: k=1->311, k=2 skipped, k=3 continue middle
# i=3, j=2: k=1->321, k=2 skipped, k=3 continue outer
@test combined_triple_nested() == [111, 121, 211, 221, 311, 321]

# Anonymous @label block (1-arg form) inside labeled loop
# break should exit the inner @label block, not the loop
function anon_block_in_labeled_loop()
    result = Int[]
    @label outer for i in 1:3
        x = @label begin
            if i == 2
                break _ 100  # break out of anonymous block with value
            end
            i * 10
        end
        push!(result, x)
        if i == 2 && x == 100
            continue  # ordinary continue - should work
        end
        push!(result, x + 1)
    end
    result
end
@test anon_block_in_labeled_loop() == [10, 11, 100, 30, 31]

# break / break _ / break _ nothing are all equivalent in @label blocks
function break_equivalence_test()
    @test (@label begin; break; error("unreachable"); end) === nothing
    @test (@label begin; break _; error("unreachable"); end) === nothing
    @test (@label begin; break _ nothing; error("unreachable"); end) === nothing
    @test (@label begin; break _ 42; error("unreachable"); end) == 42
end
break_equivalence_test()

# @label block inside loop: break exits the @label block, not the loop
function break_exits_innermost_label()
    result = Int[]
    for i in 1:3
        @label begin
            if i == 2
                break  # exits @label block, NOT the for loop
            end
            push!(result, i)
        end
    end
    result
end
@test break_exits_innermost_label() == [1, 3]

# Nested @label blocks (inner shadows outer)
function nested_anon_label_blocks()
    x = @label begin
        y = @label begin
            break _ 10
        end
        break _ y + 5
    end
    x
end
@test nested_anon_label_blocks() == 15

# @label _ and @label _ body are errors
@test_throws "use `@label expr` for anonymous blocks; `@label _` is not allowed" @eval @label _
@test_throws "use `@label expr` for anonymous blocks; `@label _ expr` is not allowed" @eval @label _ begin
    42
end

# Plain break inside named @label block is disallowed
@test Expr(:error, "plain `break` inside `@label blk` block is disallowed; use `break blk` to exit the block") ==
    Meta.lower(@__MODULE__, quote
        function break_in_named_block()
            for i in 1:3
                @label blk begin
                    break
                end
            end
        end
    end)

# continue inside anonymous @label block is disallowed
@test Expr(:error, "`continue` inside an anonymous `@label` block is not allowed") ==
    Meta.lower(@__MODULE__, quote
        function continue_in_label_block()
            for i in 1:3
                @label begin
                    continue
                end
            end
        end
    end)
