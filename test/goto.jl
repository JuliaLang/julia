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


@test Expr(:error, "goto from a try/finally block is not permitted") ==
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
