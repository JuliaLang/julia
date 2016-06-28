# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "goto" begin
function goto_test1()
    @goto a
    return false
    @label a
    return true
end
@test goto_test1()


@test_throws ErrorException eval(
    quote
        function goto_test2()
            @goto a
            @label a
            @label a
            return
        end
    end)


@test_throws ErrorException eval(
    quote
        function goto_test3()
            @goto a
            return
        end
    end)


@test_throws ErrorException eval(
    quote
        function goto_test4()
            @goto a
            try
                @label a
            catch
            end
        end
    end)


# test that labels in macros are reassigned
macro goto_test5_macro()
    @label a
end

@test_throws ErrorException eval(
    quote
        function goto_test5()
            @goto a
            @goto_test5_macro
            return
        end
    end)


@test_throws ErrorException eval(
    quote
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

@test goto_test6() == nothing


function goto_test7(x)
    @label a
    if x
        @goto a
    end
end

@test goto_test7(false) == nothing

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

end
