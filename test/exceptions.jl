# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base: catch_stack

@testset "Basic exception stack handling" begin
    # Exiting the catch block normally pops the exception
    try
        error("A")
    catch
        @test length(catch_stack()) == 1
    end
    @test length(catch_stack()) == 0
    # Exiting via a finally block does not pop the exception
    try
        try
            error("A")
        finally
            @test length(catch_stack()) == 1
        end
    catch
        @test length(catch_stack()) == 1
    end
    # The combined try-catch-finally form obeys the same rules as above
    try
        error("A")
    catch
        @test length(catch_stack()) == 1
    finally
        @test length(catch_stack()) == 0
    end
    @test length(catch_stack()) == 0
    # Errors are pushed onto the stack according to catch block nesting
    try
        error("RootCause")
    catch
        @test length(catch_stack()) == 1
        try
            error("B")
        catch
            stack = catch_stack()
            @test length(stack) == 2
            @test stack[1][1].msg == "RootCause"
            @test stack[2][1].msg == "B"
        end
        # Stack pops correctly
        stack = catch_stack()
        @test length(stack) == 1
        @test stack[1][1].msg == "RootCause"
    end
end

@testset "Exception stack lowering special cases" begin
    # try block in value position
    val = try
        error("A")
    catch
        @test length(catch_stack()) == 1
        1
    end
    @test val == 1
    function test_exc_stack_tailpos()
        # try block in tail position
        try
            error("A")
        catch
            length(catch_stack())
        end
    end
    @test test_exc_stack_tailpos() == 1
    @test length(catch_stack()) == 0
end

@testset "Exception stacks - early exit from try or catch" begin
    # Exiting a catch block early with normal control flow — break, continue,
    # return, goto — will result in popping of the exception stack.
    function test_exc_stack_catch_return()
        try
            error("A")
        catch
            @test length(catch_stack()) == 1
            return
        end
    end
    test_exc_stack_catch_return()

    for i=1:1
        try
            error("A")
        catch
            @test length(catch_stack()) == 1
            break
        end
    end
    # Also test try-break-finally forms here. See #31766
    for i=1:1
        try
            error("A")
        catch
            @test length(catch_stack()) == 1
            break
        finally
            @test length(catch_stack()) == 0
        end
    end
    @test length(catch_stack()) == 0

    for i=1:1
        try
            error("A")
        catch
            @test length(catch_stack()) == 1
            continue
        end
    end
    for i=1:1
        try
            error("A")
        catch
            @test length(catch_stack()) == 1
            continue
        finally
            @test length(catch_stack()) == 0
        end
    end
    @test length(catch_stack()) == 0

    try
        error("A")
    catch
        @test length(catch_stack()) == 1
        @goto outofcatch
    end
    @label outofcatch
    try
        error("A")
    catch
        @test length(catch_stack()) == 1
        @goto outofcatch2
    finally
        @test length(catch_stack()) == 0
    end
    @label outofcatch2
    @test length(catch_stack()) == 0

    # Exiting from a try block in various ways should not affect the exception
    # stack state.
    try
        error("ExceptionInOuterTry")
    catch
        @test length(catch_stack()) == 1
        function test_exc_stack_try_return()
            try
                return
            catch
            end
        end
        test_exc_stack_try_return()
        for i=1:1
            try
                break
            catch
            end
        end
        for i=1:1
            try
                continue
            catch
            end
        end
        try
            @goto outoftry
        catch
        end
        @label outoftry
        @test length(catch_stack()) == 1
        @test catch_stack()[1][1] == ErrorException("ExceptionInOuterTry")
    end
end

@testset "Deep exception stacks" begin
    # Generate deep exception stack with recursive handlers Note that if you
    # let this overflow the program stack (not the exception stack) julia will
    # crash. See #28577
    function test_exc_stack_deep(n)
        n != 1 || error("RootCause")
        try
            test_exc_stack_deep(n-1)
        catch
            error("n==$n")
        end
    end
    @test try
        test_exc_stack_deep(100)
    catch
        @test catch_stack()[1][1] == ErrorException("RootCause")
        length(catch_stack())
    end == 100
    @test length(catch_stack()) == 0
end

@testset "Exception stacks and Tasks" begin
    # Task switching should not affect exception state. See #12485.
    try
        error("A")
    catch
        t = @task try
            error("B")
        catch exc
            exc
        end
        yield(t)
        @test t.state == :done
        @test t.result == ErrorException("B")
        # Task exception state is preserved around task switches
        @test length(catch_stack()) == 1
        @test catch_stack()[1][1] == ErrorException("A")
    end
    @test length(catch_stack()) == 0
    # test rethrow() rethrows correct state
    bt = []
    try
        try
            error("A")
        catch
            bt = catch_backtrace()
            t = @task try
                error("B")
            catch exc
                exc
            end
            yield(t)
            @test t.state == :done
            @test t.result == ErrorException("B")
            @test bt == catch_backtrace()
            rethrow()
        end
    catch exc
        @test exc == ErrorException("A")
        @test bt == catch_backtrace()
    end
    @test length(catch_stack()) == 0
    # test rethrow with argument
    bt = []
    try
        try
            error("A")
        catch
            t = @task try
                error("B")
            catch exc
                exc
            end
            yield(t)
            @test t.state == :done
            @test t.result == ErrorException("B")
            bt = catch_backtrace()
            rethrow(ErrorException("C"))
        end
    catch exc
        @test exc == ErrorException("C")
        @test bt == catch_backtrace()
    end
    @test length(catch_stack()) == 0
    # Exception stacks on other tasks
    t = @task try
        error("A")
    catch
        error("B")
    end
    yield(t)
    @test t.state == :failed
    @test t.result == ErrorException("B")
    @test catch_stack(t, include_bt=false) == [ErrorException("A"), ErrorException("B")]
    # Exception stacks for tasks which never get the chance to start
    t = @task nothing
    @test (try
        @async Base.throwto(t, ErrorException("expected"))
        wait(t)
    catch e
        e
    end).task.exception == ErrorException("expected")
    @test length(catch_stack(t)) == 1
    @test length(catch_stack(t)[1][2]) > 0 # backtrace is nonempty
    # Exception stacks should not be accessed on concurrently running tasks
    t = @task ()->nothing
    @test_throws ErrorException("Inspecting the exception stack of a task which might "*
                                "be running concurrently isn't allowed.") catch_stack(t)
end

@testset "rethrow" begin
    @test try
        rethrow()
    catch exc
        exc
    end == ErrorException("rethrow() not allowed outside a catch block")
    @test try
        rethrow(ErrorException("A"))
    catch exc
        exc
    end == ErrorException("rethrow(exc) not allowed outside a catch block")
end
