using Test

@testset "Exception stack nesting" begin
    # Basic exception stack handling
    try
        error("A")
    catch
        @test length(catch_stack()) == 1
    end
    @test length(catch_stack()) == 0
    try
        try
            error("A")
        finally
            @test length(catch_stack()) == 1
        end
    catch
        @test length(catch_stack()) == 1
    end
    # Errors stack up
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
    # Lowering - value position
    val = try
        error("A")
    catch
        @test length(catch_stack()) == 1
        1
    end
    @test val == 1
    function test_exc_stack_tailpos()
        # exercise lowering code path for tail position
        try
            error("A")
        catch
            length(catch_stack())
        end
    end
    @test test_exc_stack_tailpos() == 1
    @test length(catch_stack()) == 0
end

@testset "Exception stacks and gotos" begin
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
    for i=1:1
        try
            error("A")
        catch
            @test length(catch_stack()) == 1
            continue
        end
    end
    try
        error("A")
    catch
        @test length(catch_stack()) == 1
        @goto outofcatch
    end
    @label outofcatch
    @test length(catch_stack()) == 0
end

@testset "Deep exception stacks" begin
    function test_exc_stack_deep(n)
        # Generate deep exception stack with recursive handlers
        # Note that if you let this overflow the program stack (not the exception
        # stack) julia will crash. See #28577
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
        length(catch_stack())
    end == 100
    @test length(catch_stack()) == 0
end

@testset "Exception stacks and Tasks" begin
    # See #12485
    try
        error("A")
    catch
        t = @task try
            error("B")
        catch ex
            ex
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
            catch ex
                ex
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
            catch ex
                ex
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
    @test try
        @async Base.throwto(t, ErrorException("expected"))
        wait(t)
    catch e
        e
    end == ErrorException("expected")
    @test length(catch_stack(t)) == 1
    @test length(catch_stack(t)[1][2]) > 0 # backtrace is nonempty
end

@testset "rethrow" begin
    @test try
        rethrow()
    catch ex
        ex
    end == ErrorException("rethrow() not allowed outside a catch block")
    @test try
        rethrow(ErrorException("A"))
    catch ex
        ex
    end == ErrorException("rethrow(exc) not allowed outside a catch block")
end
