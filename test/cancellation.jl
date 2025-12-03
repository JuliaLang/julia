const collatz_code = quote
    collatz(n) = (n & 1) == 1 ? (3n + 1) : (n÷2)
    function find_collatz_counterexample()
        i = 1
        while true
            j = i
            while true
                @Base.cancel_check
                j = collatz(j)
                j == 1 && break
                j == i && error("$j is a collatz counterexample")
            end
            i += 1
        end
    end
    function find_collatz_counterexample_inner()
        i = 1
        while true
            j = i
            while true
                j = collatz(j)
                j == 1 && break
                j == i && return j
            end
            i += 1
        end
    end
    function find_collatz_counterexample2()
        @Base.cancel_check
        return find_collatz_counterexample_inner()
    end
end
eval(collatz_code)

@testset "cancellation" begin
    # Test cancellation point for a task that has never started running
    @test_throws Base.CancellationRequest wait(@task nothing)


    # Simple cancellation of `sleep`
    t = @async sleep(10000)
    yield(); yield(); yield() # Give the task a chance to start
    Base.cancel!(t)
    @test_throws Base.CancellationRequest wait(t)

    # Simple cancellation of compute-bound function
    t = @Threads.spawn(find_collatz_counterexample())
    yield(); yield(); yield() # Give the task a chance to start
    Base.cancel!(t)
    @test_throws Base.CancellationRequest wait(t)

    # Test cancellation of @sync
    t = @async @sync begin
        @async sleep(10000)
        @async sleep(10000)
    end
    yield(); yield(); yield() # Give the task a chance to start
    Base.cancel!(t)
    @test_throws Base.CompositeException wait(t)
end

@testset "^C" begin
    # This exercises ^C needing to preempt a compute-bound task
    test_code = :(try;
        @sync ((@async sleep(10000)); @async find_collatz_counterexample())
    catch e
        println(typeof(e))
    end)
    @test read(`$(Base.julia_cmd()) -e '$(string(collatz_code)); $test_code'`, String) == "CompositeException"

    # Make sure that preemption doesn't cause problems if all tasks are blocked
    test_code = :(try;
        @sync ((@async sleep(10000)); @async sleep(10000))
    catch e
        println(typeof(e))
    end)
    @test read(`$(Base.julia_cmd()) -e '$(string(collatz_code)); $test_code'`, String) == "CompositeException"
end
