# This file is a part of Julia. License is MIT: http://julialang.org/license

# This file is a part of Julia. License is MIT: http://julialang.org/license

function select_block_test(t1, t2, t3, t4)
    c1 = Channel{Symbol}(1)
    c2 = Channel{Int}(1)
    c3 = Channel(1)

    put!(c3,1)

    @schedule begin
        sleep(t1)
        put!(c1,:a)
    end

    @schedule begin
        sleep(t2)
        put!(c2,1)
    end

    @schedule begin
        sleep(t3)
        take!(c3)
    end

    task = @schedule begin
        sleep(t4)
        :task_done
    end

    @select begin
        if c1 |> x
            "Got $x from c1"
        elseif c2
            "Got a message from c2"
        elseif c3 <| :write_test
            "Wrote to c3"
        elseif task |> z
            "Task finished with $z"
        end
    end
end

@test select_block_test(.5, 1, 1, 1) == "Got a from c1"
@test select_block_test(1, .5, 1, 1) == "Got a message from c2"
@test select_block_test(1, 1, .5, 1) == "Wrote to c3"
@test select_block_test(1, 1, 1, .5) == "Task finished with task_done"

function select_nonblock_test(test)
    c = Channel(1)
    c2 = Channel(1)
    put!(c2, 1)
    if test == :take
        put!(c, 1)
    elseif test == :put
        take!(c2)
    elseif test == :default
    end

    @select begin
        if c |> x
            "Got $x from c"
        elseif c2 <| 1
            "Wrote to c2"
        else
            "Default case"
        end
    end
end

@test select_nonblock_test(:take) == "Got 1 from c"
@test select_nonblock_test(:put) == "Wrote to c2"
@test select_nonblock_test(:default) == "Default case"

let c = Channel(1)
    @test select([(:take, c), (:put, c, :msg)], true) == (2, :msg)
    @test select([(:take, c), (:put, c, :msg)], true) == (1, :msg)
end

let c = Channel(1)
    @test select([(:take, c)], false) == (0, nothing)
end
