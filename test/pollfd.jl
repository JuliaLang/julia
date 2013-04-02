require("testdefs.jl")

t0 = int64(time() * 1000)


pipe_fds = Array(Cint,2)
@test 0 == ccall(:pipe, Cint, (Ptr{Cint},), pipe_fds)

function test_poll(timeout_ms)
    rc = poll_fd(OS_FD(pipe_fds[1]), UV_READABLE, timeout_ms)
    produce(rc)
end


function test_timeout(tval)
    println("Testing timeout event on poll_fd...")
    t1 = int64(time() * 1000)
    t = Task(()->test_poll(tval))
    tr = consume(t)
    t2 = int64(time() * 1000)

    @test tr == (:timeout, 0)

    tdiff = t2-t1
    @test tval <= tdiff

    if (tdiff > (tval + 50))
        @printf("WARNING: poll_fd timeout took much longer than expected. Expected[%d], Actual[%d]\n", tval, tdiff)
    end
end

function test_read(slval)
    println("Testing read event on poll_fd...")

    tval = slval + 100
    t1 = int64(time() * 1000)
    t = Task(()->test_poll(tval))

    @time sleep(slval/1000.0)
    @test 1 == ccall(:write, Csize_t, (Cint, Ptr{Uint8},Csize_t), pipe_fds[2], bytestring("A"), 1)

    tr = consume(t)
    t2 = int64(time() * 1000)

    @test tr[1] == :poll
    @test tr[2] == 0
    @test tr[3] == UV_READABLE || (UV_READABLE | UV_WRITEABLE)

    dout = Array(Uint8, 1)
    @test 1 == ccall(:read, Csize_t, (Cint, Ptr{Uint8},Csize_t), pipe_fds[1], dout, 1)
    @test dout[1] == int8('A')

    tdiff = t2-t1

#    @test slval <= tdiff
    
    if (tdiff > (slval + 50))
        @printf("WARNING: poll_fd with read event took much longer than expected. Expected[%d], Actual[%d]\n", slval, tdiff)
    elseif (slval > tdiff )
        @printf("FAILED: sleep() in poll_fd with read event exited earlier than expected. Expected[%d], Actual[%d]\n", slval, tdiff)
    end
    
    
end


test_timeout(100)
test_timeout(1000)
test_read(100)
test_read(1000)
