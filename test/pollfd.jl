@unix_only begin
require("testdefs.jl")

pipe_fds = Array(Cint,2)
@test 0 == ccall(:pipe, Cint, (Ptr{Cint},), pipe_fds)

function test_poll(timeout_ms)
    rc = poll_fd(RawFD(pipe_fds[1]), timeout_ms; readable=true)
    produce(rc)
end

function test_timeout(tval)
    tic()
    t = Task(()->test_poll(tval))
    tr = consume(t)
    t_elapsed = toq()

    @test tr == 0

    tdiff = t_elapsed * 1000
    @test tval <= tdiff
end

function test_read(slval)
    tval = slval + 100
    tic()
    t = Task(()->test_poll(tval))

    sleep(slval/1000.0)
    @test 1 == ccall(:write, Csize_t, (Cint, Ptr{Uint8},Csize_t), pipe_fds[2], bytestring("A"), 1)

    tr = consume(t)
    t_elapsed = toq()

    @test tr == UV_READABLE || (UV_READABLE | UV_WRITEABLE)

    dout = Array(Uint8, 1)
    @test 1 == ccall(:read, Csize_t, (Cint, Ptr{Uint8},Csize_t), pipe_fds[1], dout, 1)
    @test dout[1] == int8('A')

    tdiff = t_elapsed * 1000

    @test slval <= tdiff
end

test_timeout(.1)
test_timeout(1)
test_read(.1)
test_read(1)

ccall(:close, Cint, (Cint,), pipe_fds[1])
ccall(:close, Cint, (Cint,), pipe_fds[2])

end
