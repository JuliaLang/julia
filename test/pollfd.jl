@unix_only begin
require("testdefs.jl")

pipe_fds = Array(Cint,2)
@test 0 == ccall(:pipe, Cint, (Ptr{Cint},), pipe_fds)

function test_poll_fd(timeout_s)
    rc = poll_fd(RawFD(pipe_fds[1]), timeout_s; readable=true)
    produce(rc)
end

function test_timeout_fd(tval)
    tic()
    t = Task(()->test_poll_fd(tval))
    tr = consume(t)
    t_elapsed = toq()

    @test tr.timedout

    @test tval <= t_elapsed
end

function test_read(slval)
    tval = slval * 1.1
    tic()
    t = Task(()->test_poll_fd(tval))

    sleep(slval)
    @test 1 == ccall(:write, Csize_t, (Cint, Ptr{UInt8},Csize_t), pipe_fds[2], bytestring("A"), 1)

    tr = consume(t)
    t_elapsed = toq()

    @test isreadable(tr) || iswritable(tr)

    dout = Array(UInt8, 1)
    @test 1 == ccall(:read, Csize_t, (Cint, Ptr{UInt8},Csize_t), pipe_fds[1], dout, 1)
    @test dout[1] == int8('A')

    @test slval <= t_elapsed
end

test_timeout_fd(.1)
test_timeout_fd(1)
test_read(.1)
test_read(1)

ccall(:close, Cint, (Cint,), pipe_fds[1])
ccall(:close, Cint, (Cint,), pipe_fds[2])

end
