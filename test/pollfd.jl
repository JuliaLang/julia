# This script does the following
# Sets up n unix pipes
# For the odd pipes, a byte is written to the write end at intervals specified in intvls
# Nothing is written into the even numbered pipes
# Odd numbered pipes are tested for reads
# Even numbered pipes are tested for timeouts
# Writable ends are always tested for writability before a write

n = 20
intvls = [2, .2, .1, .002]

pipe_fds = cell(n)
for i in 1:n
    pipe_fds[i] = Array(Cint, 2)
    @test 0 == ccall(:pipe, Cint, (Ptr{Cint},), pipe_fds[i])
end


function pfd_tst_reads(idx, intvl)
    global ready += 1
    wait(ready_c)
    tic()
    evt = poll_fd(RawFD(pipe_fds[idx][1]), intvl; readable=true, writable=true)
    t_elapsed = toq()
    @test !evt.timedout
    @test evt.readable
    @test !evt.writable

    # println("Expected ", intvl, ", actual ", t_elapsed, ", diff ", t_elapsed - intvl)
    # Assuming that a 2 second buffer is good enough on a modern system
    @test t_elapsed <= (intvl + 1)

    dout = Array(Uint8, 1)
    @test 1 == ccall(:read, Csize_t, (Cint, Ptr{Uint8},Csize_t), pipe_fds[idx][1], dout, 1)
    @test dout[1] == int8('A')
end


function pfd_tst_timeout(idx, intvl)
    global ready += 1
    wait(ready_c)
    tic()
    evt = poll_fd(RawFD(pipe_fds[idx][1]), intvl; readable=true, writable=false)
    @test evt.timedout
    @test !evt.readable
    @test !evt.writable
    t_elapsed = toq()

    @test (intvl <= t_elapsed) && (t_elapsed <= (intvl + 1))
end


# Odd numbers trigger reads, even numbers timeout
for (i, intvl) in enumerate(intvls)
    @sync begin
        global ready = 0
        global ready_c = Condition()
        for i in 1:n
            if isodd(i)
                @async pfd_tst_reads(i, intvl)
            else
                @async pfd_tst_timeout(i, intvl)
            end
        end

        while ready < n
            sleep(0.1)
        end
        ready = 0
        # tickle only the odd ones, but test for writablity for everyone
        for idx in 1:n
            evt = wait(RawFD(pipe_fds[idx][2]); readable=true, writable=true)
            @test !evt.timedout
            @test !evt.readable
            @test evt.writable

            if isodd(idx)
                @test 1 == ccall(:write, Csize_t, (Cint, Ptr{Uint8},Csize_t), pipe_fds[idx][2], bytestring("A"), 1)
            end
        end
        notify(ready_c, all=true)
    end
end


for i in 1:n
    ccall(:close, Cint, (Cint,), pipe_fds[i][1])
    ccall(:close, Cint, (Cint,), pipe_fds[i][2])
end
