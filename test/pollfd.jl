# This file is a part of Julia. License is MIT: http://julialang.org/license

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
    @test 0 ==
        @windows ? begin
            pipe_fds[i] = Array(Libc.WindowsRawSocket, 2)
            ccall(:wsasocketpair, Cint, (Cint, Cuint, Cint, Ptr{Libc.WindowsRawSocket}), 1, 1, 6, pipe_fds[i])
        end : begin
            pipe_fds[i] = Array(RawFD, 2)
            ccall(:pipe, Cint, (Ptr{RawFD},), pipe_fds[i])
        end
end

function pfd_tst_reads(idx, intvl)
    global ready += 1
    wait(ready_c)
    tic()
    evt = poll_fd(pipe_fds[idx][1], intvl; readable=true, writable=true)
    t_elapsed = toq()
    @test !evt.timedout
    @test evt.readable
    @test @windows ? evt.writable : !evt.writable

    # println("Expected ", intvl, ", actual ", t_elapsed, ", diff ", t_elapsed - intvl)
    # Assuming that a 2 second buffer is good enough on a modern system
    @test t_elapsed <= (intvl + 1)

    dout = Array(UInt8, 1)
    @test 1 == @windows ? (
        ccall(:recv, stdcall, Cint, (Ptr{Void}, Ptr{UInt8}, Cint, Cint), pipe_fds[idx][1], dout, 1, 0)
    ) : (
        ccall(:read, Csize_t, (Cint, Ptr{UInt8}, Csize_t), pipe_fds[idx][1], dout, 1)
    )
    @test dout[1] == Int8('A')
end


function pfd_tst_timeout(idx, intvl)
    global ready += 1
    wait(ready_c)
    tic()
    evt = poll_fd(pipe_fds[idx][1], intvl; readable=true, writable=false)
    @test evt.timedout
    @test !evt.readable
    @test !evt.writable
    t_elapsed = toq()

    @unix_only @test (intvl <= t_elapsed) # TODO: enable this test on windows when the libuv version is bumped
    @test (t_elapsed <= (intvl + 1))
end


# Odd numbers trigger reads, even numbers timeout
for (i, intvl) in enumerate(intvls)
    @sync begin
        global ready = 0
        global ready_c = Condition()
        for idx in 1:n
            if isodd(idx)
                @async pfd_tst_reads(idx, intvl)
            else
                @async pfd_tst_timeout(idx, intvl)
            end
        end

        while ready < n
            sleep(0.1)
        end
        ready = 0
        # tickle only the odd ones, but test for writablity for everyone
        for idx in 1:n
            event = poll_fd(pipe_fds[idx][2], 0.001; readable=true, writable=true)
            @test !event.timedout
            @test !event.readable
            @test event.writable

            if isodd(idx)
                @test 1 == @windows ? (
                   ccall(:send, stdcall, Cint, (Ptr{Void}, Ptr{UInt8}, Cint, Cint), pipe_fds[idx][2], "A", 1, 0)
                ) : (
                   ccall(:write, Csize_t, (Cint, Ptr{UInt8}, Csize_t), pipe_fds[idx][2], "A", 1)
                )
            end
        end
        notify(ready_c, all=true)
    end
end

for i in 1:n
    for j = 1:2
        @test 0 == @windows ? (
            ccall(:closesocket, stdcall, Cint, (Ptr{Void},), pipe_fds[i][j])
        ) : (
            ccall(:close, Cint, (Cint,), pipe_fds[i][j])
        )
    end
end
