# This file is a part of Julia. License is MIT: http://julialang.org/license

# This script does the following
# Sets up n unix pipes
# For the odd pipes, a byte is written to the write end at intervals specified in intvls
# Nothing is written into the even numbered pipes
# Odd numbered pipes are tested for reads
# Even numbered pipes are tested for timeouts
# Writable ends are always tested for writability before a write

n = 20
intvls = [2, .2, .1, .005]

pipe_fds = Vector{Any}(n)
for i in 1:n
    @static if is_windows()
        pipe_fds[i] = Array{Libc.WindowsRawSocket}(2)
        0 == ccall(:wsasocketpair, Cint, (Cint, Cuint, Cint, Ptr{Libc.WindowsRawSocket}), 1, 1, 6, pipe_fds[i]) || error(Libc.FormatMessage())
    else
        pipe_fds[i] = Array{RawFD}(2)
        @test 0 == ccall(:pipe, Cint, (Ptr{RawFD},), pipe_fds[i])
    end
end

function pfd_tst_reads(idx, intvl)
    global ready += 1
    wait(ready_c)
    tic()
    start_evt2 = Condition()
    evt2 = @async (notify(start_evt2); poll_fd(pipe_fds[idx][1], intvl; readable=true, writable=false))
    wait(start_evt2); yield() # make sure the async poll_fd is pumping events
    evt = poll_fd(pipe_fds[idx][1], intvl; readable=true, writable=false)
    t_elapsed = toq()
    @test !evt.timedout
    @test evt.readable
    @test !evt.writable
    @test evt === wait(evt2)

    # println("Expected ", intvl, ", actual ", t_elapsed, ", diff ", t_elapsed - intvl)
    # Disabled since this assertion fails randomly, notably on build VMs (issue #12824)
    # @test t_elapsed <= (intvl + 1)

    dout = Array{UInt8}(1)
    @static if is_windows()
        1 == ccall(:recv, stdcall, Cint, (Ptr{Void}, Ptr{UInt8}, Cint, Cint), pipe_fds[idx][1], dout, 1, 0) || error(Libc.FormatMessage())
    else
        @test 1 == ccall(:read, Csize_t, (Cint, Ptr{UInt8}, Csize_t), pipe_fds[idx][1], dout, 1)
    end
    @test dout[1] == Int8('A')
end


function pfd_tst_timeout(idx, intvl)
    global ready += 1
    wait(ready_c)
    tic()
    start_evt2 = Condition()
    evt2 = @async (notify(start_evt2); poll_fd(pipe_fds[idx][1], intvl; readable=true, writable=false))
    wait(start_evt2); yield() # make sure the async poll_fd is pumping events
    evt = poll_fd(pipe_fds[idx][1], intvl; readable=true, writable=false)
    @test evt.timedout
    @test !evt.readable
    @test !evt.writable
    @test evt === wait(evt2)
    t_elapsed = toq()

    # Disabled since these assertions fail randomly, notably on build VMs (issue #12824)
    # @test intvl <= t_elapsed
    # @test t_elapsed <= (intvl + 1)
end


# Odd numbers trigger reads, even numbers timeout
for (i, intvl) in enumerate(intvls)
    @sync begin
        global ready = 0
        global ready_c = Condition()
        t = Vector{Task}(n)
        for idx in 1:n
            if isodd(idx)
                t[idx] = @async pfd_tst_reads(idx, intvl)
            else
                t[idx] = @async pfd_tst_timeout(idx, intvl)
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
                @static if is_windows()
                    1 == ccall(:send, stdcall, Cint, (Ptr{Void}, Ptr{UInt8}, Cint, Cint), pipe_fds[idx][2], "A", 1, 0) || error(Libc.FormatMessage())
                else
                    @test 1 == ccall(:write, Csize_t, (Cint, Ptr{UInt8}, Csize_t), pipe_fds[idx][2], "A", 1)
                end
            end
        end
        notify(ready_c, all=true)
        for idx in 1:n
            wait(t[idx])
        end
    end
end

for i in 1:n
    for j = 1:2
        @static if is_windows()
            0 == ccall(:closesocket, stdcall, Cint, (Ptr{Void},), pipe_fds[i][j]) || error(Libc.FormatMessage())
        else
            @test 0 == ccall(:close, Cint, (Cint,), pipe_fds[i][j])
        end
    end
end

# issue #12473
# make sure 1-shot timers work
let a = []
    Timer(t->push!(a, 1), 0.01, 0)
    sleep(0.2)
    @test a == [1]
end
