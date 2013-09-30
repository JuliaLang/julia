@unix_only begin
require("testdefs.jl")

# This script does the following
# Sets up n unix pipes
# For the odd pipes, a byte is written to the write end at times defined in intvls 
# Nothing is written into the even numbered pipes
# Odd numbered pipes are tested for reads
# Even numbered pipes are tested for timeouts
# Writable ends are always tested for writability before a write

n = 20
intvls = [0.1, 0.1, 1.0, 2.0]  # NOTE: The first interval is just used to let the readers/writers sort of synchnronize.

pipe_fds = cell(n)
for i in 1:n
    pipe_fds[i] = Array(Cint, 2)
    @test 0 == ccall(:pipe, Cint, (Ptr{Cint},), pipe_fds[i])
end


function pfd_tst_reads(idx)
    for (i, intvl) in enumerate(intvls)
        tic()
        evt = poll_fd(RawFD(pipe_fds[idx][1]), intvl * 10.0; readable=true, writable=true)
        t_elapsed = toq()
        @test isreadable(evt) && !iswritable(evt) && !(evt.timedout)
        
        # ignore the first one, everyone is just getting setup and synchronized
        if i > 1
#            println("i ", i, ", Expected ", intvl, ", actual ", t_elapsed, ", diff ", t_elapsed - intvl)
            # Assuming that a 200 millisecond buffer is good enough on a modern system
            @test t_elapsed <= (intvl + 0.2)
        end
        
        
        dout = Array(Uint8, 1)
        @test 1 == ccall(:read, Csize_t, (Cint, Ptr{Uint8},Csize_t), pipe_fds[idx][1], dout, 1)
        @test dout[1] == int8('A')
    end    
end


function pfd_tst_timeout(idx)
    for intvl in intvls
        tic()
        evt = poll_fd(RawFD(pipe_fds[idx][1]), intvl; readable=true, writable=true)
        @test !isreadable(evt) && !iswritable(evt) && evt.timedout
        t_elapsed = toq()
        
        @test (intvl <= t_elapsed) && (t_elapsed <= (intvl + 0.2)) 
    end
end


# Odd numbers trigger reads, even numbers timeout
@sync begin
    for i in 1:n
        if isodd(i)
            @async pfd_tst_reads(i)
        else
            @async pfd_tst_timeout(i)
        end
    end
    
    for (i, intvl) in enumerate(intvls)
        sleep(intvl)
        # tickle only the odd ones, but test for writablity for everyone
        for idx in 1:n
            evt = poll_fd(RawFD(pipe_fds[idx][2]), 0.0; readable=true, writable=true)
            @test !isreadable(evt) && iswritable(evt) && !(evt.timedout)
            
            if isodd(idx)
                @test 1 == ccall(:write, Csize_t, (Cint, Ptr{Uint8},Csize_t), pipe_fds[idx][2], bytestring("A"), 1)
            end
        end    
    end
end


for i in 1:n
    ccall(:close, Cint, (Cint,), pipe_fds[i][1])
    ccall(:close, Cint, (Cint,), pipe_fds[i][2])
end


end
