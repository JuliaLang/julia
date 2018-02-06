# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, FileWatching

# This script does the following
# Sets up n unix pipes
# For the odd pipes, a byte is written to the write end at intervals specified in intvls
# Nothing is written into the even numbered pipes
# Odd numbered pipes are tested for reads
# Even numbered pipes are tested for timeouts
# Writable ends are always tested for writability before a write

n = 20
intvls = [2, .2, .1, .005]

pipe_fds = Vector{Any}(uninitialized, n)
for i in 1:n
    @static if Sys.iswindows()
        pipe_fds[i] = Vector{Libc.WindowsRawSocket}(uninitialized, 2)
        0 == ccall(:wsasocketpair, Cint, (Cint, Cuint, Cint, Ptr{Libc.WindowsRawSocket}), 1, 1, 6, pipe_fds[i]) || error(Libc.FormatMessage())
    else
        pipe_fds[i] = Array{RawFD}(uninitialized, 2)
        @test 0 == ccall(:pipe, Cint, (Ptr{RawFD},), pipe_fds[i])
    end
end

function pfd_tst_reads(idx, intvl)
    global ready += 1
    wait(ready_c)
    t_elapsed = @elapsed begin
        start_evt2 = Condition()
        evt2 = @async (notify(start_evt2); poll_fd(pipe_fds[idx][1], intvl; readable=true, writable=false))
        wait(start_evt2); yield() # make sure the async poll_fd is pumping events
        evt = poll_fd(pipe_fds[idx][1], intvl; readable=true, writable=false)
    end
    @test !evt.timedout
    @test evt.readable
    @test !evt.writable
    @test evt === wait(evt2)

    # println("Expected ", intvl, ", actual ", t_elapsed, ", diff ", t_elapsed - intvl)
    # Disabled since this assertion fails randomly, notably on build VMs (issue #12824)
    # @test t_elapsed <= (intvl + 1)

    dout = Vector{UInt8}(uninitialized, 1)
    @static if Sys.iswindows()
        1 == ccall(:recv, stdcall, Cint, (Ptr{Cvoid}, Ptr{UInt8}, Cint, Cint), pipe_fds[idx][1], dout, 1, 0) || error(Libc.FormatMessage())
    else
        @test 1 == ccall(:read, Csize_t, (Cint, Ptr{UInt8}, Csize_t), pipe_fds[idx][1], dout, 1)
    end
    @test dout[1] == Int8('A')
end


function pfd_tst_timeout(idx, intvl)
    global ready += 1
    wait(ready_c)
    t_elapsed = @elapsed begin
        start_evt2 = Condition()
        evt2 = @async (notify(start_evt2); poll_fd(pipe_fds[idx][1], intvl; readable=true, writable=false))
        wait(start_evt2); yield() # make sure the async poll_fd is pumping events
        evt = poll_fd(pipe_fds[idx][1], intvl; readable=true, writable=false)
        @test evt.timedout
        @test !evt.readable
        @test !evt.writable
        @test evt === wait(evt2)
    end

    # Disabled since these assertions fail randomly, notably on build VMs (issue #12824)
    # @test intvl <= t_elapsed
    # @test t_elapsed <= (intvl + 1)
end


# Odd numbers trigger reads, even numbers timeout
for (i, intvl) in enumerate(intvls)
    @sync begin
        global ready = 0
        global ready_c = Condition()
        t = Vector{Task}(uninitialized, n)
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
                @static if Sys.iswindows()
                    1 == ccall(:send, stdcall, Cint, (Ptr{Cvoid}, Ptr{UInt8}, Cint, Cint), pipe_fds[idx][2], "A", 1, 0) || error(Libc.FormatMessage())
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
        @static if Sys.iswindows()
            0 == ccall(:closesocket, stdcall, Cint, (Ptr{Cvoid},), pipe_fds[i][j]) || error(Libc.FormatMessage())
        else
            @test 0 == ccall(:close, Cint, (Cint,), pipe_fds[i][j])
        end
    end
end

# issue #12473
# make sure 1-shot timers work
let a = []
    Timer(t -> push!(a, 1), 0.01, interval = 0)
    sleep(0.2)
    @test a == [1]
end

# make sure repeating timers work
@noinline function make_unrooted_timer(a)
    t = Timer(0.0, interval = 0.1)
    finalizer(t -> a[] += 1, t)
    wait(t)
    e = @elapsed for i = 1:5
        wait(t)
    end
    @test 1.5 > e >= 0.4
    @test a[] == 0
    nothing
end
let a = Ref(0)
    make_unrooted_timer(a)
    GC.gc()
    @test a[] == 1
end

for f in (watch_file, poll_file)
    local f
    @test_throws ArgumentError f("adir\0bad")
end

#issue #12992
function test_12992()
    pfw = PollingFileWatcher(@__FILE__, 0.01)
    close(pfw)
    pfw = PollingFileWatcher(@__FILE__, 0.01)
    close(pfw)
    pfw = PollingFileWatcher(@__FILE__, 0.01)
    close(pfw)
    GC.gc()
    GC.gc()
end

# Make sure multiple close is fine
function test2_12992()
    pfw = PollingFileWatcher(@__FILE__, 0.01)
    close(pfw)
    close(pfw)
    pfw = PollingFileWatcher(@__FILE__, 0.01)
    close(pfw)
    close(pfw)
    pfw = PollingFileWatcher(@__FILE__, 0.01)
    close(pfw)
    close(pfw)
    GC.gc()
    GC.gc()
end

test_12992()
test_12992()
test_12992()

test2_12992()
test2_12992()
test2_12992()

#######################################################################
# This section tests file watchers.                                   #
#######################################################################
dir = mktempdir()
file = joinpath(dir, "afile.txt")
# like touch, but lets the operating system update the timestamp
# for greater precision on some platforms (windows)
@test close(open(file,"w")) === nothing

function test_file_poll(channel,interval,timeout_s)
    rc = poll_file(file, interval, timeout_s)
    put!(channel,rc)
end

function test_timeout(tval)
    t_elapsed = @elapsed begin
        channel = Channel(1)
        @async test_file_poll(channel, 10, tval)
        tr = take!(channel)
    end
    @test tr[1] === Base.Filesystem.StatStruct() && tr[2] === EOFError()
    @test tval <= t_elapsed
end

function test_touch(slval)
    tval = slval*1.1
    channel = Channel(1)
    @async test_file_poll(channel, tval/3, tval)
    sleep(tval/3)  # one poll period
    f = open(file,"a")
    write(f,"Hello World\n")
    close(f)
    tr = take!(channel)
    @test ispath(tr[1]) && ispath(tr[2])
end

function test_watch_file_timeout(tval)
    watch = @async watch_file(file, tval)
    @test wait(watch) == FileWatching.FileEvent(false, false, true)
end

function test_watch_file_change(tval)
    watch = @async watch_file(file, tval)
    sleep(tval/3)
    open(file, "a") do f
        write(f, "small change\n")
    end
    @test wait(watch) == FileWatching.FileEvent(false, true, false)
end

function test_monitor_wait(tval)
    fm = FileMonitor(file)
    @async begin
        sleep(tval)
        f = open(file,"a")
        write(f,"Hello World\n")
        close(f)
    end
    fname, events = wait(fm)
    close(fm)
    if Sys.islinux() || Sys.iswindows() || Sys.isapple()
        @test fname == basename(file)
    else
        @test fname == ""  # platforms where F_GETPATH is not available
    end
    @test events.changed
end

function test_monitor_wait_poll()
    pfw = PollingFileWatcher(file, 5.007)
    @async begin
        sleep(2.5)
        f = open(file,"a")
        write(f,"Hello World\n")
        close(f)
    end
    (old, new) = wait(pfw)
    close(pfw)
    @test new.mtime - old.mtime > 2.5 - 1.5 # mtime may only have second-level accuracy (plus add some hysteresis)
end

test_timeout(0.1)
test_timeout(1)
test_touch(6)
test_monitor_wait(0.1)
test_monitor_wait(0.1)
test_monitor_wait_poll()
test_monitor_wait_poll()
test_watch_file_timeout(0.1)
test_watch_file_change(6)

@test_throws Base.UVError watch_file("____nonexistent_file", 10)
@test(@elapsed(
    @test(poll_file("____nonexistent_file", 1, 3.1) ===
          (Base.Filesystem.StatStruct(), EOFError()))) > 3)

rm(file)
rm(dir)
