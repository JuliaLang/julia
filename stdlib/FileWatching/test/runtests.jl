# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, FileWatching
using Base: uv_error, Experimental

# This script does the following
# Sets up N unix pipes (or WSA sockets)
# For the odd pipes, a byte is written to the write end at intervals specified in intvls
# Nothing is written into the even numbered pipes
# Odd numbered pipes are tested for reads
# Even numbered pipes are tested for timeouts
# Writable ends are always tested for write-ability before a write

n = 20
intvls = [2, .2, .1, .005]

pipe_fds = fill((Base.INVALID_OS_HANDLE, Base.INVALID_OS_HANDLE), n)
for i in 1:n
    if Sys.iswindows() || i > n ÷ 2
        uv_error("socketpair", ccall(:uv_socketpair, Cint, (Cint, Cint, Ptr{NTuple{2, Base.OS_HANDLE}}, Cint, Cint), 1, (Sys.iswindows() ? 6 : 0), Ref(pipe_fds, i), 0, 0))
    else
        uv_error("pipe", ccall(:uv_pipe, Cint, (Ptr{NTuple{2, Base.OS_HANDLE}}, Cint, Cint), Ref(pipe_fds, i), 0, 0))
    end
    Ctype = Sys.iswindows() ? Ptr{Cvoid} : Cint
    FDmax = Sys.iswindows() ? 0x7fff : (n + 60) # expectations on reasonable values
    @test 0 <= Int(Base.cconvert(Ctype, pipe_fds[i][1])) <= FDmax
    @test 0 <= Int(Base.cconvert(Ctype, pipe_fds[i][2])) <= FDmax
end

function pfd_tst_reads(idx, intvl)
    global ready += 1
    wait(ready_c)
    start_evt2 = Condition()
    evt2 = @async (notify(start_evt2); poll_fd(pipe_fds[idx][1], intvl; readable=true, writable=false))
    wait(start_evt2); yield() # make sure the async poll_fd is pumping events
    evt = poll_fd(pipe_fds[idx][1], intvl; readable=true, writable=false)
    @test !evt.timedout
    @test evt.readable
    @test !evt.writable
    @test evt === fetch(evt2)

    dout = zeros(UInt8, 1)
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
    start_evt2 = Condition()
    evt2 = @async (notify(start_evt2); poll_fd(pipe_fds[idx][1], intvl; readable=true, writable=false))
    wait(start_evt2); yield() # make sure the async poll_fd is pumping events
    evt = poll_fd(pipe_fds[idx][1], intvl; readable=true, writable=false)
    @test evt.timedout
    @test !evt.readable
    @test !evt.writable
    @test evt === fetch(evt2)
end

# Odd numbers trigger reads, even numbers timeout
for (i, intvl) in enumerate(intvls)
    @Experimental.sync begin
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
                @static if Sys.iswindows()
                    1 == ccall(:send, stdcall, Cint, (Ptr{Cvoid}, Ptr{UInt8}, Cint, Cint), pipe_fds[idx][2], "A", 1, 0) || error(Libc.FormatMessage())
                else
                    @test 1 == ccall(:write, Csize_t, (Cint, Ptr{UInt8}, Csize_t), pipe_fds[idx][2], "A", 1)
                end
            end
        end
        notify(ready_c, all=true)
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

for f in (watch_file, watch_folder, poll_file)
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
const F_GETPATH = Sys.islinux() || Sys.iswindows() || Sys.isapple()  # platforms where F_GETPATH is available
const F_PATH = F_GETPATH ? "afile.txt" : ""
dir = mktempdir()
file = joinpath(dir, "afile.txt")

# initialize a watch_folder instance and create afile.txt
function test_init_afile()
    @test isempty(FileWatching.watched_folders)
    @test(watch_folder(dir, 0) == ("" => FileWatching.FileEvent()))
    @test @elapsed(@test(watch_folder(dir, 0) == ("" => FileWatching.FileEvent()))) <= 0.5
    @test length(FileWatching.watched_folders) == 1
    @test unwatch_folder(dir) === nothing
    @test isempty(FileWatching.watched_folders)
    @test 0.002 <= @elapsed(@test(watch_folder(dir, 0.004) == ("" => FileWatching.FileEvent())))
    @test 0.002 <= @elapsed(@test(watch_folder(dir, 0.004) == ("" => FileWatching.FileEvent()))) <= 0.5
    @test unwatch_folder(dir) === nothing
    @test 0.99 <= @elapsed(@test(watch_folder(dir, 1) == ("" => FileWatching.FileEvent())))
    @test 0.99 <= @elapsed(@test(watch_folder(dir, 1) == ("" => FileWatching.FileEvent())))
    # like touch, but lets the operating system update the timestamp
    # for greater precision on some platforms (windows)
    @test close(open(file, "w")) === nothing
    @test(watch_folder(dir) == (F_PATH => FileWatching.FileEvent(FileWatching.UV_RENAME)))
    @test close(open(file, "w")) === nothing
    sleep(3)
    let c
        c = watch_folder(dir, 0)
        if F_GETPATH
            @test c.first == F_PATH
            @test c.second.changed ⊻ c.second.renamed
            @test !c.second.timedout
        else # we don't expect to be able to detect file changes in this case
            @test c.first == ""
            @test !c.second.changed && !c.second.renamed
            @test c.second.timedout
        end
    end
    @test unwatch_folder(dir) === nothing
    @test(watch_folder(dir, 0) == ("" => FileWatching.FileEvent()))
    @test 0.9 <= @elapsed(@test(watch_folder(dir, 1) == ("" => FileWatching.FileEvent())))
    @test length(FileWatching.watched_folders) == 1
    nothing
end

function test_file_poll(channel, interval, timeout_s)
    rc = poll_file(file, interval, timeout_s)
    put!(channel, rc)
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
    t = @async test_file_poll(channel, tval/3, tval)
    sleep(tval/3)  # one poll period
    f = open(file, "a")
    write(f, "Hello World\n")
    close(f)
    tr = take!(channel)
    @test ispath(tr[1]) && ispath(tr[2])
    fetch(t)
end

function test_watch_file_timeout(tval)
    watch = @async watch_file(file, tval)
    @test fetch(watch) == FileWatching.FileEvent(false, false, true)
end

function test_watch_file_change(tval)
    watch = @async watch_file(file, tval)
    sleep(tval/3)
    open(file, "a") do f
        write(f, "small change\n")
    end
    @test fetch(watch) == FileWatching.FileEvent(false, true, false)
end

function test_monitor_wait(tval)
    fm = FileMonitor(file)
    @sync begin
        @async begin
            sleep(tval)
            f = open(file, "a")
            write(f, "Hello World\n")
            close(f)
        end
        events = wait(fm)
        close(fm)
        @test events.changed && !events.timedout && !events.renamed
    end
end

function test_dirmonitor_wait(tval)
    fm = FolderMonitor(file)
    @sync begin
        @async begin
            sleep(tval)
            for i = 1:3
                f = open(file, "w")
                write(f, "Hello World\n")
                close(f)
            end
        end
        fname, events = wait(fm)::Pair
        @test fname == F_PATH
        @test events.changed && !events.timedout && !events.renamed
        close(fm)
    end
end

function test_dirmonitor_wait2(tval)
    fm = FolderMonitor(dir)
    @sync begin
        @async begin
            sleep(tval)
            for i = 1:3
                f = open("$file$i", "w")
                write(f, "Hello World $i\n")
                close(f)
            end
        end
        if F_GETPATH
            let (fname, events) = wait(fm)::Pair
                for i = 1:3
                    @test fname == "$F_PATH$i"
                    @test (!events.changed && events.renamed) && !events.timedout
                    i == 3 && break
                    fname, events = wait(fm)
                    if fname == "$F_PATH$i"
                        @test (events.changed ⊻ events.renamed) && !events.timedout
                        fname, events = wait(fm)
                    end
                end
            end
        else
            let (fname, events) = wait(fm)::Pair
                @test fname == ""
                @test (!events.changed && events.renamed) && !events.timedout
            end
            close(fm)
            fm = FolderMonitor(dir)
        end
    end
    @sync begin
        @async begin
            for i = 1:3
                sleep(tval)
                rm("$file$i")
            end
        end
        if F_GETPATH
            let (fname, events) = wait(fm)::Pair
                if fname == "$(F_PATH)3" # actually one more event from above
                    @test events.changed && !events.timedout && !events.renamed
                    fname, events = wait(fm)
                end
                for i = 1:3
                    @test fname == "$F_PATH$i"
                    @test !events.changed && !events.timedout && events.renamed
                    i == 3 && break
                    fname, events = wait(fm)
                end
            end
        else
            let (fname, events) = wait(fm)::Pair
                @test fname == ""
                @test !events.changed && !events.timedout && events.renamed
            end
        end
    end
    close(fm)
end

function test_monitor_wait_poll()
    pfw = PollingFileWatcher(file, 5.007)
    @sync begin
        @async begin
            sleep(2.5)
            f = open(file, "a")
            write(f, "Hello World\n")
            close(f)
        end
        (old, new) = wait(pfw)
        close(pfw)
        @test new.mtime - old.mtime > 2.5 - 1.5 # mtime may only have second-level accuracy (plus add some hysteresis)
    end
end

test_init_afile()
test_timeout(0.1)
test_timeout(1)
test_touch(6)
test_monitor_wait(0.2)
test_monitor_wait(0.2)
test_dirmonitor_wait(0.2)
test_dirmonitor_wait(0.2)
test_monitor_wait_poll()
test_monitor_wait_poll()
test_watch_file_timeout(0.2)
test_watch_file_change(6)
test_dirmonitor_wait2(0.2)
test_dirmonitor_wait2(0.2)

mv(file, file * "~")
mv(file * "~", file)
let changes = []
    while true
        let c
            Sys.iswindows() && sleep(0.1)
            @test @elapsed(c = watch_folder(dir, 0.0)) < 0.5
            push!(changes, c)
            (c.second::FileWatching.FileEvent).timedout && break
        end
    end
    if F_GETPATH
        @test 12 < length(changes) < 48
    else
        @test 5 < length(changes) < 16
    end
    @test pop!(changes) == ("" => FileWatching.FileEvent())
    if F_GETPATH
        Sys.iswindows() && @test pop!(changes) == (F_PATH => FileWatching.FileEvent(FileWatching.UV_CHANGE))
        p = pop!(changes)
        if !Sys.isapple()
            @test p == (F_PATH => FileWatching.FileEvent(FileWatching.UV_RENAME))
        end
        while changes[end][1] == F_PATH
            @test pop!(changes)[2] == FileWatching.FileEvent(FileWatching.UV_RENAME)
        end
        p = pop!(changes)
        if !Sys.isapple()
            @test p == (F_PATH * "~" => FileWatching.FileEvent(FileWatching.UV_RENAME))
        end
        while changes[end][1] == F_PATH * "~"
            @test pop!(changes)[2] == FileWatching.FileEvent(FileWatching.UV_RENAME)
        end
        if changes[end][1] == F_PATH
            @test pop!(changes)[2] == FileWatching.FileEvent(FileWatching.UV_RENAME)
        end
        for j = 1:4
            for i = 3:-1:1
                while changes[end - 1][1] == "$F_PATH$i"
                    @test let x = pop!(changes)[2]; x.changed ⊻ x.renamed; end
                end
                p = pop!(changes)
                if !Sys.isapple()
                    @test p == ("$F_PATH$i" => FileWatching.FileEvent(FileWatching.UV_RENAME))
                end
            end
        end
    end
    @test all(x -> (isa(x, Pair) && x[1] == F_PATH && (x[2].changed ⊻ x[2].renamed)), changes) || changes
end

@test_throws(Base._UVError("FileMonitor (start)", Base.UV_ENOENT),
             watch_file("____nonexistent_file", 10))
@test_throws(Base._UVError("FolderMonitor (start)", Base.UV_ENOENT),
             watch_folder("____nonexistent_file", 10))
@test(@elapsed(
    @test(poll_file("____nonexistent_file", 1, 3.1) ===
          (Base.Filesystem.StatStruct(), EOFError()))) > 3)

unwatch_folder(dir)
@test isempty(FileWatching.watched_folders)
rm(file)
rm(dir)
