# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, FileWatching
using Base: uv_error, Experimental
using Base.Filesystem: File
using FileWatching: write_pidfile, parse_pidfile, isvalidpid, stale_pidfile, tryopen_exclusive, open_exclusive

# This script does the following
# Sets up N unix pipes (or WSA sockets)
# For the odd pipes, a byte is written to the write end at intervals specified in intvls
# Nothing is written into the even numbered pipes
# Odd numbered pipes are tested for reads
# Even numbered pipes are tested for timeouts
# Writable ends are always tested for write-ability before a write

n = 20
intvls = [2, .2, .1, .005, .00001]

pipe_fds = fill((Base.INVALID_OS_HANDLE, Base.INVALID_OS_HANDLE), n)
for i in 1:n
    if Sys.iswindows() || i > n ÷ 2
        uv_error("socketpair", ccall(:uv_socketpair, Cint, (Cint, Cint, Ptr{NTuple{2, Base.OS_HANDLE}}, Cint, Cint), 1, (Sys.iswindows() ? 6 : 0), Ref(pipe_fds, i), 0, 0))
    else
        uv_error("pipe", ccall(:uv_pipe, Cint, (Ptr{NTuple{2, Base.OS_HANDLE}}, Cint, Cint), Ref(pipe_fds, i), 0, 0))
    end
    Ctype = Sys.iswindows() ? Ptr{Cvoid} : Cint
    FDmax = Sys.iswindows() ? 0x7fff : (n + 60) # expectations on reasonable values
    fd_in_limits =
        0 <= Int(Base.cconvert(Ctype, pipe_fds[i][1])) <= FDmax &&
        0 <= Int(Base.cconvert(Ctype, pipe_fds[i][2])) <= FDmax
    # Dump out what file descriptors are open for easier debugging of failure modes
    if !fd_in_limits && Sys.islinux()
        run(`ls -la /proc/$(getpid())/fd`)
    end
    @test fd_in_limits
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


## pidfile-based utilities
# helper utilities
struct MemoryFile <: Base.AbstractPipe
    io::IOBuffer
    mtime::Float64
end
Base.pipe_reader(io::MemoryFile) = io.io
Base.Filesystem.mtime(io::MemoryFile) = io.mtime

# set the process umask so we can test the behavior of
# open mask without interference from parent's state
# and create a test environment temp directory
umask(new_mask) = ccall((@static Sys.iswindows() ? :_umask : :umask), Cint, (Cint,), new_mask)
@testset "pidfile-based utilities" begin
old_umask = umask(0o002)
try
    mktempdir() do dir
        cd(dir) do
# now start tests definitions:

@testset "validpid" begin
    mypid = getpid() % Cuint
    @test isvalidpid(gethostname(), mypid)
    @test isvalidpid("", mypid)
    @test !isvalidpid("", 0 % Cuint)
    @test isvalidpid("NOT" * gethostname(), mypid)
    @test isvalidpid("NOT" * gethostname(), 0 % Cuint)
    @test isvalidpid("NOT" * gethostname(), -1 % Cuint)
    if !Sys.iswindows()
        @test isvalidpid("", 1 % Cuint)
        @test !isvalidpid("", -1 % Cuint)
        @test !isvalidpid("", -mypid)
    end
end

@testset "write_pidfile" begin
    buf = IOBuffer()
    pid, host, age = 0, "", 123
    pid2, host2, age2 = parse_pidfile(MemoryFile(seekstart(buf), time() - age))
    @test pid == pid2
    @test host == host2
    @test age ≈ age2 atol=5

    host = " host\r\n"
    write(buf, "-1 $host")
    pid2, host2, age2 = parse_pidfile(MemoryFile(seekstart(buf), time() - age))
    @test pid == pid2
    @test host == host2
    @test age ≈ age2 atol=5
    truncate(seekstart(buf), 0)

    pid, host = getpid(), gethostname()
    write_pidfile(buf, pid)
    @test read(seekstart(buf), String) == "$pid $host"
    pid2, host2, age2 = parse_pidfile(MemoryFile(seekstart(buf), time() - age))
    @test pid == pid2
    @test host == host2
    @test age ≈ age2 atol=5
    truncate(seekstart(buf), 0)

    @testset "parse_pidfile" begin
        age = 0
        @test parse_pidfile("nonexist") === (Cuint(0), "", 0.0)
        open(io -> write_pidfile(io, pid), "pidfile", "w")
        pid2, host2, age2 = parse_pidfile("pidfile")
        @test pid == pid2
        @test host == host2
        @test age ≈ age2 atol=10
        rm("pidfile")
    end
end

@testset "open_exclusive" begin
    f = open_exclusive("pidfile")::File
    try
        # check that f is open and read-writable
        @test isfile("pidfile")
        @test filemode("pidfile") & 0o777 == 0o444
        @test filemode(f) & 0o777 == 0o444
        @test filesize(f) == 0
        @test write(f, "a") == 1
        @test filesize(f) == 1
        @test read(seekstart(f), String) == "a"
        chmod("pidfile", 0o600)
        @test filemode(f) & 0o777 == (Sys.iswindows() ? 0o666 : 0o600)
    finally
        close(f)
    end

    # release the pidfile after a short delay
    deleted = false
    rmtask = @async begin
        sleep(3)
        rm("pidfile")
        deleted = true
    end
    @test isfile("pidfile")
    @test !deleted

    # open the pidfile again (should wait for it to disappear first)
    t = @elapsed f2 = open_exclusive("pidfile")::File
    try
        @test deleted
        @test isfile("pidfile")
        @test t > 2
        if t > 6
            println("INFO: watch_file optimization appears to have NOT succeeded")
        end
        @test filemode(f2) & 0o777 == 0o444
        @test filesize(f2) == 0
        @test write(f2, "bc") == 2
        @test read(seekstart(f2), String) == "bc"
        @test filesize(f2) == 2
    finally
        close(f2)
    end
    rm("pidfile")
    wait(rmtask)

    # now test with a long delay and other non-default options
    f = open_exclusive("pidfile", mode = 0o000)::File
    try
        @test filemode(f) & 0o777 == (Sys.iswindows() ? 0o444 : 0o000)
    finally
        close(f)
    end
    deleted = false
    rmtask = @async begin
        sleep(8)
        rm("pidfile")
        deleted = true
    end
    @test isfile("pidfile")
    @test !deleted
    # open the pidfile again (should wait for it to disappear first)
    t = @elapsed f2 = open_exclusive("pidfile", mode = 0o777, poll_interval = 1.0)::File
    try
        @test deleted
        @test isfile("pidfile")
        @test filemode(f2) & 0o777 == (Sys.iswindows() ? 0o666 : 0o775)
        @test write(f2, "def") == 3
        @test read(seekstart(f2), String) == "def"
        @test t > 7
    finally
        close(f2)
    end
    rm("pidfile")
    wait(rmtask)
end

@testset "open_exclusive: break lock" begin
    # test for stale_age
    t = @elapsed f = open_exclusive("pidfile", poll_interval=3, stale_age=10)::File
    try
        write_pidfile(f, getpid())
    finally
        close(f)
    end
    @test t < 2
    t = @elapsed f = open_exclusive("pidfile", poll_interval=3, stale_age=1)::File
    close(f)
    @test 20 < t < 50
    rm("pidfile")

    t = @elapsed f = open_exclusive("pidfile", poll_interval=3, stale_age=10)::File
    close(f)
    @test t < 2
    t = @elapsed f = open_exclusive("pidfile", poll_interval=3, stale_age=10)::File
    close(f)
    @test 8 < t < 20
    rm("pidfile")
end

@testset "open_exclusive: other errors" begin
    error = @test_throws(Base.IOError, open_exclusive("nonexist/folder"))
    @test error.value.code == Base.UV_ENOENT
end

@testset "mkpidlock" begin
    lockf = mkpidlock("pidfile")
    waittask = @async begin
        sleep(3)
        cd(homedir()) do
            return close(lockf)
        end
    end
    t = @elapsed lockf1 = mkpidlock("pidfile")
    @test t > 2
    @test istaskdone(waittask) && fetch(waittask)
    @test !close(lockf)
    finalize(lockf1)
    t = @elapsed lockf2 = mkpidlock("pidfile")
    @test t < 2
    @test !close(lockf1)

    # test manual breakage of the lock
    # is correctly handled
    if Sys.iswindows()
        mv("pidfile", "xpidfile")
    else
        rm("pidfile")
    end
    t = @elapsed lockf3 = mkpidlock("pidfile")
    @test t < 2
    @test isopen(lockf2.fd)
    @test !close(lockf2)
    @test !isopen(lockf2.fd)
    @test isfile("pidfile")
    @test close(lockf3)
    @test !isfile("pidfile")
    if Sys.iswindows()
        rm("xpidfile")
    end

    # Just for coverage's sake, run a test with do-block syntax
    lock_times = Float64[]
    t_loop = @async begin
        for idx in 1:100
            t = @elapsed mkpidlock("do_block_pidfile") do
            end
            sleep(0.01)
            push!(lock_times, t)
        end
    end
    mkpidlock("do_block_pidfile") do
        sleep(3)
    end
    wait(t_loop)
    @test maximum(lock_times) > 2
    @test minimum(lock_times) < 1
end

end; end # cd(tempdir)
finally
    umask(old_umask)
end; end #testset
