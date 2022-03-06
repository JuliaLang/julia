using FileWatching.Pidfile

using Test

using Base.Filesystem: File
using FileWatching.Pidfile: iswindows,
    write_pidfile, parse_pidfile,
    isvalidpid, stale_pidfile,
    tryopen_exclusive, open_exclusive

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
umask(new_mask) = ccall((@static iswindows() ? :_umask : :umask), Cint, (Cint,), new_mask)

# TODO: Use targeted @test_log tests instead of suppressing all logs to hide the expected warnings
Base.CoreLogging.with_logger(Base.CoreLogging.NullLogger()) do

@testset "Pidfile.jl" begin
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
    if !iswindows()
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

@assert !ispath("pidfile")
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
        @test filemode(f) & 0o777 == (iswindows() ? 0o666 : 0o600)
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
    isdefined(Base, :errormonitor) && Base.errormonitor(rmtask)
    @test isfile("pidfile")
    @test !deleted

    # open the pidfile again (should wait for it to disappear first)
    t = @elapsed f2 = open_exclusive(joinpath(dir, "pidfile"))::File
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
        @test filemode(f) & 0o777 == (iswindows() ? 0o444 : 0o000)
    finally
        close(f)
    end
    deleted = false
    rmtask = @async begin
        sleep(8)
        rm("pidfile")
        deleted = true
    end
    isdefined(Base, :errormonitor) && Base.errormonitor(rmtask)
    @test isfile("pidfile")
    @test !deleted
    # open the pidfile again (should wait for it to disappear first)
    t = @elapsed f2 = open_exclusive("pidfile", mode = 0o777, poll_interval = 1.0)::File
    try
        @test deleted
        @test isfile("pidfile")
        @test filemode(f2) & 0o777 == (iswindows() ? 0o666 : 0o775)
        @test write(f2, "def") == 3
        @test read(seekstart(f2), String) == "def"
        @test t > 7
    finally
        close(f2)
    end
    rm("pidfile")
    wait(rmtask)

    @testset "test for wait == false cases" begin
        f = open_exclusive("pidfile", wait=false)
        @test isfile("pidfile")
        close(f)
        rm("pidfile")

        f = open_exclusive("pidfile")::File
        deleted = false
        rmtask = @async begin
            sleep(2)
            @test Pidfile.tryrmopenfile("pidfile")
            deleted = true
        end
        isdefined(Base, :errormonitor) && Base.errormonitor(rmtask)

        t1 = time()
        @test_throws ErrorException open_exclusive("pidfile", wait=false)
        @test time()-t1 ≈ 0 atol=1

        sleep(1)
        @test !deleted

        t1 = time()
        @test_throws ErrorException open_exclusive("pidfile", wait=false)
        @test time()-t1 ≈ 0 atol=1

        wait(rmtask)
        @test deleted
        t = @elapsed f2 = open_exclusive("pidfile", wait=false)::File
        @test isfile("pidfile")
        @test t ≈ 0 atol=1
        close(f)
        close(f2)
        rm("pidfile")
    end
end

@assert !ispath("pidfile")
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

    error = @test_throws(Base.IOError, open_exclusive(""))
    @test error.value.code == Base.UV_ENOENT
end

@assert !ispath("pidfile")
@testset "mkpidlock" begin
    lockf = mkpidlock("pidfile")
    @test lockf.update === nothing
    waittask = @async begin
        sleep(3)
        cd(homedir()) do
            return close(lockf)
        end
    end
    isdefined(Base, :errormonitor) && Base.errormonitor(waittask)

    # mkpidlock with no waiting
    t = @elapsed @test_throws ErrorException mkpidlock("pidfile", wait=false)
    @test t ≈ 0 atol=1

    t = @elapsed lockf1 = mkpidlock(joinpath(dir, "pidfile"))
    @test t > 2
    @test istaskdone(waittask) && fetch(waittask)
    @test !close(lockf)
    finalize(lockf1)
    t = @elapsed lockf2 = mkpidlock("pidfile")
    @test t < 2
    @test !close(lockf1)

    # test manual breakage of the lock
    # is correctly handled
    @test Pidfile.tryrmopenfile("pidfile")
    t = @elapsed lockf3 = mkpidlock("pidfile")
    @test t < 2
    @test isopen(lockf2.fd)
    @test !close(lockf2)
    @test !isopen(lockf2.fd)
    @test isfile("pidfile")
    @test close(lockf3)
    @test !isfile("pidfile")

    # Just for coverage's sake, run a test with do-block syntax
    lock_times = Float64[]
    t_loop = @async begin
        for idx in 1:100
            t = @elapsed mkpidlock("do_block_pidfile") do
                # nothing
            end
            sleep(0.01)
            push!(lock_times, t)
        end
    end
    isdefined(Base, :errormonitor) && Base.errormonitor(t_loop)
    mkpidlock("do_block_pidfile") do
        sleep(3)
    end
    wait(t_loop)
    @test maximum(lock_times) > 2
    @test minimum(lock_times) < 1
end

@assert !ispath("pidfile")
@testset "mkpidlock update" begin
    lockf = mkpidlock("pidfile")
    @test lockf.update === nothing
    new = mtime(lockf.fd)
    @test new ≈ time() atol=1
    sleep(1)
    @test mtime(lockf.fd) == new
    touch(lockf)
    old, new = new, mtime(lockf.fd)
    @test new != old
    @test new ≈ time() atol=1
    close(lockf)

    lockf = mkpidlock("pidfile"; refresh=0.2)
    new = mtime(lockf.fd)
    @test new ≈ time() atol=1
    for i = 1:10
        sleep(0.5)
        old, new = new, mtime(lockf.fd)
        @test new != old
        @test new ≈ time() atol=1
    end
    @test isopen(lockf.update::Timer)
    close(lockf)
    @test !isopen(lockf.update::Timer)

    lockf = mkpidlock("pidfile"; stale_age=10)
    @test lockf.update isa Timer
    close(lockf.update) # simulate a finalizer running in an undefined order
    close(lockf)
end

@assert !ispath("pidfile")
@testset "mkpidlock for child" begin
    proc = open(`cat`, "w", devnull)
    lock = mkpidlock("pidfile", proc)
    @test isopen(lock.fd)
    @test isfile("pidfile")
    close(proc)
    @test success(proc)
    sleep(1) # give some time for the other task to finish releasing the lock resources
    @test !isopen(lock.fd)
    @test !isfile("pidfile")

    error = @test_throws Base.IOError mkpidlock("pidfile", proc)
    @test error.value.code == Base.UV_ESRCH
end

@assert !ispath("pidfile-2")
@testset "mkpidlock non-blocking stale lock break" begin
    # mkpidlock with no waiting
    lockf = mkpidlock("pidfile-2", wait=false)
    @test lockf.update === nothing

    sleep(1)
    t = @elapsed @test_throws ErrorException mkpidlock("pidfile-2", wait=false, stale_age=1, poll_interval=1, refresh=0)
    @test t ≈ 0 atol=1

    sleep(5)
    t = @elapsed (lockf2 = mkpidlock("pidfile-2", wait=false, stale_age=.1, poll_interval=1, refresh=0))
    @test t ≈ 0 atol=1
    close(lockf)
    close(lockf2)
end

end; end # cd(tempdir)
finally
    umask(old_umask)
end; end # testset

end # with_logger
