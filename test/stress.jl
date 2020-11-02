# This file is a part of Julia. License is MIT: https://julialang.org/license

# test for proper handling of FD exhaustion
if Sys.isunix()
    # Run this test with a really small ulimit. If the ulimit is too high,
    # we might saturate kernel resources (See #23143)
    run(`sh -c "ulimit -n 100; $(Base.shell_escape(Base.julia_cmd())) --startup-file=no $(joinpath(@__DIR__, "stress_fd_exec.jl"))"`)
end

# issue 13559
if !Sys.iswindows()
    function test_13559()
        fn = tempname()
        run(`mkfifo $fn`)
        # use subprocess to write 127 bytes to FIFO
        writer_cmds = """
            using Test
            x = open($(repr(fn)), "w")
            for i in 1:120
                write(x, 0xaa)
            end
            flush(x)
            Test.@test read(stdin, Int8) == 31
            for i in 1:7
                write(x, 0xaa)
            end
            close(x)
        """
        p = open(pipeline(`$(Base.julia_cmd()) --startup-file=no -e $writer_cmds`, stderr=stderr), "w")
        # quickly read FIFO, draining it and blocking but not failing with EOFError yet
        r = open(fn, "r")
        # 15 proper reads
        for i in 1:15
            @test read(r, UInt64) === 0xaaaaaaaaaaaaaaaa
        end
        write(p, 0x1f)
        # last read should throw EOFError when FIFO closes, since there are only 7 bytes (or less) available.
        @test_throws EOFError read(r, UInt64)
        close(r)
        @test success(p)
        rm(fn)
    end
    test_13559()
end

# issue #22566
if !Sys.iswindows()
    function test_22566()
        fn = tempname()
        run(`mkfifo $fn`)

        script = """
            using Test
            x = open($(repr(fn)), "w")
            write(x, 0x42)
            flush(x)
            Test.@test read(stdin, Int8) == 21
            close(x)
        """
        cmd = `$(Base.julia_cmd()) --startup-file=no -e $script`
        p = open(pipeline(cmd, stderr=stderr), "w")

        r = open(fn, "r")
        @test read(r, Int8) == 66
        write(p, 0x15)
        close(r)
        @test success(p)
        rm(fn)
    end

    # repeat opening/closing fifo file, ensure no EINTR popped out
    for i = 1:50
        test_22566()
    end
end  # !Sys.iswindows

# sig 2 is SIGINT per the POSIX.1-1990 standard
if !Sys.iswindows()
    Base.exit_on_sigint(false)
    @test_throws InterruptException begin
        ccall(:kill, Cvoid, (Cint, Cint,), getpid(), 2)
        for i in 1:10
            Libc.systemsleep(0.1)
            ccall(:jl_gc_safepoint, Cvoid, ()) # wait for SIGINT to arrive
        end
    end
    Base.exit_on_sigint(true)
end
