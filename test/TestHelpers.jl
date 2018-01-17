# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestHelpers

include("dimensionful.jl")
export Furlong

mutable struct FakeTerminal <: Base.Terminals.UnixTerminal
    in_stream::Base.IO
    out_stream::Base.IO
    err_stream::Base.IO
    hascolor::Bool
    raw::Bool
    FakeTerminal(stdin,stdout,stderr,hascolor=true) =
        new(stdin,stdout,stderr,hascolor,false)
end

Base.Terminals.hascolor(t::FakeTerminal) = t.hascolor
Base.Terminals.raw!(t::FakeTerminal, raw::Bool) = t.raw = raw
Base.Terminals.size(t::FakeTerminal) = (24, 80)

function open_fake_pty()
    @static if Sys.iswindows()
        error("Unable to create a fake PTY in Windows")
    end

    O_RDWR = Base.Filesystem.JL_O_RDWR
    O_NOCTTY = Base.Filesystem.JL_O_NOCTTY

    fdm = ccall(:posix_openpt, Cint, (Cint,), O_RDWR|O_NOCTTY)
    fdm == -1 && error("Failed to open PTY master")
    rc = ccall(:grantpt, Cint, (Cint,), fdm)
    rc != 0 && error("grantpt failed")
    rc = ccall(:unlockpt, Cint, (Cint,), fdm)
    rc != 0 && error("unlockpt")

    fds = ccall(:open, Cint, (Ptr{UInt8}, Cint),
        ccall(:ptsname, Ptr{UInt8}, (Cint,), fdm), O_RDWR|O_NOCTTY)

    # slave
    slave = RawFD(fds)
    master = Base.TTY(RawFD(fdm); readable = true)
    slave, master
end

function with_fake_pty(f)
    slave, master = open_fake_pty()
    try
        f(slave, master)
    finally
        ccall(:close,Cint,(Cint,),slave) # XXX: this causes the kernel to throw away all unread data on the pty
        close(master)
    end
end

function challenge_prompt(code::Expr, challenges; timeout::Integer=10, debug::Bool=true)
    output_file = tempname()
    wrapped_code = quote
        result = let
            $code
        end
        open($output_file, "w") do fp
            serialize(fp, result)
        end
    end
    cmd = `$(Base.julia_cmd()) --startup-file=no -e $wrapped_code`
    try
        challenge_prompt(cmd, challenges, timeout=timeout, debug=debug)
        return open(output_file, "r") do fp
            deserialize(fp)
        end
    finally
        isfile(output_file) && rm(output_file)
    end
    return nothing
end

function challenge_prompt(cmd::Cmd, challenges; timeout::Integer=10, debug::Bool=true)
    function format_output(output)
        !debug && return ""
        str = read(seekstart(output), String)
        isempty(str) && return ""
        "Process output found:\n\"\"\"\n$str\n\"\"\""
    end
    out = IOBuffer()
    with_fake_pty() do slave, master
        p = spawn(detach(cmd), slave, slave, slave)

        # Kill the process if it takes too long. Typically occurs when process is waiting
        # for input.
        done = Channel(1)
        @async begin
            sleep(timeout)
            if process_running(p)
                kill(p)
                put!(done, :timed_out)
            else
                put!(done, :exited)
            end
            close(master)
        end

        try
            for (challenge, response) in challenges
                write(out, readuntil(master, challenge))
                if !isopen(master)
                    error("Could not locate challenge: \"$challenge\". ",
                          format_output(out))
                end
                write(master, response)
            end
            wait(p)
        finally
            kill(p)
        end

        # Process timed out or aborted
        if !success(p)
            write(out, readavailable(master))
            if isready(done) && fetch(done) == :timed_out
                error("Process timed out possibly waiting for a response. ",
                      format_output(out))
            else
                error("Failed process. ", format_output(out), "\n", p)
            end
        end
    end
    nothing
end

end
