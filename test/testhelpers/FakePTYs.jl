# This file is a part of Julia. License is MIT: https://julialang.org/license

module FakePTYs

if Sys.iswindows()
    pushfirst!(LOAD_PATH, Sys.STDLIB)
    using Sockets
    Sockets.__init__()
    popfirst!(LOAD_PATH)
end


function open_fake_pty()
    @static if Sys.iswindows()
        # Fake being cygwin
        pid = string(getpid(), base=16, pad=16)
        pipename = """\\\\?\\pipe\\cygwin-$pid-pty10-abcdefg"""
        server = listen(pipename)
        pty_slave = connect(pipename)
        @assert ccall(:jl_ispty, Cint, (Ptr{Cvoid},), pty_slave.handle) == 1
        pty_master = accept(server)
        close(server)
        # extract just the file descriptor
        fds = Libc.dup(Base._fd(pty_slave))
        close(pty_slave)
        pty_slave = fds
        # convert pty_slave handle to a TTY
        #fds = pty_slave.handle
        #pty_slave.status = Base.StatusClosed
        #pty_slave.handle = C_NULL
        #pty_slave = Base.TTY(fds, Base.StatusOpen)
    else
        O_RDWR = Base.Filesystem.JL_O_RDWR
        O_NOCTTY = Base.Filesystem.JL_O_NOCTTY

        fdm = ccall(:posix_openpt, Cint, (Cint,), O_RDWR | O_NOCTTY)
        fdm == -1 && error("Failed to open pty_master")
        rc = ccall(:grantpt, Cint, (Cint,), fdm)
        rc != 0 && error("grantpt failed")
        rc = ccall(:unlockpt, Cint, (Cint,), fdm)
        rc != 0 && error("unlockpt")

        fds = ccall(:open, Cint, (Ptr{UInt8}, Cint),
            ccall(:ptsname, Ptr{UInt8}, (Cint,), fdm), O_RDWR | O_NOCTTY)

        pty_slave = RawFD(fds)
        # pty_slave = fdio(fds, true)
        # pty_slave = Base.Filesystem.File(RawFD(fds))
        # pty_slave = Base.TTY(RawFD(fds); readable = false)
        pty_master = Base.TTY(RawFD(fdm))
    end
    return pty_slave, pty_master
end

function with_fake_pty(f)
    pty_slave, pty_master = open_fake_pty()
    try
        f(pty_slave, pty_master)
    finally
        close(pty_master)
    end
    nothing
end

end
