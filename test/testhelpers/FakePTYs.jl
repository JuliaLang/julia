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
        pts = connect(pipename)
        @assert ccall(:jl_ispty, Cint, (Ptr{Cvoid},), pts.handle) == 1
        ptm = accept(server)
        close(server)
        # extract just the file descriptor
        fds = Libc.dup(Base._fd(pts))
        close(pts)
        pts = fds
        # convert pts handle to a TTY
        #fds = pts.handle
        #pts.status = Base.StatusClosed
        #pts.handle = C_NULL
        #pts = Base.TTY(fds, Base.StatusOpen)
    else
        O_RDWR = Base.Filesystem.JL_O_RDWR
        O_NOCTTY = Base.Filesystem.JL_O_NOCTTY

        fdm = ccall(:posix_openpt, Cint, (Cint,), O_RDWR | O_NOCTTY)
        fdm == -1 && error("Failed to open ptm")
        rc = ccall(:grantpt, Cint, (Cint,), fdm)
        rc != 0 && error("grantpt failed")
        rc = ccall(:unlockpt, Cint, (Cint,), fdm)
        rc != 0 && error("unlockpt")

        fds = ccall(:open, Cint, (Ptr{UInt8}, Cint),
            ccall(:ptsname, Ptr{UInt8}, (Cint,), fdm), O_RDWR | O_NOCTTY)

            pts = RawFD(fds)
        # pts = fdio(fds, true)
        # pts = Base.Filesystem.File(RawFD(fds))
        # pts = Base.TTY(RawFD(fds); readable = false)
        ptm = Base.TTY(RawFD(fdm))
    end
    return pts, ptm
end

function with_fake_pty(f)
    pts, ptm = open_fake_pty()
    try
        f(pts, ptm)
    finally
        close(ptm)
    end
    nothing
end

end
