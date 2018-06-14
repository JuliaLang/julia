# This file is a part of Julia. License is MIT: https://julialang.org/license

module FakePTYs

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

end
