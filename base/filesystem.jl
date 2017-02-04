# This file is a part of Julia. License is MIT: http://julialang.org/license

## File Operations (Libuv-based) ##

module Filesystem

const S_IRUSR = 0o400
const S_IWUSR = 0o200
const S_IXUSR = 0o100
const S_IRWXU = 0o700
const S_IRGRP = 0o040
const S_IWGRP = 0o020
const S_IXGRP = 0o010
const S_IRWXG = 0o070
const S_IROTH = 0o004
const S_IWOTH = 0o002
const S_IXOTH = 0o001
const S_IRWXO = 0o007

export File,
       StatStruct,
       # open,
       futime,
       unlink,
       write,
       JL_O_WRONLY,
       JL_O_RDONLY,
       JL_O_RDWR,
       JL_O_APPEND,
       JL_O_CREAT,
       JL_O_EXCL,
       JL_O_TRUNC,
       JL_O_TEMPORARY,
       JL_O_SHORT_LIVED,
       JL_O_SEQUENTIAL,
       JL_O_RANDOM,
       JL_O_NOCTTY,
       S_IRUSR, S_IWUSR, S_IXUSR, S_IRWXU,
       S_IRGRP, S_IWGRP, S_IXGRP, S_IRWXG,
       S_IROTH, S_IWOTH, S_IXOTH, S_IRWXO

import Base:
    UVError, _sizeof_uv_fs, check_open, close, eof, eventloop, fd, isopen,
    nb_available, position, read, read!, readavailable, seek, seekend, show,
    skip, stat, unsafe_read, unsafe_write, transcode, uv_error, uvhandle,
    uvtype, write

if is_windows()
    import Base: cwstring
end

include("path.jl")
include("stat.jl")
include("file.jl")
include("poll.jl")
include(string(length(Core.ARGS)>=2?Core.ARGS[2]:"","file_constants.jl"))  # include($BUILDROOT/base/file_constants.jl)

## Operations with File (fd) objects ##

abstract AbstractFile <: IO

type File <: AbstractFile
    open::Bool
    handle::RawFD
    File(fd::RawFD) = new(true, fd)
end

# Not actually a pointer, but that's how we pass it through the C API so it's fine
uvhandle(file::File) = convert(Ptr{Void}, Base.cconvert(Cint, file.handle) % UInt)
uvtype(::File) = Base.UV_RAW_FD

# Filesystem.open, not Base.open
function open(path::AbstractString, flags::Integer, mode::Integer=0)
    req = Libc.malloc(_sizeof_uv_fs)
    local handle
    try
        ret = ccall(:uv_fs_open, Int32,
                    (Ptr{Void}, Ptr{Void}, Cstring, Int32, Int32, Ptr{Void}),
                    eventloop(), req, path, flags, mode, C_NULL)
        handle = ccall(:jl_uv_fs_result, Int32, (Ptr{Void},), req)
        ccall(:uv_fs_req_cleanup, Void, (Ptr{Void},), req)
        uv_error("open", ret)
    finally # conversion to Cstring could cause an exception
        Libc.free(req)
    end
    return File(RawFD(handle))
end

isopen(f::File) = f.open
function check_open(f::File)
    if !isopen(f)
        throw(ArgumentError("file is closed"))
    end
end

function close(f::File)
    check_open(f)
    err = ccall(:jl_fs_close, Int32, (Int32,), f.handle)
    uv_error("close", err)
    f.handle = RawFD(-1)
    f.open = false
    return nothing
end

# sendfile is the most efficient way to copy a file (or any file descriptor)
function sendfile(dst::File, src::File, src_offset::Int64, bytes::Int)
    check_open(dst)
    check_open(src)
    err = ccall(:jl_fs_sendfile, Int32, (Int32, Int32, Int64, Csize_t),
                src.handle, dst.handle, src_offset, bytes)
    uv_error("sendfile", err)
    nothing
end

function unsafe_write(f::File, buf::Ptr{UInt8}, len::UInt, offset::Int64=Int64(-1))
    check_open(f)
    err = ccall(:jl_fs_write, Int32, (Int32, Ptr{UInt8}, Csize_t, Int64),
                f.handle, buf, len, offset)
    uv_error("write", err)
    return len
end

write(f::File, c::UInt8) = write(f, Ref{UInt8}(c))

function truncate(f::File, n::Integer)
    check_open(f)
    req = Libc.malloc(_sizeof_uv_fs)
    err = ccall(:uv_fs_ftruncate, Int32,
                (Ptr{Void}, Ptr{Void}, Int32, Int64, Ptr{Void}),
                eventloop(), req, f.handle, n, C_NULL)
    Libc.free(req)
    uv_error("ftruncate", err)
    return f
end

function futime(f::File, atime::Float64, mtime::Float64)
    check_open(f)
    req = Libc.malloc(_sizeof_uv_fs)
    err = ccall(:uv_fs_futime, Int32,
                (Ptr{Void}, Ptr{Void}, Int32, Float64, Float64, Ptr{Void}),
                eventloop(), req, f.handle, atime, mtime, C_NULL)
    Libc.free(req)
    uv_error("futime", err)
    return f
end

function read(f::File, ::Type{UInt8})
    check_open(f)
    ret = ccall(:jl_fs_read_byte, Int32, (Int32,), f.handle)
    uv_error("read", ret)
    return ret % UInt8
end

function unsafe_read(f::File, p::Ptr{UInt8}, nel::UInt)
    check_open(f)
    ret = ccall(:jl_fs_read, Int32, (Int32, Ptr{Void}, Csize_t),
                f.handle, p, nel)
    uv_error("read",ret)
    ret == nel || throw(EOFError())
    nothing
end

nb_available(f::File) = max(0, filesize(f) - position(f)) # position can be > filesize

eof(f::File) = nb_available(f) == 0

function readbytes!(f::File, b::Array{UInt8}, nb=length(b))
    nr = min(nb, nb_available(f))
    if length(b) < nr
        resize!(b, nr)
    end
    ret = ccall(:jl_fs_read, Int32, (Int32, Ptr{Void}, Csize_t),
                f.handle, b, nr)
    uv_error("read",ret)
    return ret
end
read(io::File) = read!(io, Base.StringVector(nb_available(io)))
readavailable(io::File) = read(io)
read(io::File, nb::Integer) = read!(io, Base.StringVector(min(nb, nb_available(io))))

const SEEK_SET = Int32(0)
const SEEK_CUR = Int32(1)
const SEEK_END = Int32(2)

function seek(f::File, n::Integer)
    ret = ccall(:jl_lseek, Int64, (Int32, Int64, Int32), f.handle, n, SEEK_SET)
    systemerror("seek", ret == -1)
    return f
end

function seekend(f::File)
    ret = ccall(:jl_lseek, Int64, (Int32, Int64, Int32), f.handle, 0, SEEK_END)
    systemerror("seekend", ret == -1)
    return f
end

function skip(f::File, n::Integer)
    ret = ccall(:jl_lseek, Int64, (Int32, Int64, Int32), f.handle, n, SEEK_CUR)
    systemerror("skip", ret == -1)
    return f
end

function position(f::File)
    check_open(f)
    ret = ccall(:jl_lseek, Int64, (Int32, Int64, Int32), f.handle, 0, SEEK_CUR)
    systemerror("lseek", ret == -1)
    return ret
end

fd(f::File) = f.handle
stat(f::File) = stat(f.handle)

end
