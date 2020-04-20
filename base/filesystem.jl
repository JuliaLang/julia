# This file is a part of Julia. License is MIT: https://julialang.org/license

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

import .Base:
    IOError, _UVError, _sizeof_uv_fs, check_open, close, eof, eventloop, fd, isopen,
    bytesavailable, position, read, read!, readavailable, seek, seekend, show,
    skip, stat, unsafe_read, unsafe_write, write, transcode, uv_error,
    rawhandle, OS_HANDLE, INVALID_OS_HANDLE, windowserror

import .Base.RefValue

if Sys.iswindows()
    import .Base: cwstring
end

# Average buffer size including null terminator for several filesystem operations.
# On Windows we use the MAX_PATH = 260 value on Win32.
const AVG_PATH = Sys.iswindows() ? 260 : 512

include("path.jl")
include("stat.jl")
include("file.jl")
include(string(length(Core.ARGS) >= 2 ? Core.ARGS[2] : "", "file_constants.jl"))  # include($BUILDROOT/base/file_constants.jl)

## Operations with File (fd) objects ##

abstract type AbstractFile <: IO end

mutable struct File <: AbstractFile
    open::Bool
    handle::OS_HANDLE
    File(fd::OS_HANDLE) = new(true, fd)
end
if OS_HANDLE !== RawFD
    File(fd::RawFD) = File(Libc._get_osfhandle(fd)) # TODO: calling close would now destroy the wrong handle
end

rawhandle(file::File) = file.handle

# Filesystem.open, not Base.open
function open(path::AbstractString, flags::Integer, mode::Integer=0)
    req = Libc.malloc(_sizeof_uv_fs)
    local handle
    try
        ret = ccall(:uv_fs_open, Int32,
                    (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Int32, Int32, Ptr{Cvoid}),
                    C_NULL, req, path, flags, mode, C_NULL)
        handle = ccall(:uv_fs_get_result, Cssize_t, (Ptr{Cvoid},), req)
        ccall(:uv_fs_req_cleanup, Cvoid, (Ptr{Cvoid},), req)
        uv_error("open", ret)
    finally # conversion to Cstring could cause an exception
        Libc.free(req)
    end
    return File(OS_HANDLE(@static Sys.iswindows() ? Ptr{Cvoid}(handle) : Cint(handle)))
end

isopen(f::File) = f.open

function check_open(f::File)
    if !isopen(f)
        throw(ArgumentError("file is closed"))
    end
end

function close(f::File)
    if isopen(f)
        f.open = false
        err = ccall(:jl_fs_close, Int32, (OS_HANDLE,), f.handle)
        f.handle = INVALID_OS_HANDLE
        uv_error("close", err)
    end
    nothing
end

# sendfile is the most efficient way to copy from a file descriptor
function sendfile(dst::File, src::File, src_offset::Int64, bytes::Int)
    check_open(dst)
    check_open(src)
    while true
        result = ccall(:jl_fs_sendfile, Int32, (OS_HANDLE, OS_HANDLE, Int64, Csize_t),
                       src.handle, dst.handle, src_offset, bytes)
        uv_error("sendfile", result)
        nsent = result
        bytes -= nsent
        src_offset += nsent
        bytes <= 0 && break
    end
    nothing
end

function unsafe_write(f::File, buf::Ptr{UInt8}, len::UInt, offset::Int64=Int64(-1))
    check_open(f)
    err = ccall(:jl_fs_write, Int32, (OS_HANDLE, Ptr{UInt8}, Csize_t, Int64),
                f.handle, buf, len, offset)
    uv_error("write", err)
    return len
end

write(f::File, c::UInt8) = write(f, Ref{UInt8}(c))

function truncate(f::File, n::Integer)
    check_open(f)
    req = Libc.malloc(_sizeof_uv_fs)
    err = ccall(:uv_fs_ftruncate, Int32,
                (Ptr{Cvoid}, Ptr{Cvoid}, OS_HANDLE, Int64, Ptr{Cvoid}),
                C_NULL, req, f.handle, n, C_NULL)
    Libc.free(req)
    uv_error("ftruncate", err)
    return f
end

function futime(f::File, atime::Float64, mtime::Float64)
    check_open(f)
    req = Libc.malloc(_sizeof_uv_fs)
    err = ccall(:uv_fs_futime, Int32,
                (Ptr{Cvoid}, Ptr{Cvoid}, OS_HANDLE, Float64, Float64, Ptr{Cvoid}),
                C_NULL, req, f.handle, atime, mtime, C_NULL)
    Libc.free(req)
    uv_error("futime", err)
    return f
end

function read(f::File, ::Type{UInt8})
    check_open(f)
    ret = ccall(:jl_fs_read_byte, Int32, (OS_HANDLE,), f.handle)
    uv_error("read", ret)
    return ret % UInt8
end

function read(f::File, ::Type{Char})
    b0 = read(f, UInt8)
    l = 8 * (4 - leading_ones(b0))
    c = UInt32(b0) << 24
    if l < 24
        s = 16
        while s ≥ l && !eof(f)
            p = position(f)
            b = read(f, UInt8)
            if b & 0xc0 != 0x80
                seek(f, p)
                break
            end
            c |= UInt32(b) << s
            s -= 8
        end
    end
    return reinterpret(Char, c)
end

read(f::File, ::Type{T}) where {T<:AbstractChar} = T(read(f, Char)) # fallback

function unsafe_read(f::File, p::Ptr{UInt8}, nel::UInt)
    check_open(f)
    ret = ccall(:jl_fs_read, Int32, (OS_HANDLE, Ptr{Cvoid}, Csize_t),
                f.handle, p, nel)
    uv_error("read", ret)
    ret == nel || throw(EOFError())
    nothing
end

bytesavailable(f::File) = max(0, filesize(f) - position(f)) # position can be > filesize

eof(f::File) = bytesavailable(f) == 0

function readbytes!(f::File, b::Array{UInt8}, nb=length(b))
    nr = min(nb, bytesavailable(f))
    if length(b) < nr
        resize!(b, nr)
    end
    ret = ccall(:jl_fs_read, Int32, (OS_HANDLE, Ptr{Cvoid}, Csize_t),
                f.handle, b, nr)
    uv_error("read", ret)
    return ret
end
read(io::File) = read!(io, Base.StringVector(bytesavailable(io)))
readavailable(io::File) = read(io)
read(io::File, nb::Integer) = read!(io, Base.StringVector(min(nb, bytesavailable(io))))

const SEEK_SET = Int32(0)
const SEEK_CUR = Int32(1)
const SEEK_END = Int32(2)

function seek(f::File, n::Integer)
    ret = ccall(:jl_lseek, Int64, (OS_HANDLE, Int64, Int32), f.handle, n, SEEK_SET)
    systemerror("seek", ret == -1)
    return f
end

function seekend(f::File)
    ret = ccall(:jl_lseek, Int64, (OS_HANDLE, Int64, Int32), f.handle, 0, SEEK_END)
    systemerror("seekend", ret == -1)
    return f
end

function skip(f::File, n::Integer)
    ret = ccall(:jl_lseek, Int64, (OS_HANDLE, Int64, Int32), f.handle, n, SEEK_CUR)
    systemerror("skip", ret == -1)
    return f
end

function position(f::File)
    check_open(f)
    ret = ccall(:jl_lseek, Int64, (OS_HANDLE, Int64, Int32), f.handle, 0, SEEK_CUR)
    systemerror("lseek", ret == -1)
    return ret
end

fd(f::File) = f.handle
stat(f::File) = stat(f.handle)

end
