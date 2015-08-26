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
       # open,
       # close,
       write,
       unlink,
       rename,
       sendfile,
       symlink,
       readlink,
       chmod,
       futime,
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

import Base: uvtype, uvhandle, eventloop, fd, position, stat, close, write, read, read!, readbytes, isopen,
            check_open, _sizeof_uv_fs, uv_error, show

include("stat.jl")
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

function open(path::AbstractString, flags::Integer, mode::Integer=0) # FS.open, not Base.open
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
    return f
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

function write(f::File, buf::Ptr{UInt8}, len::Integer, offset::Integer=-1)
    check_open(f)
    err = ccall(:jl_fs_write, Int32, (Int32, Ptr{UInt8}, Csize_t, Int64),
                f.handle, buf, len, offset)
    uv_error("write", err)
    return len
end

write(f::File, c::UInt8) = write(f, UInt8[c])

function write{T}(f::File, a::Array{T})
    if isbits(T)
        write(f, pointer(a), sizeof(a))
    else
        invoke(write, Tuple{IO, Array}, f, a)
    end
end

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

function read!(f::File, a::Vector{UInt8}, nel=length(a))
    check_open(f)
    if nel < 0 || nel > length(a)
        throw(BoundsError())
    end
    ret = ccall(:jl_fs_read, Int32, (Int32, Ptr{Void}, Csize_t),
                f.handle, a, nel)
    uv_error("read",ret)
    return a
end

nb_available(f::File) = filesize(f) - position(f)

function readbytes!(f::File, b::Array{UInt8}, nb=length(b))
    nr = min(nb, nb_available(f))
    if length(b) < nr
        resize!(b, nr)
    end
    read!(f, b, nr)
    return nr
end
readbytes(io::File) = read!(io, Array(UInt8, nb_available(io)))
readbytes(io::File, nb) = read!(io, Array(UInt8, min(nb, nb_available(io))))

function readbytes(f::File)
    a = Array(UInt8, nb_available(f))
    read!(f,a)
    return a
end

const SEEK_SET = Int32(0)
const SEEK_CUR = Int32(1)
const SEEK_END = Int32(2)

function position(f::File)
    check_open(f)
    ret = ccall(:jl_lseek, Coff_t, (Int32, Coff_t, Int32), f.handle, 0, SEEK_CUR)
    systemerror("lseek", ret == -1)
    return ret
end

fd(f::File) = f.handle
stat(f::File) = stat(f.handle)

# Operations with the file system (paths) ##

function unlink(p::AbstractString)
    err = ccall(:jl_fs_unlink, Int32, (Cstring,), p)
    uv_error("unlink", err)
    nothing
end

# For move command
function rename(src::AbstractString, dst::AbstractString)
    err = ccall(:jl_fs_rename, Int32, (Cstring, Cstring), src, dst)
    # on error, default to cp && rm
    if err < 0
        # remove_destination: is already done in the mv function
        cp(src, dst; remove_destination=false, follow_symlinks=false)
        rm(src; recursive=true)
    end
    nothing
end

function sendfile(src::AbstractString, dst::AbstractString)
    local src_open = false,
          dst_open = false,
          src_file,
          dst_file
    try
        src_file = open(src, JL_O_RDONLY)
        src_open = true
        dst_file = open(dst, JL_O_CREAT | JL_O_TRUNC | JL_O_WRONLY,
             S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP| S_IROTH | S_IWOTH)
        dst_open = true

        bytes = filesize(stat(src_file))
        sendfile(dst_file, src_file, Int64(0), Int(bytes))
    finally
        if src_open && isopen(src_file)
            close(src_file)
        end
        if dst_open && isopen(dst_file)
            close(dst_file)
        end
    end
end

@windows_only const UV_FS_SYMLINK_JUNCTION = 0x0002
function symlink(p::AbstractString, np::AbstractString)
    @windows_only if Base.windows_version() < Base.WINDOWS_VISTA_VER
        error("Windows XP does not support soft symlinks")
    end
    flags = 0
    @windows_only if isdir(p); flags |= UV_FS_SYMLINK_JUNCTION; p = abspath(p); end
    err = ccall(:jl_fs_symlink, Int32, (Cstring, Cstring, Cint), p, np, flags)
    @windows_only if err < 0
        Base.warn_once("Note: on Windows, creating file symlinks requires Administrator privileges.")
    end
    uv_error("symlink",err)
end

function readlink(path::AbstractString)
    req = Libc.malloc(_sizeof_uv_fs)
    try
        ret = ccall(:uv_fs_readlink, Int32,
            (Ptr{Void}, Ptr{Void}, Cstring, Ptr{Void}),
            eventloop(), req, path, C_NULL)
        if ret < 0
            ccall(:uv_fs_req_cleanup, Void, (Ptr{Void}, ), req)
            uv_error("readlink", ret)
            assert(false)
        end
        tgt = bytestring(ccall(:jl_uv_fs_t_ptr, Ptr{Cchar}, (Ptr{Void}, ), req))
        ccall(:uv_fs_req_cleanup, Void, (Ptr{Void}, ), req)
        return tgt
    finally
        Libc.free(req)
    end
end

function chmod(p::AbstractString, mode::Integer)
    err = ccall(:jl_fs_chmod, Int32, (Cstring, Cint), p, mode)
    uv_error("chmod",err)
    nothing
end

end
