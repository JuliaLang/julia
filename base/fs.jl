# This file is a part of Julia. License is MIT: http://julialang.org/license

## UV based file operations ##

module FS

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
            _sizeof_uv_fs, uv_error

include("file_constants.jl")

abstract AbstractFile <: IO

type File <: AbstractFile
    path::AbstractString
    open::Bool
    handle::Int32
    File(path::AbstractString) = new(path,false,-1)
    File(fd::RawFD) = new("",true,fd.fd)
end

type AsyncFile <: AbstractFile
    path::AbstractString
    open::Bool
end

isopen(f::Union{File,AsyncFile}) = f.open

# Not actually a pointer, but that's how we pass it through the C API so it's fine
uvhandle(file::File) = convert(Ptr{Void}, file.handle % UInt)
uvtype(::File) = Base.UV_RAW_FD

_uv_fs_result(req) = ccall(:jl_uv_fs_result,Int32,(Ptr{Void},),req)

function open(f::File,flags::Integer,mode::Integer=0)
    req = Libc.malloc(_sizeof_uv_fs)
    ret = ccall(:uv_fs_open,Int32,(Ptr{Void},Ptr{Void},Cstring,Int32,Int32,Ptr{Void}),
                eventloop(), req, f.path, flags,mode, C_NULL)
    f.handle = _uv_fs_result(req)
    ccall(:uv_fs_req_cleanup,Void,(Ptr{Void},),req)
    Libc.free(req)
    uv_error("open",ret)
    f.open = true
    f
end
open(f::AbstractString,flags,mode) = open(File(f),flags,mode)
open(f::AbstractString,flags) = open(f,flags,0)

function close(f::File)
    if !f.open
        throw(ArgumentError("file \"$(f.path)\" is already closed"))
    end
    err = ccall(:jl_fs_close, Int32, (Int32,), f.handle)
    uv_error("close",err)
    f.handle = -1
    f.open = false
    f
end

function unlink(p::AbstractString)
    err = ccall(:jl_fs_unlink, Int32, (Cstring,), p)
    uv_error("unlink",err)
end
function unlink(f::File)
    if isempty(f.path)
      throw(ArgumentError("no path associated with this file"))
    end
    if f.open
        close(f)
    end
    unlink(f.path)
    f
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
end

# For copy command
function sendfile(src::AbstractString, dst::AbstractString)
    src_file = File(src)
    dst_file = File(dst)
    try
        open(src_file, JL_O_RDONLY)
        if !src_file.open
            throw(ArgumentError("source file \"$(src.path)\" is not open"))
        end

        open(dst_file, JL_O_CREAT | JL_O_TRUNC | JL_O_WRONLY,
             S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP| S_IROTH | S_IWOTH)
        if !dst_file.open
            throw(ArgumentError("destination file \"$(dst.path)\" is not open"))
        end

        src_stat = stat(src_file)
        err = ccall(:jl_fs_sendfile, Int32, (Int32, Int32, Int64, Csize_t),
                    fd(src_file), fd(dst_file), 0, src_stat.size)
        uv_error("sendfile", err)
    finally
        if src_file.open
            close(src_file)
        end
        if dst_file.open
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
    ret = ccall(:uv_fs_readlink, Int32,
        (Ptr{Void}, Ptr{Void}, Cstring, Ptr{Void}),
        eventloop(), req, path, C_NULL)
    uv_error("readlink", ret)
    tgt = bytestring(ccall(:jl_uv_fs_t_ptr, Ptr{Cchar}, (Ptr{Void}, ), req))
    ccall(:uv_fs_req_cleanup, Void, (Ptr{Void}, ), req)
    Libc.free(req)
    tgt
end

function chmod(p::AbstractString, mode::Integer)
    err = ccall(:jl_fs_chmod, Int32, (Cstring, Cint), p, mode)
    uv_error("chmod",err)
end

function write(f::File, buf::Ptr{UInt8}, len::Integer, offset::Integer=-1)
    if !f.open
        throw(ArgumentError("file \"$(f.path)\" is not open"))
    end
    err = ccall(:jl_fs_write, Int32, (Int32, Ptr{UInt8}, Csize_t, Int64),
                f.handle, buf, len, offset)
    uv_error("write",err)
    len
end

write(f::File, c::UInt8) = write(f,[c])

function write{T}(f::File, a::Array{T})
    if isbits(T)
        write(f,pointer(a),length(a)*sizeof(eltype(a)))
    else
        invoke(write, Tuple{IO, Array}, f, a)
    end
end

function truncate(f::File, n::Integer)
    req = Base.Libc.malloc(_sizeof_uv_fs)
    err = ccall(:uv_fs_ftruncate,Int32,(Ptr{Void},Ptr{Void},Int32,Int64,Ptr{Void}),
                eventloop(),req,f.handle,n,C_NULL)
    Libc.free(req)
    uv_error("ftruncate", err)
    f
end

function futime(f::File, atime::Float64, mtime::Float64)
    req = Base.Libc.malloc(_sizeof_uv_fs)
    err = ccall(:uv_fs_futime,Int32,(Ptr{Void},Ptr{Void},Int32,Float64,Float64,Ptr{Void}),
                eventloop(),req,f.handle,atime,mtime,C_NULL)
    Libc.free(req)
    uv_error("futime", err)
    f
end

function read(f::File, ::Type{UInt8})
    if !f.open
        throw(ArgumentError("file \"$(f.path)\" is not open"))
    end
    ret = ccall(:jl_fs_read_byte, Int32, (Int32,), f.handle)
    uv_error("read", ret)
    return ret%UInt8
end

function read!(f::File, a::Vector{UInt8}, nel=length(a))
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
    a
end

const SEEK_SET = Int32(0)
const SEEK_CUR = Int32(1)
const SEEK_END = Int32(2)

function position(f::File)
    ret = ccall(:jl_lseek, Coff_t,(Int32,Coff_t,Int32),f.handle,0,SEEK_CUR)
    systemerror("lseek", ret == -1)
    ret
end

fd(f::File) = RawFD(f.handle)
stat(f::File) = stat(fd(f))

end
