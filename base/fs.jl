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

isopen(f::Union(File,AsyncFile)) = f.open

uvtype(::File) = Base.UV_RAW_FD
uvhandle(file::File) = file.handle

_uv_fs_result(req) = ccall(:jl_uv_fs_result,Int32,(Ptr{Void},),req)

function open(f::File,flags::Integer,mode::Integer)
    req = c_malloc(_sizeof_uv_fs)
    ret = ccall(:uv_fs_open,Int32,(Ptr{Void},Ptr{Void},Ptr{UInt8},Int32,Int32,Ptr{Void}),
                eventloop(), req, f.path, flags,mode, C_NULL)
    f.handle = _uv_fs_result(req)
    ccall(:uv_fs_req_cleanup,Void,(Ptr{Void},),req)
    c_free(req)
    uv_error("open",ret)
    f.open = true
    f
end
open(f::AbstractString,flags,mode) = open(File(f),flags,mode)
open(f::AbstractString,flags) = open(f,flags,0)

function close(f::File)
    if !f.open
        error("file is already closed")
    end
    err = ccall(:jl_fs_close, Int32, (Int32,), f.handle)
    uv_error("close",err)
    f.handle = -1
    f.open = false
    f
end

function unlink(p::AbstractString)
    err = ccall(:jl_fs_unlink, Int32, (Ptr{UInt8},), p)
    uv_error("unlink",err)
end
function unlink(f::File)
    if isempty(f.path)
      error("no path associated with this file")
    end
    if f.open
        close(f)
    end
    unlink(f.path)
    f
end

# For move command
function rename(src::AbstractString, dst::AbstractString)
    err = ccall(:jl_fs_rename, Int32, (Ptr{UInt8}, Ptr{UInt8}), src, dst)

    # on error, default to cp && rm
    if err < 0
        # Note that those two functions already handle their errors.
        # first copy
        sendfile(src, dst)

        # then rm
        unlink(src)
    end
end

# For copy command
function sendfile(src::AbstractString, dst::AbstractString)
    src_file = open(src, JL_O_RDONLY)
    if !src_file.open
        error("Src file is not open")
    end

    dst_file = open(dst, JL_O_CREAT | JL_O_TRUNC | JL_O_WRONLY,
                    S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP| S_IROTH | S_IWOTH)
    if !dst_file.open
        error("Dst file is not open")
    end

    src_stat = stat(src_file)
    err = ccall(:jl_fs_sendfile, Int32, (Int32, Int32, Int64, Csize_t),
                fd(src_file), fd(dst_file), 0, src_stat.size)
    uv_error("sendfile", err)

    if src_file.open
        close(src_file)
    end
    if dst_file.open
        close(dst_file)
    end
end

@windows_only const UV_FS_SYMLINK_JUNCTION = 0x0002
@non_windowsxp_only function symlink(p::AbstractString, np::AbstractString)
    flags = 0
    @windows_only if isdir(p); flags |= UV_FS_SYMLINK_JUNCTION; p = abspath(p); end
    err = ccall(:jl_fs_symlink, Int32, (Ptr{UInt8}, Ptr{UInt8}, Cint), p, np, flags)
    @windows_only if err < 0
        Base.warn_once("Note: on Windows, creating file symlinks requires Administrator privileges.")
    end
    uv_error("symlink",err)
end
@windowsxp_only symlink(p::AbstractString, np::AbstractString) =
    error("WindowsXP does not support soft symlinks")

function chmod(p::AbstractString, mode::Integer)
    err = ccall(:jl_fs_chmod, Int32, (Ptr{UInt8}, Cint), p, mode)
    uv_error("chmod",err)
end

function write(f::File, buf::Ptr{UInt8}, len::Integer, offset::Integer=-1)
    if !f.open
        error("file is not open")
    end
    err = ccall(:jl_fs_write, Int32, (Int32, Ptr{UInt8}, Csize_t, Int64),
                f.handle, buf, len, offset)
    uv_error("write",err)
    len
end

function write(f::File, c::UInt8)
    if !f.open
        error("file is not open")
    end
    err = ccall(:jl_fs_write_byte, Int32, (Int32, Cchar), f.handle, c)
    uv_error("write",err)
    1
end

function write{T}(f::File, a::Array{T})
    if isbits(T)
        write(f,pointer(a),length(a)*sizeof(eltype(a)))
    else
        invoke(write, (IO, Array), f, a)
    end
end

function truncate(f::File, n::Integer)
    req = Base.c_malloc(_sizeof_uv_fs)
    err = ccall(:uv_fs_ftruncate,Int32,(Ptr{Void},Ptr{Void},Int32,Int64,Ptr{Void}),
                eventloop(),req,f.handle,n,C_NULL)
    c_free(req)
    uv_error("ftruncate", err)
    f
end

function futime(f::File, atime::Float64, mtime::Float64)
    req = Base.c_malloc(_sizeof_uv_fs)
    err = ccall(:uv_fs_futime,Int32,(Ptr{Void},Ptr{Void},Int32,Float64,Float64,Ptr{Void}),
                eventloop(),req,f.handle,atime,mtime,C_NULL)
    c_free(req)
    uv_error("futime", err)
    f
end

function read(f::File, ::Type{UInt8})
    if !f.open
        error("file is not open")
    end
    ret = ccall(:jl_fs_read_byte, Int32, (Int32,), f.handle)
    uv_error("read", ret)
    return uint8(ret)
end

function read!{T}(f::File, a::Array{T}, nel=length(a))
    if nel < 0 || nel > length(a)
        throw(BoundsError())
    end
    if isbits(T)
        nb = nel*sizeof(T)
        ret = ccall(:jl_fs_read, Int32, (Int32, Ptr{Void}, Csize_t),
                    f.handle, a, nb)
        uv_error("read",ret)
    else
        invoke(read, (IO, Array), s, a)
    end
    a
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

const SEEK_SET = int32(0)
const SEEK_CUR = int32(1)
const SEEK_END = int32(2)

function position(f::File)
    ret = ccall(:jl_lseek, Coff_t,(Int32,Coff_t,Int32),f.handle,0,SEEK_CUR)
    systemerror("lseek", ret == -1)
    ret
end

fd(f::File) = RawFD(f.handle)
stat(f::File) = stat(fd(f))

end
