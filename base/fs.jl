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
       S_IRUSR, S_IWUSR, S_IXUSR, S_IRWXU,
       S_IRGRP, S_IWGRP, S_IXGRP, S_IRWXG,
       S_IROTH, S_IWOTH, S_IXOTH, S_IRWXO

import Base: uvtype, uvhandle, eventloop, fd, position, stat, close, write, read, readall, 
            _sizeof_uv_fs, c_malloc, c_free

include("file_constants.jl")

abstract AbstractFile <: IO

type File <: AbstractFile
    path::String
    open::Bool
    handle::Int32
    File(path::String) = new(path,false,-1)
    File(fd::RawFD) = new("",true,fd.fd)
end

type AsyncFile <: AbstractFile
    path::String
    open::Bool
end

uvtype(::File) = Base.UV_RAW_FD
uvhandle(file::File) = file.handle

_uv_fs_result(req) = ccall(:jl_uv_fs_result,Int32,(Ptr{Void},),req)

function open(f::File,flags::Integer,mode::Integer)
    req = c_malloc(_sizeof_uv_fs)
    ret = ccall(:uv_fs_open,Int32,(Ptr{Void},Ptr{Void},Ptr{Uint8},Int32,Int32,Ptr{Void}),
                eventloop(),req,bytestring(f.path),flags,mode,C_NULL)
    f.handle = _uv_fs_result(req)
    ccall(:uv_fs_req_cleanup,Void,(Ptr{Void},),req)
    c_free(req)
    uv_error(:open,ret==-1)
    f.open = true
    f
end
open(f::String,flags,mode) = open(File(f),flags,mode)
open(f::String,flags) = open(f,flags,0)

function close(f::File)
    if !f.open
        error("File is already closed")
    end
    req = c_malloc(_sizeof_uv_fs)
    err = ccall(:uv_fs_close,Int32,(Ptr{Void},Ptr{Void},Int32,Ptr{Void}),
                eventloop(),req,f.handle,C_NULL)
    ccall(:uv_fs_req_cleanup,Void,(Ptr{Void},),req)
    c_free(req)
    uv_error("close",err != 0)
    f.handle = -1
    f.open = false
    f
end

function unlink(p::String)
    req = c_malloc(_sizeof_uv_fs)
    err = ccall(:uv_fs_unlink,Int32,(Ptr{Void},Ptr{Void},Ptr{Uint8},Ptr{Void}),
                eventloop(),req,bytestring(p),C_NULL)
    c_free(req)
    uv_error("unlink",err != 0)
end
function unlink(f::File)
    if isempty(f.path)
      error("No path associated with this file")
    end
    if f.open
        close(f)
    end
    unlink(f.path)
    f
end

function write(f::File,buf::Ptr{Uint8},len::Integer,offset::Integer)
    if !f.open
        error("File is not open")
    end
    req = Base.c_malloc(_sizeof_uv_fs)
    err = ccall(:uv_fs_write,Int32,(Ptr{Void},Ptr{Void},Int32,Ptr{Uint8},Csize_t,Int64,Ptr{Void}),
                eventloop(),req,f.handle,buf,len,offset,C_NULL)
    Base.c_free(req)
    uv_error("write",err == -1)
    len
end

function write(f::File, c::Uint8)
    a = [c]
    write(f,pointer(a),1)
end

function write{T}(f::File, a::Array{T})
    if isbits(T)
        write(f,pointer(a),length(a)*sizeof(eltype(a)))
    else
        invoke(write, (IO, Array), f, a)
    end
end

function write(f::File, buf::Ptr{Uint8},len::Integer)
      if !f.open
        error("File is not open")
    end
    req = Base.c_malloc(_sizeof_uv_fs)
    err = ccall(:uv_fs_write,Int32,(Ptr{Void},Ptr{Void},Int32,Ptr{Uint8},Csize_t,Int64,Ptr{Void}),
                eventloop(),req,f.handle,buf,len,-1,C_NULL)
    Base.c_free(req)
    uv_error("write",err == -1)
    len
end

function truncate(f::File, n::Integer)
    req = Base.c_malloc(_sizeof_uv_fs)
    err = ccall(:uv_fs_ftruncate,Int32,(Ptr{Void},Ptr{Void},Int32,Int64,Ptr{Void}),
                eventloop(),req,f.handle,n,C_NULL)
    c_free(req)
    uv_error(err)
    f    
end

function read(f::File, ::Type{Uint8})
    if !f.open
        error("File is not open")
    end
    req = Base.c_malloc(_sizeof_uv_fs)
    buf = Array(Uint8,1)
    err = ccall(:uv_fs_read,Int32,(Ptr{Void},Ptr{Void},Int32,Ptr{Uint8},Csize_t,Int64,Ptr{Void}),
                eventloop(),req,f.handle,buf,len,offset,C_NULL)
    Base.c_free(req)
    uv_error("write",err == -1)
    len 
end

function read{T}(f::File, a::Array{T})
    if isbits(T)
        nb = length(a)*sizeof(T)
        req = Base.c_malloc(_sizeof_uv_fs) 
        buf = Array(Uint8,1)
        err = ccall(:uv_fs_read,Int32,(Ptr{Void},Ptr{Void},Int32,Ptr{Uint8},Csize_t,Int64,Ptr{Void}),
                    eventloop(),req,f.handle,a,nb,-1,C_NULL)
        Base.c_free(req)
        uv_error("write",err == -1)
    else
        invoke(read, (IO, Array), s, a)
    end
end

function readall(f::File)
    a = Array(Uint8, filesize(f) - position(f))
    read(f,a)
    is_valid_ascii(a) ? ASCIIString(a) : UTF8String(a)
end

const SEEK_SET = int32(0)
const SEEK_CUR = int32(1)
const SEEK_END = int32(2)

position(f::File) = ccall(:lseek,Coff_t,(Int32,Coff_t,Int32),f.handle,0,SEEK_CUR)

fd(f::File) = RawFD(f.handle)
stat(f::File) = stat(fd(f))

end
