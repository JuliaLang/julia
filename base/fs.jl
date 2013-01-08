## UV based file operations ##

module FS
using Base

export File, open, close, unlink, write,
    JL_O_WRONLY, JL_O_RDONLY, JL_O_RDWR, JL_O_APPEND, JL_O_CREAT, JL_O_EXCL,
    JL_O_TRUNC, JL_O_TEMPORARY, JL_O_SHORT_LIVED, JL_O_SEQUENTIAL, JL_O_RANDOM

#import Base.show, Base.open, Base.close, Base.write

include("file_constants.jl")

abstract AbstractFile <: IO

const _sizeof_uv_fs_t = ccall(:jl_sizeof_uv_fs_t,Int32,())

type File <: AbstractFile
    path::String
    open::Bool
    handle::Int32
    File(path::String) = new(path,false,-1)
end

type AsyncFile <: AbstractFile
    path::String
    open::Bool
end

_uv_fs_result(req) = ccall(:jl_uv_fs_result,Int32,(Ptr{Void},),req)

function open(f::File,flags::Integer)
    req = Base.Intrinsics.box(Ptr{Void},Intrinsics.jl_alloca(Base.Intrinsics.unbox(Int32,_sizeof_uv_fs_t)))
    ccall(:uv_fs_open,Int32,(Ptr{Void},Ptr{Void},Ptr{Uint8},Int32,Int32,Ptr{Void}),
                         globalEventLoop(),req,bytestring(f.path),flags,0,C_NULL)
    uv_error(:open)
    f.handle = _uv_fs_result(req)
    f.open = true
    ccall(:uv_fs_req_cleanup,Void,(Ptr{Void},),req)
    f
end
open(f::String,flags::Integer) = open(File(f),flags)

function close(f::File)
    if(!f.open)
        error("File is already closed")
    end
    req = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_fs_t)))
    err = ccall(:uv_fs_close,Int32,(Ptr{Void},Ptr{Void},Int32,Ptr{Void}),
                         globalEventLoop(),req,f.handle,C_NULL)
    uv_error(err)
    f.handle = -1
    f.open = false
    ccall(:uv_fs_req_cleanup,Void,(Ptr{Void},),req)
    f
end

function unlink(p::String)
    req = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_fs_t)))
    err = ccall(:uv_fs_unlink,Int32,(Ptr{Void},Ptr{Void},Ptr{Uint8},Ptr{Void}),
                         globalEventLoop(),req,bytestring(p),C_NULL)
    uv_error(err)
end
function unlink(f::File)
    if(f.open)
        close(f)
    end
    unlink(f.path)
    f
end

function write(f::File,buf::Ptr{Uint8},len::Int32,offset::Int64)
    if(!f.open)
        error("File is not open")
    end
    req = box(Ptr{Void},Intrinsics.jl_alloca(unbox(Int32,_sizeof_uv_fs_t)))
    err = ccall(:uv_fs_close,Int32,(Ptr{Void},Ptr{Void},Int32,Ptr{Uint8},Int32,Int64,Ptr{Void}),
                         globalEventLoop(),req,f.handle,buf,len,offset,C_NULL)
    uv_error(err)
    f
end

end
