# This file is a part of Julia. License is MIT: http://julialang.org/license

function GitConfig(path::AbstractString)
    cfg_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    err = ccall((:git_config_open_ondisk, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{UInt8}), cfg_ptr_ptr, path)
    err !=0 && return nothing
    return GitConfig(cfg_ptr_ptr[])
end

function GitConfig(r::GitRepo)
    cfg_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    err = ccall((:git_repository_config, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}), cfg_ptr_ptr, r.ptr)
    err !=0 && return nothing
    return GitConfig(cfg_ptr_ptr[])
end

function GitConfig()
    cfg_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    ccall((:git_config_open_default, :libgit2), Cint,
          (Ptr{Ptr{Void}}, ), cfg_ptr_ptr)
    return GitConfig(cfg_ptr_ptr[])
end

function get{T}(::Type{T}, c::GitConfig, name::AbstractString)
    if T<:AbstractString
        str_ptr = Ref{Ptr{UInt8}}(C_NULL)
        @check ccall((:git_config_get_string, :libgit2), Cint, #TODO: git_config_get_string_buf
                    (Ptr{Ptr{UInt8}}, Ptr{Void}, Ptr{UInt8}), str_ptr, c.ptr, name)
        return bytestring(str_ptr[])
    elseif is(T, Bool)
        val_ptr = Ref(Cint(0))
        @check ccall((:git_config_get_bool, :libgit2), Cint,
              (Ptr{Cint}, Ptr{Void}, Ptr{UInt8}), val_ptr, c.ptr, name)
        return Bool(val_ptr[])
    elseif is(T, Int32)
        val_ptr = Ref(Cint(0))
        @check ccall((:git_config_get_bool, :libgit2), Cint,
              (Ptr{Cint}, Ptr{Void}, Ptr{UInt8}), val_ptr, c.ptr, name)
        return val_ptr[]
    elseif is(T, Int64)
        val_ptr = Ref(Cintmax_t(0))
        @check ccall((:git_config_get_bool, :libgit2), Cint,
              (Ptr{Cintmax_t}, Ptr{Void}, Ptr{UInt8}), val_ptr, c.ptr, name)
        return val_ptr[]
    else
        return nothing
    end
end

function get{T}(c::GitConfig, name::AbstractString, default::T)
    res = default
    try res = get(T,c,name) end
    return res
end

function getconfig{T}(r::GitRepo, name::AbstractString, default::T)
    with(GitConfig, r) do cfg
        get(cfg, name, default)
    end
end

function getconfig{T}(rname::AbstractString, name::AbstractString, default::T)
    with(GitRepo, rname) do r
        with(GitConfig, r) do cfg
            get(cfg, name, default)
        end
    end
end

function getconfig{T}(name::AbstractString, default::T)
    with(GitConfig) do cfg
        get(cfg, name, default)
    end
end

function set!{T <: AbstractString}(c::GitConfig, name::AbstractString, value::T)
    @check ccall((:git_config_set_string, :libgit2), Cint,
             (Ptr{Void}, Ptr{UInt8}, Ptr{UInt8}), c.ptr, name, value)
end

function set!(c::GitConfig, name::AbstractString, value::Bool)
    bval = Int32(value)
    @check ccall((:git_config_set_bool, :libgit2), Cint,
             (Ptr{Void}, Ptr{UInt8}, Cint), c.ptr, name, bval)
end

function set!(c::GitConfig, name::AbstractString, value::Int32)
    @check ccall((:git_config_set_int32, :libgit2), Cint,
                 (Ptr{Void}, Ptr{UInt8}, Cint), c.ptr, name, value)
end

function set!(c::GitConfig, name::AbstractString, value::Int64)
    @check ccall((:git_config_set_int64, :libgit2), Cint,
             (Ptr{Void}, Ptr{UInt8}, Cintmax_t), c.ptr, name, value)
end
