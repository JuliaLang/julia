# This file is a part of Julia. License is MIT: http://julialang.org/license

function GitConfig(path::AbstractString,
                   level::Consts.GIT_CONFIG = Consts.CONFIG_LEVEL_APP,
                   force::Bool=false)
    # create new config object
    cfg_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_config_new, :libgit2), Cint, (Ptr{Ptr{Void}},), cfg_ptr_ptr)
    cfg = GitConfig(cfg_ptr_ptr[])
    try
        addfile(cfg, path, level, force)
    catch ex
        close(cfg)
        rethrow(ex)
    end
    return cfg
end

function GitConfig(repo::GitRepo)
    cfg_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_repository_config, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}), cfg_ptr_ptr, repo.ptr)
    return GitConfig(repo, cfg_ptr_ptr[])
end

function GitConfig(level::Consts.GIT_CONFIG = Consts.CONFIG_LEVEL_DEFAULT)
    cfg_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_config_open_default, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, ), cfg_ptr_ptr)
    cfg = GitConfig(cfg_ptr_ptr[])
    if level != Consts.CONFIG_LEVEL_DEFAULT
        glb_cfg_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
        tmpcfg = cfg
        try
            @check ccall((:git_config_open_level, :libgit2), Cint,
                         (Ptr{Ptr{Void}}, Ptr{Void}, Cint),
                          glb_cfg_ptr_ptr, cfg.ptr, Cint(level))
            cfg = GitConfig(glb_cfg_ptr_ptr[])
        finally
            close(tmpcfg)
        end
    end
    return cfg
end

function addfile(cfg::GitConfig, path::AbstractString,
                 level::Consts.GIT_CONFIG = Consts.CONFIG_LEVEL_APP,
                 force::Bool=false)
    @check ccall((:git_config_add_file_ondisk, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Cstring, Cint, Cint),
                   cfg.ptr, path, Cint(level), Cint(force))
end

function get{T<:AbstractString}(::Type{T}, c::GitConfig, name::AbstractString)
    buf_ref = Ref(Buffer())
    @check ccall((:git_config_get_string_buf, :libgit2), Cint,
                 (Ptr{Buffer}, Ptr{Void}, Cstring), buf_ref, c.ptr, name)
    buf = buf_ref[]
    str = unsafe_string(buf.ptr, buf.size)
    free(buf_ref)
    str
end

function get(::Type{Bool}, c::GitConfig, name::AbstractString)
    val_ptr = Ref(Cint(0))
    @check ccall((:git_config_get_bool, :libgit2), Cint,
          (Ptr{Cint}, Ptr{Void}, Cstring), val_ptr, c.ptr, name)
    return Bool(val_ptr[])
end

function get(::Type{Int32}, c::GitConfig, name::AbstractString)
    val_ptr = Ref(Cint(0))
    @check ccall((:git_config_get_int32, :libgit2), Cint,
          (Ptr{Cint}, Ptr{Void}, Cstring), val_ptr, c.ptr, name)
    return val_ptr[]
end

function get(::Type{Int64}, c::GitConfig, name::AbstractString)
    val_ptr = Ref(Cintmax_t(0))
    @check ccall((:git_config_get_int64, :libgit2), Cint,
          (Ptr{Cintmax_t}, Ptr{Void}, Cstring), val_ptr, c.ptr, name)
    return val_ptr[]
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
                  (Ptr{Void}, Cstring, Cstring), c.ptr, name, value)
end

function set!(c::GitConfig, name::AbstractString, value::Bool)
    bval = Int32(value)
    @check ccall((:git_config_set_bool, :libgit2), Cint,
                  (Ptr{Void}, Cstring, Cint), c.ptr, name, bval)
end

function set!(c::GitConfig, name::AbstractString, value::Int32)
    @check ccall((:git_config_set_int32, :libgit2), Cint,
                  (Ptr{Void}, Cstring, Cint), c.ptr, name, value)
end

function set!(c::GitConfig, name::AbstractString, value::Int64)
    @check ccall((:git_config_set_int64, :libgit2), Cint,
                  (Ptr{Void}, Cstring, Cintmax_t), c.ptr, name, value)
end
