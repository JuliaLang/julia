type GitConfig
    ptr::Ptr{Void}

    function GitConfig(ptr::Ptr{Void})
        @assert ptr != C_NULL
        cfg = new(ptr)
        finalizer(cfg, free!)
        return cfg
    end
end

function free!(cfg::GitConfig)
    if cfg.ptr != C_NULL
        ccall((:git_config_free, :libgit2), Void, (Ptr{Void},), cfg.ptr)
        cfg.ptr = C_NULL
    end
end

GitConfig(path::AbstractString) = begin
    cfg_ptr = Ptr{Void}[C_NULL]
    err = ccall((:git_config_open_ondisk, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Uint8}), cfg_ptr, path)
    err !=0 && return nothing
    return GitConfig(cfg_ptr[1])
end

GitConfig(r::GitRepo) = begin
    cfg_ptr = Ptr{Void}[C_NULL]
    err = ccall((:git_repository_config, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}), cfg_ptr, r)
    err !=0 && return nothing
    return GitConfig(cfg_ptr[1])
end

GitConfig() = begin
    cfg_ptr = Ptr{Void}[C_NULL]
    ccall((:git_config_open_default, :libgit2), Cint, (Ptr{Ptr{Void}}, ), cfg_ptr)
    return GitConfig(cfg_ptr[1])
end

function lookup{T<:AbstractString}(::Type{T}, c::GitConfig, name::AbstractString)
    out = Ptr{Uint8}[0]
    err = ccall((:git_config_get_string, :libgit2), Cint,
                (Ptr{Ptr{Uint8}}, Ptr{Void}, Ptr{Uint8}), out, c.ptr, name)
    if err == GitErrorConst.GIT_OK
        return bytestring(out[1])
    else
        return nothing
    end
end

function set!{T<:AbstractString}(::Type{T}, c::GitConfig, name::AbstractString, value::AbstractString)
    err = ccall((:git_config_set_string, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Uint8}, Ptr{Uint8}), c.ptr, name, value)
    return err
end
