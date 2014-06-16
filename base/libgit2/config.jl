export GitConfig, lookup, set!, global_config

#TODO: make sure returning git config entries is not leaking memory

type GitConfig 
    ptr::Ptr{Void}

    function GitConfig(ptr::Ptr{Void})
        @assert ptr != C_NULL
        c = new(ptr)
        finalizer(c, free!)
        return c
    end
end

GitConfig(path::String) = begin
    bpath = bytestring(path)
    cfg_ptr = Array(Ptr{Void}, 1)
    @check api.git_config_open_ondisk(cfg_ptr, bpath)
    return GitConfig(cfg_ptr[1])
end

free!(c::GitConfig) = begin
    if c.ptr != C_NULL
        api.git_config_free(c.ptr)
        c.ptr == C_NULL
    end
end

typealias GitConfigType Union(Bool,Int32,Int64,String)

Base.getindex(c::GitConfig, key::String) = begin
    return lookup(String, c, key)
end

Base.setindex!{T<:GitConfigType}(c::GitConfig, v::T, key::String) = begin
    set!(T, c, key, v)
end

Base.delete!(c::GitConfig, key::String) = begin
    @assert c.ptr != C_NULL
    err = api.git_config_delete_entry(c.ptr, bytestring(key))
    return err == api.ENOTFOUND ? false : true
end

function cb_each_key(entry_ptr::Ptr{api.GitConfigEntry}, o::Ptr{Void})
    entry = unsafe_load(entry_ptr) 
    n = bytestring(entry.name)
    produce(n)
    return api.GIT_OK
end

function cb_each_val(entry_ptr::Ptr{api.GitConfigEntry}, o::Ptr{Void})
    entry = unsafe_load(entry_ptr)
    v = bytestring(entry.value)
    produce(v)
    return api.GIT_OK
end

function cb_each_pair(entry_ptr::Ptr{api.GitConfigEntry}, o::Ptr{Void})
    entry = unsafe_load(entry_ptr)
    n = bytestring(entry.name)
    v = bytestring(entry.value)
    produce((n, v))
    return api.GIT_OK
end

const c_cb_each_key = cfunction(cb_each_key, Cint, 
                               (Ptr{api.GitConfigEntry}, Ptr{Void}))
const c_cb_each_val = cfunction(cb_each_val, Cint, 
                               (Ptr{api.GitConfigEntry}, Ptr{Void}))
const c_cb_each_kvpair = cfunction(cb_each_pair, Cint, 
                                  (Ptr{api.GitConfigEntry}, Ptr{Void}))

Base.keys(c::GitConfig) = begin
    @assert c.ptr != C_NULL
    @task api.git_config_foreach(c.ptr, c_cb_each_key, C_NULL)
end

Base.values(c::GitConfig) = begin
    @assert c.ptr != C_NULL
    @task api.git_config_foreach(c.ptr, c_cb_each_key, C_NULL)
end

Base.start(c::GitConfig) = begin
    @assert c.ptr != C_NULL
    t = @task api.git_config_foreach(c.ptr, c_cb_each_kvpair, C_NULL)
    (consume(t), t)
end

Base.done(c::GitConfig, state) = begin
    istaskdone(state[2])
end

Base.next(c::GitConfig, state) = begin
    v = consume(last(state))
    (state[1], (v, state[2]))
end

Base.Dict(c::GitConfig) = begin
   Dict{String,String}(c)
end

function global_config()
    cfg_ptr = Array(Ptr{Void}, 1)
    @check api.git_config_open_default(cfg_ptr)
    return GitConfig(cfg_ptr[1])
end

function lookup(::Type{Bool}, c::GitConfig, name::String)
    @assert c.ptr != C_NULL
    out = Int32[0]
    bname = bytestring(name)
    @check api.git_config_get_bool(out, c.ptr, name)
    if err == api.GIT_OK
        return out[1] > 0 ? true : false
    elseif err == api.ENOTFOUND
        return nothing
    else
        throw(GitError(err))
    end
end

function set!(::Type{Bool}, c::GitConfig, value::Bool)
    @assert c.ptr != C_NULL
    bname = bytestring(name)
    cval  = value? 1 : 0
    @check api.git_config_get_bool(out, c.ptr, cval)
    return nothing
end

function lookup(::Type{Int32}, c::GitConfig, name::String)
    @assert c.ptr != C_NULL
    out = Cint[0]
    bname = bytestring(n)
    err = api.git_config_get_int32(out, c.ptr, bname)
    if err == api.GIT_OK
        return out[1]
    elseif err == api.ENOTFOUND
        return nothing
    else
        throw(GitError(err))
    end
end

function set!(::Type{Int32}, c::GitConfig, name::String, value::Int32)
    @assert c.ptr != C_NULL
    bname = bytestring(name)
    @check api.git_config_set_int32(c.ptr, bname, bvalue)
    return nothing
end

function lookup(::Type{Int64}, c::GitConfig, name::String)
    @assert c.ptr != C_NULL
    out = Int64[0]
    bname = bytestring(name)
    err = api.git_config_get_int64(out, c.ptr, bname)
    if err == api.GIT_OK
        return out[1]
    elseif err == api.ENOTFOUND
        return nothing
    else
        throw(GitError(err))
    end
end

function set!(::Type{Int64}, c::GitConfig, name::String, value::Int64)
    @assert c.ptr != C_NULL
    bname = bytestring(name)
    @check api.git_config_set_int64(c.ptr, bname, bvalue)
    return nothing
end

function lookup{T<:String}(::Type{T}, c::GitConfig, name::String)
    @assert c.ptr != C_NULL
    ptr = Array(Ptr{Cchar}, 1)
    bname = bytestring(name)
    err = api.git_config_get_string(ptr, c.ptr, bname)
    if err == api.GIT_OK
        return bytestring(ptr[1])
    elseif err == api.ENOTFOUND
        return nothing
    else
        throw(GitError(err))
    end
end

function set!{T<:String}(::Type{T}, c::GitConfig, name::String, value::String)
    @assert c.ptr != C_NULL
    bname  = bytestring(name)
    bvalue = bytestring(value)
    @check api.git_config_set_string(c.ptr, bname, bvalue)
    return nothing
end
