export name, isconnected, disconnect, url, set_url!, 
       push_url, set_push_url!, fetch_refspecs, push_refspecs, 
       add_fetch!, add_push!, clear_refspecs!, save!, rename!,
       update_tips!, remote_fetch

function name(r::GitRemote)
    @assert r.ptr != C_NULL
    name_ptr = api.git_remote_name(r.ptr)
    return name_ptr != C_NULL ? bytestring(name_ptr) : nothing
end

function isconnected(r::GitRemote)
    @assert r.ptr != C_NULL
    return bool(api.git_remote_connected(r.ptr))
end

function disconnect(r::GitRemote)
    @assert r.ptr != C_NULL
    api.git_remote_disconnect(r.ptr)
    return 
end

Base.connect(r::GitRemote, direction::Symbol) = begin
    local dir::Cint
    if direction == :fetch
        dir = api.DIRECTION_FETCH
    elseif direction == :push
        dir = api.DIRECTION_PUSH
    else
        throw(ArgumentError("direction can be :fetch or :push, got :$direction"))
    end
    @check api.git_remote_connect(r.ptr, dir)
    return 
end

Base.connect(f::Function, r::GitRemote, direction::Symbol) = begin
    connect(r, direction)
    try
        f(r)
    finally
        disconnect(r)
    end
end

function check_valid_url(url::String)
    if !bool(api.git_remote_valid_url(bytestring(url))) 
        throw(ArgumentError("Invalid URL : $url"))
    end
    return true
end

function url(r::GitRemote)
    @assert r.ptr != C_NULL
    url_ptr = api.git_remote_url(r.ptr)
    return url_ptr != C_NULL ? bytestring(url_ptr) : nothing
end

function set_url!(r::GitRemote, url::String)
    @assert r.ptr != C_NULL
    check_valid_url(url)
    @check api.git_remote_set_url(r.ptr, bytestring(url))
    return r
end

function push_url(r::GitRemote)
    @assert r.ptr != C_NULL
    url_ptr = api.git_remote_pushurl(r.ptr)
    return url_ptr != C_NULL ? bytestring(url_ptr) : nothing
end

function set_push_url!(r::GitRemote, url::String)
    @assert r.ptr != C_NULL
    check_valid_url(url)
    @check api.git_remote_set_pushurl(r.ptr, bytestring(url))
    return r
end

function remote_fetch(r::GitRemote)
    @assert r.ptr != C_NULL
    @check api.git_remote_fetch(r.ptr)
    return
end

function fetch_refspecs(r::GitRemote) 
    @assert r.ptr != C_NULL
    sarr = api.GitStrArray()
    @check ccall((:git_remote_get_fetch_refspecs, api.libgit2), Cint,
                  (Ptr{api.GitStrArray}, Ptr{Void}),
                   &sarr, r.ptr)
    refs = Array(String, sarr.count)
    for i in 1:sarr.count
        refs[i] = bytestring(unsafe_load(sarr.strings, i))
    end
    return refs
end

function push_refspecs(r::GitRemote)
    @assert r.ptr != C_NULL
    sarr = api.GitStrArray()
    @check ccall((:git_remote_get_push_refspecs, api.libgit2), Cint,
                  (Ptr{api.GitStrArray}, Ptr{Void}),
                   &sarr, r.ptr)
    refs = Array(String, sarr.count)
    for i in 1:sarr.count
        refs[i] = bytestring(unsafe_load(sarr.strings, i))
    end
    return refs
end

function add_push!(r::GitRemote, ref::String)
    @assert r.ptr != C_NULL
    @check api.git_remote_add_push(r.ptr, bytestring(ref))
    return r
end

function add_fetch!(r::GitRemote, ref::String)
    @assert r.ptr != C_NULL
    @check api.git_remote_add_fetch(r.ptr, bytestring(ref))
    return r
end

function clear_refspecs!(r::GitRemote)
    @assert r.ptr != C_NULL
    api.git_remote_clear_refspecs(r.ptr)
    return r
end

function save!(r::GitRemote)
    @assert r.ptr != C_NULL
    @check api.git_remote_save(r.ptr)
    return r
end

function cb_remote_rename(refspec_name::Ptr{Cchar}, payload::Ptr{Void})
    errs = unsafe_pointer_to_objref(payload)::Array{ByteString, 1}
    push!(errs, bytestring(refspec_name))
    return api.GIT_OK
end

const c_cb_remote_rename = cfunction(cb_remote_rename, Cint, (Ptr{Cchar}, Ptr{Void}))

function rename!(r::GitRemote, name::String) 
    @assert r.ptr != C_NULL
    errs = ByteString[]
    @check ccall((:git_remote_rename, api.libgit2), Cint,
                  (Ptr{Void}, Ptr{Cchar}, Ptr{Void}, Any),
                  r.ptr, bytestring(name), c_cb_remote_rename, &errs)
    return length(errs) == 0 ? nothing : errs
end

type RemoteHead
    islocal::Bool
    id::Oid
    lid::Union(Nothing, Oid)
    name::ByteString
end

RemoteHead(ghead::api.GitRemoteHead) = begin
    oid_arr = Array(Uint8, api.OID_RAWSZ)
    @get_oid_fieldnames(oid_arr, ghead, oid)
    id = Oid(oid_arr)
    @get_oid_fieldnames(oid_arr, ghead, loid)
    lid = Oid(oid_arr)
    return RemoteHead(bool(ghead.islocal),
                      id,
                      iszero(lid)? nothing : lid,
                      ghead.name == C_NULL ? "" : bytestring(ghead.name))
end

Base.ls(r::GitRemote) = begin
    @assert r.ptr != C_NULL
    nheads = Csize_t[0]
    head_ptrs = Array(Ptr{Ptr{api.GitRemoteHead}}, 1)
    @check api.git_remote_ls(head_ptrs, nheads, r.ptr)
    head_ptr = head_ptrs[1]
    remote_heads = RemoteHead[]
    for i in 1:nheads[1]
        ghead = unsafe_load(unsafe_load(head_ptr, i))
        push!(remote_heads, RemoteHead(ghead))
    end
    return remote_heads
end

Base.download(r::GitRemote) = begin
    @assert r.ptr != C_NULL
    @check api.git_remote_download(r.ptr)
    return r
end

#TODO: this should accept a signature and message
function update_tips!(r::GitRemote)
    @assert r.ptr != C_NULL
    @check ccall((:git_remote_update_tips, api.libgit2), Cint,
                 (Ptr{Void}, Ptr{api.GitSignature}, Ptr{Cchar}),
                 r.ptr, C_NULL, C_NULL)
    return r
end
