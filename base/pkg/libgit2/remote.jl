function GitRemote(repo::GitRepo, rmt_name::AbstractString, rmt_url::AbstractString)
    rmt_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    err = ccall((:git_remote_create, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}, Ptr{UInt8}),
                rmt_ptr_ptr, repo.ptr, rmt_name, rmt_url)
    err != 0 && return GitError(err)
    return GitRemote(rmt_ptr_ptr[])
end

function get(::Type{GitRemote}, repo::GitRepo, rmt_name::AbstractString)
    rmt_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    err = ccall((:git_remote_lookup, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}),
                rmt_ptr_ptr, repo.ptr, rmt_name)
    err != 0 && return GitError(err)
    return GitRemote(rmt_ptr_ptr[])
end

function save(rmt::GitRemote)
    err = ccall((:git_remote_save, :libgit2), Cint, (Ptr{Void}, ), rmt.ptr)
    err != 0 && return GitError(err)
end

function url(rmt::GitRemote)
    url_ptr = ccall((:git_remote_url, :libgit2), Ptr{UInt8}, (Ptr{Void}, ), rmt.ptr)
    url_ptr == C_NULL && return ""
    return  bytestring(url_ptr)
end
