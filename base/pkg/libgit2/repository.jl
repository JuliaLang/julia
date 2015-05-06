type GitRepo
    ptr::Ptr{Void}

    function GitRepo(ptr::Ptr{Void}, own::Bool=true)
        @assert ptr != C_NULL
        r = new(ptr)
        own && finalizer(r, free!)
        return r
    end
end

GitRepo(path::String) = begin
    repo_ptr = Ptr{Void}[0]
    err = ccall((:git_repository_open, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Uint8}), repo_ptr, path)
    if err != GitErrorConst.GIT_OK
        if repo_ptr[1] != C_NULL
            ccall((:git_repository_free, :libgit2), Void, (Ptr{Void},), repo_ptr[1])
        end
        return nothing
    end
    return GitRepo(repo_ptr[1])
end

close(r::GitRepo) = begin
    if r.ptr != C_NULL
        ccall((:git_repository__cleanup, :libgit2), Void, (Ptr{Void},), r.ptr)
    end
end

free!(r::GitRepo) = begin
    if r.ptr != C_NULL
        ccall((:git_repository_free, :libgit2), Void, (Ptr{Void},), r.ptr)
        r.ptr = C_NULL
    end
end