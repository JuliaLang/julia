# This file is a part of Julia. License is MIT: http://julialang.org/license

function GitRemote(repo::GitRepo, rmt_name::AbstractString, rmt_url::AbstractString)
    rmt_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_remote_create, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring),
                rmt_ptr_ptr, repo.ptr, rmt_name, rmt_url)
    return GitRemote(repo, rmt_ptr_ptr[])
end

function GitRemote(repo::GitRepo, rmt_name::AbstractString, rmt_url::AbstractString, fetch_spec::AbstractString)
    rmt_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_remote_create_with_fetchspec, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring, Cstring),
                rmt_ptr_ptr, repo.ptr, rmt_name, rmt_url, fetch_spec)
    return GitRemote(repo, rmt_ptr_ptr[])
end

function GitRemoteAnon(repo::GitRepo, url::AbstractString)
    rmt_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_remote_create_anonymous, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cstring),
                rmt_ptr_ptr, repo.ptr, url)
    return GitRemote(repo, rmt_ptr_ptr[])
end

function get(::Type{GitRemote}, repo::GitRepo, rmt_name::AbstractString)
    rmt_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_remote_lookup, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cstring),
                rmt_ptr_ptr, repo.ptr, rmt_name)
    return GitRemote(repo, rmt_ptr_ptr[])
end

function url(rmt::GitRemote)
    url_ptr = ccall((:git_remote_url, :libgit2), Cstring, (Ptr{Void}, ), rmt.ptr)
    url_ptr == C_NULL && return ""
    return  unsafe_string(url_ptr)
end

function fetch_refspecs(rmt::GitRemote)
    sa_ref = Ref{StrArrayStruct}()
    try
        @check ccall((:git_remote_get_fetch_refspecs, :libgit2), Cint,
                      (Ptr{LibGit2.StrArrayStruct}, Ptr{Void}), sa_ref, rmt.ptr)
        convert(Vector{AbstractString}, sa_ref[])
    finally
        close(sa_ref[])
    end
end

function push_refspecs(rmt::GitRemote)
    sa_ref = Ref{StrArrayStruct}()
    try
        @check ccall((:git_remote_get_push_refspecs, :libgit2), Cint,
                      (Ptr{LibGit2.StrArrayStruct}, Ptr{Void}), sa_ref, rmt.ptr)
        convert(Vector{AbstractString}, sa_ref[])
    finally
        close(sa_ref[])
    end
end

function fetch{T<:AbstractString}(rmt::GitRemote, refspecs::Vector{T};
               options::FetchOptions = FetchOptions(),
               msg::AbstractString="")
    msg = "libgit2.fetch: $msg"
    no_refs = isempty(refspecs)
    !no_refs && (sa = StrArrayStruct(refspecs...))
    try
        @check ccall((:git_remote_fetch, :libgit2), Cint,
            (Ptr{Void}, Ptr{StrArrayStruct}, Ptr{FetchOptions}, Cstring),
            rmt.ptr, no_refs ? C_NULL : Ref(sa), Ref(options), msg)
    finally
        !no_refs && close(sa)
    end
end

function push{T<:AbstractString}(rmt::GitRemote, refspecs::Vector{T};
                                 force::Bool = false,
                                 options::PushOptions = PushOptions())
    no_refs = isempty(refspecs)
    !no_refs && (sa = StrArrayStruct(refspecs...))
    try
        @check ccall((:git_remote_push, :libgit2), Cint,
                      (Ptr{Void}, Ptr{StrArrayStruct}, Ptr{PushOptions}),
                       rmt.ptr, no_refs ? C_NULL : Ref(sa), Ref(options))
    finally
        !no_refs && close(sa)
    end
end
