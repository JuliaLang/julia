# This file is a part of Julia. License is MIT: http://julialang.org/license

function GitRemote(repo::GitRepo, rmt_name::AbstractString, rmt_url::AbstractString)
    rmt_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_remote_create, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring),
                rmt_ptr_ptr, repo.ptr, rmt_name, rmt_url)
    return GitRemote(rmt_ptr_ptr[])
end

function GitRemoteAnon(repo::GitRepo, url::AbstractString)
    rmt_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_remote_create_anonymous, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cstring),
                rmt_ptr_ptr, repo.ptr, url)
    return GitRemote(rmt_ptr_ptr[])
end

function get(::Type{GitRemote}, repo::GitRepo, rmt_name::AbstractString)
    rmt_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_remote_lookup, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cstring),
                rmt_ptr_ptr, repo.ptr, rmt_name)
    return GitRemote(rmt_ptr_ptr[])
end

function url(rmt::GitRemote)
    url_ptr = ccall((:git_remote_url, :libgit2), Cstring, (Ptr{Void}, ), rmt.ptr)
    url_ptr == Cstring_NULL && return ""
    return  bytestring(url_ptr)
end

function fetch{T<:AbstractString}(rmt::GitRemote, refspecs::Vector{T};
               fetch_opts::FetchOptions = FetchOptions(),
               msg::AbstractString="")
    msg = "libgit2.fetch: $msg"
    no_refs = (length(refspecs) == 0)
    !no_refs && (sa = StrArrayStruct(refspecs...))
    try
        @check ccall((:git_remote_fetch, :libgit2), Cint,
            (Ptr{Void}, Ptr{StrArrayStruct}, Ptr{FetchOptions}, Cstring),
            rmt.ptr, no_refs ? C_NULL : Ref(sa), Ref(fetch_opts), msg)
    finally
        !no_refs && finalize(sa)
    end
end

function push{T<:AbstractString}(rmt::GitRemote, refspecs::Vector{T};
              force::Bool=false)
    msg = "libgit2.push: $msg"
    push_opts = PushOptionsStruct()
    no_refs = (length(refspecs) == 0)
    !no_refs && (sa = StrArrayStruct(refspecs...))
    try
        @check ccall((:git_remote_push, :libgit2), Cint,
            (Ptr{Void}, Ptr{StrArrayStruct}, Ptr{PushOptionsStruct}),
             rmt.ptr, no_refs ? C_NULL : Ref(sa), Ref(push_opts))
    finally
        !no_refs && finalize(sa)
    end
end
