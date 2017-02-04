# This file is a part of Julia. License is MIT: http://julialang.org/license

"""
    GitRemote(repo::GitRepo, rmt_name::AbstractString, rmt_url::AbstractString) -> GitRemote

Look up a remote git repository using its name and URL. Uses the default fetch refspec.
"""
function GitRemote(repo::GitRepo, rmt_name::AbstractString, rmt_url::AbstractString)
    rmt_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_remote_create, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring),
                rmt_ptr_ptr, repo.ptr, rmt_name, rmt_url)
    return GitRemote(repo, rmt_ptr_ptr[])
end

"""
    GitRemote(repo::GitRepo, rmt_name::AbstractString, rmt_url::AbstractString, fetch_spec::AbstractString) -> GitRemote

Look up a remote git repository using the repository's name and URL,
as well as specifications for how to fetch from the remote
(e.g. which remote branch to fetch from).
"""
function GitRemote(repo::GitRepo, rmt_name::AbstractString, rmt_url::AbstractString, fetch_spec::AbstractString)
    rmt_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_remote_create_with_fetchspec, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring, Cstring),
                rmt_ptr_ptr, repo.ptr, rmt_name, rmt_url, fetch_spec)
    return GitRemote(repo, rmt_ptr_ptr[])
end

"""
    GitRemoteAnon(repo::GitRepo, url::AbstractString) -> GitRemote

Look up a remote git repository using only its URL, not its name.
"""
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

"""
    url(rmt::GitRemote)

Get the URL of a remote git repository.
"""
function url(rmt::GitRemote)
    url_ptr = ccall((:git_remote_url, :libgit2), Cstring, (Ptr{Void}, ), rmt.ptr)
    url_ptr == C_NULL && return ""
    return unsafe_string(url_ptr)
end

"""
    name(rmt::GitRemote)

Get the name of a remote repository, for instance `"origin"`.
If the remote is anonymous (see [`GitRemoteAnon`](@ref))
the name will be an empty string `""`.
"""
function name(rmt::GitRemote)
    name_ptr = ccall((:git_remote_name, :libgit2), Cstring, (Ptr{Void}, ), rmt.ptr)
    name_ptr == C_NULL && return ""
    return unsafe_string(name_ptr)
end

"""
    fetch_refspecs(rmt::GitRemote) -> Vector{String}

Get the *fetch* refspecs for the specified `rmt`. These refspecs contain
information about which branch(es) to fetch from.
"""
function fetch_refspecs(rmt::GitRemote)
    sa_ref = Ref(StrArrayStruct())
    @check ccall((:git_remote_get_fetch_refspecs, :libgit2), Cint,
                 (Ptr{StrArrayStruct}, Ptr{Void}), sa_ref, rmt.ptr)
    res = convert(Vector{String}, sa_ref[])
    free(sa_ref)
    res
end

"""
    push_refspecs(rmt::GitRemote) -> Vector{String}

Get the *push* refspecs for the specified `rmt`. These refspecs contain
information about which branch(es) to push to.
"""
function push_refspecs(rmt::GitRemote)
    sa_ref = Ref(StrArrayStruct())
    @check ccall((:git_remote_get_push_refspecs, :libgit2), Cint,
                 (Ptr{StrArrayStruct}, Ptr{Void}), sa_ref, rmt.ptr)
    res = convert(Vector{String}, sa_ref[])
    free(sa_ref)
    res
end

"""
    fetch(rmt::GitRemote, refspecs; options::FetchOptions=FetchOptions(), msg="")

Fetch from the specified `rmt` remote git repository, using `refspecs` to
determine which remote branch(es) to fetch.
The keyword arguments are:
  * `options`: determines the options for the fetch, e.g. whether to prune afterwards.
  * `msg`: a message to insert into the reflogs.
"""
function fetch{T<:AbstractString}(rmt::GitRemote, refspecs::Vector{T};
               options::FetchOptions = FetchOptions(),
               msg::AbstractString="")
    msg = "libgit2.fetch: $msg"
    @check ccall((:git_remote_fetch, :libgit2), Cint,
                 (Ptr{Void}, Ptr{StrArrayStruct}, Ptr{FetchOptions}, Cstring),
                 rmt.ptr, isempty(refspecs) ? C_NULL : refspecs, Ref(options), msg)
end

"""
    push(rmt::GitRemote, refspecs; force::Bool=false, options::PushOptions=PushOptions())

Push to the specified `rmt` remote git repository, using `refspecs` to
determine which remote branch(es) to push to.
The keyword arguments are:
  * `force`: if `true`, a force-push will occur, disregarding conflicts.
  * `options`: determines the options for the push, e.g. which proxy headers to use.
"""
function push{T<:AbstractString}(rmt::GitRemote, refspecs::Vector{T};
                                 force::Bool = false,
                                 options::PushOptions = PushOptions())
    @check ccall((:git_remote_push, :libgit2), Cint,
                 (Ptr{Void}, Ptr{StrArrayStruct}, Ptr{PushOptions}),
                 rmt.ptr, isempty(refspecs) ? C_NULL : refspecs, Ref(options))
end

Base.show(io::IO, rmt::GitRemote) = print(io, "GitRemote:\nRemote name: ", name(rmt), " url: ", url(rmt))
