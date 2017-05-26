# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    GitRemote(repo::GitRepo, rmt_name::AbstractString, rmt_url::AbstractString) -> GitRemote

Look up a remote git repository using its name and URL. Uses the default fetch refspec.

# Example

```julia
repo = LibGit2.init(repo_path)
remote = LibGit2.GitRemote(repo, "upstream", repo_url)
```
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

# Example

```julia
repo = LibGit2.init(repo_path)
refspec = "+refs/heads/mybranch:refs/remotes/origin/mybranch"
remote = LibGit2.GitRemote(repo, "upstream", repo_url, refspec)
```
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

# Example

```julia
repo = LibGit2.init(repo_path)
remote = LibGit2.GitRemoteAnon(repo, repo_url)
```
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

# Example

```julia-repl
julia> repo_url = "https://github.com/JuliaLang/Example.jl";

julia> repo = LibGit2.clone(cache_repo, "test_directory");

julia> remote = LibGit2.GitRemote(repo, "origin", repo_url);

julia> url(remote)
"https://github.com/JuliaLang/Example.jl"
```
"""
function url(rmt::GitRemote)
    url_ptr = ccall((:git_remote_url, :libgit2), Cstring, (Ptr{Void},), rmt.ptr)
    url_ptr == C_NULL && return ""
    return unsafe_string(url_ptr)
end

"""
    name(rmt::GitRemote)

Get the name of a remote repository, for instance `"origin"`.
If the remote is anonymous (see [`GitRemoteAnon`](@ref))
the name will be an empty string `""`.

# Example

```julia-repl
julia> repo_url = "https://github.com/JuliaLang/Example.jl";

julia> repo = LibGit2.clone(cache_repo, "test_directory");

julia> remote = LibGit2.GitRemote(repo, "origin", repo_url);

julia> name(remote)
"origin"
```
"""
function name(rmt::GitRemote)
    name_ptr = ccall((:git_remote_name, :libgit2), Cstring, (Ptr{Void},), rmt.ptr)
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
    add_fetch!(repo::GitRepo, rmt::GitRemote, fetch_spec::String)

Add a *fetch* refspec for the specified `rmt`. This refspec will contain
information about which branch(es) to fetch from.

# Example
```julia-repl
julia> LibGit2.add_fetch!(repo, remote, "upstream");

julia> LibGit2.fetch_refspecs(remote)
String["+refs/heads/*:refs/remotes/upstream/*"]
```
"""
function add_fetch!(repo::GitRepo, rmt::GitRemote, fetch_spec::String)
    @check ccall((:git_remote_add_fetch, :libgit2), Cint,
                 (Ptr{Void}, Cstring, Cstring), repo.ptr,
                 name(rmt), fetch_spec)
end

"""
    add_push!(repo::GitRepo, rmt::GitRemote, push_spec::String)

Add a *push* refspec for the specified `rmt`. This refspec will contain
information about which branch(es) to push to.

# Example
```julia-repl
julia> LibGit2.add_push!(repo, remote, "refs/heads/master");

julia> remote = LibGit2.get(LibGit2.GitRemote, repo, branch);

julia> LibGit2.push_refspecs(remote)
String["refs/heads/master"]
```

!!! note
    You may need to [`close`](@ref) and reopen the `GitRemote`
    in question after updating its push refspecs in order for
    the change to take effect and for calls to [`push`](@ref)
    to work.
"""
function add_push!(repo::GitRepo, rmt::GitRemote, push_spec::String)
    @check ccall((:git_remote_add_push, :libgit2), Cint,
                 (Ptr{Void}, Cstring, Cstring), repo.ptr,
                 name(rmt), push_spec)
end

"""
    fetch(rmt::GitRemote, refspecs; options::FetchOptions=FetchOptions(), msg="")

Fetch from the specified `rmt` remote git repository, using `refspecs` to
determine which remote branch(es) to fetch.
The keyword arguments are:
  * `options`: determines the options for the fetch, e.g. whether to prune afterwards.
  * `msg`: a message to insert into the reflogs.
"""
function fetch(rmt::GitRemote, refspecs::Vector{<:AbstractString};
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

!!! note
    You can add information about the push refspecs in two other ways: by setting
    an option in the repository's `GitConfig` (with `push.default` as the key) or
    by calling [`add_push!`](@ref). Otherwise you will need to explicitly specify
    a push refspec in the call to `push` for it to have any effect, like so:
    `LibGit2.push(repo, refspecs=["refs/heads/master"])`.
"""
function push(rmt::GitRemote, refspecs::Vector{<:AbstractString};
              force::Bool = false, options::PushOptions = PushOptions())
    @check ccall((:git_remote_push, :libgit2), Cint,
                 (Ptr{Void}, Ptr{StrArrayStruct}, Ptr{PushOptions}),
                 rmt.ptr, isempty(refspecs) ? C_NULL : refspecs, Ref(options))
end

Base.show(io::IO, rmt::GitRemote) = print(io, "GitRemote:\nRemote name: ", name(rmt), " url: ", url(rmt))
