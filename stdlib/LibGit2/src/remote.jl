# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    GitRemote(repo::GitRepo, rmt_name::AbstractString, rmt_url::AbstractString) -> GitRemote

Look up a remote git repository using its name and URL. Uses the default fetch refspec.

# Examples
```julia
repo = LibGit2.init(repo_path)
remote = LibGit2.GitRemote(repo, "upstream", repo_url)
```
"""
function GitRemote(repo::GitRepo, rmt_name::AbstractString, rmt_url::AbstractString)
    ensure_initialized()
    rmt_ptr_ptr = Ref{Ptr{Cvoid}}(C_NULL)
    @check ccall((:git_remote_create, :libgit2), Cint,
                (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}, Cstring, Cstring),
                rmt_ptr_ptr, repo.ptr, rmt_name, rmt_url)
    return GitRemote(repo, rmt_ptr_ptr[])
end

"""
    GitRemote(repo::GitRepo, rmt_name::AbstractString, rmt_url::AbstractString, fetch_spec::AbstractString) -> GitRemote

Look up a remote git repository using the repository's name and URL,
as well as specifications for how to fetch from the remote
(e.g. which remote branch to fetch from).

# Examples
```julia
repo = LibGit2.init(repo_path)
refspec = "+refs/heads/mybranch:refs/remotes/origin/mybranch"
remote = LibGit2.GitRemote(repo, "upstream", repo_url, refspec)
```
"""
function GitRemote(repo::GitRepo, rmt_name::AbstractString, rmt_url::AbstractString, fetch_spec::AbstractString)
    ensure_initialized()
    rmt_ptr_ptr = Ref{Ptr{Cvoid}}(C_NULL)
    @check ccall((:git_remote_create_with_fetchspec, :libgit2), Cint,
                (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}, Cstring, Cstring, Cstring),
                rmt_ptr_ptr, repo.ptr, rmt_name, rmt_url, fetch_spec)
    return GitRemote(repo, rmt_ptr_ptr[])
end

"""
    GitRemoteAnon(repo::GitRepo, url::AbstractString) -> GitRemote

Look up a remote git repository using only its URL, not its name.

# Examples
```julia
repo = LibGit2.init(repo_path)
remote = LibGit2.GitRemoteAnon(repo, repo_url)
```
"""
function GitRemoteAnon(repo::GitRepo, url::AbstractString)
    ensure_initialized()
    rmt_ptr_ptr = Ref{Ptr{Cvoid}}(C_NULL)
    @check ccall((:git_remote_create_anonymous, :libgit2), Cint,
                (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}, Cstring),
                rmt_ptr_ptr, repo.ptr, url)
    return GitRemote(repo, rmt_ptr_ptr[])
end

"""
    lookup_remote(repo::GitRepo, remote_name::AbstractString) -> Union{GitRemote, Nothing}

Determine if the `remote_name` specified exists within the `repo`. Return
either a [`GitRemote`](@ref) to the remote name if it exists, or [`nothing`](@ref)
if not.

# Examples
```julia
repo = LibGit2.GitRepo(path)
remote_name = "test"
LibGit2.lookup_remote(repo, remote_name) # will return nothing
```
"""
function lookup_remote(repo::GitRepo, remote_name::AbstractString)
    ensure_initialized()
    rmt_ptr_ptr = Ref{Ptr{Cvoid}}(C_NULL)
    err = ccall((:git_remote_lookup, :libgit2), Cint,
                (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}, Cstring),
                rmt_ptr_ptr, repo.ptr, remote_name)
    if err == Int(Error.GIT_OK)
        return GitRemote(repo, rmt_ptr_ptr[])
    elseif err == Int(Error.ENOTFOUND)
        return nothing
    else
        throw(Error.GitError(err))
    end
end

function get(::Type{GitRemote}, repo::GitRepo, rmt_name::AbstractString)
    ensure_initialized()
    rmt_ptr_ptr = Ref{Ptr{Cvoid}}(C_NULL)
    @check ccall((:git_remote_lookup, :libgit2), Cint,
                (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}, Cstring),
                rmt_ptr_ptr, repo.ptr, rmt_name)
    return GitRemote(repo, rmt_ptr_ptr[])
end

"""
    url(rmt::GitRemote)

Get the fetch URL of a remote git repository.

# Examples
```julia-repl
julia> repo_url = "https://github.com/JuliaLang/Example.jl";

julia> repo = LibGit2.init(mktempdir());

julia> remote = LibGit2.GitRemote(repo, "origin", repo_url);

julia> LibGit2.url(remote)
"https://github.com/JuliaLang/Example.jl"
```
"""
function url(rmt::GitRemote)
    ensure_initialized()
    url_ptr = ccall((:git_remote_url, :libgit2), Cstring, (Ptr{Cvoid},), rmt.ptr)
    url_ptr == C_NULL && return ""
    return unsafe_string(url_ptr)
end

"""
    push_url(rmt::GitRemote)

Get the push URL of a remote git repository.

# Examples
```julia-repl
julia> repo_url = "https://github.com/JuliaLang/Example.jl";

julia> repo = LibGit2.init(mktempdir());

julia> LibGit2.set_remote_push_url(repo, "origin", repo_url);

julia> LibGit2.push_url(LibGit2.get(LibGit2.GitRemote, repo, "origin"))
"https://github.com/JuliaLang/Example.jl"
```
"""
function push_url(rmt::GitRemote)
    ensure_initialized()
    url_ptr = ccall((:git_remote_pushurl, :libgit2), Cstring, (Ptr{Cvoid},), rmt.ptr)
    url_ptr == C_NULL && return ""
    return unsafe_string(url_ptr)
end

"""
    name(rmt::GitRemote)

Get the name of a remote repository, for instance `"origin"`.
If the remote is anonymous (see [`GitRemoteAnon`](@ref))
the name will be an empty string `""`.

# Examples
```julia-repl
julia> repo_url = "https://github.com/JuliaLang/Example.jl";

julia> repo = LibGit2.clone(cache_repo, "test_directory");

julia> remote = LibGit2.GitRemote(repo, "origin", repo_url);

julia> name(remote)
"origin"
```
"""
function name(rmt::GitRemote)
    ensure_initialized()
    name_ptr = ccall((:git_remote_name, :libgit2), Cstring, (Ptr{Cvoid},), rmt.ptr)
    name_ptr == C_NULL && return ""
    return unsafe_string(name_ptr)
end

"""
    fetch_refspecs(rmt::GitRemote) -> Vector{String}

Get the *fetch* refspecs for the specified `rmt`. These refspecs contain
information about which branch(es) to fetch from.

# Examples
```julia-repl
julia> remote = LibGit2.get(LibGit2.GitRemote, repo, "upstream");

julia> LibGit2.add_fetch!(repo, remote, "upstream");

julia> LibGit2.fetch_refspecs(remote)
String["+refs/heads/*:refs/remotes/upstream/*"]
```
"""
function fetch_refspecs(rmt::GitRemote)
    ensure_initialized()
    sa_ref = Ref(StrArrayStruct())
    @check ccall((:git_remote_get_fetch_refspecs, :libgit2), Cint,
                 (Ptr{StrArrayStruct}, Ptr{Cvoid}), sa_ref, rmt.ptr)
    res = convert(Vector{String}, sa_ref[])
    free(sa_ref)
    res
end

"""
    push_refspecs(rmt::GitRemote) -> Vector{String}

Get the *push* refspecs for the specified `rmt`. These refspecs contain
information about which branch(es) to push to.

# Examples
```julia-repl
julia> remote = LibGit2.get(LibGit2.GitRemote, repo, "upstream");

julia> LibGit2.add_push!(repo, remote, "refs/heads/master");

julia> close(remote);

julia> remote = LibGit2.get(LibGit2.GitRemote, repo, "upstream");

julia> LibGit2.push_refspecs(remote)
String["refs/heads/master"]
```
"""
function push_refspecs(rmt::GitRemote)
    ensure_initialized()
    sa_ref = Ref(StrArrayStruct())
    @check ccall((:git_remote_get_push_refspecs, :libgit2), Cint,
                 (Ptr{StrArrayStruct}, Ptr{Cvoid}), sa_ref, rmt.ptr)
    res = convert(Vector{String}, sa_ref[])
    free(sa_ref)
    res
end

"""
    add_fetch!(repo::GitRepo, rmt::GitRemote, fetch_spec::String)

Add a *fetch* refspec for the specified `rmt`. This refspec will contain
information about which branch(es) to fetch from.

# Examples
```julia-repl
julia> LibGit2.add_fetch!(repo, remote, "upstream");

julia> LibGit2.fetch_refspecs(remote)
String["+refs/heads/*:refs/remotes/upstream/*"]
```
"""
function add_fetch!(repo::GitRepo, rmt::GitRemote, fetch_spec::String)
    ensure_initialized()
    @check ccall((:git_remote_add_fetch, :libgit2), Cint,
                 (Ptr{Cvoid}, Cstring, Cstring), repo.ptr,
                 name(rmt), fetch_spec)
end

"""
    add_push!(repo::GitRepo, rmt::GitRemote, push_spec::String)

Add a *push* refspec for the specified `rmt`. This refspec will contain
information about which branch(es) to push to.

# Examples
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
    ensure_initialized()
    @check ccall((:git_remote_add_push, :libgit2), Cint,
                 (Ptr{Cvoid}, Cstring, Cstring), repo.ptr,
                 name(rmt), push_spec)
end

"""
    fetch(rmt::GitRemote, refspecs; options::FetchOptions=FetchOptions(), msg="")

Fetch from the specified `rmt` remote git repository, using `refspecs` to
determine which remote branch(es) to fetch.
The keyword arguments are:
  * `options`: determines the options for the fetch, e.g. whether to prune afterwards.
    See [`FetchOptions`](@ref) for more information.
  * `msg`: a message to insert into the reflogs.
"""
function fetch(rmt::GitRemote, refspecs::Vector{<:AbstractString};
               options::FetchOptions = FetchOptions(),
               msg::AbstractString="")
    ensure_initialized()
    msg = "libgit2.fetch: $msg"
    @check ccall((:git_remote_fetch, :libgit2), Cint,
                 (Ptr{Cvoid}, Ptr{StrArrayStruct}, Ptr{FetchOptions}, Cstring),
                 rmt.ptr, isempty(refspecs) ? C_NULL : refspecs, Ref(options), msg)
end

"""
    push(rmt::GitRemote, refspecs; force::Bool=false, options::PushOptions=PushOptions())

Push to the specified `rmt` remote git repository, using `refspecs` to
determine which remote branch(es) to push to.
The keyword arguments are:
  * `force`: if `true`, a force-push will occur, disregarding conflicts.
  * `options`: determines the options for the push, e.g. which proxy headers to use.
    See [`PushOptions`](@ref) for more information.

!!! note
    You can add information about the push refspecs in two other ways: by setting
    an option in the repository's `GitConfig` (with `push.default` as the key) or
    by calling [`add_push!`](@ref). Otherwise you will need to explicitly specify
    a push refspec in the call to `push` for it to have any effect, like so:
    `LibGit2.push(repo, refspecs=["refs/heads/master"])`.
"""
function push(rmt::GitRemote, refspecs::Vector{<:AbstractString};
              force::Bool = false, options::PushOptions = PushOptions())
    ensure_initialized()
    @check ccall((:git_remote_push, :libgit2), Cint,
                 (Ptr{Cvoid}, Ptr{StrArrayStruct}, Ptr{PushOptions}),
                 rmt.ptr, isempty(refspecs) ? C_NULL : refspecs, Ref(options))
end

"""
    remote_delete(repo::GitRepo, remote_name::AbstractString) -> Nothing

Delete the `remote_name` from the git `repo`.
"""
function remote_delete(repo::GitRepo, remote_name::AbstractString)
    ensure_initialized()
    @check ccall((:git_remote_delete, :libgit2), Cint,
                 (Ptr{Cvoid}, Cstring),
                 repo.ptr, remote_name)
end

Base.show(io::IO, rmt::GitRemote) = print(io, "GitRemote:\nRemote name: ", name(rmt), " url: ", url(rmt))


"""
    set_remote_fetch_url(repo::GitRepo, remote_name, url)
    set_remote_fetch_url(path::String, remote_name, url)

Set the fetch `url` for the specified `remote_name` for the [`GitRepo`](@ref) or the git repository
located at `path`. Typically git repos use `"origin"` as the remote name.
"""
function set_remote_fetch_url end

function set_remote_fetch_url(repo::GitRepo, remote_name::AbstractString, url::AbstractString)
    ensure_initialized()
    @check ccall((:git_remote_set_url, :libgit2), Cint,
                 (Ptr{Cvoid}, Cstring, Cstring),
                 repo.ptr, remote_name, url)
end

function set_remote_fetch_url(path::AbstractString, remote_name::AbstractString, url::AbstractString)
    with(GitRepo, path) do repo
        set_remote_fetch_url(repo, remote_name, url)
    end
end


"""
    set_remote_push_url(repo::GitRepo, remote_name, url)
    set_remote_push_url(path::String, remote_name, url)

Set the push `url` for the specified `remote_name` for the [`GitRepo`](@ref) or the git repository
located at `path`. Typically git repos use `"origin"` as the remote name.
"""
function set_remote_push_url end

function set_remote_push_url(repo::GitRepo, remote_name::AbstractString, url::AbstractString)
    ensure_initialized()
    @check ccall((:git_remote_set_pushurl, :libgit2), Cint,
                 (Ptr{Cvoid}, Cstring, Cstring),
                 repo.ptr, remote_name, url)
end

function set_remote_push_url(path::AbstractString, remote_name::AbstractString, url::AbstractString)
    with(GitRepo, path) do repo
        set_remote_push_url(repo, remote_name, url)
    end
end


"""
    set_remote_url(repo::GitRepo, remote_name, url)
    set_remote_url(repo::String, remote_name, url)

Set both the fetch and push `url` for `remote_name` for the [`GitRepo`](@ref) or the git repository
located at `path`. Typically git repos use `"origin"` as the remote name.

# Examples
```julia
repo_path = joinpath(tempdir(), "Example")
repo = LibGit2.init(repo_path)
LibGit2.set_remote_url(repo, "upstream", "https://github.com/JuliaLang/Example.jl")
LibGit2.set_remote_url(repo_path, "upstream2", "https://github.com/JuliaLang/Example2.jl")
```
"""
function set_remote_url end

function set_remote_url(repo::GitRepo, remote_name::AbstractString, url::AbstractString)
    set_remote_fetch_url(repo, remote_name, url)
    set_remote_push_url(repo, remote_name, url)
end

function set_remote_url(path::AbstractString, remote_name::AbstractString, url::AbstractString)
    with(GitRepo, path) do repo
        set_remote_url(repo, remote_name, url)
    end
end
