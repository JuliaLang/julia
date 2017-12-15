# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    LibGit2.GitRepo(path::AbstractString)

Open a git repository at `path`.
"""
function GitRepo(path::AbstractString)
    repo_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_repository_open, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Cstring), repo_ptr_ptr, path)
    return GitRepo(repo_ptr_ptr[])
end

"""
    LibGit2.GitRepoExt(path::AbstractString, flags::Cuint = Cuint(Consts.REPOSITORY_OPEN_DEFAULT))

Open a git repository at `path` with extended controls (for instance, if the current
user must be a member of a special access group to read `path`).
"""
function GitRepoExt(path::AbstractString, flags::Cuint = Cuint(Consts.REPOSITORY_OPEN_DEFAULT))
    separator = @static Sys.iswindows() ? ";" : ":"
    repo_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_repository_open_ext, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Cstring, Cuint, Cstring),
                 repo_ptr_ptr, path, flags, separator)
    return GitRepo(repo_ptr_ptr[])
end

function cleanup(r::GitRepo)
    if r.ptr != C_NULL
        ccall((:git_repository__cleanup, :libgit2), Void, (Ptr{Void},), r.ptr)
    end
end

"""
    LibGit2.init(path::AbstractString, bare::Bool=false) -> GitRepo

Open a new git repository at `path`. If `bare` is `false`,
the working tree will be created in `path/.git`. If `bare`
is `true`, no working directory will be created.
"""
function init(path::AbstractString, bare::Bool=false)
    repo_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_repository_init, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Cstring, Cuint), repo_ptr_ptr, path, bare)
    return GitRepo(repo_ptr_ptr[])
end

"""
    LibGit2.head_oid(repo::GitRepo) -> GitHash

Lookup the object id of the current HEAD of git
repository `repo`.
"""
function head_oid(repo::GitRepo)
    head_ref = head(repo)
    try
        return GitHash(head_ref)
    finally
        close(head_ref)
    end
end

"""
    LibGit2.headname(repo::GitRepo)

Lookup the name of the current HEAD of git
repository `repo`. If `repo` is currently
detached, return the name of the HEAD it's
detached from.
"""
function headname(repo::GitRepo)
    with(head(repo)) do href
        if isattached(repo)
            shortname(href)
        else
            "(detached from $(string(GitHash(href))[1:7]))"
        end
    end
end

"""
    isbare(repo::GitRepo) -> Bool

Determine if `repo` is bare. Suppose the top level directory of `repo` is `DIR`.
A non-bare repository is one in which the git directory (see [`gitdir`](@ref)) is
`DIR/.git`, and the working tree can be checked out. A bare repository is one in
which all of git's administrative files are simply in `DIR`, rather than "hidden"
in the `.git` subdirectory. This means that there is nowhere to check out the working
tree, and no tracking information for remote branches or configurations is present.
"""
function isbare(repo::GitRepo)
    return ccall((:git_repository_is_bare, :libgit2), Cint, (Ptr{Void},), repo.ptr) == 1
end

"""
    isattached(repo::GitRepo) -> Bool

Determine if `repo` is detached - that is, whether its HEAD points to a commit
(detached) or whether HEAD points to a branch tip (attached).
"""
function isattached(repo::GitRepo)
    ccall((:git_repository_head_detached, :libgit2), Cint, (Ptr{Void},), repo.ptr) != 1
end

@doc """
    GitObject(repo::GitRepo, hash::AbstractGitHash)
    GitObject(repo::GitRepo, spec::AbstractString)

Return the specified object ([`GitCommit`](@ref), [`GitBlob`](@ref), [`GitTree`](@ref) or [`GitTag`](@ref)) from `repo`
specified by `hash`/`spec`.

- `hash` is a full (`GitHash`) or partial (`GitShortHash`) hash.
- `spec` is a textual specification: see [the git docs](https://git-scm.com/docs/git-rev-parse.html#_specifying_revisions) for a full list.
""" GitObject

for T in (:GitCommit, :GitBlob, :GitTree, :GitTag)
    @eval @doc $"""
    $T(repo::GitRepo, hash::AbstractGitHash)
    $T(repo::GitRepo, spec::AbstractString)

Return a `$T` object from `repo` specified by `hash`/`spec`.

- `hash` is a full (`GitHash`) or partial (`GitShortHash`) hash.
- `spec` is a textual specification: see [the git docs](https://git-scm.com/docs/git-rev-parse.html#_specifying_revisions) for a full list.
""" $T
end

function (::Type{T})(repo::GitRepo, spec::AbstractString) where T<:GitObject
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_revparse_single, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Cstring), obj_ptr_ptr, repo.ptr, spec)
    # check object is of correct type
    if T != GitObject && T != GitUnknownObject
        t = Consts.OBJECT(obj_ptr_ptr[])
        t == Consts.OBJECT(T) || throw(GitError(Error.Object, Error.ERROR, "Expected object of type $T, received object of type $(objtype(t))"))
    end
    return T(repo, obj_ptr_ptr[])
end

function (::Type{T})(repo::GitRepo, oid::GitHash) where T<:GitObject
    oid_ptr  = Ref(oid)
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)

    @check ccall((:git_object_lookup, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{GitHash}, Consts.OBJECT),
                 obj_ptr_ptr, repo.ptr, oid_ptr, Consts.OBJECT(T))

    return T(repo, obj_ptr_ptr[])
end
function (::Type{T})(repo::GitRepo, oid::GitShortHash) where T<:GitObject
    oid_ptr  = Ref(oid.hash)
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)

    @check ccall((:git_object_lookup_prefix, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{GitHash}, Csize_t, Consts.OBJECT),
                 obj_ptr_ptr, repo.ptr, oid_ptr, oid.len, Consts.OBJECT(T))

    return T(repo, obj_ptr_ptr[])
end

# TODO: deprecate this function
revparseid(repo::GitRepo, spec) = GitHash(GitUnknownObject(repo, spec))

"""
    LibGit2.gitdir(repo::GitRepo)

Return the location of the "git" files of `repo`:

 - for normal repositories, this is the location of the `.git` folder.
 - for bare repositories, this is the location of the repository itself.

See also [`workdir`](@ref), [`path`](@ref).
"""
function gitdir(repo::GitRepo)
    return unsafe_string(ccall((:git_repository_path, :libgit2), Cstring,
                        (Ptr{Void},), repo.ptr))
end

"""
    LibGit2.workdir(repo::GitRepo)

Return the location of the working directory of `repo`.
This will throw an error for bare repositories.

!!! note

    This will typically be the parent directory of `gitdir(repo)`, but can be different in
    some cases: e.g. if either the `core.worktree` configuration variable or the
    `GIT_WORK_TREE` environment variable is set.

See also [`gitdir`](@ref), [`path`](@ref).
"""
function workdir(repo::GitRepo)
    sptr = ccall((:git_repository_workdir, :libgit2), Cstring,
                (Ptr{Void},), repo.ptr)
    sptr == C_NULL && throw(GitError(Error.Object, Error.ERROR, "No working directory found."))
    return unsafe_string(sptr)
end

"""
    LibGit2.path(repo::GitRepo)

Return the base file path of the repository `repo`.

 - for normal repositories, this will typically be the parent directory of the ".git"
   directory (note: this may be different than the working directory, see `workdir` for
   more details).
 - for bare repositories, this is the location of the "git" files.

See also [`gitdir`](@ref), [`workdir`](@ref).
"""
function path(repo::GitRepo)
    d = gitdir(repo)
    if isdirpath(d)
        d = dirname(d) # strip trailing separator
    end
    if isbare(repo)
        return d
    else
        parent, base = splitdir(d)
        return base == ".git" ? parent : d
    end
end

"""
    peel([T,] obj::GitObject)

Recursively peel `obj` until an object of type `T` is obtained. If no `T` is provided,
then `obj` will be peeled until the type changes.

- A `GitTag` will be peeled to the object it references.
- A `GitCommit` will be peeled to a `GitTree`.
"""
function peel(::Type{T}, obj::GitObject) where T<:GitObject
    new_ptr_ptr = Ref{Ptr{Void}}(C_NULL)

    @check ccall((:git_object_peel, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cint), new_ptr_ptr, obj.ptr, Consts.OBJECT(T))

    return T(obj.owner, new_ptr_ptr[])
end
peel(obj::GitObject) = peel(GitObject, obj)

"""
    LibGit2.GitDescribeResult(commitish::GitObject; kwarg...)

Produce a `GitDescribeResult` of the `commitish` `GitObject`, which
contains detailed information about it based on the keyword argument:

  * `options::DescribeOptions=DescribeOptions()`

A git decription of a `commitish` object looks for the tag (by default, annotated,
although a search of all tags can be performed) which can be reached from `commitish`
which is most recent. If the tag is pointing to `commitish`, then only the tag is
included in the description. Otherwise, a suffix is included which contains the
number of commits between `commitish` and the most recent tag. If there is no such
tag, the default behavior is for the description to fail, although this can be
changed through `options`.

Equivalent to `git describe <commitish>`. See [`DescribeOptions`](@ref) for more
information.
"""
function GitDescribeResult(commitish::GitObject;
                           options::DescribeOptions=DescribeOptions())
    result_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_describe_commit, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{DescribeOptions}),
                 result_ptr_ptr, commitish.ptr, Ref(options))
    return GitDescribeResult(commitish.owner, result_ptr_ptr[])
end

"""
    LibGit2.GitDescribeResult(repo::GitRepo; kwarg...)

Produce a `GitDescribeResult` of the repository `repo`'s working directory.
The `GitDescribeResult` contains detailed information about the workdir based
on the keyword argument:

  * `options::DescribeOptions=DescribeOptions()`

In this case, the description is run on HEAD, producing the most recent tag
which is an ancestor of HEAD. Afterwards, a status check on
the [`workdir`](@ref) is performed and if the `workdir` is dirty
(see [`isdirty`](@ref)) the description is also considered dirty.

Equivalent to `git describe`. See [`DescribeOptions`](@ref) for more
information.
"""
function GitDescribeResult(repo::GitRepo; options::DescribeOptions=DescribeOptions())
    result_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_describe_workdir, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{DescribeOptions}),
                 result_ptr_ptr, repo.ptr, Ref(options))
    return GitDescribeResult(repo, result_ptr_ptr[])
end

"""
    LibGit2.format(result::GitDescribeResult; kwarg...) -> String

Produce a formatted string based on a `GitDescribeResult`.
Formatting options are controlled by the keyword argument:

  * `options::DescribeFormatOptions=DescribeFormatOptions()`
"""
function format(result::GitDescribeResult; options::DescribeFormatOptions=DescribeFormatOptions())
    buf_ref = Ref(Buffer())
    @check ccall((:git_describe_format, :libgit2), Cint,
                 (Ptr{Buffer}, Ptr{Void}, Ptr{DescribeFormatOptions}),
                 buf_ref, result.ptr, Ref(options))
    buf = buf_ref[]
    str = unsafe_string(buf.ptr, buf.size)
    free(buf_ref)
    return str
end

function Base.show(io::IO, result::GitDescribeResult)
    fmt_desc = format(result)
    println(io, "GitDescribeResult:")
    println(io, fmt_desc)
end

"""
    checkout_tree(repo::GitRepo, obj::GitObject; options::CheckoutOptions = CheckoutOptions())

Update the working tree and index of `repo` to match the tree pointed to by `obj`.
`obj` can be a commit, a tag, or a tree. `options` controls how the checkout will
be performed. See [`CheckoutOptions`](@ref) for more information.
"""
function checkout_tree(repo::GitRepo, obj::GitObject;
                       options::CheckoutOptions = CheckoutOptions())
    @check ccall((:git_checkout_tree, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Ptr{CheckoutOptions}),
                 repo.ptr, obj.ptr, Ref(options))
end

"""
    checkout_index(repo::GitRepo, idx::Union{GitIndex, Void} = nothing; options::CheckoutOptions = CheckoutOptions())

Update the working tree of `repo` to match the index `idx`. If `idx` is `nothing`, the
index of `repo` will be used. `options` controls how the checkout will be performed.
See [`CheckoutOptions`](@ref) for more information.
"""
function checkout_index(repo::GitRepo, idx::Union{GitIndex, Void} = nothing;
                        options::CheckoutOptions = CheckoutOptions())
    @check ccall((:git_checkout_index, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Ptr{CheckoutOptions}),
                 repo.ptr,
                 idx === nothing ? C_NULL : idx.ptr,
                 Ref(options))
end

"""
    checkout_head(repo::GitRepo; options::CheckoutOptions = CheckoutOptions())

Update the index and working tree of `repo` to match the commit pointed to by HEAD.
`options` controls how the checkout will be performed. See [`CheckoutOptions`](@ref) for more information.

!!! warning
    *Do not* use this function to switch branches! Doing so will cause checkout
    conflicts.
"""
function checkout_head(repo::GitRepo; options::CheckoutOptions = CheckoutOptions())
    @check ccall((:git_checkout_head, :libgit2), Cint,
                 (Ptr{Void}, Ptr{CheckoutOptions}),
                 repo.ptr, Ref(options))
end

"""
    LibGit2.cherrypick(repo::GitRepo, commit::GitCommit; options::CherrypickOptions = CherrypickOptions())

Cherrypick the commit `commit` and apply the changes in it to the current state of `repo`.
The keyword argument `options` sets checkout and merge options for the cherrypick.

!!! note
    `cherrypick` will *apply* the changes in `commit` but not *commit* them, so `repo` will
    be left in a dirty state. If you want to also commit the changes in `commit` you must
    call [`commit`](@ref) yourself.
"""
function cherrypick(repo::GitRepo, commit::GitCommit; options::CherrypickOptions = CherrypickOptions())
    @check ccall((:git_cherrypick, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Ptr{CherrypickOptions}),
                 repo.ptr, commit.ptr, Ref(options))
end

"""Updates some entries, determined by the `pathspecs`, in the index from the target commit tree."""
function reset!(repo::GitRepo, obj::Union{GitObject, Void}, pathspecs::AbstractString...)
    @check ccall((:git_reset_default, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Ptr{StrArrayStruct}),
                 repo.ptr,
                 obj === nothing ? C_NULL : obj.ptr,
                 collect(pathspecs))
    return head_oid(repo)
end

"""Sets the current head to the specified commit oid and optionally resets the index and working tree to match."""
function reset!(repo::GitRepo, obj::GitObject, mode::Cint;
               checkout_opts::CheckoutOptions = CheckoutOptions())
    @check ccall((:git_reset, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Cint, Ptr{CheckoutOptions}),
                  repo.ptr, obj.ptr, mode, Ref(checkout_opts))
    return head_oid(repo)
end

"""
    clone(repo_url::AbstractString, repo_path::AbstractString, clone_opts::CloneOptions)

Clone the remote repository at `repo_url` (which can be a remote URL or a path on the local
filesystem) to `repo_path` (which must be a path on the local filesystem). Options for the
clone, such as whether to perform a bare clone or not, are set by [`CloneOptions`](@ref).

# Examples
```julia
repo_url = "https://github.com/JuliaLang/Example.jl"
repo = LibGit2.clone(repo_url, "/home/me/projects/Example")
```
"""
function clone(repo_url::AbstractString, repo_path::AbstractString,
               clone_opts::CloneOptions)
    clone_opts_ref = Ref(clone_opts)
    repo_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_clone, :libgit2), Cint,
            (Ptr{Ptr{Void}}, Cstring, Cstring, Ref{CloneOptions}),
            repo_ptr_ptr, repo_url, repo_path, clone_opts_ref)
    return GitRepo(repo_ptr_ptr[])
end

"""
    fetchheads(repo::GitRepo) -> Vector{FetchHead}

Return the list of all the fetch heads for `repo`, each represented as a [`FetchHead`](@ref),
including their names, URLs, and merge statuses.

# Examples
```julia-repl
julia> fetch_heads = LibGit2.fetchheads(repo);

julia> fetch_heads[1].name
"refs/heads/master"

julia> fetch_heads[1].ismerge
true

julia> fetch_heads[2].name
"refs/heads/test_branch"

julia> fetch_heads[2].ismerge
false
```
"""
function fetchheads(repo::GitRepo)
    fh = FetchHead[]
    ffcb = fetchhead_foreach_cb()
    @check ccall((:git_repository_fetchhead_foreach, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Any),
                 repo.ptr, ffcb, fh)
    return fh
end

"""
    LibGit2.remotes(repo::GitRepo)

Return a vector of the names of the remotes of `repo`.
"""
function remotes(repo::GitRepo)
    sa_ref = Ref(StrArrayStruct())
    @check ccall((:git_remote_list, :libgit2), Cint,
                  (Ptr{StrArrayStruct}, Ptr{Void}), sa_ref, repo.ptr)
    res = convert(Vector{String}, sa_ref[])
    free(sa_ref)
    return res
end

function Base.show(io::IO, repo::GitRepo)
    print(io, "LibGit2.GitRepo(")
    show(io, path(repo))
    print(io, ")")
end
