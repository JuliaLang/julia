# This file is a part of Julia. License is MIT: http://julialang.org/license

"""
    LibGit2.GitRepo(path::AbstractString)

Opens a git repository at `path`.
"""
function GitRepo(path::AbstractString)
    repo_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    err = ccall((:git_repository_open, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Cstring), repo_ptr_ptr, path)
    if err != Int(Error.GIT_OK)
        if repo_ptr_ptr[] != C_NULL
            close(GitRepo(repo_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    return GitRepo(repo_ptr_ptr[])
end

function GitRepoExt(path::AbstractString, flags::Cuint = Cuint(Consts.REPOSITORY_OPEN_DEFAULT))
    separator = @static is_windows() ? ";" : ":"
    repo_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    err = ccall((:git_repository_open_ext, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Cstring, Cuint, Cstring),
                 repo_ptr_ptr, path, flags, separator)
    if err != Int(Error.GIT_OK)
        if repo_ptr_ptr[] != C_NULL
            close(GitRepo(repo_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    return GitRepo(repo_ptr_ptr[])
end

function cleanup(r::GitRepo)
    if r.ptr != C_NULL
        ccall((:git_repository__cleanup, :libgit2), Void, (Ptr{Void},), r.ptr)
    end
end

function init(path::AbstractString, bare::Bool=false)
    repo_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_repository_init, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Cstring, Cuint), repo_ptr_ptr, path, bare)
    return GitRepo(repo_ptr_ptr[])
end

function head_oid(repo::GitRepo)
    head_ref = head(repo)
    try
        return GitHash(head_ref)
    finally
        close(head_ref)
    end
end

function headname(repo::GitRepo)
    with(head(repo)) do href
        if isattached(repo)
            shortname(href)
        else
            "(detached from $(string(GitHash(href))[1:7]))"
        end
    end
end

function isbare(repo::GitRepo)
    return ccall((:git_repository_is_bare, :libgit2), Cint, (Ptr{Void},), repo.ptr) == 1
end

function isattached(repo::GitRepo)
    ccall((:git_repository_head_detached, :libgit2), Cint, (Ptr{Void},), repo.ptr) != 1
end

@doc """
    GitObject(repo::GitRepo, hash::AbstractGitHash)
    GitObject(repo::GitRepo, spec::AbstractString)

Return the specified object (`GitCommit`, `GitBlob`, `GitTree` or `GitTag`) from `repo`
specified by `hash`/`spec`.

- `hash` is a full (`GitHash`) or partial (`GitShortHash`) hash.
- `spec` is a textual specification: see https://git-scm.com/docs/git-rev-parse.html#_specifying_revisions for a full list.
""" GitObject

for T in (:GitCommit, :GitBlob, :GitTree, :GitTag)
    @eval @doc $"""
    $T(repo::GitRepo, hash::AbstractGitHash)
    $T(repo::GitRepo, spec::AbstractString)

Return a `$T` object from `repo` specified by `hash`/`spec`.

- `hash` is a full (`GitHash`) or partial (`GitShortHash`) hash.
- `spec` is a textual specification: see https://git-scm.com/docs/git-rev-parse.html#_specifying_revisions for a full list.
""" $T
end

function (::Type{T}){T<:GitObject}(repo::GitRepo, spec::AbstractString)
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

function (::Type{T}){T<:GitObject}(repo::GitRepo, oid::GitHash)
    oid_ptr  = Ref(oid)
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)

    @check ccall((:git_object_lookup, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{GitHash}, Consts.OBJECT),
                 obj_ptr_ptr, repo.ptr, oid_ptr, Consts.OBJECT(T))

    return T(repo, obj_ptr_ptr[])
end
function (::Type{T}){T<:GitObject}(repo::GitRepo, oid::GitShortHash)
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

Returns the location of the "git" files of `repo`:

 - for normal repositories, this is the location of the `.git` folder.
 - for bare repositories, this is the location of the repository itself.

See also `workdir`, `path`
"""
function gitdir(repo::GitRepo)
    return unsafe_string(ccall((:git_repository_path, :libgit2), Cstring,
                        (Ptr{Void},), repo.ptr))
end

"""
    LibGit2.workdir(repo::GitRepo)

The location of the working directory of `repo`. This will throw an error for bare
repositories.

!!! note

    This will typically be the parent directory of `gitdir(repo)`, but can be different in
    some cases: e.g. if either the `core.worktree` configuration variable or the
    `GIT_WORK_TREE` environment variable is set.

See also `gitdir`, `path`
"""
function workdir(repo::GitRepo)
    sptr = ccall((:git_repository_workdir, :libgit2), Cstring,
                (Ptr{Void},), repo.ptr)
    sptr == C_NULL && throw(GitError(Error.Object, Error.ERROR, "No working directory found."))
    return unsafe_string(sptr)
end

"""
    LibGit2.path(repo::GitRepo)

The base file path of the repository `repo`.

 - for normal repositories, this will typically be the parent directory of the ".git"
   directory (note: this may be different than the working directory, see `workdir` for
   more details).
 - for bare repositories, this is the location of the "git" files.

See also `gitdir`, `workdir`.
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
function peel{T<:GitObject}(::Type{T}, obj::GitObject)
    new_ptr_ptr = Ref{Ptr{Void}}(C_NULL)

    @check ccall((:git_object_peel, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cint), new_ptr_ptr, obj.ptr, Consts.OBJECT(T))

    return T(obj.repo, new_ptr_ptr[])
end
peel(obj::GitObject) = peel(GitObject, obj)


function checkout_tree(repo::GitRepo, obj::GitObject;
                       options::CheckoutOptions = CheckoutOptions())
    @check ccall((:git_checkout_tree, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Ptr{CheckoutOptions}),
                 repo.ptr, obj.ptr, Ref(options))
end

function checkout_index(repo::GitRepo, idx::Nullable{GitIndex} = Nullable{GitIndex}();
                        options::CheckoutOptions = CheckoutOptions())
    @check ccall((:git_checkout_index, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Ptr{CheckoutOptions}),
                 repo.ptr,
                 isnull(idx) ? C_NULL : Base.get(idx).ptr,
                 Ref(options))
end

function checkout_head(repo::GitRepo; options::CheckoutOptions = CheckoutOptions())
    @check ccall((:git_checkout_head, :libgit2), Cint,
                 (Ptr{Void}, Ptr{CheckoutOptions}),
                 repo.ptr, Ref(options))
end

"""Updates some entries, determined by the `pathspecs`, in the index from the target commit tree."""
function reset!{T<:AbstractString, S<:GitObject}(repo::GitRepo, obj::Nullable{S}, pathspecs::T...)
    @check ccall((:git_reset_default, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Ptr{StrArrayStruct}),
                 repo.ptr,
                 isnull(obj) ? C_NULL: Base.get(obj).ptr,
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

function clone(repo_url::AbstractString, repo_path::AbstractString,
               clone_opts::CloneOptions)
    clone_opts_ref = Ref(clone_opts)
    repo_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_clone, :libgit2), Cint,
            (Ptr{Ptr{Void}}, Cstring, Cstring, Ref{CloneOptions}),
            repo_ptr_ptr, repo_url, repo_path, clone_opts_ref)
    return GitRepo(repo_ptr_ptr[])
end

function fetchheads(repo::GitRepo)
    fhr = Ref{Vector{FetchHead}}(FetchHead[])
    ffcb = fetchhead_foreach_cb()
    @check ccall((:git_repository_fetchhead_foreach, :libgit2), Cint,
                  (Ptr{Void}, Ptr{Void}, Ptr{Void}),
                   repo.ptr, ffcb, fhr)
    return fhr[]
end

"""
    LibGit2.remotes(repo::GitRepo)

Returns a vector of the names of the remotes of `repo`.
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
