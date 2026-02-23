# This file is a part of Julia. License is MIT: https://julialang.org/license

repository(c::GitCommit) = c.owner

"""
    message(c::GitCommit, raw::Bool=false)

Return the commit message describing the changes made in commit `c`. If
`raw` is `false`, return a slightly "cleaned up" message (which has any
leading newlines removed). If `raw` is `true`, the message is not stripped
of any such newlines.
"""
function message(c::GitCommit, raw::Bool=false)
    ensure_initialized()
    GC.@preserve c begin
        local msg_ptr::Cstring
        msg_ptr = raw ? ccall((:git_commit_message_raw, libgit2), Cstring, (Ptr{Cvoid},), c.ptr) :
                        ccall((:git_commit_message, libgit2), Cstring, (Ptr{Cvoid},), c.ptr)
        if msg_ptr == C_NULL
            return nothing
        end
        msg_str = unsafe_string(msg_ptr)
    end
    return msg_str
end

"""
    author(c::GitCommit)

Return the `Signature` of the author of the commit `c`. The author is
the person who made changes to the relevant file(s). See also [`committer`](@ref).
"""
function author(c::GitCommit)
    ensure_initialized()
    GC.@preserve c begin
        ptr = ccall((:git_commit_author, libgit2), Ptr{SignatureStruct}, (Ptr{Cvoid},), c.ptr)
        @assert ptr != C_NULL
        sig = Signature(ptr)
    end
    return sig
end

"""
    committer(c::GitCommit)

Return the `Signature` of the committer of the commit `c`. The committer is
the person who committed the changes originally authored by the [`author`](@ref), but
need not be the same as the `author`, for example, if the `author` emailed a patch to
a `committer` who committed it.
"""
function committer(c::GitCommit)
    ensure_initialized()
    GC.@preserve c begin
        ptr = ccall((:git_commit_committer, libgit2), Ptr{SignatureStruct}, (Ptr{Cvoid},), c.ptr)
        sig = Signature(ptr)
    end
    return sig
end

function Base.show(io::IO, c::GitCommit)
    authstr = sprint(show, author(c))
    cmtrstr = sprint(show, committer(c))
    print(io, "Git Commit:\nCommit Author: $authstr\nCommitter: $cmtrstr\nSHA: $(GitHash(c))\nMessage:\n$(message(c))")
end

function commit(repo::GitRepo,
                refname::AbstractString,
                msg::AbstractString,
                author::GitSignature,
                committer::GitSignature,
                tree::GitTree,
                parents::GitCommit...)
    ensure_initialized()
    commit_id_ptr = Ref(GitHash())
    nparents = length(parents)
    GC.@preserve parents begin
        parentptrs = Ptr{Cvoid}[c.ptr for c in parents]
        @check ccall((:git_commit_create, libgit2), Cint,
                     (Ptr{GitHash}, Ptr{Cvoid}, Ptr{UInt8},
                      Ptr{SignatureStruct}, Ptr{SignatureStruct},
                      Ptr{UInt8}, Ptr{UInt8}, Ptr{Cvoid},
                      Csize_t, Ptr{Ptr{Cvoid}}),
                     commit_id_ptr, repo, isempty(refname) ? C_NULL : refname,
                     author, committer,
                     C_NULL, msg, tree,
                     nparents, nparents > 0 ? parentptrs : C_NULL)
    end
    return commit_id_ptr[]
end

"""
    commit(repo::GitRepo, msg::AbstractString; kwargs...)::GitHash

Wrapper around [`git_commit_create`](https://libgit2.org/libgit2/#HEAD/group/commit/git_commit_create).
Create a commit in the repository `repo`. `msg` is the commit message. Return the OID of the new commit.

The keyword arguments are:
  * `refname::AbstractString=Consts.HEAD_FILE`: if not NULL, the name of the reference to update to point to
    the new commit. For example, `"HEAD"` will update the HEAD of the current branch. If the reference does
    not yet exist, it will be created.
  * `author::Signature = Signature(repo)` is a `Signature` containing information about the person who authored the commit.
  * `committer::Signature = Signature(repo)` is a `Signature` containing information about the person who committed the commit to
    the repository. Not necessarily the same as `author`, for instance if `author` emailed a patch to
    `committer` who committed it.
  * `tree_id::GitHash = GitHash()` is a git tree to use to create the commit, showing its ancestry and relationship with
    any other history. `tree` must belong to `repo`.
  * `parent_ids::Vector{GitHash}=GitHash[]` is a list of commits by [`GitHash`](@ref) to use as parent
    commits for the new one, and may be empty. A commit might have multiple parents if it is a merge commit, for example.
"""
function commit(repo::GitRepo, msg::AbstractString;
                refname::AbstractString=Consts.HEAD_FILE,
                author::Signature = Signature(repo),
                committer::Signature = Signature(repo),
                tree_id::GitHash = GitHash(),
                parent_ids::Vector{GitHash}=GitHash[])
    # Retrieve tree identifier
    if iszero(tree_id)
        tree_id = with(GitIndex, repo) do idx; write_tree!(idx) end
    end

    # Retrieve parents from HEAD
    if isempty(parent_ids)
        try # if throws then HEAD not found -> empty repo
            Base.push!(parent_ids, GitHash(repo, refname))
        catch
        end
    end

    # return commit id
    commit_id  = GitHash()

    # get necessary objects
    tree = GitTree(repo, tree_id)
    auth_sig = convert(GitSignature, author)
    comm_sig = convert(GitSignature, committer)
    parents = GitCommit[]
    try
        for id in parent_ids
            Base.push!(parents, GitCommit(repo, id))
        end
        commit_id = commit(repo, refname, msg, auth_sig, comm_sig, tree, parents...)
    finally
        for parent in parents
            close(parent)
        end
        close(tree)
        close(auth_sig)
        close(comm_sig)
    end
    return commit_id
end

"""
    parentcount(c::GitCommit)

Get the number of parents of this commit.

See also [`parent`](@ref), [`parent_id`](@ref).
"""
parentcount(c::GitCommit) =
    Int(ccall((:git_commit_parentcount, libgit2), Cuint, (Ptr{Cvoid},), c))

"""
    parent(c::GitCommit, n)

Get the `n`-th (1-based) parent of the commit.

See also [`parentcount`](@ref), [`parent_id`](@ref).
"""
function parent(c::GitCommit, n)
    ptr_ref = Ref{Ptr{Cvoid}}()
    @check ccall((:git_commit_parent, libgit2), Cint,
                 (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}, Cuint), ptr_ref, c, n - 1)
    return GitCommit(c.owner, ptr_ref[])
end

"""
    parent_id(c::GitCommit, n)

Get the oid of the `n`-th (1-based) parent for a commit.

See also [`parentcount`](@ref), [`parent`](@ref).
"""
function parent_id(c::GitCommit, n)
    oid_ptr = ccall((:git_commit_parent_id, libgit2), Ptr{GitHash},
                    (Ptr{Cvoid}, Cuint), c, n - 1)
    if oid_ptr == C_NULL
        # 0-based indexing mimicking the error message from libgit2
        throw(GitError(Error.Invalid, Error.ENOTFOUND,
                       "parent $(n - 1) does not exist"))
    end
    return unsafe_load(oid_ptr)
end
