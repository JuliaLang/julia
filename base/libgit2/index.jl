# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    GitIndex(repo::GitRepo)

Load the index file for the repository `repo`.
"""
function GitIndex(repo::GitRepo)
    idx_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_repository_index, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}), idx_ptr_ptr, repo.ptr)
    return GitIndex(repo, idx_ptr_ptr[])
end

"""
    read!(idx::GitIndex, force::Bool = false) -> GitIndex

Update the contents of `idx` by reading changes made on disk. For example, `idx`
might be updated if a file has been added to the repository since it was created.
If `force` is `true`, any changes in memory (any changes in `idx` since its last
[`write!`](@ref), or since its creation if no writes have occurred) are discarded.
If `force` is `false`, the index data is only updated from disk if the data on disk
has changed since the last time it was loaded into `idx`.
"""
function read!(idx::GitIndex, force::Bool = false)
    @check ccall((:git_index_read, :libgit2), Cint, (Ptr{Void}, Cint), idx.ptr, Cint(force))
    return idx
end

"""
    write!(idx::GitIndex) -> GitIndex

Write the state of index `idx` to disk using a file lock.
"""
function write!(idx::GitIndex)
    @check ccall((:git_index_write, :libgit2), Cint, (Ptr{Void},), idx.ptr)
    return idx
end

"""
    write_tree!(idx::GitIndex) -> GitHash

Write the index `idx` as a [`GitTree`](@ref) on disk. Trees will be recursively
created for each subtree in `idx`. The returned [`GitHash`](@ref) can be used to
create a [`GitCommit`](@ref). `idx` must have a parent repository and this
repository cannot be bare. `idx` must not contain any files with conflicts.
"""
function write_tree!(idx::GitIndex)
    oid_ptr = Ref(GitHash())
    @check ccall((:git_index_write_tree, :libgit2), Cint,
                 (Ptr{GitHash}, Ptr{Void}), oid_ptr, idx.ptr)
    return oid_ptr[]
end

function repository(idx::GitIndex)
    if isnull(idx.owner)
        throw(GitError(Error.Index, Error.ENOTFOUND, "Index does not have an owning repository."))
    else
        return Base.get(idx.owner)
    end
end

"""
    LibGit2.read_tree!(idx::GitIndex, tree::GitTree)
    LibGit2.read_tree!(idx::GitIndex, treehash::AbstractGitHash)

Read the tree `tree` (or the tree pointed to by `treehash` in the repository owned by
`idx`) into the index `idx`. The current index contents will be replaced.
"""
function read_tree!(idx::GitIndex, tree::GitTree)
    @check ccall((:git_index_read_tree, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}), idx.ptr, tree.ptr)
end
read_tree!(idx::GitIndex, hash::AbstractGitHash) =
    read_tree!(idx, GitTree(repository(idx), hash))

function add!(idx::GitIndex, files::AbstractString...;
              flags::Cuint = Consts.INDEX_ADD_DEFAULT)
    @check ccall((:git_index_add_all, :libgit2), Cint,
                 (Ptr{Void}, Ptr{StrArrayStruct}, Cuint, Ptr{Void}, Ptr{Void}),
                 idx.ptr, collect(files), flags, C_NULL, C_NULL)
end

function update!(idx::GitIndex, files::AbstractString...)
    @check ccall((:git_index_update_all, :libgit2), Cint,
                 (Ptr{Void}, Ptr{StrArrayStruct}, Ptr{Void}, Ptr{Void}),
                 idx.ptr, collect(files), C_NULL, C_NULL)
end

function remove!(idx::GitIndex, files::AbstractString...)
    @check ccall((:git_index_remove_all, :libgit2), Cint,
                 (Ptr{Void}, Ptr{StrArrayStruct}, Ptr{Void}, Ptr{Void}),
                 idx.ptr, collect(files), C_NULL, C_NULL)
end

function add!(repo::GitRepo, files::AbstractString...;
              flags::Cuint = Consts.INDEX_ADD_DEFAULT)
    with(GitIndex, repo) do idx
        add!(idx, files..., flags = flags)
        write!(idx)
    end
    return
end

function update!(repo::GitRepo, files::AbstractString...)
    with(GitIndex, repo) do idx
        update!(idx, files...)
        write!(idx)
    end
    return
end

function remove!(repo::GitRepo, files::AbstractString...)
    with(GitIndex, repo) do idx
        remove!(idx, files...)
        write!(idx)
    end
    return
end

function read!(repo::GitRepo, force::Bool = false)
    with(GitIndex, repo) do idx
        read!(idx, force)
    end
    return
end

function Base.count(idx::GitIndex)
    return ccall((:git_index_entrycount, :libgit2), Csize_t, (Ptr{Void},), idx.ptr)
end

function Base.getindex(idx::GitIndex, i::Integer)
    ie_ptr = ccall((:git_index_get_byindex, :libgit2),
                   Ptr{IndexEntry},
                   (Ptr{Void}, Csize_t), idx.ptr, i-1)
    ie_ptr == C_NULL && return nothing
    return unsafe_load(ie_ptr)
end

function Base.find(path::String, idx::GitIndex)
    pos_ref = Ref{Csize_t}(0)
    ret = ccall((:git_index_find, :libgit2), Cint,
                  (Ref{Csize_t}, Ptr{Void}, Cstring), pos_ref, idx.ptr, path)
    ret == Cint(Error.ENOTFOUND) && return Nullable{Csize_t}()
    return Nullable(pos_ref[]+1)
end

"""
    stage(ie::IndexEntry) -> Cint

Get the stage number of `ie`. The stage number `0` represents the current state
of the working tree, but other numbers can be used in the case of a merge conflict.
In such a case, the various stage numbers on an `IndexEntry` describe which side(s)
of the conflict the current state of the file belongs to. Stage `0` is the state
before the attempted merge, stage `1` is the changes which have been made locally,
stages `2` and larger are for changes from other branches (for instance, in the case
of a multi-branch "octopus" merge, stages `2`, `3`, and `4` might be used).
"""
stage(ie::IndexEntry) = ccall((:git_index_entry_stage, :libgit2), Cint, (Ptr{IndexEntry},), Ref(ie))

function Base.show(io::IO, idx::GitIndex)
    println(io, "GitIndex:\nRepository: ", repository(idx), "\nNumber of elements: ", count(idx))
end
