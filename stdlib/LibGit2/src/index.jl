# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    GitIndex(repo::GitRepo)

Load the index file for the repository `repo`.
"""
function GitIndex(repo::GitRepo)
    ensure_initialized()
    idx_ptr_ptr = Ref{Ptr{Cvoid}}(C_NULL)
    @check ccall((:git_repository_index, libgit2), Cint,
                 (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}), idx_ptr_ptr, repo.ptr)
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
    ensure_initialized()
    @check ccall((:git_index_read, libgit2), Cint, (Ptr{Cvoid}, Cint), idx.ptr, Cint(force))
    return idx
end

"""
    write!(idx::GitIndex) -> GitIndex

Write the state of index `idx` to disk using a file lock.
"""
function write!(idx::GitIndex)
    ensure_initialized()
    @check ccall((:git_index_write, libgit2), Cint, (Ptr{Cvoid},), idx.ptr)
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
    ensure_initialized()
    oid_ptr = Ref(GitHash())
    @check ccall((:git_index_write_tree, libgit2), Cint,
                 (Ptr{GitHash}, Ptr{Cvoid}), oid_ptr, idx.ptr)
    return oid_ptr[]
end

function repository(idx::GitIndex)
    if idx.owner === nothing
        throw(GitError(Error.Index, Error.ENOTFOUND, "Index does not have an owning repository."))
    else
        return idx.owner
    end
end

"""
    LibGit2.read_tree!(idx::GitIndex, tree::GitTree)
    LibGit2.read_tree!(idx::GitIndex, treehash::AbstractGitHash)

Read the tree `tree` (or the tree pointed to by `treehash` in the repository owned by
`idx`) into the index `idx`. The current index contents will be replaced.
"""
function read_tree!(idx::GitIndex, tree::GitTree)
    ensure_initialized()
    @check ccall((:git_index_read_tree, libgit2), Cint,
                 (Ptr{Cvoid}, Ptr{Cvoid}), idx.ptr, tree.ptr)
end
read_tree!(idx::GitIndex, hash::AbstractGitHash) =
    read_tree!(idx, GitTree(repository(idx), hash))

"""
    add!(repo::GitRepo, files::AbstractString...; flags::Cuint = Consts.INDEX_ADD_DEFAULT)
    add!(idx::GitIndex, files::AbstractString...; flags::Cuint = Consts.INDEX_ADD_DEFAULT)

Add all the files with paths specified by `files` to the index `idx` (or the index
of the `repo`). If the file already exists, the index entry will be updated.
If the file does not exist already, it will be newly added into the index.
`files` may contain glob patterns which will be expanded and any matching files will
be added (unless `INDEX_ADD_DISABLE_PATHSPEC_MATCH` is set, see below).
If a file has been ignored (in `.gitignore` or in the config), it *will not* be
added, *unless* it is already being tracked in the index, in which case it *will* be
updated. The keyword argument `flags` is a set of bit-flags which control the behavior
with respect to ignored files:
  * `Consts.INDEX_ADD_DEFAULT` - default, described above.
  * `Consts.INDEX_ADD_FORCE` - disregard the existing ignore rules and force addition of
    the file to the index even if it is already ignored.
  * `Consts.INDEX_ADD_CHECK_PATHSPEC` - cannot be used at the same time as `INDEX_ADD_FORCE`.
    Check that each file in `files` which exists on disk is not in the ignore list. If one
    of the files *is* ignored, the function will return `EINVALIDSPEC`.
  * `Consts.INDEX_ADD_DISABLE_PATHSPEC_MATCH` - turn off glob matching, and only add files
    to the index which exactly match the paths specified in `files`.
"""
function add!(idx::GitIndex, files::AbstractString...;
              flags::Cuint = Consts.INDEX_ADD_DEFAULT)
    ensure_initialized()
    @check ccall((:git_index_add_all, libgit2), Cint,
                 (Ptr{Cvoid}, Ptr{StrArrayStruct}, Cuint, Ptr{Cvoid}, Ptr{Cvoid}),
                 idx.ptr, collect(files), flags, C_NULL, C_NULL)
end

"""
    update!(repo::GitRepo, files::AbstractString...)
    update!(idx::GitIndex, files::AbstractString...)

Update all the files with paths specified by `files` in the index `idx` (or the index
of the `repo`). Match the state of each file in the index with the current state on
disk, removing it if it has been removed on disk, or updating its entry in the object
database.
"""
function update!(idx::GitIndex, files::AbstractString...)
    ensure_initialized()
    @check ccall((:git_index_update_all, libgit2), Cint,
                 (Ptr{Cvoid}, Ptr{StrArrayStruct}, Ptr{Cvoid}, Ptr{Cvoid}),
                 idx.ptr, collect(files), C_NULL, C_NULL)
end

"""
    remove!(repo::GitRepo, files::AbstractString...)
    remove!(idx::GitIndex, files::AbstractString...)

Remove all the files with paths specified by `files` in the index `idx` (or the index
of the `repo`).
"""
function remove!(idx::GitIndex, files::AbstractString...)
    ensure_initialized()
    @check ccall((:git_index_remove_all, libgit2), Cint,
                 (Ptr{Cvoid}, Ptr{StrArrayStruct}, Ptr{Cvoid}, Ptr{Cvoid}),
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

function count(idx::GitIndex)
    ensure_initialized()
    return ccall((:git_index_entrycount, libgit2), Csize_t, (Ptr{Cvoid},), idx.ptr)
end

function Base.getindex(idx::GitIndex, i::Integer)
    ensure_initialized()
    GC.@preserve idx begin
        ie_ptr = ccall((:git_index_get_byindex, libgit2),
                       Ptr{IndexEntry},
                       (Ptr{Cvoid}, Csize_t), idx.ptr, i-1)
        ie_ptr == C_NULL && return nothing
        elem = unsafe_load(ie_ptr)
    end
    return elem
end

function Base.findall(path::String, idx::GitIndex)
    ensure_initialized()
    pos_ref = Ref{Csize_t}(0)
    ret = ccall((:git_index_find, libgit2), Cint,
                  (Ref{Csize_t}, Ptr{Cvoid}, Cstring), pos_ref, idx.ptr, path)
    ret == Cint(Error.ENOTFOUND) && return nothing
    return pos_ref[]+1
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
function stage(ie::IndexEntry)
    ensure_initialized()
    return ccall((:git_index_entry_stage, libgit2), Cint, (Ptr{IndexEntry},), Ref(ie))
end

function Base.show(io::IO, idx::GitIndex)
    println(io, "GitIndex:\nRepository: ", repository(idx), "\nNumber of elements: ", count(idx))
end
