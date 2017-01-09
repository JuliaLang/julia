# This file is a part of Julia. License is MIT: http://julialang.org/license

function GitIndex(repo::GitRepo)
    idx_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_repository_index, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}), idx_ptr_ptr, repo.ptr)
    return GitIndex(repo, idx_ptr_ptr[])
end

function read!(idx::GitIndex, force::Bool = false)
    @check ccall((:git_index_read, :libgit2), Cint, (Ptr{Void}, Cint), idx.ptr, Cint(force))
    return idx
end

function write!(idx::GitIndex)
    @check ccall((:git_index_write, :libgit2), Cint, (Ptr{Void},), idx.ptr)
    return idx
end

function write_tree!(idx::GitIndex)
    oid_ptr = Ref(Oid())
    @check ccall((:git_index_write_tree, :libgit2), Cint,
                 (Ptr{Oid}, Ptr{Void}), oid_ptr, idx.ptr)
    return oid_ptr[]
end

function owner(idx::GitIndex)
    isnull(idx.nrepo) && throw(GitError(Error.Index, Error.ENOTFOUND, "Index does not have an owning repository."))
    return Base.get(idx.nrepo)
end

function read_tree!(idx::GitIndex, tree_id::Oid)
    repo = owner(idx)
    tree = get(GitTree, repo, tree_id)
    try
        @check ccall((:git_index_read_tree, :libgit2), Cint,
                     (Ptr{Void}, Ptr{Void}), idx.ptr, tree.ptr)
    finally
        close(tree)
    end
end

function add!{T<:AbstractString}(idx::GitIndex, files::T...;
             flags::Cuint = Consts.INDEX_ADD_DEFAULT)
    sa = StrArrayStruct(files...)
    try
        @check ccall((:git_index_add_all, :libgit2), Cint,
                     (Ptr{Void}, Ptr{StrArrayStruct}, Cuint, Ptr{Void}, Ptr{Void}),
                      idx.ptr, Ref(sa), flags, C_NULL, C_NULL)
    finally
        close(sa)
    end
end

function update!{T<:AbstractString}(idx::GitIndex, files::T...)
    sa = StrArrayStruct(files...)
    try
        @check ccall((:git_index_update_all, :libgit2), Cint,
                     (Ptr{Void}, Ptr{StrArrayStruct}, Ptr{Void}, Ptr{Void}),
                      idx.ptr, Ref(sa), C_NULL, C_NULL)
    finally
        close(sa)
    end
end

function remove!{T<:AbstractString}(idx::GitIndex, files::T...)
    sa = StrArrayStruct(files...)
    try
        @check ccall((:git_index_remove_all, :libgit2), Cint,
                     (Ptr{Void}, Ptr{StrArrayStruct}, Ptr{Void}, Ptr{Void}),
                      idx.ptr, Ref(sa), C_NULL, C_NULL)
    finally
        close(sa)
    end
end

function add!{T<:AbstractString}(repo::GitRepo, files::T...;
             flags::Cuint = Consts.INDEX_ADD_DEFAULT)
    with(GitIndex, repo) do idx
        add!(idx, files..., flags = flags)
        write!(idx)
    end
    return
end

function update!{T<:AbstractString}(repo::GitRepo, files::T...)
    with(GitIndex, repo) do idx
        update!(idx, files...)
        write!(idx)
    end
    return
end

function remove!{T<:AbstractString}(repo::GitRepo, files::T...)
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
    ret == Error.ENOTFOUND && return Nullable{Csize_t}()
    return Nullable(pos_ref[]+1)
end

stage(ie::IndexEntry) = ccall((:git_index_entry_stage, :libgit2), Cint, (Ptr{IndexEntry},), Ref(ie))
