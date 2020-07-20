# This file is a part of Julia. License is MIT: https://julialang.org/license

function GitTree(c::GitCommit)
    tree_out = Ref{Ptr{Cvoid}}(C_NULL)
    @check ccall((:git_commit_tree, :libgit2), Cint, (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}), tree_out, c)
    GitTree(repository(c), tree_out[])
end

"""
    treewalk(f, tree::GitTree, post::Bool=false)

Traverse the entries in `tree` and its subtrees in post or pre order. Preorder
means beginning at the root and then traversing the leftmost subtree (and
recursively on down through that subtree's leftmost subtrees) and moving right
through the subtrees. Postorder means beginning at the bottom of the leftmost
subtree, traversing upwards through it, then traversing the next right subtree
(again beginning at the bottom) and finally visiting the tree root last of all.

The function parameter `f` should have following signature:

    (String, GitTreeEntry) -> Cint

A negative value returned from `f` stops the tree walk. A positive value means
that the entry will be skipped if `post` is `false`.
"""
function treewalk(f, tree::GitTree, post::Bool = false)
    ensure_initialized()
    # NOTE: don't use @check/GitError directly, because the code can be arbitrary
    payload = Any[ tree, f ]
    cbf = @cfunction(function treewalk_callback(root_cstr::Cstring, entry_ptr::Ptr{Cvoid}, payload::Vector{Any})::Cint
            # decode arguments
            root = unsafe_string(root_cstr)
            tree = payload[1]::GitTree
            f = payload[2]
            entry = GitTreeEntry(tree, entry_ptr, false)
            return f(root, entry)
        end, Cint, (Cstring, Ptr{Cvoid}, Ref{Vector{Any}}))
    err = ccall((:git_tree_walk, :libgit2), Cint,
                (Ptr{Cvoid}, Cint, Ptr{Cvoid}, Any),
                tree.ptr, post, cbf, payload)
    if err < 0
        err_class, _ = Error.last_error()
        if err_class != Error.Callback
            # now we now the code is valid
            throw(GitError(err))
        end
    end
    nothing
end

repository(tree::GitTree) = tree.owner
repository(te::GitTreeEntry) = repository(te.owner)

"""
    filename(te::GitTreeEntry)

Return the filename of the object on disk to which `te` refers.
"""
function filename(te::GitTreeEntry)
    ensure_initialized()
    str = ccall((:git_tree_entry_name, :libgit2), Cstring, (Ptr{Cvoid},), te.ptr)
    str != C_NULL && return unsafe_string(str)
    return nothing
end

"""
    filemode(te::GitTreeEntry) -> Cint

Return the UNIX filemode of the object on disk to which `te` refers as an integer.
"""
function filemode(te::GitTreeEntry)
    ensure_initialized()
    return ccall((:git_tree_entry_filemode, :libgit2), Cint, (Ptr{Cvoid},), te.ptr)
end

"""
    entrytype(te::GitTreeEntry)

Return the type of the object to which `te` refers. The result will be
one of the types which [`objtype`](@ref) returns, e.g. a `GitTree` or `GitBlob`.
"""
function entrytype(te::GitTreeEntry)
    ensure_initialized()
    otype = ccall((:git_tree_entry_type, :libgit2), Cint, (Ptr{Cvoid},), te.ptr)
    return objtype(Consts.OBJECT(otype))
end

"""
    entryid(te::GitTreeEntry)

Return the [`GitHash`](@ref) of the object to which `te` refers.
"""
function entryid(te::GitTreeEntry)
    ensure_initialized()
    GC.@preserve te begin
        oid_ptr = ccall((:git_tree_entry_id, :libgit2), Ptr{UInt8}, (Ptr{Cvoid},), te.ptr)
        oid = GitHash(oid_ptr)
    end
    return oid
end

function count(tree::GitTree)
    ensure_initialized()
    return ccall((:git_tree_entrycount, :libgit2), Csize_t, (Ptr{Cvoid},), tree.ptr)
end

function Base.getindex(tree::GitTree, i::Integer)
    if i < 1 || i > count(tree)
        throw(BoundsError(tree, i))
    end
    ensure_initialized()
    te_ptr = ccall((:git_tree_entry_byindex, :libgit2),
                   Ptr{Cvoid},
                   (Ptr{Cvoid}, Csize_t), tree.ptr, i-1)
    return GitTreeEntry(tree, te_ptr, false)
end

"""
    (::Type{T})(te::GitTreeEntry) where T<:GitObject

Get the git object to which `te` refers and return it as its actual type (the type
[`entrytype`](@ref) would show), for instance a `GitBlob` or `GitTag`.

# Examples
```julia
tree = LibGit2.GitTree(repo, "HEAD^{tree}")
tree_entry = tree[1]
blob = LibGit2.GitBlob(tree_entry)
```
"""
GitObject(e::GitTreeEntry)
function (::Type{T})(te::GitTreeEntry) where T<:GitObject
    ensure_initialized()
    repo = repository(te)
    obj_ptr_ptr = Ref{Ptr{Cvoid}}(C_NULL)
    @check ccall((:git_tree_entry_to_object, :libgit2), Cint,
                  (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}, Ptr{Cvoid}),
                   obj_ptr_ptr, repo, te)
    return T(repo, obj_ptr_ptr[])
end

function Base.show(io::IO, te::GitTreeEntry)
    println(io, "GitTreeEntry:")
    println(io, "Entry name: ", filename(te))
    println(io, "Entry type: ", entrytype(te))
    println(io, "Entry OID: ", entryid(te))
end

function Base.show(io::IO, tree::GitTree)
    println(io, "GitTree:")
    println(io, "Owner: ", repository(tree))
    println(io, "Number of entries: ", count(tree))
end

function _getindex(tree::GitTree, target::AbstractString)
    if basename(target) == ""
        # get rid of any trailing separator
        target = dirname(target)
    end
    if isempty(target) || target == "/"
        return tree
    end

    entry = Ref{Ptr{Cvoid}}(C_NULL)
    err = ccall((:git_tree_entry_bypath, :libgit2), Cint, (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}, Cstring), entry, tree, target)
    err == Int(Error.ENOTFOUND) && return nothing
    err < 0 && throw(Error.GitError(err))
    entry = GitTreeEntry(tree, entry[], true #= N.B.: Most other lookups need false here =#)
    return GitObject(entry)
end

"""
    getindex(tree::GitTree, target::AbstractString) -> GitObject

Look up `target` path in the `tree`, returning a [`GitObject`](@ref) (a [`GitBlob`](@ref) in
the case of a file, or another [`GitTree`](@ref) if looking up a directory).

# Examples
```julia
tree = LibGit2.GitTree(repo, "HEAD^{tree}")
readme = tree["README.md"]
subtree = tree["test"]
runtests = subtree["runtests.jl"]
```
"""
function Base.getindex(tree::GitTree, target::AbstractString)
    e = _getindex(tree, target)
    e === nothing && throw(KeyError(target))
    return e
end

function Base.haskey(tree::GitTree, target::AbstractString)
    return _getindex(tree, target) !== nothing
end
