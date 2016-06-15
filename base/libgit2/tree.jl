# This file is a part of Julia. License is MIT: http://julialang.org/license

"""Traverse the entries in a tree and its subtrees in post or preorder.

Function parameter should have following signature:

    (Cstring, Ptr{Void}, Ptr{Void}) -> Cint
"""
function treewalk(f::Function, tree::GitTree, payload=Any[], post::Bool = false)
    cbf = cfunction(f, Cint, (Cstring, Ptr{Void}, Ptr{Void}))
    cbf_payload = Ref{typeof(payload)}(payload)
    @check ccall((:git_tree_walk, :libgit2), Cint,
                  (Ptr{Void}, Cint, Ptr{Void}, Ptr{Void}),
                   tree.ptr, post, cbf, cbf_payload)
    return cbf_payload
end

"Returns a file name, as a `String`, of a tree entry."
function filename(te::GitTreeEntry)
    str = ccall((:git_tree_entry_name, :libgit2), Cstring, (Ptr{Void},), te.ptr)
    str == C_NULL && throw(Error.GitError(Error.Tree,
                                          LibGit2.Error.ENOTFOUND, "not found"))
    return unsafe_string(str)
end

"Returns UNIX file attributes, as a `Cint`, of a tree entry."
function filemode(te::GitTreeEntry)
    return ccall((:git_tree_entry_filemode, :libgit2), Cint, (Ptr{Void},), te.ptr)
end

"Returns a `GitAnyObject` which is referenced by a tree entry."
function object(repo::GitRepo, te::GitTreeEntry)
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_tree_entry_to_object, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ref{Void}),
                   obj_ptr_ptr, repo.ptr, te.ptr)
    return GitAnyObject(obj_ptr_ptr[])
end

"Returns an object identifier, as a `Oid`, of a tree entry."
function oid(te::GitTreeEntry)
    oid_ptr = ccall((:git_tree_entry_id, :libgit2), Ptr{Oid}, (Ptr{Void},), te.ptr)
    oid_ptr == C_NULL && throw(Error.GitError(Error.Tree,
                                              LibGit2.Error.ENOTFOUND, "not found"))
    return Oid(oid_ptr)
end

"""Retrieve a tree entry contained in a tree or in any of its subtrees, given its relative path.

The returned tree entry is owned by the user and must be freed explicitly.
"""
function GitTreeEntry(tree::GitTree, tepath::AbstractString)
    te_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    err = ccall((:git_tree_entry_bypath, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ref{Void}, Cstring),
                   te_ptr_ptr, tree.ptr, tepath)
    if err == Cint(Error.ENOTFOUND)
        return Nullable{GitTreeEntry}()
    elseif err != Cint(Error.GIT_OK)
        if te_ptr_ptr[] != C_NULL
            finalize(GitTreeEntry(te_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    return Nullable(GitTreeEntry(te_ptr_ptr[]))
end

"""Lookup a tree entry by SHA value.

This returns a `GitTreeEntry` that is owned by the `GitTree`.
You don't have to free it, but you must not use it after the `GitTree` is released.
"""
function GitTreeEntry(tree::GitTree, teoid::Oid)
    res = ccall((:git_tree_entry_byid, :libgit2), Ptr{Void},
                (Ref{Void}, Ref{Oid}),
                tree.ptr, Ref(teoid))
    res == C_NULL && return Nullable{GitTreeEntry}()
    return Nullable(GitTreeEntry(res))
end

"""Lookup a tree entry by its file name.

This returns a `GitTreeEntry` that is owned by the `GitTree`.
You don't have to free it, but you must not use it after the `GitTree` is released.
"""
function lookup(tree::GitTree, fname::AbstractString)
    res = ccall((:git_tree_entry_byname, :libgit2), Ptr{Void},
                (Ref{Void}, Cstring), tree.ptr, fname)
    res == C_NULL && return Nullable{GitTreeEntry}()
    return Nullable(GitTreeEntry(res))
end
