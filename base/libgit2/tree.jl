# This file is a part of Julia. License is MIT: http://julialang.org/license

"""
Traverse the entries in a tree and its subtrees in post or pre order.

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

function filename(te::GitTreeEntry)
    str = ccall((:git_tree_entry_name, :libgit2), Cstring, (Ptr{Void},), te.ptr)
    str != C_NULL && return unsafe_string(str)
    return nothing
end

function filemode(te::GitTreeEntry)
    return ccall((:git_tree_entry_filemode, :libgit2), Cint, (Ptr{Void},), te.ptr)
end


function object(repo::GitRepo, te::GitTreeEntry)
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_tree_entry_to_object, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ref{Void}),
                   obj_ptr_ptr, repo.ptr, te.ptr)
    return GitAnyObject(repo, obj_ptr_ptr[])
end
