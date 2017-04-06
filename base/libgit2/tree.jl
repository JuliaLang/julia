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

function repository(tree::GitTree)
    repo_ptr = ccall((:git_tree_owner, :libgit2), Ptr{Void},
                 (Ptr{Void},), tree.ptr)
    return GitRepo(repo_ptr)
end

function filename(te::GitTreeEntry)
    str = ccall((:git_tree_entry_name, :libgit2), Cstring, (Ptr{Void},), te.ptr)
    str != C_NULL && return unsafe_string(str)
    return nothing
end

function filemode(te::GitTreeEntry)
    return ccall((:git_tree_entry_filemode, :libgit2), Cint, (Ptr{Void},), te.ptr)
end

function entrytype(te::GitTreeEntry)
    otype = ccall((:git_tree_entry_type, :libgit2), Cint, (Ptr{Void},), te.ptr)
    return getobjecttype(otype)
end

function entryid(te::GitTreeEntry)
    oid_ptr = ccall((:git_tree_entry_id, :libgit2), Cint, (Ptr{Void},), te.ptr)
    return GitHash(oid_ptr[])
end

function (::Type{T}){T<:GitObject}(repo::GitRepo, te::GitTreeEntry)
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_tree_entry_to_object, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ref{Void}),
                   obj_ptr_ptr, repo.ptr, te.ptr)
    return T(repo, obj_ptr_ptr[])
end

function Base.show(io::IO, te::GitTreeEntry)
    println(io, "GitTreeEntry:")
    println(io, "Entry name: ", filename(te))
    println(io, "Entry type: ", entrytype(te))
    println(io, "Entry OID: ", entryid(te))
end
