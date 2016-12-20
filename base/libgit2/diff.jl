# This file is a part of Julia. License is MIT: http://julialang.org/license

function diff_tree(repo::GitRepo, tree::GitTree, pathspecs::AbstractString=""; cached::Bool=false)
    emptypathspec = isempty(pathspecs)
    diff_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    if !emptypathspec
        sa = StrArrayStruct(pathspecs)
        diff_opts = DiffOptionsStruct(pathspec = sa)
    end
    try
        if cached
            @check ccall((:git_diff_tree_to_index, :libgit2), Cint,
                          (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{DiffOptionsStruct}),
                           diff_ptr_ptr, repo.ptr, tree.ptr, C_NULL,  emptypathspec ? C_NULL : Ref(diff_opts))
        else
            @check ccall((:git_diff_tree_to_workdir_with_index, :libgit2), Cint,
                          (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{DiffOptionsStruct}),
                           diff_ptr_ptr, repo.ptr, tree.ptr, emptypathspec ? C_NULL : Ref(diff_opts))
        end
    finally
        !emptypathspec && close(sa)
    end
    return GitDiff(diff_ptr_ptr[])
end

function diff_tree(repo::GitRepo, oldtree::GitTree, newtree::GitTree)
    diff_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_diff_tree_to_tree, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{DiffOptionsStruct}),
                   diff_ptr_ptr, repo.ptr, oldtree.ptr, newtree.ptr, C_NULL)
    return GitDiff(diff_ptr_ptr[])
end

function Base.count(diff::GitDiff)
    return ccall((:git_diff_num_deltas, :libgit2), Cint, (Ptr{Void},), diff.ptr)
end

function Base.getindex(diff::GitDiff, i::Integer)
    delta_ptr = ccall((:git_diff_get_delta, :libgit2),
                      Ptr{DiffDelta},
                      (Ptr{Void}, Csize_t), diff.ptr, i-1)
    delta_ptr == C_NULL && return nothing
    return unsafe_load(delta_ptr)
end
