# This file is a part of Julia. License is MIT: http://julialang.org/license

# TODO: make this a general purpose solution
function Base.cconvert(::Type{Ptr{DiffOptionsStruct}}, pathspecs::AbstractString)
    str_ref = Base.cconvert(Ref{Cstring}, [pathspecs])
    sa = StrArrayStruct(Base.unsafe_convert(Ref{Cstring}, str_ref), 1)
    do_ref = Ref(DiffOptionsStruct(pathspec = sa))
    do_ref, str_ref
end
function Base.unsafe_convert(::Type{Ptr{DiffOptionsStruct}}, rr::Tuple{Ref{DiffOptionsStruct}, Ref{Cstring}})
    Base.unsafe_convert(Ptr{DiffOptionsStruct}, first(rr))
end


function diff_tree(repo::GitRepo, tree::GitTree, pathspecs::AbstractString=""; cached::Bool=false)
    diff_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    if cached
        @check ccall((:git_diff_tree_to_index, :libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{DiffOptionsStruct}),
                     diff_ptr_ptr, repo.ptr, tree.ptr, C_NULL, isempty(pathspecs) ? C_NULL : pathspecs)
    else
        @check ccall((:git_diff_tree_to_workdir_with_index, :libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{DiffOptionsStruct}),
                     diff_ptr_ptr, repo.ptr, tree.ptr, isempty(pathspecs) ? C_NULL : pathspecs)
    end
    return GitDiff(repo, diff_ptr_ptr[])
end

function diff_tree(repo::GitRepo, oldtree::GitTree, newtree::GitTree)
    diff_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_diff_tree_to_tree, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{DiffOptionsStruct}),
                   diff_ptr_ptr, repo.ptr, oldtree.ptr, newtree.ptr, C_NULL)
    return GitDiff(repo, diff_ptr_ptr[])
end

function Base.count(diff::GitDiff)
    return ccall((:git_diff_num_deltas, :libgit2), Cint, (Ptr{Void},), diff.ptr)
end

function Base.getindex(diff::GitDiff, i::Integer)
    delta_ptr = ccall((:git_diff_get_delta, :libgit2),
                      Ptr{DiffDelta},
                      (Ptr{Void}, Csize_t), diff.ptr, i-1)
    delta_ptr == C_NULL && throw(BoundsError(diff, (i,)))
    return unsafe_load(delta_ptr)
end
