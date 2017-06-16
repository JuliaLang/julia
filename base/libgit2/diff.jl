# This file is a part of Julia. License is MIT: https://julialang.org/license

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

function GitDiffStats(diff::GitDiff)
    diff_stat_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_diff_get_stats, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}),
                  diff_stat_ptr_ptr, diff.ptr)
    return GitDiffStats(diff.owner, diff_stat_ptr_ptr[])
end

function files_changed(diff_stat::GitDiffStats)
    return ccall((:git_diff_stats_files_changed, :libgit2), Csize_t, (Ptr{Void},), diff_stat.ptr)
end

function insertions(diff_stat::GitDiffStats)
    return ccall((:git_diff_stats_insertions, :libgit2), Csize_t, (Ptr{Void},), diff_stat.ptr)
end

function deletions(diff_stat::GitDiffStats)
    return ccall((:git_diff_stats_deletions, :libgit2), Csize_t, (Ptr{Void},), diff_stat.ptr)
end

function Base.count(diff::GitDiff)
    return ccall((:git_diff_num_deltas, :libgit2), Cint, (Ptr{Void},), diff.ptr)
end

function Base.getindex(diff::GitDiff, i::Integer)
    if i < 1 || i > count(diff)
        throw(BoundsError(diff, (i,)))
    end
    delta_ptr = ccall((:git_diff_get_delta, :libgit2),
                      Ptr{DiffDelta},
                      (Ptr{Void}, Csize_t), diff.ptr, i-1)
    return unsafe_load(delta_ptr)
end

function Base.show(io::IO, diff_stat::GitDiffStats)
    println(io, "GitDiffStats:")
    println(io, "Files changed: $(files_changed(diff_stat))")
    println(io, "Insertions: $(insertions(diff_stat))")
    println(io, "Deletions: $(deletions(diff_stat))")
end

function Base.show(io::IO, diff::GitDiff)
    println(io, "GitDiff:")
    println(io, "Number of deltas: $(count(diff))")
    show(io, GitDiffStats(diff))
end
