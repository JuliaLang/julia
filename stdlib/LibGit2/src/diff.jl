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

"""
    diff_tree(repo::GitRepo, tree::GitTree, pathspecs::AbstractString=""; cached::Bool=false)

Generate a [`GitDiff`](@ref) between `tree` (which will be used for the "old"
side of the [`DiffDelta`](@ref)) and `repo` (which will be used for the "new" side).
If `repo` is `cached`, calls [`git_diff_tree_to_index`](https://libgit2.org/libgit2/#HEAD/group/diff/git_diff_tree_to_index).
The `cached` version is generally used to examine the diff for staged changes from one
commit to the next. If `cached` is `false`, calls
[`git_diff_tree_to_workdir_with_index`](https://libgit2.org/libgit2/#HEAD/group/diff/git_diff_tree_to_workdir_with_index).
This compares the current working directory against the [`GitIndex`](@ref) and can,
for example, be used to examine the changes in staged files before a commit.
"""
function diff_tree(repo::GitRepo, tree::GitTree, pathspecs::AbstractString=""; cached::Bool=false)
    ensure_initialized()
    diff_ptr_ptr = Ref{Ptr{Cvoid}}(C_NULL)
    if cached
        @check ccall((:git_diff_tree_to_index, :libgit2), Cint,
                     (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}, Ptr{DiffOptionsStruct}),
                     diff_ptr_ptr, repo.ptr, tree.ptr, C_NULL, isempty(pathspecs) ? C_NULL : pathspecs)
    else
        @check ccall((:git_diff_tree_to_workdir_with_index, :libgit2), Cint,
                     (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}, Ptr{Cvoid}, Ptr{DiffOptionsStruct}),
                     diff_ptr_ptr, repo.ptr, tree.ptr, isempty(pathspecs) ? C_NULL : pathspecs)
    end
    return GitDiff(repo, diff_ptr_ptr[])
end

"""
    diff_tree(repo::GitRepo, oldtree::GitTree, newtree::GitTree)

Generate a [`GitDiff`](@ref) between `oldtree` (which will be used for the "old"
side of the [`DiffDelta`](@ref)) and `newtree` (which will be used for the "new"
side of the `DiffDelta`). Equivalent to [`git_diff_tree_to_tree`](https://libgit2.org/libgit2/#HEAD/group/diff/git_diff_tree_to_tree).
This can be used to generate a diff between two commits. For instance, it could
be used to compare a commit made 2 months ago with the current latest commit, or
to compare a commit on another branch with the current latest commit on `master`.
"""
function diff_tree(repo::GitRepo, oldtree::GitTree, newtree::GitTree)
    ensure_initialized()
    diff_ptr_ptr = Ref{Ptr{Cvoid}}(C_NULL)
    @check ccall((:git_diff_tree_to_tree, :libgit2), Cint,
                  (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}, Ptr{DiffOptionsStruct}),
                   diff_ptr_ptr, repo.ptr, oldtree.ptr, newtree.ptr, C_NULL)
    return GitDiff(repo, diff_ptr_ptr[])
end

"""
    GitDiffStats(diff::GitDiff)

Get the diff statistics from the [`GitDiff`](@ref) `diff`. This object records a
summary of changes made across the `diff`. In particular, it records how many
files were changed, how many insertions were made, and how many deletions were made.
"""
function GitDiffStats(diff::GitDiff)
    ensure_initialized()
    diff_stat_ptr_ptr = Ref{Ptr{Cvoid}}(C_NULL)
    @check ccall((:git_diff_get_stats, :libgit2), Cint,
                  (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}),
                  diff_stat_ptr_ptr, diff.ptr)
    return GitDiffStats(diff.owner, diff_stat_ptr_ptr[])
end

"""
    files_changed(diff_stat::GitDiffStats) -> Csize_t

Return how many files were changed (added/modified/deleted) in the [`GitDiff`](@ref)
summarized by `diff_stat`. The result may vary depending on the [`DiffOptionsStruct`](@ref)
used to generate the parent `GitDiff` of `diff_stat` (for instance, whether ignored files
are to be included or not).
"""
function files_changed(diff_stat::GitDiffStats)
    ensure_initialized()
    return ccall((:git_diff_stats_files_changed, :libgit2), Csize_t, (Ptr{Cvoid},), diff_stat.ptr)
end

"""
    insertions(diff_stat::GitDiffStats) -> Csize_t

Return the total number of insertions (lines added) in the [`GitDiff`](@ref)
summarized by `diff_stat`. The result may vary depending on the [`DiffOptionsStruct`](@ref)
used to generate the parent `GitDiff` of `diff_stat` (for instance, whether ignored files
are to be included or not).
"""
function insertions(diff_stat::GitDiffStats)
    ensure_initialized()
    return ccall((:git_diff_stats_insertions, :libgit2), Csize_t, (Ptr{Cvoid},), diff_stat.ptr)
end

"""
    deletions(diff_stat::GitDiffStats) -> Csize_t

Return the total number of deletions (lines removed) in the [`GitDiff`](@ref)
summarized by `diff_stat`. The result may vary depending on the [`DiffOptionsStruct`](@ref)
used to generate the parent `GitDiff` of `diff_stat` (for instance, whether ignored files
are to be included or not).
"""
function deletions(diff_stat::GitDiffStats)
    ensure_initialized()
    return ccall((:git_diff_stats_deletions, :libgit2), Csize_t, (Ptr{Cvoid},), diff_stat.ptr)
end

function count(diff::GitDiff)
    ensure_initialized()
    return ccall((:git_diff_num_deltas, :libgit2), Cint, (Ptr{Cvoid},), diff.ptr)
end

function Base.getindex(diff::GitDiff, i::Integer)
    if i < 1 || i > count(diff)
        throw(BoundsError(diff, (i,)))
    end
    ensure_initialized()
    delta_ptr = ccall((:git_diff_get_delta, :libgit2),
                      Ptr{DiffDelta},
                      (Ptr{Cvoid}, Csize_t), diff.ptr, i-1)
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
