# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    LibGit2.GitStatus(repo::GitRepo; status_opts=StatusOptions())

Collect information about the status of each file in the git
repository `repo` (e.g. is the file modified, staged, etc.).
`status_opts` can be used to set various options, for instance
whether or not to look at untracked files or whether to include
submodules or not.
"""
function GitStatus(repo::GitRepo; status_opts=StatusOptions())
    stat_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_status_list_new, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{StatusOptions}),
                  stat_ptr_ptr, repo.ptr, Ref(status_opts))
    return GitStatus(repo, stat_ptr_ptr[])
end

function Base.length(status::GitStatus)
    return Int(ccall((:git_status_list_entrycount, :libgit2), Csize_t,
                      (Ptr{Ptr{Void}},), status.ptr))
end

function Base.getindex(status::GitStatus, i::Integer)
    1 <= i <= length(status) || throw(BoundsError())
    entry_ptr = ccall((:git_status_byindex, :libgit2),
                      Ptr{StatusEntry},
                      (Ptr{Void}, Csize_t),
                      status.ptr, i-1)
    entry_ptr == C_NULL && throw(Error.GitError(Error.ERROR))
    return unsafe_load(entry_ptr)
end

"""
    LibGit2.status(repo::GitRepo, path::String)

Lookup the status of the file at `path` in the git
repository `repo`. For instance, this can be used
to check if the file at `path` has been modified
and needs to be staged and committed.
"""
function status(repo::GitRepo, path::String)
    status_ptr = Ref{Cuint}(0)
    ret =  ccall((:git_status_file, :libgit2), Cint,
                  (Ref{Cuint}, Ptr{Void}, Cstring),
                  status_ptr, repo.ptr, path)
    (ret == Cint(Error.ENOTFOUND) || ret == Cint(Error.EAMBIGUOUS)) && return Nullable{Cuint}()
    return Nullable(status_ptr[])
end
