# This file is a part of Julia. License is MIT: http://julialang.org/license

function GitStatus(repo::GitRepo; status_opts=StatusOptions())
    stat_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_status_list_new, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{StatusOptions}),
                  stat_ptr_ptr, repo.ptr, Ref(status_opts))
    return GitStatus(stat_ptr_ptr[])
end

function Base.length(status::GitStatus)
    return Int(ccall((:git_status_list_entrycount, :libgit2), Csize_t,
                      (Ptr{Ptr{Void}}, ), status.ptr))
end

function Base.getindex(status::GitStatus, i::Csize_t)
    entry_ptr = ccall((:git_status_byindex, :libgit2), Ptr{Void},
                       (Ptr{Void}, Csize_t), status.ptr, i-1)
    entry_ptr == C_NULL && return nothing
    return unsafe_load(convert(Ptr{StatusEntry}, entry_ptr), 1)
end
Base.getindex(status::GitStatus, i::Int) = getindex(status, Csize_t(i))


