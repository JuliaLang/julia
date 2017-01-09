# This file is a part of Julia. License is MIT: http://julialang.org/license

function GitRebase(repo::GitRepo, branch::GitAnnotated, upstream::GitAnnotated;
                   onto::Nullable{GitAnnotated}=Nullable{GitAnnotated}(),
                   opts::RebaseOptions = RebaseOptions())
    rebase_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_rebase_init, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}, Ptr{Void},
                   Ptr{Void}, Ptr{RebaseOptions}),
                   rebase_ptr_ptr, repo.ptr, branch.ptr, upstream.ptr,
                   isnull(onto) ? C_NULL : Base.get(onto).ptr, Ref(opts))
    return GitRebase(repo, rebase_ptr_ptr[])
end

function Base.count(rb::GitRebase)
    return ccall((:git_rebase_operation_entrycount, :libgit2), Csize_t, (Ptr{Void},), rb.ptr)
end

function current(rb::GitRebase)
    return ccall((:git_rebase_operation_current, :libgit2), Csize_t, (Ptr{Void},), rb.ptr)
end

function Base.getindex(rb::GitRebase, i::Integer)
    rb_op_ptr = ccall((:git_rebase_operation_byindex, :libgit2),
                      Ptr{RebaseOperation},
                      (Ptr{Void}, Csize_t), rb.ptr, i-1)
    rb_op_ptr == C_NULL && return nothing
    return unsafe_load(rb_op_ptr)
end

function Base.next(rb::GitRebase)
    rb_op_ptr_ptr = Ref{Ptr{RebaseOperation}}(C_NULL)
    try
        @check ccall((:git_rebase_next, :libgit2), Cint,
                      (Ptr{Ptr{RebaseOperation}}, Ptr{Void}),
                       rb_op_ptr_ptr, rb.ptr)
    catch err
        err.code == Error.ITEROVER && return nothing
        rethrow(err)
    end
    return unsafe_load(rb_op_ptr_ptr[])
end


"""
    LibGit2.commit(rb::GitRebase, sig::GitSignature)

Commits the current patch to the rebase `rb`, using `sig` as the committer. Is silent if
the commit has already been applied.
"""
function commit(rb::GitRebase, sig::GitSignature)
    oid_ptr = Ref(GitHash())
    try
        @check ccall((:git_rebase_commit, :libgit2), Error.Code,
                     (Ptr{GitHash}, Ptr{Void}, Ptr{SignatureStruct}, Ptr{SignatureStruct}, Ptr{UInt8}, Ptr{UInt8}),
                      oid_ptr, rb.ptr, C_NULL, sig.ptr, C_NULL, C_NULL)
    catch err
        # TODO: return current HEAD instead
        err.code == Error.EAPPLIED && return nothing
        rethrow(err)
    end
    return oid_ptr[]
end

function abort(rb::GitRebase)
    return ccall((:git_rebase_abort, :libgit2), Csize_t,
                      (Ptr{Void},), rb.ptr)
end

function finish(rb::GitRebase, sig::GitSignature)
    return ccall((:git_rebase_finish, :libgit2), Csize_t,
                  (Ptr{Void}, Ptr{SignatureStruct}),
                   rb.ptr, sig.ptr)
end
