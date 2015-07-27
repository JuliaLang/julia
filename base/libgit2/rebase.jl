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
    return GitRebase(rebase_ptr_ptr[])
end

function Base.count(rb::GitRebase)
    return ccall((:git_rebase_operation_entrycount, :libgit2), Csize_t, (Ptr{Void},), rb.ptr)
end

function current(rb::GitRebase)
    return ccall((:git_rebase_operation_current, :libgit2), Csize_t, (Ptr{Void},), rb.ptr)
end

function Base.getindex(rb::GitRebase, i::Csize_t)
    rb_op_ptr = ccall((:git_rebase_operation_byindex, :libgit2), Ptr{Void},
                       (Ptr{Void}, Csize_t), rb.ptr, i-1)
    rb_op_ptr == C_NULL && return nothing
    return unsafe_load(convert(Ptr{RebaseOperation}, rb_op_ptr), 1)
end
Base.getindex(rb::GitRebase, i::Int) = getindex(rb, Csize_t(i))

function Base.next(rb::GitRebase)
    rb_op_ptr_ptr = Ref{Ptr{RebaseOperation}}(C_NULL)
    try
        @check ccall((:git_rebase_next, :libgit2), Cint,
                      (Ptr{Ptr{RebaseOperation}}, Ptr{Void}),
                       rb_op_ptr_ptr, rb.ptr)
    catch err
        err.code == Int(Error.ITEROVER) && return nothing
        rethrow(err)
    end
    return unsafe_load(convert(Ptr{RebaseOperation}, rb_op_ptr_ptr[]), 1)
end

function commit(rb::GitRebase, sig::GitSignature)
    oid_ptr = Ref(Oid())
    @check ccall((:git_rebase_commit, :libgit2), Cint,
                  (Ptr{Oid}, Ptr{Void}, Ptr{SignatureStruct}, Ptr{SignatureStruct}, Ptr{UInt8}, Ptr{UInt8}),
                   oid_ptr, rb.ptr, C_NULL, sig.ptr, C_NULL, C_NULL)
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
