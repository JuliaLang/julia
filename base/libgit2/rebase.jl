# This file is a part of Julia. License is MIT: https://julialang.org/license

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

"""
    current(rb::GitRebase) -> Csize_t

Return the index of the current [`RebaseOperation`](@ref). If no operation has
yet been applied (because the [`GitRebase`](@ref) has been constructed but `next`
has not yet been called or iteration over `rb` has not yet begun), return
`GIT_REBASE_NO_OPERATION`, which is equal to `typemax(Csize_t)`.
"""
function current(rb::GitRebase)
    return ccall((:git_rebase_operation_current, :libgit2), Csize_t, (Ptr{Void},), rb.ptr)
end

function Base.getindex(rb::GitRebase, i::Integer)
    if !(1 <= i <= count(rb))
        throw(BoundsError(rb, (i,)))
    end
    rb_op_ptr = ccall((:git_rebase_operation_byindex, :libgit2),
                      Ptr{RebaseOperation},
                      (Ptr{Void}, Csize_t), rb.ptr, i-1)
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

function Base.show(io::IO, rb::GitRebase)
    println(io, "GitRebase:")
    println(io, "Number: ", count(rb))
    println(io, "Currently performing operation: ", current(rb)+1)
end

"""
    LibGit2.commit(rb::GitRebase, sig::GitSignature)

Commit the current patch to the rebase `rb`, using `sig` as the committer. Is silent if
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

"""
    abort(rb::GitRebase) -> Csize_t

Cancel the in-progress rebase, undoing all changes made so far and returning
the parent repository of `rb` and its working directory to their state before
the rebase was initiated. Returns `0` if the abort is successful,
`LibGit2.Error.ENOTFOUND` if no rebase is in progress (for example, if the
rebase had completed), and `-1` for other errors.
"""
function abort(rb::GitRebase)
    return ccall((:git_rebase_abort, :libgit2), Csize_t,
                      (Ptr{Void},), rb.ptr)
end

"""
    finish(rb::GitRebase, sig::GitSignature) -> Csize_t

Complete the rebase described by `rb`. `sig` is a [`GitSignature`](@ref)
to specify the identity of the user finishing the rebase. Returns `0` if the
rebase finishes successfully, `-1` if there is an error.
"""
function finish(rb::GitRebase, sig::GitSignature)
    return ccall((:git_rebase_finish, :libgit2), Csize_t,
                  (Ptr{Void}, Ptr{SignatureStruct}),
                   rb.ptr, sig.ptr)
end
