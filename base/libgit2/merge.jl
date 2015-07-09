# This file is a part of Julia. License is MIT: http://julialang.org/license

function GitAnnotated(repo::GitRepo, commit_id::Oid)
    ann_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_annotated_commit_lookup, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Oid}),
                   ann_ptr_ptr, repo.ptr, Ref(commit_id))
    return GitAnnotated(ann_ptr_ptr[])
end

function GitAnnotated(repo::GitRepo, ref::GitReference)
    ann_ref_ref = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_annotated_commit_from_ref, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}),
                   ann_ref_ref, repo.ptr, ref.ptr)
    return GitAnnotated(ann_ref_ref[])
end

function GitAnnotated(repo::GitRepo, comittish::AbstractString)
    obj = revparse(repo, comittish)
    try
        cmt = peel(obj, GitConst.OBJ_COMMIT)
        cmt == nothing && return nothing
        return GitAnnotated(repo, Oid(cmt))
    finally
        finalize(obj)
    end
end

function commit(ann::GitAnnotated)
    return Oid(ccall((:git_annotated_commit_id, :libgit2), Ptr{Oid}, (Ptr{Void},), ann.ptr))
end

function merge_analysis(repo::GitRepo, ann::GitAnnotated)
    analysis = Ref{Cint}(0)
    preference = Ref{Cint}(0)
    @check ccall((:git_merge_analysis, :libgit2), Cint,
                  (Ptr{Cint}, Ptr{Cint}, Ptr{Void}, Ptr{Ptr{Void}}, Csize_t),
                   analysis, preference, repo.ptr, Ref{Ptr{Void}}(ann.ptr), 1)
    return analysis[], preference[]
end

""" Merge changes into current head """
function merge!(repo::GitRepo, their_head::GitAnnotated;
               merge_opts = MergeOptionsStruct(),
               checkout_opts = CheckoutOptions(checkout_strategy = GitConst.CHECKOUT_SAFE))
    return @check ccall((:git_merge, :libgit2), Cint,
                         (Ptr{Void}, Ptr{Ptr{Void}}, Csize_t,
                          Ptr{MergeOptionsStruct}, Ptr{CheckoutOptions}),
                          repo.ptr, Ref{Ptr{Void}}(their_head.ptr), 1,
                          Ref(merge_opts), Ref(checkout_opts))
end

function merge_base(repo::GitRepo, one::AbstractString, two::AbstractString)
    oid1_ptr = Ref(Oid(one))
    oid2_ptr = Ref(Oid(two))
    moid_ptr = Ref(Oid())
    moid = try
        @check ccall((:git_merge_base, :libgit2), Cint,
                (Ptr{Oid}, Ptr{Void}, Ptr{Oid}, Ptr{Oid}),
                moid_ptr, repo.ptr, oid1_ptr, oid2_ptr)
        moid_ptr[]
    catch e
        #warn("Pkg:",path(repo),"=>",e.msg)
        Oid()
    end
    return moid
end