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

function GitAnnotated(repo::GitRepo, fh::FetchHead)
    ann_ref_ref = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_annotated_commit_from_fetchhead, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring, Ptr{Oid}),
                   ann_ref_ref, repo.ptr, fh.name, fh.url, fh.oid)
    return GitAnnotated(ann_ref_ref[])
end

function GitAnnotated(repo::GitRepo, comittish::AbstractString)
    obj = revparse(repo, comittish)
    try
        cmt = peel(obj, GitConst.OBJ_COMMIT)
        cmt === nothing && return nothing
        return GitAnnotated(repo, Oid(cmt))
    finally
        finalize(obj)
    end
end

function commit(ann::GitAnnotated)
    return Oid(ccall((:git_annotated_commit_id, :libgit2), Ptr{Oid}, (Ptr{Void},), ann.ptr))
end

function merge_analysis(repo::GitRepo, anns::Vector{GitAnnotated})
    analysis = Ref{Cint}(0)
    preference = Ref{Cint}(0)
    anns_ref = Ref(map(a->a.ptr, anns))
    anns_size = Csize_t(length(anns))
    @check ccall((:git_merge_analysis, :libgit2), Cint,
                  (Ptr{Cint}, Ptr{Cint}, Ptr{Void}, Ptr{Ptr{Void}}, Csize_t),
                   analysis, preference, repo.ptr, anns_ref, anns_size)
    return analysis[], preference[]
end

"""Fastforward merge changes into current head """
function ffmerge!(repo::GitRepo, ann::GitAnnotated)
    ann_cmt_oid = commit(ann)
    cmt = get(GitCommit, repo, ann_cmt_oid)
    cmt === nothing && return false # could not find commit tree
    try
        checkout_tree(repo, cmt)
        with(head(repo)) do head_ref
            cmt_oid = Oid(cmt)
            msg = "libgit2.merge: fastforward $(string(cmt_oid)) into $(name(head_ref))"
            new_head_ref = if reftype(head_ref) == GitConst.REF_OID
                target!(head_ref, cmt_oid, msg=msg)
            else
                GitReference(repo, cmt_oid, fullname(head_ref), msg=msg)
            end
            finalize(new_head_ref)
        end
    finally
        finalize(cmt)
    end
    return true
end

""" Merge changes into current head """
function merge!(repo::GitRepo, anns::Vector{GitAnnotated}, merge_opts::MergeOptions,
                checkout_opts = CheckoutOptions(checkout_strategy = GitConst.CHECKOUT_SAFE))
    anns_size = Csize_t(length(anns))
    @check ccall((:git_merge, :libgit2), Cint,
                  (Ptr{Void}, Ptr{Ptr{Void}}, Csize_t,
                   Ptr{MergeOptionsStruct}, Ptr{CheckoutOptions}),
                   repo.ptr, anns, anns_size,
                   Ref(merge_opts), Ref(checkout_opts))
    info("Review and commit merged changes.")
    return true
end

"""Internal implementation of merge.
Returns `true` if merge was successful, otherwise `false`
"""
function merge!(repo::GitRepo, anns::Vector{GitAnnotated}, fastforward::Bool, options::MergeOptions)
    ma, mp = merge_analysis(repo, anns)
    if isset(ma, Cint(GitConst.MERGE_ANALYSIS_UP_TO_DATE))
        return true # no merge - everything is up to date
    end

    ffPref = if fastforward
        GitConst.MERGE_PREFERENCE_FASTFORWARD_ONLY
    elseif isset(mp, Cint(GitConst.MERGE_PREFERENCE_NONE))
        GitConst.MERGE_PREFERENCE_NONE
    elseif isset(mp, Cint(GitConst.MERGE_PREFERENCE_NO_FASTFORWARD))
        GitConst.MERGE_PREFERENCE_NO_FASTFORWARD
    elseif isset(mp, Cint(GitConst.MERGE_PREFERENCE_FASTFORWARD_ONLY))
        GitConst.MERGE_PREFERENCE_FASTFORWARD_ONLY
    end
    if ffPref === nothing
        warn("Unknown merge preference: $(mp).")
        return false
    end

    mergeResult = if ffPref == GitConst.MERGE_PREFERENCE_NONE
        if isset(ma, Cint(GitConst.MERGE_ANALYSIS_FASTFORWARD))
            if length(anns) > 1
                warn("Unable to perform Fast-Forward merge with mith multiple merge heads.")
                false
            else
                ffmerge!(repo, anns[1])
            end
        elseif isset(ma, Cint(GitConst.MERGE_ANALYSIS_NORMAL))
            merge!(repo, anns, options)
        end
    elseif ffPref == GitConst.MERGE_PREFERENCE_FASTFORWARD_ONLY
        if isset(ma, Cint(GitConst.MERGE_ANALYSIS_FASTFORWARD))
            if length(anns) > 1
                warn("Unable to perform Fast-Forward merge with mith multiple merge heads.")
                false
            else
                ffmerge!(repo, anns[1])
            end
        else
            warn("Cannot perform fast-forward merge.")
            false
        end
    elseif ffPref == GitConst.MERGE_PREFERENCE_NO_FASTFORWARD
        if isset(ma, Cint(GitConst.MERGE_ANALYSIS_NORMAL))
            merge!(repo, anns, options)
        end
    end

    if mergeResult === nothing
        warn("Unknown merge analysis result: $(ma). Merging is not possible.")
        return false
    end

    return mergeResult
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