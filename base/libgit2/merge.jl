# This file is a part of Julia. License is MIT: http://julialang.org/license

function GitAnnotated(repo::GitRepo, commit_id::GitHash)
    ann_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_annotated_commit_lookup, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{GitHash}),
                   ann_ptr_ptr, repo.ptr, Ref(commit_id))
    return GitAnnotated(repo, ann_ptr_ptr[])
end

function GitAnnotated(repo::GitRepo, ref::GitReference)
    ann_ref_ref = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_annotated_commit_from_ref, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}),
                   ann_ref_ref, repo.ptr, ref.ptr)
    return GitAnnotated(repo, ann_ref_ref[])
end

function GitAnnotated(repo::GitRepo, fh::FetchHead)
    ann_ref_ref = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_annotated_commit_from_fetchhead, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Cstring, Ptr{GitHash}),
                   ann_ref_ref, repo.ptr, fh.name, fh.url, Ref(fh.oid))
    return GitAnnotated(repo, ann_ref_ref[])
end

function GitAnnotated(repo::GitRepo, comittish::AbstractString)
    obj = GitObject(repo, comittish)
    cmt = peel(GitCommit, obj)
    return GitAnnotated(repo, GitHash(cmt))
end

function GitHash(ann::GitAnnotated)
    unsafe_load(ccall((:git_annotated_commit_id, :libgit2), Ptr{GitHash}, (Ptr{Void},), ann.ptr))
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
    cmt = GitCommit(repo, GitHash(ann))

    checkout_tree(repo, cmt)
    with(head(repo)) do head_ref
        cmt_oid = GitHash(cmt)
        msg = "libgit2.merge: fastforward $(string(cmt_oid)) into $(name(head_ref))"
        new_head_ref = if reftype(head_ref) == Consts.REF_OID
            target!(head_ref, cmt_oid, msg=msg)
        else
            GitReference(repo, cmt_oid, fullname(head_ref), msg=msg)
        end
        close(new_head_ref)
    end
    return true
end

""" Merge changes into current head """
function merge!(repo::GitRepo, anns::Vector{GitAnnotated};
                merge_opts::MergeOptions = MergeOptions(),
                checkout_opts::CheckoutOptions = CheckoutOptions())
    anns_size = Csize_t(length(anns))
    @check ccall((:git_merge, :libgit2), Cint,
                  (Ptr{Void}, Ptr{Ptr{Void}}, Csize_t,
                   Ptr{MergeOptions}, Ptr{CheckoutOptions}),
                   repo.ptr, anns, anns_size,
                   Ref(merge_opts), Ref(checkout_opts))
    info("Review and commit merged changes.")
    return true
end

"""Internal implementation of merge.
Returns `true` if merge was successful, otherwise `false`
"""
function merge!(repo::GitRepo, anns::Vector{GitAnnotated}, fastforward::Bool;
                merge_opts::MergeOptions = MergeOptions(),
                checkout_opts::CheckoutOptions = CheckoutOptions())
    ma, mp = merge_analysis(repo, anns)
    if isset(ma, Cint(Consts.MERGE_ANALYSIS_UP_TO_DATE))
        return true # no merge - everything is up to date
    end

    ffpref = if fastforward
        Consts.MERGE_PREFERENCE_FASTFORWARD_ONLY
    elseif isset(mp, Cint(Consts.MERGE_PREFERENCE_NONE))
        Consts.MERGE_PREFERENCE_NONE
    elseif isset(mp, Cint(Consts.MERGE_PREFERENCE_NO_FASTFORWARD))
        Consts.MERGE_PREFERENCE_NO_FASTFORWARD
    elseif isset(mp, Cint(Consts.MERGE_PREFERENCE_FASTFORWARD_ONLY))
        Consts.MERGE_PREFERENCE_FASTFORWARD_ONLY
    else
        throw(ArgumentError("unknown merge preference: $(mp)."))
    end

    merge_result = if ffpref == Consts.MERGE_PREFERENCE_NONE
        if isset(ma, Cint(Consts.MERGE_ANALYSIS_FASTFORWARD))
            if length(anns) > 1
                warn("Unable to perform Fast-Forward merge with mith multiple merge heads.")
                false
            else
                ffmerge!(repo, anns[1])
            end
        elseif isset(ma, Cint(Consts.MERGE_ANALYSIS_NORMAL))
            merge!(repo, anns,
                   merge_opts=merge_opts,
                   checkout_opts=checkout_opts)
        end
    elseif ffpref == Consts.MERGE_PREFERENCE_FASTFORWARD_ONLY
        if isset(ma, Cint(Consts.MERGE_ANALYSIS_FASTFORWARD))
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
    elseif ffpref == Consts.MERGE_PREFERENCE_NO_FASTFORWARD
        if isset(ma, Cint(Consts.MERGE_ANALYSIS_NORMAL))
            merge!(repo, anns,
                   merge_opts=merge_opts,
                   checkout_opts=checkout_opts)
        end
    else
        throw(ArgumentError("unknown merge analysis result: $(ma)"))
    end
    return merge_result
end

function merge_base(repo::GitRepo, one::AbstractString, two::AbstractString)
    oid1_ptr = Ref(GitHash(one))
    oid2_ptr = Ref(GitHash(two))
    moid_ptr = Ref(GitHash())
    moid = try
        @check ccall((:git_merge_base, :libgit2), Cint,
                (Ptr{GitHash}, Ptr{Void}, Ptr{GitHash}, Ptr{GitHash}),
                moid_ptr, repo.ptr, oid1_ptr, oid2_ptr)
        moid_ptr[]
    catch e
        #warn("Pkg:",path(repo),"=>",e.msg)
        GitHash()
    end
    return moid
end
