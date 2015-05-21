# This file is a part of Julia. License is MIT: http://julialang.org/license

function GitAnnotated(repo::GitRepo, commit_id::Oid)
    ann_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_annotated_commit_lookup, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Oid}),
                   ann_ptr_ptr, repo.ptr, commit_id)
    return GitAnnotated(ann_ptr_ptr[])
end

function GitAnnotated(repo::GitRepo, ref::GitReference)
    ann_ref_ref = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_annotated_commit_from_ref, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Void}),
                   ann_ref_ref, repo.ptr, ref.ptr)
    return GitAnnotated(ann_ref_ref[])
end

function merge_analysis(repo::GitRepo, ann::GitAnnotated)
    analysis = Ref{Cint}(0)
    preference = Ref{Cint}(0)
    @check ccall((:git_merge_analysis, :libgit2), Cint,
                  (Ptr{Cint}, Ptr{Cint}, Ptr{Void}, Ptr{Ptr{Void}}, Csize_t),
                   analysis, preference, repo.ptr, Ref{Ptr{Void}}(ann.ptr), 1)
    return analysis[], preference[]
end

function commit(ann::GitAnnotated)
    return Oid(ccall((:git_annotated_commit_id, :libgit2), Ptr{Oid}, (Ptr{Void},), ann.ptr))
end

""" Merge changes into current head """
function merge!(repo::GitRepo; fast_forward::Bool=false)
    # get head annotated upstream reference
    with(head(repo)) do hr
        with(upstream(hr)) do hur
            with(GitAnnotated(repo, hur)) do hua
                ma, mp = merge_analysis(repo, hua)
                (ma & GitConst.MERGE_ANALYSIS_UP_TO_DATE == GitConst.MERGE_ANALYSIS_UP_TO_DATE) && return
                if (ma & GitConst.MERGE_ANALYSIS_FASTFORWARD == GitConst.MERGE_ANALYSIS_FASTFORWARD)
                    # do fastforward: checkout tree and update branch references
                    # hur_oid = Oid(hur)
                    # with(get(GitCommit, repo, hur_oid)) do cmt
                    #     checkout_tree(repo, cmt)
                    # end
                    # target!(hr, hur_oid, msg="pkg.libgit2.megre!: fastforward $(name(hur)) into $(name(hr))")
                    # head!(repo, hur, msg="--fastforward")

                    hur_oid = Oid(hur)
                    target!(hr, hur_oid, msg="pkg.libgit2.megre!: fastforward $(name(hur)) into $(name(hr))")
                    reset!(repo, hur_oid, GitConst.RESET_HARD)
                elseif (ma & GitConst.MERGE_ANALYSIS_NORMAL == GitConst.MERGE_ANALYSIS_NORMAL)
                    if fast_forward
                        warn("Fastforward merge is not possible. Abort merging.")
                        return
                    end
                    merge_opts = MergeOptionsStruct()
                    checkout_opts = CheckoutOptionsStruct(checkout_strategy = GitConst.CHECKOUT_SAFE)
                    @check ccall((:git_merge, :libgit2), Cint,
                                  (Ptr{Void}, Ptr{Ptr{Void}}, Csize_t, Ptr{MergeOptionsStruct}, Ptr{CheckoutOptionsStruct}),
                                   repo.ptr, Ref{Ptr{Void}}(hua.ptr), 1, Ref(merge_opts), Ref(checkout_opts))
                    cleanup(repo)
                    info("Review and commit merged changes.")
                else
                    warn("Unknown merge analysis result. Merging is not possible.")
                end
            end
        end
    end
end
