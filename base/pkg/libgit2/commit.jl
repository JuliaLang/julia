function message(c::GitCommit, raw::Bool=false)
    local msg_ptr::Ptr{UInt8}
    msg_ptr = raw? ccall((:git_commit_message_raw, :libgit2), Ptr{UInt8}, (Ptr{Void},), c.ptr) :
                   ccall((:git_commit_message, :libgit2), Ptr{UInt8}, (Ptr{Void},), c.ptr)
    if msg_ptr == C_NULL
        return nothing
    end
    return bytestring(msg_ptr)
end

function author(c::GitCommit)
    ptr = ccall((:git_commit_author, :libgit2), Ptr{SignatureStruct}, (Ptr{Void},), c.ptr)
    @assert ptr != C_NULL
    return Signature(ptr)
end

function committer(c::GitCommit)
    ptr = ccall((:git_commit_committer, :libgit2), Ptr{SignatureStruct}, (Ptr{Void},), c.ptr)
    @assert ptr != C_NULL
    return Signature(ptr)
end

""" Wrapper around `git_commit_create` """
function commit(repo::GitRepo,
                refname::AbstractString,
                msg::AbstractString,
                author::GitSignature,
                committer::GitSignature,
                tree::GitTree,
                parents::GitCommit...)
    commit_id_ptr = Ref(Oid())
    nparents = length(parents)
    parentptrs = Ptr{Void}[c.ptr for c in parents]
    @check ccall((:git_commit_create, :libgit2), Cint,
                 (Ptr{Oid}, Ptr{Void}, Ptr{Uint8},
                  Ptr{SignatureStruct}, Ptr{SignatureStruct},
                  Ptr{Uint8}, Ptr{Uint8}, Ptr{Void},
                  Csize_t, Ptr{Ptr{Void}}),
                 commit_id_ptr, repo.ptr, isempty(refname) ? C_NULL : refname,
                 author.ptr, committer.ptr,
                 C_NULL, msg, tree.ptr,
                 nparents, nparents > 0 ? parentptrs : C_NULL)
    return commit_id_ptr[]
end

"""Commit changes to repository"""
function commit(repo::GitRepo, msg::AbstractString,
                refname::AbstractString="HEAD",
                author::Signature = Signature(repo),
                committer::Signature = Signature(repo),
                tree_id::Oid = Oid(),
                parent_ids::Vector{Oid}=Oid[])
    # Retrieve tree identifier
    if iszero(tree_id)
        idx = GitIndex(repo)
        try
            tree_id = write_tree!(idx)
        catch err
            rethrow(err)
        finally
            finalize(idx)
        end
    end

    # return commit id
    commit_id  = Oid()

    # get necessary objects
    tree = get(GitTree, repo, tree_id)
    auth_sig = convert(GitSignature, author)
    comm_sig = convert(GitSignature, committer)
    parents = GitCommit[]
    try
        for parent in parents
            push!(parents, get(GitCommit, repo, parent))
        end
        commit_id = commit(repo, refname, msg, auth_sig, comm_sig, tree, parents...)
    finally
        for parent in parents
            finalize(parent)
        end
        finalize(tree)
        finalize(auth_sig)
        finalize(auth_sig)
    end
    return commit_id
end