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

function commit(repo::GitRepo,
                refname::AbstractString,
                msg::AbstractString,
                author::Signature,
                committer::Signature,
                tree::GitTree,
                parents::GitCommit...)
    id_ptr = Ref(Oid())
    nparents = length(parents)
    parentptrs = Ptr{Void}[c.ptr for c in parents]
    err = ccall((:git_commit_create, :libgit2), Cint,
                 (Ptr{Oid}, Ptr{Void}, Ptr{Uint8},
                  Ptr{SignatureStruct}, Ptr{SignatureStruct},
                  Ptr{Uint8}, Ptr{Uint8}, Ptr{Void},
                  Csize_t, Ptr{Ptr{Void}}),
                 id_ptr, repo.ptr, isempty(refname) ? C_NULL : refname,
                 author, committer,
                 C_NULL, msg, tree,
                 nparents, nparents > 0 ? parentptrs : C_NULL)
    err !=0 && return GitError(err)
    return id_ptr[]
end

function commit(repo::GitRepo, msg::AbstractString,
                refname::AbstractString="",
                author::Signature = Signature(repo),
                committer::Signature = Signature(repo),
                tree_id::Oid = Oid(),
                parent_ids::Vector{Oid}=Oid[])
    tree = if isempty(tree_id)
        get(GitTree, repo)
    else
        get(GitTree, repo, tree_id)
    end
    isa(err, GitError)&& return err

    parents = [get(GitCommit, repo, parent) for parent in parents]
    return commit(repo, refname, msg, author, committer, tree, parents)
end