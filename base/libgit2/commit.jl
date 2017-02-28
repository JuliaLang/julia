# This file is a part of Julia. License is MIT: http://julialang.org/license

function message(c::GitCommit, raw::Bool=false)
    local msg_ptr::Cstring
    msg_ptr = raw? ccall((:git_commit_message_raw, :libgit2), Cstring, (Ptr{Void},), c.ptr) :
                   ccall((:git_commit_message, :libgit2), Cstring, (Ptr{Void},), c.ptr)
    if msg_ptr == C_NULL
        return nothing
    end
    return unsafe_string(msg_ptr)
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

function Base.show(io::IO, c::GitCommit)
    authstr = sprint(show, author(c))
    cmtrstr = sprint(show, committer(c))
    print(io, "Git Commit:\nCommit Author: $authstr\nCommitter: $cmtrstr\nSHA: $(GitHash(c))\nMessage:\n$(message(c))")
end

""" Wrapper around `git_commit_create` """
function commit(repo::GitRepo,
                refname::AbstractString,
                msg::AbstractString,
                author::GitSignature,
                committer::GitSignature,
                tree::GitTree,
                parents::GitCommit...)
    commit_id_ptr = Ref(GitHash())
    nparents = length(parents)
    parentptrs = Ptr{Void}[c.ptr for c in parents]
    @check ccall((:git_commit_create, :libgit2), Cint,
                 (Ptr{GitHash}, Ptr{Void}, Ptr{UInt8},
                  Ptr{SignatureStruct}, Ptr{SignatureStruct},
                  Ptr{UInt8}, Ptr{UInt8}, Ptr{Void},
                  Csize_t, Ptr{Ptr{Void}}),
                 commit_id_ptr, repo.ptr, isempty(refname) ? C_NULL : refname,
                 author.ptr, committer.ptr,
                 C_NULL, msg, tree.ptr,
                 nparents, nparents > 0 ? parentptrs : C_NULL)
    return commit_id_ptr[]
end

"""Commit changes to repository"""
function commit(repo::GitRepo, msg::AbstractString;
                refname::AbstractString=Consts.HEAD_FILE,
                author::Signature = Signature(repo),
                committer::Signature = Signature(repo),
                tree_id::GitHash = GitHash(),
                parent_ids::Vector{GitHash}=GitHash[])
    # Retrieve tree identifier
    if iszero(tree_id)
        tree_id = with(GitIndex, repo) do idx; write_tree!(idx) end
    end

    # Retrieve parents from HEAD
    if isempty(parent_ids)
        try # if throws then HEAD not found -> empty repo
            push!(parent_ids, GitHash(repo, refname))
        end
    end

    # return commit id
    commit_id  = GitHash()

    # get necessary objects
    tree = GitTree(repo, tree_id)
    auth_sig = convert(GitSignature, author)
    comm_sig = convert(GitSignature, committer)
    parents = GitCommit[]
    try
        for id in parent_ids
            push!(parents, GitCommit(repo, id))
        end
        commit_id = commit(repo, refname, msg, auth_sig, comm_sig, tree, parents...)
    finally
        for parent in parents
            close(parent)
        end
        close(tree)
        close(auth_sig)
        close(comm_sig)
    end
    return commit_id
end
