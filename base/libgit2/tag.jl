# This file is a part of Julia. License is MIT: http://julialang.org/license

function tag_list(repo::GitRepo)
    sa_ref = Ref(StrArrayStruct())
    @check ccall((:git_tag_list, :libgit2), Cint,
                 (Ptr{StrArrayStruct}, Ptr{Void}), sa_ref, repo.ptr)
    res = convert(Vector{String}, sa_ref[])
    free(sa_ref)
    res
end

function tag_delete(repo::GitRepo, tag::AbstractString)
    @check ccall((:git_tag_delete, :libgit2), Cint,
                  (Ptr{Void}, Cstring, ), repo.ptr, tag)
end

function tag_create(repo::GitRepo, tag::AbstractString, commit::Union{AbstractString,AbstractGitHash};
                    msg::AbstractString = "",
                    force::Bool = false,
                    sig::Signature = Signature(repo))
    oid_ptr = Ref(GitHash())
    with(GitCommit(repo, commit)) do commit_obj
        commit_obj === nothing && return oid_ptr[] # return empty oid
        with(convert(GitSignature, sig)) do git_sig
            @check ccall((:git_tag_create, :libgit2), Cint,
                 (Ptr{GitHash}, Ptr{Void}, Cstring, Ptr{Void}, Ptr{SignatureStruct}, Cstring, Cint),
                  oid_ptr, repo.ptr, tag, commit_obj.ptr, git_sig.ptr, msg, Cint(force))
        end
    end
    return oid_ptr[]
end

function name(tag::GitTag)
    str_ptr = ccall((:git_tag_name, :libgit2), Cstring, (Ptr{Void}, ), tag.ptr)
    str_ptr == C_NULL && throw(Error.GitError(Error.ERROR))
    return unsafe_string(str_ptr)
end

# should we return the actual object? i.e. git_tag_target?
"""
    LibGit2.target(tag::GitTag)

The `GitHash` of the target object of `tag`.
"""
function target(tag::GitTag)
    oid_ptr = ccall((:git_tag_target_id, :libgit2), Ptr{GitHash}, (Ptr{Void}, ), tag.ptr)
    oid_ptr == C_NULL && throw(Error.GitError(Error.ERROR))
    return unsafe_load(oid_ptr)
end

Base.show(io::IO, tag::GitTag) = print(io, "GitTag:\nTag name: $(name(tag)) target: $(target(tag))")
