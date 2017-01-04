# This file is a part of Julia. License is MIT: http://julialang.org/license

function tag_list(repo::GitRepo)
    with(StrArrayStruct()) do sa
        sa_ref = Ref(sa)
        @check ccall((:git_tag_list, :libgit2), Cint,
                      (Ptr{StrArrayStruct}, Ptr{Void}), sa_ref, repo.ptr)
        convert(Vector{AbstractString}, sa_ref[])
    end
end

function tag_delete(repo::GitRepo, tag::AbstractString)
    @check ccall((:git_tag_delete, :libgit2), Cint,
                  (Ptr{Void}, Cstring, ), repo.ptr, tag)
end

function tag_create(repo::GitRepo, tag::AbstractString, commit::Union{AbstractString,Oid};
                    msg::AbstractString = "",
                    force::Bool = false,
                    sig::Signature = Signature(repo))
    oid_ptr = Ref(Oid())
    with(get(GitCommit, repo, commit)) do commit_obj
        commit_obj === nothing && return oid_ptr[] # return empty oid
        with(convert(GitSignature, sig)) do git_sig
            @check ccall((:git_tag_create, :libgit2), Cint,
                 (Ptr{Oid}, Ptr{Void}, Cstring, Ptr{Void}, Ptr{SignatureStruct}, Cstring, Cint),
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

function target(tag::GitTag)
    oid_ptr = ccall((:git_tag_target_id, :libgit2), Ptr{Oid}, (Ptr{Void}, ), tag.ptr)
    oid_ptr == C_NULL && throw(Error.GitError(Error.ERROR))
    return Oid(oid_ptr)
end
