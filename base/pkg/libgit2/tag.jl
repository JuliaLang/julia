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
                  (Ptr{Void}, Ptr{UInt8}, ), repo.ptr, tag)
end

function tag_create(repo::GitRepo, tag::AbstractString, commit::AbstractString;
                    msg::AbstractString = "",
                    force::Bool = false)
    oid_ptr = Ref(Oid())
    with(get(GitCommit, repo, commit)) do commit_obj
        with(default_signature(repo)) do sig
            @check ccall((:git_tag_create, :libgit2), Cint,
                 (Ptr{Oid}, Ptr{Void}, Ptr{UInt8}, Ptr{Void}, Ptr{SignatureStruct}, Ptr{UInt8}, Cint),
                  oid_ptr, repo.ptr, tag, commit_obj.ptr, sig.ptr, msg, Cint(force))
        end
    end
    return oid_ptr[]
end
