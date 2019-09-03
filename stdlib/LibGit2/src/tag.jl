# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    LibGit2.tag_list(repo::GitRepo) -> Vector{String}

Get a list of all tags in the git repository `repo`.
"""
function tag_list(repo::GitRepo)
    ensure_initialized()
    sa_ref = Ref(StrArrayStruct())
    @check ccall((:git_tag_list, :libgit2), Cint,
                 (Ptr{StrArrayStruct}, Ptr{Cvoid}), sa_ref, repo.ptr)
    res = convert(Vector{String}, sa_ref[])
    free(sa_ref)
    res
end

"""
    LibGit2.tag_delete(repo::GitRepo, tag::AbstractString)

Remove the git tag `tag` from the repository `repo`.
"""
function tag_delete(repo::GitRepo, tag::AbstractString)
    ensure_initialized()
    @check ccall((:git_tag_delete, :libgit2), Cint,
                  (Ptr{Cvoid}, Cstring), repo.ptr, tag)
end

"""
    LibGit2.tag_create(repo::GitRepo, tag::AbstractString, commit; kwargs...)

Create a new git tag `tag` (e.g. `"v0.5"`) in the repository `repo`, at
the commit `commit`.

The keyword arguments are:
  * `msg::AbstractString=""`: the message for the tag.
  * `force::Bool=false`: if `true`, existing references will be overwritten.
  * `sig::Signature=Signature(repo)`: the tagger's signature.
"""
function tag_create(repo::GitRepo, tag::AbstractString, commit::Union{AbstractString,AbstractGitHash};
                    msg::AbstractString = "",
                    force::Bool = false,
                    sig::Signature = Signature(repo))
    oid_ptr = Ref(GitHash())
    with(GitCommit(repo, commit)) do commit_obj
        commit_obj === nothing && return oid_ptr[] # return empty oid
        with(convert(GitSignature, sig)) do git_sig
            ensure_initialized()
            @check ccall((:git_tag_create, :libgit2), Cint,
                 (Ptr{GitHash}, Ptr{Cvoid}, Cstring, Ptr{Cvoid}, Ptr{SignatureStruct}, Cstring, Cint),
                  oid_ptr, repo.ptr, tag, commit_obj.ptr, git_sig.ptr, msg, Cint(force))
        end
    end
    return oid_ptr[]
end

"""
    LibGit2.name(tag::GitTag)

The name of `tag` (e.g. `"v0.5"`).
"""
function name(tag::GitTag)
    ensure_initialized()
    GC.@preserve tag begin
        str_ptr = ccall((:git_tag_name, :libgit2), Cstring, (Ptr{Cvoid},), tag.ptr)
        str_ptr == C_NULL && throw(Error.GitError(Error.ERROR))
        str = unsafe_string(str_ptr)
    end
    return str
end

# should we return the actual object? i.e. git_tag_target?
"""
    LibGit2.target(tag::GitTag)

The `GitHash` of the target object of `tag`.
"""
function target(tag::GitTag)
    ensure_initialized()
    GC.@preserve tag begin
        oid_ptr = ccall((:git_tag_target_id, :libgit2), Ptr{GitHash}, (Ptr{Cvoid},), tag.ptr)
        oid_ptr == C_NULL && throw(Error.GitError(Error.ERROR))
        str = unsafe_load(oid_ptr)
    end
    return str
end

Base.show(io::IO, tag::GitTag) = print(io, "GitTag:\nTag name: $(name(tag)) target: $(target(tag))")
