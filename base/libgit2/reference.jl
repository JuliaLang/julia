# This file is a part of Julia. License is MIT: http://julialang.org/license

function GitReference(repo::GitRepo, ref_name::AbstractString)
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_reference_lookup, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Cstring),
                   ref_ptr_ptr, repo.ptr, ref_name)
    return GitReference(ref_ptr_ptr[])
end

function head(repo::GitRepo)
    head_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_repository_head, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}), head_ptr_ptr, repo.ptr)
    return GitReference(head_ptr_ptr[])
end

function shortname(ref::GitReference)
    isempty(ref) && return ""
    name_ptr = ccall((:git_reference_shorthand, :libgit2), Ptr{UInt8}, (Ptr{Void},), ref.ptr)
    name_ptr == C_NULL && return ""
    return bytestring(name_ptr)
end

function reftype(ref::GitReference)
    return ccall((:git_reference_type, :libgit2), Cint, (Ptr{Void},), ref.ptr)
end

function fullname(ref::GitReference)
    isempty(ref) && return ""
    reftype(ref) == GitConst.REF_OID && return ""
    rname = ccall((:git_reference_symbolic_target, :libgit2), Ptr{UInt8}, (Ptr{Void},), ref.ptr)
    rname == C_NULL && return ""
    return bytestring(rname)
end

function name(ref::GitReference)
    isempty(ref) && return ""
    name_ptr = ccall((:git_reference_name, :libgit2), Ptr{UInt8}, (Ptr{Void},), ref.ptr)
    name_ptr == C_NULL && return ""
    return bytestring(name_ptr)
end

function branch(ref::GitReference)
    isempty(ref) && return ""
    str_ptr_ptr = Ref(LibGit2.Cstring_NULL)
    @check ccall((:git_branch_name, :libgit2), Cint,
                  (Ptr{Cstring}, Ptr{Void},), str_ptr_ptr, ref.ptr)
    return bytestring(str_ptr_ptr[])
end

function peel{T <: GitObject}(::Type{T}, ref::GitReference)
    git_otype = getobjecttype(T)
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    err = ccall((:git_reference_peel, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Cint), obj_ptr_ptr, ref.ptr, git_otype)
    if err == Error.ENOTFOUND[]
        return Oid()
    elseif err != Error.GIT_OK[]
        if obj_ptr_ptr[] != C_NULL
            finalize(GitAnyObject(obj_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    return T(obj_ptr_ptr[])
end

function create_reference(repo::GitRepo, obj_oid::Oid, refname::AbstractString = GitConst.HEAD_FILE;
                          force::Bool=false, msg::AbstractString="")
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_reference_create, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}, Ptr{Oid}, Cint, Cstring),
                   ref_ptr_ptr, repo.ptr, refname, Ref(obj_oid), Cint(force),
                   isempty(msg) ? Cstring_NULL : msg)
    return GitReference(ref_ptr_ptr[])
end

function create_branch(repo::GitRepo, commit_obj::GitCommit, bname::AbstractString;
                       force::Bool=false)
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_branch_create, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Ptr{Void}, Cint),
                   ref_ptr_ptr, repo.ptr, bname, commit_obj.ptr, Cint(force))
    return GitReference(ref_ptr_ptr[])
end

function head!(repo::GitRepo, ref::GitReference)
    ref_name = name(ref)
    @check ccall((:git_repository_set_head, :libgit2), Cint,
                  (Ptr{Void}, Cstring), repo.ptr, ref_name)
    return ref
end

function lookup_branch(repo::GitRepo, branch_name::AbstractString, remote::Bool=false)
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    branch_type = remote ? GitConst.BRANCH_REMOTE : GitConst.BRANCH_LOCAL
    err = ccall((:git_branch_lookup, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}, Cint),
                  ref_ptr_ptr, repo.ptr, branch_name, branch_type)
    if err == Error.ENOTFOUND[]
        return nothing
    elseif err != Error.GIT_OK[]
        if repo_ptr_ptr[] != C_NULL
            finalize(GitReference(ref_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    return GitReference(ref_ptr_ptr[])
end

function upstream(ref::GitReference)
    isempty(ref) && return nothing
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_branch_upstream, :libgit2), Cint,
                  (Ref{Ptr{Void}}, Ptr{Void},), ref_ptr_ptr, ref.ptr)
    return GitReference(ref_ptr_ptr[])
end

function owner(ref::GitReference)
    repo_ptr = ccall((:git_reference_owner, :libgit2), Ptr{Void},
                      (Ptr{Void},), ref.ptr)
    return GitRepo(repo_ptr)
end

function target!(ref::GitReference, new_oid::Oid; msg::AbstractString="")
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_reference_set_target, :libgit2), Cint,
             (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Oid}, Cstring),
             ref_ptr_ptr, ref.ptr, Ref(new_oid), isempty(msg) ? Cstring_NULL : msg)
    return GitReference(ref_ptr_ptr[])
end
