function GitReference(repo::GitRepo, ref_name::AbstractString)
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_reference_lookup, :libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}),
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

function fullname(ref::GitReference)
    isempty(ref) && return ""
    typ = ccall((:git_reference_type, :libgit2), Cint, (Ptr{Void},), ref.ptr)
    typ == 1 && return ""
    rname = ccall((:git_reference_symbolic_target, :libgit2), Ptr{UInt8}, (Ptr{Void},), ref.ptr)
    rname == C_NULL && return ""
    return bytestring(rname)
end

function branch(ref::GitReference)
    isempty(ref) && return ""
    str_ptr_ptr = Ref{Ptr{UInt8}}(C_NULL)
    @check ccall((:git_branch_name, :libgit2), Cint,
                 (Ref{Ptr{UInt8}}, Ptr{Void},), str_ptr_ptr, ref.ptr)
    return bytestring(str_ptr_ptr[])
end

function peel(ref::GitReference, obj_type::Cint)
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    err = ccall((:git_reference_peel, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cint), obj_ptr_ptr, ref.ptr, obj_type)
    if err == Error.ENOTFOUND
        return Oid()
    elseif err != Error.GIT_OK
        if obj_ptr_ptr[] != C_NULL
            finalize(GitAnyObject(obj_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    id = Oid(obj_ptr_ptr[])
    finalize(GitAnyObject(obj_ptr_ptr[]))
    return id
end

function create_reference(repo::GitRepo, obj_oid::Oid, refname::AbstractString = GitConst.HEAD_FILE;
                          force::Bool=false, msg::AbstractString="")
    sig = default_signature(repo)
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    try
        @check ccall((:git_reference_create, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}, Ptr{Oid}, Cint, Ptr{SignatureStruct}, Ptr{UInt8}),
                  ref_ptr_ptr, repo.ptr, refname, Ref(obj_oid), Cint(force),
                  sig.ptr, isempty(msg) ? Ptr{UInt8}(C_NULL) : msg)
    catch err
        rethrow(err)
    finally
        finalize(sig)
    end
    return GitReference(ref_ptr_ptr[])
end

function create_branch(repo::GitRepo, commit_obj::GitCommit, bname::AbstractString;
                          force::Bool=false, msg::AbstractString="")
    sig = default_signature(repo)
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    try
        @check ccall((:git_branch_create, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}, Ptr{Void}, Cint, Ptr{SignatureStruct}, Ptr{UInt8}),
                  ref_ptr_ptr, repo.ptr, bname, commit_obj.ptr, Cint(force),
                  sig.ptr, isempty(msg) ? Ptr{UInt8}(C_NULL) : msg)
    catch err
        rethrow(err)
    finally
        finalize(sig)
    end
    return GitReference(ref_ptr_ptr[])
end