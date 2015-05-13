function GitRepo(path::AbstractString)
    repo_ptr_ptr = Ptr{Void}[0]
    err = ccall((:git_repository_open, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{UInt8}), repo_ptr_ptr, path)
    if err != Error.GIT_OK
        if repo_ptr_ptr[] != C_NULL
            finalize(GitRepo(repo_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    return GitRepo(repo_ptr_ptr[])
end

function close(r::GitRepo)
    if r.ptr != C_NULL
        ccall((:git_repository__cleanup, :libgit2), Void, (Ptr{Void},), r.ptr)
    end
end

function init(path::AbstractString, bare::Cuint = Cuint(0))
    repo_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_repository_init, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{UInt8}, Cuint), repo_ptr_ptr, path, bare)
    return GitRepo(repo_ptr_ptr[])
end

function head(repo::GitRepo)
    head_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_repository_head, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}), head_ptr_ptr, repo.ptr)
    return GitReference(head_ptr_ptr[])
end

function head_oid(repo::GitRepo)
    head_ref = head(repo)
    oid = Oid(head_ref)
    finalize(head_ref)
    return oid
end

function isbare(repo::GitRepo)
    return ccall((:git_repository_is_bare, :libgit2), Cint, (Ptr{Void},), repo.ptr) == 1
end

function isattached(repo::GitRepo)
    ccall((:git_repository_head_detached, :libgit2), Cint, (Ptr{Void},), repo.ptr) != 1
end

""" Returns a found object """
function revparse(repo::GitRepo, obj::AbstractString)
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    err = ccall((:git_revparse_single, :libgit2), Cint,
            (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}), obj_ptr_ptr, repo.ptr, obj)
    err != 0 && return GitAnyObject(C_NULL)
    return GitAnyObject(obj_ptr_ptr[])
end

""" Returns id of a found object """
function revparseid(repo::GitRepo, obj::AbstractString)
    obj = revparse(repo, objname)
    isempty(obj) && return Oid()
    oid = Oid(obj.ptr)
    finalize(obj)
    return oid
end

function get{T <: GitObject}(::Type{T}, r::GitRepo, oid::Oid, prefix::Bool=false)
    id_ptr  = Ref(oid)
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)

    git_otype = if T == GitCommit
        GitConst.OBJ_COMMIT
    elseif T == GitTree
        GitConst.OBJ_TREE
    elseif T == GitAnyObject
        GitConst.OBJ_ANY
    else
        error("Type $T is not supported")
    end

    @check if prefix
        ccall((:git_object_lookup_prefix, :libgit2), Cint,
              (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Oid}, Csize_t, Cint),
              obj_ptr_ptr, r.ptr, id_ptr, len, git_otype)
    else
        ccall((:git_object_lookup, :libgit2), Cint,
              (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Oid}, Cint),
              obj_ptr_ptr, r.ptr, id_ptr, git_otype)
    end
    return T(obj_ptr_ptr[])
end

function get{T <: GitObject}(::Type{T}, r::GitRepo, oid::AbstractString)
    return get(T, r, Oid(oid), length(oid) != OID_HEXSZ)
end

function path(repo::GitRepo)
    return bytestring(ccall((:git_repository_path, :libgit2), Ptr{UInt8},
                            (Ptr{Void},), repo.ptr))
end

function checkout(repo::GitRepo, spec::AbstractString)
    try
        obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
        @check ccall((:git_revparse_single, :libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}),
                      obj_ptr_ptr, repo.ptr, spec)
        obj = GitAnyObject(obj_ptr_ptr[])
        try
            checkout_tree(repo, obj)
        catch err
            rethrow(err)
        finally
            finalize(obj)
        end
    catch err
        warn("'checkout' thrown exception: $err")
    end
end

function checkout_tree(repo::GitRepo, obj::GitAnyObject)
    @check ccall((:git_checkout_tree, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Ptr{Void}),
                 repo.ptr, obj.ptr, C_NULL)
end

function checkout_index(repo::GitRepo, idx::GitIndex)
    @check ccall((:git_checkout_index, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Ptr{Void}),
                 repo.ptr, obj.ptr, C_NULL)
end
