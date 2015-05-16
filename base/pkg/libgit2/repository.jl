function GitRepo(path::AbstractString)
    repo_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
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
function revparse(repo::GitRepo, objname::AbstractString)
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    err = ccall((:git_revparse_single, :libgit2), Cint,
            (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}), obj_ptr_ptr, repo.ptr, objname)
    err != 0 && return nothing
    return GitAnyObject(obj_ptr_ptr[])
end

""" Returns id of a found object """
function revparseid(repo::GitRepo, objname::AbstractString)
    obj = revparse(repo, objname)
    obj == nothing && return Oid()
    oid = Oid(obj.ptr)
    finalize(obj)
    return oid
end

function get{T <: GitObject}(::Type{T}, r::GitRepo, oid::Oid, prefix::Bool=false)
    id_ptr  = Ref(oid)
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    git_otype = getobjecttype(T)

    err = if prefix
        ccall((:git_object_lookup_prefix, :libgit2), Cint,
              (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Oid}, Csize_t, Cint),
              obj_ptr_ptr, r.ptr, id_ptr, len, git_otype) #TODO: length
    else
        ccall((:git_object_lookup, :libgit2), Cint,
              (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Oid}, Cint),
              obj_ptr_ptr, r.ptr, id_ptr, git_otype)
    end
    if err == Error.ENOTFOUND
        return nothing
    elseif err != Error.GIT_OK
        if obj_ptr_ptr[] != C_NULL
            finalize(GitAnyObject(obj_ptr_ptr[]))
        end
        throw(Error.GitError(err))
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


function peel(obj::GitObject, obj_type::Cint)
    peeled_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    git_otype = getobjecttype(obj_type)
    err = ccall((:git_object_peel, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cint), peeled_ptr_ptr, obj.ptr, obj_type)
    if err == Error.ENOTFOUND
        return Oid()
    elseif err != Error.GIT_OK
        if peeled_ptr_ptr[] != C_NULL
            finalize(GitAnyObject(peeled_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    return git_otype(peeled_ptr_ptr[])
end

function checkout_tree(repo::GitRepo, obj::GitObject;
                       options::CheckoutOptionsStruct = CheckoutOptionsStruct())
    @check ccall((:git_checkout_tree, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Ptr{CheckoutOptionsStruct}),
                 repo.ptr, obj.ptr,
                 options == CheckoutOptionsStruct() ? C_NULL : Ref(options))
end

function checkout_index(repo::GitRepo, idx::Nullable{GitIndex} = Nullable{GitIndex}();
                        options::CheckoutOptionsStruct = CheckoutOptionsStruct())
    @check ccall((:git_checkout_index, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Ptr{CheckoutOptionsStruct}),
                 repo.ptr,
                 isnull(idx) ? C_NULL : Base.get(idx).ptr,
                 options == CheckoutOptionsStruct() ? C_NULL : Ref(options))
end

function checkout_head(repo::GitRepo; options::CheckoutOptionsStruct = CheckoutOptionsStruct())
    @check ccall((:git_checkout_head, :libgit2), Cint,
                 (Ptr{Void}, Ptr{CheckoutOptionsStruct}),
                 repo.ptr, options == CheckoutOptionsStruct() ? C_NULL : Ref(options))
end

function fetch(rmt::GitRemote)
    @check ccall((:git_remote_fetch, :libgit2), Cint,
            (Ptr{Void}, Ptr{Void}, Ptr{UInt8}),
            rmt.ptr, C_NULL, C_NULL)
end

function reset!(repo::GitRepo, obj::Nullable{GitAnyObject}, pathspecs::AbstractString...)
    sa = StrArrayStruct(pathspecs...)
    try
        @check ccall((:git_reset_default, :libgit2), Cint,
                (Ptr{Void}, Ptr{Void}, Ptr{StrArrayStruct}),
                repo.ptr,
                isnull(obj) ? C_NULL: Base.get(obj).ptr,
                Ref(sa))
    catch err
        rethrow(err)
    finally
        finalize(sa)
    end
end

function reset!(repo::GitRepo, obj::GitObject, mode::Cint;
               checkout_opts::CheckoutOptionsStruct = CheckoutOptionsStruct())
    sig = default_signature(repo)
    msg = "pkg.libgit2.reset: moving to $(string(Oid(obj)))"
    try
        @check ccall((:git_reset, :libgit2), Cint,
                     (Ptr{Void}, Ptr{Void}, Cint, Ptr{CheckoutOptionsStruct}, Ptr{SignatureStruct}, Ptr{UInt8}),
                      repo.ptr, obj.ptr, mode, Ref(checkout_opts), sig.ptr, msg)
    catch err
        rethrow(err)
    finally
        finalize(sig)
    end
    return
end

