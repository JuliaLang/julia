# This file is a part of Julia. License is MIT: http://julialang.org/license

function GitRepo(path::AbstractString)
    repo_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    err = ccall((:git_repository_open, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Cstring), repo_ptr_ptr, path)
    if err != Int(Error.GIT_OK)
        if repo_ptr_ptr[] != C_NULL
            finalize(GitRepo(repo_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    return GitRepo(repo_ptr_ptr[])
end

function GitRepoExt(path::AbstractString, flags::Cuint = Cuint(Consts.REPOSITORY_OPEN_DEFAULT))
    separator = @static is_windows() ? ";" : ":"
    repo_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    err = ccall((:git_repository_open_ext, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Cstring, Cuint, Cstring),
                 repo_ptr_ptr, path, flags, separator)
    if err != Int(Error.GIT_OK)
        if repo_ptr_ptr[] != C_NULL
            finalize(GitRepo(repo_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    return GitRepo(repo_ptr_ptr[])
end

function cleanup(r::GitRepo)
    if r.ptr != C_NULL
        ccall((:git_repository__cleanup, :libgit2), Void, (Ptr{Void},), r.ptr)
    end
end

function init(path::AbstractString, bare::Bool=false)
    repo_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_repository_init, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Cstring, Cuint), repo_ptr_ptr, path, bare)
    return GitRepo(repo_ptr_ptr[])
end

function head_oid(repo::GitRepo)
    head_ref = head(repo)
    try
        return Oid(head_ref)
    finally
        finalize(head_ref)
    end
end

function headname(repo::GitRepo)
    with(head(repo)) do href
        if isattached(repo)
            shortname(href)
        else
            "(detached from $(string(Oid(href))[1:7]))"
        end
    end
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
            (Ptr{Ptr{Void}}, Ptr{Void}, Cstring), obj_ptr_ptr, repo.ptr, objname)
    err != 0 && return nothing
    return GitAnyObject(obj_ptr_ptr[])
end

""" Returns id of a found object """
function revparseid(repo::GitRepo, objname::AbstractString)
    obj = revparse(repo, objname)
    obj === nothing && return Oid()
    oid = Oid(obj.ptr)
    finalize(obj)
    return oid
end

function get{T <: GitObject}(::Type{T}, r::GitRepo, oid::Oid, oid_size::Int=OID_HEXSZ)
    id_ptr  = Ref(oid)
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    git_otype = getobjecttype(T)

    err = if oid_size != OID_HEXSZ
        ccall((:git_object_lookup_prefix, :libgit2), Cint,
              (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Oid}, Csize_t, Cint),
              obj_ptr_ptr, r.ptr, id_ptr, Csize_t(oid_size), git_otype)
    else
        ccall((:git_object_lookup, :libgit2), Cint,
              (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Oid}, Cint),
              obj_ptr_ptr, r.ptr, id_ptr, git_otype)
    end
    if err == Int(Error.ENOTFOUND)
        return nothing
    elseif err != Int(Error.GIT_OK)
        if obj_ptr_ptr[] != C_NULL
            finalize(GitAnyObject(obj_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    return T(obj_ptr_ptr[])
end

function get{T <: GitObject}(::Type{T}, r::GitRepo, oid::AbstractString)
    return get(T, r, Oid(oid), length(oid))
end

function gitdir(repo::GitRepo)
    return unsafe_string(ccall((:git_repository_path, :libgit2), Cstring,
                        (Ptr{Void},), repo.ptr))
end

function path(repo::GitRepo)
    rpath = gitdir(repo)
    return isbare(repo) ? rpath : splitdir(rpath[1:end-1])[1]*"/" # remove '.git' part
end

function peel(obj::GitObject, obj_type::Cint)
    peeled_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    git_otype = getobjecttype(obj_type)
    err = ccall((:git_object_peel, :libgit2), Cint,
                (Ptr{Ptr{Void}}, Ptr{Void}, Cint), peeled_ptr_ptr, obj.ptr, obj_type)
    if err == Int(Error.ENOTFOUND)
        return Oid()
    elseif err != Int(Error.GIT_OK)
        if peeled_ptr_ptr[] != C_NULL
            finalize(GitAnyObject(peeled_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    return git_otype(peeled_ptr_ptr[])
end

peel{T <: GitObject}(::Type{T}, obj::GitObject) = peel(obj, getobjecttype(T))

function checkout_tree(repo::GitRepo, obj::GitObject;
                       options::CheckoutOptions = CheckoutOptions())
    @check ccall((:git_checkout_tree, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Ptr{CheckoutOptions}),
                 repo.ptr, obj.ptr, Ref(options))
end

function checkout_index(repo::GitRepo, idx::Nullable{GitIndex} = Nullable{GitIndex}();
                        options::CheckoutOptions = CheckoutOptions())
    @check ccall((:git_checkout_index, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Ptr{CheckoutOptions}),
                 repo.ptr,
                 isnull(idx) ? C_NULL : Base.get(idx).ptr,
                 Ref(options))
end

function checkout_head(repo::GitRepo; options::CheckoutOptions = CheckoutOptions())
    @check ccall((:git_checkout_head, :libgit2), Cint,
                 (Ptr{Void}, Ptr{CheckoutOptions}),
                 repo.ptr, Ref(options))
end

"""Updates some entries, determined by the `pathspecs`, in the index from the target commit tree."""
function reset!{T<:AbstractString, S<:GitObject}(repo::GitRepo, obj::Nullable{S}, pathspecs::T...)
    with(StrArrayStruct(pathspecs...)) do sa
        @check ccall((:git_reset_default, :libgit2), Cint,
                (Ptr{Void}, Ptr{Void}, Ptr{StrArrayStruct}),
                repo.ptr,
                isnull(obj) ? C_NULL: Base.get(obj).ptr,
                Ref(sa))
    end
end

"""Sets the current head to the specified commit oid and optionally resets the index and working tree to match."""
function reset!(repo::GitRepo, obj::GitObject, mode::Cint;
               checkout_opts::CheckoutOptions = CheckoutOptions())
    @check ccall((:git_reset, :libgit2), Cint,
                 (Ptr{Void}, Ptr{Void}, Cint, Ptr{CheckoutOptions}),
                  repo.ptr, obj.ptr, mode, Ref(checkout_opts))
end

function clone(repo_url::AbstractString, repo_path::AbstractString,
               clone_opts::CloneOptions)
    clone_opts_ref = Ref(clone_opts)
    repo_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_clone, :libgit2), Cint,
            (Ptr{Ptr{Void}}, Cstring, Cstring, Ref{CloneOptions}),
            repo_ptr_ptr, repo_url, repo_path, clone_opts_ref)
    return GitRepo(repo_ptr_ptr[])
end

function fetchheads(repo::GitRepo)
    fhr = Ref{Vector{FetchHead}}(FetchHead[])
    ffcb = fetchhead_foreach_cb()
    @check ccall((:git_repository_fetchhead_foreach, :libgit2), Cint,
                  (Ptr{Void}, Ptr{Void}, Ptr{Void}),
                   repo.ptr, ffcb, fhr)
    return fhr[]
end

function remotes(repo::GitRepo)
    out = Ref(StrArrayStruct())
    @check ccall((:git_remote_list, :libgit2), Cint,
                  (Ptr{Void}, Ptr{Void}), out, repo.ptr)
    return convert(Vector{AbstractString}, out[])
end
