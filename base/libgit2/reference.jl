# This file is a part of Julia. License is MIT: http://julialang.org/license

function GitReference(repo::GitRepo, refname::AbstractString)
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_reference_lookup, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Cstring),
                   ref_ptr_ptr, repo.ptr, refname)
    return GitReference(repo, ref_ptr_ptr[])
end

function GitReference(repo::GitRepo, obj_oid::GitHash, refname::AbstractString = Consts.HEAD_FILE;
                      force::Bool=false, msg::AbstractString="")
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_reference_create, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}, Ptr{GitHash}, Cint, Cstring),
                   ref_ptr_ptr, repo.ptr, refname, Ref(obj_oid), Cint(force),
                   isempty(msg) ? C_NULL : msg)
    return GitReference(repo, ref_ptr_ptr[])
end

"""
    LibGit2.isorphan(repo::GitRepo)

Checks if the current branch is an "orphan" branch, i.e. has no commits. The first commit
to this branch will have no parents.
"""
function isorphan(repo::GitRepo)
    r = @check ccall((:git_repository_head_unborn, :libgit2), Cint,
                     (Ptr{Void},), repo.ptr)
    r != 0
end

function head(repo::GitRepo)
    head_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_repository_head, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}), head_ptr_ptr, repo.ptr)
    return GitReference(repo, head_ptr_ptr[])
end

function shortname(ref::GitReference)
    isempty(ref) && return ""
    name_ptr = ccall((:git_reference_shorthand, :libgit2), Cstring, (Ptr{Void},), ref.ptr)
    name_ptr == C_NULL && return ""
    return unsafe_string(name_ptr)
end

function reftype(ref::GitReference)
    return ccall((:git_reference_type, :libgit2), Cint, (Ptr{Void},), ref.ptr)
end

function fullname(ref::GitReference)
    isempty(ref) && return ""
    reftype(ref) == Consts.REF_OID && return ""
    rname = ccall((:git_reference_symbolic_target, :libgit2), Cstring, (Ptr{Void},), ref.ptr)
    rname == C_NULL && return ""
    return unsafe_string(rname)
end

function name(ref::GitReference)
    isempty(ref) && return ""
    name_ptr = ccall((:git_reference_name, :libgit2), Cstring, (Ptr{Void},), ref.ptr)
    name_ptr == C_NULL && return ""
    return unsafe_string(name_ptr)
end

function branch(ref::GitReference)
    isempty(ref) && return ""
    str_ptr_ptr = Ref{Cstring}()
    @check ccall((:git_branch_name, :libgit2), Cint,
                  (Ptr{Cstring}, Ptr{Void},), str_ptr_ptr, ref.ptr)
    return unsafe_string(str_ptr_ptr[])
end

function ishead(ref::GitReference)
    isempty(ref) && return false
    err = ccall((:git_branch_is_head, :libgit2), Cint,
                  (Ptr{Void},), ref.ptr)
    return err == 1
end

function isbranch(ref::GitReference)
    isempty(ref) && return false
    err = ccall((:git_reference_is_branch, :libgit2), Cint,
                  (Ptr{Void},), ref.ptr)
    return err == 1
end

function istag(ref::GitReference)
    isempty(ref) && return false
    err = ccall((:git_reference_is_tag, :libgit2), Cint,
                  (Ptr{Void},), ref.ptr)
    return err == 1
end

function isremote(ref::GitReference)
    isempty(ref) && return false
    err = ccall((:git_reference_is_remote, :libgit2), Cint,
                  (Ptr{Void},), ref.ptr)
    return err == 1
end

function Base.show(io::IO, ref::GitReference)
    println(io, "GitReference:")
    if isremote(ref)
        println(io, "Remote with name ", name(ref))
    elseif isbranch(ref)
        println(io, "Branch with name ", name(ref))
        if ishead(ref)
            println(io, "Branch is HEAD.")
        else
            println(io, "Branch is not HEAD.")
        end
    elseif istag(ref)
        println(io, "Tag with name ", name(ref))
    end
end

"""
    peel([T,] ref::GitReference)

Recursively peel `ref` until an object of type `T` is obtained. If no `T` is provided,
then `ref` will be peeled until an object other than a `GitTag` is obtained.

- A `GitTag` will be peeled to the object it references.
- A `GitCommit` will be peeled to a `GitTree`.
"""
function peel{T<:GitObject}(::Type{T}, ref::GitReference)
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_reference_peel, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Cint), obj_ptr_ptr, ref.ptr, Consts.OBJECT(T))
    return T(ref.repo, obj_ptr_ptr[])
end
peel(ref::GitReference) = peel(GitObject, ref)

function ref_list(repo::GitRepo)
    sa_ref = Ref(StrArrayStruct())
    @check ccall((:git_reference_list, :libgit2), Cint,
                      (Ptr{StrArrayStruct}, Ptr{Void}), sa_ref, repo.ptr)
    res = convert(Vector{String}, sa_ref[])
    free(sa_ref)
    res
end

function create_branch(repo::GitRepo,
                       bname::AbstractString,
                       commit_obj::GitCommit;
                       force::Bool=false)
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_branch_create, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Cstring, Ptr{Void}, Cint),
                   ref_ptr_ptr, repo.ptr, bname, commit_obj.ptr, Cint(force))
    return GitReference(repo, ref_ptr_ptr[])
end

function delete_branch(branch::GitReference)
    @check ccall((:git_branch_delete, :libgit2), Cint, (Ptr{Void},), branch.ptr)
end

function head!(repo::GitRepo, ref::GitReference)
    ref_name = name(ref)
    @check ccall((:git_repository_set_head, :libgit2), Cint,
                  (Ptr{Void}, Cstring), repo.ptr, ref_name)
    return ref
end

"""
    lookup_branch(repo::GitRepo, branch_name::AbstractString, remote::Bool=false) -> Nullable{GitReference}

Determine if the branch specified by `branch_name` exists in the repository `repo`.
If `remote` is `true`, `repo` is assumed to be a remote git repository. Otherwise, it
is part of the local filesystem.

`lookup_branch` returns a `Nullable`, which will be null if the requested branch does
not exist yet. If the branch does exist, the `Nullable` contains a `GitReference` to
the branch.
"""
function lookup_branch(repo::GitRepo,
                       branch_name::AbstractString,
                       remote::Bool=false)
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    branch_type = remote ? Consts.BRANCH_REMOTE : Consts.BRANCH_LOCAL
    err = ccall((:git_branch_lookup, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}, Cint),
                  ref_ptr_ptr, repo.ptr, branch_name, branch_type)
    if err != Int(Error.GIT_OK)
        if err == Int(Error.ENOTFOUND)
            return Nullable{GitReference}()
        end
        if ref_ptr_ptr[] != C_NULL
            close(GitReference(repo, ref_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    return Nullable{GitReference}(GitReference(repo, ref_ptr_ptr[]))
end

"""
    upstream(ref::GitReference) -> Nullable{GitReference}

Determine if the branch containing `ref` has a specified upstream branch.

`upstream` returns a `Nullable`, which will be null if the requested branch does
not have an upstream counterpart. If the upstream branch does exist, the `Nullable`
contains a `GitReference` to the upstream branch.
"""
function upstream(ref::GitReference)
    isempty(ref) && return nothing
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    err = ccall((:git_branch_upstream, :libgit2), Cint,
                  (Ref{Ptr{Void}}, Ptr{Void},), ref_ptr_ptr, ref.ptr)
    if err != Int(Error.GIT_OK)
        if err == Int(Error.ENOTFOUND)
            return Nullable{GitReference}()
        end
        if ref_ptr_ptr[] != C_NULL
            close(GitReference(ref.repo, ref_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    return Nullable{GitReference}(GitReference(ref.repo, ref_ptr_ptr[]))
end

repository(ref::GitReference) = ref.repo

function target!(ref::GitReference, new_oid::GitHash; msg::AbstractString="")
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_reference_set_target, :libgit2), Cint,
             (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{GitHash}, Cstring),
             ref_ptr_ptr, ref.ptr, Ref(new_oid), isempty(msg) ? C_NULL : msg)
    return GitReference(ref.repo, ref_ptr_ptr[])
end

function GitBranchIter(repo::GitRepo, flags::Cint=Cint(Consts.BRANCH_LOCAL))
    bi_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_branch_iterator_new, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}, Cint), bi_ptr, repo.ptr, flags)
    return GitBranchIter(repo, bi_ptr[])
end

function Base.start(bi::GitBranchIter)
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    btype = Ref{Cint}()
    err = ccall((:git_branch_next, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Cint}, Ptr{Void}),
                  ref_ptr_ptr, btype, bi.ptr)
    err != Int(Error.GIT_OK) && return (nothing, -1, true)
    return (GitReference(bi.repo, ref_ptr_ptr[]), btype[], false)
end

Base.done(bi::GitBranchIter, state) = Bool(state[3])

function Base.next(bi::GitBranchIter, state)
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    btype = Ref{Cint}()
    err = ccall((:git_branch_next, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Cint}, Ptr{Void}),
                  ref_ptr_ptr, btype, bi.ptr)
    err != Int(Error.GIT_OK) && return (state[1:2], (nothing, -1, true))
    return (state[1:2], (GitReference(bi.repo, ref_ptr_ptr[]), btype[], false))
end

Base.iteratorsize(::Type{GitBranchIter}) = Base.SizeUnknown()

function Base.map(f::Function, bi::GitBranchIter)
    res = nothing
    s = start(bi)
    while !done(bi, s)
        val = f(s[1:2])
        if res === nothing
            res = Array{typeof(val)}(0)
        end
        push!(res, val)
        val, s = next(bi, s)
    end
    return res
end
