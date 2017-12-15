# This file is a part of Julia. License is MIT: https://julialang.org/license

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

Check if the current branch is an "orphan" branch, i.e. has no commits. The first commit
to this branch will have no parents.
"""
function isorphan(repo::GitRepo)
    r = @check ccall((:git_repository_head_unborn, :libgit2), Cint,
                     (Ptr{Void},), repo.ptr)
    r != 0
end

"""
    LibGit2.head(repo::GitRepo) -> GitReference

Return a `GitReference` to the current HEAD of `repo`.
"""
function head(repo::GitRepo)
    head_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_repository_head, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}), head_ptr_ptr, repo.ptr)
    return GitReference(repo, head_ptr_ptr[])
end

"""
    LibGit2.shortname(ref::GitReference)

Return a shortened version of the name of `ref` that's
"human-readable".

```julia-repl
julia> repo = LibGit2.GitRepo(path_to_repo);

julia> branch_ref = LibGit2.head(repo);

julia> LibGit2.name(branch_ref)
"refs/heads/master"

julia> LibGit2.shortname(branch_ref)
"master"
```
"""
function shortname(ref::GitReference)
    isempty(ref) && return ""
    Base.@gc_preserve ref begin
        name_ptr = ccall((:git_reference_shorthand, :libgit2), Cstring, (Ptr{Void},), ref.ptr)
        name_ptr == C_NULL && return ""
        name = unsafe_string(name_ptr)
    end
    return name
end

"""
    LibGit2.reftype(ref::GitReference) -> Cint

Return a `Cint` corresponding to the type of `ref`:
  * `0` if the reference is invalid
  * `1` if the reference is an object id
  * `2` if the reference is symbolic
"""
function reftype(ref::GitReference)
    return ccall((:git_reference_type, :libgit2), Cint, (Ptr{Void},), ref.ptr)
end

"""
    LibGit2.fullname(ref::GitReference)

Return the name of the reference pointed to by the
symbolic reference `ref`. If `ref` is not a symbolic
reference, return an empty string.
"""
function fullname(ref::GitReference)
    isempty(ref) && return ""
    reftype(ref) == Consts.REF_OID && return ""
    Base.@gc_preserve ref begin
        rname = ccall((:git_reference_symbolic_target, :libgit2), Cstring, (Ptr{Void},), ref.ptr)
        rname == C_NULL && return ""
        name = unsafe_string(rname)
    end
    return name
end

"""
    LibGit2.name(ref::GitReference)

Return the full name of `ref`.
"""
function name(ref::GitReference)
    isempty(ref) && return ""
    Base.@gc_preserve ref begin
        name_ptr = ccall((:git_reference_name, :libgit2), Cstring, (Ptr{Void},), ref.ptr)
        name_ptr == C_NULL && return ""
        name = unsafe_string(name_ptr)
    end
    return name
end

function branch(ref::GitReference)
    isempty(ref) && return ""
    str_ptr_ptr = Ref{Cstring}()
    Base.@gc_preserve ref begin
        @check ccall((:git_branch_name, :libgit2), Cint,
                      (Ptr{Cstring}, Ptr{Void},), str_ptr_ptr, ref.ptr)
        str = unsafe_string(str_ptr_ptr[])
    end
    return str
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
then `ref` will be peeled until an object other than a [`GitTag`](@ref) is obtained.

- A `GitTag` will be peeled to the object it references.
- A [`GitCommit`](@ref) will be peeled to a [`GitTree`](@ref).

!!! note
    Only annotated tags can be peeled to `GitTag` objects. Lightweight tags (the default)
    are references under `refs/tags/` which point directly to `GitCommit` objects.
"""
function peel(::Type{T}, ref::GitReference) where T<:GitObject
    obj_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_reference_peel, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Cint), obj_ptr_ptr, ref.ptr, Consts.OBJECT(T))
    return T(ref.owner, obj_ptr_ptr[])
end
peel(ref::GitReference) = peel(GitObject, ref)

"""
    LibGit2.ref_list(repo::GitRepo) -> Vector{String}

Get a list of all reference names in the `repo` repository.
"""
function ref_list(repo::GitRepo)
    sa_ref = Ref(StrArrayStruct())
    @check ccall((:git_reference_list, :libgit2), Cint,
                      (Ptr{StrArrayStruct}, Ptr{Void}), sa_ref, repo.ptr)
    res = convert(Vector{String}, sa_ref[])
    free(sa_ref)
    res
end

"""
    LibGit2.create_branch(repo::GitRepo, bname::AbstractString, commit_obj::GitCommit; force::Bool=false)

Create a new branch in the repository `repo` with name `bname`, which
points to commit `commit_obj` (which has to be part of `repo`). If
`force` is `true`, overwrite an existing branch named `bname` if it
exists. If `force` is `false` and a branch already exists named `bname`,
this function will throw an error.
"""
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

"""
    LibGit2.delete_branch(branch::GitReference)

Delete the branch pointed to by `branch`.
"""
function delete_branch(branch::GitReference)
    @check ccall((:git_branch_delete, :libgit2), Cint, (Ptr{Void},), branch.ptr)
end

"""
    LibGit2.head!(repo::GitRepo, ref::GitReference) -> GitReference

Set the HEAD of `repo` to the object pointed to by `ref`.
"""
function head!(repo::GitRepo, ref::GitReference)
    ref_name = name(ref)
    @check ccall((:git_repository_set_head, :libgit2), Cint,
                  (Ptr{Void}, Cstring), repo.ptr, ref_name)
    return ref
end

"""
    lookup_branch(repo::GitRepo, branch_name::AbstractString, remote::Bool=false) -> Union{GitReference, Void}

Determine if the branch specified by `branch_name` exists in the repository `repo`.
If `remote` is `true`, `repo` is assumed to be a remote git repository. Otherwise, it
is part of the local filesystem.

Return either a `GitReference` to the requested branch
if it exists, or [`nothing`](@ref) if not.
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
            return nothing
        end
        if ref_ptr_ptr[] != C_NULL
            close(GitReference(repo, ref_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    return GitReference(repo, ref_ptr_ptr[])
end

"""
    upstream(ref::GitReference) -> Union{GitReference, Void}

Determine if the branch containing `ref` has a specified upstream branch.

Return either a `GitReference` to the upstream branch if it exists,
or [`nothing`](@ref) if the requested branch does not have an upstream counterpart.
"""
function upstream(ref::GitReference)
    isempty(ref) && return nothing
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    err = ccall((:git_branch_upstream, :libgit2), Cint,
                  (Ref{Ptr{Void}}, Ptr{Void},), ref_ptr_ptr, ref.ptr)
    if err != Int(Error.GIT_OK)
        if err == Int(Error.ENOTFOUND)
            return nothing
        end
        if ref_ptr_ptr[] != C_NULL
            close(GitReference(ref.owner, ref_ptr_ptr[]))
        end
        throw(Error.GitError(err))
    end
    return GitReference(ref.owner, ref_ptr_ptr[])
end

repository(ref::GitReference) = ref.owner

function target!(ref::GitReference, new_oid::GitHash; msg::AbstractString="")
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_reference_set_target, :libgit2), Cint,
             (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{GitHash}, Cstring),
             ref_ptr_ptr, ref.ptr, Ref(new_oid), isempty(msg) ? C_NULL : msg)
    return GitReference(ref.owner, ref_ptr_ptr[])
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
    return (GitReference(bi.owner, ref_ptr_ptr[]), btype[], false)
end

Base.done(bi::GitBranchIter, state) = Bool(state[3])

function Base.next(bi::GitBranchIter, state)
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    btype = Ref{Cint}()
    err = ccall((:git_branch_next, :libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Cint}, Ptr{Void}),
                  ref_ptr_ptr, btype, bi.ptr)
    err != Int(Error.GIT_OK) && return (state[1:2], (nothing, -1, true))
    return (state[1:2], (GitReference(bi.owner, ref_ptr_ptr[]), btype[], false))
end

Base.iteratorsize(::Type{GitBranchIter}) = Base.SizeUnknown()

function Base.map(f::Function, bi::GitBranchIter)
    res = nothing
    s = start(bi)
    while !done(bi, s)
        val = f(s[1:2])
        if res === nothing
            res = Vector{typeof(val)}()
        end
        push!(res, val)
        val, s = next(bi, s)
    end
    return res
end
