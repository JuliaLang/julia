# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    GitRevWalker(repo::GitRepo)

A `GitRevWalker` *walks* through the *revisions* (i.e. commits) of
a git repository `repo`. It is a collection of the commits
in the repository, and supports iteration and calls to [`map`](@ref)
and [`count`](@ref) (for instance, `count` could be used to determine
what percentage of commits in a repository were made by a certain
author).

# Examples
```julia
oids = LibGit2.with(LibGit2.GitRevWalker(repo)) do walker
    LibGit2.map((oid,repo)->string(oid), walker, by = LibGit2.Consts.SORT_TIME)
end
```
Here, `map` visits each commit using the `GitRevWalker` and finds its [`GitHash`](@ref).

```julia
cnt = LibGit2.with(LibGit2.GitRevWalker(repo)) do walker
    count((oid,repo)->(oid == commit_oid1), walker, oid=commit_oid1, by=LibGit2.Consts.SORT_TIME)
end
```
Here, `count` finds the number of commits along the walk with a certain `GitHash`.
Since the `GitHash` is unique to a commit, `cnt` will be `1`.
"""
function GitRevWalker(repo::GitRepo)
    w_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_revwalk_new, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}), w_ptr, repo.ptr)
    return GitRevWalker(repo, w_ptr[])
end

function Base.start(w::GitRevWalker)
    id_ptr = Ref(GitHash())
    err = ccall((:git_revwalk_next, :libgit2), Cint,
                (Ptr{GitHash}, Ptr{Void}), id_ptr, w.ptr)
    err != Int(Error.GIT_OK) && return (nothing, true)
    return (id_ptr[], false)
end

Base.done(w::GitRevWalker, state) = Bool(state[2])

function Base.next(w::GitRevWalker, state)
    id_ptr = Ref(GitHash())
    err = ccall((:git_revwalk_next, :libgit2), Cint,
                (Ptr{GitHash}, Ptr{Void}), id_ptr, w.ptr)
    err != Int(Error.GIT_OK) && return (state[1], (nothing, true))
    return (state[1], (id_ptr[], false))
end

Base.iteratorsize(::Type{GitRevWalker}) = Base.SizeUnknown()

"""
    push_head!(w::GitRevWalker)

Push the HEAD commit and its ancestors onto the [`GitRevWalker`](@ref)
`w`. This ensures that HEAD and its ancestor commits will be encountered
during the walk.
"""
function push_head!(w::GitRevWalker)
    @check ccall((:git_revwalk_push_head, :libgit2), Cint, (Ptr{Void},), w.ptr)
    return w
end

function Base.push!(w::GitRevWalker, cid::GitHash)
    @check ccall((:git_revwalk_push, :libgit2), Cint, (Ptr{Void}, Ptr{GitHash}), w.ptr, Ref(cid))
    return w
end

function Base.push!(w::GitRevWalker, range::AbstractString)
    @check ccall((:git_revwalk_push_range, :libgit2), Cint, (Ptr{Void}, Ptr{UInt8}), w.ptr, range)
    return w
end

function Base.sort!(w::GitRevWalker; by::Cint = Consts.SORT_NONE, rev::Bool=false)
    rev && (by |= Consts.SORT_REVERSE)
    ccall((:git_revwalk_sorting, :libgit2), Void, (Ptr{Void}, Cint), w.ptr, by)
    return w
end

repository(w::GitRevWalker) = w.owner

function Base.map(f::Function, walker::GitRevWalker;
                  oid::GitHash=GitHash(),
                  range::AbstractString="",
                  by::Cint = Consts.SORT_NONE,
                  rev::Bool=false,
                  count::Int=0)
    res = []
    sort!(walker, by=by, rev=rev)
    if !iszero(oid)
        push!(walker, oid)
    elseif !isempty(range)
        push!(walker, range)
    else
        push_head!(walker)
    end
    s = start(walker)

    c = 0
    repo = repository(walker)
    while !done(walker, s)
        val = f(s[1], repo)
        push!(res, val)
        val, s = next(walker, s)
        c +=1
        count == c && break
    end
    return res
end

function Base.count(f::Function, walker::GitRevWalker;
                  oid::GitHash=GitHash(),
                  by::Cint = Consts.SORT_NONE,
                  rev::Bool=false)
    c = 0
    sort!(walker, by=by, rev=rev)
    if !iszero(oid)
        push!(walker, oid)
    else
        push_head!(walker)
    end
    s = start(walker)

    repo = repository(walker)
    while !done(walker, s)
        val = f(s[1], repo)
        _, s = next(walker, s)
        c += (val == true)
    end
    return c
end
