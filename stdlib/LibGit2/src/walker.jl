# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    GitRevWalker(repo::GitRepo)

A `GitRevWalker` *walks* through the *revisions* (i.e. commits) of
a git repository `repo`. It is a collection of the commits
in the repository, and supports iteration and calls to [`map`](@ref LibGit2.map)
and [`count`](@ref LibGit2.count) (for instance, `count` could be used to determine
what percentage of commits in a repository were made by a certain
author).

```julia
cnt = LibGit2.with(LibGit2.GitRevWalker(repo)) do walker
    count((oid,repo)->(oid == commit_oid1), walker, oid=commit_oid1, by=LibGit2.Consts.SORT_TIME)
end
```
Here, `count` finds the number of commits along the walk with a certain `GitHash`.
Since the `GitHash` is unique to a commit, `cnt` will be `1`.
"""
function GitRevWalker(repo::GitRepo)
    ensure_initialized()
    w_ptr = Ref{Ptr{Cvoid}}(C_NULL)
    @check ccall((:git_revwalk_new, :libgit2), Cint,
                  (Ptr{Ptr{Cvoid}}, Ptr{Cvoid}), w_ptr, repo.ptr)
    return GitRevWalker(repo, w_ptr[])
end

function Base.iterate(w::GitRevWalker, state=nothing)
    ensure_initialized()
    id_ptr = Ref(GitHash())
    err = ccall((:git_revwalk_next, :libgit2), Cint,
                (Ptr{GitHash}, Ptr{Cvoid}), id_ptr, w.ptr)
    if err == Cint(Error.GIT_OK)
        return (id_ptr[], nothing)
    elseif err == Cint(Error.ITEROVER)
        return nothing
    else
        throw(GitError(err))
    end
end

Base.IteratorSize(::Type{GitRevWalker}) = Base.SizeUnknown()

"""
    LibGit2.push_head!(w::GitRevWalker)

Push the HEAD commit and its ancestors onto the [`GitRevWalker`](@ref)
`w`. This ensures that HEAD and all its ancestor commits will be encountered
during the walk.
"""
function push_head!(w::GitRevWalker)
    ensure_initialized()
    @check ccall((:git_revwalk_push_head, :libgit2), Cint, (Ptr{Cvoid},), w.ptr)
    return w
end

"""
    LibGit2.push!(w::GitRevWalker, cid::GitHash)

Start the [`GitRevWalker`](@ref) `walker` at commit `cid`. This function can be used
to apply a function to all commits since a certain year, by passing the first commit
of that year as `cid` and then passing the resulting `w` to [`map`](@ref LibGit2.map).
"""
function push!(w::GitRevWalker, cid::GitHash)
    ensure_initialized()
    @check ccall((:git_revwalk_push, :libgit2), Cint, (Ptr{Cvoid}, Ptr{GitHash}), w.ptr, Ref(cid))
    return w
end

function push!(w::GitRevWalker, range::AbstractString)
    ensure_initialized()
    @check ccall((:git_revwalk_push_range, :libgit2), Cint, (Ptr{Cvoid}, Ptr{UInt8}), w.ptr, range)
    return w
end

function Base.sort!(w::GitRevWalker; by::Cint = Consts.SORT_NONE, rev::Bool=false)
    ensure_initialized()
    rev && (by |= Consts.SORT_REVERSE)
    ccall((:git_revwalk_sorting, :libgit2), Cvoid, (Ptr{Cvoid}, Cint), w.ptr, by)
    return w
end

repository(w::GitRevWalker) = w.owner

"""
    LibGit2.map(f::Function, walker::GitRevWalker; oid::GitHash=GitHash(), range::AbstractString="", by::Cint=Consts.SORT_NONE, rev::Bool=false)

Using the [`GitRevWalker`](@ref) `walker` to "walk" over every commit in the repository's history,
apply `f` to each commit in the walk. The keyword arguments are:
    * `oid`: The [`GitHash`](@ref) of the commit to begin the walk from. The default is to use
      [`push_head!`](@ref) and therefore the HEAD commit and all its ancestors.
    * `range`: A range of `GitHash`s in the format `oid1..oid2`. `f` will be
      applied to all commits between the two.
    * `by`: The sorting method. The default is not to sort. Other options are to sort by
      topology (`LibGit2.Consts.SORT_TOPOLOGICAL`), to sort forwards in time
      (`LibGit2.Consts.SORT_TIME`, most ancient first) or to sort backwards in time
      (`LibGit2.Consts.SORT_REVERSE`, most recent first).
    * `rev`: Whether to reverse the sorted order (for instance, if topological sorting is used).

# Examples
```julia
oids = LibGit2.with(LibGit2.GitRevWalker(repo)) do walker
    LibGit2.map((oid, repo)->string(oid), walker, by=LibGit2.Consts.SORT_TIME)
end
```
Here, `map` visits each commit using the `GitRevWalker` and finds its `GitHash`.
"""
function map(f::Function, walker::GitRevWalker;
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

    repo = repository(walker)
    for val in (count == 0 ? walker : Iterators.take(walker, count))
        Base.push!(res, f(val, repo))
    end
    return res
end

"""
    LibGit2.count(f::Function, walker::GitRevWalker; oid::GitHash=GitHash(), by::Cint=Consts.SORT_NONE, rev::Bool=false)

Using the [`GitRevWalker`](@ref) `walker` to "walk" over every commit in the repository's history,
find the number of commits which return `true` when `f` is applied to them. The keyword arguments
are:
    * `oid`: The [`GitHash`](@ref) of the commit to begin the walk from. The default is to use
      [`push_head!`](@ref) and therefore the HEAD commit and all its ancestors.
    * `by`: The sorting method. The default is not to sort. Other options are to sort by
      topology (`LibGit2.Consts.SORT_TOPOLOGICAL`), to sort forwards in time
      (`LibGit2.Consts.SORT_TIME`, most ancient first) or to sort backwards in time
      (`LibGit2.Consts.SORT_REVERSE`, most recent first).
    * `rev`: Whether to reverse the sorted order (for instance, if topological sorting is used).

# Examples
```julia
cnt = LibGit2.with(LibGit2.GitRevWalker(repo)) do walker
    count((oid, repo)->(oid == commit_oid1), walker, oid=commit_oid1, by=LibGit2.Consts.SORT_TIME)
end
```
`count` finds the number of commits along the walk with a certain `GitHash` `commit_oid1`, starting
the walk from that commit and moving forwards in time from it. Since the `GitHash` is unique to
a commit, `cnt` will be `1`.
"""
function count(f::Function, walker::GitRevWalker;
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

    repo = repository(walker)
    for val in walker
        c += f(val, repo) == true
    end
    return c
end
