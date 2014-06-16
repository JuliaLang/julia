export GitRevWalker, hide!, reset!, walk

abstract GitSortType
type SortDate <: GitSortType end 

type GitRevWalker
    repo::Repository
    ptr::Ptr{Void}

    function GitRevWalker(repo::Repository, ptr::Ptr{Void})
        @assert ptr != C_NULL
        @assert repo.ptr != C_NULL
        w = new(repo, ptr)
        finalizer(w, free!)
        return w
    end
end

GitRevWalker(r::Repository) = begin
    @assert r.ptr != C_NULL
    walk_ptr = Array(Ptr{Void}, 1)
    @check api.git_revwalk_new(walk_ptr, r.ptr)
    return GitRevWalker(r, walk_ptr[1])
end

free!(w::GitRevWalker) = begin
    if w.ptr != C_NULL
        api.git_revwalk_free(w.ptr)
        w.ptr = C_NULL
    end
end

Base.start(w::GitRevWalker) = begin
    @assert w.ptr != C_NULL
    commit_oid = Oid()
    err = api.git_revwalk_next(commit_oid.oid, w.ptr)
    if err == api.GIT_OK
        gitcommit = lookup_commit(w.repo, commit_oid)
        return (gitcommit, false)
    elseif err == api.ITEROVER
        return (nothing, true)
    end
    throw(LibGitError(err))
end

Base.done(w::GitRevWalker, state) = state[2]::Bool

Base.next(w::GitRevWalker, state) = begin
    commit_oid = Oid()
    err = api.git_revwalk_next(commit_oid.oid, w.ptr)
    if err == api.GIT_OK
        gitcommit = lookup_commit(w.repo, commit_oid)
        return (state[1], (gitcommit, false))
    elseif err == api.ITEROVER
        return (state[1], (nothing, true))
    end
    throw(LibGitError(err))
end

Base.push!(w::GitRevWalker, cid::Oid) = begin
    @assert w.ptr != C_NULL
    @check api.git_revwalk_push(w.ptr, cid.oid)
    return 
end

#TODO: this does not mimic Base's sortby! functionality so it should be renamed
Base.sortby!(w::GitRevWalker, sort_mode::Symbol; rev::Bool=false) = begin
    s = symbol_to_gitsort(sort_mode)
    rev && (s |= api.SORT_REVERSE)
    sortby!(w, s)
    return 
end

Base.sortby!(w::GitRevWalker, sort_mode::Cint) = begin
    @assert w.ptr != C_NULL
    api.git_revwalk_sorting(w.ptr, sort_mode)
    return 
end

function hide!(w::GitRevWalker, cid::Oid)
    @assert w.ptr != C_NULL
    @check api.git_revwalk_hide(w.ptr, cid.oid)
    return 
end

function reset!(w::GitRevWalker)
    @assert w.ptr != C_NULL
    api.git_revwalk_reset(w.ptr)
    return 
end

function symbol_to_gitsort(s::Symbol)
    s == :none && return api.SORT_NONE
    s == :topo && return api.SORT_TOPOLOGICAL
    s == :date && return api.SORT_TIME
    error("unknown git sort flag :$s")
end

function walk(r::Repository, from::Oid, sorting=SortDate)
    walker = GitRevWalker(r)
    sortby!(walker, api.SORT_TIME)
    push!(walker, from)
    return (@task for c in walker
        produce(c)
    end)
end

# walk(repo, oid) do commit
      # do something with commit
# end
function walk(f::Function, r::Repository, from::Oid, sorting=SortDate)
    walker = GitRevWalker(r)
    sortby!(walker, api.SORT_TIME)
    push!(walker, from) 
    for c in walker
        f(c)
    end
    return 
end
