function GitRevWalker(r::GitRepo)
    w_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_revwalk_new, :libgit2), Cint,
                  (Ptr{Ptr{Void}}, Ptr{Void}), w_ptr, r.ptr)
    return GitRevWalker(w_ptr[])
end

function Base.start(w::GitRevWalker)
    id_ptr = Ref(Oid())
    err = ccall((:git_revwalk_next, :libgit2), Cint,
                (Ptr{Oid}, Ptr{Void}), id_ptr, w.ptr)
    err != Error.GIT_OK && return (nothing, true)
    return (id_ptr[], false)
end

Base.done(w::GitRevWalker, state) = Bool(state[2])

function Base.next(w::GitRevWalker, state)
    id_ptr = Ref(Oid())
    err = ccall((:git_revwalk_next, :libgit2), Cint,
                (Ptr{Oid}, Ptr{Void}), id_ptr, w.ptr)
    err != Error.GIT_OK && return (state[1], (nothing, true))
    return (state[1], (id_ptr[], false))
end

function push_head!(w::GitRevWalker)
    @check ccall((:git_revwalk_push_head, :libgit2), Cint, (Ptr{Void},), w.ptr)
    return w
end

function Base.push!(w::GitRevWalker, cid::Oid)
    @check ccall((:git_revwalk_push, :libgit2), Cint, (Ptr{Void}, Ptr{Oid}), w.ptr, Ref(cid))
    return w
end

function Base.sort!(w::GitRevWalker; by::Cint = GitConst.SORT_NONE, rev::Bool=false)
    rev && (by |= GitConst.SORT_REVERSE)
    ccall((:git_revwalk_sorting, :libgit2), Void, (Ptr{Void}, Cint), w.ptr, by)
    return w
end

function Base.map(f::Function, repo::GitRepo; oid::Oid=Oid(), by::Cint = GitConst.SORT_NONE, rev::Bool=false)
    walker = GitRevWalker(repo)
    res = nothing
    try
        sort!(walker, by=by, rev=rev)
        if iszero(oid)
            push_head!(walker)
        else
            push!(walker, oid)
        end
        s = start(walker)

        while !done(walker, s)
            val = f(s[1], repo)
            if res == nothing
                res = Array(typeof(val),0)
            end
            push!(res, val)
            val, s = next(walker, s)
        end
    finally
        finalize(walker)
    end
    return res
end