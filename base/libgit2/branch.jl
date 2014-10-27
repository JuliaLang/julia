export tip, resolve, owner, name, canonical_name, ishead, move,
       remote_name, remote, isremote, upstream, set_upstream!

Base.delete!(b::GitBranch) = begin
    @assert b.ptr != C_NULL  
    @check api.git_branch_delete(b.ptr)
    return nothing
end

Base.(:(==))(b1::GitBranch, b2::GitBranch) = canonical_name(b1) == canonical_name(b2)
Base.isequal(b1::GitBranch, b2::GitBranch) = b1 == b2 

Base.start(b::GitBranch) = begin
    @assert b.ptr != C_NULL
end

Base.done(b::GitBranch, state) = begin
end

Base.next(b::GitBranch, state) = begin
end

function tip(b::GitBranch)
    lookup(owner(b), target(resolve(b)))
end

function owner(b::GitBranch)
    @assert b.ptr != C_NULL
    repo_ptr = api.git_reference_owner(b.ptr)
    #XXX: do not free repository handle
    return Repository(repo_ptr, false)
end

function resolve(b::GitBranch)
    @assert b.ptr != C_NULL
    ref_ptr = Array(Ptr{Void}, 1)
    @check api.git_reference_resolve(ref_ptr, b.ptr)
    return GitReference(ref_ptr[1])
end

function name(b::GitBranch)
    @assert b.ptr != C_NULL
    name_ptr = Array(Ptr{Cchar}, 1)
    api.git_branch_name(name_ptr, b.ptr)
    ret = bytestring(name_ptr[1])
    #TODO: memory leak...
    #c_free(ptr)
    return ret
end

#TODO: this is redundant with git_reference
function canonical_name(b::GitBranch)
    @assert b.ptr != C_NULL
    return bytestring(api.git_reference_name(b.ptr))
end

function ishead(b::GitBranch)
    @assert b.ptr != C_NULL
    return bool(api.git_branch_is_head(b.ptr))
end

function isremote(b::GitBranch)
    @assert b.ptr != C_NULL
    return bool(api.git_reference_is_remote(b.ptr))
end

function move(b::GitBranch, new_name::String; 
              force::Bool=false, sig=nothing, logmsg=nothing)
    @assert b.ptr != C_NULL
    branch_ptr = Array(Ptr{Void}, 1)
    bname = bytestring(new_name)
    bmsg  = logmsg != nothing ? bytestring(logmsg) : C_NULL
    if sig != nothing
        @assert isa(sig, Signature)
        gsig = git_signature(sig)
        @check ccall((:git_branch_move, api.libgit2), Cint,
                      (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Cint,
                       Ptr{api.GitSignature}, Ptr{Cchar}),
                       branch_ptr, b.ptr, bname, force? 1:0, &gsig, bmsg)
    else
        @check ccall((:git_branch_move, api.libgit2), Cint,
                      (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Cint,
                       Ptr{api.GitSignature}, Ptr{Cchar}),
                       branch_ptr, b.ptr, bname, force? 1:0, C_NULL, bmsg)
    end
    return GitBranch(branch_ptr[1])
end

function remote_name(b::GitBranch)
    @assert b.ptr != C_NULL
    return_ref = nothing
    ref_ptr = Array(Ptr{Void}, 1)
    if isremote(b)
        ref_ptr[1] = b.ptr 
    else
        err = api.git_branch_upstream(ref_ptr, b.ptr)
        if err == api.ENOTFOUND
            return nothing
        elseif err != api.GIT_OK
            #TODO: free ref pointer??
            throw(GitError(err))
        end
    end
    refname = bytestring(api.git_reference_name(ref_ptr[1]))
    repo_ptr = api.git_reference_owner(b.ptr)
    buf = api.GitBuffer()
    err = ccall((:git_branch_remote_name, api.libgit2), Cint,
                (Ptr{api.GitBuffer}, Ptr{Void}, Ptr{Cchar}),
                 &buf, repo_ptr, refname)
    if err == api.GIT_OK
        return bytestring(buf.ptr)
        #return UTF8String(rname[1:end-1])
    elseif err != api.GIT_OK
        throw(GitError(err))
    end
end

function remote(b::GitBranch)
    @assert b.ptr != C_NULL
    rname = remote_name(b)
    if rname != nothing
        return lookup_remote(owner(b), "origin")
    end
end

function upstream(b::GitBranch)
    @assert b.ptr != C_NULL
    isremote = api.git_reference_is_remote(b.ptr)
    if bool(isremote)
        return nothing
    end
    ubranch_ptr = Array(Ptr{Void}, 1)
    err = api.git_branch_upstream(ubranch_ptr, b.ptr)
    if err == api.ENOTFOUND
        return nothing
    elseif err != api.GIT_OK
        throw(GitError(err))
    end
    return GitBranch(ubranch_ptr[1])
end

function set_upstream!(b::GitBranch, target::Nothing)
    @assert b.ptr != C_NULL
    @check api.git_branch_set_upstream(b.ptr, C_NULL)
    return b
end

function set_upstream!(b::GitBranch,
                      target::Union(GitBranch, GitReference))
    @assert b.ptr != C_NULL && target.ptr != C_NULL
    #TODO: memory leak?
    name_ptr = Array(Ptr{Cchar}, 1)
    @check api.git_branch_name(name_ptr, target.ptr)
    @check api.git_branch_set_upstream(b.ptr, name_ptr[1])
    return b
end 

function set_upstream(b::GitBranch, ustream::GitReference)
    @assert b.ptr != C_NULL && ustream.ptr != C_NULL
    @check api.git_branch_set_upstream(b.ptr, C_NULL)
end

function rename(b::GitBranch, new_name::String; 
                force::Bool=false, sig=nothing, logmsg=nothing)
    return move(b, new_name, force=force, sig=sig, logmsg=logmsg)
end 
