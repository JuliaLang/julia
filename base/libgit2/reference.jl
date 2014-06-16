export ReflogEntry,
       set_target, set_symbolic_target, resolve, 
       rename, target, symbolic_target, name, shorthand,
       git_reftype, isvalid_ref, reflog, has_reflog, peel, log!

function GitReference(ptr::Ptr{Void})
    @assert ptr != C_NULL
    ty = api.git_reference_type(ptr)
    RType = ty == 1 ? Oid : Sym 
    ref = GitReference{RType}(ptr)
    finalizer(ref, free!)
    return ref
end

function isvalid_ref(ref::String)
    return api.git_reference_is_valid_name(bytestring(ref))
end

function set_symbolic_target(r::GitReference, target::String;
                             msg=nothing, sig=nothing)
    @assert r.ptr != C_NULL
    btarget = bytestring(target)
    bmsg = msg != nothing ? bytestring(msg) : C_NULL
    ref_ptr = Array(Ptr{Void}, 1)
    if sig == nothing
        @check ccall((:git_reference_set_symbolic_target, api.libgit2), Cint,
                      (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, 
                       Ptr{Void}, Ptr{Cchar}),
                       ref_ptr, r.ptr, btarget, 
                       C_NULL, bmsg)
    else
        @assert isa(sig, Signature)
        gsig = git_signature(sig)
        @check ccall((:git_reference_set_symbolic_target, api.libgit2), Cint,
                      (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, 
                       Ptr{api.GitSignature}, Ptr{Cchar}),
                       ref_ptr, r.ptr, btarget, 
                       &gsig, bmsg)
    end
    return GitReference(ref_ptr[1])
end

function set_target(r::GitReference, id::Oid;
                    sig=nothing, logmsg=nothing)
    @assert r.ptr != C_NULL
    bmsg = logmsg != nothing ? bytestring(logmsg) : C_NULL
    ref_ptr = Array(Ptr{Void}, 1)
    if sig == nothing
        @check ccall((:git_reference_set_target, api.libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Uint8},
                      Ptr{Void}, Ptr{Cchar}),
                      ref_ptr, r.ptr, id.oid,
                      C_NULL, bmsg)
    else
        @assert isa(sig, Signature)
        gsig = git_signature(sig)
        @check ccall((:git_reference_set_target, api.libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Uint8},
                      Ptr{api.GitSignature}, Ptr{Cchar}),
                      ref_ptr, r.ptr, id.oid,
                      &gsig, bmsg)
    end
    return GitReference(ref_ptr[1])
end

function resolve(r::GitReference)
    @assert r.ptr != C_NULL
    ref_ptr = Array(Ptr{Void}, 1)
    @check api.git_reference_resolve(ref_ptr, r.ptr)
    return GitReference(ref_ptr[1])
end

function rename(r::GitReference, name::String;
                force::Bool=false, sig=nothing, logmsg=nothing)
    @assert r.ptr != C_NULL
    bname = bytestring(name)
    bmsg  = logmsg != nothing ? bytestring(logmsg) : C_NULL
    ref_ptr = Array(Ptr{Void}, 1)
    if sig != nothing
        @assert(isa(sig, Signature))
        gsig = git_signature(sig)
        @check ccall((:git_reference_rename, api.libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Cint,
                      Ptr{api.GitSignature}, Ptr{Cchar}),
                      ref_ptr, r.ptr, bname, force? 1:0, &gsig, bmsg)
    else
        @check ccall((:git_reference_rename, api.libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{Cchar}, Cint,
                      Ptr{api.GitSignature}, Ptr{Cchar}),
                      ref_ptr, r.ptr, bname, force? 1:0, C_NULL, bmsg)
    end
    return GitReference(ref_ptr[1])
end

function target(r::GitReference)
    @assert r.ptr != C_NULL
    oid_ptr = api.git_reference_target(r.ptr)
    if oid_ptr == C_NULL 
        return nothing
    end
    return Oid(oid_ptr)
end

function symbolic_target(r::GitReference)
    @assert r.ptr != C_NULL
    sym_ptr = api.git_reference_symbolic_target(r.ptr)
    if sym_ptr == C_NULL
        return ""
    end
    return bytestring(sym_ptr)
end

function name(r::GitReference)
    @assert r.ptr != C_NULL
    return bytestring(api.git_reference_name(r.ptr))
end

function shorthand(r::GitReference)
    @assert r.ptr != C_NULL
    return bytestring(api.git_reference_shorthand(r.ptr))
end

function peel{T}(r::GitReference{T})
    @assert r.ptr != C_NULL
    obj_ptr = Array(Ptr{Void}, 1)
    err = api.git_reference_peel(obj_ptr, r.ptr, api.OBJ_ANY)
    if err == api.ENOTFOUND
        return nothing
    elseif err != api.GIT_OK
        throw(LibGitError(err))
    end
    if T <: Oid && !bool(api.git_oid_cmp(obj_ptr[1], api.git_reference_target(r.ptr)))
        api.git_object_free(obj_ptr[1])
        return nothing
    else
        id = Oid(api.git_object_id(obj_ptr[1]))
        api.git_object_free(obj_ptr[1])
        return id
    end
end


type Reflog
    ptr::Ptr{Void}

    function Reflog(ptr::Ptr{Void})
        @assert ptr != C_NULL
        rl = new(ptr)
        finalizer(fl, free!)
        return rl
    end
end

free!(r::Reflog) = begin
    if r.ptr != C_NULL
        api.git_reflog_free(r.ptr)
        r.ptr = C_NULL
    end
end

type ReflogEntry
    id_old::Oid
    id_new::Oid
    committer::Signature
    message::String
end

function new_reflog_entry(entry_ptr::Ptr{Void})
    @assert entry_ptr != C_NULL
    id_old  = Oid(api.git_reflog_entry_id_old(entry_ptr))
    id_new  = Oid(api.git_reflog_entry_id_new(entry_ptr))
    sig_ptr = api.git_reflog_entry_committer(entry_ptr)
    msg_ptr = api.git_reflog_entry_message(entry_ptr)
    gsig = unsafe_load(sig_ptr)
    sig = Signature(gsig)
    # don't free gsig as its data is cleaned up
    # when git_reflog is free'd
    return ReflogEntry(id_old,
                       id_new,
                       sig,  
                       msg_ptr == C_NULL ? "" : bytestring(msg_ptr))
end

function reflog(r::GitReference)
    @assert r.ptr != C_NULL
    reflog_ptr = Array(Ptr{Void}, 1)
    @check api.git_reflog_read(reflog_ptr, api.git_reference_owner(r.ptr), name(r))
    refcount = api.git_reflog_entrycount(reflog_ptr[1])
    entries = {}
    for i in 0:refcount-1
        entry_ptr = api.git_reflog_entry_byindex(reflog_ptr[1], refcount - i - 1)
        @assert entry_ptr != C_NULL
        push!(entries, new_reflog_entry(entry_ptr))
    end
    api.git_reflog_free(reflog_ptr[1])
    return entries
end

function has_reflog(r::GitReference)
    @assert r.ptr != C_NULL
    return bool(api.git_reference_has_log(r.ptr))
end

function log!(r::GitReference, msg=nothing, committer=nothing)
    @assert r.ptr != C_NULL
    if msg == nothing
        msg = ""
    end
    bmsg = bytestring(msg)
    reflog_ptr = Array(Ptr{Void}, 1) 
    @check api.git_reflog_read(reflog_ptr,
                               api.git_reference_owner(r.ptr),
                               api.git_reference_name(r.ptr))
    
    repo_ptr = api.git_reference_owner(r.ptr)
    #TODO: memory leak with signature?
    local gsig::api.GitSignature
    if committer == nothing
        sig_ptr = Array(Ptr{api.GitSignature}, 1)
        @check api.git_signature_default(sig_ptr, repo_ptr)
        gsig = unsafe_load(sig_ptr[1])
    else
        gsig = git_signature(committer)
    end
    err = ccall((:git_reflog_append, api.libgit2), Cint,
                 (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{api.GitSignature}, Ptr{Cchar}),
                 reflog_ptr[1], api.git_reference_target(r.ptr), &gsig, bmsg)
    
    if err == api.GIT_OK
        err = api.git_reflog_write(reflog_ptr[1])
    end
    api.git_reflog_free(reflog_ptr[1])
    if err != api.GIT_OK
        throw(LibGitError(err))
    end
    return nothing
end

git_reftype{T}(r::GitReference{T}) = begin
    if T <: Sym
        return api.REF_SYMBOLIC
    elseif T <: Oid
        return api.REF_OID
    else
        error("Unknown reference type $T")
    end
end

