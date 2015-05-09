function Signature(ptr::Ptr{SignatureStruct})
    sig   = unsafe_load(ptr)::SignatureStruct
    name  = bytestring(sig.name)
    email = bytestring(sig.email)
    time   = sig.when.time
    offset = sig.when.offset
    return Signature(name, email, time, offset)
end

function Signature(name::AbstractString, email::AbstractString)
    sig_ptr_ptr = Ref{Ptr{SignatureStruct}}(C_NULL)
    err = ccall((:git_signature_now, :libgit2), Cint,
                 (Ptr{Ptr{SignatureStruct}}, Ptr{UInt8}, Ptr{UInt8}), sig_ptr_ptr, name, email)
    err != 0 && return GitError(err)
    sig_ptr = sig_ptr_ptr[]
    s = Signature(sig_ptr)
    ccall((:git_signature_free, :libgit2), Void, (Ptr{SignatureStruct},), sig_ptr)
    return s
end

function Signature(repo::GitRepo)
    sig_ptr_ptr = Ref{Ptr{SignatureStruct}}(C_NULL)
    err = ccall((:git_signature_default, libgit2), Cint,
                 (Ptr{Ptr{SignatureStruct}}, Ptr{Void}), sig_ptr_ptr, repo.ptr)
    err != 0 && return GitError(err)
    sig_ptr = sig_ptr_ptr[]
    s = Signature(sig_ptr)
    ccall((:git_signature_free, :libgit2), Void, (Ptr{SignatureStruct},), sig_ptr)
    return s
end