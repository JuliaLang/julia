# This file is a part of Julia. License is MIT: https://julialang.org/license

function Signature(ptr::Ptr{SignatureStruct})
    sig   = unsafe_load(ptr)::SignatureStruct
    name  = unsafe_string(sig.name)
    email = unsafe_string(sig.email)
    time   = sig.when.time
    offset = sig.when.offset
    return Signature(name, email, time, offset)
end
Signature(sig::GitSignature) = Signature(sig.ptr)

function Signature(name::AbstractString, email::AbstractString)
    sig_ptr_ptr = Ref{Ptr{SignatureStruct}}(C_NULL)
    @check ccall((:git_signature_now, :libgit2), Cint,
                 (Ptr{Ptr{SignatureStruct}}, Cstring, Cstring), sig_ptr_ptr, name, email)
    sig = GitSignature(sig_ptr_ptr[])
    s = Signature(sig.ptr)
    close(sig)
    return s
end

function Signature(repo::GitRepo)
    sig = default_signature(repo)
    s = Signature(sig.ptr)
    close(sig)
    return s
end

function Base.convert(::Type{GitSignature}, sig::Signature)
    sig_ptr_ptr = Ref{Ptr{SignatureStruct}}(C_NULL)
    @check ccall((:git_signature_new, :libgit2), Cint,
                 (Ptr{Ptr{SignatureStruct}}, Cstring, Cstring, Int64, Cint),
                 sig_ptr_ptr, sig.name, sig.email, sig.time, sig.time_offset)
    return GitSignature(sig_ptr_ptr[])
end

function Base.show(io::IO, sig::Signature)
    print(io, "Name: ", sig.name, ", ")
    print(io, "Email: ", sig.email, ", ")
    print(io, "Time: ", Dates.unix2datetime(sig.time + 60*sig.time_offset))
    @printf(io, "%+03i:%02i", divrem(sig.time_offset, 60)...)
end

"""Return signature object. Free it after use."""
function default_signature(repo::GitRepo)
    sig_ptr_ptr = Ref{Ptr{SignatureStruct}}(C_NULL)
    @check ccall((:git_signature_default, :libgit2), Cint,
                 (Ptr{Ptr{SignatureStruct}}, Ptr{Void}), sig_ptr_ptr, repo.ptr)
    return GitSignature(sig_ptr_ptr[])
end
