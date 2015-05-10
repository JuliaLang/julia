function shortname(ref::GitReference)
    ref == nothing && return ""

    name_ptr = ccall((:git_reference_shorthand, :libgit2), Ptr{UInt8}, (Ptr{Void},), ref.ptr)
    name_ptr == C_NULL && return ""
    return bytestring(name_ptr)
end

function fullname(ref::GitReference)
    ref == nothing && return ""

    typ = ccall((:git_reference_type, :libgit2), Cint, (Ptr{Void},), ref.ptr)
    typ == 1 && return ""
    rname = ccall((:git_reference_symbolic_target, :libgit2), Ptr{UInt8}, (Ptr{Void},), ref.ptr)
    rname == C_NULL && return ""
    return bytestring(rname)
end

function GitReference(repo::GitRepo, ref_name::AbstractString)
    ref_ptr_ptr = Ref{Ptr{Void}}(C_NULL)
    @check ccall((:git_reference_lookup, :libgit2), Cint,
                     (Ptr{Ptr{Void}}, Ptr{Void}, Ptr{UInt8}),
                     ref_ptr_ptr, repo.ptr, ref_name)
    return GitReference(ref_ptr_ptr[])
end