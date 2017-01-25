# This file is a part of Julia. License is MIT: http://julialang.org/license

GitHash(id::GitHash) = id
GitHash(ptr::Ptr{GitHash}) = unsafe_load(ptr)::GitHash

function GitHash(ptr::Ptr{UInt8})
    if ptr == C_NULL
        throw(ArgumentError("NULL pointer passed to GitHash() constructor"))
    end
    oid_ptr = Ref(GitHash())
    ccall((:git_oid_fromraw, :libgit2), Void, (Ptr{GitHash}, Ptr{UInt8}), oid_ptr, ptr)
    return oid_ptr[]
end

function GitHash(id::Array{UInt8,1})
    if length(id) != OID_RAWSZ
        throw(ArgumentError("invalid raw buffer size"))
    end
    return GitHash(pointer(id))
end

function GitHash(id::AbstractString)
    bstr = String(id)
    len = sizeof(bstr)
    oid_ptr  = Ref(GitHash())
    err = if len < OID_HEXSZ
        ccall((:git_oid_fromstrn, :libgit2), Cint,
              (Ptr{GitHash}, Ptr{UInt8}, Csize_t), oid_ptr, bstr, len)
    else
        ccall((:git_oid_fromstrp, :libgit2), Cint,
              (Ptr{GitHash}, Cstring), oid_ptr, bstr)
    end
    err != 0 && return GitHash()
    return oid_ptr[]
end

function GitHash(ref::GitReference)
    isempty(ref) && return GitHash()
    reftype(ref) != Consts.REF_OID && return GitHash()
    oid_ptr = ccall((:git_reference_target, :libgit2), Ptr{UInt8}, (Ptr{Void},), ref.ptr)
    oid_ptr == C_NULL && return GitHash()
    return GitHash(oid_ptr)
end

function GitHash(repo::GitRepo, ref_name::AbstractString)
    isempty(repo) && return GitHash()
    oid_ptr  = Ref(GitHash())
    @check ccall((:git_reference_name_to_id, :libgit2), Cint,
                    (Ptr{GitHash}, Ptr{Void}, Cstring),
                     oid_ptr, repo.ptr, ref_name)
    return oid_ptr[]
end

function GitHash(obj::Ptr{Void})
    oid_ptr = ccall((:git_object_id, :libgit2), Ptr{UInt8}, (Ptr{Void},), obj)
    oid_ptr == C_NULL && return GitHash()
    return GitHash(oid_ptr)
end

function GitHash{T<:GitObject}(obj::T)
    obj === nothing && return GitHash()
    return GitHash(obj.ptr)
end

Base.hex(id::GitHash) = join([hex(i,2) for i in id.val])

raw(id::GitHash) = collect(id.val)

Base.string(id::GitHash) = hex(id)

Base.show(io::IO, id::GitHash) = print(io, "GitHash($(string(id)))")

Base.hash(id::GitHash, h::UInt) = hash(id.val, h)

cmp(id1::GitHash, id2::GitHash) = Int(ccall((:git_oid_cmp, :libgit2), Cint,
                                    (Ptr{GitHash}, Ptr{GitHash}), Ref(id1), Ref(id2)))

==(id1::GitHash, id2::GitHash) = cmp(id1, id2) == 0
Base.isless(id1::GitHash, id2::GitHash)  = cmp(id1, id2) < 0

function iszero(id::GitHash)
    for i in 1:OID_RAWSZ
        id.val[i] != zero(UInt8) && return false
    end
    return true
end

Base.zero(::Type{GitHash}) = GitHash()
