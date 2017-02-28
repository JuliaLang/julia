# This file is a part of Julia. License is MIT: http://julialang.org/license

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
    if len < OID_HEXSZ
        throw(ArgumentError("Input string is too short, use `GitShortHash` for partial hashes"))
    end
    oid_ptr = Ref{GitHash}()
    @check ccall((:git_oid_fromstrn, :libgit2), Cint,
              (Ptr{GitHash}, Ptr{UInt8}, Csize_t), oid_ptr, bstr, len)
    return oid_ptr[]
end
function GitShortHash(id::AbstractString)
    bstr = String(id)
    len = sizeof(bstr)
    oid_ptr = Ref{GitHash}()
    @check ccall((:git_oid_fromstrn, :libgit2), Cint,
              (Ptr{GitHash}, Ptr{UInt8}, Csize_t), oid_ptr, bstr, len)
    GitShortHash(oid_ptr[], len)
end

macro githash_str(id)
    bstr = String(id)
    if sizeof(bstr) < OID_HEXSZ
        GitShortHash(id)
    else
        GitHash(id)
    end
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

function GitHash(obj::GitObject)
    GitHash(ccall((:git_object_id, :libgit2), Ptr{UInt8}, (Ptr{Void},), obj.ptr))
end

Base.hex(id::GitHash) = join([hex(i,2) for i in id.val])
Base.hex(id::GitShortHash) = hex(id.hash)[1:id.len]

raw(id::GitHash) = collect(id.val)

Base.string(id::AbstractGitHash) = hex(id)

Base.show(io::IO, id::GitHash) = print(io, "GitHash(\"$(string(id))\")")
Base.show(io::IO, id::GitShortHash) = print(io, "GitShortHash(\"$(string(id))\")")

Base.hash(id::GitHash, h::UInt) = hash(id.val, h)

function Base.cmp(id1::GitHash, id2::GitHash)
    Int(ccall((:git_oid_cmp, :libgit2), Cint,
              (Ptr{GitHash}, Ptr{GitHash}),
              Ref(id1), Ref(id2)))
end
function Base.cmp(id1::GitShortHash, id2::GitShortHash)
    # shortened hashes appear at the beginning of the order, i.e.
    # 000 < 01 < 010 < 011 < 0112
    c = Int(ccall((:git_oid_ncmp, :libgit2), Cint,
                  (Ptr{GitHash}, Ptr{GitHash}, Csize_t),
                  Ref(id1.hash), Ref(id2.hash), min(id1.len, id2.len)))
    return c == 0 ? cmp(id1.len, id2.len) : c
end
Base.cmp(id1::GitHash, id2::GitShortHash) = cmp(GitShortHash(id1, OID_HEXSZ), id2)
Base.cmp(id1::GitShortHash, id2::GitHash) = cmp(id1, GitShortHash(id2, OID_HEXSZ))

==(id1::GitHash, id2::GitHash) = cmp(id1, id2) == 0
Base.isless(id1::AbstractGitHash, id2::AbstractGitHash)  = cmp(id1, id2) < 0

function iszero(id::GitHash)
    for i in 1:OID_RAWSZ
        id.val[i] != zero(UInt8) && return false
    end
    return true
end

Base.zero(::Type{GitHash}) = GitHash()
