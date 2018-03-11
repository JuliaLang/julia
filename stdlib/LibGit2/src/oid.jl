# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    GitHash(ptr::Ptr{UInt8})

Construct a `GitHash` from a pointer to `UInt8` data containing the bytes of the
SHA-1 hash. The constructor throws an error if the pointer is null, i.e. equal to
`C_NULL`.
"""
function GitHash(ptr::Ptr{UInt8})
    if ptr == C_NULL
        throw(ArgumentError("NULL pointer passed to GitHash() constructor"))
    end
    oid_ptr = Ref(GitHash())
    ccall((:git_oid_fromraw, :libgit2), Cvoid, (Ptr{GitHash}, Ptr{UInt8}), oid_ptr, ptr)
    return oid_ptr[]
end

"""
    GitHash(id::Vector{UInt8})

Construct a `GitHash` from a vector of $OID_RAWSZ bytes.
"""
function GitHash(id::Array{UInt8,1})
    if length(id) != OID_RAWSZ
        throw(ArgumentError("invalid raw buffer size"))
    end
    return GitHash(pointer(id))
end

"""
    GitHash(id::AbstractString)

Construct a `GitHash` from a string of $OID_HEXSZ hexadecimal digits.
"""
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

"""
    GitShortHash(buf::Buffer)

Construct a `GitShortHash` from the data stored in the given [`Buffer`](@ref).
"""
function GitShortHash(buf::Buffer)
    oid_ptr = Ref{GitHash}()
    @check ccall((:git_oid_fromstrn, :libgit2), Cint,
              (Ptr{GitHash}, Ptr{UInt8}, Csize_t), oid_ptr, buf.ptr, buf.size)
    GitShortHash(oid_ptr[], buf.size)
end

"""
    GitShortHash(id::AbstractString)

Construct a `GitShortHash` from a string of at most $OID_HEXSZ hexadecimal digits.
"""
function GitShortHash(id::AbstractString)
    bstr = String(id)
    len = sizeof(bstr)
    oid_ptr = Ref{GitHash}()
    @check ccall((:git_oid_fromstrn, :libgit2), Cint,
              (Ptr{GitHash}, Ptr{UInt8}, Csize_t), oid_ptr, bstr, len)
    GitShortHash(oid_ptr[], len)
end

"""
    @githash_str -> AbstractGitHash

Construct a git hash object from the given string, returning a `GitShortHash` if
the string is shorter than $OID_HEXSZ hexadecimal digits, otherwise a `GitHash`.

# Examples
```jldoctest
julia> LibGit2.githash"d114feb74ce633"
GitShortHash("d114feb74ce633")

julia> LibGit2.githash"d114feb74ce63307afe878a5228ad014e0289a85"
GitHash("d114feb74ce63307afe878a5228ad014e0289a85")
```
"""
macro githash_str(id)
    bstr = String(id)
    if sizeof(bstr) < OID_HEXSZ
        GitShortHash(id)
    else
        GitHash(id)
    end
end


"""
    GitHash(ref::GitReference)

Get the identifier (`GitHash`) of the object referred to by the direct reference
`ref`. Note: this does not work for symbolic references; in such cases use
`GitHash(repo::GitRepo, ref_name::AbstractString)` instead.
"""
function GitHash(ref::GitReference)
    isempty(ref) && return GitHash()
    reftype(ref) != Consts.REF_OID && return GitHash()
    GC.@preserve ref begin
        oid_ptr = ccall((:git_reference_target, :libgit2), Ptr{UInt8}, (Ptr{Cvoid},), ref.ptr)
        oid_ptr == C_NULL && return GitHash()
        oid = GitHash(oid_ptr)
    end
    return oid
end


"""
    GitHash(repo::GitRepo, ref_name::AbstractString)

Get the identifier (`GitHash`) of the object referred to by reference specified by
`ref_name`.
"""
function GitHash(repo::GitRepo, ref_name::AbstractString)
    isempty(repo) && return GitHash()
    oid_ptr  = Ref(GitHash())
    @check ccall((:git_reference_name_to_id, :libgit2), Cint,
                    (Ptr{GitHash}, Ptr{Cvoid}, Cstring),
                     oid_ptr, repo.ptr, ref_name)
    return oid_ptr[]
end

"""
    GitHash(obj::GitObject)

Get the identifier (`GitHash`) of `obj`.
"""
function GitHash(obj::GitObject)
    GitHash(ccall((:git_object_id, :libgit2), Ptr{UInt8}, (Ptr{Cvoid},), obj.ptr))
end

==(obj1::GitObject, obj2::GitObject) = GitHash(obj1) == GitHash(obj2)

"""
    GitShortHash(obj::GitObject)

Get a shortened identifier (`GitShortHash`) of `obj`. The minimum length (in characters)
is determined by the `core.abbrev` config option, and will be of sufficient length to
unambiuously identify the object in the repository.
"""
function GitShortHash(obj::GitObject)
    buf_ref = Ref(Buffer())
    @check ccall((:git_object_short_id, :libgit2), Cint,
                 (Ptr{Buffer},Ptr{Cvoid}), buf_ref, obj.ptr)
    sid = GitShortHash(buf_ref[])
    free(buf_ref)
    return sid
end

"""
    raw(id::GitHash) -> Vector{UInt8}

Obtain the raw bytes of the [`GitHash`](@ref) as a vector of length $OID_RAWSZ.
"""
raw(id::GitHash) = collect(id.val)

function Base.print(io::IO, id::GitHash)
    for i in id.val
        print(io, string(i, base = 16, pad = 2))
    end
end
Base.string(id::GitShortHash) = string(id.hash)[1:id.len]

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

"""
    iszero(id::GitHash) -> Bool

Determine whether all hexadecimal digits of the given [`GitHash`](@ref) are zero.
"""
function iszero(id::GitHash)
    for i in 1:OID_RAWSZ
        id.val[i] != zero(UInt8) && return false
    end
    return true
end

Base.zero(::Type{GitHash}) = GitHash()
