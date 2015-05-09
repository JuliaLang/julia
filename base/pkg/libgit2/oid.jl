const OID_RAWSZ = 20
const OID_HEXSZ = OID_RAWSZ * 2
const OID_MINPREFIXLEN = 4

# immutable Oid
#   id1::UInt8
#   id2::UInt8
#   ...
#   id20::UInt8
# end
@eval begin
    $(Expr(:type, false, :Oid,
        Expr(:block,
            [Expr(:(::), symbol("id$i"), :UInt8) for i=1:OID_RAWSZ]...)))
end

# default Oid constructor (all zeros)
@generated function Oid()
    return Expr(:call, :Oid, [:(zero(UInt8)) for _=1:OID_RAWSZ]...)
end
Oid(id::Oid) = id
Oid(ptr::Ptr{Oid}) = unsafe_load(ptr)::Oid

function Oid(ptr::Ptr{UInt8})
    if ptr == C_NULL
        throw(ArgumentError("NULL pointer passed to Oid() constructor"))
    end
    oid_ptr = Ref(Oid())
    ccall((:git_oid_fromraw, :libgit2), Void, (Ptr{Oid}, Ptr{UInt8}), oid_ptr, ptr)
    return oid_ptr[]
end

function Oid(id::Array{UInt8,1})
    if length(id) != OID_RAWSZ
        throw(ArgumentError("invalid raw buffer size"))
    end
    return Oid(pointer(id))
end

function Oid(id::AbstractString)
    bstr = bytestring(id)
    len = sizeof(bstr)
    oid_ptr  = Ref(Oid())
    err = if len < OID_HEXSZ
        ccall((:git_oid_fromstrn, :libgit2), Cint,
              (Ptr{Oid}, Ptr{UInt8}, Csize_t), oid_ptr, bstr, len)
    else
        ccall((:git_oid_fromstrp, :libgit2), Cint,
              (Ptr{Oid}, Ptr{Cchar}), oid_ptr, bstr)
    end
    err != 0 && return nothing
    return oid_ptr[]
end

function Oid(ref::GitReference)
    ref == nothing && return Oid()

    typ = ccall((:git_reference_type, :libgit2), Cint, (Ptr{Void},), ref.ptr)
    typ != 1 && return Oid()
    oid_ptr = ccall((:git_reference_target, :libgit2), Ptr{UInt8}, (Ptr{Void},), ref.ptr)
    oid_ptr == C_NULL && return ""
    return Oid(oid_ptr)
end

function Oid(obj::Ptr{Void})
    oid_ptr = ccall((:git_object_id, :libgit2), Ptr{UInt8}, (Ptr{Void},), obj)
    oid_ptr == C_NULL && return ""
    return Oid(oid_ptr)
end

function Oid(obj::GitObject)
    obj == nothing && return Oid()
    return Oid(obj.ptr)
end

function hex(id::Oid)
    _hexstr = zeros(UInt8, OID_HEXSZ)
    ccall((:git_oid_nfmt, :libgit2), Void,
          (Ptr{UInt8}, Csize_t, Ptr{Oid}), Ref(_hexstr), OID_HEXSZ, Ref(id))
    return bytestring(_hexstr)
end

raw(id::Oid) = hex2bytes(hex(id))

Base.string(id::Oid) = hex(id)

Base.show(io::IO, id::Oid) = print(io, "Oid($(string(id)))")

hash(id::Oid) = hash(hex(id))

cmp(id1::Oid, id2::Oid) = int(ccall((:git_oid_cmp, :libgit2), Cint,
                                         (Ptr{Oid}, Ptr{Oid}), Ref(id1), Ref(id2)))

Base.isequal(id1::Oid, id2::Oid) = cmp(id1, id2) == 0
Base.isless(id1::Oid, id2::Oid)  = cmp(id1, id2) < 0

iszero(id::Oid) = begin
    bytes = raw(id)
    for i=1:OID_RAWSZ
        if bytes[i] != zero(UInt8)
            return false
        end
    end
    return true
end

#randoid() = bytestring(rand("0123456789abcdef".data,OID_HEXSZ))
