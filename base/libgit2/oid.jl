export Oid, raw, iszero

type Sha1
    sha::ByteString
    
    Sha1(s::String) = begin
        if length(s) != api.OID_HEXSZ
            throw(ArgumentError("invalid sha1 string length"))
        end
        return new(bytestring(s))
    end
end

macro sha1_str(s)
    Sha1(s)
end

macro sha_str(s)
    Sha1(s)
end

immutable Oid
    oid::Array{Uint8,1}

    Oid(b::Array{Uint8, 1}) = begin
        if length(b) != api.OID_RAWSZ
            throw(ArgumentError("invalid raw buffer size"))
        end
        return new(b)
    end
end

Oid() = Oid(zeros(Uint8, api.OID_RAWSZ))

Oid(id::String) = begin
    if length(id) != api.OID_HEXSZ
        throw(ArgumentError("invalid hex size"))
    end
    bytes = hex2bytes(bytestring(id))
    return Oid(bytes)
end

Oid(ptr::Ptr{Uint8}) = begin
    @assert ptr != C_NULL
    oid = Array(Uint8, api.OID_RAWSZ)
    unsafe_copy!(pointer(oid), ptr, api.OID_RAWSZ)
    return Oid(oid)
end

Oid(id::Oid) = id
raw(id::Oid) = copy(id.oid)

Base.string(id::Oid) = hex(id)
Base.show(io::IO, id::Oid) = print(io, "Oid($(string(id)))")

Base.hex(id::Oid)  = bytes2hex(id.oid)
Base.hash(id::Oid) = hash(hex(id))

Base.cmp(id1::Oid, id2::Oid) = api.git_oid_cmp(pointer(id1.oid), pointer(id2.oid))

Base.(:(==))(id1::Oid, id2::Oid) = cmp(id1, id2) == 0
Base.isequal(id1::Oid, id2::Oid) = cmp(id1, id2) == 0
Base.isless(id1::Oid, id2::Oid)  = cmp(id1, id2) < 0

Base.copy(id::Oid) = Oid(copy(id.oid))

iszero(id::Oid) = begin
    for i in api.OID_RAWSZ
        if id.oid[i] != zero(Uint8)
            return false
        end
    end
    return true
end
