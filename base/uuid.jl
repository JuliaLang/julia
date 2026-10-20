# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Represents a Universally Unique Identifier (UUID).
Can be built from one `UInt128` (all byte values), two `UInt64`, or four `UInt32`.
Conversion from a string will check the UUID validity.
"""
struct UUID
    value::UInt128
end
UUID(u::UUID) = u
UUID(u::NTuple{2, UInt64}) = UUID((UInt128(u[1]) << 64) | UInt128(u[2]))
UUID(u::NTuple{4, UInt32}) = UUID((UInt128(u[1]) << 96) | (UInt128(u[2]) << 64) |
                                  (UInt128(u[3]) << 32) | UInt128(u[4]))

function convert(::Type{NTuple{2, UInt64}}, uuid::UUID)
    bytes = uuid.value
    hi = UInt64((bytes >> 64) & 0xffffffffffffffff)
    lo = UInt64(bytes & 0xffffffffffffffff)
    return (hi, lo)
end

function convert(::Type{NTuple{4, UInt32}}, uuid::UUID)
    bytes = uuid.value
    hh = UInt32((bytes >> 96) & 0xffffffff)
    hl = UInt32((bytes >> 64) & 0xffffffff)
    lh = UInt32((bytes >> 32) & 0xffffffff)
    ll = UInt32(bytes & 0xffffffff)
    return (hh, hl, lh, ll)
end

UInt128(u::UUID) = u.value

let
    uuid_hash_seed = UInt === UInt64 ? 0xd06fa04f86f11b53 : 0x96a1f36d
    Base.hash(uuid::UUID, h::UInt) = hash(uuid_hash_seed, hash(convert(NTuple{2, UInt64}, uuid), h))
end

_crc32c(uuid::UUID, crc::UInt32=0x00000000) = _crc32c(uuid.value, crc)

# The 32 hex digits are read as four words of eight: the first group, the second and third,
# the fourth and the first four of the last, and the rest of the last.
function Parsers.parsevalue(::Type{UUID}, bytes::AbstractVector{UInt8})
    require_one_based_indexing(bytes)
    length(bytes) == 36 || return Parsers.INVALID
    @inbounds (bytes[9] == UInt8('-')) & (bytes[14] == UInt8('-')) &
              (bytes[19] == UInt8('-')) & (bytes[24] == UInt8('-')) || return Parsers.INVALID
    w1 = Parsers.load8(bytes, 1)
    w2 = Parsers.load8(bytes, 10) & 0x00000000_ffffffff | Parsers.load8(bytes, 15) << 32
    w3 = Parsers.load8(bytes, 20) & 0x00000000_ffffffff | Parsers.load8(bytes, 25) << 32
    w4 = Parsers.load8(bytes, 29)
    Parsers.ishexdigits8(w1) & Parsers.ishexdigits8(w2) & Parsers.ishexdigits8(w3) &
        Parsers.ishexdigits8(w4) || return Parsers.INVALID
    return UUID((Parsers.hexdigits8(w1) << 32 | Parsers.hexdigits8(w2),
                 Parsers.hexdigits8(w3) << 32 | Parsers.hexdigits8(w4)))
end

function tryparse(::Type{UUID}, s::AbstractString)
    bytes = codeunits(s)
    bytes isa AbstractVector{UInt8} || return tryparse(UUID, String(s))
    u = Parsers.parsevalue(UUID, bytes)
    return u isa UUID ? u : nothing
end

let
    @noinline throw_malformed_uuid(s) = throw(ArgumentError("Malformed UUID string: $(repr(s))"))
    function Base.parse(::Type{UUID}, s::AbstractString)
        uuid = tryparse(UUID, s)
        return uuid === nothing ? throw_malformed_uuid(s) : uuid
    end
end

UUID(s::AbstractString) = parse(UUID, s)

let groupings = [36:-1:25; 23:-1:20; 18:-1:15; 13:-1:10; 8:-1:1]
    global string
    function string(u::UUID)
        u = u.value
        str = Base._string_n(36)
        GC.@preserve str begin
            p = pointer(str)
            for i in groupings
                unsafe_store!(p, @inbounds(hex_chars[1 + u & 0xf]), i)
                u >>= 4
            end
            unsafe_store!(p, UInt8('-'), 9)
            unsafe_store!(p, UInt8('-'), 14)
            unsafe_store!(p, UInt8('-'), 19)
            unsafe_store!(p, UInt8('-'), 24)
        end
        return str
    end
end

print(io::IO, u::UUID) = print(io, string(u))
show(io::IO, u::UUID) = print(io, UUID, "(\"", u, "\")")

isless(a::UUID, b::UUID) = isless(a.value, b.value)

# give UUID scalar behavior in broadcasting
Base.broadcastable(x::UUID) = Ref(x)
