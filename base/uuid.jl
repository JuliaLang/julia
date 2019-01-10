# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Represents a Universally Unique Identifier (UUID).
    Can be built from one `UInt128` (all byte values), two `UInt64`, or four `UInt32`.
    Conversion from a string will check the UUID validity.
"""
struct UUID
    value::UInt128
end
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

let groupings = [1:8; 10:13; 15:18; 20:23; 25:36]
    global UUID
    function UUID(s::AbstractString)
        s = lowercase(s)

        if !occursin(r"^[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}$", s)
            throw(ArgumentError("Malformed UUID string: $(repr(s))"))
        end

        u = UInt128(0)
        for i in groupings
            u <<= 4
            d = s[i] - '0'
            u |= 0xf & (d - 39*(d > 9))
        end
        return UUID(u)
    end
end

let groupings = [36:-1:25; 23:-1:20; 18:-1:15; 13:-1:10; 8:-1:1]
    global string
    function string(u::UUID)
        u = u.value
        a = Base.StringVector(36)
        for i in groupings
            a[i] = hex_chars[1 + u & 0xf]
            u >>= 4
        end
        a[24] = a[19] = a[14] = a[9] = '-'
        return String(a)
    end
end

print(io::IO, u::UUID) = print(io, string(u))
show(io::IO, u::UUID) = print(io, "UUID(\"", u, "\")")
