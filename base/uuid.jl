# This file is a part of Julia. License is MIT: https://julialang.org/license

struct UUID
    value::UInt128
end
UUID(u::NTuple{2, UInt64}) = UUID((UInt128(u[1]) << 64) | UInt128(u[2]))
UUID(u::NTuple{4, UInt32}) = UUID((UInt128(u[1]) << 96) | (UInt128(u[2]) << 64) |
                                  (UInt128(u[3]) << 32) | UInt128(u[4]))

function convert(::Type{NTuple{2, UInt64}}, uuid::UUID)
    uuid = uuid.value
    hi = UInt64((uuid >> 64) & 0xffffffffffffffff)
    lo = UInt64(uuid & 0xffffffffffffffff)
    return (hi, lo)
end

function convert(::Type{NTuple{4, UInt32}}, uuid::UUID)
    uuid = uuid.value
    hh = UInt32((uuid >> 96) & 0xffffffff)
    hl = UInt32((uuid >> 64) & 0xffffffff)
    lh = UInt32((uuid >> 32) & 0xffffffff)
    ll = UInt32(uuid & 0xffffffff)
    return (hh, hl, lh, ll)
end

# TODO: update documentation for new location
"""
    uuid_version(u::UUID) -> Int

Inspects the given UUID and returns its version
(see [RFC 4122](https://www.ietf.org/rfc/rfc4122)).

# Examples
```jldoctest
julia> rng = MersenneTwister(1234);

julia> Random.uuid_version(Random.uuid4(rng))
4
```
"""
uuid_version(u::UUID) = Int((u.value >> 76) & 0xf)

UInt128(u::UUID) = u.value

let groupings = [1:8; 10:13; 15:18; 20:23; 25:36]
    global UUID
    function UUID(s::AbstractString)
        s = lowercase(s)

        if !contains(s, r"^[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}$")
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
            d = u & 0xf
            a[i] = '0' + d + 39*(d > 9)
            u >>= 4
        end
        a[24] = a[19] = a[14] = a[9] = '-'
        return String(a)
    end
end

show(io::IO, u::UUID) = write(io, string(u))
