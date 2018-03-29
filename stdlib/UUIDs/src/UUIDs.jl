# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

module UUIDs

using Random

export UUID, uuid1, uuid4, uuid_version

import Base: UUID

"""
    uuid_version(u::UUID) -> Int

Inspects the given UUID and returns its version
(see [RFC 4122](https://www.ietf.org/rfc/rfc4122)).

# Examples
```jldoctest
julia> uuid_version(uuid4())
4
```
"""
uuid_version(u::UUID) = Int((u.value >> 76) & 0xf)

"""
    uuid1([rng::AbstractRNG=GLOBAL_RNG]) -> UUID

Generates a version 1 (time-based) universally unique identifier (UUID), as specified
by RFC 4122. Note that the Node ID is randomly generated (does not identify the host)
according to section 4.5 of the RFC.

# Examples
```jldoctest; filter = r"[a-z0-9]{8}-([a-z0-9]{4}-){3}[a-z0-9]{12}"
julia> rng = MersenneTwister(1234);

julia> uuid1(rng)
fcbd9b64-1bc2-11e8-1f13-43a2532b2fa8
```
"""
function uuid1(rng::AbstractRNG=Random.GLOBAL_RNG)
    u = rand(rng, UInt128)

    # mask off clock sequence and node
    u &= 0x00000000000000003fffffffffffffff

    # set the unicast/multicast bit and version
    u |= 0x00000000000010000000010000000000

    # 0x01b21dd213814000 is the number of 100 nanosecond intervals
    # between the UUID epoch and Unix epoch
    timestamp = round(UInt64, time() * 1e7) + 0x01b21dd213814000
    ts_low = timestamp & typemax(UInt32)
    ts_mid = (timestamp >> 32) & typemax(UInt16)
    ts_hi = (timestamp >> 48) & 0x0fff

    u |= UInt128(ts_low) << 96
    u |= UInt128(ts_mid) << 80
    u |= UInt128(ts_hi) << 64

    UUID(u)
end

"""
    uuid4([rng::AbstractRNG=GLOBAL_RNG]) -> UUID

Generates a version 4 (random or pseudo-random) universally unique identifier (UUID),
as specified by RFC 4122.

# Examples
```jldoctest
julia> rng = MersenneTwister(1234);

julia> uuid4(rng)
196f2941-2d58-45ba-9f13-43a2532b2fa8
```
"""
function uuid4(rng::AbstractRNG=Random.GLOBAL_RNG)
    u = rand(rng, UInt128)
    u &= 0xffffffffffff0fff3fffffffffffffff
    u |= 0x00000000000040008000000000000000
    UUID(u)
end

end
