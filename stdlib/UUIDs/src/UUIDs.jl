# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
This module provides universally unique identifiers (UUIDs),
along with functions creating the different variants.
"""
module UUIDs

using Random

import SHA

export UUID, uuid1, uuid4, uuid5, uuid_version

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

# Some UUID namespaces provided in the appendix of RFC 4122
# https://tools.ietf.org/html/rfc4122.html#appendix-C
const namespace_dns  = UUID(0x6ba7b8109dad11d180b400c04fd430c8) # 6ba7b810-9dad-11d1-80b4-00c04fd430c8
const namespace_url  = UUID(0x6ba7b8119dad11d180b400c04fd430c8) # 6ba7b811-9dad-11d1-80b4-00c04fd430c8
const namespace_oid  = UUID(0x6ba7b8129dad11d180b400c04fd430c8) # 6ba7b812-9dad-11d1-80b4-00c04fd430c8
const namespace_x500 = UUID(0x6ba7b8149dad11d180b400c04fd430c8) # 6ba7b814-9dad-11d1-80b4-00c04fd430c8

"""
    uuid1([rng::AbstractRNG]) -> UUID

Generates a version 1 (time-based) universally unique identifier (UUID), as specified
by RFC 4122. Note that the Node ID is randomly generated (does not identify the host)
according to section 4.5 of the RFC.

The default rng used by `uuid1` is not `GLOBAL_RNG` and every invocation of `uuid1()` without
an argument should be expected to return a unique identifier. Importantly, the outputs of
`uuid1` do not repeat even when `Random.seed!(seed)` is called. Currently (as of Julia 1.6),
`uuid1` uses `Random.RandomDevice` as the default rng. However, this is an implementation
detail that may change in the future.

!!! compat "Julia 1.6"
    The output of `uuid1` does not depend on `GLOBAL_RNG` as of Julia 1.6.

# Examples
```jldoctest; filter = r"[a-z0-9]{8}-([a-z0-9]{4}-){3}[a-z0-9]{12}"
julia> rng = MersenneTwister(1234);

julia> uuid1(rng)
UUID("cfc395e8-590f-11e8-1f13-43a2532b2fa8")
```
"""
function uuid1(rng::AbstractRNG=Random.RandomDevice())
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
    uuid4([rng::AbstractRNG]) -> UUID

Generates a version 4 (random or pseudo-random) universally unique identifier (UUID),
as specified by RFC 4122.

The default rng used by `uuid4` is not `GLOBAL_RNG` and every invocation of `uuid4()` without
an argument should be expected to return a unique identifier. Importantly, the outputs of
`uuid4` do not repeat even when `Random.seed!(seed)` is called. Currently (as of Julia 1.6),
`uuid4` uses `Random.RandomDevice` as the default rng. However, this is an implementation
detail that may change in the future.

!!! compat "Julia 1.6"
    The output of `uuid4` does not depend on `GLOBAL_RNG` as of Julia 1.6.

# Examples
```jldoctest
julia> rng = MersenneTwister(1234);

julia> uuid4(rng)
UUID("196f2941-2d58-45ba-9f13-43a2532b2fa8")
```
"""
function uuid4(rng::AbstractRNG=Random.RandomDevice())
    u = rand(rng, UInt128)
    u &= 0xffffffffffff0fff3fffffffffffffff
    u |= 0x00000000000040008000000000000000
    UUID(u)
end

"""
    uuid5(ns::UUID, name::String) -> UUID

Generates a version 5 (namespace and domain-based) universally unique identifier (UUID),
as specified by RFC 4122.

!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.

# Examples
```jldoctest
julia> rng = MersenneTwister(1234);

julia> u4 = uuid4(rng)
UUID("196f2941-2d58-45ba-9f13-43a2532b2fa8")

julia> u5 = uuid5(u4, "julia")
UUID("b37756f8-b0c0-54cd-a466-19b3d25683bc")
```
"""
function uuid5(ns::UUID, name::String)
    nsbytes = zeros(UInt8, 16)
    nsv = ns.value
    for idx in Base.OneTo(16)
        nsbytes[idx] = nsv >> 120
        nsv = nsv << 8
    end
    hash_result = SHA.sha1(append!(nsbytes, convert(Vector{UInt8}, codeunits(unescape_string(name)))))
    # set version number to 5
    hash_result[7] = (hash_result[7] & 0x0F) | (0x50)
    hash_result[9] = (hash_result[9] & 0x3F) | (0x80)
    v = zero(UInt128)
    #use only the first 16 bytes of the SHA1 hash
    for idx in Base.OneTo(16)
        v = (v << 0x08) | hash_result[idx]
    end
    return UUID(v)
end

end
