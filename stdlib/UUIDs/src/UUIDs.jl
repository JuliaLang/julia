# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
This module provides universally unique identifiers (UUIDs),
along with functions creating the different variants.
"""
module UUIDs

using Random

import SHA

export UUID, uuid1, uuid4, uuid5, uuid7, uuid_version

import Base: UUID

"""
    uuid_version(u::UUID) -> Int

Inspects the given UUID and returns its version
(see [RFC 4122](https://tools.ietf.org/html/rfc4122)).

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
by [RFC 4122](https://tools.ietf.org/html/rfc4122). Note that the Node ID is randomly generated (does not identify the host)
according to section 4.5 of the RFC.

The default rng used by `uuid1` is not `Random.default_rng()` and every invocation of `uuid1()` without
an argument should be expected to return a unique identifier. Importantly, the outputs of
`uuid1` do not repeat even when `Random.seed!(seed)` is called. Currently (as of Julia 1.6),
`uuid1` uses `Random.RandomDevice` as the default rng. However, this is an implementation
detail that may change in the future.

!!! compat "Julia 1.6"
    The output of `uuid1` does not depend on `Random.default_rng()` as of Julia 1.6.

# Examples
```jldoctest; filter = r"[a-z0-9]{8}-([a-z0-9]{4}-){3}[a-z0-9]{12}"
julia> using Random

julia> rng = MersenneTwister(1234);

julia> uuid1(rng)
UUID("cfc395e8-590f-11e8-1f13-43a2532b2fa8")
```
"""
function uuid1(rng::AbstractRNG=Random.RandomDevice())
    # 0x01b21dd213814000 is the number of 100 nanosecond intervals
    # between the UUID epoch and Unix epoch
    timestamp = round(UInt64, time() * 1e7) + 0x01b21dd213814000
    _build_uuid1(rng, timestamp)
end

function _build_uuid1(rng::AbstractRNG, timestamp::UInt64)
    u = rand(rng, UInt128)

    # mask off clock sequence and node
    u &= 0x00000000000000003fffffffffffffff

    # set the unicast/multicast bit and version
    u |= 0x00000000000010000000010000000000

    ts_low = timestamp & typemax(UInt32)
    ts_mid = (timestamp >> 32) & typemax(UInt16)
    ts_hi = (timestamp >> 48) & 0x0fff

    u |= UInt128(ts_low) << 96
    u |= UInt128(ts_mid) << 80
    u |= UInt128(ts_hi) << 64

    return UUID(u)
end

"""
    uuid4([rng::AbstractRNG]) -> UUID

Generates a version 4 (random or pseudo-random) universally unique identifier (UUID),
as specified by [RFC 4122](https://tools.ietf.org/html/rfc4122).

The default rng used by `uuid4` is not `Random.default_rng()` and every invocation of `uuid4()` without
an argument should be expected to return a unique identifier. Importantly, the outputs of
`uuid4` do not repeat even when `Random.seed!(seed)` is called. Currently (as of Julia 1.6),
`uuid4` uses `Random.RandomDevice` as the default rng. However, this is an implementation
detail that may change in the future.

!!! compat "Julia 1.6"
    The output of `uuid4` does not depend on `Random.default_rng()` as of Julia 1.6.

# Examples
```jldoctest
julia> using Random

julia> rng = Xoshiro(123);

julia> uuid4(rng)
UUID("856e446e-0c6a-472a-9638-f7b8557cd282")
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
as specified by [RFC 4122](https://tools.ietf.org/html/rfc4122).

!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.

# Examples
```jldoctest
julia> using Random

julia> rng = Xoshiro(123);

julia> u4 = uuid4(rng)
UUID("856e446e-0c6a-472a-9638-f7b8557cd282")

julia> u5 = uuid5(u4, "julia")
UUID("2df91e3f-da06-5362-a6fe-03772f2e14c9")
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

"""
    uuid7([rng::AbstractRNG]) -> UUID

Generates a version 7 (random or pseudo-random) universally unique identifier (UUID),
as specified by [RFC 9562](https://tools.ietf.org/html/rfc9562).

The default rng used by `uuid7` is not `Random.default_rng()` and every invocation of `uuid7()` without
an argument should be expected to return a unique identifier. Importantly, the outputs of
`uuid7` do not repeat even when `Random.seed!(seed)` is called. Currently (as of Julia 1.12),
`uuid7` uses `Random.RandomDevice` as the default rng. However, this is an implementation
detail that may change in the future.

!!! compat "Julia 1.12"
    `uuid7()` is available as of Julia 1.12.

# Examples
```jldoctest; filter = r"[a-z0-9]{8}-([a-z0-9]{4}-){3}[a-z0-9]{12}"
julia> using Random

julia> rng = Xoshiro(123);

julia> uuid7(rng)
UUID("019026ca-e086-772a-9638-f7b8557cd282")
```
"""
function uuid7(rng::AbstractRNG=Random.RandomDevice())
    # current time in ms, rounded to an Integer
    timestamp = round(UInt128, time() * 1e3)
    _build_uuid7(rng, timestamp)
end

function _build_uuid7(rng::AbstractRNG, timestamp::UInt128)
    bytes = rand(rng, UInt128)
    # make space for the timestamp
    bytes &= 0x0000000000000fff3fffffffffffffff
    # version & variant
    bytes |= 0x00000000000070008000000000000000

    bytes |= timestamp << UInt128(80)

    return UUID(bytes)
end

end
