# This file is a part of Julia. License is MIT: https://julialang.org/license

module Scheduler

"""
    cong(max::UInt32)

Return a random UInt32 in the range `1:max` except if max is 0, in that case return 0.
"""
cong(max::UInt32) = iszero(max) ? UInt32(0) : rand_ptls(max) + UInt32(1) #TODO: make sure users don't use 0 and remove this check

get_ptls_rng() = ccall(:jl_get_ptls_rng, UInt64, ())

set_ptls_rng(seed::UInt64) = ccall(:jl_set_ptls_rng, Cvoid, (UInt64,), seed)

"""
    rand_ptls(max::UInt32)

Return a random UInt32 in the range `0:max-1` using the thread-local RNG
state. Max must be greater than 0.
"""
Base.@assume_effects :removable :inaccessiblememonly :notaskstate function rand_ptls(max::UInt32)
    rngseed = get_ptls_rng()
    val, seed = rand_uniform_max_int32(max, rngseed)
    set_ptls_rng(seed)
    return val % UInt32
end

# This implementation is based on OpenSSLs implementation of rand_uniform
# https://github.com/openssl/openssl/blob/1d2cbd9b5a126189d5e9bc78a3bdb9709427d02b/crypto/rand/rand_uniform.c#L13-L99
# Comments are vendored from their implementation as well.
# For the original developer check the PR to swift https://github.com/apple/swift/pull/39143.

# Essentially it boils down to incrementally generating a fixed point
# number on the interval [0, 1) and multiplying this number by the upper
# range limit.  Once it is certain what the fractional part contributes to
# the integral part of the product, the algorithm has produced a definitive
# result.
"""
    rand_uniform_max_int32(max::UInt32, seed::UInt64)

Return a random UInt32 in the range `0:max-1` using the given seed.
Max must be greater than 0.
"""
Base.@assume_effects :total function rand_uniform_max_int32(max::UInt32, seed::UInt64)
    if max == UInt32(1)
        return UInt32(0), seed
    end
    # We are generating a fixed point number on the interval [0, 1).
    # Multiplying this by the range gives us a number on [0, upper).
    # The high word of the multiplication result represents the integral part
    # This is not completely unbiased as it's missing the fractional part of the original implementation but it's good enough for our purposes
    seed = UInt64(69069) * seed + UInt64(362437)
    prod = (UInt64(max)) * (seed % UInt32) # 64 bit product
    i = prod >> 32 % UInt32 # integral part
    return i % UInt32, seed
end

include("scheduler/partr.jl")

const ChosenScheduler = Partr



# Scheduler interface:
    # enqueue! which pushes a runnable Task into it
    # dequeue! which pops a runnable Task from it
    # checktaskempty which returns true if the scheduler has no available Tasks

enqueue!(t::Task) = ChosenScheduler.enqueue!(t)
dequeue!() = ChosenScheduler.dequeue!()
checktaskempty() = ChosenScheduler.checktaskempty()

end
