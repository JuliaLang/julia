# This file is a part of Julia. License is MIT: https://julialang.org/license

## Xoshiro RNG
# Lots of implementation is shared with TaskLocalRNG

"""
    Xoshiro

Xoshiro256++ is a fast pseudorandom number generator described by David Blackman and
Sebastiano Vigna in "Scrambled Linear Pseudorandom Number Generators",
ACM Trans. Math. Softw., 2021. Reference implementation is available
at http://prng.di.unimi.it

Apart from the high speed, Xoshiro has a small memory footprint, making it suitable for
applications where many different random states need to be held for long time.

Julia's Xoshiro implementation has a bulk-generation mode; this seeds new virtual PRNGs
from the parent, and uses SIMD to generate in parallel (i.e. the bulk stream consists of
multiple interleaved xoshiro instances).
The virtual PRNGs are discarded once the bulk request has been serviced (and should cause
no heap allocations).
"""
mutable struct Xoshiro <: AbstractRNG
    s0::UInt64
    s1::UInt64
    s2::UInt64
    s3::UInt64

    Xoshiro(s0::Integer, s1::Integer, s2::Integer, s3::Integer) = new(s0, s1, s2, s3)
    Xoshiro(seed=nothing) = seed!(new(), seed)
end

function setstate!(x::Xoshiro, s0::UInt64, s1::UInt64, s2::UInt64, s3::UInt64)
    x.s0 = s0
    x.s1 = s1
    x.s2 = s2
    x.s3 = s3
    x
end

copy(rng::Xoshiro) = Xoshiro(rng.s0, rng.s1, rng.s2, rng.s3)

function copy!(dst::Xoshiro, src::Xoshiro)
    dst.s0, dst.s1, dst.s2, dst.s3 = src.s0, src.s1, src.s2, src.s3
    dst
end

function ==(a::Xoshiro, b::Xoshiro)
    a.s0 == b.s0 && a.s1 == b.s1 && a.s2 == b.s2 && a.s3 == b.s3
end

rng_native_52(::Xoshiro) = UInt64

@inline function rand(rng::Xoshiro, ::SamplerType{UInt64})
    s0, s1, s2, s3 = rng.s0, rng.s1, rng.s2, rng.s3
    tmp = s0 + s3
    res = ((tmp << 23) | (tmp >> 41)) + s0
    t = s1 << 17
    s2 = xor(s2, s0)
    s3 = xor(s3, s1)
    s1 = xor(s1, s2)
    s0 = xor(s0, s3)
    s2 = xor(s2, t)
    s3 = s3 << 45 | s3 >> 19
    rng.s0, rng.s1, rng.s2, rng.s3 = s0, s1, s2, s3
    res
end


## Task local RNG

"""
    TaskLocalRNG

The `TaskLocalRNG` has state that is local to its task, not its thread.
It is seeded upon task creation, from the state of its parent task.
Therefore, task creation is an event that changes the parent's RNG state.

As an upside, the `TaskLocalRNG` is pretty fast, and permits reproducible
multithreaded simulations (barring race conditions), independent of scheduler
decisions. As long as the number of threads is not used to make decisions on
task creation, simulation results are also independent of the number of available
threads / CPUs. The random stream should not depend on hardware specifics, up to
endianness and possibly word size.

Using or seeding the RNG of any other task than the one returned by `current_task()`
is undefined behavior: it will work most of the time, and may sometimes fail silently.
"""
struct TaskLocalRNG <: AbstractRNG end
TaskLocalRNG(::Nothing) = TaskLocalRNG()
rng_native_52(::TaskLocalRNG) = UInt64

function setstate!(x::TaskLocalRNG, s0::UInt64, s1::UInt64, s2::UInt64, s3::UInt64)
    t = current_task()
    t.rngState0 = s0
    t.rngState1 = s1
    t.rngState2 = s2
    t.rngState3 = s3
    x
end

@inline function rand(::TaskLocalRNG, ::SamplerType{UInt64})
    task = current_task()
    s0, s1, s2, s3 = task.rngState0, task.rngState1, task.rngState2, task.rngState3
    tmp = s0 + s3
    res = ((tmp << 23) | (tmp >> 41)) + s0
    t = s1 << 17
    s2 = xor(s2, s0)
    s3 = xor(s3, s1)
    s1 = xor(s1, s2)
    s0 = xor(s0, s3)
    s2 = xor(s2, t)
    s3 = s3 << 45 | s3 >> 19
    task.rngState0, task.rngState1, task.rngState2, task.rngState3 = s0, s1, s2, s3
    res
end

# Shared implementation between Xoshiro and TaskLocalRNG -- seeding

function seed!(x::Union{TaskLocalRNG,Xoshiro})
    # as we get good randomness from RandomDevice, we can skip hashing
    parent = RandomDevice()
    # Constants have nothing up their sleeve, see task.c
    # 0x02011ce34bce797f == hash(UInt(1))|0x01
    # 0x5a94851fb48a6e05 == hash(UInt(2))|0x01
    # 0x3688cf5d48899fa7 == hash(UInt(3))|0x01
    # 0x867b4bb4c42e5661 == hash(UInt(4))|0x01
    setstate!(x,
              0x02011ce34bce797f * rand(parent, UInt64),
              0x5a94851fb48a6e05 * rand(parent, UInt64),
              0x3688cf5d48899fa7 * rand(parent, UInt64),
              0x867b4bb4c42e5661 * rand(parent, UInt64))
end

function seed!(rng::Union{TaskLocalRNG,Xoshiro}, seed::NTuple{4,UInt64})
    # TODO: Consider a less ad-hoc construction
    # We can afford burning a handful of cycles here, and we don't want any
    # surprises with respect to bad seeds / bad interactions.

    s0 = s  = Base.hash_64_64(seed[1])
    s1 = s += Base.hash_64_64(seed[2])
    s2 = s += Base.hash_64_64(seed[3])
    s3 = s += Base.hash_64_64(seed[4])

    setstate!(rng, s0, s1, s2, s3)

    rand(rng, UInt64)
    rand(rng, UInt64)
    rand(rng, UInt64)
    rand(rng, UInt64)
    rng
end

function seed!(rng::Union{TaskLocalRNG, Xoshiro}, seed::UInt128)
    seed0 = seed % UInt64
    seed1 = (seed>>>64) % UInt64
    seed!(rng, (seed0, seed1, zero(UInt64), zero(UInt64)))
end
seed!(rng::Union{TaskLocalRNG, Xoshiro}, seed::Integer) = seed!(rng, UInt128(seed))

function seed!(rng::Union{TaskLocalRNG, Xoshiro}, seed::AbstractVector{UInt64})
    if length(seed) > 4
        throw(ArgumentError("seed should have no more than 256 bits"))
    end
    seed0 = length(seed)>0 ? seed[1] : UInt64(0)
    seed1 = length(seed)>1 ? seed[2] : UInt64(0)
    seed2 = length(seed)>2 ? seed[3] : UInt64(0)
    seed3 = length(seed)>3 ? seed[4] : UInt64(0)
    seed!(rng, (seed0, seed1, seed2, seed3))
end

function seed!(rng::Union{TaskLocalRNG, Xoshiro}, seed::AbstractVector{UInt32})
    if iseven(length(seed))
        seed!(rng, reinterpret(UInt64, seed))
    else
        seed!(rng, UInt64[reinterpret(UInt64, @view(seed[begin:end-1])); seed[end] % UInt64])
    end
end

@inline function rand(rng::Union{TaskLocalRNG, Xoshiro}, ::SamplerType{UInt128})
    first = rand(rng, UInt64)
    second = rand(rng,UInt64)
    second + UInt128(first)<<64
end

@inline rand(rng::Union{TaskLocalRNG, Xoshiro}, ::SamplerType{Int128}) = rand(rng, UInt128) % Int128

@inline function rand(rng::Union{TaskLocalRNG, Xoshiro},
                      T::SamplerUnion(Bool, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64))
    S = T[]
    # use upper bits
    (rand(rng, UInt64) >>> (64 - 8*sizeof(S))) % S
end

function copy(rng::TaskLocalRNG)
    t = current_task()
    Xoshiro(t.rngState0, t.rngState1, t.rngState2, t.rngState3)
end

function copy!(dst::TaskLocalRNG, src::Xoshiro)
    t = current_task()
    t.rngState0, t.rngState1, t.rngState2, t.rngState3 = src.s0, src.s1, src.s2, src.s3
    dst
end

function copy!(dst::Xoshiro, src::TaskLocalRNG)
    t = current_task()
    dst.s0, dst.s1, dst.s2, dst.s3 = t.rngState0, t.rngState1, t.rngState2, t.rngState3
    dst
end

function ==(a::Xoshiro, b::TaskLocalRNG)
    t = current_task()
    a.s0 == t.rngState0 && a.s1 == t.rngState1 && a.s2 == t.rngState2 && a.s3 == t.rngState3
end

==(a::TaskLocalRNG, b::Xoshiro) = b == a

# for partial words, use upper bits from Xoshiro

rand(r::Union{TaskLocalRNG, Xoshiro}, ::SamplerTrivial{UInt52Raw{UInt64}}) = rand(r, UInt64) >>> 12
rand(r::Union{TaskLocalRNG, Xoshiro}, ::SamplerTrivial{UInt52{UInt64}})    = rand(r, UInt64) >>> 12
rand(r::Union{TaskLocalRNG, Xoshiro}, ::SamplerTrivial{UInt104{UInt128}})  = rand(r, UInt104Raw())

rand(r::Union{TaskLocalRNG, Xoshiro}, ::SamplerTrivial{CloseOpen01{Float16}}) =
    Float16(Float32(rand(r, UInt16) >>> 5) * Float32(0x1.0p-11))

rand(r::Union{TaskLocalRNG, Xoshiro}, ::SamplerTrivial{CloseOpen01{Float32}}) =
    Float32(rand(r, UInt32) >>> 8) * Float32(0x1.0p-24)

rand(r::Union{TaskLocalRNG, Xoshiro}, ::SamplerTrivial{CloseOpen01_64}) =
    Float64(rand(r, UInt64) >>> 11) * 0x1.0p-53
