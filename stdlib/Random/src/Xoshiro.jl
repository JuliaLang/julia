# This file is a part of Julia. License is MIT: https://julialang.org/license

## Xoshiro RNG
# Lots of implementation is shared with TaskLocalRNG

"""
    Xoshiro(seed)
    Xoshiro()

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

# Examples
```jldoctest
julia> using Random

julia> rng = Xoshiro(1234);

julia> x1 = rand(rng, 2)
2-element Vector{Float64}:
 0.32597672886359486
 0.5490511363155669

julia> rng = Xoshiro(1234);

julia> x2 = rand(rng, 2)
2-element Vector{Float64}:
 0.32597672886359486
 0.5490511363155669

julia> x1 == x2
true
```
"""
mutable struct Xoshiro <: AbstractRNG
    s0::UInt64
    s1::UInt64
    s2::UInt64
    s3::UInt64
    s4::UInt64 # internal splitmix state

    Xoshiro(s0::Integer, s1::Integer, s2::Integer, s3::Integer, s4::Integer) = new(s0, s1, s2, s3, s4)
    Xoshiro(s0::Integer, s1::Integer, s2::Integer, s3::Integer) = initstate!(new(), map(UInt64, (s0, s1, s2, s3)))
    Xoshiro(seed=nothing) = seed!(new(), seed)
end

@inline function setstate!(x::Xoshiro, (s0, s1, s2, s3, s4))
    x.s0 = s0
    x.s1 = s1
    x.s2 = s2
    x.s3 = s3
    if s4 !== nothing
        x.s4 = s4
    end
    x
end

@inline getstate(x::Xoshiro) = (x.s0, x.s1, x.s2, x.s3, x.s4)

rng_native_52(::Xoshiro) = UInt64


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

@inline function setstate!(x::TaskLocalRNG, (s0, s1, s2, s3, s4))
    t = current_task()
    t.rngState0 = s0
    t.rngState1 = s1
    t.rngState2 = s2
    t.rngState3 = s3
    if s4 !== nothing
        t.rngState4 = s4
    end
    x
end

@inline function getstate(::TaskLocalRNG)
    t = current_task()
    (t.rngState0, t.rngState1, t.rngState2, t.rngState3, t.rngState4)
end

rng_native_52(::TaskLocalRNG) = UInt64


## Shared implementation between Xoshiro and TaskLocalRNG

# this variant of setstate! initializes the internal splitmix state, a.k.a. `s4`
@inline initstate!(x::Union{TaskLocalRNG, Xoshiro}, (s0, s1, s2, s3)::NTuple{4, UInt64}) =
    setstate!(x, (s0, s1, s2, s3, 1s0 + 3s1 + 5s2 + 7s3))

copy(rng::Union{TaskLocalRNG, Xoshiro}) = Xoshiro(getstate(rng)...)
copy!(dst::Union{TaskLocalRNG, Xoshiro}, src::Union{TaskLocalRNG, Xoshiro}) = setstate!(dst, getstate(src))
==(x::Union{TaskLocalRNG, Xoshiro}, y::Union{TaskLocalRNG, Xoshiro}) = getstate(x) == getstate(y)

function seed!(rng::Union{TaskLocalRNG, Xoshiro})
    # as we get good randomness from RandomDevice, we can skip hashing
    rd = RandomDevice()
    s0 = rand(rd, UInt64)
    s1 = rand(rd, UInt64)
    s2 = rand(rd, UInt64)
    s3 = rand(rd, UInt64)
    initstate!(rng, (s0, s1, s2, s3))
end

function seed!(rng::Union{TaskLocalRNG, Xoshiro}, seed::Union{Vector{UInt32}, Vector{UInt64}})
    c = SHA.SHA2_256_CTX()
    SHA.update!(c, reinterpret(UInt8, seed))
    s0, s1, s2, s3 = reinterpret(UInt64, SHA.digest!(c))
    initstate!(rng, (s0, s1, s2, s3))
end

seed!(rng::Union{TaskLocalRNG, Xoshiro}, seed::Integer) = seed!(rng, make_seed(seed))

@inline function rand(x::Union{TaskLocalRNG, Xoshiro}, ::SamplerType{UInt64})
    s0, s1, s2, s3 = getstate(x)
    tmp = s0 + s3
    res = ((tmp << 23) | (tmp >> 41)) + s0
    t = s1 << 17
    s2 ⊻= s0
    s3 ⊻= s1
    s1 ⊻= s2
    s0 ⊻= s3
    s2 ⊻= t
    s3 = s3 << 45 | s3 >> 19
    setstate!(x, (s0, s1, s2, s3, nothing))
    res
end

@inline function rand(rng::Union{TaskLocalRNG, Xoshiro}, ::SamplerType{UInt128})
    first = rand(rng, UInt64)
    second = rand(rng,UInt64)
    second + UInt128(first) << 64
end

@inline rand(rng::Union{TaskLocalRNG, Xoshiro}, ::SamplerType{Int128}) = rand(rng, UInt128) % Int128

@inline function rand(rng::Union{TaskLocalRNG, Xoshiro},
                      T::SamplerUnion(Bool, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64))
    S = T[]
    # use upper bits
    (rand(rng, UInt64) >>> (64 - 8*sizeof(S))) % S
end

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
