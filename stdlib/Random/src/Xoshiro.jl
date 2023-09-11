# This file is a part of Julia. License is MIT: https://julialang.org/license

## Xoshiro RNG
# Lots of implementation is shared with TaskLocalRNG and XoshiroSplit

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

    Xoshiro(s0::Integer, s1::Integer, s2::Integer, s3::Integer) = new(s0, s1, s2, s3)
    Xoshiro(seed=nothing) = seed!(new(), seed)
end

# NON-PUBLIC
@inline function get_xoshiro_state(x::Xoshiro)
    x.s0, x.s1, x.s2, x.s3
end

# NON-PUBLIC
@inline function set_xoshiro_state!(x::Xoshiro, s0::UInt64, s1::UInt64, s2::UInt64, s3::UInt64)
    x.s0 = s0
    x.s1 = s1
    x.s2 = s2
    x.s3 = s3
    x
end

# NON-PUBLIC
function seedstate!(x::Xoshiro, s0::UInt64, s1::UInt64, s2::UInt64, s3::UInt64)
    set_xoshiro_state!(x, s0, s1, s2, s3)
end

copy(rng::Xoshiro) = Xoshiro(rng.s0, rng.s1, rng.s2, rng.s3)

function copy!(dst::Xoshiro, src::Xoshiro)
    dst.s0, dst.s1, dst.s2, dst.s3 = src.s0, src.s1, src.s2, src.s3
    dst
end

function ==(a::Xoshiro, b::Xoshiro)
    a.s0 == b.s0 && a.s1 == b.s1 && a.s2 == b.s2 && a.s3 == b.s3
end


"""
    XoshiroSplit(seed)
    XoshiroSplit()

Creates the same stream as Xoshiro, but has an additional splitting ability.

For more discussion, cf rng_split in task.c

This is the type currently returned by `copy(default_rng())`.

!!! note
    What the default RNG is is an implementation detail.  Across different versions of
    Julia, you should not expect the default RNG to be always the same, nor that it will
    return the same stream of random numbers for a given seed.
"""
mutable struct XoshiroSplit <: AbstractRNG
    s0::UInt64
    s1::UInt64
    s2::UInt64
    s3::UInt64
    s4::UInt64

    XoshiroSplit(
        s0::Integer, s1::Integer, s2::Integer, s3::Integer, # xoshiro256 state
        s4::Integer, # internal splitmix state
        ) = new(s0, s1, s2, s3, s4)
    XoshiroSplit(seed=nothing) = seed!(new(), seed)
end

# NON-PUBLIC
@inline function get_xoshiro_state(x::XoshiroSplit)
    x.s0, x.s1, x.s2, x.s3
end

# NON-PUBLIC
@inline function set_xoshiro_state!(x::XoshiroSplit, s0::UInt64, s1::UInt64, s2::UInt64, s3::UInt64)
    x.s0 = s0
    x.s1 = s1
    x.s2 = s2
    x.s3 = s3
    x
end

# NON-PUBLIC
function seedstate!(x::XoshiroSplit, s0::UInt64, s1::UInt64, s2::UInt64, s3::UInt64)
    set_xoshiro_state!(x, s0, s1, s2, s3)
    x.s4 = 1s0 + 3s1 + 5s2 + 7s3
    x
end

copy(rng::XoshiroSplit) = XoshiroSplit(rng.s0, rng.s1, rng.s2, rng.s3, rng.s4)

function copy!(dst::XoshiroSplit, src::XoshiroSplit)
    dst.s0, dst.s1, dst.s2, dst.s3, dst.s4 = src.s0, src.s1, src.s2, src.s3, src.s4
    dst
end

function ==(a::XoshiroSplit, b::XoshiroSplit)
    a.s0 == b.s0 && a.s1 == b.s1 && a.s2 == b.s2 && a.s3 == b.s3 && a.s4 == b.s4
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

# NON-PUBLIC
@inline function get_xoshiro_state(x::TaskLocalRNG)
    t = current_task()
    t.rngState0, t.rngState1, t.rngState2, t.rngState3
end

# NON-PUBLIC
@inline function set_xoshiro_state!(x::TaskLocalRNG, s0::UInt64, s1::UInt64, s2::UInt64, s3::UInt64)
    t = current_task()
    t.rngState0 = s0
    t.rngState1 = s1
    t.rngState2 = s2
    t.rngState3 = s3
    x
end

# NON-PUBLIC
function seedstate!(x::TaskLocalRNG, s0::UInt64, s1::UInt64, s2::UInt64, s3::UInt64)
    t = current_task()
    t.rngState0 = s0
    t.rngState1 = s1
    t.rngState2 = s2
    t.rngState3 = s3
    t.rngState4 = 1s0 + 3s1 + 5s2 + 7s3
    x
end

function copy(rng::TaskLocalRNG)
    t = current_task()
    XoshiroSplit(t.rngState0, t.rngState1, t.rngState2, t.rngState3, t.rngState4)
end

function copy!(dst::TaskLocalRNG, src::XoshiroSplit)
    t = current_task()
    t.rngState0 = src.s0
    t.rngState1 = src.s1
    t.rngState2 = src.s2
    t.rngState3 = src.s3
    t.rngState4 = src.s4
    return dst
end

function copy!(dst::XoshiroSplit, src::TaskLocalRNG)
    t = current_task()
    dst.s0 = t.rngState0
    dst.s1 = t.rngState1
    dst.s2 = t.rngState2
    dst.s3 = t.rngState3
    dst.s4 = t.rngState4
    return dst
end

function ==(a::XoshiroSplit, b::TaskLocalRNG)
    t = current_task()
    a.s0 == t.rngState0 && a.s1 == t.rngState1 && a.s2 == t.rngState2 && a.s3 == t.rngState3 && a.s4 == t.rngState4
end

==(a::TaskLocalRNG, b::XoshiroSplit) = b == a

# Shared implementation between Xoshiro, XoshiroSplit, and TaskLocalRNG

const XoshiroLike = Union{TaskLocalRNG, Xoshiro, XoshiroSplit}

rng_native_52(::XoshiroLike) = UInt64

@inline function rand(rng::XoshiroLike, ::SamplerType{UInt64})
    s0, s1, s2, s3 = get_xoshiro_state(rng)
    tmp = s0 + s3
    res = ((tmp << 23) | (tmp >> 41)) + s0
    t = s1 << 17
    s2 ⊻= s0
    s3 ⊻= s1
    s1 ⊻= s2
    s0 ⊻= s3
    s2 ⊻= t
    s3 = s3 << 45 | s3 >> 19
    set_xoshiro_state!(rng, s0, s1, s2, s3)
    res
end

function seed!(rng::XoshiroLike)
    # as we get good randomness from RandomDevice, we can skip hashing
    rd = RandomDevice()
    seedstate!(rng, rand(rd, UInt64), rand(rd, UInt64), rand(rd, UInt64), rand(rd, UInt64))
end

function seed!(rng::XoshiroLike, seed::Union{Vector{UInt32}, Vector{UInt64}})
    c = SHA.SHA2_256_CTX()
    SHA.update!(c, reinterpret(UInt8, seed))
    s0, s1, s2, s3 = reinterpret(UInt64, SHA.digest!(c))
    seedstate!(rng, s0, s1, s2, s3)
end

seed!(rng::XoshiroLike, seed::Integer) = seed!(rng, make_seed(seed))


@inline function rand(rng::XoshiroLike, ::SamplerType{UInt128})
    first = rand(rng, UInt64)
    second = rand(rng,UInt64)
    second + UInt128(first) << 64
end

@inline rand(rng::XoshiroLike, ::SamplerType{Int128}) = rand(rng, UInt128) % Int128

@inline function rand(rng::XoshiroLike,
                      T::SamplerUnion(Bool, Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64))
    S = T[]
    # use upper bits
    (rand(rng, UInt64) >>> (64 - 8*sizeof(S))) % S
end



# for partial words, use upper bits from Xoshiro

rand(r::XoshiroLike, ::SamplerTrivial{UInt52Raw{UInt64}}) = rand(r, UInt64) >>> 12
rand(r::XoshiroLike, ::SamplerTrivial{UInt52{UInt64}})    = rand(r, UInt64) >>> 12
rand(r::XoshiroLike, ::SamplerTrivial{UInt104{UInt128}})  = rand(r, UInt104Raw())

rand(r::XoshiroLike, ::SamplerTrivial{CloseOpen01{Float16}}) =
    Float16(Float32(rand(r, UInt16) >>> 5) * Float32(0x1.0p-11))

rand(r::XoshiroLike, ::SamplerTrivial{CloseOpen01{Float32}}) =
    Float32(rand(r, UInt32) >>> 8) * Float32(0x1.0p-24)

rand(r::XoshiroLike, ::SamplerTrivial{CloseOpen01_64}) =
    Float64(rand(r, UInt64) >>> 11) * 0x1.0p-53
