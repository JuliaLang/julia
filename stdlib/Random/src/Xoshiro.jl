# This file is a part of Julia. License is MIT: https://julialang.org/license

## Xoshiro RNG
# Lots of implementation is shared with TaskLocalRNG

"""
    Xoshiro(seed::Union{Integer, AbstractString})
    Xoshiro()

Xoshiro256++ is a fast pseudorandom number generator described by David Blackman and
Sebastiano Vigna in "Scrambled Linear Pseudorandom Number Generators",
ACM Trans. Math. Softw., 2021. Reference implementation is available
at https://prng.di.unimi.it

Apart from the high speed, Xoshiro has a small memory footprint, making it suitable for
applications where many different random states need to be held for long time.

Julia's Xoshiro implementation has a bulk-generation mode; this seeds new virtual PRNGs
from the parent, and uses SIMD to generate in parallel (i.e. the bulk stream consists of
multiple interleaved xoshiro instances).
The virtual PRNGs are discarded once the bulk request has been serviced (and should cause
no heap allocations).

If no seed is provided, a randomly generated one is created (using entropy from the system).
See the [`seed!`](@ref) function for reseeding an already existing `Xoshiro` object.

!!! compat "Julia 1.11"
    Passing a negative integer seed requires at least Julia 1.11.

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

# Jump functions from: https://xoshiro.di.unimi.it/xoshiro256plusplus.c

for (fname, JUMP) in ((:jump_128, (0x180ec6d33cfd0aba, 0xd5a61266f0c9392c, 0xa9582618e03fc9aa, 0x39abdc4529b1661c)),
                      (:jump_192, (0x76e15d3efefdcbbf, 0xc5004e441c522fb3, 0x77710069854ee241, 0x39109bb02acbe635)))
    local fname! = Symbol(fname, :!)
    @eval function $fname!(rng::Xoshiro)
        _s0 = 0x0000000000000000
        _s1 = 0x0000000000000000
        _s2 = 0x0000000000000000
        _s3 = 0x0000000000000000
        s0, s1, s2, s3 = rng.s0, rng.s1, rng.s2, rng.s3
        for j in $JUMP
            for b in 0x0000000000000000:0x000000000000003f
                if (j & 0x0000000000000001 << b) != 0
                    _s0 ⊻= s0
                    _s1 ⊻= s1
                    _s2 ⊻= s2
                    _s3 ⊻= s3
                end
                t = s1 << 17
                s2 = xor(s2, s0)
                s3 = xor(s3, s1)
                s1 = xor(s1, s2)
                s0 = xor(s0, s3)
                s2 = xor(s2, t)
                s3 = s3 << 45 | s3 >> 19
            end
        end
        setstate!(rng, (_s0, _s1, _s2, _s3, nothing))
    end
    @eval $fname(rng::Xoshiro) = $fname!(copy(rng))

    @eval function $fname!(rng::Xoshiro, n::Integer)
        n < 0 && throw(DomainError(n, "the number of jumps must be ≥ 0"))
        i = zero(n)
        while i < n
            $fname!(rng)
            i += one(n)
        end
        rng
    end

    @eval $fname(rng::Xoshiro, n::Integer) = $fname!(copy(rng), n)
end

for (fname, sz) in ((:jump_128, 128), (:jump_192, 192))
    local fname! = Symbol(fname, :!)
    local see_other = Symbol(fname === :jump_128 ? :jump_192 : :jump_128)
    local see_other! = Symbol(see_other, :!)
    local seq_pow = 256 - sz
    @eval begin
        """
            $($fname!)(rng::Xoshiro, [n::Integer=1])

        Jump forward, advancing the state equivalent to `2^$($sz)` calls which consume
        8 bytes (i.e. a full `UInt64`) each.

        If `n > 0` is provided, the state is advanced equivalent to `n * 2^$($sz)` calls; if `n = 0`,
        the state remains unchanged.

        This can be used to generate `2^$($seq_pow)` non-overlapping subsequences for parallel computations.

        See also: [`$($fname)`](@ref), [`$($see_other!)`](@ref)

        # Examples
        ```julia-repl
        julia> $($fname!)($($fname!)(Xoshiro(1))) == $($fname!)(Xoshiro(1), 2)
        true
        ```
        """
        function $fname! end
    end

    @eval begin
        """
            $($fname)(rng::Xoshiro, [n::Integer=1])

        Return a copy of `rng` with the state advanced equivalent to `n * 2^$($sz)` calls which consume
        8 bytes (i.e. a full `UInt64`) each; if `n = 0`, the state of the returned copy will be
        identical to `rng`.

        This can be used to generate `2^$($seq_pow)` non-overlapping subsequences for parallel computations.

        See also: [`$($fname!)`](@ref), [`$($see_other)`](@ref)

        # Examples
        ```julia-repl
        julia> x = Xoshiro(1);

        julia> $($fname)($($fname)(x)) == $($fname)(x, 2)
        true

        julia> $($fname)(x, 0) == x
        true

        julia> $($fname)(x, 0) === x
        false
        ```
        """
        function $fname end
    end
end

## Task local RNG

"""
    TaskLocalRNG

The `TaskLocalRNG` has state that is local to its task, not its thread.
It is seeded upon task creation, from the state of its parent task, but without
advancing the state of the parent's RNG.

As an upside, the `TaskLocalRNG` is pretty fast, and permits reproducible
multithreaded simulations (barring race conditions), independent of scheduler
decisions. As long as the number of threads is not used to make decisions on
task creation, simulation results are also independent of the number of available
threads / CPUs. The random stream should not depend on hardware specifics, up to
endianness and possibly word size.

Using or seeding the RNG of any other task than the one returned by `current_task()`
is undefined behavior: it will work most of the time, and may sometimes fail silently.

When seeding `TaskLocalRNG()` with [`seed!`](@ref), the passed seed, if any,
may be any integer.

!!! compat "Julia 1.11"
    Seeding `TaskLocalRNG()` with a negative integer seed requires at least Julia 1.11.

!!! compat "Julia 1.10"
    Task creation no longer advances the parent task's RNG state as of Julia 1.10.
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
@inline function initstate!(x::Union{TaskLocalRNG, Xoshiro}, state)
    length(state) == 4 && eltype(state) == UInt64 ||
        throw(ArgumentError("initstate! expects a list of 4 `UInt64` values"))
    s0, s1, s2, s3 = state
    setstate!(x, (s0, s1, s2, s3, 1s0 + 3s1 + 5s2 + 7s3))
end

copy(rng::Union{TaskLocalRNG, Xoshiro}) = Xoshiro(getstate(rng)...)
copy!(dst::Union{TaskLocalRNG, Xoshiro}, src::Union{TaskLocalRNG, Xoshiro}) = setstate!(dst, getstate(src))
==(x::Union{TaskLocalRNG, Xoshiro}, y::Union{TaskLocalRNG, Xoshiro}) = getstate(x) == getstate(y)
# use a magic (random) number to scramble `h` so that `hash(x)` is distinct from `hash(getstate(x))`
hash(x::Union{TaskLocalRNG, Xoshiro}, h::UInt) = hash(getstate(x), h + 0x49a62c2dda6fa9be % UInt)

function seed!(rng::Union{TaskLocalRNG, Xoshiro}, ::Nothing)
    # as we get good randomness from RandomDevice, we can skip hashing
    rd = RandomDevice()
    s0 = rand(rd, UInt64)
    s1 = rand(rd, UInt64)
    s2 = rand(rd, UInt64)
    s3 = rand(rd, UInt64)
    initstate!(rng, (s0, s1, s2, s3))
end

seed!(rng::Union{TaskLocalRNG, Xoshiro}, seed) =
    initstate!(rng, reinterpret(UInt64, hash_seed(seed)))


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

for FT in (Float16, Float32, Float64)
    UT = Base.uinttype(FT)
    # Helper function: scale an unsigned integer to a floating point number of the same size
    # in the interval [0, 1).  This is equivalent to, but more easily extensible than
    #     Float16(i >>>  5) * Float16(0x1.0p-11)
    #     Float32(i >>>  8) * Float32(0x1.0p-24)
    #     Float32(i >>> 11) * Float64(0x1.0p-53)
    @eval @inline _uint2float(i::$(UT), ::Type{$(FT)}) =
        $(FT)(i >>> $(8 * sizeof(FT) - precision(FT))) * $(FT(2) ^ -precision(FT))

    @eval rand(r::Union{TaskLocalRNG, Xoshiro}, ::SamplerTrivial{CloseOpen01{$(FT)}}) =
        _uint2float(rand(r, $(UT)), $(FT))
end
