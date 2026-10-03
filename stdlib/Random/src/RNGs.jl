# This file is a part of Julia. License is MIT: https://julialang.org/license

## default RNG

"""
    Random.default_rng() -> rng

Return the default global random number generator (RNG), which is used by `rand`-related functions when
no explicit RNG is provided.

When the `Random` module is loaded, the default RNG is _randomly_ seeded, via [`Random.seed!()`](@ref):
this means that each time a new julia session is started, the first call to `rand()` produces a different
result, unless `seed!(seed)` is called first.

It is thread-safe: distinct threads can safely call `rand`-related functions on `default_rng()` concurrently,
e.g. `rand(default_rng())`.

!!! note
    The type of the default RNG is an implementation detail. Across different versions of
    Julia, you should not expect the default RNG to always have the same type, nor that it will
    produce the same stream of random numbers for a given seed.

!!! compat "Julia 1.3"
    This function was introduced in Julia 1.3.
"""
@inline default_rng() = TaskLocalRNG()
@inline default_rng(tid::Int) = TaskLocalRNG()

# defined only for backward compatibility with pre-v1.3 code when `default_rng()` didn't exist;
# `GLOBAL_RNG` was never really documented, but was appearing in the docstring of `rand`
const GLOBAL_RNG = default_rng()

# In v1.0, the GLOBAL_RNG was storing the seed which was used to initialize it; this seed was used to implement
# the following feature of `@testset`:
# > Before the execution of the body of a `@testset`, there is an implicit
# > call to `Random.seed!(seed)` where `seed` is the current seed of the global RNG.
# But the global RNG is now `TaskLocalRNG()` and doesn't store its seed; in order to not break `@testset`,
# in a call like `seed!(seed)` *without* an explicit RNG, we now store the state of `TaskLocalRNG()` in
# `task_local_storage()`

# GLOBAL_SEED is used as a fall-back when no tls seed is found
# only `Random.__init__` is allowed to set it
const GLOBAL_SEED = Xoshiro(0, 0, 0, 0, 0)

get_tls_seed() = get!(() -> copy(GLOBAL_SEED), task_local_storage(),
                      :__RANDOM_GLOBAL_RNG_SEED_uBlmfA8ZS__)::Xoshiro

# seed the default RNG
function seed!(seed=nothing)
    seed!(default_rng(), seed)
    copy!(get_tls_seed(), default_rng())
    default_rng()
end

function __init__()
    # do not call no-arg `seed!()` to not update `task_local_storage()` unnecessarily at startup
    seed!(default_rng())
    copy!(GLOBAL_SEED, TaskLocalRNG())
    ccall(:jl_gc_init_finalizer_rng_state, Cvoid, ())
end


## RandomDevice

"""
    RandomDevice()

Create a `RandomDevice` RNG object.
Two such objects will always generate different streams of random numbers.
The entropy is obtained from the operating system.
"""
struct RandomDevice <: AbstractRNG; end
RandomDevice(seed::Nothing) = RandomDevice()
seed!(rng::RandomDevice, ::Nothing) = rng

rand(rd::RandomDevice, sp::SamplerBoolBitInteger) = Libc.getrandom!(Ref{sp[]}())[]
rand(rd::RandomDevice, ::SamplerType{Bool}) = rand(rd, UInt8) % Bool

# specialization for homogeneous tuple types of builtin integers, to avoid
# repeated system calls
rand(rd::RandomDevice, sp::SamplerTag{Ref{Tuple{Vararg{T, N}}}, Tuple{S}}
     ) where {T, N, S <: SamplerUnion(Base.BitInteger_types...)} =
         Libc.getrandom!(Ref{gentype(sp)}())[]

function rand!(rd::RandomDevice, A::Array{Bool}, ::SamplerType{Bool})
    Libc.getrandom!(A)
    # we need to mask the result so that only the LSB in each byte can be non-zero
    GC.@preserve A begin
        p = Ptr{UInt8}(pointer(A))
        for i = 1:length(A)
            unsafe_store!(p, unsafe_load(p) & 0x1)
            p += 1
        end
    end
    return A
end
for T in BitInteger_types
    @eval rand!(rd::RandomDevice, A::Array{$T}, ::SamplerType{$T}) = Libc.getrandom!(A)
end

# RandomDevice produces natively UInt64
rng_native_52(::RandomDevice) = UInt64


## seeding

"""
    seed!([rng=default_rng()], seed) -> rng
    seed!([rng=default_rng()]) -> rng

Reseed the random number generator: `rng` will give a reproducible
sequence of numbers if and only if a `seed` is provided. Some RNGs
don't accept a seed, like `RandomDevice`.
After the call to `seed!`, `rng` is equivalent to a newly created
object initialized with the same seed.

The types of accepted seeds depend on the type of `rng`, but in general,
integer seeds should work. Providing `nothing` as the seed should be
equivalent to not providing one.

If `rng` is not specified, it defaults to seeding the state of the
shared task-local generator.

# Examples
```jldoctest; filter = r"(true|false)"
julia> Random.seed!(1234);

julia> x1 = rand(2)
2-element Vector{Float64}:
 0.32597672886359486
 0.5490511363155669

julia> Random.seed!(1234);

julia> x2 = rand(2)
2-element Vector{Float64}:
 0.32597672886359486
 0.5490511363155669

julia> x1 == x2
true

julia> rng = Xoshiro(1234); rand(rng, 2) == x1
true

julia> Xoshiro(1) == Random.seed!(rng, 1)
true

julia> rand(Random.seed!(rng), Bool) # not reproducible
true

julia> rand(Random.seed!(rng), Bool) # not reproducible either
false

julia> rand(Xoshiro(), Bool) # not reproducible either
true
```
"""
seed!

function seed!(rng::AbstractRNG, seed::Any=nothing)
    if seed === nothing
        seed!(rng, RandomDevice())
    elseif seed isa AbstractRNG
        # avoid getting into an infinite recursive call from the other branches
        throw(MethodError(seed!, (rng, seed)))
    else
        seed!(rng, SeedHasher(seed))
    end
end


### hashseed!()

"""
    Random.hashseed!(ctx::SeedHasher, seed)

Update `ctx` via `ingest!` with the content of `seed`.
This function is used by the [`SeedHasher`](@ref) RNG to produce
random bytes.

`seed` can currently be of type
`Union{Integer, AbstractString, AbstractArray{UInt32}, AbstractArray{UInt64}}`,
but modules can extend this function for types they own.

`hashseed!` is "injective" : for two equivalent context objects `cn` and `cm`,
if `n != m`, then `cn` and `cm` will be distinct after calling
`hashseed!(cn, n); hashseed!(cm, m)`.
Moreover, if `n == m`, then `cn` and `cm` remain equivalent after calling
`hashseed!(cn, n); hashseed!(cm, m)`.
"""
function hashseed!! end

function hashseed!(ctx::SeedHasher, seed::Integer)
    neg = signbit(seed)
    if neg
        seed = ~seed
    end
    @assert seed >= 0
    while true
        word = (seed % UInt32) & 0xffffffff
        seed >>>= 32
        ingest!(ctx, reinterpret(NTuple{4, UInt8}, word))
        iszero(seed) && break
    end
    # make sure the hash of negative numbers is different from the hash of positive numbers
    neg && ingest!(ctx, (0x01,))
    nothing
end

function hashseed!(ctx::SeedHasher, seed::Union{AbstractArray{UInt32}, AbstractArray{UInt64}})
    for xx in seed
        ingest!(ctx, reinterpret(NTuple{8, UInt8}, UInt64(xx)))
    end
    # discriminate from hashseed!(ctx, ::Integer)
    ingest!(ctx, (0x10,))
end

function hashseed!(ctx::SeedHasher, str::AbstractString)
    # convert to String such that `codeunits(str)` below is consistent between equal
    # strings of different types
    str = String(str)
    ingest!(ctx, codeunits(str))
    # signature for strings: so far, all hashseed! functions end-up hashing a multiple
    # of 4 bytes of data, and add the signature (1 byte) at the end; so hash as many
    # bytes as necessary to have a total number of hashed bytes equal to 0 mod 4 (padding),
    # and then hash the signature 0x05; in order for strings of different lengths to have
    # different hashes, padding bytes are set equal to the number of padding bytes
    pad = 4 - mod(ncodeunits(str), 4)
    for _=1:pad
        ingest!(ctx, (pad % UInt8,))
    end
    ingest!(ctx, (0x05,))
end
