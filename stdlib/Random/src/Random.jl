# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

module Random

include("DSFMT.jl")

using .DSFMT
using Base.GMP.MPZ
using Base.GMP: Limb

using Base: BitInteger, BitInteger_types, BitUnsigned

import Base: copymutable, copy, copy!, ==, hash, convert
using Serialization
import Serialization: serialize, deserialize
import Base: rand, randn

export srand,
       rand!, randn!,
       randexp, randexp!,
       bitrand,
       randstring,
       randsubseq, randsubseq!,
       shuffle, shuffle!,
       randperm, randperm!,
       randcycle, randcycle!,
       AbstractRNG, MersenneTwister, RandomDevice,
       randjump


## general definitions

abstract type AbstractRNG end


### integers

# we define types which encode the generation of a specific number of bits
# the "raw" version means that the unused bits are not zeroed

abstract type UniformBits{T<:BitInteger} end

struct UInt10{T}    <: UniformBits{T} end
struct UInt10Raw{T} <: UniformBits{T} end

struct UInt23{T}    <: UniformBits{T} end
struct UInt23Raw{T} <: UniformBits{T} end

struct UInt52{T}    <: UniformBits{T} end
struct UInt52Raw{T} <: UniformBits{T} end

struct UInt104{T}    <: UniformBits{T} end
struct UInt104Raw{T} <: UniformBits{T} end

struct UInt2x52{T}    <: UniformBits{T} end
struct UInt2x52Raw{T} <: UniformBits{T} end

uint_sup(::Type{<:Union{UInt10,UInt10Raw}}) = UInt16
uint_sup(::Type{<:Union{UInt23,UInt23Raw}}) = UInt32
uint_sup(::Type{<:Union{UInt52,UInt52Raw}}) = UInt64
uint_sup(::Type{<:Union{UInt104,UInt104Raw}}) = UInt128
uint_sup(::Type{<:Union{UInt2x52,UInt2x52Raw}}) = UInt128

for UI = (:UInt10, :UInt10Raw, :UInt23, :UInt23Raw, :UInt52, :UInt52Raw,
          :UInt104, :UInt104Raw, :UInt2x52, :UInt2x52Raw)
    @eval begin
        $UI(::Type{T}=uint_sup($UI)) where {T} = $UI{T}()
        # useful for defining rand generically:
        uint_default(::$UI) = $UI{uint_sup($UI)}()
    end
end


### floats

abstract type FloatInterval{T<:AbstractFloat} end

struct CloseOpen01{T<:AbstractFloat} <: FloatInterval{T} end # interval [0,1)
struct CloseOpen12{T<:AbstractFloat} <: FloatInterval{T} end # interval [1,2)

const FloatInterval_64 = FloatInterval{Float64}
const CloseOpen01_64   = CloseOpen01{Float64}
const CloseOpen12_64   = CloseOpen12{Float64}

CloseOpen01(::Type{T}=Float64) where {T<:AbstractFloat} = CloseOpen01{T}()
CloseOpen12(::Type{T}=Float64) where {T<:AbstractFloat} = CloseOpen12{T}()

Base.eltype(::Type{<:FloatInterval{T}}) where {T<:AbstractFloat} = T

const BitFloatType = Union{Type{Float16},Type{Float32},Type{Float64}}

### Sampler

abstract type Sampler{E} end

Base.eltype(::Type{<:Sampler{E}}) where {E} = E

# temporarily for BaseBenchmarks
RangeGenerator(x) = Sampler(GLOBAL_RNG, x)

# In some cases, when only 1 random value is to be generated,
# the optimal sampler can be different than if multiple values
# have to be generated. Hence a `Repetition` parameter is used
# to choose the best one depending on the need.
const Repetition = Union{Val{1},Val{Inf}}

# these default fall-back for all RNGs would be nice,
# but generate difficult-to-solve ambiguities
# Sampler(::AbstractRNG, X, ::Val{Inf}) = Sampler(X)
# Sampler(::AbstractRNG, ::Type{X}, ::Val{Inf}) where {X} = Sampler(X)

Sampler(rng::AbstractRNG, sp::Sampler, ::Repetition) =
    throw(ArgumentError("Sampler for this object is not defined"))

# default shortcut for the general case
Sampler(rng::AbstractRNG, X) = Sampler(rng, X, Val(Inf))
Sampler(rng::AbstractRNG, ::Type{X}) where {X} = Sampler(rng, X, Val(Inf))

#### pre-defined useful Sampler types

# default fall-back for types
struct SamplerType{T} <: Sampler{T} end

Sampler(::AbstractRNG, ::Type{T}, ::Repetition) where {T} = SamplerType{T}()

Base.getindex(::SamplerType{T}) where {T} = T

# default fall-back for values
struct SamplerTrivial{T,E} <: Sampler{E}
    self::T
end

SamplerTrivial(x::T) where {T} = SamplerTrivial{T,eltype(T)}(x)

Sampler(::AbstractRNG, x, ::Repetition) = SamplerTrivial(x)

Base.getindex(sp::SamplerTrivial) = sp.self

# simple sampler carrying data (which can be anything)
struct SamplerSimple{T,S,E} <: Sampler{E}
    self::T
    data::S
end

SamplerSimple(x::T, data::S) where {T,S} = SamplerSimple{T,S,eltype(T)}(x, data)

Base.getindex(sp::SamplerSimple) = sp.self

# simple sampler carrying a (type) tag T and data
struct SamplerTag{T,S,E} <: Sampler{E}
    data::S
    SamplerTag{T}(s::S) where {T,S} = new{T,S,eltype(T)}(s)
end


#### helper samplers

# TODO: make constraining constructors to enforce that those
# types are <: Sampler{T}

##### Adapter to generate a randome value in [0, n]

struct LessThan{T<:Integer,S} <: Sampler{T}
    sup::T
    s::S    # the scalar specification/sampler to feed to rand
end

function rand(rng::AbstractRNG, sp::LessThan)
    while true
        x = rand(rng, sp.s)
        x <= sp.sup && return x
    end
end

struct Masked{T<:Integer,S} <: Sampler{T}
    mask::T
    s::S
end

rand(rng::AbstractRNG, sp::Masked) = rand(rng, sp.s) & sp.mask

##### Uniform

struct UniformT{T} <: Sampler{T} end

uniform(::Type{T}) where {T} = UniformT{T}()

rand(rng::AbstractRNG, ::UniformT{T}) where {T} = rand(rng, T)


### machinery for generation with Sampler

# This describes how to generate random scalars or arrays, by generating a Sampler
# and calling rand on it (which should be defined in "generation.jl").
# NOTE: this section could be moved into a separate file when more containers are supported.

#### scalars

rand(rng::AbstractRNG, X)                                      = rand(rng, Sampler(rng, X, Val(1)))
rand(rng::AbstractRNG=GLOBAL_RNG, ::Type{X}=Float64) where {X} = rand(rng, Sampler(rng, X, Val(1)))

rand(X)                   = rand(GLOBAL_RNG, X)
rand(::Type{X}) where {X} = rand(GLOBAL_RNG, X)

#### arrays

rand!(A::AbstractArray{T}, X) where {T}             = rand!(GLOBAL_RNG, A, X)
rand!(A::AbstractArray{T}, ::Type{X}=T) where {T,X} = rand!(GLOBAL_RNG, A, X)

rand!(rng::AbstractRNG, A::AbstractArray{T}, X) where {T}             = rand!(rng, A, Sampler(rng, X))
rand!(rng::AbstractRNG, A::AbstractArray{T}, ::Type{X}=T) where {T,X} = rand!(rng, A, Sampler(rng, X))

function rand!(rng::AbstractRNG, A::AbstractArray{T}, sp::Sampler) where T
    for i in eachindex(A)
        @inbounds A[i] = rand(rng, sp)
    end
    A
end

rand(r::AbstractRNG, dims::Integer...) = rand(r, Float64, Dims(dims))
rand(                dims::Integer...) = rand(Float64, Dims(dims))

rand(r::AbstractRNG, X, dims::Dims)  = rand!(r, Array{eltype(X)}(undef, dims), X)
rand(                X, dims::Dims)  = rand(GLOBAL_RNG, X, dims)

rand(r::AbstractRNG, X, d::Integer, dims::Integer...) = rand(r, X, Dims((d, dims...)))
rand(                X, d::Integer, dims::Integer...) = rand(X, Dims((d, dims...)))
# note: the above methods would trigger an ambiguity warning if d was not separated out:
# rand(r, ()) would match both this method and rand(r, dims::Dims)
# moreover, a call like rand(r, NotImplementedType()) would be an infinite loop

rand(r::AbstractRNG, ::Type{X}, dims::Dims) where {X} = rand!(r, Array{eltype(X)}(undef, dims), X)
rand(                ::Type{X}, dims::Dims) where {X} = rand(GLOBAL_RNG, X, dims)

rand(r::AbstractRNG, ::Type{X}, d::Integer, dims::Integer...) where {X} = rand(r, X, Dims((d, dims...)))
rand(                ::Type{X}, d::Integer, dims::Integer...) where {X} = rand(X, Dims((d, dims...)))


## __init__ & include

function __init__()
    try
        srand()
    catch ex
        Base.showerror_nostdio(ex,
            "WARNING: Error during initialization of module Random")
    end
end

include("RNGs.jl")
include("generation.jl")
include("normal.jl")
include("misc.jl")
include("deprecated.jl")

## rand & rand! & srand docstrings

"""
    rand([rng=GLOBAL_RNG], [S], [dims...])

Pick a random element or array of random elements from the set of values specified by `S`;
`S` can be

* an indexable collection (for example `1:n` or `['x','y','z']`),
* an `AbstractDict` or `AbstractSet` object,
* a string (considered as a collection of characters), or
* a type: the set of values to pick from is then equivalent to `typemin(S):typemax(S)` for
  integers (this is not applicable to [`BigInt`](@ref)), and to ``[0, 1)`` for floating
  point numbers;

`S` defaults to [`Float64`](@ref)
(except when `dims` is a tuple of integers, in which case `S` must be specified).

# Examples
```julia-repl
julia> rand(Int, 2)
2-element Array{Int64,1}:
 1339893410598768192
 1575814717733606317

julia> rand(MersenneTwister(0), Dict(1=>2, 3=>4))
1=>2
```

!!! note
    The complexity of `rand(rng, s::Union{AbstractDict,AbstractSet})`
    is linear in the length of `s`, unless an optimized method with
    constant complexity is available, which is the case for `Dict`,
    `Set` and `BitSet`. For more than a few calls, use `rand(rng,
    collect(s))` instead, or either `rand(rng, Dict(s))` or `rand(rng,
    Set(s))` as appropriate.
"""
rand

"""
    rand!([rng=GLOBAL_RNG], A, [S=eltype(A)])

Populate the array `A` with random values. If `S` is specified
(`S` can be a type or a collection, cf. [`rand`](@ref) for details),
the values are picked randomly from `S`.
This is equivalent to `copyto!(A, rand(rng, S, size(A)))`
but without allocating a new array.

# Examples
```jldoctest
julia> rng = MersenneTwister(1234);

julia> rand!(rng, zeros(5))
5-element Array{Float64,1}:
 0.5908446386657102
 0.7667970365022592
 0.5662374165061859
 0.4600853424625171
 0.7940257103317943
```
"""
rand!

"""
    srand([rng=GLOBAL_RNG], seed) -> rng
    srand([rng=GLOBAL_RNG]) -> rng

Reseed the random number generator: `rng` will give a reproducible
sequence of numbers if and only if a `seed` is provided. Some RNGs
don't accept a seed, like `RandomDevice`.
After the call to `srand`, `rng` is equivalent to a newly created
object initialized with the same seed.

# Examples
```julia-repl
julia> srand(1234);

julia> x1 = rand(2)
2-element Array{Float64,1}:
 0.590845
 0.766797

julia> srand(1234);

julia> x2 = rand(2)
2-element Array{Float64,1}:
 0.590845
 0.766797

julia> x1 == x2
true

julia> rng = MersenneTwister(1234); rand(rng, 2) == x1
true

julia> MersenneTwister(1) == srand(rng, 1)
true

julia> rand(srand(rng), Bool) # not reproducible
true

julia> rand(srand(rng), Bool)
false

julia> rand(MersenneTwister(), Bool) # not reproducible either
true
```
"""
srand(rng::AbstractRNG, ::Nothing) = srand(rng)

end # module
