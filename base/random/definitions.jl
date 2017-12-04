# This file is a part of Julia. License is MIT: https://julialang.org/license

# general definitions

abstract type AbstractRNG end

## Distributions

abstract type Distribution{T} end

Base.eltype(::Type{Distribution{T}}) where {T} = T

abstract type Combine{T} <: Distribution{T} end

struct Combine0{T} <: Combine{T} end

Combine(::Type{T}) where {T} = Combine0{T}()

struct Combine1{T,X} <: Combine{T}
    x::X
end

Combine(::Type{T}, x::X) where {T,X} = Combine1{T,X}(x)
Combine(::Type{T}, ::Type{X}) where {T,X} = Combine1{T,Type{X}}(X)

struct Combine2{T,X,Y} <: Combine{T}
    x::X
    y::Y
end

Combine(::Type{T}, x::X, y::Y) where {T,X,Y} = Combine2{deduce_type(T,X,Y),X,Y}(x, y)
Combine(::Type{T}, ::Type{X}, y::Y) where {T,X,Y} = Combine2{deduce_type(T,X,Y),Type{X},Y}(X, y)
Combine(::Type{T}, x::X, ::Type{Y}) where {T,X,Y} = Combine2{deduce_type(T,X,Y),X,Type{Y}}(x, Y)
Combine(::Type{T}, ::Type{X}, ::Type{Y}) where {T,X,Y} = Combine2{deduce_type(T,X,Y),Type{X},Type{Y}}(X, Y)

deduce_type(::Type{T}, ::Type{X}, ::Type{Y}) where {T,X,Y} = _deduce_type(T, Val(isconcrete(T)), eltype(X), eltype(Y))
deduce_type(::Type{T}, ::Type{X}) where {T,X} = _deduce_type(T, Val(isconcrete(T)), eltype(X))

_deduce_type(::Type{T}, ::Val{true},  ::Type{X}, ::Type{Y}) where {T,X,Y} = T
_deduce_type(::Type{T}, ::Val{false}, ::Type{X}, ::Type{Y}) where {T,X,Y} = deduce_type(T{X}, Y)

_deduce_type(::Type{T}, ::Val{true},  ::Type{X}) where {T,X} = T
_deduce_type(::Type{T}, ::Val{false}, ::Type{X}) where {T,X} = T{X}


### Uniform

abstract type Uniform{T} <: Distribution{T} end


struct UniformType{T} <: Uniform{T} end

Uniform(::Type{T}) where {T} = UniformType{T}()

Base.getindex(::UniformType{T}) where {T} = T

struct UniformWrap{T,E} <: Uniform{E}
    val::T
end

Uniform(x::T) where {T} = UniformWrap{T,eltype(T)}(x)

Base.getindex(x::UniformWrap) = x.val


### Normal & Exponential

abstract type Normal{T} <: Distribution{T} end

struct Normal01{T} <: Normal{T} end

struct Normalμσ{T} <: Normal{T}
    μ::T
    σ::T
end

Normal(::Type{T}=Float64) where {T} = Normal01{T}()
Normal(μ::T, σ::T) where {T} = Normalμσ(μ, σ)

abstract type Exponential{T} <: Distribution{T} end

struct Exponential1{T} <: Exponential{T} end

struct Exponentialθ{T} <: Exponential{T}
    θ::T
end

Exponential(::Type{T}=Float64) where {T<:AbstractFloat} = Exponential1{T}()
Exponential(θ::T) where {T<:AbstractFloat} = Exponentialθ(θ)


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

abstract type FloatInterval{T<:AbstractFloat} <: Uniform{T} end
abstract type CloseOpen{T<:AbstractFloat} <: FloatInterval{T} end

struct CloseOpen01{T<:AbstractFloat} <: CloseOpen{T} end # interval [0,1)
struct CloseOpen12{T<:AbstractFloat} <: CloseOpen{T} end # interval [1,2)

struct CloseOpenAB{T<:AbstractFloat} <: CloseOpen{T} # interval [a,b)
    a::T
    b::T
end

const FloatInterval_64 = FloatInterval{Float64}
const CloseOpen01_64   = CloseOpen01{Float64}
const CloseOpen12_64   = CloseOpen12{Float64}

CloseOpen01(::Type{T}=Float64) where {T<:AbstractFloat} = CloseOpen01{T}()
CloseOpen12(::Type{T}=Float64) where {T<:AbstractFloat} = CloseOpen12{T}()

CloseOpen(::Type{T}=Float64) where {T<:AbstractFloat} = CloseOpen01{T}()
CloseOpen(a::T, b::T) where {T<:AbstractFloat} = CloseOpenAB{T}(a, b)


Base.eltype(::Type{<:FloatInterval{T}}) where {T<:AbstractFloat} = T

const BitFloatType = Union{Type{Float16},Type{Float32},Type{Float64}}


## Sampler

abstract type Sampler{E} end

Base.eltype(::Sampler{E}) where {E} = E

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

### pre-defined useful Sampler types

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

# a dummy container type to take advangage of SamplerTag constructor

struct Cont{T} end

Base.eltype(::Type{Cont{T}}) where {T} = T

### helper samplers

# TODO: make constraining constructors to enforce that those
# types are <: Sampler{T}

#### Adapter to generate a randome value in [0, n]

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

#### Uniform

struct UniformT{T} <: Sampler{T} end

uniform(::Type{T}) where {T} = UniformT{T}()

rand(rng::AbstractRNG, ::UniformT{T}) where {T} = rand(rng, T)
