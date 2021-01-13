# This file is a part of Julia. License is MIT: https://julialang.org/license

## numeric/object traits
# trait for objects that have an ordering
abstract type OrderStyle end
struct Ordered <: OrderStyle end
struct Unordered <: OrderStyle end

OrderStyle(instance) = OrderStyle(typeof(instance))
OrderStyle(::Type{<:Real}) = Ordered()
OrderStyle(::Type{<:AbstractString}) = Ordered()
OrderStyle(::Type{Symbol}) = Ordered()
OrderStyle(::Type{<:Any}) = Unordered()
OrderStyle(::Type{Union{}}) = Ordered()

# trait for objects that support arithmetic
abstract type ArithmeticStyle end
struct ArithmeticRounds <: ArithmeticStyle end     # least significant bits can be lost
struct ArithmeticWraps <: ArithmeticStyle end      #  most significant bits can be lost
struct ArithmeticUnknown <: ArithmeticStyle end

ArithmeticStyle(instance) = ArithmeticStyle(typeof(instance))
ArithmeticStyle(::Type{<:AbstractFloat}) = ArithmeticRounds()
ArithmeticStyle(::Type{<:Integer}) = ArithmeticWraps()
ArithmeticStyle(::Type{<:Any}) = ArithmeticUnknown()

# trait for objects that support ranges with regular step
"""
    RangeStepStyle(instance)
    RangeStepStyle(T::Type)

Indicate whether an instance or a type supports constructing a range with
a perfectly regular step or not. A regular step means that
[`step`](@ref) will always be exactly equal to the difference between two
subsequent elements in a range, i.e. for a range `r::AbstractRange{T}`:
```julia
all(diff(r) .== step(r))
```

When a type `T` always leads to ranges with regular steps, it should
define the following method:
```julia
Base.RangeStepStyle(::Type{<:AbstractRange{<:T}}) = Base.RangeStepRegular()
```
This will allow [`hash`](@ref) to use an O(1) algorithm for `AbstractRange{T}`
objects instead of the default O(N) algorithm (with N the length of the range).

In some cases, whether the step will be regular depends not only on the
element type `T`, but also on the type of the step `S`. In that case, more
specific methods should be defined:
```julia
Base.RangeStepStyle(::Type{<:OrdinalRange{<:T, <:S}}) = Base.RangeStepRegular()
```

By default, all range types are assumed to be `RangeStepIrregular`, except
ranges with an element type which is a subtype of `Integer`.
"""
abstract type RangeStepStyle end
struct RangeStepRegular   <: RangeStepStyle end # range with regular step
struct RangeStepIrregular <: RangeStepStyle end # range with rounding error

RangeStepStyle(instance) = RangeStepStyle(typeof(instance))

# trait for objects that support eachindex
"""
    EachIndexSupport(T::Type) -> EachIndexSupport

Given a type, return on of the following values:

* `HasEachIndex()` if `eachindex(x)` returns for some `x::T`.
* `NoEachIndex()` otherwise

The default value (for types that do not define this function) is `NotEachIndex()`.

```jldoctest
julia> Base.EachIndexSupport([1,2])
Base.HasEachIndex()

julia> Base.EachIndexSupport(:x)
Base.NoEachIndex()

```
"""
abstract type EachIndexSupport end
struct HasEachIndex <: EachIndexSupport end
struct NoEachIndex <: EachIndexSupport end

EachIndexSupport(x) = EachIndexSupport(typeof(x))
EachIndexSupport(::Type) = NoEachIndex()
EachIndexSupport(::Type{<:AbstractArray}) = HasEachIndex()
EachIndexSupport(::Type{<:AbstractString}) = HasEachIndex()
EachIndexSupport(::Type{<:AbstractDict}) = HasEachIndex()
EachIndexSupport(::Type{<:Tuple}) = HasEachIndex()
EachIndexSupport(::Type{<:NamedTuple}) = HasEachIndex()
EachIndexSupport(::Type{<:Number}) = HasEachIndex()
EachIndexSupport(::Type{<:IO}) = HasEachIndex()
EachIndexSupport(::Type{<:Core.SimpleVector}) = HasEachIndex()
EachIndexSupport(::Type{<:Generator}) = HasEachIndex()

"""
    eachindextype(x) -> Union{Type,Missing}

    Given an instance `x`, return the type of `eachindex(x)` if its type's `Base.EachIndexSupport(T)` is `Base.HasEachIndex()`, otherwise return `missing`.

```jldoctest
julia> Base.eachindextype([1,2,3])
Base.OneTo{$Int}

julia> Base.eachindextype("abc")
Base.EachStringIndex{String}

julia> Base.eachindextype(Dict(1=>'a', 2=>'b'))
Base.KeySet{$Int, Dict{$Int, Char}}

julia> Base.eachindextype(:symbol)
missing

```
"""
eachindextype(x) = eachindextype(x, EachIndexSupport(x))
eachindextype(x, ::HasEachIndex) = typeof(eachindex(x))
eachindextype(x, ::NoEachIndex) = missing

# trait for objects that support prevind and nextind
abstract type AdjacentIndexSupport end
struct NoAdjacentIndex <: AdjacentIndexSupport end
struct HasNextInd <: AdjacentIndexSupport end
struct HasPrevInd <: AdjacentIndexSupport end
struct HasAdjacentIndex <: AdjacentIndexSupport end

"""
    AdjacentIndexSupport(T::Type) -> AdjacentIndexSupport

Given a type, return one of the following values:

* `NoAdjacentIndex()` if neither `prevind` or `nextind` are defined for `T`.
* `HasNextInd()` if `nextind(x,i)` returns for some `x::T` and index `i`.
* `HasPrevInd()` if `prevind(x,i)` returns for some `x::T` and index `i`.
* `HasAdjacentIndex()` if both `prevind` and `nextind` are defined for `T`.

The default value (for types that do not define this function) is `NoAdjacentIndex()`.

```jldoctest
julia> Base.AdjacentIndexSupport([1,2])
Base.HasAdjacentIndex()

julia> Base.AdjacentIndexSupport(:x)
Base.NoAdjacentIndex()

```
"""
AdjacentIndexSupport(x) = AdjacentIndexSupport(typeof(x))
AdjacentIndexSupport(::Type) = NoAdjacentIndex()
AdjacentIndexSupport(::Type{<:AbstractArray}) = HasAdjacentIndex()
AdjacentIndexSupport(::Type{<:AbstractString}) = HasAdjacentIndex()
AdjacentIndexSupport(::Type{<:Tuple}) = HasAdjacentIndex()
AdjacentIndexSupport(::Type{<:NamedTuple}) = HasAdjacentIndex()
