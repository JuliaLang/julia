# This file is a part of Julia. License is MIT: https://julialang.org/license

## numeric/object traits

"""
    OrderedStyle()
 
An abstract type of a trait that signals if some type `T` guarantees
that a call `isless(::T, ::T)` will return `true` or `false`.

It has two concrete subtypes [`Ordered`](@ref) and
[`Unordered`](@ref), which is the default.

Types supporting [`isless`](@ref) are recommended to support
[`Ordered`](@ref) trait to enable optimizations.
"""
abstract type OrderStyle end

"""
    Ordered()

Indicate that a type `T` guarantees that a call `isless(::T, ::T)` will return `true` or `false`.
It is recommended that custom types defining `isless` implement this
trait as this information can be used to enable optimizations.
"""
struct Ordered <: OrderStyle end

"""
    Unordered()

A default `OrderStyle` for any type `T` indicating that optimizations
cannot rely on the fact that it is guaranteed that a call `isless(::T, ::T)`
will return `true` or `false`.
"""
struct Unordered <: OrderStyle end

OrderStyle(instance) = OrderStyle(typeof(instance))
OrderStyle(::Type{Union{}}) = Ordered()
OrderStyle(::Type{<:Union{Missing,Real}}) = Ordered()
OrderStyle(::Type{<:Union{Missing,AbstractString}}) = Ordered()
OrderStyle(::Type{<:Union{Missing,AbstractChar}}) = Ordered()
OrderStyle(::Type{<:Union{Missing,Symbol}}) = Ordered()
OrderStyle(::Type{<:Missing}) = Ordered()
OrderStyle(::Type{<:Any}) = Unordered()

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
