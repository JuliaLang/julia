# This file is a part of Julia. License is MIT: https://julialang.org/license

## numeric/object traits
# trait for objects that have an ordering
abstract type OrderStyle end
struct Ordered <: OrderStyle end
struct Unordered <: OrderStyle end

OrderStyle(instance) = OrderStyle(typeof(instance))
OrderStyle(::Type{<:Real}) = Ordered()
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

# trait for diff-based hashing, O(1) for ranges with regular step
"""
    ArrayHashingStyle(instance)
    ArrayHashingStyle(T::Type)

Indicate whether an instance or a type supports hashing an `AbstractArray`
by computing the difference between subsequent elements (`ArrayHashingDiff`)
or not (`ArrayHashingDefault`). This allows [`hash](@ref) to use an O(1)
hashing algorithm for [`RangeStepRegular`](@ref RangeStepStyle) ranges,
but requires `T` to implement [`widen`](@ref) in order to compute
the difference without any risk of overflow (only needed for hashing
heterogeneous arrays).

Note that all types `S` and `T` for which `isequal(::S, ::T)` can return `true`
are required to use the same `ArrayHashingStyle`: else, equal arrays containing
such elements would not hash to the same value.

By default, only `Number` types are assumed to be `ArrayHashingDiff`, and
therefore need to implement `widen` to support arrays hashing in all cases.
"""
abstract type ArrayHashingStyle end
struct ArrayHashingDefault <: RangeStepStyle end # range with regular step
struct ArrayHashingDiff    <: RangeStepStyle end # range with rounding error

ArrayHashingStyle(instance) = ArrayHashingStyle(typeof(instance))

ArrayHashingStyle(::Type{<:Number}) = ArrayHashingDiff()
ArrayHashingStyle(::Type) = ArrayHashingDefault()
