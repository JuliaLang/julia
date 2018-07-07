# Random Numbers

```@meta
DocTestSetup = :(using Random)
```

Random number generation in Julia uses the [Mersenne Twister library](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/#dSFMT)
via `MersenneTwister` objects. Julia has a global RNG, which is used by default. Other RNG types
can be plugged in by inheriting the `AbstractRNG` type; they can then be used to have multiple
streams of random numbers. Besides `MersenneTwister`, Julia also provides the `RandomDevice` RNG
type, which is a wrapper over the OS provided entropy.

Most functions related to random generation accept an optional `AbstractRNG` object as first argument,
which defaults to the global one if not provided. Moreover, some of them accept optionally
dimension specifications `dims...` (which can be given as a tuple) to generate arrays of random
values.

A `MersenneTwister` or `RandomDevice` RNG can generate uniformly random numbers of the following types:
[`Float16`](@ref), [`Float32`](@ref), [`Float64`](@ref), [`BigFloat`](@ref), [`Bool`](@ref),
[`Int8`](@ref), [`UInt8`](@ref), [`Int16`](@ref), [`UInt16`](@ref), [`Int32`](@ref),
[`UInt32`](@ref), [`Int64`](@ref), [`UInt64`](@ref), [`Int128`](@ref), [`UInt128`](@ref),
[`BigInt`](@ref) (or complex numbers of those types).
Random floating point numbers are generated uniformly in ``[0, 1)``. As `BigInt` represents
unbounded integers, the interval must be specified (e.g. `rand(big.(1:6))`).

Additionally, normal and exponential distributions are implemented for some `AbstractFloat` and
`Complex` types, see [`randn`](@ref) and [`randexp`](@ref) for details.


## Random generation functions

```@docs
Random.rand
Random.rand!
Random.bitrand
Random.randn
Random.randn!
Random.randexp
Random.randexp!
Random.randstring
```

## Subsequences, permutations and shuffling

```@docs
Random.randsubseq
Random.randsubseq!
Random.randperm
Random.randperm!
Random.randcycle
Random.randcycle!
Random.shuffle
Random.shuffle!
```

## Generators

```@docs
Random.srand
Random.MersenneTwister
Random.RandomDevice
```

## Hooking into the `Random` API

There are two orthogonal ways to extend `Random` functionalities:
1) generating random values of custom types
2) creating new generators

The API for 1) is quite functional, but is relatively recent so it may still have to evolve in subsequent releases of the `Random` module.
For example, it's typically sufficient to implement one `rand` method in order to have all other usual methods work automatically.

The API for 2) is close to non-existent, and may require more work than strictly necessary from the implementor, in order to support standard types of generated values.

### Generating random values of custom types

There are two categories: generating values from a type (e.g. `rand(Int)`), or from a collection (e.g. `rand(1:3)`).
The simple cases are explained first, and more advanced usage is presented later.

#### generating values from a type

Given a type `T`, it's currently assumed that if `rand(T)` is defined, an object of type `T` will be produced.
In order to define `rand(T)`, the following method can be defined:
`rand(rng::AbstractRNG, ::Random.SamplerType{T})`.

Let's take the following example: we implement a `Die` type, with a variable number `n` of sides, numbered from `1` to `n`.
We want `rand(Die)` to produce a die with a random number of up to 20 sides (and at least 4):

```julia
struct Die
    nsides::Int # number of sides
end

Random.rand(rng::AbstractRNG, ::Random.SamplerType{Die}) = Die(rand(rng, 4:20))
```

Scalar and array methods for `Die` now work as expected:

```julia-repl
julia> rand(Die)
Die(13)

julia> rand(MersenneTwister(0), Die)
Die(4)

julia> rand(Die, 3)
3-element Array{Die,1}:
 Die(7)
 Die(18)
 Die(11)

julia> a = Vector{Die}(undef, 3); rand!(a)
3-element Array{Die,1}:
 Die(3)
 Die(5)
 Die(17)
```

#### generating values from a collection

Given a collection type `T`, it's currently assumed that if `rand(::T)` is defined, an object of type `eltype(T)` will be produced.
In order to define `rand(::T)`, the following method can be defined:
`rand(rng::AbstractRNG, sp::Random.SamplerTrivial{T})`. There, `sp` simply wraps an object of type `T`, which can be accessed via `sp[]`.
Continuing the `Die` example, we want now to define `rand(d::Die)` to produce an `Int` corresponding to one of `d`'s sides:

```julia-repl
julia> Random.rand(rng::AbstractRNG, d::SamplerTrivial{Die}) = rand(rng, 1:d[].nsides);

julia> rand(Die(4))
2

julia> rand(Die(4), 3)

3-element Array{Any,1}:
 3
 4
 2
```

In the last example, a `Vector{Any}` is produced; the reason is that `eltype(Die) == Any`. The remedy is to define
`Base.eltype(::Type{Die}) = Int`.


#### generating values for an `AbstractFloat` type

`AbstractFloat` types are special-cased, because by default random values are not produced in the whole type domain, but rather
in `[0,1)`. The following method should be implemented for `T <: AbstractFloat`:
`Random.rand(::AbstractRNG, ::SamplerTrivial{CloseOpen01{T}})`


#### optimizing generation with cached computation between calls

When repeatedly generating random values (with the same `rand` parameters), it happens for some types
that the result of a computation is used for each call. In this case, the computation can be decoupled
from actually generating the values. This is the case for example with the default implementation for
`AbstractArray`. Assume that `rand(1:20)` has to be called repeatedly in a loop: the way to take advantage
of this decoupling is as follows:

```julia
rng = MersenneTwister()
sp = Random.Sampler(rng, 1:20)
for x in X
    n = rand(rng, sp) # similar to n = rand(rng, 1:20)
    # use n
end
```

This is of course used by the default implementation of random array generation (like in `rand(1:20, 10)`).
In order to implement this decoupling for a custom type, a helper type can be used.
Going back to our `Die` example: `rand(::Die)` uses random generation from a range, so
this could take advantage of this optimization opportunity.

```julia
import Random: Sampler, rand

struct SamplerDie <: Sampler{Int} # generates values of type Int
    die::Die
    sp::Sampler{Int} # this is an abstract type, so this could be improved
end

Sampler(rng::AbstractRNG, die::Die, v::Random.Repetition) =
    SamplerDie(die, Sampler(rng, 1:die.nsides, v))
# the `v` parameter will be explained later on

rand(rng::AbstractRNG, sp::SamplerDie) = rand(rng, sp.sp)
```

It's now possible to get a sampler with `sp = Sampler(rng, die)`, and use `sp` instead of `die` in any `rand` call
involving `rng`.
In the simplistic example above, `die` doesn't need to be stored in `SamplerDie` but this is the case in practice.

This pattern is so frequent that a helper type named `Random.SamplerSimple` is available: we could have implemented
our decoupling with:

```julia
Sampler(rng::AbstractRNG, die::Die, v::Random.Repetition) =
    SamplerSimple(die, Sampler(rng, 1:die.nsides, v))

rand(rng::AbstractRNG, sp::SamplerSimple{Die}) = rand(rng, sp.data)
```

Here, `sp.data` refers to the second parameter in the call to the `SamplerSimple` constructor
(here equal to `Sampler(rng, 1:die.nsides, v)`), while the `Die` object can be accessed
via `sp[]`.

Another helper type is currently available for slighlty more complex cases, `Random.SamplerTag`, but is
considered as internal API, and can break at any time without proper deprecations.

#### using distinct algorithms for scalar or array generation

In some cases, whether one want to generate only a handful of values or a large number of values
will have an impact on the choice of algorithm. This is handled with the third parameter of the
`Sampler` constructor. Let's assume we defined two helper types for `Die`, say `SamplerDie1`
which should be used to generate only few random values, and `SamplerDieMany` for the many values.
We can use those types as follows:

```julia
Sampler(rng::AbstractRNG, die::Die, ::Val{1}) = SamplerDie1(...)
Sampler(rng::AbstractRNG, die::Die, ::Val{Inf}) = SamplerDieMany(...)
```

Of course, `rand` must also be defined on those types (i.e. `rand(::AbstractRNG, ::SamplerDie1)`
and `rand(::AbstractRNG, ::SamplerDieMany)`).

Note: `Sampler(rng, x)` is simply a shorthand for `Sampler(rng, x, Val(Inf))`, and
`Random.Repetition` is an alias for `Union{Val{1}, Val{Inf}}`.


### creating new generators

The API is not clearly defined yet, but as a rule of thumb:
1) any `rand` method producing basic type (`isbitstype` integer and floating types in `Base`)
   may have to be defined;
2) other documented `rand` methods accepting an `AbstractRNG` should work out of the box.

Concerning 1), a `rand` method may happen to work automatically, but it's not officially
supported and may break without warnings in a subsequent release.

To define a new `rand` method for an hypothetical `MyRNG` generator, and a value specification
of `s`
(e.g. `s == Int`, or `s == 1:10`) of type `S==typeof(s)` or `S==Type{s}` if `s` is a type, two methods must be defined:
1) `Sampler(::MyRNG, ::S, ::Repetition)`, which returns an object of type say `SamplerS`
2) `rand(rng::MyRNG, sp::SamplerS)`

In many cases, `Sampler(rng::AbstractRNG, ::S, ::Repetition)` will already be
defined in the `Random` module. It would then be possible to skip step 1)
in practice, but the corresponding `SamplerS` type is considered as
internal detail, and may change without warning.


#### specializing array generation

In some cases, for a given RNG type, generating an array of random
values can be more efficient with a specialized method than by merely
using the decoupling technique explained before. This is for example
the case for `MersenneTwister`, which natively writes random values in
an array.

To implement this specialization for `MyRNG`
and for a specification `s`, it is first required to know the return type of
`Sampler(::MyRNG, s, Val(Inf))`, say `SamplerType{s}` (assuming `s` is
a type); then define `rand!(rng::MyRNG, a::AbstractArray{s},
::SamplerType{s})`. Instead of `AbstractArray`, it's possible to
implement the functionality only for a subtype, e.g. `Array{s}`. In
case `s` is a collection, the `eltype` of `a` should be equal to that
of `s` (i.e. `a::AbstractArray{eltype(s)}`). The non-mutating array
method of `rand` will automatically call this specialization
internally.

```@meta
DocTestSetup = :(using Random)
```
