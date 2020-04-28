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

!!! warn
    Because the precise way in which random numbers are generated is considered an implementation detail, bug fixes and speed improvements may change the stream of numbers that are generated after a version change. Relying on a specific seed or generated stream of numbers during unit testing is thus discouraged - consider testing properties of the methods in question instead.

## Random numbers module
```@docs
Random.Random
```

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

## Generators (creation and seeding)

```@docs
Random.seed!
Random.AbstractRNG
Random.MersenneTwister
Random.RandomDevice
```

## Hooking into the `Random` API

There are two mostly orthogonal ways to extend `Random` functionalities:
1) generating random values of custom types
2) creating new generators

The API for 1) is quite functional, but is relatively recent so it may still have to evolve in subsequent releases of the `Random` module.
For example, it's typically sufficient to implement one `rand` method in order to have all other usual methods work automatically.

The API for 2) is still rudimentary, and may require more work than strictly necessary from the implementor,
in order to support usual types of generated values.

### Generating random values of custom types

Generating random values for some distributions may involve various trade-offs. *Pre-computed* values, such as an [alias table](https://en.wikipedia.org/wiki/Alias_method) for discrete distributions, or [“squeezing” functions](https://en.wikipedia.org/wiki/Rejection_sampling) for univariate distributions, can speed up sampling considerably. How much information should be pre-computed can depend on the number of values we plan to draw from a distribution. Also, some random number generators can have certain properties that various algorithms may want to exploit.

The `Random` module defines a customizable framework for obtaining random values that can address these issues. Each invocation of `rand` generates a *sampler* which can be customized with the above trade-offs in mind, by adding methods to `Sampler`, which in turn can dispatch on the random number generator, the object that characterizes the distribution, and a suggestion for the number of repetitions. Currently, for the latter, `Val{1}` (for a single sample) and `Val{Inf}` (for an arbitrary number) are used, with `Random.Repetition` an alias for both.

The object returned by `Sampler` is then used to generate the random values. When implementing the random generation interface for a value `X` that can be sampled from, the implementor should define the method

```julia
rand(rng, sampler)
```
for the particular `sampler` returned by `Sampler(rng, X, repetition)`.

Samplers can be arbitrary values that implement `rand(rng, sampler)`, but for most applications the following predefined samplers may be sufficient:

1. `SamplerType{T}()` can be used for implementing samplers that draw from type `T` (e.g. `rand(Int)`). This is the default returned by `Sampler` for *types*.

2. `SamplerTrivial(self)` is a simple wrapper for `self`, which can be accessed with `[]`. This is the recommended sampler when no pre-computed information is needed (e.g. `rand(1:3)`), and is the default returned by `Sampler` for *values*.

3. `SamplerSimple(self, data)` also contains the additional `data` field, which can be used to store arbitrary pre-computed values, which should be computed in a *custom method* of `Sampler`.

We provide examples for each of these. We assume here that the choice of algorithm is independent of the RNG, so we use `AbstractRNG` in our signatures.

```@docs
Random.Sampler
Random.SamplerType
Random.SamplerTrivial
Random.SamplerSimple
```

Decoupling pre-computation from actually generating the values is part of the API, and is also available to the user. As an example, assume that `rand(rng, 1:20)` has to be called repeatedly in a loop: the way to take advantage of this decoupling is as follows:

```julia
rng = MersenneTwister()
sp = Random.Sampler(rng, 1:20) # or Random.Sampler(MersenneTwister, 1:20)
for x in X
    n = rand(rng, sp) # similar to n = rand(rng, 1:20)
    # use n
end
```

This is the mechanism that is also used in the standard library, e.g. by the default implementation of random array generation (like in `rand(1:20, 10)`).

#### Generating values from a type

Given a type `T`, it's currently assumed that if `rand(T)` is defined, an object of type `T` will be produced. `SamplerType` is the *default sampler for types*. In order to define random generation of values of type `T`, the `rand(rng::AbstractRNG, ::Random.SamplerType{T})` method should be defined, and should return values what `rand(rng, T)` is expected to return.

Let's take the following example: we implement a `Die` type, with a variable number `n` of sides, numbered from `1` to `n`. We want `rand(Die)` to produce a `Die` with a random number of up to 20 sides (and at least 4):

```jldoctest Die
struct Die
    nsides::Int # number of sides
end

Random.rand(rng::AbstractRNG, ::Random.SamplerType{Die}) = Die(rand(rng, 4:20))

# output

```

Scalar and array methods for `Die` now work as expected:

```jldoctest Die; setup = :(Random.seed!(1))
julia> rand(Die)
Die(18)

julia> rand(MersenneTwister(0), Die)
Die(4)

julia> rand(Die, 3)
3-element Array{Die,1}:
 Die(6)
 Die(11)
 Die(5)

julia> a = Vector{Die}(undef, 3); rand!(a)
3-element Array{Die,1}:
 Die(18)
 Die(6)
 Die(8)
```

#### A simple sampler without pre-computed data

Here we define a sampler for a collection. If no pre-computed data is required, it can be implemented with a `SamplerTrivial` sampler, which is in fact the *default fallback for values*.

In order to define random generation out of objects of type `S`, the following method should be defined: `rand(rng::AbstractRNG, sp::Random.SamplerTrivial{S})`. Here, `sp` simply wraps an object of type `S`, which can be accessed via `sp[]`. Continuing the `Die` example, we want now to define `rand(d::Die)` to produce an `Int` corresponding to one of `d`'s sides:

```jldoctest Die; setup = :(Random.seed!(1))
julia> Random.rand(rng::AbstractRNG, d::Random.SamplerTrivial{Die}) = rand(rng, 1:d[].nsides);

julia> rand(Die(4))
3

julia> rand(Die(4), 3)
3-element Array{Any,1}:
 3
 4
 2
```

Given a collection type `S`, it's currently assumed that if `rand(::S)` is defined, an object of type `eltype(S)` will be produced. In the last example, a `Vector{Any}` is produced; the reason is that `eltype(Die) == Any`. The remedy is to define `Base.eltype(::Type{Die}) = Int`.

#### Generating values for an `AbstractFloat` type

`AbstractFloat` types are special-cased, because by default random values are not produced in the whole type domain, but rather in `[0,1)`. The following method should be implemented for `T <: AbstractFloat`: `Random.rand(::AbstractRNG, ::Random.SamplerTrivial{Random.CloseOpen01{T}})`

#### An optimized sampler with pre-computed data

Consider a discrete distribution, where numbers `1:n` are drawn with given probabilities that sum to one. When many values are needed from this distribution, the fastest method is using an [alias table](https://en.wikipedia.org/wiki/Alias_method). We don't provide the algorithm for building such a table here, but suppose it is available in `make_alias_table(probabilities)` instead, and `draw_number(rng, alias_table)` can be used to draw a random number from it.

Suppose that the distribution is described by
```julia
struct DiscreteDistribution{V <: AbstractVector}
    probabilities::V
end
```
and that we *always* want to build an alias table, regardless of the number of values needed (we learn how to customize this below). The methods
```julia
Random.eltype(::Type{<:DiscreteDistribution}) = Int

function Random.Sampler(::Type{<:AbstractRNG}, distribution::DiscreteDistribution, ::Repetition)
    SamplerSimple(disribution, make_alias_table(distribution.probabilities))
end
```
should be defined to return a sampler with pre-computed data, then
```julia
function rand(rng::AbstractRNG, sp::SamplerSimple{<:DiscreteDistribution})
    draw_number(rng, sp.data)
end
```
will be used to draw the values.

#### Custom sampler types

The `SamplerSimple` type is sufficient for most use cases with precomputed data. However, in order to demonstrate how to use custom sampler types, here we implement something similar to `SamplerSimple`.

Going back to our `Die` example: `rand(::Die)` uses random generation from a range, so there is an opportunity for this optimization. We call our custom sampler `SamplerDie`.

```julia
import Random: Sampler, rand

struct SamplerDie <: Sampler{Int} # generates values of type Int
    die::Die
    sp::Sampler{Int} # this is an abstract type, so this could be improved
end

Sampler(RNG::Type{<:AbstractRNG}, die::Die, r::Random.Repetition) =
    SamplerDie(die, Sampler(RNG, 1:die.nsides, r))
# the `r` parameter will be explained later on

rand(rng::AbstractRNG, sp::SamplerDie) = rand(rng, sp.sp)
```

It's now possible to get a sampler with `sp = Sampler(rng, die)`, and use `sp` instead of `die` in any `rand` call involving `rng`. In the simplistic example above, `die` doesn't need to be stored in `SamplerDie` but this is often the case in practice.

Of course, this pattern is so frequent that the helper type used above, namely `Random.SamplerSimple`, is available,
saving us the definition of `SamplerDie`: we could have implemented our decoupling with:

```julia
Sampler(RNG::Type{<:AbstractRNG}, die::Die, r::Random.Repetition) =
    SamplerSimple(die, Sampler(RNG, 1:die.nsides, r))

rand(rng::AbstractRNG, sp::SamplerSimple{Die}) = rand(rng, sp.data)
```

Here, `sp.data` refers to the second parameter in the call to the `SamplerSimple` constructor
(in this case equal to `Sampler(rng, 1:die.nsides, r)`), while the `Die` object can be accessed
via `sp[]`.

Like `SamplerDie`, any custom sampler must be a subtype of `Sampler{T}` where `T` is the type
of the generated values. Note that `SamplerSimple(x, data) isa Sampler{eltype(x)}`,
so this constrains what the first argument to `SamplerSimple` can be
(it's recommended to use `SamplerSimple` like in the `Die` example, where
`x` is simply forwarded while defining a `Sampler` method).
Similarly, `SamplerTrivial(x) isa Sampler{eltype(x)}`.

Another helper type is currently available for other cases, `Random.SamplerTag`, but is
considered as internal API, and can break at any time without proper deprecations.


#### Using distinct algorithms for scalar or array generation

In some cases, whether one wants to generate only a handful of values or a large number of values
will have an impact on the choice of algorithm. This is handled with the third parameter of the
`Sampler` constructor. Let's assume we defined two helper types for `Die`, say `SamplerDie1`
which should be used to generate only few random values, and `SamplerDieMany` for many values.
We can use those types as follows:

```julia
Sampler(RNG::Type{<:AbstractRNG}, die::Die, ::Val{1}) = SamplerDie1(...)
Sampler(RNG::Type{<:AbstractRNG}, die::Die, ::Val{Inf}) = SamplerDieMany(...)
```

Of course, `rand` must also be defined on those types (i.e. `rand(::AbstractRNG, ::SamplerDie1)` and `rand(::AbstractRNG, ::SamplerDieMany)`). Note that, as usual, `SamplerTrivial` and `SamplerSimple` can be used if custom types are not necessary.

Note: `Sampler(rng, x)` is simply a shorthand for `Sampler(rng, x, Val(Inf))`, and
`Random.Repetition` is an alias for `Union{Val{1}, Val{Inf}}`.


### Creating new generators

The API is not clearly defined yet, but as a rule of thumb:
1) any `rand` method producing "basic" types (`isbitstype` integer and floating types in `Base`)
   should be defined for this specific RNG, if they are needed;
2) other documented `rand` methods accepting an `AbstractRNG` should work out of the box,
   (provided the methods from 1) what are relied on are implemented),
   but can of course be specialized for this RNG if there is room for optimization;
3) `copy` for pseudo-RNGs should return an independent copy that generates the exact same random sequence as the
   original from that point when called in the same way. When this is not feasible (e.g. hardware-based RNGs),
   `copy` must not be implemented.

Concerning 1), a `rand` method may happen to work automatically, but it's not officially
supported and may break without warnings in a subsequent release.

To define a new `rand` method for an hypothetical `MyRNG` generator, and a value specification `s`
(e.g. `s == Int`, or `s == 1:10`) of type `S==typeof(s)` or `S==Type{s}` if `s` is a type,
the same two methods as we saw before must be defined:

1) `Sampler(::Type{MyRNG}, ::S, ::Repetition)`, which returns an object of type say `SamplerS`
2) `rand(rng::MyRNG, sp::SamplerS)`

It can happen that `Sampler(rng::AbstractRNG, ::S, ::Repetition)` is
already defined in the `Random` module. It would then be possible to
skip step 1) in practice (if one wants to specialize generation for
this particular RNG type), but the corresponding `SamplerS` type is
considered as internal detail, and may be changed without warning.


#### Specializing array generation

In some cases, for a given RNG type, generating an array of random
values can be more efficient with a specialized method than by merely
using the decoupling technique explained before. This is for example
the case for `MersenneTwister`, which natively writes random values in
an array.

To implement this specialization for `MyRNG`
and for a specification `s`, producing elements of type `S`,
the following method can be defined:
`rand!(rng::MyRNG, a::AbstractArray{S}, ::SamplerS)`,
where `SamplerS` is the type of the sampler returned by `Sampler(MyRNG, s, Val(Inf))`.
Instead of `AbstractArray`, it's possible to implement the functionality only for a subtype, e.g. `Array{S}`.
The non-mutating array method of `rand` will automatically call this specialization internally.

```@meta
DocTestSetup = nothing
```

# Reproducibility

By using an RNG parameter initialized with a given seed, you can reproduce the same pseudorandom number sequence when running your program multiple times.  However, a minor release of Julia (e.g. 1.3 to 1.4) *may change* the sequence of pseudorandom numbers generated from a specific seed.  (Even if the sequence produced by a low-level function like [`rand`](@ref) does not change, the output of higher-level functions like [`randsubseq`](@ref) may change due to algorithm updates.)   Rationale: guaranteeing that pseudorandom streams never change prohibits many algorithmic improvements.

If you need to guarantee exact reproducibility of random data, it is advisable to simply *save the data* (e.g. as a supplementary attachment in a scientific publication).  (You can also, of course, specify a
particular Julia version and package manifest, especially if you require bit reproducibility.)

Software tests that rely on *specific* "random" data should also generally save the data or embed it into the test code.  On the other hand, tests that should pass for *most* random data (e.g. testing `A \ (A*x) ≈ x` for a random matrix `A = randn(n,n)`) can use an RNG with a fixed seed to ensure that simply running the test many times does not encounter a failure due to very improbable data (e.g. an extremely ill-conditioned matrix).

The statistical *distribution* from which random samples are drawn *is* guaranteed to be the same across any minor Julia releases.
