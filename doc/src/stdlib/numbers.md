# [Numbers](@id lib-numbers)

## Standard Numeric Types

### Abstract number types

```@docs
Core.Number
Core.Real
Core.AbstractFloat
Core.Integer
Core.Signed
Core.Unsigned
Base.AbstractIrrational
```

### Concrete number types

```@docs
Core.Float16
Core.Float32
Core.Float64
Base.BigFloat
Core.Bool
Core.Int8
Core.UInt8
Core.Int16
Core.UInt16
Core.Int32
Core.UInt32
Core.Int64
Core.UInt64
Core.Int128
Core.UInt128
Base.BigInt
Base.Complex
Base.Rational
Base.Irrational
```

## Data Formats

```@docs
Base.bin
Base.hex
Base.dec
Base.oct
Base.base
Base.digits
Base.digits!
Base.bitstring
Base.parse(::Type, ::Any, ::Any)
Base.tryparse
Base.big
Base.signed
Base.unsigned
Base.float(::Any)
Base.Math.significand
Base.Math.exponent
Base.complex(::Complex)
Base.bswap
Base.hex2bytes
Base.hex2bytes!
Base.bytes2hex
```

## General Number Functions and Constants

```@docs
Base.one
Base.oneunit
Base.zero
Base.im
Base.MathConstants.pi
Base.MathConstants.ℯ
Base.MathConstants.catalan
Base.MathConstants.eulergamma
Base.MathConstants.golden
Base.Inf
Base.Inf32
Base.Inf16
Base.NaN
Base.NaN32
Base.NaN16
Base.issubnormal
Base.isfinite
Base.isinf
Base.isnan
Base.iszero
Base.isone
Base.nextfloat
Base.prevfloat
Base.isinteger
Base.isreal
Core.Float32(::Any)
Core.Float64(::Any)
Base.GMP.BigInt(::Any)
Base.MPFR.BigFloat(::Any)
Base.Rounding.rounding
Base.Rounding.setrounding(::Type, ::Any)
Base.Rounding.setrounding(::Function, ::Type, ::RoundingMode)
Base.Rounding.get_zero_subnormals
Base.Rounding.set_zero_subnormals
```

### Integers

```@docs
Base.count_ones
Base.count_zeros
Base.leading_zeros
Base.leading_ones
Base.trailing_zeros
Base.trailing_ones
Base.isodd
Base.iseven
```

## BigFloats

The [`BigFloat`](@ref) type implements arbitrary-precision floating-point arithmetic using
the [GNU MPFR library](http://www.mpfr.org/).

```@docs
Base.precision
Base.MPFR.precision(::Type{BigFloat})
Base.MPFR.setprecision
Base.MPFR.BigFloat(x, prec::Int)
BigFloat(x::Union{Integer, AbstractFloat, String}, rounding::RoundingMode)
Base.MPFR.BigFloat(x, prec::Int, rounding::RoundingMode)
Base.MPFR.BigFloat(x::String)
```

## Random Numbers

Random number generation in Julia uses the [Mersenne Twister library](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/#dSFMT)
via `MersenneTwister` objects. Julia has a global RNG, which is used by default. Other RNG types
can be plugged in by inheriting the `AbstractRNG` type; they can then be used to have multiple
streams of random numbers. Besides `MersenneTwister`, Julia also provides the `RandomDevice` RNG
type, which is a wrapper over the OS provided entropy.

Most functions related to random generation accept an optional `AbstractRNG` as the first argument,
`rng` , which defaults to the global one if not provided. Morever, some of them accept optionally
dimension specifications `dims...` (which can be given as a tuple) to generate arrays of random
values.

A `MersenneTwister` or `RandomDevice` RNG can generate random numbers of the following types:
[`Float16`](@ref), [`Float32`](@ref), [`Float64`](@ref), [`BigFloat`](@ref), [`Bool`](@ref),
[`Int8`](@ref), [`UInt8`](@ref), [`Int16`](@ref), [`UInt16`](@ref), [`Int32`](@ref),
[`UInt32`](@ref), [`Int64`](@ref), [`UInt64`](@ref), [`Int128`](@ref), [`UInt128`](@ref),
[`BigInt`](@ref) (or complex numbers of those types).
Random floating point numbers are generated uniformly in ``[0, 1)``. As `BigInt` represents
unbounded integers, the interval must be specified (e.g. `rand(big(1:6))`).

```@docs
Base.Random.srand
Base.Random.MersenneTwister
Base.Random.RandomDevice
Base.Random.rand
Base.Random.rand!
Base.Random.bitrand
Base.Random.randn
Base.Random.randn!
Base.Random.randexp
Base.Random.randexp!
Base.Random.randjump
```
