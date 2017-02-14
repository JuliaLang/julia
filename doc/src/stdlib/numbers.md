# [Numbers](@id lib-numbers)

## Standard Numeric Types

  * `Bool`
  * `Int8`
  * `UInt8`
  * `Int16`
  * `UInt16`
  * `Int32`
  * `UInt32`
  * `Int64`
  * `UInt64`
  * `Int128`
  * `UInt128`
  * `Float16`
  * `Float32`
  * `Float64`
  * `Complex64`
  * `Complex128`

## Data Formats

```@docs
Base.bin
Base.hex
Base.dec
Base.oct
Base.base
Base.digits
Base.digits!
Base.bits
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
Base.num2hex
Base.hex2num
Base.hex2bytes
Base.bytes2hex
```

## General Number Functions and Constants

```@docs
Base.one
Base.oneunit
Base.zero
Base.pi
Base.im
Base.eu
Base.catalan
Base.eulergamma
Base.golden
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
Base.nextfloat
Base.prevfloat
Base.isinteger
Base.isreal
Core.Float32
Core.Float64
Base.GMP.BigInt
Base.MPFR.BigFloat
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

The `BigFloat` type implements arbitrary-precision floating-point arithmetic using the [GNU MPFR library](http://www.mpfr.org/).

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
`Float16`, `Float32`, `Float64`, `Bool`, `Int8`, `UInt8`, `Int16`, `UInt16`, `Int32`, `UInt32`,
`Int64`, `UInt64`, `Int128`, `UInt128`, `BigInt` (or complex numbers of those types). Random floating
point numbers are generated uniformly in ``[0, 1)``. As `BigInt` represents unbounded integers,
the interval must be specified (e.g. `rand(big(1:6))`).

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
