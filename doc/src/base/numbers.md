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
Base.digits
Base.digits!
Base.bitstring
Base.parse
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
