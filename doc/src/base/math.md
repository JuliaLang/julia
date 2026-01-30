# Mathematics

## [Mathematical Operators](@id math-ops)

```@docs
Base.:-(::Any)
Base.:(+)
Base.:-(::Any, ::Any)
Base.:*(::Any, ::Any...)
Base.:(/)
Base.:\(::Any, ::Any)
Base.:^(::Number, ::Number)
Base.fma
Base.muladd
Base.inv(::Number)
Base.div
Base.div(::Any, ::Any, ::RoundingMode)
Base.fld
Base.cld
Base.mod
Base.rem
Base.rem(::Any, ::Any, ::RoundingMode)
Base.rem2pi
Base.Math.mod2pi
Base.divrem
Base.fldmod
Base.fld1
Base.mod1
Base.fldmod1
Base.:(//)
Base.rationalize
Base.numerator
Base.denominator
```

## Comparisons

```@docs
Base.:(==)
Base.:(!=)
Base.:(!==)
Base.:(<)
Base.:(<=)
Base.:(>)
Base.:(>=)
Base.cmp
Base.isapprox
```

## Bit Operators

```@docs
Base.:(<<)
Base.:(>>)
Base.:(>>>)
Base.bitrotate
Base.:(~)
Base.:(&)
Base.:(|)
Base.xor
Base.nand
Base.nor
```

## Logical Operators

```@docs
Base.:(!)
&&
||
```

## Trigonometry

### ... in Radians

```@docs
Base.sin(::Number)
Base.cos(::Number)
Base.sincos(::Float64)
Base.tan(::Number)
```

```@docs
Base.asin(::Number)
Base.acos(::Number)
Base.atan(::Number)
```

```@docs
Base.Math.sec(::Number)
Base.Math.csc(::Number)
Base.Math.cot(::Number)
```

```@docs
Base.Math.asec(::Number)
Base.Math.acsc(::Number)
Base.Math.acot(::Number)
```

### ... in Degrees

```@docs
Base.Math.sind
Base.Math.cosd
Base.Math.tand
Base.Math.sincosd
```

```@docs
Base.Math.asind
Base.Math.acosd
Base.Math.atand
```

```@docs
Base.Math.secd
Base.Math.cscd
Base.Math.cotd
```

```@docs
Base.Math.asecd
Base.Math.acscd
Base.Math.acotd
```

### ... in Multiples of π

```@docs
Base.Math.sinpi
Base.Math.cospi
Base.Math.tanpi
Base.Math.sincospi
```

### Hyperbolic

```@docs
Base.sinh(::Number)
Base.cosh(::Number)
Base.tanh(::Number)
```

```@docs
Base.asinh(::Number)
Base.acosh(::Number)
Base.atanh(::Number)
```

```@docs
Base.Math.sech(::Number)
Base.Math.csch(::Number)
Base.Math.coth(::Number)
```

```@docs
Base.Math.asech(::Number)
Base.Math.acsch(::Number)
Base.Math.acoth(::Number)
```

### Other Trigonometric Functions

```@docs
Base.Math.sinc
Base.Math.cosc
Base.Math.deg2rad
Base.Math.rad2deg
Base.Math.hypot
```

## Logarithms

```@docs
Base.log(::Number)
Base.log(::Number, ::Number)
Base.log2
Base.log10
Base.log1p
```

## Exponentiation

```@docs
Base.exp(::Float64)
Base.exp2
Base.exp10
Base.expm1
```

## Rounding

```@docs
Base.round
Base.Rounding.RoundingMode
Base.Rounding.RoundNearest
Base.Rounding.RoundNearestTiesAway
Base.Rounding.RoundNearestTiesUp
Base.Rounding.RoundToZero
Base.Rounding.RoundFromZero
Base.Rounding.RoundUp
Base.Rounding.RoundDown
Base.round(::Complex{<: AbstractFloat}, ::RoundingMode, ::RoundingMode)
Base.ceil
Base.floor
Base.trunc
Base.unsafe_trunc
Base.Math.modf
```

## Extrema

```@docs
Base.min
Base.max
Base.minmax
```

## Clamping

```@docs
Base.Math.clamp
Base.Math.clamp!
```

## Checked Operations

```@docs
Base.Checked
Base.Checked.checked_abs
Base.Checked.checked_neg
Base.Checked.checked_add
Base.Checked.checked_sub
Base.Checked.checked_mul
Base.Checked.checked_div
Base.Checked.checked_rem
Base.Checked.checked_fld
Base.Checked.checked_mod
Base.Checked.checked_cld
Base.Checked.checked_pow
Base.Checked.add_with_overflow
Base.Checked.sub_with_overflow
Base.Checked.mul_with_overflow
```

## Signs

```@docs
Base.abs
Base.abs2
Base.uabs
```

```@docs
Base.copysign
Base.sign
Base.signbit
Base.flipsign
```

```@docs
Base.ispositive
Base.isnegative
```

## Roots

```@docs
Base.sqrt(::Number)
Base.isqrt
Base.Math.cbrt(::AbstractFloat)
Base.fourthroot(::Number)
```

## Complex Functions

```@docs
Base.real
Base.imag
Base.reim
Base.conj
Base.angle
Base.cis
Base.cispi
```

## Combinatorics and Number Theory

```@docs
Base.binomial
Base.factorial
Base.gcd
Base.lcm
Base.gcdx
Base.ispow2
Base.nextpow
Base.prevpow
Base.nextprod
Base.invmod
Base.powermod
```

## Other Mathematical Functions

```@docs
Base.add_sum
Base.widemul
Base.Math.evalpoly
Base.Math.@evalpoly
Base.FastMath.@fastmath
```

## Customizable binary operators

Some unicode characters can be used to define new binary operators
that support infix notation.
For example
```⊗(x,y) = kron(x,y)```
defines the `⊗` (otimes) function to be the Kronecker product,
and one can call it as binary operator using infix syntax:
```C = A ⊗ B```
as well as with the usual prefix syntax
```C = ⊗(A,B)```.

Other characters that support such extensions include
\odot `⊙`
and
\oplus `⊕`

The complete list is in the parser code:
<https://github.com/JuliaLang/julia/blob/master/src/julia-parser.scm>

Those that are parsed like `*` (in terms of precedence) include
`* / ÷ % & ⋅ ∘ × |\\| ∩ ∧ ⊗ ⊘ ⊙ ⊚ ⊛ ⊠ ⊡ ⊓ ∗ ∙ ∤ ⅋ ≀ ⊼ ⋄ ⋆ ⋇ ⋉ ⋊ ⋋ ⋌ ⋏ ⋒ ⟑ ⦸ ⦼ ⦾ ⦿ ⧶ ⧷ ⨇ ⨰ ⨱ ⨲ ⨳ ⨴ ⨵ ⨶ ⨷ ⨸ ⨻ ⨼ ⨽ ⩀ ⩃ ⩄ ⩋ ⩍ ⩎ ⩑ ⩓ ⩕ ⩘ ⩚ ⩜ ⩞ ⩟ ⩠ ⫛ ⊍ ▷ ⨝ ⟕ ⟖ ⟗`
and those that are parsed like `+` include
`+ - |\|| ⊕ ⊖ ⊞ ⊟ |++| ∪ ∨ ⊔ ± ∓ ∔ ∸ ≏ ⊎ ⊻ ⊽ ⋎ ⋓ ⟇ ⧺ ⧻ ⨈ ⨢ ⨣ ⨤ ⨥ ⨦ ⨧ ⨨ ⨩ ⨪ ⨫ ⨬ ⨭ ⨮ ⨹ ⨺ ⩁ ⩂ ⩅ ⩊ ⩌ ⩏ ⩐ ⩒ ⩔ ⩖ ⩗ ⩛ ⩝ ⩡ ⩢ ⩣`
There are many others that are related to arrows, comparisons, and powers.
