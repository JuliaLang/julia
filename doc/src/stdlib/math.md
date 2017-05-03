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
Base.div
Base.fld
Base.cld
Base.mod
Base.rem
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
Base.:(<<)
Base.:(>>)
Base.:(>>>)
Base.colon
Base.range
Base.OneTo
Base.StepRangeLen
Base.:(==)
Base.:(!=)
Base.:(!==)
Base.:(<)
Base.:(<=)
Base.:(>)
Base.:(>=)
Base.cmp
Base.:(~)
Base.:(&)
Base.:(|)
Base.xor
Base.:(!)
&&
||
```

## Mathematical Functions

```@docs
Base.isapprox
Base.sin
Base.cos
Base.tan
Base.Math.sind
Base.Math.cosd
Base.Math.tand
Base.Math.sinpi
Base.Math.cospi
Base.sinh
Base.cosh
Base.tanh
Base.asin
Base.acos
Base.atan
Base.Math.atan2
Base.Math.asind
Base.Math.acosd
Base.Math.atand
Base.Math.sec
Base.Math.csc
Base.Math.cot
Base.Math.secd
Base.Math.cscd
Base.Math.cotd
Base.Math.asec
Base.Math.acsc
Base.Math.acot
Base.Math.asecd
Base.Math.acscd
Base.Math.acotd
Base.Math.sech
Base.Math.csch
Base.Math.coth
Base.asinh
Base.acosh
Base.atanh
Base.Math.asech
Base.Math.acsch
Base.Math.acoth
Base.Math.sinc
Base.Math.cosc
Base.Math.deg2rad
Base.Math.rad2deg
Base.Math.hypot
Base.log(::Any)
Base.log(::Number, ::Number)
Base.log2
Base.log10
Base.log1p
Base.Math.frexp
Base.exp
Base.exp2
Base.exp10
Base.Math.ldexp
Base.Math.modf
Base.expm1
Base.round(::Type, ::Any)
Base.Rounding.RoundingMode
Base.Rounding.RoundNearest
Base.Rounding.RoundNearestTiesAway
Base.Rounding.RoundNearestTiesUp
Base.Rounding.RoundToZero
Base.Rounding.RoundUp
Base.Rounding.RoundDown
Base.round{T <: AbstractFloat, MR, MI}(::Complex{T}, ::RoundingMode{MR}, ::RoundingMode{MI})
Base.ceil
Base.floor
Base.trunc
Base.unsafe_trunc
Base.signif
Base.min
Base.max
Base.minmax
Base.Math.clamp
Base.Math.clamp!
Base.abs
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
Base.Checked.add_with_overflow
Base.Checked.sub_with_overflow
Base.Checked.mul_with_overflow
Base.abs2
Base.copysign
Base.sign
Base.signbit
Base.flipsign
Base.sqrt
Base.isqrt
Base.Math.cbrt
Base.real(::Complex)
Base.imag
Base.reim
Base.conj
Base.angle
Base.cis
Base.binomial
Base.factorial
Base.gcd
Base.lcm
Base.gcdx
Base.ispow2
Base.nextpow2
Base.prevpow2
Base.nextpow
Base.prevpow
Base.nextprod
Base.invmod
Base.powermod
Base.Math.gamma
Base.Math.lgamma
Base.Math.lfact
Base.Math.beta
Base.Math.lbeta
Base.ndigits
Base.widemul
Base.Math.@evalpoly
```

## Statistics

```@docs
Base.mean
Base.mean!
Base.std
Base.stdm
Base.var
Base.varm
Base.middle
Base.median
Base.median!
Base.quantile
Base.quantile!
Base.cov
Base.cor
```

## Signal Processing

Fast Fourier transform (FFT) functions in Julia are implemented by calling functions from [FFTW](http://www.fftw.org).

```@docs
Base.DFT.fft
Base.DFT.fft!
Base.DFT.ifft
Base.DFT.ifft!
Base.DFT.bfft
Base.DFT.bfft!
Base.DFT.plan_fft
Base.DFT.plan_ifft
Base.DFT.plan_bfft
Base.DFT.plan_fft!
Base.DFT.plan_ifft!
Base.DFT.plan_bfft!
Base.DFT.rfft
Base.DFT.irfft
Base.DFT.brfft
Base.DFT.plan_rfft
Base.DFT.plan_brfft
Base.DFT.plan_irfft
Base.DFT.FFTW.dct
Base.DFT.FFTW.dct!
Base.DFT.FFTW.idct
Base.DFT.FFTW.idct!
Base.DFT.FFTW.plan_dct
Base.DFT.FFTW.plan_dct!
Base.DFT.FFTW.plan_idct
Base.DFT.FFTW.plan_idct!
Base.DFT.fftshift(::Any)
Base.DFT.fftshift(::Any, ::Any)
Base.DFT.ifftshift
Base.DSP.filt
Base.DSP.filt!
Base.DSP.deconv
Base.DSP.conv
Base.DSP.conv2
Base.DSP.xcorr
```

The following functions are defined within the `Base.FFTW` module.

```@docs
Base.DFT.FFTW.r2r
Base.DFT.FFTW.r2r!
Base.DFT.FFTW.plan_r2r
Base.DFT.FFTW.plan_r2r!
```
