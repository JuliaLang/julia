# This file is a part of Julia. License is MIT: http://julialang.org/license

module SpecialFunctions

export erf, erfc, erfcx, erfi, dawson,
       lgamma, gamma, lfact,
       airyai, airyaiprime, airybi, airybiprime,
       airyaix, airyaiprimex, airybix, airybiprimex,
       besselj0, besselj1, besselj, besseljx,
       bessely0, bessely1, bessely, besselyx,
       hankelh1, hankelh2, hankelh1x, hankelh2x,
       besseli, besselix, besselk, besselkx, besselh, besselhx,
       beta, lbeta, eta, zeta, polygamma, invdigamma, digamma, trigamma,
       erfinv, erfcinv

using Base.Math: @horner

const libm = Base.libm_name
const openspecfun = "libopenspecfun"

"""
    erf(x)

Compute the error function of `x`, defined by ``\\frac{2}{\\sqrt{\\pi}} \\int_0^x e^{-t^2} dt``
for arbitrary complex `x`.
"""
erf(x)

"""
    erfc(x)

Compute the complementary error function of `x`, defined by ``1 - \\operatorname{erf}(x)``.
"""
erfc(x)

# functions with no domain error
for f in (:erf, :erfc)
    @eval begin
        ($f)(x::Float64) = ccall(($(string(f)),libm), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(($(string(f,"f")),libm), Float32, (Float32,), x)
        ($f)(x::Real) = ($f)(float(x))
    end
end

# fallback definitions to prevent infinite loop from $f(x::Real) def above
for f in (:erf, :erfc)
    @eval ($f)(x::AbstractFloat) = error("not implemented for ", typeof(x))
end

# utility for converting NaN return to DomainError
@inline nan_dom_err(f, x) = isnan(f) & !isnan(x) ? throw(DomainError()) : f

# functions that return NaN on non-NaN argument for domain error
for f in (:lgamma,)
    @eval begin
        ($f)(x::Float64) = nan_dom_err(ccall(($(string(f)),libm), Float64, (Float64,), x), x)
        ($f)(x::Float32) = nan_dom_err(ccall(($(string(f,"f")),libm), Float32, (Float32,), x), x)
        ($f)(x::Real) = ($f)(float(x))
    end
end

include("bessel.jl")
include("erf.jl")
include("gamma.jl")

# Float16 definitions

for func in (:lgamma,:erf,:erfc)
    @eval begin
        $func(a::Float16) = Float16($func(Float32(a)))
        $func(a::Complex32) = Complex32($func(Complex64(a)))
    end
end

end # module
