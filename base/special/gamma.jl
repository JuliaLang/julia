# This file is a part of Julia. License is MIT: https://julialang.org/license

gamma(x::Float64) = nan_dom_err(ccall((:tgamma,libm),  Float64, (Float64,), x), x)
gamma(x::Float32) = nan_dom_err(ccall((:tgammaf,libm),  Float32, (Float32,), x), x)

"""
    gamma(x)

Compute the gamma function of `x`.
"""
gamma(x::Real) = gamma(float(x))

function lgamma_r(x::Float64)
    signp = Ref{Int32}()
    y = ccall((:lgamma_r,libm),  Float64, (Float64, Ptr{Int32}), x, signp)
    return y, signp[]
end
function lgamma_r(x::Float32)
    signp = Ref{Int32}()
    y = ccall((:lgammaf_r,libm),  Float32, (Float32, Ptr{Int32}), x, signp)
    return y, signp[]
end
lgamma_r(x::Real) = lgamma_r(float(x))
lgamma_r(x::Number) = lgamma(x), 1 # lgamma does not take abs for non-real x
"`lgamma_r(x)`: return L,s such that `gamma(x) = s * exp(L)`" lgamma_r

"""
    lfact(x)

Compute the logarithmic factorial of a nonnegative integer `x`.
Equivalent to [`lgamma`](@ref) of `x + 1`, but `lgamma` extends this function
to non-integer `x`.
"""
lfact(x::Integer) = x < 0 ? throw(DomainError(x, "`x` must be non-negative.")) : lgamma(x + oneunit(x))

"""
    lgamma(x)

Compute the logarithm of the absolute value of [`gamma`](@ref) for
[`Real`](@ref) `x`, while for [`Complex`](@ref) `x` compute the
principal branch cut of the logarithm of `gamma(x)` (defined for negative `real(x)`
by analytic continuation from positive `real(x)`).
"""
function lgamma end

# asymptotic series for log(gamma(z)), valid for sufficiently large real(z) or |imag(z)|
@inline function lgamma_asymptotic(z::Complex{Float64})
    zinv = inv(z)
    t = zinv*zinv
    # coefficients are bernoulli[2:n+1] .// (2*(1:n).*(2*(1:n) - 1))
    return (z-0.5)*log(z) - z + 9.1893853320467274178032927e-01 + # <-- log(2pi)/2
       zinv*@evalpoly(t, 8.3333333333333333333333368e-02,-2.7777777777777777777777776e-03,
                         7.9365079365079365079365075e-04,-5.9523809523809523809523806e-04,
                         8.4175084175084175084175104e-04,-1.9175269175269175269175262e-03,
                         6.4102564102564102564102561e-03,-2.9550653594771241830065352e-02)
end

# Compute the logΓ(z) function using a combination of the asymptotic series,
# the Taylor series around z=1 and z=2, the reflection formula, and the shift formula.
# Many details of these techniques are discussed in D. E. G. Hare,
# "Computing the principal branch of log-Gamma," J. Algorithms 25, pp. 221-236 (1997),
# and similar techniques are used (in a somewhat different way) by the
# SciPy loggamma function.  The key identities are also described
# at http://functions.wolfram.com/GammaBetaErf/LogGamma/
function lgamma(z::Complex{Float64})
    x = real(z)
    y = imag(z)
    yabs = abs(y)
    if !isfinite(x) || !isfinite(y) # Inf or NaN
        if isinf(x) && isfinite(y)
            return Complex(x, x > 0 ? (y == 0 ? y : copysign(Inf, y)) : copysign(Inf, -y))
        elseif isfinite(x) && isinf(y)
            return Complex(-Inf, y)
        else
            return Complex(NaN, NaN)
        end
    elseif x > 7 || yabs > 7 # use the Stirling asymptotic series for sufficiently large x or |y|
        return lgamma_asymptotic(z)
    elseif x < 0.1 # use reflection formula to transform to x > 0
        if x == 0 && y == 0 # return Inf with the correct imaginary part for z == 0
            return Complex(Inf, signbit(x) ? copysign(oftype(x, pi), -y) : -y)
        end
        # the 2pi * floor(...) stuff is to choose the correct branch cut for log(sinpi(z))
        return Complex(1.1447298858494001741434262, # log(pi)
                       copysign(6.2831853071795864769252842, y) # 2pi
                       * floor(0.5*x+0.25)) -
               log(sinpi(z)) - lgamma(1-z)
    elseif abs(x - 1) + yabs < 0.1
        # taylor series around zero at z=1
        # ... coefficients are [-eulergamma; [(-1)^k * zeta(k)/k for k in 2:15]]
        w = Complex(x - 1, y)
        return w * @evalpoly(w, -5.7721566490153286060651188e-01,8.2246703342411321823620794e-01,
                                -4.0068563438653142846657956e-01,2.705808084277845478790009e-01,
                                -2.0738555102867398526627303e-01,1.6955717699740818995241986e-01,
                                -1.4404989676884611811997107e-01,1.2550966952474304242233559e-01,
                                -1.1133426586956469049087244e-01,1.000994575127818085337147e-01,
                                -9.0954017145829042232609344e-02,8.3353840546109004024886499e-02,
                                -7.6932516411352191472827157e-02,7.1432946295361336059232779e-02,
                                -6.6668705882420468032903454e-02)
    elseif abs(x - 2) + yabs < 0.1
        # taylor series around zero at z=2
        # ... coefficients are [1-eulergamma; [(-1)^k * (zeta(k)-1)/k for k in 2:12]]
        w = Complex(x - 2, y)
        return w * @evalpoly(w, 4.2278433509846713939348812e-01,3.2246703342411321823620794e-01,
                               -6.7352301053198095133246196e-02,2.0580808427784547879000897e-02,
                               -7.3855510286739852662729527e-03,2.8905103307415232857531201e-03,
                               -1.1927539117032609771139825e-03,5.0966952474304242233558822e-04,
                               -2.2315475845357937976132853e-04,9.9457512781808533714662972e-05,
                               -4.4926236738133141700224489e-05,2.0507212775670691553131246e-05)
    end
    # use recurrence relation lgamma(z) = lgamma(z+1) - log(z) to shift to x > 7 for asymptotic series
    shiftprod = Complex(x,yabs)
    x += 1
    sb = false # == signbit(imag(shiftprod)) == signbit(yabs)
    # To use log(product of shifts) rather than sum(logs of shifts),
    # we need to keep track of the number of + to - sign flips in
    # imag(shiftprod), as described in Hare (1997), proposition 2.2.
    signflips = 0
    while x <= 7
        shiftprod *= Complex(x,yabs)
        sb′ = signbit(imag(shiftprod))
        signflips += sb′ & (sb′ != sb)
        sb = sb′
        x += 1
    end
    shift = log(shiftprod)
    if signbit(y) # if y is negative, conjugate the shift
        shift = Complex(real(shift), signflips*-6.2831853071795864769252842 - imag(shift))
    else
        shift = Complex(real(shift), imag(shift) + signflips*6.2831853071795864769252842)
    end
    return lgamma_asymptotic(Complex(x,y)) - shift
end
lgamma(z::Complex{T}) where {T<:Union{Integer,Rational}} = lgamma(float(z))
lgamma(z::Complex{T}) where {T<:Union{Float32,Float16}} = Complex{T}(lgamma(Complex{Float64}(z)))

gamma(z::Complex) = exp(lgamma(z))

"""
    beta(x, y)

Euler integral of the first kind ``\\operatorname{B}(x,y) = \\Gamma(x)\\Gamma(y)/\\Gamma(x+y)``.
"""
function beta(x::Number, w::Number)
    yx, sx = lgamma_r(x)
    yw, sw = lgamma_r(w)
    yxw, sxw = lgamma_r(x+w)
    return exp(yx + yw - yxw) * (sx*sw*sxw)
end

"""
    lbeta(x, y)

Natural logarithm of the absolute value of the [`beta`](@ref)
function ``\\log(|\\operatorname{B}(x,y)|)``.
"""
lbeta(x::Number, w::Number) = lgamma(x)+lgamma(w)-lgamma(x+w)
