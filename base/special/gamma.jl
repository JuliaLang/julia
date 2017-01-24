# This file is a part of Julia. License is MIT: http://julialang.org/license

gamma(x::Float64) = nan_dom_err(ccall((:tgamma,libm),  Float64, (Float64,), x), x)
gamma(x::Float32) = nan_dom_err(ccall((:tgammaf,libm),  Float32, (Float32,), x), x)

"""
    gamma(x)

Compute the gamma function of `x`.
"""
gamma(x::Real) = gamma(float(x))

function lgamma_r(x::Float64)
    signp = Array{Int32}(1)
    y = ccall((:lgamma_r,libm),  Float64, (Float64, Ptr{Int32}), x, signp)
    return y, signp[1]
end
function lgamma_r(x::Float32)
    signp = Array{Int32}(1)
    y = ccall((:lgammaf_r,libm),  Float32, (Float32, Ptr{Int32}), x, signp)
    return y, signp[1]
end
lgamma_r(x::Real) = lgamma_r(float(x))
lgamma_r(x::Number) = lgamma(x), 1 # lgamma does not take abs for non-real x
"`lgamma_r(x)`: return L,s such that `gamma(x) = s * exp(L)`" lgamma_r

"""
    lfact(x)

Compute the logarithmic factorial of `x`
"""
lfact(x::Real) = (x<=1 ? zero(float(x)) : lgamma(x+oneunit(x)))

"""
    lgamma(x)

Compute the logarithm of the absolute value of [`gamma`](@ref) for
`Real` `x`, while for `Complex` `x` it computes the
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
lgamma{T<:Union{Integer,Rational}}(z::Complex{T}) = lgamma(float(z))
lgamma{T<:Union{Float32,Float16}}(z::Complex{T}) = Complex{T}(lgamma(Complex{Float64}(z)))

gamma(z::Complex) = exp(lgamma(z))

# Bernoulli numbers B_{2k}, using tabulated numerators and denominators from
# the online encyclopedia of integer sequences.  (They actually have data
# up to k=249, but we stop here at k=20.)  Used for generating the polygamma
# (Stirling series) coefficients below.
#   const A000367 = map(BigInt, split("1,1,-1,1,-1,5,-691,7,-3617,43867,-174611,854513,-236364091,8553103,-23749461029,8615841276005,-7709321041217,2577687858367,-26315271553053477373,2929993913841559,-261082718496449122051", ","))
#   const A002445 = [1,6,30,42,30,66,2730,6,510,798,330,138,2730,6,870,14322,510,6,1919190,6,13530]
#   const bernoulli = A000367 .// A002445 # even-index Bernoulli numbers

"""
    digamma(x)

Compute the digamma function of `x` (the logarithmic derivative of [`gamma(x)`](@ref)).
"""
function digamma(z::Union{Float64,Complex{Float64}})
    # Based on eq. (12), without looking at the accompanying source
    # code, of: K. S. Kölbig, "Programs for computing the logarithm of
    # the gamma function, and the digamma function, for complex
    # argument," Computer Phys. Commun.  vol. 4, pp. 221–226 (1972).
    x = real(z)
    if x <= 0 # reflection formula
        ψ = -π * cot(π*z)
        z = 1 - z
        x = real(z)
    else
        ψ = zero(z)
    end
    if x < 7
        # shift using recurrence formula
        n = 7 - floor(Int,x)
        for ν = 1:n-1
            ψ -= inv(z + ν)
        end
        ψ -= inv(z)
        z += n
    end
    t = inv(z)
    ψ += log(z) - 0.5*t
    t *= t # 1/z^2
    # the coefficients here are Float64(bernoulli[2:9] .// (2*(1:8)))
    ψ -= t * @evalpoly(t,0.08333333333333333,-0.008333333333333333,0.003968253968253968,-0.004166666666666667,0.007575757575757576,-0.021092796092796094,0.08333333333333333,-0.4432598039215686)
end

"""
    trigamma(x)

Compute the trigamma function of `x` (the logarithmic second derivative of [`gamma(x)`](@ref)).
"""
function trigamma(z::Union{Float64,Complex{Float64}})
    # via the derivative of the Kölbig digamma formulation
    x = real(z)
    if x <= 0 # reflection formula
        return (π * csc(π*z))^2 - trigamma(1 - z)
    end
    ψ = zero(z)
    if x < 8
        # shift using recurrence formula
        n = 8 - floor(Int,x)
        ψ += inv(z)^2
        for ν = 1:n-1
            ψ += inv(z + ν)^2
        end
        z += n
    end
    t = inv(z)
    w = t * t # 1/z^2
    ψ += t + 0.5*w
    # the coefficients here are Float64(bernoulli[2:9])
    ψ += t*w * @evalpoly(w,0.16666666666666666,-0.03333333333333333,0.023809523809523808,-0.03333333333333333,0.07575757575757576,-0.2531135531135531,1.1666666666666667,-7.092156862745098)
end

signflip(m::Number, z) = (-1+0im)^m * z
signflip(m::Integer, z) = iseven(m) ? z : -z

# (-1)^m d^m/dz^m cot(z) = p_m(cot z), where p_m is a polynomial
# that satisfies the recurrence p_{m+1}(x) = p_m′(x) * (1 + x^2).
# Note that p_m(x) has only even powers of x if m is odd, and
# only odd powers of x if m is even.  Therefore, we write p_m(x)
# as p_m(x) = q_m(x^2) m! for m odd and p_m(x) = x q_m(x^2) m! if m is even.
# Hence the polynomials q_m(y) satisfy the recurrence:
#     m odd:  q_{m+1}(y) = 2 q_m′(y) * (1+y) / (m+1)
#    m even:  q_{m+1}(y) = [q_m(y) + 2 y q_m′(y)] * (1+y) / (m+1)
# This function computes the coefficients of the polynomial q_m(y),
# returning an array of the coefficients of 1, y, y^2, ...,
function cotderiv_q(m::Int)
    m < 0 && throw(ArgumentError("$m < 0 not allowed"))
    m == 0 && return [1.0]
    m == 1 && return [1.0, 1.0]
    q₋ = cotderiv_q(m-1)
    d = length(q₋) - 1 # degree of q₋
    if isodd(m-1)
        q = Array{Float64}(length(q₋))
        q[end] = d * q₋[end] * 2/m
        for i = 1:length(q)-1
            q[i] = ((i-1)*q₋[i] + i*q₋[i+1]) * 2/m
        end
    else # iseven(m-1)
        q = Array{Float64}(length(q₋) + 1)
        q[1] = q₋[1] / m
        q[end] = (1 + 2d) * q₋[end] / m
        for i = 2:length(q)-1
            q[i] = ((1 + 2(i-1))*q₋[i] + (1 + 2(i-2))*q₋[i-1]) / m
        end
    end
    return q
end

# precompute a table of cot derivative polynomials
const cotderiv_Q = [cotderiv_q(m) for m in 1:100]

# Evaluate (-1)^m d^m/dz^m [π cot(πz)] / m!.  If m is small, we
# use the explicit derivative formula (a polynomial in cot(πz));
# if m is large, we use the derivative of Euler's harmonic series:
#          π cot(πz) = ∑ inv(z + n)
function cotderiv(m::Integer, z)
    isinf(imag(z)) && return zero(z)
    if m <= 0
        m == 0 && return π * cot(π*z)
        throw(DomainError())
    end
    if m <= length(cotderiv_Q)
        q = cotderiv_Q[m]
        x = cot(π*z)
        y = x*x
        s = q[1] + q[2] * y
        t = y
        # evaluate q(y) using Horner (TODO: Knuth for complex z?)
        for i = 3:length(q)
            t *= y
            s += q[i] * t
        end
        return π^(m+1) * (isodd(m) ? s : x*s)
    else # m is large, series derivative should converge quickly
        p = m+1
        z -= round(real(z))
        s = inv(z^p)
        n = 1
        sₒ = zero(s)
        while s != sₒ
            sₒ = s
            a = (z+n)^p
            b = (z-n)^p
            s += (a + b) / (a * b)
            n += 1
        end
        return s
    end
end

# Helper macro for polygamma(m, z):
#   Evaluate p[1]*c[1] + x*p[2]*c[2] + x^2*p[3]*c[3] + ...
#   where c[1] = m + 1
#         c[k] = c[k-1] * (2k+m-1)*(2k+m-2) / ((2k-1)*(2k-2)) = c[k-1] * d[k]
#         i.e. d[k] = c[k]/c[k-1] = (2k+m-1)*(2k+m-2) / ((2k-1)*(2k-2))
#   by a modified version of Horner's rule:
#      c[1] * (p[1] + d[2]*x * (p[2] + d[3]*x * (p[3] + ...))).
# The entries of p must be literal constants and there must be > 1 of them.
macro pg_horner(x, m, p...)
    k = length(p)
    me = esc(m)
    xe = esc(x)
    ex = :(($me + $(2k-1)) * ($me + $(2k-2)) * $(p[end]/((2k-1)*(2k-2))))
    for k = length(p)-1:-1:2
        cdiv = 1 / ((2k-1)*(2k-2))
        ex = :(($cdiv * ($me + $(2k-1)) * ($me + $(2k-2))) *
               ($(p[k]) + $xe * $ex))
    end
    :(($me + 1) * ($(p[1]) + $xe * $ex))
end

# compute oftype(x, y)^p efficiently, choosing the correct branch cut
pow_oftype(x, y, p) = oftype(x, y)^p
pow_oftype(x::Complex, y::Real, p::Complex) = oftype(x, y^p)
function pow_oftype(x::Complex, y::Real, p::Real)
    if p >= 0
        # note: this will never be called for y < 0,
        # which would throw an error for non-integer p here
        return oftype(x, y^p)
    else
        yp = y^-p # use real power for efficiency
        return oftype(x, Complex(yp, -zero(yp))) # get correct sign of zero!
    end
end

# Generalized zeta function, which is related to polygamma
# (at least for integer m > 0 and real(z) > 0) by:
#    polygamma(m, z) = (-1)^(m+1) * gamma(m+1) * zeta(m+1, z).
# Our algorithm for the polygamma is just the m-th derivative
# of our digamma approximation, and this derivative process yields
# a function of the form
#          (-1)^(m) * gamma(m+1) * (something)
# So identifying the (something) with the -zeta function, we get
# the zeta function for free and might as well export it, especially
# since this is a common generalization of the Riemann zeta function
# (which Julia already exports).   Note that this geneneralization
# is equivalent to Mathematica's Zeta[s,z], and is equivalent to the
# Hurwitz zeta function for real(z) > 0.

"""
    zeta(s, z)

Generalized zeta function ``\\zeta(s, z)``, defined
by the sum ``\\sum_{k=0}^\\infty ((k+z)^2)^{-s/2}``, where
any term with ``k+z=0`` is excluded.  For ``\\Re z > 0``,
this definition is equivalent to the Hurwitz zeta function
``\\sum_{k=0}^\\infty (k+z)^{-s}``.   For ``z=1``, it yields
the Riemann zeta function ``\\zeta(s)``.
"""
zeta(s,z)

function zeta(s::Union{Int,Float64,Complex{Float64}},
              z::Union{Float64,Complex{Float64}})
    ζ = zero(promote_type(typeof(s), typeof(z)))

    (z == 1 || z == 0) && return oftype(ζ, zeta(s))
    s == 2 && return oftype(ζ, trigamma(z))

    x = real(z)

    # annoying s = Inf or NaN cases:
    if !isfinite(s)
        (isnan(s) || isnan(z)) && return (s*z)^2 # type-stable NaN+Nan*im
        if real(s) == Inf
            z == 1 && return one(ζ)
            if x > 1 || (x >= 0.5 ? abs(z) > 1 : abs(z - round(x)) > 1)
                return zero(ζ) # distance to poles is > 1
            end
            x > 0 && imag(z) == 0 && imag(s) == 0 && return oftype(ζ, Inf)
        end
        throw(DomainError()) # nothing clever to return
    end
    if isnan(x)
        if imag(z)==0 && imag(s)==0
            return oftype(ζ, x)
        else
            return oftype(ζ, Complex(x,x))
        end
    end

    m = s - 1

    # Algorithm is just the m-th derivative of digamma formula above,
    # with a modified cutoff of the final asymptotic expansion.

    # Note: we multiply by -(-1)^m m! in polygamma below, so this factor is
    #       pulled out of all of our derivatives.

    cutoff = 7 + real(m) + abs(imag(m)) # TODO: this cutoff is too conservative?
    if x < cutoff
        # shift using recurrence formula
        xf = floor(x)
        nx = Int(xf)
        n = ceil(Int,cutoff - nx)
        minus_s = -s
        if nx < 0 # x < 0
            # need to use (-z)^(-s) recurrence to be correct for real z < 0
            # [the general form of the recurrence term is (z^2)^(-s/2)]
            minus_z = -z
            ζ += pow_oftype(ζ, minus_z, minus_s) # ν = 0 term
            if xf != z
                ζ += pow_oftype(ζ, z - nx, minus_s) # real(z - nx) > 0, so use correct branch cut
                # otherwise, if xf==z, then the definition skips this term
            end
            # do loop in different order, depending on the sign of s,
            # so that we are looping from largest to smallest summands and
            # can halt the loop early if possible; see issue #15946
            # FIXME: still slow for small m, large Im(z)
            if real(s) > 0
                for ν in -nx-1:-1:1
                    ζₒ= ζ
                    ζ += pow_oftype(ζ, minus_z - ν, minus_s)
                    ζ == ζₒ && break # prevent long loop for large -x > 0
                end
            else
                for ν in 1:-nx-1
                    ζₒ= ζ
                    ζ += pow_oftype(ζ, minus_z - ν, minus_s)
                    ζ == ζₒ && break # prevent long loop for large -x > 0
                end
            end
        else # x ≥ 0 && z != 0
            ζ += pow_oftype(ζ, z, minus_s)
        end
        # loop order depends on sign of s, as above
        if real(s) > 0
            for ν in max(1,1-nx):n-1
                ζₒ= ζ
                ζ += pow_oftype(ζ, z + ν, minus_s)
                ζ == ζₒ && break # prevent long loop for large m
            end
        else
            for ν in n-1:-1:max(1,1-nx)
                ζₒ= ζ
                ζ += pow_oftype(ζ, z + ν, minus_s)
                ζ == ζₒ && break # prevent long loop for large m
            end
        end
        z += n
    end

    t = inv(z)
    w = isa(t, Real) ? conj(oftype(ζ, t))^m : oftype(ζ, t)^m
    ζ += w * (inv(m) + 0.5*t)

    t *= t # 1/z^2
    ζ += w*t * @pg_horner(t,m,0.08333333333333333,-0.008333333333333333,0.003968253968253968,-0.004166666666666667,0.007575757575757576,-0.021092796092796094,0.08333333333333333,-0.4432598039215686,3.0539543302701198)

    return ζ
end

"""
    polygamma(m, x)

Compute the polygamma function of order `m` of argument `x` (the `(m+1)th` derivative of the
logarithm of [`gamma(x)`](@ref))
"""
function polygamma(m::Integer, z::Union{Float64,Complex{Float64}})
    m == 0 && return digamma(z)
    m == 1 && return trigamma(z)

    # In principle, we could support non-integer m here, but the
    # extension to complex m seems to be non-unique, the obvious
    # extension (just plugging in a complex m below) does not seem to
    # be the one used by Mathematica or Maple, and sources do not
    # agree on what the "right" extension is (e.g. Mathematica & Maple
    # disagree).   So, at least for now, we require integer m.

    # real(m) < 0 is valid, but I don't think our asymptotic expansion
    # here works in this case.  m < 0 polygamma is called a
    # "negapolygamma" function in the literature, and there are
    # multiple possible definitions arising from different integration
    # constants. We throw a DomainError() since the definition is unclear.
    real(m) < 0 && throw(DomainError())

    s = m+1
    if real(z) <= 0 # reflection formula
        (zeta(s, 1-z) + signflip(m, cotderiv(m,z))) * (-gamma(s))
    else
        signflip(m, zeta(s,z) * (-gamma(s)))
    end
end

# TODO: better way to do this
f64(x::Real) = Float64(x)
f64(z::Complex) = Complex128(z)
f32(x::Real) = Float32(x)
f32(z::Complex) = Complex64(z)
f16(x::Real) = Float16(x)
f16(z::Complex) = Complex32(z)

# If we really cared about single precision, we could make a faster
# Float32 version by truncating the Stirling series at a smaller cutoff.
for (f,T) in ((:f32,Float32),(:f16,Float16))
    @eval begin
        zeta(s::Integer, z::Union{$T,Complex{$T}}) = $f(zeta(Int(s), f64(z)))
        zeta(s::Union{Float64,Complex128}, z::Union{$T,Complex{$T}}) = zeta(s, f64(z))
        zeta(s::Number, z::Union{$T,Complex{$T}}) = $f(zeta(f64(s), f64(z)))
        polygamma(m::Integer, z::Union{$T,Complex{$T}}) = $f(polygamma(Int(m), f64(z)))
        digamma(z::Union{$T,Complex{$T}}) = $f(digamma(f64(z)))
        trigamma(z::Union{$T,Complex{$T}}) = $f(trigamma(f64(z)))
    end
end

zeta(s::Integer, z::Number) = zeta(Int(s), f64(z))
zeta(s::Number, z::Number) = zeta(f64(s), f64(z))
for f in (:digamma, :trigamma)
    @eval begin
        $f(z::Number) = $f(f64(z))
    end
end
polygamma(m::Integer, z::Number) = polygamma(m, f64(z))

# Inverse digamma function:
# Implementation of fixed point algorithm described in
#  "Estimating a Dirichlet distribution" by Thomas P. Minka, 2000
function invdigamma(y::Float64)
    # Closed form initial estimates
    if y >= -2.22
        x_old = exp(y) + 0.5
        x_new = x_old
    else
        x_old = -1.0 / (y - digamma(1.0))
        x_new = x_old
    end

    # Fixed point algorithm
    delta = Inf
    iteration = 0
    while delta > 1e-12 && iteration < 25
        iteration += 1
        x_new = x_old - (digamma(x_old) - y) / trigamma(x_old)
        delta = abs(x_new - x_old)
        x_old = x_new
    end

    return x_new
end
invdigamma(x::Float32) = Float32(invdigamma(Float64(x)))

"""
    invdigamma(x)

Compute the inverse [`digamma`](@ref) function of `x`.
"""
invdigamma(x::Real) = invdigamma(Float64(x))

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

# Riemann zeta function; algorithm is based on specializing the Hurwitz
# zeta function above for z==1.
function zeta(s::Union{Float64,Complex{Float64}})
    # blows up to ±Inf, but get correct sign of imaginary zero
    s == 1 && return NaN + zero(s) * imag(s)

    if !isfinite(s) # annoying NaN and Inf cases
        isnan(s) && return imag(s) == 0 ? s : s*s
        if isfinite(imag(s))
            real(s) > 0 && return 1.0 - zero(s)*imag(s)
            imag(s) == 0 && return NaN + zero(s)
        end
        return NaN*zero(s) # NaN + NaN*im
    elseif real(s) < 0.5
        if abs(real(s)) + abs(imag(s)) < 1e-3 # Taylor series for small |s|
            return @evalpoly(s, -0.5,
                             -0.918938533204672741780329736405617639861,
                             -1.0031782279542924256050500133649802190,
                             -1.00078519447704240796017680222772921424,
                             -0.9998792995005711649578008136558752359121)
        end
        return zeta(1 - s) * gamma(1 - s) * sinpi(s*0.5) * (2π)^s / π
    end

    m = s - 1

    # shift using recurrence formula:
    #   n is a semi-empirical cutoff for the Stirling series, based
    #   on the error term ~ (|m|/n)^18 / n^real(m)
    n = ceil(Int,6 + 0.7*abs(imag(s-1))^inv(1 + real(m)*0.05))
    ζ = one(s)
    for ν = 2:n
        ζₒ= ζ
        ζ += inv(ν)^s
        ζ == ζₒ && break # prevent long loop for large m
    end
    z = 1 + n
    t = inv(z)
    w = t^m
    ζ += w * (inv(m) + 0.5*t)

    t *= t # 1/z^2
    ζ += w*t * @pg_horner(t,m,0.08333333333333333,-0.008333333333333333,0.003968253968253968,-0.004166666666666667,0.007575757575757576,-0.021092796092796094,0.08333333333333333,-0.4432598039215686,3.0539543302701198)

    return ζ
end

zeta(x::Integer) = zeta(Float64(x))
zeta(x::Real)    = oftype(float(x),zeta(Float64(x)))

"""
    zeta(s)

Riemann zeta function ``\\zeta(s)``.
"""
zeta(z::Complex) = oftype(float(z),zeta(Complex128(z)))

function eta(z::Union{Float64,Complex{Float64}})
    δz = 1 - z
    if abs(real(δz)) + abs(imag(δz)) < 7e-3 # Taylor expand around z==1
        return 0.6931471805599453094172321214581765 *
               @evalpoly(δz,
                         1.0,
                         -0.23064207462156020589789602935331414700440,
                         -0.047156357547388879740146103148112380421254,
                         -0.002263576552598880778433550956278702759143568,
                         0.001081837223249910136105931217561387128141157)
    else
        return -zeta(z) * expm1(0.6931471805599453094172321214581765*δz)
    end
end
eta(x::Integer) = eta(Float64(x))
eta(x::Real)    = oftype(float(x),eta(Float64(x)))

"""
    eta(x)

Dirichlet eta function ``\\eta(s) = \\sum^\\infty_{n=1}(-1)^{n-1}/n^{s}``.
"""
eta(z::Complex) = oftype(float(z),eta(Complex128(z)))
