# This file is a part of Julia. License is MIT: https://julialang.org/license

# sinh, cosh, tanh, asinh, acosh, and atanh are heavily based on FDLIBM code:
# e_sinh.c, e_sinhf, e_cosh.c, e_coshf, s_tanh.c, s_tanhf.c, s_asinh.c,
# s_asinhf.c, e_acosh.c, e_coshf.c, e_atanh.c, and e_atanhf.c
# that are made available under the following licence:

# ====================================================
# Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
#
# Developed at SunSoft, a Sun Microsystems, Inc. business.
# Permission to use, copy, modify, and distribute this
# software is freely granted, provided that this notice
# is preserved.
# ====================================================

@inline function exthorner(x, p::Tuple)
	# polynomial evaluation using compensated summation.
	# much more accurate, especially when lo can be combined with other rounding errors
    hi, lo = p[end], zero(x)
    for i in length(p)-1:-1:1
        pi = p[i]
        prod = hi*x
        err = fma(hi, x, -prod)
        hi = pi+prod
        lo = fma(lo, x, prod - (hi - pi) + err)
    end
    return hi, lo
end

# Hyperbolic functions
# sinh methods
H_SMALL_X(::Type{Float64}) = 2.0^-28
H_MEDIUM_X(::Type{Float64}) = 22.0

H_SMALL_X(::Type{Float32}) = 2f-12
H_MEDIUM_X(::Type{Float32}) = 9f0

H_LARGE_X(::Type{Float64}) = 709.7822265633563 # nextfloat(709.7822265633562)
H_OVERFLOW_X(::Type{Float64}) = 710.475860073944 # nextfloat(710.4758600739439)

H_LARGE_X(::Type{Float32}) = 88.72283f0
H_OVERFLOW_X(::Type{Float32}) = 89.415985f0

SINH_SMALL_X(::Type{Float64}) = 2.0
SINH_SMALL_X(::Type{Float32}) = 4.0

function sinh_kernel(x2::Float64)
    hi_order = evalpoly(x2, (8.333333333337979e-3, 1.984126984007895e-4,
                             2.755731937687675e-6, 2.5052097364218946e-8,
                             1.6059510146369204e-10, 7.635683932974871e-13,
                             2.9632282505934393e-15))
    return exthorner(x2, (1.0, 0.16666666666666596, hi_order))
end

function sinh_kernel(x2::Float32)
    hi_order = evalpoly(x2, (0.0001983419f0, 2.7681986f-6, 2.393391f-8, 2.093227f-10))
    return exthorner(x2, (1.0f0, 0.16666648f0, 0.008333524f0, hi_order))
end

function sinh(x::T) where T<:Union{Float32,Float64}
    # Method
    # mathematically sinh(x) is defined to be (exp(x)-exp(-x))/2
    #    1. Sometimes replace x by |x| (sinh(-x) = -sinh(x)).
    #    2. Find the branch and the expression to calculate and return it
    #      a)   0 <= x < SINH_SMALL_X
    #               approximate sinh(x) with a  minimax polynomial
    #      b)   SINH_SMALL_X <= x < H_LARGE_X
    #               return sinh(x) = (exp(x) - exp(-x))/2
    #      d)   H_LARGE_X  <= x < H_OVERFLOW_X
    #               return sinh(x) = exp(x/2)/2 * exp(x/2)
    #               Note that this branch automatically deals with Infs and NaNs

    absx = abs(x)
    if absx <= SINH_SMALL_X(T)
        hi, lo = sinh_kernel(x*x)
        return muladd(x, hi, x*lo)
    elseif absx >= H_LARGE_X(T)
        E = exp(T(.5)*absx)
        return copysign(T(.5)*E*E, x)
    end
    E = exp(absx)
    return copysign(T(.5)*(E - 1/E),x)
end
sinh(x::Real) = sinh(float(x))

COSH_SMALL_X(::Type{T}) where T= one(T)

function cosh_kernel(x2::Float32)
    return evalpoly(x2, (1.0f0, 0.49999997f0, 0.041666888f0, 0.0013882756f0, 2.549933f-5))
end

function cosh_kernel(x2::Float64)
    return evalpoly(x2, (1.0, 0.5000000000000002, 0.04166666666666269,
                         1.3888888889206764e-3, 2.4801587176784207e-5,
                         2.7557345825742837e-7, 2.0873617441235094e-9,
                         1.1663435515945578e-11))
end

function cosh(x::T) where T<:Union{Float32,Float64}
    # Method
    # mathematically cosh(x) is defined to be (exp(x)+exp(-x))/2
    #    1. Replace x by |x| (cosh(x) = cosh(-x)).
    #    2. Find the branch and the expression to calculate and return it
    #      a)   x <= COSH_SMALL_X
    #               approximate sinh(x) with a minimax polynomial
    #      b)   COSH_SMALL_X <= x < H_LARGE_X
    #               return cosh(x) = = (exp(x) + exp(-x))/2
    #      e)   H_LARGE_X  <= x < H_OVERFLOW_X
    #               return cosh(x) = exp(x/2)/2 * exp(x/2)
    #      			Note that this branch automatically deals with Infs and NaNs

    absx = abs(x)
    if absx <= COSH_SMALL_X(T)
        return cosh_kernel(x*x)
    elseif absx >= H_LARGE_X(T)
        E = exp(T(.5)*absx)
        return T(.5)*E*E
    end
    E = exp(absx)
    return T(.5)*(E + 1/E)
end
cosh(x::Real) = cosh(float(x))

# tanh methods
TANH_LARGE_X(::Type{Float64}) = 22.0
TANH_LARGE_X(::Type{Float32}) = 9.0f0
function tanh(x::T) where T<:Union{Float32, Float64}
    # Method
    # mathematically tanh(x) is defined to be (exp(x)-exp(-x))/(exp(x)+exp(-x))
    #    1. reduce x to non-negative by tanh(-x) = -tanh(x).
    #    2. Find the branch and the expression to calculate and return it
    #      a) 0 <= x < H_SMALL_X
    #             return x
    #      b) H_SMALL_X <= x < 1
    #            -expm1(-2x)/(expm1(-2x) + 2)
    #      c) 1 <= x < TANH_LARGE_X
    #           1 - 2/(expm1(2x) + 2)
    #      d) TANH_LARGE_X <= x
    #            return 1
    if isnan(x)
        return x
    elseif isinf(x)
        return copysign(T(1), x)
    end

    absx = abs(x)
    if absx < TANH_LARGE_X(T)
        # in a)
        if absx < H_SMALL_X(T)
            return x
        end
        if absx >= T(1)
            # in c)
            t = expm1(T(2)*absx)
            z = T(1) - T(2)/(t + T(2))
        else
            # in b)
            t = expm1(-T(2)*absx)
            z = -t/(t + T(2))
        end
    else
        # in d)
        z = T(1)
    end
    return copysign(z, x)
end
tanh(x::Real) = tanh(float(x))

# Inverse hyperbolic functions
AH_LN2(::Type{Float64}) = 6.93147180559945286227e-01
AH_LN2(::Type{Float32}) = 6.9314718246f-01
# asinh methods
function asinh(x::T) where T <: Union{Float32, Float64}
    # Method
    # mathematically asinh(x) = sign(x)*log(|x| + sqrt(x*x + 1))
    # is the principle value of the inverse hyperbolic sine
    # 1. Find the branch and the expression to calculate and return it
    #    a) |x| < 2^-28
    #        return x
    #    b) |x| < 2
    #        return sign(x)*log1p(|x| + x^2/(1 + sqrt(1+x^2)))
    #    c) 2 <= |x| < 2^28
    #        return sign(x)*log(2|x|+1/(|x|+sqrt(x*x+1)))
    #    d) |x| >= 2^28
    #        return sign(x)*(log(x)+ln2))
    if isnan(x) || isinf(x)
        return x
    end
    absx = abs(x)
    if absx < T(2)
        # in a)
        if absx < T(2)^-28
            return x
        end
        # in b)
        t = x*x
        w = log1p(absx + t/(T(1) + sqrt(T(1) + t)))
    elseif absx < T(2)^28
        # in c)
        t = absx
        w = log(T(2)*t + T(1)/(sqrt(x*x + T(1)) + t))
    else
        # in d)
        w = log(absx) + AH_LN2(T)
    end
    return copysign(w, x)
end
asinh(x::Real) = asinh(float(x))

# acosh methods
@noinline acosh_domain_error(x) = throw(DomainError(x, "acosh(x) is only defined for x ≥ 1."))
function acosh(x::T) where T <: Union{Float32, Float64}
    # Method
    # mathematically acosh(x) if defined to be log(x + sqrt(x*x-1))
    # 1. Find the branch and the expression to calculate and return it
    #     a) x = 1
    #         return log1p(t+sqrt(2.0*t+t*t)) where t=x-1.
    #     b) 1 < x < 2
    #         return log1p(t+sqrt(2.0*t+t*t)) where t=x-1.
    #     c) 2 <= x <
    #         return log(2x-1/(sqrt(x*x-1)+x))
    #     d) x >= 2^28
    #         return log(x)+ln2
    # Special cases:
    #     if x < 1 throw DomainError

    isnan(x) && return x

    if x < T(1)
        return acosh_domain_error(x)
    elseif x == T(1)
        # in a)
        return T(0)
    elseif x < T(2)
        # in b)
        t = x - T(1)
        return log1p(t + sqrt(T(2)*t + t*t))
    elseif x < T(2)^28
        # in c)
        t = x*x
        return log(T(2)*x - T(1)/(x+sqrt(t - T(1))))
    else
        # in d)
        return log(x) + AH_LN2(T)
    end
end
acosh(x::Real) = acosh(float(x))

# atanh methods
@noinline atanh_domain_error(x) = throw(DomainError(x, "atanh(x) is only defined for |x| ≤ 1."))
function atanh(x::T) where T <: Union{Float32, Float64}
    # Method
    # 1.Reduced x to positive by atanh(-x) = -atanh(x)
    # 2. Find the branch and the expression to calculate and return it
    #     a) 0 <= x < 2^-28
    #         return x
    #     b) 2^-28 <= x < 0.5
    #         return 0.5*log1p(2x+2x*x/(1-x))
    #     c) 0.5 <= x < 1
    #         return 0.5*log1p(2x/1-x)
    #     d) x = 1
    #         return Inf
    # Special cases:
    #    if |x| > 1 throw DomainError
    isnan(x) && return x

    absx = abs(x)

    if absx > 1
        atanh_domain_error(x)
    end
    if absx < T(2)^-28
        # in a)
        return x
    end
    if absx < T(0.5)
        # in b)
        t = absx+absx
        t = T(0.5)*log1p(t+t*absx/(T(1)-absx))
    elseif absx < T(1)
        # in c)
        t = T(0.5)*log1p((absx + absx)/(T(1)-absx))
    elseif absx == T(1)
        # in d)
        return copysign(T(Inf), x)
    end
    return copysign(t, x)
end
atanh(x::Real) = atanh(float(x))
