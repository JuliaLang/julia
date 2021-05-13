# This file is a part of Julia. License is MIT: https://julialang.org/license

# asinh, acosh, and atanh are heavily based on FDLIBM code:
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


# Hyperbolic functions
# sinh methods
H_SMALL_X(::Type{Float64}) = 2.0^-28
H_MEDIUM_X(::Type{Float64}) = 22.0

H_SMALL_X(::Type{Float32}) = 2f-12
H_MEDIUM_X(::Type{Float32}) = 9f0

H_LARGE_X(::Type{Float64}) = 709.7822265633563 # nextfloat(709.7822265633562)

H_LARGE_X(::Type{Float32}) = 88.72283f0

SINH_SMALL_X(::Type{Float64}) = 2.1
SINH_SMALL_X(::Type{Float32}) = 3.0f0

# For Float64, use DoubleFloat scheme for extra accuracy
function sinh_kernel(x::Float64)
    x2 = x*x
    x2lo = fma(x,x,-x2)
    hi_order = evalpoly(x2, (8.333333333336817e-3, 1.9841269840165435e-4,
                             2.7557319381151335e-6, 2.5052096530035283e-8,
                             1.6059550718903307e-10, 7.634842144412119e-13,
                             2.9696954760355812e-15))
    hi,lo = exthorner(x2, (1.0, 0.16666666666666635, hi_order))
    return muladd(x, hi, muladd(x, lo, x*x2lo*0.16666666666666635))
end
# For Float32, using Float64 is simpler, faster, and doesn't require FMA
function sinh_kernel(x::Float32)
    x=Float64(x)
    res = evalpoly(x*x, (1.0, 0.1666666779967941, 0.008333336726447933,
                         0.00019841001151414065, 2.7555538207080807e-6,
                         2.5143389765825282e-8, 1.6260094552031644e-10))
    return Float32(res*x)
end

@inline function sinh16_kernel(x::Float32)
    res = evalpoly(x*x, (1.0f0, 0.16666667f0, 0.008333337f0, 0.00019841001f0,
                         2.7555539f-6, 2.514339f-8, 1.6260095f-10))
    return Float16(res*x)
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
    #      d)   H_LARGE_X  <= x
    #               return sinh(x) = exp(x/2)/2 * exp(x/2)
    #               Note that this branch automatically deals with Infs and NaNs

    absx = abs(x)
    if absx <= SINH_SMALL_X(T)
        return sinh_kernel(x)
    elseif absx >= H_LARGE_X(T)
        E = exp(T(.5)*absx)
        return copysign(T(.5)*E*E, x)
    end
    E = exp(absx)
    return copysign(T(.5)*(E - 1/E),x)
end

function Base.sinh(a::Float16)
    x = Float32(a)
    absx = abs(x)
    absx <= SINH_SMALL_X(Float32) && return sinh16_kernel(x)
    E = exp(absx)
    return Float16(copysign(.5f0*(E - 1/E),x))
end

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
    #      e)   H_LARGE_X  <= x
    #               return cosh(x) = exp(x/2)/2 * exp(x/2)
    #               Note that this branch automatically deals with Infs and NaNs

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

# tanh methods
TANH_LARGE_X(::Type{Float64}) = 44.0
TANH_LARGE_X(::Type{Float32}) = 18.0f0
TANH_SMALL_X(::Type{Float64}) = 1.0
TANH_SMALL_X(::Type{Float32}) = 1.3862944f0       #2*log(2)
@inline function tanh_kernel(x::Float64)
    return evalpoly(x, (1.0, -0.33333333333332904, 0.13333333333267555,
                        -0.05396825393066753, 0.02186948742242217,
                        -0.008863215974794633, 0.003591910693118715,
                        -0.0014542587440487815, 0.0005825521659411748,
                        -0.00021647574085351332, 5.5752458452673005e-5))
end
@inline function tanh_kernel(x::Float32)
    return evalpoly(x, (1.0f0, -0.3333312f0, 0.13328037f0,
                        -0.05350336f0, 0.019975215f0, -0.0050525228f0))
end
function tanh(x::T) where T<:Union{Float32, Float64}
    # Method
    # mathematically tanh(x) is defined to be (exp(x)-exp(-x))/(exp(x)+exp(-x))
    #    1. reduce x to non-negative by tanh(-x) = -tanh(x).
    #    2. Find the branch and the expression to calculate and return it
    #      a) 0 <= x < H_SMALL_X
    #             Use a minimax polynomial over the range
    #      b) H_SMALL_X <= x < TANH_LARGE_X
    #           1 - 2/(exp(2x) + 1)
    #      c) TANH_LARGE_X <= x
    #            return 1
    abs2x = abs(2x)
    abs2x >= TANH_LARGE_X(T) && return copysign(one(T), x)
    abs2x <= TANH_SMALL_X(T) && return x*tanh_kernel(x*x)
    k = exp(abs2x)
    return copysign(1 - 2/(k+1), x)
end

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

# atanh methods
@noinline atanh_domain_error(x) = throw(DomainError(x, "atanh(x) is only defined for |x| ≤ 1."))
function atanh(x::T) where T <: Union{Float32, Float64}
    # Method
    # 1.Reduced x to positive by atanh(-x) = -atanh(x)
    # 2. Find the branch and the expression to calculate and return it
    #     a) 0 <= x < 0.5
    #         return 0.5*log1p(2x/(1-x))
    #     b) 0.5 <= x <= 1
    #         return 0.5*log((x+1)/(1-x))
    # Special cases:
    #    if |x| > 1 throw DomainError
    isnan(x) && return x

    absx = abs(x)

    if absx > 1
        atanh_domain_error(x)
    end
    if absx < T(0.5)
        # in a)
        t = log1p(T(2)*absx/(T(1)-absx))
    else
        # in b)
        t = log((T(1)+absx)/(T(1)-absx))
    end
    return T(0.5)*copysign(t, x)
end
