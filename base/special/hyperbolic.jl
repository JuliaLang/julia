# ====================================================
# Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
#
# Developed at SunSoft, a Sun Microsystems, Inc. business.
# Permission to use, copy, modify, and distribute this
# software is freely granted, provided that this notice
# is preserved.
# ====================================================

_ldexp_exp(x::Float64, i::T) where T = ccall(("__ldexp_exp", libm), Float64, (Float64, T), x, i)
_ldexp_exp(x::Float32, i::T) where T = ccall(("__ldexp_expf",libm), Float32, (Float32, T), x, i)
_ldexp_exp(x::Real, i) = _ldexp_exp(float(x, i))

SINH_SMALL_X(::Type{Float64}) = 2.0^-28
H_MEDIUM_X(::Type{Float64}) = 22.0

SINH_SMALL_X(::Type{Float32}) = 2f-12
H_MEDIUM_X(::Type{Float32}) = 9f0

H_LARGE_X(::Type{Float64}) = 709.7822265633563 # nextfloat(709.7822265633562)
H_OVERFLOW_X(::Type{Float64}) = 710.475860073944 # nextfloat(710.4758600739439)

H_LARGE_X(::Type{Float32}) = 88.72283f0
H_OVERFLOW_X(::Type{Float32}) = 89.415985f0
function sinh(x::T) where T <: Union{Float32, Float64}
    # Method :
    # mathematically sinh(x) if defined to be (exp(x)-exp(-x))/2
    #    1. Replace x by |x| (sinh(-x) = -sinh(x)).
    #    2. Find the the branch and the expression to calculate and return it
    #      a)   0 <= x < SINH_SMALL_X
    #               return x
    #      b)   SINH_SMALL_X <= x < H_MEDIUM_X
    #               return sinh(x) = (E + E/(E+1))/2, where E=expm1(x)
    #      c)   H_MEDIUM_X <= x < H_LARGE_X
    #               return sinh(x) = exp(x)/2
    #      d)   H_LARGE_X  <= x < H_OVERFLOW_X
    #               return sinh(x) = exp(x/2)/2 * exp(x/2)
    #      e)   H_OVERFLOW_X <=  x
    #               return sinh(x) = T(Inf)
    #
    # Notes:
    #    only sinh(0) = 0 is exact for finite x.

    isnan(x) && return x

    absx = abs(x)

    h = T(0.5)
    if x < 0
        h = -h
    end
    # in a) or b)
    if absx < H_MEDIUM_X(T)
        # in a)
        if absx < SINH_SMALL_X(T)
            return x
        end
        t = expm1(absx)
        if absx < T(1)
            return h*(T(2)*t - t*t/(t + T(1)))
        end
        return h*(t + t/(t + T(1)))
    end
    # in c)
    if absx < H_LARGE_X(T)
        return h*exp(absx)
    end
    # in d)
    if absx < H_OVERFLOW_X(T)
        return h*T(2)*_ldexp_exp(absx, -1)
    end
    # in e)
    return copysign(T(Inf), x)
end
sinh(x::Real) = sinh(float(x))

COSH_SMALL_X(::Type{Float32}) = 0.00024414062f0
COSH_SMALL_X(::Type{Float64}) = 2.7755602085408512e-17
function cosh(x::T) where T <: Union{Float32, Float64}
    # Method :
    # mathematically cosh(x) if defined to be (exp(x)+exp(-x))/2
    #    1. Replace x by |x| (cosh(x) = cosh(-x)).
    #    2. Find the the branch and the expression to calculate and return it
    #      a)   x <= COSH_SMALL_X
    #               return T(1)
    #      b)   0 <= x <= ln2/2
    #               return 1+expm1(|x|)^2/(2*exp(|x|))
    #      c)   ln2/2 <= x <= H_MEDIUM_X
    #               return (exp(|x|)+1/exp(|x|)/2
    #      d)   H_MEDIUM_X <= x < H_LARGE_X
    #               return cosh(x) = exp(x)/2
    #      e)   H_LARGE_X  <= x < H_OVERFLOW_X
    #               return cosh(x) = exp(x/2)/2 * exp(x/2)
    #      f)   H_OVERFLOW_X <=  x
    #               return cosh(x) = T(Inf)

    isnan(x) && return x

    absx = abs(x)

    h = T(0.5)
    # in a) or b)
    if absx < log(T(2))/2
        # in a)
        if absx < COSH_SMALL_X(T)
            return T(1)
        end
        t = expm1(absx)
        w = T(1) + t
        return T(1) + (t*t)/(w + w)
    end
    # in c)
    if absx < H_MEDIUM_X(T)
        t = exp(absx)
        return h*t + h/t
    end
    # in d)
    if absx < H_LARGE_X(T)
        return h*exp(absx)
    end
    # in e)
    if absx < H_OVERFLOW_X(T)
        return _ldexp_exp(absx, -1)
    end
    # in f)
    return T(Inf)
end
cosh(x::Real) = cosh(float(x))

TANH_LARGE_X(T::Type{Float64}) = 22.0
TANH_LARGE_X(T::Type{Float32}) = 9.0
TANH_SMALL_X(T::Type{Float64}) = 2.0^-28
TANH_SMALL_X(T::Type{Float32}) = 2f0^-12
function tanh(x::T) where T<:Union{Float32, Float64}
# Method :
# mathematically tanh(x) if defined to be (exp(x)-exp(-x))/(exp(x)+exp(-x))
#    1. reduce x to non-negative by tanh(-x) = -tanh(x).
#    2.  0 <= x < TANH_SMALL_X
#            return x
#        TANH_SMALL_X <= x < 1
#            -expm1(-2x)/(expm1(-2x) + 2)
#        1 <= x < TANH_LARGE_X
#           1 - 2/(expm1(2x) + 2)
#        TANH_LARGE_X <= x
#            return 1
# Special cases:
#    tanh(NaN) is NaN
#    only tanh(0)=0 is exact for finite argument.

    if isnan(x)
        return x
    elseif isinf(x)
        return copysign(T(1), x)
    end

    absx = abs(x)

    if absx < TANH_LARGE_X(T)
        if absx < TANH_SMALL_X(T)
            return x
        end
        if absx >= T(1)
            t = expm1(T(2)*absx)
            z = T(1) - T(2)/(t + T(2))
        else
            t = expm1(-T(2)*absx)
            z = -t/(t + T(2))
        end
    else
        z = T(1)
    end
    return copysign(z, x)
end
tanh(x::Real) = tanh(float(x))
