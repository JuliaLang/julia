# Based on FDLIBM http://www.netlib.org/fdlibm/e_exp.c
# which is made available under the following licence

## Copyright (C) 2004 by Sun Microsystems, Inc. All rights reserved. Permission
## to use, copy, modify, and distribute this software is freely granted,
## provided that this notice is preserved.

# Method
# 1. Argument reduction: Reduce x to an r so that |r| <= 0.5*ln(2). Given x,
#    find r and integer k such that
#       x = k*ln(2) + r,  |r| <= 0.5*ln(2).
#    Here r is represented as r = hi - lo for better accuracy.
#
# 2. Approximate exp(r) by a special rational function on [0, 0.5*ln(2)]:
#       R(r^2) = r*(exp(r)+1)/(exp(r)-1) = 2 + r*r/6 - r^4/360 + ...
#
#    A special Remez algorithm on [0, 0.5*ln(2)] is used to generate a
#    polynomial to approximate R.
#
#    The computation of exp(r) thus becomes
#                       2*r
#       exp(r) = 1 + ----------
#                     R(r) - r
#                          r*c(r)
#              = 1 + r + ----------- (for better accuracy)
#                         2 - c(r)
#    where
#       c(r) = r - (P1*r^2  + P2*r^4  + ... + P5*r^10 + ...).
#
# 3. Scale back: exp(x) = 2^k * exp(r)

# log(2)
const LN2 = 6.931471805599453094172321214581765680755001343602552541206800094933936219696955e-01
# log2(e)
const LOG2_E = 1.442695040888963407359924681001892137426646

# log(2) into upper and lower bits
LN2U(::Type{Float64}) = 6.93147180369123816490e-1
LN2U(::Type{Float32}) = 6.9313812256f-1

LN2L(::Type{Float64}) = 1.90821492927058770002e-10
LN2L(::Type{Float32}) = 9.0580006145f-6

# max and min arguments
MAX_EXP(::Type{Float64}) = 7.09782712893383996732e2 # log 2^1023*(2-2^-52)
MAX_EXP(::Type{Float32}) = 88.72283905206835f0      # log 2^127 *(2-2^-23)

# one less than the min exponent since we can sqeeze a bit more from the exp function
MIN_EXP(::Type{Float64}) = -7.451332191019412076235e2 # log 2^-1075
MIN_EXP(::Type{Float32}) = -103.97207708f0            # log 2^-150

@inline exp_kernel(x::Float64) = @horner(x, 1.66666666666666019037e-1,
    -2.77777777770155933842e-3, 6.61375632143793436117e-5,
    -1.65339022054652515390e-6, 4.13813679705723846039e-8)

@inline exp_kernel(x::Float32) = @horner(x, 1.6666625440f-1, -2.7667332906f-3)

# for values smaller than this threshold just use a Taylor expansion
@eval exp_small_thres(::Type{Float64}) = $(2.0^-28)
@eval exp_small_thres(::Type{Float32}) = $(2.0f0^-13)

"""
    exp(x)

Compute the natural base exponential of `x`, in other words ``e^x``.

# Examples
```jldoctest
julia> exp(1.0)
2.718281828459045
```
"""
exp(x::Real) = exp(float(x))
function exp(x::T) where T<:Union{Float32,Float64}
    xa = reinterpret(Unsigned, x) & ~sign_mask(T)
    xsb = signbit(x)

    # filter out non-finite arguments
    if xa > reinterpret(Unsigned, MAX_EXP(T))
        if xa >= exponent_mask(T)
            xa & significand_mask(T) != 0 && return T(NaN)
            return xsb ? T(0.0) : T(Inf) # exp(+-Inf)
        end
        x > MAX_EXP(T) && return T(Inf)
        x < MIN_EXP(T) && return T(0.0)
    end
    # This implementation gives 2.7182818284590455 for exp(1.0) when T ==
    # Float64, which is well within the allowable error; however,
    # 2.718281828459045 is closer to the true value so we prefer that answer,
    # given that 1.0 is such an important argument value.
    if x == T(1.0) && T == Float64
        return 2.718281828459045235360
    end
    # compute approximation
    if xa > reinterpret(Unsigned, T(0.5)*T(LN2)) # |x| > 0.5 log(2)
        # argument reduction
        if xa < reinterpret(Unsigned, T(1.5)*T(LN2)) # |x| < 1.5 log(2)
            if xsb
                k = -1
                hi = x + LN2U(T)
                lo = -LN2L(T)
            else
                k = 1
                hi = x - LN2U(T)
                lo = LN2L(T)
            end
        else
            n = round(T(LOG2_E)*x)
            k = unsafe_trunc(Int,n)
            hi = muladd(n, -LN2U(T), x)
            lo = n*LN2L(T)
        end
        # compute approximation on reduced argument
        r = hi - lo
        z = r*r
        p = r - z*exp_kernel(z)
        y = T(1.0) - ((lo - (r*p)/(T(2.0) - p)) - hi)
        # scale back
        if k > -significand_bits(T)
            # multiply by 2.0 first to prevent overflow, which helps extends the range
            k == exponent_max(T) && return y * T(2.0) * T(2.0)^(exponent_max(T) - 1)
            twopk = reinterpret(T, rem(exponent_bias(T) + k, uinttype(T)) << significand_bits(T))
            return y*twopk
        else
            # add significand_bits(T) + 1 to lift the range outside the subnormals
            twopk = reinterpret(T, rem(exponent_bias(T) + significand_bits(T) + 1 + k, uinttype(T)) << significand_bits(T))
            return y * twopk * T(2.0)^(-significand_bits(T) - 1)
        end
    elseif xa < reinterpret(Unsigned, exp_small_thres(T)) # |x| < exp_small_thres
        # Taylor approximation for small values: exp(x) ≈ 1.0 + x
        return T(1.0) + x
    else
        # primary range with k = 0, so compute approximation directly
        z = x*x
        p = x - z*exp_kernel(z)
        return T(1.0) - ((x*p)/(p - T(2.0)) - x)
    end
end
