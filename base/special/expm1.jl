# This code is a Julia translation of the C code from ... with the following license:
# ====================================================
# Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
#
# Developed at SunPro, a Sun Microsystems, Inc. business.
# Permission to use, copy, modify, and distribute this
# software is freely granted, provided that this notice
# is preserved.
# ====================================================
# Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.


# expm1(x)
# Returns exp(x)-1, the exponential of x minus 1.
#
# Method
#   1. Argument reduction:
#	Given x, find r and integer k such that
#
#               x = k*ln2 + r,  |r| <= 0.5*ln2 ~ 0.34658
#
#      Here a correction term c will be computed to compensate
#	the error in r when rounded to a floating-point number.
#
#   2. Approximating expm1(r) by a special rational function on
#	the interval [0,0.34658]:
#	Since
#	    r*(exp(r)+1)/(exp(r)-1) = 2+ r^2/6 - r^4/360 + ...
#	we define R1(r*r) by
#	    r*(exp(r)+1)/(exp(r)-1) = 2+ r^2/6 * R1(r*r)
#	That is,
#     R1(r**2) = 6/r *((exp(r)+1)/(exp(r)-1) - 2/r)
# 	     = 6/r * ( 1 + 2.0*(1/(exp(r)-1) - 1/r))
# 	     = 1 - r^2/60 + r^4/2520 - r^6/100800 + ...
#      We use a special Remez algorithm on [0,0.347] to generate
# 	a polynomial of degree 5 in r*r to approximate R1. The
# maximum error of this polynomial approximation is bounded
# by 2**-61. In other words,
#     R1(z) ~ 1.0 + Q1*z + Q2*z**2 + Q3*z**3 + Q4*z**4 + Q5*z**5
# where 	Q1  =  -1.6666666666666567384E-2,
# 		Q2  =   3.9682539681370365873E-4,
# 		Q3  =  -9.9206344733435987357E-6,
# 		Q4  =   2.5051361420808517002E-7,
# 		Q5  =  -6.2843505682382617102E-9;
# 	z   =  r*r,
# with error bounded by
#     |                  5           |     -61
#     | 1.0+Q1*z+...+Q5*z   -  R1(z) | <= 2
#     |                              |
#
#	expm1(r) = exp(r)-1 is then computed by the following
# 	specific way which minimize the accumulation rounding error:
#			               2     3
#			              r     r    [ 3 - (R1 + R1*r/2)  ]
#	      expm1(r) = r + --- + --- * [--------------------]
#		              2     2    [ 6 - r*(3 - R1*r/2) ]
#
#	To compensate the error in the argument reduction, we use
#		expm1(r+c) = expm1(r) + c + expm1(r)*c
#			   ~ expm1(r) + c + r*c
#	Thus c+r*c will be added in as the correction terms for
#	expm1(r+c). Now rearrange the term to avoid optimization
# 	screw up:
#		                (      2                                    2 )
#		                ({  ( r    [ R1 -  (3 - R1*r/2) ]  )  }    r  )
#   expm1(r+c)~r - ({r*(--- * [--------------------]-c)-c} - --- )
#	                    ({  ( 2    [ 6 - r*(3 - R1*r/2) ]  )  }    2  )
#                       (                                             )
#
#		   = r - E
#   3. Scale back to obtain expm1(x):
#	From step 1, we have
#	   expm1(x) = either 2^k*[expm1(r)+1] - 1
#		    = or     2^k*[expm1(r) + (1-2^-k)]
#   4. Implementation notes:
#	(A). To save one multiplication, we scale the coefficient Qi
#	     to Qi*2^i, and replace z by (x^2)/2.
#	(B). To achieve maximum accuracy, we compute expm1(x) by
#	  (i)   if x < -56*ln2, return -1.0, (raise inexact if x!=inf)
#	  (ii)  if k=0, return r-E
#	  (iii) if k=-1, return 0.5*(r-E)-0.5
#        (iv)	if k=1 if r < -0.25, return 2*((r+0.5)- E)
#	       	       else	     return  1.0+2.0*(r-E);
#	  (v)   if (k<-2||k>56) return 2^k(1-(E-r)) - 1 (or exp(x)-1)
#	  (vi)  if k <= 20, return 2^k((1-2^-k)-(E-r)), else
#	  (vii) return 2^k(1-((E+2^-k)-r))
#
# Special cases:
#	expm1(T(Inf)) is T(Inf), expm1(T(NaN)) is T(NaN)
#	expm1(-T(Inf)) is T(-1), and
#	for finite argument, only expm1(0)=0 is exact.
#
# Accuracy:
#	according to an error analysis, the error is always less than
#	1 ulp (unit in the last place).
#
expm1_ln2_hi(::Type{Float64}) = 6.93147180369123816490e-01
expm1_ln2_lo(::Type{Float64}) = 1.90821492927058770002e-10

@inline expm1_invln2(::Type{Float64}) = 1.44269504088896338700e+00
@inline expm1_huge(T::Type{Float64}) = 56.0*expm1_ln2(T)

expm1_overflow(::Type{Float64}) = 7.09782712893383973096e+02
expm1_underflow(::Type{Float64}) = 5.551115123125783e-17# 2.0^-54
# Scaled Q's: Qn_here = 2**n * Qn_above, for R(2*z) where z = hfxs = x*x/2:

expm1_p(hfxs::Float64) = @horner(hfxs, -3.33333333333331316428e-02, # Q1
                                        1.58730158725481460165e-03, # Q2
                                       -7.93650757867487942473e-05, # Q3
                                        4.00821782732936239552e-06, # Q4
                                       -2.01099218183624371326e-07) # Q5)

expm1_ln2_hi(::Type{Float32}) = 6.9313812256f-01
expm1_ln2_lo(::Type{Float32}) = 9.0580006145f-06
expm1_invln2(::Type{Float32}) = 1.4426950216f0
expm1_huge(::Type{Float32}) = 18.714973f0 # around 27*ln2, taken from source
expm1_overflow(::Type{Float32}) = 88.72168f0
expm1_underflow(::Type{Float32}) = 2.9802322f-8 # 2f0^-25

expm1_ln2(::Type{Float64}) = 0.6931471805599453 # expm1_ln2_hi(T) + expm1_ln2_lo(T)
expm1_ln2(::Type{Float32}) = 0.6931472f0 # expm1_ln2_hi(T) + expm1_ln2_lo(T)

expm1_big_k(::Type{Float64}) = 1024
expm1_big_k(::Type{Float32}) = 128
expm1_small_k(::Type{Float64}) = Int32(20)
expm1_small_k(::Type{Float32}) = Int32(23)

# Domain [-0.34568, 0.34568], range ~[-6.694e-10, 6.696e-10]:
# |6 / x * (1 + 2 * (1 / (exp(x) - 1) - 1 / x)) - q(x)| < 2**-30.04
# Scaled coefficients: Qn_here = 2**n * Qn_for_q (see s_expm1.c):
@inline expm1_p(hfxs::Float32) = @horner(hfxs, -3.3333212137f-2, # Q1
                                        1.5807170421f-3) # Q2

@inline expm1_two_pow_mk(::Type{Float32}, k) = reinterpret(Float32, UInt32((0x7f-k)<<23))
@inline expm1_two_pow_mk(::Type{Float64}, k) = reinterpret(Float64, UInt64(0x3ff00000+((-k)<<20))<<32)

@inline expm1_two_pow_k(::Type{Float32}, k) = reinterpret(Float32, UInt32(0x3f800000+(k<<23)))
@inline expm1_two_pow_k(::Type{Float64}, k) = reinterpret(Float64, UInt64(0x3ff00000+(k<<20))<<32)

#this is 2^-k not 1-2f0^k
@inline expm1_one_m_2_pow_mk(::Type{Float32}, k) = reinterpret(Float32, UInt32(0x3f800000 - (0x1000000>>k)))
# 0x00000000 after & comes from lowword(1.0) = unsafe_trunc(UInt32, reinterpret(UInt64, 1.0))
# @inline expm1_one_m_2_pow_mk(::Type{Float64}, k) = reinterpret(Float64, (UInt64(0x3ff00000 - (0x200000>>k))<<32)|0x00000000)
@inline expm1_one_m_2_pow_mk(::Type{Float64}, k) = reinterpret(Float64, (UInt64(0x3ff00000 - (0x200000>>k))<<32) | unsafe_trunc(UInt32, reinterpret(UInt64, 1.0)))

@inline expm1_two_pow_big_km1(T::Type{Float64}) = 8.98846567431158e307 # T(2.0)^(expm1_big_k(T) - 1)
@inline expm1_two_pow_big_km1(T::Type{Float32}) = 1.7014118f38# T(2.0)^(expm1_big_k(T) - 1)

function expm1(x::T) where T<:Union{Float32, Float64}
    xsign = signbit(x)
    absx = abs(x)

    # filter out huge and non-finite argument
    if absx >= expm1_huge(T) # quit early for large absolute values of x
        if isinf(absx)
            return ifelse(xsign, -T(1.0), x)
        end
        if x > expm1_overflow(T) # largest value T can take before overflowing
            return T(Inf)
        end
        if xsign # x < -expm1_huge(T)
            return -T(1.0)
        end
    end

    # argument reduction
    if absx > T(0.5)*expm1_ln2(T)
        if absx < T(1.5)*expm1_ln2(T)
            if xsign
                hi = x + expm1_ln2_hi(T)
                lo = -expm1_ln2_lo(T)
                k = Int32(-1)
            else
                hi = x - expm1_ln2_hi(T)
                lo = expm1_ln2_lo(T)
                k = Int32(1)
            end
        else
            # k  = round(Int32, expm1_invln2(T)*x)
            # the next line could be replaced with the above but we don't need the
            # safety of round, so we round by adding one half with the appropriate
            # sign and truncate to Int32
            k = unsafe_trunc(Int32, expm1_invln2(T)*x + ifelse(xsign, -T(0.5), T(0.5)))
            hi = x - k*expm1_ln2_hi(T) # t*expm1_ln2_hi is exact here
            lo = k*expm1_ln2_lo(T)
        end
        x = hi - lo
        c  = (hi - x) - lo
    elseif absx < expm1_underflow(T)
        return x
    else
        k = Int32(0)
    end
    # x is now in primary range
    hfx = T(0.5)*x
    hfxs = x*hfx
    r1 = T(1.0) + hfxs*expm1_p(hfxs)
    t  = T(3.0) - r1*hfx
    e  = hfxs*((r1 - t)/(T(6.0) - x*t))
    if k == 0
        return x - (x*e - hfxs) # c is 0
    else
        e  = (x*(e - c) - c)
        e -= hfxs
        if k == -1
            return T(0.5)*(x - e) - T(0.5)
        end
        if k == 1
            if x < -T(0.25)
                return -T(2.0)*(e - (x + T(0.5)))
            else
                return  T(1.0) + T(2.0)*(x - e)
            end
        end
        two_pow_k = expm1_two_pow_k(T, k) # T(2.0)^k but 9-10 times faster
        if (k <= -2 || k > 56) # suffice to return exp(x)-1
            y = T(1.0) - (e - x)
            if k == expm1_big_k(T)
                y = y*T(2.0)*expm1_two_pow_big_km1(T) # T(2.0)^(expm1_big_k(T) - 1)
            else
                y = y*two_pow_k # y*2^k
            end
            return y-T(1.0)
        end
        if k < expm1_small_k(T)
            t = expm1_one_m_2_pow_mk(T, k) # T(1.0)-T(2.0)^-k, but 4-5 times faster
            y = t-(e-x)
            y = y*two_pow_k # y*2^k
        else
            t = expm1_two_pow_mk(T, k) # T(2.0)^-k
            y = x-(e+t)
            y += T(1.0)
            y = y*two_pow_k # y*2^k
        end
    end
    y
end

expm1(x::Real) = expm1(float(x))
