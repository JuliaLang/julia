# This file is a part of Julia. License is MIT: https://julialang.org/license

#  Method
#  1. Argument reduction: Reduce x to an r so that |r| <= 0.5*log10(2). Given x,
#     find r and integer k such that
#
#                x = k*log10(2) + r,  |r| <= 0.5*log10(2).
#
# 2. Approximate exp10(r) by a polynomial on the interval [-0.5*log10(2), 0.5*log10(2)]:
#
#           exp10(x) = 1.0 + polynomial(x),
#
#    sup norm relative error within the interval of the polynomial approximations:
#    Float64 : [2.7245504724394698952e-18; 2.7245529895753476720e-18]
#    Float32 : [9.6026471477842205871e-10; 9.6026560194009888672e-10]
#
# 3. Scale back: exp10(x) = 2^k * exp10(r)

# log2(10)
const LOG2_10 = 3.321928094887362347870319429489390175864831393024580612054756395815934776608624
# log10(2)
const LOG10_2 = 3.010299956639811952137388947244930267681898814621085413104274611271081892744238e-01
# log(10)
const LN10 = 2.302585092994045684017991454684364207601101488628772976033327900967572609677367

# log10(2) into upper and lower bits
LOG10_2U(::Type{Float64}) = 3.01025390625000000000e-1
LOG10_2U(::Type{Float32}) = 3.00781250000000000000f-1

LOG10_2L(::Type{Float64}) = 4.60503898119521373889e-6
LOG10_2L(::Type{Float32}) = 2.48745663981195213739f-4

# max and min arguments
MAX_EXP10(::Type{Float64}) = 3.08254715559916743851e2 # log 2^1023*(2-2^-52)
MAX_EXP10(::Type{Float32}) = 38.531839419103626f0     # log 2^127 *(2-2^-23)

# one less than the min exponent since we can sqeeze a bit more from the exp10 function
MIN_EXP10(::Type{Float64}) = -3.23607245338779784854769e2 # log10 2^-1075
MIN_EXP10(::Type{Float32}) = -45.15449934959718f0         # log10 2^-150

@inline exp10_kernel(x::Float64) =
    @horner(x, 1.0,
    2.30258509299404590109361379290930926799774169921875,
    2.6509490552391992146397114993305876851081848144531,
    2.03467859229323178027470930828712880611419677734375,
    1.17125514891212478829629617393948137760162353515625,
    0.53938292928868392106522833273629657924175262451172,
    0.20699584873167015119932443667494226247072219848633,
    6.8089348259156870502017966373387025669217109680176e-2,
    1.9597690535095281527677713029333972372114658355713e-2,
    5.015553121397981796436571499953060992993414402008e-3,
    1.15474960721768829356725927226534622604958713054657e-3,
    1.55440426715227567738830671828509366605430841445923e-4,
    3.8731032432074128681303432086835414338565897196531e-5,
    2.3804466459036747669197886523306806338950991630554e-3,
    9.3881392238209649520573607528461934634833596646786e-5,
    -2.64330486232183387018679354696359951049089431762695e-2)

@inline exp10_kernel(x::Float32) =
    @horner(x, 1.0f0,
    2.302585124969482421875f0,
    2.650949001312255859375f0,
    2.0346698760986328125f0,
    1.17125606536865234375f0,
    0.5400512218475341796875f0,
    0.20749187469482421875f0,
    5.2789829671382904052734375f-2)

@eval exp10_small_thres(::Type{Float64}) = $(2.0^-29)
@eval exp10_small_thres(::Type{Float32}) = $(2.0f0^-14)

"""
    exp10(x)

Compute ``10^x``.

# Examples
```jldoctest
julia> exp10(2)
100.0

julia> exp10(0.2)
1.5848931924611136
```
"""
exp10(x::Real) = exp10(float(x))
function exp10(x::T) where T<:Union{Float32,Float64}
    xa = reinterpret(Unsigned, x) & ~sign_mask(T)
    xsb = signbit(x)

    # filter out non-finite arguments
    if xa > reinterpret(Unsigned, MAX_EXP10(T))
        if xa >= exponent_mask(T)
            xa & significand_mask(T) != 0 && return T(NaN)
            return xsb ? T(0.0) : T(Inf) # exp10(+-Inf)
        end
        x > MAX_EXP10(T) && return T(Inf)
        x < MIN_EXP10(T) && return T(0.0)
    end
    # compute approximation
    if xa > reinterpret(Unsigned, T(0.5)*T(LOG10_2)) # |x| > 0.5 log10(2).
        # argument reduction
        if xa < reinterpret(Unsigned, T(1.5)*T(LOG10_2)) # |x| <= 1.5 log10(2)
            if xsb
                k = -1
                r = LOG10_2U(T) + x
                r = LOG10_2L(T) + r
            else
                k = 1
                r = x - LOG10_2U(T)
                r = r - LOG10_2L(T)
            end
        else
            n = round(T(LOG2_10)*x)
            k = unsafe_trunc(Int,n)
            r = muladd(n, -LOG10_2U(T), x)
            r = muladd(n, -LOG10_2L(T), r)
        end
        # compute approximation on reduced argument
        y = exp10_kernel(r)
        # scale back
        if k > -significand_bits(T)
            # multiply by 2.0 first to prevent overflow, extending the range
            k == exponent_max(T) && return y * T(2.0) * T(2.0)^(exponent_max(T) - 1)
            twopk = reinterpret(T, rem(exponent_bias(T) + k, uinttype(T)) << significand_bits(T))
            return y*twopk
        else
            # add significand_bits(T) + 1 to lift the range outside the subnormals
            twopk = reinterpret(T, rem(exponent_bias(T) + significand_bits(T) + 1 + k, uinttype(T)) << significand_bits(T))
            return y * twopk * T(2.0)^(-significand_bits(T) - 1)
        end
    elseif xa < reinterpret(Unsigned, exp10_small_thres(T))  # |x| < exp10_small_thres
        # Taylor approximation for small values: exp10(x) ≈ 1.0 + log(10)*x
        return muladd(x, T(LN10), T(1.0))
    else
        # primary range with k = 0, so compute approximation directly
        return exp10_kernel(x)
    end
end
