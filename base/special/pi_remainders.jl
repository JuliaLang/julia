# This file is a part of Julia. License is MIT: https://julialang.org/license

# Remainders modulo multiples of π: rem_pio16 reduces by π/16 for the trigonometric kernels in
# trig.jl, and rem2pi is built on it. Arguments too large for the Cody-Waite reductions use
# Payne-Hanek, which reduces by π/16 directly.

# 16/π, and π/16 ≈ PIO16_HI + PIO16_LO
const INV_PIO16 = 0x1.45f306dc9c883p+2
const PIO16_HI = 0x1.921fb54442d18p-3
const PIO16_LO = 0x1.1a62633145c07p-57

# round(a*c) as a float and as an Int, for |a*c| < 2^51 (Float64) or 2^21 (Float32), using
# MAGIC_ROUND_CONST: the integer is then also the low bits of the sum, which avoids separate round
# and float-to-int instructions.
@inline function roundmul(a::T, c::T) where T<:Union{Float32, Float64}
    t = muladd(a, c, MAGIC_ROUND_CONST(T))
    return t - MAGIC_ROUND_CONST(T), Int(reinterpret(Signed, t) - reinterpret(Signed, MAGIC_ROUND_CONST(T)))
end

## Payne-Hanek

# Bits of 1/2π
#   1/2π == sum(x / 0x1p64^i for i,x = enumerate(INV_2PI))
# Can be obtained by:
#
#    setprecision(BigFloat, 4096)
#    I = 0.5/big(pi)
#    for i = 1:19
#        I *= 0x1p64
#        k = trunc(UInt64, I)
#        @printf "0x%016x,\n" k
#        I -= k
#    end

const INV_2PI = (
    0x28be_60db_9391_054a,
    0x7f09_d5f4_7d4d_3770,
    0x36d8_a566_4f10_e410,
    0x7f94_58ea_f7ae_f158,
    0x6dc9_1b8e_9093_74b8,
    0x0192_4bba_8274_6487,
    0x3f87_7ac7_2c4a_69cf,
    0xba20_8d7d_4bae_d121,
    0x3a67_1c09_ad17_df90,
    0x4e64_758e_60d4_ce7d,
    0x2721_17e2_ef7e_4a0e,
    0xc7fe_25ff_f781_6603,
    0xfbcb_c462_d682_9b47,
    0xdb4d_9fb3_c9f2_c26d,
    0xd3d1_8fd9_a797_fa8b,
    0x5d49_eeb1_faf9_7c5e,
    0xcf41_ce7d_e294_a4ba,
    0x9afe_d7ec_47e3_5742,
    0x1580_cc11_bf1e_daea)

# f/2^128 as a double-double (z_hi, z_lo) with |z_lo| <= ulp(z_hi)/2. f >> 1 is exact (f is w << 5),
# and |f >> 1| <= 2^126, so rounding it to Float64 can't leave the range of Int128.
function fromfraction(f::Int128)
    g = f >> 1
    h = Float64(g)
    return h*0x1p-127, Float64(g - unsafe_trunc(Int128, h))*0x1p-127
end

"""
    paynehanek(x::Float64)

Reduce `x > 0` modulo π/16 for arbitrarily large `x`, using the Payne-Hanek algorithm. Returns
`(n, z_hi, z_lo)` with `0 <= n <= 32`, `|z_hi + z_lo| <= 1/2` and `|z_lo| <= ulp(z_hi)/2`, such that
``x ≡ π/16*(n + z_{hi} + z_{lo}) \\pmod{2π}``.
"""
function paynehanek(x::Float64)
    # 1. Write x = X*2^k, where X is the 53-bit integer significand and k = exponent(x) - 52.
    u = reinterpret(UInt64, x)
    X = (u & significand_mask(Float64)) | (one(UInt64) << significand_bits(Float64))
    raw_exponent = ((u & exponent_mask(Float64)) >> significand_bits(Float64)) % Int
    k = raw_exponent - exponent_bias(Float64) - significand_bits(Float64)

    # 2. With α = 1/2π, α*x mod 1 ≡ [(α*2^k mod 1)*X] mod 1, so the first k bits of α can be
    # skipped: take the next three 64-bit words a1, a2, a3 of α from INV_2PI.
    # (idx, shift = divrem(k, 64), but divrem is slower.)
    idx = k >> 6
    shift = k - (idx << 6)
    @assume_effects :nothrow :noub @inbounds if shift == 0
        a1 = INV_2PI[idx+1]
        a2 = INV_2PI[idx+2]
        a3 = INV_2PI[idx+3]
    else
        # use shifts to extract the relevant 64 bit window
        a1 = (idx < 0 ? zero(UInt64) : INV_2PI[idx+1] << shift) | (INV_2PI[idx+2] >> (64 - shift))
        a2 = (INV_2PI[idx+2] << shift) | (INV_2PI[idx+3] >> (64 - shift))
        a3 = (INV_2PI[idx+3] << shift) | (INV_2PI[idx+4] >> (64 - shift))
    end

    # 3. Multiply, keeping only the fraction w of α*x as a 128-bit fixed-point number
    # (the integer part and the lowest bits are dropped):
    #
    #      X.  0  0  0
    #   ×  0. a1 a2 a3
    #   ==============
    #      _.  w  w  _
    w1 = UInt128(X *% a1) << 64 # overflow becomes integer
    w2 = widemul(X, a2)
    w3 = widemul(X, a3) >> 64
    w = w1 +% w2 +% w3

    # 4. Round to the nearest multiple n of π/16, leaving the fraction f/2^128 in [-1/2, 1/2).
    n = (((w>>122)%Int + 1)>>1)
    f = (w<<5) % Int128
    return n, fromfraction(f)...
end

## rem_pio16

# rem_pio16(a) takes a = |x| for finite x and returns n and the remainder z, with |z| <= 1/2 and
# a = π/16*(n + z). Float16 and Float32 return (z, n), with z in Float32 and Float64 respectively;
# Float64 returns (rh, rl, n), with the reduced angle π/16*z = rh + rl in radians.

# Radians, Float16 (in Float32). 16/π ≈ Float32(INV_PIO16) + 2.0546042f-7, and the fma subtracts n
# from the exact product of a with the first part, so only the small remainder is rounded.
@inline function rem_pio16(a::Float16)
    af = Float32(a)
    id, n = roundmul(af, Float32(INV_PIO16))
    return muladd(af, 2.0546042f-7, fma(af, Float32(INV_PIO16), -id)), n
end

# Radians, Float32 (in Float64). For a < 2^26, 16/π is split so that the high part times a is exact.
# n is rounded from the full product, so that |z| <= 1/2.
@inline function rem_pio16(a::Float32)
    a < 0x1p26 || return rem_pio16_large(a)
    ad = Float64(a)
    idh = 0x1.45f306ep+2*ad # exact: 25-bit constant times 24-bit a
    idl = -0x1.b1bbead603d8bp-29*ad
    id, n = roundmul(ad, INV_PIO16)
    return (idh - id) + idl, n
end
@noinline function rem_pio16_large(a::Float32)
    n, z, _ = paynehanek(Float64(a))
    return z, n
end

# Radians, Float64. For a < 2^30, π/16 = PIO16_HI + PIO16_LO + P3: n*PIO16_HI is subtracted
# exactly with an fma and n*PIO16_LO with Fast2Sum, keeping its rounding error in rl. Larger
# arguments use Payne-Hanek.
@inline function rem_pio16(a::Float64)
    a < 0x1p30 || return rem_pio16_large(a)
    n, k = roundmul(a, INV_PIO16)
    rh0 = fma(-n, PIO16_HI, a) # exact
    t = n*PIO16_LO
    rh = rh0 - t
    rl = (((rh0 - rh) - t) - fma(n, PIO16_LO, -t)) - n*-0x1.f1976b7ed8fbcp-113
    return rh, rl, k
end
@noinline function rem_pio16_large(a::Float64)
    n, z_hi, z_lo = paynehanek(a)
    # π/16*(z_hi + z_lo) as hi + lo
    rh = z_hi*PIO16_HI
    return rh, fma(z_hi, PIO16_HI, -rh) + muladd(z_lo, PIO16_HI, z_hi*PIO16_LO), n
end

## rem2pi

# 2π as an unevaluated sum hi + lo
const pi4o2_h  = 6.283185307179586      # convert(Float64, pi * BigFloat(2))
const pi4o2_l  = 2.4492935982947064e-16 # convert(Float64, pi * BigFloat(2) - pi4o2_h)

# Returns (rh, rl, k) with x ≡ π/16*k + rh + rl (mod 2π), 0 <= k < 32 and |rh + rl| <= π/32.
@inline function rem2pi_kernel(x::Float64)
    rh, rl, n = rem_pio16(abs(x))
    return flipsign(rh, x), flipsign(rl, x), flipsign(n, x) & 31
end

# π/16*k + rh + rl, rounded to Float64
@inline function add_kpio16(rh::Float64, rl::Float64, k::Int)
    kf = Float64(k)
    kh = kf*PIO16_HI
    return add22condh(rh, rl, kh, muladd(kf, PIO16_LO, fma(kf, PIO16_HI, -kh)))
end

function rem2pi(x::Float64, ::RoundingMode{:Nearest})
    isnan(x) && return x
    isinf(x) && return NaN

    abs(x) < pi && return x

    rh, rl, k = rem2pi_kernel(x)
    # result in [-π, π]
    k = (k > 16 || (k == 16 && rh > 0)) ? k - 32 : k
    return add_kpio16(rh, rl, k)
end
function rem2pi(x::Float64, ::RoundingMode{:ToZero})
    isnan(x) && return x
    isinf(x) && return NaN

    ax = abs(x)
    ax <= 2*Float64(pi,RoundDown) && return x

    return copysign(rem2pi(ax, RoundDown), x)
end
function rem2pi(x::Float64, ::RoundingMode{:Down})
    isnan(x) && return x
    isinf(x) && return NaN

    if x < pi4o2_h
        if x >= 0
            return x
        elseif x > -pi4o2_h
            return add22condh(x,0.0,pi4o2_h,pi4o2_l)
        end
    end

    rh, rl, k = rem2pi_kernel(x)
    # result in [0, 2π)
    k = (k == 0 && rh < 0) ? 32 : k
    return add_kpio16(rh, rl, k)
end
function rem2pi(x::Float64, ::RoundingMode{:Up})
    isnan(x) && return x
    isinf(x) && return NaN

    if x > -pi4o2_h
        if x <= 0
            return x
        elseif x < pi4o2_h
            return add22condh(x,0.0,-pi4o2_h,-pi4o2_l)
        end
    end

    rh, rl, k = rem2pi_kernel(x)
    # result in (-2π, 0]
    k = (k == 0 && rh < 0) ? 0 : k - 32
    return add_kpio16(rh, rl, k)
end

rem2pi(x::Float32, r::RoundingMode) = Float32(rem2pi(Float64(x), r))
rem2pi(x::Float16, r::RoundingMode) = Float16(rem2pi(Float64(x), r))
rem2pi(x::Int32, r::RoundingMode) = rem2pi(Float64(x), r)

# general fallback
function rem2pi(x::Integer, r::RoundingMode)
    fx = float(x)
    fx == x || throw(ArgumentError(LazyString(typeof(x), " argument to rem2pi is too large: ", x)))
    rem2pi(fx, r)
end
