# This file is a part of Julia. License is MIT: https://julialang.org/license

# Remainders modulo multiples of π: rem_pio16 reduces by π/16 for the trigonometric kernels in
# trig.jl. Arguments too large for its Cody-Waite reductions use Payne-Hanek (in rem_pio2.jl).

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
    n, y = paynehanek(Float64(a))
    t = y.hi*INV_PIO16
    id = round(t)
    return t - id, 8n + unsafe_trunc(Int, id)
end

# Radians, Float64. For a < 2^30, π/16 = PIO16_HI + PIO16_LO + P3: n*PIO16_HI is subtracted
# exactly with an fma and n*PIO16_LO with Fast2Sum, keeping its rounding error in rl. Larger
# arguments use rem_pio2_kernel.
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
    q, y = rem_pio2_kernel(a) # a = q*π/2 + y.hi + y.lo
    n = round(y.hi*INV_PIO16)
    rh0 = fma(-n, PIO16_HI, y.hi) # exact
    rl = y.lo - n*PIO16_LO
    rh = rh0 + rl
    return rh, (rh0 - rh) + rl, 8q + unsafe_trunc(Int, n)
end
