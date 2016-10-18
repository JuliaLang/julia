# This file is a part of Julia. License is MIT: http://julialang.org/license
"""
    RTLIB

Implements the runtime library for Julia. The implementations are based on
llvm's compiler-rt. This implementations follows the compiler-rt naming convention
and registers the pure Julia implementation as `extern_c` so that LLVM can find them.

As a secondary interface `RTLIB.convert(::Type{T}, x)` is provided.
"""
module RTLIB

register(f::Function, rtype::ANY, argt::ANY, name::String) =
    ccall(:jl_extern_c, Void, (Any, Any, Any, Cstring),
          f, rtype, argt, name)

include("rtlib/fp_util.jl")
include("rtlib/fp_extend.jl")
include("rtlib/fp_trunc.jl")
include("rtlib/fp_fixint.jl")

# All these function names are enumerated in lib/CodeGen/TargetLoweringBase.cpp
# right now we don't have a good way of getting at this information.

###
# Floating point extend and trunc functions
###

# "convert Float64 to Float128"
# extenddftf2(x::Float64) = extendXfYf2(Float128, x)
# convert(::Type{Float128}, x::Float64) = extenddftf2(x)

# "convert Float32 to Float128"
# extendsftf2(x::Float32) = extendXfYf2(Float128, x)
# convert(::Type{Float128}, x::Float32) = extendsftf2(x)

"convert Float32 to Float64"
extendsfdf2(x::Float32) = extendXfYf2(Float64, x)
convert(::Type{Float64}, x::Float32) = extendsfdf2(x)

"convert Float16 to Float32"
extendhfsf2(x::Float16) = extendXfYf2(Float32, x)
convert(::Type{Float32}, x::Float16) = extendhfsf2(x)

"convert Float32 to Float16"
truncsfhf2(x::Float32) = truncXfYf2(Float16, x)
convert(::Type{Float16}, x::Float32) = truncsfhf2(x)

"convert Float64 to Float16"
truncdfhf2(x::Float64) = truncXfYf2(Float16, x)
convert(::Type{Float16}, x::Float64) = truncdfhf2(x)

# "convert Float128 to Float16"
# trunctfhf2(x :: Float128) = truncXfYf2(Float16, x)
# convert(::Type{Float16}, x::Float128) = trunctfhf2(x)

"convert Float64 to Float32"
truncdfsf2(x::Float64) = truncXfYf2(Float32, x)
convert(::Type{Float32}, x::Float64) = truncdfsf2(x)

# "convert Float128 to Float32"
# trunctfsf2(x :: Float128) = truncXfYf2(Float32, x)
# convert(::Type{Float32}, x::Float128) = trunctfsf2(x)

# "convert Float128 to Float64"
# trunctfdf2(x :: Float128) = truncXfYf2(Float32, x)
# convert(::Type{Float64}, x::Float128) = trunctfdf2(x)

###
# Conversion between integers and floats
###

"convert Float32 to Int32"
fixsfsi(x::Float32) = fixint(Int32, x)
convert(::Type{Int32}, x::Float32) = fixsfsi(x)

"convert Float32 to Int64"
fixsfdi(x::Float32) = fixint(Int64, x)
convert(::Type{Int64}, x::Float32) = fixsfdi(x)

"convert Float32 to Int64"
fixsfti(x::Float32) = fixint(Int128, x)
convert(::Type{Int128}, x::Float32) = fixsfti(x)

"convert Float64 to Int32"
fixdfsi(x::Float64) = fixint(Int32, x)
convert(::Type{Int32}, x::Float64) = fixdfsi(x)

"convert Float64 to Int64"
fixdfdi(x::Float64) = fixint(Int64, x)
convert(::Type{Int64}, x::Float64) = fixdfdi(x)

"convert Float64 to Int64"
fixdfti(x::Float64) = fixint(Int128, x)
convert(::Type{Int128}, x::Float64) = fixdfti(x)

# "convert Float128 to Int32"
# fixtfsi(x::Float128) = fixint(Int32, x)
# convert(::Type{Int32}, x::Float128) = fixtfsi(x)

# "convert Float128 to Int64"
# fixtfdi(x::Float128) = fixint(Int64, x)
# convert(::Type{Int64}, x::Float128) = fixtfdi(x)

# "convert Float128 to Int64"
# fixtfti(x::Float128) = fixint(Int128, x)
# convert(::Type{Int128}, x::Float128) = fixtfti(x)

# Names[RTLIB::FPTOUINT_F32_I32] = "__fixunssfsi";
# Names[RTLIB::FPTOUINT_F32_I64] = "__fixunssfdi";
# Names[RTLIB::FPTOUINT_F32_I128] = "__fixunssfti";
# Names[RTLIB::FPTOUINT_F64_I32] = "__fixunsdfsi";
# Names[RTLIB::FPTOUINT_F64_I64] = "__fixunsdfdi";
# Names[RTLIB::FPTOUINT_F64_I128] = "__fixunsdfti";
# Names[RTLIB::FPTOUINT_F128_I32] = "__fixunstfsi";
# Names[RTLIB::FPTOUINT_F128_I64] = "__fixunstfdi";
# Names[RTLIB::FPTOUINT_F128_I128] = "__fixunstfti";
# Names[RTLIB::SINTTOFP_I32_F32] = "__floatsisf";
# Names[RTLIB::SINTTOFP_I32_F64] = "__floatsidf";
# Names[RTLIB::SINTTOFP_I32_F128] = "__floatsitf";
# Names[RTLIB::SINTTOFP_I64_F32] = "__floatdisf";
# Names[RTLIB::SINTTOFP_I64_F64] = "__floatdidf";
# Names[RTLIB::SINTTOFP_I64_F128] = "__floatditf";

"convert Int128 to Float32"
function floattisf(x::Int128)
    x == 0 && return 0f0
    s = ((x >>> 96) % UInt32) & 0x8000_0000 # sign bit
    x = abs(x) % UInt128
    n = 128-leading_zeros(x) # ndigits0z(x,2)
    if n <= 24
        y = ((x % UInt32) << (24-n)) & 0x007f_ffff
    else
        y = ((x >> (n-25)) % UInt32) & 0x00ff_ffff # keep 1 extra bit
        y = (y+one(UInt32))>>1 # round, ties up (extra leading bit in case of next exponent)
        y &= ~UInt32(trailing_zeros(x) == (n-25)) # fix last bit to round to even
    end
    d = ((n+126) % UInt32) << 23
    reinterpret(Float32, s | d + y)
end
convert(::Type{Float32}, x::Int128) = floattisf(x)

"convert Int128 to Float64"
function floattidf(x::Int128)
    x == 0 && return 0.0
    s = ((x >>> 64) % UInt64) & 0x8000_0000_0000_0000 # sign bit
    x = abs(x) % UInt128
    n = 128-leading_zeros(x) # ndigits0z(x,2)
    if n <= 53
        y = ((x % UInt64) << (53-n)) & 0x000f_ffff_ffff_ffff
    else
        y = ((x >> (n-54)) % UInt64) & 0x001f_ffff_ffff_ffff # keep 1 extra bit
        y = (y+1)>>1 # round, ties up (extra leading bit in case of next exponent)
        y &= ~UInt64(trailing_zeros(x) == (n-54)) # fix last bit to round to even
    end
    d = ((n+1022) % UInt64) << 52
    reinterpret(Float64, s | d + y)
end
convert(::Type{Float64}, x::Int128) = floattidf(x)

# Names[RTLIB::SINTTOFP_I128_F128] = "__floattitf";
# Names[RTLIB::UINTTOFP_I32_F32] = "__floatunsisf";
# Names[RTLIB::UINTTOFP_I32_F64] = "__floatunsidf";
# Names[RTLIB::UINTTOFP_I32_F128] = "__floatunsitf";
# Names[RTLIB::UINTTOFP_I64_F32] = "__floatundisf";
# Names[RTLIB::UINTTOFP_I64_F64] = "__floatundidf";
# Names[RTLIB::UINTTOFP_I64_F128] = "__floatunditf";

"convert UInt128 to Float32"
function floatuntisf(x::UInt128)
    x == 0 && return 0f0
    n = 128-leading_zeros(x) # ndigits0z(x,2)
    if n <= 24
        y = ((x % UInt32) << (24-n)) & 0x007f_ffff
    else
        y = ((x >> (n-25)) % UInt32) & 0x00ff_ffff # keep 1 extra bit
        y = (y+one(UInt32))>>1 # round, ties up (extra leading bit in case of next exponent)
        y &= ~UInt32(trailing_zeros(x) == (n-25)) # fix last bit to round to even
    end
    d = ((n+126) % UInt32) << 23
    reinterpret(Float32, d + y)
end
convert(::Type{Float32}, x::UInt128) = floatuntisf(x)

"convert UInt128 to Float64"
function floatuntidf(x::UInt128)
    x == 0 && return 0.0
    n = 128-leading_zeros(x) # ndigits0z(x,2)
    if n <= 53
        y = ((x % UInt64) << (53-n)) & 0x000f_ffff_ffff_ffff
    else
        y = ((x >> (n-54)) % UInt64) & 0x001f_ffff_ffff_ffff # keep 1 extra bit
        y = (y+1)>>1 # round, ties up (extra leading bit in case of next exponent)
        y &= ~UInt64(trailing_zeros(x) == (n-54)) # fix last bit to round to even
    end
    d = ((n+1022) % UInt64) << 52
    reinterpret(Float64, d + y)
end
convert(::Type{Float64}, x::UInt128) = floatuntidf(x)

# Names[RTLIB::UINTTOFP_I128_F128] = "__floatuntitf";
end

# RTLIB.register(RTLIB.extenddftf2, Float128, Tuple{Float64}, "__extenddftf2")
# RTLIB.register(RTLIB.extendsftf2, Float128, Tuple{Float32}, "__extendsftf2")
RTLIB.register(RTLIB.extendsfdf2, Float64, Tuple{Float32}, "__extendsfdf2")
if is_apple()
    RTLIB.register(RTLIB.extendhfsf2, Float32, Tuple{Float16}, "__extendhfsf2")
    RTLIB.register(RTLIB.truncsfhf2, Float16, Tuple{Float32}, "__truncsfhf2")
else
    RTLIB.register(RTLIB.extendhfsf2, Float32, Tuple{Float16}, "__gnu_h2f_ieee")
    RTLIB.register(RTLIB.truncsfhf2, Float16, Tuple{Float32}, "__gnu_f2h_ieee")
end
RTLIB.register(RTLIB.truncdfhf2, Float16, Tuple{Float64}, "__truncdfhf2")
# RTLIB.register(RTLIB.trunctfhf2, Float16, Tuple{Float128}, "__trunctfhf2")
RTLIB.register(RTLIB.truncdfsf2, Float32, Tuple{Float64}, "__truncdfsf2")
# RTLIB.register(RTLIB.trunctfsf2, Float32, Tuple{Float128}, "__trunctfsf2")
# RTLIB.register(RTLIB.trunctfdf2, Float64, Tuple{Float128}, "__trunctfdf2")

RTLIB.register(RTLIB.fixsfsi, Int32, Tuple{Float32}, "__fixsfsi")
RTLIB.register(RTLIB.fixsfdi, Int64, Tuple{Float32}, "__fixsfdi")
RTLIB.register(RTLIB.fixsfti, Int128, Tuple{Float32}, "__fixsfti")
RTLIB.register(RTLIB.fixdfsi, Int32, Tuple{Float64}, "__fixdfsi")
RTLIB.register(RTLIB.fixdfdi, Int64, Tuple{Float64}, "__fixdfdi")
RTLIB.register(RTLIB.fixdfti, Int128, Tuple{Float64}, "__fixdfti")
# RTLIB.register(RTLIB.fixtfsi, Int32, Tuple{Float128}, "__fixtfsi")
# RTLIB.register(RTLIB.fixtfdi, Int64, Tuple{Float128}, "__fixtfdi")
# RTLIB.register(RTLIB.fixtfti, Int128, Tuple{Float128}, "__fixtfti")

RTLIB.register(RTLIB.floattisf, Float32, Tuple{Int128}, "__floattisf")
RTLIB.register(RTLIB.floattidf, Float64, Tuple{Int128}, "__floattidf")
RTLIB.register(RTLIB.floatuntisf, Float32, Tuple{UInt128}, "__floatuntisf")
RTLIB.register(RTLIB.floatuntidf, Float64, Tuple{UInt128}, "__floatuntidf")
