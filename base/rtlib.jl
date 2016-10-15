###
# In this file we implement the RTLIB libcalls llvm emits
##
module RTLIB

register(f::Function, rtype::ANY, argt::ANY, name::String) =
    ccall(:jl_extern_c, Void, (Any, Any, Any, Cstring),
          f, rtype, argt, name)

# All these function names are enumerated in lib/CodeGen/TargetLoweringBase.cpp
# right now we don't have a good way of getting at this information.

###
# Floating point extend and trunc functions
###

# Names[RTLIB::FPEXT_F64_F128] = "__extenddftf2";
# Names[RTLIB::FPEXT_F32_F128] = "__extendsftf2";
# Names[RTLIB::FPEXT_F32_F64] = "__extendsfdf2";

function extendhfsf2(val::Float16)
    local ival::UInt32 = reinterpret(UInt16, val),
          sign::UInt32 = (ival & 0x8000) >> 15,
          exp::UInt32  = (ival & 0x7c00) >> 10,
          sig::UInt32  = (ival & 0x3ff) >> 0,
          ret::UInt32

    if exp == 0
        if sig == 0
            sign = sign << 31
            ret = sign | exp | sig
        else
            n_bit = 1
            bit = 0x0200
            while (bit & sig) == 0
                n_bit = n_bit + 1
                bit = bit >> 1
            end
            sign = sign << 31
            exp = (-14 - n_bit + 127) << 23
            sig = ((sig & (~bit)) << n_bit) << (23 - 10)
            ret = sign | exp | sig
        end
    elseif exp == 0x1f
        if sig == 0  # Inf
            if sign == 0
                ret = 0x7f800000
            else
                ret = 0xff800000
            end
        else  # NaN
            ret = 0x7fc00000 | (sign<<31)
        end
    else
        sign = sign << 31
        exp  = (exp - 15 + 127) << 23
        sig  = sig << (23 - 10)
        ret = sign | exp | sig
    end
    return reinterpret(Float32, ret)
end

function truncsfhf2(val::Float32)
    f = reinterpret(UInt32, val)
    i = (f >> 23) & 0x1ff + 1
    sh = shifttable[i]
    f &= 0x007fffff
    h::UInt16 = basetable[i] + (f >> sh)
    # round
    # NOTE: we maybe should ignore NaNs here, but the payload is
    # getting truncated anyway so "rounding" it might not matter
    nextbit = (f >> (sh-1)) & 1
    if nextbit != 0
        # Round halfway to even or check lower bits
        if h&1 == 1 || (f & ((1<<(sh-1))-1)) != 0
            h += 1
        end
    end
    reinterpret(Float16, h)
end

function truncdfhf2(x::Float64)
    throw(MethodError(truncdfhf2, x))
end

# function trunctfhf2(x :: Float128)
#   return truncsfhf2(convert(Float32, x))
# end
# register(trunctfhf2, Float16, Tuple{Float128}, "__trunctfhf2")

# Names[RTLIB::FPROUND_F64_F32] = "__truncdfsf2";
# Names[RTLIB::FPROUND_F128_F32] = "__trunctfsf2";
# Names[RTLIB::FPROUND_F128_F64] = "__trunctfdf2";

###
# Conversion between integers and floats
###
# Names[RTLIB::FPTOSINT_F32_I32] = "__fixsfsi";
# Names[RTLIB::FPTOSINT_F32_I64] = "__fixsfdi";
# Names[RTLIB::FPTOSINT_F32_I128] = "__fixsfti";
# Names[RTLIB::FPTOSINT_F64_I32] = "__fixdfsi";
# Names[RTLIB::FPTOSINT_F64_I64] = "__fixdfdi";
# Names[RTLIB::FPTOSINT_F64_I128] = "__fixdfti";
# Names[RTLIB::FPTOSINT_F128_I32] = "__fixtfsi";
# Names[RTLIB::FPTOSINT_F128_I64] = "__fixtfdi";
# Names[RTLIB::FPTOSINT_F128_I128] = "__fixtfti";
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

# Names[RTLIB::SINTTOFP_I128_F128] = "__floattitf";
# Names[RTLIB::UINTTOFP_I32_F32] = "__floatunsisf";
# Names[RTLIB::UINTTOFP_I32_F64] = "__floatunsidf";
# Names[RTLIB::UINTTOFP_I32_F128] = "__floatunsitf";
# Names[RTLIB::UINTTOFP_I64_F32] = "__floatundisf";
# Names[RTLIB::UINTTOFP_I64_F64] = "__floatundidf";
# Names[RTLIB::UINTTOFP_I64_F128] = "__floatunditf";

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

# Names[RTLIB::UINTTOFP_I128_F128] = "__floatuntitf";


###
# helpers
###

# Float32 -> Float16 algorithm from:
#   "Fast Half Float Conversion" by Jeroen van der Zijp
#   ftp://ftp.fox-toolkit.org/pub/fasthalffloatconversion.pdf

const basetable = Array{UInt16}(512)
const shifttable = Array{UInt8}(512)

for i = 0:255
    e = i - 127
    if e < -24  # Very small numbers map to zero
        basetable[i|0x000+1] = 0x0000
        basetable[i|0x100+1] = 0x8000
        shifttable[i|0x000+1] = 24
        shifttable[i|0x100+1] = 24
    elseif e < -14  # Small numbers map to denorms
        basetable[i|0x000+1] = (0x0400>>(-e-14))
        basetable[i|0x100+1] = (0x0400>>(-e-14)) | 0x8000
        shifttable[i|0x000+1] = -e-1
        shifttable[i|0x100+1] = -e-1
    elseif e <= 15  # Normal numbers just lose precision
        basetable[i|0x000+1] = ((e+15)<<10)
        basetable[i|0x100+1] = ((e+15)<<10) | 0x8000
        shifttable[i|0x000+1] = 13
        shifttable[i|0x100+1] = 13
    elseif e < 128  # Large numbers map to Infinity
        basetable[i|0x000+1] = 0x7C00
        basetable[i|0x100+1] = 0xFC00
        shifttable[i|0x000+1] = 24
        shifttable[i|0x100+1] = 24
    else  # Infinity and NaN's stay Infinity and NaN's
        basetable[i|0x000+1] = 0x7C00
        basetable[i|0x100+1] = 0xFC00
        shifttable[i|0x000+1] = 13
        shifttable[i|0x100+1] = 13
    end
end
end

if is_apple()
    RTLIB.register(RTLIB.extendhfsf2, Float32, Tuple{Float16}, "__extendhfsf2")
    RTLIB.register(RTLIB.truncsfhf2, Float16, Tuple{Float32}, "__truncsfhf2")
else
    RTLIB.register(RTLIB.extendhfsf2, Float32, Tuple{Float16}, "__gnu_h2f_ieee")
    RTLIB.register(RTLIB.truncsfhf2, Float16, Tuple{Float32}, "__gnu_f2h_ieee")
end
RTLIB.register(RTLIB.truncdfhf2, Float16, Tuple{Float64}, "__truncdfhf2")
RTLIB.register(RTLIB.floattisf, Float32, Tuple{Int128}, "__floattisf")
RTLIB.register(RTLIB.floattidf, Float64, Tuple{Int128}, "__floattidf")
RTLIB.register(RTLIB.floatuntisf, Float32, Tuple{UInt128}, "__floatuntisf")
RTLIB.register(RTLIB.floatuntidf, Float64, Tuple{UInt128}, "__floatuntidf")

