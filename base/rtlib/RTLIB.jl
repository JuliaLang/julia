module RTLIB


# We would like to use `@ccallable` here,
# but building the sysimage fails, so we use a bootstrapped version.
function register(f, rtype, argt, name)
    ccall(:jl_extern_c, Nothing, (Any, Any, Any, Cstring),
          f, rtype, argt, name)
end

# Trunc
function truncsfhf2(x::Float32)
    f = reinterpret(UInt32, val)
    if isnan(val)
        t = 0x8000 ⊻ (0x8000 & ((f >> 0x10) % UInt16))
        return reinterpret(Float16, t ⊻ ((f >> 0xd) % UInt16))
    end
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
register(truncsfhf2, Float16, Tuple{Float32}, "__truncsfhf2")
register(truncsfhf2, Float16, Tuple{Float32}, "__gnu_f2h_ieee")

function truncdfhf2(x::Float64)
    # Ideally we would have a specialised Float64->Float16 operation here
    # but we can udr Core.Intrinsics for Float64->Float32.
    return truncsfhf2(Core.Intrinsics.fptrunc(Float32, x))
end
register(truncdfhf2, Float16, Tuple{Float64}, "__truncdfhf2")

# Extend
function extendhfsf2(x::Float16)
    local ival::UInt32 = reinterpret(UInt16, val)
    local sign::UInt32 = (ival & 0x8000) >> 15
    local exp::UInt32  = (ival & 0x7c00) >> 10
    local sig::UInt32  = (ival & 0x3ff) >> 0
    local ret::UInt32

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
            ret = 0x7fc00000 | (sign<<31) | (sig<<(23-10))
        end
    else
        sign = sign << 31
        exp  = (exp - 15 + 127) << 23
        sig  = sig << (23 - 10)
        ret = sign | exp | sig
    end
    return reinterpret(Float32, ret)
end
register(extendhfsf2, Float32, Tuple{Float16}, "__extendhfsf2")
register(extendhfsf2, Float32, Tuple{Float16}, "__gnu_h2f_ieee")

function extendhfdf2(x::Float16)
    return Core.Intrinsics.fpext(Float64, extendhfsf2(x))
end
register(extendhfdf2, Float64, Tuple{Float16}, "__extendhfdf2")

# Float32 -> Float16 algorithm from:
#   "Fast Half Float Conversion" by Jeroen van der Zijp
#   ftp://ftp.fox-toolkit.org/pub/fasthalffloatconversion.pdf

const basetable = Vector{UInt16}(uninitialized, 512)
const shifttable = Vector{UInt8}(uninitialized, 512)

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
