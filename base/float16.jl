# This file is a part of Julia. License is MIT: http://julialang.org/license

function convert(::Type{Float32}, val::Float16)
    ival::UInt32 = reinterpret(UInt16, val)
    sign::UInt32 = (ival & 0x8000) >> 15
    exp::UInt32  = (ival & 0x7c00) >> 10
    sig::UInt32  = (ival & 0x3ff) >> 0
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

# Float32 -> Float16 algorithm from:
#   "Fast Half Float Conversion" by Jeroen van der Zijp
#   ftp://ftp.fox-toolkit.org/pub/fasthalffloatconversion.pdf

const basetable = Array(UInt16, 512)
const shifttable = Array(UInt8, 512)

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

function convert(::Type{Float16}, val::Float32)
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
        if h&1 == 1 ||  # round halfway to even
            (f & ((1<<(sh-1))-1)) != 0  # check lower bits
            h += 1
        end
    end
    reinterpret(Float16, h)
end

convert(::Type{Bool},    x::Float16) = x==0 ? false : x==1 ? true : throw(InexactError())
convert(::Type{Int128},  x::Float16) = convert(Int128, Float32(x))
convert(::Type{UInt128}, x::Float16) = convert(UInt128, Float32(x))

convert{T<:Integer}(::Type{T}, x::Float16) = convert(T, Float32(x))

round{T<:Integer}(::Type{T}, x::Float16) = round(T, Float32(x))
trunc{T<:Integer}(::Type{T}, x::Float16) = trunc(T, Float32(x))
floor{T<:Integer}(::Type{T}, x::Float16) = floor(T, Float32(x))
ceil{ T<:Integer}(::Type{T}, x::Float16) = ceil(T, Float32(x))

round(x::Float16) = Float16(round(Float32(x)))
trunc(x::Float16) = Float16(trunc(Float32(x)))
floor(x::Float16) = Float16(floor(Float32(x)))
 ceil(x::Float16) = Float16( ceil(Float32(x)))

isnan(x::Float16)    = reinterpret(UInt16,x)&0x7fff  > 0x7c00
isinf(x::Float16)    = reinterpret(UInt16,x)&0x7fff == 0x7c00
isfinite(x::Float16) = reinterpret(UInt16,x)&0x7c00 != 0x7c00

function ==(x::Float16, y::Float16)
    ix = reinterpret(UInt16,x)
    iy = reinterpret(UInt16,y)
    if (ix|iy)&0x7fff > 0x7c00 #isnan(x) || isnan(y)
        return false
    end
    if (ix|iy)&0x7fff == 0x0000
        return true
    end
    return ix == iy
end

-(x::Float16) = reinterpret(Float16, reinterpret(UInt16,x) $ 0x8000)
abs(x::Float16) = reinterpret(Float16, reinterpret(UInt16,x) & 0x7fff)
for op in (:+,:-,:*,:/,:\,:^)
    @eval ($op)(a::Float16, b::Float16) = Float16(($op)(Float32(a), Float32(b)))
end
function fma(a::Float16, b::Float16, c::Float16)
    Float16(fma(Float32(a), Float32(b), Float32(c)))
end
function muladd(a::Float16, b::Float16, c::Float16)
    Float16(muladd(Float32(a), Float32(b), Float32(c)))
end
for op in (:<,:<=,:isless)
    @eval ($op)(a::Float16, b::Float16) = ($op)(Float32(a), Float32(b))
end
for func in (:sin,:cos,:tan,:asin,:acos,:atan,:sinh,:cosh,:tanh,:asinh,:acosh,
             :atanh,:exp,:log,:log2,:log10,:sqrt,:lgamma,:log1p,:erf,:erfc)
    @eval begin
        $func(a::Float16) = Float16($func(Float32(a)))
        $func(a::Complex32) = Complex32($func(Complex64(a)))
    end
end

for func in (:div,:fld,:cld,:rem,:mod,:atan2,:hypot)
    @eval begin
        $func(a::Float16,b::Float16) = Float16($func(Float32(a),Float32(a)))
    end
end

ldexp(a::Float16, b::Integer) = Float16(ldexp(Float32(a), b))

^(x::Float16, y::Integer) = Float16(Float32(x)^y)

rationalize{T<:Integer}(::Type{T}, x::Float16; tol::Real=eps(x)) = rationalize(T, Float32(x); tol=tol)

reinterpret(::Type{Unsigned}, x::Float16) = reinterpret(UInt16,x)

sign_mask(::Type{Float16}) =        0x8000
exponent_mask(::Type{Float16}) =    0x7c00
exponent_one(::Type{Float16}) =     0x3c00
exponent_half(::Type{Float16}) =    0x3800
significand_mask(::Type{Float16}) = 0x03ff
