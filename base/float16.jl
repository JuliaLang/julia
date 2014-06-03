function convert(::Type{Float32}, val::Float16)
    ival::Uint32 = reinterpret(Uint16, val)
    sign::Uint32 = (ival & 0x8000) >> 15
    exp::Uint32  = (ival & 0x7c00) >> 10
    sig::Uint32  = (ival & 0x3ff) >> 0
    ret::Uint32

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

const basetable = Array(Uint16, 512)
const shifttable = Array(Uint8, 512)

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
    f = reinterpret(Uint32, val)
    i = (f >> 23) & 0x1ff + 1
    sh = shifttable[i]
    f &= 0x007fffff
    h::Uint16 = uint16(basetable[i] + (f >> sh))
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

convert(::Type{Integer}, x::Float16) = convert(Int,float32(x))

round(x::Float16) = float16(round(float32(x)))
trunc(x::Float16) = float16(trunc(float32(x)))
floor(x::Float16) = float16(floor(float32(x)))
 ceil(x::Float16) = float16( ceil(float32(x)))

isnan(x::Float16)    = reinterpret(Uint16,x)&0x7fff  > 0x7c00
isinf(x::Float16)    = reinterpret(Uint16,x)&0x7fff == 0x7c00
isfinite(x::Float16) = reinterpret(Uint16,x)&0x7c00 != 0x7c00

function ==(x::Float16, y::Float16)
    ix = reinterpret(Uint16,x)
    iy = reinterpret(Uint16,y)
    if (ix|iy)&0x7fff > 0x7c00 #isnan(x) || isnan(y)
        return false
    end
    if (ix|iy)&0x7fff == 0x0000
        return true
    end
    return ix == iy
end

-(x::Float16) = reinterpret(Float16, reinterpret(Uint16,x) $ 0x8000)
abs(x::Float16) = reinterpret(Float16, reinterpret(Uint16,x) & 0x7fff)
for op in (:+,:-,:*,:/,:\,:^)
    @eval ($op)(a::Float16, b::Float16) = float16(($op)(float32(a), float32(b)))
end
for op in (:<,:<=,:isless)
    @eval ($op)(a::Float16, b::Float16) = ($op)(float32(a), float32(b))
end
for func in (:sin,:cos,:tan,:asin,:acos,:atan,:sinh,:cosh,:tanh,:asinh,:acosh,
             :atanh,:exp,:log,:sqrt)
    @eval begin
        $func(a::Float16) = float16($func(float32(a)))
        $func(a::Complex32) = complex32($func(complex64(a)))
    end
end
atan2(a::Float16, b::Float16) = float16(atan2(float32(a), float32(b)))
hypot(a::Float16, b::Float16) = float16(hypot(float32(a), float32(b)))
ldexp(a::Float16, b::Integer) = float16(ldexp(float32(a), b))
exponent(x::Float16) = exponent(float32(x))
^(x::Float16, y::Integer) = x^float16(y)
