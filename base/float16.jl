# This file is a part of Julia. License is MIT: http://julialang.org/license
import Base.llvmcall
# Implement conversion to and from Float16 with llvm intrinsics
convert(::Type{Float32}, val::Float16) =
    llvmcall(("""declare float @llvm.convert.from.fp16.f32(i16)""",
              """%2 = call float @llvm.convert.from.fp16.f32(i16 %0)
                 ret float %2"""),
              Float32, Tuple{Float16}, val)

convert(::Type{Float64}, val::Float16) =
    llvmcall(("""declare double @llvm.convert.from.fp16.f64(i16)""",
              """%2 = call double @llvm.convert.from.fp16.f64(i16 %0)
                 ret double %2"""),
              Float64, Tuple{Float16}, val)

convert(::Type{Float16}, val::Float32) =
    llvmcall(("""declare i16 @llvm.convert.to.fp16.f32(float)""",
              """%2 = call i16 @llvm.convert.to.fp16.f32(float %0)
                 ret i16 %2"""),
              Float16, Tuple{Float32}, val)

convert(::Type{Float16}, val::Float64) =
    llvmcall(("""declare i16 @llvm.convert.to.fp16.f64(double)""",
              """%2 = call i16 @llvm.convert.to.fp16.f64(double %0)
                 ret i16 %2"""),
              Float16, Tuple{Float64}, val)

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
        $func(a::Float16,b::Float16) = Float16($func(Float32(a),Float32(b)))
    end
end

ldexp(a::Float16, b::Integer) = Float16(ldexp(Float32(a), b))

^(x::Float16, y::Integer) = Float16(Float32(x)^y)

rationalize{T<:Integer}(::Type{T}, x::Float16; tol::Real=eps(x)) = rationalize(T, Float32(x); tol=tol)

reinterpret(::Type{Unsigned}, x::Float16) = reinterpret(UInt16,x)
reinterpret(::Type{Signed}, x::Float16) = reinterpret(Int16,x)

sign_mask(::Type{Float16}) =        0x8000
exponent_mask(::Type{Float16}) =    0x7c00
exponent_one(::Type{Float16}) =     0x3c00
exponent_half(::Type{Float16}) =    0x3800
significand_mask(::Type{Float16}) = 0x03ff
