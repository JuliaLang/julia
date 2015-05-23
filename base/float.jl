# This file is a part of Julia. License is MIT: http://julialang.org/license

## floating point traits ##

const Inf16 = box(Float16,unbox(UInt16,0x7c00))
const NaN16 = box(Float16,unbox(UInt16,0x7e00))
const Inf32 = box(Float32,unbox(UInt32,0x7f800000))
const NaN32 = box(Float32,unbox(UInt32,0x7fc00000))
const Inf = box(Float64,unbox(UInt64,0x7ff0000000000000))
const NaN = box(Float64,unbox(UInt64,0x7ff8000000000000))

## conversions to floating-point ##
convert(::Type{Float16}, x::Integer) = convert(Float16, convert(Float32,x))
for t in (Int8,Int16,Int32,Int64,Int128,UInt8,UInt16,UInt32,UInt64,UInt128)
    @eval promote_rule(::Type{Float16}, ::Type{$t}) = Float32
end
promote_rule(::Type{Float16}, ::Type{Bool}) = Float16

for t1 in (Float32,Float64)
    for st in (Int8,Int16,Int32,Int64)
        @eval begin
            convert(::Type{$t1},x::($st)) = box($t1,sitofp($t1,unbox($st,x)))
            promote_rule(::Type{$t1}, ::Type{$st}  ) = $t1
        end
    end
    for ut in (Bool,UInt8,UInt16,UInt32,UInt64)
        @eval begin
            convert(::Type{$t1},x::($ut)) = box($t1,uitofp($t1,unbox($ut,x)))
            promote_rule(::Type{$t1}, ::Type{$ut}  ) = $t1
        end
    end
end

promote_rule(::Type{Float64}, ::Type{UInt128}) = Float64
promote_rule(::Type{Float64}, ::Type{Int128}) = Float64
promote_rule(::Type{Float32}, ::Type{UInt128}) = Float32
promote_rule(::Type{Float32}, ::Type{Int128}) = Float32

function convert(::Type{Float64}, x::UInt128)
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

function convert(::Type{Float64}, x::Int128)
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

function convert(::Type{Float32}, x::UInt128)
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

function convert(::Type{Float32}, x::Int128)
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

#convert(::Type{Float16}, x::Float32) = box(Float16,fptrunc(Float16,x))
convert(::Type{Float16}, x::Float64) = convert(Float16, convert(Float32,x))
convert(::Type{Float32}, x::Float64) = box(Float32,fptrunc(Float32,unbox(Float64,x)))

#convert(::Type{Float32}, x::Float16) = box(Float32,fpext(Float32,x))
convert(::Type{Float64}, x::Float16) = convert(Float64, convert(Float32,x))
convert(::Type{Float64}, x::Float32) = box(Float64,fpext(Float64,unbox(Float32,x)))

convert(::Type{FloatingPoint}, x::Bool)    = convert(Float64, x)
convert(::Type{FloatingPoint}, x::Int8)    = convert(Float64, x)
convert(::Type{FloatingPoint}, x::Int16)   = convert(Float64, x)
convert(::Type{FloatingPoint}, x::Int32)   = convert(Float64, x)
convert(::Type{FloatingPoint}, x::Int64)   = convert(Float64, x) # LOSSY
convert(::Type{FloatingPoint}, x::Int128)  = convert(Float64, x) # LOSSY
convert(::Type{FloatingPoint}, x::UInt8)   = convert(Float64, x)
convert(::Type{FloatingPoint}, x::UInt16)  = convert(Float64, x)
convert(::Type{FloatingPoint}, x::UInt32)  = convert(Float64, x)
convert(::Type{FloatingPoint}, x::UInt64)  = convert(Float64, x) # LOSSY
convert(::Type{FloatingPoint}, x::UInt128) = convert(Float64, x) # LOSSY

float(x) = convert(FloatingPoint, x)

for Ti in (Int8, Int16, Int32, Int64)
    @eval begin
        unsafe_trunc(::Type{$Ti}, x::Float32) = box($Ti,fptosi($Ti,unbox(Float32,x)))
        unsafe_trunc(::Type{$Ti}, x::Float64) = box($Ti,fptosi($Ti,unbox(Float64,x)))
    end
end
for Ti in (UInt8, UInt16, UInt32, UInt64)
    @eval begin
        unsafe_trunc(::Type{$Ti}, x::Float32) = box($Ti,fptoui($Ti,unbox(Float32,x)))
        unsafe_trunc(::Type{$Ti}, x::Float64) = box($Ti,fptoui($Ti,unbox(Float64,x)))
    end
end

function unsafe_trunc(::Type{UInt128}, x::Float64)
    xu = reinterpret(UInt64,x)
    k = Int(xu >> 52) & 0x07ff - 1075
    xu = (xu & 0x000f_ffff_ffff_ffff) | 0x0010_0000_0000_0000
    if k <= 0
        UInt128(xu >> -k)
    else
        UInt128(xu) << k
    end
end
function unsafe_trunc(::Type{Int128}, x::Float64)
    copysign(unsafe_trunc(UInt128,x) % Int128, x)
end

function unsafe_trunc(::Type{UInt128}, x::Float32)
    xu = reinterpret(UInt32,x)
    k = Int(xu >> 23) & 0x00ff - 150
    xu = (xu & 0x007f_ffff) | 0x0080_0000
    if k <= 0
        UInt128(xu >> -k)
    else
        UInt128(xu) << k
    end
end
function unsafe_trunc(::Type{Int128}, x::Float32)
    copysign(unsafe_trunc(UInt128,x) % Int128, x)
end


# matches convert methods
# also determines floor, ceil, round
trunc(::Type{Signed}, x::Float32) = trunc(Int,x)
trunc(::Type{Signed}, x::Float64) = trunc(Int,x)
trunc(::Type{Unsigned}, x::Float32) = trunc(UInt,x)
trunc(::Type{Unsigned}, x::Float64) = trunc(UInt,x)
trunc(::Type{Integer}, x::Float32) = trunc(Int,x)
trunc(::Type{Integer}, x::Float64) = trunc(Int,x)

# fallbacks
floor{T<:Integer}(::Type{T}, x::FloatingPoint) = trunc(T,floor(x))
ceil {T<:Integer}(::Type{T}, x::FloatingPoint) = trunc(T,ceil(x))
round {T<:Integer}(::Type{T}, x::FloatingPoint) = trunc(T,round(x))

trunc(x::Float64) = box(Float64,trunc_llvm(unbox(Float64,x)))
trunc(x::Float32) = box(Float32,trunc_llvm(unbox(Float32,x)))

floor(x::Float64) = box(Float64,floor_llvm(unbox(Float64,x)))
floor(x::Float32) = box(Float32,floor_llvm(unbox(Float32,x)))

ceil(x::Float64) = box(Float64,ceil_llvm(unbox(Float64,x)))
ceil(x::Float32) = box(Float32,ceil_llvm(unbox(Float32,x)))

round(x::Float64) = box(Float64,rint_llvm(unbox(Float64,x)))
round(x::Float32) = box(Float32,rint_llvm(unbox(Float32,x)))

## floating point promotions ##
promote_rule(::Type{Float32}, ::Type{Float16}) = Float32
promote_rule(::Type{Float64}, ::Type{Float16}) = Float64
promote_rule(::Type{Float64}, ::Type{Float32}) = Float64

widen(::Type{Float16}) = Float32
widen(::Type{Float32}) = Float64

## floating point arithmetic ##
-(x::Float32) = box(Float32,neg_float(unbox(Float32,x)))
-(x::Float64) = box(Float64,neg_float(unbox(Float64,x)))

+(x::Float32, y::Float32) = box(Float32,add_float(unbox(Float32,x),unbox(Float32,y)))
+(x::Float64, y::Float64) = box(Float64,add_float(unbox(Float64,x),unbox(Float64,y)))
-(x::Float32, y::Float32) = box(Float32,sub_float(unbox(Float32,x),unbox(Float32,y)))
-(x::Float64, y::Float64) = box(Float64,sub_float(unbox(Float64,x),unbox(Float64,y)))
*(x::Float32, y::Float32) = box(Float32,mul_float(unbox(Float32,x),unbox(Float32,y)))
*(x::Float64, y::Float64) = box(Float64,mul_float(unbox(Float64,x),unbox(Float64,y)))
/(x::Float32, y::Float32) = box(Float32,div_float(unbox(Float32,x),unbox(Float32,y)))
/(x::Float64, y::Float64) = box(Float64,div_float(unbox(Float64,x),unbox(Float64,y)))

muladd(x::Float32, y::Float32, z::Float32) = box(Float32,muladd_float(unbox(Float32,x),unbox(Float32,y),unbox(Float32,z)))
muladd(x::Float64, y::Float64, z::Float64) = box(Float64,muladd_float(unbox(Float64,x),unbox(Float64,y),unbox(Float64,z)))

# TODO: faster floating point div?
# TODO: faster floating point fld?
# TODO: faster floating point mod?
rem(x::Float32, y::Float32) = box(Float32,rem_float(unbox(Float32,x),unbox(Float32,y)))
rem(x::Float64, y::Float64) = box(Float64,rem_float(unbox(Float64,x),unbox(Float64,y)))

cld{T<:FloatingPoint}(x::T, y::T) = -fld(-x,y)

function mod{T<:FloatingPoint}(x::T, y::T)
    r = rem(x,y)
    if r == 0
        copysign(r,y)
    elseif (r > 0) $ (y > 0)
        r+y
    else
        r
    end
end

## floating point comparisons ##
==(x::Float32, y::Float32) = eq_float(unbox(Float32,x),unbox(Float32,y))
==(x::Float64, y::Float64) = eq_float(unbox(Float64,x),unbox(Float64,y))
!=(x::Float32, y::Float32) = ne_float(unbox(Float32,x),unbox(Float32,y))
!=(x::Float64, y::Float64) = ne_float(unbox(Float64,x),unbox(Float64,y))
< (x::Float32, y::Float32) = lt_float(unbox(Float32,x),unbox(Float32,y))
< (x::Float64, y::Float64) = lt_float(unbox(Float64,x),unbox(Float64,y))
<=(x::Float32, y::Float32) = le_float(unbox(Float32,x),unbox(Float32,y))
<=(x::Float64, y::Float64) = le_float(unbox(Float64,x),unbox(Float64,y))

isequal(x::Float32, y::Float32) = fpiseq(unbox(Float32,x),unbox(Float32,y))
isequal(x::Float64, y::Float64) = fpiseq(unbox(Float64,x),unbox(Float64,y))
isless (x::Float32, y::Float32) = fpislt(unbox(Float32,x),unbox(Float32,y))
isless (x::Float64, y::Float64) = fpislt(unbox(Float64,x),unbox(Float64,y))

function cmp(x::FloatingPoint, y::FloatingPoint)
    (isnan(x) || isnan(y)) && throw(DomainError())
    ifelse(x<y, -1, ifelse(x>y, 1, 0))
end

function cmp(x::Real, y::FloatingPoint)
    isnan(y) && throw(DomainError())
    ifelse(x<y, -1, ifelse(x>y, 1, 0))
end

function cmp(x::FloatingPoint, y::Real)
    isnan(x) && throw(DomainError())
    ifelse(x<y, -1, ifelse(x>y, 1, 0))
end

for Ti in (Int64,UInt64,Int128,UInt128)
    for Tf in (Float32,Float64)
        @eval begin
            function ==(x::$Tf, y::$Ti)
                fy = ($Tf)(y)
                (x == fy) & (y == unsafe_trunc($Ti,fy))
            end
            ==(y::$Ti, x::$Tf) = x==y

            function <(x::$Ti, y::$Tf)
                fx = ($Tf)(x)
                (fx < y) | ((fx == y) & ((fx == $(Tf(typemax(Ti)))) | (x < unsafe_trunc($Ti,fx)) ))
            end
            function <=(x::$Ti, y::$Tf)
                fx = ($Tf)(x)
                (fx < y) | ((fx == y) & ((fx == $(Tf(typemax(Ti)))) | (x <= unsafe_trunc($Ti,fx)) ))
            end

            function <(x::$Tf, y::$Ti)
                fy = ($Tf)(y)
                (x < fy) | ((x == fy) & (fy < $(Tf(typemax(Ti)))) & (unsafe_trunc($Ti,fy) < y))
            end
            function <=(x::$Tf, y::$Ti)
                fy = ($Tf)(y)
                (x < fy) | ((x == fy) & (fy < $(Tf(typemax(Ti)))) & (unsafe_trunc($Ti,fy) <= y))
            end
        end
    end
end

==(x::Float32, y::Union(Int32,UInt32)) = Float64(x)==Float64(y)
==(x::Union(Int32,UInt32), y::Float32) = Float64(x)==Float64(y)

<(x::Float32, y::Union(Int32,UInt32)) = Float64(x)<Float64(y)
<(x::Union(Int32,UInt32), y::Float32) = Float64(x)<Float64(y)

<=(x::Float32, y::Union(Int32,UInt32)) = Float64(x)<=Float64(y)
<=(x::Union(Int32,UInt32), y::Float32) = Float64(x)<=Float64(y)

abs(x::Float64) = box(Float64,abs_float(unbox(Float64,x)))
abs(x::Float32) = box(Float32,abs_float(unbox(Float32,x)))

isnan(x::FloatingPoint) = x != x
isnan(x::Real) = false

isfinite(x::FloatingPoint) = x - x == 0
isfinite(x::Real) = decompose(x)[3] != 0
isfinite(x::Integer) = true

isinf(x::Real) = !isnan(x) & !isfinite(x)

## hashing small, built-in numeric types ##

hx(a::UInt64, b::Float64, h::UInt) = hash_uint64((3a + reinterpret(UInt64,b)) - h)
const hx_NaN = hx(UInt64(0), NaN, UInt(0  ))

hash(x::UInt64,  h::UInt) = hx(x, Float64(x), h)
hash(x::Int64,   h::UInt) = hx(reinterpret(UInt64,abs(x)), Float64(x), h)
hash(x::Float64, h::UInt) = isnan(x) ? (hx_NaN $ h) : hx(box(UInt64,fptoui(unbox(Float64,abs(x)))), x, h)

hash(x::Union(Bool,Char,Int8,UInt8,Int16,UInt16,Int32,UInt32), h::UInt) = hash(Int64(x), h)
hash(x::Float32, h::UInt) = hash(Float64(x), h)

## precision, as defined by the effective number of bits in the mantissa ##
precision(::Type{Float16}) = 11
precision(::Type{Float32}) = 24
precision(::Type{Float64}) = 53
precision{T<:FloatingPoint}(::T) = precision(T)

function float_lex_order(f::Integer, delta::Integer)
    # convert from signed magnitude to 2's complement and back
    neg = f < 0
    if neg
        f = oftype(f, -(f & typemax(f)))
    end
    f = oftype(f, f + delta)
    neg && f == 0 && return typemin(f)  # nextfloat(-5e-324) === -0.0
    f < 0 ? oftype(f, -(f & typemax(f))) : f
end

nextfloat(x::Float16, i::Integer) =
    (isinf(x)&&sign(x)==sign(i)) ? x : reinterpret(Float16,float_lex_order(reinterpret(Int16,x), i))
nextfloat(x::Float32, i::Integer) =
    (isinf(x)&&sign(x)==sign(i)) ? x : reinterpret(Float32,float_lex_order(reinterpret(Int32,x), i))
nextfloat(x::Float64, i::Integer) =
    (isinf(x)&&sign(x)==sign(i)) ? x : reinterpret(Float64,float_lex_order(reinterpret(Int64,x), i))
nextfloat(x::FloatingPoint) = nextfloat(x,1)
prevfloat(x::FloatingPoint) = nextfloat(x,-1)

for Ti in (Int8, Int16, Int32, Int64, Int128, UInt8, UInt16, UInt32, UInt64, UInt128)
    for Tf in (Float32, Float64)
        if sizeof(Ti) < sizeof(Tf) || Ti <: Unsigned # Tf(typemin(Ti))-1 is exact
            @eval function trunc(::Type{$Ti},x::$Tf)
                $(Tf(typemin(Ti))-one(Tf)) < x < $(Tf(typemax(Ti))+one(Tf)) || throw(InexactError())
                unsafe_trunc($Ti,x)
            end
        else
            @eval function trunc(::Type{$Ti},x::$Tf)
                $(Tf(typemin(Ti))) <= x < $(Tf(typemax(Ti))) || throw(InexactError())
                unsafe_trunc($Ti,x)
            end
        end
    end
end



@eval begin
    issubnormal(x::Float32) = (abs(x) < $(box(Float32,unbox(UInt32,0x00800000)))) & (x!=0)
    issubnormal(x::Float64) = (abs(x) < $(box(Float64,unbox(UInt64,0x0010000000000000)))) & (x!=0)

    typemin(::Type{Float16}) = $(box(Float16,unbox(UInt16,0xfc00)))
    typemax(::Type{Float16}) = $(Inf16)
    typemin(::Type{Float32}) = $(-Inf32)
    typemax(::Type{Float32}) = $(Inf32)
    typemin(::Type{Float64}) = $(-Inf)
    typemax(::Type{Float64}) = $(Inf)
    typemin{T<:Real}(x::T) = typemin(T)
    typemax{T<:Real}(x::T) = typemax(T)

    realmin(::Type{Float16}) = $(box(Float16,unbox(UInt16,0x0400)))
    realmin(::Type{Float32}) = $(box(Float32,unbox(UInt32,0x00800000)))
    realmin(::Type{Float64}) = $(box(Float64,unbox(UInt64,0x0010000000000000)))
    realmax(::Type{Float16}) = $(box(Float16,unbox(UInt16,0x7bff)))
    realmax(::Type{Float32}) = $(box(Float32,unbox(UInt32,0x7f7fffff)))
    realmax(::Type{Float64}) = $(box(Float64,unbox(UInt64,0x7fefffffffffffff)))
    realmin{T<:FloatingPoint}(x::T) = realmin(T)
    realmax{T<:FloatingPoint}(x::T) = realmax(T)
    realmin() = realmin(Float64)
    realmax() = realmax(Float64)

    eps(x::FloatingPoint) = isfinite(x) ? abs(x) >= realmin(x) ? ldexp(eps(typeof(x)),exponent(x)) : nextfloat(zero(x)) : oftype(x,NaN)
    eps(::Type{Float16}) = $(box(Float16,unbox(UInt16,0x1400)))
    eps(::Type{Float32}) = $(box(Float32,unbox(UInt32,0x34000000)))
    eps(::Type{Float64}) = $(box(Float64,unbox(UInt64,0x3cb0000000000000)))
    eps() = eps(Float64)
end

# fused multiply-add
fma_libm(x::Float32, y::Float32, z::Float32) =
    ccall(("fmaf", libm_name), Float32, (Float32,Float32,Float32), x, y, z)
fma_libm(x::Float64, y::Float64, z::Float64) =
    ccall(("fma", libm_name), Float64, (Float64,Float64,Float64), x, y, z)
fma_llvm(x::Float32, y::Float32, z::Float32) =
    box(Float32,fma_float(unbox(Float32,x),unbox(Float32,y),unbox(Float32,z)))
fma_llvm(x::Float64, y::Float64, z::Float64) =
    box(Float64,fma_float(unbox(Float64,x),unbox(Float64,y),unbox(Float64,z)))
# Disable LLVM's fma if it is incorrect, e.g. because LLVM falls back
# onto a broken system libm; if so, use openlibm's fma instead
# 1.0000305f0 = 1 + 1/2^15
# 1.0000000009313226 = 1 + 1/2^30
# If fma_llvm() clobbers the rounding mode, the result of 0.1 + 0.2 will be 0.3
# instead of the properly-rounded 0.30000000000000004; check after calling fma
if (fma_llvm(1.0000305f0, 1.0000305f0, -1.0f0) == 6.103609f-5 &&
    (fma_llvm(1.0000000009313226, 1.0000000009313226, -1.0) ==
     1.8626451500983188e-9) && 0.1 + 0.2 == 0.30000000000000004)
    fma(x::Float32, y::Float32, z::Float32) = fma_llvm(x,y,z)
    fma(x::Float64, y::Float64, z::Float64) = fma_llvm(x,y,z)
else
    fma(x::Float32, y::Float32, z::Float32) = fma_libm(x,y,z)
    fma(x::Float64, y::Float64, z::Float64) = fma_libm(x,y,z)
end
# This is necessary at least on 32-bit Intel Linux, since fma_llvm may
# have called glibc, and some broken glibc fma implementations don't
# properly restore the rounding mode
Rounding.set_rounding_raw(Float32, Rounding.JL_FE_TONEAREST)
Rounding.set_rounding_raw(Float64, Rounding.JL_FE_TONEAREST)

## byte order swaps for arbitrary-endianness serialization/deserialization ##
bswap(x::Float32) = box(Float32,bswap_int(unbox(Float32,x)))
bswap(x::Float64) = box(Float64,bswap_int(unbox(Float64,x)))

# bit patterns
reinterpret(::Type{Unsigned}, x::Float64) = reinterpret(UInt64,x)
reinterpret(::Type{Unsigned}, x::Float32) = reinterpret(UInt32,x)

sign_mask(::Type{Float64}) =        0x8000_0000_0000_0000
exponent_mask(::Type{Float64}) =    0x7ff0_0000_0000_0000
exponent_one(::Type{Float64}) =     0x3ff0_0000_0000_0000
exponent_half(::Type{Float64}) =    0x3fe0_0000_0000_0000
significand_mask(::Type{Float64}) = 0x000f_ffff_ffff_ffff

sign_mask(::Type{Float32}) =        0x8000_0000
exponent_mask(::Type{Float32}) =    0x7f80_0000
exponent_one(::Type{Float32}) =     0x3f80_0000
exponent_half(::Type{Float32}) =    0x3f00_0000
significand_mask(::Type{Float32}) = 0x007f_ffff

significand_bits{T<:FloatingPoint}(::Type{T}) = trailing_ones(significand_mask(T))
exponent_bits{T<:FloatingPoint}(::Type{T}) = sizeof(T)*8 - significand_bits(T) - 1
exponent_bias{T<:FloatingPoint}(::Type{T}) = Int(exponent_one(T) >> significand_bits(T))

## Array operations on floating point numbers ##

float{T<:FloatingPoint}(x::AbstractArray{T}) = x

float{T<:Integer64}(x::AbstractArray{T}) = convert(AbstractArray{typeof(float(zero(T)))}, x)

function float(A::AbstractArray)
    cnv(x) = convert(FloatingPoint,x)
    map_promote(cnv, A)
end

for fn in (:float,:big)
    @eval begin
        $fn(r::StepRange) = $fn(r.start):$fn(r.step):$fn(last(r))
        $fn(r::UnitRange) = $fn(r.start):$fn(last(r))
        $fn(r::FloatRange) = FloatRange($fn(r.start), $fn(r.step), r.len, $fn(r.divisor))
    end
end

big{T<:FloatingPoint,N}(x::AbstractArray{T,N}) = convert(AbstractArray{BigFloat,N}, x)
big{T<:Integer,N}(x::AbstractArray{T,N}) = convert(AbstractArray{BigInt,N}, x)
