# This file is a part of Julia. License is MIT: https://julialang.org/license

const IEEEFloat = Union{Float16, Float32, Float64}

## floating point traits ##

"""
    Inf16

Positive infinity of type [`Float16`](@ref).
"""
const Inf16 = bitcast(Float16, 0x7c00)
"""
    NaN16

A not-a-number value of type [`Float16`](@ref).
"""
const NaN16 = bitcast(Float16, 0x7e00)
"""
    Inf32

Positive infinity of type [`Float32`](@ref).
"""
const Inf32 = bitcast(Float32, 0x7f800000)
"""
    NaN32

A not-a-number value of type [`Float32`](@ref).
"""
const NaN32 = bitcast(Float32, 0x7fc00000)
const Inf64 = bitcast(Float64, 0x7ff0000000000000)
const NaN64 = bitcast(Float64, 0x7ff8000000000000)

const Inf = Inf64
"""
    Inf, Inf64

Positive infinity of type [`Float64`](@ref).
"""
Inf, Inf64

const NaN = NaN64
"""
    NaN, NaN64

A not-a-number value of type [`Float64`](@ref).
"""
NaN, NaN64

# bit patterns
reinterpret(::Type{Unsigned}, x::Float64) = reinterpret(UInt64, x)
reinterpret(::Type{Unsigned}, x::Float32) = reinterpret(UInt32, x)
reinterpret(::Type{Unsigned}, x::Float16) = reinterpret(UInt16, x)
reinterpret(::Type{Signed}, x::Float64) = reinterpret(Int64, x)
reinterpret(::Type{Signed}, x::Float32) = reinterpret(Int32, x)
reinterpret(::Type{Signed}, x::Float16) = reinterpret(Int16, x)

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

sign_mask(::Type{Float16}) =        0x8000
exponent_mask(::Type{Float16}) =    0x7c00
exponent_one(::Type{Float16}) =     0x3c00
exponent_half(::Type{Float16}) =    0x3800
significand_mask(::Type{Float16}) = 0x03ff

for T in (Float16, Float32, Float64)
    @eval significand_bits(::Type{$T}) = $(trailing_ones(significand_mask(T)))
    @eval exponent_bits(::Type{$T}) = $(sizeof(T)*8 - significand_bits(T) - 1)
    @eval exponent_bias(::Type{$T}) = $(Int(exponent_one(T) >> significand_bits(T)))
    # maximum float exponent
    @eval exponent_max(::Type{$T}) = $(Int(exponent_mask(T) >> significand_bits(T)) - exponent_bias(T) - 1)
    # maximum float exponent without bias
    @eval exponent_raw_max(::Type{$T}) = $(Int(exponent_mask(T) >> significand_bits(T)))
end

"""
    exponent_max(T)

Maximum [`exponent`](@ref) value for a floating point number of type `T`.

# Examples
```jldoctest
julia> Base.exponent_max(Float64)
1023
```

Note, `exponent_max(T) + 1` is a possible value of the exponent field
with bias, which might be used as sentinel value for `Inf` or `NaN`.
"""
function exponent_max end

"""
    exponent_raw_max(T)

Maximum value of the [`exponent`](@ref) field for a floating point number of type `T` without bias,
i.e. the maximum integer value representable by [`exponent_bits(T)`](@ref) bits.
"""
function exponent_raw_max end

## conversions to floating-point ##

# TODO: deprecate in 2.0
Float16(x::Integer) = convert(Float16, convert(Float32, x)::Float32)

for t1 in (Float16, Float32, Float64)
    for st in (Int8, Int16, Int32, Int64)
        @eval begin
            (::Type{$t1})(x::($st)) = sitofp($t1, x)
            promote_rule(::Type{$t1}, ::Type{$st}) = $t1
        end
    end
    for ut in (Bool, UInt8, UInt16, UInt32, UInt64)
        @eval begin
            (::Type{$t1})(x::($ut)) = uitofp($t1, x)
            promote_rule(::Type{$t1}, ::Type{$ut}) = $t1
        end
    end
end

Bool(x::Real) = x==0 ? false : x==1 ? true : throw(InexactError(:Bool, Bool, x))

promote_rule(::Type{Float64}, ::Type{UInt128}) = Float64
promote_rule(::Type{Float64}, ::Type{Int128}) = Float64
promote_rule(::Type{Float32}, ::Type{UInt128}) = Float32
promote_rule(::Type{Float32}, ::Type{Int128}) = Float32
promote_rule(::Type{Float16}, ::Type{UInt128}) = Float16
promote_rule(::Type{Float16}, ::Type{Int128}) = Float16

function Float64(x::UInt128)
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

function Float64(x::Int128)
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

function Float32(x::UInt128)
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

function Float32(x::Int128)
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

# TODO: optimize
Float16(x::UInt128) = convert(Float16, Float32(x))
Float16(x::Int128)  = convert(Float16, Float32(x))

Float16(x::Float32) = fptrunc(Float16, x)
Float16(x::Float64) = fptrunc(Float16, x)
Float32(x::Float64) = fptrunc(Float32, x)

Float32(x::Float16) = fpext(Float32, x)
Float64(x::Float32) = fpext(Float64, x)
Float64(x::Float16) = fpext(Float64, x)

AbstractFloat(x::Bool)    = Float64(x)
AbstractFloat(x::Int8)    = Float64(x)
AbstractFloat(x::Int16)   = Float64(x)
AbstractFloat(x::Int32)   = Float64(x)
AbstractFloat(x::Int64)   = Float64(x) # LOSSY
AbstractFloat(x::Int128)  = Float64(x) # LOSSY
AbstractFloat(x::UInt8)   = Float64(x)
AbstractFloat(x::UInt16)  = Float64(x)
AbstractFloat(x::UInt32)  = Float64(x)
AbstractFloat(x::UInt64)  = Float64(x) # LOSSY
AbstractFloat(x::UInt128) = Float64(x) # LOSSY

Bool(x::Float16) = x==0 ? false : x==1 ? true : throw(InexactError(:Bool, Bool, x))

"""
    float(x)

Convert a number or array to a floating point data type.
"""
float(x) = AbstractFloat(x)

"""
    float(T::Type)

Return an appropriate type to represent a value of type `T` as a floating point value.
Equivalent to `typeof(float(zero(T)))`.

# Examples
```jldoctest
julia> float(Complex{Int})
ComplexF64 (alias for Complex{Float64})

julia> float(Int)
Float64
```
"""
float(::Type{T}) where {T<:Number} = typeof(float(zero(T)))
float(::Type{T}) where {T<:AbstractFloat} = T

"""
    unsafe_trunc(T, x)

Return the nearest integral value of type `T` whose absolute value is
less than or equal to `x`. If the value is not representable by `T`, an arbitrary value will
be returned.
"""
function unsafe_trunc end

for Ti in (Int8, Int16, Int32, Int64)
    @eval begin
        unsafe_trunc(::Type{$Ti}, x::Float16) = fptosi($Ti, x)
        unsafe_trunc(::Type{$Ti}, x::Float32) = fptosi($Ti, x)
        unsafe_trunc(::Type{$Ti}, x::Float64) = fptosi($Ti, x)
    end
end
for Ti in (UInt8, UInt16, UInt32, UInt64)
    @eval begin
        unsafe_trunc(::Type{$Ti}, x::Float16) = fptoui($Ti, x)
        unsafe_trunc(::Type{$Ti}, x::Float32) = fptoui($Ti, x)
        unsafe_trunc(::Type{$Ti}, x::Float64) = fptoui($Ti, x)
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

unsafe_trunc(::Type{UInt128}, x::Float16) = unsafe_trunc(UInt128, Float32(x))
unsafe_trunc(::Type{Int128}, x::Float16) = unsafe_trunc(Int128, Float32(x))

# matches convert methods
# also determines floor, ceil, round
trunc(::Type{Signed}, x::Float16) = trunc(Int,x)
trunc(::Type{Signed}, x::Float32) = trunc(Int,x)
trunc(::Type{Signed}, x::Float64) = trunc(Int,x)
trunc(::Type{Unsigned}, x::Float16) = trunc(UInt,x)
trunc(::Type{Unsigned}, x::Float32) = trunc(UInt,x)
trunc(::Type{Unsigned}, x::Float64) = trunc(UInt,x)
trunc(::Type{Integer}, x::Float16) = trunc(Int,x)
trunc(::Type{Integer}, x::Float32) = trunc(Int,x)
trunc(::Type{Integer}, x::Float64) = trunc(Int,x)

# fallbacks
floor(::Type{T}, x::AbstractFloat) where {T<:Integer} = trunc(T,round(x, RoundDown))
ceil(::Type{T}, x::AbstractFloat) where {T<:Integer} = trunc(T,round(x, RoundUp))
round(::Type{T}, x::AbstractFloat) where {T<:Integer} = trunc(T,round(x, RoundNearest))

round(x::Float64, r::RoundingMode{:ToZero})  = trunc_llvm(x)
round(x::Float32, r::RoundingMode{:ToZero})  = trunc_llvm(x)
round(x::Float16, r::RoundingMode{:ToZero})  = trunc_llvm(x)
round(x::Float64, r::RoundingMode{:Down})    = floor_llvm(x)
round(x::Float32, r::RoundingMode{:Down})    = floor_llvm(x)
round(x::Float16, r::RoundingMode{:Down})    = floor_llvm(x)
round(x::Float64, r::RoundingMode{:Up})      = ceil_llvm(x)
round(x::Float32, r::RoundingMode{:Up})      = ceil_llvm(x)
round(x::Float16, r::RoundingMode{:Up})      = ceil_llvm(x)
round(x::Float64, r::RoundingMode{:Nearest}) = rint_llvm(x)
round(x::Float32, r::RoundingMode{:Nearest}) = rint_llvm(x)
round(x::Float16, r::RoundingMode{:Nearest}) = rint_llvm(x)

## floating point promotions ##
promote_rule(::Type{Float32}, ::Type{Float16}) = Float32
promote_rule(::Type{Float64}, ::Type{Float16}) = Float64
promote_rule(::Type{Float64}, ::Type{Float32}) = Float64

widen(::Type{Float16}) = Float32
widen(::Type{Float32}) = Float64

## floating point arithmetic ##
-(x::Float64) = neg_float(x)
-(x::Float32) = neg_float(x)
-(x::Float16) = neg_float(x)

+(x::Float16, y::Float16) = add_float(x, y)
+(x::Float32, y::Float32) = add_float(x, y)
+(x::Float64, y::Float64) = add_float(x, y)
-(x::Float16, y::Float16) = sub_float(x, y)
-(x::Float32, y::Float32) = sub_float(x, y)
-(x::Float64, y::Float64) = sub_float(x, y)
*(x::Float16, y::Float16) = mul_float(x, y)
*(x::Float32, y::Float32) = mul_float(x, y)
*(x::Float64, y::Float64) = mul_float(x, y)
/(x::Float16, y::Float16) = div_float(x, y)
/(x::Float32, y::Float32) = div_float(x, y)
/(x::Float64, y::Float64) = div_float(x, y)

muladd(x::Float16, y::Float16, z::Float16) = muladd_float(x, y, z)
muladd(x::Float32, y::Float32, z::Float32) = muladd_float(x, y, z)
muladd(x::Float64, y::Float64, z::Float64) = muladd_float(x, y, z)

# TODO: faster floating point div?
# TODO: faster floating point fld?
# TODO: faster floating point mod?

rem(x::Float16, y::Float16) = rem_float(x, y)
rem(x::Float32, y::Float32) = rem_float(x, y)
rem(x::Float64, y::Float64) = rem_float(x, y)

cld(x::T, y::T) where {T<:AbstractFloat} = -fld(-x,y)

function mod(x::T, y::T) where T<:AbstractFloat
    r = rem(x,y)
    if r == 0
        copysign(r,y)
    elseif (r > 0) ⊻ (y > 0)
        r+y
    else
        r
    end
end

## floating point comparisons ##
==(x::Float16, y::Float16) = eq_float(x, y)
==(x::Float32, y::Float32) = eq_float(x, y)
==(x::Float64, y::Float64) = eq_float(x, y)
!=(x::Float16, y::Float16) = ne_float(x, y)
!=(x::Float32, y::Float32) = ne_float(x, y)
!=(x::Float64, y::Float64) = ne_float(x, y)
<( x::Float16, y::Float16) = lt_float(x, y)
<( x::Float32, y::Float32) = lt_float(x, y)
<( x::Float64, y::Float64) = lt_float(x, y)
<=(x::Float16, y::Float16) = le_float(x, y)
<=(x::Float32, y::Float32) = le_float(x, y)
<=(x::Float64, y::Float64) = le_float(x, y)

isequal(x::Float16, y::Float16) = fpiseq(x, y)
isequal(x::Float32, y::Float32) = fpiseq(x, y)
isequal(x::Float64, y::Float64) = fpiseq(x, y)
isless( x::Float16, y::Float16) = fpislt(x, y)
isless( x::Float32, y::Float32) = fpislt(x, y)
isless( x::Float64, y::Float64) = fpislt(x, y)

# Exact Float (Tf) vs Integer (Ti) comparisons
# Assumes:
# - typemax(Ti) == 2^n-1
# - typemax(Ti) can't be exactly represented by Tf:
#   => Tf(typemax(Ti)) == 2^n or Inf
# - typemin(Ti) can be exactly represented by Tf
#
# 1. convert y::Ti to float fy::Tf
# 2. perform Tf comparison x vs fy
# 3. if x == fy, check if (1) resulted in rounding:
#  a. convert fy back to Ti and compare with original y
#  b. unsafe_convert undefined behaviour if fy == Tf(typemax(Ti))
#     (but consequently x == fy > y)
for Ti in (Int64,UInt64,Int128,UInt128)
    for Tf in (Float16,Float32,Float64)
        @eval begin
            function ==(x::$Tf, y::$Ti)
                fy = ($Tf)(y)
                (x == fy) & (fy != $(Tf(typemax(Ti)))) & (y == unsafe_trunc($Ti,fy))
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
for op in (:(==), :<, :<=)
    @eval begin
        ($op)(x::Float16, y::Union{Int128,UInt128,Int64,UInt64}) = ($op)(Float64(x), Float64(y))
        ($op)(x::Union{Int128,UInt128,Int64,UInt64}, y::Float16) = ($op)(Float64(x), Float64(y))

        ($op)(x::Union{Float16,Float32}, y::Union{Int32,UInt32}) = ($op)(Float64(x), Float64(y))
        ($op)(x::Union{Int32,UInt32}, y::Union{Float16,Float32}) = ($op)(Float64(x), Float64(y))

        ($op)(x::Float16, y::Union{Int16,UInt16}) = ($op)(Float32(x), Float32(y))
        ($op)(x::Union{Int16,UInt16}, y::Float16) = ($op)(Float32(x), Float32(y))
    end
end


abs(x::Float16) = abs_float(x)
abs(x::Float32) = abs_float(x)
abs(x::Float64) = abs_float(x)

"""
    isnan(f) -> Bool

Test whether a number value is a NaN, an indeterminate value which is neither an infinity
nor a finite number ("not a number").
"""
isnan(x::AbstractFloat) = (x != x)::Bool
isnan(x::Number) = false

isfinite(x::AbstractFloat) = x - x == 0
isfinite(x::Real) = decompose(x)[3] != 0
isfinite(x::Integer) = true

"""
    isinf(f) -> Bool

Test whether a number is infinite.
"""
isinf(x::Real) = !isnan(x) & !isfinite(x)

const hx_NaN = hash_uint64(reinterpret(UInt64, NaN))
let Tf = Float64, Tu = UInt64, Ti = Int64
    @eval function hash(x::$Tf, h::UInt)
        # see comments on trunc and hash(Real, UInt)
        if $(Tf(typemin(Ti))) <= x < $(Tf(typemax(Ti)))
            xi = fptosi($Ti, x)
            if isequal(xi, x)
                return hash(xi, h)
            end
        elseif $(Tf(typemin(Tu))) <= x < $(Tf(typemax(Tu)))
            xu = fptoui($Tu, x)
            if isequal(xu, x)
                return hash(xu, h)
            end
        elseif isnan(x)
            return hx_NaN ⊻ h # NaN does not have a stable bit pattern
        end
        return hash_uint64(bitcast(UInt64, x)) - 3h
    end
end

hash(x::Float32, h::UInt) = hash(Float64(x), h)
hash(x::Float16, h::UInt) = hash(Float64(x), h)

## generic hashing for rational values ##

function hash(x::Real, h::UInt)
    # decompose x as num*2^pow/den
    num, pow, den = decompose(x)

    # handle special values
    num == 0 && den == 0 && return hash(NaN, h)
    num == 0 && return hash(ifelse(den > 0, 0.0, -0.0), h)
    den == 0 && return hash(ifelse(num > 0, Inf, -Inf), h)

    # normalize decomposition
    if den < 0
        num = -num
        den = -den
    end
    z = trailing_zeros(num)
    if z != 0
        num >>= z
        pow += z
    end
    z = trailing_zeros(den)
    if z != 0
        den >>= z
        pow -= z
    end

    # handle values representable as Int64, UInt64, Float64
    if den == 1
        left = ndigits0z(num,2) + pow
        right = trailing_zeros(num) + pow
        if -1074 <= right
            if 0 <= right && left <= 64
                left <= 63                     && return hash(Int64(num) << Int(pow), h)
                signbit(num) == signbit(den)   && return hash(UInt64(num) << Int(pow), h)
            end # typemin(Int64) handled by Float64 case
            left <= 1024 && left - right <= 53 && return hash(ldexp(Float64(num),pow), h)
        end
    end

    # handle generic rational values
    h = hash_integer(den, h)
    h = hash_integer(pow, h)
    h = hash_integer(num, h)
    return h
end

#=
`decompose(x)`: non-canonical decomposition of rational values as `num*2^pow/den`.

The decompose function is the point where rational-valued numeric types that support
hashing hook into the hashing protocol. `decompose(x)` should return three integer
values `num, pow, den`, such that the value of `x` is mathematically equal to

    num*2^pow/den

The decomposition need not be canonical in the sense that it just needs to be *some*
way to express `x` in this form, not any particular way – with the restriction that
`num` and `den` may not share any odd common factors. They may, however, have powers
of two in common – the generic hashing code will normalize those as necessary.

Special values:

 - `x` is zero: `num` should be zero and `den` should have the same sign as `x`
 - `x` is infinite: `den` should be zero and `num` should have the same sign as `x`
 - `x` is not a number: `num` and `den` should both be zero
=#

decompose(x::Integer) = x, 0, 1

function decompose(x::Float16)::NTuple{3,Int}
    isnan(x) && return 0, 0, 0
    isinf(x) && return ifelse(x < 0, -1, 1), 0, 0
    n = reinterpret(UInt16, x)
    s = (n & 0x03ff) % Int16
    e = ((n & 0x7c00) >> 10) % Int
    s |= Int16(e != 0) << 10
    d = ifelse(signbit(x), -1, 1)
    s, e - 25 + (e == 0), d
end

function decompose(x::Float32)::NTuple{3,Int}
    isnan(x) && return 0, 0, 0
    isinf(x) && return ifelse(x < 0, -1, 1), 0, 0
    n = reinterpret(UInt32, x)
    s = (n & 0x007fffff) % Int32
    e = ((n & 0x7f800000) >> 23) % Int
    s |= Int32(e != 0) << 23
    d = ifelse(signbit(x), -1, 1)
    s, e - 150 + (e == 0), d
end

function decompose(x::Float64)::Tuple{Int64, Int, Int}
    isnan(x) && return 0, 0, 0
    isinf(x) && return ifelse(x < 0, -1, 1), 0, 0
    n = reinterpret(UInt64, x)
    s = (n & 0x000fffffffffffff) % Int64
    e = ((n & 0x7ff0000000000000) >> 52) % Int
    s |= Int64(e != 0) << 52
    d = ifelse(signbit(x), -1, 1)
    s, e - 1075 + (e == 0), d
end


"""
    precision(num::AbstractFloat)

Get the precision of a floating point number, as defined by the effective number of bits in
the significand.
"""
function precision end

precision(::Type{Float16}) = 11
precision(::Type{Float32}) = 24
precision(::Type{Float64}) = 53
precision(::T) where {T<:AbstractFloat} = precision(T)

"""
    uabs(x::Integer)

Return the absolute value of `x`, possibly returning a different type should the
operation be susceptible to overflow. This typically arises when `x` is a two's complement
signed integer, so that `abs(typemin(x)) == typemin(x) < 0`, in which case the result of
`uabs(x)` will be an unsigned integer of the same size.
"""
uabs(x::Integer) = abs(x)
uabs(x::BitSigned) = unsigned(abs(x))


"""
    nextfloat(x::AbstractFloat, n::Integer)

The result of `n` iterative applications of `nextfloat` to `x` if `n >= 0`, or `-n`
applications of `prevfloat` if `n < 0`.
"""
function nextfloat(f::IEEEFloat, d::Integer)
    F = typeof(f)
    fumax = reinterpret(Unsigned, F(Inf))
    U = typeof(fumax)

    isnan(f) && return f
    fi = reinterpret(Signed, f)
    fneg = fi < 0
    fu = unsigned(fi & typemax(fi))

    dneg = d < 0
    da = uabs(d)
    if da > typemax(U)
        fneg = dneg
        fu = fumax
    else
        du = da % U
        if fneg ⊻ dneg
            if du > fu
                fu = min(fumax, du - fu)
                fneg = !fneg
            else
                fu = fu - du
            end
        else
            if fumax - fu < du
                fu = fumax
            else
                fu = fu + du
            end
        end
    end
    if fneg
        fu |= sign_mask(F)
    end
    reinterpret(F, fu)
end

"""
    nextfloat(x::AbstractFloat)

Return the smallest floating point number `y` of the same type as `x` such `x < y`. If no
such `y` exists (e.g. if `x` is `Inf` or `NaN`), then return `x`.
"""
nextfloat(x::AbstractFloat) = nextfloat(x,1)

"""
    prevfloat(x::AbstractFloat, n::Integer)

The result of `n` iterative applications of `prevfloat` to `x` if `n >= 0`, or `-n`
applications of `nextfloat` if `n < 0`.
"""
prevfloat(x::AbstractFloat, d::Integer) = nextfloat(x, -d)

"""
    prevfloat(x::AbstractFloat)

Return the largest floating point number `y` of the same type as `x` such `y < x`. If no
such `y` exists (e.g. if `x` is `-Inf` or `NaN`), then return `x`.
"""
prevfloat(x::AbstractFloat) = nextfloat(x,-1)

for Ti in (Int8, Int16, Int32, Int64, Int128, UInt8, UInt16, UInt32, UInt64, UInt128)
    for Tf in (Float16, Float32, Float64)
        if Ti <: Unsigned || sizeof(Ti) < sizeof(Tf)
            # Here `Tf(typemin(Ti))-1` is exact, so we can compare the lower-bound
            # directly. `Tf(typemax(Ti))+1` is either always exactly representable, or
            # rounded to `Inf` (e.g. when `Ti==UInt128 && Tf==Float32`).
            @eval begin
                function trunc(::Type{$Ti},x::$Tf)
                    if $(Tf(typemin(Ti))-one(Tf)) < x < $(Tf(typemax(Ti))+one(Tf))
                        return unsafe_trunc($Ti,x)
                    else
                        throw(InexactError(:trunc, $Ti, x))
                    end
                end
                function (::Type{$Ti})(x::$Tf)
                    if ($(Tf(typemin(Ti))) <= x <= $(Tf(typemax(Ti)))) && (round(x, RoundToZero) == x)
                        return unsafe_trunc($Ti,x)
                    else
                        throw(InexactError($(Expr(:quote,Ti.name.name)), $Ti, x))
                    end
                end
            end
        else
            # Here `eps(Tf(typemin(Ti))) > 1`, so the only value which can be truncated to
            # `Tf(typemin(Ti)` is itself. Similarly, `Tf(typemax(Ti))` is inexact and will
            # be rounded up. This assumes that `Tf(typemin(Ti)) > -Inf`, which is true for
            # these types, but not for `Float16` or larger integer types.
            @eval begin
                function trunc(::Type{$Ti},x::$Tf)
                    if $(Tf(typemin(Ti))) <= x < $(Tf(typemax(Ti)))
                        return unsafe_trunc($Ti,x)
                    else
                        throw(InexactError(:trunc, $Ti, x))
                    end
                end
                function (::Type{$Ti})(x::$Tf)
                    if ($(Tf(typemin(Ti))) <= x < $(Tf(typemax(Ti)))) && (round(x, RoundToZero) == x)
                        return unsafe_trunc($Ti,x)
                    else
                        throw(InexactError($(Expr(:quote,Ti.name.name)), $Ti, x))
                    end
                end
            end
        end
    end
end

"""
    issubnormal(f) -> Bool

Test whether a floating point number is subnormal.
"""
function issubnormal(x::T) where {T<:IEEEFloat}
    y = reinterpret(Unsigned, x)
    (y & exponent_mask(T) == 0) & (y & significand_mask(T) != 0)
end

ispow2(x::AbstractFloat) = !iszero(x) && frexp(x)[1] == 0.5
iseven(x::AbstractFloat) = isinteger(x) && (abs(x) > maxintfloat(x) || iseven(Integer(x)))
isodd(x::AbstractFloat) = isinteger(x) && abs(x) ≤ maxintfloat(x) && isodd(Integer(x))

@eval begin
    typemin(::Type{Float16}) = $(bitcast(Float16, 0xfc00))
    typemax(::Type{Float16}) = $(Inf16)
    typemin(::Type{Float32}) = $(-Inf32)
    typemax(::Type{Float32}) = $(Inf32)
    typemin(::Type{Float64}) = $(-Inf64)
    typemax(::Type{Float64}) = $(Inf64)
    typemin(x::T) where {T<:Real} = typemin(T)
    typemax(x::T) where {T<:Real} = typemax(T)

    floatmin(::Type{Float16}) = $(bitcast(Float16, 0x0400))
    floatmin(::Type{Float32}) = $(bitcast(Float32, 0x00800000))
    floatmin(::Type{Float64}) = $(bitcast(Float64, 0x0010000000000000))
    floatmax(::Type{Float16}) = $(bitcast(Float16, 0x7bff))
    floatmax(::Type{Float32}) = $(bitcast(Float32, 0x7f7fffff))
    floatmax(::Type{Float64}) = $(bitcast(Float64, 0x7fefffffffffffff))

    eps(x::AbstractFloat) = isfinite(x) ? abs(x) >= floatmin(x) ? ldexp(eps(typeof(x)), exponent(x)) : nextfloat(zero(x)) : oftype(x, NaN)
    eps(::Type{Float16}) = $(bitcast(Float16, 0x1400))
    eps(::Type{Float32}) = $(bitcast(Float32, 0x34000000))
    eps(::Type{Float64}) = $(bitcast(Float64, 0x3cb0000000000000))
    eps() = eps(Float64)
end

"""
    floatmin(T = Float64)

Return the smallest positive normal number representable by the floating-point
type `T`.

# Examples
```jldoctest
julia> floatmin(Float16)
Float16(6.104e-5)

julia> floatmin(Float32)
1.1754944f-38

julia> floatmin()
2.2250738585072014e-308
```
"""
floatmin(x::T) where {T<:AbstractFloat} = floatmin(T)

"""
    floatmax(T = Float64)

Return the largest finite number representable by the floating-point type `T`.

# Examples
```jldoctest
julia> floatmax(Float16)
Float16(6.55e4)

julia> floatmax(Float32)
3.4028235f38

julia> floatmax()
1.7976931348623157e308
```
"""
floatmax(x::T) where {T<:AbstractFloat} = floatmax(T)

floatmin() = floatmin(Float64)
floatmax() = floatmax(Float64)

"""
    eps(::Type{T}) where T<:AbstractFloat
    eps()

Return the *machine epsilon* of the floating point type `T` (`T = Float64` by
default). This is defined as the gap between 1 and the next largest value representable by
`typeof(one(T))`, and is equivalent to `eps(one(T))`.  (Since `eps(T)` is a
bound on the *relative error* of `T`, it is a "dimensionless" quantity like [`one`](@ref).)

# Examples
```jldoctest
julia> eps()
2.220446049250313e-16

julia> eps(Float32)
1.1920929f-7

julia> 1.0 + eps()
1.0000000000000002

julia> 1.0 + eps()/2
1.0
```
"""
eps(::Type{<:AbstractFloat})

"""
    eps(x::AbstractFloat)

Return the *unit in last place* (ulp) of `x`. This is the distance between consecutive
representable floating point values at `x`. In most cases, if the distance on either side
of `x` is different, then the larger of the two is taken, that is

    eps(x) == max(x-prevfloat(x), nextfloat(x)-x)

The exceptions to this rule are the smallest and largest finite values
(e.g. `nextfloat(-Inf)` and `prevfloat(Inf)` for [`Float64`](@ref)), which round to the
smaller of the values.

The rationale for this behavior is that `eps` bounds the floating point rounding
error. Under the default `RoundNearest` rounding mode, if ``y`` is a real number and ``x``
is the nearest floating point number to ``y``, then

```math
|y-x| \\leq \\operatorname{eps}(x)/2.
```

# Examples
```jldoctest
julia> eps(1.0)
2.220446049250313e-16

julia> eps(prevfloat(2.0))
2.220446049250313e-16

julia> eps(2.0)
4.440892098500626e-16

julia> x = prevfloat(Inf)      # largest finite Float64
1.7976931348623157e308

julia> x + eps(x)/2            # rounds up
Inf

julia> x + prevfloat(eps(x)/2) # rounds down
1.7976931348623157e308
```
"""
eps(::AbstractFloat)


## byte order swaps for arbitrary-endianness serialization/deserialization ##
bswap(x::IEEEFloat) = bswap_int(x)

# integer size of float
uinttype(::Type{Float64}) = UInt64
uinttype(::Type{Float32}) = UInt32
uinttype(::Type{Float16}) = UInt16

## Array operations on floating point numbers ##

float(A::AbstractArray{<:AbstractFloat}) = A

function float(A::AbstractArray{T}) where T
    if !isconcretetype(T)
        error("`float` not defined on abstractly-typed arrays; please convert to a more specific type")
    end
    convert(AbstractArray{typeof(float(zero(T)))}, A)
end

float(r::StepRange) = float(r.start):float(r.step):float(last(r))
float(r::UnitRange) = float(r.start):float(last(r))
float(r::StepRangeLen{T}) where {T} =
    StepRangeLen{typeof(float(T(r.ref)))}(float(r.ref), float(r.step), length(r), r.offset)
function float(r::LinRange)
    LinRange(float(r.start), float(r.stop), length(r))
end
