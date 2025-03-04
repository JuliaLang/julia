# This file is a part of Julia. License is MIT: https://julialang.org/license

const IEEEFloat = Union{Float16, Float32, Float64}

import Core: Float16, Float32, Float64, AbstractFloat
import Core: Int8, Int16, Int32, Int64, Int128, UInt8, UInt16, UInt32, UInt64, UInt128

## floating point traits ##

"""
    Inf16

Positive infinity of type [`Float16`](@ref).
"""
const Inf16 = bitcast(Float16, 0x7c00)
"""
    NaN16

A not-a-number value of type [`Float16`](@ref).

See also: [`NaN`](@ref).
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

See also: [`NaN`](@ref).
"""
const NaN32 = bitcast(Float32, 0x7fc00000)
const Inf64 = bitcast(Float64, 0x7ff0000000000000)
const NaN64 = bitcast(Float64, 0x7ff8000000000000)

const Inf = Inf64
"""
    Inf, Inf64

Positive infinity of type [`Float64`](@ref).

See also: [`isfinite`](@ref), [`typemax`](@ref), [`NaN`](@ref), [`Inf32`](@ref).

# Examples
```jldoctest
julia> π/0
Inf

julia> +1.0 / -0.0
-Inf

julia> ℯ^-Inf
0.0
```
"""
Inf, Inf64

const NaN = NaN64
"""
    NaN, NaN64

A not-a-number value of type [`Float64`](@ref).

See also: [`isnan`](@ref), [`missing`](@ref), [`NaN32`](@ref), [`Inf`](@ref).

# Examples
```jldoctest
julia> 0/0
NaN

julia> Inf - Inf
NaN

julia> NaN == NaN, isequal(NaN, NaN), isnan(NaN)
(false, true, true)
```

!!! note
    Always use [`isnan`](@ref) or [`isequal`](@ref) for checking for `NaN`.
    Using `x === NaN` may give unexpected results:
    ```julia-repl
    julia> reinterpret(UInt32, NaN32)
    0x7fc00000

    julia> NaN32p1 = reinterpret(Float32, 0x7fc00001)
    NaN32

    julia> NaN32p1 === NaN32, isequal(NaN32p1, NaN32), isnan(NaN32p1)
    (false, true, true)
    ```
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

mantissa(x::T) where {T} = reinterpret(Unsigned, x) & significand_mask(T)

for T in (Float16, Float32, Float64)
    sb = trailing_ones(significand_mask(T))
    em = exponent_mask(T)
    eb = Int(exponent_one(T) >> sb)
    @eval significand_bits(::Type{$T}) = $(sb)
    @eval exponent_bits(::Type{$T}) = $(sizeof(T)*8 - sb - 1)
    @eval exponent_bias(::Type{$T}) = $(eb)
    # maximum float exponent
    @eval exponent_max(::Type{$T}) = $(Int(em >> sb) - eb - 1)
    # maximum float exponent without bias
    @eval exponent_raw_max(::Type{$T}) = $(Int(em >> sb))
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

"""
IEEE 754 definition of the minimum exponent.
"""
ieee754_exponent_min(::Type{T}) where {T<:IEEEFloat} = Int(1 - exponent_max(T))::Int

exponent_min(::Type{Float16}) = ieee754_exponent_min(Float16)
exponent_min(::Type{Float32}) = ieee754_exponent_min(Float32)
exponent_min(::Type{Float64}) = ieee754_exponent_min(Float64)

function ieee754_representation(
    ::Type{F}, sign_bit::Bool, exponent_field::Integer, significand_field::Integer
) where {F<:IEEEFloat}
    T = uinttype(F)
    ret::T = sign_bit
    ret <<= exponent_bits(F)
    ret |= exponent_field
    ret <<= significand_bits(F)
    ret |= significand_field
end

# ±floatmax(T)
function ieee754_representation(
    ::Type{F}, sign_bit::Bool, ::Val{:omega}
) where {F<:IEEEFloat}
    ieee754_representation(F, sign_bit, exponent_raw_max(F) - 1, significand_mask(F))
end

# NaN or an infinity
function ieee754_representation(
    ::Type{F}, sign_bit::Bool, significand_field::Integer, ::Val{:nan}
) where {F<:IEEEFloat}
    ieee754_representation(F, sign_bit, exponent_raw_max(F), significand_field)
end

# NaN with default payload
function ieee754_representation(
    ::Type{F}, sign_bit::Bool, ::Val{:nan}
) where {F<:IEEEFloat}
    ieee754_representation(F, sign_bit, one(uinttype(F)) << (significand_bits(F) - 1), Val(:nan))
end

# Infinity
function ieee754_representation(
    ::Type{F}, sign_bit::Bool, ::Val{:inf}
) where {F<:IEEEFloat}
    ieee754_representation(F, sign_bit, false, Val(:nan))
end

# Subnormal or zero
function ieee754_representation(
    ::Type{F}, sign_bit::Bool, significand_field::Integer, ::Val{:subnormal}
) where {F<:IEEEFloat}
    ieee754_representation(F, sign_bit, false, significand_field)
end

# Zero
function ieee754_representation(
    ::Type{F}, sign_bit::Bool, ::Val{:zero}
) where {F<:IEEEFloat}
    ieee754_representation(F, sign_bit, false, Val(:subnormal))
end

"""
    uabs(x::Integer)

Return the absolute value of `x`, possibly returning a different type should the
operation be susceptible to overflow. This typically arises when `x` is a two's complement
signed integer, so that `abs(typemin(x)) == typemin(x) < 0`, in which case the result of
`uabs(x)` will be an unsigned integer of the same size.
"""
uabs(x::Integer) = abs(x)
uabs(x::BitSigned) = unsigned(abs(x))

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

promote_rule(::Type{Float64}, ::Type{UInt128}) = Float64
promote_rule(::Type{Float64}, ::Type{Int128}) = Float64
promote_rule(::Type{Float32}, ::Type{UInt128}) = Float32
promote_rule(::Type{Float32}, ::Type{Int128}) = Float32
promote_rule(::Type{Float16}, ::Type{UInt128}) = Float16
promote_rule(::Type{Float16}, ::Type{Int128}) = Float16

function Float64(x::UInt128)
    if x < UInt128(1) << 104 # Can fit it in two 52 bits mantissas
        low_exp = 0x1p52
        high_exp = 0x1p104
        low_bits = (x % UInt64) & Base.significand_mask(Float64)
        low_value = reinterpret(Float64, reinterpret(UInt64, low_exp) | low_bits) - low_exp
        high_bits = ((x >> 52) % UInt64)
        high_value = reinterpret(Float64, reinterpret(UInt64, high_exp) | high_bits) - high_exp
        low_value + high_value
    else # Large enough that low bits only affect rounding, pack low bits
        low_exp = 0x1p76
        high_exp = 0x1p128
        low_bits = ((x >> 12) % UInt64) >> 12 | (x % UInt64) & 0xFFFFFF
        low_value = reinterpret(Float64, reinterpret(UInt64, low_exp) | low_bits) - low_exp
        high_bits = ((x >> 76) % UInt64)
        high_value = reinterpret(Float64, reinterpret(UInt64, high_exp) | high_bits) - high_exp
        low_value + high_value
    end
end

function Float64(x::Int128)
    sign_bit = ((x >> 127) % UInt64) << 63
    ux = uabs(x)
    if ux < UInt128(1) << 104 # Can fit it in two 52 bits mantissas
        low_exp = 0x1p52
        high_exp = 0x1p104
        low_bits = (ux % UInt64) & Base.significand_mask(Float64)
        low_value = reinterpret(Float64, reinterpret(UInt64, low_exp) | low_bits) - low_exp
        high_bits = ((ux >> 52) % UInt64)
        high_value = reinterpret(Float64, reinterpret(UInt64, high_exp) | high_bits) - high_exp
        reinterpret(Float64, sign_bit | reinterpret(UInt64, low_value + high_value))
    else # Large enough that low bits only affect rounding, pack low bits
        low_exp = 0x1p76
        high_exp = 0x1p128
        low_bits = ((ux >> 12) % UInt64) >> 12 | (ux % UInt64) & 0xFFFFFF
        low_value = reinterpret(Float64, reinterpret(UInt64, low_exp) | low_bits) - low_exp
        high_bits = ((ux >> 76) % UInt64)
        high_value = reinterpret(Float64, reinterpret(UInt64, high_exp) | high_bits) - high_exp
        reinterpret(Float64, sign_bit | reinterpret(UInt64, low_value + high_value))
    end
end

function Float32(x::UInt128)
    x == 0 && return 0f0
    n = top_set_bit(x) # ndigits0z(x,2)
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
    n = top_set_bit(x) # ndigits0z(x,2)
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
Float16(x::UInt128) = convert(Float16, Float64(x))
Float16(x::Int128)  = convert(Float16, Float64(x))

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

See also: [`complex`](@ref), [`oftype`](@ref), [`convert`](@ref).

# Examples
```jldoctest
julia> float(typemax(Int32))
2.147483647e9
```
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
float(::Type{Union{}}, slurp...) = Union{}(0.0)

"""
    unsafe_trunc(T, x)

Return the nearest integral value of type `T` whose absolute value is
less than or equal to the absolute value of `x`. If the value is not representable by `T`,
an arbitrary value will be returned.
See also [`trunc`](@ref).

# Examples
```jldoctest
julia> unsafe_trunc(Int, -2.2)
-2

julia> unsafe_trunc(Int, NaN)
-9223372036854775808
```
"""
function unsafe_trunc end

for Ti in (Int8, Int16, Int32, Int64)
    @eval begin
        unsafe_trunc(::Type{$Ti}, x::IEEEFloat) = fptosi($Ti, x)
    end
end
for Ti in (UInt8, UInt16, UInt32, UInt64)
    @eval begin
        unsafe_trunc(::Type{$Ti}, x::IEEEFloat) = fptoui($Ti, x)
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
# also determines trunc, floor, ceil
round(::Type{Signed},   x::IEEEFloat, r::RoundingMode) = round(Int, x, r)
round(::Type{Unsigned}, x::IEEEFloat, r::RoundingMode) = round(UInt, x, r)
round(::Type{Integer},  x::IEEEFloat, r::RoundingMode) = round(Int, x, r)

round(x::IEEEFloat, ::RoundingMode{:ToZero})  = trunc_llvm(x)
round(x::IEEEFloat, ::RoundingMode{:Down})    = floor_llvm(x)
round(x::IEEEFloat, ::RoundingMode{:Up})      = ceil_llvm(x)
round(x::IEEEFloat, ::RoundingMode{:Nearest}) = rint_llvm(x)

rounds_up(x, ::RoundingMode{:Down}) = false
rounds_up(x, ::RoundingMode{:Up}) = true
rounds_up(x, ::RoundingMode{:ToZero}) = signbit(x)
rounds_up(x, ::RoundingMode{:FromZero}) = !signbit(x)
function _round_convert(::Type{T}, x_integer, x, r::Union{RoundingMode{:ToZero}, RoundingMode{:FromZero}, RoundingMode{:Up}, RoundingMode{:Down}}) where {T<:AbstractFloat}
    x_t = convert(T, x_integer)
    if rounds_up(x, r)
        x_t < x ? nextfloat(x_t) : x_t
    else
        x_t > x ? prevfloat(x_t) : x_t
    end
end

## floating point promotions ##
promote_rule(::Type{Float32}, ::Type{Float16}) = Float32
promote_rule(::Type{Float64}, ::Type{Float16}) = Float64
promote_rule(::Type{Float64}, ::Type{Float32}) = Float64

widen(::Type{Float16}) = Float32
widen(::Type{Float32}) = Float64

## floating point arithmetic ##
-(x::IEEEFloat) = neg_float(x)

+(x::T, y::T) where {T<:IEEEFloat} = add_float(x, y)
-(x::T, y::T) where {T<:IEEEFloat} = sub_float(x, y)
*(x::T, y::T) where {T<:IEEEFloat} = mul_float(x, y)
/(x::T, y::T) where {T<:IEEEFloat} = div_float(x, y)

muladd(x::T, y::T, z::T) where {T<:IEEEFloat} = muladd_float(x, y, z)

# TODO: faster floating point div?
# TODO: faster floating point fld?
# TODO: faster floating point mod?

function unbiased_exponent(x::T) where {T<:IEEEFloat}
    return (reinterpret(Unsigned, x) & exponent_mask(T)) >> significand_bits(T)
end

function explicit_mantissa_noinfnan(x::T) where {T<:IEEEFloat}
    m = mantissa(x)
    issubnormal(x) || (m |= significand_mask(T) + uinttype(T)(1))
    return m
end

function _to_float(number::U, ep) where {U<:Unsigned}
    F = floattype(U)
    S = signed(U)
    epint = unsafe_trunc(S,ep)
    lz::signed(U) = unsafe_trunc(S, Core.Intrinsics.ctlz_int(number) - U(exponent_bits(F)))
    number <<= lz
    epint -= lz
    bits = U(0)
    if epint >= 0
        bits = number & significand_mask(F)
        bits |= ((epint + S(1)) << significand_bits(F)) & exponent_mask(F)
    else
        bits = (number >> -epint) & significand_mask(F)
    end
    return reinterpret(F, bits)
end

@assume_effects :terminates_locally :nothrow function rem_internal(x::T, y::T) where {T<:IEEEFloat}
    xuint = reinterpret(Unsigned, x)
    yuint = reinterpret(Unsigned, y)
    if xuint <= yuint
        if xuint < yuint
            return x
        end
        return zero(T)
    end

    e_x = unbiased_exponent(x)
    e_y = unbiased_exponent(y)
    # Most common case where |y| is "very normal" and |x/y| < 2^EXPONENT_WIDTH
    if e_y > (significand_bits(T)) && (e_x - e_y) <= (exponent_bits(T))
        m_x = explicit_mantissa_noinfnan(x)
        m_y = explicit_mantissa_noinfnan(y)
        d = urem_int((m_x << (e_x - e_y)),  m_y)
        iszero(d) && return zero(T)
        return _to_float(d, e_y - uinttype(T)(1))
    end
    # Both are subnormals
    if e_x == 0 && e_y == 0
        return reinterpret(T, urem_int(xuint, yuint) & significand_mask(T))
    end

    m_x = explicit_mantissa_noinfnan(x)
    e_x -= uinttype(T)(1)
    m_y = explicit_mantissa_noinfnan(y)
    lz_m_y = uinttype(T)(exponent_bits(T))
    if e_y > 0
        e_y -= uinttype(T)(1)
    else
        m_y = mantissa(y)
        lz_m_y = Core.Intrinsics.ctlz_int(m_y)
    end

    tz_m_y = Core.Intrinsics.cttz_int(m_y)
    sides_zeroes_cnt = lz_m_y + tz_m_y

    # n>0
    exp_diff = e_x - e_y
    # Shift hy right until the end or n = 0
    right_shift = min(exp_diff, tz_m_y)
    m_y >>= right_shift
    exp_diff -= right_shift
    e_y += right_shift
    # Shift hx left until the end or n = 0
    left_shift = min(exp_diff, uinttype(T)(exponent_bits(T)))
    m_x <<= left_shift
    exp_diff -= left_shift

    m_x = urem_int(m_x, m_y)
    iszero(m_x) && return zero(T)
    iszero(exp_diff) && return _to_float(m_x, e_y)

    while exp_diff > sides_zeroes_cnt
        exp_diff -= sides_zeroes_cnt
        m_x <<= sides_zeroes_cnt
        m_x = urem_int(m_x, m_y)
    end
    m_x <<= exp_diff
    m_x = urem_int(m_x, m_y)
    return _to_float(m_x, e_y)
end

function rem(x::T, y::T) where {T<:IEEEFloat}
    if isfinite(x) && !iszero(x) && isfinite(y) && !iszero(y)
        return copysign(rem_internal(abs(x), abs(y)), x)
    elseif isinf(x) || isnan(y) || iszero(y)  # y can still be Inf
        return T(NaN)
    else
        return x
    end
end

function mod(x::T, y::T) where {T<:AbstractFloat}
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
==(x::T, y::T) where {T<:IEEEFloat} = eq_float(x, y)
!=(x::T, y::T) where {T<:IEEEFloat} = ne_float(x, y)
<( x::T, y::T) where {T<:IEEEFloat} = lt_float(x, y)
<=(x::T, y::T) where {T<:IEEEFloat} = le_float(x, y)

isequal(x::T, y::T) where {T<:IEEEFloat} = fpiseq(x, y)

# interpret as sign-magnitude integer
@inline function _fpint(x)
    IntT = inttype(typeof(x))
    ix = reinterpret(IntT, x)
    return ifelse(ix < zero(IntT), ix ⊻ typemax(IntT), ix)
end

@inline function isless(a::T, b::T) where T<:IEEEFloat
    (isnan(a) || isnan(b)) && return !isnan(a)

    return _fpint(a) < _fpint(b)
end

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
    for Tf in (Float32,Float64)
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


abs(x::IEEEFloat) = abs_float(x)

"""
    isnan(f) -> Bool

Test whether a number value is a NaN, an indeterminate value which is neither an infinity
nor a finite number ("not a number").

See also: [`iszero`](@ref), [`isone`](@ref), [`isinf`](@ref), [`ismissing`](@ref).
"""
isnan(x::AbstractFloat) = (x != x)::Bool
isnan(x::Number) = false

isfinite(x::AbstractFloat) = !isnan(x - x)
isfinite(x::Real) = decompose(x)[3] != 0
isfinite(x::Integer) = true

"""
    isinf(f) -> Bool

Test whether a number is infinite.

See also: [`Inf`](@ref), [`iszero`](@ref), [`isfinite`](@ref), [`isnan`](@ref).
"""
isinf(x::Real) = !isnan(x) & !isfinite(x)
isinf(x::IEEEFloat) = abs(x) === oftype(x, Inf)

const hx_NaN = hash_uint64(reinterpret(UInt64, NaN))
function hash(x::Float64, h::UInt)
    # see comments on trunc and hash(Real, UInt)
    if typemin(Int64) <= x < typemax(Int64)
        xi = fptosi(Int64, x)
        if isequal(xi, x)
            return hash(xi, h)
        end
    elseif typemin(UInt64) <= x < typemax(UInt64)
        xu = fptoui(UInt64, x)
        if isequal(xu, x)
            return hash(xu, h)
        end
    elseif isnan(x)
        return hx_NaN ⊻ h # NaN does not have a stable bit pattern
    end
    return hash_uint64(bitcast(UInt64, x)) - 3h
end

hash(x::Float32, h::UInt) = hash(Float64(x), h)

function hash(x::Float16, h::UInt)
    # see comments on trunc and hash(Real, UInt)
    if isfinite(x) # all finite Float16 fit in Int64
        xi = fptosi(Int64, x)
        if isequal(xi, x)
            return hash(xi, h)
        end
    elseif isnan(x)
        return hx_NaN ⊻ h # NaN does not have a stable bit pattern
    end
    return hash_uint64(bitcast(UInt64, Float64(x))) - 3h
end

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
    num_z = trailing_zeros(num)
    num >>= num_z
    den_z = trailing_zeros(den)
    den >>= den_z
    pow += num_z - den_z
    # If the real can be represented as an Int64, UInt64, or Float64, hash as those types.
    # To be an Integer the denominator must be 1 and the power must be non-negative.
    if den == 1
        # left = ceil(log2(num*2^pow))
        left = top_set_bit(abs(num)) + pow
        # 2^-1074 is the minimum Float64 so if the power is smaller, not a Float64
        if -1074 <= pow
            if 0 <= pow # if pow is non-negative, it is an integer
                left <= 63 && return hash(Int64(num) << Int(pow), h)
                left <= 64 && !signbit(num) && return hash(UInt64(num) << Int(pow), h)
            end # typemin(Int64) handled by Float64 case
            # 2^1024 is the maximum Float64 so if the power is greater, not a Float64
            # Float64s only have 53 mantisa bits (including implicit bit)
            left <= 1024 && left - pow <= 53 && return hash(ldexp(Float64(num), pow), h)
        end
    else
        h = hash_integer(den, h)
    end
    # handle generic rational values
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
    precision(num::AbstractFloat; base::Integer=2)
    precision(T::Type; base::Integer=2)

Get the precision of a floating point number, as defined by the effective number of bits in
the significand, or the precision of a floating-point type `T` (its current default, if
`T` is a variable-precision type like [`BigFloat`](@ref)).

If `base` is specified, then it returns the maximum corresponding
number of significand digits in that base.

!!! compat "Julia 1.8"
    The `base` keyword requires at least Julia 1.8.
"""
function precision end

_precision_with_base_2(::Type{Float16}) = 11
_precision_with_base_2(::Type{Float32}) = 24
_precision_with_base_2(::Type{Float64}) = 53
function _precision(x, base::Integer)
    base > 1 || throw(DomainError(base, "`base` cannot be less than 2."))
    p = _precision_with_base_2(x)
    return base == 2 ? Int(p) : floor(Int, p / log2(base))
end
precision(::Type{T}; base::Integer=2) where {T<:AbstractFloat} = _precision(T, base)
precision(::T; base::Integer=2) where {T<:AbstractFloat} = precision(T; base)


"""
    nextfloat(x::AbstractFloat, n::Integer)

The result of `n` iterative applications of `nextfloat` to `x` if `n >= 0`, or `-n`
applications of [`prevfloat`](@ref) if `n < 0`.
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

Return the smallest floating point number `y` of the same type as `x` such that `x < y`.
If no such `y` exists (e.g. if `x` is `Inf` or `NaN`), then return `x`.

See also: [`prevfloat`](@ref), [`eps`](@ref), [`issubnormal`](@ref).
"""
nextfloat(x::AbstractFloat) = nextfloat(x,1)

"""
    prevfloat(x::AbstractFloat, n::Integer)

The result of `n` iterative applications of `prevfloat` to `x` if `n >= 0`, or `-n`
applications of [`nextfloat`](@ref) if `n < 0`.
"""
prevfloat(x::AbstractFloat, d::Integer) = nextfloat(x, -d)

"""
    prevfloat(x::AbstractFloat)

Return the largest floating point number `y` of the same type as `x` such that `y < x`.
If no such `y` exists (e.g. if `x` is `-Inf` or `NaN`), then return `x`.
"""
prevfloat(x::AbstractFloat) = nextfloat(x,-1)

for Ti in (Int8, Int16, Int32, Int64, Int128, UInt8, UInt16, UInt32, UInt64, UInt128)
    for Tf in (Float16, Float32, Float64)
        if Ti <: Unsigned || sizeof(Ti) < sizeof(Tf)
            # Here `Tf(typemin(Ti))-1` is exact, so we can compare the lower-bound
            # directly. `Tf(typemax(Ti))+1` is either always exactly representable, or
            # rounded to `Inf` (e.g. when `Ti==UInt128 && Tf==Float32`).
            @eval begin
                function round(::Type{$Ti},x::$Tf,::RoundingMode{:ToZero})
                    if $(Tf(typemin(Ti))-one(Tf)) < x < $(Tf(typemax(Ti))+one(Tf))
                        return unsafe_trunc($Ti,x)
                    else
                        throw(InexactError(:round, $Ti, x, RoundToZero))
                    end
                end
                function (::Type{$Ti})(x::$Tf)
                    # When typemax(Ti) is not representable by Tf but typemax(Ti) + 1 is,
                    # then < Tf(typemax(Ti) + 1) is stricter than <= Tf(typemax(Ti)). Using
                    # the former causes us to throw on UInt64(Float64(typemax(UInt64))+1)
                    if ($(Tf(typemin(Ti))) <= x < $(Tf(typemax(Ti))+one(Tf))) && isinteger(x)
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
                function round(::Type{$Ti},x::$Tf,::RoundingMode{:ToZero})
                    if $(Tf(typemin(Ti))) <= x < $(Tf(typemax(Ti)))
                        return unsafe_trunc($Ti,x)
                    else
                        throw(InexactError(:round, $Ti, x, RoundToZero))
                    end
                end
                function (::Type{$Ti})(x::$Tf)
                    if ($(Tf(typemin(Ti))) <= x < $(Tf(typemax(Ti)))) && isinteger(x)
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

An IEEE floating point number is [subnormal](https://en.wikipedia.org/wiki/Subnormal_number)
when its exponent bits are zero and its significand is not zero.

# Examples
```jldoctest
julia> floatmin(Float32)
1.1754944f-38

julia> issubnormal(1.0f-37)
false

julia> issubnormal(1.0f-38)
true
```
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

    eps(::Type{Float16}) = $(bitcast(Float16, 0x1400))
    eps(::Type{Float32}) = $(bitcast(Float32, 0x34000000))
    eps(::Type{Float64}) = $(bitcast(Float64, 0x3cb0000000000000))
    eps() = eps(Float64)
end

eps(x::AbstractFloat) = isfinite(x) ? abs(x) >= floatmin(x) ? ldexp(eps(typeof(x)), exponent(x)) : nextfloat(zero(x)) : oftype(x, NaN)

function eps(x::T) where T<:IEEEFloat
    # For isfinite(x), toggling the LSB will produce either prevfloat(x) or
    # nextfloat(x) but will never change the sign or exponent.
    # For !isfinite(x), this will map Inf to NaN and NaN to NaN or Inf.
    y = reinterpret(T, reinterpret(Unsigned, x) ⊻ true)
    # The absolute difference between these values is eps(x). This is true even
    # for Inf/NaN values.
    return abs(x - y)
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

See also: [`typemax`](@ref), [`floatmin`](@ref), [`eps`](@ref).

# Examples
```jldoctest
julia> floatmax(Float16)
Float16(6.55e4)

julia> floatmax(Float32)
3.4028235f38

julia> floatmax()
1.7976931348623157e308

julia> typemax(Float64)
Inf
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

See also: [`nextfloat`](@ref), [`issubnormal`](@ref), [`floatmax`](@ref).

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
inttype(::Type{Float64}) = Int64
inttype(::Type{Float32}) = Int32
inttype(::Type{Float16}) = Int16
# float size of integer
floattype(::Type{UInt64}) = Float64
floattype(::Type{UInt32}) = Float32
floattype(::Type{UInt16}) = Float16
floattype(::Type{Int64}) = Float64
floattype(::Type{Int32}) = Float32
floattype(::Type{Int16}) = Float16


## Array operations on floating point numbers ##
"""
    float(A::AbstractArray)

Return an array containing the floating-point analog of each entry in array `A`.

Equivalent to `float.(A)`, except that the return value may share memory with all or
part of `A` in accordance with the behavior of `convert(T, A)` given output type `T`.

# Examples
```jldoctest
julia> float(1:1000)
1.0:1.0:1000.0
```
"""
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
