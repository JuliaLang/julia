## conversions to floating-point ##

convert(::Type{Float32}, x::Bool)    = boxf32(sitofp32(unbox8(x)))
convert(::Type{Float32}, x::Char)    = boxf32(uitofp32(unbox32(x)))
convert(::Type{Float32}, x::Int8)    = boxf32(sitofp32(unbox8(x)))
convert(::Type{Float32}, x::Int16)   = boxf32(sitofp32(unbox16(x)))
convert(::Type{Float32}, x::Int32)   = boxf32(sitofp32(unbox32(x)))
convert(::Type{Float32}, x::Int64)   = boxf32(sitofp32(unbox64(x)))
convert(::Type{Float32}, x::Uint8)   = boxf32(uitofp32(unbox8(x)))
convert(::Type{Float32}, x::Uint16)  = boxf32(uitofp32(unbox16(x)))
convert(::Type{Float32}, x::Uint32)  = boxf32(uitofp32(unbox32(x)))
convert(::Type{Float32}, x::Uint64)  = boxf32(uitofp32(unbox64(x)))
convert(::Type{Float32}, x::Float64) = boxf32(fptrunc32(unbox64(x)))

convert(::Type{Float64}, x::Bool)    = boxf64(sitofp64(unbox8(x)))
convert(::Type{Float64}, x::Char)    = boxf64(uitofp64(unbox32(x)))
convert(::Type{Float64}, x::Int8)    = boxf64(sitofp64(unbox8(x)))
convert(::Type{Float64}, x::Int16)   = boxf64(sitofp64(unbox16(x)))
convert(::Type{Float64}, x::Int32)   = boxf64(sitofp64(unbox32(x)))
convert(::Type{Float64}, x::Int64)   = boxf64(sitofp64(unbox64(x)))
convert(::Type{Float64}, x::Uint8)   = boxf64(uitofp64(unbox8(x)))
convert(::Type{Float64}, x::Uint16)  = boxf64(uitofp64(unbox16(x)))
convert(::Type{Float64}, x::Uint32)  = boxf64(uitofp64(unbox32(x)))
convert(::Type{Float64}, x::Uint64)  = boxf64(uitofp64(unbox64(x)))
convert(::Type{Float64}, x::Float32) = boxf64(fpext64(unbox32(x)))

convert(::Type{Float}, x::Bool)   = convert(Float32, x)
convert(::Type{Float}, x::Char)   = convert(Float32, x)
convert(::Type{Float}, x::Int8)   = convert(Float32, x)
convert(::Type{Float}, x::Int16)  = convert(Float32, x)
convert(::Type{Float}, x::Int32)  = convert(Float64, x)
convert(::Type{Float}, x::Int64)  = convert(Float64, x) # LOSSY
convert(::Type{Float}, x::Uint8)  = convert(Float32, x)
convert(::Type{Float}, x::Uint16) = convert(Float32, x)
convert(::Type{Float}, x::Uint32) = convert(Float64, x)
convert(::Type{Float}, x::Uint64) = convert(Float64, x) # LOSSY

float32(x) = convert(Float32, x)
float64(x) = convert(Float64, x)
float(x)   = convert(Float,   x)

## conversions from floating-point ##

if WORD_SIZE == 64
    iround(x::Float32) = iround(float64(x))
    itrunc(x::Float32) = itrunc(float64(x))
else
    iround(x::Float32) = boxsi32(fpsiround32(unbox32(x)))
    itrunc(x::Float32) = boxsi32(fptosi32(unbox32(x)))
end
iround(x::Float64) = boxsi64(fpsiround64(unbox64(x)))
itrunc(x::Float64) = boxsi64(fptosi64(unbox64(x)))

# this is needed very early because it is used by Range and colon
floor(x::Float64) = ccall(dlsym(_jl_libfdm,:floor), Float64, (Float64,), x)

iceil(x::Float)  = itrunc(ceil(x))  # TODO: fast primitive for iceil
ifloor(x::Float) = itrunc(floor(x)) # TOOD: fast primitive for ifloor

convert(::Type{Integer}, x::Float) = iround(x)
convert(::Type{Int32}, x::Float) = int32(iround(x))
convert(::Type{Int64}, x::Float) = int64(iround(x))

## floating point promotions ##

promote_rule(::Type{Float64}, ::Type{Float32} ) = Float64

promote_rule(::Type{Float32}, ::Type{Int8} ) = Float32
promote_rule(::Type{Float32}, ::Type{Int16}) = Float32
promote_rule(::Type{Float32}, ::Type{Int32}) = Float64
promote_rule(::Type{Float32}, ::Type{Int64}) = Float64 # TODO: should be Float80

promote_rule(::Type{Float64}, ::Type{Int8} ) = Float64
promote_rule(::Type{Float64}, ::Type{Int16}) = Float64
promote_rule(::Type{Float64}, ::Type{Int32}) = Float64
promote_rule(::Type{Float64}, ::Type{Int64}) = Float64 # TODO: should be Float80

promote_rule(::Type{Float32}, ::Type{Uint8} ) = Float32
promote_rule(::Type{Float32}, ::Type{Uint16}) = Float32
promote_rule(::Type{Float32}, ::Type{Uint32}) = Float64
promote_rule(::Type{Float32}, ::Type{Uint64}) = Float64 # TODO: should be Float80

promote_rule(::Type{Float64}, ::Type{Uint8} ) = Float64
promote_rule(::Type{Float64}, ::Type{Uint16}) = Float64
promote_rule(::Type{Float64}, ::Type{Uint32}) = Float64
promote_rule(::Type{Float64}, ::Type{Uint64}) = Float64 # TODO: should be Float80

promote_rule(::Type{Float32}, ::Type{Char}) = Float32
promote_rule(::Type{Float64}, ::Type{Char}) = Float64

## floating point arithmetic ##

-(x::Float32) = boxf32(neg_float(unbox32(x)))
-(x::Float64) = boxf64(neg_float(unbox64(x)))
+(x::Float32, y::Float32) = boxf32(add_float(unbox32(x), unbox32(y)))
+(x::Float64, y::Float64) = boxf64(add_float(unbox64(x), unbox64(y)))
-(x::Float32, y::Float32) = boxf32(sub_float(unbox32(x), unbox32(y)))
-(x::Float64, y::Float64) = boxf64(sub_float(unbox64(x), unbox64(y)))
*(x::Float32, y::Float32) = boxf32(mul_float(unbox32(x), unbox32(y)))
*(x::Float64, y::Float64) = boxf64(mul_float(unbox64(x), unbox64(y)))
/(x::Float32, y::Float32) = boxf32(div_float(unbox32(x), unbox32(y)))
/(x::Float64, y::Float64) = boxf64(div_float(unbox64(x), unbox64(y)))

# TODO: faster floating point div?
# TODO: faster floating point fld?
# TODO: faster floating point mod?

rem(x::Float32, y::Float32) = boxf32(rem_float(unbox32(x), unbox32(y)))
rem(x::Float64, y::Float64) = boxf64(rem_float(unbox64(x), unbox64(y)))

## floating point comparisons ##

==(x::Float32, y::Float32) = eq_float(unbox32(x),unbox32(y))
==(x::Float64, y::Float64) = eq_float(unbox64(x),unbox64(y))
!=(x::Float32, y::Float32) = ne_float(unbox32(x),unbox32(y))
!=(x::Float64, y::Float64) = ne_float(unbox64(x),unbox64(y))
< (x::Float32, y::Float32) = lt_float(unbox32(x),unbox32(y))
< (x::Float64, y::Float64) = lt_float(unbox64(x),unbox64(y))
<=(x::Float32, y::Float32) = le_float(unbox32(x),unbox32(y))
<=(x::Float64, y::Float64) = le_float(unbox64(x),unbox64(y))

isequal(x::Float32, y::Float32) = fpiseq32(unbox32(x),unbox32(y))
isequal(x::Float64, y::Float64) = fpiseq64(unbox64(x),unbox64(y))
isless (x::Float32, y::Float32) = fpislt32(unbox32(x),unbox32(y))
isless (x::Float64, y::Float64) = fpislt64(unbox64(x),unbox64(y))

isequal(a::Integer, b::Float) = (a==b) & isequal(float(a),b)
isequal(a::Float, b::Integer) = isequal(b, a)
isless (a::Integer, b::Float) = (a<b) | isless(float(a),b)
isless (a::Float, b::Integer) = (a<b) | isless(a,float(b))

==(x::Float64, y::Int64  ) = eqfsi64(unbox64(x),unbox64(y))
==(x::Float64, y::Uint64 ) = eqfui64(unbox64(x),unbox64(y))
==(x::Int64  , y::Float64) = eqfsi64(unbox64(y),unbox64(x))
==(x::Uint64 , y::Float64) = eqfui64(unbox64(y),unbox64(x))

==(x::Float32, y::Int64  ) = eqfsi64(unbox64(float64(x)),unbox64(y))
==(x::Float32, y::Uint64 ) = eqfui64(unbox64(float64(x)),unbox64(y))
==(x::Int64  , y::Float32) = eqfsi64(unbox64(float64(y)),unbox64(x))
==(x::Uint64 , y::Float32) = eqfui64(unbox64(float64(y)),unbox64(x))

< (x::Float64, y::Int64  ) = ltfsi64(unbox64(x),unbox64(y))
< (x::Float64, y::Uint64 ) = ltfui64(unbox64(x),unbox64(y))
< (x::Int64  , y::Float64) = ltsif64(unbox64(x),unbox64(y))
< (x::Uint64 , y::Float64) = ltuif64(unbox64(x),unbox64(y))

< (x::Float32, y::Int64  ) = ltfsi64(unbox64(float64(x)),unbox64(y))
< (x::Float32, y::Uint64 ) = ltfui64(unbox64(float64(x)),unbox64(y))
< (x::Int64  , y::Float32) = ltsif64(unbox64(x),unbox64(float64(y)))
< (x::Uint64 , y::Float32) = ltuif64(unbox64(x),unbox64(float64(y)))

<=(x::Float64, y::Int64  ) = lefsi64(unbox64(x),unbox64(y))
<=(x::Float64, y::Uint64 ) = lefui64(unbox64(x),unbox64(y))
<=(x::Int64  , y::Float64) = lesif64(unbox64(x),unbox64(y))
<=(x::Uint64 , y::Float64) = leuif64(unbox64(x),unbox64(y))

<=(x::Float32, y::Int64  ) = lefsi64(unbox64(float64(x)),unbox64(y))
<=(x::Float32, y::Uint64 ) = lefui64(unbox64(float64(x)),unbox64(y))
<=(x::Int64  , y::Float32) = lesif64(unbox64(x),unbox64(float64(y)))
<=(x::Uint64 , y::Float32) = leuif64(unbox64(x),unbox64(float64(y)))

## floating point traits ##

const Inf32 = boxf32(unbox32(0x7f800000))
const NaN32 = boxf32(unbox32(0x7fc00000))
const Inf = boxf64(unbox64(0x7ff0000000000000))
const NaN = boxf64(unbox64(0x7ff8000000000000))

@eval begin
    inf(::Type{Float32}) = $Inf32
    nan(::Type{Float32}) = $NaN32
    inf(::Type{Float64}) = $Inf
    nan(::Type{Float64}) = $NaN
    inf{T<:Float}(x::T) = inf(T)
    nan{T<:Float}(x::T) = nan(T)

    isdenormal(x::Float32) = (abs(x) < $boxf32(unbox32(0x00800000)))
    isdenormal(x::Float64) = (abs(x) < $boxf64(unbox64(0x0010000000000000)))

    typemin(::Type{Float32}) = $(-Inf32)
    typemax(::Type{Float32}) = $(Inf32)
    typemin(::Type{Float64}) = $(-Inf)
    typemax(::Type{Float64}) = $(Inf)
    typemin{T<:Real}(x::T) = typemin(T)
    typemax{T<:Real}(x::T) = typemax(T)

    realmin(::Type{Float32}) = $boxf32(unbox32(0x00800000))
    realmin(::Type{Float64}) = $boxf64(unbox64(0x0010000000000000))
    realmax(::Type{Float32}) = $boxf32(unbox32(0x7f7fffff))
    realmax(::Type{Float64}) = $boxf64(unbox64(0x7fefffffffffffff))
    realmin{T<:Float}(x::T) = realmin(T)
    realmax{T<:Float}(x::T) = realmax(T)
    realmin() = realmin(Float64)
    realmax() = realmax(Float64)

    nextfloat(x::Float32, i::Integer) = boxf32(add_int(unbox32(x),unbox32(int32(i))))
    nextfloat(x::Float64, i::Integer) = boxf64(add_int(unbox64(x),unbox64(int64(i))))
    nextfloat(x::Float) = nextfloat(x,1)
    prevfloat(x::Float) = nextfloat(x,-1)

    eps(x::Float) = isfinite(x) ? abs(nextfloat(x)-x) : nan(x)
    eps(::Type{Float32}) = $boxf32(unbox32(0x34000000))
    eps(::Type{Float64}) = $boxf64(unbox64(0x3cb0000000000000))
    eps() = eps(Float64)
end

sizeof(::Type{Float32}) = 4
sizeof(::Type{Float64}) = 8

## mathematical constants ##

const e  = 2.71828182845904523536
const pi = 3.14159265358979323846
