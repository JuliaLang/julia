#bitstype 16 Float16 <: FloatingPoint
## conversions to floating-point ##

convert(::Type{Float32}, x::Int128)  = float32(uint128(abs(x)))*(1-2(x<0))
convert(::Type{Float32}, x::Uint128) = float32(uint64(x)) + ldexp(float32(uint64(x>>>64)),64)
promote_rule(::Type{Float32}, ::Type{Int128} ) = Float32
promote_rule(::Type{Float32}, ::Type{Uint128}) = Float32

convert(::Type{Float64}, x::Int128)  = float64(uint128(abs(x)))*(1-2(x<0))
convert(::Type{Float64}, x::Uint128) = float64(uint64(x)) + ldexp(float64(uint64(x>>>64)),64)
promote_rule(::Type{Float64}, ::Type{Int128} ) = Float64
promote_rule(::Type{Float64}, ::Type{Uint128}) = Float64

for t1 in (Float32,Float64) #,Float16)
    for st in (Int8,Int16,Int32,Int64)
        @eval begin 
            convert(::Type{$t1},x::($st)) = box($t1,sitofp($t1,unbox($st,x)))
            promote_rule(::Type{$t1}, ::Type{$st}  ) = $t1
        end
    end
    for ut in (Bool,Char,Uint8,Uint16,Uint32,Uint64)
        @eval begin
            convert(::Type{$t1},x::($ut)) = box($t1,uitofp($t1,unbox($ut,x)))
            promote_rule(::Type{$t1}, ::Type{$ut}  ) = $t1
        end
    end
end
#convert(::Type{Float16}, x::Union(Float32,Float64)) = box(Float16,fptrunc(x,Float16))
#convert(::Type{Float32}, x::Float16) = box(Float32,fpext(Float32,x))
convert(::Type{Float32}, x::Float64) = box(Float32,fptrunc(Float32,x))

# REPLACE when enabling Float16
#convert(::Type{Float64}, x::Union(Float32,Float16)) = box(Float64,fpext(Float64,x))
convert(::Type{Float64}, x::Float32) = box(Float64,fpext(Float64,x))

convert(::Type{FloatingPoint}, x::Bool)    = convert(Float32, x)
convert(::Type{FloatingPoint}, x::Char)    = convert(Float32, x)
convert(::Type{FloatingPoint}, x::Int8)    = convert(Float32, x)
convert(::Type{FloatingPoint}, x::Int16)   = convert(Float32, x)
convert(::Type{FloatingPoint}, x::Int32)   = convert(Float64, x)
convert(::Type{FloatingPoint}, x::Int64)   = convert(Float64, x) # LOSSY
convert(::Type{FloatingPoint}, x::Int128)  = convert(Float64, x) # LOSSY
convert(::Type{FloatingPoint}, x::Uint8)   = convert(Float32, x)
convert(::Type{FloatingPoint}, x::Uint16)  = convert(Float32, x)
convert(::Type{FloatingPoint}, x::Uint32)  = convert(Float64, x)
convert(::Type{FloatingPoint}, x::Uint64)  = convert(Float64, x) # LOSSY
convert(::Type{FloatingPoint}, x::Uint128) = convert(Float64, x) # LOSSY

#float16(x) = convert(Float16, x)
float32(x) = convert(Float32, x)
float64(x) = convert(Float64, x)
float(x)   = convert(FloatingPoint, x)

## conversions from floating-point ##

# fallbacks using only convert, trunc, ceil, floor, round
itrunc(x::FloatingPoint) = convert(Integer,trunc(x))
iceil (x::FloatingPoint) = convert(Integer,ceil(x))  # TODO: fast primitive for iceil
ifloor(x::FloatingPoint) = convert(Integer,floor(x)) # TOOD: fast primitive for ifloor
iround(x::FloatingPoint) = convert(Integer,round(x))

itrunc{T<:Integer}(::Type{T}, x::FloatingPoint) = convert(T,trunc(x))
iceil {T<:Integer}(::Type{T}, x::FloatingPoint) = convert(T,ceil(x))
ifloor{T<:Integer}(::Type{T}, x::FloatingPoint) = convert(T,floor(x))
iround{T<:Integer}(::Type{T}, x::FloatingPoint) = convert(T,round(x))

## fast specific type converions ##

if WORD_SIZE == 64
    iround(x::Float32) = iround(float64(x))
    itrunc(x::Float32) = itrunc(float64(x))
    iround(x::Float64) = box(Int64,fpsiround(unbox(Float64,x)))
    itrunc(x::Float64) = box(Int64,fptosi(unbox(Float64,x)))
else
    iround(x::Float32) = box(Int32,fpsiround(unbox(Float32,x)))
    itrunc(x::Float32) = box(Int32,fptosi(unbox(Float32,x)))
    iround(x::Float64) = int32(box(Int64,fpsiround(unbox(Float64,x))))
    itrunc(x::Float64) = int32(box(Int64,fptosi(unbox(Float64,x))))
end

for to in (Int8, Uint8, Int16, Uint16)
    @eval begin
        iround(::Type{$to}, x::Float32) = box($to,trunc_int($to,fpsiround(unbox(Float32,x))))
        iround(::Type{$to}, x::Float64) = box($to,trunc_int($to,fpsiround(unbox(Float64,x))))
    end
end

iround(::Type{Int32}, x::Float32) = box(Int32,fpsiround(unbox(Float32,x)))
iround(::Type{Int32}, x::Float64) = box(Int32,trunc_int(Int32,fpsiround(unbox(Float64,x))))
iround(::Type{Uint32}, x::Float32) = box(Uint32,fpuiround(unbox(Float32,x)))
iround(::Type{Uint32}, x::Float64) = box(Uint32,trunc_int(Uint32,fpuiround(unbox(Float64,x))))
iround(::Type{Int64}, x::Float32) = box(Int64,fpsiround(float64(x)))
iround(::Type{Int64}, x::Float64) = box(Int64,fpsiround(unbox(Float64,x)))
iround(::Type{Uint64}, x::Float32) = box(Uint64,fpuiround(float64(x)))
iround(::Type{Uint64}, x::Float64) = box(Uint64,fpuiround(unbox(Float64,x)))

iround(::Type{Int128}, x::Float32) = convert(Int128,round(x))
iround(::Type{Int128}, x::Float64) = convert(Int128,round(x))
iround(::Type{Uint128}, x::Float32) = convert(Uint128,round(x))
iround(::Type{Uint128}, x::Float64) = convert(Uint128,round(x))

# this is needed very early because it is used by Range and colon
round(x::Float64) = ccall((:round, Base.libm_name), Float64, (Float64,), x)
floor(x::Float64) = ccall((:floor, Base.libm_name), Float64, (Float64,), x)

## floating point promotions ##

#promote_rule(::Type{Float32}, ::Type{Float16}) = Float32
promote_rule(::Type{Float64}, ::Type{Float32}) = Float64

#morebits(::Type{Float16}) = Float32
morebits(::Type{Float32}) = Float64

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

# TODO: faster floating point div?
# TODO: faster floating point fld?
# TODO: faster floating point mod?

rem(x::Float32, y::Float32) = box(Float32,rem_float(unbox(Float32,x),unbox(Float32,y)))
rem(x::Float64, y::Float64) = box(Float64,rem_float(unbox(Float64,x),unbox(Float64,y)))

mod{T<:FloatingPoint}(x::T, y::T) = rem(y+rem(x,y),y)

## floating point comparisons ##

==(x::Float32, y::Float32) = eq_float(unbox(Float32,x),unbox(Float32,y))
==(x::Float64, y::Float64) = eq_float(unbox(Float64,x),unbox(Float64,y))
!=(x::Float32, y::Float32) = ne_float(unbox(Float32,x),unbox(Float32,y))
!=(x::Float64, y::Float64) = ne_float(unbox(Float64,x),unbox(Float64,y))
< (x::Float32, y::Float32) = lt_float(unbox(Float32,x),unbox(Float32,y))
< (x::Float64, y::Float64) = lt_float(unbox(Float64,x),unbox(Float64,y))
<=(x::Float32, y::Float32) = le_float(unbox(Float32,x),unbox(Float32,y))
<=(x::Float64, y::Float64) = le_float(unbox(Float64,x),unbox(Float64,y))

isequal(x::FloatingPoint, y::FloatingPoint) =
    ((x==y) & (signbit(x)==signbit(y))) | (isnan(x)&isnan(y))

isequal(x::Float32, y::Float32) = fpiseq(unbox(Float32,x),unbox(Float32,y))
isequal(x::Float64, y::Float64) = fpiseq(unbox(Float64,x),unbox(Float64,y))
isless (x::Float32, y::Float32) = fpislt(unbox(Float32,x),unbox(Float32,y))
isless (x::Float64, y::Float64) = fpislt(unbox(Float64,x),unbox(Float64,y))

isequal(a::Integer, b::FloatingPoint) = (a==b) & isequal(float(a),b)
isequal(a::FloatingPoint, b::Integer) = isequal(b, a)
isless (a::Integer, b::FloatingPoint) = (a<b) | isless(float(a),b)
isless (a::FloatingPoint, b::Integer) = (a<b) | isless(a,float(b))

==(x::Float64, y::Int64  ) = eqfsi64(unbox(Float64,x),unbox(Int64,y))
==(x::Float64, y::Uint64 ) = eqfui64(unbox(Float64,x),unbox(Uint64,y))
==(x::Int64  , y::Float64) = eqfsi64(unbox(Float64,y),unbox(Int64,x))
==(x::Uint64 , y::Float64) = eqfui64(unbox(Float64,y),unbox(Uint64,x))

==(x::Float32, y::Int64  ) = eqfsi64(unbox(Float64,float64(x)),unbox(Int64,y))
==(x::Float32, y::Uint64 ) = eqfui64(unbox(Float64,float64(x)),unbox(Uint64,y))
==(x::Int64  , y::Float32) = eqfsi64(unbox(Float64,float64(y)),unbox(Int64,x))
==(x::Uint64 , y::Float32) = eqfui64(unbox(Float64,float64(y)),unbox(Uint64,x))

< (x::Float64, y::Int64  ) = ltfsi64(unbox(Float64,x),unbox(Int64,y))
< (x::Float64, y::Uint64 ) = ltfui64(unbox(Float64,x),unbox(Uint64,y))
< (x::Int64  , y::Float64) = ltsif64(unbox(Int64,x),unbox(Float64,y))
< (x::Uint64 , y::Float64) = ltuif64(unbox(Uint64,x),unbox(Float64,y))

< (x::Float32, y::Int64  ) = ltfsi64(unbox(Float64,float64(x)),unbox(Int64,y))
< (x::Float32, y::Uint64 ) = ltfui64(unbox(Float64,float64(x)),unbox(Uint64,y))
< (x::Int64  , y::Float32) = ltsif64(unbox(Int64,x),unbox(Float64,float64(y)))
< (x::Uint64 , y::Float32) = ltuif64(unbox(Uint64,x),unbox(Float64,float64(y)))

<=(x::Float64, y::Int64  ) = lefsi64(unbox(Float64,x),unbox(Int64,y))
<=(x::Float64, y::Uint64 ) = lefui64(unbox(Float64,x),unbox(Uint64,y))
<=(x::Int64  , y::Float64) = lesif64(unbox(Int64,x),unbox(Float64,y))
<=(x::Uint64 , y::Float64) = leuif64(unbox(Uint64,x),unbox(Float64,y))

<=(x::Float32, y::Int64  ) = lefsi64(unbox(Float64,float64(x)),unbox(Int64,y))
<=(x::Float32, y::Uint64 ) = lefui64(unbox(Float64,float64(x)),unbox(Uint64,y))
<=(x::Int64  , y::Float32) = lesif64(unbox(Int64,x),unbox(Float64,float64(y)))
<=(x::Uint64 , y::Float32) = leuif64(unbox(Uint64,x),unbox(Float64,float64(y)))

==(x::Float32, y::Union(Int32,Uint32)) = float64(x)==float64(y)
==(x::Union(Int32,Uint32), y::Float32) = float64(x)==float64(y)

<(x::Float32, y::Union(Int32,Uint32)) = float64(x)<float64(y)
<(x::Union(Int32,Uint32), y::Float32) = float64(x)<float64(y)

<=(x::Float32, y::Union(Int32,Uint32)) = float64(x)<=float64(y)
<=(x::Union(Int32,Uint32), y::Float32) = float64(x)<=float64(y)

## floating point traits ##

const Inf32 = box(Float32,unbox(Uint32,0x7f800000))
const NaN32 = box(Float32,unbox(Uint32,0x7fc00000))
const Inf = box(Float64,unbox(Uint64,0x7ff0000000000000))
const NaN = box(Float64,unbox(Uint64,0x7ff8000000000000))

@eval begin
    inf(::Type{Float32}) = $Inf32
    nan(::Type{Float32}) = $NaN32
    inf(::Type{Float64}) = $Inf
    nan(::Type{Float64}) = $NaN
    inf{T<:FloatingPoint}(x::T) = inf(T)
    nan{T<:FloatingPoint}(x::T) = nan(T)

    issubnormal(x::Float32) = (abs(x) < $(box(Float32,unbox(Uint32,0x00800000)))) & (x!=0)
    issubnormal(x::Float64) = (abs(x) < $(box(Float64,unbox(Uint64,0x0010000000000000)))) & (x!=0)

    typemin(::Type{Float32}) = $(-Inf32)
    typemax(::Type{Float32}) = $(Inf32)
    typemin(::Type{Float64}) = $(-Inf)
    typemax(::Type{Float64}) = $(Inf)
    typemin{T<:Real}(x::T) = typemin(T)
    typemax{T<:Real}(x::T) = typemax(T)

    realmin(::Type{Float32}) = $(box(Float32,unbox(Uint32,0x00800000)))
    realmin(::Type{Float64}) = $(box(Float64,unbox(Uint64,0x0010000000000000)))
    realmax(::Type{Float32}) = $(box(Float32,unbox(Uint32,0x7f7fffff)))
    realmax(::Type{Float64}) = $(box(Float64,unbox(Uint64,0x7fefffffffffffff)))
    realmin{T<:FloatingPoint}(x::T) = realmin(T)
    realmax{T<:FloatingPoint}(x::T) = realmax(T)
    realmin() = realmin(Float64)
    realmax() = realmax(Float64)

    nextfloat(x::Float32, i::Integer) = box(Float32,add_int(unbox(Float32,x),unbox(Int32,int32(i))))
    nextfloat(x::Float64, i::Integer) = box(Float64,add_int(unbox(Float64,x),unbox(Int64,int64(i))))
    nextfloat(x::FloatingPoint) = nextfloat(x,1)
    prevfloat(x::FloatingPoint) = nextfloat(x,-1)

    eps(x::FloatingPoint) = isfinite(x) ? abs(nextfloat(x)-x) : nan(x)
    eps(::Type{Float32}) = $(box(Float32,unbox(Uint32,0x34000000)))
    eps(::Type{Float64}) = $(box(Float64,unbox(Uint64,0x3cb0000000000000)))
    eps() = eps(Float64)
end

sizeof(::Type{Float32}) = 4
sizeof(::Type{Float64}) = 8
