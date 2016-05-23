# This file is a part of Julia. License is MIT: http://julialang.org/license

## general machinery for irrational mathematical constants

immutable Irrational{sym} <: Real end

show{sym}(io::IO, x::Irrational{sym}) = print(io, "$sym = $(string(float(x))[1:15])...")

promote_rule{s}(::Type{Irrational{s}}, ::Type{Float32}) = Float32
promote_rule{s,t}(::Type{Irrational{s}}, ::Type{Irrational{t}}) = Float64
promote_rule{s,T<:Number}(::Type{Irrational{s}}, ::Type{T}) = promote_type(Float64,T)

convert(::Type{AbstractFloat}, x::Irrational) = Float64(x)
convert(::Type{Float16}, x::Irrational) = Float16(Float32(x))
convert{T<:Real}(::Type{Complex{T}}, x::Irrational) = convert(Complex{T}, convert(T,x))

@pure function convert{T<:Integer}(::Type{Rational{T}}, x::Irrational)
    o = precision(BigFloat)
    p = 256
    while true
        setprecision(BigFloat, p)
        bx = BigFloat(x)
        r = rationalize(T, bx, tol=0)
        if abs(BigFloat(r) - bx) > eps(bx)
            setprecision(BigFloat, o)
            return r
        end
        p += 32
    end
end
convert(::Type{Rational{BigInt}}, x::Irrational) = throw(ArgumentError("Cannot convert an Irrational to a Rational{BigInt}: use rationalize(Rational{BigInt}, x) instead"))

@pure function (t::Type{T}){T<:Union{Float32,Float64}}(x::Irrational, r::RoundingMode)
    setprecision(BigFloat, 256) do
        T(BigFloat(x), r)
    end
end

=={s}(::Irrational{s}, ::Irrational{s}) = true
==(::Irrational, ::Irrational) = false

# Irrationals, by definition, can't have a finite representation equal them exactly
==(x::Irrational, y::Real) = false
==(x::Real, y::Irrational) = false

# Irrational vs AbstractFloat
<(x::Irrational, y::Float64) = Float64(x,RoundUp) <= y
<(x::Float64, y::Irrational) = x <= Float64(y,RoundDown)
<(x::Irrational, y::Float32) = Float32(x,RoundUp) <= y
<(x::Float32, y::Irrational) = x <= Float32(y,RoundDown)
<(x::Irrational, y::Float16) = Float32(x,RoundUp) <= y
<(x::Float16, y::Irrational) = x <= Float32(y,RoundDown)
<(x::Irrational, y::BigFloat) = setprecision(precision(y)+32) do
    big(x) < y
end
<(x::BigFloat, y::Irrational) = setprecision(precision(x)+32) do
    x < big(y)
end

<=(x::Irrational,y::AbstractFloat) = x < y
<=(x::AbstractFloat,y::Irrational) = x < y

# Irrational vs Rational
@generated function <{T}(x::Irrational, y::Rational{T})
    bx = big(x())
    bx < 0 && T <: Unsigned && return true
    rx = rationalize(T,bx,tol=0)
    rx < bx ? :($rx < y) : :($rx <= y)
end
@generated function <{T}(x::Rational{T}, y::Irrational)
    by = big(y())
    by < 0 && T <: Unsigned && return false
    ry = rationalize(T,by,tol=0)
    ry < by ? :(x <= $ry) : :(x < $ry)
end
<(x::Irrational, y::Rational{BigInt}) = big(x) < y
<(x::Rational{BigInt}, y::Irrational) = x < big(y)

<=(x::Irrational,y::Rational) = x < y
<=(x::Rational,y::Irrational) = x < y

isfinite(::Irrational) = true

hash(x::Irrational, h::UInt) = 3*object_id(x) - h

-(x::Irrational) = -Float64(x)
for op in Symbol[:+, :-, :*, :/, :^]
    @eval $op(x::Irrational, y::Irrational) = $op(Float64(x),Float64(y))
end

macro irrational(sym, val, def)
    esym = esc(sym)
    qsym = esc(Expr(:quote, sym))
    bigconvert = isa(def,Symbol) ? quote
        function Base.convert(::Type{BigFloat}, ::Irrational{$qsym})
            c = BigFloat()
            ccall(($(string("mpfr_const_", def)), :libmpfr),
                  Cint, (Ptr{BigFloat}, Int32),
                  &c, MPFR.ROUNDING_MODE[end])
            return c
        end
    end : quote
        Base.convert(::Type{BigFloat}, ::Irrational{$qsym}) = $(esc(def))
    end
    quote
        const $esym = Irrational{$qsym}()
        $bigconvert
        Base.convert(::Type{Float64}, ::Irrational{$qsym}) = $val
        Base.convert(::Type{Float32}, ::Irrational{$qsym}) = $(Float32(val))
        @assert isa(big($esym), BigFloat)
        @assert Float64($esym) == Float64(big($esym))
        @assert Float32($esym) == Float32(big($esym))
    end
end

big(x::Irrational) = convert(BigFloat,x)

## specific irriational mathematical constants

@irrational π        3.14159265358979323846  pi
@irrational e        2.71828182845904523536  exp(big(1))
@irrational γ        0.57721566490153286061  euler
@irrational catalan  0.91596559417721901505  catalan
@irrational φ        1.61803398874989484820  (1+sqrt(big(5)))/2

# aliases
const pi = π
const eu = e
const eulergamma = γ
const golden = φ

# special behaviors

# use exp for e^x or e.^x, as in
#    ^(::Irrational{:e}, x::Number) = exp(x)
#    .^(::Irrational{:e}, x) = exp(x)
# but need to loop over types to prevent ambiguity with generic rules for ^(::Number, x) etc.
for T in (Irrational, Rational, Integer, Number)
    ^(::Irrational{:e}, x::T) = exp(x)
end
for T in (Range, BitArray, StridedArray, AbstractArray)
    .^(::Irrational{:e}, x::T) = exp(x)
end

log(::Irrational{:e}) = 1 # use 1 to correctly promote expressions like log(x)/log(e)
log(::Irrational{:e}, x::Number) = log(x)

# align along = for nice Array printing
function alignment(io::IO, x::Irrational)
    m = match(r"^(.*?)(=.*)$", sprint(0, showcompact, x, env=io))
    m === nothing ? (length(sprint(0, showcompact, x, env=io)), 0) :
    (length(m.captures[1]), length(m.captures[2]))
end
