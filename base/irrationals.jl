# This file is a part of Julia. License is MIT: https://julialang.org/license

## general machinery for irrational mathematical constants

"""
    AbstractIrrational <: Real

Number type representing an exact irrational value.
"""
abstract type AbstractIrrational <: Real end

"""
    Irrational{sym} <: AbstractIrrational

Number type representing an exact irrational value denoted by the
symbol `sym`.
"""
struct Irrational{sym} <: AbstractIrrational end

show(io::IO, x::Irrational{sym}) where {sym} = print(io, sym)

function show(io::IO, ::MIME"text/plain", x::Irrational{sym}) where {sym}
    if get(io, :compact, false)
        print(io, sym)
    else
        print(io, sym, " = ", string(float(x))[1:15], "...")
    end
end

promote_rule(::Type{<:AbstractIrrational}, ::Type{Float16}) = Float16
promote_rule(::Type{<:AbstractIrrational}, ::Type{Float32}) = Float32
promote_rule(::Type{<:AbstractIrrational}, ::Type{<:AbstractIrrational}) = Float64
promote_rule(::Type{<:AbstractIrrational}, ::Type{T}) where {T<:Real} = promote_type(Float64, T)
promote_rule(::Type{S}, ::Type{T}) where {S<:AbstractIrrational,T<:Number} = promote_type(promote_type(S, real(T)), T)

AbstractFloat(x::AbstractIrrational) = Float64(x)
Float16(x::AbstractIrrational) = Float16(Float32(x))
Complex{T}(x::AbstractIrrational) where {T<:Real} = Complex{T}(T(x))

@pure function Rational{T}(x::AbstractIrrational) where T<:Integer
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
(::Type{Rational{BigInt}})(x::AbstractIrrational) = throw(ArgumentError("Cannot convert an AbstractIrrational to a Rational{BigInt}: use rationalize(Rational{BigInt}, x) instead"))

@pure function (t::Type{T})(x::AbstractIrrational, r::RoundingMode) where T<:Union{Float32,Float64}
    setprecision(BigFloat, 256) do
        T(BigFloat(x), r)
    end
end

float(::Type{<:AbstractIrrational}) = Float64

==(::Irrational{s}, ::Irrational{s}) where {s} = true
==(::AbstractIrrational, ::AbstractIrrational) = false

<(::Irrational{s}, ::Irrational{s}) where {s} = false
function <(x::AbstractIrrational, y::AbstractIrrational)
    Float64(x) != Float64(y) || throw(MethodError(<, (x, y)))
    return Float64(x) < Float64(y)
end

<=(::Irrational{s}, ::Irrational{s}) where {s} = true
<=(x::AbstractIrrational, y::AbstractIrrational) = x==y || x<y

# Irrationals, by definition, can't have a finite representation equal them exactly
==(x::AbstractIrrational, y::Real) = false
==(x::Real, y::AbstractIrrational) = false

# Irrational vs AbstractFloat
<(x::AbstractIrrational, y::Float64) = Float64(x,RoundUp) <= y
<(x::Float64, y::AbstractIrrational) = x <= Float64(y,RoundDown)
<(x::AbstractIrrational, y::Float32) = Float32(x,RoundUp) <= y
<(x::Float32, y::AbstractIrrational) = x <= Float32(y,RoundDown)
<(x::AbstractIrrational, y::Float16) = Float32(x,RoundUp) <= y
<(x::Float16, y::AbstractIrrational) = x <= Float32(y,RoundDown)
<(x::AbstractIrrational, y::BigFloat) = setprecision(precision(y)+32) do
    big(x) < y
end
<(x::BigFloat, y::AbstractIrrational) = setprecision(precision(x)+32) do
    x < big(y)
end

<=(x::AbstractIrrational, y::AbstractFloat) = x < y
<=(x::AbstractFloat, y::AbstractIrrational) = x < y

# Irrational vs Rational
@pure function rationalize(::Type{T}, x::AbstractIrrational; tol::Real=0) where T
    return rationalize(T, big(x), tol=tol)
end
@pure function lessrational(rx::Rational{<:Integer}, x::AbstractIrrational)
    # an @pure version of `<` for determining if the rationalization of
    # an irrational number required rounding up or down
    return rx < big(x)
end
function <(x::AbstractIrrational, y::Rational{T}) where T
    T <: Unsigned && x < 0.0 && return true
    rx = rationalize(T, x)
    if lessrational(rx, x)
        return rx < y
    else
        return rx <= y
    end
end
function <(x::Rational{T}, y::AbstractIrrational) where T
    T <: Unsigned && y < 0.0 && return false
    ry = rationalize(T, y)
    if lessrational(ry, y)
        return x <= ry
    else
        return x < ry
    end
end
<(x::AbstractIrrational, y::Rational{BigInt}) = big(x) < y
<(x::Rational{BigInt}, y::AbstractIrrational) = x < big(y)

<=(x::AbstractIrrational, y::Rational) = x < y
<=(x::Rational, y::AbstractIrrational) = x < y

isfinite(::AbstractIrrational) = true
isinteger(::AbstractIrrational) = false
iszero(::AbstractIrrational) = false
isone(::AbstractIrrational) = false

hash(x::Irrational, h::UInt) = 3*objectid(x) - h

widen(::Type{T}) where {T<:Irrational} = T

zero(::AbstractIrrational) = false
zero(::Type{<:AbstractIrrational}) = false

one(::AbstractIrrational) = true
one(::Type{<:AbstractIrrational}) = true

oneunit(T::Type{<:AbstractIrrational}) = throw(ArgumentError("The number one cannot be of type $T"))
oneunit(x::T) where {T <: AbstractIrrational} = oneunit(T)

-(x::AbstractIrrational) = -Float64(x)
for op in Symbol[:+, :-, :*, :/, :^]
    @eval $op(x::AbstractIrrational, y::AbstractIrrational) = $op(Float64(x),Float64(y))
end
*(x::Bool, y::AbstractIrrational) = ifelse(x, Float64(y), 0.0)

round(x::Irrational, r::RoundingMode) = round(float(x), r)

"""
	@irrational sym val def
	@irrational(sym, val, def)

Define a new `Irrational` value, `sym`, with pre-computed `Float64` value `val`,
and arbitrary-precision definition in terms of `BigFloat`s given be the expression `def`.
"""
macro irrational(sym, val, def)
    esym = esc(sym)
    qsym = esc(Expr(:quote, sym))
    bigconvert = isa(def,Symbol) ? quote
        function Base.BigFloat(::Irrational{$qsym}, r::MPFR.MPFRRoundingMode=MPFR.ROUNDING_MODE[]; precision=precision(BigFloat))
            c = BigFloat(;precision=precision)
            ccall(($(string("mpfr_const_", def)), :libmpfr),
                  Cint, (Ref{BigFloat}, MPFR.MPFRRoundingMode), c, r)
            return c
        end
    end : quote
        function Base.BigFloat(::Irrational{$qsym}; precision=precision(BigFloat))
            setprecision(BigFloat, precision) do
                $(esc(def))
            end
        end
    end
    quote
        const $esym = Irrational{$qsym}()
        $bigconvert
        Base.Float64(::Irrational{$qsym}) = $val
        Base.Float32(::Irrational{$qsym}) = $(Float32(val))
        @assert isa(big($esym), BigFloat)
        @assert Float64($esym) == Float64(big($esym))
        @assert Float32($esym) == Float32(big($esym))
    end
end

big(x::AbstractIrrational) = BigFloat(x)
big(::Type{<:AbstractIrrational}) = BigFloat

# align along = for nice Array printing
function alignment(io::IO, x::AbstractIrrational)
    m = match(r"^(.*?)(=.*)$", sprint(show, x, context=io, sizehint=0))
    m === nothing ? (length(sprint(show, x, context=io, sizehint=0)), 0) :
    (length(m.captures[1]), length(m.captures[2]))
end

# inv
inv(x::AbstractIrrational) = 1/x
