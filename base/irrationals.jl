# This file is a part of Julia. License is MIT: https://julialang.org/license

## general machinery for irrational mathematical constants

"""
    AbstractIrrational <: Real

Number type representing an exact irrational value, which is automatically rounded to the correct precision in
arithmetic operations with other numeric quantities.

Subtypes `MyIrrational <: AbstractIrrational` should implement at least `==(::MyIrrational, ::MyIrrational)`,
`hash(x::MyIrrational, h::UInt)`, and `convert(::Type{F}, x::MyIrrational) where {F <: Union{BigFloat,Float32,Float64}}`.

If a subtype is used to represent values that may occasionally be rational (e.g. a square-root type that represents `√n`
for integers `n` will give a rational result when `n` is a perfect square), then it should also implement
`isinteger`, `iszero`, `isone`, and `==` with `Real` values (since all of these default to `false` for
`AbstractIrrational` types), as well as defining [`hash`](@ref) to equal that of the corresponding `Rational`.
"""
abstract type AbstractIrrational <: Real end

"""
    Irrational{sym} <: AbstractIrrational

Number type representing an exact irrational value denoted by the
symbol `sym`, such as [`π`](@ref pi), [`ℯ`](@ref) and [`γ`](@ref Base.MathConstants.eulergamma).

See also [`AbstractIrrational`](@ref).
"""
struct Irrational{sym} <: AbstractIrrational end

typemin(::Type{T}) where {T<:Irrational} = T()
typemax(::Type{T}) where {T<:Irrational} = T()

show(io::IO, x::Irrational{sym}) where {sym} = print(io, sym)

function show(io::IO, ::MIME"text/plain", x::Irrational{sym}) where {sym}
    if get(io, :compact, false)::Bool
        print(io, sym)
    else
        print(io, sym, " = ", string(float(x))[1:min(end,15)], "...")
    end
end

promote_rule(::Type{<:AbstractIrrational}, ::Type{Float16}) = Float16
promote_rule(::Type{<:AbstractIrrational}, ::Type{Float32}) = Float32
promote_rule(::Type{<:AbstractIrrational}, ::Type{<:AbstractIrrational}) = Float64
promote_rule(::Type{<:AbstractIrrational}, ::Type{T}) where {T<:Real} = promote_type(Float64, T)

function promote_rule(::Type{S}, ::Type{T}) where {S<:AbstractIrrational,T<:Number}
    U = promote_type(S, real(T))
    if S <: U
        # prevent infinite recursion
        promote_type(Float64, T)
    else
        promote_type(U, T)
    end
end

AbstractFloat(x::AbstractIrrational) = Float64(x)::Float64
Float16(x::AbstractIrrational) = Float16(Float32(x)::Float32)
Complex{T}(x::AbstractIrrational) where {T<:Real} = Complex{T}(T(x))

function _irrational_to_rational_at_current_precision(::Type{T}, x::AbstractIrrational) where {T <: Integer}
    bx = BigFloat(x)
    r = rationalize(T, bx, tol = 0)
    if abs(BigFloat(r) - bx) > eps(bx)
        r
    else
        nothing  # Error is too small, repeat with greater precision.
    end
end
function _irrational_to_rational_at_precision(::Type{T}, x::AbstractIrrational, p::Int) where {T <: Integer}
    f = let x = x
        () -> _irrational_to_rational_at_current_precision(T, x)
    end
    setprecision(f, BigFloat, p)
end
function _irrational_to_rational_at_current_rounding_mode(::Type{T}, x::AbstractIrrational) where {T <: Integer}
    if T <: BigInt
        _throw_argument_error_irrational_to_rational_bigint()  # avoid infinite loop
    end
    p = 256
    while true
        r = _irrational_to_rational_at_precision(T, x, p)
        if r isa Number
            return r
        end
        p += 32
    end
end
function _irrational_to_rational(::Type{T}, x::AbstractIrrational) where {T <: Integer}
    f = let x = x
        () -> _irrational_to_rational_at_current_rounding_mode(T, x)
    end
    setrounding(f, BigFloat, RoundNearest)
end
Rational{T}(x::AbstractIrrational) where {T<:Integer} = _irrational_to_rational(T, x)
_throw_argument_error_irrational_to_rational_bigint() = throw(ArgumentError("Cannot convert an AbstractIrrational to a Rational{BigInt}: use rationalize(BigInt, x) instead"))
Rational{BigInt}(::AbstractIrrational) = _throw_argument_error_irrational_to_rational_bigint()

function _irrational_to_float(::Type{T}, x::AbstractIrrational, r::RoundingMode) where T<:Union{Float32,Float64}
    setprecision(BigFloat, 256) do
        T(BigFloat(x)::BigFloat, r)
    end
end
(::Type{T})(x::AbstractIrrational, r::RoundingMode) where {T<:Union{Float32,Float64}} = _irrational_to_float(T, x, r)

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
function _rationalize_irrational(::Type{T}, x::AbstractIrrational, tol::Real) where {T<:Integer}
    return rationalize(T, big(x), tol=tol)
end
function rationalize(::Type{T}, x::AbstractIrrational; tol::Real=0) where {T<:Integer}
    return _rationalize_irrational(T, x, tol)
end
function _lessrational(rx::Rational, x::AbstractIrrational)
    return rx < big(x)
end
function lessrational(rx::Rational, x::AbstractIrrational)
    return _lessrational(rx, x)
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

sign(x::AbstractIrrational) = ifelse(x < zero(x), -1.0, 1.0)

-(x::AbstractIrrational) = -Float64(x)
for op in Symbol[:+, :-, :*, :/, :^]
    @eval $op(x::AbstractIrrational, y::AbstractIrrational) = $op(Float64(x),Float64(y))
end
*(x::Bool, y::AbstractIrrational) = ifelse(x, Float64(y), 0.0)

round(x::Irrational, r::RoundingMode) = round(float(x), r)

"""
    @irrational sym [val] def

Define a new `Irrational` value, `sym`, with arbitrary-precision definition in terms
of `BigFloat`s given by the expression `def`.

Optionally provide a pre-computed `Float64` value `val` which must equal `Float64(def)`.
`val` will be computed automatically if omitted.

An `AssertionError` is thrown when either `big(def) isa BigFloat` or `Float64(val) == Float64(def)`
returns `false`.

!!! warning
    This macro should not be used outside of `Base` Julia.

    The macro creates a new type `Irrational{:sym}` regardless of where it's invoked. This can
    lead to conflicting definitions if two packages define an irrational number with the same
    name but different values.


# Examples
```jldoctest
julia> Base.@irrational twoπ 2*big(π)

julia> twoπ
twoπ = 6.2831853071795...

julia> Base.@irrational sqrt2 1.4142135623730950488 √big(2)

julia> sqrt2
sqrt2 = 1.4142135623730...

julia> Base.@irrational sqrt2 1.4142135623730950488 big(2)
ERROR: AssertionError: big($(Expr(:escape, :sqrt2))) isa BigFloat

julia> Base.@irrational sqrt2 1.41421356237309 √big(2)
ERROR: AssertionError: Float64($(Expr(:escape, :sqrt2))) == Float64(big($(Expr(:escape, :sqrt2))))
```
"""
macro irrational(sym, val, def)
    irrational(sym, val, def)
end
macro irrational(sym, def)
    irrational(sym, :(big($(esc(sym)))), def)
end
function irrational(sym, val, def)
    esym = esc(sym)
    qsym = esc(Expr(:quote, sym))
    bigconvert = isa(def,Symbol) ? quote
        function Base.BigFloat(::Irrational{$qsym}, r::MPFR.MPFRRoundingMode=Rounding.rounding_raw(BigFloat); precision=precision(BigFloat))
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
        let v = $val, v64 = Float64(v), v32 = Float32(v)
            Base.Float64(::Irrational{$qsym}) = v64
            Base.Float32(::Irrational{$qsym}) = v32
        end
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
    (length(something(m.captures[1])), length(something(m.captures[2])))
end

# inv
inv(x::AbstractIrrational) = 1/x
