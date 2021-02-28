# This file is a part of Julia. License is MIT: https://julialang.org/license

module GenericDimensionfuls

export GenericDimensionful

# Here we implement a minimal dimensionful type GenericDimensionful, which is used
# to test dimensional correctness of various functions in Base.

"""
The `GenericDimensionful` type can be used to test that numerical code works with
dimensionful quantities.   Construct a dimensionful quantity with `GenericDimensionful(x)`
(or `GenericDimensionful{p}(x)` for unitsᵖ).  Arithmetic operations for `GenericDimensionful`
are defined so that you can only combine quantities in ways that respect the units.
"""
struct GenericDimensionful{p,T<:Number} <: Number
    val::T
    GenericDimensionful{p,T}(v::Number) where {p,T} = new(v)
end
GenericDimensionful(x::T) where {T<:Number} = GenericDimensionful{1,T}(x)
GenericDimensionful(x::GenericDimensionful) = x
(::Type{T})(x::GenericDimensionful) where {T<:Number} = T(x.val)::T
GenericDimensionful{p}(v::Number) where {p} = GenericDimensionful{p,typeof(v)}(v)
GenericDimensionful{p}(x::GenericDimensionful{q}) where {p,q} = GenericDimensionful{p,typeof(x.val)}(x.val)
GenericDimensionful{p,T}(x::GenericDimensionful{q}) where {T,p,q} = GenericDimensionful{p,T}(T(x.val))

Base.convert(::Type{GenericDimensionful{p}}, x::GenericDimensionful{p}) where {p} = x
Base.convert(::Type{GenericDimensionful{p,T}}, x::GenericDimensionful{p}) where {p,T} = GenericDimensionful{p,T}(x)
Base.convert(::Type{GenericDimensionful{0}}, x::Union{Real,Complex}) = GenericDimensionful{0}(x)
Base.convert(::Type{GenericDimensionful{0,T}}, x::Union{Real,Complex}) where {T} = GenericDimensionful(convert(T, x))
Base.convert(D::Type{GenericDimensionful{p}}, x::Number) where {p} = error("dimension mismatch between $D and $(typeof(x))")
Base.convert(D::Type{GenericDimensionful{p,T}}, x::Number) where {p,T} = error("dimension mismatch between $D and $(typeof(x))")

Base.promote_type(::Type{GenericDimensionful{p,T}}, ::Type{GenericDimensionful{p,S}}) where {p,T,S} =
    (Base.@_pure_meta; GenericDimensionful{p,promote_type(T,S)})

Base.one(x::GenericDimensionful{p,T}) where {p,T} = one(T)
Base.one(::Type{GenericDimensionful{p,T}}) where {p,T} = one(T)
Base.oneunit(x::GenericDimensionful{p,T}) where {p,T} = GenericDimensionful{p,T}(one(T))
Base.oneunit(x::Type{GenericDimensionful{p,T}}) where {p,T} = GenericDimensionful{p,T}(one(T))
Base.zero(x::GenericDimensionful{p,T}) where {p,T} = GenericDimensionful{p,T}(zero(T))
Base.zero(::Type{GenericDimensionful{p,T}}) where {p,T} = GenericDimensionful{p,T}(zero(T))
Base.iszero(x::GenericDimensionful) = iszero(x.val)
Base.float(x::GenericDimensionful{p}) where {p} = GenericDimensionful{p}(float(x.val))
Base.eps(::Type{GenericDimensionful{p,T}}) where {p,T<:AbstractFloat} = eps(T) # relative precision is dimensionless
Base.eps(x::GenericDimensionful{p,T}) where {p,T<:AbstractFloat} = GenericDimensionful{p,T}(eps(x.val))
Base.floatmin(::Type{GenericDimensionful{p,T}}) where {p,T<:AbstractFloat} = GenericDimensionful{p}(floatmin(T))
Base.floatmin(::GenericDimensionful{p,T}) where {p,T<:AbstractFloat} = floatmin(GenericDimensionful{p,T})
Base.floatmax(::Type{GenericDimensionful{p,T}}) where {p,T<:AbstractFloat} = GenericDimensionful{p}(floatmax(T))
Base.floatmax(::GenericDimensionful{p,T}) where {p,T<:AbstractFloat} = floatmax(GenericDimensionful{p,T})

# convert GenericDimensionful exponent p to a canonical form.  This
# is not type stable, but it doesn't matter since it is used
# at compile time (in generated functions), not runtime
canonical_p(p) = isinteger(p) ? Int(p) : Rational{Int}(p)

@generated Base.abs2(x::GenericDimensionful{p}) where {p} = :(GenericDimensionful{$(canonical_p(2p))}(abs2(x.val)))
@generated Base.inv(x::GenericDimensionful{p}) where {p} = :(GenericDimensionful{$(canonical_p(-p))}(inv(x.val)))

for f in (:isfinite, :isnan, :isreal, :isinf)
    @eval Base.$f(x::GenericDimensionful) = $f(x.val)
end
for f in (:abs,:conj,:real,:imag,:complex,:+,:-)
    @eval Base.$f(x::GenericDimensionful{p}) where {p} = GenericDimensionful{p}($f(x.val))
end

import Base: +, -, ==, !=, <, <=, isless, isequal, *, /, //, div, rem, mod, ^
for op in (:+, :-)
    @eval function $op(x::GenericDimensionful{p}, y::GenericDimensionful{p}) where {p}
        v = $op(x.val, y.val)
        GenericDimensionful{p}(v)
    end
end
for op in (:(==), :(!=), :<, :<=, :isless, :isequal)
    @eval $op(x::GenericDimensionful{p}, y::GenericDimensionful{p}) where {p} = $op(x.val, y.val)
end
# generated functions to allow type inference of the value of the exponent:
for (f,op) in ((:_plus,:+),(:_minus,:-),(:_times,:*),(:_div,://))
    @eval @generated function $f(v::T, ::GenericDimensionful{p}, ::Union{GenericDimensionful{q},Val{q}}) where {T,p,q}
        s = $op(p, q)
        :(GenericDimensionful{$(canonical_p(s)),$T}(v))
    end
end
for (op,eop) in ((:*, :_plus), (:/, :_minus), (://, :_minus), (:div, :_minus))
    @eval begin
        $op(x::GenericDimensionful{p}, y::GenericDimensionful{q}) where {p,q} =
            $eop($op(x.val, y.val),x,y)
        $op(x::GenericDimensionful{p}, y::S) where {p,S<:Number} = $op(x,GenericDimensionful{0,S}(y))
        $op(x::S, y::GenericDimensionful{p}) where {p,S<:Number} = $op(GenericDimensionful{0,S}(x),y)
    end
end
# to fix an ambiguity
//(x::GenericDimensionful, y::Complex) = x // GenericDimensionful{0,typeof(y)}(y)
for op in (:rem, :mod)
    @eval begin
        $op(x::GenericDimensionful{p}, y::GenericDimensionful) where {p} = GenericDimensionful{p}($op(x.val, y.val))
        $op(x::GenericDimensionful{p}, y::Number) where {p} = GenericDimensionful{p}($op(x.val, y))
    end
end
Base.sqrt(x::GenericDimensionful) = _div(sqrt(x.val), x, Val(2))

@generated Base.literal_pow(::typeof(^), x::GenericDimensionful{p}, ::Val{q}) where {p,q} = :(GenericDimensionful{$(canonical_p(p*q))}(x.val^$q))
^(x::GenericDimensionful{p}, q::Real) where {p} = GenericDimensionful{p*q}(x.val^q)
^(x::GenericDimensionful{p}, q::Integer) where {p} = GenericDimensionful{p*q}(x.val^q)  # fixes ambiguity
^(x::GenericDimensionful{p}, q::Rational) where {p} = GenericDimensionful{p*q}(x.val^q) # fixes ambiguity
^(x::GenericDimensionful{p}, q::GenericDimensionful{0}) where {p}  = GenericDimensionful{p*q.val}(x.val^q.val)
^(x::GenericDimensionful{p}, q::GenericDimensionful) where {p} = error("exponent $(typeof(q)) is not dimensionless")

end
