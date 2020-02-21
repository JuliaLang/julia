# This file is a part of Julia. License is MIT: https://julialang.org/license

module Furlongs

export Furlong

# Here we implement a minimal dimensionful type Furlong, which is used
# to test dimensional correctness of various functions in Base.

# represents a quantity in furlongs^p
struct Furlong{p,T<:Number} <: Number
    val::T
    Furlong{p,T}(v::Number) where {p,T} = new(v)
    Furlong{p,T}(x::Furlong{p}) where {p,T} = new(x.val)
end
Furlong(x::T) where {T<:Number} = Furlong{1,T}(x)
(::Type{T})(x::Furlong) where {T<:Number} = T(x.val)
Furlong{p}(v::Number) where {p} = Furlong{p,typeof(v)}(v)
Furlong{p,T}(x::Furlong{p,S}) where {T,p,S} = Furlong{p,T}(T(x.val))

Base.promote_type(::Type{Furlong{p,T}}, ::Type{Furlong{p,S}}) where {p,T,S} =
    (Base.@_pure_meta; Furlong{p,promote_type(T,S)})

Base.one(x::Furlong{p,T}) where {p,T} = one(T)
Base.one(::Type{Furlong{p,T}}) where {p,T} = one(T)
Base.oneunit(x::Furlong{p,T}) where {p,T} = Furlong{p,T}(one(T))
Base.oneunit(x::Type{Furlong{p,T}}) where {p,T} = Furlong{p,T}(one(T))
Base.zero(x::Furlong{p,T}) where {p,T} = Furlong{p,T}(zero(T))
Base.zero(::Type{Furlong{p,T}}) where {p,T} = Furlong{p,T}(zero(T))
Base.iszero(x::Furlong) = iszero(x.val)

# convert Furlong exponent p to a canonical form.  This
# is not type stable, but it doesn't matter since it is used
# at compile time (in generated functions), not runtime
canonical_p(p) = isinteger(p) ? Int(p) : Rational{Int}(p)

Base.abs(x::Furlong{p}) where {p} = Furlong{p}(abs(x.val))
@generated Base.abs2(x::Furlong{p}) where {p} = :(Furlong{$(canonical_p(2p))}(abs2(x.val)))
@generated Base.inv(x::Furlong{p}) where {p} = :(Furlong{$(canonical_p(-p))}(inv(x.val)))

for f in (:isfinite, :isnan, :isreal, :isinf)
    @eval Base.$f(x::Furlong) = $f(x.val)
end
for f in (:real,:imag,:complex,:+,:-)
    @eval Base.$f(x::Furlong{p}) where {p} = Furlong{p}($f(x.val))
end

import Base: +, -, ==, !=, <, <=, isless, isequal, *, /, //, div, rem, mod, ^
for op in (:+, :-)
    @eval function $op(x::Furlong{p}, y::Furlong{p}) where {p}
        v = $op(x.val, y.val)
        Furlong{p}(v)
    end
end
for op in (:(==), :(!=), :<, :<=, :isless, :isequal)
    @eval $op(x::Furlong{p}, y::Furlong{p}) where {p} = $op(x.val, y.val)
end
# generated functions to allow type inference of the value of the exponent:
for (f,op) in ((:_plus,:+),(:_minus,:-),(:_times,:*),(:_div,://))
    @eval @generated function $f(v::T, ::Furlong{p}, ::Union{Furlong{q},Val{q}}) where {T,p,q}
        s = $op(p, q)
        :(Furlong{$(canonical_p(s)),$T}(v))
    end
end
for (op,eop) in ((:*, :_plus), (:/, :_minus), (://, :_minus), (:div, :_minus))
    @eval begin
        $op(x::Furlong{p}, y::Furlong{q}) where {p,q} =
            $eop($op(x.val, y.val),x,y)
        $op(x::Furlong{p}, y::S) where {p,S<:Number} = $op(x,Furlong{0,S}(y))
        $op(x::S, y::Furlong{p}) where {p,S<:Number} = $op(Furlong{0,S}(x),y)
    end
end
for op in (:rem, :mod)
    @eval begin
        $op(x::Furlong{p}, y::Furlong) where {p} = Furlong{p}($op(x.val, y.val))
        $op(x::Furlong{p}, y::Number) where {p} = Furlong{p}($op(x.val, y))
    end
end
Base.sqrt(x::Furlong) = _div(sqrt(x.val), x, Val(2))

end
