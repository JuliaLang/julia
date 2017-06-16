# This file is a part of Julia. License is MIT: https://julialang.org/license

# Here we implement a minimal dimensionful type Furlong, which is used
# to test dimensional correctness of various functions in Base.  Furlong
# is exported by the TestHelpers module.

# represents a quantity in furlongs^p
struct Furlong{p,T<:Number} <: Number
    val::T
    Furlong{p,T}(v::Number) where {p,T} = new(v)
    Furlong{p,T}(x::Furlong{p}) where {p,T} = new(x.val)
end
Furlong{T<:Number}(x::T) = Furlong{1,T}(x)
(::Type{T}){p,T}(x::Furlong{p,T}) = x.val
(::Type{Furlong{p}}){p}(v::Number) = Furlong{p,typeof(v)}(v)
Base.convert{T,p,S}(::Type{Furlong{p,T}}, x::Furlong{p,S}) = Furlong{p,T}(convert(T,x.val))
Base.convert{T}(::Type{Furlong{0,T}}, x::Furlong{0}) = Furlong{0,T}(convert(T, x.val))
Base.convert{T<:Number}(::Type{T}, x::Furlong{0}) = convert(T, x.val)
Base.convert{T}(::Type{Furlong{0,T}}, x::Number) = Furlong{0,T}(convert(T, x))

Base.promote_type{p,T,S}(::Type{Furlong{p,T}}, ::Type{Furlong{p,S}}) =
    (Base.@_pure_meta; Furlong{p,promote_type(T,S)})

Base.one{p,T}(x::Furlong{p,T}) = one(T)
Base.one{p,T}(::Type{Furlong{p,T}}) = one(T)
Base.zero{p,T}(x::Furlong{p,T}) = Furlong{p,T}(zero(T))
Base.zero{p,T}(::Type{Furlong{p,T}}) = Furlong{p,T}(zero(T))
Base.iszero(x::Furlong) = iszero(x.val)

# convert Furlong exponent p to a canonical form.  This
# is not type stable, but it doesn't matter since it is used
# at compile time (in generated functions), not runtime
canonical_p(p) = isinteger(p) ? Int(p) : Rational{Int}(p)

Base.abs{p}(x::Furlong{p}) = Furlong{p}(abs(x.val))
@generated Base.abs2{p}(x::Furlong{p}) = :(Furlong{$(canonical_p(2p))}(abs2(x.val)))
@generated Base.inv{p}(x::Furlong{p}) = :(Furlong{$(canonical_p(-p))}(inv(x.val)))
Base.sylvester(a::Furlong,b::Furlong,c::Furlong) = -c / (a + b)

for f in (:isfinite, :isnan, :isreal)
    @eval Base.$f(x::Furlong) = $f(x.val)
end
for f in (:real,:imag,:complex,:middle,:+,:-)
    @eval Base.$f{p}(x::Furlong{p}) = Furlong{p}($f(x.val))
end

import Base: +, -, ==, !=, <, <=, isless, isequal, *, /, //, div, rem, mod, ^,
             middle, hypot
for op in (:+, :-, :middle, :hypot)
    @eval function $op{p}(x::Furlong{p}, y::Furlong{p})
        v = $op(x.val, y.val)
        Furlong{p}(v)
    end
end
for op in (:(==), :(!=), :<, :<=, :isless, :isequal)
    @eval $op{p}(x::Furlong{p}, y::Furlong{p}) = $op(x.val, y.val)
end
# generated functions to allow type inference of the value of the exponent:
for (f,op) in ((:_plus,:+),(:_minus,:-),(:_times,:*),(:_div,://))
    @eval @generated function $f{T,p,q}(v::T, ::Furlong{p}, ::Union{Furlong{q},Type{Val{q}}})
        s = $op(p, q)
        :(Furlong{$(canonical_p(s)),$T}(v))
    end
end
for (op,eop) in ((:*, :_plus), (:/, :_minus), (://, :_minus), (:div, :_minus))
    @eval begin
        $op{p,q}(x::Furlong{p}, y::Furlong{q}) =
            $eop($op(x.val, y.val),x,y)
        $op{p,S<:Number}(x::Furlong{p}, y::S) = $op(x,Furlong{0,S}(y))
        $op{p,S<:Number}(x::S, y::Furlong{p}) = $op(Furlong{0,S}(x),y)
    end
end
for op in (:rem, :mod)
    @eval begin
        $op{p}(x::Furlong{p}, y::Furlong) = Furlong{p}($op(x.val, y.val))
        $op{p}(x::Furlong{p}, y::Number) = Furlong{p}($op(x.val, y))
    end
end
Base.sqrt(x::Furlong) = _div(sqrt(x.val), x, Val{2})
