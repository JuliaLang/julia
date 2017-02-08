# Tests to make sure that Julia functions work with types representing
# dimensionful quantities, like in the Unitful.jl package.  Here
# we implement a minimal dimensionful type and use it to test various
# functions in Base.

using Base.Test

# represents a quantity in furlongs^p
immutable Furlong{p,T<:Number} <: Number
    val::T
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

Base.abs{p,T}(x::Furlong{p,T}) = Furlong{p,T}(abs(x.val))

for f in (:isfinite, :isnan, :isreal)
    @eval Base.$f(x::Furlong) = $f(x.val)
end
for f in (:real,:imag,:complex)
    @eval Base.$f{p,T}(x::Furlong{p,T}) = Furlong{p}($f(x.val))
end

import Base: +, -, ==, !=, <, <=, isless, isequal, *, /, //, div, rem, mod, ^
for op in (:+, :-)
    @eval function $op{T,p,S}(x::Furlong{p,T}, y::Furlong{p,S})
        v = $op(x.val, y.val)
        Furlong{p}(v)
    end
end
for op in (:(==), :(!=), :<, :<=, :isless, :isequal)
    @eval $op{T,p,S}(x::Furlong{p,T}, y::Furlong{p,S}) = $op(x.val, y.val)
end
# generated functions to allow type inference of the value of the exponent:
for (f,op) in ((:_plus,:+),(:_minus,:-),(:_times,:*),(:_div,://))
    @eval @generated function $f{T,p,q}(v::T, ::Furlong{p}, ::Union{Furlong{q},Type{Val{q}}})
        s = $op(p, q)
        :(Furlong{$s,$T}(v))
    end
end
for (op,eop) in ((:*, :_plus), (:/, :_minus), (://, :_minus), (:div, :_minus))
    @eval begin
        $op{T,p,q,S}(x::Furlong{p,T}, y::Furlong{q,S}) =
            $eop($op(x.val, y.val),x,y)
        $op{p,T,S<:Number}(x::Furlong{p,T}, y::S) = $op(x,Furlong{0,S}(y))
        $op{p,T,S<:Number}(x::S, y::Furlong{p,T}) = $op(Furlong{0,S}(x),y)
    end
end
for op in (:rem, :mod)
    @eval begin
        $op{p,T}(x::Furlong{p,T}, y::Furlong) = Furlong{p}($op(x.val, y.val))
        $op{p,T}(x::Furlong{p,T}, y::Number) = Furlong{p}($op(x.val, y))
    end
end
Base.sqrt{p,T}(x::Furlong{p,T}) = _div(sqrt(x.val), x, Val{2})

@test collect(Furlong(2):Furlong(10)) == collect(Furlong(2):Furlong(1):Furlong(10)) == Furlong.(2:10)
@test collect(Furlong(1.0):Furlong(0.5):Furlong(10.0)) == Furlong.(1:0.5:10)
