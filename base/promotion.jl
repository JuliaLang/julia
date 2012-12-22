## promotion mechanism ##

promote_type{T}(::Type{T}) = T
promote_type(::Type{None}, ::Type{None}) = None
promote_type{T}(::Type{T}, ::Type{T}) = T
promote_type{T}(::Type{T}, ::Type{None}) = T
promote_type{T}(::Type{None}, ::Type{T}) = T
promote_type(S::Type, T::Type...) = promote_type(S, promote_type(T...))

function promote_type{T,S}(::Type{T}, ::Type{S})
    if applicable(promote_rule, T, S)
        return promote_rule(T,S)
    elseif applicable(promote_rule, S, T)
        return promote_rule(S,T)
    else
        error("no promotion exists for ",T," and ",S)
    end
end

promote() = ()
promote(x) = (x,)
function promote{T,S}(x::T, y::S)
    (convert(promote_type(T,S),x), convert(promote_type(T,S),y))
end
function promote{T,S,U}(x::T, y::S, z::U)
    R = promote_type(promote_type(T,S), U)
    convert((R...), (x, y, z))
end
function promote{T,S}(x::T, y::S, zs...)
    R = promote_type(T,S)
    for z in zs
        R = promote_type(R,typeof(z))
    end
    convert((R...), tuple(x,y,zs...))
end
# TODO: promote{T}(x::T, ys::T...) here to catch all circularities?

## promotions in arithmetic, etc. ##

+(x::Number, y::Number) = +(promote(x,y)...)
*(x::Number, y::Number) = *(promote(x,y)...)
-(x::Number, y::Number) = -(promote(x,y)...)
/(x::Number, y::Number) = /(promote(x,y)...)
^(x::Number, y::Number) = ^(promote(x,y)...)

(&)(x::Integer, y::Integer) = (&)(promote(x,y)...)
(|)(x::Integer, y::Integer) = (|)(promote(x,y)...)
($)(x::Integer, y::Integer) = ($)(promote(x,y)...)

==(x::Number, y::Number) = (==)(promote(x,y)...)
< (x::Real, y::Real)     = (< )(promote(x,y)...)
<=(x::Real, y::Real)     = (<=)(promote(x,y)...)

div(x::Real, y::Real) = div(promote(x,y)...)
fld(x::Real, y::Real) = fld(promote(x,y)...)
rem(x::Real, y::Real) = rem(promote(x,y)...)
mod(x::Real, y::Real) = mod(promote(x,y)...)

mod1(x::Real, y::Real) = mod1(promote(x,y)...)
cmp(x::Real, y::Real) = cmp(promote(x,y)...)

max(x::Real, y::Real) = max(promote(x,y)...)
min(x::Real, y::Real) = min(promote(x,y)...)

## catch-alls to prevent infinite recursion when definitions are missing ##

no_op_err(name, T) = error(name," not defined for ",T)
+{T<:Number}(x::T, y::T) = no_op_err("+", T)
*{T<:Number}(x::T, y::T) = no_op_err("*", T)
-{T<:Number}(x::T, y::T) = no_op_err("-", T)
/{T<:Number}(x::T, y::T) = no_op_err("/", T)
^{T<:Number}(x::T, y::T) = no_op_err("^", T)

(&){T<:Integer}(x::T, y::T) = no_op_err("&", T)
(|){T<:Integer}(x::T, y::T) = no_op_err("|", T)
($){T<:Integer}(x::T, y::T) = no_op_err("\$", T)

=={T<:Number}(x::T, y::T) = no_op_err("==", T)
<{T<:Real}(x::T, y::T) = no_op_err("<", T)

max{T<:Real}(x::T, y::T) = y < x ? x : y
min{T<:Real}(x::T, y::T) = x < y ? x : y
