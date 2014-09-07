## boolean conversions ##

convert(::Type{Bool}, x::Bool) = x
convert(::Type{Bool}, x::Real) = (x!=0)

# promote Bool to any other numeric type
promote_rule{T<:Number}(::Type{Bool}, ::Type{T}) = T

bool(x::Bool) = x
bool(x::Number) = convert(Bool, x)

typemin(::Type{Bool}) = false
typemax(::Type{Bool}) = true

## boolean operations ##

!(x::Bool) = box(Bool,not_int(unbox(Bool,x)))

(~)(x::Bool) = !x
(&)(x::Bool, y::Bool) = box(Bool,and_int(unbox(Bool,x),unbox(Bool,y)))
(|)(x::Bool, y::Bool) = box(Bool,or_int(unbox(Bool,x),unbox(Bool,y)))
($)(x::Bool, y::Bool) = (x!=y)

signbit(x::Bool) = false
sign(x::Bool) = x
abs(x::Bool) = x
abs2(x::Bool) = x

<(x::Bool, y::Bool) = y&!x
<=(x::Bool, y::Bool) = y|!x

## do arithmetic as Int ##

+(x::Bool) =  int(x)
-(x::Bool) = -int(x)

+(x::Bool, y::Bool) = int(x) + int(y)
-(x::Bool, y::Bool) = int(x) - int(y)
*(x::Bool, y::Bool) = x & y
^(x::Bool, y::Bool) = x | !y
^(x::Integer, y::Bool) = ifelse(y, x, one(x))

function +{T<:FloatingPoint}(x::Bool, y::T)
    ifelse(x, one(promote_type(Bool,T)) + convert(promote_type(Bool,T),y),
           convert(promote_type(Bool,T),y))
end
+(y::FloatingPoint, x::Bool) = x + y

function *{T<:Number}(x::Bool, y::T)
    ifelse(x, convert(promote_type(Bool,T),y),
           ifelse(signbit(y), -zero(promote_type(Bool,T)), zero(promote_type(Bool,T))))
end
function *{T<:Unsigned}(x::Bool, y::T)
    ifelse(x, convert(promote_type(Bool,T),y), zero(promote_type(Bool,T)))
end
*(y::Number, x::Bool) = x * y

div(x::Bool, y::Bool) = y ? x : throw(DivideError())
fld(x::Bool, y::Bool) = div(x,y)
cld(x::Bool, y::Bool) = div(x,y)
rem(x::Bool, y::Bool) = y ? false : throw(DivideError())
mod(x::Bool, y::Bool) = rem(x,y)
