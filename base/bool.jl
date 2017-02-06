# This file is a part of Julia. License is MIT: http://julialang.org/license

## boolean conversions ##

convert(::Type{Bool}, x::Bool) = x
convert(::Type{Bool}, x::Float16) = x==0 ? false : x==1 ? true : throw(InexactError())
convert(::Type{Bool}, x::Real) = x==0 ? false : x==1 ? true : throw(InexactError())

# promote Bool to any other numeric type
promote_rule{T<:Number}(::Type{Bool}, ::Type{T}) = T

typemin(::Type{Bool}) = false
typemax(::Type{Bool}) = true

## boolean operations ##

"""
    !(x)

Boolean not.

```jldoctest
julia> !true
false

julia> !false
true

julia> ![true false true]
1×3 BitArray{2}:
 false  true  false
```
"""
function !(x::Bool)
    ## We need a better heuristic to detect this automatically
    @_pure_meta
    return not_int(x)
end

(~)(x::Bool) = !x
(&)(x::Bool, y::Bool) = and_int(x, y)
(|)(x::Bool, y::Bool) = or_int(x, y)

"""
    xor(x, y)
    ⊻(x, y)

Bitwise exclusive or of `x` and `y`.  The infix operation
`a ⊻ b` is a synonym for `xor(a,b)`, and
`⊻` can be typed by tab-completing `\\xor`
or `\\veebar` in the Julia REPL.

```jldoctest
julia> [true; true; false] .⊻ [true; false; false]
3-element BitArray{1}:
 false
  true
 false
```
"""
xor(x::Bool, y::Bool) = (x != y)

>>(x::Bool, c::Unsigned) = Int(x) >> c
<<(x::Bool, c::Unsigned) = Int(x) << c
>>>(x::Bool, c::Unsigned) = Int(x) >>> c

>>(x::Bool, c::Int) = Int(x) >> c
<<(x::Bool, c::Int) = Int(x) << c
>>>(x::Bool, c::Int) = Int(x) >>> c

>>(x::Bool, c::Integer) = Int(x) >> c
<<(x::Bool, c::Integer) = Int(x) << c
>>>(x::Bool, c::Integer) = Int(x) >>> c

signbit(x::Bool) = false
sign(x::Bool) = x
abs(x::Bool) = x
abs2(x::Bool) = x
iszero(x::Bool) = !x

<(x::Bool, y::Bool) = y&!x
<=(x::Bool, y::Bool) = y|!x

## do arithmetic as Int ##

+(x::Bool) =  Int(x)
-(x::Bool) = -Int(x)

+(x::Bool, y::Bool) = Int(x) + Int(y)
-(x::Bool, y::Bool) = Int(x) - Int(y)
*(x::Bool, y::Bool) = x & y
^(x::Bool, y::Bool) = x | !y
^(x::Integer, y::Bool) = ifelse(y, x, one(x))

function +{T<:AbstractFloat}(x::Bool, y::T)::promote_type(Bool,T)
    return ifelse(x, oneunit(y) + y, y)
end
+(y::AbstractFloat, x::Bool) = x + y

function *{T<:Number}(x::Bool, y::T)::promote_type(Bool,T)
    return ifelse(x, y, copysign(zero(y), y))
end
function *{T<:Unsigned}(x::Bool, y::T)::promote_type(Bool,T)
    return ifelse(x, y, zero(y))
end
*(y::Number, x::Bool) = x * y

div(x::Bool, y::Bool) = y ? x : throw(DivideError())
fld(x::Bool, y::Bool) = div(x,y)
cld(x::Bool, y::Bool) = div(x,y)
rem(x::Bool, y::Bool) = y ? false : throw(DivideError())
mod(x::Bool, y::Bool) = rem(x,y)
