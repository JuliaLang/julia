## types ##

<:(T,S) = subtype(T,S)
>:(T,S) = subtype(S,T)

super(T::Union(StructKind,BitsKind,AbstractKind)) = T.super

## comparison ##

isequal(x, y) = is(x, y)

==(x,y) = isequal(x,y)
!=(x,y) = !(x==y)
==(x::Number, y::Number) = (==)(promote(x,y)...)

# this definition allows Number types to implement == instead of isequal,
# which is more idiomatic.
isequal{T<:Number}(x::T, y::T) = (x==y)

< (x::Real, y::Real) = (<)(promote(x,y)...)
> (x::Real, y::Real) = (y < x)
<=(x::Real, y::Real) = (x < y) || (x == y)
>=(x::Real, y::Real) = (x > y) || (x == y)

## definitions providing basic traits of arithmetic operators ##

+() = 0
*() = 1
&() = error("zero-argument & is ambiguous")
|() = error("zero-argument | is ambiguous")
$() = error("zero-argument \$ is ambiguous")

+(x::Number) = x
*(x::Number) = x
&(x::Int) = x
|(x::Int) = x
$(x::Int) = x

for op = (:+, :*, :&, :|, :$)
    @eval begin
        ($op)(a,b,c) = ($op)(($op)(a,b),c)
        ($op)(a,b,c,d) = ($op)(($op)(($op)(a,b),c),d)
        ($op)(a,b,c,d,e) = ($op)(($op)(($op)(($op)(a,b),c),d),e)
        function ($op)(a, b, c, xs...)
            accum = ($op)(($op)(a,b),c)
            for x = xs
                accum = ($op)(accum,x)
            end
            accum
        end
    end
end

\(x::Number, y::Number) = y/x

# .<op> defaults to <op>
./(x::Number,y::Number) = x/y
.\(x::Number,y::Number) = y./x
.*(x::Number,y::Number) = x*y
.^(x::Number,y::Number) = x^y

div(x::Real, y::Real) = y != 0 ? truncate(x/y)        : throw(DivideByZeroError())
fld(x::Real, y::Real) = y != 0 ? truncate(floor(x/y)) : throw(DivideByZeroError())

rem{T}(x::T, y::T) = convert(T, x-y*div(x,y))
mod{T}(x::T, y::T) = convert(T, x-y*fld(x,y))

rem(x,y) = rem(promote(x,y)...)
mod(x,y) = mod(promote(x,y)...)

%(x,y) = mod(x,y)
mod1(x,y) = (m=mod(x-sign(y),y); m+sign(y))

oftype{T}(x::T,c) = convert(T,c)
oftype{T}(x::Type{T},c) = convert(T,c)

sizeof{T}(x::T) = sizeof(T)
sizeof(t::Type) = error(strcat("size of type ",t," unknown"))

zero(x) = oftype(x,0)
one(x)  = oftype(x,1)

## promotion mechanism ##

promote_type{T}(::Type{T}) = T
promote_type{T}(::Type{T}, ::Type{T}) = T
promote_type(S::Type, T::Type...) = promote_type(S, promote_type(T...))

function promote_type{T,S}(::Type{T}, ::Type{S})
    # print("promote_type: ",T,", ",S,"\n")
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
    # print("promote: ",T,", ",S,"\n")
    #R = promote_type(T,S)
    # print("= ", R,"\n")
    (convert(promote_type(T,S),x), convert(promote_type(T,S),y))
end
function promote{T,S,U}(x::T, y::S, z::U)
    R = promote_type(promote_type(T,S), U)
    convert((R...), (x, y, z))
end
function promote{T,S}(x::T, y::S, zs...)
    R = promote_type(T,S)
    for z = zs
        R = promote_type(R,typeof(z))
    end
    convert((R...), tuple(x,y,zs...))
end

## promotion in arithmetic ##

+(x::Number, y::Number) = +(promote(x,y)...)
*(x::Number, y::Number) = *(promote(x,y)...)
-(x::Number, y::Number) = -(promote(x,y)...)
/(x::Number, y::Number) = /(promote(x,y)...)

# these are defined for the fundamental < and == so that if a method is
# not found for e.g. <=, it is translated to < and == first, then promotion
# is handled after.

## integer-specific promotions ##

div(x::Int, y::Int) = div(promote(x,y)...)
rem(x::Int, y::Int) = rem(promote(x,y)...)

&(x::Int...) = &(promote(x...)...)
|(x::Int...) = |(promote(x...)...)
$(x::Int...) = $(promote(x...)...)

## promotion catch-alls for undefined operations ##

no_op_err(name, T) = error(name," not defined for ",T)
+{T<:Number}(x::T, y::T) = no_op_err("+", T)
*{T<:Number}(x::T, y::T) = no_op_err("*", T)
-{T<:Number}(x::T, y::T) = no_op_err("-", T)
/{T<:Number}(x::T, y::T) = no_op_err("/", T)
<{T<:Real}  (x::T, y::T) = no_op_err("<", T)
=={T<:Number}(x::T, y::T) = no_op_err("==", T)

div{T<:Int}(x::T, y::T) = no_op_err("div", T)
rem{T<:Int}(x::T, y::T) = no_op_err("rem", T)

&{T<:Int}(x::T, y::T) = no_op_err("&", T)
|{T<:Int}(x::T, y::T) = no_op_err("|", T)
${T<:Int}(x::T, y::T) = no_op_err("\$", T)

## miscellaneous ##

copy(x::ANY) = x
foreach(f::Function, itr) = for x = itr; f(x); end
