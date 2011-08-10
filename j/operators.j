## types ##

<:(T,S) = subtype(T,S)
>:(T,S) = subtype(S,T)

super(T::Union(CompositeKind,BitsKind,AbstractKind)) = T.super

## comparison ##

isequal(x,y) = is(x,y)
==(x,y) = isequal(x,y)
!=(x,y) = !(x==y)

# this definition allows Number types to implement
# == instead of isequal, which is more idiomatic:
isequal{T<:Number}(x::T, y::T) = (x==y)

> {T<:Real}(x::T, y::T) = (y < x)
<={T<:Real}(x::T, y::T) = (x < y) || (x == y)
>={T<:Real}(x::T, y::T) = (y <= x)

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

# fallback division:
/{T<:Real}(x::T, y::T) = float64(x)/float64(y)

\(x,y) = y/x

# .<op> defaults to <op>
./(x,y) = x/y
.\(x,y) = y./x
.*(x,y) = x*y
.^(x,y) = x^y

# core << >> and >>> takes Int32 as second arg
<<(x,y::Int)  = x << int32(y)
>>(x,y::Int)  = x >> int32(y)
>>>(x,y::Int) = x >>> int32(y)

# fallback div, fld, rem & mod implementations
div{T<:Real}(x::T, y::T) = convert(T,trunc(x/y))
fld{T<:Real}(x::T, y::T) = convert(T,floor(x/y))
rem{T<:Real}(x::T, y::T) = convert(T,x-y*div(x,y))
mod{T<:Real}(x::T, y::T) = convert(T,x-y*fld(x,y))

# operator alias
% = mod

# mod returns in [0,y) whereas mod1 returns in (0,y]
mod1{T<:Real}(x::T, y::T) = y-mod(y-x,y)

# cmp returns -1, 0, +1 indicating ordering
cmp{T<:Real}(x::T, y::T) = sign(y-x)

oftype{T}(::Type{T},c) = convert(T,c)
oftype{T}(x::T,c) = convert(T,c)

zero(x) = oftype(x,0)
one(x)  = oftype(x,1)

sizeof(T::Type) = error(strcat("size of type ",T," unknown"))
sizeof{T}(x::T) = sizeof(T)

copy(x::ANY) = x
foreach(f::Function, itr) = for x = itr; f(x); end

# function composition
one(f::Function) = identity
one(::Type{Function}) = identity
*(f::Function, g::Function) = x->f(g(x))

# vectorization

macro vectorize_1arg(S,f)
    quote
        function ($f){T<:$S}(x::AbstractArray{T,1})
            [ ($f)(x[i]) | i=1:length(x) ]
        end
        function ($f){T<:$S}(x::AbstractArray{T,2})
            [ ($f)(x[i,j]) | i=1:size(x,1), j=1:size(x,2) ]
        end
    end
end

macro vectorize_2arg(S,f)
    quote
        function ($f){T<:$S}(x::T, y::AbstractArray{T,1})
            [ ($f)(x,y[i]) | i=1:length(y) ]
        end
        function ($f){T<:$S}(x::AbstractArray{T,1}, y::T)
            [ ($f)(x[i],y) | i=1:length(x) ]
        end
        function ($f){T<:$S}(x::T, y::AbstractArray{T,2})
            [ ($f)(x,y[i,j]) | i=1:size(y,1), j=1:size(y,2) ]
        end
        function ($f){T<:$S}(x::AbstractArray{T,2}, y::T)
            [ ($f)(x[i,j],y) | i=1:size(x,1), j=1:size(x,2) ]
        end
        function ($f){T<:$S}(x::AbstractArray{T,1}, y::AbstractArray{T,1})
            if size(x) != size(y)
                error("vector length mismatch")
            end
            [ ($f)(x[i],y[i]) | i=1:length(x) ]
        end
        function ($f){T<:$S}(x::AbstractArray{T,2}, y::AbstractArray{T,2})
            if size(x) != size(y)
                error("matrix dimension mismatch")
            end
            [ ($f)(x[i,j],y[i,j]) | i=1:size(x,1), j=1:size(x,2) ]
        end
    end
end
