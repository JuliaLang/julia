## types ##

const (<:) = subtype

super(T::Union(CompositeKind,BitsKind,AbstractKind)) = T.super

## comparison ##

isequal(x,y) = is(x,y)
==(x,y) = isequal(x,y)
!=(x,y) = !(x==y)

< (x,y) = isless(x,y)
> (x,y) = y < x
<=(x,y) = !(y < x)
>=(x,y) = (y <= x)

# these definitions allow Number types to implement
# == and < instead of isequal and isless, which is more idiomatic:
isequal(x::Number, y::Number) = x==y
isless(x::Real, y::Real) = x<y

max(x,y) = y < x ? x : y
min(x,y) = x < y ? x : y

## definitions providing basic traits of arithmetic operators ##

+() = 0
*() = 1
(&)() = error("zero-argument & is ambiguous")
(|)() = error("zero-argument | is ambiguous")
($)() = error("zero-argument \$ is ambiguous")

+(x::Number) = x
*(x::Number) = x
(&)(x::Integer) = x
(|)(x::Integer) = x
($)(x::Integer) = x

for op = (:+, :*, :&, :|, :$, :min, :max)
    @eval begin
        ($op)(a,b,c) = ($op)(($op)(a,b),c)
        ($op)(a,b,c,d) = ($op)(($op)(($op)(a,b),c),d)
        ($op)(a,b,c,d,e) = ($op)(($op)(($op)(($op)(a,b),c),d),e)
        function ($op)(a, b, c, xs...)
            accum = ($op)(($op)(a,b),c)
            for x in xs
                accum = ($op)(accum,x)
            end
            accum
        end
    end
end

\(x,y) = y/x

# .<op> defaults to <op>
./(x,y) = x/y
.\(x,y) = y./x
.*(x,y) = x*y
.^(x,y) = x^y

# core << >> and >>> takes Int32 as second arg
<<(x,y::Integer)  = x << convert(Int32,y)
<<(x,y::Int32)    = no_op_err("<<", typeof(x))
>>(x,y::Integer)  = x >> convert(Int32,y)
>>(x,y::Int32)    = no_op_err(">>", typeof(x))
>>>(x,y::Integer) = x >>> convert(Int32,y)
>>>(x,y::Int32)   = no_op_err(">>>", typeof(x))

# fallback div, fld, rem & mod implementations
div{T<:Real}(x::T, y::T) = convert(T,trunc(x/y))
fld{T<:Real}(x::T, y::T) = convert(T,floor(x/y))
rem{T<:Real}(x::T, y::T) = convert(T,x-y*div(x,y))
mod{T<:Real}(x::T, y::T) = convert(T,x-y*fld(x,y))

# operator alias
const % = mod

# mod returns in [0,y) whereas mod1 returns in (0,y]
mod1{T<:Real}(x::T, y::T) = y-mod(y-x,y)

# cmp returns -1, 0, +1 indicating ordering
cmp{T<:Real}(x::T, y::T) = int(sign(x-y))

# transposed multiply
aCb (a,b) = ctranspose(a)*b
abC (a,b) = a*ctranspose(b)
aCbC(a,b) = ctranspose(a)*ctranspose(b)
aTb (a,b) = transpose(a)*b
abT (a,b) = a*transpose(b)
aTbT(a,b) = transpose(a)*transpose(b)

oftype{T}(::Type{T},c) = convert(T,c)
oftype{T}(x::T,c) = convert(T,c)

zero(x) = oftype(x,0)
one(x)  = oftype(x,1)

sizeof(T::Type) = error(strcat("size of type ",T," unknown"))
sizeof(T::BitsKind) = div(T.nbits,8)
sizeof{T}(x::T) = sizeof(T)

copy(x::ANY) = x
foreach(f::Function, itr) = for x = itr; f(x); end

# function composition
one(f::Function) = identity
one(::Type{Function}) = identity
*(f::Function, g::Function) = x->f(g(x))

# vectorization

function promote_shape(a::(Int,), b::(Int,))
    if a[1] != b[1]
        error("argument dimensions must match")
    end
    return a
end

function promote_shape(a::(Int,Int), b::(Int,))
    if a[1] != b[1] || a[2] != 1
        error("argument dimensions must match")
    end
    return a
end

promote_shape(a::(Int,), b::(Int,Int)) = promote_shape(b, a)

function promote_shape(a::Dims, b::Dims)
    if length(a) < length(b)
        return promote_shape(b, a)
    end
    for i=1:length(b)
        if a[i] != b[i]
            error("argument dimensions must match")
        end
    end
    for i=length(b)+1:length(a)
        if a[i] != 1
            error("argument dimensions must match")
        end
    end
    return a
end

macro vectorize_1arg(S,f)
    quote
        function ($f){T<:$S}(x::AbstractArray{T,1})
            [ ($f)(x[i]) for i=1:length(x) ]
        end
        function ($f){T<:$S}(x::AbstractArray{T,2})
            [ ($f)(x[i,j]) for i=1:size(x,1), j=1:size(x,2) ]
        end
        function ($f){T<:$S}(x::AbstractArray{T})
            reshape([ ($f)(x[i]) for i=1:numel(x) ], size(x))
        end
    end
end

macro vectorize_2arg(S,f)
    quote
        function ($f){T1<:$S, T2<:$S}(x::T1, y::AbstractArray{T2})
            reshape([ ($f)(x, y[i]) for i=1:numel(y) ], size(y))
        end
        function ($f){T1<:$S, T2<:$S}(x::AbstractArray{T1}, y::T2)
            reshape([ ($f)(x[i], y) for i=1:numel(x) ], size(x))
        end

        function ($f){T1<:$S, T2<:$S}(x::AbstractArray{T1}, y::AbstractArray{T2})
            shp = promote_shape(size(x),size(y))
            reshape([ ($f)(x[i], y[i]) for i=1:numel(x) ], shp)
        end
    end
end
