## types ##

const (<:) = subtype

super(T::DataType) = T.super

## comparison ##

isequal(x,y) = is(x,y)
==(x,y) = isequal(x,y)
!=(x,y) = !(x==y)

< (x,y) = isless(x,y)
> (x,y) = y < x
<=(x,y) = !(y < x)
>=(x,y) = (y <= x)
.> (x,y) = y.<x
.>=(x,y) = y.<=x

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

\(x::Number,y::Number) = y/x

# .<op> defaults to <op>
./(x::Number,y::Number) = x/y
.\(x::Number,y::Number) = y./x
.*(x::Number,y::Number) = x*y
.^(x::Number,y::Number) = x^y
.+(x,y) = x+y
.-(x,y) = x-y

.==(x::Number,y::Number) = x==y
.!=(x::Number,y::Number) = x!=y
.< (x::Real,y::Real) = x<y
.<=(x::Real,y::Real) = x<=y

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
const % = rem

# mod returns in [0,y) whereas mod1 returns in (0,y]
mod1{T<:Real}(x::T, y::T) = y-mod(y-x,y)

# cmp returns -1, 0, +1 indicating ordering
cmp{T<:Real}(x::T, y::T) = int(sign(x-y))

# transposed multiply
Ac_mul_B (a,b) = ctranspose(a)*b
A_mul_Bc (a,b) = a*ctranspose(b)
Ac_mul_Bc(a,b) = ctranspose(a)*ctranspose(b)
At_mul_B (a,b) = transpose(a)*b
A_mul_Bt (a,b) = a*transpose(b)
At_mul_Bt(a,b) = transpose(a)*transpose(b)

# transposed divide
Ac_rdiv_B (a,b) = ctranspose(a)/b
A_rdiv_Bc (a,b) = a/ctranspose(b)
Ac_rdiv_Bc(a,b) = ctranspose(a)/ctranspose(b)
At_rdiv_B (a,b) = transpose(a)/b
A_rdiv_Bt (a,b) = a/transpose(b)
At_rdiv_Bt(a,b) = transpose(a)/transpose(b)

Ac_ldiv_B (a,b) = ctranspose(a)\b
A_ldiv_Bc (a,b) = a\ctranspose(b)
Ac_ldiv_Bc(a,b) = ctranspose(a)\ctranspose(b)
At_ldiv_B (a,b) = transpose(a)\b
A_ldiv_Bt (a,b) = a\transpose(b)
At_ldiv_Bt(a,b) = transpose(a)\transpose(b)


oftype{T}(::Type{T},c) = convert(T,c)
oftype{T}(x::T,c) = convert(T,c)

zero(x) = oftype(x,0)
one(x)  = oftype(x,1)

sizeof(T::Type) = error(string("size of type ",T," unknown"))
sizeof(T::DataType) = if isleaftype(T) T.size else error("type does not have a native size") end
sizeof(x) = sizeof(typeof(x))

# copying immutable things
copy(x::Union(Symbol,Number,String,Function,Tuple,LambdaStaticData,
              TopNode,QuoteNode,DataType,UnionType)) = x

# function pipelining
|(x, f::Function) = f(x)

# array shape rules

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

# shape of array to create for getindex() with indexes I
function index_shape(I...)
    n = length(I)
    while n > 0 && isa(I[n],Real); n-=1; end
    tuple([length(I[i]) for i=1:n]...)
end

index_shape(i::Real) = ()
index_shape(i)       = (length(i),)
index_shape(i::Real,j::Real) = ()
index_shape(i      ,j::Real) = (length(i),)
index_shape(i      ,j)       = (length(i),length(j))
index_shape(i::Real,j::Real,k::Real) = ()
index_shape(i      ,j::Real,k::Real) = (length(i),)
index_shape(i      ,j      ,k::Real) = (length(i),length(j))
index_shape(i      ,j      ,k      ) = (length(i),length(j),length(k))

# check for valid sizes in A[I...] = X where X <: AbstractArray
function setindex_shape_check(X::AbstractArray, I...)
    nel = 1
    for idx in I
        nel *= length(idx)
    end
    if length(X) != nel
        error("argument dimensions must match")
    end
    if ndims(X) > 1
        for i = 1:length(I)
            if size(X,i) != length(I[i])
                error("argument dimensions must match")
            end
        end
    end
end

# convert to integer index
to_index(i)       = i
to_index(i::Real) = convert(Int, i)
to_index(i::Int)  = i

# vectorization

macro vectorize_1arg(S,f)
    S = esc(S); f = esc(f); T = esc(:T)
    quote
        ($f){$T<:$S}(x::AbstractArray{$T,1}) = [ ($f)(x[i]) for i=1:length(x) ]
        ($f){$T<:$S}(x::AbstractArray{$T,2}) =
            [ ($f)(x[i,j]) for i=1:size(x,1), j=1:size(x,2) ]
        ($f){$T<:$S}(x::AbstractArray{$T}) =
            reshape([ ($f)(x[i]) for i=1:length(x) ], size(x))
    end
end

macro vectorize_2arg(S,f)
    S = esc(S); f = esc(f); T1 = esc(:T1); T2 = esc(:T2)
    quote
        ($f){$T1<:$S, $T2<:$S}(x::($T1), y::AbstractArray{$T2}) =
            reshape([ ($f)(x, y[i]) for i=1:length(y) ], size(y))
        ($f){$T1<:$S, $T2<:$S}(x::AbstractArray{$T1}, y::($T2)) =
            reshape([ ($f)(x[i], y) for i=1:length(x) ], size(x))

        function ($f){$T1<:$S, $T2<:$S}(x::AbstractArray{$T1}, y::AbstractArray{$T2})
            shp = promote_shape(size(x),size(y))
            reshape([ ($f)(x[i], y[i]) for i=1:length(x) ], shp)
        end
    end
end

# some operators not defined yet
global //, .>>, .<<, &>, &>>, &<, &<<

module Operators

export
    !,
    !=,
    $,
    %,
    &,
    *,
    +,
    -,
    .!=,
    .+,
    .-,
    .*,
    ./,
    .<,
    .<=,
    .==,
    .>,
    .>=,
    .\,
    .^,
    /,
    //,
    <,
    <:,
    <<,
    <=,
    ==,
    >,
    >=,
    >>,
    .>>,
    .<<,
    >>>,
    &>,
    &>>,
    &<,
    &<<,
    \,
    ^,
    |,
    ~

import
    Base.!, Base.!=, Base.$, Base.%, Base.&, Base.*, Base.+, Base.-, Base..!=,
    Base..+, Base..-, Base..*, Base../, Base..<, Base..<=, Base..==, Base..>,
    Base..>=, Base..\, Base..^, Base./, Base.//, Base.<, Base.<:, Base.<<,
    Base.<=, Base.==, Base.>, Base.>=, Base.>>, Base..>>, Base..<<, Base.>>>,
    Base.&>, Base.&>>, Base.&<, Base.&<<, Base.\, Base.^, Base.|, Base.~

end
