## types ##

const (<:) = issubtype

super(T::DataType) = T.super

# avoid ambiguity with isequal(::Tuple, ::Tuple)
isequal(T::(Type...), S::(Type...)) = typeseq(T, S)
isequal(T::Type, S::Type) = typeseq(T, S)

## comparison ##

isequal(x,y) = is(x,y)
==(x,y) = isequal(x,y)
!=(x,y) = !(x==y)
!==(x,y) = !is(x,y)

< (x,y) = isless(x,y)
> (x,y) = y < x
<=(x,y) = !(y < x)
>=(x,y) = (y <= x)
.> (x,y) = y.<x
.>=(x,y) = y.<=x

# this definition allows Number types to implement < instead of isless,
# which is more idiomatic:
isless(x::Real, y::Real) = x<y

ifelse(c::Bool, x, y) = Intrinsics.select_value(c, x, y)

cmp(x,y) = isless(x,y) ? -1 : isless(y,x) ? 1 : 0
lexcmp(x,y) = cmp(x,y)
lexless(x,y) = lexcmp(x,y)<0

max(x,y) = ifelse(y < x, x, y)
min(x,y) = ifelse(x < y, x, y)

scalarmax(x,y) = max(x,y)
scalarmax(x::AbstractArray, y::AbstractArray) = error("max: ordering is not well-defined for arrays")
scalarmax(x               , y::AbstractArray) = error("max: ordering is not well-defined for arrays")
scalarmax(x::AbstractArray, y               ) = error("max: ordering is not well-defined for arrays")

scalarmin(x,y) = min(x,y)
scalarmin(x::AbstractArray, y::AbstractArray) = error("min: ordering is not well-defined for arrays")
scalarmin(x               , y::AbstractArray) = error("min: ordering is not well-defined for arrays")
scalarmin(x::AbstractArray, y               ) = error("min: ordering is not well-defined for arrays")

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
        # note: these definitions must not cause a dispatch loop when +(a,b) is
        # not defined, and must only try to call 2-argument definitions, so
        # that defining +(a,b) is sufficient for full functionality.
        ($op)(a, b, c)        = ($op)(($op)(a,b),c)
        ($op)(a, b, c, xs...) = ($op)(($op)(($op)(a,b),c), xs...)
        # a further concern is that it's easy for a type like (Int,Int...)
        # to match many definitions, so we need to keep the number of
        # definitions down to avoid losing type information.
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

# fallback div and fld implementations
# NOTE: C89 fmod() and x87 FPREM implicitly provide truncating float division,
# so it is used here as the basis of float div().
div{T<:Real}(x::T, y::T) = convert(T,round((x-rem(x,y))/y))
fld{T<:Real}(x::T, y::T) = convert(T,round((x-mod(x,y))/y))
#rem{T<:Real}(x::T, y::T) = convert(T,x-y*trunc(x/y))
#mod{T<:Real}(x::T, y::T) = convert(T,x-y*floor(x/y))

# operator alias
const % = rem
.%(x::Real, y::Real) = x%y

# mod returns in [0,y) whereas mod1 returns in (0,y]
mod1{T<:Real}(x::T, y::T) = y-mod(y-x,y)
rem1{T<:Real}(x::T, y::T) = rem(x-1,y)+1
fld1{T<:Real}(x::T, y::T) = fld(x-1,y)+1

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
sizeof(::Type{Symbol}) = error("type does not have a native size")
sizeof{T<:Array}(::Type{T}) = error("type does not have a native size")
sizeof(x) = sizeof(typeof(x))

# copying immutable things
copy(x::Union(Symbol,Number,String,Function,Tuple,LambdaStaticData,
              TopNode,QuoteNode,DataType,UnionType)) = x

# function pipelining
|>(x, f::Function) = f(x)

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

function promote_shape(a::(Int, Int), b::(Int, Int))
    if a[1] != b[1] || a[2] != b[2]
        error("argument dimensions must match")
    end
    return a
end

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

# vectorized ifelse

function ifelse(c::AbstractArray{Bool}, x, y)
    reshape([ifelse(ci, x, y) for ci in c], size(c))
end

function ifelse(c::AbstractArray{Bool}, x::AbstractArray, y::AbstractArray)
    shp = promote_shape(size(c), promote_shape(size(x), size(y)))
    reshape([ifelse(c[i], x[i], y[i]) for i = 1 : length(c)], shp)
end

function ifelse(c::AbstractArray{Bool}, x::AbstractArray, y)
    shp = promote_shape(size(c), size(c))
    reshape([ifelse(c[i], x[i], y) for i = 1 : length(c)], shp)
end

function ifelse(c::AbstractArray{Bool}, x, y::AbstractArray)
    shp = promote_shape(size(c), size(y))
    reshape([ifelse(c[i], x, y[i]) for i = 1 : length(c)], shp)
end

# some operators not defined yet
global //, .>>, .<<, >:, <|, |>, hcat, hvcat

module Operators

export
    !,
    !=,
    !==,
    $,
    %,
    .%,
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
    >:,
    <<,
    <=,
    ==,
    >,
    >=,
    >>,
    .>>,
    .<<,
    >>>,
    \,
    ^,
    |,
    |>,
    <|,
    ~,
    colon,
    hcat,
    vcat,
    hvcat,
    getindex,
    setindex!,
    transpose,
    ctranspose

import Base: !, !=, $, %, .%, &, *, +, -, .!=, .+, .-, .*, ./, .<, .<=, .==, .>,
    .>=, .\, .^, /, //, <, <:, <<, <=, ==, >, >=, >>, .>>, .<<, >>>,
    <|, |>, \, ^, |, ~, !==, >:, colon, hcat, vcat, hvcat, getindex, setindex!,
    transpose, ctranspose

end
