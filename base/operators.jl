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
lexcmp(x::Real, y::Real) = isless(x,y) ? -1 : ifelse(isless(y,x), 1, 0)

ifelse(c::Bool, x, y) = Intrinsics.select_value(c, x, y)

cmp(x,y) = isless(x,y) ? -1 : ifelse(isless(y,x), 1, 0)
lexcmp(x,y) = cmp(x,y)
lexless(x,y) = lexcmp(x,y)<0

# cmp returns -1, 0, +1 indicating ordering
cmp(x::Real, y::Real) = int(sign(x-y))

max(x,y) = ifelse(y < x, x, y)
min(x,y) = ifelse(x < y, x, y)

scalarmax(x,y) = max(x,y)
scalarmax(x::AbstractArray, y::AbstractArray) = error("ordering is not well-defined for arrays")
scalarmax(x               , y::AbstractArray) = error("ordering is not well-defined for arrays")
scalarmax(x::AbstractArray, y               ) = error("ordering is not well-defined for arrays")

scalarmin(x,y) = min(x,y)
scalarmin(x::AbstractArray, y::AbstractArray) = error("ordering is not well-defined for arrays")
scalarmin(x               , y::AbstractArray) = error("ordering is not well-defined for arrays")
scalarmin(x::AbstractArray, y               ) = error("ordering is not well-defined for arrays")

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

for op = (:+, :*, :&, :|, :$, :min, :max, :kron)
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

widen{T<:Number}(x::T) = convert(widen(T), x)

sizeof(T::Type) = error(string("size of type ",T," unknown"))
sizeof(T::DataType) = if isleaftype(T) T.size else error("type does not have a native size") end
sizeof(::Type{Symbol}) = error("type does not have a native size")
sizeof{T<:Array}(::Type{T}) = error("type $(T) does not have a native size")
sizeof(x) = sizeof(typeof(x))

# copying immutable things
copy(x::Union(Symbol,Number,String,Function,Tuple,LambdaStaticData,
              TopNode,QuoteNode,DataType,UnionType)) = x

# function pipelining
|>(x, f::Function) = f(x)

# array shape rules

function promote_shape(a::(Int,), b::(Int,))
    if a[1] != b[1]
        error("dimensions must match")
    end
    return a
end

function promote_shape(a::(Int,Int), b::(Int,))
    if a[1] != b[1] || a[2] != 1
        error("dimensions must match")
    end
    return a
end

promote_shape(a::(Int,), b::(Int,Int)) = promote_shape(b, a)

function promote_shape(a::(Int, Int), b::(Int, Int))
    if a[1] != b[1] || a[2] != b[2]
        error("dimensions must match")
    end
    return a
end

function promote_shape(a::Dims, b::Dims)
    if length(a) < length(b)
        return promote_shape(b, a)
    end
    for i=1:length(b)
        if a[i] != b[i]
            error("dimensions must match")
        end
    end
    for i=length(b)+1:length(a)
        if a[i] != 1
            error("dimensions must match")
        end
    end
    return a
end

# shape of array to create for getindex() with indexes I
# drop dimensions indexed with trailing scalars
index_shape(I::Real...) = ()
index_shape(i, I...) = tuple(length(i), index_shape(I...)...)

function throw_setindex_mismatch(X, I)
    if length(I) == 1
        e = DimensionMismatch("tried to assign $(length(X)) elements to $(length(I[1])) destinations")
    else
        e = DimensionMismatch("tried to assign $(dims2string(size(X))) array to $(dims2string(map(length,I))) destination")
    end
    throw(e)
end

# check for valid sizes in A[I...] = X where X <: AbstractArray
# we want to allow dimensions that are equal up to permutation, but only
# for permutations that leave array elements in the same linear order.
# those are the permutations that preserve the order of the non-singleton
# dimensions.
function setindex_shape_check(X::AbstractArray, I...)
    li = ndims(X)
    lj = length(I)
    i = j = 1
    while true
        ii = size(X,i)
        jj = length(I[j])::Int
        if i == li || j == lj
            while i < li
                i += 1
                ii *= size(X,i)
            end
            while j < lj
                j += 1
                jj *= length(I[j])::Int
            end
            if ii != jj
                throw_setindex_mismatch(X, I)
            end
            return
        end
        if ii == jj
            i += 1
            j += 1
        elseif ii == 1
            i += 1
        elseif jj == 1
            j += 1
        else
            throw_setindex_mismatch(X, I)
        end
    end
end

setindex_shape_check(X::AbstractArray) =
    (length(X)==1 || throw_setindex_mismatch(X,()))

setindex_shape_check(X::AbstractArray, i) =
    (length(X)==length(i) || throw_setindex_mismatch(X, (i,)))

setindex_shape_check{T}(X::AbstractArray{T,1}, i) =
    (length(X)==length(i) || throw_setindex_mismatch(X, (i,)))

setindex_shape_check{T}(X::AbstractArray{T,1}, i, j) =
    (length(X)==length(i)*length(j) || throw_setindex_mismatch(X, (i,j)))

function setindex_shape_check{T}(X::AbstractArray{T,2}, i, j)
    li, lj = length(i), length(j)
    if length(X) != li*lj
        throw_setindex_mismatch(X, (i,j))
    end
    sx1 = size(X,1)
    if !(li == 1 || li == sx1 || sx1 == 1)
        throw_setindex_mismatch(X, (i,j))
    end
end

# convert to integer index
to_index(i)       = error("invalid index: $i")
to_index(i::Real) = convert(Int, i)
to_index(i::Int)  = i
to_index(r::Range1{Int}) = r
to_index{T<:Real}(r::Range1{T}) = to_index(first(r)):to_index(last(r))
to_index(I::AbstractArray{Bool,1}) = find(I)
to_index(I::Range1{Bool}) = find(I)
to_index{T<:Real}(A::AbstractArray{T}) = int(A)
to_index(i1, i2)         = to_index(i1), to_index(i2)
to_index(i1, i2, i3)     = to_index(i1), to_index(i2), to_index(i3)
to_index(i1, i2, i3, i4) = to_index(i1), to_index(i2), to_index(i3), to_index(i4)
to_index(I...) = to_index(I)
to_index(I::(Any,))            = (to_index(I[1]), )
to_index(I::(Any,Any,))        = (to_index(I[1]), to_index(I[2]))
to_index(I::(Any,Any,Any))     = (to_index(I[1]), to_index(I[2]), to_index(I[3]))
to_index(I::(Any,Any,Any,Any)) = (to_index(I[1]), to_index(I[2]), to_index(I[3]), to_index(I[4]))
to_index(I::Tuple) = map(to_index, I)

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
