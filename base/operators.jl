# This file is a part of Julia. License is MIT: http://julialang.org/license

## types ##

const (<:) = issubtype

super(T::DataType) = T.super

## generic comparison ##

==(x,y) = x === y

isequal(x, y) = x == y
isequal(x::FloatingPoint, y::FloatingPoint) = (isnan(x) & isnan(y)) | (signbit(x) == signbit(y)) & (x == y)
isequal(x::Real,          y::FloatingPoint) = (isnan(x) & isnan(y)) | (signbit(x) == signbit(y)) & (x == y)
isequal(x::FloatingPoint, y::Real         ) = (isnan(x) & isnan(y)) | (signbit(x) == signbit(y)) & (x == y)

isless(x::FloatingPoint, y::FloatingPoint) = (!isnan(x) & isnan(y)) | (signbit(x) & !signbit(y)) | (x < y)
isless(x::Real,          y::FloatingPoint) = (!isnan(x) & isnan(y)) | (signbit(x) & !signbit(y)) | (x < y)
isless(x::FloatingPoint, y::Real         ) = (!isnan(x) & isnan(y)) | (signbit(x) & !signbit(y)) | (x < y)

==(T::Type, S::Type) = typeseq(T, S)

## comparison fallbacks ##

!=(x,y) = !(x==y)
const ≠ = !=
const ≡ = is
!==(x,y) = !is(x,y)
const ≢ = !==

<(x,y) = isless(x,y)
>(x,y) = y < x
<=(x,y) = !(y < x)
const ≤ = <=
>=(x,y) = (y <= x)
const ≥ = >=
.>(x,y) = y .< x
.>=(x,y) = y .<= x
const .≥ = .>=

# this definition allows Number types to implement < instead of isless,
# which is more idiomatic:
isless(x::Real, y::Real) = x<y
lexcmp(x::Real, y::Real) = isless(x,y) ? -1 : ifelse(isless(y,x), 1, 0)

ifelse(c::Bool, x, y) = Intrinsics.select_value(c, x, y)

cmp(x,y) = isless(x,y) ? -1 : ifelse(isless(y,x), 1, 0)
lexcmp(x,y) = cmp(x,y)
lexless(x,y) = lexcmp(x,y)<0

# cmp returns -1, 0, +1 indicating ordering
cmp(x::Integer, y::Integer) = ifelse(isless(x,y), -1, ifelse(isless(y,x), 1, 0))

max(x,y) = ifelse(y < x, x, y)
min(x,y) = ifelse(y < x, y, x)
minmax(x,y) = y < x ? (y, x) : (x, y)

scalarmax(x,y) = max(x,y)
scalarmax(x::AbstractArray, y::AbstractArray) = error("ordering is not well-defined for arrays")
scalarmax(x               , y::AbstractArray) = error("ordering is not well-defined for arrays")
scalarmax(x::AbstractArray, y               ) = error("ordering is not well-defined for arrays")

scalarmin(x,y) = min(x,y)
scalarmin(x::AbstractArray, y::AbstractArray) = error("ordering is not well-defined for arrays")
scalarmin(x               , y::AbstractArray) = error("ordering is not well-defined for arrays")
scalarmin(x::AbstractArray, y               ) = error("ordering is not well-defined for arrays")

## definitions providing basic traits of arithmetic operators ##

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
.+(x::Number,y::Number) = x+y
.-(x::Number,y::Number) = x-y
.<<(x::Number,y::Number) = x<<y
.>>(x::Number,y::Number) = x>>y

.==(x::Number,y::Number) = x == y
.!=(x::Number,y::Number) = x != y
.< (x::Real,y::Real) = x < y
.<=(x::Real,y::Real) = x <= y
const .≤ = .<=
const .≠ = .!=

# core << >> and >>> takes Int32 as second arg
<<(x,y::Int32)    = no_op_err("<<", typeof(x))
>>(x,y::Int32)    = no_op_err(">>", typeof(x))
>>>(x,y::Int32)   = no_op_err(">>>", typeof(x))
<<(x,y::Integer)  = x << convert(Int32,y)
>>(x,y::Integer)  = x >> convert(Int32,y)
>>>(x,y::Integer) = x >>> convert(Int32,y)

# fallback div, fld, and cld implementations
# NOTE: C89 fmod() and x87 FPREM implicitly provide truncating float division,
# so it is used here as the basis of float div().
div{T<:Real}(x::T, y::T) = convert(T,round((x-rem(x,y))/y))
fld{T<:Real}(x::T, y::T) = convert(T,round((x-mod(x,y))/y))
cld{T<:Real}(x::T, y::T) = convert(T,round((x-modCeil(x,y))/y))
#rem{T<:Real}(x::T, y::T) = convert(T,x-y*trunc(x/y))
#mod{T<:Real}(x::T, y::T) = convert(T,x-y*floor(x/y))
modCeil{T<:Real}(x::T, y::T) = convert(T,x-y*ceil(x/y))

# operator alias
const % = rem
.%(x::Real, y::Real) = x%y
const ÷ = div

# mod returns in [0,y) whereas mod1 returns in (0,y]
mod1{T<:Real}(x::T, y::T) = (m=mod(x,y); ifelse(m==0, y, m))
rem1{T<:Real}(x::T, y::T) = rem(x-1,y)+1
fld1{T<:Real}(x::T, y::T) = fld(x-1,y)+1

# transpose
transpose(x) = x
ctranspose(x) = conj(transpose(x))
conj(x) = x

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
At_ldiv_Bt(a,b) = At_ldiv_B(a,transpose(b))
Ac_ldiv_Bt(a,b) = Ac_ldiv_B(a,transpose(b))

widen{T<:Number}(x::T) = convert(widen(T), x)

eltype(::Type) = Any
eltype(::Type{Any}) = Any
eltype(t::DataType) = eltype(super(t))
eltype(x) = eltype(typeof(x))

# copying immutable things
copy(x::Union(Symbol,Number,AbstractString,Function,Tuple,LambdaStaticData,
              TopNode,QuoteNode,DataType,UnionType)) = x

# function pipelining
|>(x, f::Callable) = f(x)

# array shape rules

function promote_shape(a::Tuple{Int,}, b::Tuple{Int,})
    if a[1] != b[1]
        throw(DimensionMismatch("dimensions must match"))
    end
    return a
end

function promote_shape(a::Tuple{Int,Int}, b::Tuple{Int,})
    if a[1] != b[1] || a[2] != 1
        throw(DimensionMismatch("dimensions must match"))
    end
    return a
end

promote_shape(a::Tuple{Int,}, b::Tuple{Int,Int}) = promote_shape(b, a)

function promote_shape(a::Tuple{Int, Int}, b::Tuple{Int, Int})
    if a[1] != b[1] || a[2] != b[2]
        throw(DimensionMismatch("dimensions must match"))
    end
    return a
end

function promote_shape(a::Dims, b::Dims)
    if length(a) < length(b)
        return promote_shape(b, a)
    end
    for i=1:length(b)
        if a[i] != b[i]
            throw(DimensionMismatch("dimensions must match"))
        end
    end
    for i=length(b)+1:length(a)
        if a[i] != 1
            throw(DimensionMismatch("dimensions must match"))
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
        throw(DimensionMismatch("tried to assign $(length(X)) elements to $(length(I[1])) destinations"))
    else
        throw(DimensionMismatch("tried to assign $(dims2string(size(X))) array to $(dims2string(map(length,I))) destination"))
    end
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
to_index(i::Int) = i
to_index(i::Integer) = convert(Int,i)::Int
to_index(r::UnitRange{Int}) = r
to_index(r::Range{Int}) = r
to_index(I::UnitRange{Bool}) = find(I)
to_index(I::Range{Bool}) = find(I)
to_index{T<:Integer}(r::UnitRange{T}) = to_index(first(r)):to_index(last(r))
to_index{T<:Integer}(r::StepRange{T}) = to_index(first(r)):to_index(step(r)):to_index(last(r))
to_index(I::AbstractArray{Bool}) = find(I)
to_index(A::AbstractArray{Int}) = A
to_index{T<:Integer}(A::AbstractArray{T}) = [to_index(x) for x in A]
to_index(i1, i2)         = to_index(i1), to_index(i2)
to_index(i1, i2, i3)     = to_index(i1), to_index(i2), to_index(i3)
to_index(i1, i2, i3, i4) = to_index(i1), to_index(i2), to_index(i3), to_index(i4)
to_index(I...) = to_index(I)
to_index(I::Tuple{Any,})            = (to_index(I[1]), )
to_index(I::Tuple{Any,Any,})        = (to_index(I[1]), to_index(I[2]))
to_index(I::Tuple{Any,Any,Any})     = (to_index(I[1]), to_index(I[2]), to_index(I[3]))
to_index(I::Tuple{Any,Any,Any,Any}) = (to_index(I[1]), to_index(I[2]), to_index(I[3]), to_index(I[4]))
to_index(I::Tuple) = map(to_index, I)
to_index(i) = error("invalid index: $i")

# Addition/subtraction of ranges
for f in (:+, :-)
    @eval begin
        function $f(r1::OrdinalRange, r2::OrdinalRange)
            r1l = length(r1)
            r1l == length(r2) || error("argument dimensions must match")
            range($f(r1.start,r2.start), $f(step(r1),step(r2)), r1l)
        end

        function $f{T<:FloatingPoint}(r1::FloatRange{T}, r2::FloatRange{T})
            len = r1.len
            len == r2.len || error("argument dimensions must match")
            divisor1, divisor2 = r1.divisor, r2.divisor
            if divisor1 == divisor2
                FloatRange{T}($f(r1.start,r2.start), $f(r1.step,r2.step),
                              len, divisor1)
            else
                d1 = Int(divisor1)
                d2 = Int(divisor2)
                d = lcm(d1,d2)
                s1 = div(d,d1)
                s2 = div(d,d2)
                FloatRange{T}($f(r1.start*s1, r2.start*s2),
                              $f(r1.step*s1, r2.step*s2),  len, d)
            end
        end

        $f(r1::FloatRange, r2::FloatRange) = $f(promote(r1,r2)...)
        $f(r1::FloatRange, r2::OrdinalRange) = $f(promote(r1,r2)...)
        $f(r1::OrdinalRange, r2::FloatRange) = $f(promote(r1,r2)...)
    end
end

# vectorization

macro vectorize_1arg(S,f)
    S = esc(S); f = esc(f); T = esc(:T)
    quote
        ($f){$T<:$S}(x::AbstractArray{$T,1}) = [ ($f)(x[i]) for i=1:length(x) ]
        ($f){$T<:$S}(x::AbstractArray{$T,2}) =
            [ ($f)(x[i,j]) for i=1:size(x,1), j=1:size(x,2) ]
        ($f){$T<:$S}(x::AbstractArray{$T}) =
            reshape([ ($f)(x[i]) for i in eachindex(x) ], size(x))
    end
end

macro vectorize_2arg(S,f)
    S = esc(S); f = esc(f); T1 = esc(:T1); T2 = esc(:T2)
    quote
        ($f){$T1<:$S, $T2<:$S}(x::($T1), y::AbstractArray{$T2}) =
            reshape([ ($f)(x, y[i]) for i in eachindex(y) ], size(y))
        ($f){$T1<:$S, $T2<:$S}(x::AbstractArray{$T1}, y::($T2)) =
            reshape([ ($f)(x[i], y) for i in eachindex(x) ], size(x))

        function ($f){$T1<:$S, $T2<:$S}(x::AbstractArray{$T1}, y::AbstractArray{$T2})
            shp = promote_shape(size(x),size(y))
            reshape([ ($f)(x[i], y[i]) for i in eachindex(x,y) ], shp)
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

# Pair

immutable Pair{A,B}
    first::A
    second::B
end

const => = Pair

start(p::Pair) = 1
done(p::Pair, i) = i>2
next(p::Pair, i) = (getfield(p,i), i+1)

indexed_next(p::Pair, i::Int, state) = (getfield(p,i), i+1)

hash(p::Pair, h::UInt) = hash(p.second, hash(p.first, h))

==(p::Pair, q::Pair) = (p.first==q.first) & (p.second==q.second)
isequal(p::Pair, q::Pair) = isequal(p.first,q.first) & isequal(p.second,q.second)

isless(p::Pair, q::Pair) = ifelse(!isequal(p.first,q.first), isless(p.first,q.first),
                                                             isless(p.second,q.second))
getindex(p::Pair,i::Int) = getfield(p,i)
getindex(p::Pair,i::Real) = getfield(p, convert(Int, i))
reverse(p::Pair) = Pair(p.second, p.first)

# some operators not defined yet
global //, >:, <|, hcat, hvcat, ⋅, ×, ∈, ∉, ∋, ∌, ⊆, ⊈, ⊊, ∩, ∪, √, ∛

this_module = current_module()
baremodule Operators

export
    !,
    !=,
    !==,
    ===,
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
    ≥,
    ≤,
    ≠,
    .≥,
    .≤,
    .≠,
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
    ÷,
    ⋅,
    ×,
    ∈,
    ∉,
    ∋,
    ∌,
    ⊆,
    ⊈,
    ⊊,
    ∩,
    ∪,
    √,
    ∛,
    colon,
    hcat,
    vcat,
    hvcat,
    getindex,
    setindex!,
    transpose,
    ctranspose,
    call

import ..this_module: !, !=, $, %, .%, &, *, +, -, .!=, .+, .-, .*, ./, .<, .<=, .==, .>,
    .>=, .\, .^, /, //, <, <:, <<, <=, ==, >, >=, >>, .>>, .<<, >>>,
    <|, |>, \, ^, |, ~, !==, ===, >:, colon, hcat, vcat, hvcat, getindex, setindex!,
    transpose, ctranspose, call,
    ≥, ≤, ≠, .≥, .≤, .≠, ÷, ⋅, ×, ∈, ∉, ∋, ∌, ⊆, ⊈, ⊊, ∩, ∪, √, ∛

end
