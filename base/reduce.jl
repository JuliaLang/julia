# This file is a part of Julia. License is MIT: http://julialang.org/license

## reductions ##

###### Generic (map)reduce functions ######

if Int === Int32
typealias SmallSigned Union{Int8,Int16}
typealias SmallUnsigned Union{UInt8,UInt16}
else
typealias SmallSigned Union{Int8,Int16,Int32}
typealias SmallUnsigned Union{UInt8,UInt16,UInt32}
end

typealias CommonReduceResult Union{UInt64,UInt128,Int64,Int128,Float32,Float64}
typealias WidenReduceResult Union{SmallSigned, SmallUnsigned, Float16}

# r_promote: promote x to the type of reduce(op, [x])
r_promote(op, x::WidenReduceResult) = widen(x)
r_promote(op, x) = x
r_promote(::AddFun, x::WidenReduceResult) = widen(x)
r_promote(::MulFun, x::WidenReduceResult) = widen(x)
r_promote(::AddFun, x::Number) = oftype(x + zero(x), x)
r_promote(::MulFun, x::Number) = oftype(x * one(x), x)
r_promote(::AddFun, x) = x
r_promote(::MulFun, x) = x
r_promote(::MaxFun, x::WidenReduceResult) = x
r_promote(::MinFun, x::WidenReduceResult) = x
r_promote(::MaxFun, x) = x
r_promote(::MinFun, x) = x


## foldl && mapfoldl

function mapfoldl_impl(f, op, v0, itr, i)
    # Unroll the while loop once; if v0 is known, the call to op may
    # be evaluated at compile time
    if done(itr, i)
        return v0
    else
        (x, i) = next(itr, i)
        v = op(v0, f(x))
        while !done(itr, i)
            (x, i) = next(itr, i)
            v = op(v, f(x))
        end
        return v
    end
end

mapfoldl(f, op, v0, itr) = mapfoldl_impl(f, op, v0, itr, start(itr))

mapfoldl(f, op::Function, v0, itr) = mapfoldl_impl(f, specialized_binary(op), v0, itr, start(itr))

function mapfoldl(f, op, itr)
    i = start(itr)
    if done(itr, i)
        return Base.mr_empty(f, op, eltype(itr))
    end
    (x, i) = next(itr, i)
    v0 = f(x)
    mapfoldl_impl(f, op, v0, itr, i)
end

foldl(op, v0, itr) = mapfoldl(IdFun(), op, v0, itr)
foldl(op, itr) = mapfoldl(IdFun(), op, itr)

## foldr & mapfoldr

function mapfoldr_impl(f, op, v0, itr, i::Integer)
    # Unroll the while loop once; if v0 is known, the call to op may
    # be evaluated at compile time
    if i == 0
        return v0
    else
        x = itr[i]
        v  = op(f(x), v0)
        while i > 1
            x = itr[i -= 1]
            v = op(f(x), v)
        end
        return v
    end
end

mapfoldr(f, op, v0, itr) = mapfoldr_impl(f, op, v0, itr, endof(itr))
mapfoldr(f, op, itr) = (i = endof(itr); mapfoldr_impl(f, op, f(itr[i]), itr, i-1))

foldr(op, v0, itr) = mapfoldr(IdFun(), op, v0, itr)
foldr(op, itr) = mapfoldr(IdFun(), op, itr)

## reduce & mapreduce

# mapreduce_***_impl require ifirst < ilast
function mapreduce_seq_impl(f, op, A::AbstractArray, ifirst::Int, ilast::Int)
    @inbounds fx1 = r_promote(op, f(A[ifirst]))
    @inbounds fx2 = f(A[ifirst+=1])
    @inbounds v = op(fx1, fx2)
    while ifirst < ilast
        @inbounds fx = f(A[ifirst+=1])
        v = op(v, fx)
    end
    return v
end

function mapreduce_pairwise_impl(f, op, A::AbstractArray, ifirst::Int, ilast::Int, blksize::Int)
    if ifirst + blksize > ilast
        return mapreduce_seq_impl(f, op, A, ifirst, ilast)
    else
        imid = (ifirst + ilast) >>> 1
        v1 = mapreduce_pairwise_impl(f, op, A, ifirst, imid, blksize)
        v2 = mapreduce_pairwise_impl(f, op, A, imid+1, ilast, blksize)
        return op(v1, v2)
    end
end

mapreduce(f, op, itr) = mapfoldl(f, op, itr)
mapreduce(f, op, v0, itr) = mapfoldl(f, op, v0, itr)
mapreduce_impl(f, op, A::AbstractArray, ifirst::Int, ilast::Int) =
    mapreduce_pairwise_impl(f, op, A, ifirst, ilast, 1024)

# handling empty arrays
mr_empty(f, op, T) = throw(ArgumentError("reducing over an empty collection is not allowed"))
# use zero(T)::T to improve type information when zero(T) is not defined
mr_empty(::IdFun, op::AddFun, T) = r_promote(op, zero(T)::T)
mr_empty(::AbsFun, op::AddFun, T) = r_promote(op, abs(zero(T)::T))
mr_empty(::Abs2Fun, op::AddFun, T) = r_promote(op, abs2(zero(T)::T))
mr_empty(::IdFun, op::MulFun, T) = r_promote(op, one(T)::T)
mr_empty(::AbsFun, op::MaxFun, T) = abs(zero(T)::T)
mr_empty(::Abs2Fun, op::MaxFun, T) = abs2(zero(T)::T)
mr_empty(f, op::AndFun, T) = true
mr_empty(f, op::OrFun, T) = false

function _mapreduce{T}(f, op, A::AbstractArray{T})
    n = Int(length(A))
    if n == 0
        return mr_empty(f, op, T)
    elseif n == 1
        return r_promote(op, f(A[1]))
    elseif n < 16
        @inbounds fx1 = r_promote(op, f(A[1]))
        @inbounds fx2 = r_promote(op, f(A[2]))
        s = op(fx1, fx2)
        i = 2
        while i < n
            @inbounds fx = f(A[i+=1])
            s = op(s, fx)
        end
        return s
    else
        return mapreduce_impl(f, op, A, 1, n)
    end
end

mapreduce(f, op, A::AbstractArray) = _mapreduce(f, op, A)
mapreduce(f, op, a::Number) = f(a)

mapreduce(f, op::Function, A::AbstractArray) = mapreduce(f, specialized_binary(op), A)

reduce(op, v0, itr) = mapreduce(IdFun(), op, v0, itr)
reduce(op, itr) = mapreduce(IdFun(), op, itr)
reduce(op, a::Number) = a

### short-circuiting specializations of mapreduce

## conditions and results of short-circuiting

const ShortCircuiting = Union{AndFun, OrFun}
const ReturnsBool     = Union{EqX, Predicate}

shortcircuits(::AndFun, x::Bool) = !x
shortcircuits(::OrFun,  x::Bool) =  x

shorted(::AndFun) = false
shorted(::OrFun)  = true

sc_finish(::AndFun) = true
sc_finish(::OrFun)  = false

## short-circuiting (sc) mapreduce definitions

function mapreduce_sc_impl(f, op, itr::AbstractArray)
    @inbounds for x in itr
        shortcircuits(op, f(x)) && return shorted(op)
    end
    return sc_finish(op)
end

function mapreduce_sc_impl(f, op, itr)
    for x in itr
        shortcircuits(op, f(x)) && return shorted(op)
    end
    return sc_finish(op)
end

# mapreduce_sc tests if short-circuiting is safe;
# if so, mapreduce_sc_impl is called. If it's not
# safe, call mapreduce_no_sc, which redirects to
# non-short-circuiting definitions.

mapreduce_no_sc(f, op, itr::Any)           =  mapfoldl(f, op, itr)
mapreduce_no_sc(f, op, itr::AbstractArray) = _mapreduce(f, op, itr)

mapreduce_sc(f::Function,    op, itr) = mapreduce_sc(specialized_unary(f), op, itr)
mapreduce_sc(f::ReturnsBool, op, itr) = mapreduce_sc_impl(f, op, itr)
mapreduce_sc(f::Func{1},     op, itr) = mapreduce_no_sc(f, op, itr)

mapreduce_sc(f::IdFun, op, itr) =
    eltype(itr) <: Bool ?
        mapreduce_sc_impl(f, op, itr) :
        mapreduce_no_sc(f, op, itr)

mapreduce(f, op::ShortCircuiting, n::Number) = n
mapreduce(f, op::ShortCircuiting, itr::AbstractArray) = mapreduce_sc(f,op,itr)
mapreduce(f, op::ShortCircuiting, itr::Any)           = mapreduce_sc(f,op,itr)


###### Specific reduction functions ######

## sum

function mapreduce_seq_impl(f, op::AddFun, a::AbstractArray, ifirst::Int, ilast::Int)
    @inbounds begin
        s = r_promote(op, f(a[ifirst])) + f(a[ifirst+1])
        @simd for i = ifirst+2:ilast
            s += f(a[i])
        end
    end
    s
end

# Note: sum_seq usually uses four or more accumulators after partial
# unrolling, so each accumulator gets at most 256 numbers
sum_pairwise_blocksize(f) = 1024

# This appears to show a benefit from a larger block size
sum_pairwise_blocksize(::Abs2Fun) = 4096

mapreduce_impl(f, op::AddFun, A::AbstractArray, ifirst::Int, ilast::Int) =
    mapreduce_pairwise_impl(f, op, A, ifirst, ilast, sum_pairwise_blocksize(f))

sum(f::Union{Callable,Func{1}}, a) = mapreduce(f, AddFun(), a)
sum(a) = mapreduce(IdFun(), AddFun(), a)
sum(a::AbstractArray{Bool}) = countnz(a)
sumabs(a) = mapreduce(AbsFun(), AddFun(), a)
sumabs2(a) = mapreduce(Abs2Fun(), AddFun(), a)

# Kahan (compensated) summation: O(1) error growth, at the expense
# of a considerable increase in computational expense.
function sum_kbn{T<:AbstractFloat}(A::AbstractArray{T})
    n = length(A)
    c = r_promote(AddFun(), zero(T)::T)
    if n == 0
        return c
    end
    s = A[1] + c
    for i in 2:n
        @inbounds Ai = A[i]
        t = s + Ai
        if abs(s) >= abs(Ai)
            c += ((s-t) + Ai)
        else
            c += ((Ai-t) + s)
        end
        s = t
    end
    s + c
end


## prod

prod(f::Union{Callable,Func{1}}, a) = mapreduce(f, MulFun(), a)
prod(a) = mapreduce(IdFun(), MulFun(), a)

prod(A::AbstractArray{Bool}) =
    error("use all() instead of prod() for boolean arrays")

## maximum & minimum

function mapreduce_impl(f, op::MaxFun, A::AbstractArray, first::Int, last::Int)
    # locate the first non NaN number
    v = f(A[first])
    i = first + 1
    while v != v && i <= last
        @inbounds v = f(A[i])
        i += 1
    end
    while i <= last
        @inbounds x = f(A[i])
        if x > v
            v = x
        end
        i += 1
    end
    v
end

function mapreduce_impl(f, op::MinFun, A::AbstractArray, first::Int, last::Int)
    # locate the first non NaN number
    v = f(A[first])
    i = first + 1
    while v != v && i <= last
        @inbounds v = f(A[i])
        i += 1
    end
    while i <= last
        @inbounds x = f(A[i])
        if x < v
            v = x
        end
        i += 1
    end
    v
end

maximum(f::Union{Callable,Func{1}}, a) = mapreduce(f, MaxFun(), a)
minimum(f::Union{Callable,Func{1}}, a) = mapreduce(f, MinFun(), a)

maximum(a) = mapreduce(IdFun(), MaxFun(), a)
minimum(a) = mapreduce(IdFun(), MinFun(), a)

maxabs(a) = mapreduce(AbsFun(), MaxFun(), a)
minabs(a) = mapreduce(AbsFun(), MinFun(), a)

## extrema

extrema(r::Range) = (minimum(r), maximum(r))
extrema(x::Real) = (x, x)

function extrema(itr)
    s = start(itr)
    done(itr, s) && throw(ArgumentError("collection must be non-empty"))
    (v, s) = next(itr, s)
    while v != v && !done(itr, s)
        (x, s) = next(itr, s)
        v = x
    end
    vmin = v
    vmax = v
    while !done(itr, s)
        (x, s) = next(itr, s)
        if x > vmax
            vmax = x
        elseif x < vmin
            vmin = x
        end
    end
    return (vmin, vmax)
end

## all & any

any(itr) = any(IdFun(), itr)
all(itr) = all(IdFun(), itr)

any(f::Any,       itr) = any(Predicate(f), itr)
any(f::Predicate, itr) = mapreduce_sc_impl(f, OrFun(), itr)
any(f::IdFun,     itr) =
    eltype(itr) <: Bool ?
        mapreduce_sc_impl(f, OrFun(), itr) :
        nonboolean_any(itr)

all(f::Any,       itr) = all(Predicate(f), itr)
all(f::Predicate, itr) = mapreduce_sc_impl(f, AndFun(), itr)
all(f::IdFun,     itr) =
    eltype(itr) <: Bool ?
        mapreduce_sc_impl(f, AndFun(), itr) :
        nonboolean_all(itr)

## in & contains

in(x, itr) = any(EqX(x), itr)

const ∈ = in
∉(x, itr)=!∈(x, itr)
∋(itr, x)= ∈(x, itr)
∌(itr, x)=!∋(itr, x)

function contains(eq::Function, itr, x)
    for y in itr
        eq(y, x) && return true
    end
    return false
end


## countnz & count

function count(pred, itr)
    n = 0
    for x in itr
        pred(x) && (n += 1)
    end
    return n
end

function count(pred, a::AbstractArray)
    n = 0
    for i = 1:length(a)
        @inbounds if pred(a[i])
            n += 1
        end
    end
    return n
end

immutable NotEqZero <: Func{1} end
call(::NotEqZero, x) = x != 0

countnz(a) = count(NotEqZero(), a)
