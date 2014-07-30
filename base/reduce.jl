## reductions ##

###### Functors ######

# Note that functors are merely used as internal machinery to enhance code reuse.
# They are not exported.
# When function arguments can be inlined, the use of functors can be removed.

abstract Func{N}

type IdFun <: Func{1} end
type AbsFun <: Func{1} end
type Abs2Fun <: Func{1} end
type ExpFun <: Func{1} end
type LogFun <: Func{1} end

type AddFun <: Func{2} end
type MulFun <: Func{2} end
type AndFun <: Func{2} end
type OrFun <: Func{2} end
type MaxFun <: Func{2} end
type MinFun <: Func{2} end

evaluate(::IdFun, x) = x
evaluate(::AbsFun, x) = abs(x)
evaluate(::Abs2Fun, x) = abs2(x)
evaluate(::ExpFun, x) = exp(x)
evaluate(::LogFun, x) = log(x)
evaluate(f::Callable, x) = f(x)

evaluate(::AddFun, x, y) = x + y
evaluate(::MulFun, x, y) = x * y
evaluate(::AndFun, x, y) = x & y
evaluate(::OrFun, x, y) = x | y
evaluate(::MaxFun, x, y) = scalarmax(x, y)
evaluate(::MinFun, x, y) = scalarmin(x, y)
evaluate(f::Callable, x, y) = f(x, y)


###### Generic (map)reduce functions ######

# r_promote: promote x to the type of reduce(op, [x])
r_promote(op, x) = x
r_promote(::AddFun, x) = x + zero(x)
r_promote(::MulFun, x) = x * one(x)


## foldl && mapfoldl

function mapfoldl_impl(f, op, v0, itr, i)
    if done(itr, i)
        return v0
    else
        (x, i) = next(itr, i)
        v = evaluate(op, v0, evaluate(f, x))
        while !done(itr, i)
            (x, i) = next(itr, i)
            v = evaluate(op, v, evaluate(f, x))
        end
        return v
    end
end

mapfoldl(f, op, v0, itr) = mapfoldl_impl(f, op, v0, itr, start(itr))

function mapfoldl(f, op::Function, v0, itr)
    is(op, +) ? mapfoldl(f, AddFun(), v0, itr) :
    is(op, *) ? mapfoldl(f, MulFun(), v0, itr) :
    is(op, &) ? mapfoldl(f, AndFun(), v0, itr) :
    is(op, |) ? mapfoldl(f, OrFun(), v0, itr) :
    mapfoldl_impl(f, op, v0, itr, start(itr))
end

function mapfoldl(f, op, itr)
    i = start(itr)
    if done(itr, i)
        return Base.mr_empty(f, op, eltype(itr))
    end
    (x, i) = next(itr, i)
    v0 = evaluate(f, x)
    mapfoldl_impl(f, op, v0, itr, i)
end

foldl(op, v0, itr) = mapfoldl(IdFun(), op, v0, itr)
foldl(op, itr) = mapfoldl(IdFun(), op, itr)

## foldr & mapfoldr

function mapfoldr_impl(f, op, v0, itr, i::Integer)
    if i == 0
        return v0
    else
        x = itr[i]
        v = evaluate(op, evaluate(f, x), v0)
        while i > 1
            x = itr[i -= 1]
            v = evaluate(op, evaluate(f, x), v)
        end
        return v
    end
end

mapfoldr(f, op, v0, itr) = mapfoldr_impl(f, op, v0, itr, endof(itr))
mapfoldr(f, op, itr) = (i = endof(itr); mapfoldr_impl(f, op, evaluate(f, itr[i]), itr, i-1))

foldr(op, v0, itr) = mapfoldr(IdFun(), op, v0, itr)
foldr(op, itr) = mapfoldr(IdFun(), op, itr)

## reduce & mapreduce

# mapreduce_***_impl require ifirst < ilast
function mapreduce_seq_impl(f, op, A::AbstractArray, ifirst::Int, ilast::Int)
    @inbounds fx1 = evaluate(f, A[ifirst])
    @inbounds fx2 = evaluate(f, A[ifirst+=1])
    @inbounds v = evaluate(op, fx1, fx2)
    while ifirst < ilast
        @inbounds fx = evaluate(f, A[ifirst+=1])
        v = evaluate(op, v, fx)
    end
    return v
end

function mapreduce_pairwise_impl(f, op, A::AbstractArray, ifirst::Int, ilast::Int, blksize::Int)
    if ifirst + blksize > ilast
        return mapreduce_seq_impl(f, op, A, ifirst, ilast)
    else
        imid = (ifirst + ilast) >>> 1
        v1 = mapreduce_seq_impl(f, op, A, ifirst, imid)
        v2 = mapreduce_seq_impl(f, op, A, imid+1, ilast)
        return evaluate(op, v1, v2)
    end
end

mapreduce(f, op, itr) = mapfoldl(f, op, itr)
mapreduce(f, op, v0, itr) = mapfoldl(f, op, v0, itr)
mapreduce_impl(f, op, A::AbstractArray, ifirst::Int, ilast::Int) = 
    mapreduce_seq_impl(f, op, A, ifirst, ilast)

# handling empty arrays
mr_empty(f, op, T) = error("Reducing over an empty array is not allowed.")
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
    n = length(A)
    if n == 0
        return mr_empty(f, op, T)
    elseif n == 1
        return r_promote(op, evaluate(f, A[1]))
    elseif n < 16
        @inbounds fx1 = evaluate(f, A[1])
        @inbounds fx2 = evaluate(f, A[2])
        s = evaluate(op, fx1, fx2)
        i = 2
        while i < n
            @inbounds fx = evaluate(f, A[i+=1])
            s = evaluate(op, s, fx)
        end
        return s
    else
        return mapreduce_impl(f, op, A, 1, n)
    end
end

mapreduce(f, op, A::AbstractArray) = _mapreduce(f, op, A)
mapreduce(f, op, a::Number) = evaluate(f, a)

function mapreduce(f, op::Function, A::AbstractArray)
    is(op, +) ? _mapreduce(f, AddFun(), A) :
    is(op, *) ? _mapreduce(f, MulFun(), A) :
    is(op, &) ? _mapreduce(f, AndFun(), A) :
    is(op, |) ? _mapreduce(f, OrFun(), A) :
    _mapreduce(f, op, A)
end

reduce(op, v0, itr) = mapreduce(IdFun(), op, v0, itr)
reduce(op, itr) = mapreduce(IdFun(), op, itr)
reduce(op, a::Number) = a


###### Specific reduction functions ######

## sum

function mapreduce_seq_impl(f, op::AddFun, a::AbstractArray, ifirst::Int, ilast::Int)
    @inbounds if ifirst + 6 >= ilast  # length(a) < 8
        i = ifirst
        s = evaluate(f, a[i]) + evaluate(f, a[i+1])
        i = i+1
        while i < ilast
            s += evaluate(f, a[i+=1])
        end
        return s

    else # length(a) >= 8, manual unrolling
        s1 = evaluate(f, a[ifirst]) + evaluate(f, a[ifirst + 4])
        s2 = evaluate(f, a[ifirst + 1]) + evaluate(f, a[ifirst + 5])
        s3 = evaluate(f, a[ifirst + 2]) + evaluate(f, a[ifirst + 6])
        s4 = evaluate(f, a[ifirst + 3]) + evaluate(f, a[ifirst + 7])
        i = ifirst + 8
        il = ilast - 3
        while i <= il
            s1 += evaluate(f, a[i])
            s2 += evaluate(f, a[i+1])
            s3 += evaluate(f, a[i+2])
            s4 += evaluate(f, a[i+3])
            i += 4
        end
        while i <= ilast
            s1 += evaluate(f, a[i])
            i += 1
        end
        return s1 + s2 + s3 + s4
    end    
end

# Note: sum_seq uses four accumulators, so each accumulator gets at most 256 numbers
sum_pairwise_blocksize(f) = 1024

# This appears to show a benefit from a larger block size
sum_pairwise_blocksize(::Abs2Fun) = 4096

mapreduce_impl(f, op::AddFun, A::AbstractArray, ifirst::Int, ilast::Int) = 
    mapreduce_pairwise_impl(f, op, A, ifirst, ilast, sum_pairwise_blocksize(f))

sum(f::Union(Callable,Func{1}), a) = mapreduce(f, AddFun(), a)
sum(a) = mapreduce(IdFun(), AddFun(), a)
sum(a::AbstractArray{Bool}) = countnz(a)
sumabs(a) = mapreduce(AbsFun(), AddFun(), a)
sumabs2(a) = mapreduce(Abs2Fun(), AddFun(), a)

# Kahan (compensated) summation: O(1) error growth, at the expense
# of a considerable increase in computational expense.
function sum_kbn{T<:FloatingPoint}(A::AbstractArray{T})
    n = length(A)
    if n == 0
        return sumzero(T)
    end
    c = zero(T)
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

prod(f::Union(Callable,Func{1}), a) = mapreduce(f, MulFun(), a)
prod(a) = mapreduce(IdFun(), MulFun(), a)

prod(A::AbstractArray{Bool}) =
    error("use all() instead of prod() for boolean arrays")

## maximum & minimum

function mapreduce_impl(f, op::MaxFun, A::AbstractArray, first::Int, last::Int)
    # locate the first non NaN number
    v = evaluate(f, A[first])
    i = first + 1
    while v != v && i <= last
        @inbounds v = evaluate(f, A[i])
        i += 1
    end
    while i <= last
        @inbounds x = evaluate(f, A[i])
        if x > v
            v = x
        end
        i += 1
    end
    v
end

function mapreduce_impl(f, op::MinFun, A::AbstractArray, first::Int, last::Int)
    # locate the first non NaN number
    v = evaluate(f, A[first])
    i = first + 1
    while v != v && i <= last
        @inbounds v = evaluate(f, A[i])
        i += 1
    end
    while i <= last
        @inbounds x = evaluate(f, A[i])
        if x < v
            v = x
        end
        i += 1
    end
    v
end

maximum(f::Union(Callable,Func{1}), a) = mapreduce(f, MaxFun(), a)
minimum(f::Union(Callable,Func{1}), a) = mapreduce(f, MinFun(), a)

maximum(a) = mapreduce(IdFun(), MaxFun(), a)
minimum(a) = mapreduce(IdFun(), MinFun(), a)

maxabs(a) = mapreduce(AbsFun(), MaxFun(), a)
minabs(a) = mapreduce(AbsFun(), MinFun(), a)

## extrema

extrema(r::Range) = (minimum(r), maximum(r))
extrema(x::Real) = (x, x)

function extrema(itr)
    s = start(itr)
    done(itr, s) && error("argument is empty")
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

function mapfoldl(f, ::AndFun, itr)
    for x in itr
        if !evaluate(f, x)
            return false
        end
    end
    return true
end

function mapfoldl(f, ::OrFun, itr)
    for x in itr
        if evaluate(f, x)
            return true
        end
    end
    return false
end

function mapreduce_impl(f, op::AndFun, A::AbstractArray, ifirst::Int, ilast::Int)
    while ifirst <= ilast
        @inbounds x = A[ifirst]
        if !evaluate(f, x)
            return false
        end
        ifirst += 1
    end
    return true
end

function mapreduce_impl(f, op::OrFun, A::AbstractArray, ifirst::Int, ilast::Int)
    while ifirst <= ilast
        @inbounds x = A[ifirst]
        if evaluate(f, x)
            return true
        end
        ifirst += 1
    end
    return false
end

all(a) = mapreduce(IdFun(), AndFun(), a)
any(a) = mapreduce(IdFun(), OrFun(), a)

all(pred::Union(Callable,Func{1}), a) = mapreduce(pred, AndFun(), a)
any(pred::Union(Callable,Func{1}), a) = mapreduce(pred, OrFun(), a)


## in & contains

immutable EqX{T} <: Func{1}
    x::T
end
EqX{T}(x::T) = EqX{T}(x)
evaluate(f::EqX, y) = (y == f.x)

in(x, itr) = any(EqX(x), itr)

const ∈ = in
∉(x, itr)=!∈(x, itr)
∋(itr, x)= ∈(x, itr)
∌(itr, x)=!∋(itr, x)

function contains(eq::Function, itr, x)
    for y in itr
        if eq(y, x)
            return true
        end
    end
    return false
end


## countnz & count

function count(pred::Union(Function,Func{1}), itr)
    n = 0
    for x in itr
        if evaluate(pred, x)
            n += 1
        end
    end
    return n
end

function count(pred::Union(Function,Func{1}), a::AbstractArray)
    n = 0
    for i = 1:length(a)
        @inbounds if evaluate(pred, a[i])
            n += 1
        end
    end
    return n
end

type NotEqZero <: Func{1} end
evaluate(NotEqZero, x) = (x != 0)

countnz(a) = count(NotEqZero(), a)

