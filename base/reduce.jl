## reductions ##

###### higher level reduction functions ######

# Note that getting type-stable results from reduction functions,
# or at least having type-stable loops, is nontrivial (#6069).

# reduce

function reduce(op::Callable, itr) # this is a left fold
    if is(op,+)
        return sum(itr)
    elseif is(op,*)
        return prod(itr)
    elseif is(op,|)
        return any(itr)
    elseif is(op,&)
        return all(itr)
    end
    s = start(itr)
    if done(itr, s)
        error("argument is empty")
    end
    (v, s) = next(itr, s)
    if done(itr, s)
        return v
    else # specialize for length > 1 to have a hopefully type-stable loop
        (x, s) = next(itr, s)
        result = op(v, x)
        while !done(itr, s)
            (x, s) = next(itr, s)
            result = op(result, x)
        end
        return result
    end
end

# pairwise reduction, requires n > 1 (to allow type-stable loop)
function r_pairwise(op::Callable, A::AbstractArray, i1,n)
    if n < 128
        @inbounds v = op(A[i1], A[i1+1])
        for i = i1+2:i1+n-1
            @inbounds v = op(v,A[i])
        end
        return v
    else
        n2 = div(n,2)
        return op(r_pairwise(op,A, i1,n2), r_pairwise(op,A, i1+n2,n-n2))
    end
end

function reduce(op::Callable, A::AbstractArray)
    n = length(A)
    n == 0 ? error("argument is empty") : n == 1 ? A[1] : r_pairwise(op,A, 1,n)
end

function reduce(op::Callable, v0, A::AbstractArray)
    n = length(A)
    n == 0 ? v0 : n == 1 ? op(v0, A[1]) : op(v0, r_pairwise(op,A, 1,n))
end

function reduce(op::Callable, v0, itr)
    v = v0
    if is(op,+)
        for x in itr
            v = v+x
        end
    elseif is(op,*)
        for x in itr
            v = v*x
        end
    else
        u = v0
        for x in itr
            u = op(u,x)
        end
        return u
    end
    return v
end

# foldl & foldr

function foldl(op::Callable, v0, itr, i=start(itr))
    v = v0
    while !done(itr,i)
        x, i = next(itr,i)
        v = op(v,x)
    end
    return v
end
function foldl(op::Callable, itr)
    v0, i = next(itr,start(itr))
    foldl(op, v0, itr, i)
end

function foldr(op::Callable, v0, itr, i=endof(itr))
    v = v0
    while i > 0
        x = itr[i]
        v = op(x,v)
        i = i - 1
    end
    return v
end
function foldr(op::Callable, itr)
    i = endof(itr)
    foldr(op, itr[i], itr, i-1)
end


###### Specific reduction functions ######

## in & contains

function in(x, itr)
    for y in itr
        if y==x
            return true
        end
    end
    return false
end

function contains(itr, x)
    depwarn("contains(collection, item) is deprecated, use in(item, collection) instead", :contains)
    in(x, itr)
end

function contains(eq::Function, itr, x)
    for y in itr
        if eq(y,x)
            return true
        end
    end
    return false
end

## countnz & count


function countnz(itr)
    n = 0
    for x in itr
        n += (x != 0)
    end
    return n
end

function countnz{T}(a::AbstractArray{T})
    n = 0
    for i = 1:length(a)
        @inbounds n += (a[i] != 0)
    end
    return n
end

function countnz(a::AbstractArray{Bool})
    n = 0
    for x in a
        if x; n += 1; end
    end
    return n
end

function count(pred::Function, itr)
    s = 0
    for x in itr
        if pred(x)
            s+=1
        end
    end
    s
end

## sum

function sum(itr)
    s = start(itr)
    if done(itr, s)
        if applicable(eltype, itr)
            T = eltype(itr)
            return zero(T) + zero(T)
        else
            throw(ArgumentError("sum(itr) is undefined for empty collections; instead, do isempty(itr) ? z : sum(itr), where z is the correct type of zero for your sum"))
        end
    end
    (v, s) = next(itr, s)
    done(itr, s) && return v + zero(v) # adding zero for type stability
    # specialize for length > 1 to have type-stable loop
    (x, s) = next(itr, s)
    result = v + x
    while !done(itr, s)
        (x, s) = next(itr, s)
        result += x
    end
    return result
end

sum(A::AbstractArray{Bool}) = countnz(A)

# a fast implementation of sum in sequential order (from left to right).
# to allow type-stable loops, requires length > 1
function sum_seq{T}(a::AbstractArray{T}, ifirst::Int, ilast::Int)
    
    @inbounds if ifirst + 6 >= ilast  # length(a) < 8
        i = ifirst
        s = a[i] + a[i+1]
        i = i+1
        while i < ilast
            s += a[i+=1]
        end
        return s

    else # length(a) >= 8

        # more effective utilization of the instruction
        # pipeline through manually unrolling the sum
        # into four-way accumulation. Benchmark shows
        # that this results in about 2x speed-up.                

        s1 = a[ifirst] + a[ifirst + 4]
        s2 = a[ifirst + 1] + a[ifirst + 5]
        s3 = a[ifirst + 2] + a[ifirst + 6]
        s4 = a[ifirst + 3] + a[ifirst + 7]

        i = ifirst + 8
        il = ilast - 3
        while i <= il
            s1 += a[i]
            s2 += a[i+1]
            s3 += a[i+2]
            s4 += a[i+3]
            i += 4
        end

        while i <= ilast
            s1 += a[i]
            i += 1
        end

        return s1 + s2 + s3 + s4
    end
end

# Pairwise (cascade) summation of A[i1:i1+n-1], which has O(log n) error growth
# [vs O(n) for a simple loop] with negligible performance cost if
# the base case is large enough.  See, e.g.:
#        http://en.wikipedia.org/wiki/Pairwise_summation
#        Higham, Nicholas J. (1993), "The accuracy of floating point
#        summation", SIAM Journal on Scientific Computing 14 (4): 783–799.
# In fact, the root-mean-square error growth, assuming random roundoff
# errors, is only O(sqrt(log n)), which is nearly indistinguishable from O(1)
# in practice.  See:
#        Manfred Tasche and Hansmartin Zeuner, Handbook of
#        Analytic-Computational Methods in Applied Mathematics (2000).
#

# Note: sum_seq uses four accumulators, so each accumulator gets at most 256 numbers
const PAIRWISE_SUM_BLOCKSIZE = 1024

# note: requires length > 1, due to sum_seq
function sum_pairwise(a::AbstractArray, ifirst::Int, ilast::Int)
    # bsiz: maximum block size

    if ifirst + PAIRWISE_SUM_BLOCKSIZE >= ilast
        sum_seq(a, ifirst, ilast)
    else
        imid = (ifirst + ilast) >>> 1
        sum_pairwise(a, ifirst, imid) + sum_pairwise(a, imid+1, ilast)
    end
end

function sum{T}(a::AbstractArray{T})
    n = length(a)
    n == 0 && return zero(T) + zero(T)
    n == 1 && return a[1] + zero(a[1])
    sum_pairwise(a, 1, length(a))
end

function sum{T<:Integer}(a::AbstractArray{T})
    n = length(a)
    n == 0 && return zero(T) + zero(T)
    n == 1 && return a[1] + zero(a[1])
    sum_seq(a, 1, length(a))
end

# Kahan (compensated) summation: O(1) error growth, at the expense
# of a considerable increase in computational expense.
function sum_kbn{T<:FloatingPoint}(A::AbstractArray{T})
    n = length(A)
    if (n == 0)
        return zero(T)+zero(T)
    end
    s = A[1]+zero(T)
    c = zero(T)+zero(T)
    for i in 2:n
        Ai = A[i]
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

function prod(itr)
    s = start(itr)
    if done(itr, s)
        if applicable(eltype, itr)
            T = eltype(itr)
            return one(T) * one(T)
        else
            throw(ArgumentError("prod(itr) is undefined for empty collections; instead, do isempty(itr) ? o : prod(itr), where o is the correct type of identity for your product"))
        end
    end
    (v, s) = next(itr, s)
    done(itr, s) && return v * one(v) # multiplying by one for type stability
    # specialize for length > 1 to have type-stable loop
    (x, s) = next(itr, s)
    result = v * x
    while !done(itr, s)
        (x, s) = next(itr, s)
        result *= x
    end
    return result
end

prod(A::AbstractArray{Bool}) =
    error("use all() instead of prod() for boolean arrays")

function prod_rgn{T}(A::AbstractArray{T}, first::Int, last::Int)
    if first > last
        return one(T) * one(T)
    end
    i = first
    @inbounds v = A[i]
    i == last && return v * one(v)
    @inbounds result = v * A[i+=1]
    while i < last
        @inbounds result *= A[i+=1]
    end
    return result
end
prod{T}(A::AbstractArray{T}) = prod_rgn(A, 1, length(A))


## maximum & minimum 

function maximum(itr)
    s = start(itr)
    if done(itr, s)
        error("argument is empty")
    end
    (v, s) = next(itr, s)
    while !done(itr, s)
        (x, s) = next(itr, s)
        v = scalarmax(v,x)
    end
    return v
end

function minimum(itr)
    s = start(itr)
    if done(itr, s)
        error("argument is empty")
    end
    (v, s) = next(itr, s)
    while !done(itr, s)
        (x, s) = next(itr, s)
        v = scalarmin(v,x)
    end
    return v
end

function maximum_rgn{T<:Real}(A::AbstractArray{T}, first::Int, last::Int)
    if first > last; error("argument range must not be empty"); end

    # locate the first non NaN number
    v = A[first]
    i = first + 1
    while v != v && i <= last
        @inbounds v = A[i]
        i += 1
    end

    while i <= last
        @inbounds x = A[i]
        if x > v
            v = x
        end
        i += 1
    end
    v
end

function minimum_rgn{T<:Real}(A::AbstractArray{T}, first::Int, last::Int)
    if first > last; error("argument range must not be empty"); end

    # locate the first non NaN number
    v = A[first]
    i = first + 1
    while v != v && i <= last
        @inbounds v = A[i]
        i += 1
    end

    while i <= last
        @inbounds x = A[i]
        if x < v
            v = x
        end
        i += 1
    end
    v
end

maximum{T<:Real}(A::AbstractArray{T}) = maximum_rgn(A, 1, length(A))
minimum{T<:Real}(A::AbstractArray{T}) = minimum_rgn(A, 1, length(A))

## extrema

extrema(r::Range) = (minimum(r), maximum(r))

function extrema(itr)
    s = start(itr)
    if done(itr, s)
        error("argument is empty")
    end
    (v, s) = next(itr, s)
    vmin = v
    vmax = v
    while !done(itr, s)
        (x, s) = next(itr, s)
        if x == x
            if x > vmax
                vmax = x
            elseif x < vmin
                vmin = x
            end
        end
    end
    return (vmin, vmax)
end

function extrema{T<:Real}(A::AbstractArray{T})
    if isempty(A); error("argument must not be empty"); end
    n = length(A)

    # locate the first non NaN number
    v = A[1]
    i = 2
    while v != v && i <= n
        @inbounds v = A[i]
        i += 1
    end

    vmin = v
    vmax = v
    while i <= n
        @inbounds v = A[i]
        if v > vmax
            vmax = v
        elseif v < vmin
            vmin = v
        end
        i += 1
    end

    return (vmin, vmax)
end


## all & any

function all(itr)
    for x in itr
        if !x
            return false
        end
    end
    return true
end

function any(itr)
    for x in itr
        if x
            return true
        end
    end
    return false
end


###### mapreduce ######

function mapreduce(f::Callable, op::Callable, itr)
    s = start(itr)
    if done(itr, s)
        error("argument is empty")
    end
    (x, s) = next(itr, s)
    v = f(x)
    if done(itr, s)
        return v
    else # specialize for length > 1 to have a hopefully type-stable loop
        (x, s) = next(itr, s)
        result = op(v, f(x))
        while !done(itr, s)
            (x, s) = next(itr, s)
            result = op(result, f(x))
        end
        return result
    end
end

function mapreduce(f::Callable, op::Callable, v0, itr)
    v = v0
    for x in itr
        v = op(v,f(x))
    end
    return v
end

# pairwise reduction, requires n > 1 (to allow type-stable loop)
function mr_pairwise(f::Callable, op::Callable, A::AbstractArray, i1,n)
    if n < 128
        @inbounds v = op(f(A[i1]), f(A[i1+1]))
        for i = i1+2:i1+n-1
            @inbounds v = op(v,f(A[i]))
        end
        return v
    else
        n2 = div(n,2)
        return op(mr_pairwise(f,op,A, i1,n2), mr_pairwise(f,op,A, i1+n2,n-n2))
    end
end
function mapreduce(f::Callable, op::Callable, A::AbstractArray)
    n = length(A)
    n == 0 ? error("argument is empty") : n == 1 ? f(A[1]) : mr_pairwise(f,op,A, 1,n)
end
function mapreduce(f::Callable, op::Callable, v0, A::AbstractArray)
    n = length(A)
    n == 0 ? v0 : n == 1 ? op(v0, f(A[1])) : op(v0, mr_pairwise(f,op,A, 1,n))
end

# specific mapreduce functions

maximum(f::Function, itr) = mapreduce(f, scalarmax, itr)
minimum(f::Function, itr) = mapreduce(f, scalarmin, itr)
sum(f::Function, itr)     = mapreduce(f, +        , itr)
prod(f::Function, itr)    = mapreduce(f, *        , itr)

function any(pred::Function, itr)
    for x in itr
        if pred(x)
            return true
        end
    end
    return false
end

function all(pred::Function, itr)
    for x in itr
        if !pred(x)
            return false
        end
    end
    return true
end

