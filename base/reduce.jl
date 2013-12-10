## reductions ##

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
        return op()  # empty collection
    end
    (v, s) = next(itr, s)
    while !done(itr, s)
        (x, s) = next(itr, s)
        v = op(v,x)
    end
    return v
end

function maximum(itr)
    s = start(itr)
    if done(itr, s)
        error("maximum: argument is empty")
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
        error("minimum: argument is empty")
    end
    (v, s) = next(itr, s)
    while !done(itr, s)
        (x, s) = next(itr, s)
        v = scalarmin(v,x)
    end
    return v
end

function sum(itr)
    s = start(itr)
    if done(itr, s)
        return +()
    end
    (v, s) = next(itr, s)
    while !done(itr, s)
        (x, s) = next(itr, s)
        v = v+x
    end
    return v
end

function prod(itr)
    s = start(itr)
    if done(itr, s)
        return *()
    end
    (v, s) = next(itr, s)
    while !done(itr, s)
        (x, s) = next(itr, s)
        v = v*x
    end
    return v
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

# left-associative and right-associative folds

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

##
# generic map on any iterator
function map(f::Callable, iters...)
    result = {}
    len = length(iters)
    states = [start(iters[idx]) for idx in 1:len]
    nxtvals = cell(len)
    cont = true
    for idx in 1:len
        done(iters[idx], states[idx]) && (cont = false; break)
    end
    while cont
        for idx in 1:len
            nxtvals[idx],states[idx] = next(iters[idx], states[idx])
        end
        push!(result, f(nxtvals...))
        for idx in 1:len
            done(iters[idx], states[idx]) && (cont = false; break)
        end
    end
    result
end

function mapreduce(f::Callable, op::Callable, itr)
    s = start(itr)
    if done(itr, s)
        return op()  # empty collection
    end
    (x, s) = next(itr, s)
    v = f(x)
    while !done(itr, s)
        (x, s) = next(itr, s)
        v = op(v,f(x))
    end
    return v
end

function mapreduce(f::Callable, op::Callable, v0, itr)
    v = v0
    for x in itr
        v = op(v,f(x))
    end
    return v
end

# mapreduce for random-access arrays, using pairwise recursive reduction
# for improved accuracy (see sum_pairwise)
function mr_pairwise(f::Callable, op::Callable, A::AbstractArray, i1,n)
    if n < 128
        @inbounds v = f(A[i1])
        for i = i1+1:i1+n-1
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
    n == 0 ? op() : mr_pairwise(f,op,A, 1,n)
end
function mapreduce(f::Callable, op::Callable, v0, A::AbstractArray)
    n = length(A)
    n == 0 ? v0 : op(v0, mr_pairwise(f,op,A, 1,n))
end

function r_pairwise(op::Callable, A::AbstractArray, i1,n)
    if n < 128
        @inbounds v = A[i1]
        for i = i1+1:i1+n-1
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
    n == 0 ? op() : r_pairwise(op,A, 1,n)
end
function reduce(op::Callable, v0, A::AbstractArray)
    n = length(A)
    n == 0 ? v0 : op(v0, r_pairwise(op,A, 1,n))
end

function any(itr)
    for x in itr
        if x
            return true
        end
    end
    return false
end

function all(itr)
    for x in itr
        if !x
            return false
        end
    end
    return true
end

maximum(f::Function, itr) = mapreduce(f, scalarmax, itr)
minimum(f::Function, itr) = mapreduce(f, scalarmin, itr)
sum(f::Function, itr)     = mapreduce(f, +        , itr)
prod(f::Function, itr)    = mapreduce(f, *        , itr)

function count(pred::Function, itr)
    s = 0
    for x in itr
        if pred(x)
            s+=1
        end
    end
    s
end

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
