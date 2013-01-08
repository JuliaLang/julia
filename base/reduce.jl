## reductions ##

function reduce(op, itr) # this is a left fold
    if is(op,max)
        return max(itr)
    elseif is(op,min)
        return min(itr)
    elseif is(op,+)
        return sum(itr)
    elseif is(op,*)
        return prod(itr)
    elseif is(op,any)
        return any(itr)
    elseif is(op,all)
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

function max(itr)
    s = start(itr)
    if done(itr, s)
        return typemin(eltype(itr))
    end
    (v, s) = next(itr, s)
    while !done(itr, s)
        (x, s) = next(itr, s)
        v = max(v,x)
    end
    return v
end

function min(itr)
    s = start(itr)
    if done(itr, s)
        return typemax(eltype(itr))
    end
    (v, s) = next(itr, s)
    while !done(itr, s)
        (x, s) = next(itr, s)
        v = min(v,x)
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

function mean(itr)
    return sum(itr) / numel(itr)
end

function reduce(op::Function, v0, itr)
    v = v0
    if is(op,max)
        for x in itr
            v = max(v,x)
        end
    elseif is(op,min)
        for x in itr
            v = min(v,x)
        end
    elseif is(op,+)
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

function mapreduce(op, f, itr)
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

function mapreduce(op::Function, f::Function, v0, itr)
    v = v0
    for x in itr
        v = op(v,f(x))
    end
    return v
end

function any(itr)
    for x in itr
        if x
            return true
        end
    end
    return false
end
any(args::Bool...) = any(args)
function all(itr)
    for x in itr
        if !x
            return false
        end
    end
    return true
end
all(args::Bool...) = all(args)

max(f::Function, itr)   = mapreduce(max, f, itr)
min(f::Function, itr)   = mapreduce(min, f, itr)
sum(f::Function, itr)   = mapreduce(+,   f, itr)
prod(f::Function, itr)  = mapreduce(*,   f, itr)
any(f::Function, itr)   = anyp(f, itr)
all(f::Function, itr)   = allp(f, itr)
count(f::Function, itr) = countp(f, itr)

function count(itr)
    c = 0
    for x in itr
        c += (x ? 1 : 0)
    end
    return c
end

function countp(pred, itr)
    c = 0
    for x in itr
        if pred(x)
            c += 1
        end
    end
    return c
end

function anyp(pred, itr)
    for x in itr
        if pred(x)
            return true
        end
    end
    return false
end

function allp(pred, itr)
    for x in itr
        if !pred(x)
            return false
        end
    end
    return true
end

function contains(itr, x)
    for y in itr
        if isequal(y,x)
            return true
        end
    end
    return false
end

function contains_is(itr, x::ANY)
    for y in itr
        if is(y,x)
            return true
        end
    end
    return false
end
