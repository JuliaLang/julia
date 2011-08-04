## reductions ##

## reduction operator base cases ##
max() = -Inf
min() = +Inf
sum() = 0
prod() = 1
any() = false
all() = true
count() = 0

reduce(op, itr) = reduce(op, op(), itr)

function reduce(op::Function, v0, itr)
    v = v0
    for x = itr
        v = op(v,x)
    end
    return v
end

mapreduce(op::Function, f::Function, itr) = mapreduce(op, f, op(), itr)

function mapreduce(op::Function, f::Function, v0, itr)
    v = v0
    for x = itr
        v = op(v,f(x))
    end
    return v
end

max(itr)  = reduce(max, itr)
min(itr)  = reduce(min, itr)
sum(itr)  = reduce(+,   itr)
prod(itr) = reduce(*,   itr)
any(itr)  = reduce(any, itr)
all(itr)  = reduce(all, itr)

max(f::Function, itr)  = mapreduce(max, f, itr)
min(f::Function, itr)  = mapreduce(min, f, itr)
sum(f::Function, itr)  = mapreduce(+,   f, itr)
prod(f::Function, itr) = mapreduce(*,   f, itr)
any(f::Function, itr)  = mapreduce(any, f, itr)
all(f::Function, itr)  = mapreduce(all, f, itr)

max(x, y, z, rest...)  = reduce(max,  max(max(x,y),z),   rest)
min(x, y, z, rest...)  = reduce(min,  min(min(x,y),z),   rest)
sum(x, y, z, rest...)  = reduce(sum,  sum(sum(x,y),z),   rest)
prod(x, y, z, rest...) = reduce(prod, prod(prod(x,y),z), rest)
any(x, y, z, rest...)  = reduce(any,  any(any(x,y),z),   rest)
all(x, y, z, rest...)  = reduce(all,  all(all(x,y),z),   rest)

function count(itr)
    c = 0
    for x = itr
        c += count(x)
    end
    return c
end

function countp(pred, itr)
    c = 0
    for x = itr
        if pred(x)
            c += 1
        end
    end
    return c
end

function anyp(pred, itr)
    for x = itr
        if pred(x)
            return true
        end
    end
    return false
end

function allp(pred, itr)
    for x = itr
        if !pred(x)
            return false
        end
    end
    return true
end

## Scans ##

scan(op::Function, x::()) = ()
function scan(op::Function, x::Tuple)
    n = length(x)
    s = (x[1],)
    for i=2:n
        s = tuple(s..., op(s[i-1], x[i]))
    end
    return s
end

cumsum() = ()
cumsum(x::Number) = (x,)
cumsum(x::Number, y::Number) = (x, x+y)
cumsum(x::Number, y::Number, z::Number) = (x, x+y, x+y+z)
cumsum(itr::(Number...))  = scan(+, itr)
cumsum(itr::Number...) = scan(+, itr)

cumprod() = ()
cumprod(x::Number) = (x,)
cumprod(x::Number, y::Number) = (x, x*y)
cumprod(x::Number, y::Number, z::Number) = (x, x*y, x*y*z)
cumprod(itr::(Number...)) = scan(*, itr)
cumprod(itr::Number...) = scan(*, itr)
