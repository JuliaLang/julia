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

max(itr)  = reduce(max, itr)
min(itr)  = reduce(min, itr)
sum(itr)  = reduce(+,   itr)
prod(itr) = reduce(*,   itr)
any(itr)  = reduce(any, itr)
all(itr)  = reduce(all, itr)

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

## scans ##

## Base cases ##
cumsum() = ()
cumprod() = ()

scan(op, x) = (x,)

function scan(op::Function, x::Tuple)
    n = length(x)
    s = (x[1],)
    for i=2:n
        s = tuple(s..., op(s[i-1], x[i]))
    end
    return s
end

cumsum(itr)  = scan(+, itr)
cumsum(itr...) = scan(+, itr)

cumprod(itr) = scan(*, itr)
cumprod(itr...) = scan(*, itr)
