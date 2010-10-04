## reductions ##

max() = -Inf
min() = +Inf
sum() = 0
prod() = 1
any() = false
all() = true

max(x::Scalar)  = x
min(x::Scalar)  = x
sum(x::Scalar)  = x
prod(x::Scalar) = x
any(x::Scalar)  = x
all(x::Scalar)  = x

reduce(op, itr) = reduce(op, op(), itr)

function reduce(op, v0, itr)
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
