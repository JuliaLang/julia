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

max(rest...)  = max(rest)
min(rest...)  = min(rest)
sum(rest...)  = sum(rest)
prod(rest...) = prod(rest)
any(rest...)  = any(rest)
all(rest...)  = all(rest)

function reduce(op, itr)
    v = op()
    for x = itr
        v = op(v,x)
    end
    return v
end

max(itr)  = reduce(max, itr)
min(itr)  = reduce(min, itr)
sum(itr)  = reduce(sum, itr)
prod(itr) = reduce(prod, itr)
any(itr)  = reduce(any, itr)
all(itr)  = reduce(all, itr)
