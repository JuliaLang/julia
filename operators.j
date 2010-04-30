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
sum(itr)  = reduce(+,   itr)
prod(itr) = reduce(*,   itr)
any(itr)  = reduce(any, itr)
all(itr)  = reduce(all, itr)

promote_table(Int32, Float64) => Float64

function promote_type(t::Type, s::Type)
    if subtype(t,s) || subtype(s,t)
        error("no method")
    end
    if method_exists(promote_table,(t,s))
        return promote_table(t,s)
    else
        return promote_table(s,t)
    end
end

promote() = ()
promote(x) = (x,)
promote{T,S}(x::T, y::S) =
    (convert(x,promote_type(T,S)), convert(y,promote_type(T,S)))
function promote(xs...)
    t = promote_type(typeof(xs[1]), typeof(xs[2]))
    for i=3:length(xs)
        t = promote_type(t,xs[i])
    end
    map(x->convert(x,t), xs)
end
