## reduce operations ##

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

## promotions ##

function promote_type{T,S}(::Type{T}, ::Type{S})
    # print("promote_type: ",T,", ",S,"\n")
    if T <: S
        return T
    end
    if S <: T
        return S
    end
    if method_exists(promote_rule,(T,S))
        return promote_rule(T,S)
    elseif method_exists(promote_rule,(S,T))
        return promote_rule(S,T)
    else
        error(strcat("no promotion exists for ",string(T)," and ",string(S)))
    end
end

promote() = ()
promote(x) = (x,)
function promote{T,S}(x::T, y::S)
    # print("promote: ",T,", ",S,"\n")
    R = promote_type(T,S)
    (convert(R,x), convert(R,y))
end
function promote{T,S}(x::T, y::S, zs...)
    R = promote_type(T,S)
    for z = zs
        R = promote_type(R,typeof(z))
    end
    map(x->convert(R,x), tuple(x,y,zs...))
end
