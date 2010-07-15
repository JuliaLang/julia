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

## promotions ##

promote_type{T}(::Type{T}, ::Type{T}) = T

function promote_type{T,S}(::Type{T}, ::Type{S})
    # print("promote_type: ",T,", ",S,"\n")
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
    #R = promote_type(T,S)
    # print("= ", R,"\n")
    (convert(promote_type(T,S),x), convert(promote_type(T,S),y))
end
function promote{T,S}(x::T, y::S, zs...)
    R = promote_type(T,S)
    for z = zs
        R = promote_type(R,typeof(z))
    end
    map(x->convert(R,x), tuple(x,y,zs...))
end
