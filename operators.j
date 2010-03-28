macro def_binary_op(bigtype, smallertypes, opname, prim)
    st = smallertypes.args
    `begin
         function ($opname)(x::($bigtype), y::($bigtype))
             return box($bigtype, ($prim)(unbox(x), unbox(y)))
         end
         function ($opname)(x::($bigtype), y::Union($(st...)))
             return box($bigtype, ($prim)(unbox(x),
                                          unbox(convert(y,$bigtype))))
         end
         function ($opname)(y::Union($(st...)), x::($bigtype))
             return box($bigtype, ($prim)(unbox(convert(y,$bigtype)),
                                          unbox(x)))
         end
     end
end

macro def_compare_ops(bigtype, smallertypes, lt, eq)
    st = smallertypes.args
    bt = bigtype
    `begin
        <=(x::($bt), y::($bt)) = ($lt)(unbox(x),unbox(y)) || ($eq)(unbox(x),unbox(y))
        < (x::($bt), y::($bt)) = ($lt)(unbox(x),unbox(y))
        > (x::($bt), y::($bt)) = ($lt)(unbox(y),unbox(x))
        >=(x::($bt), y::($bt)) = (x>y) || ($eq)(unbox(x),unbox(y))
        ==(x::($bt), y::($bt)) = ($eq)(unbox(x),unbox(y))
        
        <=(x::($bt), y::Union($(st...))) = (x <= convert(y,$bt))
        < (x::($bt), y::Union($(st...))) = (x <  convert(y,$bt))
        > (x::($bt), y::Union($(st...))) = (x >  convert(y,$bt))
        >=(x::($bt), y::Union($(st...))) = (x >= convert(y,$bt))
        ==(x::($bt), y::Union($(st...))) = (x == convert(y,$bt))
        
        <=(y::Union($(st...)), x::($bt)) = (convert(y,$bt) <= x)
        < (y::Union($(st...)), x::($bt)) = (convert(y,$bt) <  x)
        > (y::Union($(st...)), x::($bt)) = (convert(y,$bt) >  x)
        >=(y::Union($(st...)), x::($bt)) = (convert(y,$bt) >= x)
        ==(y::Union($(st...)), x::($bt)) = (convert(y,$bt) == x)
     end
end

max() = -Inf
min() = +Inf
sum() = 0
prod() = 1
any() = false
all() = true

macro def_reduce_op(op)
    `begin
        ($op)(x::Scalar) = x
        function ($op)(itr)
            v = ($op)()
            for x = itr
                v = ($op)(v,x)
            end
            return v
        end
        ($op)(rest...) = ($op)(rest)
    end
end

def_reduce_op(max)
def_reduce_op(min)
def_reduce_op(sum)
def_reduce_op(prod)
def_reduce_op(any)
def_reduce_op(all)
