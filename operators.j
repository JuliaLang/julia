macro def_binary_op(bigtype, smallertypes, opname, prim)
    st = smallertypes.args
    `begin
         function ($opname)(x::($bigtype), y::($bigtype))
             return box($bigtype, ($prim)(unbox(x), unbox(y)))
         end
         function ($opname)(x::($bigtype), y::Union[$(st...)])
             return box($bigtype, ($prim)(unbox(x),
                                          unbox(convert(y,$bigtype))))
         end
         function ($opname)(y::Union[$(st...)], x::($bigtype))
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
        
        <=(x::($bt), y::Union[$(st...)]) = (x <= convert(y,$bt))
        < (x::($bt), y::Union[$(st...)]) = (x <  convert(y,$bt))
        > (x::($bt), y::Union[$(st...)]) = (x >  convert(y,$bt))
        >=(x::($bt), y::Union[$(st...)]) = (x >= convert(y,$bt))
        ==(x::($bt), y::Union[$(st...)]) = (x == convert(y,$bt))
        
        <=(y::Union[$(st...)], x::($bt)) = (convert(y,$bt) <= x)
        < (y::Union[$(st...)], x::($bt)) = (convert(y,$bt) <  x)
        > (y::Union[$(st...)], x::($bt)) = (convert(y,$bt) >  x)
        >=(y::Union[$(st...)], x::($bt)) = (convert(y,$bt) >= x)
        ==(y::Union[$(st...)], x::($bt)) = (convert(y,$bt) == x)
     end
end
