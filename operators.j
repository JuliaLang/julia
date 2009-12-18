macro def_binary_op(bigtype, smallertypes, opname, prim)
    mixed = map(t->
                `begin
                   function ($opname)(x::($bigtype), y::($t))
                     return box($bigtype, ($prim)(unbox(x),
                                                  unbox(convert(y,$bigtype))))
                   end
                   function ($opname)(y::($t), x::($bigtype))
                     return box($bigtype, ($prim)(unbox(convert(y,$bigtype)),
                                                  unbox(x)))
                   end
                 end,
                smallertypes.args)
    `begin
         function ($opname)(x::($bigtype), y::($bigtype))
             return box($bigtype, ($prim)(unbox(x), unbox(y)))
         end
         $(mixed...)
     end
end
