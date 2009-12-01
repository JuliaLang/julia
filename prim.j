typealias Nullable Union[`T,()]
typealias Index Int32
typealias Size  Int32

typealias Unboxable Union[Bool,
                          Int8,Uint8,Int16,Uint16,Int32,Uint32,Single,Double]
typealias UnboxedBuffer Union[Buffer[Bool],
                              Buffer[Int8],Buffer[Uint8],
                              Buffer[Int16],Buffer[Uint16],
                              Buffer[Int32],Buffer[Uint32],
                              Buffer[Single],Buffer[Double]]

ref(t::Tuple, i::Index) = tupleref(t, unbox(i))
length(t::Tuple) = box(Size, tuplelen(t))

function print(x::Any)
    # default print function, call builtin
    _print(x)
    return ()
end

!(x::Bool) = eq_int32(unbox(x),unbox(0))
!(x) = false
!=(x, y) = !(x == y)
