typealias Nullable[T] Union(T,())
typealias Index Int32
typealias Size  Int32

typealias Unboxable Union(Bool,
                          Int8,Uint8,Int16,Uint16,Int32,Uint32,Float32,Float64)
typealias UnboxedBuffer Union(Buffer[Bool],
                              Buffer[Int8],Buffer[Uint8],
                              Buffer[Int16],Buffer[Uint16],
                              Buffer[Int32],Buffer[Uint32],
                              Buffer[Float32],Buffer[Float64])

ref(t::Tuple, i::Index) = tupleref(t, unbox(i))
length(t::Tuple) = box(Size, tuplelen(t))

function print(x)
    # default print function, call builtin
    _print(x)
    return ()
end

!(x::Bool) = eq_int32(unbox(x),unbox(0))
!(x) = false
!=(x, y) = !(x == y)

# bootstrapping versions of operators needed by for loops
(-)(x::Int32) = box(Int32, neg_int32(unbox(x)))
(+)(x::Int32, y::Int32) = box(Int32, add_int32(unbox(x), unbox(y)))
(-)(x::Int32, y::Int32) = box(Int32, sub_int32(unbox(x), unbox(y)))
(*)(x::Int32, y::Int32) = box(Int32, mul_int32(unbox(x), unbox(y)))
div(x::Int32, y::Int32) = box(Int32, div_int32(unbox(x), unbox(y)))
< (x::Int32, y::Int32) = lt_int32(unbox(x),unbox(y))
==(x::Int32, y::Int32) = eq_int32(unbox(x),unbox(y))
<=(x::Int32, y::Int32) = lt_int32(unbox(x),unbox(y)) || eq_int32(unbox(x),unbox(y))
