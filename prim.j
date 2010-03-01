typealias Nullable[T] Union(T,())
typealias Index Int32
typealias Size  Int32

typealias Unboxable Union(Bool,Int8,Uint8,Int16,Uint16,Int32,Uint32,Float32,Float64)
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

# fallback definitions for emulating N-arg operators with 2-arg definitions

(*)() = 1
(*)(x::Tensor) = x
(*)(x,y) = error("No matching method for function *")
(*)(a,b,c) = (*)((*)(a,b),c)
(*)(a,b,c,d) = (*)((*)((*)(a,b),c),d)
(*)(a,b,c,d,e) = (*)((*)((*)((*)(a,b),c),d),e)
function (*)(x1, x2, xs...)
    accum = x1*x2
    n = length(xs)
    for i=1:n
        accum = accum * xs[i]
    end
    accum
end

(+)() = 0
(+)(x::Tensor) = x
(+)(x,y) = error("No matching method for function +")
(+)(a,b,c) = (+)((+)(a,b),c)
(+)(a,b,c,d) = (+)((+)((+)(a,b),c),d)
(+)(a,b,c,d,e) = (+)((+)((+)((+)(a,b),c),d),e)
function (+)(x1, x2, xs...)
    accum = x1+x2
    n = length(xs)
    for i=1:n
        accum = accum + xs[i]
    end
    accum
end

# iterating over tuples
start(t::Tuple) = 1
done(t::Tuple, i) = (i > length(t))
next(t::Tuple, i) = (t[i], i+1)
