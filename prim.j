typealias Nullable[T] Union(T,())
typealias Index Int32
typealias Size  Int32

ref(t::Tuple, i::Index) = tupleref(t, i)
length(t::Tuple) = tuplelen(t)

function print(x)
    # default print function, call builtin
    _print(x)
    return ()
end

!(x::Bool) = eq_int(unbox8(x),unbox8(0))
!(x) = false
!=(x, y) = !(x == y)

# bootstrapping versions of operators needed by for loops
(-)(x::Int32) = boxsi32(neg_int(unbox32(x)))
(+)(x::Int32, y::Int32) = boxsi32(add_int(unbox32(x), unbox32(y)))
(-)(x::Int32, y::Int32) = boxsi32(sub_int(unbox32(x), unbox32(y)))
(*)(x::Int32, y::Int32) = boxsi32(mul_int(unbox32(x), unbox32(y)))
div(x::Int32, y::Int32) = boxsi32(sdiv_int(unbox32(x), unbox32(y)))
< (x::Int32, y::Int32) = slt_int(unbox32(x),unbox32(y))
==(x::Int32, y::Int32) = eq_int(unbox32(x),unbox32(y))
<=(x::Int32, y::Int32) = slt_int(unbox32(x),unbox32(y)) || eq_int(unbox32(x),unbox32(y))

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
