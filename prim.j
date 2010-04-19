typealias Nullable[T] Union(T,())
typealias Index Int32
typealias Size  Int32

ref(t::Tuple, i::Index) = tupleref(t, i)
length(t::Tuple) = tuplelen(t)

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
(*)(a,b,c) = (*)((*)(a,b),c)
(*)(a,b,c,d) = (*)((*)((*)(a,b),c),d)
(*)(a,b,c,d,e) = (*)((*)((*)((*)(a,b),c),d),e)
function (*)(x1, x2, x3, xs...)
    accum = (*)((*)(x1,x2),x3)
    n = length(xs)
    for i=1:n
        accum = accum * xs[i]
    end
    accum
end

(+)() = 0
(+)(x::Tensor) = x
(+)(a,b,c) = (+)((+)(a,b),c)
(+)(a,b,c,d) = (+)((+)((+)(a,b),c),d)
(+)(a,b,c,d,e) = (+)((+)((+)((+)(a,b),c),d),e)
function (+)(x1, x2, x3, xs...)
    accum = (+)((+)(x1,x2),x3)
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

map(f, t::()) = ()
map(f, t::Tuple) = maptuple(f, t...)
maptuple(f, first, rest...) = tuple(f(first), maptuple(f, rest...)...)
maptuple(f) = ()

ref(t::Tuple, r::Range) = accumtuple(t, r, start(r))
function accumtuple(t::Tuple, r::Range, i, elts...)
    if (done(r, i))
        return elts
    end
    accumtuple(t, r, i+r.step, elts..., t[i])
end
ref(t::Tuple, r::RangeFrom) = t[range(r.start,r.step,length(t))]
ref(t::Tuple, r::RangeTo)   = t[range(1,r.step,r.stop)]
ref(t::Tuple, r::RangeBy)   = t[range(1,r.step,length(t))]

function ==(t1::Tuple, t2::Tuple)
    if length(t1) != length(t2)
        return false
    end
    for i = 1:length(t1)
        if t1[i] != t2[i]
            return false
        end
    end
    return true
end

function append(t1::Tuple, ts::Tuple...)
    if (length(ts)==0)
        return t1
    end
    return tuple(t1..., append(ts...)...)
end

print(x...) = for i=x; print(i); end

expr(hd::Symbol, args...)  = Expr{hd, args}

function cell_literal(xs...)
    n = length(xs)
    a = Array[Any,1].new(n)
    for i=1:n
        arrayset(a,i,xs[i])
    end
    a
end
