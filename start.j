typealias Nullable Union[`T,()]
typealias Index Int32
typealias Size  Int32

function print(x::Any)
    # default print function, call builtin
    _print(x)
    return ()
end

function ref(t::Tuple, i::Index)
    return tupleref(t, unbox(i))
end

typealias Unboxable Union[Bool,
                          Int8,Uint8,Int16,Uint16,Int32,Uint32,Float,Double]
typealias UnboxedBuffer Union[Buffer[Bool],
                              Buffer[Int8],Buffer[Uint8],
                              Buffer[Int16],Buffer[Uint16],
                              Buffer[Int32],Buffer[Uint32],
                              Buffer[Float],Buffer[Double]]

function ref(b::UnboxedBuffer, i::Index)
    return box(typeof(b).parameters[1], bufferref(b, unbox(i)))
end

function ref(b::Buffer[`T], i::Index)
    return bufferref(b, unbox(i))
end

function set(b::UnboxedBuffer, i::Index, x::Unboxable)
    bufferset(b, unbox(i), unbox(x))
    return x
end

function set(b::Buffer[`T], i::Index, x)
    bufferset(b, unbox(i), x)
    return x
end

function length(t::Tuple)
    return box(Size, tuplelen(t))
end

function length(b::Buffer)
    return b.length
end

function !(x::Bool)
    return eq_int32(unbox(x),unbox(0))
end

function !(x)
    return false
end

function assert(c)
    if !c
        error("Assertion failed.")
    end
    true
end

function (+)(x::Int32, y::Int32)
    return box(Int32, add_int32(unbox(x), unbox(y)))
end

function (-)(x::Int32, y::Int32)
    return box(Int32, sub_int32(unbox(x), unbox(y)))
end

function -(x::Int32)
    return box(Int32, neg_int32(unbox(x)))
end

function *(x::Int32, y::Int32)
    return box(Int32, mul_int32(unbox(x), unbox(y)))
end

function /(x::Int32, y::Int32)
    return box(Int32, div_int32(unbox(x), unbox(y)))
end

function %(x::Int32, y::Int32)
    return box(Int32, mod_int32(unbox(x), unbox(y)))
end

function <=(x::Int32, y::Int32)
    return lt_int32(unbox(x),unbox(y)) || eq_int32(unbox(x),unbox(y))
end

function <(x::Int32, y::Int32)
    return lt_int32(unbox(x),unbox(y))
end

function >(x::Int32, y::Int32)
    return lt_int32(unbox(y),unbox(x))
end

function >=(x::Int32, y::Int32)
    return (x>y) || eq_int32(unbox(x),unbox(y))
end

function ==(x::Int32, y::Int32)
    return eq_int32(unbox(x),unbox(y))
end

function !=(x, y)
    return !(x == y)
end

function (+)(x::Double, y::Double)
    return box(Double, add_double(unbox(x), unbox(y)))
end

function (-)(x::Double, y::Double)
    return box(Double, sub_double(unbox(x), unbox(y)))
end

function -(x::Double)
    return box(Double, neg_double(unbox(x)))
end

function *(x::Double, y::Double)
    return box(Double, mul_double(unbox(x), unbox(y)))
end

function /(x::Double, y::Double)
    return box(Double, div_double(unbox(x), unbox(y)))
end

function <=(x::Double, y::Double)
    return lt_double(unbox(x),unbox(y)) || eq_double(unbox(x),unbox(y))
end

function <(x::Double, y::Double)
    return lt_double(unbox(x),unbox(y))
end

function >(x::Double, y::Double)
    return lt_double(unbox(y),unbox(x))
end

function >=(x::Double, y::Double)
    return (x>y) || eq_double(unbox(x),unbox(y))
end

function ==(x::Double, y::Double)
    return eq_double(unbox(x),unbox(y))
end

function !=(x::Double, y::Double)
    return ne_double(unbox(x),unbox(y))
end

function sign(x::Real)
    if 1.0/x < 0
        return -1
    elseif x == 0
        return 0
    else
        return 1
    end
end

function sign(x::Scalar)
    if x < 0
        return -1
    elseif x == 0
        return 0
    else
        return 1
    end
end

# explicit scalar conversions
function int32(x::Scalar)
    return box(Int32,to_int32(unbox(x)))
end

function uint32(x::Scalar)
    return box(Uint32,to_uint32(unbox(x)))
end

function double(x::Scalar)
    return box(Double,to_double(unbox(x)))
end

conversion x::Int8-->Double
    return double(x)
end

conversion x::Int32-->Double
    return double(x)
end

conversion x::Uint32-->Double
    return double(x)
end


type List[`T]
    maxsize:: Size
    size:: Size
    offset:: Size
    data:: Buffer[`T]
end

function list(elts...)
    n = length(elts)
    if n < 4
        maxsize = 4
    else
        maxsize = n
    end
    data = new(Buffer[Any], maxsize)
    for i = 1:n
        data[i] = elts[i]
    end
    return new(List[Any], maxsize, n, 0, data)
end

function ref(l::List, i::Index)
    if i > l.size
        error("Out of bounds")
    end
    return l.data[i+l.offset]
end

function set(l::List, i::Index, elt)
    if i > l.size
        error("Out of bounds")
    end
    l.data[i+l.offset] = elt
end

function length(l::List)
    return l.size
end

function print(l::List)
    print("{")
    for i=1:length(l)
        if i > 1
            print(", ")
        end
        print(l[i])
    end
    print("}")
end

function grow(a::List, inc::Size)
    if (inc == 0)
        return a
    end
    if (a.size + inc > a.maxsize - a.offset)
        newsize = a.maxsize*2
        while (a.size+inc > newsize-a.offset)
            newsize *= 2
        end
        newlist = new(Buffer[Any], newsize)
        a.maxsize = newsize
        for i=1:a.size
            newlist[i+a.offset] = a.data[i+a.offset]
        end
        a.data = newlist
    end
    a.size += inc
    return a
end

function push(a::List, item)
    grow(a, 1)
    a[a.size] = item
    return a
end

function pop(a::List)
    if (a.size == 0)
        error("pop: List is empty")
    end
    item = a[a.size]
    a[a.size] = ()
    a.size -= 1
    return item
end

# http://en.wikipedia.org/wiki/Linear_congruential_generator
function make_rand(seed)
    function rand()
        seed = (22695477*seed + 1) % 4294967296
        #interval_01 = seed / 4294967296.0
    end
    return rand
end

rand = make_rand(12345)

type Complex[`T] < Scalar
    re::T
    im::T
end

function complex(re::`T, im::`T)
    return new(Complex[T], re, im)
end

function re(z::Complex)
    return z.re
end

function im(z::Complex)
    return z.im
end

function (+)(z::Complex, w::Complex)
    return complex(z.re + w.re, z.im + w.im)
end

function (-)(z::Complex, w::Complex)
    return complex(z.re - w.re, z.im - w.im)
end

function -(z::Complex)
    return complex(-z.re, -z.im)
end

function *(z::Complex, w::Complex)
    return complex(z.re*w.re - z.im*w.im, z.re*w.im + z.im*w.re)
end

function /(z::Complex, x::Real)
    return complex(z.re/x, z.im/x)
end

function ctranspose(z::Complex)
    return complex(z.re,-z.im)
end

function /(z::Complex, w::Complex)
    return z*w'/(w.re*w.re + w.im*w.im)
end

conversion x::Real-->Complex
    return complex(x,0)
end

function print(c::Complex)
    print(re(c))
    i = im(c)
    if sign(i) == -1
        i = -i
        print(" - ")
    else
        print(" + ")
    end
    print(i)
    print("i")
end

type Node[`T]
    data::T
    left::Nullable[Node[T]]
    right::Nullable[Node[T]]
end

typealias Tree Nullable[Node[`T]]
