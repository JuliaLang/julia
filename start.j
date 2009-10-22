function ref(t::Type, params...)
    return instantiate_type(t, params)
end

typealias Nullable Union[T,()]
typealias Index Int32
typealias Size  Int32

`-->` = (a,b)->Function[a,b]

function print(x::Any)
    # default print function, call builtin
    _print(x)
    return ()
end

function ref(t::Tuple, i::Index)
    return tupleref(t, unbox(i))
end

function ref(b::Buffer, i::Index)
    return box(typeof(b).parameters[1], bufferref(b, unbox(i)))
end

function ref(b::Buffer[Any], i::Index)
    return bufferref(b, unbox(i))
end

function set(b::Buffer, i::Index, x)
    bufferset(b, unbox(i), unbox(x))
    return x
end

function set(b::Buffer[Any], i::Index, x)
    bufferset(b, unbox(i), x)
    return x
end

function length(t::Tuple)
    return box(Size, tuplelen(t))
end

function length(b::Buffer)
    return b.length
end

function `!`(x::Bool)
    return eq_int32(unbox(x),unbox(0))
end

function `!`(x)
    return false
end

function assert(c)
    if !c
        error("Assertion failed.")
    end
    true
end

function `+`(x::Int32, y::Int32)
    return box(Int32, add_int32(unbox(x), unbox(y)))
end

function `-`(x::Int32, y::Int32)
    return box(Int32, sub_int32(unbox(x), unbox(y)))
end

function `-`(x::Int32)
    return box(Int32, neg_int32(unbox(x)))
end

function `*`(x::Int32, y::Int32)
    return box(Int32, mul_int32(unbox(x), unbox(y)))
end

function `/`(x::Int32, y::Int32)
    return box(Int32, div_int32(unbox(x), unbox(y)))
end

function `%`(x::Int32, y::Int32)
    return box(Int32, mod_int32(unbox(x), unbox(y)))
end

function `<=`(x::Int32, y::Int32)
    return lt_int32(unbox(x),unbox(y)) || eq_int32(unbox(x),unbox(y))
end

function `<`(x::Int32, y::Int32)
    return lt_int32(unbox(x),unbox(y))
end

function `>`(x::Int32, y::Int32)
    return lt_int32(unbox(y),unbox(x))
end

function `>=`(x::Int32, y::Int32)
    return (x>y) || eq_int32(unbox(x),unbox(y))
end

function `==`(x::Int32, y::Int32)
    return eq_int32(unbox(x),unbox(y))
end

function `+`(x::Double, y::Double)
    return box(Double, add_double(unbox(x), unbox(y)))
end

function `-`(x::Double, y::Double)
    return box(Double, sub_double(unbox(x), unbox(y)))
end

function `-`(x::Double)
    return box(Double, neg_double(unbox(x)))
end

function `*`(x::Double, y::Double)
    return box(Double, mul_double(unbox(x), unbox(y)))
end

function `/`(x::Double, y::Double)
    return box(Double, div_double(unbox(x), unbox(y)))
end

function `<=`(x::Double, y::Double)
    return lt_double(unbox(x),unbox(y)) || eq_double(unbox(x),unbox(y))
end

function `<`(x::Double, y::Double)
    return lt_double(unbox(x),unbox(y))
end

function `>`(x::Double, y::Double)
    return lt_double(unbox(y),unbox(x))
end

function `>=`(x::Double, y::Double)
    return (x>y) || eq_double(unbox(x),unbox(y))
end

function `==`(x::Double, y::Double)
    return eq_double(unbox(x),unbox(y))
end


type List[T]
    maxsize:: Size
    size:: Size
    offset:: Size
    data:: Buffer[T]
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
    seed = 0
    function rand()
        seed = (22695477*seed + 1) % 4294967296
        #interval_01 = seed / 4294967296.0
    end
    return rand
end

rand = make_rand(0)

