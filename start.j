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

function !=(x, y)
    return !(x == y)
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

load("int32.j")
load("double.j")
load("complex.j")
load("list.j")
load("tree.j")
load("rand.j")
