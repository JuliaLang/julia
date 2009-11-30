typealias Nullable Union[`T,()]
typealias Index Int32
typealias Size  Int32

function print(x::Any)
    # default print function, call builtin
    _print(x)
    return ()
end

ref(t::Tuple, i::Index) = tupleref(t, unbox(i))

typealias Unboxable Union[Bool,
                          Int8,Uint8,Int16,Uint16,Int32,Uint32,Float,Double]
typealias UnboxedBuffer Union[Buffer[Bool],
                              Buffer[Int8],Buffer[Uint8],
                              Buffer[Int16],Buffer[Uint16],
                              Buffer[Int32],Buffer[Uint32],
                              Buffer[Float],Buffer[Double]]

ref(b::UnboxedBuffer, i::Index) = box(typeof(b).parameters[1], bufferref(b, unbox(i)))
ref(b::Buffer[`T], i::Index) = bufferref(b, unbox(i))

function set(b::UnboxedBuffer, i::Index, x::Unboxable)
    bufferset(b, unbox(i), unbox(x))
    return x
end

function set(b::Buffer[`T], i::Index, x)
    bufferset(b, unbox(i), x)
    return x
end

length(t::Tuple) = box(Size, tuplelen(t))
length(b::Buffer) = b.length

function buffer(elts::`T...)
    b = new(Buffer[T],length(elts))
    for i = 1:length(elts)
        b[i] = elts[i]
    end
    return b
end

function apply_op(op::Function, b1::Buffer, b2::Buffer)
    n = length(b1) > length(b2) ? length(b1) : length(b2)
    T = (length(b1) > 0 && length(b2) > 0) ? typeof(op(b1[1],b2[1])) : (length(b1) > 0 ? typeof(b1[1]) : typeof(b2[1]))
    b = new(Buffer[T],n)
    for i = 1:n
        b[i] = (length(b1) >= i && length(b2) >= i) ? op(b1[i],b2[i]) : (length(b1) >= i ? b1[i] : b2[i])
    end
    return b
end

function ==(b1::Buffer, b2::Buffer)
    if length(b1) != length(b2)
        return false
    end
    for i = 1:length(b1)
        if b1[i] != b2[i]
            return false
        end
    end
    return true
end

!(x::Bool) = eq_int32(unbox(x),unbox(0))
!(x) = false

function assert(c)
    if !c
        error("Assertion failed.")
    end
    true
end

!=(x, y) = !(x == y)

function signbit(x::Real)
    if x < 0
        return -1
    elseif x > 0
        return 1
    elseif 1.0/x < 0
        return -1
    end
    return 1
end

function signbit(x)
    if x < 0
        return -1
    elseif x > 0
        return 1
    end
    return 1
end

function sign(x::Scalar)
    if x < 0
        return -1
    elseif x > 0
        return 1
    end
    return 0
end

conj(x::Scalar) = x
transpose(x::Scalar) = x
ctranspose(x::Scalar) = conj(x)

load("int32.j")
load("double.j")
load("complex.j")
load("rational.j")
load("list.j")
load("tree.j")
load("rand.j")
