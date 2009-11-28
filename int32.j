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
    return double(x)/double(y)
end

function div(x::Int32, y::Int32)
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

function int32(x::Scalar)
    return box(Int32,to_int32(unbox(x)))
end

function uint32(x::Scalar)
    return box(Uint32,to_uint32(unbox(x)))
end

function gcd(a::Int32, b::Int32)
    while b != 0
        t = b
        b = a % b
        a = t
    end
    return a
end
