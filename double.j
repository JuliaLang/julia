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

function double(x::Scalar)
    return box(Double,to_double(unbox(x)))
end

function truncate(x::Real)
    return box(Int32,_truncate(unbox(x)))
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
