double(x::Scalar) = box(Double,to_double(unbox(x)))
truncate(x::Real) = box(Int32,_truncate(unbox(x)))

def_binary_op(Double, {Int8, Uint8, Int16, Uint16, Int32, Uint32}, +,add_double)
def_binary_op(Double, {Int8, Uint8, Int16, Uint16, Int32, Uint32}, -,sub_double)
def_binary_op(Double, {Int8, Uint8, Int16, Uint16, Int32, Uint32}, *,mul_double)
def_binary_op(Double, {Int8, Uint8, Int16, Uint16, Int32, Uint32}, /,div_double)
#(+)(x::Double, y::Double) = box(Double, add_double(unbox(x), unbox(y)))
#(-)(x::Double, y::Double) = box(Double, sub_double(unbox(x), unbox(y)))
#(*)(x::Double, y::Double) = box(Double, mul_double(unbox(x), unbox(y)))
#(/)(x::Double, y::Double) = box(Double, div_double(unbox(x), unbox(y)))

(-)(x::Double) = box(Double, neg_double(unbox(x)))

#<=(x::Double, y::Double) = lt_double(unbox(x),unbox(y)) || eq_double(unbox(x),unbox(y))
#< (x::Double, y::Double) = lt_double(unbox(x),unbox(y))
#> (x::Double, y::Double) = lt_double(unbox(y),unbox(x))
#>=(x::Double, y::Double) = (x>y) || eq_double(unbox(x),unbox(y))
#==(x::Double, y::Double) = eq_double(unbox(x),unbox(y))
def_compare_ops(Double, {Int8, Uint8, Int16, Uint16, Int32, Uint32}, lt_double, eq_double)

!=(x::Double, y::Double) = ne_double(unbox(x),unbox(y))

function Double.convert(x::Int8)
    return double(x)
end
function Double.convert(x::Uint8)
    return double(x)
end
function Double.convert(x::Int16)
    return double(x)
end
function Double.convert(x::Uint16)
    return double(x)
end
function Double.convert(x::Int32)
    return double(x)
end
function Double.convert(x::Uint32)
    return double(x)
end
