int8(x::Scalar) = box(Int8,to_int8(unbox(x)))
uint8(x::Scalar) = box(Uint8,to_uint8(unbox(x)))
int16(x::Scalar) = box(Int16,to_int16(unbox(x)))
uint16(x::Scalar) = box(Uint16,to_uint16(unbox(x)))
int32(x::Scalar) = box(Int32,to_int32(unbox(x)))
uint32(x::Scalar) = box(Uint32,to_uint32(unbox(x)))

div(x::Int32, y::Int32) = box(Int32, div_int32(unbox(x), unbox(y)))
(+)(x::Int32, y::Int32) = box(Int32, add_int32(unbox(x), unbox(y)))
(-)(x::Int32, y::Int32) = box(Int32, sub_int32(unbox(x), unbox(y)))
(-)(x::Int32) = box(Int32, neg_int32(unbox(x)))
(*)(x::Int32, y::Int32) = box(Int32, mul_int32(unbox(x), unbox(y)))
(/)(x::Int32, y::Int32) = double(x)/double(y)
(%)(x::Int32, y::Int32) = box(Int32, mod_int32(unbox(x), unbox(y)))

<=(x::Int32, y::Int32) = lt_int32(unbox(x),unbox(y)) || eq_int32(unbox(x),unbox(y))
< (x::Int32, y::Int32) = lt_int32(unbox(x),unbox(y))
> (x::Int32, y::Int32) = lt_int32(unbox(y),unbox(x))
>=(x::Int32, y::Int32) = (x>y) || eq_int32(unbox(x),unbox(y))
==(x::Int32, y::Int32) = eq_int32(unbox(x),unbox(y))

function gcd(a::Int, b::Int)
    while b != 0
        t = b
        b = a % b
        a = t
    end
    return a
end
