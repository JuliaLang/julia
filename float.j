float64(x::Scalar) = box(Float64,to_float64(unbox(x)))
truncate(x::Real) = box(Int32,_truncate(unbox(x)))

def_binary_op(Float64, {Int8, Uint8, Int16, Uint16, Int32, Uint32}, +, add_float64)
def_binary_op(Float64, {Int8, Uint8, Int16, Uint16, Int32, Uint32}, -, sub_float64)
def_binary_op(Float64, {Int8, Uint8, Int16, Uint16, Int32, Uint32}, *, mul_float64)
def_binary_op(Float64, {Int8, Uint8, Int16, Uint16, Int32, Uint32}, /, div_float64)
#(+)(x::Float64, y::Float64) = box(Float64, add_float64(unbox(x), unbox(y)))
#(-)(x::Float64, y::Float64) = box(Float64, sub_float64(unbox(x), unbox(y)))
#(*)(x::Float64, y::Float64) = box(Float64, mul_float64(unbox(x), unbox(y)))
#(/)(x::Float64, y::Float64) = box(Float64, div_float64(unbox(x), unbox(y)))

(-)(x::Float64) = box(Float64, neg_float64(unbox(x)))

#<=(x::Float64, y::Float64) = lt_float64(unbox(x),unbox(y)) || eq_float64(unbox(x),unbox(y))
#< (x::Float64, y::Float64) = lt_float64(unbox(x),unbox(y))
#> (x::Float64, y::Float64) = lt_float64(unbox(y),unbox(x))
#>=(x::Float64, y::Float64) = (x>y) || eq_float64(unbox(x),unbox(y))
#==(x::Float64, y::Float64) = eq_float64(unbox(x),unbox(y))
def_compare_ops(Float64, {Int8, Uint8, Int16, Uint16, Int32, Uint32}, lt_float64, eq_float64)

!=(x::Float64, y::Float64) = ne_float64(unbox(x),unbox(y))

Float64.convert(x::Int8) = float64(x)
Float64.convert(x::Uint8) = float64(x)
Float64.convert(x::Int16) = float64(x)
Float64.convert(x::Uint16) = float64(x)
Float64.convert(x::Int32) = float64(x)
Float64.convert(x::Uint32) = float64(x)

Inf = 1/0
NaN = 0/0
