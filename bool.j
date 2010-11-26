## boolean conversions ##

convert(::Type{Bool}, x::Number) = (x!=0)

bool(x) = true
bool(x::Bool) = x
bool(x::Number) = convert(Bool, x)

sizeof(::Type{Bool}) = 1

## boolean operations ##

!(x::Bool) = eq_int(unbox8(x),trunc8(unbox32(0)))
==(x::Bool, y::Bool) = eq_int(unbox8(x),unbox8(y))

(~)(x::Bool) = !x
(&)(x::Bool, y::Bool) = (x&&y)
(|)(x::Bool, y::Bool) = (x||y)
($)(x::Bool, y::Bool) = (x!=y)

any(x::Bool)  = x
all(x::Bool)  = x
count(x::Bool) = (x == true ? 1 : 0)

any(x::Bool, y::Bool) = x || y ? true : false
all(x::Bool, y::Bool) = x && y ? true : false
count(x::Bool, y::Bool) = count(x) + count(y)
