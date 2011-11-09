## boolean conversions ##

convert(::Type{Bool}, x::Number) = (x!=0)

bool(x) = true
bool(x::Bool) = x
bool(x::Number) = convert(Bool, x)

sizeof(::Type{Bool}) = 1

## boolean operations ##

!(x::Bool) = eq_int(unbox8(x),trunc8(0))
isequal(x::Bool, y::Bool) = eq_int(unbox8(x),unbox8(y))

(~)(x::Bool) = !x
(&)(x::Bool, y::Bool) = eq_int(and_int(unbox8(x),unbox8(y)),trunc8(1))
(|)(x::Bool, y::Bool) = eq_int( or_int(unbox8(x),unbox8(y)),trunc8(1))
($)(x::Bool, y::Bool) = (x!=y)

any() = false
all() = true
count() = 0

any(x::Bool)  = x
all(x::Bool)  = x
count(x::Bool) = (x ? 1 : 0)

any(x::Bool, y::Bool) = x | y
all(x::Bool, y::Bool) = x & y
count(x::Bool, y::Bool) = count(x) + count(y)

count(x::Int, y::Bool) = x + count(y)
count(x::Bool, y::Int) = count(x) + y
