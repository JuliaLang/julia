convert(::Type{Bool}, x::Number) = (x!=0)

!(x::Bool) = eq_int(unbox8(x),trunc8(unbox32(0)))
==(x::Bool, y::Bool) = eq_int(unbox8(x),unbox8(y))

(~)(x::Bool) = !x
(&)(x::Bool, y::Bool) = (x&&y)
(|)(x::Bool, y::Bool) = (x||y)
($)(x::Bool, y::Bool) = !eq_int(xor_int(unbox8(x),unbox8(y)),unbox8(0))

bool(x) = true
bool(x::Bool) = x
bool(x::Number) = convert(Bool, x)
