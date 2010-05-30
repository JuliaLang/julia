!(x::Bool) = eq_int(unbox8(x),trunc8(unbox32(0)))
==(x::Bool, y::Bool) = eq_int(unbox8(x),unbox8(y))

bool(x) = true
bool(x::Bool) = x
bool(x::Number) = x != 0
bool(x::Array) = numel(x) != 0
bool(x::Tuple) = length(x) != 0
