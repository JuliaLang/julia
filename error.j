error(e::Exception) = throw(e)
error{E<:Exception}(::Type{E}) = throw(E())
error(s::Union(Latin1String,UTF8String)) = throw(ErrorException(s))
error(s...) = error(print_to_string(print, s...))

assert(b::Bool) = b ? true : error("Assertion failed.")
assert(B::Tensor{Bool}) = assert(all(B))
