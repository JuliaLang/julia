error(s::Union(Latin1String,UTF8String)) = throw(ErrorException(s))
error(s...) = error(print_to_string(print, s...))
assert(c) = c ? true : error("Assertion failed.")
assertall(c) = assert(all(c))
