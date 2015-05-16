# This file is a part of Julia. License is MIT: http://julialang.org/license

## hashing small, built-in numeric types
## for a system image built without floating point support

hx(a::UInt64, b::UInt64, h::UInt) = hash_uint64(3a + b - h)

hash(x::UInt64,  h::UInt) = hx(x, x, h)
hash(x::Int64,   h::UInt) = hx(reinterpret(UInt64,abs(x)), reinterpret(UInt64,x), h)
hash(x::Union(Bool,Char,Int8,UInt8,Int16,UInt16,Int32,UInt32), h::UInt) = hash(Int64(x), h)
