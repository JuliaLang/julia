# This file is a part of Julia. License is MIT: http://julialang.org/license

## hashing a single value ##

hash(x::Any) = hash(x, zero(UInt))
hash(w::WeakRef, h::UInt) = hash(w.value, h)

## hashing general objects ##

hash(x::ANY, h::UInt) = 3*object_id(x) - h

## core data hashing functions ##

function hash_64_64(n::UInt64)
    local a::UInt64 = n
    a = ~a + a << 21
    a =  a $ a >> 24
    a =  a + a << 3 + a << 8
    a =  a $ a >> 14
    a =  a + a << 2 + a << 4
    a =  a $ a >> 28
    a =  a + a << 31
    return a
end

function hash_64_32(n::UInt64)
    local a::UInt64 = n
    a = ~a + a << 18
    a =  a $ a >> 31
    a =  a * 21
    a =  a $ a >> 11
    a =  a + a << 6
    a =  a $ a >> 22
    return a % UInt32
end

function hash_32_32(n::UInt32)
    local a::UInt32 = n
    a = a + 0x7ed55d16 + a << 12
    a = a $ 0xc761c23c $ a >> 19
    a = a + 0x165667b1 + a << 5
    a = a + 0xd3a2646c $ a << 9
    a = a + 0xfd7046c5 + a << 3
    a = a $ 0xb55a4f09 $ a >> 16
    return a
end

if UInt === UInt64
    hash_uint64(x::UInt64) = hash_64_64(x)
    hash_uint(x::UInt)     = hash_64_64(x)
else
    hash_uint64(x::UInt64) = hash_64_32(x)
    hash_uint(x::UInt)     = hash_32_32(x)
end

## symbol & expression hashing ##

hash(x::Symbol, h::UInt) = 3*object_id(x) - h
if UInt === UInt64
    hash(x::Expr, h::UInt) = hash(x.args, hash(x.head, h + 0x83c7900696d26dc6))
else
    hash(x::Expr, h::UInt) = hash(x.args, hash(x.head, h + 0x96d26dc6))
end


# hashing ranges by component at worst leads to collisions for very similar ranges
const hashr_seed = UInt === UInt64 ? 0x80707b6821b70087 : 0x21b70087
function hash(r::Range, h::UInt)
    h += hashr_seed
    h = hash(first(r), h)
    h = hash(step(r), h)
    h = hash(last(r), h)
end
