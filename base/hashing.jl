# This file is a part of Julia. License is MIT: https://julialang.org/license

## hashing a single value ##

"""
    hash(x[, h::UInt]) -> UInt

Compute an integer hash code such that `isequal(x,y)` implies `hash(x)==hash(y)`. The
optional second argument `h` is another hash code to be mixed with the result.

New types should implement the 2-argument form, typically by calling the 2-argument `hash`
method recursively in order to mix hashes of the contents with each other (and with `h`).
Typically, any type that implements `hash` should also implement its own [`==`](@ref) (hence
[`isequal`](@ref)) to guarantee the property mentioned above.

The hash value may change when a new Julia process is started.

```jldoctest
julia> a = hash(10)
0x95ea2955abd45275

julia> hash(10, a) # only use the output of another hash function as the second argument
0xd42bad54a8575b16
```

See also: [`objectid`](@ref), [`Dict`](@ref), [`Set`](@ref).
"""
hash(x::Any) = hash(x, zero(UInt))
hash(w::WeakRef, h::UInt) = hash(w.value, h)

# Types can't be deleted, so marking as total allows the compiler to look up the hash
hash(T::Type, h::UInt) = hash_uint(3h - @assume_effects :total ccall(:jl_type_hash, UInt, (Any,), T))

## hashing general objects ##

hash(@nospecialize(x), h::UInt) = hash_uint(3h - objectid(x))

hash(x::Symbol) = objectid(x)

## core data hashing functions ##

function hash_64_64(n::UInt64)
    a::UInt64 = n
    a = ~a + a << 21
    a =  a ⊻ a >> 24
    a =  a + a << 3 + a << 8
    a =  a ⊻ a >> 14
    a =  a + a << 2 + a << 4
    a =  a ⊻ a >> 28
    a =  a + a << 31
    return a
end

function hash_64_32(n::UInt64)
    a::UInt64 = n
    a = ~a + a << 18
    a =  a ⊻ a >> 31
    a =  a * 21
    a =  a ⊻ a >> 11
    a =  a + a << 6
    a =  a ⊻ a >> 22
    return a % UInt32
end

function hash_32_32(n::UInt32)
    a::UInt32 = n
    a = a + 0x7ed55d16 + a << 12
    a = a ⊻ 0xc761c23c ⊻ a >> 19
    a = a + 0x165667b1 + a << 5
    a = a + 0xd3a2646c ⊻ a << 9
    a = a + 0xfd7046c5 + a << 3
    a = a ⊻ 0xb55a4f09 ⊻ a >> 16
    return a
end

if UInt === UInt64
    hash_uint64(x::UInt64) = hash_64_64(x)
    hash_uint(x::UInt)     = hash_64_64(x)
else
    hash_uint64(x::UInt64) = hash_64_32(x)
    hash_uint(x::UInt)     = hash_32_32(x)
end

## efficient value-based hashing of integers ##

hash(x::Int64,  h::UInt) = hash_uint64(bitcast(UInt64, x)) - 3h
hash(x::UInt64, h::UInt) = hash_uint64(x) - 3h
hash(x::Union{Bool,Int8,UInt8,Int16,UInt16,Int32,UInt32}, h::UInt) = hash(Int64(x), h)

function hash_integer(n::Integer, h::UInt)
    h ⊻= hash_uint((n % UInt) ⊻ h)
    n = abs(n)
    n >>>= sizeof(UInt) << 3
    while n != 0
        h ⊻= hash_uint((n % UInt) ⊻ h)
        n >>>= sizeof(UInt) << 3
    end
    return h
end

## symbol & expression hashing ##

if UInt === UInt64
    hash(x::Expr, h::UInt) = hash(x.args, hash(x.head, h + 0x83c7900696d26dc6))
    hash(x::QuoteNode, h::UInt) = hash(x.value, h + 0x2c97bf8b3de87020)
else
    hash(x::Expr, h::UInt) = hash(x.args, hash(x.head, h + 0x96d26dc6))
    hash(x::QuoteNode, h::UInt) = hash(x.value, h + 0x469d72af)
end

## hashing strings ##

const memhash = UInt === UInt64 ? :memhash_seed : :memhash32_seed
const memhash_seed = UInt === UInt64 ? 0x71e729fd56419c81 : 0x56419c81

@assume_effects :total function hash(s::String, h::UInt)
    h += memhash_seed
    ccall(memhash, UInt, (Ptr{UInt8}, Csize_t, UInt32), s, sizeof(s), h % UInt32) + h
end
