# This file is a part of Julia. License is MIT: https://julialang.org/license

## hashing a single value ##

"""
    hash(x[, h::UInt])

Compute an integer hash code such that `isequal(x,y)` implies `hash(x)==hash(y)`. The
optional second argument `h` is a hash code to be mixed with the result.

New types should implement the 2-argument form, typically by calling the 2-argument `hash`
method recursively in order to mix hashes of the contents with each other (and with `h`).
Typically, any type that implements `hash` should also implement its own `==` (hence
`isequal`) to guarantee the property mentioned above. Types supporting subtraction
(operator `-`) should also implement [`widen`](@ref), which is required to hash
values inside heterogeneous arrays.
"""
hash(x::Any) = hash(x, zero(UInt))
hash(w::WeakRef, h::UInt) = hash(w.value, h)

## hashing general objects ##

hash(@nospecialize(x), h::UInt) = hash_uint(3h - objectid(x))

## core data hashing functions ##

function hash_uint(n::T) where {T <: Union{UInt64, UInt32}}
    # random constants, last must be odd
    n += 0xe391e8b4ff155ee5 % T
    n ⊻= 0xb9d8ebf206d49927 % T
    n *= 0xafa64689f53d9ee1 % T
end

## symbol & expression hashing ##

if UInt === UInt64
    hash(x::Expr, h::UInt) = hash(x.args, hash(x.head, h + 0x83c7900696d26dc6))
    hash(x::QuoteNode, h::UInt) = hash(x.value, h + 0x2c97bf8b3de87020)
else
    hash(x::Expr, h::UInt) = hash(x.args, hash(x.head, h + 0x96d26dc6))
    hash(x::QuoteNode, h::UInt) = hash(x.value, h + 0x469d72af)
end
