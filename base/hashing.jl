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
const RAPID_SEED = UInt64(0xbdd89aa982704029)
const RAPID_SECRET = tuple(
    0x2d358dccaa6c78a5,
    0x8bb84b93962eacc9,
    0x4b33a62ed433d4a3,
)

mul_hi64(A::UInt64, B::UInt64) = ((widen(A) * B) >> 64) % UInt64
rapid_mix(A, B) = mul_hi64(A, B) ⊻ (A * B)

load_le(::Type{T}, ptr::Ptr{UInt8}, i) where {T <: Union{UInt32, UInt64}} =
    unsafe_load(convert(Ptr{T}, ptr + i - 1))

function read_small(ptr::Ptr{UInt8}, n::Int)
    return (UInt64(unsafe_load(ptr)) << 56) |
        (UInt64(unsafe_load(ptr, div(n + 1, 2))) << 32) |
        UInt64(unsafe_load(ptr, n))
end

function hash(
        ptr::Ptr{UInt8},
        n::Int,
        seed::UInt64,
        secret::NTuple{3, UInt64}
    )
    buflen = UInt64(n)
    seed = seed ⊻ (rapid_mix(seed ⊻ secret[1], secret[2]) ⊻ buflen)

    a = zero(UInt64)
    b = zero(UInt64)

    if buflen ≤ 16
        if buflen ≥ 4
            a = (UInt64(load_le(UInt32, ptr, 1)) << 32) |
                UInt64(load_le(UInt32, ptr, n - 4 + 1))

            delta = (buflen & 24) >>> (buflen >>> 3)
            b = (UInt64(load_le(UInt32, ptr, delta + 1)) << 32) |
                UInt64(load_le(UInt32, ptr, n - 4 - delta + 1))
        elseif buflen > 0
            a = read_small(ptr, n)
        end
    else
        pos = 1
        i = buflen
        if i > 48
            see1 = seed
            see2 = seed
            while i ≥ 48
                seed = rapid_mix(
                    load_le(UInt64, ptr, pos) ⊻ secret[1],
                    load_le(UInt64, ptr, pos + 8) ⊻ seed
                )
                see1 = rapid_mix(
                    load_le(UInt64, ptr, pos + 16) ⊻ secret[2],
                    load_le(UInt64, ptr, pos + 24) ⊻ see1
                )
                see2 = rapid_mix(
                    load_le(UInt64, ptr, pos + 32) ⊻ secret[3],
                    load_le(UInt64, ptr, pos + 40) ⊻ see2
                )
                pos += 48
                i -= 48
            end
            seed = seed ⊻ see1 ⊻ see2
        end
        if i > 16
            seed = rapid_mix(
                load_le(UInt64, ptr, pos) ⊻ secret[3],
                load_le(UInt64, ptr, pos + 8) ⊻ seed ⊻ secret[2]
            )
            if i > 32
                seed = rapid_mix(
                    load_le(UInt64, ptr, pos + 16) ⊻ secret[3],
                    load_le(UInt64, ptr, pos + 24) ⊻ seed
                )
            end
        end

        a = load_le(UInt64, ptr, n - 17)
        b = load_le(UInt64, ptr, n - 9)
    end

    a = a ⊻ secret[2]
    b = b ⊻ seed
    a, b = a * b, mul_hi64(a, b)
    return rapid_mix(a ⊻ secret[1] ⊻ buflen, b ⊻ secret[2])
end


function hash_64_64(data::UInt64, seed::UInt64, secret::NTuple{3, UInt64})
    seed = seed ⊻ (rapid_mix(seed ⊻ secret[1], secret[2]) ⊻ 8)

    a = (UInt64(bswap((data >>> 32) % UInt32)) << 32) | UInt64(bswap(data % UInt32))
    b = (a << 32) | (a >>> 32)
    a = a ⊻ secret[2]
    b = b ⊻ seed
    a, b = a * b, mul_hi64(a, b)
    return rapid_mix(a ⊻ secret[1] ⊻ 8, b ⊻ secret[2])
end
hash_64_32(data::UInt64, seed::UInt64, secret::NTuple{3, UInt64}) =
    hash_64_64(data, seed, secret) % UInt32
hash_32_32(data::UInt32, seed::UInt64, secret::NTuple{3, UInt64}) =
    hash_64_32(UInt64(data), seed, secret)

if UInt === UInt64
    const hash_uint64 = hash_64_64
    const hash_uint = hash_64_64
else
    const hash_uint64 = hash_64_32
    const hash_uint = hash_32_32
end

hash(x::UInt64, h::UInt) = hash_uint64(x, h, RAPID_SECRET)
hash(x::Int64, h::UInt) = hash(bitcast(UInt64, x), h)
hash(x::Union{Bool, Int8, UInt8, Int16, UInt16, Int32, UInt32}, h::UInt) = hash(Int64(x), h)

function hash_integer(n::Integer, h::UInt)
    h ⊻= hash((n % UInt) ⊻ h)
    n = abs(n)
    n >>>= sizeof(UInt) << 3
    while n != 0
        h ⊻= hash((n % UInt) ⊻ h)
        n >>>= sizeof(UInt) << 3
    end
    return h
end


if UInt === UInt64
    hash(x::Expr, h::UInt) = hash(x.args, hash(x.head, h + 0x83c7900696d26dc6))
    hash(x::QuoteNode, h::UInt) = hash(x.value, h + 0x2c97bf8b3de87020)
else
    hash(x::Expr, h::UInt) = hash(x.args, hash(x.head, h + 0x96d26dc6))
    hash(x::QuoteNode, h::UInt) = hash(x.value, h + 0x469d72af)
end


hash(data::String, h::UInt64) = GC.@preserve data hash(pointer(data), sizeof(data), h, RAPID_SECRET)

hash(w::WeakRef, h::UInt64) = hash(w.value, h)
function hash(T::Type, h::UInt64)
    return hash((Base.@assume_effects :total ccall(:jl_type_hash, UInt, (Any,), T)), h)
end

hash(x::Symbol) = objectid(x)

# generic dispatch
hash(data) = hash(data, RAPID_SEED)
hash(@nospecialize(data), h::UInt64) = hash(objectid(data), h)
