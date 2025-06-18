# This file is a part of Julia. License is MIT: https://julialang.org/license

const HASH_SEED = UInt == UInt64 ? 0xbdd89aa982704029 : 0xeabe9406
const HASH_SECRET = tuple(
    0x2d358dccaa6c78a5,
    0x8bb84b93962eacc9,
    0x4b33a62ed433d4a3,
)

"""
    hash(x[, h::UInt])::UInt

Compute an integer hash code such that `isequal(x,y)` implies `hash(x)==hash(y)`. The
optional second argument `h` is another hash code to be mixed with the result.

New types should implement the 2-argument form, typically by calling the 2-argument `hash`
method recursively in order to mix hashes of the contents with each other (and with `h`).
Typically, any type that implements `hash` should also implement its own [`==`](@ref) (hence
[`isequal`](@ref)) to guarantee the property mentioned above.

The hash value may change when a new Julia process is started.

```jldoctest; filter = r"0x[0-9a-f]{16}"
julia> a = hash(10)
0x759d18cc5346a65f

julia> hash(10, a) # only use the output of another hash function as the second argument
0x03158cd61b1b0bd1
```

See also: [`objectid`](@ref), [`Dict`](@ref), [`Set`](@ref).
"""
hash(data::Any) = hash(data, HASH_SEED)
hash(w::WeakRef, h::UInt) = hash(w.value, h)

# Types can't be deleted, so marking as total allows the compiler to look up the hash
@noinline _jl_type_hash(T::Type) = @assume_effects :total ccall(:jl_type_hash, UInt, (Any,), T)
hash(T::Type, h::UInt) = hash(_jl_type_hash(T), h)
hash(@nospecialize(data), h::UInt) = hash(objectid(data), h)

function mul_parts(a::UInt64, b::UInt64)
    p = widemul(a, b)
    return (p >> 64) % UInt64, p % UInt64
end
hash_mix(a::UInt64, b::UInt64) = ⊻(mul_parts(a, b)...)

# faster-but-weaker than hash_mix intended for small keys
hash_mix_linear(x::UInt64, h::UInt) = 3h - x
function hash_finalizer(x::UInt64)
    x ⊻= (x >> 32)
    x *= 0x63652a4cd374b267
    x ⊻= (x >> 33)
    return x
end

hash_64_64(data::UInt64) = hash_finalizer(data)
hash_64_32(data::UInt64) = hash_64_64(data) % UInt32
hash_32_32(data::UInt32) = hash_64_32(UInt64(data))

if UInt === UInt64
    const hash_uint64 = hash_64_64
    const hash_uint = hash_64_64
else
    const hash_uint64 = hash_64_32
    const hash_uint = hash_32_32
end

hash(x::UInt64, h::UInt) = hash_uint64(hash_mix_linear(x, h))
hash(x::Int64, h::UInt) = hash(bitcast(UInt64, x), h)
hash(x::Union{Bool, Int8, UInt8, Int16, UInt16, Int32, UInt32}, h::UInt) = hash(Int64(x), h)

hash_integer(x::Integer, h::UInt) = _hash_integer(x, UInt64(h)) % UInt
function _hash_integer(
        x::Integer,
        seed::UInt64 = HASH_SEED,
        secret::NTuple{3, UInt64} = HASH_SECRET
    )
    seed ⊻= (x < 0)
    u = abs(x)

    # always left-pad to full byte
    buflen = UInt(max(cld(top_set_bit(u), 8), 1))
    seed = seed ⊻ (hash_mix(seed ⊻ secret[1], secret[2]) ⊻ buflen)

    a = zero(UInt64)
    b = zero(UInt64)

    if buflen ≤ 16
        if buflen ≥ 4
            a = (UInt64(u % UInt32) << 32) |
                UInt64((u >>> ((buflen - 4) * 8)) % UInt32)

            delta = (buflen & 24) >>> (buflen >>> 3)

            b = (UInt64((u >>> (8 * delta)) % UInt32) << 32) |
                UInt64((u >>> (8 * (buflen - 4 - delta))) % UInt32)
        else # buflen > 0
            b0 = u % UInt8
            b1 = (u >>> (8 * div(buflen, 2))) % UInt8
            b2 = (u >>> (8 * (buflen - 1))) % UInt8
            a = (UInt64(b0) << 56) |
                (UInt64(b1) << 32) |
                UInt64(b2)
        end
    else
        a = (u >>> 8(buflen - 16)) % UInt
        b = (u >>> 8(buflen - 8)) % UInt

        i = buflen
        if i > 48
            see1 = seed
            see2 = seed
            while i ≥ 48
                l0 = u % UInt; u >>>= 64
                l1 = u % UInt; u >>>= 64
                l2 = u % UInt; u >>>= 64
                l3 = u % UInt; u >>>= 64
                l4 = u % UInt; u >>>= 64
                l5 = u % UInt; u >>>= 64

                seed = hash_mix(l0 ⊻ secret[1], l1 ⊻ seed)
                see1 = hash_mix(l2 ⊻ secret[2], l3 ⊻ see1)
                see2 = hash_mix(l4 ⊻ secret[3], l5 ⊻ see2)
                i -= 48
            end
            seed = seed ⊻ see1 ⊻ see2
        end
        if i > 16
            l0 = u % UInt; u >>>= 64
            l1 = u % UInt; u >>>= 64
            seed = hash_mix(l0 ⊻ secret[3], l1 ⊻ seed ⊻ secret[2])
            if i > 32
                l2 = u % UInt; u >>>= 64
                l3 = u % UInt; u >>>= 64
                seed = hash_mix(l2 ⊻ secret[3], l3 ⊻ seed)
            end
        end
    end

    a = a ⊻ secret[2]
    b = b ⊻ seed
    b, a = mul_parts(a, b)
    return hash_mix(a ⊻ secret[1] ⊻ buflen, b ⊻ secret[2])
end


## efficient value-based hashing of floats ##

const hx_NaN = hash(reinterpret(UInt64, NaN))
function hash(x::Float64, h::UInt)
    # see comments on trunc and hash(Real, UInt)
    if typemin(Int64) <= x < typemax(Int64)
        xi = fptosi(Int64, x)
        if isequal(xi, x)
            return hash(xi, h)
        end
    elseif typemin(UInt64) <= x < typemax(UInt64)
        xu = fptoui(UInt64, x)
        if isequal(xu, x)
            return hash(xu, h)
        end
    elseif isnan(x)
        return hx_NaN ⊻ h # NaN does not have a stable bit pattern
    end
    return hash(bitcast(UInt64, x), h)
end

hash(x::Float32, h::UInt) = hash(Float64(x), h)

function hash(x::Float16, h::UInt)
    # see comments on trunc and hash(Real, UInt)
    if isfinite(x) # all finite Float16 fit in Int64
        xi = fptosi(Int64, x)
        if isequal(xi, x)
            return hash(xi, h)
        end
    elseif isnan(x)
        return hx_NaN ⊻ h # NaN does not have a stable bit pattern
    end
    return hash(bitcast(UInt64, Float64(x)), h)
end

## generic hashing for rational values ##
_hash_shl!(x, n) = (x << n)
function hash(x::Real, h::UInt)
    # decompose x as num*2^pow/den
    num, pow, den = decompose(x)

    # handle special values
    num == 0 && den == 0 && return hash(NaN, h)
    num == 0 && return hash(ifelse(den > 0, 0.0, -0.0), h)
    den == 0 && return hash(ifelse(num > 0, Inf, -Inf), h)

    # normalize decomposition
    if den < 0
        num = -num
        den = -den
    end
    num_z = trailing_zeros(num)

    num >>= num_z
    den_z = trailing_zeros(den)
    den >>= den_z
    pow += num_z - den_z
    # If the real can be represented as an Int64, UInt64, or Float64, hash as those types.
    # To be an Integer the denominator must be 1 and the power must be non-negative.
    if den == 1
        # left = ceil(log2(num*2^pow))
        left = top_set_bit(abs(num)) + pow
        # 2^-1074 is the minimum Float64 so if the power is smaller, not a Float64
        if -1074 <= pow
            if 0 <= pow # if pow is non-negative, it is an integer
                left <= 63 && return hash(Int64(num) << Int(pow), h)
                left <= 64 && !signbit(num) && return hash(UInt64(num) << Int(pow), h)
            end # typemin(Int64) handled by Float64 case
            # 2^1024 is the maximum Float64 so if the power is greater, not a Float64
            # Float64s only have 53 mantisa bits (including implicit bit)
            left <= 1024 && left - pow <= 53 && return hash(ldexp(Float64(num), pow), h)
        end
    else
        h = hash_integer(den, h)
    end
    # handle generic rational values
    h = hash_integer(pow, h)

    # trimming only whole bytes of trailing zeros simplifies greatly
    # some specializations for memory-backed bitintegers
    h = hash_integer((pow > 0) ? _hash_shl!(num, pow % 8) : num, h)
    return h
end


## symbol & expression hashing ##
if UInt === UInt64
    hash(x::Expr, h::UInt) = hash(x.args, hash(x.head, h ⊻ 0x83c7900696d26dc6))
    hash(x::QuoteNode, h::UInt) = hash(x.value, h ⊻ 0x2c97bf8b3de87020)
else
    hash(x::Expr, h::UInt) = hash(x.args, hash(x.head, h ⊻ 0x469d72af))
    hash(x::QuoteNode, h::UInt) = hash(x.value, h ⊻ 0x469d72af)
end

hash(x::Symbol) = objectid(x)


load_le(::Type{T}, ptr::Ptr{UInt8}, i) where {T <: Union{UInt32, UInt64}} =
    unsafe_load(convert(Ptr{T}, ptr + i - 1))

function read_small(ptr::Ptr{UInt8}, n::Int)
    return (UInt64(unsafe_load(ptr)) << 56) |
        (UInt64(unsafe_load(ptr, div(n, 2) + 1)) << 32) |
        UInt64(unsafe_load(ptr, n))
end

@assume_effects :terminates_globally function hash_bytes(
        ptr::Ptr{UInt8},
        n::Int,
        seed::UInt64,
        secret::NTuple{3, UInt64}
    )
    # Adapted with gratitude from [rapidhash](https://github.com/Nicoshev/rapidhash)
    buflen = UInt64(n)
    seed = seed ⊻ (hash_mix(seed ⊻ secret[1], secret[2]) ⊻ buflen)

    a = zero(UInt64)
    b = zero(UInt64)

    if buflen ≤ 16
        if buflen ≥ 4
            a = (UInt64(load_le(UInt32, ptr, 1)) << 32) |
                UInt64(load_le(UInt32, ptr, n - 3))

            delta = (buflen & 24) >>> (buflen >>> 3)
            b = (UInt64(load_le(UInt32, ptr, delta + 1)) << 32) |
                UInt64(load_le(UInt32, ptr, n - 3 - delta))
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
                seed = hash_mix(
                    load_le(UInt64, ptr, pos) ⊻ secret[1],
                    load_le(UInt64, ptr, pos + 8) ⊻ seed
                )
                see1 = hash_mix(
                    load_le(UInt64, ptr, pos + 16) ⊻ secret[2],
                    load_le(UInt64, ptr, pos + 24) ⊻ see1
                )
                see2 = hash_mix(
                    load_le(UInt64, ptr, pos + 32) ⊻ secret[3],
                    load_le(UInt64, ptr, pos + 40) ⊻ see2
                )
                pos += 48
                i -= 48
            end
            seed = seed ⊻ see1 ⊻ see2
        end
        if i > 16
            seed = hash_mix(
                load_le(UInt64, ptr, pos) ⊻ secret[3],
                load_le(UInt64, ptr, pos + 8) ⊻ seed ⊻ secret[2]
            )
            if i > 32
                seed = hash_mix(
                    load_le(UInt64, ptr, pos + 16) ⊻ secret[3],
                    load_le(UInt64, ptr, pos + 24) ⊻ seed
                )
            end
        end

        a = load_le(UInt64, ptr, n - 15)
        b = load_le(UInt64, ptr, n - 7)
    end

    a = a ⊻ secret[2]
    b = b ⊻ seed
    b, a = mul_parts(a, b)
    return hash_mix(a ⊻ secret[1] ⊻ buflen, b ⊻ secret[2])
end

@assume_effects :total hash(data::String, h::UInt) =
    GC.@preserve data hash_bytes(pointer(data), sizeof(data), UInt64(h), HASH_SECRET) % UInt

# no longer used in Base, but a lot of packages access these internals
const memhash = UInt === UInt64 ? :memhash_seed : :memhash32_seed
const memhash_seed = UInt === UInt64 ? 0x71e729fd56419c81 : 0x56419c81
