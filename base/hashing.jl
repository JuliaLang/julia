# This file is a part of Julia. License is MIT: https://julialang.org/license

const HASH_SEED = UInt == UInt64 ? 0xbdd89aa982704029 : 0xeabe9406
const HASH_SECRET = (
    0x2d358dccaa6c78a5,
    0x8bb84b93962eacc9,
    0x4b33a62ed433d4a3,
    0xaaaaaaaaaaaaaaaa,
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
hash_mix_linear(x::Union{UInt64, UInt32}, h::UInt) = 3h - x
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

# IntegerCodeUnits provides a little-endian byte representation of integers
struct IntegerCodeUnits{T<:Integer} <: AbstractVector{UInt8}
    uvalue::T
    num_bytes::Int

    function IntegerCodeUnits(x::T) where {T<:Integer}
        # Calculate number of bytes needed (always pad to full byte)
        u = abs(x)
        num_bytes = max(cld(top_set_bit(u), 8), 1)
        return new{T}(u, num_bytes)
    end
end
size(units::IntegerCodeUnits) = (units.num_bytes,)
length(units::IntegerCodeUnits) = units.num_bytes
@inline getindex(units::IntegerCodeUnits, i::Int) = (units.uvalue >>> (8 * (i - 1))) % UInt8
@inline load_le_array(::Type{UInt64}, units::IntegerCodeUnits, idx) = (units.uvalue >>> (8 * (idx - 1))) % UInt64
@inline load_le_array(::Type{UInt32}, units::IntegerCodeUnits, idx) = (units.uvalue >>> (8 * (idx - 1))) % UInt32


# Main interface function to get little-endian byte representation of integers
codeunits(x::Integer) = IntegerCodeUnits(x)

# UTF8Units provides UTF-8 byte iteration for any AbstractString
struct UTF8Units{T<:AbstractString}
    string::T
end

utf8units(s::AbstractString) = codeunit(s) <: UInt8 ? codeunits(s) : UTF8Units(s)

# Iterator state: (char_iter_state, remaining_utf8_bytes)
function iterate(units::UTF8Units)
    char_result = iterate(units.string)
    char_result === nothing && return nothing
    char, char_state = char_result

    # Decode char to UTF-8 bytes (similar to the write function)
    u = bswap(reinterpret(UInt32, char))

    # Return first byte and set up state for remaining bytes
    first_byte = u % UInt8
    remaining_bytes = u >> 8
    return first_byte, (char_state, remaining_bytes)
end

function iterate(units::UTF8Units, state)
    char_state, remaining_bytes = state
    # If we have more bytes from current char, return next byte
    if remaining_bytes != 0
        byte = remaining_bytes % UInt8
        new_remaining = remaining_bytes >> 8
        return byte, (char_state, new_remaining)
    end

    # Move to next char
    char_result = iterate(units.string, char_state)
    char_result === nothing && return nothing
    char, new_char_state = char_result

    # Decode new char to UTF-8 bytes
    u = bswap(reinterpret(UInt32, char))

    # Return first byte and set up state for remaining bytes
    first_byte = u % UInt8
    remaining_bytes = u >> 8

    return first_byte, (new_char_state, remaining_bytes)
end

hash_integer(x::Integer, h::UInt) = _hash_integer(x, UInt64(h)) % UInt
function _hash_integer(
        x::Integer,
        seed::UInt64,
        secret::NTuple{4, UInt64} = HASH_SECRET
    )
    # Handle sign by XOR-ing with seed
    seed ⊻= (x < 0)
    # Get little-endian byte representation of absolute value
    # and hash using the new safe hash_bytes function
    u = abs(x) # n.b.: this hashes typemin(IntN) correctly even if abs fails
    return hash_bytes(codeunits(u), seed, secret)
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
    # conservatively hash using == equality of all of the data, even though == often uses === internally
    hash(x::QuoteNode, h::UInt) = hash(x.value, h ⊻ 0x2c97bf8b3de87020)
    hash(x::PhiNode, h::UInt) = hash(x.edges, hash(x.values, h ⊻ 0x2c97bf8b3de87020))
    hash(x::PhiCNode, h::UInt) = hash(x.values, h ⊻ 0x2c97bf8b3de87020)
else
    hash(x::QuoteNode, h::UInt) = hash(x.value, h ⊻ 0x469d72af)
    hash(x::PhiNode, h::UInt) = hash(x.edges, hash(x.values, h ⊻ 0x469d72af))
    hash(x::PhiCNode, h::UInt) = hash(x.values, h ⊻ 0x469d72af)
end

function hash(x::Expr, h::UInt)
    h = hash(x.head, h ⊻ (UInt === UInt64 ? 0x83c7900696d26dc6 : 0x469d72af))
    # Hint that `x.args::Vector{Any}` is mostly Expr, Symbol, and LineNumberNode.
    hash_shaped(x.args, h ⊻ hash_abstractarray_seed, (Val{Expr}(), Val{Symbol}(), Val{LineNumberNode}()))
end

function hash(x::CodeInfo, h::UInt)
    h ⊻= UInt === UInt64 ? 0x2c97bf8b3de87020 : 0x469d72af
    for i in 1:nfields(x)
        h = hash(isdefined(x, i) ? getfield(x, i) : missing, h)
    end
    return h
end

function hash(x::DebugInfo, h::UInt)
    h ⊻= UInt === UInt64 ? 0x2c97bf8b3de87020 : 0x469d72af
    for i in 1:nfields(x)
        h = hash(getfield(x, i), h)
    end
    return h
end

hash(x::Symbol) = objectid(x)


load_le(::Type{T}, ptr::Ptr{UInt8}, i) where {T <: Union{UInt32, UInt64}} =
    unsafe_load(convert(Ptr{T}, ptr + i - 1))

@assume_effects :terminates_globally function hash_bytes(
        ptr::Ptr{UInt8},
        n::Int,
        seed::UInt64,
        secret::NTuple{4, UInt64}
    )
    # Adapted with gratitude from [rapidhash](https://github.com/Nicoshev/rapidhash)
    buflen = UInt64(n)
    seed = seed ⊻ hash_mix(seed ⊻ secret[3], secret[2])

    a = zero(UInt64)
    b = zero(UInt64)
    i = buflen

    if buflen ≤ 16
        if buflen ≥ 4
            seed ⊻= buflen
            if buflen ≥ 8
                a = load_le(UInt64, ptr, 1)
                b = load_le(UInt64, ptr, n - 7)
            else
                a = UInt64(load_le(UInt32, ptr, 1))
                b = UInt64(load_le(UInt32, ptr, n - 3))
            end
        elseif buflen > 0
            a = (UInt64(unsafe_load(ptr)) << 45) | UInt64(unsafe_load(ptr, n))
            b = UInt64(unsafe_load(ptr, div(n, 2) + 1))
        end
    else
        pos = 1
        if i > 48
            see1 = seed
            see2 = seed
            while i > 48
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
            seed ⊻= see1
            seed ⊻= see2
        end
        if i > 16
            seed = hash_mix(
                load_le(UInt64, ptr, pos) ⊻ secret[3],
                load_le(UInt64, ptr, pos + 8) ⊻ seed
            )
            if i > 32
                seed = hash_mix(
                    load_le(UInt64, ptr, pos + 16) ⊻ secret[3],
                    load_le(UInt64, ptr, pos + 24) ⊻ seed
                )
            end
        end

        a = load_le(UInt64, ptr, n - 15) ⊻ i
        b = load_le(UInt64, ptr, n - 7)
    end

    a = a ⊻ secret[2]
    b = b ⊻ seed
    b, a = mul_parts(a, b)
    return hash_mix(a ⊻ secret[4], b ⊻ secret[2] ⊻ i)
end

@inline function load_le_array(::Type{UInt64}, arr::AbstractArray{UInt8}, idx)
    # n.b. for whatever reason, writing this as a loop ensures LLVM
    # optimizations (particular SROA) don't make a disaster of this code
    # early on so it can actually emit the optimal result
    result = zero(UInt64)
    for i in 0:7
        byte = @inbounds arr[idx + i]
        result |= UInt64(byte) << (8 * i)
    end
    return result
end

@inline function load_le_array(::Type{UInt32}, arr::AbstractArray{UInt8}, idx)
    result = zero(UInt32)
    for i in 0:3
        byte = @inbounds arr[idx + i]
        result |= UInt32(byte) << (8 * i)
    end
    return result
end

@assume_effects :terminates_globally function hash_bytes(
        arr::AbstractArray{UInt8},
        seed::UInt64,
        secret::NTuple{4, UInt64}
    )
    # Adapted with gratitude from [rapidhash](https://github.com/Nicoshev/rapidhash)
    n = length(arr)
    buflen = UInt64(n)
    seed = seed ⊻ hash_mix(seed ⊻ secret[3], secret[2])
    firstidx = firstindex(arr)

    a = zero(UInt64)
    b = zero(UInt64)
    i = buflen

    if buflen ≤ 16
        if buflen ≥ 4
            seed ⊻= buflen
            if buflen ≥ 8
                a = load_le_array(UInt64, arr, firstidx)
                b = load_le_array(UInt64, arr, firstidx + n - 8)
            else
                a = UInt64(load_le_array(UInt32, arr, firstidx))
                b = UInt64(load_le_array(UInt32, arr, firstidx + n - 4))
            end
        elseif buflen > 0
            a = (UInt64(@inbounds arr[firstidx]) << 45) | UInt64(@inbounds arr[firstidx + n - 1])
            b = UInt64(@inbounds arr[firstidx + div(n, 2)])
        end
    else
        pos = 0
        if i > 48
            see1 = seed
            see2 = seed
            while i > 48
                seed = hash_mix(
                    load_le_array(UInt64, arr, firstidx + pos) ⊻ secret[1],
                    load_le_array(UInt64, arr, firstidx + pos + 8) ⊻ seed
                )
                see1 = hash_mix(
                    load_le_array(UInt64, arr, firstidx + pos + 16) ⊻ secret[2],
                    load_le_array(UInt64, arr, firstidx + pos + 24) ⊻ see1
                )
                see2 = hash_mix(
                    load_le_array(UInt64, arr, firstidx + pos + 32) ⊻ secret[3],
                    load_le_array(UInt64, arr, firstidx + pos + 40) ⊻ see2
                )
                pos += 48
                i -= 48
            end
            seed ⊻= see1
            seed ⊻= see2
        end
        if i > 16
            seed = hash_mix(
                load_le_array(UInt64, arr, firstidx + pos) ⊻ secret[3],
                load_le_array(UInt64, arr, firstidx + pos + 8) ⊻ seed
            )
            if i > 32
                seed = hash_mix(
                    load_le_array(UInt64, arr, firstidx + pos + 16) ⊻ secret[3],
                    load_le_array(UInt64, arr, firstidx + pos + 24) ⊻ seed
                )
            end
        end

        a = load_le_array(UInt64, arr, firstidx + n - 16) ⊻ i
        b = load_le_array(UInt64, arr, firstidx + n - 8)
    end

    a = a ⊻ secret[2]
    b = b ⊻ seed
    b, a = mul_parts(a, b)
    return hash_mix(a ⊻ secret[4], b ⊻ secret[2] ⊻ i)
end


# Helper function to concatenate two UInt64 values with a byte shift
# Returns the result of shifting 'low' right by 'shift_bytes' bytes and
# filling the high bits with the low bits of 'high'
@inline function concat_shift(low::UInt64, high::UInt64, shift_bytes::UInt8)
    shift_bits = (shift_bytes * 0x8) & 0x3f
    return (low >> shift_bits) | (high << (0x40 - shift_bits))
end

@inline function read_uint64_from_uint8_iter(iter, state)
    value = zero(UInt64)
    @nexprs 8 i -> begin
        next_result = iterate(iter, state)
        next_result === nothing && return value, state, UInt8(i - 1)
        byte, state = next_result
        value |= UInt64(byte) << ((i - 1) * 8)
    end
    return value, state, 0x8
end

@inline function read_uint64_from_uint8_iter(iter)
    next_result = iterate(iter)
    next_result === nothing && return nothing
    byte, state = next_result
    value = UInt64(byte)
    @nexprs 7 i -> begin
        next_result = iterate(iter, state)
        next_result === nothing && return value, state, UInt8(i)
        byte, state = next_result
        value |= UInt64(byte::UInt8) << (i * 8)
    end
    return value, state, 0x8
end

@assume_effects :terminates_globally function hash_bytes(
        iter,
        seed::UInt64,
        secret::NTuple{4, UInt64}
    )
    seed = seed ⊻ hash_mix(seed ⊻ secret[3], secret[2])

    a = zero(UInt64)
    b = zero(UInt64)
    buflen = zero(UInt64)

    see1 = seed
    see2 = seed
    l0 = zero(UInt64)
    l1 = zero(UInt64)
    l2 = zero(UInt64)
    l3 = zero(UInt64)
    l4 = zero(UInt64)
    l5 = zero(UInt64)
    b0 = 0x0
    b1 = 0x0
    b2 = 0x0
    b3 = 0x0
    b4 = 0x0
    b5 = 0x0
    t0 = zero(UInt64)
    t1 = zero(UInt64)

    # Handle first iteration separately
    read = read_uint64_from_uint8_iter(iter)
    if read !== nothing
        l0, state, b0 = read
        # Repeat hashing chunks until a short read
        while true
            l1, state, b1 = read_uint64_from_uint8_iter(iter, state)
            if b1 == 0x8
                l2, state, b2 = read_uint64_from_uint8_iter(iter, state)
                if b2 == 0x8
                    l3, state, b3 = read_uint64_from_uint8_iter(iter, state)
                    if b3 == 0x8
                        l4, state, b4 = read_uint64_from_uint8_iter(iter, state)
                        if b4 == 0x8
                            l5, state, b5 = read_uint64_from_uint8_iter(iter, state)
                            if b5 == 0x8
                                # Read start of next chunk
                                read = read_uint64_from_uint8_iter(iter, state)
                                if read[3] == 0x0
                                    # Read exactly 48 bytes
                                    t0 = l4
                                    t1 = l5
                                    break
                                else
                                    # Read more than 48 bytes - process and continue to next chunk
                                    seed = hash_mix(l0 ⊻ secret[1], l1 ⊻ seed)
                                    see1 = hash_mix(l2 ⊻ secret[2], l3 ⊻ see1)
                                    see2 = hash_mix(l4 ⊻ secret[3], l5 ⊻ see2)
                                    buflen += 48
                                    l0, state, b0 = read
                                    b1 = 0
                                    b2 = 0
                                    b3 = 0
                                    b4 = 0
                                    b5 = 0
                                    if b0 < 8
                                        t0 = concat_shift(l4, l5, b0)
                                        t1 = concat_shift(l5, l0, b0)
                                        break
                                    end
                                end
                            else
                                # Extract final 16 bytes at the first short read
                                t0 = concat_shift(l3, l4, b5)
                                t1 = concat_shift(l4, l5, b5)
                                break
                            end
                        else
                            t0 = concat_shift(l2, l3, b4)
                            t1 = concat_shift(l3, l4, b4)
                            break
                        end
                    else
                        t0 = concat_shift(l1, l2, b3)
                        t1 = concat_shift(l2, l3, b3)
                        break
                    end
                else
                    t0 = concat_shift(l0, l1, b2)
                    t1 = concat_shift(l1, l2, b2)
                    break
                end
            else
                t0 = concat_shift(l5, l0, b1)
                t1 = concat_shift(l0, l1, b1)
                break
            end
        end
    end

    # Partial chunk, handle based on size
    bytes_chunk = b0 + b1 + b2 + b3 + b4 + b5
    if buflen > 0
        # Finalize last full chunk
        seed ⊻= see1
        seed ⊻= see2
    end
    buflen += bytes_chunk
    if buflen ≤ 16
        if bytes_chunk ≥ 0x4
            seed ⊻= bytes_chunk
            if bytes_chunk ≥ 0x8
                a = l0
                b = t1
            else
                a = UInt64(l0 % UInt32)
                b = UInt64((l0 >>> ((0x8 * (bytes_chunk - 0x4)) % 0x3f)) % UInt32)
            end
        elseif bytes_chunk > 0x0
            b0 = l0 % UInt8
            b1 = (l0 >>> ((0x8 * div(bytes_chunk, 0x2)) % 0x3f)) % UInt8
            b2 = (l0 >>> ((0x8 * (bytes_chunk - 0x1)) % 0x3f)) % UInt8
            a = (UInt64(b0) << 45) | UInt64(b2)
            b = UInt64(b1)
        end
    else
        if bytes_chunk > 0x10
            seed = hash_mix(l0 ⊻ secret[3], l1 ⊻ seed)
            if bytes_chunk > 0x20
                seed = hash_mix(l2 ⊻ secret[3], l3 ⊻ seed)
            end
        end
        a = t0 ⊻ bytes_chunk
        b = t1
    end

    a = a ⊻ secret[2]
    b = b ⊻ seed
    b, a = mul_parts(a, b)
    return hash_mix(a ⊻ secret[4], b ⊻ secret[2] ⊻ bytes_chunk)
end

hash(data::AbstractString, h::UInt) =
    hash_bytes(utf8units(data), UInt64(h), HASH_SECRET) % UInt
@assume_effects :total hash(data::String, h::UInt) =
    GC.@preserve data hash_bytes(pointer(data), sizeof(data), UInt64(h), HASH_SECRET) % UInt

# no longer used in Base, but a lot of packages access these internals
const memhash = UInt === UInt64 ? :memhash_seed : :memhash32_seed
const memhash_seed = UInt === UInt64 ? 0x71e729fd56419c81 : 0x56419c81
