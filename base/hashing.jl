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

Compute an integer hash code such that `isequal(x,y)` implies `isequal(hash(x), hash(y))`. The
optional second argument `h` is another hash code to be mixed with the result.

New types should implement the 2-argument form, typically by calling the 2-argument `hash`
method recursively in order to mix hashes of the contents with each other (and with `h`).
Typically, any type that implements `hash` should also implement its own [`==`](@ref) (hence
[`isequal`](@ref)) to guarantee the property mentioned above.

The hash value may change when a new Julia process is started.

!!! warning
    When implementing the 2-argument form, the second argument `h` should _not_ be given a
    default value such `h = UInt(0)` as this will implicitly create a 1-argument method that
    is more specific than the fallback (see [Note on Optional and keyword Arguments](@ref)),
    but potentially with the wrong seed, causing hash inconsistencies.

```jldoctest; filter = r"0x[0-9a-f]{16}"
julia> a = hash(10)
0x759d18cc5346a65f

julia> hash(10, a) # only use the output of another hash function as the second argument
0x03158cd61b1b0bd1
```

See also [`objectid`](@ref), [`Dict`](@ref), [`Set`](@ref).
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
hash_mix_linear(x::Union{UInt64, UInt32}, h::UInt) = 3 *% h -% x
function hash_finalizer(x::UInt64)
    x ⊻= (x >> 32)
    x *%= 0x63652a4cd374b267
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
        u = abs(x) # n.b.: typemin(IntN) works even though abs overflows, since `>>>` is logical
        num_bytes = max(cld(top_set_bit(u), 8), 1)
        return new{T}(u, num_bytes)
    end
end
size(units::IntegerCodeUnits) = (units.num_bytes,)
length(units::IntegerCodeUnits) = units.num_bytes
@inline getindex(units::IntegerCodeUnits, i::Int) = (units.uvalue >>> (8 * (i - 1))) % UInt8
@inline load_le_array(::Type{UInt64}, units::IntegerCodeUnits, idx) = (units.uvalue >>> (8 * (idx - 1))) % UInt64
@inline load_le_array(::Type{UInt32}, units::IntegerCodeUnits, idx) = (units.uvalue >>> (8 * (idx - 1))) % UInt32


# ShiftedIntegerCodeUnits provides the little-endian bytes of abs(x) >>> shift without computing
# it, which would allocate for x::BigInt, and for a left shift (negative `shift`) would need
# `widen` to not overflow.
# It is separate from IntegerCodeUnits so that the common unshifted case avoids the shifting.
struct ShiftedIntegerCodeUnits{T<:Integer} <: AbstractVector{UInt8}
    # An integer with the same magnitude as x: abs(x) by default, or x itself for types
    # with a method of load_shifted that reads the magnitude directly (such as BigInt).
    value::T
    shift::Int
    num_bytes::Int
end
# Types for which abs allocates (such as BigInt) can add a method that stores x itself.
function ShiftedIntegerCodeUnits(x::Integer, shift::Int)
    u = abs(x) # n.b.: as in IntegerCodeUnits, typemin(IntN) works since `>>>` is logical
    # Calculate number of bytes needed (always pad to full byte, and use at least one byte)
    num_bytes = max(cld(top_set_bit(u) - shift, 8), 1)
    return ShiftedIntegerCodeUnits(u, shift, num_bytes)
end
size(units::ShiftedIntegerCodeUnits) = (units.num_bytes,)
length(units::ShiftedIntegerCodeUnits) = units.num_bytes
# Load a `U` from the bytes of abs(x) >> shift, starting at byte `i`. This is
# (abs(big(x)) >> (shift + 8 * (i - 1))) % U
@inline function load_shifted(::Type{U}, units::ShiftedIntegerCodeUnits, i) where {U<:Union{UInt8, UInt32, UInt64}}
    bitpos = units.shift + 8 * (i - 1)
    if bitpos < 0
        # Widen to U first to avoid overflow
        # if value is too narrow
        (units.value % U) << -bitpos
    else
        (units.value >>> bitpos) % U
    end
end
@inline function getindex(units::ShiftedIntegerCodeUnits, i::Int)
    @boundscheck checkbounds(units, i)
    return load_shifted(UInt8, units, i)
end
# load_le_array specialized for faster hash_bytes
@inline load_le_array(::Type{UInt64}, units::ShiftedIntegerCodeUnits, idx) = load_shifted(UInt64, units, idx)
@inline load_le_array(::Type{UInt32}, units::ShiftedIntegerCodeUnits, idx) = load_shifted(UInt32, units, idx)

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

hash_integer(x::Integer, h::UInt) = hash_shifted_integer(x, 0, signbit(x), h)

# The helpers below take the magnitude of x and its sign separately, and shift lazily, so that
# `hash(::Real)` never computes a negated or shifted copy of an integer returned by `decompose`
# (which would allocate for `BigInt`), and never mutates it. A negative `shift` shifts left,
# as if `x` were widened first so nothing overflows.
# GMP specializes ShiftedIntegerCodeUnits and shifted_abs_uint64 to read the limbs of a `BigInt`.

# Hash the integer with magnitude abs(x) >> shift, and sign bit `negative`: the sign is XOR-ed
# into the seed, and the little-endian bytes of the magnitude are hashed with hash_bytes.
@inline function hash_shifted_integer(x::Integer, shift::Int, negative::Bool, h::UInt)
    seed = UInt64(h) ⊻ negative
    # codeunits is IntegerCodeUnits, which avoids the shifting in the common unshifted case
    shift == 0 && return hash_bytes(codeunits(x), seed, HASH_SECRET) % UInt
    return hash_bytes(ShiftedIntegerCodeUnits(x, shift), seed, HASH_SECRET) % UInt
end

# `(abs(x) >> shift) % UInt64`, the low 64 bits of the magnitude of x shifted right by `shift >= 0`.
# n.b.: as in IntegerCodeUnits, typemin(IntN) works even though abs overflows, since `>>>` is logical
shifted_abs_uint64(x::Integer, shift::Int) = (abs(x) >>> shift) % UInt64


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
function hash(x::Real, h::UInt)
    # decompose x as num*2^pow/den
    _num, _pow, _den = decompose(x)
    # promote Bool to Int8 for trailing_zeros and widen
    num = _num isa Bool ? Int8(_num) : _num
    den = _den isa Bool ? Int8(_den) : _den

    # handle special values
    iszero(num) && iszero(den) && return hash(NaN, h)
    iszero(num) && return hash(ifelse(den > 0, 0.0, -0.0), h)
    iszero(den) && return hash(ifelse(num > 0, Inf, -Inf), h)

    # normalize decomposition: x == (num >> num_z) * 2^pow / (den >> den_z), where both shifted
    # values are odd. num and den are never actually shifted, and must not be mutated since
    # decompose may return internal state (e.g. `BigInt`s). Instead num_z and den_z are passed to
    # shifted_abs_uint64 and hash_shifted_integer, so no new integers need to be allocated.
    num_z::Int = trailing_zeros(num)
    den_z::Int = trailing_zeros(den)
    # effective pow for hashing purposes is defined as pow%Int64
    pow::Int64 = _pow%Int64 +% num_z%Int64 -% den_z%Int64
    num_signbit = signbit(num)
    den_signbit = signbit(den)
    x_signbit = num_signbit ⊻ den_signbit
    # The sign is not normalized: num may be unsigned, and negating a BigInt allocates.
    # Instead the sign of x is passed separately, with the magnitudes of num and den: the hash
    # treats den as positive, and num as having the sign of x.
    # If the real can be represented as an Int64, UInt64, or Float64, hash as those types.
    # To be an Integer the denominator must be 1 and the power must be non-negative.
    # den >> den_z is 1 or -1 exactly when abs(den) == 2^den_z, i.e. when its only set bit is bit den_z
    if exponent(den) == den_z
        num_sig_dig = exponent(num) + 1 - num_z
        # 2^-1074 is the minimum Float64 so if the power is smaller, not a Float64
        if -1074 <= pow
            if 0 <= pow # if pow is non-negative, it is an integer
                # Int64 and UInt64 hash by their 64-bit pattern, so compute the pattern of
                # num*2^pow modulo 2^64. It is exact when the value fits in Int64 or UInt64.
                if pow <= 64 - x_signbit - num_sig_dig
                    unum = shifted_abs_uint64(num, num_z)
                    return hash(ifelse(x_signbit, -unum, unum) << Int(pow), h)
                end
            end # typemin(Int64) handled by Float64 case
            # 2^1024 is the maximum Float64 so if the power is greater, not a Float64
            # Float64s have 53 mantissa bits (including implicit bit)
            if pow <= 1024 - num_sig_dig && num_sig_dig <= 53
                # exact: abs(num) >> num_z has at most 53 bits here, so the conversion from
                # Int64 (cheaper than from UInt64) is exact too
                fnum = Float64(shifted_abs_uint64(num, num_z) % Int64)
                return hash(ldexp(ifelse(x_signbit, -fnum, fnum), Int(pow)), h)
            end
        end
    else
        h = hash_shifted_integer(den, den_z, false, h)
    end
    # handle generic rational values
    h = hash_integer(pow, h)

    # Hash the odd part of num, shifted back left by pow % 8 when pow > 0. For an integer this
    # trims only whole bytes of trailing zeros, which simplifies greatly some specializations
    # for memory-backed bitintegers, such as `hash(::BigInt)`, and `hash(::Rational{<:BitInteger64})`.
    # The two shifts are folded into net_shift, which may be negative (a left shift by up to 7 bits).
    net_shift = num_z - ((pow > 0) ? Int(pow % 8) : 0)
    return hash_shifted_integer(num, net_shift, x_signbit, h)
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

const hashgr_seed = 0xe19bede84d316c06 % UInt
const hashps_seed = 0xee5c84c1439961a8 % UInt
hash(x::GlobalRef, h::UInt) = hash(x.name, hash(x.mod, h ⊻ hashgr_seed))
function hash(x::PartialStruct, h::UInt)
    h ⊻= hashps_seed
    h = hash(x.typ, h)
    h = hash(x.undefs, h)
    h = hash(x.fields, h)
    return h
end


load_le(::Type{T}, ptr::Ptr{UInt8}, i, align::Int=1) where {T <: Union{UInt32, UInt64}} =
    pointerref(convert(Ptr{T}, ptr + i - 1), 1, align)

# `A` is a known power-of-two alignment of `ptr`. Only the loads at multiples of 8 bytes from
# `ptr` can use it; the overlapping reads of the tail may be misaligned.
@assume_effects :terminates_globally function hash_bytes(
        ptr::Ptr{UInt8},
        n::Int,
        seed::UInt64,
        secret::NTuple{4, UInt64},
        ::Val{A}=Val(1)
    ) where {A}
    # Adapted with gratitude from [rapidhash](https://github.com/Nicoshev/rapidhash)
    align = min(A, 8)
    buflen = UInt64(n)
    seed = seed ⊻ hash_mix(seed ⊻ secret[3], secret[2])

    a = zero(UInt64)
    b = zero(UInt64)
    i = buflen

    if buflen ≤ 16
        if buflen ≥ 4
            seed ⊻= buflen
            if buflen ≥ 8
                a = load_le(UInt64, ptr, 1, align)
                b = load_le(UInt64, ptr, n - 7)
            else
                a = UInt64(load_le(UInt32, ptr, 1, align))
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
                    load_le(UInt64, ptr, pos, align) ⊻ secret[1],
                    load_le(UInt64, ptr, pos + 8, align) ⊻ seed
                )
                see1 = hash_mix(
                    load_le(UInt64, ptr, pos + 16, align) ⊻ secret[2],
                    load_le(UInt64, ptr, pos + 24, align) ⊻ see1
                )
                see2 = hash_mix(
                    load_le(UInt64, ptr, pos + 32, align) ⊻ secret[3],
                    load_le(UInt64, ptr, pos + 40, align) ⊻ see2
                )
                pos += 48
                i -= 48
            end
            seed ⊻= see1
            seed ⊻= see2
        end
        if i > 16
            seed = hash_mix(
                load_le(UInt64, ptr, pos, align) ⊻ secret[3],
                load_le(UInt64, ptr, pos + 8, align) ⊻ seed
            )
            if i > 32
                seed = hash_mix(
                    load_le(UInt64, ptr, pos + 16, align) ⊻ secret[3],
                    load_le(UInt64, ptr, pos + 24, align) ⊻ seed
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
# String data follows the length field of a 4-byte-aligned object
@assume_effects :total hash(data::String, h::UInt) =
    GC.@preserve data hash_bytes(
        pointer(data), sizeof(data), UInt64(h), HASH_SECRET, Val(sizeof(Int))) % UInt
