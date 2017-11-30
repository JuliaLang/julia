# This file is a part of Julia. License is MIT: https://julialang.org/license

# Uniform random generation

# This file contains the creation of Sampler objects and the associated generation of
# random values from them. More specifically, given the specification S of a set
# of values to pick from (e.g. 1:10, or "a string"), we define
#
# 1) Sampler(rng, S, ::Repetition) -> sampler
# 2) rand(rng, sampler) -> random value
#
# Note that the 1) is automated when the sampler is not intended to carry information,
# i.e. the default fall-backs SamplerType and SamplerTrivial are used.


## from types: rand(::Type, [dims...])

### random floats

Sampler(rng::AbstractRNG, ::Type{T}, n::Repetition) where {T<:AbstractFloat} =
    Sampler(rng, CloseOpen(T), n)

# generic random generation function which can be used by RNG implementors
# it is not defined as a fallback rand method as this could create ambiguities

rand_generic(r::AbstractRNG, ::CloseOpen{Float16}) =
    Float16(reinterpret(Float32,
                        (rand_ui10_raw(r) % UInt32 << 13) & 0x007fe000 | 0x3f800000) - 1)

rand_generic(r::AbstractRNG, ::CloseOpen{Float32}) =
    reinterpret(Float32, rand_ui23_raw(r) % UInt32 & 0x007fffff | 0x3f800000) - 1

rand_generic(r::AbstractRNG, ::Close1Open2_64) =
    reinterpret(Float64, 0x3ff0000000000000 | rand(r, UInt64) & 0x000fffffffffffff)

rand_generic(r::AbstractRNG, ::CloseOpen_64) = rand(r, Close1Open2()) - 1.0

#### BigFloat

const bits_in_Limb = sizeof(Limb) << 3
const Limb_high_bit = one(Limb) << (bits_in_Limb-1)

struct SamplerBigFloat{I<:FloatInterval{BigFloat}} <: Sampler
    prec::Int
    nlimbs::Int
    limbs::Vector{Limb}
    shift::UInt

    function SamplerBigFloat{I}(prec::Int) where I<:FloatInterval{BigFloat}
        nlimbs = (prec-1) ÷ bits_in_Limb + 1
        limbs = Vector{Limb}(uninitialized, nlimbs)
        shift = nlimbs * bits_in_Limb - prec
        new(prec, nlimbs, limbs, shift)
    end
end

Sampler(::AbstractRNG, I::FloatInterval{BigFloat}, ::Repetition) =
    SamplerBigFloat{typeof(I)}(precision(BigFloat))

function _rand(rng::AbstractRNG, sp::SamplerBigFloat)
    z = BigFloat()
    limbs = sp.limbs
    rand!(rng, limbs)
    @inbounds begin
        limbs[1] <<= sp.shift
        randbool = iszero(limbs[end] & Limb_high_bit)
        limbs[end] |= Limb_high_bit
    end
    z.sign = 1
    Base.@gc_preserve limbs unsafe_copy!(z.d, pointer(limbs), sp.nlimbs)
    (z, randbool)
end

function _rand(rng::AbstractRNG, sp::SamplerBigFloat, ::Close1Open2{BigFloat})
    z = _rand(rng, sp)[1]
    z.exp = 1
    z
end

function _rand(rng::AbstractRNG, sp::SamplerBigFloat, ::CloseOpen{BigFloat})
    z, randbool = _rand(rng, sp)
    z.exp = 0
    randbool &&
        ccall((:mpfr_sub_d, :libmpfr), Int32,
              (Ref{BigFloat}, Ref{BigFloat}, Cdouble, Int32),
              z, z, 0.5, Base.MPFR.ROUNDING_MODE[])
    z
end

# alternative, with 1 bit less of precision
# TODO: make an API for requesting full or not-full precision
function _rand(rng::AbstractRNG, sp::SamplerBigFloat, ::CloseOpen{BigFloat}, ::Void)
    z = _rand(rng, sp, Close1Open2(BigFloat))
    ccall((:mpfr_sub_ui, :libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Culong, Int32),
          z, z, 1, Base.MPFR.ROUNDING_MODE[])
    z
end

rand(rng::AbstractRNG, sp::SamplerBigFloat{T}) where {T<:FloatInterval{BigFloat}} =
    _rand(rng, sp, T())

### random integers

rand_ui10_raw(r::AbstractRNG) = rand(r, UInt16)
rand_ui23_raw(r::AbstractRNG) = rand(r, UInt32)

rand_ui52_raw(r::AbstractRNG) = reinterpret(UInt64, rand(r, Close1Open2()))
rand_ui52(r::AbstractRNG) = rand_ui52_raw(r) & 0x000fffffffffffff

### random complex numbers

rand(r::AbstractRNG, ::SamplerType{Complex{T}}) where {T<:Real} =
    complex(rand(r, T), rand(r, T))

### random characters

# returns a random valid Unicode scalar value (i.e. 0 - 0xd7ff, 0xe000 - # 0x10ffff)
function rand(r::AbstractRNG, ::SamplerType{Char})
    c = rand(r, 0x00000000:0x0010f7ff)
    (c < 0xd800) ? Char(c) : Char(c+0x800)
end


## Generate random integer within a range

### BitInteger

# remainder function according to Knuth, where rem_knuth(a, 0) = a
rem_knuth(a::UInt, b::UInt) = a % (b + (b == 0)) + a * (b == 0)
rem_knuth(a::T, b::T) where {T<:Unsigned} = b != 0 ? a % b : a

# maximum multiple of k <= 2^bits(T) decremented by one,
# that is 0xFFFF...FFFF if k = typemax(T) - typemin(T) with intentional underflow
# see http://stackoverflow.com/questions/29182036/integer-arithmetic-add-1-to-uint-max-and-divide-by-n-without-overflow
maxmultiple(k::T) where {T<:Unsigned} =
    (div(typemax(T) - k + oneunit(k), k + (k == 0))*k + k - oneunit(k))::T

# maximum multiple of k within 1:2^32 or 1:2^64 decremented by one, depending on size
maxmultiplemix(k::UInt64) = k >> 32 != 0 ?
    maxmultiple(k) :
    (div(0x0000000100000000, k + (k == 0))*k - oneunit(k))::UInt64

struct SamplerRangeInt{T<:Integer,U<:Unsigned} <: Sampler
    a::T   # first element of the range
    k::U   # range length or zero for full range
    u::U   # rejection threshold
end

# generators with 32, 128 bits entropy
SamplerRangeInt(a::T, k::U) where {T,U<:Union{UInt32,UInt128}} =
    SamplerRangeInt{T,U}(a, k, maxmultiple(k))

# mixed 32/64 bits entropy generator
SamplerRangeInt(a::T, k::UInt64) where {T} =
    SamplerRangeInt{T,UInt64}(a, k, maxmultiplemix(k))

function Sampler(::AbstractRNG, r::UnitRange{T}, ::Repetition) where T<:Unsigned
    isempty(r) && throw(ArgumentError("range must be non-empty"))
    SamplerRangeInt(first(r), last(r) - first(r) + oneunit(T))
end

for (T, U) in [(UInt8, UInt32), (UInt16, UInt32),
               (Int8, UInt32), (Int16, UInt32), (Int32, UInt32),
               (Int64, UInt64), (Int128, UInt128), (Bool, UInt32)]

    @eval Sampler(::AbstractRNG, r::UnitRange{$T}, ::Repetition) = begin
        isempty(r) && throw(ArgumentError("range must be non-empty"))
        # overflow ok:
        SamplerRangeInt(first(r), convert($U, unsigned(last(r) - first(r)) + one($U)))
    end
end

# this function uses 32 bit entropy for small ranges of length <= typemax(UInt32) + 1
# SamplerRangeInt is responsible for providing the right value of k
function rand(rng::AbstractRNG, sp::SamplerRangeInt{T,UInt64}) where T<:Union{UInt64,Int64}
    local x::UInt64
    if (sp.k - 1) >> 32 == 0
        x = rand(rng, UInt32)
        while x > sp.u
            x = rand(rng, UInt32)
        end
    else
        x = rand(rng, UInt64)
        while x > sp.u
            x = rand(rng, UInt64)
        end
    end
    return reinterpret(T, reinterpret(UInt64, sp.a) + rem_knuth(x, sp.k))
end

function rand(rng::AbstractRNG, sp::SamplerRangeInt{T,U}) where {T<:Integer,U<:Unsigned}
    x = rand(rng, U)
    while x > sp.u
        x = rand(rng, U)
    end
    (unsigned(sp.a) + rem_knuth(x, sp.k)) % T
end

### BigInt

struct SamplerBigInt <: Sampler
    a::BigInt         # first
    m::BigInt         # range length - 1
    nlimbs::Int       # number of limbs in generated BigInt's (z ∈ [0, m])
    nlimbsmax::Int    # max number of limbs for z+a
    mask::Limb        # applied to the highest limb
end

function Sampler(::AbstractRNG, r::UnitRange{BigInt}, ::Repetition)
    m = last(r) - first(r)
    m < 0 && throw(ArgumentError("range must be non-empty"))
    nd = ndigits(m, 2)
    nlimbs, highbits = divrem(nd, 8*sizeof(Limb))
    highbits > 0 && (nlimbs += 1)
    mask = highbits == 0 ? ~zero(Limb) : one(Limb)<<highbits - one(Limb)
    nlimbsmax = max(nlimbs, abs(last(r).size), abs(first(r).size))
    return SamplerBigInt(first(r), m, nlimbs, nlimbsmax, mask)
end

function rand(rng::AbstractRNG, sp::SamplerBigInt)
    x = MPZ.realloc2(sp.nlimbsmax*8*sizeof(Limb))
    limbs = unsafe_wrap(Array, x.d, sp.nlimbs)
    while true
        rand!(rng, limbs)
        @inbounds limbs[end] &= sp.mask
        MPZ.mpn_cmp(x, sp.m, sp.nlimbs) <= 0 && break
    end
    # adjust x.size (normally done by mpz_limbs_finish, in GMP version >= 6)
    x.size = sp.nlimbs
    while x.size > 0
        @inbounds limbs[x.size] != 0 && break
        x.size -= 1
    end
    MPZ.add!(x, sp.a)
end


## random values from AbstractArray

Sampler(rng::AbstractRNG, r::AbstractArray, n::Repetition) =
    SamplerSimple(r, Sampler(rng, 1:length(r), n))

rand(rng::AbstractRNG, sp::SamplerSimple{<:AbstractArray,<:Sampler}) =
    @inbounds return sp[][rand(rng, sp.data)]


## random values from Dict, Set, BitSet

for x in (1, Inf) # eval because of ambiguity otherwise
    for T in (Dict, Set, BitSet)
        @eval Sampler(::AbstractRNG, t::$T, ::Val{$x}) = SamplerTrivial(t)
    end
end

function rand(rng::AbstractRNG, sp::SamplerTrivial{<:Dict})
    isempty(sp[]) && throw(ArgumentError("collection must be non-empty"))
    rsp = Sampler(rng, 1:length(sp[].slots))
    while true
        i = rand(rng, rsp)
        Base.isslotfilled(sp[], i) && @inbounds return (sp[].keys[i] => sp[].vals[i])
    end
end

rand(rng::AbstractRNG, sp::SamplerTrivial{<:Set}) = rand(rng, sp[].dict).first

function rand(rng::AbstractRNG, sp::SamplerTrivial{BitSet})
    isempty(sp[]) && throw(ArgumentError("collection must be non-empty"))
    # sp[] can be empty while sp[].bits is not, so we cannot rely on the
    # length check in Sampler below
    rsp = Sampler(rng, 1:length(sp[].bits))
    while true
        n = rand(rng, rsp)
        @inbounds b = sp[].bits[n]
        b && return n
    end
end

## random values from Associative/AbstractSet

# avoid linear complexity for repeated calls
Sampler(rng::AbstractRNG, t::Union{Associative,AbstractSet}, n::Repetition) =
    Sampler(rng, collect(t), n)

# when generating only one element, avoid the call to collect
Sampler(::AbstractRNG, t::Union{Associative,AbstractSet}, ::Val{1}) =
    SamplerTrivial(t)

function nth(iter, n::Integer)::eltype(iter)
    for (i, x) in enumerate(iter)
        i == n && return x
    end
end

rand(rng::AbstractRNG, sp::SamplerTrivial{<:Union{Associative,AbstractSet}}) =
    nth(sp[], rand(rng, 1:length(sp[])))


## random characters from a string

# we use collect(str), which is most of the time more efficient than specialized methods
# (except maybe for very small arrays)
Sampler(rng::AbstractRNG, str::AbstractString, n::Repetition) = Sampler(rng, collect(str), n)

# when generating only one char from a string, the specialized method below
# is usually more efficient
Sampler(::AbstractRNG, str::AbstractString, ::Val{1}) = SamplerTrivial(str)

isvalid_unsafe(s::String, i) = !Base.is_valid_continuation(Base.@gc_preserve s unsafe_load(pointer(s), i))
isvalid_unsafe(s::AbstractString, i) = isvalid(s, i)
_endof(s::String) = sizeof(s)
_endof(s::AbstractString) = endof(s)

function rand(rng::AbstractRNG, sp::SamplerTrivial{<:AbstractString})::Char
    str = sp[]
    sp_pos = Sampler(rng, 1:_endof(str))
    while true
        pos = rand(rng, sp_pos)
        isvalid_unsafe(str, pos) && return str[pos]
    end
end
