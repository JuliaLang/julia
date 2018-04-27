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
    Sampler(rng, CloseOpen01(T), n)

# generic random generation function which can be used by RNG implementors
# it is not defined as a fallback rand method as this could create ambiguities

rand(r::AbstractRNG, ::SamplerTrivial{CloseOpen01{Float16}}) =
    Float16(reinterpret(Float32,
                        (rand(r, UInt10(UInt32)) << 13)  | 0x3f800000) - 1)

rand(r::AbstractRNG, ::SamplerTrivial{CloseOpen01{Float32}}) =
    reinterpret(Float32, rand(r, UInt23()) | 0x3f800000) - 1

rand(r::AbstractRNG, ::SamplerTrivial{CloseOpen12_64}) =
    reinterpret(Float64, 0x3ff0000000000000 | rand(r, UInt52()))

rand(r::AbstractRNG, ::SamplerTrivial{CloseOpen01_64}) = rand(r, CloseOpen12()) - 1.0

#### BigFloat

const bits_in_Limb = sizeof(Limb) << 3
const Limb_high_bit = one(Limb) << (bits_in_Limb-1)

struct SamplerBigFloat{I<:FloatInterval{BigFloat}} <: Sampler{BigFloat}
    prec::Int
    nlimbs::Int
    limbs::Vector{Limb}
    shift::UInt

    function SamplerBigFloat{I}(prec::Int) where I<:FloatInterval{BigFloat}
        nlimbs = (prec-1) ÷ bits_in_Limb + 1
        limbs = Vector{Limb}(undef, nlimbs)
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
    GC.@preserve limbs unsafe_copyto!(z.d, pointer(limbs), sp.nlimbs)
    (z, randbool)
end

function _rand(rng::AbstractRNG, sp::SamplerBigFloat, ::CloseOpen12{BigFloat})
    z = _rand(rng, sp)[1]
    z.exp = 1
    z
end

function _rand(rng::AbstractRNG, sp::SamplerBigFloat, ::CloseOpen01{BigFloat})
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
function _rand(rng::AbstractRNG, sp::SamplerBigFloat, ::CloseOpen01{BigFloat}, ::Nothing)
    z = _rand(rng, sp, CloseOpen12(BigFloat))
    ccall((:mpfr_sub_ui, :libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Culong, Int32),
          z, z, 1, Base.MPFR.ROUNDING_MODE[])
    z
end

rand(rng::AbstractRNG, sp::SamplerBigFloat{T}) where {T<:FloatInterval{BigFloat}} =
    _rand(rng, sp, T())

### random integers

#### UniformBits

rand(r::AbstractRNG, ::SamplerTrivial{UInt10Raw{UInt16}}) = rand(r, UInt16)
rand(r::AbstractRNG, ::SamplerTrivial{UInt23Raw{UInt32}}) = rand(r, UInt32)

rand(r::AbstractRNG, ::SamplerTrivial{UInt52Raw{UInt64}}) =
    _rand52(r, rng_native_52(r))

_rand52(r::AbstractRNG, ::Type{Float64}) = reinterpret(UInt64, rand(r, CloseOpen12()))
_rand52(r::AbstractRNG, ::Type{UInt64})  = rand(r, UInt64)

rand(r::AbstractRNG, ::SamplerTrivial{UInt104Raw{UInt128}}) =
    rand(r, UInt52Raw(UInt128)) << 52 ⊻ rand(r, UInt52Raw(UInt128))

rand(r::AbstractRNG, ::SamplerTrivial{UInt10{UInt16}})   = rand(r, UInt10Raw())  & 0x03ff
rand(r::AbstractRNG, ::SamplerTrivial{UInt23{UInt32}})   = rand(r, UInt23Raw())  & 0x007fffff
rand(r::AbstractRNG, ::SamplerTrivial{UInt52{UInt64}})   = rand(r, UInt52Raw())  & 0x000fffffffffffff
rand(r::AbstractRNG, ::SamplerTrivial{UInt104{UInt128}}) = rand(r, UInt104Raw()) & 0x000000ffffffffffffffffffffffffff

rand(r::AbstractRNG, sp::SamplerTrivial{<:UniformBits{T}}) where {T} =
        rand(r, uint_default(sp[])) % T

#### BitInteger

# rand_generic methods are intended to help RNG implementors with common operations
# we don't call them simply `rand` as this can easily contribute to create
# amibuities with user-side methods (forcing the user to resort to @eval)

rand_generic(r::AbstractRNG, T::Union{Bool,Int8,UInt8,Int16,UInt16,Int32,UInt32}) =
    rand(r, UInt52Raw()) % T[]

rand_generic(r::AbstractRNG, ::Type{UInt64}) =
    rand(r, UInt52Raw()) << 32 ⊻ rand(r, UInt52Raw())

rand_generic(r::AbstractRNG, ::Type{UInt128}) = _rand128(r, rng_native_52(r))

_rand128(r::AbstractRNG, ::Type{UInt64}) =
    ((rand(r, UInt64) % UInt128) << 64) ⊻ rand(r, UInt64)

function _rand128(r::AbstractRNG, ::Type{Float64})
    xor(rand(r, UInt52Raw(UInt128))  << 96,
        rand(r, UInt52Raw(UInt128))  << 48,
        rand(r, UInt52Raw(UInt128)))
end

rand_generic(r::AbstractRNG, ::Type{Int128}) = rand(r, UInt128) % Int128
rand_generic(r::AbstractRNG, ::Type{Int64})  = rand(r, UInt64) % Int64

### random complex numbers

rand(r::AbstractRNG, ::SamplerType{Complex{T}}) where {T<:Real} =
    complex(rand(r, T), rand(r, T))

### random characters

# returns a random valid Unicode scalar value (i.e. 0 - 0xd7ff, 0xe000 - # 0x10ffff)
function rand(r::AbstractRNG, ::SamplerType{T}) where {T<:AbstractChar}
    c = rand(r, 0x00000000:0x0010f7ff)
    (c < 0xd800) ? T(c) : T(c+0x800)
end


## Generate random integer within a range

### BitInteger

# there are two implemented samplers for unit ranges, which assume that Float64 (i.e.
# 52 random bits) is the native type for the RNG:
# 1) "Fast", which is the most efficient when the underlying RNG produces rand(Float64)
#     "fast enough". The tradeoff is faster creation of the sampler, but more
#     consumption of entropy bits
# 2) "Default" which tries to use as few entropy bits as possible, at the cost of a
#    a bigger upfront price associated with the creation of the sampler

#### helper functions

uint_sup(::Type{<:Base.BitInteger32}) = UInt32
uint_sup(::Type{<:Union{Int64,UInt64}}) = UInt64
uint_sup(::Type{<:Union{Int128,UInt128}}) = UInt128

#### Fast

struct SamplerRangeFast{U<:BitUnsigned,T<:BitInteger} <: Sampler{T}
    a::T      # first element of the range
    bw::UInt  # bit width
    m::U      # range length - 1
    mask::U   # mask generated values before threshold rejection
end

SamplerRangeFast(r::AbstractUnitRange{T}) where T<:BitInteger =
    SamplerRangeFast(r, uint_sup(T))

function SamplerRangeFast(r::AbstractUnitRange{T}, ::Type{U}) where {T,U}
    isempty(r) && throw(ArgumentError("range must be non-empty"))
    m = (last(r) - first(r)) % unsigned(T) % U # % unsigned(T) to not propagate sign bit
    bw = (sizeof(U) << 3 - leading_zeros(m)) % UInt # bit-width
    mask = (1 % U << bw) - (1 % U)
    SamplerRangeFast{U,T}(first(r), bw, m, mask)
end

function rand(rng::AbstractRNG, sp::SamplerRangeFast{UInt32,T}) where T
    a, bw, m, mask = sp.a, sp.bw, sp.m, sp.mask
    # below, we don't use UInt32, to get reproducible values, whether Int is Int64 or Int32
    x = rand(rng, LessThan(m, Masked(mask, UInt52Raw(UInt32))))
    (x + a % UInt32) % T
end

function rand(rng::AbstractRNG, sp::SamplerRangeFast{UInt64,T}) where T
    a, bw, m, mask = sp.a, sp.bw, sp.m, sp.mask
    x = bw <= 52 ? rand(rng, LessThan(m, Masked(mask, UInt52Raw()))) :
                   rand(rng, LessThan(m, Masked(mask, uniform(UInt64))))
    (x + a % UInt64) % T
end

function rand(rng::AbstractRNG, sp::SamplerRangeFast{UInt128,T}) where T
    a, bw, m, mask = sp.a, sp.bw, sp.m, sp.mask
    x = bw <= 52  ?
        rand(rng, LessThan(m % UInt64, Masked(mask % UInt64, UInt52Raw()))) % UInt128 :
    bw <= 104 ?
        rand(rng, LessThan(m, Masked(mask, UInt104Raw()))) :
        rand(rng, LessThan(m, Masked(mask, uniform(UInt128))))
    x % T + a
end

#### Default

# remainder function according to Knuth, where rem_knuth(a, 0) = a
rem_knuth(a::UInt, b::UInt) = a % (b + (b == 0)) + a * (b == 0)
rem_knuth(a::T, b::T) where {T<:Unsigned} = b != 0 ? a % b : a

# maximum multiple of k <= sup decremented by one,
# that is 0xFFFF...FFFF if k = (typemax(T) - typemin(T)) + 1 and sup == typemax(T) - 1
# with intentional underflow
# see http://stackoverflow.com/questions/29182036/integer-arithmetic-add-1-to-uint-max-and-divide-by-n-without-overflow

# sup == 0 means typemax(T) + 1
maxmultiple(k::T, sup::T=zero(T)) where {T<:Unsigned} =
    (div(sup - k, k + (k == 0))*k + k - one(k))::T

# similar but sup must not be equal to typemax(T)
unsafe_maxmultiple(k::T, sup::T) where {T<:Unsigned} =
    div(sup, k + (k == 0))*k - one(k)

struct SamplerRangeInt{T<:Integer,U<:Unsigned} <: Sampler{T}
    a::T      # first element of the range
    bw::Int   # bit width
    k::U      # range length or zero for full range
    u::U      # rejection threshold
end


SamplerRangeInt(r::AbstractUnitRange{T}) where T<:BitInteger =
    SamplerRangeInt(r, uint_sup(T))

function SamplerRangeInt(r::AbstractUnitRange{T}, ::Type{U}) where {T,U}
    isempty(r) && throw(ArgumentError("range must be non-empty"))
    a = first(r)
    m = (last(r) - first(r)) % unsigned(T) % U
    k = m + one(U)
    bw = (sizeof(U) << 3 - leading_zeros(m)) % Int
    mult = if U === UInt32
        maxmultiple(k)
    elseif U === UInt64
        bw <= 52 ? unsafe_maxmultiple(k, one(UInt64) << 52) :
                   maxmultiple(k)
    else # U === UInt128
        bw <= 52  ? unsafe_maxmultiple(k, one(UInt128) << 52) :
        bw <= 104 ? unsafe_maxmultiple(k, one(UInt128) << 104) :
                    maxmultiple(k)
    end

    SamplerRangeInt{T,U}(a, bw, k, mult) # overflow ok
end

Sampler(::AbstractRNG, r::AbstractUnitRange{T},
        ::Repetition) where {T<:BitInteger} = SamplerRangeInt(r)


rand(rng::AbstractRNG, sp::SamplerRangeInt{T,UInt32}) where {T<:BitInteger} =
    (unsigned(sp.a) + rem_knuth(rand(rng, LessThan(sp.u, UInt52Raw(UInt32))), sp.k)) % T

# this function uses 52 bit entropy for small ranges of length <= 2^52
function rand(rng::AbstractRNG, sp::SamplerRangeInt{T,UInt64}) where T<:BitInteger
    x = sp.bw <= 52 ? rand(rng, LessThan(sp.u, UInt52())) :
                      rand(rng, LessThan(sp.u, uniform(UInt64)))
    return ((sp.a % UInt64) + rem_knuth(x, sp.k)) % T
end

function rand(rng::AbstractRNG, sp::SamplerRangeInt{T,UInt128}) where T<:BitInteger
    x = sp.bw <= 52  ? rand(rng, LessThan(sp.u, UInt52(UInt128))) :
        sp.bw <= 104 ? rand(rng, LessThan(sp.u, UInt104(UInt128))) :
                       rand(rng, LessThan(sp.u, uniform(UInt128)))
    return ((sp.a % UInt128) + rem_knuth(x, sp.k)) % T
end


### BigInt

struct SamplerBigInt <: Sampler{BigInt}
    a::BigInt         # first
    m::BigInt         # range length - 1
    nlimbs::Int       # number of limbs in generated BigInt's (z ∈ [0, m])
    nlimbsmax::Int    # max number of limbs for z+a
    mask::Limb        # applied to the highest limb
end

function Sampler(::AbstractRNG, r::AbstractUnitRange{BigInt}, ::Repetition)
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
    GC.@preserve x begin
        limbs = UnsafeView(x.d, sp.nlimbs)
        while true
            rand!(rng, limbs)
            limbs[end] &= sp.mask
            MPZ.mpn_cmp(x, sp.m, sp.nlimbs) <= 0 && break
        end
        # adjust x.size (normally done by mpz_limbs_finish, in GMP version >= 6)
        x.size = sp.nlimbs
        while x.size > 0
            limbs[x.size] != 0 && break
            x.size -= 1
        end
    end
    MPZ.add!(x, sp.a)
end


## random values from AbstractArray

Sampler(rng::AbstractRNG, r::AbstractArray, n::Repetition) =
    SamplerSimple(r, Sampler(rng, linearindices(r), n))

rand(rng::AbstractRNG, sp::SamplerSimple{<:AbstractArray,<:Sampler}) =
    @inbounds return sp[][rand(rng, sp.data)]


## random values from Dict

function Sampler(rng::AbstractRNG, t::Dict, ::Repetition)
    isempty(t) && throw(ArgumentError("collection must be non-empty"))
    # we use Val(Inf) below as rand is called repeatedly internally
    # even for generating only one random value from t
    SamplerSimple(t, Sampler(rng, linearindices(t.slots), Val(Inf)))
end

function rand(rng::AbstractRNG, sp::SamplerSimple{<:Dict,<:Sampler})
    while true
        i = rand(rng, sp.data)
        Base.isslotfilled(sp[], i) && @inbounds return (sp[].keys[i] => sp[].vals[i])
    end
end

## random values from Set

Sampler(rng::AbstractRNG, t::Set{T}, n::Repetition) where {T} =
    SamplerTag{Set{T}}(Sampler(rng, t.dict, n))

rand(rng::AbstractRNG, sp::SamplerTag{<:Set,<:Sampler}) = rand(rng, sp.data).first

## random values from BitSet

function Sampler(rng::AbstractRNG, t::BitSet, n::Repetition)
    isempty(t) && throw(ArgumentError("collection must be non-empty"))
    SamplerSimple(t, Sampler(rng, minimum(t):maximum(t), Val(Inf)))
end

function rand(rng::AbstractRNG, sp::SamplerSimple{BitSet,<:Sampler})
    while true
        n = rand(rng, sp.data)
        n in sp[] && return n
    end
end

## random values from AbstractDict/AbstractSet

# we defer to _Sampler to avoid ambiguities with a call like Sampler(rng, Set(1), Val(1))
Sampler(rng::AbstractRNG, t::Union{AbstractDict,AbstractSet}, n::Repetition) =
    _Sampler(rng, t, n)

# avoid linear complexity for repeated calls
_Sampler(rng::AbstractRNG, t::Union{AbstractDict,AbstractSet}, n::Val{Inf}) =
    Sampler(rng, collect(t), n)

# when generating only one element, avoid the call to collect
_Sampler(::AbstractRNG, t::Union{AbstractDict,AbstractSet}, ::Val{1}) =
    SamplerTrivial(t)

function nth(iter, n::Integer)::eltype(iter)
    for (i, x) in enumerate(iter)
        i == n && return x
    end
end

rand(rng::AbstractRNG, sp::SamplerTrivial{<:Union{AbstractDict,AbstractSet}}) =
    nth(sp[], rand(rng, 1:length(sp[])))


## random characters from a string

# we use collect(str), which is most of the time more efficient than specialized methods
# (except maybe for very small arrays)
Sampler(rng::AbstractRNG, str::AbstractString, n::Val{Inf}) = Sampler(rng, collect(str), n)

# when generating only one char from a string, the specialized method below
# is usually more efficient
Sampler(rng::AbstractRNG, str::AbstractString, ::Val{1}) =
    SamplerSimple(str, Sampler(rng, 1:_lastindex(str), Val(Inf)))

isvalid_unsafe(s::String, i) = !Base.is_valid_continuation(GC.@preserve s unsafe_load(pointer(s), i))
isvalid_unsafe(s::AbstractString, i) = isvalid(s, i)
_lastindex(s::String) = sizeof(s)
_lastindex(s::AbstractString) = lastindex(s)

function rand(rng::AbstractRNG, sp::SamplerSimple{<:AbstractString,<:Sampler})::Char
    str = sp[]
    while true
        pos = rand(rng, sp.data)
        isvalid_unsafe(str, pos) && return str[pos]
    end
end
