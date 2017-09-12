# This file is a part of Julia. License is MIT: https://julialang.org/license

# Uniform random generation

## from types: rand(::Type, [dims...])

### GLOBAL_RNG fallback for all types

rand(::Type{T}) where {T} = rand(GLOBAL_RNG, T)

### random floats

# CloseOpen(T) is the fallback for an AbstractFloat T
rand(r::AbstractRNG=GLOBAL_RNG, ::Type{T}=Float64) where {T<:AbstractFloat} =
    rand(r, CloseOpen(T))

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

struct BigFloatRandGenerator
    prec::Int
    nlimbs::Int
    limbs::Vector{Limb}
    shift::UInt

    function BigFloatRandGenerator(prec::Int=precision(BigFloat))
        nlimbs = (prec-1) ÷ bits_in_Limb + 1
        limbs = Vector{Limb}(nlimbs)
        shift = nlimbs * bits_in_Limb - prec
        new(prec, nlimbs, limbs, shift)
    end
end

function _rand(rng::AbstractRNG, gen::BigFloatRandGenerator)
    z = BigFloat()
    limbs = gen.limbs
    rand!(rng, limbs)
    @inbounds begin
        limbs[1] <<= gen.shift
        randbool = iszero(limbs[end] & Limb_high_bit)
        limbs[end] |= Limb_high_bit
    end
    z.sign = 1
    unsafe_copy!(z.d, pointer(limbs), gen.nlimbs)
    (z, randbool)
end

function rand(rng::AbstractRNG, gen::BigFloatRandGenerator, ::Close1Open2{BigFloat})
    z = _rand(rng, gen)[1]
    z.exp = 1
    z
end

function rand(rng::AbstractRNG, gen::BigFloatRandGenerator, ::CloseOpen{BigFloat})
    z, randbool = _rand(rng, gen)
    z.exp = 0
    randbool &&
        ccall((:mpfr_sub_d, :libmpfr), Int32,
              (Ref{BigFloat}, Ref{BigFloat}, Cdouble, Int32),
              z, z, 0.5, Base.MPFR.ROUNDING_MODE[])
    z
end

# alternative, with 1 bit less of precision
# TODO: make an API for requesting full or not-full precision
function rand(rng::AbstractRNG, gen::BigFloatRandGenerator, ::CloseOpen{BigFloat}, ::Void)
    z = rand(rng, Close1Open2(BigFloat), gen)
    ccall((:mpfr_sub_ui, :libmpfr), Int32, (Ref{BigFloat}, Ref{BigFloat}, Culong, Int32),
          z, z, 1, Base.MPFR.ROUNDING_MODE[])
    z
end

rand_generic(rng::AbstractRNG, I::FloatInterval{BigFloat}) =
    rand(rng, BigFloatRandGenerator(), I)

### random integers

rand_ui10_raw(r::AbstractRNG) = rand(r, UInt16)
rand_ui23_raw(r::AbstractRNG) = rand(r, UInt32)

rand_ui52_raw(r::AbstractRNG) = reinterpret(UInt64, rand(r, Close1Open2()))
rand_ui52(r::AbstractRNG) = rand_ui52_raw(r) & 0x000fffffffffffff

### random complex numbers

rand(r::AbstractRNG, ::Type{Complex{T}}) where {T<:Real} = complex(rand(r, T), rand(r, T))

### random characters

# returns a random valid Unicode scalar value (i.e. 0 - 0xd7ff, 0xe000 - # 0x10ffff)
function rand(r::AbstractRNG, ::Type{Char})
    c = rand(r, 0x00000000:0x0010f7ff)
    (c < 0xd800) ? Char(c) : Char(c+0x800)
end

### arrays of random numbers

function rand!(r::AbstractRNG, A::AbstractArray{T}, ::Type{X}=T) where {T,X}
    for i in eachindex(A)
        @inbounds A[i] = rand(r, X)
    end
    A
end

rand!(A::AbstractArray, ::Type{X}) where {X} = rand!(GLOBAL_RNG, A, X)
# NOTE: if the second parameter above is defaulted to eltype(A) and the
# method below is removed, then some specialized methods (e.g. for
# rand!(::Array{Float64})) will fail to be called
rand!(A::AbstractArray) = rand!(GLOBAL_RNG, A)


rand(r::AbstractRNG, dims::Dims)       = rand(r, Float64, dims)
rand(                dims::Dims)       = rand(GLOBAL_RNG, dims)
rand(r::AbstractRNG, dims::Integer...) = rand(r, Dims(dims))
rand(                dims::Integer...) = rand(Dims(dims))

rand(r::AbstractRNG, ::Type{T}, dims::Dims) where {T} = rand!(r, Array{T}(dims))
rand(                ::Type{T}, dims::Dims) where {T} = rand(GLOBAL_RNG, T, dims)

rand(r::AbstractRNG, ::Type{T}, d::Integer, dims::Integer...) where {T} =
    rand(r, T, Dims((d, dims...)))

rand(                ::Type{T}, d::Integer, dims::Integer...) where {T} =
    rand(T, Dims((d, dims...)))
# note: the above methods would trigger an ambiguity warning if d was not separated out:
# rand(r, ()) would match both this method and rand(r, dims::Dims)
# moreover, a call like rand(r, NotImplementedType()) would be an infinite loop

#### arrays of floats

rand!(r::AbstractRNG, A::AbstractArray, ::Type{T}) where {T<:AbstractFloat} =
    rand!(r, A, CloseOpen{T}())

function rand!(r::AbstractRNG, A::AbstractArray, I::FloatInterval)
    for i in eachindex(A)
        @inbounds A[i] = rand(r, I)
    end
    A
end

function rand!(rng::AbstractRNG, A::AbstractArray, I::FloatInterval{BigFloat})
    gen = BigFloatRandGenerator()
    for i in eachindex(A)
        @inbounds A[i] = rand(rng, gen, I)
    end
    A
end

rand!(A::AbstractArray, I::FloatInterval) = rand!(GLOBAL_RNG, A, I)

## Generate random integer within a range

abstract type RangeGenerator end

### RangeGenerator for BitInteger

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

struct RangeGeneratorInt{T<:Integer,U<:Unsigned} <: RangeGenerator
    a::T   # first element of the range
    k::U   # range length or zero for full range
    u::U   # rejection threshold
end

# generators with 32, 128 bits entropy
RangeGeneratorInt(a::T, k::U) where {T,U<:Union{UInt32,UInt128}} =
    RangeGeneratorInt{T,U}(a, k, maxmultiple(k))

# mixed 32/64 bits entropy generator
RangeGeneratorInt(a::T, k::UInt64) where {T} =
    RangeGeneratorInt{T,UInt64}(a, k, maxmultiplemix(k))

function RangeGenerator(r::UnitRange{T}) where T<:Unsigned
    isempty(r) && throw(ArgumentError("range must be non-empty"))
    RangeGeneratorInt(first(r), last(r) - first(r) + oneunit(T))
end

for (T, U) in [(UInt8, UInt32), (UInt16, UInt32),
               (Int8, UInt32), (Int16, UInt32), (Int32, UInt32),
               (Int64, UInt64), (Int128, UInt128), (Bool, UInt32)]

    @eval RangeGenerator(r::UnitRange{$T}) = begin
        isempty(r) && throw(ArgumentError("range must be non-empty"))
        # overflow ok:
        RangeGeneratorInt(first(r), convert($U, unsigned(last(r) - first(r)) + one($U)))
    end
end

### RangeGenerator for BigInt

struct RangeGeneratorBigInt <: RangeGenerator
    a::BigInt         # first
    m::BigInt         # range length - 1
    nlimbs::Int       # number of limbs in generated BigInt's (z ∈ [0, m])
    nlimbsmax::Int    # max number of limbs for z+a
    mask::Limb        # applied to the highest limb
end


function RangeGenerator(r::UnitRange{BigInt})
    m = last(r) - first(r)
    m < 0 && throw(ArgumentError("range must be non-empty"))
    nd = ndigits(m, 2)
    nlimbs, highbits = divrem(nd, 8*sizeof(Limb))
    highbits > 0 && (nlimbs += 1)
    mask = highbits == 0 ? ~zero(Limb) : one(Limb)<<highbits - one(Limb)
    nlimbsmax = max(nlimbs, abs(last(r).size), abs(first(r).size))
    return RangeGeneratorBigInt(first(r), m, nlimbs, nlimbsmax, mask)
end

### rand(::RangeGenerator)

# this function uses 32 bit entropy for small ranges of length <= typemax(UInt32) + 1
# RangeGeneratorInt is responsible for providing the right value of k
function rand(rng::AbstractRNG, g::RangeGeneratorInt{T,UInt64}) where T<:Union{UInt64,Int64}
    local x::UInt64
    if (g.k - 1) >> 32 == 0
        x = rand(rng, UInt32)
        while x > g.u
            x = rand(rng, UInt32)
        end
    else
        x = rand(rng, UInt64)
        while x > g.u
            x = rand(rng, UInt64)
        end
    end
    return reinterpret(T, reinterpret(UInt64, g.a) + rem_knuth(x, g.k))
end

function rand(rng::AbstractRNG, g::RangeGeneratorInt{T,U}) where {T<:Integer,U<:Unsigned}
    x = rand(rng, U)
    while x > g.u
        x = rand(rng, U)
    end
    (unsigned(g.a) + rem_knuth(x, g.k)) % T
end

function rand(rng::AbstractRNG, g::RangeGeneratorBigInt)
    x = MPZ.realloc2(g.nlimbsmax*8*sizeof(Limb))
    limbs = unsafe_wrap(Array, x.d, g.nlimbs)
    while true
        rand!(rng, limbs)
        @inbounds limbs[end] &= g.mask
        MPZ.mpn_cmp(x, g.m, g.nlimbs) <= 0 && break
    end
    # adjust x.size (normally done by mpz_limbs_finish, in GMP version >= 6)
    x.size = g.nlimbs
    while x.size > 0
        @inbounds limbs[x.size] != 0 && break
        x.size -= 1
    end
    MPZ.add!(x, g.a)
end

#### arrays

function rand!(rng::AbstractRNG, A::AbstractArray, g::RangeGenerator)
    for i in eachindex(A)
        @inbounds A[i] = rand(rng, g)
    end
    return A
end

### random values from UnitRange

rand(rng::AbstractRNG, r::UnitRange{<:Integer}) = rand(rng, RangeGenerator(r))

rand!(rng::AbstractRNG, A::AbstractArray, r::UnitRange{<:Integer}) =
    rand!(rng, A, RangeGenerator(r))

## random values from AbstractArray

rand(rng::AbstractRNG, r::AbstractArray) = @inbounds return r[rand(rng, 1:length(r))]
rand(                  r::AbstractArray) = rand(GLOBAL_RNG, r)

### arrays

function rand!(rng::AbstractRNG, A::AbstractArray, r::AbstractArray)
    g = RangeGenerator(1:(length(r)))
    for i in eachindex(A)
        @inbounds A[i] = r[rand(rng, g)]
    end
    return A
end

rand!(A::AbstractArray, r::AbstractArray) = rand!(GLOBAL_RNG, A, r)

rand(rng::AbstractRNG, r::AbstractArray{T}, dims::Dims) where {T} =
    rand!(rng, Array{T}(dims), r)
rand(                  r::AbstractArray, dims::Dims)       = rand(GLOBAL_RNG, r, dims)
rand(rng::AbstractRNG, r::AbstractArray, dims::Integer...) = rand(rng, r, Dims(dims))
rand(                  r::AbstractArray, dims::Integer...) = rand(GLOBAL_RNG, r, Dims(dims))


## random values from Dict, Set, IntSet

function rand(r::AbstractRNG, t::Dict)
    isempty(t) && throw(ArgumentError("collection must be non-empty"))
    rg = RangeGenerator(1:length(t.slots))
    while true
        i = rand(r, rg)
        Base.isslotfilled(t, i) && @inbounds return (t.keys[i] => t.vals[i])
    end
end

rand(r::AbstractRNG, s::Set) = rand(r, s.dict).first

function rand(r::AbstractRNG, s::IntSet)
    isempty(s) && throw(ArgumentError("collection must be non-empty"))
    # s can be empty while s.bits is not, so we cannot rely on the
    # length check in RangeGenerator below
    rg = RangeGenerator(1:length(s.bits))
    while true
        n = rand(r, rg)
        @inbounds b = s.bits[n]
        b && return n
    end
end

function nth(iter, n::Integer)::eltype(iter)
    for (i, x) in enumerate(iter)
        i == n && return x
    end
end
nth(iter::AbstractArray, n::Integer) = iter[n]

rand(r::AbstractRNG, s::Union{Associative,AbstractSet}) = nth(s, rand(r, 1:length(s)))

rand(s::Union{Associative,AbstractSet}) = rand(GLOBAL_RNG, s)

### arrays

function rand!(r::AbstractRNG, A::AbstractArray, s::Union{Dict,Set,IntSet})
    for i in eachindex(A)
        @inbounds A[i] = rand(r, s)
    end
    A
end

# avoid linear complexity for repeated calls with generic containers
rand!(r::AbstractRNG, A::AbstractArray, s::Union{Associative,AbstractSet}) =
    rand!(r, A, collect(s))

rand!(A::AbstractArray, s::Union{Associative,AbstractSet}) = rand!(GLOBAL_RNG, A, s)

rand(r::AbstractRNG, s::Associative{K,V}, dims::Dims) where {K,V} =
    rand!(r, Array{Pair{K,V}}(dims), s)

rand(r::AbstractRNG, s::AbstractSet{T}, dims::Dims) where {T} = rand!(r, Array{T}(dims), s)
rand(r::AbstractRNG, s::Union{Associative,AbstractSet}, dims::Integer...) =
    rand(r, s, Dims(dims))
rand(s::Union{Associative,AbstractSet}, dims::Integer...) = rand(GLOBAL_RNG, s, Dims(dims))
rand(s::Union{Associative,AbstractSet}, dims::Dims) = rand(GLOBAL_RNG, s, dims)


## random characters from a string

isvalid_unsafe(s::String, i) = !Base.is_valid_continuation(unsafe_load(pointer(s), i))
isvalid_unsafe(s::AbstractString, i) = isvalid(s, i)
_endof(s::String) = sizeof(s)
_endof(s::AbstractString) = endof(s)

function rand(rng::AbstractRNG, s::AbstractString)::Char
    g = RangeGenerator(1:_endof(s))
    while true
        pos = rand(rng, g)
        isvalid_unsafe(s, pos) && return s[pos]
    end
end

rand(s::AbstractString) = rand(GLOBAL_RNG, s)

### arrays

# we use collect(str), which is most of the time more efficient than specialized methods
# (except maybe for very small arrays)
rand!(rng::AbstractRNG, A::AbstractArray, str::AbstractString) = rand!(rng, A, collect(str))
rand!(A::AbstractArray, str::AbstractString) = rand!(GLOBAL_RNG, A, str)
rand(rng::AbstractRNG, str::AbstractString, dims::Dims) =
    rand!(rng, Array{eltype(str)}(dims), str)

rand(rng::AbstractRNG, str::AbstractString, d::Integer, dims::Integer...) =
    rand(rng, str, Dims((d, dims...)))

rand(str::AbstractString, dims::Dims) = rand(GLOBAL_RNG, str, dims)
rand(str::AbstractString, d::Integer, dims::Integer...) = rand(GLOBAL_RNG, str, d, dims...)
