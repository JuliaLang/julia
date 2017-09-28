# This file is a part of Julia. License is MIT: https://julialang.org/license

module Random

using Base.dSFMT
using Base.GMP: GMP_VERSION, Limb
import Base: copymutable, copy, copy!, ==

export srand,
       rand, rand!,
       randn, randn!,
       randexp, randexp!,
       bitrand,
       randstring,
       randsubseq,randsubseq!,
       shuffle,shuffle!,
       randperm, randcycle,
       AbstractRNG, MersenneTwister, RandomDevice,
       GLOBAL_RNG, randjump


abstract type AbstractRNG end

abstract type FloatInterval end
mutable struct CloseOpen <: FloatInterval end
mutable struct Close1Open2 <: FloatInterval end


## RandomDevice

if is_windows()

    struct RandomDevice <: AbstractRNG
        buffer::Vector{UInt128}

        RandomDevice() = new(Vector{UInt128}(1))
    end

    function rand{T<:Union{Bool, Base.BitInteger}}(rd::RandomDevice, ::Type{T})
        win32_SystemFunction036!(rd.buffer)
        @inbounds return rd.buffer[1] % T
    end

    rand!(rd::RandomDevice, A::Array{<:Union{Bool, Base.BitInteger}}) = (win32_SystemFunction036!(A); A)
else # !windows
    struct RandomDevice <: AbstractRNG
        file::IOStream
        unlimited::Bool

        RandomDevice(unlimited::Bool=true) = new(open(unlimited ? "/dev/urandom" : "/dev/random"), unlimited)
    end

    rand{T<:Union{Bool, Base.BitInteger}}(rd::RandomDevice, ::Type{T}) = read( rd.file, T)
    rand!(rd::RandomDevice, A::Array{<:Union{Bool, Base.BitInteger}})  = read!(rd.file, A)
end # os-test


"""
    RandomDevice()

Create a `RandomDevice` RNG object. Two such objects will always generate different streams of random numbers.
"""
RandomDevice


rand(rng::RandomDevice, ::Type{Close1Open2}) =
    reinterpret(Float64, 0x3ff0000000000000 | rand(rng, UInt64) & 0x000fffffffffffff)

rand(rng::RandomDevice, ::Type{CloseOpen}) = rand(rng, Close1Open2) - 1.0


## MersenneTwister

const MTCacheLength = dsfmt_get_min_array_size()

mutable struct MersenneTwister <: AbstractRNG
    seed::Vector{UInt32}
    state::DSFMT_state
    vals::Vector{Float64}
    idx::Int

    function MersenneTwister(seed, state, vals, idx)
        length(vals) == MTCacheLength &&  0 <= idx <= MTCacheLength || throw(DomainError())
        new(seed, state, vals, idx)
    end
end

MersenneTwister(seed::Vector{UInt32}, state::DSFMT_state) =
    MersenneTwister(seed, state, zeros(Float64, MTCacheLength), MTCacheLength)

"""
    MersenneTwister(seed)

Create a `MersenneTwister` RNG object. Different RNG objects can have their own seeds, which
may be useful for generating different streams of random numbers.

# Example
```jldoctest
julia> rng = MersenneTwister(1234);
```
"""
MersenneTwister(seed) = srand(MersenneTwister(Vector{UInt32}(), DSFMT_state()), seed)

function copy!(dst::MersenneTwister, src::MersenneTwister)
    copy!(resize!(dst.seed, length(src.seed)), src.seed)
    copy!(dst.state, src.state)
    copy!(dst.vals, src.vals)
    dst.idx = src.idx
    dst
end

copy(src::MersenneTwister) =
    MersenneTwister(copy(src.seed), copy(src.state), copy(src.vals), src.idx)

==(r1::MersenneTwister, r2::MersenneTwister) =
    r1.seed == r2.seed && r1.state == r2.state && isequal(r1.vals, r2.vals) && r1.idx == r2.idx


## Low level API for MersenneTwister

@inline mt_avail(r::MersenneTwister) = MTCacheLength - r.idx
@inline mt_empty(r::MersenneTwister) = r.idx == MTCacheLength
@inline mt_setfull!(r::MersenneTwister) = r.idx = 0
@inline mt_setempty!(r::MersenneTwister) = r.idx = MTCacheLength
@inline mt_pop!(r::MersenneTwister) = @inbounds return r.vals[r.idx+=1]

function gen_rand(r::MersenneTwister)
    dsfmt_fill_array_close1_open2!(r.state, pointer(r.vals), length(r.vals))
    mt_setfull!(r)
end

@inline reserve_1(r::MersenneTwister) = mt_empty(r) && gen_rand(r)
# `reserve` allows one to call `rand_inbounds` n times
# precondition: n <= MTCacheLength
@inline reserve(r::MersenneTwister, n::Int) = mt_avail(r) < n && gen_rand(r)

# precondition: !mt_empty(r)
@inline rand_inbounds(r::MersenneTwister, ::Type{Close1Open2}) = mt_pop!(r)
@inline rand_inbounds(r::MersenneTwister, ::Type{CloseOpen}) = rand_inbounds(r, Close1Open2) - 1.0
@inline rand_inbounds(r::MersenneTwister) = rand_inbounds(r, CloseOpen)

# produce Float64 values
@inline rand{I<:FloatInterval}(r::MersenneTwister, ::Type{I}) = (reserve_1(r); rand_inbounds(r, I))

@inline rand_ui52_raw_inbounds(r::MersenneTwister) = reinterpret(UInt64, rand_inbounds(r, Close1Open2))
@inline rand_ui52_raw(r::MersenneTwister) = (reserve_1(r); rand_ui52_raw_inbounds(r))
@inline rand_ui2x52_raw(r::MersenneTwister) = rand_ui52_raw(r) % UInt128 << 64 | rand_ui52_raw(r)

function srand(r::MersenneTwister, seed::Vector{UInt32})
    copy!(resize!(r.seed, length(seed)), seed)
    dsfmt_init_by_array(r.state, r.seed)
    mt_setempty!(r)
    return r
end

# MersenneTwister jump

"""
    randjump(r::MersenneTwister, jumps::Integer, [jumppoly::AbstractString=dSFMT.JPOLY1e21]) -> Vector{MersenneTwister}

Create an array of the size `jumps` of initialized `MersenneTwister` RNG objects. The
first RNG object given as a parameter and following `MersenneTwister` RNGs in the array are
initialized such that a state of the RNG object in the array would be moved forward (without
generating numbers) from a previous RNG object array element on a particular number of steps
encoded by the jump polynomial `jumppoly`.

Default jump polynomial moves forward `MersenneTwister` RNG state by `10^20` steps.
"""
function randjump(mt::MersenneTwister, jumps::Integer, jumppoly::AbstractString)
    mts = MersenneTwister[]
    push!(mts, mt)
    for i in 1:jumps-1
        cmt = mts[end]
        push!(mts, MersenneTwister(cmt.seed, dSFMT.dsfmt_jump(cmt.state, jumppoly)))
    end
    return mts
end
randjump(r::MersenneTwister, jumps::Integer) = randjump(r, jumps, dSFMT.JPOLY1e21)

## initialization

function __init__()
    try
        srand()
    catch ex
        Base.showerror_nostdio(ex,
            "WARNING: Error during initialization of module Random")
    end
end


## make_seed()
# make_seed methods produce values of type Array{UInt32}, suitable for MersenneTwister seeding

function make_seed()
    try
        return rand(RandomDevice(), UInt32, 4)
    catch
        println(STDERR, "Entropy pool not available to seed RNG; using ad-hoc entropy sources.")
        seed = reinterpret(UInt64, time())
        seed = hash(seed, UInt64(getpid()))
        try
        seed = hash(seed, parse(UInt64, readstring(pipeline(`ifconfig`, `sha1sum`))[1:40], 16))
        end
        return make_seed(seed)
    end
end

function make_seed(n::Integer)
    n < 0 && throw(DomainError())
    seed = UInt32[]
    while true
        push!(seed, n & 0xffffffff)
        n >>= 32
        if n == 0
            return seed
        end
    end
end

function make_seed(filename::AbstractString, n::Integer)
    read!(filename, Vector{UInt32}(Int(n)))
end

## srand()

"""
    srand([rng=GLOBAL_RNG], [seed]) -> rng
    srand([rng=GLOBAL_RNG], filename, n=4) -> rng

Reseed the random number generator. If a `seed` is provided, the RNG will give a
reproducible sequence of numbers, otherwise Julia will get entropy from the system. For
`MersenneTwister`, the `seed` may be a non-negative integer, a vector of [`UInt32`](@ref) integers
or a filename, in which case the seed is read from a file (`4n` bytes are read from the file,
where `n` is an optional argument). `RandomDevice` does not support seeding.
"""
srand(r::MersenneTwister) = srand(r, make_seed())
srand(r::MersenneTwister, n::Integer) = srand(r, make_seed(n))
srand(r::MersenneTwister, filename::AbstractString, n::Integer=4) = srand(r, make_seed(filename, n))


function dsfmt_gv_srand()
    # Temporary fix for #8874 and #9124: update global RNG for Rmath
    dsfmt_gv_init_by_array(GLOBAL_RNG.seed+UInt32(1))
    return GLOBAL_RNG
end

function srand()
    srand(GLOBAL_RNG)
    dsfmt_gv_srand()
end

function srand(seed::Union{Integer, Vector{UInt32}})
    srand(GLOBAL_RNG, seed)
    dsfmt_gv_srand()
end

function srand(filename::AbstractString, n::Integer=4)
    srand(GLOBAL_RNG, filename, n)
    dsfmt_gv_srand()
end

## Global RNG

const GLOBAL_RNG = MersenneTwister(0)
globalRNG() = GLOBAL_RNG

# rand: a non-specified RNG defaults to GLOBAL_RNG

"""
    rand([rng=GLOBAL_RNG], [S], [dims...])

Pick a random element or array of random elements from the set of values specified by `S`; `S` can be

* an indexable collection (for example `1:n` or `['x','y','z']`), or
* a type: the set of values to pick from is then equivalent to `typemin(S):typemax(S)` for
  integers (this is not applicable to [`BigInt`](@ref)), and to ``[0, 1)`` for floating
  point numbers;

`S` defaults to [`Float64`](@ref).
"""
@inline rand() = rand(GLOBAL_RNG, CloseOpen)
@inline rand(T::Type) = rand(GLOBAL_RNG, T)
rand(dims::Dims) = rand(GLOBAL_RNG, dims)
rand(dims::Integer...) = rand(convert(Tuple{Vararg{Int}}, dims))
rand(T::Type, dims::Dims) = rand(GLOBAL_RNG, T, dims)
rand(T::Type, d1::Integer, dims::Integer...) = rand(T, tuple(Int(d1), convert(Tuple{Vararg{Int}}, dims)...))
rand!(A::AbstractArray) = rand!(GLOBAL_RNG, A)

rand(r::AbstractArray) = rand(GLOBAL_RNG, r)

"""
    rand!([rng=GLOBAL_RNG], A, [coll])

Populate the array `A` with random values. If the indexable collection `coll` is specified,
the values are picked randomly from `coll`. This is equivalent to `copy!(A, rand(rng, coll, size(A)))`
or `copy!(A, rand(rng, eltype(A), size(A)))` but without allocating a new array.

# Example

```jldoctest
julia> rng = MersenneTwister(1234);

julia> rand!(rng, zeros(5))
5-element Array{Float64,1}:
 0.590845
 0.766797
 0.566237
 0.460085
 0.794026
```
"""
rand!(A::AbstractArray, r::AbstractArray) = rand!(GLOBAL_RNG, A, r)

rand(r::AbstractArray, dims::Dims) = rand(GLOBAL_RNG, r, dims)
rand(r::AbstractArray, dims::Integer...) = rand(GLOBAL_RNG, r, convert(Tuple{Vararg{Int}}, dims))

## random floating point values

@inline rand(r::AbstractRNG) = rand(r, CloseOpen)

# MersenneTwister & RandomDevice
@inline rand(r::Union{RandomDevice,MersenneTwister}, ::Type{Float64}) = rand(r, CloseOpen)

rand_ui10_raw(r::MersenneTwister) = rand_ui52_raw(r)
rand_ui23_raw(r::MersenneTwister) = rand_ui52_raw(r)
rand_ui10_raw(r::AbstractRNG)    = rand(r, UInt16)
rand_ui23_raw(r::AbstractRNG)    = rand(r, UInt32)

rand(r::Union{RandomDevice,MersenneTwister}, ::Type{Float16}) =
    Float16(reinterpret(Float32, (rand_ui10_raw(r) % UInt32 << 13) & 0x007fe000 | 0x3f800000) - 1)

rand(r::Union{RandomDevice,MersenneTwister}, ::Type{Float32}) =
    reinterpret(Float32, rand_ui23_raw(r) % UInt32 & 0x007fffff | 0x3f800000) - 1


## random integers

@inline rand_ui52_raw(r::AbstractRNG) = reinterpret(UInt64, rand(r, Close1Open2))
@inline rand_ui52(r::AbstractRNG) = rand_ui52_raw(r) & 0x000fffffffffffff

# MersenneTwister

@inline rand{T<:Union{Bool, Int8, UInt8, Int16, UInt16, Int32, UInt32}}(r::MersenneTwister, ::Type{T}) = rand_ui52_raw(r) % T

function rand(r::MersenneTwister, ::Type{UInt64})
    reserve(r, 2)
    rand_ui52_raw_inbounds(r) << 32 ⊻ rand_ui52_raw_inbounds(r)
end

function rand(r::MersenneTwister, ::Type{UInt128})
    reserve(r, 3)
    xor(rand_ui52_raw_inbounds(r) % UInt128 << 96,
        rand_ui52_raw_inbounds(r) % UInt128 << 48,
        rand_ui52_raw_inbounds(r))
end

rand(r::MersenneTwister, ::Type{Int64})   = reinterpret(Int64,  rand(r, UInt64))
rand(r::MersenneTwister, ::Type{Int128})  = reinterpret(Int128, rand(r, UInt128))

## random Complex values

rand{T<:Real}(r::AbstractRNG, ::Type{Complex{T}}) = complex(rand(r, T), rand(r, T))

# random Char values
# returns a random valid Unicode scalar value (i.e. 0 - 0xd7ff, 0xe000 - # 0x10ffff)
function rand(r::AbstractRNG, ::Type{Char})
    c = rand(r, 0x00000000:0x0010f7ff)
    (c < 0xd800) ? Char(c) : Char(c+0x800)
end

# random values from Dict or Set (for efficiency)
function rand(r::AbstractRNG, t::Dict)
    isempty(t) && throw(ArgumentError("dict must be non-empty"))
    n = length(t.slots)
    while true
        i = rand(r, 1:n)
        Base.isslotfilled(t, i) && return (t.keys[i] => t.vals[i])
    end
end
rand(t::Dict) = rand(GLOBAL_RNG, t)
rand(r::AbstractRNG, s::Set) = rand(r, s.dict).first
rand(s::Set) = rand(GLOBAL_RNG, s)

## Arrays of random numbers

rand(r::AbstractRNG, dims::Dims) = rand(r, Float64, dims)
rand(r::AbstractRNG, dims::Integer...) = rand(r, convert(Tuple{Vararg{Int}}, dims))

rand(r::AbstractRNG, T::Type, dims::Dims) = rand!(r, Array{T}(dims))
rand(r::AbstractRNG, T::Type, d1::Integer, dims::Integer...) = rand(r, T, tuple(Int(d1), convert(Tuple{Vararg{Int}}, dims)...))
# note: the above method would trigger an ambiguity warning if d1 was not separated out:
# rand(r, ()) would match both this method and rand(r, dims::Dims)
# moreover, a call like rand(r, NotImplementedType()) would be an infinite loop

function rand!{T}(r::AbstractRNG, A::AbstractArray{T})
    for i in eachindex(A)
        @inbounds A[i] = rand(r, T)
    end
    A
end

# MersenneTwister

function rand_AbstractArray_Float64!{I<:FloatInterval}(r::MersenneTwister, A::AbstractArray{Float64}, n=length(A), ::Type{I}=CloseOpen)
    # what follows is equivalent to this simple loop but more efficient:
    # for i=1:n
    #     @inbounds A[i] = rand(r, I)
    # end
    m = 0
    while m < n
        s = mt_avail(r)
        if s == 0
            gen_rand(r)
            s = mt_avail(r)
        end
        m2 = min(n, m+s)
        for i=m+1:m2
            @inbounds A[i] = rand_inbounds(r, I)
        end
        m = m2
    end
    A
end

rand!(r::MersenneTwister, A::AbstractArray{Float64}) = rand_AbstractArray_Float64!(r, A)

fill_array!(s::DSFMT_state, A::Ptr{Float64}, n::Int, ::Type{CloseOpen}) = dsfmt_fill_array_close_open!(s, A, n)
fill_array!(s::DSFMT_state, A::Ptr{Float64}, n::Int, ::Type{Close1Open2}) = dsfmt_fill_array_close1_open2!(s, A, n)

function rand!{I<:FloatInterval}(r::MersenneTwister, A::Array{Float64}, n::Int=length(A), ::Type{I}=CloseOpen)
    # depending on the alignment of A, the data written by fill_array! may have
    # to be left-shifted by up to 15 bytes (cf. unsafe_copy! below) for
    # reproducibility purposes;
    # so, even for well aligned arrays, fill_array! is used to generate only
    # the n-2 first values (or n-3 if n is odd), and the remaining values are
    # generated by the scalar version of rand
    if n > length(A)
        throw(BoundsError(A,n))
    end
    n2 = (n-2) ÷ 2 * 2
    if n2 < dsfmt_get_min_array_size()
        rand_AbstractArray_Float64!(r, A, n, I)
    else
        pA = pointer(A)
        align = Csize_t(pA) % 16
        if align > 0
            pA2 = pA + 16 - align
            fill_array!(r.state, pA2, n2, I) # generate the data in-place, but shifted
            unsafe_copy!(pA, pA2, n2) # move the data to the beginning of the array
        else
            fill_array!(r.state, pA, n2, I)
        end
        for i=n2+1:n
            @inbounds A[i] = rand(r, I)
        end
    end
    A
end

@inline mask128(u::UInt128, ::Type{Float16}) = (u & 0x03ff03ff03ff03ff03ff03ff03ff03ff) | 0x3c003c003c003c003c003c003c003c00
@inline mask128(u::UInt128, ::Type{Float32}) = (u & 0x007fffff007fffff007fffff007fffff) | 0x3f8000003f8000003f8000003f800000

function rand!{T<:Union{Float16, Float32}}(r::MersenneTwister, A::Array{T}, ::Type{Close1Open2})
    n = length(A)
    n128 = n * sizeof(T) ÷ 16
    rand!(r, unsafe_wrap(Array, convert(Ptr{Float64}, pointer(A)), 2*n128), 2*n128, Close1Open2)
    A128 = unsafe_wrap(Array, convert(Ptr{UInt128}, pointer(A)), n128)
    @inbounds for i in 1:n128
        u = A128[i]
        u ⊻= u << 26
        # at this point, the 64 low bits of u, "k" being the k-th bit of A128[i] and "+" the bit xor, are:
        # [..., 58+32,..., 53+27, 52+26, ..., 33+7, 32+6, ..., 27+1, 26, ..., 1]
        # the bits needing to be random are
        # [1:10, 17:26, 33:42, 49:58] (for Float16)
        # [1:23, 33:55] (for Float32)
        # this is obviously satisfied on the 32 low bits side, and on the high side, the entropy comes
        # from bits 33:52 of A128[i] and then from bits 27:32 (which are discarded on the low side)
        # this is similar for the 64 high bits of u
        A128[i] = mask128(u, T)
    end
    for i in 16*n128÷sizeof(T)+1:n
        @inbounds A[i] = rand(r, T) + oneunit(T)
    end
    A
end

function rand!{T<:Union{Float16, Float32}}(r::MersenneTwister, A::Array{T}, ::Type{CloseOpen})
    rand!(r, A, Close1Open2)
    I32 = one(Float32)
    for i in eachindex(A)
        @inbounds A[i] = T(Float32(A[i])-I32) # faster than "A[i] -= one(T)" for T==Float16
    end
    A
end

rand!(r::MersenneTwister, A::Array{<:Union{Float16, Float32}}) = rand!(r, A, CloseOpen)


function rand!(r::MersenneTwister, A::Array{UInt128}, n::Int=length(A))
    if n > length(A)
        throw(BoundsError(A,n))
    end
    Af = unsafe_wrap(Array, convert(Ptr{Float64}, pointer(A)), 2n)
    i = n
    while true
        rand!(r, Af, 2i, Close1Open2)
        n < 5 && break
        i = 0
        @inbounds while n-i >= 5
            u = A[i+=1]
            A[n]    ⊻= u << 48
            A[n-=1] ⊻= u << 36
            A[n-=1] ⊻= u << 24
            A[n-=1] ⊻= u << 12
            n-=1
        end
    end
    if n > 0
        u = rand_ui2x52_raw(r)
        for i = 1:n
            @inbounds A[i] ⊻= u << 12*i
        end
    end
    A
end

function rand!{T<:Union{Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128}}(r::MersenneTwister, A::Array{T})
    n=length(A)
    n128 = n * sizeof(T) ÷ 16
    rand!(r, unsafe_wrap(Array, convert(Ptr{UInt128}, pointer(A)), n128))
    for i = 16*n128÷sizeof(T)+1:n
        @inbounds A[i] = rand(r, T)
    end
    A
end

## Generate random integer within a range

# remainder function according to Knuth, where rem_knuth(a, 0) = a
rem_knuth(a::UInt, b::UInt) = a % (b + (b == 0)) + a * (b == 0)
rem_knuth(a::T, b::T) where {T<:Unsigned} = b != 0 ? a % b : a

# maximum multiple of k <= 2^bits(T) decremented by one,
# that is 0xFFFF...FFFF if k = typemax(T) - typemin(T) with intentional underflow
# see http://stackoverflow.com/questions/29182036/integer-arithmetic-add-1-to-uint-max-and-divide-by-n-without-overflow
maxmultiple(k::T) where {T<:Unsigned} = (div(typemax(T) - k + oneunit(k), k + (k == 0))*k + k - oneunit(k))::T

# maximum multiple of k within 1:2^32 or 1:2^64 decremented by one, depending on size
maxmultiplemix(k::UInt64) = if k >> 32 != 0; maxmultiple(k); else (div(0x0000000100000000, k + (k == 0))*k - oneunit(k))::UInt64; end

abstract type RangeGenerator end

struct RangeGeneratorInt{T<:Integer,U<:Unsigned} <: RangeGenerator
    a::T   # first element of the range
    k::U   # range length or zero for full range
    u::U   # rejection threshold
end
# generators with 32, 128 bits entropy
RangeGeneratorInt(a::T, k::U) where {T,U<:Union{UInt32,UInt128}} = RangeGeneratorInt{T,U}(a, k, maxmultiple(k))
# mixed 32/64 bits entropy generator
RangeGeneratorInt(a::T, k::UInt64) where {T} = RangeGeneratorInt{T,UInt64}(a, k, maxmultiplemix(k))
# generator for ranges
function RangeGenerator(r::UnitRange{T}) where T<:Unsigned
    if isempty(r)
        throw(ArgumentError("range must be non-empty"))
    end
    RangeGeneratorInt(first(r), last(r) - first(r) + oneunit(T))
end

# specialized versions
for (T, U) in [(UInt8, UInt32), (UInt16, UInt32),
               (Int8, UInt32), (Int16, UInt32), (Int32, UInt32), (Int64, UInt64), (Int128, UInt128),
               (Bool, UInt32)]

    @eval RangeGenerator(r::UnitRange{$T}) = begin
        if isempty(r)
            throw(ArgumentError("range must be non-empty"))
        end
        RangeGeneratorInt(first(r), convert($U, unsigned(last(r) - first(r)) + one($U))) # overflow ok
    end
end

if GMP_VERSION.major >= 6
    struct RangeGeneratorBigInt <: RangeGenerator
        a::BigInt             # first
        m::BigInt             # range length - 1
        nlimbs::Int           # number of limbs in generated BigInt's
        mask::Limb            # applied to the highest limb
    end

else
    struct RangeGeneratorBigInt <: RangeGenerator
        a::BigInt             # first
        m::BigInt             # range length - 1
        limbs::Vector{Limb}   # buffer to be copied into generated BigInt's
        mask::Limb            # applied to the highest limb

        RangeGeneratorBigInt(a, m, nlimbs, mask) = new(a, m, Vector{Limb}(nlimbs), mask)
    end
end


function RangeGenerator(r::UnitRange{BigInt})
    m = last(r) - first(r)
    m < 0 && throw(ArgumentError("range must be non-empty"))
    nd = ndigits(m, 2)
    nlimbs, highbits = divrem(nd, 8*sizeof(Limb))
    highbits > 0 && (nlimbs += 1)
    mask = highbits == 0 ? ~zero(Limb) : one(Limb)<<highbits - one(Limb)
    return RangeGeneratorBigInt(first(r), m, nlimbs, mask)
end


# this function uses 32 bit entropy for small ranges of length <= typemax(UInt32) + 1
# RangeGeneratorInt is responsible for providing the right value of k
function rand{T<:Union{UInt64, Int64}}(rng::AbstractRNG, g::RangeGeneratorInt{T,UInt64})
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

function rand{T<:Integer, U<:Unsigned}(rng::AbstractRNG, g::RangeGeneratorInt{T,U})
    x = rand(rng, U)
    while x > g.u
        x = rand(rng, U)
    end
    (unsigned(g.a) + rem_knuth(x, g.k)) % T
end

if GMP_VERSION.major >= 6
    # mpz_limbs_write and mpz_limbs_finish are available only in GMP version 6
    function rand(rng::AbstractRNG, g::RangeGeneratorBigInt)
        x = BigInt()
        while true
            # note: on CRAY computers, the second argument may be of type Cint (48 bits) and not Clong
            xd = ccall((:__gmpz_limbs_write, :libgmp), Ptr{Limb}, (Ptr{BigInt}, Clong), &x, g.nlimbs)
            limbs = unsafe_wrap(Array, xd, g.nlimbs)
            rand!(rng, limbs)
            limbs[end] &= g.mask
            ccall((:__gmpz_limbs_finish, :libgmp), Void, (Ptr{BigInt}, Clong), &x, g.nlimbs)
            x <= g.m && break
        end
        ccall((:__gmpz_add, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &x, &x, &g.a)
        return x
    end
else
    function rand(rng::AbstractRNG, g::RangeGeneratorBigInt)
        x = BigInt()
        while true
            rand!(rng, g.limbs)
            g.limbs[end] &= g.mask
            ccall((:__gmpz_import, :libgmp), Void,
                  (Ptr{BigInt}, Csize_t, Cint, Csize_t, Cint, Csize_t, Ptr{Limb}),
                  &x, length(g.limbs), -1, sizeof(Limb), 0, 0, g.limbs)
            x <= g.m && break
        end
        ccall((:__gmpz_add, :libgmp), Void, (Ptr{BigInt}, Ptr{BigInt}, Ptr{BigInt}), &x, &x, &g.a)
        return x
    end
end

rand(rng::AbstractRNG, r::UnitRange{<:Union{Signed,Unsigned,BigInt,Bool}}) = rand(rng, RangeGenerator(r))


# Randomly draw a sample from an AbstractArray r
# (e.g. r is a range 0:2:8 or a vector [2, 3, 5, 7])
rand(rng::AbstractRNG, r::AbstractArray) = @inbounds return r[rand(rng, 1:length(r))]

function rand!(rng::AbstractRNG, A::AbstractArray, g::RangeGenerator)
    for i in eachindex(A)
        @inbounds A[i] = rand(rng, g)
    end
    return A
end

rand!(rng::AbstractRNG, A::AbstractArray, r::UnitRange{<:Union{Signed,Unsigned,BigInt,Bool,Char}}) = rand!(rng, A, RangeGenerator(r))

function rand!(rng::AbstractRNG, A::AbstractArray, r::AbstractArray)
    g = RangeGenerator(1:(length(r)))
    for i in eachindex(A)
        @inbounds A[i] = r[rand(rng, g)]
    end
    return A
end

rand{T}(rng::AbstractRNG, r::AbstractArray{T}, dims::Dims) = rand!(rng, Array{T}(dims), r)
rand(rng::AbstractRNG, r::AbstractArray, dims::Int...) = rand(rng, r, dims)

## random BitArrays (AbstractRNG)

function rand!(rng::AbstractRNG, B::BitArray)
    isempty(B) && return B
    Bc = B.chunks
    rand!(rng, Bc)
    Bc[end] &= Base._msk_end(B)
    return B
end

"""
    bitrand([rng=GLOBAL_RNG], [dims...])

Generate a `BitArray` of random boolean values.

# Example

```jldoctest
julia> rng = MersenneTwister(1234);

julia> bitrand(rng, 10)
10-element BitArray{1}:
  true
  true
  true
 false
  true
 false
 false
  true
 false
  true
```
"""
bitrand(r::AbstractRNG, dims::Dims)   = rand!(r, BitArray(dims))
bitrand(r::AbstractRNG, dims::Int...) = rand!(r, BitArray(dims))

bitrand(dims::Dims)   = rand!(BitArray(dims))
bitrand(dims::Int...) = rand!(BitArray(dims))

## randn() - Normally distributed random numbers using Ziggurat algorithm

# The Ziggurat Method for generating random variables - Marsaglia and Tsang
# Paper and reference code: http://www.jstatsoft.org/v05/i08/

# randmtzig (covers also exponential variates)
## Tables for normal variates
const ki =
    UInt64[0x0007799ec012f7b2,0x0000000000000000,0x0006045f4c7de363,0x0006d1aa7d5ec0a5,
           0x000728fb3f60f777,0x0007592af4e9fbc0,0x000777a5c0bf655d,0x00078ca3857d2256,
           0x00079bf6b0ffe58b,0x0007a7a34ab092ad,0x0007b0d2f20dd1cb,0x0007b83d3aa9cb52,
           0x0007be597614224d,0x0007c3788631abe9,0x0007c7d32bc192ee,0x0007cb9263a6e86d,
           0x0007ced483edfa84,0x0007d1b07ac0fd39,0x0007d437ef2da5fc,0x0007d678b069aa6e,
           0x0007d87db38c5c87,0x0007da4fc6a9ba62,0x0007dbf611b37f3b,0x0007dd7674d0f286,
           0x0007ded5ce8205f6,0x0007e018307fb62b,0x0007e141081bd124,0x0007e2533d712de8,
           0x0007e3514bbd7718,0x0007e43d54944b52,0x0007e5192f25ef42,0x0007e5e67481118d,
           0x0007e6a6897c1ce2,0x0007e75aa6c7f64c,0x0007e803df8ee498,0x0007e8a326eb6272,
           0x0007e93954717a28,0x0007e9c727f8648f,0x0007ea4d4cc85a3c,0x0007eacc5c4907a9,
           0x0007eb44e0474cf6,0x0007ebb754e47419,0x0007ec242a3d8474,0x0007ec8bc5d69645,
           0x0007ecee83d3d6e9,0x0007ed4cb8082f45,0x0007eda6aee0170f,0x0007edfcae2dfe68,
           0x0007ee4ef5dccd3e,0x0007ee9dc08c394e,0x0007eee9441a17c7,0x0007ef31b21b4fb1,
           0x0007ef773846a8a7,0x0007efba00d35a17,0x0007effa32ccf69f,0x0007f037f25e1278,
           0x0007f0736112d12c,0x0007f0ac9e145c25,0x0007f0e3c65e1fcc,0x0007f118f4ed8e54,
           0x0007f14c42ed0dc8,0x0007f17dc7daa0c3,0x0007f1ad99aac6a5,0x0007f1dbcce80015,
           0x0007f20874cf56bf,0x0007f233a36a3b9a,0x0007f25d69a604ad,0x0007f285d7694a92,
           0x0007f2acfba75e3b,0x0007f2d2e4720909,0x0007f2f79f09c344,0x0007f31b37ec883b,
           0x0007f33dbae36abc,0x0007f35f330f08d5,0x0007f37faaf2fa79,0x0007f39f2c805380,
           0x0007f3bdc11f4f1c,0x0007f3db71b83850,0x0007f3f846bba121,0x0007f4144829f846,
           0x0007f42f7d9a8b9d,0x0007f449ee420432,0x0007f463a0f8675e,0x0007f47c9c3ea77b,
           0x0007f494e643cd8e,0x0007f4ac84e9c475,0x0007f4c37dc9cd50,0x0007f4d9d638a432,
           0x0007f4ef934a5b6a,0x0007f504b9d5f33d,0x0007f5194e78b352,0x0007f52d55994a96,
           0x0007f540d36aba0c,0x0007f553cbef0e77,0x0007f56642f9ec8f,0x0007f5783c32f31e,
           0x0007f589bb17f609,0x0007f59ac2ff1525,0x0007f5ab5718b15a,0x0007f5bb7a71427c,
           0x0007f5cb2ff31009,0x0007f5da7a67cebe,0x0007f5e95c7a24e7,0x0007f5f7d8b7171e,
           0x0007f605f18f5ef4,0x0007f613a958ad0a,0x0007f621024ed7e9,0x0007f62dfe94f8cb,
           0x0007f63aa036777a,0x0007f646e928065a,0x0007f652db488f88,0x0007f65e786213ff,
           0x0007f669c22a7d8a,0x0007f674ba446459,0x0007f67f623fc8db,0x0007f689bb9ac294,
           0x0007f693c7c22481,0x0007f69d881217a6,0x0007f6a6fdd6ac36,0x0007f6b02a4c61ee,
           0x0007f6b90ea0a7f4,0x0007f6c1abf254c0,0x0007f6ca03521664,0x0007f6d215c2db82,
           0x0007f6d9e43a3559,0x0007f6e16fa0b329,0x0007f6e8b8d23729,0x0007f6efc09e4569,
           0x0007f6f687c84cbf,0x0007f6fd0f07ea09,0x0007f703570925e2,0x0007f709606cad03,
           0x0007f70f2bc8036f,0x0007f714b9a5b292,0x0007f71a0a85725d,0x0007f71f1edc4d9e,
           0x0007f723f714c179,0x0007f728938ed843,0x0007f72cf4a03fa0,0x0007f7311a945a16,
           0x0007f73505ac4bf8,0x0007f738b61f03bd,0x0007f73c2c193dc0,0x0007f73f67bd835c,
           0x0007f74269242559,0x0007f745305b31a1,0x0007f747bd666428,0x0007f74a103f12ed,
           0x0007f74c28d414f5,0x0007f74e0709a42d,0x0007f74faab939f9,0x0007f75113b16657,
           0x0007f75241b5a155,0x0007f753347e16b8,0x0007f753ebb76b7c,0x0007f75467027d05,
           0x0007f754a5f4199d,0x0007f754a814b207,0x0007f7546ce003ae,0x0007f753f3c4bb29,
           0x0007f7533c240e92,0x0007f75245514f41,0x0007f7510e91726c,0x0007f74f971a9012,
           0x0007f74dde135797,0x0007f74be2927971,0x0007f749a39e051c,0x0007f747202aba8a,
           0x0007f744571b4e3c,0x0007f741473f9efe,0x0007f73def53dc43,0x0007f73a4dff9bff,
           0x0007f73661d4deaf,0x0007f732294f003f,0x0007f72da2d19444,0x0007f728cca72bda,
           0x0007f723a5000367,0x0007f71e29f09627,0x0007f7185970156b,0x0007f7123156c102,
           0x0007f70baf5c1e2c,0x0007f704d1150a23,0x0007f6fd93f1a4e5,0x0007f6f5f53b10b6,
           0x0007f6edf211023e,0x0007f6e587671ce9,0x0007f6dcb2021679,0x0007f6d36e749c64,
           0x0007f6c9b91bf4c6,0x0007f6bf8e1c541b,0x0007f6b4e95ce015,0x0007f6a9c68356ff,
           0x0007f69e20ef5211,0x0007f691f3b517eb,0x0007f6853997f321,0x0007f677ed03ff19,
           0x0007f66a08075bdc,0x0007f65b844ab75a,0x0007f64c5b091860,0x0007f63c8506d4bc,
           0x0007f62bfa8798fe,0x0007f61ab34364b0,0x0007f608a65a599a,0x0007f5f5ca4737e8,
           0x0007f5e214d05b48,0x0007f5cd7af7066e,0x0007f5b7f0e4c2a1,0x0007f5a169d68fcf,
           0x0007f589d80596a5,0x0007f5712c8d0174,0x0007f557574c912b,0x0007f53c46c77193,
           0x0007f51fe7feb9f2,0x0007f5022646ecfb,0x0007f4e2eb17ab1d,0x0007f4c21dd4a3d1,
           0x0007f49fa38ea394,0x0007f47b5ebb62eb,0x0007f4552ee27473,0x0007f42cf03d58f5,
           0x0007f4027b48549f,0x0007f3d5a44119df,0x0007f3a63a8fb552,0x0007f37408155100,
           0x0007f33ed05b55ec,0x0007f3064f9c183e,0x0007f2ca399c7ba1,0x0007f28a384bb940,
           0x0007f245ea1b7a2b,0x0007f1fcdffe8f1b,0x0007f1ae9af758cd,0x0007f15a8917f27e,
           0x0007f10001ccaaab,0x0007f09e413c418a,0x0007f034627733d7,0x0007efc15815b8d5,
           0x0007ef43e2bf7f55,0x0007eeba84e31dfe,0x0007ee237294df89,0x0007ed7c7c170141,
           0x0007ecc2f0d95d3a,0x0007ebf377a46782,0x0007eb09d6deb285,0x0007ea00a4f17808,
           0x0007e8d0d3da63d6,0x0007e771023b0fcf,0x0007e5d46c2f08d8,0x0007e3e937669691,
           0x0007e195978f1176,0x0007deb2c0e05c1c,0x0007db0362002a19,0x0007d6202c151439,
           0x0007cf4b8f00a2cb,0x0007c4fd24520efd,0x0007b362fbf81816,0x00078d2d25998e24]
const wi =
    [1.7367254121602630e-15,9.5586603514556339e-17,1.2708704834810623e-16,
     1.4909740962495474e-16,1.6658733631586268e-16,1.8136120810119029e-16,
     1.9429720153135588e-16,2.0589500628482093e-16,2.1646860576895422e-16,
     2.2622940392218116e-16,2.3532718914045892e-16,2.4387234557428771e-16,
     2.5194879829274225e-16,2.5962199772528103e-16,2.6694407473648285e-16,
     2.7395729685142446e-16,2.8069646002484804e-16,2.8719058904113930e-16,
     2.9346417484728883e-16,2.9953809336782113e-16,3.0543030007192440e-16,
     3.1115636338921572e-16,3.1672988018581815e-16,3.2216280350549905e-16,
     3.2746570407939751e-16,3.3264798116841710e-16,3.3771803417353232e-16,
     3.4268340353119356e-16,3.4755088731729758e-16,3.5232663846002031e-16,
     3.5701624633953494e-16,3.6162480571598339e-16,3.6615697529653540e-16,
     3.7061702777236077e-16,3.7500889278747798e-16,3.7933619401549554e-16,
     3.8360228129677279e-16,3.8781025861250247e-16,3.9196300853257678e-16,
     3.9606321366256378e-16,4.0011337552546690e-16,4.0411583124143332e-16,
     4.0807276830960448e-16,4.1198623774807442e-16,4.1585816580828064e-16,
     4.1969036444740733e-16,4.2348454071520708e-16,4.2724230518899761e-16,
     4.3096517957162941e-16,4.3465460355128760e-16,4.3831194100854571e-16,
     4.4193848564470665e-16,4.4553546609579137e-16,4.4910405058828750e-16,
     4.5264535118571397e-16,4.5616042766900381e-16,4.5965029108849407e-16,
     4.6311590702081647e-16,4.6655819856008752e-16,4.6997804906941950e-16,
     4.7337630471583237e-16,4.7675377680908526e-16,4.8011124396270155e-16,
     4.8344945409350080e-16,4.8676912627422087e-16,4.9007095245229938e-16,
     4.9335559904654139e-16,4.9662370843221783e-16,4.9987590032409088e-16,
     5.0311277306593187e-16,5.0633490483427195e-16,5.0954285476338923e-16,
     5.1273716399787966e-16,5.1591835667857364e-16,5.1908694086703434e-16,
     5.2224340941340417e-16,5.2538824077194543e-16,5.2852189976823820e-16,
     5.3164483832166176e-16,5.3475749612647295e-16,5.3786030129452348e-16,
     5.4095367096239933e-16,5.4403801186554671e-16,5.4711372088173611e-16,
     5.5018118554603362e-16,5.5324078453927836e-16,5.5629288815190902e-16,
     5.5933785872484621e-16,5.6237605106900435e-16,5.6540781286489604e-16,
     5.6843348504368141e-16,5.7145340215092040e-16,5.7446789269419609e-16,
     5.7747727947569648e-16,5.8048187991076857e-16,5.8348200633338921e-16,
     5.8647796628943653e-16,5.8947006281858718e-16,5.9245859472561339e-16,
     5.9544385684180598e-16,5.9842614027720281e-16,6.0140573266426640e-16,
     6.0438291839361250e-16,6.0735797884236057e-16,6.1033119259564394e-16,
     6.1330283566179110e-16,6.1627318168165963e-16,6.1924250213258470e-16,
     6.2221106652737879e-16,6.2517914260879998e-16,6.2814699653988953e-16,
     6.3111489309056042e-16,6.3408309582080600e-16,6.3705186726088149e-16,
     6.4002146908880247e-16,6.4299216230548961e-16,6.4596420740788321e-16,
     6.4893786456033965e-16,6.5191339376461587e-16,6.5489105502874154e-16,
     6.5787110853507413e-16,6.6085381480782587e-16,6.6383943488035057e-16,
     6.6682823046247459e-16,6.6982046410815579e-16,6.7281639938375311e-16,
     6.7581630103719006e-16,6.7882043516829803e-16,6.8182906940062540e-16,
     6.8484247305500383e-16,6.8786091732516637e-16,6.9088467545571690e-16,
     6.9391402292275690e-16,6.9694923761748294e-16,6.9999060003307640e-16,
     7.0303839345521508e-16,7.0609290415654822e-16,7.0915442159548734e-16,
     7.1222323861967788e-16,7.1529965167453030e-16,7.1838396101720629e-16,
     7.2147647093647067e-16,7.2457748997883870e-16,7.2768733118146927e-16,
     7.3080631231227429e-16,7.3393475611774048e-16,7.3707299057898310e-16,
     7.4022134917657997e-16,7.4338017116476479e-16,7.4654980185558890e-16,
     7.4973059291369793e-16,7.5292290266240584e-16,7.5612709640179217e-16,
     7.5934354673958895e-16,7.6257263393567558e-16,7.6581474626104873e-16,
     7.6907028037219191e-16,7.7233964170182985e-16,7.7562324486711744e-16,
     7.7892151409638524e-16,7.8223488367564108e-16,7.8556379841610841e-16,
     7.8890871414417552e-16,7.9227009821522709e-16,7.9564843005293662e-16,
     7.9904420171571300e-16,8.0245791849212591e-16,8.0589009952726568e-16,
     8.0934127848215009e-16,8.1281200422845008e-16,8.1630284158098775e-16,
     8.1981437207065329e-16,8.2334719476060504e-16,8.2690192710884700e-16,
     8.3047920588053737e-16,8.3407968811366288e-16,8.3770405214202216e-16,
     8.4135299867980282e-16,8.4502725197240968e-16,8.4872756101861549e-16,
     8.5245470086955962e-16,8.5620947401062333e-16,8.5999271183276646e-16,
     8.6380527620052589e-16,8.6764806112455816e-16,8.7152199454736980e-16,
     8.7542804025171749e-16,8.7936719990210427e-16,8.8334051523084080e-16,
     8.8734907038131345e-16,8.9139399442240861e-16,8.9547646404950677e-16,
     8.9959770648910994e-16,9.0375900262601175e-16,9.0796169037400680e-16,
     9.1220716831348461e-16,9.1649689962191353e-16,9.2083241632623076e-16,
     9.2521532390956933e-16,9.2964730630864167e-16,9.3413013134252651e-16,
     9.3866565661866598e-16,9.4325583596767065e-16,9.4790272646517382e-16,
     9.5260849610662787e-16,9.5737543220974496e-16,9.6220595062948384e-16,
     9.6710260588230542e-16,9.7206810229016259e-16,9.7710530627072088e-16,
     9.8221725991905411e-16,9.8740719604806711e-16,9.9267855488079765e-16,
     9.9803500261836449e-16,1.0034804521436181e-15,1.0090190861637457e-15,
     1.0146553831467086e-15,1.0203941464683124e-15,1.0262405372613567e-15,
     1.0322001115486456e-15,1.0382788623515399e-15,1.0444832676000471e-15,
     1.0508203448355195e-15,1.0572977139009890e-15,1.0639236690676801e-15,
     1.0707072623632994e-15,1.0776584002668106e-15,1.0847879564403425e-15,
     1.0921079038149563e-15,1.0996314701785628e-15,1.1073733224935752e-15,
     1.1153497865853155e-15,1.1235791107110833e-15,1.1320817840164846e-15,
     1.1408809242582780e-15,1.1500027537839792e-15,1.1594771891449189e-15,
     1.1693385786910960e-15,1.1796266352955801e-15,1.1903876299282890e-15,
     1.2016759392543819e-15,1.2135560818666897e-15,1.2261054417450561e-15,
     1.2394179789163251e-15,1.2536093926602567e-15,1.2688244814255010e-15,
     1.2852479319096109e-15,1.3031206634689985e-15,1.3227655770195326e-15,
     1.3446300925011171e-15,1.3693606835128518e-15,1.3979436672775240e-15,
     1.4319989869661328e-15,1.4744848603597596e-15,1.5317872741611144e-15,
     1.6227698675312968e-15]
const fi =
    [1.0000000000000000e+00,9.7710170126767082e-01,9.5987909180010600e-01,
     9.4519895344229909e-01,9.3206007595922991e-01,9.1999150503934646e-01,
     9.0872644005213032e-01,8.9809592189834297e-01,8.8798466075583282e-01,
     8.7830965580891684e-01,8.6900868803685649e-01,8.6003362119633109e-01,
     8.5134625845867751e-01,8.4291565311220373e-01,8.3471629298688299e-01,
     8.2672683394622093e-01,8.1892919160370192e-01,8.1130787431265572e-01,
     8.0384948317096383e-01,7.9654233042295841e-01,7.8937614356602404e-01,
     7.8234183265480195e-01,7.7543130498118662e-01,7.6863731579848571e-01,
     7.6195334683679483e-01,7.5537350650709567e-01,7.4889244721915638e-01,
     7.4250529634015061e-01,7.3620759812686210e-01,7.2999526456147568e-01,
     7.2386453346862967e-01,7.1781193263072152e-01,7.1183424887824798e-01,
     7.0592850133275376e-01,7.0009191813651117e-01,6.9432191612611627e-01,
     6.8861608300467136e-01,6.8297216164499430e-01,6.7738803621877308e-01,
     6.7186171989708166e-01,6.6639134390874977e-01,6.6097514777666277e-01,
     6.5561147057969693e-01,6.5029874311081637e-01,6.4503548082082196e-01,
     6.3982027745305614e-01,6.3465179928762327e-01,6.2952877992483625e-01,
     6.2445001554702606e-01,6.1941436060583399e-01,6.1442072388891344e-01,
     6.0946806492577310e-01,6.0455539069746733e-01,5.9968175261912482e-01,
     5.9484624376798689e-01,5.9004799633282545e-01,5.8528617926337090e-01,
     5.8055999610079034e-01,5.7586868297235316e-01,5.7121150673525267e-01,
     5.6658776325616389e-01,5.6199677581452390e-01,5.5743789361876550e-01,
     5.5291049042583185e-01,5.4841396325526537e-01,5.4394773119002582e-01,
     5.3951123425695158e-01,5.3510393238045717e-01,5.3072530440366150e-01,
     5.2637484717168403e-01,5.2205207467232140e-01,5.1775651722975591e-01,
     5.1348772074732651e-01,5.0924524599574761e-01,5.0502866794346790e-01,
     5.0083757512614835e-01,4.9667156905248933e-01,4.9253026364386815e-01,
     4.8841328470545758e-01,4.8432026942668288e-01,4.8025086590904642e-01,
     4.7620473271950547e-01,4.7218153846772976e-01,4.6818096140569321e-01,
     4.6420268904817391e-01,4.6024641781284248e-01,4.5631185267871610e-01,
     4.5239870686184824e-01,4.4850670150720273e-01,4.4463556539573912e-01,
     4.4078503466580377e-01,4.3695485254798533e-01,4.3314476911265209e-01,
     4.2935454102944126e-01,4.2558393133802180e-01,4.2183270922949573e-01,
     4.1810064983784795e-01,4.1438753404089090e-01,4.1069314827018799e-01,
     4.0701728432947315e-01,4.0335973922111429e-01,3.9972031498019700e-01,
     3.9609881851583223e-01,3.9249506145931540e-01,3.8890886001878855e-01,
     3.8534003484007706e-01,3.8178841087339344e-01,3.7825381724561896e-01,
     3.7473608713789086e-01,3.7123505766823922e-01,3.6775056977903225e-01,
     3.6428246812900372e-01,3.6083060098964775e-01,3.5739482014578022e-01,
     3.5397498080007656e-01,3.5057094148140588e-01,3.4718256395679348e-01,
     3.4380971314685055e-01,3.4045225704452164e-01,3.3711006663700588e-01,
     3.3378301583071823e-01,3.3047098137916342e-01,3.2717384281360129e-01,
     3.2389148237639104e-01,3.2062378495690530e-01,3.1737063802991350e-01,
     3.1413193159633707e-01,3.1090755812628634e-01,3.0769741250429189e-01,
     3.0450139197664983e-01,3.0131939610080288e-01,2.9815132669668531e-01,
     2.9499708779996164e-01,2.9185658561709499e-01,2.8872972848218270e-01,
     2.8561642681550159e-01,2.8251659308370741e-01,2.7943014176163772e-01,
     2.7635698929566810e-01,2.7329705406857691e-01,2.7025025636587519e-01,
     2.6721651834356114e-01,2.6419576399726080e-01,2.6118791913272082e-01,
     2.5819291133761890e-01,2.5521066995466168e-01,2.5224112605594190e-01,
     2.4928421241852824e-01,2.4633986350126363e-01,2.4340801542275012e-01,
     2.4048860594050039e-01,2.3758157443123795e-01,2.3468686187232990e-01,
     2.3180441082433859e-01,2.2893416541468023e-01,2.2607607132238020e-01,
     2.2323007576391746e-01,2.2039612748015194e-01,2.1757417672433113e-01,
     2.1476417525117358e-01,2.1196607630703015e-01,2.0917983462112499e-01,
     2.0640540639788071e-01,2.0364274931033485e-01,2.0089182249465656e-01,
     1.9815258654577511e-01,1.9542500351413428e-01,1.9270903690358912e-01,
     1.9000465167046496e-01,1.8731181422380025e-01,1.8463049242679927e-01,
     1.8196065559952254e-01,1.7930227452284767e-01,1.7665532144373500e-01,
     1.7401977008183875e-01,1.7139559563750595e-01,1.6878277480121151e-01,
     1.6618128576448205e-01,1.6359110823236570e-01,1.6101222343751107e-01,
     1.5844461415592431e-01,1.5588826472447920e-01,1.5334316106026283e-01,
     1.5080929068184568e-01,1.4828664273257453e-01,1.4577520800599403e-01,
     1.4327497897351341e-01,1.4078594981444470e-01,1.3830811644855071e-01,
     1.3584147657125373e-01,1.3338602969166913e-01,1.3094177717364430e-01,
     1.2850872227999952e-01,1.2608687022018586e-01,1.2367622820159654e-01,
     1.2127680548479021e-01,1.1888861344290998e-01,1.1651166562561080e-01,
     1.1414597782783835e-01,1.1179156816383801e-01,1.0944845714681163e-01,
     1.0711666777468364e-01,1.0479622562248690e-01,1.0248715894193508e-01,
     1.0018949876880981e-01,9.7903279038862284e-02,9.5628536713008819e-02,
     9.3365311912690860e-02,9.1113648066373634e-02,8.8873592068275789e-02,
     8.6645194450557961e-02,8.4428509570353374e-02,8.2223595813202863e-02,
     8.0030515814663056e-02,7.7849336702096039e-02,7.5680130358927067e-02,
     7.3522973713981268e-02,7.1377949058890375e-02,6.9245144397006769e-02,
     6.7124653827788497e-02,6.5016577971242842e-02,6.2921024437758113e-02,
     6.0838108349539864e-02,5.8767952920933758e-02,5.6710690106202902e-02,
     5.4666461324888914e-02,5.2635418276792176e-02,5.0617723860947761e-02,
     4.8613553215868521e-02,4.6623094901930368e-02,4.4646552251294443e-02,
     4.2684144916474431e-02,4.0736110655940933e-02,3.8802707404526113e-02,
     3.6884215688567284e-02,3.4980941461716084e-02,3.3093219458578522e-02,
     3.1221417191920245e-02,2.9365939758133314e-02,2.7527235669603082e-02,
     2.5705804008548896e-02,2.3902203305795882e-02,2.2117062707308864e-02,
     2.0351096230044517e-02,1.8605121275724643e-02,1.6880083152543166e-02,
     1.5177088307935325e-02,1.3497450601739880e-02,1.1842757857907888e-02,
     1.0214971439701471e-02,8.6165827693987316e-03,7.0508754713732268e-03,
     5.5224032992509968e-03,4.0379725933630305e-03,2.6090727461021627e-03,
     1.2602859304985975e-03]

## Tables for exponential variates
const ke =
    UInt64[0x000e290a13924be3,0x0000000000000000,0x0009beadebce18bf,0x000c377ac71f9e08,
           0x000d4ddb99075857,0x000de893fb8ca23e,0x000e4a8e87c4328d,0x000e8dff16ae1cb9,
           0x000ebf2deab58c59,0x000ee49a6e8b9638,0x000f0204efd64ee4,0x000f19bdb8ea3c1b,
           0x000f2d458bbe5bd1,0x000f3da104b78236,0x000f4b86d784571f,0x000f577ad8a7784f,
           0x000f61de83da32ab,0x000f6afb7843cce7,0x000f730a57372b44,0x000f7a37651b0e68,
           0x000f80a5bb6eea52,0x000f867189d3cb5b,0x000f8bb1b4f8fbbd,0x000f9079062292b8,
           0x000f94d70ca8d43a,0x000f98d8c7dcaa99,0x000f9c8928abe083,0x000f9ff175b734a6,
           0x000fa319996bc47d,0x000fa6085f8e9d07,0x000fa8c3a62e1991,0x000fab5084e1f660,
           0x000fadb36c84cccb,0x000faff041086846,0x000fb20a6ea22bb9,0x000fb404fb42cb3c,
           0x000fb5e295158173,0x000fb7a59e99727a,0x000fb95038c8789d,0x000fbae44ba684eb,
           0x000fbc638d822e60,0x000fbdcf89209ffa,0x000fbf29a303cfc5,0x000fc0731df1089c,
           0x000fc1ad1ed6c8b1,0x000fc2d8b02b5c89,0x000fc3f6c4d92131,0x000fc5083ac9ba7d,
           0x000fc60ddd1e9cd6,0x000fc7086622e825,0x000fc7f881009f0b,0x000fc8decb41ac70,
           0x000fc9bbd623d7ec,0x000fca9027c5b26d,0x000fcb5c3c319c49,0x000fcc20864b4449,
           0x000fccdd70a35d40,0x000fcd935e34bf80,0x000fce42ab0db8bd,0x000fceebace7ec01,
           0x000fcf8eb3b0d0e7,0x000fd02c0a049b60,0x000fd0c3f59d199c,0x000fd156b7b5e27e,
           0x000fd1e48d670341,0x000fd26daff73551,0x000fd2f2552684be,0x000fd372af7233c1,
           0x000fd3eeee528f62,0x000fd4673e73543a,0x000fd4dbc9e72ff7,0x000fd54cb856dc2c,
           0x000fd5ba2f2c4119,0x000fd62451ba02c2,0x000fd68b415fcff4,0x000fd6ef1dabc160,
           0x000fd75004790eb6,0x000fd7ae120c583f,0x000fd809612dbd09,0x000fd8620b40effa,
           0x000fd8b8285b78fd,0x000fd90bcf594b1d,0x000fd95d15efd425,0x000fd9ac10bfa70c,
           0x000fd9f8d364df06,0x000fda437086566b,0x000fda8bf9e3c9fe,0x000fdad28062fed5,
           0x000fdb17141bff2c,0x000fdb59c4648085,0x000fdb9a9fda83cc,0x000fdbd9b46e3ed4,
           0x000fdc170f6b5d04,0x000fdc52bd81a3fb,0x000fdc8ccacd07ba,0x000fdcc542dd3902,
           0x000fdcfc30bcb793,0x000fdd319ef77143,0x000fdd6597a0f60b,0x000fdd98245a48a2,
           0x000fddc94e575271,0x000fddf91e64014f,0x000fde279ce914ca,0x000fde54d1f0a06a,
           0x000fde80c52a47cf,0x000fdeab7def394e,0x000fded50345eb35,0x000fdefd5be59fa0,
           0x000fdf248e39b26f,0x000fdf4aa064b4af,0x000fdf6f98435894,0x000fdf937b6f30ba,
           0x000fdfb64f414571,0x000fdfd818d48262,0x000fdff8dd07fed8,0x000fe018a08122c4,
           0x000fe03767adaa59,0x000fe05536c58a13,0x000fe07211ccb4c5,0x000fe08dfc94c532,
           0x000fe0a8fabe8ca1,0x000fe0c30fbb87a5,0x000fe0dc3ecf3a5a,0x000fe0f48b107521,
           0x000fe10bf76a82ef,0x000fe122869e41ff,0x000fe1383b4327e1,0x000fe14d17c83187,
           0x000fe1611e74c023,0x000fe1745169635a,0x000fe186b2a09176,0x000fe19843ef4e07,
           0x000fe1a90705bf63,0x000fe1b8fd6fb37c,0x000fe1c828951443,0x000fe1d689ba4bfd,
           0x000fe1e4220099a4,0x000fe1f0f26655a0,0x000fe1fcfbc726d4,0x000fe2083edc2830,
           0x000fe212bc3bfeb4,0x000fe21c745adfe3,0x000fe225678a8895,0x000fe22d95fa23f4,
           0x000fe234ffb62282,0x000fe23ba4a800d9,0x000fe2418495fddc,0x000fe2469f22bffb,
           0x000fe24af3cce90d,0x000fe24e81ee9858,0x000fe25148bcda19,0x000fe253474703fe,
           0x000fe2547c75fdc6,0x000fe254e70b754f,0x000fe25485a0fd1a,0x000fe25356a71450,
           0x000fe2515864173a,0x000fe24e88f316f1,0x000fe24ae64296fa,0x000fe2466e132f60,
           0x000fe2411df611bd,0x000fe23af34b6f73,0x000fe233eb40bf41,0x000fe22c02cee01b,
           0x000fe22336b81710,0x000fe2198385e5cc,0x000fe20ee586b707,0x000fe20358cb5dfb,
           0x000fe1f6d92465b1,0x000fe1e9621f2c9e,0x000fe1daef02c8da,0x000fe1cb7accb0a6,
           0x000fe1bb002d22c9,0x000fe1a9798349b8,0x000fe196e0d9140c,0x000fe1832fdebc44,
           0x000fe16e5fe5f931,0x000fe15869dccfcf,0x000fe1414647fe78,0x000fe128ed3cf8b2,
           0x000fe10f565b69cf,0x000fe0f478c633ab,0x000fe0d84b1bdd9e,0x000fe0bac36e6688,
           0x000fe09bd73a6b5b,0x000fe07b7b5d920a,0x000fe059a40c26d2,0x000fe03644c5d7f8,
           0x000fe011504979b2,0x000fdfeab887b95c,0x000fdfc26e94a447,0x000fdf986297e305,
           0x000fdf6c83bb8663,0x000fdf3ec0193eed,0x000fdf0f04a5d30a,0x000fdedd3d1aa204,
           0x000fdea953dcfc13,0x000fde7331e3100d,0x000fde3abe9626f2,0x000fddffdfb1dbd5,
           0x000fddc2791ff351,0x000fdd826cd068c6,0x000fdd3f9a8d3856,0x000fdcf9dfc95b0c,
           0x000fdcb1176a55fe,0x000fdc65198ba50b,0x000fdc15bb3b2daa,0x000fdbc2ce2dc4ae,
           0x000fdb6c206aaaca,0x000fdb117becb4a1,0x000fdab2a6379bf0,0x000fda4f5fdfb4e9,
           0x000fd9e76401f3a3,0x000fd97a67a9ce1f,0x000fd90819221429,0x000fd8901f2d4b02,
           0x000fd812182170e1,0x000fd78d98e23cd3,0x000fd7022bb3f082,0x000fd66f4edf96b9,
           0x000fd5d473200305,0x000fd530f9ccff94,0x000fd48432b7b351,0x000fd3cd59a8469e,
           0x000fd30b9368f90a,0x000fd23dea45f500,0x000fd16349e2e04a,0x000fd07a7a3ef98a,
           0x000fcf8219b5df05,0x000fce7895bcfcde,0x000fcd5c220ad5e2,0x000fcc2aadbc17dc,
           0x000fcae1d5e81fbc,0x000fc97ed4e778f9,0x000fc7fe6d4d720e,0x000fc65ccf39c2fc,
           0x000fc4957623cb03,0x000fc2a2fc826dc7,0x000fc07ee19b01cd,0x000fbe213c1cf493,
           0x000fbb8051ac1566,0x000fb890078d120e,0x000fb5411a5b9a95,0x000fb18000547133,
           0x000fad334827f1e2,0x000fa839276708b9,0x000fa263b32e37ed,0x000f9b72d1c52cd1,
           0x000f930a1a281a05,0x000f889f023d820a,0x000f7b577d2be5f3,0x000f69c650c40a8f,
           0x000f51530f0916d8,0x000f2cb0e3c5933e,0x000eeefb15d605d8,0x000e6da6ecf27460]

const we =
    [1.9311480126418366e-15,1.4178028487910829e-17,2.3278824993382448e-17,
     3.0487830247064320e-17,3.6665697714474878e-17,4.2179302189289733e-17,
     4.7222561556862764e-17,5.1911915446217879e-17,5.6323471083955047e-17,
     6.0510082606427647e-17,6.4510165096727506e-17,6.8352646803700541e-17,
     7.2059939574689050e-17,7.5649815537392981e-17,7.9136643961951065e-17,
     8.2532235563518929e-17,8.5846436168850513e-17,8.9087554865647428e-17,
     9.2262679629663719e-17,9.5377914505292719e-17,9.8438560874559257e-17,
     1.0144925809006294e-16,1.0441409405585343e-16,1.0733669323436384e-16,
     1.1022028745670189e-16,1.1306777346479334e-16,1.1588176009705533e-16,
     1.1866460730417886e-16,1.2141845865694359e-16,1.2414526862326387e-16,
     1.2684682560606153e-16,1.2952477151912284e-16,1.3218061851538810e-16,
     1.3481576335745444e-16,1.3743149982367625e-16,1.4002902946807859e-16,
     1.4260947099321287e-16,1.4517386844829297e-16,1.4772319842763584e-16,
     1.5025837641447456e-16,1.5278026239101652e-16,1.5528966581595696e-16,
     1.5778735005459581e-16,1.6027403633350909e-16,1.6275040728083524e-16,
     1.6521711010420076e-16,1.6767475945078279e-16,1.7012393998770646e-16,
     1.7256520873568226e-16,1.7499909718432365e-16,1.7742611321380505e-16,
     1.7984674284430714e-16,1.8226145183195818e-16,1.8467068712763576e-16,
     1.8707487821298258e-16,1.8947443832625899e-16,1.9186976558915995e-16,
     1.9426124404443042e-16,1.9664924461299023e-16,1.9903412597830144e-16,
     2.0141623540485899e-16,2.0379590949693882e-16,2.0617347490308439e-16,
     2.0854924897123771e-16,2.1092354035891528e-16,2.1329664960238294e-16,
     2.1566886964838970e-16,2.1804048635167009e-16,2.2041177894111562e-16,
     2.2278302045723950e-16,2.2515447816331350e-16,2.2752641393233694e-16,
     2.2989908461180186e-16,2.3227274236804366e-16,2.3464763501180916e-16,
     2.3702400630653389e-16,2.3940209626069303e-16,2.4178214140547710e-16,
     2.4416437505894123e-16,2.4654902757768304e-16,2.4893632659702250e-16,
     2.5132649726057970e-16,2.5371976244007951e-16,2.5611634294614988e-16,
     2.5851645773082391e-16,2.6092032408240577e-16,2.6332815781331452e-16,
     2.6574017344147618e-16,2.6815658436579989e-16,2.7057760303623509e-16,
     2.7300344111887955e-16,2.7543430965657619e-16,2.7787041922541278e-16,
     2.8031198008751431e-16,2.8275920234049704e-16,2.8521229606393309e-16,
     2.8767147146315804e-16,2.9013693901073754e-16,2.9260890958589514e-16,
     2.9508759461219033e-16,2.9757320619372521e-16,3.0006595725014739e-16,
     3.0256606165070789e-16,3.0507373434762511e-16,3.0758919150899939e-16,
     3.1011265065151543e-16,3.1264433077316750e-16,3.1518445248623523e-16,
     3.1773323815073683e-16,3.2029091200858335e-16,3.2285770031865573e-16,
     3.2543383149302610e-16,3.2801953623454359e-16,3.3061504767600738e-16,
     3.3322060152114841e-16,3.3583643618764577e-16,3.3846279295240445e-16,
     3.4109991609932597e-16,3.4374805306980633e-16,3.4640745461620167e-16,
     3.4907837495850680e-16,3.5176107194449828e-16,3.5445580721360130e-16,
     3.5716284636474652e-16,3.5988245912849274e-16,3.6261491954370031e-16,
     3.6536050613905045e-16,3.6811950211971757e-16,3.7089219555951389e-16,
     3.7367887959883854e-16,3.7647985264877841e-16,3.7929541860172334e-16,
     3.8212588704887531e-16,3.8497157350504876e-16,3.8783279964117988e-16,
     3.9070989352498183e-16,3.9360318987020748e-16,3.9651303029500381e-16,
     3.9943976358986842e-16,4.0238374599574693e-16,4.0534534149283966e-16,
     4.0832492210071775e-16,4.1132286819038357e-16,4.1433956880894741e-16,
     4.1737542201763194e-16,4.2043083524385856e-16,4.2350622564821518e-16,
     4.2660202050715582e-16,4.2971865761233266e-16,4.3285658568752094e-16,
     4.3601626482415681e-16,4.3919816693657415e-16,4.4240277623809919e-16,
     4.4563058973923611e-16,4.4888211776926172e-16,4.5215788452263475e-16,
     4.5545842863172421e-16,4.5878430376746227e-16,4.6213607926964266e-16,
     4.6551434080870692e-16,4.6891969108099157e-16,4.7235275053955480e-16,
     4.7581415816285534e-16,4.7930457226372470e-16,4.8282467134125866e-16,
     4.8637515497845119e-16,4.8995674478861404e-16,4.9357018541385775e-16,
     4.9721624557917034e-16,5.0089571920591141e-16,5.0460942658884340e-16,
     5.0835821564116245e-16,5.1214296321235415e-16,5.1596457648410618e-16,
     5.1982399444994938e-16,5.2372218948478484e-16,5.2766016901098856e-16,
     5.3163897726836902e-16,5.3565969719590503e-16,5.3972345243389779e-16,
     5.4383140945596370e-16,5.4798477984116296e-16,5.5218482269752343e-16,
     5.5643284724928722e-16,5.6073021560139669e-16,5.6507834569605064e-16,
     5.6947871447763482e-16,5.7393286128396354e-16,5.7844239148359912e-16,
     5.8300898038105864e-16,5.8763437741400573e-16,5.9232041066909314e-16,
     5.9706899174600906e-16,6.0188212100252363e-16,6.0676189321700068e-16,
     6.1171050370897217e-16,6.1673025496306200e-16,6.2182356380685327e-16,
     6.2699296919933262e-16,6.3224114069342115e-16,6.3757088764394262e-16,
     6.4298516924135947e-16,6.4848710546189033e-16,6.5407998903644809e-16,
     6.5976729855445663e-16,6.6555271283433428e-16,6.7144012671064882e-16,
     6.7743366840910103e-16,6.8353771870512740e-16,6.8975693209068478e-16,
     6.9609626020748846e-16,7.0256097784459588e-16,7.0915671184495837e-16,
     7.1588947332085531e-16,7.2276569364381212e-16,7.2979226475290851e-16,
     7.3697658441912426e-16,7.4432660721604146e-16,7.5185090208325131e-16,
     7.5955871753377488e-16,7.6746005575784274e-16,7.7556575712157906e-16,
     7.8388759686228577e-16,7.9243839615735500e-16,8.0123215021130834e-16,
     8.1028417659131464e-16,8.1961128778061250e-16,8.2923199285818092e-16,
     8.3916673441467979e-16,8.4943816836487701e-16,8.6007149633349414e-16,
     8.7109486293879040e-16,8.8253983380721398e-16,8.9444197485198646e-16,
     9.0684155971316690e-16,9.1978444098118649e-16,9.3332313294229516e-16,
     9.4751817065249841e-16,9.6243983456584759e-16,9.7817036547844198e-16,
     9.9480684723838795e-16,1.0124650144288319e-15,1.0312843657756166e-15,
     1.0514351604044550e-15,1.0731281954224043e-15,1.0966288068517408e-15,
     1.1222774909350319e-15,1.1505212963006663e-15,1.1819635283304206e-15,
     1.2174462832361815e-15,1.2581958069755114e-15,1.3060984107128082e-15,
     1.3642786158057857e-15,1.4384889932178723e-15,1.5412190700064194e-15,
     1.7091034077168055e-15]
const fe =
    [1.0000000000000000e+00,9.3814368086217470e-01,9.0046992992574648e-01,
     8.7170433238120359e-01,8.4778550062398961e-01,8.2699329664305032e-01,
     8.0842165152300838e-01,7.9152763697249562e-01,7.7595685204011555e-01,
     7.6146338884989628e-01,7.4786862198519510e-01,7.3503809243142348e-01,
     7.2286765959357202e-01,7.1127476080507601e-01,7.0019265508278816e-01,
     6.8956649611707799e-01,6.7935057226476536e-01,6.6950631673192473e-01,
     6.6000084107899970e-01,6.5080583341457110e-01,6.4189671642726609e-01,
     6.3325199421436607e-01,6.2485273870366598e-01,6.1668218091520766e-01,
     6.0872538207962201e-01,6.0096896636523223e-01,5.9340090169173343e-01,
     5.8601031847726803e-01,5.7878735860284503e-01,5.7172304866482582e-01,
     5.6480919291240017e-01,5.5803828226258745e-01,5.5140341654064129e-01,
     5.4489823767243961e-01,5.3851687200286191e-01,5.3225388026304332e-01,
     5.2610421398361973e-01,5.2006317736823360e-01,5.1412639381474856e-01,
     5.0828977641064288e-01,5.0254950184134772e-01,4.9690198724154955e-01,
     4.9134386959403253e-01,4.8587198734188491e-01,4.8048336393045421e-01,
     4.7517519303737737e-01,4.6994482528395998e-01,4.6478975625042618e-01,
     4.5970761564213769e-01,4.5469615747461550e-01,4.4975325116275500e-01,
     4.4487687341454851e-01,4.4006510084235390e-01,4.3531610321563657e-01,
     4.3062813728845883e-01,4.2599954114303434e-01,4.2142872899761658e-01,
     4.1691418643300288e-01,4.1245446599716118e-01,4.0804818315203240e-01,
     4.0369401253053028e-01,3.9939068447523107e-01,3.9513698183329016e-01,
     3.9093173698479711e-01,3.8677382908413765e-01,3.8266218149600983e-01,
     3.7859575940958079e-01,3.7457356761590216e-01,3.7059464843514600e-01,
     3.6665807978151416e-01,3.6276297335481777e-01,3.5890847294874978e-01,
     3.5509375286678746e-01,3.5131801643748334e-01,3.4758049462163698e-01,
     3.4388044470450241e-01,3.4021714906678002e-01,3.3658991402867761e-01,
     3.3299806876180899e-01,3.2944096426413633e-01,3.2591797239355619e-01,
     3.2242848495608917e-01,3.1897191284495724e-01,3.1554768522712895e-01,
     3.1215524877417955e-01,3.0879406693456019e-01,3.0546361924459026e-01,
     3.0216340067569353e-01,2.9889292101558179e-01,2.9565170428126120e-01,
     2.9243928816189257e-01,2.8925522348967775e-01,2.8609907373707683e-01,
     2.8297041453878075e-01,2.7986883323697292e-01,2.7679392844851736e-01,
     2.7374530965280297e-01,2.7072259679906002e-01,2.6772541993204479e-01,
     2.6475341883506220e-01,2.6180624268936298e-01,2.5888354974901623e-01,
     2.5598500703041538e-01,2.5311029001562946e-01,2.5025908236886230e-01,
     2.4743107566532763e-01,2.4462596913189211e-01,2.4184346939887721e-01,
     2.3908329026244918e-01,2.3634515245705964e-01,2.3362878343743335e-01,
     2.3093391716962741e-01,2.2826029393071670e-01,2.2560766011668407e-01,
     2.2297576805812019e-01,2.2036437584335949e-01,2.1777324714870053e-01,
     2.1520215107537868e-01,2.1265086199297828e-01,2.1011915938898826e-01,
     2.0760682772422204e-01,2.0511365629383771e-01,2.0263943909370902e-01,
     2.0018397469191127e-01,1.9774706610509887e-01,1.9532852067956322e-01,
     1.9292814997677135e-01,1.9054576966319539e-01,1.8818119940425432e-01,
     1.8583426276219711e-01,1.8350478709776746e-01,1.8119260347549629e-01,
     1.7889754657247831e-01,1.7661945459049488e-01,1.7435816917135349e-01,
     1.7211353531532006e-01,1.6988540130252766e-01,1.6767361861725019e-01,
     1.6547804187493600e-01,1.6329852875190182e-01,1.6113493991759203e-01,
     1.5898713896931421e-01,1.5685499236936523e-01,1.5473836938446808e-01,
     1.5263714202744286e-01,1.5055118500103989e-01,1.4848037564386679e-01,
     1.4642459387834494e-01,1.4438372216063478e-01,1.4235764543247220e-01,
     1.4034625107486245e-01,1.3834942886358020e-01,1.3636707092642886e-01,
     1.3439907170221363e-01,1.3244532790138752e-01,1.3050573846833077e-01,
     1.2858020454522817e-01,1.2666862943751067e-01,1.2477091858083096e-01,
     1.2288697950954514e-01,1.2101672182667483e-01,1.1916005717532768e-01,
     1.1731689921155557e-01,1.1548716357863353e-01,1.1367076788274431e-01,
     1.1186763167005630e-01,1.1007767640518538e-01,1.0830082545103380e-01,
     1.0653700405000166e-01,1.0478613930657017e-01,1.0304816017125772e-01,
     1.0132299742595363e-01,9.9610583670637132e-02,9.7910853311492199e-02,
     9.6223742550432798e-02,9.4549189376055859e-02,9.2887133556043541e-02,
     9.1237516631040155e-02,8.9600281910032858e-02,8.7975374467270218e-02,
     8.6362741140756913e-02,8.4762330532368119e-02,8.3174093009632383e-02,
     8.1597980709237419e-02,8.0033947542319905e-02,7.8481949201606421e-02,
     7.6941943170480503e-02,7.5413888734058410e-02,7.3897746992364746e-02,
     7.2393480875708738e-02,7.0901055162371829e-02,6.9420436498728755e-02,
     6.7951593421936601e-02,6.6494496385339774e-02,6.5049117786753749e-02,
     6.3615431999807334e-02,6.2193415408540995e-02,6.0783046445479633e-02,
     5.9384305633420266e-02,5.7997175631200659e-02,5.6621641283742877e-02,
     5.5257689676697037e-02,5.3905310196046087e-02,5.2564494593071692e-02,
     5.1235237055126281e-02,4.9917534282706372e-02,4.8611385573379497e-02,
     4.7316792913181548e-02,4.6033761076175170e-02,4.4762297732943282e-02,
     4.3502413568888183e-02,4.2254122413316234e-02,4.1017441380414819e-02,
     3.9792391023374125e-02,3.8578995503074857e-02,3.7377282772959361e-02,
     3.6187284781931423e-02,3.5009037697397410e-02,3.3842582150874330e-02,
     3.2687963508959535e-02,3.1545232172893609e-02,3.0414443910466604e-02,
     2.9295660224637393e-02,2.8188948763978636e-02,2.7094383780955800e-02,
     2.6012046645134217e-02,2.4942026419731783e-02,2.3884420511558171e-02,
     2.2839335406385240e-02,2.1806887504283581e-02,2.0787204072578117e-02,
     1.9780424338009743e-02,1.8786700744696030e-02,1.7806200410911362e-02,
     1.6839106826039948e-02,1.5885621839973163e-02,1.4945968011691148e-02,
     1.4020391403181938e-02,1.3109164931254991e-02,1.2212592426255381e-02,
     1.1331013597834597e-02,1.0464810181029979e-02,9.6144136425022099e-03,
     8.7803149858089753e-03,7.9630774380170400e-03,7.1633531836349839e-03,
     6.3819059373191791e-03,5.6196422072054830e-03,4.8776559835423923e-03,
     4.1572951208337953e-03,3.4602647778369040e-03,2.7887987935740761e-03,
     2.1459677437189063e-03,1.5362997803015724e-03,9.6726928232717454e-04,
     4.5413435384149677e-04]


const ziggurat_nor_r      = 3.6541528853610087963519472518
const ziggurat_nor_inv_r  = inv(ziggurat_nor_r)
const ziggurat_exp_r      = 7.6971174701310497140446280481

"""
    randn([rng=GLOBAL_RNG], [T=Float64], [dims...])

Generate a normally-distributed random number of type `T` with mean 0 and standard deviation 1.
Optionally generate an array of normally-distributed random numbers.
The `Base` module currently provides an implementation for the types
[`Float16`](@ref), [`Float32`](@ref), and [`Float64`](@ref) (the default).

# Examples

```jldoctest
julia> rng = MersenneTwister(1234);

julia> randn(rng, Float64)
0.8673472019512456

julia> randn(rng, Float32, (2, 4))
2×4 Array{Float32,2}:
 -0.901744  -0.902914  2.21188   -0.271735
 -0.494479   0.864401  0.532813   0.502334
```
"""
@inline function randn(rng::AbstractRNG=GLOBAL_RNG)
    @inbounds begin
        r = rand_ui52(rng)
        rabs = Int64(r>>1) # One bit for the sign
        idx = rabs & 0xFF
        x = ifelse(r % Bool, -rabs, rabs)*wi[idx+1]
        rabs < ki[idx+1] && return x # 99.3% of the time we return here 1st try
        return randn_unlikely(rng, idx, rabs, x)
    end
end

# this unlikely branch is put in a separate function for better efficiency
function randn_unlikely(rng, idx, rabs, x)
    @inbounds if idx == 0
        while true
            xx = -ziggurat_nor_inv_r*log(rand(rng))
            yy = -log(rand(rng))
            yy+yy > xx*xx && return (rabs >> 8) % Bool ? -ziggurat_nor_r-xx : ziggurat_nor_r+xx
        end
    elseif (fi[idx] - fi[idx+1])*rand(rng) + fi[idx+1] < exp(-0.5*x*x)
        return x # return from the triangular area
    else
        return randn(rng)
    end
end

"""
    randexp([rng=GLOBAL_RNG], [T=Float64], [dims...])

Generate a random number of type `T` according to the exponential distribution with scale 1.
Optionally generate an array of such random numbers.
The `Base` module currently provides an implementation for the types
[`Float16`](@ref), [`Float32`](@ref), and [`Float64`](@ref) (the default).

# Examples

```jldoctest
julia> rng = MersenneTwister(1234);

julia> randexp(rng, Float32)
2.4835055f0

julia> randexp(rng, 3, 3)
3×3 Array{Float64,2}:
 1.5167    1.30652   0.344435
 0.604436  2.78029   0.418516
 0.695867  0.693292  0.643644
```
"""
@inline function randexp(rng::AbstractRNG=GLOBAL_RNG)
    @inbounds begin
        ri = rand_ui52(rng)
        idx = ri & 0xFF
        x = ri*we[idx+1]
        ri < ke[idx+1] && return x # 98.9% of the time we return here 1st try
        return randexp_unlikely(rng, idx, x)
    end
end

function randexp_unlikely(rng, idx, x)
    @inbounds if idx == 0
        return ziggurat_exp_r - log(rand(rng))
    elseif (fe[idx] - fe[idx+1])*rand(rng) + fe[idx+1] < exp(-x)
        return x # return from the triangular area
    else
        return randexp(rng)
    end
end

"""
    randn!([rng=GLOBAL_RNG], A::AbstractArray) -> A

Fill the array `A` with normally-distributed (mean 0, standard deviation 1) random numbers.
Also see the [`rand`](@ref) function.

# Example

```jldoctest
julia> rng = MersenneTwister(1234);

julia> randn!(rng, zeros(5))
5-element Array{Float64,1}:
  0.867347
 -0.901744
 -0.494479
 -0.902914
  0.864401
```
"""
function randn! end

"""
    randexp!([rng=GLOBAL_RNG], A::AbstractArray) -> A

Fill the array `A` with random numbers following the exponential distribution (with scale 1).

# Example

```jldoctest
julia> rng = MersenneTwister(1234);

julia> randexp!(rng, zeros(5))
5-element Array{Float64,1}:
 2.48351
 1.5167
 0.604436
 0.695867
 1.30652
```
"""
function randexp! end

let Floats = Union{Float16,Float32,Float64}
    for randfun in [:randn, :randexp]
        randfun! = Symbol(randfun, :!)
        @eval begin
            # scalars
            $randfun(rng::AbstractRNG, ::Type{T}) where {T<:$Floats} = convert(T, $randfun(rng))
            $randfun(::Type{T}) where {T} = $randfun(GLOBAL_RNG, T)

            # filling arrays
            function $randfun!(rng::AbstractRNG, A::AbstractArray{T}) where T
                for i in eachindex(A)
                    @inbounds A[i] = $randfun(rng, T)
                end
                A
            end

            $randfun!(A::AbstractArray) = $randfun!(GLOBAL_RNG, A)

            # generating arrays
            $randfun(rng::AbstractRNG, ::Type{T}, dims::Dims                     ) where {T} = $randfun!(rng, Array{T}(dims))
            # Note that this method explicitly does not define $randfun(rng, T), in order to prevent an infinite recursion.
            $randfun(rng::AbstractRNG, ::Type{T}, dim1::Integer, dims::Integer...) where {T} = $randfun!(rng, Array{T}(dim1, dims...))
            $randfun(                  ::Type{T}, dims::Dims                     ) where {T} = $randfun(GLOBAL_RNG, T, dims)
            $randfun(                  ::Type{T}, dims::Integer...               ) where {T} = $randfun(GLOBAL_RNG, T, dims...)
            $randfun(rng::AbstractRNG,            dims::Dims                     )           = $randfun(rng, Float64, dims)
            $randfun(rng::AbstractRNG,            dims::Integer...               )           = $randfun(rng, Float64, dims...)
            $randfun(                             dims::Dims                     )           = $randfun(GLOBAL_RNG, Float64, dims)
            $randfun(                             dims::Integer...               )           = $randfun(GLOBAL_RNG, Float64, dims...)
        end
    end
end

## random UUID generation

struct UUID
    value::UInt128

    UUID(u::UInt128) = new(u)
end

"""
    uuid1([rng::AbstractRNG=GLOBAL_RNG]) -> UUID

Generates a version 1 (time-based) universally unique identifier (UUID), as specified
by RFC 4122. Note that the Node ID is randomly generated (does not identify the host)
according to section 4.5 of the RFC.

# Example

```jldoctest
julia> rng = MersenneTwister(1234);

julia> Base.Random.uuid1(rng)
2cc938da-5937-11e7-196e-0f4ef71aa64b
```
"""
function uuid1(rng::AbstractRNG=GLOBAL_RNG)
    u = rand(rng, UInt128)

    # mask off clock sequence and node
    u &= 0x00000000000000003fffffffffffffff

    # set the unicast/multicast bit and version
    u |= 0x00000000000010000000010000000000

    # 0x01b21dd213814000 is the number of 100 nanosecond intervals
    # between the UUID epoch and Unix epoch
    timestamp = round(UInt64, time() * 1e7) + 0x01b21dd213814000
    ts_low = timestamp & typemax(UInt32)
    ts_mid = (timestamp >> 32) & typemax(UInt16)
    ts_hi = (timestamp >> 48) & 0x0fff

    u |= UInt128(ts_low) << 96
    u |= UInt128(ts_mid) << 80
    u |= UInt128(ts_hi) << 64

    UUID(u)
end

"""
    uuid4([rng::AbstractRNG=GLOBAL_RNG]) -> UUID

Generates a version 4 (random or pseudo-random) universally unique identifier (UUID),
as specified by RFC 4122.

# Example
```jldoctest
julia> rng = MersenneTwister(1234);

julia> Base.Random.uuid4(rng)
82015f10-44cc-4827-996e-0f4ef71aa64b
```
"""
function uuid4(rng::AbstractRNG=GLOBAL_RNG)
    u = rand(rng, UInt128)
    u &= 0xffffffffffff0fff3fffffffffffffff
    u |= 0x00000000000040008000000000000000
    UUID(u)
end

"""
    uuid_version(u::UUID) -> Integer

Inspects the given UUID and returns its version (see RFC 4122).

# Example

```jldoctest
julia> rng = MersenneTwister(1234);

julia> Base.Random.uuid_version(Base.Random.uuid4(rng))
4
```
"""
function uuid_version(u::UUID)
    Int((u.value >> 76) & 0xf)
end

Base.convert(::Type{UInt128}, u::UUID) = u.value

function Base.convert(::Type{UUID}, s::AbstractString)
    s = lowercase(s)

    if !ismatch(r"^[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}$", s)
        throw(ArgumentError("Malformed UUID string"))
    end

    u = UInt128(0)
    for i in [1:8; 10:13; 15:18; 20:23; 25:36]
        u <<= 4
        d = s[i]-'0'
        u |= 0xf & (d-39*(d>9))
    end
    return UUID(u)
end

function Base.repr(u::UUID)
    u = u.value
    a = Vector{UInt8}(36)
    for i = [36:-1:25; 23:-1:20; 18:-1:15; 13:-1:10; 8:-1:1]
        d = u & 0xf
        a[i] = '0'+d+39*(d>9)
        u >>= 4
    end
    a[[24,19,14,9]] = '-'

    return String(a)
end

Base.show(io::IO, u::UUID) = write(io, Base.repr(u))

# return a random string (often useful for temporary filenames/dirnames)
let b = UInt8['0':'9';'A':'Z';'a':'z']
    global randstring
    randstring(r::AbstractRNG, n::Int) = String(b[rand(r, 1:length(b), n)])
    randstring(r::AbstractRNG) = randstring(r,8)
    randstring(n::Int) = randstring(GLOBAL_RNG, n)
    randstring() = randstring(GLOBAL_RNG)
end


# Fill S (resized as needed) with a random subsequence of A, where
# each element of A is included in S with independent probability p.
# (Note that this is different from the problem of finding a random
#  size-m subset of A where m is fixed!)
function randsubseq!(r::AbstractRNG, S::AbstractArray, A::AbstractArray, p::Real)
    0 <= p <= 1 || throw(ArgumentError("probability $p not in [0,1]"))
    n = length(A)
    p == 1 && return copy!(resize!(S, n), A)
    empty!(S)
    p == 0 && return S
    nexpected = p * length(A)
    sizehint!(S, round(Int,nexpected + 5*sqrt(nexpected)))
    if p > 0.15 # empirical threshold for trivial O(n) algorithm to be better
        for i = 1:n
            rand(r) <= p && push!(S, A[i])
        end
    else
        # Skip through A, in order, from each element i to the next element i+s
        # included in S. The probability that the next included element is
        # s==k (k > 0) is (1-p)^(k-1) * p, and hence the probability (CDF) that
        # s is in {1,...,k} is 1-(1-p)^k = F(k).   Thus, we can draw the skip s
        # from this probability distribution via the discrete inverse-transform
        # method: s = ceil(F^{-1}(u)) where u = rand(), which is simply
        # s = ceil(log(rand()) / log1p(-p)).
        # -log(rand()) is an exponential variate, so can use randexp().
        L = -1 / log1p(-p) # L > 0
        i = 0
        while true
            s = randexp(r) * L
            s >= n - i && return S # compare before ceil to avoid overflow
            push!(S, A[i += ceil(Int,s)])
        end
        # [This algorithm is similar in spirit to, but much simpler than,
        #  the one by Vitter for a related problem in "Faster methods for
        #  random sampling," Comm. ACM Magazine 7, 703-718 (1984).]
    end
    return S
end
randsubseq!(S::AbstractArray, A::AbstractArray, p::Real) = randsubseq!(GLOBAL_RNG, S, A, p)

randsubseq(r::AbstractRNG, A::AbstractArray{T}, p::Real) where {T} = randsubseq!(r, T[], A, p)

"""
    randsubseq(A, p) -> Vector

Return a vector consisting of a random subsequence of the given array `A`, where each
element of `A` is included (in order) with independent probability `p`. (Complexity is
linear in `p*length(A)`, so this function is efficient even if `p` is small and `A` is
large.) Technically, this process is known as "Bernoulli sampling" of `A`.
"""
randsubseq(A::AbstractArray, p::Real) = randsubseq(GLOBAL_RNG, A, p)

"Return a random `Int` (masked with `mask`) in ``[0, n)``, when `n <= 2^52`."
@inline function rand_lt(r::AbstractRNG, n::Int, mask::Int=nextpow2(n)-1)
    # this duplicates the functionality of RangeGenerator objects,
    # to optimize this special case
    while true
        x = (rand_ui52_raw(r) % Int) & mask
        x < n && return x
    end
end

"""
    shuffle!([rng=GLOBAL_RNG,] v)

In-place version of [`shuffle`](@ref): randomly permute the array `v` in-place,
optionally supplying the random-number generator `rng`.

# Example

```jldoctest
julia> rng = MersenneTwister(1234);

julia> shuffle!(rng, collect(1:16))
16-element Array{Int64,1}:
  2
 15
  5
 14
  1
  9
 10
  6
 11
  3
 16
  7
  4
 12
  8
 13
```
"""
function shuffle!(r::AbstractRNG, a::AbstractVector)
    n = length(a)
    @assert n <= Int64(2)^52
    mask = nextpow2(n) - 1
    for i = n:-1:2
        (mask >> 1) == i && (mask >>= 1)
        j = 1 + rand_lt(r, i, mask)
        a[i], a[j] = a[j], a[i]
    end
    return a
end

shuffle!(a::AbstractVector) = shuffle!(GLOBAL_RNG, a)

"""
    shuffle([rng=GLOBAL_RNG,] v)

Return a randomly permuted copy of `v`. The optional `rng` argument specifies a random
number generator (see [Random Numbers](@ref)).
To permute `v` in-place, see [`shuffle!`](@ref).  To obtain randomly permuted
indices, see [`randperm`](@ref).

# Example

```jldoctest
julia> rng = MersenneTwister(1234);

julia> shuffle(rng, collect(1:10))
10-element Array{Int64,1}:
  6
  1
 10
  2
  3
  9
  5
  7
  4
  8
```
"""
shuffle(r::AbstractRNG, a::AbstractVector) = shuffle!(r, copymutable(a))
shuffle(a::AbstractVector) = shuffle(GLOBAL_RNG, a)

"""
    randperm([rng=GLOBAL_RNG,] n::Integer)

Construct a random permutation of length `n`. The optional `rng` argument specifies a random
number generator (see [Random Numbers](@ref)).
To randomly permute a arbitrary vector, see [`shuffle`](@ref)
or [`shuffle!`](@ref).

# Example

```jldoctest
julia> rng = MersenneTwister(1234);

julia> randperm(rng, 4)
4-element Array{Int64,1}:
 2
 1
 4
 3
```
"""
function randperm(r::AbstractRNG, n::Integer)
    a = Vector{typeof(n)}(n)
    @assert n <= Int64(2)^52
    if n == 0
       return a
    end
    a[1] = 1
    mask = 3
    @inbounds for i = 2:Int(n)
        j = 1 + rand_lt(r, i, mask)
        if i != j # a[i] is uninitialized (and could be #undef)
            a[i] = a[j]
        end
        a[j] = i
        i == 1+mask && (mask = 2mask + 1)
    end
    return a
end
randperm(n::Integer) = randperm(GLOBAL_RNG, n)

"""
    randcycle([rng=GLOBAL_RNG,] n::Integer)

Construct a random cyclic permutation of length `n`. The optional `rng`
argument specifies a random number generator, see [Random Numbers](@ref).

# Example

```jldoctest
julia> rng = MersenneTwister(1234);

julia> randcycle(rng, 6)
6-element Array{Int64,1}:
 3
 5
 4
 6
 1
 2
```
"""
function randcycle(r::AbstractRNG, n::Integer)
    a = Vector{typeof(n)}(n)
    n == 0 && return a
    @assert n <= Int64(2)^52
    a[1] = 1
    mask = 3
    @inbounds for i = 2:Int(n)
        j = 1 + rand_lt(r, i-1, mask)
        a[i] = a[j]
        a[j] = i
        i == 1+mask && (mask = 2mask + 1)
    end
    return a
end
randcycle(n::Integer) = randcycle(GLOBAL_RNG, n)

end # module
