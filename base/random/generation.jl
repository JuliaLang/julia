# This file is a part of Julia. License is MIT: https://julialang.org/license

# rand: a non-specified RNG defaults to GLOBAL_RNG

"""
    rand([rng=GLOBAL_RNG], [S], [dims...])

Pick a random element or array of random elements from the set of values specified by `S`; `S` can be

* an indexable collection (for example `1:n` or `['x','y','z']`),
* an `Associative` or `AbstractSet` object,
* a string (considered as a collection of characters), or
* a type: the set of values to pick from is then equivalent to `typemin(S):typemax(S)` for
  integers (this is not applicable to [`BigInt`](@ref)), and to ``[0, 1)`` for floating
  point numbers;

`S` defaults to [`Float64`](@ref).

# Examples
```julia-repl
julia> rand(Int, 2)
2-element Array{Int64,1}:
 1339893410598768192
 1575814717733606317

julia> rand(MersenneTwister(0), Dict(1=>2, 3=>4))
1=>2
```

!!! note
    The complexity of `rand(rng, s::Union{Associative,AbstractSet})`
    is linear in the length of `s`, unless an optimized method with
    constant complexity is available, which is the case for `Dict`,
    `Set` and `IntSet`. For more than a few calls, use `rand(rng,
    collect(s))` instead, or either `rand(rng, Dict(s))` or `rand(rng,
    Set(s))` as appropriate.
"""
@inline rand() = rand(GLOBAL_RNG, CloseOpen)
@inline rand(T::Type) = rand(GLOBAL_RNG, T)
rand(dims::Dims) = rand(GLOBAL_RNG, dims)
rand(dims::Integer...) = rand(convert(Dims, dims))
rand(T::Type, dims::Dims) = rand(GLOBAL_RNG, T, dims)
rand(T::Type, d1::Integer, dims::Integer...) = rand(T, tuple(Int(d1), convert(Dims, dims)...))
rand!(A::AbstractArray) = rand!(GLOBAL_RNG, A)

rand(r::AbstractArray) = rand(GLOBAL_RNG, r)

"""
    rand!([rng=GLOBAL_RNG], A, [S=eltype(A)])

Populate the array `A` with random values. If `S` is specified
(`S` can be a type or a collection, cf. [`rand`](@ref) for details),
the values are picked randomly from `S`.
This is equivalent to `copy!(A, rand(rng, S, size(A)))`
but without allocating a new array.

# Examples
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
rand(r::AbstractArray, dims::Integer...) = rand(GLOBAL_RNG, r, convert(Dims, dims))

## random floating point values

@inline rand(r::AbstractRNG) = rand(r, CloseOpen)

rand_ui10_raw(r::AbstractRNG)    = rand(r, UInt16)
rand_ui23_raw(r::AbstractRNG)    = rand(r, UInt32)

## random integers

@inline rand_ui52_raw(r::AbstractRNG) = reinterpret(UInt64, rand(r, Close1Open2))
@inline rand_ui52(r::AbstractRNG) = rand_ui52_raw(r) & 0x000fffffffffffff

## random Complex values

rand(r::AbstractRNG, ::Type{Complex{T}}) where {T<:Real} = complex(rand(r, T), rand(r, T))

# random Char values
# returns a random valid Unicode scalar value (i.e. 0 - 0xd7ff, 0xe000 - # 0x10ffff)
function rand(r::AbstractRNG, ::Type{Char})
    c = rand(r, 0x00000000:0x0010f7ff)
    (c < 0xd800) ? Char(c) : Char(c+0x800)
end

# random values from Dict, Set, IntSet (for efficiency)
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

## Arrays of random numbers

rand(r::AbstractRNG, dims::Dims) = rand(r, Float64, dims)
rand(r::AbstractRNG, dims::Integer...) = rand(r, convert(Dims, dims))

rand(r::AbstractRNG, T::Type, dims::Dims) = rand!(r, Array{T}(dims))
rand(r::AbstractRNG, T::Type, d1::Integer, dims::Integer...) = rand(r, T, tuple(Int(d1), convert(Dims, dims)...))
# note: the above method would trigger an ambiguity warning if d1 was not separated out:
# rand(r, ()) would match both this method and rand(r, dims::Dims)
# moreover, a call like rand(r, NotImplementedType()) would be an infinite loop

function rand!(r::AbstractRNG, A::AbstractArray{T}, ::Type{X}=T) where {T,X}
    for i in eachindex(A)
        @inbounds A[i] = rand(r, X)
    end
    A
end

rand!(A::AbstractArray, ::Type{X}) where {X} = rand!(GLOBAL_RNG, A, X)

function rand!(r::AbstractRNG, A::AbstractArray, s::Union{Dict,Set,IntSet})
    for i in eachindex(A)
        @inbounds A[i] = rand(r, s)
    end
    A
end

# avoid linear complexity for repeated calls with generic containers
rand!(r::AbstractRNG, A::AbstractArray, s::Union{Associative,AbstractSet}) = rand!(r, A, collect(s))

rand!(A::AbstractArray, s::Union{Associative,AbstractSet}) = rand!(GLOBAL_RNG, A, s)

rand(r::AbstractRNG, s::Associative{K,V}, dims::Dims) where {K,V} = rand!(r, Array{Pair{K,V}}(dims), s)
rand(r::AbstractRNG, s::AbstractSet{T}, dims::Dims) where {T} = rand!(r, Array{T}(dims), s)
rand(r::AbstractRNG, s::Union{Associative,AbstractSet}, dims::Integer...) = rand(r, s, convert(Dims, dims))
rand(s::Union{Associative,AbstractSet}, dims::Integer...) = rand(GLOBAL_RNG, s, convert(Dims, dims))
rand(s::Union{Associative,AbstractSet}, dims::Dims) = rand(GLOBAL_RNG, s, dims)


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

function rand(rng::AbstractRNG, g::RangeGeneratorInt{T,U}) where U<:Unsigned where T<:Integer
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

rand(rng::AbstractRNG, r::AbstractArray{T}, dims::Dims) where {T} = rand!(rng, Array{T}(dims), r)
rand(rng::AbstractRNG, r::AbstractArray, dims::Integer...) = rand(rng, r, convert(Dims, dims))

# rand from a string

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

## rand from a string for arrays
# we use collect(str), which is most of the time more efficient than specialized methods
# (except maybe for very small arrays)
rand!(rng::AbstractRNG, A::AbstractArray, str::AbstractString) = rand!(rng, A, collect(str))
rand!(A::AbstractArray, str::AbstractString) = rand!(GLOBAL_RNG, A, str)
rand(rng::AbstractRNG, str::AbstractString, dims::Dims) = rand!(rng, Array{eltype(str)}(dims), str)
rand(rng::AbstractRNG, str::AbstractString, d1::Integer, dims::Integer...) = rand(rng, str, convert(Dims, tuple(d1, dims...)))
rand(str::AbstractString, dims::Dims) = rand(GLOBAL_RNG, str, dims)
rand(str::AbstractString, d1::Integer, dims::Integer...) = rand(GLOBAL_RNG, str, d1, dims...)
