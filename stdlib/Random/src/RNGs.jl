# This file is a part of Julia. License is MIT: https://julialang.org/license

## RandomDevice

# SamplerUnion(Union{X,Y,...}) == Union{SamplerType{X},SamplerType{Y},...}
SamplerUnion(U::Union) = Union{map(T->SamplerType{T}, Base.uniontypes(U))...}
const SamplerBoolBitInteger = SamplerUnion(Union{Bool, BitInteger})

if Sys.iswindows()
    struct RandomDevice <: AbstractRNG
        buffer::Vector{UInt128}

        RandomDevice() = new(Vector{UInt128}(undef, 1))
    end

    function rand(rd::RandomDevice, sp::SamplerBoolBitInteger)
        rand!(rd, rd.buffer)
        @inbounds return rd.buffer[1] % sp[]
    end
else # !windows
    struct RandomDevice <: AbstractRNG
        file::IOStream
        unlimited::Bool

        RandomDevice(; unlimited::Bool=true) =
            new(open(unlimited ? "/dev/urandom" : "/dev/random"), unlimited)
    end

    rand(rd::RandomDevice, sp::SamplerBoolBitInteger) = read( rd.file, sp[])

    # function serialize(s::AbstractSerializer, rd::RandomDevice)
    #     Serialization.serialize_type(s, typeof(rd))
    #     serialize(s, rd.unlimited)
    # end
    # function deserialize(s::AbstractSerializer, t::Type{RandomDevice})
    #     unlimited = deserialize(s)
    #     return RandomDevice(unlimited=unlimited)
    # end

end # os-test

# NOTE: this can't be put within the if-else block above
for T in (Bool, BitInteger_types...)
    if Sys.iswindows()
        @eval function rand!(rd::RandomDevice, A::Array{$T}, ::SamplerType{$T})
            ccall((:SystemFunction036, :Advapi32), stdcall, UInt8, (Ptr{Cvoid}, UInt32),
                  A, sizeof(A))
            A
        end
    else
        @eval rand!(rd::RandomDevice, A::Array{$T}, ::SamplerType{$T}) = read!(rd.file, A)
    end
end

# RandomDevice produces natively UInt64
rng_native_52(::RandomDevice) = UInt64

"""
    RandomDevice()

Create a `RandomDevice` RNG object.
Two such objects will always generate different streams of random numbers.
The entropy is obtained from the operating system.
"""
RandomDevice

RandomDevice(::Nothing) = RandomDevice()
seed!(rng::RandomDevice) = rng


## MersenneTwister

const MT_CACHE_F = 501 << 1 # number of Float64 in the cache
const MT_CACHE_I = 501 << 4 # number of bytes in the UInt128 cache

@assert dsfmt_get_min_array_size() <= MT_CACHE_F

mutable struct MersenneTwister <: AbstractRNG
    seed::Vector{UInt32}
    state::DSFMT_state
    vals::Vector{Float64}
    ints::Vector{UInt128}
    idxF::Int
    idxI::Int

    function MersenneTwister(seed, state, vals, ints, idxF, idxI)
        length(vals) == MT_CACHE_F && 0 <= idxF <= MT_CACHE_F ||
            throw(DomainError((length(vals), idxF),
                      "`length(vals)` and `idxF` must be consistent with $MT_CACHE_F"))
        length(ints) == MT_CACHE_I >> 4 && 0 <= idxI <= MT_CACHE_I ||
            throw(DomainError((length(ints), idxI),
                      "`length(ints)` and `idxI` must be consistent with $MT_CACHE_I"))
        new(seed, state, vals, ints, idxF, idxI)
    end
end

MersenneTwister(seed::Vector{UInt32}, state::DSFMT_state) =
    MersenneTwister(seed, state,
                    Vector{Float64}(undef, MT_CACHE_F),
                    Vector{UInt128}(undef, MT_CACHE_I >> 4),
                    MT_CACHE_F, 0)

"""
    MersenneTwister(seed)
    MersenneTwister()

Create a `MersenneTwister` RNG object. Different RNG objects can have
their own seeds, which may be useful for generating different streams
of random numbers.
The `seed` may be a non-negative integer or a vector of
`UInt32` integers. If no seed is provided, a randomly generated one
is created (using entropy from the system).
See the [`seed!`](@ref) function for reseeding an already existing
`MersenneTwister` object.


# Examples
```jldoctest
julia> rng = MersenneTwister(1234);

julia> x1 = rand(rng, 2)
2-element Array{Float64,1}:
 0.5908446386657102
 0.7667970365022592

julia> rng = MersenneTwister(1234);

julia> x2 = rand(rng, 2)
2-element Array{Float64,1}:
 0.5908446386657102
 0.7667970365022592

julia> x1 == x2
true
```
"""
MersenneTwister(seed=nothing) =
    seed!(MersenneTwister(Vector{UInt32}(), DSFMT_state()), seed)


function copy!(dst::MersenneTwister, src::MersenneTwister)
    copyto!(resize!(dst.seed, length(src.seed)), src.seed)
    copy!(dst.state, src.state)
    copyto!(dst.vals, src.vals)
    copyto!(dst.ints, src.ints)
    dst.idxF = src.idxF
    dst.idxI = src.idxI
    dst
end

copy(src::MersenneTwister) =
    MersenneTwister(copy(src.seed), copy(src.state), copy(src.vals), copy(src.ints),
                    src.idxF, src.idxI)


==(r1::MersenneTwister, r2::MersenneTwister) =
    r1.seed == r2.seed && r1.state == r2.state &&
    isequal(r1.vals, r2.vals) &&
    isequal(r1.ints, r2.ints) &&
    r1.idxF == r2.idxF && r1.idxI == r2.idxI

hash(r::MersenneTwister, h::UInt) =
    foldr(hash, (r.seed, r.state, r.vals, r.ints, r.idxF, r.idxI); init=h)

function fillcache_zeros!(r::MersenneTwister)
    # the use of this function is not strictly necessary, but it makes
    # comparing two MersenneTwister RNGs easier
    fill!(r.vals, 0.0)
    fill!(r.ints, zero(UInt128))
    r
end


### low level API

#### floats

mt_avail(r::MersenneTwister) = MT_CACHE_F - r.idxF
mt_empty(r::MersenneTwister) = r.idxF == MT_CACHE_F
mt_setfull!(r::MersenneTwister) = r.idxF = 0
mt_setempty!(r::MersenneTwister) = r.idxF = MT_CACHE_F
mt_pop!(r::MersenneTwister) = @inbounds return r.vals[r.idxF+=1]

function gen_rand(r::MersenneTwister)
    GC.@preserve r dsfmt_fill_array_close1_open2!(r.state, pointer(r.vals), length(r.vals))
    mt_setfull!(r)
end

reserve_1(r::MersenneTwister) = (mt_empty(r) && gen_rand(r); nothing)
# `reserve` allows one to call `rand_inbounds` n times
# precondition: n <= MT_CACHE_F
reserve(r::MersenneTwister, n::Int) = (mt_avail(r) < n && gen_rand(r); nothing)

#### ints

logsizeof(::Type{<:Union{Bool,Int8,UInt8}}) = 0
logsizeof(::Type{<:Union{Int16,UInt16}}) = 1
logsizeof(::Type{<:Union{Int32,UInt32}}) = 2
logsizeof(::Type{<:Union{Int64,UInt64}}) = 3
logsizeof(::Type{<:Union{Int128,UInt128}}) = 4

idxmask(::Type{<:Union{Bool,Int8,UInt8}}) = 15
idxmask(::Type{<:Union{Int16,UInt16}}) = 7
idxmask(::Type{<:Union{Int32,UInt32}}) = 3
idxmask(::Type{<:Union{Int64,UInt64}}) = 1
idxmask(::Type{<:Union{Int128,UInt128}}) = 0


mt_avail(r::MersenneTwister, ::Type{T}) where {T<:BitInteger} =
    r.idxI >> logsizeof(T)

function mt_setfull!(r::MersenneTwister, ::Type{<:BitInteger})
    rand!(r, r.ints)
    r.idxI = MT_CACHE_I
end

mt_setempty!(r::MersenneTwister, ::Type{<:BitInteger}) = r.idxI = 0

function reserve1(r::MersenneTwister, ::Type{T}) where T<:BitInteger
    r.idxI < sizeof(T) && mt_setfull!(r, T)
    nothing
end

function mt_pop!(r::MersenneTwister, ::Type{T}) where T<:BitInteger
    reserve1(r, T)
    r.idxI -= sizeof(T)
    i = r.idxI
    @inbounds x128 = r.ints[1 + i >> 4]
    i128 = (i >> logsizeof(T)) & idxmask(T) # 0-based "indice" in x128
    (x128 >> (i128 * (sizeof(T) << 3))) % T
end

# not necessary, but very slightly more efficient
function mt_pop!(r::MersenneTwister, ::Type{T}) where {T<:Union{Int128,UInt128}}
    reserve1(r, T)
    @inbounds res = r.ints[r.idxI >> 4]
    r.idxI -= 16
    res % T
end


### seeding

#### make_seed()

# make_seed produces values of type Vector{UInt32}, suitable for MersenneTwister seeding
function make_seed()
    try
        return rand(RandomDevice(), UInt32, 4)
    catch
        println(stderr,
                "Entropy pool not available to seed RNG; using ad-hoc entropy sources.")
        seed = reinterpret(UInt64, time())
        seed = hash(seed, UInt64(getpid()))
        try
            seed = hash(seed, parse(UInt64,
                                    read(pipeline(`ifconfig`, `sha1sum`), String)[1:40],
                                    base = 16))
        catch
        end
        return make_seed(seed)
    end
end

function make_seed(n::Integer)
    n < 0 && throw(DomainError(n, "`n` must be non-negative."))
    seed = UInt32[]
    while true
        push!(seed, n & 0xffffffff)
        n >>= 32
        if n == 0
            return seed
        end
    end
end

#### seed!()

function seed!(r::MersenneTwister, seed::Vector{UInt32})
    copyto!(resize!(r.seed, length(seed)), seed)
    dsfmt_init_by_array(r.state, r.seed)
    mt_setempty!(r)
    mt_setempty!(r, UInt128)
    fillcache_zeros!(r)
    return r
end

seed!(r::MersenneTwister=GLOBAL_RNG) = seed!(r, make_seed())
seed!(r::MersenneTwister, n::Integer) = seed!(r, make_seed(n))
seed!(seed::Union{Integer,Vector{UInt32}}) = seed!(GLOBAL_RNG, seed)


### Global RNG (must be defined after seed!)

const GLOBAL_RNG = MersenneTwister(0)


### generation

# MersenneTwister produces natively Float64
rng_native_52(::MersenneTwister) = Float64

#### helper functions

# precondition: !mt_empty(r)
rand_inbounds(r::MersenneTwister, ::CloseOpen12_64) = mt_pop!(r)
rand_inbounds(r::MersenneTwister, ::CloseOpen01_64=CloseOpen01()) =
    rand_inbounds(r, CloseOpen12()) - 1.0

rand_inbounds(r::MersenneTwister, ::UInt52Raw{T}) where {T<:BitInteger} =
    reinterpret(UInt64, rand_inbounds(r, CloseOpen12())) % T

function rand(r::MersenneTwister, x::SamplerTrivial{UInt52Raw{UInt64}})
    reserve_1(r)
    rand_inbounds(r, x[])
end

function rand(r::MersenneTwister, ::SamplerTrivial{UInt2x52Raw{UInt128}})
    reserve(r, 2)
    rand_inbounds(r, UInt52Raw(UInt128)) << 64 | rand_inbounds(r, UInt52Raw(UInt128))
end

function rand(r::MersenneTwister, ::SamplerTrivial{UInt104Raw{UInt128}})
    reserve(r, 2)
    rand_inbounds(r, UInt52Raw(UInt128)) << 52 ⊻ rand_inbounds(r, UInt52Raw(UInt128))
end

#### floats

rand(r::MersenneTwister, sp::SamplerTrivial{CloseOpen12_64}) =
    (reserve_1(r); rand_inbounds(r, sp[]))

#### integers

rand(r::MersenneTwister, T::SamplerUnion(Union{Int64,UInt64,Int128,UInt128})) =
    mt_pop!(r, T[])

rand(r::MersenneTwister, T::SamplerUnion(Union{Bool,Int8,UInt8,Int16,UInt16,Int32,UInt32})) =
    rand(r, UInt52Raw()) % T[]

#### arrays of floats

##### AbstractArray

function rand!(r::MersenneTwister, A::AbstractArray{Float64},
               I::SamplerTrivial{<:FloatInterval_64})
    region = LinearIndices(A)
    # what follows is equivalent to this simple loop but more efficient:
    # for i=region
    #     @inbounds A[i] = rand(r, I[])
    # end
    m = Base.checked_sub(first(region), 1)
    n = last(region)
    while m < n
        s = mt_avail(r)
        if s == 0
            gen_rand(r)
            s = mt_avail(r)
        end
        m2 = min(n, m+s)
        for i=m+1:m2
            @inbounds A[i] = rand_inbounds(r, I[])
        end
        m = m2
    end
    A
end


##### Array : internal functions

# internal array-like type to circumevent the lack of flexibility with reinterpret
struct UnsafeView{T} <: DenseArray{T,1}
    ptr::Ptr{T}
    len::Int
end

Base.length(a::UnsafeView) = a.len
Base.getindex(a::UnsafeView, i::Int) = unsafe_load(a.ptr, i)
Base.setindex!(a::UnsafeView, x, i::Int) = unsafe_store!(a.ptr, x, i)
Base.pointer(a::UnsafeView) = a.ptr
Base.size(a::UnsafeView) = (a.len,)
Base.elsize(::UnsafeView{T}) where {T} = sizeof(T)

# this is essentially equivalent to rand!(r, ::AbstractArray{Float64}, I) above, but due to
# optimizations which can't be done currently when working with pointers, we have to re-order
# manually the computation flow to get the performance
# (see https://discourse.julialang.org/t/unsafe-store-sometimes-slower-than-arrays-setindex)
function _rand_max383!(r::MersenneTwister, A::UnsafeView{Float64}, I::FloatInterval_64)
    n = length(A)
    @assert n <= dsfmt_get_min_array_size()+1 # == 383
    mt_avail(r) == 0 && gen_rand(r)
    # from now on, at most one call to gen_rand(r) will be necessary
    m = min(n, mt_avail(r))
    GC.@preserve r unsafe_copyto!(A.ptr, pointer(r.vals, r.idxF+1), m)
    if m == n
        r.idxF += m
    else # m < n
        gen_rand(r)
        GC.@preserve r unsafe_copyto!(A.ptr+m*sizeof(Float64), pointer(r.vals), n-m)
        r.idxF = n-m
    end
    if I isa CloseOpen01
        for i=1:n
            A[i] -= 1.0
        end
    end
    A
end


fill_array!(s::DSFMT_state, A::Ptr{Float64}, n::Int, ::CloseOpen01_64) =
    dsfmt_fill_array_close_open!(s, A, n)

fill_array!(s::DSFMT_state, A::Ptr{Float64}, n::Int, ::CloseOpen12_64) =
    dsfmt_fill_array_close1_open2!(s, A, n)


function rand!(r::MersenneTwister, A::UnsafeView{Float64},
               I::SamplerTrivial{<:FloatInterval_64})
    # depending on the alignment of A, the data written by fill_array! may have
    # to be left-shifted by up to 15 bytes (cf. unsafe_copyto! below) for
    # reproducibility purposes;
    # so, even for well aligned arrays, fill_array! is used to generate only
    # the n-2 first values (or n-3 if n is odd), and the remaining values are
    # generated by the scalar version of rand
    n = length(A)
    n2 = (n-2) ÷ 2 * 2
    n2 < dsfmt_get_min_array_size() && return _rand_max383!(r, A, I[])

    pA = A.ptr
    align = Csize_t(pA) % 16
    if align > 0
        pA2 = pA + 16 - align
        fill_array!(r.state, pA2, n2, I[]) # generate the data in-place, but shifted
        unsafe_copyto!(pA, pA2, n2) # move the data to the beginning of the array
    else
        fill_array!(r.state, pA, n2, I[])
    end
    for i=n2+1:n
        A[i] = rand(r, I[])
    end
    A
end

# fills up A reinterpreted as an array of Float64 with n64 values
function _rand!(r::MersenneTwister, A::Array{T}, n64::Int, I::FloatInterval_64) where T
    # n64 is the length in terms of `Float64` of the target
    @assert sizeof(Float64)*n64 <= sizeof(T)*length(A) && isbitstype(T)
    GC.@preserve A rand!(r, UnsafeView{Float64}(pointer(A), n64), SamplerTrivial(I))
    A
end

##### Array: Float64, Float16, Float32

rand!(r::MersenneTwister, A::Array{Float64}, I::SamplerTrivial{<:FloatInterval_64}) =
    _rand!(r, A, length(A), I[])

mask128(u::UInt128, ::Type{Float16}) =
    (u & 0x03ff03ff03ff03ff03ff03ff03ff03ff) | 0x3c003c003c003c003c003c003c003c00

mask128(u::UInt128, ::Type{Float32}) =
    (u & 0x007fffff007fffff007fffff007fffff) | 0x3f8000003f8000003f8000003f800000

for T in (Float16, Float32)
    @eval function rand!(r::MersenneTwister, A::Array{$T}, ::SamplerTrivial{CloseOpen12{$T}})
        n = length(A)
        n128 = n * sizeof($T) ÷ 16
        _rand!(r, A, 2*n128, CloseOpen12())
        GC.@preserve A begin
            A128 = UnsafeView{UInt128}(pointer(A), n128)
            for i in 1:n128
                u = A128[i]
                u ⊻= u << 26
                # at this point, the 64 low bits of u, "k" being the k-th bit of A128[i] and "+"
                # the bit xor, are:
                # [..., 58+32,..., 53+27, 52+26, ..., 33+7, 32+6, ..., 27+1, 26, ..., 1]
                # the bits needing to be random are
                # [1:10, 17:26, 33:42, 49:58] (for Float16)
                # [1:23, 33:55] (for Float32)
                # this is obviously satisfied on the 32 low bits side, and on the high side,
                # the entropy comes from bits 33:52 of A128[i] and then from bits 27:32
                # (which are discarded on the low side)
                # this is similar for the 64 high bits of u
                A128[i] = mask128(u, $T)
            end
        end
        for i in 16*n128÷sizeof($T)+1:n
            @inbounds A[i] = rand(r, $T) + one($T)
        end
        A
    end

    @eval function rand!(r::MersenneTwister, A::Array{$T}, ::SamplerTrivial{CloseOpen01{$T}})
        rand!(r, A, CloseOpen12($T))
        I32 = one(Float32)
        for i in eachindex(A)
            @inbounds A[i] = Float32(A[i])-I32 # faster than "A[i] -= one(T)" for T==Float16
        end
        A
    end
end

#### arrays of integers

function rand!(r::MersenneTwister, A::UnsafeView{UInt128}, ::SamplerType{UInt128})
    n::Int=length(A)
    i = n
    while true
        rand!(r, UnsafeView{Float64}(A.ptr, 2i), CloseOpen12())
        n < 5 && break
        i = 0
        while n-i >= 5
            u = A[i+=1]
            A[n]    ⊻= u << 48
            A[n-=1] ⊻= u << 36
            A[n-=1] ⊻= u << 24
            A[n-=1] ⊻= u << 12
            n-=1
        end
    end
    if n > 0
        u = rand(r, UInt2x52Raw())
        for i = 1:n
            A[i] ⊻= u << (12*i)
        end
    end
    A
end

for T in BitInteger_types
    @eval rand!(r::MersenneTwister, A::Array{$T}, sp::SamplerType{$T}) =
        (GC.@preserve A rand!(r, UnsafeView(pointer(A), length(A)), sp); A)

    T == UInt128 && continue

    @eval function rand!(r::MersenneTwister, A::UnsafeView{$T}, ::SamplerType{$T})
        n = length(A)
        n128 = n * sizeof($T) ÷ 16
        rand!(r, UnsafeView{UInt128}(pointer(A), n128))
        for i = 16*n128÷sizeof($T)+1:n
            @inbounds A[i] = rand(r, $T)
        end
        A
    end
end

#### from a range

for T in BitInteger_types, R=(1, Inf) # eval because of ambiguity otherwise
    @eval Sampler(::Type{MersenneTwister}, r::UnitRange{$T}, ::Val{$R}) =
        SamplerRangeFast(r)
end


### randjump

# Old randjump methods are deprecated, the scalar version is in the Future module.

_randjump(r::MersenneTwister, jumppoly::DSFMT.GF2X) =
    fillcache_zeros!(MersenneTwister(copy(r.seed), DSFMT.dsfmt_jump(r.state, jumppoly)))
