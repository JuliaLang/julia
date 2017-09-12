# This file is a part of Julia. License is MIT: https://julialang.org/license

## RandomDevice

const BoolBitIntegerType = Union{Type{Bool},Base.BitIntegerType}
const BoolBitIntegerArray = Union{Array{Bool},Base.BitIntegerArray}

if Sys.iswindows()
    struct RandomDevice <: AbstractRNG
        buffer::Vector{UInt128}

        RandomDevice() = new(Vector{UInt128}(1))
    end

    function rand(rd::RandomDevice, T::BoolBitIntegerType)
        rand!(rd, rd.buffer)
        @inbounds return rd.buffer[1] % T
    end

    function rand!(rd::RandomDevice, A::BoolBitIntegerArray)
        ccall((:SystemFunction036, :Advapi32), stdcall, UInt8, (Ptr{Void}, UInt32),
              A, sizeof(A))
        A
    end
else # !windows
    struct RandomDevice <: AbstractRNG
        file::IOStream
        unlimited::Bool

        RandomDevice(unlimited::Bool=true) =
            new(open(unlimited ? "/dev/urandom" : "/dev/random"), unlimited)
    end

    rand(rd::RandomDevice, T::BoolBitIntegerType)   = read( rd.file, T)
    rand!(rd::RandomDevice, A::BoolBitIntegerArray) = read!(rd.file, A)
end # os-test

"""
    RandomDevice()

Create a `RandomDevice` RNG object.
Two such objects will always generate different streams of random numbers.
The entropy is obtained from the operating system.
"""
RandomDevice

RandomDevice(::Void) = RandomDevice()
srand(rng::RandomDevice) = rng

### generation of floats

rand(r::RandomDevice, I::FloatInterval) = rand_generic(r, I)


## MersenneTwister

const MTCacheLength = dsfmt_get_min_array_size()

mutable struct MersenneTwister <: AbstractRNG
    seed::Vector{UInt32}
    state::DSFMT_state
    vals::Vector{Float64}
    idx::Int

    function MersenneTwister(seed, state, vals, idx)
        length(vals) == MTCacheLength && 0 <= idx <= MTCacheLength ||
            throw(DomainError((length(vals), idx),
                      "`length(vals)` and `idx` must be consistent with $MTCacheLength"))
        new(seed, state, vals, idx)
    end
end

MersenneTwister(seed::Vector{UInt32}, state::DSFMT_state) =
    MersenneTwister(seed, state, zeros(Float64, MTCacheLength), MTCacheLength)

"""
    MersenneTwister(seed)
    MersenneTwister()

Create a `MersenneTwister` RNG object. Different RNG objects can have
their own seeds, which may be useful for generating different streams
of random numbers.
The `seed` may be a non-negative integer or a vector of
`UInt32` integers. If no seed is provided, a randomly generated one
is created (using entropy from the system).
See the [`srand`](@ref) function for reseeding an already existing
`MersenneTwister` object.


# Examples
```jldoctest
julia> rng = MersenneTwister(1234);

julia> x1 = rand(rng, 2)
2-element Array{Float64,1}:
 0.590845
 0.766797

julia> rng = MersenneTwister(1234);

julia> x2 = rand(rng, 2)
2-element Array{Float64,1}:
 0.590845
 0.766797

julia> x1 == x2
true
```
"""
MersenneTwister(seed=nothing) =
    srand(MersenneTwister(Vector{UInt32}(), DSFMT_state()), seed)

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
    r1.seed == r2.seed && r1.state == r2.state && isequal(r1.vals, r2.vals) &&
    r1.idx == r2.idx

hash(r::MersenneTwister, h::UInt) = foldr(hash, h, (r.seed, r.state, r.vals, r.idx))


### low level API

mt_avail(r::MersenneTwister) = MTCacheLength - r.idx
mt_empty(r::MersenneTwister) = r.idx == MTCacheLength
mt_setfull!(r::MersenneTwister) = r.idx = 0
mt_setempty!(r::MersenneTwister) = r.idx = MTCacheLength
mt_pop!(r::MersenneTwister) = @inbounds return r.vals[r.idx+=1]

function gen_rand(r::MersenneTwister)
    dsfmt_fill_array_close1_open2!(r.state, pointer(r.vals), length(r.vals))
    mt_setfull!(r)
end

reserve_1(r::MersenneTwister) = (mt_empty(r) && gen_rand(r); nothing)
# `reserve` allows one to call `rand_inbounds` n times
# precondition: n <= MTCacheLength
reserve(r::MersenneTwister, n::Int) = (mt_avail(r) < n && gen_rand(r); nothing)


### seeding

#### make_seed()

# make_seed produces values of type Vector{UInt32}, suitable for MersenneTwister seeding
function make_seed()
    try
        return rand(RandomDevice(), UInt32, 4)
    catch
        println(STDERR,
                "Entropy pool not available to seed RNG; using ad-hoc entropy sources.")
        seed = reinterpret(UInt64, time())
        seed = hash(seed, UInt64(getpid()))
        try
            seed = hash(seed, parse(UInt64,
                                    read(pipeline(`ifconfig`, `sha1sum`), String)[1:40],
                                    16))
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

#### srand()

function srand(r::MersenneTwister, seed::Vector{UInt32})
    copy!(resize!(r.seed, length(seed)), seed)
    dsfmt_init_by_array(r.state, r.seed)
    mt_setempty!(r)
    return r
end

srand(r::MersenneTwister=GLOBAL_RNG) = srand(r, make_seed())
srand(r::MersenneTwister, n::Integer) = srand(r, make_seed(n))
srand(seed::Union{Integer,Vector{UInt32}}) = srand(GLOBAL_RNG, seed)


### Global RNG (must be defined after srand)

const GLOBAL_RNG = MersenneTwister(0)


### generation

#### helper functions

# precondition: !mt_empty(r)
rand_inbounds(r::MersenneTwister, ::Close1Open2_64) = mt_pop!(r)
rand_inbounds(r::MersenneTwister, ::CloseOpen_64) =
    rand_inbounds(r, Close1Open2()) - 1.0
rand_inbounds(r::MersenneTwister) = rand_inbounds(r, CloseOpen())

rand_ui52_raw_inbounds(r::MersenneTwister) =
    reinterpret(UInt64, rand_inbounds(r, Close1Open2()))
rand_ui52_raw(r::MersenneTwister) = (reserve_1(r); rand_ui52_raw_inbounds(r))

function rand_ui2x52_raw(r::MersenneTwister)
    reserve(r, 2)
    rand_ui52_raw_inbounds(r) % UInt128 << 64 | rand_ui52_raw_inbounds(r)
end

function rand_ui104_raw(r::MersenneTwister)
    reserve(r, 2)
    rand_ui52_raw_inbounds(r) % UInt128 << 52 ⊻ rand_ui52_raw_inbounds(r)
end

rand_ui10_raw(r::MersenneTwister) = rand_ui52_raw(r)
rand_ui23_raw(r::MersenneTwister) = rand_ui52_raw(r)

#### floats

rand(r::MersenneTwister, I::FloatInterval_64) = (reserve_1(r); rand_inbounds(r, I))

rand(r::MersenneTwister, I::FloatInterval) = rand_generic(r, I)

#### integers

rand(r::MersenneTwister,
             ::Type{T}) where {T<:Union{Bool,Int8,UInt8,Int16,UInt16,Int32,UInt32}} =
                 rand_ui52_raw(r) % T

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

rand(r::MersenneTwister, ::Type{Int64})  = reinterpret(Int64,  rand(r, UInt64))
rand(r::MersenneTwister, ::Type{Int128}) = reinterpret(Int128, rand(r, UInt128))

#### arrays of floats

function rand_AbstractArray_Float64!(r::MersenneTwister, A::AbstractArray{Float64},
                                     n=length(A), I::FloatInterval_64=CloseOpen())
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

fill_array!(s::DSFMT_state, A::Ptr{Float64}, n::Int, ::CloseOpen_64) =
    dsfmt_fill_array_close_open!(s, A, n)

fill_array!(s::DSFMT_state, A::Ptr{Float64}, n::Int, ::Close1Open2_64) =
    dsfmt_fill_array_close1_open2!(s, A, n)

function rand!(r::MersenneTwister, A::Array{Float64}, n::Int=length(A),
               I::FloatInterval_64=CloseOpen())
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

mask128(u::UInt128, ::Type{Float16}) =
    (u & 0x03ff03ff03ff03ff03ff03ff03ff03ff) | 0x3c003c003c003c003c003c003c003c00

mask128(u::UInt128, ::Type{Float32}) =
    (u & 0x007fffff007fffff007fffff007fffff) | 0x3f8000003f8000003f8000003f800000

function rand!(r::MersenneTwister, A::Union{Array{Float16},Array{Float32}},
               ::Close1Open2_64)
    T = eltype(A)
    n = length(A)
    n128 = n * sizeof(T) ÷ 16
    rand!(r, unsafe_wrap(Array, convert(Ptr{Float64}, pointer(A)), 2*n128),
          2*n128, Close1Open2())
    A128 = unsafe_wrap(Array, convert(Ptr{UInt128}, pointer(A)), n128)
    @inbounds for i in 1:n128
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
        A128[i] = mask128(u, T)
    end
    for i in 16*n128÷sizeof(T)+1:n
        @inbounds A[i] = rand(r, T) + oneunit(T)
    end
    A
end

function rand!(r::MersenneTwister, A::Union{Array{Float16},Array{Float32}}, ::CloseOpen_64)
    rand!(r, A, Close1Open2())
    I32 = one(Float32)
    for i in eachindex(A)
        @inbounds A[i] = Float32(A[i])-I32 # faster than "A[i] -= one(T)" for T==Float16
    end
    A
end

rand!(r::MersenneTwister, A::Union{Array{Float16},Array{Float32}}) =
    rand!(r, A, CloseOpen())

#### arrays of integers

function rand!(r::MersenneTwister, A::Array{UInt128}, n::Int=length(A))
    if n > length(A)
        throw(BoundsError(A,n))
    end
    Af = unsafe_wrap(Array, convert(Ptr{Float64}, pointer(A)), 2n)
    i = n
    while true
        rand!(r, Af, 2i, Close1Open2())
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
            @inbounds A[i] ⊻= u << (12*i)
        end
    end
    A
end

# A::Array{UInt128} will match the specialized method above
function rand!(r::MersenneTwister, A::Base.BitIntegerArray)
    n = length(A)
    T = eltype(A)
    n128 = n * sizeof(T) ÷ 16
    rand!(r, unsafe_wrap(Array, convert(Ptr{UInt128}, pointer(A)), n128))
    for i = 16*n128÷sizeof(T)+1:n
        @inbounds A[i] = rand(r, T)
    end
    A
end

#### from a range

function rand_lteq(r::AbstractRNG, randfun, u::U, mask::U) where U<:Integer
    while true
        x = randfun(r) & mask
        x <= u && return x
    end
end

function rand(rng::MersenneTwister, r::UnitRange{T}) where T<:Union{Base.BitInteger64,Bool}
    isempty(r) && throw(ArgumentError("range must be non-empty"))
    m = last(r) % UInt64 - first(r) % UInt64
    bw = (64 - leading_zeros(m)) % UInt # bit-width
    mask = (1 % UInt64 << bw) - (1 % UInt64)
    x = bw <= 52 ? rand_lteq(rng, rand_ui52_raw, m, mask) :
                   rand_lteq(rng, rng->rand(rng, UInt64), m, mask)
    (x + first(r) % UInt64) % T
end

function rand(rng::MersenneTwister, r::UnitRange{T}) where T<:Union{Int128,UInt128}
    isempty(r) && throw(ArgumentError("range must be non-empty"))
    m = (last(r)-first(r)) % UInt128
    bw = (128 - leading_zeros(m)) % UInt # bit-width
    mask = (1 % UInt128 << bw) - (1 % UInt128)
    x = bw <= 52  ? rand_lteq(rng, rand_ui52_raw, m % UInt64, mask % UInt64) % UInt128 :
        bw <= 104 ? rand_lteq(rng, rand_ui104_raw, m, mask) :
                    rand_lteq(rng, rng->rand(rng, UInt128), m, mask)
    x % T + first(r)
end


### randjump

"""
    randjump(r::MersenneTwister, jumps::Integer,
             [jumppoly::AbstractString=dSFMT.JPOLY1e21]) -> Vector{MersenneTwister}

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
        push!(mts, MersenneTwister(copy(cmt.seed), dSFMT.dsfmt_jump(cmt.state, jumppoly)))
    end
    return mts
end

randjump(r::MersenneTwister, jumps::Integer) = randjump(r, jumps, dSFMT.JPOLY1e21)
