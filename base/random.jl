module Random

using Base.LibRandom

export srand,
       rand, rand!,
       randn, randn!,
       randbool, randbool!,
       AbstractRNG, RNG, MersenneTwister

abstract AbstractRNG

type MersenneTwister <: AbstractRNG
    state::DSFMT_state
    seed::Union(Uint32,Vector{Uint32})

    function MersenneTwister(seed::Vector{Uint32})
        state = DSFMT_state()
        dsfmt_init_by_array(state, seed)
        return new(state, seed)
    end

    MersenneTwister(seed=0) = MersenneTwister(make_seed(seed))
end

function srand(r::MersenneTwister, seed) 
    r.seed = seed
    dsfmt_init_gen_rand(r.state, seed)
    return r
end

## initialization

function __init__()

@unix_only begin
    try
        srand("/dev/urandom")
    catch
        println(STDERR, "Entropy pool not available to seed RNG; using ad-hoc entropy sources.")
        seed = reinterpret(Uint64, time())
        seed = bitmix(seed, uint64(getpid()))
        try
            seed = bitmix(seed, parseint(Uint64, readall(`ifconfig` |> `sha1sum`)[1:40], 16))
        catch
            # ignore
        end
        srand(seed)
    end
end

@windows_only begin
    a = zeros(Uint32, 2)
    win32_SystemFunction036!(a)
    srand(a)
end
    randmtzig_create_ziggurat_tables()
end

## srand()

function srand(seed::Vector{Uint32})
    global RANDOM_SEED = seed
    dsfmt_gv_init_by_array(seed)
end
srand(n::Integer) = srand(make_seed(n))

function make_seed(n::Integer)
    n < 0 && throw(DomainError())
    seed = Uint32[]
    while true
        push!(seed, n & 0xffffffff)
        n >>= 32
        if n == 0
            return seed
        end
    end
end

function srand(filename::String, n::Integer)
    open(filename) do io
        a = Array(Uint32, int(n))
        read!(io, a)
        srand(a)
    end
end
srand(filename::String) = srand(filename, 4)

## random floating point values

rand(::Type{Float64}) = dsfmt_gv_genrand_close_open()
rand() = dsfmt_gv_genrand_close_open()

rand(::Type{Float32}) = float32(rand())
rand(::Type{Float16}) = float16(rand())

rand{T<:Real}(::Type{Complex{T}}) = complex(rand(T),rand(T))


rand(r::MersenneTwister) = dsfmt_genrand_close_open(r.state)

## random integers

dsfmt_randui32() = dsfmt_gv_genrand_uint32()
dsfmt_randui64() = uint64(dsfmt_randui32()) | (uint64(dsfmt_randui32())<<32)

rand(::Type{Uint8})   = uint8(rand(Uint32))
rand(::Type{Uint16})  = uint16(rand(Uint32))
rand(::Type{Uint32})  = dsfmt_randui32()
rand(::Type{Uint64})  = dsfmt_randui64()
rand(::Type{Uint128}) = uint128(rand(Uint64))<<64 | rand(Uint64)

rand(::Type{Int8})    = int8(rand(Uint8))
rand(::Type{Int16})   = int16(rand(Uint16))
rand(::Type{Int32})   = int32(rand(Uint32))
rand(::Type{Int64})   = int64(rand(Uint64))
rand(::Type{Int128})  = int128(rand(Uint128))

# Arrays of random numbers

rand!(A::Array{Float64}) = dsfmt_gv_fill_array_close_open!(A)
rand(::Type{Float64}, dims::Dims) = rand!(Array(Float64, dims))
rand(::Type{Float64}, dims::Int...) = rand(Float64, dims)

rand(dims::Dims) = rand(Float64, dims)
rand(dims::Int...) = rand(Float64, dims)

rand!(r::MersenneTwister, A::Array{Float64}) = dsfmt_fill_array_close_open!(r.state, A)
rand(r::AbstractRNG, dims::Dims) = rand!(r, Array(Float64, dims))
rand(r::AbstractRNG, dims::Int...) = rand(r, dims)

function rand!{T}(A::Array{T})
    for i=1:length(A)
        A[i] = rand(T)
    end
    A
end
rand(T::Type, dims::Dims) = rand!(Array(T, dims))
rand{T<:Number}(::Type{T}) = error("no random number generator for type $T; try a more specific type")
rand{T<:Number}(::Type{T}, dims::Int...) = rand(T, dims)

# Generate random integer within a range

immutable RandIntGen{T<:Integer, U<:Unsigned}
    a::T   # first element of the range
    k::U   # range length
    u::U   # maximum multiple of k within the domain of U

    RandIntGen(a::T, k::U) = new(a, k, div(typemax(U),k)*k)
end

RandIntGen{T<:Unsigned}(r::UnitRange{T}) = RandIntGen{T,T}(first(r), convert(T, length(r)))

# specialized versions
for (T, U) in [(Uint8, Uint32), (Uint16, Uint32), (Int8, Uint32), (Int16, Uint32), 
               (Int32, Uint32), (Int64, Uint64), (Int128, Uint128), 
               (Bool, Uint32), (Char, Uint32)]

    @eval RandIntGen(r::UnitRange{$T}) = RandIntGen{$T, $U}(first(r), convert($U, length(r)))
end

function rand{T<:Integer,U<:Unsigned}(g::RandIntGen{T,U})
    x = rand(U)
    while x >= g.u
        x = rand(U)
    end
    convert(T, g.a + rem(x, g.k))
end

rand{T<:Union(Signed,Unsigned,Bool,Char)}(r::UnitRange{T}) = rand(RandIntGen(r))
rand{T<:Real}(r::Range{T}) = convert(T, first(r) + rand(0:(length(r)-1)) * step(r))

function rand!(g::RandIntGen, A::AbstractArray)
    for i = 1 : length(A)
        @inbounds A[i] = rand(g)
    end    
    return A
end

rand!{T<:Union(Signed,Unsigned,Bool,Char)}(r::UnitRange{T}, A::AbstractArray) = rand!(RandIntGen(r), A)

function rand!{T<:Real}(r::Range{T}, A::AbstractArray)
    g = RandIntGen(0:(length(r)-1))
    f = first(r)
    s = step(r)
    if s == 1
        for i = 1 : length(A)
            @inbounds A[i] = convert(T, f + rand(g))
        end
    else
        for i = 1 : length(A)
            @inbounds A[i] = convert(T, f + rand(g) * s)
        end
    end
    return A
end

rand{T<:Real}(r::Range{T}, dims::Dims) = rand!(r, Array(T, dims))
rand(r::Range, dims::Int...) = rand(r, dims)


## random Bools

rand!(B::BitArray) = Base.bitarray_rand_fill!(B)

randbool(dims::Dims) = rand!(BitArray(dims))
randbool(dims::Int...) = rand!(BitArray(dims))

randbool() = ((dsfmt_randui32() & 1) == 1)
randbool!(B::BitArray) = rand!(B)

## randn() - Normally distributed random numbers using Ziggurat algorithm

# The Ziggurat Method for generating random variables - Marsaglia and Tsang
# Paper and reference code: http://www.jstatsoft.org/v05/i08/ 

randn() = randmtzig_gv_randn()
randn(rng::MersenneTwister) = randmtzig_randn(rng.state)
randn!(A::Array{Float64}) = randmtzig_gv_fill_randn!(A)
randn!(rng::MersenneTwister, A::Array{Float64}) = randmtzig_fill_randn!(rng.state, A)
randn(dims::Dims) = randn!(Array(Float64, dims))
randn(dims::Int...) = randn!(Array(Float64, dims...))

## random UUID generation

immutable UUID
    value::Uint128
end

function uuid4()
    u = rand(Uint128)
    u &= 0xffffffffffff0fff3fffffffffffffff
    u |= 0x00000000000040008000000000000000
    UUID(u)
end

function Base.convert(::Type{Vector{Uint8}}, u::UUID)
    u = u.value
    a = Array(Uint8,36)
    for i = [36:-1:25; 23:-1:20; 18:-1:15; 13:-1:10; 8:-1:1]
        d = u & 0xf
        a[i] = '0'+d+39*(d>9)
        u >>= 4
    end
    a[[24,19,14,9]] = '-'
    return a
end

Base.show(io::IO, u::UUID) = write(io,convert(Vector{Uint8},u))
Base.repr(u::UUID) = ASCIIString(convert(Vector{Uint8},u))

end # module
