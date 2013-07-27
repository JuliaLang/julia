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

    function MersenneTwister()
        seed = uint32(0)
        state = DSFMT_state()
        dsfmt_init_gen_rand(state, seed)
        return new(state, seed)
    end

    function MersenneTwister(seed::Uint32)
        state = DSFMT_state()
        dsfmt_init_gen_rand(state, seed)
        return new(state, seed)
    end

    function MersenneTwister(seed::Vector{Uint32})
        state = DSFMT_state()
        dsfmt_init_by_array(state, seed)
        return new(state, seed)
    end

    MersenneTwister(seed) = MersenneTwister(reinterpret(Uint32, [seed]))
end

function srand(r::MersenneTwister, seed) 
    r.seed = seed
    dsfmt_init_gen_rand(r.state, seed)
    return r
end

## initialization

function librandom_init()

@unix_only begin
    try
        srand("/dev/urandom")
    catch
        println(STDERR, "Entropy pool not available to seed RNG, using ad-hoc entropy sources.")
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
        n2 = n >> 32
        if n2 == 0 || n2 == n
            return seed
        end
        n = n2
    end
end

function srand(filename::String, n::Integer)
    open(filename) do io
        a = Array(Uint32, int(n))
        read(io, a)
        srand(a)
    end
end
srand(filename::String) = srand(filename, 4)

## random floating point values

rand(::Type{Float64}) = dsfmt_gv_genrand_close_open()
rand() = dsfmt_gv_genrand_close_open()

rand(::Type{Float32}) = float32(rand())

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

for T in (:Uint8, :Uint16, :Uint32, :Uint64, :Uint128,
          :Int8,  :Int16,  :Int32,  :Int64,  :Int128,  
          :Float32)
    @eval begin
        function rand!(A::Array{$T})
            for i=1:length(A)
                A[i] = rand($T)
            end
            A
        end
        rand(::Type{$T}, dims::Dims) = rand!(Array($T, dims))
        rand(::Type{$T}, dims::Int...) = rand($T, dims)
    end
end

function randu{T<:Union(Uint32,Uint64,Uint128)}(k::T)
    # generate an unsigned integer in 0:k-1
    # largest multiple of k that can be represented in T
    u = convert(T, div(typemax(T),k)*k)
    x = rand(T)
    while x >= u
        x = rand(T)
    end
    rem(x, k)
end

# random integer from lo to hi inclusive
function rand{T<:Union(Uint32,Uint64,Uint128)}(r::Range1{T})
    ulen = convert(T, length(r))
    convert(T, first(r) + randu(ulen))
end

function rand(r::Range1{Int32})
    ulen = convert(Uint32, length(r))
    convert(Int32, first(r) + randu(ulen))
end
function rand(r::Range1{Int64})
    ulen = convert(Uint64, length(r))
    convert(Int64, first(r) + randu(ulen))
end
function rand(r::Range1{Int128})
    ulen = convert(Uint128, length(r))
    convert(Int128, first(r) + randu(ulen))
end


# fallback for other integer types
rand{T<:Integer}(r::Range1{T}) = convert(T, rand(int(r)))

function rand!{T<:Integer}(r::Range1{T}, A::Array{T})
    for i=1:length(A) 
        A[i] = rand(r)
    end
    return A
end

rand{T<:Integer}(r::Range1{T}, dims::Dims) = rand!(r, Array(T, dims))
rand{T<:Integer}(r::Range1{T}, dims::Int...) = rand(r, dims)

## random Bools

rand!(B::BitArray) = Base.bitarray_rand_fill!(B)

randbool(dims::Dims) = rand!(BitArray(dims))
randbool(dims::Int...) = rand!(BitArray(dims))

randbool() = ((dsfmt_randui32() & 1) == 1)
randbool!(B::BitArray) = rand!(B)

## randn() - Normally distributed random numbers using Ziggurat algorithm

# The Ziggurat Method for generating random variables - Marsaglia and Tsang
# Paper and reference code: http://www.jstatsoft.org/v05/i08/ 

randn() = randmtzig_randn()
randn!(A::Array{Float64}) = randmtzig_fill_randn!(A)
randn(dims::Dims) = randn!(Array(Float64, dims))
randn(dims::Int...) = randn!(Array(Float64, dims...))

immutable UUID
    value::Uint128
end

@eval function uuid4()
    u = rand(Uint128)
    u &= $(parseint(Uint128,"ffffffffffff0fff3fffffffffffffff",16))
    u |= $(parseint(Uint128,"00000000000040008000000000000000",16))
    UUID(u)
end

function Base.convert(::Type{Vector{Uint8}}, u::UUID)
    u = u.value
    a = Array(Uint8,36)
    for i = [36:-1:25; 23:-1:20; 18:-1:15; 13:-1:10; 8:-1:1]
        a[i] = Base.digit(u & 0xf)
        u >>= 4
    end
    a[[24,19,14,9]] = '-'
    return a
end

Base.show(io::IO, u::UUID) = write(io,convert(Vector{Uint8},u))
Base.repr(u::UUID) = ASCIIString(convert(Vector{Uint8},u))

end # module
