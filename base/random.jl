module Random

using Base.LibRandom

export librandom_init, srand,
       rand, rand!,
       randn, randn!,
       randbool, randbool!,
       AbstractRNG, RNG, MersenneTwister

abstract AbstractRNG

type MersenneTwister <: AbstractRNG
    state::DSFMT_state
    seed::Union(Uint32,Vector{Uint32})
    len::Int   # Use for iteration. Set to -1 otherwise.

    function MersenneTwister()
        seed = uint32(0)
        state = DSFMT_state()
        dsfmt_init_gen_rand(state, seed)
        return new(state, seed, -1)
    end

    MersenneTwister(seed) = MersenneTwister(seed, -1)

    function MersenneTwister(seed::Uint32, len::Int)
        state = DSFMT_state()
        dsfmt_init_gen_rand(state, seed)
        return new(state, seed, len)
    end

    function MersenneTwister(seed::Vector{Uint32}, len::Int)
        state = DSFMT_state()
        dsfmt_init_by_array(state, seed)
        return new(state, seed, len)
    end
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
            seed = bitmix(seed, parseint(Uint64, readall(`ifconfig`|`sha1sum`)[1:40], 16))
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

## rand()

rand() = dsfmt_gv_genrand_close_open()
rand(r::MersenneTwister) = dsfmt_genrand_close_open(r.state)

rand!(A::Array{Float64}) = dsfmt_gv_fill_array_close_open!(A)
rand(dims::Dims) = rand!(Array(Float64, dims))
rand(dims::Int...) = rand!(Array(Float64, dims...))

rand!(r::MersenneTwister, A::Array{Float64}) = dsfmt_fill_array_close_open!(r.state, A)
rand(r::AbstractRNG, dims::Dims) = rand!(r, Array(Float64, dims))
rand(r::AbstractRNG, dims::Int...) = rand(r, dims)

## random integers

dsfmt_randui32() = dsfmt_gv_genrand_uint32()
dsfmt_randui64() = uint64(dsfmt_randui32()) | (uint64(dsfmt_randui32())<<32)

rand(::Type{Uint32})  = dsfmt_randui32()
rand(::Type{Uint64})  = dsfmt_randui64()
rand(::Type{Uint128}) = uint128(rand(Uint64))<<64 | rand(Uint64)

rand(::Type{Int32})   = int32(rand(Uint32))
rand(::Type{Int64})   = int64(rand(Uint64))
rand(::Type{Int128})  = int128(rand(Uint128))

for itype in (:Uint32, :Uint64, :Uint128, :Int32, :Int64, :Int128) 
    @eval begin
        function rand!(A::Array{$itype})
            for i=1:length(A)
                A[i] = rand($itype)
            end
            A
        end
        rand(::Type{$itype}, dims::Dims) = rand!(Array($itype, dims))
        rand(::Type{$itype}, dims::Int...) = rand($itype, dims)
    end
end

# random integer from lo to hi inclusive
function rand{T<:Integer}(r::Range1{T})
    if !applicable(rand, T)
        # Fallback for integer types where rand(T) is not defined.
        return convert(T, rand(int(r)))
    end

    lo = r[1]
    hi = r[end]
    m = typemax(T)

    if hi - lo > m || hi - lo < 0
        # Fallback for signed integer types when the length of the range
        # is larger than typemax(T), e.g. rand(int32(-1):typemax(Int32)).
        return convert(T, rand(unsigned(zero(T)):unsigned(hi - lo)) + lo)
    end

    s = rand(T) & m
    if (hi-lo == m)
        return convert(T, s + lo)
    end
    r = hi-lo+1
    if (r&(r-1))==0
        # power of 2 range
        return convert(T, s&(r-1) + lo)
    end
    # note: m>=0 && r>=0
    lim = m - rem(rem(m,r)+1, r)
    while s > lim
        s = rand(T) & m
    end
    return convert(T, rem(s,r) + lo)
end

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

end # module
