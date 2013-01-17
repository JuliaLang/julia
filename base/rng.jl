module RNG

using Base.LibRandom

export librandom_init, srand,
       rand, rand!,
       randn, randn!,
       randi, randi!, randival, randival!,
       randg, randg!,
       randexp, randexp!,
       randchi2, randchi2!,
       randbeta, randbeta!,
       randbool, randbool!,
       AbstractRNG, MersenneTwister

abstract AbstractRNG

const RNG = MersenneTwister

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
            seed = bitmix(seed, parse_int(Uint64, readall(`ifconfig`|`sha1sum`)[1:40], 16))
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
rand(dims::Int...) = rand(dims)

rand!(r::MersenneTwister, A::Array{Float64}) = dsfmt_fill_array_close_open!(r.state, A)
rand(r::AbstractRNG, dims::Dims) = rand!(r, Array(Float64, dims))
rand(r::AbstractRNG, dims::Int...) = rand(r, dims)

## random integers

dsfmt_randui32() = dsfmt_gv_genrand_uint32()
dsfmt_randui64() = uint64(dsfmt_randui32()) | (uint64(dsfmt_randui32())<<32)

randi(::Type{Uint32})  = dsfmt_randui32()
randi(::Type{Uint64})  = dsfmt_randui64()
randi(::Type{Uint128}) = uint128(randi(Uint64))<<64 | randi(Uint64)

randi(::Type{Int32})   = int32(randi(Uint32)) & typemax(Int32)
randi(::Type{Int64})   = int64(randi(Uint64)) & typemax(Int64)
randi(::Type{Int128})  = int128(randi(Uint128)) & typemax(Int128)

randi() = randi(Int)

# random integer from lo to hi inclusive
function randival{T<:Integer}(lo::T, hi::T)
    if lo > hi
        error("randi: invalid range")
    end
    m = typemax(T)
    s = randi(T)
    if (hi-lo == m)
        return s + lo
    end
    r = hi-lo+1
    if (r&(r-1))==0
        # power of 2 range
        return s&(r-1) + lo
    end
    # note: m>=0 && r>=0
    lim = m - rem(rem(m,r)+1, r)
    while s > lim
        s = randi(T)
    end
    return rem(s,r) + lo
end

function randival!{T<:Integer}(lo, hi, A::Array{T})
    lo = convert(T,lo)
    hi = convert(T,hi)
    for i = 1:length(A)
        A[i] = randival(lo, hi)
    end
    return A
end
randival{T<:Integer}(lo::T, hi::T, dims::Dims) = randival!(lo, hi, Array(T, dims))
randival(lo, hi, dims::Dims)   = randival(promote(lo, hi)..., dims)
randival(lo, hi, dims::Int...) = randival(lo, hi, dims)

randi!(max::Integer, A::Array)         = randival!(one(max), max, A)
randi!(r::(Integer,Integer), A::Array) = randival!(r[1], r[2], A)

randi(max::Integer)                    = randival(one(max), max)
randi(max::Integer, dims::Dims)        = randival(one(max), max, dims)
randi(max::Integer, dims::Int...)      = randival(one(max), max, dims)
randi(r::(Integer,Integer))               = randival(r[1], r[2])
randi(r::(Integer,Integer), dims::Dims)   = randival(r[1], r[2], dims)
randi(r::(Integer,Integer), dims::Int...) = randival(r[1], r[2], dims)

## random Bools

rand!(B::BitArray) = Base.bitarray_rand_fill!(B)

randbool(dims::Dims) = rand!(BitArray(dims))
randbool(dims::Int...) = rand!(BitArray(dims))

randbool() = ((dsfmt_randui32() & 1) == 1)

randbool!(B::BitArray) = rand!(B)

function randbool!(A::Array)
    for i = 1:length(A)
        A[i] = randbool()
    end
    return A
end

## randn() - Normally distributed random numbers using Ziggurat algorithm

# The Ziggurat Method for generating random variables - Marsaglia and Tsang
# Paper and reference code: http://www.jstatsoft.org/v05/i08/ 

randn() = randmtzig_randn()
randn!(A::Array{Float64}) = randmtzig_fill_randn!(A)
randn(dims::Dims) = randn!(Array(Float64, dims))
randn(dims::Int...) = randn(dims)

## randexp() - Exponentially distributed random numbers using Ziggurat algorithm

randexp() = randmtzig_exprnd()
randexp!(A::Array{Float64}) = randmtzig_fill_exprnd!(A)
randexp(dims::Dims) = randexp!(Array(Float64, dims))
randexp(dims::Int...) = randexp(dims)

## randg()

# A simple method for generating gamma variables - Marsaglia and Tsang (2000)
# http://www.cparity.com/projects/AcmClassification/samples/358414.pdf
# Page 369

# basic simulation loop for pre-computed d and c
function randg2(d::Float64, c::Float64) 
    while true
        x = v = 0.0
        while v <= 0.0
            x = randn()
            v = 1.0 + c*x
        end
        v = v^3
        U = rand()
        x2 = x^2
        if U < 1.0-0.331*x2^2 || log(U) < 0.5*x2+d*(1.0-v+log(v))
            return d*v
        end
    end
end

function randg!(a::Real, A::Array{Float64})
    if a <= 0. error("shape parameter a must be > 0") end
    d = (a <= 1. ? a + 1 : a) - 1.0/3.0
    c = 1.0/sqrt(9.0d)
    for i in 1:length(A) A[i] = randg2(d, c) end
    if a <= 1.
        ainv = 1./a
        for i in 1:length(A) A[i] *= rand()^ainv end
    end
    A
end

function randg(a::Real)
    if a <= 0. error("shape parameter a must be > 0") end
    d = (a <= 1. ? a + 1 : a) - 1.0/3.0
    randg2(d, 1.0/sqrt(9.0d)) * (a > 1. ? 1. : rand()^(1./a))
end

randg(a::Real, dims::Dims) = randg!(a, Array(Float64, dims))
randg(a::Real, dims::Int...) = randg(a, dims)

## randchi2 - the distribution chi^2(df) is 2*gamma(df/2)
## for integer n, a chi^2(n) is the sum of n squared standard normals

function randchi2!(df::Real, A::Array{Float64})
    if df == 1
        for i in 1:length(A)
            A[i] = randn()^2
            end
        return A
    end
    d = df >= 2 ? df/2. - 1.0/3.0 : error("require degrees of freedom df >= 2")
    c = 1.0/sqrt(9.0d)
    for i in 1:length(A) A[i] = 2.randg2(d,c) end
    A
end

randchi2(df::Real) = df == 1 ? randn()^2 : 2.randg(df/2.)
randchi2(df::Real, dims::Dims) = randchi2!(df, Array(Float64, dims))
randchi2(df::Real, dims::Int...) = randchi2(df, dims)

randbeta(alpha::Real, beta::Real) = (u = randg(alpha); u / (u + randg(beta)))
randbeta(alpha::Real, beta::Real, dims::Dims) = (u = randg(alpha, dims); u ./ (u + randg(beta, dims)))
randbeta(alpha::Real, beta::Real, dims::Int...) = randbeta(alpha, beta, dims)

end # module
