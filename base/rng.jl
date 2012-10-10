module RNG

import Base.*
import Base.LibRandom.*

export librandom_init, srand,
       rand, rand!,
       randn, randn!,
       randi, randi!, randival, randival!,
       randg, randg!,
       randexp, randexp!, exprnd,
       randchi2, randchi2!, chi2rnd,
       randbeta, randbeta!, betarnd,
       randbit, randbit!, randbool, randbool!,
       Rng, Rng_MT

abstract Rng

start(r::Rng) = 0
done(r::Rng, count) = r.len == count ? true : false
next(r::Rng, count) = (rand(r), count + 1)

type Rng_MT <: Rng
    state::DSFMT_state
    seed::Union(Uint32,Vector{Uint32})
    len::Int   # Use for iteration. Set to -1 otherwise.

    function Rng_MT()
        seed = uint32(0)
        state = DSFMT_state()
        dsfmt_init_gen_rand(state, seed)
        return new(state, seed, -1)
    end

    Rng_MT(seed) = Rng_MT(seed, -1)

    function Rng_MT(seed::Uint32, len::Int)
        state = DSFMT_state()
        dsfmt_init_gen_rand(state, seed)
        return new(state, seed, len)
    end

    function Rng_MT(seed::Vector{Uint32}, len::Int)
        state = DSFMT_state()
        dsfmt_init_by_array(state, seed)
        return new(state, seed, len)
    end
end

function srand(r::Rng_MT, seed) 
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
        println(stderr, "Entropy pool not available to seed RNG, using ad-hoc entropy sources.")
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

# macros to generate random arrays

macro rand_matrix_builder(T, f)
    f! = esc(symbol("$(f)!"))
    f = esc(f)
    quote
        function ($f!)(A::Array{$T})
            for i = 1:numel(A)
                A[i] = ($f)()
            end
            return A
        end
        ($f)(dims::Dims) = ($f!)(Array($T, dims))
        ($f)(dims::Int...) = ($f)(dims)
    end
end

macro rand_matrix_builder_1arg(T, f)
    f! = esc(symbol("$(f)!"))
    f = esc(f)
    quote
        function ($f!)(arg, A::Array{$T})
            for i = 1:numel(A)
                A[i] = ($f)(arg)
            end
            return A
        end
        ($f)(arg::Number, dims::Dims) = ($f!)(arg, Array($T, dims))
        ($f)(arg::Number, dims::Int...) = ($f)(arg, dims)
    end
end

## srand()

function srand(seed::Uint32)
    global RANDOM_SEED = seed
    dsfmt_gv_init_gen_rand(seed)
end

function srand(seed::Vector{Uint32})
    global RANDOM_SEED = seed
    dsfmt_gv_init_by_array(seed)
end

srand(seed::Uint64) = srand([uint32(seed),uint32(seed>>32)])
srand(seed::Int32) = srand(uint32(seed))
srand(seed::Int64) = srand(uint64(seed))

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
rand(r::Rng_MT) = dsfmt_genrand_close_open(r.state)

rand!(A::Array{Float64}) = dsfmt_gv_fill_array_close_open!(A)
rand(dims::Dims) = rand!(Array(Float64, dims))
rand(dims::Int...) = rand(dims)

rand!(r::Rng_MT, A::Array{Float64}) = dsfmt_fill_array_close_open!(r.state, A)
rand(r::Rng, dims::Dims) = rand!(r, Array(Float64, dims))
rand(r::Rng, dims::Int...) = rand(r, dims)

## random integers

dsfmt_randui32() = dsfmt_gv_genrand_uint32()

dsfmt_randui64() =
    box(Uint64,or_int(zext64(unbox(Uint32,dsfmt_randui32())),
                      shl_int(zext64(unbox(Uint32,dsfmt_randui32())), 32)))

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
    for i = 1:numel(A)
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

randbit() = int(dsfmt_randui32() & 1)
@rand_matrix_builder Int randbit

randbool() = randbit() == 1
@rand_matrix_builder Bool randbool

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

const exprnd = randexp

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
    d = (a < 1. ? a + 1 : a) - 1.0/3.0
    c = 1.0/sqrt(9.0d)
    for i in 1:numel(A) A[i] = randg2(d, c) end
    if a < 1.
        ainv = 1./a
        for i in 1:numel(A) A[i] *= rand()^ainv end
    end
    A
end

function randg(a::Real)
    if a <= 0. error("shape parameter a must be > 0") end
    d = (a < 1. ? a + 1 : a) - 1.0/3.0
    randg2(d, 1.0/sqrt(9.0d)) * (a > 1. ? 1. : rand()^(1./a))
end

randg(a::Real, dims::Dims) = randg!(a, Array(Float64, dims))
randg(a::Real, dims::Int...) = randg(a, dims)

## randchi2 - the distribution chi^2(df) is 2*gamma(df/2)
## for integer n, a chi^2(n) is the sum of n squared standard normals

function randchi2!(df::Real, A::Array{Float64})
    if df == 1
        for i in 1:numel(A)
            A[i] = randn()^2
            end
        return A
    end
    d = df >= 2 ? df/2. - 1.0/3.0 : error("require degrees of freedom df >= 2")
    c = 1.0/sqrt(9.0d)
    for i in 1:numel(A) A[i] = 2.randg2(d,c) end
    A
end

randchi2(df::Real) = df == 1 ? randn()^2 : 2.randg(df/2.)
randchi2(df::Real, dims::Dims) = randchi2!(df, Array(Float64, dims))
randchi2(df::Real, dims::Int...) = randchi2(df, dims)

const chi2rnd = randchi2 # alias chi2rnd

function randbeta!(alpha::Real, beta::Real, A::Array{Float64})
    d1 = alpha >= 1 ? alpha - 1.0/3.0 : error("require alpha >= 1")
    c1 = 1.0/sqrt(9.0d1)
    d2 = beta >= 1 ? beta - 1.0/3.0 : error("require beta >= 1")
    c2 = 1.0/sqrt(9.0d2)
    for i in 1:numel(A)
       u = randg2(d1,c1)
       A[i] = u/(u + randg2(d2,c2))
    end
    A
end

randbeta(alpha::Real, beta::Real) = (u=randg(alpha); u/(u + randg(beta)))

function randbeta(alpha::Real, beta::Real, dims::Dims)
    randbeta!(alpha, beta, Array(Float64, dims))
end

randbeta(alpha::Real, beta::Real, dims::Int...) = randbeta(alpha, beta, dims)

const betarnd = randbeta

end # module
