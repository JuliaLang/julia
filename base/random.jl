_jl_librandom = dlopen("librandom")
@windows_only _jl_advapi32 = dlopen("Advapi32")

## initialization

function _jl_librandom_init()
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
    _jl_randn_zig_init()
end
@windows_only begin
    a=zeros(Uint32,2)
    ccall(dlsym(_jl_advapi32,:SystemFunction036),stdcall,Uint8,(Ptr{Void},Uint64),convert(Ptr{Void},a),8)
    srand(a)
end
end

# macros to generate random arrays

macro _jl_rand_matrix_builder(T, f)
    f! = symbol("$(f)!")
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

macro _jl_rand_matrix_builder_1arg(T, f)
    f! = symbol("$(f)!")
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
    ccall(dlsym(_jl_librandom, :dsfmt_gv_init_gen_rand),
          Void, (Uint32,), seed)
end

function srand(seed::Vector{Uint32})
    global RANDOM_SEED = seed
    ccall(dlsym(_jl_librandom, :dsfmt_gv_init_by_array),
          Void, (Ptr{Uint32}, Int32), seed, length(seed))
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

rand() = ccall(dlsym(_jl_librandom, :dsfmt_gv_genrand_close_open), Float64, ())

const _jl_dsfmt_get_min_array_size =
    ccall(dlsym(_jl_librandom, :dsfmt_get_min_array_size), Int32, ())

function rand!(A::Array{Float64})
    n = numel(A)
    if n <= _jl_dsfmt_get_min_array_size
        for i = 1:n
            A[i] = rand()
        end
    else
        ccall(dlsym(_jl_librandom, :dsfmt_gv_fill_array_close_open),
              Void, (Ptr{Void}, Int32), A, n & 0xfffffffe)
        if isodd(n)
            A[n] = rand()
        end
    end
    return A
end

rand(dims::Dims) = rand!(Array(Float64, dims))
rand(dims::Int...) = rand(dims)

## random integers

_jl_dsfmt_randui32() =
    ccall(dlsym(_jl_librandom, :dsfmt_gv_genrand_uint32), Uint32, ())

_jl_dsfmt_randui64() =
    box(Uint64,or_int(zext64(unbox(Uint32,_jl_dsfmt_randui32())),
                      shl_int(zext64(unbox(Uint32,_jl_dsfmt_randui32())), 32)))

randi(::Type{Uint32})  = _jl_dsfmt_randui32()
randi(::Type{Uint64})  = _jl_dsfmt_randui64()
randi(::Type{Uint128}) = uint128(randi(Uint64))<<64 | randi(Uint64)

randi(::Type{Int32})   = int32(randi(Uint32)) & typemax(Int32)
randi(::Type{Int64})   = int64(randi(Uint64)) & typemax(Int64)
randi(::Type{Int128})  = int128(randi(Uint128)) & typemax(Uint128)

randi() = randi(Int)

# random integer from lo to hi inclusive
function randival{T<:Integer}(lo::T, hi::T)
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

randbit() = int(_jl_dsfmt_randui32() & 1)
@_jl_rand_matrix_builder Int randbit

randbool() = randbit() == 1
@_jl_rand_matrix_builder Bool randbool

## randn() - Normally distributed random numbers using Ziggurat algorithm

# The Ziggurat Method for generating random variables - Marsaglia and Tsang
# Paper and reference code: http://www.jstatsoft.org/v05/i08/ 

_jl_randn_zig_init() =
    ccall(dlsym(_jl_librandom, :randmtzig_create_ziggurat_tables), Void, ())

function randn!(A::Array{Float64})
    ccall(dlsym(_jl_librandom, :randmtzig_fill_randn),
          Void, (Ptr{Float64}, Uint32), A, numel(A))
    return A
end

randn() = ccall(dlsym(_jl_librandom, :randmtzig_randn), Float64, ())
randn(dims::Dims) = randn!(Array(Float64, dims))
randn(dims::Int...) = randn(dims)

## randexp() - Exponentially distributed random numbers using Ziggurat algorithm

function randexp!(A::Array{Float64})
    ccall(dlsym(_jl_librandom, :randmtzig_fill_exprnd),
          Void, (Ptr{Float64}, Uint32), A, numel(A))
    return A
end

randexp() = ccall(dlsym(_jl_librandom, :randmtzig_exprnd), Float64, ())
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
