_jl_librandom = dlopen("librandom")

## initialization

function _jl_librandom_init()
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
    fd = open(filename)
    a = Array(Uint32, int(n))
    read(fd, a)
    srand(a)
    close(fd)
end

srand(filename::String) = srand(filename, 4)

## rand()

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

rand() = ccall(dlsym(_jl_librandom, :dsfmt_gv_genrand_close_open), Float64, ())
rand(dims::Dims) = rand!(Array(Float64, dims))
rand(dims::Int...) = rand(dims)

## random integers

_jl_dsfmt_randui32() =
    ccall(dlsym(_jl_librandom, :dsfmt_gv_genrand_uint32), Uint32, ())

_jl_dsfmt_randui64() =
    boxui64(or_int(zext64(unbox32(_jl_dsfmt_randui32())),
                          shl_int(zext64(unbox32(_jl_dsfmt_randui32())), 32)))

randi(::Type{Int32})  = int32(_jl_dsfmt_randui32()) & typemax(Int32)
randi(::Type{Uint32}) = _jl_dsfmt_randui32()
randi(::Type{Int64})  = int64(_jl_dsfmt_randui64()) & typemax(Int64)
randi(::Type{Uint64}) = _jl_dsfmt_randui64()

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

# A simple method for generating gamma variables - Marsaglia and Tsang
# http://www.cparity.com/projects/AcmClassification/samples/358414.pdf
# Page 369

function randg(a::Real)
    d = a - 1.0/3.0
    c = 1.0 / sqrt(9*d)
    while(true)
        x = randn()
        v = 1.0 + c*x
        while (v <= 0.0)
            x = randn()
            v = 1.0 + c*x
        end
        v = v*v*v
        U = rand()
        x2 = x*x
        if U < 1.0 - 0.331*x2*x2; return d*v; end
        if log(U) < 0.5*x2 + d*(1.0 - v + log(v)); return d*v; end
    end
end
@_jl_rand_matrix_builder_1arg Float64 randg

# randchi2()

randchi2(v) = 2*randg(v/2)
@_jl_rand_matrix_builder_1arg Float64 randchi2

const chi2rnd = randchi2 # alias chi2rnd

# From John D. Cook
# http://www.johndcook.com/julia_rng.html
function randbeta(a, b)
    if a <= 0 || b <= 0
        error("Beta parameters must be positive")
    end
    
    ## There are more efficient methods for generating beta samples.
    ## However such methods are a little more efficient and much more complicated.
    ## For an explanation of why the following method works, see
    ## http://www.johndcook.com/distribution_chart.html#gamma_beta

    u = randg(a)
    v = randg(b)
    return u / (u + v)
end

const betarnd = randbeta
