_jl_librandom = dlopen("librandom")

## initialization

function _jl_librandom_init()
    try
        srand("/dev/urandom")
    catch
        println("Entropy pool not available to seed RNG, using ad-hoc entropy sources.")
        seed = reinterpret(Uint64, clock())
        seed = bitmix(seed, parse_int(Uint64, readall(`ifconfig`|`sha1sum`)[1:40], 16))
        seed = bitmix(seed, uint64(getpid()))
        srand(seed)
    end
    _jl_randn_zig_init()
end

_jl_dsfmt_get_min_array_size() = ccall(dlsym(_jl_librandom, :dsfmt_get_min_array_size), Int32, ())

# macros to generate random arrays

macro _jl_rand_matrix_builder(t, f)
    quote
        function ($f)(dims::Dims)
            A = Array($t, dims)
            for i = 1:numel(A); A[i] = ($f)(); end
            return A
        end
        ($f)(dims::Size...) = ($f)(dims)
    end
end

macro _jl_rand_matrix_builder_1arg(t, f)
    quote
        function ($f)(arg, dims::Dims)
            A = Array($t, dims)
            for i = 1:numel(A); A[i] = ($f)(arg); end
            return A
        end
        ($f)(arg, dims::Size...) = ($f)(arg, dims)
    end
end

## srand()

function srand(seed::Uint32)
    global RANDOM_SEED = seed
    ccall(dlsym(_jl_librandom, :dsfmt_gv_init_gen_rand),
          Void, (Uint32,), uint32(seed))
end

function srand(seed::Vector{Uint32})
    global RANDOM_SEED = seed
    ccall(dlsym(_jl_librandom, :dsfmt_gv_init_by_array),
          Void, (Ptr{Uint32}, Int32), seed, int32(length(seed)))
end

srand(seed::Uint64) = srand([uint32(seed),uint32(seed>>32)])
srand(seed::Int32) = srand(uint32(seed))
srand(seed::Int64) = srand(uint64(seed))

function srand(filename::String, n::Int)
    fd = open(filename)
    a = Array(Uint32, long(n))
    read(fd, a)
    srand(a)
    close(fd)
end

srand(filename::String) = srand(filename, 4)

## rand()

rand() = ccall(dlsym(_jl_librandom, :dsfmt_gv_genrand_open_open), Float64, ())

function rand(dims::Dims)
    A = Array(Float64, dims)
    _jl_dsfmt_fill_array_open_open(A)
    return A
end

rand(dims::Size...) = rand(dims)

function _jl_dsfmt_fill_array_open_open(A::Array{Float64})
    n = numel(A)
    if (n <= _jl_dsfmt_get_min_array_size())
        for i=1:numel(A)
            A[i] = rand()
        end
    else
        if isodd(n)
            ccall(dlsym(_jl_librandom, :dsfmt_gv_fill_array_open_open), Void, (Ptr{Void}, Int32), A, int32(n-1))
            A[n] = rand()
        else
            ccall(dlsym(_jl_librandom, :dsfmt_gv_fill_array_open_open), Void, (Ptr{Void}, Int32), A, int32(n))
        end
    end
    return A
end

## random integers

_jl_dsfmt_randui32() = ccall(dlsym(_jl_librandom, :dsfmt_gv_genrand_uint32), Uint32, ())

_jl_dsfmt_randui64() = boxui64(or_int(zext64(unbox32(_jl_dsfmt_randui32())),
                           shl_int(zext64(unbox32(_jl_dsfmt_randui32())),unbox32(32))))

if WORD_SIZE == 64
    randi() = randi(Uint64)
else
    randi() = randi(Uint32)
end

randi(::Type{Int32})  = int32(_jl_dsfmt_randui32()) & typemax(Int32)
randi(::Type{Uint32}) = _jl_dsfmt_randui32()
randi(::Type{Int64})  = int64(_jl_dsfmt_randui64()) & typemax(Int64)
randi(::Type{Uint64}) = _jl_dsfmt_randui64()

# random integer from lo to hi inclusive
function randi_interval{T<:Int}(lo::T, hi::T)
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

function randi_interval{T<:Int}(lo::T, hi::T, dims::Dims)
    A = Array(T, dims)
    for i = 1:numel(A); A[i] = randi_interval(lo, hi); end
    return A
end
randi_interval(lo, hi, dims::Size...) = randi_interval(lo, hi, dims)

randi(max::Int)                    = randi_interval(one(max), max)
randi(max::Int, dims::Dims)        = randi_interval(one(max), max, dims)
randi(max::Int, dims::Size...)     = randi_interval(one(max), max, dims)
randi(r::(Int,Int))                = randi_interval(r[1], r[2])
randi(r::(Int,Int), dims::Dims)    = randi_interval(r[1], r[2], dims)
randi(r::(Int,Int), dims::Size...) = randi_interval(r[1], r[2], dims)

## random Bools

randbit() = _jl_dsfmt_randui32() & uint32(1)
@_jl_rand_matrix_builder Uint32 randbit

randbool() = randbit() == 1
@_jl_rand_matrix_builder Bool randbool

## randn() - Normally distributed random numbers using Ziggurat algorithm

# The Ziggurat Method for generating random variables - Marsaglia and Tsang
# Paper and reference code: http://www.jstatsoft.org/v05/i08/ 

_jl_randn_zig_init() = ccall(dlsym(_jl_librandom, :randmtzig_create_ziggurat_tables), Void, ())

randn() = ccall(dlsym(_jl_librandom, :randmtzig_randn), Float64, ())

function randn(dims::Dims)
    A = Array(Float64, dims)
    ccall(dlsym(_jl_librandom, :randmtzig_fill_randn), Void,
          (Ptr{Float64}, Uint32), 
          A, uint32(numel(A)))
    return A
end

randn(dims::Size...) = randn(dims)

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
