librandom = dlopen("librandom")

## initialization

function librandom_init()
    try
        srand("/dev/urandom", 4)
    catch
        println("Entropy pool not available to seed RNG, using ad-hoc entropy sources.")
        seed = bswap(uint64(clock()*2.0^32)) $
               parse_int(Uint64, readall(`ifconfig`|`sha1sum`)[1:40], 16) $
               uint64(getpid())
        srand(seed)
    end
    randn_zig_init()
end

dsfmt_get_min_array_size() = ccall(dlsym(librandom, :dsfmt_get_min_array_size), Int32, ())

# macros to generate random arrays

macro rand_matrix_builder(t, f)
    quote
        function ($f)(dims::Dims)
            A = Array($t, dims)
            for i = 1:numel(A); A[i] = ($f)(); end
            return A
        end
    end
end

macro rand_matrix_builder_1arg(t, f)
    quote
        function ($f)(arg, dims::Dims)
            A = Array($t, dims)
            for i = 1:numel(A); A[i] = ($f)(arg); end
            return A
        end
        ($f)(arg, dims::Size...) = ($f)(arg, dims)
    end
end

macro rand_matrix_builder_2arg(t, f)
    quote
        function ($f)(arg1, arg2, dims::Dims)
            A = Array($t, dims)
            for i = 1:numel(A); A[i] = ($f)(arg1, arg2); end
            return A
        end
        ($f)(arg1, arg2, dims::Size...) = ($f)(arg1, arg2, dims)
    end
end

## srand()

srand(seed::Uint32) = ccall(dlsym(librandom, :dsfmt_gv_init_gen_rand), Void, (Uint32, ), seed)

function srand(seed::Vector{Uint32})
    ccall(dlsym(librandom, :dsfmt_gv_init_by_array),
          Void, (Ptr{Uint32}, Int32),
          seed, int32(length(seed)))
end

srand(seed::Uint64) = srand([uint32(seed),uint32(seed>>32)])

function srand(filename::String, n::Int)
    fd = open(filename)
    a = Array(Uint32, long(n))
    read(fd, a)
    srand(a)
    close(fd)
end

srand(filename::String) = srand(filename, 4)

## rand()

rand() = ccall(dlsym(librandom, :dsfmt_gv_genrand_open_open), Float64, ())

function rand(dims::Dims)
    A = Array(Float64, dims)
    dsfmt_fill_array_open_open(A)
    return A
end

rand(dims::Size...) = rand(dims)

function dsfmt_fill_array_open_open(A::Array{Float64})
    n = numel(A)
    if (n <= dsfmt_get_min_array_size())
        for i=1:numel(A)
            A[i] = rand()
        end
    else
        if isodd(n)
            ccall(dlsym(librandom, :dsfmt_gv_fill_array_open_open), Void, (Ptr{Void}, Int32), A, int32(n-1))
            A[n] = rand()
        else
            ccall(dlsym(librandom, :dsfmt_gv_fill_array_open_open), Void, (Ptr{Void}, Int32), A, int32(n))
        end
    end
    return A
end

## random integers

dsfmt_randui32() = ccall(dlsym(librandom, :dsfmt_gv_genrand_uint32), Uint32, ())

dsfmt_randui64() = boxui64(or_int(zext64(unbox32(dsfmt_randui32())),
                           shl_int(zext64(unbox32(dsfmt_randui32())),unbox32(32))))

if WORD_SIZE == 64
    randi() = randi(Uint64)
    @rand_matrix_builder Uint64 randi
else
    randi() = randi(Uint32)
    @rand_matrix_builder Uint32 randi
end

randi(::Type{Int32})  = int32(dsfmt_randui32()) & typemax(Int32)
@rand_matrix_builder_1arg Int32 randi

randi(::Type{Uint32}) = dsfmt_randui32()
@rand_matrix_builder_1arg Uint32 randi

randi(::Type{Int64})  = int64(dsfmt_randui64()) & typemax(Int64)
@rand_matrix_builder_1arg Int64 randi

randi(::Type{Uint64}) = dsfmt_randui64()
@rand_matrix_builder_1arg Uint64 randi

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

@rand_matrix_builder_2arg Int randi_interval

# random integer from 1 to n
randi_max(n::Int) = randi_interval(one(n), n)
@rand_matrix_builder_1arg Int randi_max

## random Bools

randbit() = dsfmt_randui32() & uint32(1)
@rand_matrix_builder Uint32 randbit

randbool() = randbit() == 1
@rand_matrix_builder Bool randbool

## randn() - Normally distributed random numbers using Ziggurat algorithm

randn_zig_init() = ccall(dlsym(librandom, :randmtzig_create_ziggurat_tables), Void, ())

randn() = ccall(dlsym(librandom, :randmtzig_randn), Float64, ())

function randn(dims::Dims)
    A = Array(Float64, dims)
    ccall(dlsym(librandom, :randmtzig_fill_randn), Void,
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

@rand_matrix_builder_1arg Float64 randg

# chi2rnd

chi2rnd(v) = 2*randg(v/2)
@rand_matrix_builder_1arg Float64 chi2rnd
