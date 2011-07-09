libmt = dlopen("libMT")

function mt_init()
    try
        srand("/dev/urandom", 4)
    catch
        srand(uint64(clock()*2.0^32))
    end
end

dsfmt_get_min_array_size() = ccall(dlsym(libmt, :dsfmt_get_min_array_size), Int32, ())

dsfmt_randn_reset() = ccall(dlsym(libmt, :dsfmt_randn_reset), Void, ())

srand(seed::Uint32) = (ccall(dlsym(libmt, :dsfmt_gv_init_gen_rand), Void, (Uint32, ), seed);
                       dsfmt_randn_reset())

srand(seed::Uint64) = (srand([uint32(seed),uint32(seed>>32)]);
                       dsfmt_randn_reset())

function srand(seed::DenseVector{Uint32})
    ccall(dlsym(libmt, :dsfmt_gv_init_by_array),
          Void, (Ptr{Uint32}, Int32),
          seed, length(seed))
    dsfmt_randn_reset()
end

randf() = float32(rand())

rand() = ccall(dlsym(libmt, :dsfmt_gv_genrand_open_open), Float64, ())

randui32() = ccall(dlsym(libmt, :dsfmt_gv_genrand_uint32), Uint32, ())

randn() = ccall(dlsym(libmt, :dsfmt_randn), Float64, ())

function dsfmt_fill_array_open_open(A::Array{Float64})
    n = numel(A)
    if (n <= dsfmt_get_min_array_size())
        for i=1:numel(A)
            A[i] = rand()
        end
    else
        if isodd(n)
            ccall(dlsym(libmt, :dsfmt_gv_fill_array_open_open), Void, (Ptr{Void}, Int32), A, n-1)
            A[n] = rand()
        else
            ccall(dlsym(libmt, :dsfmt_gv_fill_array_open_open), Void, (Ptr{Void}, Int32), A, n)
        end
    end
    return A
end

## Seed from a file

function srand(fname::String, n::Int32)
    fid = open(fname)
    a = Array(Uint32, n)
    read(fid, a)
    srand(a)
    close(fid)
end

## Random integers

randui64() = boxui64(or_int(zext64(unbox32(randui32())),
                            shl_int(zext64(unbox32(randui32())),unbox32(32))))

randint(::Type{Int32}) = int32(randui32())&typemax(Int32)
randint(::Type{Uint32}) = randui32()
randint(::Type{Int64}) = int64(randui64())&typemax(Int64)
randint(::Type{Uint64}) = randui64()
randint() = randint(Int32)

# random integer from lo to hi inclusive
function randint{T<:Int}(lo::T, hi::T)
    m = typemax(T)
    s = randint(T)
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
        s = randint(T)
    end
    return rem(s,r) + lo
end

# random integer from 1 to n
randint(n::Int) = randint(one(n), n)

## Arrays of random numbers

function rand(dims::Dims)
    A = Array(Float64, dims)
    dsfmt_fill_array_open_open(A)
    return A
end

rand(dims::Size...) = rand(dims)

macro rand_matrix_builder(t, f)
    quote

        function ($f)(dims::Dims)
            A = Array($t, dims)
            for i = 1:numel(A)
                A[i] = ($f)()
            end
            return A
        end

        ($f)(dims::Size...) = ($f)(dims)

    end # quote
end # macro

@rand_matrix_builder Float64 randn
@rand_matrix_builder Float32 randf
@rand_matrix_builder Uint32 randui32
@rand_matrix_builder Uint64 randui64
