libdsfmt = dlopen("libdSFMT")

DSFMT_MEXP = int32(19937)
DSFMT_STATE = Array(Int32, 1000)

DSFMT_POOL_SIZE = 4096
DSFMT_POOL = Array(Float64, DSFMT_POOL_SIZE)
DSFMT_POOL_PTR = DSFMT_POOL_SIZE

dsfmt_init() = ccall(dlsym(libdsfmt, :dsfmt_chk_init_gen_rand),
                     Void, (Ptr{Void}, Uint32, Int32),
                     DSFMT_STATE, uint32(0), DSFMT_MEXP)

dsfmt_fill_array_open_open(A::Array{Float64}, n::Size) =
    ccall(dlsym(libdsfmt, :dsfmt_fill_array_open_open),
          Void, (Ptr{Void}, Ptr{Float64}, Int32),
          DSFMT_STATE, A, n)

function dsfmt_rand()
    global DSFMT_POOL_PTR
    global DSFMT_POOL_SIZE

    if DSFMT_POOL_PTR < DSFMT_POOL_SIZE
        DSFMT_POOL_PTR += 1
        return DSFMT_POOL[DSFMT_POOL_PTR]
    else
        dsfmt_fill_array_open_open(DSFMT_POOL, DSFMT_POOL_SIZE)
        DSFMT_POOL_PTR = 1
        return DSFMT_POOL[1]
    end
end

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

# Floating point random numbers
rand()     = ccall(:rand_double,   Float64, ())
randf()    = ccall(:rand_float,    Float32, ())
randui32() = ccall(:genrand_int32, Uint32,  ())
randn()    = ccall(:randn,         Float64, ())
srand(s::Union(Int32,Uint32)) = ccall(:randomseed32, Void, (Uint32,), uint32(s))
srand(s::Union(Int64,Uint64)) = ccall(:randomseed64, Void, (Uint64,), uint64(s))

# Arrays of random numbers
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

@rand_matrix_builder Float64 rand
@rand_matrix_builder Float32 randf
@rand_matrix_builder Float64 randn
@rand_matrix_builder Uint32 randui32
