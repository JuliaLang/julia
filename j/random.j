libmt = dlopen("libMT")

randomize() = ccall(dlsym(libmt, :randomize), Void, ())

function mt_init()
    randomize()
    dsfmt_init()
end

### DSFMT ###

dsfmt_init() = ccall(dlsym(libmt, :dsfmt_gv_init_gen_rand), Void, (Uint32, ), uint32(0))

dsfmt_get_min_array_size() = ccall(dlsym(libmt, :dsfmt_get_min_array_size), Int32, ())

dsfmt_genrand_open_open() = ccall(dlsym(libmt, :dsfmt_gv_genrand_open_open), Float64, ())

dsfmt_genrand_uint32() = ccall(dlsym(libmt, :dsfmt_gv_genrand_uint32), Uint32, ())

function dsfmt_fill_array_open_open(A::Array{Float64})
    n = numel(A)
    if (n <= dsfmt_get_min_array_size())
        for i=1:numel(A)
            A[i] = dsfmt_genrand_open_open()
        end
    else
        ccall(dlsym(libmt, :dsfmt_gv_fill_array_open_open), Void, (Ptr{Void}, Int32), A, n)
    end
    return A
end

### MT ###

rand()     = ccall(dlsym(libmt, :rand_double),   Float64, ())
randf()    = ccall(dlsym(libmt, :rand_float),    Float32, ())
randui32() = ccall(dlsym(libmt, :genrand_int32), Uint32,  ())
randn()    = ccall(dlsym(libmt, :randn),         Float64, ())
srand(s::Union(Int32,Uint32)) = ccall(dlsym(libmt, :randomseed32), Void, (Uint32,), uint32(s))
srand(s::Union(Int64,Uint64)) = ccall(dlsym(libmt, :randomseed64), Void, (Uint64,), uint64(s))

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
