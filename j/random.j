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

rand()     = ccall(:rand_double,   Float64, ())
randf()    = ccall(:rand_float,    Float32, ())
randui32() = ccall(:genrand_int32, Uint32,  ())
randn()    = ccall(:randn,         Float64, ())
srand(s::Union(Int32,Uint32)) = ccall(:randomseed32, Void, (Uint32,), uint32(s))
srand(s::Union(Int64,Uint64)) = ccall(:randomseed64, Void, (Uint64,), uint64(s))
