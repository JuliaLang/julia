## hashing BigInts, BigFloats, and Float16s ##

function hash_integer(n::BigInt, h::Uint=zero(Uint))
    s = n.size
    s == 0 && return hash_integer(0, h)
    p = convert(Ptr{Uint}, n.d)
    b = unsafe_load(p)
    h = hash_uint(ifelse(s < 0, -b, b) $ h) $ h
    for k = 2:abs(s)
        h = hash_uint(unsafe_load(p, k) $ h) $ h
    end
    return h
end

function decompose(x::BigFloat)
    isnan(x) && return big(0), 0, 0
    isinf(x) && return big(x.sign), 0, 0
    x == 0 && return big(0), 0, int(x.sign)
    s = BigInt()
    ccall((:__gmpz_realloc2, :libgmp), Void, (Ptr{BigInt}, Culong), &s, x.prec)
    s.size = -fld(-x.prec,(sizeof(Culong)<<3))
    ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Csize_t), s.d, x.d, s.size*sizeof(Culong))
    s, int(x.exp - x.prec), int(x.sign)
end

hash(x::Float16, h::Uint=zero(Uint)) = hash(float64(x), h)

## hashing strings ##

function hash{T<:ByteString}(s::Union(T,SubString{T}), h::Uint=zero(Uint))
    h += 0x71e729fd56419c81
    ccall(:memhash_seed, Uint64, (Ptr{Void}, Int, Uint32), pointer(s), sizeof(s), h) + h
end
hash(s::String, h::Uint=zero(Uint)) = hash(bytestring(s), h)

## hashing collections ##

function hash(v::Union(Tuple,AbstractArray,Associative), h::Uint=zero(Uint))
    h += object_id(eltype(v))
    for x = v
        h = hash(x, h)
    end
    return h
end

hash(s::Set, h::Uint=zero(Uint)) = hash(sort(s.dict.keys[s.dict.slots .!= 0]), h)

hash(r::Range{Bool}, h::Uint=zero(Uint)) = invoke(hash, (Range, Uint), r, h)
hash(B::BitArray, h::Uint=zero(Uint)) = hash((size(B),B.chunks), h)
hash(a::AbstractArray{Bool}, h::Uint=zero(Uint)) = hash(bitpack(a), h)

# hashing ranges by component at worst leads to collisions for very similar ranges
hash{T<:Range}(r::T, h::Uint=zero(Uint)) =
    hash(first(r), hash(step(r), hash(last(r), h + object_id(eltype(T)))))

## hashing general objects and expressions ##

hash(x::ANY,  h::Uint=zero(Uint)) = hash(object_id(x), h)
