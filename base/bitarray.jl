# This file is a part of Julia. License is MIT: http://julialang.org/license

## BitArray

# notes: bits are stored in contiguous chunks
#        unused bits must always be set to 0
type BitArray{N} <: DenseArray{Bool, N}
    chunks::Vector{UInt64}
    len::Int
    dims::NTuple{N,Int}
    function BitArray(dims::Int...)
        length(dims) == N || throw(ArgumentError("number of dimensions must be $N, got $(length(dims))"))
        n = 1
        i = 1
        for d in dims
            d >= 0 || throw(ArgumentError("dimension size must be ≥ 0, got $d for dimension $i"))
            n *= d
            i += 1
        end
        nc = num_bit_chunks(n)
        chunks = Array(UInt64, nc)
        nc > 0 && (chunks[end] = UInt64(0))
        b = new(chunks, n)
        N != 1 && (b.dims = dims)
        return b
    end
end

BitArray{N}(dims::NTuple{N,Int}) = BitArray{N}(dims...)
BitArray(dims::Int...) = BitArray(dims)

typealias BitVector BitArray{1}
typealias BitMatrix BitArray{2}

(::Type{BitVector})() = BitArray{1}(0)

## utility functions ##

length(B::BitArray) = B.len
size(B::BitVector) = (B.len,)
size(B::BitArray) = B.dims

size(B::BitVector, d) = begin
    if d == 1
        return B.len
    elseif d > 1
        return 1
    end
    throw(ArgumentError("dimension must be ≥ 1, got $d"))
end
size{N}(B::BitArray{N}, d) = (d>N ? 1 : B.dims[d])

isassigned{N}(B::BitArray{N}, i::Int) = 1 <= i <= length(B)

## aux functions ##

const _msk64 = ~UInt64(0)
@inline _div64(l) = l >>> 6
@inline _mod64(l) = l & 63
@inline _msk_end(l::Integer) = _msk64 >>> _mod64(-l)
@inline _msk_end(B::BitArray) = _msk_end(length(B))
num_bit_chunks(n::Int) = _div64(n+63)

@inline get_chunks_id(i::Integer) = _div64(Int(i)-1)+1, _mod64(Int(i)-1)

function glue_src_bitchunks(src::Vector{UInt64}, k::Int, ks1::Int, msk_s0::UInt64, ls0::Int)
    @inbounds begin
        chunk = ((src[k] & msk_s0) >>> ls0)
        if ks1 > k && ls0 > 0
            chunk_n = (src[k + 1] & ~msk_s0)
            chunk |= (chunk_n << (64 - ls0))
        end
    end
    return chunk
end

function copy_chunks!(dest::Vector{UInt64}, pos_d::Integer, src::Vector{UInt64}, pos_s::Integer, numbits::Integer)
    numbits == 0 && return
    if dest === src && pos_d > pos_s
        return copy_chunks_rtol!(dest, pos_d, pos_s, numbits)
    end

    kd0, ld0 = get_chunks_id(pos_d)
    kd1, ld1 = get_chunks_id(pos_d + numbits - 1)
    ks0, ls0 = get_chunks_id(pos_s)
    ks1, ls1 = get_chunks_id(pos_s + numbits - 1)

    delta_kd = kd1 - kd0
    delta_ks = ks1 - ks0

    u = _msk64
    if delta_kd == 0
        msk_d0 = ~(u << ld0) | (u << (ld1+1))
    else
        msk_d0 = ~(u << ld0)
        msk_d1 = (u << (ld1+1))
    end
    if delta_ks == 0
        msk_s0 = (u << ls0) & ~(u << (ls1+1))
    else
        msk_s0 = (u << ls0)
    end

    chunk_s0 = glue_src_bitchunks(src, ks0, ks1, msk_s0, ls0)

    dest[kd0] = (dest[kd0] & msk_d0) | ((chunk_s0 << ld0) & ~msk_d0)

    delta_kd == 0 && return

    for i = 1 : kd1 - kd0 - 1
        chunk_s1 = glue_src_bitchunks(src, ks0 + i, ks1, msk_s0, ls0)

        chunk_s = (chunk_s0 >>> (64 - ld0)) | (chunk_s1 << ld0)

        dest[kd0 + i] = chunk_s

        chunk_s0 = chunk_s1
    end

    if ks1 >= ks0 + delta_kd
        chunk_s1 = glue_src_bitchunks(src, ks0 + delta_kd, ks1, msk_s0, ls0)
    else
        chunk_s1 = UInt64(0)
    end

    chunk_s = (chunk_s0 >>> (64 - ld0)) | (chunk_s1 << ld0)

    dest[kd1] = (dest[kd1] & msk_d1) | (chunk_s & ~msk_d1)

    return
end

function copy_chunks_rtol!(chunks::Vector{UInt64}, pos_d::Integer, pos_s::Integer, numbits::Integer)
    pos_d == pos_s && return
    pos_d < pos_s && return copy_chunks!(chunks, pos_d, chunks, pos_s, numbits)

    left = numbits
    s = min(left, 64)
    b = left - s
    ps = pos_s + b
    pd = pos_d + b
    u = _msk64
    while left > 0
        kd0, ld0 = get_chunks_id(pd)
        kd1, ld1 = get_chunks_id(pd + s - 1)
        ks0, ls0 = get_chunks_id(ps)
        ks1, ls1 = get_chunks_id(ps + s - 1)

        delta_kd = kd1 - kd0
        delta_ks = ks1 - ks0

        if delta_kd == 0
            msk_d0 = ~(u << ld0) | (u << (ld1+1))
        else
            msk_d0 = ~(u << ld0)
            msk_d1 = (u << (ld1+1))
        end
        if delta_ks == 0
            msk_s0 = (u << ls0) & ~(u << (ls1+1))
        else
            msk_s0 = (u << ls0)
        end

        chunk_s0 = glue_src_bitchunks(chunks, ks0, ks1, msk_s0, ls0) & ~(u << s)
        chunks[kd0] = (chunks[kd0] & msk_d0) | ((chunk_s0 << ld0) & ~msk_d0)

        if delta_kd != 0
            chunk_s = (chunk_s0 >>> (64 - ld0))

            chunks[kd1] = (chunks[kd1] & msk_d1) | (chunk_s & ~msk_d1)
        end

        left -= s
        s = min(left, 64)
        b = left - s
        ps = pos_s + b
        pd = pos_d + b
    end
end

function fill_chunks!(Bc::Array{UInt64}, x::Bool, pos::Integer, numbits::Integer)
    numbits <= 0 && return
    k0, l0 = get_chunks_id(pos)
    k1, l1 = get_chunks_id(pos+numbits-1)

    u = _msk64
    if k1 == k0
        msk0 = (u << l0) & ~(u << (l1+1))
    else
        msk0 = (u << l0)
        msk1 = ~(u << (l1+1))
    end
    @inbounds if x
        Bc[k0] |= msk0
        for k = k0+1:k1-1
            Bc[k] = u
        end
        k1 > k0 && (Bc[k1] |= msk1)
    else
        Bc[k0] &= ~msk0
        for k = k0+1:k1-1
            Bc[k] = 0
        end
        k1 > k0 && (Bc[k1] &= ~msk1)
    end
end

copy_to_bitarray_chunks!(dest::Vector{UInt64}, pos_d::Integer, src::BitArray, pos_s::Integer, numbits::Integer) =
    copy_chunks!(dest, pos_d, src.chunks, pos_s, numbits)

# pack 8 Bools encoded as one contiguous UIn64 into a single byte, e.g.:
# 0000001:0000001:00000000:00000000:00000001:00000000:00000000:00000001 → 11001001 → 0xc9
function pack8bools(z::UInt64)
    z |= z >>> 7
    z |= z >>> 14
    z |= z >>> 28
    z &= 0xFF
    return z
end

function copy_to_bitarray_chunks!(Bc::Vector{UInt64}, pos_d::Int, C::Array{Bool}, pos_s::Int, numbits::Int)
    kd0, ld0 = get_chunks_id(pos_d)
    kd1, ld1 = get_chunks_id(pos_d + numbits - 1)

    delta_kd = kd1 - kd0

    u = _msk64
    if delta_kd == 0
        msk_d0 = msk_d1 = ~(u << ld0) | (u << (ld1+1))
        lt0 = ld1
    else
        msk_d0 = ~(u << ld0)
        msk_d1 = (u << (ld1+1))
        lt0 = 63
    end

    bind = kd0
    ind = pos_s
    @inbounds if ld0 > 0
        c = UInt64(0)
        for j = ld0:lt0
            c |= (UInt64(C[ind]) << j)
            ind += 1
        end
        Bc[kd0] = (Bc[kd0] & msk_d0) | (c & ~msk_d0)
        bind += 1
    end

    nc = _div64(numbits - ind + pos_s)
    nc8 = (nc >>> 3) << 3
    if nc8 > 0
        ind8 = 1
        C8 = reinterpret(UInt64, pointer_to_array(pointer(C, ind), nc8 << 6))
        @inbounds for i = 1:nc8
            c = UInt64(0)
            for j = 0:7
                c |= (pack8bools(C8[ind8]) << (j<<3))
                ind8 += 1
            end
            Bc[bind] = c
            bind += 1
        end
        ind += (ind8-1) << 3
    end
    @inbounds for i = (nc8+1):nc
        c = UInt64(0)
        for j = 0:63
            c |= (UInt64(C[ind]) << j)
            ind += 1
        end
        Bc[bind] = c
        bind += 1
    end
    @inbounds if bind ≤ kd1
        @assert bind == kd1
        c = UInt64(0)
        for j = 0:ld1
            c |= (UInt64(C[ind]) << j)
            ind += 1
        end
        Bc[kd1] = (Bc[kd1] & msk_d1) | (c & ~msk_d1)
    end
end

function copy_to_bitarray_chunks!(Bc::Vector{UInt64}, pos_d::Int, C::Array, pos_s::Int, numbits::Int)
    kd0, ld0 = get_chunks_id(pos_d)
    kd1, ld1 = get_chunks_id(pos_d + numbits - 1)

    delta_kd = kd1 - kd0

    u = _msk64
    if delta_kd == 0
        msk_d0 = msk_d1 = ~(u << ld0) | (u << (ld1+1))
        lt0 = ld1
    else
        msk_d0 = ~(u << ld0)
        msk_d1 = (u << (ld1+1))
        lt0 = 63
    end

    bind = kd0
    ind = pos_s
    @inbounds if ld0 > 0
        c = UInt64(0)
        for j = ld0:lt0
            c |= (UInt64(Bool(C[ind])) << j)
            ind += 1
        end
        Bc[kd0] = (Bc[kd0] & msk_d0) | (c & ~msk_d0)
        bind += 1
    end

    nc = _div64(numbits - ind + pos_s)
    @inbounds for i = 1:nc
        c = UInt64(0)
        for j = 0:63
            c |= (UInt64(Bool(C[ind])) << j)
            ind += 1
        end
        Bc[bind] = c
        bind += 1
    end

    @inbounds if bind ≤ kd1
        @assert bind == kd1
        c = UInt64(0)
        for j = 0:ld1
            c |= (UInt64(Bool(C[ind])) << j)
            ind += 1
        end
        Bc[kd1] = (Bc[kd1] & msk_d1) | (c & ~msk_d1)
    end
end

## custom iterator ##
start(B::BitArray) = 0
next(B::BitArray, i::Int) = (B.chunks[_div64(i)+1] & (UInt64(1)<<_mod64(i)) != 0, i+1)
done(B::BitArray, i::Int) = i >= length(B)

## similar, fill!, copy! etc ##

similar(B::BitArray) = BitArray(size(B))
similar(B::BitArray, dims::Int...) = BitArray(dims)
similar(B::BitArray, dims::Dims) = BitArray(dims...)

similar(B::BitArray, T::Type{Bool}, dims::Dims) = BitArray(dims)
# changing type to a non-Bool returns an Array
# (this triggers conversions like float(bitvector) etc.)
similar(B::BitArray, T::Type, dims::Dims) = Array(T, dims)

function fill!(B::BitArray, x)
    y = convert(Bool, x)
    isempty(B) && return B
    Bc = B.chunks
    if !y
        fill!(Bc, 0)
    else
        fill!(Bc, _msk64)
        Bc[end] &= _msk_end(B)
    end
    return B
end

falses(args...) = fill!(BitArray(args...), false)
trues(args...) = fill!(BitArray(args...), true)

function one(x::BitMatrix)
    m, n = size(x)
    m == n || throw(DimensionMismatch("multiplicative identity defined only for square matrices"))
    a = falses(n, n)
    for i = 1:n
        a[i,i] = true
    end
    return a
end

function copy!(dest::BitArray, src::BitArray)
    length(src) > length(dest) && throw(BoundsError(dest, length(dest)+1))
    destc = dest.chunks; srcc = src.chunks
    nc = min(length(destc), length(srcc))
    nc == 0 && return dest
    @inbounds begin
        for i = 1 : nc - 1
            destc[i] = srcc[i]
        end
        if length(src) == length(dest)
            destc[nc] = srcc[nc]
        else
            msk_s = _msk_end(src)
            msk_d = ~msk_s
            destc[nc] = (msk_d & destc[nc]) | (msk_s & srcc[nc])
        end
    end
    return dest
end

function unsafe_copy!(dest::BitArray, doffs::Integer, src::Union{BitArray,Array}, soffs::Integer, n::Integer)
    copy_to_bitarray_chunks!(dest.chunks, doffs, src, soffs, n)
    return dest
end

function copy!(dest::BitArray, doffs::Integer, src::Array, soffs::Integer, n::Integer)
    n == 0 && return dest
    soffs < 1 && throw(BoundsError(src, soffs))
    doffs < 1 && throw(BoundsError(dest, doffs))
    soffs+n-1 > length(src) && throw(BoundsError(src, length(src)+1))
    doffs+n-1 > length(dest) && throw(BoundsError(dest, length(dest)+1))
    return unsafe_copy!(dest, doffs, src, soffs, n)
end

function copy!(dest::BitArray, src::Array)
    length(src) > length(dest) && throw(BoundsError(dest, length(dest)+1))
    length(src) == 0 && return det
    return unsafe_copy!(dest, 1, src, 1, length(src))
end


function reshape{N}(B::BitArray, dims::NTuple{N,Int})
    prod(dims) == length(B) ||
        throw(DimensionMismatch("new dimensions $(dims) must be consistent with array size $(length(B))"))
    dims == size(B) && return B
    Br = BitArray{N}(ntuple(i->0,N)...)
    Br.chunks = B.chunks
    Br.len = prod(dims)
    N != 1 && (Br.dims = dims)
    return Br
end

## Conversions ##

convert{T,N}(::Type{Array{T}}, B::BitArray{N}) = convert(Array{T,N}, B)
convert{T,N}(::Type{Array{T,N}}, B::BitArray{N}) = _convert(Array{T,N}, B) # see #15801
function _convert{T,N}(::Type{Array{T,N}}, B::BitArray{N})
    A = Array(T, size(B))
    Bc = B.chunks
    @inbounds for i = 1:length(A)
        A[i] = unsafe_bitgetindex(Bc, i)
    end
    return A
end

convert{T,N}(::Type{BitArray}, A::AbstractArray{T,N}) = convert(BitArray{N}, A)
function convert{T,N}(::Type{BitArray{N}}, A::AbstractArray{T,N})
    B = BitArray(size(A))
    Bc = B.chunks
    l = length(B)
    l == 0 && return B
    ind = 1
    @inbounds begin
        for i = 1:length(Bc)-1
            c = UInt64(0)
            for j = 0:63
                c |= (UInt64(A[ind] != 0) << j)
                ind += 1
            end
            Bc[i] = c
        end
        c = UInt64(0)
        for j = 0:_mod64(l-1)
            c |= (UInt64(A[ind] != 0) << j)
            ind += 1
        end
        Bc[end] = c
    end
    return B
end

function convert{N}(::Type{BitArray{N}}, A::Array{Bool,N})
    B = BitArray(size(A))
    Bc = B.chunks
    l = length(B)
    l == 0 && return B
    copy_to_bitarray_chunks!(Bc, 1, A, 1, l)
    return B
end

convert{N}(::Type{BitArray{N}}, B::BitArray{N}) = B
convert{T,N}(::Type{AbstractArray{T,N}}, B::BitArray{N}) = convert(Array{T,N}, B)

reinterpret{N}(::Type{Bool}, B::BitArray, dims::NTuple{N,Int}) = reinterpret(B, dims)
reinterpret{N}(B::BitArray, dims::NTuple{N,Int}) = reshape(B, dims)

# shorthand forms BitArray <-> Array
bitunpack{N}(B::BitArray{N}) = convert(Array{Bool,N}, B)
bitpack{T,N}(A::AbstractArray{T,N}) = convert(BitArray{N}, A)

## Indexing: getindex ##

@inline function unsafe_bitgetindex(Bc::Vector{UInt64}, i::Int)
    i1, i2 = get_chunks_id(i)
    u = UInt64(1) << i2
    @inbounds r = (Bc[i1] & u) != 0
    return r
end

@inline function getindex(B::BitArray, i::Int)
    @boundscheck checkbounds(B, i)
    unsafe_bitgetindex(B.chunks, i)
end

## Indexing: setindex! ##

@inline function unsafe_bitsetindex!(Bc::Array{UInt64}, x::Bool, i::Int)
    i1, i2 = get_chunks_id(i)
    u = UInt64(1) << i2
    @inbounds begin
        c = Bc[i1]
        Bc[i1] = ifelse(x, c | u, c & ~u)
    end
end

@inline function setindex!(B::BitArray, x, i::Int)
    @boundscheck checkbounds(B, i)
    unsafe_bitsetindex!(B.chunks, convert(Bool, x), i)
    return B
end

# logical indexing

# When indexing with a BitArray, we can operate whole chunks at a time for a ~100x gain
@inline function setindex!(B::BitArray, x, I::BitArray)
    @boundscheck checkbounds(B, I)
    _unsafe_setindex!(B, x, I)
end
function _unsafe_setindex!(B::BitArray, x, I::BitArray)
    y = convert(Bool, x)
    Bc = B.chunks
    Ic = I.chunks
    length(Bc) == length(Ic) || throw_boundserror(B, I)
    @inbounds if y
        for i = 1:length(Bc)
            Bc[i] |= Ic[i]
        end
    else
        for i = 1:length(Bc)
            Bc[i] &= ~Ic[i]
        end
    end
    return B
end

# Assigning an array of bools is more complicated, but we can still do some
# work on chunks by combining X and I 64 bits at a time to improve perf by ~40%
@inline function setindex!(B::BitArray, X::AbstractArray, I::BitArray)
    @boundscheck checkbounds(B, I)
    _unsafe_setindex!(B, X, I)
end
function _unsafe_setindex!(B::BitArray, X::AbstractArray, I::BitArray)
    Bc = B.chunks
    Ic = I.chunks
    length(Bc) == length(Ic) || throw_boundserror(B, I)
    lc = length(Bc)
    lx = length(X)
    last_chunk_len = _mod64(length(B)-1)+1

    c = 1
    for i = 1:lc
        @inbounds Imsk = Ic[i]
        @inbounds C = Bc[i]
        u = UInt64(1)
        for j = 1:(i < lc ? 64 : last_chunk_len)
            if Imsk & u != 0
                lx < c && throw_setindex_mismatch(X, c)
                @inbounds x = convert(Bool, X[c])
                C = ifelse(x, C | u, C & ~u)
                c += 1
            end
            u <<= 1
        end
        @inbounds Bc[i] = C
    end
    if length(X) != c-1
        throw_setindex_mismatch(X, c-1)
    end
    return B
end

## Dequeue functionality ##

function push!(B::BitVector, item)
    # convert first so we don't grow the bitarray if the assignment won't work
    item = convert(Bool, item)

    Bc = B.chunks

    l = _mod64(length(B))
    if l == 0
        ccall(:jl_array_grow_end, Void, (Any, UInt), Bc, 1)
        Bc[end] = UInt64(0)
    end
    B.len += 1
    if item
        B[end] = true
    end
    return B
end

function append!(B::BitVector, items::BitVector)
    n0 = length(B)
    n1 = length(items)
    if n1 == 0
        return B
    end
    Bc = B.chunks
    k0 = length(Bc)
    k1 = num_bit_chunks(n0 + n1)
    if k1 > k0
        ccall(:jl_array_grow_end, Void, (Any, UInt), Bc, k1 - k0)
        Bc[end] = UInt64(0)
    end
    B.len += n1
    copy_chunks!(Bc, n0+1, items.chunks, 1, n1)
    return B
end

append!(B::BitVector, items::AbstractVector{Bool}) = append!(B, bitpack(items))
append!(A::Vector{Bool}, items::BitVector) = append!(A, bitunpack(items))

function prepend!(B::BitVector, items::BitVector)
    n0 = length(B)
    n1 = length(items)
    if n1 == 0
        return B
    end
    Bc = B.chunks
    k0 = length(Bc)
    k1 = num_bit_chunks(n0 + n1)
    if k1 > k0
        ccall(:jl_array_grow_end, Void, (Any, UInt), Bc, k1 - k0)
        Bc[end] = UInt64(0)
    end
    B.len += n1
    copy_chunks!(Bc, 1 + n1, Bc, 1, n0)
    copy_chunks!(Bc, 1, items.chunks, 1, n1)
    return B
end

prepend!(B::BitVector, items::AbstractVector{Bool}) = prepend!(B, bitpack(items))
prepend!(A::Vector{Bool}, items::BitVector) = prepend!(A, bitunpack(items))

function sizehint!(B::BitVector, sz::Integer)
    ccall(:jl_array_sizehint, Void, (Any, UInt), B.chunks, num_bit_chunks(sz))
    return B
end

function resize!(B::BitVector, n::Integer)
    n0 = length(B)
    n == n0 && return B
    n >= 0 || throw(BoundsError(B, n))
    if n < n0
        deleteat!(B, n+1:n0)
        return B
    end
    Bc = B.chunks
    k0 = length(Bc)
    k1 = num_bit_chunks(Int(n))
    if k1 > k0
        ccall(:jl_array_grow_end, Void, (Any, UInt), Bc, k1 - k0)
        Bc[end] = UInt64(0)
    end
    B.len = n
    return B
end

function pop!(B::BitVector)
    isempty(B) && throw(ArgumentError("argument must not be empty"))
    item = B[end]
    B[end] = false

    l = _mod64(length(B))
    l == 1 && ccall(:jl_array_del_end, Void, (Any, UInt), B.chunks, 1)
    B.len -= 1

    return item
end

function unshift!(B::BitVector, item)
    item = convert(Bool, item)

    Bc = B.chunks

    l = _mod64(length(B))
    if l == 0
        ccall(:jl_array_grow_end, Void, (Any, UInt), Bc, 1)
        Bc[end] = UInt64(0)
    end
    B.len += 1
    if B.len == 1
        Bc[1] = item
        return B
    end
    for i = length(Bc) : -1 : 2
        Bc[i] = (Bc[i] << 1) | (Bc[i-1] >>> 63)
    end
    Bc[1] = UInt64(item) | (Bc[1] << 1)
    return B
end

function shift!(B::BitVector)
    isempty(B) && throw(ArgumentError("argument must not be empty"))
    @inbounds begin
        item = B[1]

        Bc = B.chunks

        for i = 1 : length(Bc) - 1
            Bc[i] = (Bc[i] >>> 1) | (Bc[i+1] << 63)
        end

        l = _mod64(length(B))
        if l == 1
            ccall(:jl_array_del_end, Void, (Any, UInt), Bc, 1)
        else
            Bc[end] >>>= 1
        end
        B.len -= 1
    end

    return item
end

function insert!(B::BitVector, i::Integer, item)
    n = length(B)
    1 <= i <= n+1 || throw(BoundsError(B, i))
    item = convert(Bool, item)

    Bc = B.chunks

    k, j = get_chunks_id(i)

    l = _mod64(length(B))
    if l == 0
        ccall(:jl_array_grow_end, Void, (Any, UInt), Bc, 1)
        Bc[end] = UInt64(0)
    end
    B.len += 1

    for t = length(Bc) : -1 : k + 1
        Bc[t] = (Bc[t] << 1) | (Bc[t - 1] >>> 63)
    end

    msk_aft = (_msk64 << j)
    msk_bef = ~msk_aft
    Bc[k] = (msk_bef & Bc[k]) | ((msk_aft & Bc[k]) << 1)
    B[i] = item
    B
end

function _deleteat!(B::BitVector, i::Integer)

    k, j = get_chunks_id(i)

    msk_bef = _msk64 >>> (63 - j)
    msk_aft = ~msk_bef
    msk_bef >>>= 1

    Bc = B.chunks

    @inbounds begin
        Bc[k] = (msk_bef & Bc[k]) | ((msk_aft & Bc[k]) >> 1)
        if length(Bc) > k
            Bc[k] |= (Bc[k + 1] << 63)
        end

        for t = k + 1 : length(Bc) - 1
            Bc[t] = (Bc[t] >>> 1) | (Bc[t + 1] << 63)
        end

        l = _mod64(length(B))

        if l == 1
            ccall(:jl_array_del_end, Void, (Any, UInt), Bc, 1)
        elseif length(Bc) > k
            Bc[end] >>>= 1
        end
    end

    B.len -= 1

    return B
end

function deleteat!(B::BitVector, i::Integer)
    n = length(B)
    1 <= i <= n || throw(BoundsError(B, i))

    return _deleteat!(B, i)
end

function deleteat!(B::BitVector, r::UnitRange{Int})
    n = length(B)
    i_f = first(r)
    i_l = last(r)
    1 <= i_f || throw(BoundsError(B, i_f))
    i_l <= n || throw(BoundsError(B, n+1))

    Bc = B.chunks
    new_l = length(B) - length(r)
    delta_k = num_bit_chunks(new_l) - length(Bc)

    copy_chunks!(Bc, i_f, Bc, i_l+1, n-i_l)

    delta_k < 0 && ccall(:jl_array_del_end, Void, (Any, UInt), Bc, -delta_k)

    B.len = new_l

    if new_l > 0
        Bc[end] &= _msk_end(new_l)
    end

    return B
end

function deleteat!(B::BitVector, inds)
    n = new_l = length(B)
    s = start(inds)
    done(inds, s) && return B

    Bc = B.chunks

    (p, s) = next(inds, s)
    q = p+1
    new_l -= 1
    while !done(inds, s)
        (i,s) = next(inds, s)
        if !(q <= i <= n)
            i < q && throw(ArgumentError("indices must be unique and sorted"))
            throw(BoundsError(B, i))
        end
        new_l -= 1
        if i > q
            copy_chunks!(Bc, p, Bc, q, i-q)
            p += i-q
        end
        q = i+1
    end

    q <= n && copy_chunks!(Bc, p, Bc, q, n-q+1)

    delta_k = num_bit_chunks(new_l) - length(Bc)
    delta_k < 0 && ccall(:jl_array_del_end, Void, (Any, UInt), Bc, -delta_k)

    B.len = new_l

    if new_l > 0
        Bc[end] &= _msk_end(new_l)
    end

    return B
end

function splice!(B::BitVector, i::Integer)
    n = length(B)
    1 <= i <= n || throw(BoundsError(B, i))

    v = B[i]   # TODO: change to a copy if/when subscripting becomes an ArrayView
    _deleteat!(B, i)
    return v
end

const _default_bit_splice = BitVector(0)

function splice!(B::BitVector, r::Union{UnitRange{Int}, Integer}, ins::AbstractArray = _default_bit_splice)
    n = length(B)
    i_f = first(r)
    i_l = last(r)

    1 <= i_f <= n+1 || throw(BoundsError(B, i_f))
    i_l <= n || throw(BoundsError(B, n+1))

    Bins = convert(BitArray, ins)

    if (i_f > n)
        append!(B, Bins)
        return BitVector(0)
    end

    v = B[r]  # TODO: change to a copy if/when subscripting becomes an ArrayView

    Bc = B.chunks

    lins = length(Bins)
    ldel = length(r)

    new_l = length(B) + lins - ldel
    delta_k = num_bit_chunks(new_l) - length(Bc)

    delta_k > 0 && ccall(:jl_array_grow_end, Void, (Any, UInt), Bc, delta_k)

    copy_chunks!(Bc, i_f+lins, Bc, i_l+1, n-i_l)
    copy_chunks!(Bc, i_f, Bins.chunks, 1, lins)

    delta_k < 0 && ccall(:jl_array_del_end, Void, (Any, UInt), Bc, -delta_k)

    B.len = new_l

    if new_l > 0
        Bc[end] &= _msk_end(new_l)
    end

    return v
end

function splice!(B::BitVector, r::Union{UnitRange{Int}, Integer}, ins)
    Bins = BitArray(length(ins))
    i = 1
    for x in ins
        Bins[i] = Bool(x)
        i += 1
    end
    return splice!(B, r, Bins)
end


function empty!(B::BitVector)
    ccall(:jl_array_del_end, Void, (Any, UInt), B.chunks, length(B.chunks))
    B.len = 0
    return B
end

## Misc functions
abs(B::BitArray) = copy(B)

## Unary operators ##

function (-)(B::BitArray)
    A = zeros(Int, size(B))
    l = length(B)
    l == 0 && return A
    Bc = B.chunks
    ind = 1
    for i = 1:length(Bc)-1
        u = UInt64(1)
        c = Bc[i]
        for j = 1:64
            if c & u != 0
                A[ind] = -1
            end
            ind += 1
            u <<= 1
        end
    end
    u = UInt64(1)
    c = Bc[end]
    for j = 0:_mod64(l-1)
        if c & u != 0
            A[ind] = -1
        end
        ind += 1
        u <<= 1
    end
    return A
end
sign(B::BitArray) = copy(B)

function (~)(B::BitArray)
    C = similar(B)
    Bc = B.chunks
    if !isempty(Bc)
        Cc = C.chunks
        for i = 1:length(Bc)
            Cc[i] = ~Bc[i]
        end
        Cc[end] &= _msk_end(B)
    end
    return C
end

function flipbits!(B::BitArray)
    Bc = B.chunks
    @inbounds if !isempty(Bc)
        for i = 1:length(Bc)
            Bc[i] = ~Bc[i]
        end
        Bc[end] &= _msk_end(B)
    end
    return B
end

!(B::BitArray) = ~B

## Binary arithmetic operators ##

for f in (:+, :-)
    @eval function ($f)(A::BitArray, B::BitArray)
        r = Array(Int, promote_shape(size(A), size(B)))
        ai = start(A)
        bi = start(B)
        ri = 1
        while !done(A, ai)
            a, ai = next(A, ai)
            b, bi = next(B, bi)
            @inbounds r[ri] = ($f)(a, b)
            ri += 1
        end
        return r
    end
end
for (f,F) in ((:.+, DotAddFun()),
              (:.-, DotSubFun()))
    for (arg1, arg2, T, fargs) in ((:(B::BitArray), :(x::Bool)    , Int                                   , :(b, x)),
                                   (:(B::BitArray), :(x::Number)  , :(promote_array_type($F, typeof(x), Bool)), :(b, x)),
                                   (:(x::Bool)    , :(B::BitArray), Int                                   , :(x, b)),
                                   (:(x::Number)  , :(B::BitArray), :(promote_array_type($F, typeof(x), Bool)), :(x, b)))
        @eval function ($f)($arg1, $arg2)
            r = Array($T, size(B))
            bi = start(B)
            ri = 1
            while !done(B, bi)
                b, bi = next(B, bi)
                @inbounds r[ri] = ($f)($fargs...)
                ri += 1
            end
            return r
        end
    end
end

for f in (:/, :\)
    @eval begin
        ($f)(A::BitArray, B::BitArray) = ($f)(bitunpack(A), bitunpack(B))
    end
end
(/)(B::BitArray, x::Number) = (/)(bitunpack(B), x)
(/)(x::Number, B::BitArray) = (/)(x, bitunpack(B))

function div(A::BitArray, B::BitArray)
    shp = promote_shape(size(A), size(B))
    all(B) || throw(DivideError())
    return reshape(copy(A), shp)
end
div(A::BitArray, B::Array{Bool}) = div(A, bitpack(B))
div(A::Array{Bool}, B::BitArray) = div(bitpack(A), B)
function div(B::BitArray, x::Bool)
    return x ? copy(B) : throw(DivideError())
end
function div(x::Bool, B::BitArray)
    all(B) || throw(DivideError())
    return x ? trues(size(B)) : falses(size(B))
end
function div(x::Number, B::BitArray)
    all(B) || throw(DivideError())
    pt = promote_array_type(IDivFun(), typeof(x), Bool)
    y = div(x, true)
    reshape(pt[ y for i = 1:length(B) ], size(B))
end

function mod(A::BitArray, B::BitArray)
    shp = promote_shape(size(A), size(B))
    all(B) || throw(DivideError())
    return falses(shp)
end
mod(A::BitArray, B::Array{Bool}) = mod(A, bitpack(B))
mod(A::Array{Bool}, B::BitArray) = mod(bitpack(A), B)
function mod(B::BitArray, x::Bool)
    return x ? falses(size(B)) : throw(DivideError())
end
function mod(x::Bool, B::BitArray)
    all(B) || throw(DivideError())
    return falses(size(B))
end
function mod(x::Number, B::BitArray)
    all(B) || throw(DivideError())
    pt = promote_array_type(ModFun(), typeof(x), Bool)
    y = mod(x, true)
    reshape(pt[ y for i = 1:length(B) ], size(B))
end

for (f,F) in ((:div, IDivFun()),
              (:mod, ModFun()))
    @eval begin
        function ($f)(B::BitArray, x::Number)
            F = Array(promote_array_type($F, typeof(x), Bool), size(B))
            for i = 1:length(F)
                F[i] = ($f)(B[i], x)
            end
            return F
        end
    end
end

function (&)(B::BitArray, x::Bool)
    x ? copy(B) : falses(size(B))
end
(&)(x::Bool, B::BitArray) = B & x

function (|)(B::BitArray, x::Bool)
    x ? trues(size(B)) : copy(B)
end
(|)(x::Bool, B::BitArray) = B | x

function ($)(B::BitArray, x::Bool)
    x ? ~B : copy(B)
end
($)(x::Bool, B::BitArray) = B $ x

for f in (:&, :|, :$)
    @eval begin
        function ($f)(A::BitArray, B::BitArray)
            F = BitArray(promote_shape(size(A),size(B))...)
            Fc = F.chunks
            Ac = A.chunks
            Bc = B.chunks
            (isempty(Ac) || isempty(Bc)) && return F
            for i = 1:length(Fc)
                Fc[i] = ($f)(Ac[i], Bc[i])
            end
            Fc[end] &= _msk_end(F)
            return F
        end
        ($f)(A::DenseArray{Bool}, B::BitArray) = ($f)(bitpack(A), B)
        ($f)(B::BitArray, A::DenseArray{Bool}) = ($f)(B, bitpack(A))
        ($f)(x::Number, B::BitArray) = ($f)(x, bitunpack(B))
        ($f)(B::BitArray, x::Number) = ($f)(bitunpack(B), x)
    end
end

function (.^)(B::BitArray, x::Bool)
    x ? copy(B) : trues(size(B))
end
function (.^)(x::Bool, B::BitArray)
    x ? trues(size(B)) : ~B
end
function (.^)(x::Number, B::BitArray)
    z = x ^ false
    u = x ^ true
    reshape([ B[i] ? u : z for i = 1:length(B) ], size(B))
end
function (.^)(B::BitArray, x::Integer)
    if x == 0
        return trues(size(B))
    elseif x < 0
        throw(DomainError())
    else
        return copy(B)
    end
end
function (.^){T<:Number}(B::BitArray, x::T)
    if x == 0
        return ones(typeof(true ^ x), size(B))
    elseif T <: Real && x > 0
        return convert(Array{T}, B)
    else
        z = nothing
        u = nothing
        zerr = nothing
        uerr = nothing
        try
            z = false^x
        catch err
            zerr = err
        end
        try
            u = true^x
        catch err
            uerr = err
        end
        if zerr === nothing && uerr === nothing
            t = promote_type(typeof(z), typeof(u))
        elseif zerr === nothing
            t = typeof(z)
        else
            t = typeof(u)
        end
        F = Array(t, size(B))
        for i = 1:length(B)
            if B[i]
                if uerr === nothing
                    F[i] = u
                else
                    throw(uerr)
                end
            else
                if zerr === nothing
                    F[i] = z
                else
                    throw(zerr)
                end
            end
        end
        return F
    end
end

(.*)(x::Bool, B::BitArray) = x & B
(.*)(B::BitArray, x::Bool) = B & x
(.*)(x::Number, B::BitArray) = x .* bitunpack(B)
(.*)(B::BitArray, x::Number) = bitunpack(B) .* x

## promotion to complex ##

# TODO?

## comparison operators ##

function (==)(A::BitArray, B::BitArray)
    size(A) != size(B) && return false
    return A.chunks == B.chunks
end


## Data movement ##

# TODO some of this could be optimized

function slicedim(A::BitArray, d::Integer, i::Integer)
    d_in = size(A)
    leading = d_in[1:(d-1)]
    d_out = tuple(leading..., 1, d_in[(d+1):end]...)

    M = prod(leading)
    N = length(A)
    stride = M * d_in[d]

    B = BitArray(d_out)
    index_offset = 1 + (i-1)*M

    l = 1

    if M==1
        for j=0:stride:(N-stride)
            B[l] = A[j + index_offset]
            l += 1
        end
    else
        for j=0:stride:(N-stride)
            offs = j + index_offset
            for k=0:(M-1)
                B[l] = A[offs + k]
                l += 1
            end
        end
    end
    return B
end

function flipdim(A::BitArray, d::Integer)
    nd = ndims(A)
    sd = d > nd ? 1 : size(A, d)
    if sd == 1
        return copy(A)
    end

    B = similar(A)

    nnd = 0
    for i = 1:nd
        nnd += Int(size(A,i)==1 || i==d)
    end
    if nnd == nd
        # flip along the only non-singleton dimension
        for i = 1:sd
            B[i] = A[sd+1-i]
        end
        return B
    end

    d_in = size(A)
    leading = d_in[1:(d-1)]
    M = prod(leading)
    N = length(A)
    stride = M * sd

    if M == 1
        for j = 0:stride:(N-stride)
            for i = 1:sd
                ri = sd+1-i
                B[j + ri] = A[j + i]
            end
        end
    else
        for i = 1:sd
            ri = sd+1-i
            for j=0:stride:(N-stride)
                offs = j + 1 + (i-1)*M
                boffs = j + 1 + (ri-1)*M
                copy_chunks!(B.chunks, boffs, A.chunks, offs, M)
            end
        end
    end
    return B
end

function reverse_bits(src::UInt64)
    z    = src
    z    = ((z >>>  1) & 0x5555555555555555) | ((z <<  1) & 0xaaaaaaaaaaaaaaaa)
    z    = ((z >>>  2) & 0x3333333333333333) | ((z <<  2) & 0xcccccccccccccccc)
    z    = ((z >>>  4) & 0x0f0f0f0f0f0f0f0f) | ((z <<  4) & 0xf0f0f0f0f0f0f0f0)
    z    = ((z >>>  8) & 0x00ff00ff00ff00ff) | ((z <<  8) & 0xff00ff00ff00ff00)
    z    = ((z >>> 16) & 0x0000ffff0000ffff) | ((z << 16) & 0xffff0000ffff0000)
    return ((z >>> 32) & 0x00000000ffffffff) | ((z << 32) & 0xffffffff00000000)
end

function reverse!(B::BitVector)
    # Basic idea: each chunk is divided into two blocks of size k = n % 64, and
    # h = 64 - k. Walk from either end (with indexes i and j) reversing chunks
    # and separately ORing their two blocks into place.
    #
    #           chunk 3                  chunk 2                  chunk 1
    # ┌───────────────┬───────┐┌───────────────┬───────┐┌───────────────┬───────┐
    # │000000000000000│   E   ││       D       │   C   ││       B       │   A   │
    # └───────────────┴───────┘└───────────────┴───────┘└───────────────┴───────┘
    #                     k            h           k            h            k
    # yielding;
    # ┌───────────────┬───────┐┌───────────────┬───────┐┌───────────────┬───────┐
    # │000000000000000│  A'   ││      B'       │  C'   ││      D'       │  E'   │
    # └───────────────┴───────┘└───────────────┴───────┘└───────────────┴───────┘

    n = length(B)
    if n == 0
        return B
    end

    k = _mod64(n+63) + 1
    h = 64 - k

    i, j = 0, length(B.chunks)
    u = UInt64(0)
    v = reverse_bits(B.chunks[j])
    B.chunks[j] = 0
    @inbounds while true
        i += 1
        if i == j
            break
        end
        u = reverse_bits(B.chunks[i])
        B.chunks[i] = 0
        B.chunks[j] |= u >>> h
        B.chunks[i] |= v >>> h

        j -= 1
        if i == j
            break
        end
        v = reverse_bits(B.chunks[j])
        B.chunks[j] = 0
        B.chunks[i] |= v << k
        B.chunks[j] |= u << k
    end

    if isodd(length(B.chunks))
        B.chunks[i] |= v >>> h
    else
        B.chunks[i] |= u << k
    end

    return B
end

reverse(v::BitVector) = reverse!(copy(v))

function (<<)(B::BitVector, i::Int)
    n = length(B)
    i == 0 && return copy(B)
    A = falses(n)
    i < n && copy_chunks!(A.chunks, 1, B.chunks, i+1, n-i)
    return A
end

function (>>>)(B::BitVector, i::Int)
    n = length(B)
    i == 0 && return copy(B)
    A = falses(n)
    i < n && copy_chunks!(A.chunks, i+1, B.chunks, 1, n-i)
    return A
end

(>>)(B::BitVector, i::Int) = B >>> i

function rol!(dest::BitVector, src::BitVector, i::Integer)
    length(dest) == length(src) || throw(ArgumentError("destination and source should be of same size"))
    n = length(dest)
    i %= n
    i == 0 && return (src === dest ? src : copy!(dest, src))
    i < 0 && return ror!(dest, src, -i)
    Bc = (src === dest ? copy(src.chunks) : src.chunks)
    copy_chunks!(dest.chunks, 1, Bc, i+1, n-i)
    copy_chunks!(dest.chunks, n-i+1, Bc, 1, i)
    return dest
end

function rol!(B::BitVector, i::Integer)
    return rol!(B, B, i)
end

function rol(B::BitVector, i::Integer)
    return rol!(similar(B), B, i)
end

function ror!(dest::BitVector, src::BitVector, i::Integer)
    length(dest) == length(src) || throw(ArgumentError("destination and source should be of same size"))
    n = length(dest)
    i %= n
    i == 0 && return (src === dest ? src : copy!(dest, src))
    i < 0 && return rol!(dest, src, -i)
    Bc = (src === dest ? copy(src.chunks) : src.chunks)
    copy_chunks!(dest.chunks, i+1, Bc, 1, n-i)
    copy_chunks!(dest.chunks, 1, Bc, n-i+1, i)
    return dest
end

function ror!(B::BitVector, i::Integer)
    return ror!(B, B, i)
end

function ror(B::BitVector, i::Integer)
    return ror!(similar(B), B, i)
end

## countnz & find ##

function countnz(B::BitArray)
    n = 0
    Bc = B.chunks
    @inbounds for i = 1:length(Bc)
        n += count_ones(Bc[i])
    end
    return n
end

# returns the index of the next non-zero element, or 0 if all zeros
function findnext(B::BitArray, start::Integer)
    start > 0 || throw(BoundsError(B, start))
    start > length(B) && return 0

    Bc = B.chunks

    chunk_start = _div64(start-1)+1
    within_chunk_start = _mod64(start-1)
    mask = _msk64 << within_chunk_start

    @inbounds begin
        if Bc[chunk_start] & mask != 0
            return (chunk_start-1) << 6 + trailing_zeros(Bc[chunk_start] & mask) + 1
        end

        for i = chunk_start+1:length(Bc)
            if Bc[i] != 0
                return (i-1) << 6 + trailing_zeros(Bc[i]) + 1
            end
        end
    end
    return 0
end
#findfirst(B::BitArray) = findnext(B, 1)  ## defined in array.jl

# aux function: same as findnext(~B, start), but performed without temporaries
function findnextnot(B::BitArray, start::Integer)
    start > 0 || throw(BoundsError(B, start))
    start > length(B) && return 0

    Bc = B.chunks
    l = length(Bc)
    l == 0 && return 0

    chunk_start = _div64(start-1)+1
    within_chunk_start = _mod64(start-1)
    mask = ~(_msk64 << within_chunk_start)

    @inbounds if chunk_start < l
        if Bc[chunk_start] | mask != _msk64
            return (chunk_start-1) << 6 + trailing_ones(Bc[chunk_start] | mask) + 1
        end
        for i = chunk_start+1:l-1
            if Bc[i] != _msk64
                return (i-1) << 6 + trailing_ones(Bc[i]) + 1
            end
        end
        if Bc[l] != _msk_end(B)
            return (l-1) << 6 + trailing_ones(Bc[l]) + 1
        end
    elseif Bc[l] | mask != _msk_end(B)
        return (l-1) << 6 + trailing_ones(Bc[l] | mask) + 1
    end
    return 0
end
findfirstnot(B::BitArray) = findnextnot(B,1)

# returns the index of the first matching element
function findnext(B::BitArray, v, start::Integer)
    v == false && return findnextnot(B, start)
    v == true && return findnext(B, start)
    return 0
end
#findfirst(B::BitArray, v) = findnext(B, 1, v)  ## defined in array.jl

# returns the index of the first element for which the function returns true
function findnext(testf::Function, B::BitArray, start::Integer)
    f0::Bool = testf(false)
    f1::Bool = testf(true)
    !f0 && f1 && return findnext(B, start)
    f0 && !f1 && return findnextnot(B, start)

    start > 0 || throw(BoundsError(B, start))
    start > length(B) && return 0
    f0 && f1 && return Int(start)
    return 0 # last case: !f0 && !f1
end
#findfirst(testf::Function, B::BitArray) = findnext(testf, B, 1)  ## defined in array.jl

# returns the index of the previous non-zero element, or 0 if all zeros
function findprev(B::BitArray, start::Integer)
    start > 0 || return 0
    start > length(B) && throw(BoundsError(B, start))

    Bc = B.chunks

    chunk_start = _div64(start-1)+1
    mask = _msk_end(start)

    @inbounds begin
        if Bc[chunk_start] & mask != 0
            return (chunk_start-1) << 6 + (64 - leading_zeros(Bc[chunk_start] & mask))
        end

        for i = chunk_start-1:-1:1
            if Bc[i] != 0
                return (i-1) << 6 + (64 - leading_zeros(Bc[i]))
            end
        end
    end
    return 0
end

function findprevnot(B::BitArray, start::Integer)
    start > 0 || return 0
    start > length(B) && throw(BoundsError(B, start))

    Bc = B.chunks

    chunk_start = _div64(start-1)+1
    mask = ~_msk_end(start)

    @inbounds begin
        if Bc[chunk_start] | mask != _msk64
            return (chunk_start-1) << 6 + (64 - leading_ones(Bc[chunk_start] | mask))
        end

        for i = chunk_start-1:-1:1
            if Bc[i] != _msk64
                return (i-1) << 6 + (64 - leading_ones(Bc[i]))
            end
        end
    end
    return 0
end
findlastnot(B::BitArray) = findprevnot(B, length(B))

# returns the index of the previous matching element
function findprev(B::BitArray, v, start::Integer)
    v == false && return findprevnot(B, start)
    v == true && return findprev(B, start)
    return 0
end
#findlast(B::BitArray, v) = findprev(B, 1, v)  ## defined in array.jl

# returns the index of the previous element for which the function returns true
function findprev(testf::Function, B::BitArray, start::Integer)
    f0::Bool = testf(false)
    f1::Bool = testf(true)
    !f0 && f1 && return findprev(B, start)
    f0 && !f1 && return findprevnot(B, start)

    start > 0 || return 0
    start > length(B) && throw(BoundsError(B, start))
    f0 && f1 && return Int(start)
    return 0 # last case: !f0 && !f1
end
#findlast(testf::Function, B::BitArray) = findprev(testf, B, 1)  ## defined in array.jl

function find(B::BitArray)
    l = length(B)
    nnzB = countnz(B)
    I = Array(Int, nnzB)
    nnzB == 0 && return I
    Bc = B.chunks
    Bcount = 1
    Icount = 1
    for i = 1:length(Bc)-1
        u = UInt64(1)
        c = Bc[i]
        for j = 1:64
            if c & u != 0
                I[Icount] = Bcount
                Icount += 1
            end
            Bcount += 1
            u <<= 1
        end
    end
    u = UInt64(1)
    c = Bc[end]
    for j = 0:_mod64(l-1)
        if c & u != 0
            I[Icount] = Bcount
            Icount += 1
        end
        Bcount += 1
        u <<= 1
    end
    return I
end

findn(B::BitVector) = find(B)

function findn(B::BitMatrix)
    nnzB = countnz(B)
    I = Array(Int, nnzB)
    J = Array(Int, nnzB)
    count = 1
    for j = 1:size(B,2), i = 1:size(B,1)
        if B[i,j]
            I[count] = i
            J[count] = j
            count += 1
        end
    end
    return I, J
end

function findnz(B::BitMatrix)
    I, J = findn(B)
    return I, J, trues(length(I))
end

## Reductions ##

sum(A::BitArray, region) = reducedim(AddFun(), A, region)
sum(B::BitArray) = countnz(B)

function all(B::BitArray)
    isempty(B) && return true
    Bc = B.chunks
    @inbounds begin
        for i = 1:length(Bc)-1
            Bc[i] == _msk64 || return false
        end
        Bc[end] == _msk_end(B) || return false
    end
    return true
end

function any(B::BitArray)
    isempty(B) && return false
    Bc = B.chunks
    @inbounds begin
        for i = 1:length(Bc)
            Bc[i] == 0 || return true
        end
    end
    return false
end

minimum(B::BitArray) = isempty(B) ? throw(ArgumentError("argument must be non-empty")) : all(B)
maximum(B::BitArray) = isempty(B) ? throw(ArgumentError("argument must be non-empty")) : any(B)

## map over bitarrays ##

# Specializing map is even more important for bitarrays than it is for generic
# arrays since there can be a 64x speedup by working at the level of Int64
# instead of looping bit-by-bit.

map(f::Function, A::BitArray) = map(specialized_bitwise_unary(f), A)
map(f::Function, A::BitArray, B::BitArray) = map(specialized_bitwise_binary(f), A, B)
map(f::BitFunctorUnary, A::BitArray) = map!(f, similar(A), A)
map(f::BitFunctorBinary, A::BitArray, B::BitArray) = map!(f, similar(A), A, B)

map!(f, A::BitArray) = map!(f, A, A)
map!(f::Function, dest::BitArray, A::BitArray) = map!(specialized_bitwise_unary(f), dest, A)
map!(f::Function, dest::BitArray, A::BitArray, B::BitArray) = map!(specialized_bitwise_binary(f), dest, A, B)

# If we were able to specialize the function to a known bitwise operation,
# map across the chunks. Otherwise, fall-back to the AbstractArray method that
# iterates bit-by-bit.
function map!(f::BitFunctorUnary, dest::BitArray, A::BitArray)
    size(A) == size(dest) || throw(DimensionMismatch("sizes of dest and A must match"))
    isempty(A) && return dest
    for i=1:length(A.chunks)-1
        dest.chunks[i] = f(A.chunks[i])
    end
    dest.chunks[end] = f(A.chunks[end]) & _msk_end(A)
    dest
end
function map!(f::BitFunctorBinary, dest::BitArray, A::BitArray, B::BitArray)
    size(A) == size(B) == size(dest) || throw(DimensionMismatch("sizes of dest, A, and B must all match"))
    isempty(A) && return dest
    for i=1:length(A.chunks)-1
        dest.chunks[i] = f(A.chunks[i], B.chunks[i])
    end
    dest.chunks[end] = f(A.chunks[end], B.chunks[end]) & _msk_end(A)
    dest
end

## Filter ##

function filter(f, Bs::BitArray)
    boolmap::Array{Bool} = map(f, Bs)
    Bs[boolmap]
end

## Transpose ##

transpose(B::BitVector) = reshape(copy(B), 1, length(B))

# fast 8x8 bit transpose from Henry S. Warrens's "Hacker's Delight"
# http://www.hackersdelight.org/hdcodetxt/transpose8.c.txt
function transpose8x8(x::UInt64)
    y = x
    t = (y $ (y >>> 7)) & 0x00aa00aa00aa00aa
    y = y $ t $ (t << 7)
    t = (y $ (y >>> 14)) & 0x0000cccc0000cccc
    y = y $ t $ (t << 14)
    t = (y $ (y >>> 28)) & 0x00000000f0f0f0f0
    return y $ t $ (t << 28)
end

function form_8x8_chunk(Bc::Vector{UInt64}, i1::Int, i2::Int, m::Int, cgap::Int, cinc::Int, nc::Int, msk8::UInt64)
    x = UInt64(0)

    k, l = get_chunks_id(i1 + (i2 - 1) * m)
    r = 0
    for j = 1:8
        k > nc && break
        x |= ((Bc[k] >>> l) & msk8) << r
        if l + 8 >= 64 && nc > k
            r0 = 8 - _mod64(l + 8)
            x |= (Bc[k + 1] & (msk8 >>> r0)) << (r + r0)
        end
        k += cgap + (l + cinc >= 64 ? 1 : 0)
        l = _mod64(l + cinc)
        r += 8
    end
    return x
end

# note: assumes B is filled with 0's
function put_8x8_chunk(Bc::Vector{UInt64}, i1::Int, i2::Int, x::UInt64, m::Int, cgap::Int, cinc::Int, nc::Int, msk8::UInt64)
    k, l = get_chunks_id(i1 + (i2 - 1) * m)
    r = 0
    for j = 1:8
        k > nc && break
        Bc[k] |= ((x >>> r) & msk8) << l
        if l + 8 >= 64 && nc > k
            r0 = 8 - _mod64(l + 8)
            Bc[k + 1] |= ((x >>> (r + r0)) & (msk8 >>> r0))
        end
        k += cgap + (l + cinc >= 64 ? 1 : 0)
        l = _mod64(l + cinc)
        r += 8
    end
    return
end

function transpose(B::BitMatrix)
    l1 = size(B, 1)
    l2 = size(B, 2)
    Bt = falses(l2, l1)

    cgap1, cinc1 = _div64(l1), _mod64(l1)
    cgap2, cinc2 = _div64(l2), _mod64(l2)

    Bc = B.chunks
    Btc = Bt.chunks

    nc = length(Bc)

    for i = 1:8:l1

        msk8_1 = UInt64(0xff)
        if (l1 < i + 7)
            msk8_1 >>>= i + 7 - l1
        end

        for j = 1:8:l2
            x = form_8x8_chunk(Bc, i, j, l1, cgap1, cinc1, nc, msk8_1)
            x = transpose8x8(x)

            msk8_2 = UInt64(0xff)
            if (l2 < j + 7)
                msk8_2 >>>= j + 7 - l2
            end

            put_8x8_chunk(Btc, j, i, x, l2, cgap2, cinc2, nc, msk8_2)
        end
    end
    return Bt
end

ctranspose(B::BitArray) = transpose(B)

## Concatenation ##

function hcat(B::BitVector...)
    height = length(B[1])
    for j = 2:length(B)
        if length(B[j]) != height
            throw(DimensionMismatch("dimensions must match"))
        end
    end
    M = BitArray(height, length(B))
    for j = 1:length(B)
        copy_chunks!(M.chunks, (height*(j-1))+1, B[j].chunks, 1, height)
    end
    return M
end

function vcat(V::BitVector...)
    n = 0
    for Vk in V
        n += length(Vk)
    end
    B = BitArray(n)
    j = 1
    for Vk in V
        copy_chunks!(B.chunks, j, Vk.chunks, 1, length(Vk))
        j += length(Vk)
    end
    return B
end

function hcat(A::Union{BitMatrix,BitVector}...)
    nargs = length(A)
    nrows = size(A[1], 1)
    ncols = 0
    dense = true
    for j = 1:nargs
        Aj = A[j]
        nd = ndims(Aj)
        ncols += (nd==2 ? size(Aj,2) : 1)
        if size(Aj, 1) != nrows
            throw(DimensionMismatch("row lengths must match"))
        end
    end

    B = BitArray(nrows, ncols)

    pos = 1
    for k = 1:nargs
        Ak = A[k]
        n = length(Ak)
        copy_chunks!(B.chunks, pos, Ak.chunks, 1, n)
        pos += n
    end
    return B
end

function vcat(A::BitMatrix...)
    nargs = length(A)
    nrows = sum(a->size(a, 1), A)::Int
    ncols = size(A[1], 2)
    for j = 2:nargs
        if size(A[j], 2) != ncols
            throw(DimensionMismatch("column lengths must match"))
        end
    end
    B = BitArray(nrows, ncols)
    Bc = B.chunks
    nrowsA = [size(a, 1) for a in A]
    Ac = [a.chunks for a in A]
    pos_d = 1
    pos_s = ones(Int, nargs)
    for j = 1:ncols, k = 1:nargs
        copy_chunks!(Bc, pos_d, Ac[k], pos_s[k], nrowsA[k])
        pos_s[k] += nrowsA[k]
        pos_d += nrowsA[k]
    end
    return B
end

function cat(catdim::Integer, X::Integer...)
    reshape([X...], (ones(Int,catdim-1)..., length(X)))
end

# general case, specialized for BitArrays and Integers
function cat(catdim::Integer, X::Union{BitArray, Integer}...)
    nargs = length(X)
    # using integers results in conversion to Array{Int}
    # (except in the all-Bool case)
    has_integer = false
    for a in X
        if isa(a, Integer)
            has_integer = true; break
        end
    end
    dimsX = map((a->isa(a,BitArray) ? size(a) : (1,)), X)
    ndimsX = map((a->isa(a,BitArray) ? ndims(a) : 1), X)
    d_max = maximum(ndimsX)

    if catdim > d_max + 1
        for i = 1:nargs
            if dimsX[1] != dimsX[i]
                throw(DimensionMismatch("all inputs must have same dimensions when concatenating along a higher dimension"))
            end
        end
    elseif nargs >= 2
        for d = 1:d_max
            d == catdim && continue
            len = d <= ndimsX[1] ? dimsX[1][d] : 1
            for i = 2:nargs
                len == (d <= ndimsX[i] ? dimsX[i][d] : 1) || throw(DimensionMismatch("mismatch in dimension $d"))
            end
        end
    end

    cat_ranges = ntuple(i->(catdim <= ndimsX[i] ? dimsX[i][catdim] : 1), nargs)

    function compute_dims(d)
        if d == catdim
            catdim <= d_max && return sum(cat_ranges)
            return nargs
        else
            d <= ndimsX[1] && return dimsX[1][d]
            return 1
        end
    end

    ndimsC = max(catdim, d_max)
    dimsC = ntuple(compute_dims, ndimsC)::Tuple{Vararg{Int}}
    typeC = promote_type(map(x->isa(x,BitArray) ? eltype(x) : typeof(x), X)...)
    if !has_integer || typeC == Bool
        C = BitArray(dimsC)
    else
        C = Array(typeC, dimsC)
    end

    range = 1
    for k = 1:nargs
        nextrange = range + cat_ranges[k]
        cat_one = ntuple(i->(i != catdim ? (1:dimsC[i]) : (range:nextrange-1)),
                         ndimsC)
        # note: when C and X are BitArrays, this calls
        #       the special assign with ranges
        C[cat_one...] = X[k]
        range = nextrange
    end
    return C
end

# hvcat -> use fallbacks in abstractarray.jl


# BitArray I/O

write(s::IO, B::BitArray) = write(s, B.chunks)
read!(s::IO, B::BitArray) = read!(s, B.chunks)

sizeof(B::BitArray) = sizeof(B.chunks)
