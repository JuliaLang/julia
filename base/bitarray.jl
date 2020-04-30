# This file is a part of Julia. License is MIT: https://julialang.org/license

## BitArray

# notes: bits are stored in contiguous chunks
#        unused bits must always be set to 0
"""
    BitArray{N} <: AbstractArray{Bool, N}

Space-efficient `N`-dimensional boolean array, using just one bit for each boolean value.

`BitArray`s pack up to 64 values into every 8 bytes, resulting in an 8x space efficiency
over `Array{Bool, N}` and allowing some operations to work on 64 values at once.

By default, Julia returns `BitArrays` from [broadcasting](@ref Broadcasting) operations
that generate boolean elements (including dotted-comparisons like `.==`) as well as from
the functions [`trues`](@ref) and [`falses`](@ref).

!!! note 
Due to its packed storage format, concurrent access to the elements of a `BitArray`
where at least one of them is a write is not thread safe.

"""
mutable struct BitArray{N} <: AbstractArray{Bool, N}
    chunks::Vector{UInt64}
    len::Int
    dims::NTuple{N,Int}
    function BitArray{N}(::UndefInitializer, dims::Vararg{Int,N}) where N
        n = 1
        i = 1
        for d in dims
            d >= 0 || throw(ArgumentError("dimension size must be ≥ 0, got $d for dimension $i"))
            n *= d
            i += 1
        end
        nc = num_bit_chunks(n)
        chunks = Vector{UInt64}(undef, nc)
        nc > 0 && (chunks[end] = UInt64(0))
        b = new(chunks, n)
        N != 1 && (b.dims = dims)
        return b
    end
end

# note: the docs for the two signatures are unified, but only
# the first one is recognized by the help system; it would be nice
# to fix this.
"""
    BitArray(undef, dims::Integer...)
    BitArray{N}(undef, dims::NTuple{N,Int})

Construct an undef [`BitArray`](@ref) with the given dimensions.
Behaves identically to the [`Array`](@ref) constructor. See [`undef`](@ref).

# Examples
```julia-repl
julia> BitArray(undef, 2, 2)
2×2 BitArray{2}:
 0  0
 0  0

julia> BitArray(undef, (3, 1))
3×1 BitArray{2}:
 0
 0
 0
```
"""
BitArray(::UndefInitializer, dims::Integer...) = BitArray(undef, map(Int,dims))
BitArray{N}(::UndefInitializer, dims::Integer...) where {N} = BitArray{N}(undef, map(Int,dims))
BitArray(::UndefInitializer, dims::NTuple{N,Integer}) where {N} = BitArray{N}(undef, map(Int, dims)...)
BitArray{N}(::UndefInitializer, dims::NTuple{N,Integer}) where {N} = BitArray{N}(undef, map(Int, dims)...)

const BitVector = BitArray{1}
const BitMatrix = BitArray{2}

BitVector() = BitArray{1}(undef, 0)

"""
    BitVector(nt::Tuple{Vararg{Bool}})

Construct a `BitVector` from a tuple of `Bool`.
# Examples
```julia-repl
julia> nt = (true, false, true, false)
(true, false, true, false)

julia> BitVector(nt)
4-element BitArray{1}:
 1
 0
 1
 0
```
"""
function BitVector(nt::Tuple{Vararg{Bool}})
    bv = BitVector(undef, length(nt))
    bv .= nt
end

## utility functions ##

length(B::BitArray) = B.len
size(B::BitVector) = (B.len,)
size(B::BitArray) = B.dims

@inline function size(B::BitVector, d::Integer)
    d < 1 && throw_boundserror(size(B), d)
    ifelse(d == 1, B.len, 1)
end

isassigned(B::BitArray, i::Int) = 1 <= i <= length(B)

IndexStyle(::Type{<:BitArray}) = IndexLinear()

## aux functions ##

const _msk64 = ~UInt64(0)
@inline _div64(l) = l >> 6
@inline _mod64(l) = l & 63
@inline _blsr(x)= x & (x-1) #zeros the last set bit. Has native instruction on many archs. needed in multidimensional.jl
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

copy_to_bitarray_chunks!(dest::Vector{UInt64}, pos_d::Int, src::BitArray, pos_s::Int, numbits::Int) =
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
        P8 = Ptr{UInt64}(pointer(C, ind)) # unaligned i64 pointer
        @inbounds for i = 1:nc8
            c = UInt64(0)
            for j = 0:7
                # unaligned load
                c |= (pack8bools(unsafe_load(P8, ind8)) << (j<<3))
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

## More definitions in multidimensional.jl

# auxiliary definitions used when filling a BitArray via a Vector{Bool} cache
# (e.g. when constructing from an iterable, or in broadcast!)

const bitcache_chunks = 64 # this can be changed
const bitcache_size = 64 * bitcache_chunks # do not change this

dumpbitcache(Bc::Vector{UInt64}, bind::Int, C::Vector{Bool}) =
    copy_to_bitarray_chunks!(Bc, ((bind - 1) << 6) + 1, C, 1, min(bitcache_size, (length(Bc)-bind+1) << 6))


## custom iterator ##
function iterate(B::BitArray, i::Int=0)
    i >= length(B) && return nothing
    (B.chunks[_div64(i)+1] & (UInt64(1)<<_mod64(i)) != 0, i+1)
end

## similar, fill!, copy! etc ##

similar(B::BitArray) = BitArray(undef, size(B))
similar(B::BitArray, dims::Int...) = BitArray(undef, dims)
similar(B::BitArray, dims::Dims) = BitArray(undef, dims...)

similar(B::BitArray, T::Type{Bool}, dims::Dims) = BitArray(undef, dims)
# changing type to a non-Bool returns an Array
# (this triggers conversions like float(bitvector) etc.)
similar(B::BitArray, T::Type, dims::Dims) = Array{T}(undef, dims)

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

"""
    falses(dims)

Create a `BitArray` with all values set to `false`.

# Examples
```jldoctest
julia> falses(2,3)
2×3 BitArray{2}:
 0  0  0
 0  0  0
```
"""
falses(dims::DimOrInd...) = falses(dims)
falses(dims::NTuple{N, Union{Integer, OneTo}}) where {N} = falses(map(to_dim, dims))
falses(dims::NTuple{N, Integer}) where {N} = fill!(BitArray(undef, dims), false)
falses(dims::Tuple{}) = fill!(BitArray(undef, dims), false)

"""
    trues(dims)

Create a `BitArray` with all values set to `true`.

# Examples
```jldoctest
julia> trues(2,3)
2×3 BitArray{2}:
 1  1  1
 1  1  1
```
"""
trues(dims::DimOrInd...) = trues(dims)
trues(dims::NTuple{N, Union{Integer, OneTo}}) where {N} = trues(map(to_dim, dims))
trues(dims::NTuple{N, Integer}) where {N} = fill!(BitArray(undef, dims), true)
trues(dims::Tuple{}) = fill!(BitArray(undef, dims), true)

function one(x::BitMatrix)
    m, n = size(x)
    m == n || throw(DimensionMismatch("multiplicative identity defined only for square matrices"))
    a = falses(n, n)
    for i = 1:n
        a[i,i] = true
    end
    return a
end

function copyto!(dest::BitArray, src::BitArray)
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

function unsafe_copyto!(dest::BitArray, doffs::Integer, src::Union{BitArray,Array}, soffs::Integer, n::Integer)
    copy_to_bitarray_chunks!(dest.chunks, doffs, src, soffs, n)
    return dest
end

function copyto!(dest::BitArray, doffs::Integer, src::Array, soffs::Integer, n::Integer)
    n == 0 && return dest
    soffs < 1 && throw(BoundsError(src, soffs))
    doffs < 1 && throw(BoundsError(dest, doffs))
    soffs+n-1 > length(src) && throw(BoundsError(src, length(src)+1))
    doffs+n-1 > length(dest) && throw(BoundsError(dest, length(dest)+1))
    return unsafe_copyto!(dest, doffs, src, soffs, n)
end

function copyto!(dest::BitArray, src::Array)
    length(src) > length(dest) && throw(BoundsError(dest, length(dest)+1))
    length(src) == 0 && return dest
    return unsafe_copyto!(dest, 1, src, 1, length(src))
end

function reshape(B::BitArray{N}, dims::NTuple{N,Int}) where N
    return dims == size(B) ? B : _bitreshape(B, dims)
end
reshape(B::BitArray, dims::Tuple{Vararg{Int}}) = _bitreshape(B, dims)
function _bitreshape(B::BitArray, dims::NTuple{N,Int}) where N
    prod(dims) == length(B) ||
        throw(DimensionMismatch("new dimensions $(dims) must be consistent with array size $(length(B))"))
    Br = BitArray{N}(undef, ntuple(i->0,Val(N))...)
    Br.chunks = B.chunks
    Br.len = prod(dims)
    N != 1 && (Br.dims = dims)
    return Br
end

## Constructors ##

function Array{T,N}(B::BitArray{N}) where {T,N}
    A = Array{T,N}(undef, size(B))
    Bc = B.chunks
    @inbounds for i = 1:length(A)
        A[i] = unsafe_bitgetindex(Bc, i)
    end
    return A
end

BitArray(A::AbstractArray{<:Any,N}) where {N} = BitArray{N}(A)
function BitArray{N}(A::AbstractArray{T,N}) where N where T
    B = BitArray(undef, size(A))
    Bc = B.chunks
    l = length(B)
    l == 0 && return B
    ind = 1
    @inbounds begin
        for i = 1:length(Bc)-1
            c = UInt64(0)
            for j = 0:63
                c |= (UInt64(convert(Bool, A[ind])) << j)
                ind += 1
            end
            Bc[i] = c
        end
        c = UInt64(0)
        for j = 0:_mod64(l-1)
            c |= (UInt64(convert(Bool, A[ind])) << j)
            ind += 1
        end
        Bc[end] = c
    end
    return B
end

function BitArray{N}(A::Array{Bool,N}) where N
    B = BitArray(undef, size(A))
    Bc = B.chunks
    l = length(B)
    l == 0 && return B
    copy_to_bitarray_chunks!(Bc, 1, A, 1, l)
    return B
end

reinterpret(::Type{Bool}, B::BitArray, dims::NTuple{N,Int}) where {N} = reinterpret(B, dims)
reinterpret(B::BitArray, dims::NTuple{N,Int}) where {N} = reshape(B, dims)

if nameof(@__MODULE__) === :Base  # avoid method overwrite
(::Type{T})(x::T) where {T<:BitArray} = copy(x)
BitArray(x::BitArray) = copy(x)
end

"""
    BitArray(itr)

Construct a [`BitArray`](@ref) generated by the given iterable object.
The shape is inferred from the `itr` object.

# Examples
```jldoctest
julia> BitArray([1 0; 0 1])
2×2 BitArray{2}:
 1  0
 0  1

julia> BitArray(x+y == 3 for x = 1:2, y = 1:3)
2×3 BitArray{2}:
 0  1  0
 1  0  0

julia> BitArray(x+y == 3 for x = 1:2 for y = 1:3)
6-element BitArray{1}:
 0
 1
 0
 1
 0
 0
```
"""
BitArray(itr) = gen_bitarray(IteratorSize(itr), itr)

convert(T::Type{<:BitArray}, a::AbstractArray) = a isa T ? a : T(a)

# generic constructor from an iterable without compile-time info
# (we pass start(itr) explicitly to avoid a type-instability with filters)
gen_bitarray(isz::IteratorSize, itr) = gen_bitarray_from_itr(itr)

# generic iterable with known shape
function gen_bitarray(::HasShape, itr)
    B = BitArray(undef, size(itr))
    for (I,x) in zip(CartesianIndices(axes(itr)), itr)
        B[I] = x
    end
    return B
end

# generator with known shape or length
function gen_bitarray(::HasShape, itr::Generator)
    B = BitArray(undef, size(itr))
    return fill_bitarray_from_itr!(B, itr)
end
function gen_bitarray(::HasLength, itr)
    b = BitVector(undef, length(itr))
    return fill_bitarray_from_itr!(b, itr)
end

gen_bitarray(::IsInfinite, itr) =  throw(ArgumentError("infinite-size iterable used in BitArray constructor"))

# The aux functions gen_bitarray_from_itr and fill_bitarray_from_itr! both
# use a Vector{Bool} cache for performance reasons

function gen_bitarray_from_itr(itr)
    B = empty!(BitVector(undef, bitcache_size))
    C = Vector{Bool}(undef, bitcache_size)
    Bc = B.chunks
    ind = 1
    cind = 1
    y = iterate(itr)
    while y !== nothing
        x, st = y
        @inbounds C[ind] = x
        ind += 1
        if ind > bitcache_size
            resize!(B, length(B) + bitcache_size)
            dumpbitcache(Bc, cind, C)
            cind += bitcache_chunks
            ind = 1
        end
        y = iterate(itr, st)
    end
    if ind > 1
        @inbounds C[ind:bitcache_size] .= false
        resize!(B, length(B) + ind - 1)
        dumpbitcache(Bc, cind, C)
    end
    return B
end

function fill_bitarray_from_itr!(B::BitArray, itr)
    n = length(B)
    C = Vector{Bool}(undef, bitcache_size)
    Bc = B.chunks
    ind = 1
    cind = 1
    y = iterate(itr)
    while y !== nothing
        x, st = y
        @inbounds C[ind] = x
        ind += 1
        if ind > bitcache_size
            dumpbitcache(Bc, cind, C)
            cind += bitcache_chunks
            ind = 1
        end
        y = iterate(itr, st)
    end
    if ind > 1
        @inbounds C[ind:bitcache_size] .= false
        dumpbitcache(Bc, cind, C)
    end
    return B
end


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
    _unsafe_bitsetindex!(Bc, x, i1, i2)
end

@inline function _unsafe_bitsetindex!(Bc::Array{UInt64}, x::Bool, i1::Int, i2::Int)
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

indexoffset(i) = first(i)-1
indexoffset(::Colon) = 0

@propagate_inbounds function setindex!(B::BitArray, X::AbstractArray, J0::Union{Colon,UnitRange{Int}})
    _setindex!(IndexStyle(B), B, X, to_indices(B, (J0,))[1])
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
        _growend!(Bc, 1)
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
    n1 == 0 && return B
    Bc = B.chunks
    k0 = length(Bc)
    k1 = num_bit_chunks(n0 + n1)
    if k1 > k0
        _growend!(Bc, k1 - k0)
        Bc[end] = UInt64(0)
    end
    B.len += n1
    copy_chunks!(Bc, n0+1, items.chunks, 1, n1)
    return B
end

append!(B::BitVector, items) = append!(B, BitArray(items))
append!(A::Vector{Bool}, items::BitVector) = append!(A, Array(items))

function prepend!(B::BitVector, items::BitVector)
    n0 = length(B)
    n1 = length(items)
    n1 == 0 && return B
    Bc = B.chunks
    k0 = length(Bc)
    k1 = num_bit_chunks(n0 + n1)
    if k1 > k0
        _growend!(Bc, k1 - k0)
        Bc[end] = UInt64(0)
    end
    B.len += n1
    copy_chunks!(Bc, 1 + n1, Bc, 1, n0)
    copy_chunks!(Bc, 1, items.chunks, 1, n1)
    return B
end

prepend!(B::BitVector, items) = prepend!(B, BitArray(items))
prepend!(A::Vector{Bool}, items::BitVector) = prepend!(A, Array(items))

function sizehint!(B::BitVector, sz::Integer)
    ccall(:jl_array_sizehint, Cvoid, (Any, UInt), B.chunks, num_bit_chunks(sz))
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
        _growend!(Bc, k1 - k0)
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
    l == 1 && _deleteend!(B.chunks, 1)
    B.len -= 1

    return item
end

function pushfirst!(B::BitVector, item)
    item = convert(Bool, item)

    Bc = B.chunks

    l = _mod64(length(B))
    if l == 0
        _growend!(Bc, 1)
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

function popfirst!(B::BitVector)
    isempty(B) && throw(ArgumentError("argument must not be empty"))
    @inbounds begin
        item = B[1]

        Bc = B.chunks

        for i = 1 : length(Bc) - 1
            Bc[i] = (Bc[i] >>> 1) | (Bc[i+1] << 63)
        end

        l = _mod64(length(B))
        if l == 1
            _deleteend!(Bc, 1)
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
        _growend!(Bc, 1)
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
            _deleteend!(Bc, 1)
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

    delta_k < 0 && _deleteend!(Bc, -delta_k)

    B.len = new_l

    if new_l > 0
        Bc[end] &= _msk_end(new_l)
    end

    return B
end

function deleteat!(B::BitVector, inds)
    n = new_l = length(B)
    y = iterate(inds)
    y === nothing && return B
    n == 0 && throw(BoundsError(B, inds))

    Bc = B.chunks

    (p, s) = y
    q = p+1
    new_l -= 1
    y = iterate(inds, s)
    while y !== nothing
        (i, s) = y
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
        y = iterate(inds, s)
    end

    q <= n && copy_chunks!(Bc, p, Bc, q, n-q+1)

    delta_k = num_bit_chunks(new_l) - length(Bc)
    delta_k < 0 && _deleteend!(Bc, -delta_k)

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

const _default_bit_splice = BitVector()

function splice!(B::BitVector, r::Union{UnitRange{Int}, Integer}, ins::AbstractArray = _default_bit_splice)
    n = length(B)
    i_f = first(r)
    i_l = last(r)

    1 <= i_f <= n+1 || throw(BoundsError(B, i_f))
    i_l <= n || throw(BoundsError(B, n+1))

    Bins = convert(BitArray, ins)

    if (i_f > n)
        append!(B, Bins)
        return BitVector()
    end

    v = B[r]  # TODO: change to a copy if/when subscripting becomes an ArrayView

    Bc = B.chunks

    lins = length(Bins)
    ldel = length(r)

    new_l = length(B) + lins - ldel
    delta_k = num_bit_chunks(new_l) - length(Bc)

    delta_k > 0 && _growend!(Bc, delta_k)

    copy_chunks!(Bc, i_f+lins, Bc, i_l+1, n-i_l)
    copy_chunks!(Bc, i_f, Bins.chunks, 1, lins)

    delta_k < 0 && _deleteend!(Bc, -delta_k)

    B.len = new_l

    if new_l > 0
        Bc[end] &= _msk_end(new_l)
    end

    return v
end

function splice!(B::BitVector, r::Union{UnitRange{Int}, Integer}, ins)
    Bins = BitVector(undef, length(ins))
    i = 1
    for x in ins
        Bins[i] = Bool(x)
        i += 1
    end
    return splice!(B, r, Bins)
end


function empty!(B::BitVector)
    _deleteend!(B.chunks, length(B.chunks))
    B.len = 0
    return B
end

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

## Binary arithmetic operators ##

for f in (:+, :-)
    @eval function ($f)(A::BitArray, B::BitArray)
        r = Array{Int}(undef, promote_shape(size(A), size(B)))
        ay, by = iterate(A), iterate(B)
        ri = 1
        # promote_shape guarantees that A and B have the
        # same iteration space
        while ay !== nothing
            @inbounds r[ri] = ($f)(ay[1], by[1])
            ri += 1
            ay, by = iterate(A, ay[2]), iterate(B, by[2])
        end
        return r
    end
end

for f in (:/, :\)
    @eval begin
        ($f)(A::Union{BitMatrix,BitVector}, B::Union{BitMatrix,BitVector}) = ($f)(Array(A), Array(B))
    end
end
(/)(B::BitArray, x::Number) = (/)(Array(B), x)
(/)(x::Number, B::BitArray) = (/)(x, Array(B))

## promotion to complex ##

# TODO?

## comparison operators ##

function (==)(A::BitArray, B::BitArray)
    size(A) != size(B) && return false
    return A.chunks == B.chunks
end


## Data movement ##

# TODO some of this could be optimized

function reverse(A::BitArray; dims::Integer)
    nd = ndims(A); d = dims
    1 ≤ d ≤ nd || throw(ArgumentError("dimension $d is not 1 ≤ $d ≤ $nd"))
    sd = size(A, d)
    sd == 1 && return copy(A)

    B = similar(A)

    nnd = 0
    for i = 1:nd
        nnd += Int(size(A,i)==1 || i==d)
    end
    if nnd == nd
        # reverse along the only non-singleton dimension
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

function reverse!(B::BitVector)
    # Basic idea: each chunk is divided into two blocks of size k = n % 64, and
    # h = 64 - k. Walk from either end (with indices i and j) reversing chunks
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
    n == 0 && return B

    k = _mod64(n+63) + 1
    h = 64 - k

    i, j = 0, length(B.chunks)
    u = UInt64(0)
    v = bitreverse(B.chunks[j])
    B.chunks[j] = 0
    @inbounds while true
        i += 1
        if i == j
            break
        end
        u = bitreverse(B.chunks[i])
        B.chunks[i] = 0
        B.chunks[j] |= u >>> h
        B.chunks[i] |= v >>> h

        j -= 1
        if i == j
            break
        end
        v = bitreverse(B.chunks[j])
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


function (<<)(B::BitVector, i::UInt)
    n = length(B)
    i == 0 && return copy(B)
    A = falses(n)
    i < n && copy_chunks!(A.chunks, 1, B.chunks, i+1, n-i)
    return A
end

function (>>>)(B::BitVector, i::UInt)
    n = length(B)
    i == 0 && return copy(B)
    A = falses(n)
    i < n && copy_chunks!(A.chunks, i+1, B.chunks, 1, n-i)
    return A
end

"""
    >>(B::BitVector, n) -> BitVector

Right bit shift operator, `B >> n`. For `n >= 0`, the result is `B`
with elements shifted `n` positions forward, filling with `false`
values. If `n < 0`, elements are shifted backwards. Equivalent to
`B << -n`.

# Examples
```jldoctest
julia> B = BitVector([true, false, true, false, false])
5-element BitArray{1}:
 1
 0
 1
 0
 0

julia> B >> 1
5-element BitArray{1}:
 0
 1
 0
 1
 0

julia> B >> -1
5-element BitArray{1}:
 0
 1
 0
 0
 0
```
"""
(>>)(B::BitVector, i::Union{Int, UInt}) = B >>> i

# signed integer version of shift operators with handling of negative values
"""
    <<(B::BitVector, n) -> BitVector

Left bit shift operator, `B << n`. For `n >= 0`, the result is `B`
with elements shifted `n` positions backwards, filling with `false`
values. If `n < 0`, elements are shifted forwards. Equivalent to
`B >> -n`.

# Examples
```jldoctest
julia> B = BitVector([true, false, true, false, false])
5-element BitArray{1}:
 1
 0
 1
 0
 0

julia> B << 1
5-element BitArray{1}:
 0
 1
 0
 0
 0

julia> B << -1
5-element BitArray{1}:
 0
 1
 0
 1
 0
```
"""
(<<)(B::BitVector, i::Int) = (i >=0 ? B << unsigned(i) : B >> unsigned(-i))

"""
    >>>(B::BitVector, n) -> BitVector

Unsigned right bitshift operator, `B >>> n`. Equivalent to `B >> n`. See [`>>`](@ref) for
details and examples.
"""
(>>>)(B::BitVector, i::Int) = (i >=0 ? B >> unsigned(i) : B << unsigned(-i))

function circshift!(dest::BitVector, src::BitVector, i::Integer)
    length(dest) == length(src) || throw(ArgumentError("destination and source should be of same size"))
    n = length(dest)
    i %= n
    i == 0 && return (src === dest ? src : copyto!(dest, src))
    Bc = (src === dest ? copy(src.chunks) : src.chunks)
    if i > 0 # right
        copy_chunks!(dest.chunks, i+1, Bc, 1, n-i)
        copy_chunks!(dest.chunks, 1, Bc, n-i+1, i)
    else # left
        i = -i
        copy_chunks!(dest.chunks, 1, Bc, i+1, n-i)
        copy_chunks!(dest.chunks, n-i+1, Bc, 1, i)
    end
    return dest
end

circshift!(B::BitVector, i::Integer) = circshift!(B, B, i)

## count & find ##

function bitcount(Bc::Vector{UInt64})
    n = 0
    @inbounds for i = 1:length(Bc)
        n += count_ones(Bc[i])
    end
    return n
end

count(B::BitArray) = bitcount(B.chunks)

function unsafe_bitfindnext(Bc::Vector{UInt64}, start::Integer)
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
    return nothing
end

# returns the index of the next true element, or nothing if all false
function findnext(B::BitArray, start::Integer)
    start > 0 || throw(BoundsError(B, start))
    start > length(B) && return nothing
    unsafe_bitfindnext(B.chunks, start)
end

#findfirst(B::BitArray) = findnext(B, 1)  ## defined in array.jl

# aux function: same as findnext(~B, start), but performed without temporaries
function findnextnot(B::BitArray, start::Integer)
    start > 0 || throw(BoundsError(B, start))
    start > length(B) && return nothing

    Bc = B.chunks
    l = length(Bc)
    l == 0 && return nothing

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
    return nothing
end
findfirstnot(B::BitArray) = findnextnot(B,1)

# returns the index of the first matching element
function findnext(pred::Fix2{<:Union{typeof(isequal),typeof(==)},Bool},
                  B::BitArray, start::Integer)
    v = pred.x
    v == false && return findnextnot(B, start)
    v == true && return findnext(B, start)
    return nothing
end
#findfirst(B::BitArray, v) = findnext(B, 1, v)  ## defined in array.jl

# returns the index of the first element for which the function returns true
function findnext(testf::Function, B::BitArray, start::Integer)
    f0::Bool = testf(false)
    f1::Bool = testf(true)
    !f0 && f1 && return findnext(B, start)
    f0 && !f1 && return findnextnot(B, start)

    start > 0 || throw(BoundsError(B, start))
    start > length(B) && return nothing
    f0 && f1 && return Int(start)
    return nothing # last case: !f0 && !f1
end
#findfirst(testf::Function, B::BitArray) = findnext(testf, B, 1)  ## defined in array.jl

function unsafe_bitfindprev(Bc::Vector{UInt64}, start::Integer)
    chunk_start = _div64(start-1)+1
    mask = _msk_end(start)

    @inbounds begin
        if Bc[chunk_start] & mask != 0
            return (chunk_start-1) << 6 + (64 - leading_zeros(Bc[chunk_start] & mask))
        end

        for i = (chunk_start-1):-1:1
            if Bc[i] != 0
                return (i-1) << 6 + (64 - leading_zeros(Bc[i]))
            end
        end
    end
    return nothing
end

# returns the index of the previous true element, or nothing if all false
function findprev(B::BitArray, start::Integer)
    start > 0 || return nothing
    start > length(B) && throw(BoundsError(B, start))
    unsafe_bitfindprev(B.chunks, start)
end

function findprevnot(B::BitArray, start::Integer)
    start > 0 || return nothing
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
    return nothing
end
findlastnot(B::BitArray) = findprevnot(B, length(B))

# returns the index of the previous matching element
function findprev(pred::Fix2{<:Union{typeof(isequal),typeof(==)},Bool},
                  B::BitArray, start::Integer)
    v = pred.x
    v == false && return findprevnot(B, start)
    v == true && return findprev(B, start)
    return nothing
end
#findlast(B::BitArray, v) = findprev(B, 1, v)  ## defined in array.jl

# returns the index of the previous element for which the function returns true
function findprev(testf::Function, B::BitArray, start::Integer)
    f0::Bool = testf(false)
    f1::Bool = testf(true)
    !f0 && f1 && return findprev(B, start)
    f0 && !f1 && return findprevnot(B, start)

    start > 0 || return nothing
    start > length(B) && throw(BoundsError(B, start))
    f0 && f1 && return Int(start)
    return nothing # last case: !f0 && !f1
end
#findlast(testf::Function, B::BitArray) = findprev(testf, B, 1)  ## defined in array.jl

function findmax(a::BitArray)
    isempty(a) && throw(ArgumentError("BitArray must be non-empty"))
    m, mi = false, 1
    ti = 1
    ac = a.chunks
    for i = 1:length(ac)
        @inbounds k = trailing_zeros(ac[i])
        ti += k
        k == 64 || return (true, @inbounds keys(a)[ti])
    end
    return m, @inbounds keys(a)[mi]
end

function findmin(a::BitArray)
    isempty(a) && throw(ArgumentError("BitArray must be non-empty"))
    m, mi = true, 1
    ti = 1
    ac = a.chunks
    for i = 1:length(ac)-1
        @inbounds k = trailing_ones(ac[i])
        ti += k
        k == 64 || return (false, @inbounds keys(a)[ti])
    end
    l = Base._mod64(length(a)-1) + 1
    @inbounds k = trailing_ones(ac[end] & Base._msk_end(l))
    ti += k
    k == l || return (false, @inbounds keys(a)[ti])
    return (m, @inbounds keys(a)[mi])
end

# findall helper functions
# Generic case (>2 dimensions)
function allindices!(I, B::BitArray)
    ind = first(keys(B))
    for k = 1:length(B)
        I[k] = ind
        ind = nextind(B, ind)
    end
end

# Optimized case for vector
function allindices!(I, B::BitVector)
    I[:] .= 1:length(B)
end

# Optimized case for matrix
function allindices!(I, B::BitMatrix)
    k = 1
    for c = 1:size(B,2), r = 1:size(B,1)
        I[k] = CartesianIndex(r, c)
        k += 1
    end
end

@inline _overflowind(i1, irest::Tuple{}, size) = (i1, irest)
@inline function _overflowind(i1, irest, size)
    i2 = irest[1]
    while i1 > size[1]
        i1 -= size[1]
        i2 += 1
    end
    i2, irest = _overflowind(i2, tail(irest), tail(size))
    return (i1, (i2, irest...))
end

@inline _toind(i1, irest::Tuple{}) = i1
@inline _toind(i1, irest) = CartesianIndex(i1, irest...)

function findall(B::BitArray)
    nnzB = count(B)
    I = Vector{eltype(keys(B))}(undef, nnzB)
    nnzB == 0 && return I
    nnzB == length(B) && (allindices!(I, B); return I)
    Bc = B.chunks
    Bs = size(B)
    Bi = i1 = i = 1
    irest = ntuple(one, ndims(B) - 1)
    c = Bc[1]
    @inbounds while true
        while c == 0
            Bi == length(Bc) && return I
            i1 += 64
            Bi += 1
            c = Bc[Bi]
        end

        tz = trailing_zeros(c)
        c = _blsr(c)

        i1, irest = _overflowind(i1 + tz, irest, Bs)
        I[i] = _toind(i1, irest)
        i += 1
        i1 -= tz
    end
end

# For performance
findall(::typeof(!iszero), B::BitArray) = findall(B)

## Reductions ##

_sum(A::BitArray, dims)    = reduce(+, A, dims=dims)
_sum(B::BitArray, ::Colon) = count(B)

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

map(::Union{typeof(~), typeof(!)}, A::BitArray) = bit_map!(~, similar(A), A)
map(::typeof(zero), A::BitArray) = fill!(similar(A), false)
map(::typeof(one), A::BitArray) = fill!(similar(A), true)
map(::typeof(identity), A::BitArray) = copy(A)

map!(::Union{typeof(~), typeof(!)}, dest::BitArray, A::BitArray) = bit_map!(~, dest, A)
map!(::typeof(zero), dest::BitArray, A::BitArray) = fill!(dest, false)
map!(::typeof(one), dest::BitArray, A::BitArray) = fill!(dest, true)
map!(::typeof(identity), dest::BitArray, A::BitArray) = copyto!(dest, A)

for (T, f) in ((:(Union{typeof(&), typeof(*), typeof(min)}), :(&)),
               (:(Union{typeof(|), typeof(max)}),            :(|)),
               (:(Union{typeof(xor), typeof(!=)}),           :xor),
               (:(Union{typeof(>=), typeof(^)}),             :((p, q) -> p | ~q)),
               (:(typeof(<=)),                               :((p, q) -> ~p | q)),
               (:(typeof(==)),                               :((p, q) -> ~xor(p, q))),
               (:(typeof(<)),                                :((p, q) -> ~p & q)),
               (:(typeof(>)),                                :((p, q) -> p & ~q)))
    @eval map(::$T, A::BitArray, B::BitArray) = bit_map!($f, similar(A), A, B)
    @eval map!(::$T, dest::BitArray, A::BitArray, B::BitArray) = bit_map!($f, dest, A, B)
end

# If we were able to specialize the function to a known bitwise operation,
# map across the chunks. Otherwise, fall-back to the AbstractArray method that
# iterates bit-by-bit.
function bit_map!(f::F, dest::BitArray, A::BitArray) where F
    size(A) == size(dest) || throw(DimensionMismatch("sizes of dest and A must match"))
    isempty(A) && return dest
    destc = dest.chunks
    Ac = A.chunks
    for i = 1:(length(Ac)-1)
        destc[i] = f(Ac[i])
    end
    destc[end] = f(Ac[end]) & _msk_end(A)
    dest
end
function bit_map!(f::F, dest::BitArray, A::BitArray, B::BitArray) where F
    size(A) == size(B) == size(dest) || throw(DimensionMismatch("sizes of dest, A, and B must all match"))
    isempty(A) && return dest
    destc = dest.chunks
    Ac = A.chunks
    Bc = B.chunks
    for i = 1:(length(Ac)-1)
        destc[i] = f(Ac[i], Bc[i])
    end
    destc[end] = f(Ac[end], Bc[end]) & _msk_end(A)
    dest
end

## Filter ##

function filter(f, Bs::BitArray)
    boolmap::Array{Bool} = map(f, Bs)
    Bs[boolmap]
end


## Concatenation ##

function hcat(B::BitVector...)
    height = length(B[1])
    for j = 2:length(B)
        length(B[j]) == height ||
            throw(DimensionMismatch("dimensions must match"))
    end
    M = BitMatrix(undef, height, length(B))
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
    B = BitVector(undef, n)
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
        size(Aj, 1) == nrows ||
            throw(DimensionMismatch("row lengths must match"))
    end

    B = BitMatrix(undef, nrows, ncols)

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
        size(A[j], 2) == ncols ||
            throw(DimensionMismatch("column lengths must match"))
    end
    B = BitMatrix(undef, nrows, ncols)
    Bc = B.chunks
    nrowsA = [size(a, 1) for a in A]
    Ac = [a.chunks for a in A]
    pos_d = 1
    pos_s = fill(1, nargs)
    for j = 1:ncols, k = 1:nargs
        copy_chunks!(Bc, pos_d, Ac[k], pos_s[k], nrowsA[k])
        pos_s[k] += nrowsA[k]
        pos_d += nrowsA[k]
    end
    return B
end

# general case, specialized for BitArrays and Integers
function _cat(dims::Integer, X::Union{BitArray, Bool}...)
    catdims = dims2cat(dims)
    shape = cat_shape(catdims, (), map(cat_size, X)...)
    A = falses(shape)
    return __cat(A, shape, catdims, X...)
end

# hvcat -> use fallbacks in abstractarray.jl


# BitArray I/O

write(s::IO, B::BitArray) = write(s, B.chunks)
function read!(s::IO, B::BitArray)
    n = length(B)
    Bc = B.chunks
    nc = length(read!(s, Bc))
    if length(Bc) > 0 && Bc[end] & _msk_end(n) ≠ Bc[end]
        Bc[end] &= _msk_end(n) # ensure that the BitArray is not broken
        throw(DimensionMismatch("read mismatch, found non-zero bits after BitArray length"))
    end
    return B
end

sizeof(B::BitArray) = sizeof(B.chunks)
