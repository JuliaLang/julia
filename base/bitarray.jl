# preliminary definitions: constants, macros
# and functions used throughout the code
const _msk64 = ~uint64(0)
macro _mskr(l) :(_msk64 >>> (63&(64-$(esc(l))))) end
macro _div64(l) :($(esc(l)) >>> 6) end
macro _mod64(l) :($(esc(l)) & 63) end
macro _msk_end(l) :(@_mskr @_mod64 $(esc(l))) end
num_bit_chunks(n::Int) = @_div64 (n+63)
const bitcache_chunks = 64 # this can be changed
const bitcache_size = 64 * bitcache_chunks # do not change this

## BitArray

# notes: bits are stored in contiguous chunks
#        unused bits must always be set to 0
type BitArray{N} <: AbstractArray{Bool, N}
    chunks::Vector{Uint64}
    len::Int
    dims::Vector{Int}
    function BitArray(dims::Int...)
        if length(dims) != N
            error("incorrect number of dimensions")
        end
        n = prod(dims)
        nc = num_bit_chunks(n)
        chunks = Array(Uint64, nc)
        if nc > 0
            chunks[end] = uint64(0)
        end
        b = new(chunks, n)
        if N != 1
            b.dims = Int[i for i in dims]
        end
        return b
    end
end

BitArray{N}(dims::NTuple{N,Int}) = BitArray{N}(dims...)
BitArray(dims::Int...) = BitArray(dims)

typealias BitVector BitArray{1}
typealias BitMatrix BitArray{2}

## utility functions ##

length(B::BitArray) = B.len
size(B::BitVector) = (B.len,)
size(B::BitArray) = tuple(B.dims...)

size(B::BitVector, d) = (d==1 ? B.len : d>1 ? 1 : error("size: dimension out of range"))
size{N}(B::BitArray{N}, d) = (d>N ? 1 : B.dims[d])

## Aux functions ##

get_chunks_id(i::Integer) = @_div64(int(i)-1)+1, @_mod64(int(i)-1)

function glue_src_bitchunks(src::Vector{Uint64}, k::Int, ks1::Int, msk_s0::Uint64, ls0::Int)
    chunk = ((src[k] & msk_s0) >>> ls0)
    if ks1 > k && ls0 > 0
        chunk_n = (src[k + 1] & ~msk_s0)
        chunk |= (chunk_n << (64 - ls0))
    end
    return chunk
end

function copy_chunks(dest::Vector{Uint64}, pos_d::Integer, src::Vector{Uint64}, pos_s::Integer, numbits::Integer)
    if numbits == 0
        return
    end
    if dest === src && pos_d > pos_s
        return copy_chunks_rtol(dest, pos_d, pos_s, numbits)
    end

    kd0, ld0 = get_chunks_id(pos_d)
    kd1, ld1 = get_chunks_id(pos_d + numbits - 1)
    ks0, ls0 = get_chunks_id(pos_s)
    ks1, ls1 = get_chunks_id(pos_s + numbits - 1)

    delta_kd = kd1 - kd0
    delta_ks = ks1 - ks0

    u = _msk64
    if delta_kd == 0
        msk_d0 = ~(u << ld0) | (u << ld1 << 1)
    else
        msk_d0 = ~(u << ld0)
        msk_d1 = (u << ld1 << 1)
    end
    if delta_ks == 0
        msk_s0 = (u << ls0) & ~(u << ls1 << 1)
    else
        msk_s0 = (u << ls0)
    end

    chunk_s0 = glue_src_bitchunks(src, ks0, ks1, msk_s0, ls0)

    dest[kd0] = (dest[kd0] & msk_d0) | ((chunk_s0 << ld0) & ~msk_d0)

    if delta_kd == 0
        return
    end

    for i = 1 : kd1 - kd0 - 1
        chunk_s1 = glue_src_bitchunks(src, ks0 + i, ks1, msk_s0, ls0)

        chunk_s = (chunk_s0 >>> (63 - ld0) >>> 1) | (chunk_s1 << ld0)

        dest[kd0 + i] = chunk_s

        chunk_s0 = chunk_s1
    end

    if ks1 >= ks0 + delta_kd
        chunk_s1 = glue_src_bitchunks(src, ks0 + delta_kd, ks1, msk_s0, ls0)
    else
        chunk_s1 = uint64(0)
    end

    chunk_s = (chunk_s0 >>> (63 - ld0) >>> 1) | (chunk_s1 << ld0)

    dest[kd1] = (dest[kd1] & msk_d1) | (chunk_s & ~msk_d1)

    return
end

function copy_chunks_rtol(chunks::Vector{Uint64}, pos_d::Integer, pos_s::Integer, numbits::Integer)
    if pos_d == pos_s
        return
    elseif pos_d < pos_s
        return copy_chunks(chunks, pos_d, chunks, pos_s, numbits)
    end

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
            msk_d0 = ~(u << ld0) | (u << ld1 << 1)
        else
            msk_d0 = ~(u << ld0)
            msk_d1 = (u << ld1 << 1)
        end
        if delta_ks == 0
            msk_s0 = (u << ls0) & ~(u << ls1 << 1)
        else
            msk_s0 = (u << ls0)
        end

        chunk_s0 = glue_src_bitchunks(chunks, ks0, ks1, msk_s0, ls0) & ~(u << (s-1) << 1)
        chunks[kd0] = (chunks[kd0] & msk_d0) | ((chunk_s0 << ld0) & ~msk_d0)

        if delta_kd != 0
            chunk_s = (chunk_s0 >>> (63 - ld0) >>> 1)

            chunks[kd1] = (chunks[kd1] & msk_d1) | (chunk_s & ~msk_d1)
        end

        left -= s
        s = min(left, 64)
        b = left - s
        ps = pos_s + b
        pd = pos_d + b
    end
end

## custom iterator ##
start(B::BitArray) = 0
next(B::BitArray, i::Int) = (B.chunks[@_div64(i)+1] & (uint64(1)<<@_mod64(i)) != 0, i+1)
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
    if length(B) == 0
        return B
    end
    Bc = B.chunks
    if !y
        fill!(Bc, 0)
    else
        fill!(Bc, _msk64)
        Bc[end] &= @_msk_end length(B)
    end
    return B
end

falses(args...) = fill!(BitArray(args...), false)
trues(args...) = fill!(BitArray(args...), true)

function one(x::BitMatrix)
    m, n = size(x)
    a = falses(size(x))
    for i = 1 : min(m,n)
        a[i,i] = true
    end
    return a
end

function copy!(dest::BitArray, src::BitArray)
    destc = dest.chunks; srcc = src.chunks
    nc_d = length(destc)
    nc_s = length(srcc)
    nc = min(nc_s, nc_d)
    if nc == 0
        return dest
    end
    for i = 1 : nc - 1
        destc[i] = srcc[i]
    end
    if length(src) >= length(dest)
        destc[nc] = srcc[nc]
    else
        msk_s = @_msk_end length(src)
        msk_d = ~msk_s
        destc[nc] = (msk_d & destc[nc]) | (msk_s & srcc[nc])
    end
    return dest
end

function copy!(dest::BitArray, pos_d::Integer, src::BitArray, pos_s::Integer, numbits::Integer)
    if pos_s+numbits-1 > length(src) || pos_d+numbits-1 > length(dest) || pos_d < 1 || pos_s < 1
        throw(BoundsError())
    end
    copy_chunks(dest.chunks, pos_d, src.chunks, pos_s, numbits)
    return dest
end

function reshape{N}(B::BitArray, dims::NTuple{N,Int})
    if prod(dims) != length(B)
        error("reshape: invalid dimensions")
    end
    Br = BitArray{N}(ntuple(N,i->0)...)
    Br.chunks = B.chunks
    Br.len = prod(dims)
    if N != 1
        Br.dims = Int[i for i in dims]
    end
    return Br
end

## Conversions ##

convert{T,N}(::Type{Array{T}}, B::BitArray{N}) = convert(Array{T,N},B)
function convert{T,N}(::Type{Array{T,N}}, B::BitArray{N})
    A = Array(T, size(B))
    Bc = B.chunks
    for i = 1:length(A)
        A[i] = getindex_unchecked(Bc, i)
    end
    return A
end

convert{T,N}(::Type{BitArray}, A::AbstractArray{T,N}) = convert(BitArray{N},A)
function convert{T,N}(::Type{BitArray{N}}, A::AbstractArray{T,N})
    B = BitArray(size(A))
    Bc = B.chunks
    l = length(B)
    if l == 0
        return B
    end
    ind = 1
    for i = 1:length(Bc)-1
        u = uint64(1)
        c = uint64(0)
        for j = 0:63
            if bool(A[ind])
                c |= u
            end
            ind += 1
            u <<= 1
        end
        Bc[i] = c
    end
    u = uint64(1)
    c = uint64(0)
    for j = 0:@_mod64(l-1)
        if bool(A[ind])
            c |= u
        end
        ind += 1
        u <<= 1
    end
    Bc[end] = c
    return B
end

convert{N}(::Type{BitArray{N}}, B::BitArray{N}) = B

reinterpret{N}(::Type{Bool}, B::BitArray, dims::NTuple{N,Int}) = reinterpret(B, dims)
function reinterpret{N}(B::BitArray, dims::NTuple{N,Int})
    if prod(dims) != length(B)
        error("reinterpret: invalid dimensions")
    end
    A = BitArray{N}(ntuple(N,i->0)...)
    A.chunks = B.chunks
    A.len = prod(dims)
    if N != 1
        A.dims = Int[i for i in dims]
    end
    return A
end

# shorthand forms BitArray <-> Array
bitunpack{N}(B::BitArray{N}) = convert(Array{Bool,N}, B)
bitpack{T,N}(A::AbstractArray{T,N}) = convert(BitArray{N}, A)

## Random ##

function bitarray_rand_fill!(B::BitArray)
    if length(B) == 0
        return B
    end
    Bc = B.chunks
    for i = 1 : length(Bc) - 1
        Bc[i] = rand(Uint64)
    end
    msk = @_msk_end length(B)
    Bc[end] = msk & rand(Uint64)
    return B
end

## Indexing: getindex ##

function getindex_unchecked(Bc::Vector{Uint64}, i::Int)
    return (Bc[@_div64(int(i)-1)+1] & (uint64(1)<<@_mod64(int(i)-1))) != 0
end

function getindex(B::BitArray, i::Int)
    if i < 1 || i > length(B)
        throw(BoundsError())
    end
    return getindex_unchecked(B.chunks, i)
end

getindex(B::BitArray, i::Real) = getindex(B, to_index(i))

getindex(B::BitArray) = getindex(B, 1)

# 0d bitarray
getindex(B::BitArray{0}) = getindex_unchecked(B.chunks, 1)

getindex(B::BitArray, i0::Real, i1::Real) = B[to_index(i0) + size(B,1)*(to_index(i1)-1)]
getindex(B::BitArray, i0::Real, i1::Real, i2::Real) =
    B[to_index(i0) + size(B,1)*((to_index(i1)-1) + size(B,2)*(to_index(i2)-1))]
getindex(B::BitArray, i0::Real, i1::Real, i2::Real, i3::Real) =
    B[to_index(i0) + size(B,1)*((to_index(i1)-1) + size(B,2)*((to_index(i2)-1) + size(B,3)*(to_index(i3)-1)))]

function getindex(B::BitArray, I::Real...)
    ndims = length(I)
    index = to_index(I[1])
    stride = 1
    for k=2:ndims
        stride *= size(B, k - 1)
        index += (to_index(I[k]) - 1) * stride
    end
    return B[index]
end

# note: we can gain some performance if the first dimension is a range;
# TODO: extend to I:Union(Real,AbstractArray)... (i.e. not necessarily contiguous)
let getindex_cache = nothing
    global getindex
    function getindex(B::BitArray, I0::Range1{Int}, I::Union(Real,Range1{Int})...)
        # the < should become a != once
        # the stricter indexing behaviour is enforced
        if ndims(B) < 1 + length(I)
            error("wrong number of dimensions in getindex")
        end
        checkbounds(B, I0, I...)
        X = BitArray(index_shape(I0, I...))
        nI = 1 + length(I)

        I = map(x->(isa(x,Real) ? (to_index(x):to_index(x)) : indices(x)), I[1:nI-1])

        f0 = first(I0)
        l0 = length(I0)

        gap_lst = Int[last(r)-first(r)+1 for r in I]
        stride_lst = Array(Int, nI)
        stride = 1
        ind = f0
        for k = 1 : nI - 1
            stride *= size(B, k)
            stride_lst[k] = stride
            ind += stride * (first(I[k]) - 1)
            gap_lst[k] *= stride
        end
        # we only need nI-1 elements, the last one
        # is dummy (used in bodies[k,2] below)
        stride_lst[nI] = 0

        if ndims(X) == 1
            copy_chunks(X.chunks, 1, B.chunks, ind, l0)
            return X
        end

        if is(getindex_cache,nothing)
            getindex_cache = Dict()
        end

        gen_cartesian_map(getindex_cache,
            ivars->begin
                bodies = cell(nI, 2)
                bodies[1] = quote
                        copy_chunks(X.chunks, storeind, B.chunks, ind, l0)
                        storeind += l0
                        ind += stride_lst[loop_ind]
                    end
                for k = 2 : nI
                    bodies[k, 1] = quote
                        loop_ind -= 1
                    end
                    bodies[k, 2] = quote
                        ind -= gap_lst[loop_ind]
                        loop_ind += 1
                        ind += stride_lst[loop_ind]
                    end
                end
                return bodies
            end,
            I, (:B, :X, :storeind, :ind, :l0, :stride_lst, :gap_lst, :loop_ind),
            B, X, 1, ind, l0, stride_lst, gap_lst, nI)
        return X
    end
end

# note: the Range1{Int} case is still handled by the version above
#       (which is fine)
function getindex{T<:Real}(B::BitArray, I::AbstractVector{T})
    X = BitArray(length(I))
    lB = length(B)
    Xc = X.chunks
    Bc = B.chunks
    ind = 1
    for i in I
        # faster X[ind] = B[i]
        i = to_index(i)
        if i < 1 || i > lB
            throw(BoundsError())
        end
        setindex_unchecked(Xc, getindex_unchecked(Bc, i), ind)
        ind += 1
    end
    return X
end

let getindex_cache = nothing
    global getindex
    function getindex(B::BitArray, I::Union(Real,AbstractVector)...)
        I = indices(I)
        X = BitArray(index_shape(I...))
        Xc = X.chunks

        if is(getindex_cache,nothing)
            getindex_cache = Dict()
        end
        gen_cartesian_map(getindex_cache, ivars -> quote
                #faster X[storeind] = B[$(ivars...)]
                setindex_unchecked(Xc, B[$(ivars...)], ind)
                ind += 1
            end, I, (:B, :Xc, :ind), B, Xc, 1)
        return X
    end
end

# logical indexing

function getindex_bool_1d(B::BitArray, I::AbstractArray{Bool})
    n = sum(I)
    X = BitArray(n)
    lI = length(I)
    if lI != length(B)
        throw(BoundsError())
    end
    Xc = X.chunks
    Bc = B.chunks
    ind = 1
    for i = 1:length(I)
        if I[i]
            # faster X[ind] = B[i]
            setindex_unchecked(Xc, getindex_unchecked(Bc, i), ind)
            ind += 1
        end
    end
    return X
end

getindex(B::BitVector, I::AbstractVector{Bool}) = getindex_bool_1d(B, I)
getindex(B::BitVector, I::AbstractArray{Bool}) = getindex_bool_1d(B, I)
getindex(B::BitArray, I::AbstractVector{Bool}) = getindex_bool_1d(B, I)
getindex(B::BitArray, I::AbstractArray{Bool}) = getindex_bool_1d(B, I)

## Indexing: setindex! ##

function setindex_unchecked(Bc::Array{Uint64}, x::Bool, i::Int)
    i1, i2 = get_chunks_id(i)
    u = uint64(1) << i2
    if x
        Bc[i1] |= u
    else
        Bc[i1] &= ~u
    end
end

function setindex!(B::BitArray, x::Bool, i::Int)
    if i < 1 || i > length(B)
        throw(BoundsError())
    end
    setindex_unchecked(B.chunks, x, i)
    return B
end

setindex!(B::BitArray, x) = setindex!(B, x, 1)

setindex!(B::BitArray, x, i::Real) = setindex!(B, convert(Bool,x), to_index(i))

setindex!(B::BitArray, x, i0::Real, i1::Real) =
    B[to_index(i0) + size(B,1)*(to_index(i1)-1)] = x

setindex!(B::BitArray, x, i0::Real, i1::Real, i2::Real) =
    B[to_index(i0) + size(B,1)*((to_index(i1)-1) + size(B,2)*(to_index(i2)-1))] = x

setindex!(B::BitArray, x, i0::Real, i1::Real, i2::Real, i3::Real) =
    B[to_index(i0) + size(B,1)*((to_index(i1)-1) + size(B,2)*((to_index(i2)-1) + size(B,3)*(to_index(i3)-1)))] = x

function setindex!(B::BitArray, x, I0::Real, I::Real...)
    index = to_index(I0)
    stride = 1
    for k = 1:length(I)
        stride = stride * size(B, k)
        index += (to_index(I[k]) - 1) * stride
    end
    B[index] = x
    return B
end

let setindex_cache = nothing
    global setindex_array2bitarray_ranges
    function setindex_array2bitarray_ranges(B::BitArray, X::BitArray, I0::Range1{Int}, I::Range1{Int}...)
        nI = 1 + length(I)
        if ndims(B) != nI
            error("wrong number of dimensions in assigment")
        end
        lI = length(I0)
        for r in I
            lI *= length(r)
        end
        if length(X) != lI
            error("array assignment dimensions mismatch")
        end
        if lI == 0
            return B
        end
        f0 = first(I0)
        l0 = length(I0)
        if nI == 1
            copy_chunks(B.chunks, f0, X.chunks, 1, l0)
            return B
        end
        if is(setindex_cache,nothing)
            setindex_cache = Dict()
        end
        gap_lst = [last(r)-first(r)+1 for r in I]
        stride_lst = Array(Int, nI)
        stride = 1
        ind = f0
        for k = 1 : nI - 1
            stride *= size(B, k)
            stride_lst[k] = stride
            ind += stride * (first(I[k]) - 1)
            gap_lst[k] *= stride
        end
        # we only need nI-1 elements, the last one
        # is dummy (used in bodies[k,2] below)
        stride_lst[nI] = 0

        gen_cartesian_map(setindex_cache,
            ivars->begin
                bodies = cell(nI, 2)
                bodies[1] = quote
                        copy_chunks(B.chunks, ind, X.chunks, refind, l0)
                        refind += l0
                        ind += stride_lst[loop_ind]
                    end
                for k = 2 : nI
                    bodies[k, 1] = quote
                        loop_ind -= 1
                    end
                    bodies[k, 2] = quote
                        ind -= gap_lst[loop_ind]
                        loop_ind += 1
                        ind += stride_lst[loop_ind]
                    end
                end
                return bodies
            end,
            I, (:B, :X, :refind, :ind, :l0, :stride_lst, :gap_lst, :loop_ind),
            B, X, 1, ind, l0, stride_lst, gap_lst, nI)
        return B
    end
end

# note: we can gain some performance if the first dimension is a range;
#       currently this is mainly indended for the general cat case
# TODO: extend to I:Indices... (i.e. not necessarily contiguous)
function setindex!(B::BitArray, X::BitArray, I0::Range1{Int}, I::Union(Integer, Range1{Int})...)
    I = map(x->(isa(x,Integer) ? (x:x) : x), I)
    setindex_array2bitarray_ranges(B, X, I0, I...)
end

function setindex!{T<:Real}(B::BitArray, X::AbstractArray, I::AbstractVector{T})
    if length(X) != length(I); error("argument dimensions must match"); end
    count = 1
    for i in I
        B[i] = X[count]
        count += 1
    end
    return B
end

function setindex!(B::BitArray, X::AbstractArray, i0::Real)
    if length(X) != 1
        error("argument dimensions must match")
    end
    return setindex!(B, X[1], i0)
end

function setindex!(B::BitArray, X::AbstractArray, i0::Real, i1::Real)
    if length(X) != 1
        error("argument dimensions must match")
    end
    return setindex!(B, X[1], i0, i1)
end

function setindex!(B::BitArray, X::AbstractArray, I0::Real, I::Real...)
    if length(X) != 1
        error("argument dimensions must match")
    end
    return setindex!(B, X[1], i0, I...)
end

let setindex_cache = nothing
    global setindex!
    function setindex!(B::BitArray, X::AbstractArray, I::Union(Real,AbstractArray)...)
        I = indices(I)
        nel = 1
        for idx in I
            nel *= length(idx)
        end
        if length(X) != nel
            error("argument dimensions must match")
        end
        if ndims(X) > 1
            for i = 1:length(I)
                if size(X,i) != length(I[i])
                    error("argument dimensions must match")
                end
            end
        end
        if is(setindex_cache,nothing)
            setindex_cache = Dict()
        end
        gen_cartesian_map(setindex_cache,
            ivars->:(B[$(ivars...)] = X[refind]; refind += 1),
            I,
            (:B, :X, :refind),
            B, X, 1)
        return B
    end
end

function setindex!{T<:Real}(B::BitArray, x, I::AbstractVector{T})
    for i in I
        B[i] = x
    end
    return B
end

let setindex_cache = nothing
    global setindex!
    function setindex!(B::BitArray, x, I::Union(Real,AbstractArray)...)
        I = indices(I)
        if is(setindex_cache,nothing)
            setindex_cache = Dict()
        end
        gen_cartesian_map(setindex_cache, ivars->:(B[$(ivars...)] = x),
            I,
            (:B, :x),
            B, x)
        return B
    end
end

# logical indexing

function setindex_bool_scalar_1d(A::BitArray, x, I::AbstractArray{Bool})
    if length(I) > length(A)
        throw(BoundsError())
    end
    Ac = A.chunks
    for i = 1:length(I)
        if I[i]
            # faster A[i] = x
            setindex_unchecked(Ac, convert(Bool, x), i)
        end
    end
    A
end

function setindex_bool_vector_1d(A::BitArray, X::AbstractArray, I::AbstractArray{Bool})
    if length(I) > length(A)
        throw(BoundsError())
    end
    Ac = A.chunks
    c = 1
    for i = 1:length(I)
        if I[i]
            # faster A[i] = X[c]
            setindex_unchecked(Ac, convert(Bool, X[c]), i)
            c += 1
        end
    end
    A
end

setindex!(A::BitArray, X::AbstractArray, I::AbstractVector{Bool}) = setindex_bool_vector_1d(A, X, I)
setindex!(A::BitArray, X::AbstractArray, I::AbstractArray{Bool}) = setindex_bool_vector_1d(A, X, I)
setindex!(A::BitArray, x, I::AbstractVector{Bool}) = setindex_bool_scalar_1d(A, x, I)
setindex!(A::BitArray, x, I::AbstractArray{Bool}) = setindex_bool_scalar_1d(A, x, I)

setindex!(A::BitMatrix, x::AbstractArray, I::Real, J::AbstractVector{Bool}) =
    (A[I,find(J)] = x)

setindex!(A::BitMatrix, x, I::Real, J::AbstractVector{Bool}) =
    (A[I,find(J)] = x)

setindex!(A::BitMatrix, x::AbstractArray, I::AbstractVector{Bool}, J::Real) =
    (A[find(I),J] = x)

setindex!(A::BitMatrix, x, I::AbstractVector{Bool}, J::Real) =
    (A[find(I),J] = x)

setindex!(A::BitMatrix, x::AbstractArray, I::AbstractVector{Bool}, J::AbstractVector{Bool}) =
    (A[find(I),find(J)] = x)

setindex!(A::BitMatrix, x, I::AbstractVector{Bool}, J::AbstractVector{Bool}) =
    (A[find(I),find(J)] = x)

setindex!{T<:Integer}(A::BitMatrix, x::AbstractArray, I::AbstractVector{T}, J::AbstractVector{Bool}) =
    (A[I,find(J)] = x)

setindex!{T<:Real}(A::BitMatrix, x, I::AbstractVector{T}, J::AbstractVector{Bool}) =
    (A[I,find(J)] = x)

setindex!{T<:Real}(A::BitMatrix, x::AbstractArray, I::AbstractVector{Bool}, J::AbstractVector{T}) =
    (A[find(I),J] = x)

setindex!{T<:Real}(A::BitMatrix, x, I::AbstractVector{Bool}, J::AbstractVector{T}) =
    (A[find(I),J] = x)

## Dequeue functionality ##

function push!(B::BitVector, item)
    # convert first so we don't grow the bitarray if the assignment won't work
    item = convert(Bool, item)

    Bc = B.chunks

    l = @_mod64 length(B)
    if l == 0
        ccall(:jl_array_grow_end, Void, (Any, Uint), Bc, 1)
        Bc[end] = uint64(0)
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
        ccall(:jl_array_grow_end, Void, (Any, Uint), Bc, k1 - k0)
        Bc[end] = uint64(0)
    end
    B.len += n1
    copy_chunks(Bc, n0+1, items.chunks, 1, n1)
    return B
end

append!(B::BitVector, items::AbstractVector{Bool}) = append!(B, bitpack(items))
append!(A::Vector{Bool}, items::BitVector) = append!(A, bitunpack(items))

function resize!(B::BitVector, n::Integer)
    if n < 0
        throw(BoundsError())
    end
    n0 = length(B)
    if n <= n0
        splice!(B, n+1:n0)
        return B
    end
    Bc = B.chunks
    k0 = length(Bc)
    k1 = num_bit_chunks(int(n))
    if k1 > k0
        ccall(:jl_array_grow_end, Void, (Any, Uint), Bc, k1 - k0)
        Bc[end] = uint64(0)
    end
    B.len = n
    return B
end

function pop!(B::BitVector)
    if isempty(B)
        error("pop!: BitArray is empty")
    end
    item = B[end]
    B[end] = false

    l = @_mod64 length(B)
    if l == 1
        ccall(:jl_array_del_end, Void, (Any, Uint), B.chunks, 1)
    end
    B.len -= 1

    return item
end

function unshift!(B::BitVector, item)
    item = convert(Bool, item)

    Bc = B.chunks

    l = @_mod64 length(B)
    if l == 0
        ccall(:jl_array_grow_end, Void, (Any, Uint), Bc, 1)
        Bc[end] = uint64(0)
    end
    B.len += 1
    if B.len == 1
        Bc[1] = item
        return B
    end
    for i = length(Bc) : -1 : 2
        Bc[i] = (Bc[i] << 1) | (Bc[i-1] >>> 63)
    end
    Bc[1] = uint64(item) | (Bc[1] << 1)
    return B
end

function shift!(B::BitVector)
    if isempty(B)
        error("shift!: BitArray is empty")
    end
    item = B[1]

    Bc = B.chunks

    for i = 1 : length(Bc) - 1
        Bc[i] = (Bc[i] >>> 1) | (Bc[i+1] << 63)
    end

    l = @_mod64 length(B)
    if l == 1
        ccall(:jl_array_del_end, Void, (Any, Uint), Bc, 1)
    else
        Bc[end] >>>= 1
    end
    B.len -= 1

    return item
end

function insert!(B::BitVector, i::Integer, item)
    if i < 1
        throw(BoundsError())
    end
    item = convert(Bool, item)

    n = length(B)
    if i > n
        x = falses(i - n)
        append!(B, x)
    else
        Bc = B.chunks

        k, j = get_chunks_id(i)

        l = @_mod64 length(B)
        if l == 0
            ccall(:jl_array_grow_end, Void, (Any, Uint), Bc, 1)
            Bc[end] = uint64(0)
        end
        B.len += 1

        for t = length(Bc) : -1 : k + 1
            Bc[t] = (Bc[t] << 1) | (Bc[t - 1] >>> 63)
        end

        msk_aft = (_msk64 << j)
        msk_bef = ~msk_aft
        Bc[k] = (msk_bef & Bc[k]) | ((msk_aft & Bc[k]) << 1)
    end
    B[i] = item
end

function splice!(B::BitVector, i::Integer)
    n = length(B)
    if !(1 <= i <= n)
        throw(BoundsError())
    end
    v = B[i]

    k, j = get_chunks_id(i)

    msk_bef = _msk64 >>> (63 - j)
    msk_aft = ~msk_bef
    msk_bef >>>= 1

    Bc = B.chunks

    Bc[k] = (msk_bef & Bc[k]) | ((msk_aft & Bc[k]) >> 1)
    if length(Bc) > k
        Bc[k] |= (Bc[k + 1] << 63)
    end

    for t = k + 1 : length(Bc) - 1
        Bc[t] = (Bc[t] >>> 1) | (Bc[t + 1] << 63)
    end

    l = @_mod64 length(B)

    if l == 1
        ccall(:jl_array_del_end, Void, (Any, Uint), Bc, 1)
    elseif length(Bc) > k
        Bc[end] >>>= 1
    end

    B.len -= 1

    return v
end
splice!(B::BitVector, i::Integer, ins::BitVector) = splice!(B, int(i):int(i), ins)
splice!(B::BitVector, i::Integer, ins::AbstractVector{Bool}) = splice!(B, i, bitpack(ins))

const _default_bit_splice = BitVector(0)

function splice!(B::BitVector, r::Range1{Int}, ins::BitVector = _default_bit_splice)
    n = length(B)
    i_f = first(r)
    i_l = last(r)
    if !(1 <= i_f <= n+1)
        throw(BoundsError())
    end
    if !(i_l <= n)
        throw(BoundsError())
    end
    if (i_f > n)
        return append!(B, ins)
    end

    Bc = B.chunks

    lins = length(ins)
    ldel = length(r)

    new_l = length(B) + lins - ldel
    delta_k = num_bit_chunks(new_l) - length(Bc)

    if delta_k > 0
        ccall(:jl_array_grow_end, Void, (Any, Uint), Bc, delta_k)
    end
    copy_chunks(Bc, i_f+lins, Bc, i_l+1, n-i_l)
    copy_chunks(Bc, i_f, ins.chunks, 1, lins)
    if delta_k < 0
        ccall(:jl_array_del_end, Void, (Any, Uint), Bc, -delta_k)
    end

    B.len = new_l

    if new_l > 0
        Bc[end] &= @_msk_end new_l
    end

    return B
end
splice!(B::BitVector, r::Range1{Int}, ins::AbstractVector{Bool}) = splice!(B, r, bitpack(ins))

function empty!(B::BitVector)
    ccall(:jl_array_del_end, Void, (Any, Uint), B.chunks, length(B.chunks))
    B.len = 0
    return B
end

## Misc functions

for f in (:iround, :itrunc, :ifloor, :iceil, :abs)
    @eval ($f)(B::BitArray) = copy(B)
end

## Unary operators ##

function (-)(B::BitArray)
    A = zeros(Int, size(B))
    l = length(B)
    if l == 0
        return A
    end
    Bc = B.chunks
    ind = 1
    for i = 1:length(Bc)-1
        u = uint64(1)
        c = Bc[i]
        for j = 1:64
            if c & u != 0
                A[ind] = -1
            end
            ind += 1
            u <<= 1
        end
    end
    u = uint64(1)
    c = Bc[end]
    for j = 0:@_mod64(l-1)
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
        for i = 1:length(Bc)-1
            Cc[i] = ~Bc[i]
        end
        msk = @_msk_end length(B)
        Cc[end] = msk & (~Bc[end])
    end
    return C
end

function flipbits!(B::BitArray)
    Bc = B.chunks
    if !isempty(Bc)
        for i = 1:length(B.chunks) - 1
            Bc[i] = ~Bc[i]
        end
        msk = @_msk_end length(B)
        Bc[end] = msk & (~Bc[end])
    end
    return B
end

!(B::BitArray) = ~B

## Binary arithmetic operators ##

for f in (:+, :-)
    @eval begin
        function ($f)(A::BitArray, B::BitArray)
            shp = promote_shape(size(A),size(B))
            reshape(Int[ ($f)(A[i], B[i]) for i=1:length(A) ], shp)
        end
        function ($f)(B::BitArray, x::Bool)
            reshape([ ($f)(B[i], x) for i = 1:length(B) ], size(B))
        end
        function ($f)(B::BitArray, x::Number)
            pt = promote_array_type(typeof(x), Bool)
            reshape((pt)[ ($f)(B[i], x) for i = 1:length(B) ], size(B))
        end
        function ($f)(x::Bool, B::BitArray)
            reshape([ ($f)(x, B[i]) for i = 1:length(B) ], size(B))
        end
        function ($f)(x::Number, B::BitArray)
            pt = promote_array_type(typeof(x), Bool)
            reshape((pt)[ ($f)(x, B[i]) for i = 1:length(B) ], size(B))
        end
    end
end

function (./)(A::BitArray, B::BitArray)
    shp = promote_shape(size(A),size(B))
    reshape([ A[i] ./ B[i] for i=1:length(A) ], shp)
end
function (./)(B::BitArray, x::Number)
    reshape([ B[i] ./ x for i = 1:length(B) ], size(B))
end
function (./)(x::Number, B::BitArray)
    reshape([ x ./ B[i] for i = 1:length(B) ], size(B))
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
    pt = promote_array_type(typeof(x), Bool)
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
    pt = promote_array_type(typeof(x), Bool)
    y = mod(x, true)
    reshape(pt[ y for i = 1:length(B) ], size(B))
end

for f in (:div, :mod)
    @eval begin
        function ($f)(B::BitArray, x::Number)
            F = Array(promote_array_type(typeof(x), Bool), size(B))
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
            if !isempty(Ac) && !isempty(Bc)
                for i = 1:length(Fc) - 1
                    Fc[i] = ($f)(Ac[i], Bc[i])
                end
                msk = @_msk_end length(F)
                Fc[end] = msk & ($f)(Ac[end], Bc[end])
            end
            return F
        end
        ($f)(A::Array{Bool}, B::BitArray) = ($f)(bitpack(A), B)
        ($f)(B::BitArray, A::Array{Bool}) = ($f)(B, bitpack(A))
        ($f)(x::Number, B::BitArray) = ($f)(x, bitunpack(B))
        ($f)(B::BitArray, x::Number) = ($f)(bitunpack(B), x)
    end
end

function (.^)(A::BitArray, B::BitArray)
    F = BitArray(promote_shape(size(A),size(B))...)
    Fc = F.chunks
    Ac = A.chunks
    Bc = B.chunks
    if !isempty(Ac) && !isempty(Bc)
        for i = 1:length(Fc) - 1
            Fc[i] = Ac[i] | ~Bc[i]
        end
        msk = @_msk_end length(F)
        Fc[end] = msk & (Ac[end] | ~Bc[end])
    end
    return F
end
(.^)(A::Array{Bool}, B::BitArray) = (.^)(bitpack(A), B)
(.^)(B::BitArray, A::Array{Bool}) = (.^)(B, bitpack(A))
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
            z = false .^ x
        catch err
            zerr = err
        end
        try
            u = true .^ x
        catch err
            uerr = err
        end
        if zerr == nothing && uerr == nothing
            t = promote_type(typeof(z), typeof(u))
        elseif zerr == nothing
            t = typeof(z)
        else
            t = typeof(u)
        end
        F = Array(t, size(B))
        for i = 1:length(B)
            if B[i]
                if uerr == nothing
                    F[i] = u
                else
                    throw(uerr)
                end
            else
                if zerr == nothing
                    F[i] = z
                else
                    throw(zerr)
                end
            end
        end
        return F
    end
end

function bitcache_pow{T}(A::BitArray, B::Array{T}, l::Int, ind::Int, C::Vector{Bool})
    left = l - ind + 1
    for j = 1:min(bitcache_size, left)
        C[j] = bool(A[ind] .^ B[ind])
        ind += 1
    end
    C[left+1:bitcache_size] = false
    return ind
end
function (.^){T<:Integer}(A::BitArray, B::Array{T})
    F = BitArray(promote_shape(size(A),size(B)))
    Fc = F.chunks
    l = length(F)
    if l == 0
        return F
    end
    C = Array(Bool, bitcache_size)
    ind = 1
    cind = 1
    nFc = num_bit_chunks(l)
    for i = 1:div(l + bitcache_size - 1, bitcache_size)
        ind = bitcache_pow(A, B, l, ind, C)
        dumpbitcache(Fc, cind, C)
        cind += bitcache_chunks
    end
    return F
end

(.*)(A::BitArray, B::BitArray) = A & B
(.*)(A::Array{Bool}, B::BitArray) = A & B
(.*)(B::BitArray, A::Array{Bool}) = A & B
(.*)(x::Bool, B::BitArray) = x & B
(.*)(B::BitArray, x::Bool) = B & x
(.*)(x::Number, B::BitArray) = x .* bitunpack(B)
(.*)(B::BitArray, x::Number) = bitunpack(B) .* x

for f in (:+, :-, :div, :mod, :./, :.^, :.*, :&, :|, :$)
    @eval begin
        ($f)(A::BitArray, B::AbstractArray) = ($f)(bitunpack(A), B)
        ($f)(A::AbstractArray, B::BitArray) = ($f)(A, bitunpack(B))
    end
end

## promotion to complex ##

# TODO?

## element-wise comparison operators returning BitArray{Bool} ##

function dumpbitcache(Bc::Vector{Uint64}, bind::Int, C::Vector{Bool})
    ind = 1
    nc = min(@_div64(length(C)), length(Bc)-bind+1)
    for i = 1:nc
        u = uint64(1)
        c = uint64(0)
        for j = 1:64
            if C[ind]
                c |= u
            end
            ind += 1
            u <<= 1
        end
        Bc[bind] = c
        bind += 1
    end
end

for (f, cachef, scalarf) in ((:.==, :bitcache_eq , :(==)),
                             (:.< , :bitcache_lt , :<   ),
                             (:.!=, :bitcache_neq, :!=  ),
                             (:.<=, :bitcache_le , :<=  ))
    for (sigA, sigB, expA, expB, shape) in ((:AbstractArray, :AbstractArray,
                                             :(A[ind]), :(B[ind]),
                                             :(promote_shape(size(A), size(B)))),
                                            (:Any, :AbstractArray,
                                             :A, :(B[ind]),
                                             :(size(B))),
                                            (:AbstractArray, :Any,
                                             :(A[ind]), :B,
                                             :(size(A))))
        @eval begin
            function ($cachef)(A::$sigA, B::$sigB, l::Int, ind::Int, C::Vector{Bool})
                left = l - ind + 1
                for j = 1:min(bitcache_size, left)
                    C[j] = ($scalarf)($expA, $expB)
                    ind += 1
                end
                C[left+1:bitcache_size] = false
                return ind
            end
            function ($f)(A::$sigA, B::$sigB)
                F = BitArray($shape)
                Fc = F.chunks
                l = length(F)
                if l == 0
                    return F
                end
                C = Array(Bool, bitcache_size)
                ind = 1
                cind = 1
                nFc = num_bit_chunks(l)
                for i = 1:div(l + bitcache_size - 1, bitcache_size)
                    ind = ($cachef)(A, B, l, ind, C)
                    dumpbitcache(Fc, cind, C)
                    cind += bitcache_chunks
                end
                return F
            end
        end
    end
end

function (==)(A::BitArray, B::BitArray)
    if size(A) != size(B)
        return false
    end
    return A.chunks == B.chunks
end

function (!=)(A::BitArray, B::BitArray)
    if size(A) != size(B)
        return true
    end
    return A.chunks != B.chunks
end

# TODO: avoid bitpack/bitunpack
for f in (:(==), :!=)
    @eval begin
        ($f)(A::BitArray, B::AbstractArray{Bool}) = ($f)(A, bitpack(B))
        ($f)(A::AbstractArray{Bool}, B::BitArray) = ($f)(bitpack(A), B)
        ($f)(A::BitArray, B::AbstractArray) = ($f)(bitunpack(A), B)
        ($f)(A::AbstractArray, B::BitArray) = ($f)(A, bitunpack(B))
    end
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
        nnd += int(size(A,i)==1 || i==d)
    end
    if nnd==nd
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

    if M==1
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
                copy_chunks(B.chunks, boffs, A.chunks, offs, M)
            end
        end
    end
    return B
end

function reverse_bits(src::Uint64)
    z    = src
    z    = ((z >>>  1) & 0x5555555555555555) | ((z <<  1) & 0xaaaaaaaaaaaaaaaa)
    z    = ((z >>>  2) & 0x3333333333333333) | ((z <<  2) & 0xcccccccccccccccc)
    z    = ((z >>>  4) & 0x0f0f0f0f0f0f0f0f) | ((z <<  4) & 0xf0f0f0f0f0f0f0f0)
    z    = ((z >>>  8) & 0x00ff00ff00ff00ff) | ((z <<  8) & 0xff00ff00ff00ff00)
    z    = ((z >>> 16) & 0x0000ffff0000ffff) | ((z << 16) & 0xffff0000ffff0000)
    return ((z >>> 32) & 0x00000000ffffffff) | ((z << 32) & 0xffffffff00000000)
end

function reverse!(B::BitVector)
    n = length(B)
    if n == 0
        return B
    end
    pnc = length(B.chunks) & 1
    hnc = (length(B.chunks) >>> 1)

    aux_chunks = Array(Uint64, 1)

    for i = 1 : hnc
        j = ((i - 1) << 6)
        aux_chunks[1] = reverse_bits(B.chunks[i])
        copy_chunks(B.chunks, j+1, B.chunks, n-63-j, 64)
        B.chunks[i] = reverse_bits(B.chunks[i])
        copy_chunks(B.chunks, n-63-j, aux_chunks, 1, 64)
    end

    if pnc == 0
        return B
    end

    i = hnc + 1
    j = hnc << 6
    l = (@_mod64 (n+63)) + 1
    msk = @_mskr l

    aux_chunks[1] = reverse_bits(B.chunks[i] & msk)
    aux_chunks[1] >>>= (64 - l)
    copy_chunks(B.chunks, j+1, aux_chunks, 1, l)

    return B
end

reverse(v::BitVector) = reverse!(copy(v))

function (<<)(B::BitVector, i::Int64)
    n = length(B)
    i %= n
    if i == 0; return copy(B); end
    A = falses(n);
    copy_chunks(A.chunks, 1, B.chunks, i+1, n-i)
    return A
end
(<<)(B::BitVector, i::Int32) = B << int64(i)
(<<)(B::BitVector, i::Integer) = B << int64(i)

function (>>>)(B::BitVector, i::Int64)
    n = length(B)
    i %= n
    if i == 0; return copy(B); end
    A = falses(n);
    copy_chunks(A.chunks, i+1, B.chunks, 1, n-i)
    return A
end
(>>>)(B::BitVector, i::Int32) = B >>> int64(i)
(>>>)(B::BitVector, i::Integer) = B >>> int64(i)

(>>)(B::BitVector, i::Int32) = B >>> i
(>>)(B::BitVector, i::Integer) = B >>> i

function rol(B::BitVector, i::Integer)
    n = length(B)
    i %= n
    if i == 0; return copy(B); end
    A = BitArray(n);
    copy_chunks(A.chunks, 1, B.chunks, i+1, n-i)
    copy_chunks(A.chunks, n-i+1, B.chunks, 1, i)
    return A
end

function ror(B::BitVector, i::Integer)
    n = length(B)
    i %= n
    if i == 0; return copy(B); end
    A = BitArray(n);
    copy_chunks(A.chunks, i+1, B.chunks, 1, n-i)
    copy_chunks(A.chunks, 1, B.chunks, n-i+1, i)
    return A
end

#TODO: rol!, ror!

## nnz & find ##

function nnz(B::BitArray)
    n = 0
    Bc = B.chunks
    for i = 1:length(Bc)
        n += count_ones(Bc[i])
    end
    return n
end

# returns the index of the next non-zero element, or 0 if all zeros
function findnext(B::BitArray, start::Integer)
    if start < 0
        throw(BoundsError())
    elseif start > length(B)
        return 0
    end
    Bc = B.chunks

    chunk_start = @_div64(start-1)+1
    within_chunk_start = @_mod64(start-1)
    mask = _msk64 << within_chunk_start

    if Bc[chunk_start] & mask != 0
        return (chunk_start-1) << 6 + trailing_zeros(Bc[chunk_start] & mask) + 1
    end

    for i = chunk_start+1:length(Bc)
        if Bc[i] != 0
            return (i-1) << 6 + trailing_zeros(Bc[i]) + 1
        end
    end
    return 0
end
#findfirst(B::BitArray) = findnext(B, 1)  ## defined in array.jl

# aux function: same as findnext(~B, start), but performed without temporaries
function findnextnot(B::BitArray, start::Integer)
    if start < 0
        throw(BoundsError())
    elseif start > length(B)
        return 0
    end

    Bc = B.chunks
    l = length(Bc)
    if l == 0
        return 0
    end

    chunk_start = @_div64(start-1)+1
    within_chunk_start = @_mod64(start-1)
    mask = ~(_msk64 << within_chunk_start)

    if Bc[chunk_start] | mask != _msk64
        return (chunk_start-1) << 6 + trailing_ones(Bc[chunk_start] | mask) + 1
    end

    for i = chunk_start+1:l-1
        if Bc[i] != _msk64
            return (i-1) << 6 + trailing_ones(Bc[i]) + 1
        end
    end
    ce = Bc[end]
    if ce != @_msk_end length(B)
        return (l-1) << 6 + trailing_ones(ce) + 1
    end
    return 0
end
findfirstnot(B::BitArray) = findnextnot(B,1)

# returns the index of the first matching element
function findnext(B::BitArray, v, start::Integer)
    if v == false
        return findnextnot(B, start)
    elseif v == true
        return findnext(B, start)
    else
        return 0
    end
end
#findfirst(B::BitArray, v) = findnext(B, 1, v)  ## defined in array.jl

# returns the index of the first element for which the function returns true
function findnext(testf::Function, B::BitArray, start::Integer)
    f0::Bool = testf(false)
    f1::Bool = testf(true)
    if length(B) == 0 || !(f0 || f1)
        return 0
    elseif f0 && f1
        return 1
    elseif !f0 && f1
        return findnext(B, start)
    else
        return findnextnot(B, start)
    end
end
#findfirst(testf::Function, B::BitArray) = findnext(testf, B, 1)  ## defined in array.jl

function find(B::BitArray)
    l = length(B)
    nnzB = nnz(B)
    I = Array(Int, nnzB)
    if nnzB == 0
        return I
    end
    Bc = B.chunks
    Bcount = 1
    Icount = 1
    for i = 1:length(Bc)-1
        u = uint64(1)
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
    u = uint64(1)
    c = Bc[end]
    for j = 0:@_mod64(l-1)
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
    nnzB = nnz(B)
    I = Array(Int, nnzB)
    J = Array(Int, nnzB)
    count = 1
    for j=1:size(B,2), i=1:size(B,1)
        if B[i,j]
            I[count] = i
            J[count] = j
            count += 1
        end
    end
    return (I, J)
end

let findn_cache = nothing
function findn_one(ivars)
    s = { quote I[$i][count] = $(ivars[i]) end for i = 1:length(ivars)}
    quote
        Bind = B[$(ivars...)]
        if Bind
            $(s...)
            count +=1
        end
    end
end

global findn
function findn(B::BitArray)
    ndimsB = ndims(B)
    nnzB = nnz(B)
    I = ntuple(ndimsB, x->Array(Int, nnzB))
    if nnzB > 0
        ranges = ntuple(ndims(B), d->(1:size(B,d)))

        if is(findn_cache,nothing)
            findn_cache = Dict()
        end

        gen_cartesian_map(findn_cache, findn_one, ranges,
                          (:B, :I, :count), B, I, 1)
    end
    return I
end
end

function findnz(B::BitMatrix)
    I, J = findn(B)
    return (I, J, trues(length(I)))
end

nonzeros(B::BitArray) = trues(nnz(B))

## Reductions ##

sum(A::BitArray, region) = reducedim(+,A,region,0,Array(Int,reduced_dims(A,region)))

sum(B::BitArray) = nnz(B)

function all(B::BitArray)
    if length(B) == 0
        return true
    end
    Bc = B.chunks
    for i = 1:length(Bc)-1
        if Bc[i] != _msk64
            return false
        end
    end
    if Bc[end] != @_msk_end length(B)
        return false
    end
    return true
end

function any(B::BitArray)
    if length(B) == 0
        return false
    end
    Bc = B.chunks
    for i = 1:length(Bc)
        if Bc[i] != 0
            return true
        end
    end
    return false
end

min(B::BitArray) = isempty(B) ? error("min: argument is empty") : all(B)
max(B::BitArray) = isempty(B) ? error("max: argument is empty") : any(B)

## map over bitarrays ##

function map!(f::Callable, A::Union(StridedArray,BitArray))
    for i=1:length(A)
        A[i] = f(A[i])
    end
    return A
end

function map!(f::Callable, dest::Union(StridedArray,BitArray), A::Union(StridedArray,BitArray))
    for i=1:length(A)
        dest[i] = f(A[i])
    end
    return dest
end

function map!(f::Callable, dest::Union(StridedArray,BitArray), A::Union(StridedArray,BitArray), B::Union(StridedArray,BitArray))
    for i=1:length(A)
        dest[i] = f(A[i], B[i])
    end
    return dest
end

function map!(f::Callable, dest::Union(StridedArray,BitArray), A::Union(StridedArray,BitArray), B::Number)
    for i=1:length(A)
        dest[i] = f(A[i], B)
    end
    return dest
end

function map!(f::Callable, dest::Union(StridedArray,BitArray), A::Number, B::Union(StridedArray,BitArray))
    for i=1:length(B)
        dest[i] = f(A, B[i])
    end
    return dest
end

function map!(f::Callable, dest::Union(StridedArray,BitArray), As::Union(StridedArray,BitArray)...)
    n = length(As[1])
    i = 1
    ith = a->a[i]
    for i=1:n
        dest[i] = f(map(ith, As)...)
    end
    return dest
end

## Filter ##

function filter(f::Function, Bs::BitArray)
    boolmap::Array{Bool} = map(f, Bs)
    Bs[boolmap]
end

## Transpose ##

transpose(B::BitVector) = reshape(copy(B), 1, length(B))

# fast 8x8 bit transpose from Henry S. Warrens's "Hacker's Delight"
# http://www.hackersdelight.org/HDcode/transpose8.c.txt
function transpose8x8(x::Uint64)
    y = x
    t = (y $ (y >>> 7)) & 0x00aa00aa00aa00aa
    y = y $ t $ (t << 7)
    t = (y $ (y >>> 14)) & 0x0000cccc0000cccc
    y = y $ t $ (t << 14)
    t = (y $ (y >>> 28)) & 0x00000000f0f0f0f0
    return y $ t $ (t << 28)
end

function form_8x8_chunk(B::BitMatrix, i1::Int, i2::Int, m::Int, cgap::Int, cinc::Int, nc::Int, msk8::Uint64)
    x = uint64(0)

    k, l = get_chunks_id(i1 + (i2 - 1) * m)
    r = 0
    for j = 1 : 8
        if k > nc
            break
        end
        x |= ((B.chunks[k] >>> l) & msk8) << r
        if l + 8 >= 64 && nc > k
            r0 = 8 - (@_mod64 (l + 8))
            x |= (B.chunks[k + 1] & (msk8 >>> r0)) << (r + r0)
        end
        k += cgap + (l + cinc >= 64 ? 1 : 0)
        l = @_mod64 (l + cinc)
        r += 8
    end
    return x
end

# note: assumes B is filled with 0's
function put_8x8_chunk(B::BitMatrix, i1::Int, i2::Int, x::Uint64, m::Int, cgap::Int, cinc::Int, nc::Int, msk8::Uint64)
    k, l = get_chunks_id(i1 + (i2 - 1) * m)
    r = 0
    for j = 1 : 8
        if k > nc
            break
        end
        B.chunks[k] |= ((x >>> r) & msk8) << l
        if l + 8 >= 64 && nc > k
            r0 = 8 - (@_mod64 (l + 8))
            B.chunks[k + 1] |= ((x >>> (r + r0)) & (msk8 >>> r0))
        end
        k += cgap + (l + cinc >= 64 ? 1 : 0)
        l = @_mod64 (l + cinc)
        r += 8
    end
    return
end

function transpose(B::BitMatrix)
    l1 = size(B, 1)
    l2 = size(B, 2)
    Bt = falses(l2, l1)

    cgap1 = @_div64 l1
    cinc1 = @_mod64 l1

    cgap2 = @_div64 l2
    cinc2 = @_mod64 l2
    nc = length(B.chunks)

    for i = 1 : 8 : l1

        msk8_1 = uint64(0xff)
        if (l1 < i + 7)
            msk8_1 >>>= i + 7 - l1
        end

        for j = 1 : 8 : l2
            x = form_8x8_chunk(B, i, j, l1, cgap1, cinc1, nc, msk8_1)
            x = transpose8x8(x)

            msk8_2 = uint64(0xff)
            if (l2 < j + 7)
                msk8_2 >>>= j + 7 - l2
            end

            put_8x8_chunk(Bt, j, i, x, l2, cgap2, cinc2, nc, msk8_2)
        end
    end
    return Bt
end

ctranspose(B::BitArray) = transpose(B)

## Permute array dims ##

let permutedims_cache = nothing, stridenames::Array{Any,1} = {}
global permutedims
function permutedims(B::Union(BitArray,StridedArray), perm)
    dimsB = size(B)
    ndimsB = length(dimsB)
    dimsP = ntuple(ndimsB, i->dimsB[perm[i]])
    P = similar(B, dimsP)
    ranges = ntuple(ndimsB, i->(1:dimsP[i]))
    while length(stridenames) < ndimsB
        push!(stridenames, gensym())
    end

    #calculates all the strides
    if isa(B,BitArray)
        strides = [ prod(dimsB[1:(perm[dim]-1)])::Int for dim = 1:length(perm) ]
    else
        strides = [ stride(B, perm[dim]) for dim = 1:length(perm) ]
    end

    #Creates offset, because indexing starts at 1
    offset = 0
    for i in strides
        offset+=i
    end
    offset = 1-offset

    if isa(B,SubArray)
        offset += (B.first_index-1)
        B = B.parent
    end

    function permute_one_dim(ivars)
        len = length(ivars)
        counts = { symbol(string("count",i)) for i=1:len}
        toReturn = cell(len+1,2)
        for i = 1:length(toReturn)
            toReturn[i] = nothing
        end

        tmp = counts[end]
        toReturn[len+1] = quote
            ind = 1
            $tmp = $(stridenames[len])
        end

        #inner most loop
        toReturn[1] = quote
            P[ind] = B[+($(counts...))+offset]
            ind+=1
            $(counts[1]) += $(stridenames[1])
        end
        for i = 1:len-1
            tmp = counts[i]
            val = i
            toReturn[(i+1)] = quote
                $tmp = $(stridenames[val])
            end
            tmp2 = counts[i+1]
            val = i+1
            toReturn[(i+1)+(len+1)] = quote
                 $tmp2 += $(stridenames[val])
            end
        end
        toReturn
    end

    if is(permutedims_cache,nothing)
        permutedims_cache = Dict()
    end

    gen_cartesian_map(permutedims_cache, permute_one_dim, ranges,
                      tuple(:B, :P, :perm, :offset, stridenames[1:ndimsB]...),
                      B, P, perm, offset, strides...)

    return P
end
end # let

## Concatenation ##

function hcat(B::BitVector...)
    height = length(B[1])
    for j = 2:length(B)
        if length(B[j]) != height; error("hcat: mismatched dimensions"); end
    end
    M = BitArray(height, length(B))
    for j = 1:length(B)
        copy_chunks(M.chunks, (height*(j-1))+1, B[j].chunks, 1, height)
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
        copy_chunks(B.chunks, j, Vk.chunks, 1, length(Vk))
        j += length(Vk)
    end
    return B
end

function hcat(A::Union(BitMatrix,BitVector)...)
    nargs = length(A)
    nrows = size(A[1], 1)
    ncols = 0
    dense = true
    for j = 1:nargs
        Aj = A[j]
        nd = ndims(Aj)
        ncols += (nd==2 ? size(Aj,2) : 1)
        if size(Aj, 1) != nrows; error("hcat: mismatched dimensions"); end
    end

    B = BitArray(nrows, ncols)

    pos = 1
    for k=1:nargs
        Ak = A[k]
        n = length(Ak)
        copy_chunks(B.chunks, pos, Ak.chunks, 1, n)
        pos += n
    end
    return B
end

function vcat(A::BitMatrix...)
    nargs = length(A)
    nrows = sum(a->size(a, 1), A)::Int
    ncols = size(A[1], 2)
    for j = 2:nargs
        if size(A[j], 2) != ncols; error("vcat: mismatched dimensions"); end
    end
    B = BitArray(nrows, ncols)
    nrowsA = [size(a, 1) for a in A]
    pos_d = 1
    pos_s = ones(Int, nargs)
    for j = 1:ncols
        for k=1:nargs
            copy_chunks(B.chunks, pos_d, A[k].chunks, pos_s[k], nrowsA[k])
            pos_s[k] += nrowsA[k]
            pos_d += nrowsA[k]
        end
    end
    return B
end

# general case, specialized for BitArrays and Integers
function cat(catdim::Integer, X::Union(BitArray, Integer)...)
    nargs = length(X)
    # using integers results in conversion to Array{Int}
    # (except in the all-Bool case)
    has_bitarray = false
    has_integer = false
    for a in X
        if isa(a, BitArray)
            has_bitarray = true
        else
            has_integer = true
        end
    end
    # just integers and no BitArrays -> general case
    if !has_bitarray
        return invoke(cat, (Integer, Any...), catdim, X...)
    end
    dimsX = map((a->isa(a,BitArray) ? size(a) : (1,)), X)
    ndimsX = map((a->isa(a,BitArray) ? ndims(a) : 1), X)
    d_max = max(ndimsX)

    if catdim > d_max + 1
        for i=1:nargs
            if dimsX[1] != dimsX[i]
                error("cat: all inputs must have same dimensions when concatenating along a higher dimension");
            end
        end
    elseif nargs >= 2
        for d=1:d_max
            if d == catdim; continue; end
            len = d <= ndimsX[1] ? dimsX[1][d] : 1
            for i = 2:nargs
                if len != (d <= ndimsX[i] ? dimsX[i][d] : 1)
                    error("cat: dimension mismatch on dimension ", d)
                end
            end
        end
    end

    cat_ranges = ntuple(nargs, i->(catdim <= ndimsX[i] ? dimsX[i][catdim] : 1))

    function compute_dims(d)
        if d == catdim
            if catdim <= d_max
                return sum(cat_ranges)
            else
                return nargs
            end
        else
            if d <= ndimsX[1]
                return dimsX[1][d]
            else
                return 1
            end
        end
    end

    ndimsC = max(catdim, d_max)
    dimsC = ntuple(ndimsC, compute_dims)::(Int...)
    typeC = promote_type(map(x->isa(x,BitArray) ? eltype(x) : typeof(x), X)...)
    if !has_integer || typeC == Bool
        C = BitArray(dimsC)
    else
        C = Array(typeC, dimsC)
    end

    range = 1
    for k=1:nargs
        nextrange = range+cat_ranges[k]
        cat_one = ntuple(ndimsC, i->(i != catdim ?
                                     (1:dimsC[i]) : (range:nextrange-1) ))
        # note: when C and X are BitArrays, this calls
        #       the special assign with ranges
        C[cat_one...] = X[k]
        range = nextrange
    end
    return C
end

# hvcat -> use fallbacks in abstractarray.jl

isequal(A::BitArray, B::BitArray) = (A == B)

# Hashing

hash(B::BitArray) = hash((size(B), B.chunks))
