_jl_num_bit_chunks(n::Integer) = (n + 0x3f) >>> 6

# notes: bits are stored in contiguous chunks
#        unused bits must always be set to 0
type BitArray{N} <: AbstractArray{Int, N}
    chunks::Vector{Uint64}
    dims::Vector{Int}
    function BitArray(dims::Int...)
        if length(dims) == 0
            dims = 0
        end
        n = prod(dims)
        nc = _jl_num_bit_chunks(n)
        chunks = Array(Uint64, nc)
        if nc > 0
            chunks[end] = uint64(0)
        end
        new(chunks, [i::Int | i=dims])
    end
end

BitArray() = BitArray{1}(0)
BitArray(dims::Dims) = BitArray{max(length(dims), 1)}(dims...)
BitArray(dims::Int...) = BitArray{max(length(dims), 1)}(dims...)

typealias BitVector BitArray{1}
typealias BitMatrix BitArray{2}

# non-standard compact representation

function _jl_print_bit_chunk(c::Uint64, l::Integer)
    for s = 0 : l - 1
        d = (c >>> s) & 1
        print("01"[d + 1])
        if (s + 1) & 7 == 0
            print(" ")
        end
    end
end

_jl_print_bit_chunk(c::Uint64) = _jl_print_bit_chunk(c, 64)

function bitshow(B::BitArray)
    if length(B) == 0
        return
    end
    for i = 1 : length(B.chunks) - 1
        _jl_print_bit_chunk(B.chunks[i])
        print(": ")
    end
    l = ((length(B) - 1) & 0x3f) + 1
    _jl_print_bit_chunk(B.chunks[end], l)
end

# standard representation header

summary(B::BitArray) = 
    strcat(dims2string(size(B)), " BitArray")

## utility functions ##

length(B::BitArray) = prod(B.dims)
eltype(B::BitArray) = Int
ndims{N}(B::BitArray{N}) = N
numel(B::BitArray) = prod(B.dims)
size(B::BitArray) = tuple(B.dims...)
isinteger(B::BitArray) = true
isreal(B::BitArray) = true
iscomplex(B::BitArray) = false
isbool(B::BitArray) = false

## Aux functions ##

function _jl_get_chunks_id(i::Integer)
    j = uint64(i - 1)
    return int(j >>> 6) + 1, int(j & 0x3f)
end

function _jl_glue_src_bitchunks(src::Vector{Uint64}, k::Int, ks1::Int, msk_s0::Uint64, ls0::Int)
    chunk = ((src[k] & msk_s0) >>> ls0)
    if ks1 > k && ls0 > 0
        chunk_n = (src[k + 1] & ~msk_s0)
        chunk |= (chunk_n << (64 - ls0))
    end
    return chunk
end

function _jl_copy_chunks(dest::Vector{Uint64}, pos_d::Integer, src::Vector{Uint64}, pos_s::Integer, numbits::Integer)
    if numbits == 0
        return
    end

    kd0, ld0 = _jl_get_chunks_id(pos_d)
    kd1, ld1 = _jl_get_chunks_id(pos_d + numbits - 1)
    ks0, ls0 = _jl_get_chunks_id(pos_s)
    ks1, ls1 = _jl_get_chunks_id(pos_s + numbits - 1)

    delta_kd = kd1 - kd0
    delta_ks = ks1 - ks0

    u = ~(uint64(0))
    if delta_kd ==  0
        msk_d0 = ((u >>> (63 - ld0) >>> 1) | (u << ld1 << 1))
    else
        msk_d0 = (u >>> (63 - ld0) >>> 1)
        msk_d1 = ~(u >>> (63 - ld1))
    end
    if delta_ks == 0
        msk_s0 = ~((u >>> (63 - ls0) >>> 1) | (u << ls1 << 1))
    else
        msk_s0 = ~(u >>> (63 - ls0) >>> 1)
    end

    chunk_s0 = _jl_glue_src_bitchunks(src, ks0, ks1, msk_s0, ls0)

    dest[kd0] = (dest[kd0] & msk_d0) | ((chunk_s0 << ld0) & ~msk_d0)

    if delta_kd == 0
        return
    end

    for i = 1 : kd1 - kd0 - 1
        chunk_s1 = _jl_glue_src_bitchunks(src, ks0 + i, ks1, msk_s0, ls0)

        chunk_s = (chunk_s0 >>> (63 - ld0) >>> 1) | (chunk_s1 << ld0)

        dest[kd0 + i] = chunk_s

        chunk_s0 = chunk_s1
    end

    if ks1 >= ks0 + delta_kd
        chunk_s1 = _jl_glue_src_bitchunks(src, ks0 + delta_kd, ks1, msk_s0, ls0)
    else
        chunk_s1 = uint64(0)
    end

    chunk_s = (chunk_s0 >>> (63 - ld0) >>> 1) | (chunk_s1 << ld0)

    dest[kd1] = (dest[kd1] & msk_d1) | (chunk_s & ~msk_d1)

    return
end

_jl_copy_chunks(dest::Vector{Uint64}, pos_d::Int, src::Vector{Uint64}, numbits::Int) =
    _jl_copy_chunks(dest, pos_d, src, 1, numbits)


## similar, fill, copy_to etc ##

similar(B::BitArray) = BitArray(B.dims...)
similar(B::BitArray, dims::Int...) = BitArray(dims)
similar(B::BitArray, dims::Dims) = BitArray(dims...)

# changing type returns an Array, even if type is Int
# (this triggers conversions like int(bitvector) etc.)
similar(B::BitArray, T::Type, dims::Dims) = Array(T, dims)

function fill!(B::BitArray, x::Integer)
    if x == 0
        for i = 1 : length(B.chunks)
            B.chunks[i] = uint64(0)
        end
    elseif x == 1
        if length(B) == 0
            return B
        end
        u = ~uint64(0)
        for i = 1 : length(B.chunks) - 1
            B.chunks[i] = u
        end
        l = length(B) & 0x3f
        B.chunks[end] = (u >>> (64 - l))
    else
        error("invalid BitArray value")
    end
    return B
end
fill!(B::BitArray, x) = fill!(B::BitArray, int(x))

fill(B::BitArray, x::Integer) = fill!(similar(B), x)
fill(B::BitArray, x) = fill(B::BitArray, int(x))

function copy_to(dest::BitArray, src::BitArray)
    nc_d = length(dest.chunks)
    nc_s = length(src.chunks)
    nc = min(nc_s, nc_d)
    if nc == 0
        return dest
    end
    for i = 1 : nc - 1
        dest.chunks[i] = src.chunks[i]
    end
    if length(src) >= length(dest)
        dest.chunks[nc] = src.chunks[nc]
    else
        l = length(src) & 0x3f
        msk_s = (~uint64(0)) >>> (64 - l)
        msk_d = ~msk_s
        dest.chunks[nc] = (msk_d & dest.chunks[nc]) | (msk_s & src.chunks[nc])
    end
    return dest
end

function copy_to(dest::BitArray, pos_d::Integer, src::BitArray, pos_s::Integer, numbits::Integer)
    if pos_s+numbits-1 > numel(src) || pos_d+numbits-1 > numel(dest) || pos_d < 1 || pos_s < 1
        throw(BoundsError())
    end
    _jl_copy_chunks(dest.chunks, pos_d, src.chunks, pos_s, numbits)
    return dest
end

function reshape{N}(B::BitArray, dims::NTuple{N,Int})
    if prod(dims) != numel(B)
        error("reshape: invalid dimensions")
    end
    Br = BitArray{N}()
    Br.chunks = B.chunks
    Br.dims = [i::Int | i=dims]
    return Br
end

bitzeros(args...) = fill!(BitArray(args...), 0)
bitones(args...) = fill!(BitArray(args...), 1)

## Conversions ##

function convert{T,n}(::Type{Array{T,n}}, B::BitArray{n})
    A = Array(T, size(B))
    for i = 1:length(A)
        A[i] = B[i]
    end
    return A
end

function convert{T,n}(::Type{BitArray{n}}, A::AbstractArray{T,n})
    B = BitArray(size(A))
    for i = 1:length(B)
        B[i] = A[i]
    end
    return B
end

## Random ##

function bitrand!(B::BitArray)
    if length(B) == 0
        return B
    end
    for i = 1:length(B.chunks) - 1
        B.chunks[i] = _jl_dsfmt_randui64()
    end
    l = length(B) & 0x3f
    msk = (~uint64(0)) >>> (64 - l)
    B.chunks[end] = msk & _jl_dsfmt_randui64()
    return B
end

bitrand(dims::Dims) = bitrand!(BitArray(dims))
bitrand(dims::Int...) = bitrand(dims)

## Indexing: ref ##

function ref(B::BitArray, i::Integer)
    if i < 1 || i > length(B)
        throw(BoundsError())
    end
    i1, i2 = _jl_get_chunks_id(i)
    return int((B.chunks[i1] >>> i2) & 1)
end

ref(B::BitArray, i0::Integer, i1::Integer) = B[i0 + size(B,1)*(i1-1)]
ref(B::BitArray, i0::Integer, i1::Integer, i2::Integer) =
    B[i0 + size(B,1)*((i1-1) + size(B,2)*(i2-1))]
ref(B::BitArray, i0::Integer, i1::Integer, i2::Integer, i3::Integer) =
    B[i0 + size(B,1)*((i1-1) + size(B,2)*((i2-1) + size(B,3)*(i3-1)))]

function ref(B::BitArray, I::Integer...)
    ndims = length(I)
    index = I[1]
    stride = 1
    for k=2:ndims
        stride *= size(B, k - 1)
        index += (I[k] - 1) * stride
    end
    return B[index]
end

let ref_cache = nothing
global ref
function ref(B::BitArray, I::Indices...)
    i = length(I)
    while 1 > 0 && isa(I[i],Integer); i-=1; end
    d = map(length, I)::Dims
    X = similar(B, d[1:i])

    if is(ref_cache,nothing)
        ref_cache = HashTable()
    end
    gen_cartesian_map(ref_cache, ivars -> quote
            X[storeind] = B[$(ivars...)]
            storeind += 1
        end, I, (:B, :X, :storeind), B, X, 1)
    return X
end
end

# logical indexing

function _jl_ref_bool_1d(B::BitArray, I::AbstractArray{Bool})
    n = sum(I)
    out = BitArray(n)
    c = 1
    for i = 1:numel(I)
        if I[i]
            out[c] = B[i]
            c += 1
        end
    end
    out
end

ref(B::BitVector, I::AbstractVector{Bool}) = _jl_ref_bool_1d(B, I)
ref(B::BitVector, I::AbstractArray{Bool}) = _jl_ref_bool_1d(B, I)
ref(B::BitArray, I::AbstractVector{Bool}) = _jl_ref_bool_1d(B, I)
ref(B::BitArray, I::AbstractArray{Bool}) = _jl_ref_bool_1d(B, I)

ref(B::BitMatrix, I::Integer, J::AbstractVector{Bool}) = B[I, find(J)]
ref(B::BitMatrix, I::AbstractVector{Bool}, J::Integer) = B[find(I), J]
ref(B::BitMatrix, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = B[find(I), find(J)]

## Indexing: assign ##

function assign(B::BitArray, x::Integer, i::Integer)
    if i < 1 || i > length(B)
        throw(BoundsError())
    end
    i1, i2 = _jl_get_chunks_id(i)
    u = uint64(1)
    if x == 0
        B.chunks[i1] &= ~(u << i2)
    elseif x == 1
        B.chunks[i1] |= (u << i2)
    else
        error("invalid BitArray value")
    end
    return x
end

function assign(B::BitArray, x::AbstractArray, i::Int)
    B[i] = x[1]
end

function assign(B::BitArray, x::AbstractArray, i::Integer)
    B[int(i)] = x[1]
end

function assign(B::BitArray, x, i::Integer)
    B[int(i)] = int(x)
end

assign(B::BitArray, x, i0::Integer, i1::Integer) =
    B[i0 + size(B,1)*(i1-1)] = x
assign(B::BitArray, x::AbstractArray, i0::Integer, i1::Integer) =
    B[i0 + size(B,1)*(i1-1)] = x

assign(B::BitArray, x, i0::Integer, i1::Integer, i2::Integer) =
    B[i0 + size(B,1)*((i1-1) + size(B,2)*(i2-1))] = x
assign(B::BitArray, x::AbstractArray, i0::Integer, i1::Integer, i2::Integer) =
    B[i0 + size(B,1)*((i1-1) + size(B,2)*(i2-1))] = x

assign(B::BitArray, x, i0::Integer, i1::Integer, i2::Integer, i3::Integer) =
    B[i0 + size(B,1)*((i1-1) + size(B,2)*((i2-1) + size(B,3)*(i3-1)))] = x
assign(B::BitArray, x::AbstractArray, i0::Integer, i1::Integer, i2::Integer, i3::Integer) =
    B[i0 + size(B,1)*((i1-1) + size(B,2)*((i2-1) + size(B,3)*(i3-1)))] = x

assign(B::BitArray, x, I0::Integer, I::Integer...) = assign_scalarNDbit(B,x,I0,I...)
assign(B::BitArray, x::AbstractArray, I0::Integer, I::Integer...) =
    assign_scalarNDbit(B,x,I0,I...)

function assign_scalarNDbit(B::BitArray, x, I0::Integer, I::Integer...)
    index = I0
    stride = 1
    for k = 1:length(I)
        stride = stride * size(B, k)
        index += (I[k] - 1) * stride
    end
    B[index] = x
    return B
end

function assign{T<:Integer}(B::BitArray, x, I::AbstractVector{T})
    for i in I
        B[i] = x
    end
    return B
end

# this is mainly indended for the general cat case
function assign(B::BitArray, X::BitArray, I0::Range1{Int}, I::Range1{Int}...)
    nI = 1 + length(I)
    if ndims(B) != nI
        error("wrong number of dimensions in assigment")
    end
    lI = length(I0)
    for r in I
        lI *= length(r)
    end
    if numel(X) != lI
        error("array assignment dimensions mismatch")
    end
    if lI == 0
        return B
    end
    f0 = first(I0)
    l0 = length(I0)
    if nI == 1
        _jl_copy_chunks(B.chunks, f0, X.chunks, 1, l0)
        return B
    end
    #ind_lst = [first(r)::Int | r in I] # this hits a bug, refactoring...
    ind_lst = Array(Int, length(I))
    for k = 1 : length(I)
        ind_lst[k] = first(I[k])
    end
    # endofbug
    indX = 1
    iterate = true
    while iterate
        stride = 1
        ind = f0
        for k = 1 : nI - 1
            stride *= size(B, k)
            ind += stride * (ind_lst[k] - 1)
        end
        _jl_copy_chunks(B.chunks, ind, X.chunks, indX, l0)
        indX += l0
        iterate = false
        for k = 1 : nI - 1
            if ind_lst[k] < last(I[k])
                ind_lst[k] += 1
                iterate = true
                break;
            else
                ind_lst[k] = first(I[k])
            end
        end
    end
    return B
end

function assign{T<:Integer}(B::BitArray, X::AbstractArray, I::AbstractVector{T})
    for i = 1:length(I)
        B[I[i]] = X[i]
    end
    return B
end

let assign_cache = nothing
    global assign
    function assign(B::BitArray, x, I0::Indices, I::Indices...)
        if is(assign_cache,nothing)
            assign_cache = HashTable()
        end
        gen_cartesian_map(assign_cache, ivars->:(B[$(ivars...)] = x),
        tuple(I0, I...),
        (:B, :x),
        B, x)
        return B
    end
end

let assign_cache = nothing
    global assign
    function assign(B::BitArray, X::AbstractArray, I0::Indices, I::Indices...)
        if is(assign_cache,nothing)
            assign_cache = HashTable()
        end
        gen_cartesian_map(assign_cache,
            ivars->:(B[$(ivars...)] = X[refind]; refind += 1),
            tuple(I0, I...),
            (:B, :X, :refind),
            B, X, 1)
        return B
    end
end

# logical indexing

function _jl_assign_bool_scalar_1d(A::BitArray, x, I::AbstractArray{Bool})
    n = sum(I)
    for i = 1:numel(I)
        if I[i]
            A[i] = x
        end
    end
    A
end

function _jl_assign_bool_vector_1d(A::BitArray, X::AbstractArray, I::AbstractArray{Bool})
    n = sum(I)
    c = 1
    for i = 1:numel(I)
        if I[i]
            A[i] = X[c]
            c += 1
        end
    end
    A
end

assign(A::BitArray, X::AbstractArray, I::AbstractVector{Bool}) = _jl_assign_bool_vector_1d(A, X, I)
assign(A::BitArray, X::AbstractArray, I::AbstractArray{Bool}) = _jl_assign_bool_vector_1d(A, X, I)
assign(A::BitArray, x, I::AbstractVector{Bool}) = _jl_assign_bool_scalar_1d(A, x, I)
assign(A::BitArray, x, I::AbstractArray{Bool}) = _jl_assign_bool_scalar_1d(A, x, I)

assign(A::BitMatrix, x::AbstractArray, I::Integer, J::AbstractVector{Bool}) = (A[I,find(J)]=x)
assign(A::BitMatrix, x, I::Integer, J::AbstractVector{Bool}) = (A[I,find(J)]=x)

assign(A::BitMatrix, x::AbstractArray, I::AbstractVector{Bool}, J::Integer) = (A[find(I),J]=x)
assign(A::BitMatrix, x, I::AbstractVector{Bool}, J::Integer) = (A[find(I),J]=x)

assign(A::BitMatrix, x::AbstractArray, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = (A[find(I),find(J)]=x)
assign(A::BitMatrix, x, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = (A[find(I),find(J)]=x)


## Dequeue functionality ##

function push(B::BitVector, item)
    # convert first so we don't grow the bitarray if the assignment won't work
    item = convert(Int, item)
    if item != 0 && item != 1
        error("invalid BitArray value")
    end
    l = length(B) & 0x3f
    if l == 0
        ccall(:jl_array_grow_end, Void, (Any, Uint), B.chunks, 1)
        B.chunks[end] = uint64(0)
    end
    B.dims[1] += 1
    B[end] = item
    return B
end

function append!(B::BitVector, items::BitVector)
    n0 = length(B)
    n1 = length(items)
    if n1 == 0
        return B
    end
    k0 = length(B.chunks)
    k1 = int(_jl_num_bit_chunks(n0 + n1))
    if k1 > k0
        ccall(:jl_array_grow_end, Void, (Any, Uint), B.chunks, k1 - k0)
        B.chunks[end] = uint64(0)
    end
    B.dims[1] += n1
    _jl_copy_chunks(B.chunks, n0 + 1, items.chunks, n1)
    return B
end

append!{T}(B::BitVector, items::AbstractVector{T}) = append!(B, convert(BitVector, items))

function grow(B::BitVector, n::Integer)
    n0 = length(B)
    k0 = length(B.chunks)
    k1 = int(_jl_num_bit_chunks(n0 + n))
    if k1 > k0
        ccall(:jl_array_grow_end, Void, (Any, Uint), B.chunks, k1 - k0)
        B.chunks[end] = uint64(0)
    end
    B.dims[1] += n
    return B
end

function pop(B::BitVector)
    if isempty(B)
        error("pop: bitarray is empty")
    end
    item = B[end]
    B[end] = 0

    l = length(B) & 0x3f
    if l == 1
        ccall(:jl_array_del_end, Void, (Any, Uint), B.chunks, 1)
    end
    B.dims[1] -= 1

    return item
end

function enqueue(B::BitVector, item)
    item = convert(Int, item)
    if item != 0 && item != 1
        error("invalid BitArray value")
    end
    l = length(B) & 0x3f
    if l == 0
        ccall(:jl_array_grow_end, Void, (Any, Uint), B.chunks, 1)
        B.chunks[end] = uint64(0)
    end
    B.dims[1] += 1
    if B.dims[1] == 1
        B.chunks[1] = item
        return B
    end
    for i = length(B.chunks) : -1 : 2
        B.chunks[i] = (B.chunks[i] << 1) | (B.chunks[i - 1] >>> 63) 
    end
    B.chunks[1] = uint64(item) | (B.chunks[1] << 1)
    return B
end

function shift(B::BitVector)
    if isempty(B)
        error("shift: bitarray is empty")
    end
    item = B[1]

    for i = 1 : length(B.chunks) - 1
        B.chunks[i] = (B.chunks[i] >>> 1) | (B.chunks[i + 1] << 63) 
    end

    l = length(B) & 0x3f
    if l == 1
        ccall(:jl_array_del_end, Void, (Any, Uint), B.chunks, 1)
    else
        B.chunks[end] >>>= 1
    end
    B.dims[1] -= 1

    return item
end

function insert(B::BitVector, i::Integer, item)
    if i < 1
        throw(BoundsError())
    end
    item = convert(Int, item)
    if item != 0 && item != 1
        error("invalid BitArray value")
    end
    n = length(B)
    if i > n
        x = bitzeros(i - n)
        append!(B, x)
    else
        k, j = _jl_get_chunks_id(i)

        l = length(B) & 0x3f
        if l == 0
            ccall(:jl_array_grow_end, Void, (Any, Uint), B.chunks, 1)
            B.chunks[end] = uint64(0)
        end
        B.dims[1] += 1

        for t = length(B.chunks) : -1 : k + 1
            B.chunks[t] = (B.chunks[t] << 1) | (B.chunks[t - 1] >>> 63) 
        end

        msk_bef = (~uint64(0)) >>> (63 - j) >>> 1
        msk_aft = ~msk_bef
        B.chunks[k] = (msk_bef & B.chunks[k]) | ((msk_aft & B.chunks[k]) << 1)
    end
    B[i] = item
end

function del(B::BitVector, i::Integer)
    n = length(B)
    if !(1 <= i <= n)
        throw(BoundsError())
    end

    k, j = _jl_get_chunks_id(i)

    msk_bef = (~uint64(0)) >>> (63 - j)
    msk_aft = ~msk_bef
    msk_bef >>>= 1

    B.chunks[k] = (msk_bef & B.chunks[k]) | ((msk_aft & B.chunks[k]) >> 1)
    if length(B.chunks) > k
        B.chunks[k] |= (B.chunks[k + 1] << 63)
    end

    for t = k + 1 : length(B.chunks) - 1
        B.chunks[t] = (B.chunks[t] >>> 1) | (B.chunks[t + 1] << 63) 
    end

    l = length(B) & 0x3f

    if l == 1
        ccall(:jl_array_del_end, Void, (Any, Uint), B.chunks, 1)
    elseif length(B.chunks) > k
        B.chunks[end] >>>= 1
    end

    B.dims[1] -= 1

    return B
end

function del(B::BitVector, r::Range1{Int})
    n = length(B)
    i_f = first(r)
    i_l = last(r)
    if !(1 <= i_f && i_l <= n)
        throw(BoundsError())
    end
    if i_l < i_f
        return B
    end

    _jl_copy_chunks(B.chunks, i_f, B.chunks, i_l + 1, n - i_l)

    delta_l = i_l - i_f + 1
    new_l = length(B) - delta_l
    delta_k = length(B.chunks) - _jl_num_bit_chunks(new_l)

    if delta_k > 0
        ccall(:jl_array_del_end, Void, (Any, Uint), B.chunks, delta_k)
    end

    if new_l > 0
        u = ~(uint64(0))
        l = new_l & 0x3f
        B.chunks[end] &= (u >>> (64 - l))
    end

    B.dims[1] = new_l

    return B
end

function del_all(B::BitVector)
    ccall(:jl_array_del_end, Void, (Any, Uint), B.chunks, length(B.chunks))
    B.dims[1] = 0
    return B
end

## Unary operators ##

function (~)(B::BitArray)
    C = similar(B)
    for i = 1:length(B.chunks) - 1
        C.chunks[i] = (~)(B.chunks[i])
    end
    l = length(B) & 0x3f
    msk = (~uint64(0)) >>> (64 - l)
    C.chunks[end] = msk & (~B.chunks[end])
    return C
end

(-)(B::BitArray) = -int(B)
sign(B::BitArray) = copy(B)

real(B::BitArray) = copy(B)
imag(B::BitArray) = bitzeros(size(B))

conj!(B::BitArray) = B
conj(B::BitArray) = copy(B)

## Binary arithmetic operators ##

for f in (:+, :-, :div, :mod, :./, :.^, :/, :\)
    @eval begin
        ($f)(A::BitArray, B::BitArray) = ($f)(int(A), int(B))
        ($f)(B::BitArray, x::Number) = ($f)(int(B), x)
        ($f)(x::Number, B::BitArray) = ($f)(x, int(B))
    end
end

for f in (:&, :|, :$)
    @eval begin
        function ($f)(A::BitArray, B::BitArray)
            F = BitArray(promote_shape(size(A),size(B))...)
            for i = 1:length(F.chunks) - 1
                F.chunks[i] = ($f)(A.chunks[i], B.chunks[i])
            end
            l = length(F) & 0x3f
            msk = (~uint64(0)) >>> (64 - l)
            F.chunks[end] = msk & ($f)(A.chunks[end], B.chunks[end])
            return F
        end
        function ($f)(x::Number, B::BitArray)
            if x == 0
                u = uint64(0)
            elseif x == 1
                u = ~uint64(0)
            else
                #error("invalid BitArray value")
                if is($f,&)
                    return ($f)(x & 1, B)
                else
                    return ($f)(x, int(B))
                end
            end
            F = similar(B)
            for i = 1:length(F.chunks) - 1
                F.chunks[i] = ($f)(u, B.chunks[i])
            end
            l = length(F) & 0x3f
            msk = (~uint64(0)) >>> (64 - l)
            F.chunks[end] = msk & ($f)(u, B.chunks[end])
            return F
        end
        ($f)(A::BitArray, x::Number) = ($f)(x, A)
    end
end

(.*)(A::BitArray, B::BitArray) = A & B
function (.*)(x::Number, B::BitArray)
    if x == 0
        return bitzeros(size(B))
    elseif x == 1
        return copy(B)
    else
        return (.*)(x, int(B))
    end
end
(.*)(A::BitArray, x::Number) = x .* A

(*)(A::BitArray, B::BitArray) = int(A) * int(B)

for f in (:+, :-, :div, :mod, :./, :.^, :/, :\, :*, :.*, :&, :|, :$)
    @eval begin
        ($f)(A::BitArray, B::AbstractArray) = ($f)(int(A), B)
        ($f)(A::AbstractArray, B::BitArray) = ($f)(A, int(B))
    end
end


## promotion to complex ##

# TODO?

## Binary comparison operators ##

for f in (:(==), :!=, :<, :<=)
    @eval begin
        function ($f)(A::BitArray, B::BitArray)
            F = Array(Bool, promote_shape(size(A),size(B)))
            for i = 1:numel(A)
                F[i] = ($f)(A[i], B[i])
            end
            return F
        end
        function ($f)(x::Number, B::BitArray)
            F = Array(Bool, size(B))
            for i = 1:numel(F)
                F[i] = ($f)(x, B[i])
            end
            return F
        end
        function ($f)(A::BitArray, x::Number)
            F = Array(Bool, size(A))
            for i = 1:numel(F)
                F[i] = ($f)(A[i], x)
            end
            return F
        end
    end
end

for f in (:(==), :!=, :<, :<=)
    @eval begin
        ($f)(A::BitArray, B::AbstractArray) = ($f)(int(A), B)
        ($f)(A::AbstractArray, B::BitArray) = ($f)(A, int(B))
    end
end

## Data movement ##

# TODO some of this could be optimized

function slicedim(A::BitArray, d::Integer, i::Integer)
    d_in = size(A)
    leading = d_in[1:(d-1)]
    d_out = append(leading, (1,), d_in[(d+1):end])

    M = prod(leading)
    N = numel(A)
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
    N = numel(A)
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
                for k=0:(M-1)
                    B[boffs + k] = A[offs + k]
                end
            end
        end
    end
    return B
end

function rotl90(A::BitMatrix)
    m,n = size(A)
    B = BitArray(n,m)
    for i=1:m, j=1:n
        B[n-j+1,i] = A[i,j]
    end
    return B
end
function rotr90(A::BitMatrix)
    m,n = size(A)
    B = BitArray(n,m)
    for i=1:m, j=1:n
        B[j,m-i+1] = A[i,j]
    end
    return B
end
function rot180(A::BitMatrix)
    m,n = size(A)
    B = similar(A)
    for i=1:m, j=1:n
        B[m-i+1,n-j+1] = A[i,j]
    end
    return B
end
function rotl90(A::BitMatrix, k::Integer)
    k = k % 4
    k == 1 ? rotl90(A) :
    k == 2 ? rot180(A) :
    k == 3 ? rotr90(A) : copy(A)
end

function _jl_reverse_bits(x::Uint64)
    x = ((x >>>  1) & 0x5555555555555555) | ((x <<  1) & 0xaaaaaaaaaaaaaaaa)
    x = ((x >>>  2) & 0x3333333333333333) | ((x <<  2) & 0xcccccccccccccccc)
    x = ((x >>>  4) & 0x0f0f0f0f0f0f0f0f) | ((x <<  4) & 0xf0f0f0f0f0f0f0f0)
    x = ((x >>>  8) & 0x00ff00ff00ff00ff) | ((x <<  8) & 0xff00ff00ff00ff00)
    x = ((x >>> 16) & 0x0000ffff0000ffff) | ((x << 16) & 0xffff0000ffff0000)
    x = ((x >>> 32) & 0x00000000ffffffff) | ((x << 32) & 0xffffffff00000000)
    return x
end

# alternative implementation: doesn't seem to give
# any performance gain (and it has a higher WTF coefficient)
#function _jl_reverse_bits(x::Uint64)
    #x = (x << 32) | (x >>> 32)
    #x = (x & 0x0001ffff0001ffff) << 15 | (x & 0xfffe0000fffe0000) >>> 17
    #t = (x $ (x >>> 10)) & 0x003f801f003f801f
    #x = (t | (t << 10)) $ x
    #t = (x $ (x >>> 4)) & 0x0e0384210e038421
    #x = (t | (t << 4)) $ x
    #t = (x $ (x >>> 2)) & 0x2248884222488842
    #x = (t | (t << 2)) $ x
    #return x
#end


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
        aux_chunks[1] = _jl_reverse_bits(B.chunks[i])
        _jl_copy_chunks(B.chunks, j + 1, B.chunks, n - 63 - j, 64)
        B.chunks[i] = _jl_reverse_bits(B.chunks[i])
        _jl_copy_chunks(B.chunks, n - 63 - j, aux_chunks, 1, 64)
    end

    if pnc == 0
        return B
    end

    l = ((n + 63) & 63) + 1
    i = hnc + 1
    j = hnc << 6
    u = ~(uint64(0))
    msk = (u >>> (64 - l))

    aux_chunks[1] = _jl_reverse_bits(B.chunks[i] & msk) >>> (64 - l)
    _jl_copy_chunks(B.chunks, j + 1, aux_chunks, 1, l)

    return B
end

reverse(v::BitVector) = reverse!(copy(v))

## nnz & find ##

_jl_bc4(x) = x - ((x >>> 1) & 0x7777777777777777) - ((x >>> 2) & 0x3333333333333333) - ((x >>> 3) & 0x1111111111111111)

_jl_bc8(x) = (_jl_bc4(x) + (_jl_bc4(x) >>> 4)) & 0x0f0f0f0f0f0f0f0f

function _jl_count_bits_in_chunk(c::Uint64)
    return int(mod(_jl_bc8(c), 0xff))
end

function nnz(B::BitArray)
    n = 0
    for i = 1:length(B.chunks)
        n += _jl_count_bits_in_chunk(B.chunks[i])
    end
    return n
end

function find(B::BitArray)
    nnzB = nnz(B)
    I = Array(Int, nnzB)
    count = 1
    for i = 1:length(B)
        if B[i] != 0
            I[count] = i
            count += 1
        end
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
        if B[i,j] != 0
            I[count] = i
            J[count] = j
            count += 1
        end
    end
    return (I, J)
end

let findn_cache = nothing
function findn_one(ivars)
    s = { quote I[$i][count] = $ivars[i] end | i = 1:length(ivars)}
    quote
    	Bind = B[$(ivars...)]
    	if Bind != z
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
    ranges = ntuple(ndims(B), d->(1:size(B,d)))

    if is(findn_cache,nothing)
        findn_cache = HashTable()
    end

    gen_cartesian_map(findn_cache, findn_one, ranges,
                      (:B, :I, :count, :z), B,I,1, 0)
    return I
end
end

function findn_nzs(B::BitMatrix)
    I, J = findn(B)
    return (I, J, bitones(length(I)))
end

nonzeros(B::BitArray) = bitones(nnz(B))

## Reductions ##

areduce(f::Function, A::BitArray, region::Region, v0) =
    areduce(f,A,region,v0,Int)

areduce(f::Function, B::BitArray, region::Region, v0, RType::Type) =
    areduce(f, int(B), region, v0, RType)

let bitareduce_cache = nothing
# generate the body of the N-d loop to compute a reduction
function gen_bitareduce_func(n, f)
    ivars = { gensym() | i=1:n }
    # limits and vars for reduction loop
    lo    = { gensym() | i=1:n }
    hi    = { gensym() | i=1:n }
    rvars = { gensym() | i=1:n }
    setlims = { quote
        # each dim of reduction is either 1:sizeA or ivar:ivar
        if contains(region,$i)
            $lo[i] = 1
            $hi[i] = size(A,$i)
        else
            $lo[i] = $hi[i] = $ivars[i]
        end
               end | i=1:n }
    rranges = { :( ($lo[i]):($hi[i]) ) | i=1:n }  # lo:hi for all dims
    body =
    quote
        _tot = v0
        $(setlims...)
        $make_loop_nest(rvars, rranges,
                        :(_tot = ($f)(_tot, A[$(rvars...)])))
        R[_ind] = _tot
        _ind += 1
    end
    quote
        local _F_
        function _F_(f, A, region, R, v0)
            _ind = 1
            $make_loop_nest(ivars, { :(1:size(R,$i)) | i=1:n }, body)
        end
        _F_
    end
end

global bitareduce
function bitareduce(f::Function, A::BitArray, region::Region, v0)
    dimsA = size(A)
    ndimsA = ndims(A)
    dimsR = ntuple(ndimsA, i->(contains(region, i) ? 1 : dimsA[i]))
    R = BitArray(dimsR)

    if is(bitareduce_cache,nothing)
        bitareduce_cache = HashTable()
    end

    key = ndimsA
    fname = :f

    if  (is(f,*)     && (fname=:*;true)) ||
        (is(f,max)   && (fname=:max;true)) ||
        (is(f,min)   && (fname=:min;true))
        key = (fname, ndimsA)
    end

    if !has(bitareduce_cache,key)
        fexpr = gen_bitareduce_func(ndimsA, fname)
        func = eval(fexpr)
        bitareduce_cache[key] = func
    else
        func = bitareduce_cache[key]
    end

    func(f, A, region, R, v0)

    return R
end
end

max(A::BitArray, b::(), region::Region) = bitareduce(max,A,region,0)
min(A::BitArray, b::(), region::Region) = bitareduce(min,A,region,1)
sum(A::BitArray, region::Region)  = areduce(+,A,region,0)
prod(A::BitArray, region::Region) = bitareduce(*,A,region,1)

sum(B::BitArray) = nnz(B)

prod(B::BitArray) = (nnz(B) == length(B) ? 1 : 0)

min(B::BitArray) = prod(B)
max(B::BitArray) = (nnz(B) > 0 ? 1 : 0)

## map over bitarrays ##

# TODO

## Filter ##

function filter(f::Function, Bs::BitArray)
    boolmap::Array{Bool} = map(f, Bs)
    Bs[boolmap]
end

## Transpose ##

transpose(B::BitVector) = reshape(copy(B), 1, length(B))
function transpose(B::BitMatrix)
    l1 = size(B, 1)
    l2 = size(B, 2)
    Bt = BitArray(l2, l1)
    for i = 1 : l1
        for j = 1 : l2
            Bt[j, i] = B[i, j]
        end
    end
    return Bt
end

ctranspose(B::BitArray) = transpose(B)

## Permute ##

let permute_cache = nothing, stridenames::Array{Any,1} = {}
global permute
function permute(B::BitArray, perm)
    dimsB = size(B)
    ndimsB = length(dimsB)
    dimsP = ntuple(ndimsB, i->dimsB[perm[i]])
    P = BitArray(dimsP)
    ranges = ntuple(ndimsB, i->(colon(1,dimsP[i])))
    while length(stridenames) < ndimsB
        push(stridenames, gensym())
    end

    #calculates all the strides
    strides = [ stride(B, perm[dim]) | dim = 1:length(perm) ]

    #Creates offset, because indexing starts at 1
    offset = 0
    for i in strides
        offset+=i
    end
    offset = 1-offset

    function permute_one(ivars)
        len = length(ivars)
        counts = { gensym() | i=1:len}
        toReturn = cell(len+1,2)
        for i = 1:numel(toReturn)
            toReturn[i] = nothing
        end

        tmp = counts[end]
        toReturn[len+1] = quote
            ind = 1
            $tmp = $stridenames[len]
        end

        #inner most loop
        toReturn[1] = quote
            P[ind] = B[+($counts...)+offset]
            ind+=1
            $counts[1]+= $stridenames[1]
        end
        for i = 1:len-1
            tmp = counts[i]
            val = i
            toReturn[(i+1)] = quote
                $tmp = $stridenames[val]
            end
            tmp2 = counts[i+1]
            val = i+1
            toReturn[(i+1)+(len+1)] = quote
                 $tmp2 += $stridenames[val]
            end
        end
        toReturn
    end

    if is(permute_cache,nothing)
	permute_cache = HashTable()
    end

    gen_cartesian_map(permute_cache, permute_one, ranges,
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
        _jl_copy_chunks(M.chunks, (height * (j - 1)) + 1, B[j].chunks, height)
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
        _jl_copy_chunks(B.chunks, j, Vk.chunks, length(Vk))
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
        n = numel(Ak)
        _jl_copy_chunks(B.chunks, pos, Ak.chunks, n)
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
    nrowsA = [size(a, 1) | a = A]
    pos_d = 1
    pos_s = ones(Int, nargs)
    for j = 1:ncols
        for k=1:nargs
            _jl_copy_chunks(B.chunks, pos_d, A[k].chunks, pos_s[k], nrowsA[k])
            pos_s[k] += nrowsA[k]
            pos_d += nrowsA[k]
        end
    end
    return B
end

# general case, specialized for BitArrays and Integers
function cat(catdim::Integer, X::Union(BitArray, Integer)...)
    nargs = length(X)
    # using integers != 0 or 1 results in convertion to Array{Int}
    has_bitarray = false
    for a in X
        if !isa(a, BitArray) && a != 0 && a != 1
            return invoke(cat, (Integer, Any...), catdim, X...)
        elseif isa(a, BitArray)
            has_bitarray = true
        end
    end
    # just 0/1 integers and no BitArrays -> general case
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
                    error("cat: dimension mismatch on dimension", d)
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
    C = BitArray(dimsC)

    range = 1
    for k=1:nargs
        nextrange = range+cat_ranges[k]
        cat_one = ntuple(ndimsC, i->(i != catdim ?
                                     (1:dimsC[i]) : (range:nextrange-1) ))
        # note: when X is a BitArray, this calls
        #       the special assign with ranges
        C[cat_one...] = X[k]
        range = nextrange
    end
    return C
end

# hvcat -> use fallbacks in abstractarray.jl
