_jl_num_bit_chunks(n::Integer) = (n + 0x3f) >>> 6

# notes: bits are stored in contiguous chunks
#        unused bits must always be set to 0
type BitArray{T<:Integer, N} <: AbstractArray{T, N}
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

BitArray{T}(::Type{T}) = BitArray{T, 1}(0)
BitArray() = BitArray(Int, 0)
BitArray{T}(::Type{T}, dims::Dims) = BitArray{T, max(length(dims), 1)}(dims...)
BitArray(dims::Dims) = BitArray(Int, dims)
BitArray{T}(::Type{T}, dims::Int...) = BitArray{T, max(length(dims), 1)}(dims...)
BitArray(dims::Int...) = BitArray(Int, dims...)

typealias BitVector{T} BitArray{T,1}
typealias BitMatrix{T} BitArray{T,2}

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

## utility functions ##

length(B::BitArray) = prod(B.dims)
eltype{T}(B::BitArray{T}) = T
ndims{T,N}(B::BitArray{T,N}) = N
numel(B::BitArray) = prod(B.dims)
size(B::BitArray) = tuple(B.dims...)

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

similar{T}(B::BitArray{T}) = BitArray(T, B.dims...)
similar{T}(B::BitArray{T}, dims::Int...) = BitArray(T, dims)
similar{T}(B::BitArray{T}, dims::Dims) = BitArray(T, dims...)

similar{S<:Integer}(B::BitArray, T::Type{S}, dims::Dims) = BitArray(T, dims)
# changing type to a non-Integer returns an Array
# (this triggers conversions like float(bitvector) etc.)
similar(B::BitArray, T::Type, dims::Dims) = Array(T, dims)

function fill!{T<:Integer}(B::BitArray{T}, x::Number)
    y = convert(T, x)
    if y == 0
        for i = 1 : length(B.chunks)
            B.chunks[i] = uint64(0)
        end
    elseif y == 1
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

fill!(B::BitArray, x) = fill!(B, int(x))

fill{T}(B::BitArray{T}, x::Integer) = fill!(similar(B), x)
# disambiguation
# (this is going to throw an error anyway)
fill{T}(B::BitArray{T}, x::(Int64...,)) = fill(B, int(x))
# end disambiguation
fill{T}(B::BitArray{T}, x) = fill(B, convert(T, x))

bitzeros{T}(::Type{T}, args...) = fill!(BitArray(T, args...), 0)
bitzeros(args...) = fill!(BitArray(args...), 0)

bitones{T}(::Type{T}, args...) = fill!(BitArray(T, args...), 1)
bitones(args...) = fill!(BitArray(args...), 1)

# XXX: temporary!?
bitfalses(args...) = bitzeros(Bool, args...)
bittrues(args...) = bitones(Bool, args...)

biteye(n::Integer) = biteye(n, n)
function biteye(m::Integer, n::Integer)
    a = bitzeros(m,n)
    for i = 1:min(m,n)
        a[i,i] = 1
    end
    return a
end
function one{T}(x::BitMatrix{T})
    m, n = size(x)
    a = bitzeros(T,size(x))
    for i = 1:min(m,n)
        a[i,i] = 1
    end
    return a
end

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

function reshape{T,N}(B::BitArray{T}, dims::NTuple{N,Int})
    if prod(dims) != numel(B)
        error("reshape: invalid dimensions")
    end
    Br = BitArray{T,N}()
    Br.chunks = B.chunks
    Br.dims = [i::Int | i=dims]
    return Br
end

## Conversions ##

function convert{T,S,n}(::Type{Array{T,n}}, B::BitArray{S,n})
    A = Array(T, size(B))
    for i = 1:length(A)
        A[i] = B[i]
    end
    return A
end

function convert{T,S,n}(::Type{BitArray{S,n}}, A::AbstractArray{T,n})
    B = BitArray(S, size(A))
    for i = 1:length(B)
        B[i] = A[i]
    end
    return B
end

convert{T<:Integer,S<:Integer,n}(::Type{BitArray{T,n}}, B::BitArray{S,n}) =
    copy_to(similar(B, T), B)

# this version keeps dimensionality
# (it's an extension of Array's behavior, which only does
# this for Vectors)
function reinterpret{T<:Integer,S<:Integer,N}(::Type{T}, B::BitArray{S,N})
    A = BitArray{T,N}()
    A.dims = copy(B.dims)
    A.chunks = B.chunks
    return A
end
function reinterpret{T<:Integer,S<:Integer,N}(::Type{T}, B::BitArray{S}, dims::NTuple{N,Int})
    if prod(dims) != numel(B)
        error("reinterpret: invalid dimensions")
    end
    A = BitArray{T,N}()
    A.dims = [i::Int | i=dims]
    A.chunks = B.chunks
    return A
end

# shorthand forms BitArray <-> Array
bitunpack{T,n}(B::BitArray{T,n}) = convert(Array{T,n}, B)
bitpack{T,n}(A::Array{T,n}) = convert(BitArray{T,n}, A)

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

bitrand{T}(::Type{T}, dims::Dims) = bitrand!(BitArray(T, dims))
bitrand{T}(::Type{T}, dims::Int...) = bitrand(T, dims)

bitrand(dims::Dims) = bitrand!(BitArray(dims))
bitrand(dims::Int...) = bitrand(dims)

## Indexing: ref ##

function ref{T<:Integer}(B::BitArray{T}, i::Integer)
    if i < 1 || i > length(B)
        throw(BoundsError())
    end
    i1, i2 = _jl_get_chunks_id(i)
    return (B.chunks[i1] >>> i2) & one(Uint64) == one(Uint64) ? one(T) : zero(T)
end

# 0d bitarray
ref{T<:Integer}(B::BitArray{T,0}) = B[1]

ref(B::BitArray, i0::Integer, i1::Integer) = B[i0 + size(B,1)*(i1-1)]
ref(B::BitArray, i0::Integer, i1::Integer, i2::Integer) =
    B[i0 + size(B,1)*((i1-1) + size(B,2)*(i2-1))]
ref(B::BitArray, i0::Integer, i1::Integer, i2::Integer, i3::Integer) =
    B[i0 + size(B,1)*((i1-1) + size(B,2)*((i2-1) + size(B,3)*(i3-1)))]

# note: although unused, the T here is necessary, otherwhise
#       ref(::BitArray, ::Integer) is shadowed (?)
function ref{T<:Integer}(B::BitArray{T}, I::Integer...)
    ndims = length(I)
    index = I[1]
    stride = 1
    for k=2:ndims
        stride *= size(B, k - 1)
        index += (I[k] - 1) * stride
    end
    return B[index]
end

# note: we can gain some performance if the first dimension is a range;
# TODO: extend to I:Indices... (i.e. not necessarily contiguous)
let ref_cache = nothing
    global ref
    function ref(B::BitArray, I0::Range1{Int}, I::Union(Integer, Range1{Int})...)
        nI = 1 + length(I)
        # this will be uncommented when
        # the indexing behaviour is settled
        #if ndims(B) != nI
            #error("wrong number of dimensions in ref")
        #end
        while nI > 1 && isa(I[nI-1],Integer); nI-=1; end
        d = tuple(length(I0), map(length, I[1:nI-1])...)::Dims
        X = similar(B, d)

        I = map(x->(isa(x,Integer) ? (x:x) : x), I[1:nI-1])

        f0 = first(I0)
        l0 = length(I0)
        if nI == 1
            _jl_copy_chunks(X.chunks, 1, B.chunks, f0, l0)
            return X
        end
        if is(ref_cache,nothing)
            ref_cache = Dict()
        end
        gap_lst = [last(r)-first(r)+1 | r in I]
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

        gen_cartesian_map(ref_cache,
            ivars->begin
                bodies = cell(nI, 2)
                bodies[1] = quote
                        _jl_copy_chunks(X.chunks, storeind, B.chunks, ind, l0)
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

let ref_cache = nothing
    global ref
    function ref(B::BitArray, I::Indices...)
        i = length(I)
        while i > 0 && isa(I[i],Integer); i-=1; end
        d = map(length, I[1:i])::Dims
        X = similar(B, d)

        if is(ref_cache,nothing)
            ref_cache = Dict()
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
    out = similar(B, n)
    c = 1
    for i = 1:numel(I)
        if I[i]
            out[c] = B[i]
            c += 1
        end
    end
    out
end

# note : type specifiers needed to gain precedence over ref(::BitArray, ::Indices...)
ref{T<:Integer}(B::BitVector{T}, I::AbstractVector{Bool}) = _jl_ref_bool_1d(B, I)
ref{T<:Integer}(B::BitVector{T}, I::AbstractArray{Bool}) = _jl_ref_bool_1d(B, I)
ref{T<:Integer}(B::BitArray{T}, I::AbstractVector{Bool}) = _jl_ref_bool_1d(B, I)
ref{T<:Integer}(B::BitArray{T}, I::AbstractArray{Bool}) = _jl_ref_bool_1d(B, I)

ref{T<:Integer}(B::BitMatrix{T}, I::Integer, J::AbstractVector{Bool}) = B[I, find(J)]
ref{T<:Integer}(B::BitMatrix{T}, I::AbstractVector{Bool}, J::Integer) = B[find(I), J]
ref{T<:Integer}(B::BitMatrix{T}, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = B[find(I), find(J)]

## Indexing: assign ##

function assign{T<:Integer}(B::BitArray{T}, x::Number, i::Integer)
    if i < 1 || i > length(B)
        throw(BoundsError())
    end
    i1, i2 = _jl_get_chunks_id(i)
    u = uint64(1)
    y = convert(T, x)
    if y == 0
        B.chunks[i1] &= ~(u << i2)
    elseif y == 1
        B.chunks[i1] |= (u << i2)
    else
        error("invalid BitArray value")
    end
    return B
end

assign(B::BitArray, x::Number, i0::Integer, i1::Integer) =
    B[i0 + size(B,1)*(i1-1)] = x

assign(B::BitArray, x::Number, i0::Integer, i1::Integer, i2::Integer) =
    B[i0 + size(B,1)*((i1-1) + size(B,2)*(i2-1))] = x

assign(B::BitArray, x::Number, i0::Integer, i1::Integer, i2::Integer, i3::Integer) =
    B[i0 + size(B,1)*((i1-1) + size(B,2)*((i2-1) + size(B,3)*(i3-1)))] = x

function assign(B::BitArray, x::Number, I0::Integer, I::Integer...)
    index = I0
    stride = 1
    for k = 1:length(I)
        stride = stride * size(B, k)
        index += (I[k] - 1) * stride
    end
    B[index] = x
    return B
end

let assign_cache = nothing
    global _jl_assign_array2bitarray_ranges
    function _jl_assign_array2bitarray_ranges(B::BitArray, X::BitArray, I0::Range1{Int}, I::Range1{Int}...)
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
        if is(assign_cache,nothing)
            assign_cache = Dict()
        end
        gap_lst = [last(r)-first(r)+1 | r in I]
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

        gen_cartesian_map(assign_cache,
            ivars->begin
                bodies = cell(nI, 2)
                bodies[1] = quote
                        _jl_copy_chunks(B.chunks, ind, X.chunks, refind, l0)
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
function assign(B::BitArray, X::BitArray, I0::Range1{Int}, I::Union(Integer, Range1{Int})...)
    I = map(x->(isa(x,Integer) ? (x:x) : x), I)
    _jl_assign_array2bitarray_ranges(B, X, I0, I...)
end

function assign{T<:Integer}(B::BitArray, x::Number, I::AbstractVector{T})
    for i in I
        B[i] = x
    end
    return B
end

function assign{T<:Number,S<:Integer}(B::BitArray, X::AbstractArray{T}, I::AbstractVector{S})
    for i = 1:length(I)
        B[I[i]] = X[i]
    end
    return B
end

let assign_cache = nothing
    global assign
    function assign(B::BitArray, x::Number, I0::Indices, I::Indices...)
        if is(assign_cache,nothing)
            assign_cache = Dict()
        end
        gen_cartesian_map(assign_cache, ivars->:(B[$(ivars...)] = x),
            tuple(I0, I...),
            (:B, :x),
            B, x)
        return B
    end
end

# note: use of T<:Integer in the following is to disambiguate
assign{T<:Integer,S<:Number}(B::BitArray{T}, X::AbstractArray{S}, I0::Integer) =
    (B[I0] = X[1])

let assign_cache = nothing
    global assign
    function assign{T<:Integer,S<:Number}(B::BitArray{T}, X::AbstractArray{S}, I0::Indices, I::Indices...)
        if is(assign_cache,nothing)
            assign_cache = Dict()
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

function _jl_assign_bool_scalar_1d(A::BitArray, x::Number, I::AbstractArray{Bool})
    n = sum(I)
    for i = 1:numel(I)
        if I[i]
            A[i] = x
        end
    end
    A
end

function _jl_assign_bool_vector_1d{T<:Number}(A::BitArray, X::AbstractArray{T}, I::AbstractArray{Bool})
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

assign{T<:Number}(A::BitArray, X::AbstractArray{T}, I::AbstractVector{Bool}) = _jl_assign_bool_vector_1d(A, X, I)
assign{T<:Number}(A::BitArray, X::AbstractArray{T}, I::AbstractArray{Bool}) = _jl_assign_bool_vector_1d(A, X, I)
assign(A::BitArray, x::Number, I::AbstractVector{Bool}) = _jl_assign_bool_scalar_1d(A, x, I)
assign(A::BitArray, x::Number, I::AbstractArray{Bool}) = _jl_assign_bool_scalar_1d(A, x, I)

# note: use of T<:Integer in the following is to disambiguate
assign{T<:Integer,S<:Number}(A::BitMatrix{T}, x::AbstractArray{S}, I::Integer, J::AbstractVector{Bool}) =
    (A[I,find(J)] = x)

assign{T<:Integer}(A::BitMatrix{T}, x::Number, I::Integer, J::AbstractVector{Bool}) =
    (A[I,find(J)] = x)

assign{T<:Integer,S<:Number}(A::BitMatrix{T}, x::AbstractArray{S}, I::AbstractVector{Bool}, J::Integer) =
    (A[find(I),J] = x)

assign{T<:Integer}(A::BitMatrix{T}, x::Number, I::AbstractVector{Bool}, J::Integer) =
    (A[find(I),J] = x)

assign{T<:Integer,S<:Number}(A::BitMatrix{T}, x::AbstractArray{S}, I::AbstractVector{Bool}, J::AbstractVector{Bool}) =
    (A[find(I),find(J)] = x)

assign{T<:Integer}(A::BitMatrix{T}, x::Number, I::AbstractVector{Bool}, J::AbstractVector{Bool}) =
    (A[find(I),find(J)] = x)


## Dequeue functionality ##

function push{T<:Integer}(B::BitVector{T}, item)
    # convert first so we don't grow the bitarray if the assignment won't work
    item = convert(T, item)
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

append!{T<:Integer}(B::BitVector{T}, items::AbstractVector{T}) = append!(B, bitpack(items))
append!{T<:Integer}(A::Vector{T}, items::BitVector{T}) = append!(A, bitunpack(items))

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

function enqueue{T<:Integer}(B::BitVector{T}, item)
    item = convert(T, item)
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

function insert{T<:Integer}(B::BitVector{T}, i::Integer, item)
    if i < 1
        throw(BoundsError())
    end
    item = convert(T, item)
    if item != 0 && item != 1
        error("invalid BitArray value")
    end
    n = length(B)
    if i > n
        x = bitzeros(T, i - n)
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

        msk_aft = ((~uint64(0)) << j)
        msk_bef = ~msk_aft
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

## Misc functions

for f in (:iround, :itrunc, :ifloor, :iceil, :abs)
    @eval ($f)(B::BitArray) = copy(B)
end

## Unary operators ##

(~)(B::BitArray) = ~bitunpack(B)

(-)(B::BitArray) = -bitunpack(B)
sign(B::BitArray) = copy(B)

real(B::BitArray) = copy(B)
imag{T}(B::BitArray{T}) = bitzeros(T, size(B))

conj!(B::BitArray) = B
conj(B::BitArray) = copy(B)

# Bools have special treatment
function (~)(B::BitArray{Bool})
    C = similar(B)
    for i = 1:length(B.chunks) - 1
        C.chunks[i] = (~)(B.chunks[i])
    end
    l = length(B) & 0x3f
    msk = (~uint64(0)) >>> (64 - l)
    C.chunks[end] = msk & (~B.chunks[end])
    return C
end
!(B::BitArray{Bool}) = ~B
(-)(B::BitArray{Bool}) = copy(B)
sign(B::BitArray{Bool}) = convert(BitArray{Int}, B)


## Binary arithmetic operators ##

for f in (:+, :-, :div, :mod, :./, :.^, :/, :\)
    @eval begin
        ($f)(A::BitArray, B::BitArray) = ($f)(bitunpack(A), bitunpack(B))
        ($f)(B::BitArray, x::Number) = ($f)(bitunpack(B), x)
        ($f)(x::Number, B::BitArray) = ($f)(x, bitunpack(B))
    end
end

for f in (:&, :|, :$)
    @eval begin
        function ($f){T<:Integer}(A::BitArray{T}, B::BitArray{T})
            F = BitArray(T, promote_shape(size(A),size(B))...)
            for i = 1:length(F.chunks) - 1
                F.chunks[i] = ($f)(A.chunks[i], B.chunks[i])
            end
            l = length(F) & 0x3f
            msk = (~uint64(0)) >>> (64 - l)
            F.chunks[end] = msk & ($f)(A.chunks[end], B.chunks[end])
            return F
        end
        ($f)(x::Number, B::BitArray) = ($f)(x, bitunpack(B))
        ($f)(A::BitArray, x::Number) = ($f)(x, A)
    end
end

(.*){T<:Integer}(A::BitArray{T}, B::BitArray{T}) = A & B
(.*)(x::Number, B::BitArray) = x .* bitunpack(B)
(.*)(A::BitArray, x::Number) = x .* A

for f in (:+, :-, :div, :mod, :./, :.^, :.*, :&, :|, :$)
    @eval begin
        ($f)(A::BitArray, B::AbstractArray) = ($f)(bitunpack(A), B)
        ($f)(A::AbstractArray, B::BitArray) = ($f)(A, bitunpack(B))
    end
end

# specialized Bool versions
function (&)(x::Number, B::BitArray{Bool})
    if bool(x)
        return copy(B)
    else
        return bitzeros(Bool, size(B))
    end
end
function (|)(x::Number, B::BitArray{Bool})
    if bool(x)
        return bitones(Bool, size(B))
    else
        return copy(B)
    end
end
function ($)(x::Number, B::BitArray{Bool})
    if bool(x)
        return ~B
    else
        return copy(B)
    end
end
(.*)(x::Number, B::BitArray{Bool}) = x & B

## promotion to complex ##

# TODO?

## Binary comparison operators ##

# note: these return BitArray{Bool}
for f in (:(==), :!=, :<, :<=)
    @eval begin
        function ($f)(A::BitArray, B::BitArray)
            F = BitArray(Bool, promote_shape(size(A),size(B)))
            for i = 1:numel(A)
                F[i] = ($f)(A[i], B[i])
            end
            return F
        end
        function ($f)(x::Number, B::BitArray)
            F = similar(B, Bool)
            for i = 1:numel(F)
                F[i] = ($f)(x, B[i])
            end
            return F
        end
        function ($f)(A::BitArray, x::Number)
            F = similar(A, Bool)
            for i = 1:numel(F)
                F[i] = ($f)(A[i], x)
            end
            return F
        end
    end
end

for f in (:(==), :!=, :<, :<=)
    @eval begin
        ($f)(A::BitArray, B::AbstractArray) = ($f)(bitunpack(A), B)
        ($f)(A::AbstractArray, B::BitArray) = ($f)(A, bitunpack(B))
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

    B = similar(A, d_out)
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
                _jl_copy_chunks(B.chunks, boffs, A.chunks, offs, M)
            end
        end
    end
    return B
end

function rotl90(A::BitMatrix)
    m,n = size(A)
    B = similar(A,(n,m))
    for i=1:m, j=1:n
        B[n-j+1,i] = A[i,j]
    end
    return B
end
function rotr90(A::BitMatrix)
    m,n = size(A)
    B = similar(A,(n,m))
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

function nnz(B::BitArray)
    n = 0
    for i = 1:length(B.chunks)
        n += count_ones(B.chunks[i])
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
        findn_cache = Dict()
    end

    gen_cartesian_map(findn_cache, findn_one, ranges,
                      (:B, :I, :count, :z), B,I,1, 0)
    return I
end
end

function findn_nzs{T<:Integer}(B::BitMatrix{T})
    I, J = findn(B)
    return (I, J, bitones(T, length(I)))
end

nonzeros{T<:Integer}(B::BitArray{T}) = bitones(T, nnz(B))

## Reductions ##

areduce(f::Function, B::BitArray, region::Region, v0, RType::Type) =
    areduce(f, bitunpack(B), region, v0, RType)

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
function bitareduce{T<:Integer}(f::Function, A::BitArray{T}, region::Region, v0)
    dimsA = size(A)
    ndimsA = ndims(A)
    dimsR = ntuple(ndimsA, i->(contains(region, i) ? 1 : dimsA[i]))
    R = BitArray(T, dimsR)

    if is(bitareduce_cache,nothing)
        bitareduce_cache = Dict()
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

max{T}(A::BitArray{T}, b::(), region::Region) = bitareduce(max,A,region,typemin(T))
min{T}(A::BitArray{T}, b::(), region::Region) = bitareduce(min,A,region,typemax(T))
sum{T}(A::BitArray{T}, region::Region)  = areduce(+,A,region,0,Int)
prod{T}(A::BitArray{T}, region::Region) = bitareduce(*,A,region,one(T))

sum(B::BitArray) = nnz(B)

prod{T}(B::BitArray{T}) = (nnz(B) == length(B) ? one(T) : zero(T))

min(B::BitArray) = prod(B)
max{T}(B::BitArray{T}) = (nnz(B) > 0 ? one(T) : zero(T))

## map over bitarrays ##

# TODO

## Filter ##

function filter(f::Function, Bs::BitArray)
    boolmap::Array{Bool} = map(f, Bs)
    Bs[boolmap]
end

## Transpose ##

transpose(B::BitVector) = reshape(copy(B), 1, length(B))

# fast 8x8 bit transpose from Henry S. Warrens's "Hacker's Delight"
# http://www.hackersdelight.org/HDcode/transpose8.c.txt
function _jl_transpose8rS64(x::Uint64)
   t = (x $ (x >>> 7)) & 0x00aa00aa00aa00aa;
   x = x $ t $ (t << 7);
   t = (x $ (x >>> 14)) & 0x0000cccc0000cccc
   x = x $ t $ (t << 14);
   t = (x $ (x >>> 28)) & 0x00000000f0f0f0f0
   x = x $ t $ (t << 28);
   return x
end

function _jl_form_8x8_chunk(B::BitMatrix, i1::Int, i2::Int, m::Int, cgap::Int, cinc::Int, nc::Int, msk8::Uint64)
    x = uint64(0)

    k, l = _jl_get_chunks_id(i1 + (i2 - 1) * m)
    r = 0
    for j = 1 : 8
        if k > nc
            break
        end
        x |= ((B.chunks[k] >>> l) & msk8) << r
        if l + 8 >= 64 && nc > k
            r0 = 8 - ((l + 8) & 63)
            x |= (B.chunks[k + 1] & (msk8 >>> r0)) << (r + r0)
        end
        k += cgap + (l + cinc >= 64 ? 1 : 0)
        l = (l + cinc) & 63
        r += 8
    end
    return x
end

# note: assumes B is filled with 0's
function _jl_put_8x8_chunk(B::BitMatrix, i1::Int, i2::Int, x::Uint64, m::Int, cgap::Int, cinc::Int, nc::Int, msk8::Uint64)
    k, l = _jl_get_chunks_id(i1 + (i2 - 1) * m)
    r = 0
    for j = 1 : 8
        if k > nc
            break
        end
        B.chunks[k] |= ((x >>> r) & msk8) << l
        if l + 8 >= 64 && nc > k
            r0 = 8 - ((l + 8) & 63)
            B.chunks[k + 1] |= ((x >>> (r + r0)) & (msk8 >>> r0))
        end
        k += cgap + (l + cinc >= 64 ? 1 : 0)
        l = (l + cinc) & 63
        r += 8
    end
    return
end

function transpose{T<:Integer}(B::BitMatrix{T})
    l1 = size(B, 1)
    l2 = size(B, 2)
    Bt = bitzeros(T, l2, l1)

    cgap1 = l1 >>> 6
    cinc1 = l1 & 63

    cgap2 = l2 >>> 6
    cinc2 = l2 & 63
    nc = length(B.chunks)


    for i = 1 : 8 : l1

        msk8_1 = uint64(0xff)
        if (l1 < i + 7)
            msk8_1 >>>= i + 7 - l1
        end

        for j = 1 : 8 : l2
            x = _jl_form_8x8_chunk(B, i, j, l1, cgap1, cinc1, nc, msk8_1)
            x = _jl_transpose8rS64(x)

            msk8_2 = uint64(0xff)
            if (l2 < j + 7)
                msk8_2 >>>= j + 7 - l2
            end

            _jl_put_8x8_chunk(Bt, j, i, x, l2, cgap2, cinc2, nc, msk8_2)
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
    P = similar(B, dimsP)
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
	permute_cache = Dict()
    end

    gen_cartesian_map(permute_cache, permute_one, ranges,
                      tuple(:B, :P, :perm, :offset, stridenames[1:ndimsB]...),
                      B, P, perm, offset, strides...)

    return P
end
end # let

## Concatenation ##

function hcat{T}(B::BitVector{T}...)
    height = length(B[1])
    for j = 2:length(B)
        if length(B[j]) != height; error("hcat: mismatched dimensions"); end
    end
    M = BitArray(T, height, length(B))
    for j = 1:length(B)
        _jl_copy_chunks(M.chunks, (height * (j - 1)) + 1, B[j].chunks, height)
    end
    return M
end

function vcat{T}(V::BitVector{T}...)
    n = 0
    for Vk in V
        n += length(Vk)
    end
    B = BitArray(T, n)
    j = 1
    for Vk in V
        _jl_copy_chunks(B.chunks, j, Vk.chunks, length(Vk))
        j += length(Vk)
    end
    return B
end

function hcat{T}(A::Union(BitMatrix{T},BitVector{T})...)
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

    B = BitArray(T, nrows, ncols)

    pos = 1
    for k=1:nargs
        Ak = A[k]
        n = numel(Ak)
        _jl_copy_chunks(B.chunks, pos, Ak.chunks, n)
        pos += n
    end
    return B
end

function vcat{T}(A::BitMatrix{T}...)
    nargs = length(A)
    nrows = sum(a->size(a, 1), A)::Int
    ncols = size(A[1], 2)
    for j = 2:nargs
        if size(A[j], 2) != ncols; error("vcat: mismatched dimensions"); end
    end
    B = BitArray(T, nrows, ncols)
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
function cat{T}(catdim::Integer, X::Union(BitArray{T}, Integer)...)
    nargs = length(X)
    # using integers != 0 or 1 results in conversion to Array{Int}
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
        C = BitArray(typeC, dimsC)
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

## Reductions and scans ##

function isequal(A::BitArray, B::BitArray)
    if size(A) != size(B)
        return false
    end
    for i = 1:length(A.chunks)
        if A.chunks[i] != B.chunks[i]
            return false
        end
    end
    return true
end

function cumsum{T}(v::BitVector{T})
    n = length(v)
    c = Array(T, n)
    if n == 0; return c; end

    c[1] = v[1]
    for i=2:n
        c[i] = v[i] + c[i-1]
    end
    return c
end
function cumsum(v::BitVector{Bool})
    n = length(v)
    c = bitones(Bool, n)
    for i=1:n
        if !v[i]
            c[i] = false
        else
            break
        end
    end
    return c
end

function cumprod{T}(v::BitVector{T})
    n = length(v)
    c = bitzeros(T, n)
    for i=1:n
        if v[i] == one(T)
            c[i] = one(T)
        else
            break
        end
    end
    return c
end
