# prelimnary definitions: constants, macros
# and functions used throughout the code
const _msk64 = ~uint64(0)
macro _mskr(l) :(_msk64 >>> (64-$(esc(l)))) end
macro _div64(l) :($(esc(l)) >>> 6) end
macro _mod64(l) :($(esc(l)) & 63) end
macro _msk_end(l) :(@_mskr @_mod64 $(esc(l))) end
num_bit_chunks(n::Int) = @_div64 (n+63)

## BitArray

# notes: bits are stored in contiguous chunks
#        unused bits must always be set to 0
type BitArray{N} <: AbstractArray{Bool, N}
    chunks::Vector{Uint64}
    dims::Vector{Int}
    function BitArray(dims::Int...)
        if length(dims) == 0
            dims = 0
        end
        n = prod(dims)
        nc = num_bit_chunks(n)
        chunks = Array(Uint64, nc)
        if nc > 0
            chunks[end] = uint64(0)
        end
        new(chunks, [i::Int for i in dims])
    end
end

BitArray() = BitArray(0)
BitArray(dims::Dims) = BitArray{max(length(dims), 1)}(dims...)
BitArray(dims::Int...) = BitArray{max(length(dims), 1)}(dims...)

typealias BitVector BitArray{1}
typealias BitMatrix BitArray{2}

## utility functions ##

length(B::BitArray) = prod(B.dims)
eltype(B::BitArray) = Bool
ndims{N}(B::BitArray{N}) = N
length(B::BitArray) = prod(B.dims)
size(B::BitArray) = tuple(B.dims...)

## Aux functions ##

get_chunks_id(i::Integer) = @_div64(int(i-1))+1, @_mod64(int(i-1))

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

## similar, fill!, copy! etc ##

similar(B::BitArray) = BitArray(B.dims...)
similar(B::BitArray, dims::Int...) = BitArray(dims)
similar(B::BitArray, dims::Dims) = BitArray(dims...)

similar(B::BitArray, T::Type{Bool}, dims::Dims) = BitArray(dims)
# changing type to a non-Bool returns an Array
# (this triggers conversions like float(bitvector) etc.)
similar(B::BitArray, T::Type, dims::Dims) = Array(T, dims)

function fill!(B::BitArray, x)
    y = convert(Bool, x)
    Bc = B.chunks
    if !y
        for i = 1 : length(B.chunks)
            Bc[i] = uint64(0)
        end
    else
        if length(B) == 0
            return B
        end
        for i = 1 : length(B.chunks) - 1
            Bc[i] = _msk64
        end
        Bc[end] = @_msk_end length(B)
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
    Br = BitArray{N}()
    Br.chunks = B.chunks
    Br.dims = Int[i for i in dims]
    return Br
end

## Conversions ##

convert{T,N}(::Type{Array{T}}, B::BitArray{N}) = convert(Array{T,N},B)
function convert{T,N}(::Type{Array{T,N}}, B::BitArray{N})
    A = Array(T, size(B))
    for i = 1:length(A)
        A[i] = B[i]
    end
    return A
end

convert{T,N}(::Type{BitArray}, A::AbstractArray{T,N}) = convert(BitArray{N},A)
function convert{T,N}(::Type{BitArray{N}}, A::AbstractArray{T,N})
    B = BitArray(size(A))
    for i = 1:length(B)
        B[i] = A[i]
    end
    return B
end

convert{N}(::Type{BitArray{N}}, B::BitArray{N}) = B

reinterpret{N}(::Type{Bool}, B::BitArray, dims::NTuple{N,Int}) = reinterpret(B, dims)
function reinterpret{N}(B::BitArray, dims::NTuple{N,Int})
    if prod(dims) != length(B)
        error("reinterpret: invalid dimensions")
    end
    A = BitArray{N}()
    A.dims = [i::Int for i in dims]
    A.chunks = B.chunks
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

## Indexing: ref ##

function ref(B::BitArray, i::Integer)
    if i < 1 || i > length(B)
        throw(BoundsError())
    end
    i1, i2 = get_chunks_id(i)
    return (B.chunks[i1] >>> i2) & one(Uint64) == one(Uint64)
end

# 0d bitarray
ref(B::BitArray{0}) = B[1]

ref(B::BitArray, i0::Real, i1::Real) = B[to_index(i0) + size(B,1)*(to_index(i1)-1)]
ref(B::BitArray, i0::Real, i1::Real, i2::Real) =
    B[to_index(i0) + size(B,1)*((to_index(i1)-1) + size(B,2)*(to_index(i2)-1))]
ref(B::BitArray, i0::Real, i1::Real, i2::Real, i3::Real) =
    B[to_index(i0) + size(B,1)*((to_index(i1)-1) + size(B,2)*((to_index(i2)-1) + size(B,3)*(to_index(i3)-1)))]

#       ref(::BitArray, ::Real) is shadowed (?)
function ref(B::BitArray, I::Real...)
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
let ref_cache = nothing
    global ref
    function ref(B::BitArray, I0::Range1{Int}, I::Union(Real,Range1{Int})...)
        # the < should become a != once
        # the stricter indexing behaviour is enforced
        if ndims(B) < 1 + length(I)
            error("wrong number of dimensions in ref")
        end
        X = BitArray(ref_shape(I0, I...))
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

        if is(ref_cache,nothing)
            ref_cache = Dict()
        end

        gen_cartesian_map(ref_cache,
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
function ref{T<:Real}(B::BitArray, I::AbstractVector{T})
    X = BitArray(length(I))
    ind = 1
    for i in I
        X[ind] = B[i]
        ind += 1
    end
    return X
end

let ref_cache = nothing
    global ref
    function ref(B::BitArray, I::Union(Real,AbstractArray)...)
        I = indices(I)
        X = BitArray(ref_shape(I...))

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

function ref_bool_1d(B::BitArray, I::AbstractArray{Bool})
    n = sum(I)
    out = BitArray(n)
    c = 1
    for i = 1:length(I)
        if I[i]
            out[c] = B[i]
            c += 1
        end
    end
    out
end

ref(B::BitVector, I::AbstractVector{Bool}) = ref_bool_1d(B, I)
ref(B::BitVector, I::AbstractArray{Bool}) = ref_bool_1d(B, I)
ref(B::BitArray, I::AbstractVector{Bool}) = ref_bool_1d(B, I)
ref(B::BitArray, I::AbstractArray{Bool}) = ref_bool_1d(B, I)

ref(B::BitMatrix, I::Real, J::AbstractVector{Bool}) = B[I, find(J)]
ref(B::BitMatrix, I::AbstractVector{Bool}, J::Real) = B[find(I), J]
ref(B::BitMatrix, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = B[find(I), find(J)]
ref(B::BitMatrix, I::Range1{Int}, J::AbstractVector{Bool}) = B[I, find(J)]
ref{T<:Real}(B::BitMatrix, I::AbstractVector{T}, J::AbstractVector{Bool}) = B[I, find(J)]
ref{T<:Real}(B::BitMatrix, I::AbstractVector{Bool}, J::AbstractVector{T}) = B[find(I), J]

## Indexing: assign ##

function assign(B::BitArray, x, i::Real)
    i = to_index(i)
    if i < 1 || i > length(B)
        throw(BoundsError())
    end
    i1, i2 = get_chunks_id(i)
    u = uint64(1)
    y = convert(Bool, x)
    if !y
        B.chunks[i1] &= ~(u << i2)
    else
        B.chunks[i1] |= (u << i2)
    end
    return B
end

assign(B::BitArray, x, i0::Real, i1::Real) =
    B[to_index(i0) + size(B,1)*(to_index(i1)-1)] = x

assign(B::BitArray, x, i0::Real, i1::Real, i2::Real) =
    B[to_index(i0) + size(B,1)*((to_index(i1)-1) + size(B,2)*(to_index(i2)-1))] = x

assign(B::BitArray, x, i0::Real, i1::Real, i2::Real, i3::Real) =
    B[to_index(i0) + size(B,1)*((to_index(i1)-1) + size(B,2)*((to_index(i2)-1) + size(B,3)*(to_index(i3)-1)))] = x

function assign(B::BitArray, x, I0::Real, I::Real...)
    index = to_index(I0)
    stride = 1
    for k = 1:length(I)
        stride = stride * size(B, k)
        index += (to_index(I[k]) - 1) * stride
    end
    B[index] = x
    return B
end

let assign_cache = nothing
    global assign_array2bitarray_ranges
    function assign_array2bitarray_ranges(B::BitArray, X::BitArray, I0::Range1{Int}, I::Range1{Int}...)
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
        if is(assign_cache,nothing)
            assign_cache = Dict()
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

        gen_cartesian_map(assign_cache,
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
function assign(B::BitArray, X::BitArray, I0::Range1{Int}, I::Union(Integer, Range1{Int})...)
    I = map(x->(isa(x,Integer) ? (x:x) : x), I)
    assign_array2bitarray_ranges(B, X, I0, I...)
end

function assign{T<:Real}(B::BitArray, X::AbstractArray, I::AbstractVector{T})
    if length(X) != length(I); error("argument dimensions must match"); end
    count = 1
    for i in I
        B[i] = X[count]
        count += 1
    end
    return B
end

function assign(B::BitArray, X::AbstractArray, i0::Real)
    if length(X) != 1
        error("argument dimensions must match")
    end
    return assign(B, X[1], i0)
end

function assign(B::BitArray, X::AbstractArray, i0::Real, i1::Real)
    if length(X) != 1
        error("argument dimensions must match")
    end
    return assign(B, X[1], i0, i1)
end

function assign(B::BitArray, X::AbstractArray, I0::Real, I::Real...)
    if length(X) != 1
        error("argument dimensions must match")
    end
    return assign(B, X[1], i0, I...)
end

let assign_cache = nothing
    global assign
    function assign(B::BitArray, X::AbstractArray, I::Union(Real,AbstractArray)...)
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
        if is(assign_cache,nothing)
            assign_cache = Dict()
        end
        gen_cartesian_map(assign_cache,
            ivars->:(B[$(ivars...)] = X[refind]; refind += 1),
            I,
            (:B, :X, :refind),
            B, X, 1)
        return B
    end
end

function assign{T<:Real}(B::BitArray, x, I::AbstractVector{T})
    for i in I
        B[i] = x
    end
    return B
end

let assign_cache = nothing
    global assign
    function assign(B::BitArray, x, I::Union(Real,AbstractArray)...)
        I = indices(I)
        if is(assign_cache,nothing)
            assign_cache = Dict()
        end
        gen_cartesian_map(assign_cache, ivars->:(B[$(ivars...)] = x),
            I,
            (:B, :x),
            B, x)
        return B
    end
end

# logical indexing

function assign_bool_scalar_1d(A::BitArray, x, I::AbstractArray{Bool})
    n = sum(I)
    for i = 1:length(I)
        if I[i]
            A[i] = x
        end
    end
    A
end

function assign_bool_vector_1d(A::BitArray, X::AbstractArray, I::AbstractArray{Bool})
    n = sum(I)
    c = 1
    for i = 1:length(I)
        if I[i]
            A[i] = X[c]
            c += 1
        end
    end
    A
end

assign(A::BitArray, X::AbstractArray, I::AbstractVector{Bool}) = assign_bool_vector_1d(A, X, I)
assign(A::BitArray, X::AbstractArray, I::AbstractArray{Bool}) = assign_bool_vector_1d(A, X, I)
assign(A::BitArray, x, I::AbstractVector{Bool}) = assign_bool_scalar_1d(A, x, I)
assign(A::BitArray, x, I::AbstractArray{Bool}) = assign_bool_scalar_1d(A, x, I)

assign(A::BitMatrix, x::AbstractArray, I::Real, J::AbstractVector{Bool}) =
    (A[I,find(J)] = x)

assign(A::BitMatrix, x, I::Real, J::AbstractVector{Bool}) =
    (A[I,find(J)] = x)

assign(A::BitMatrix, x::AbstractArray, I::AbstractVector{Bool}, J::Real) =
    (A[find(I),J] = x)

assign(A::BitMatrix, x, I::AbstractVector{Bool}, J::Real) =
    (A[find(I),J] = x)

assign(A::BitMatrix, x::AbstractArray, I::AbstractVector{Bool}, J::AbstractVector{Bool}) =
    (A[find(I),find(J)] = x)

assign(A::BitMatrix, x, I::AbstractVector{Bool}, J::AbstractVector{Bool}) =
    (A[find(I),find(J)] = x)

assign{T<:Integer}(A::BitMatrix, x::AbstractArray, I::AbstractVector{T}, J::AbstractVector{Bool}) =
    (A[I,find(J)] = x)

assign{T<:Real}(A::BitMatrix, x, I::AbstractVector{T}, J::AbstractVector{Bool}) =
    (A[I,find(J)] = x)

assign{T<:Real}(A::BitMatrix, x::AbstractArray, I::AbstractVector{Bool}, J::AbstractVector{T}) =
    (A[find(I),J] = x)

assign{T<:Real}(A::BitMatrix, x, I::AbstractVector{Bool}, J::AbstractVector{T}) =
    (A[find(I),J] = x)

## Dequeue functionality ##

function push!(B::BitVector, item)
    # convert first so we don't grow the bitarray if the assignment won't work
    item = convert(Bool, item)

    l = @_mod64 length(B)
    if l == 0
        ccall(:jl_array_grow_end, Void, (Any, Uint), B.chunks, 1)
        B.chunks[end] = uint64(0)
    end
    B.dims[1] += 1
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
    k0 = length(B.chunks)
    k1 = num_bit_chunks(n0 + n1)
    if k1 > k0
        ccall(:jl_array_grow_end, Void, (Any, Uint), B.chunks, k1 - k0)
        B.chunks[end] = uint64(0)
    end
    B.dims[1] += n1
    copy_chunks(B.chunks, n0+1, items.chunks, 1, n1)
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
        delete!(B, n+1:n0)
        return B
    end
    k0 = length(B.chunks)
    k1 = num_bit_chunks(int(n))
    if k1 > k0
        ccall(:jl_array_grow_end, Void, (Any, Uint), B.chunks, k1 - k0)
        B.chunks[end] = uint64(0)
    end
    B.dims[1] = n
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
    B.dims[1] -= 1

    return item
end

function unshift!(B::BitVector, item)
    item = convert(Bool, item)

    l = @_mod64 length(B)
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
        B.chunks[i] = (B.chunks[i] << 1) | (B.chunks[i-1] >>> 63) 
    end
    B.chunks[1] = uint64(item) | (B.chunks[1] << 1)
    return B
end

function shift!(B::BitVector)
    if isempty(B)
        error("shift!: BitArray is empty")
    end
    item = B[1]

    for i = 1 : length(B.chunks) - 1
        B.chunks[i] = (B.chunks[i] >>> 1) | (B.chunks[i+1] << 63) 
    end

    l = @_mod64 length(B)
    if l == 1
        ccall(:jl_array_del_end, Void, (Any, Uint), B.chunks, 1)
    else
        B.chunks[end] >>>= 1
    end
    B.dims[1] -= 1

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
        k, j = get_chunks_id(i)

        l = @_mod64 length(B)
        if l == 0
            ccall(:jl_array_grow_end, Void, (Any, Uint), B.chunks, 1)
            B.chunks[end] = uint64(0)
        end
        B.dims[1] += 1

        for t = length(B.chunks) : -1 : k + 1
            B.chunks[t] = (B.chunks[t] << 1) | (B.chunks[t - 1] >>> 63) 
        end

        msk_aft = (_msk64 << j)
        msk_bef = ~msk_aft
        B.chunks[k] = (msk_bef & B.chunks[k]) | ((msk_aft & B.chunks[k]) << 1)
    end
    B[i] = item
end

function delete!(B::BitVector, i::Integer)
    n = length(B)
    if !(1 <= i <= n)
        throw(BoundsError())
    end
    v = B[i]

    k, j = get_chunks_id(i)

    msk_bef = _msk64 >>> (63 - j)
    msk_aft = ~msk_bef
    msk_bef >>>= 1

    B.chunks[k] = (msk_bef & B.chunks[k]) | ((msk_aft & B.chunks[k]) >> 1)
    if length(B.chunks) > k
        B.chunks[k] |= (B.chunks[k + 1] << 63)
    end

    for t = k + 1 : length(B.chunks) - 1
        B.chunks[t] = (B.chunks[t] >>> 1) | (B.chunks[t + 1] << 63) 
    end

    l = @_mod64 length(B)

    if l == 1
        ccall(:jl_array_del_end, Void, (Any, Uint), B.chunks, 1)
    elseif length(B.chunks) > k
        B.chunks[end] >>>= 1
    end

    B.dims[1] -= 1

    return v
end

function delete!(B::BitVector, r::Range1{Int})
    n = length(B)
    i_f = first(r)
    i_l = last(r)
    if !(1 <= i_f && i_l <= n)
        throw(BoundsError())
    end
    if i_l < i_f
        return B
    end

    copy_chunks(B.chunks, i_f, B.chunks, i_l+1, n-i_l)

    delta_l = i_l - i_f + 1
    new_l = length(B) - delta_l
    delta_k = length(B.chunks) - num_bit_chunks(new_l)

    if delta_k > 0
        ccall(:jl_array_del_end, Void, (Any, Uint), B.chunks, delta_k)
    end

    if new_l > 0
        B.chunks[end] &= @_msk_end new_l
    end

    B.dims[1] = new_l

    return B
end

function empty!(B::BitVector)
    ccall(:jl_array_del_end, Void, (Any, Uint), B.chunks, length(B.chunks))
    B.dims[1] = 0
    return B
end

## Misc functions

for f in (:iround, :itrunc, :ifloor, :iceil, :abs)
    @eval ($f)(B::BitArray) = copy(B)
end

## Unary operators ##

function (-)(B::BitArray)
    A = zeros(Int, size(B))
    for i = 1:length(B)
        if B[i]
            A[i] = -1
        end
    end
    return A
end
sign(B::BitArray) = copy(B)

real(B::BitArray) = copy(B)
imag(B::BitArray) = falses(size(B))

conj!(B::BitArray) = B
conj(B::BitArray) = copy(B)

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

for (f,t) in ((:+,Int), (:-,Int), (:./,Float64))
    @eval begin
        function ($f)(A::BitArray, B::BitArray)
            shp = promote_shape(size(A),size(B))
            reshape(($t)[ ($f)(A[i], B[i]) for i=1:length(A) ], shp)
        end
        function ($f)(B::BitArray, x::Number)
            pt = typeof(($f)(true, one(x)))
            reshape(pt[ ($f)(B[i], x) for i = 1:length(B) ], size(B))
        end
        function ($f)(x::Number, B::BitArray)
            pt = typeof(($f)(true, one(x)))
            reshape(pt[ ($f)(x, B[i]) for i = 1:length(B) ], size(B))
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

# TODO: don't unpack
for f in (:div, :mod)
    @eval begin
        ($f)(A::BitArray, B::BitArray) = ($f)(bitunpack(A), bitunpack(B))
        ($f)(B::BitArray, x::Number) = ($f)(bitunpack(B), x)
        ($f)(x::Number, B::BitArray) = ($f)(x, bitunpack(B))
    end
end

function (&)(B::BitArray, x::Bool)
    if x
        return copy(B)
    else
        return falses(size(B))
    end
end
(&)(x::Bool, B::BitArray) = B & x

function (|)(B::BitArray, x::Bool)
    if x
        return trues(size(B))
    else
        return copy(B)
    end
end
(|)(x::Bool, B::BitArray) = B | x

function ($)(B::BitArray, x::Bool)
    if x
        return ~B
    else
        return copy(B)
    end
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
    if x
        return copy(B)
    else
        return trues(size(B))
    end
end
function (.^)(x::Bool, B::BitArray)
    if x
        return trues(size(B))
    else
        return ~B
    end
end
function (.^){T<:Number}(x::T, B::BitArray)
    u = one(T)
    reshape(T[ B[i] ? x : u for i = 1:length(B) ], size(B))
end
function (.^){T<:Integer}(B::BitArray, x::T)
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
        return ones(T, size(B))
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
function (.^){T<:Integer}(A::BitArray, B::Array{T})
    F = BitArray(promote_shape(size(A),size(B))...)
    for i = 1:length(A)
        F[i] = A[i] .^ B[i]
    end
    return F
end

(.*)(A::BitArray, B::BitArray) = A & B
(.*)(A::Array{Bool}, B::BitArray) = A & B
(.*)(B::BitArray, A::Array{Bool}) = A & B
(.*)(x::Number, B::BitArray) = x .* bitunpack(B)
(.*)(B::BitArray, x::Number) = x .* B

for f in (:+, :-, :div, :mod, :./, :.^, :.*, :&, :|, :$)
    @eval begin
        ($f)(A::BitArray, B::AbstractArray) = ($f)(bitunpack(A), B)
        ($f)(A::AbstractArray, B::BitArray) = ($f)(A, bitunpack(B))
    end
end

## promotion to complex ##

# TODO?

## element-wise comparison operators returning BitArray{Bool} ##

for (f,scalarf) in ((:(.==),:(==)),
                      (:.<, :<),
                      (:.!=,:!=),
                      (:.<=,:<=))
    @eval begin
        function ($f)(A::AbstractArray, B::AbstractArray)
            F = BitArray(promote_shape(size(A),size(B)))
            for i = 1:length(B)
                F[i] = ($scalarf)(A[i], B[i])
            end
            return F
        end
        function ($f)(A, B::AbstractArray)
            F = BitArray(size(B))
            for i = 1:length(F)
                F[i] = ($scalarf)(A, B[i])
            end
            return F
        end
        function ($f)(A::AbstractArray, B)
            F = BitArray(size(A))
            for i = 1:length(F)
                F[i] = ($scalarf)(A[i], B)
            end
            return F
        end

    end
end

function (==)(A::BitArray, B::BitArray)
    if size(A) != size(B)
        return false
    end
    Ac = A.chunks; Bc = B.chunks
    for i = 1:length(A.chunks)
        if Ac[i] != Bc[i]
            return false
        end
    end
    return true
end

function (!=)(A::BitArray, B::BitArray)
    if size(A) != size(B)
        return true
    end
    Ac = A.chunks; Bc = B.chunks
    for i = 1:length(A.chunks)
        if Ac[i] != Bc[i]
            return true
        end
    end
    return false
end

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
    Bc = B.chunks
    for i = div(start-1,64)+1:length(Bc)
        if Bc[i] != 0
            return (i-1) << 6 + trailing_zeros(Bc[i]) + 1
        end
    end
    return 0
end
#findfirst(B::BitArray) = findnext(B, 1)  ## defined in array.jl

# aux function: same as findfirst(~B), but performed without temporaries
function findnextnot(B::BitArray, start::Integer)
    Bc = B.chunks
    l = length(Bc)
    if l == 0
        return 0
    end
    for i = div(start-1,64)+1:l-1
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
    nnzB = nnz(B)
    I = Array(Int, nnzB)
    count = 1
    for i = 1:length(B)
        if B[i]
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

function findn_nzs(B::BitMatrix)
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

min(B::BitArray) = all(B)
max(B::BitArray) = any(B)

## map over bitarrays ##

function map!(f, A::Union(StridedArray,BitArray))
    for i=1:length(A)
        A[i] = f(A[i])
    end
    return A
end

function map!(f, dest::Union(StridedArray,BitArray), A::Union(StridedArray,BitArray))
    for i=1:length(A)
        dest[i] = f(A[i])
    end
    return dest
end

function map!(f, dest::Union(StridedArray,BitArray), A::Union(StridedArray,BitArray), B::Union(StridedArray,BitArray))
    for i=1:length(A)
        dest[i] = f(A[i], B[i])
    end
    return dest
end

function map!(f, dest::Union(StridedArray,BitArray), A::Union(StridedArray,BitArray), B::Number)
    for i=1:length(A)
        dest[i] = f(A[i], B)
    end
    return dest
end

function map!(f, dest::Union(StridedArray,BitArray), A::Number, B::Union(StridedArray,BitArray))
    for i=1:length(B)
        dest[i] = f(A, B[i])
    end
    return dest
end

function map!(f, dest::Union(StridedArray,BitArray), As::Union(StridedArray,BitArray)...)
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

## Reductions and scans ##

isequal(A::BitArray, B::BitArray) = (A == B)

function cumsum(v::BitVector)
    n = length(v)
    c = trues(n)
    for i=1:n
        if !v[i]
            c[i] = false
        else
            break
        end
    end
    return c
end

function cumprod(v::BitVector)
    n = length(v)
    c = falses(n)
    for i=1:n
        if v[i]
            c[i] = true
        else
            break
        end
    end
    return c
end

# Hashing

hash(B::BitArray) = hash({B.dims, B.chunks})
