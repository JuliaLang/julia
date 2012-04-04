_jl_num_bit_chunks(n::Integer) = (n + 0x3f) >>> 6

function _jl_get_chunks_id(i::Integer)
    j = uint64(i - 1)
    return int(j >>> 6) + 1, int(j & 0x3f)
end

function _jl_print_bit_chunk(c::Uint64, l::Integer)
    for s = 0 : l - 1
        d = (c >>> s) & 1
        print("01"[d + 1])
        if mod(s + 1, 8) == 0
            print(" ")
        end
    end
end

_jl_print_bit_chunk(c::Uint64) = _jl_print_bit_chunk(c, 64)

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

typealias BitVector{T} BitArray{1}
typealias BitMatrix{T} BitArray{2}

# non-standard compact representation
function bitshow(B::BitArray)
    if length(B) == 0
        return
    end
    for i = 1 : length(B.chunks) - 1
        _jl_print_bit_chunk(B.chunks[i])
        print(": ")
    end
    l = length(B) & 0x3f
    if (l == 0)
        l = 64
    end
    _jl_print_bit_chunk(B.chunks[end], l)
end

summary(B::BitArray) = 
    strcat(dims2string(size(B)), " BitArray")

length(B::BitArray) = prod(B.dims)
eltype(B::BitArray) = Int
ndims{N}(B::BitArray{N}) = N
numel(B::BitArray) = prod(B.dims)
size(B::BitArray) = tuple(B.dims...)
isinteger(B::BitArray) = true
isreal(B::BitArray) = true
iscomplex(B::BitArray) = false
isbool(B::BitArray) = false

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

## random ##

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
        stride = stride * size(B, k - 1)
        index += (I[k] - 1) * stride
    end
    return B[index]
end

let ref_cache = nothing
global ref
function ref(B::BitArray, I::Indices...)
    X = similar(B, map(length, I)::Dims)

    if is(ref_cache,nothing)
        ref_cache = HashTable()
    end
    gen_cartesian_map(ref_cache, ivars -> quote
            X[storeind] = B[$(ivars...)];
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
        gen_cartesian_map(assign_cache, ivars->:(B[$(ivars...)] = X[refind];
        refind += 1),
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
    l = n0 & 0x3f
    if l == 0
        k0 += 1
        B.chunks[k0] = uint64(0)
    end
    B.chunks[k0] |= (items.chunks[1] << l)
    if k1 > k0
        for i = 1 : k1 - k0 - 1
            B.chunks[k0 + i] = (items.chunks[i] >>> (63 - l) >>> 1) | (items.chunks[i + 1] << l)
        end
        if length(items.chunks) == k1 - k0
            B.chunks[k1] = (items.chunks[k1 - k0] >>> (63 - l) >>> 1)
        else
            B.chunks[k1] = (items.chunks[k1 - k0] >>> (63 - l) >>> 1) | (items.chunks[k1 - k0 + 1] << l)
        end
    end
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
    l = n0 & 0x3f
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
        B[end] = item
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

    k_f, j_f = _jl_get_chunks_id(i_f)
    k_l, j_l = _jl_get_chunks_id(i_l)

    g_fl = (64 + j_l - j_f + 1) & 0x3f
    if (g_fl == 0)
        g_fl = 64
    end

    u = ~uint64(0)
    msk_bef_f = u >>> (63 - j_f) >>> 1
    msk_aft_l = ~(u >>> (63 - j_l))

    B.chunks[k_f] = (msk_bef_f & B.chunks[k_f]) | ((B.chunks[k_l] >>> j_l >>> 1) << j_f)
    if length(B.chunks) > k_l && (j_l >= j_f)
        B.chunks[k_f] |= (B.chunks[k_l + 1] << (64 - g_fl))
    end

    g_k = k_l - k_f - (j_l < j_f ? 1 : 0)
    for t = k_f + 1 : length(B.chunks) - (g_k + 1)
        B.chunks[t] = (B.chunks[t + g_k] >>> (g_fl - 1) >>> 1) | (B.chunks[t + g_k + 1] << (64 - g_fl)) 
    end

    delta_l = i_l - i_f + 1
    delta_k = length(B.chunks) - _jl_num_bit_chunks(length(B) - delta_l)

    if (length(B.chunks) - delta_k > k_f)
        B.chunks[end - g_k] = (B.chunks[end] >>> (g_fl - 1) >>> 1)
    end

    ccall(:jl_array_del_end, Void, (Any, Uint), B.chunks, delta_k)

    B.dims[1] -= delta_l

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

real(B::BitArray) = copy(B)
imag(B::BitArray) = bitzeros(size(B))

## Binary arithmetic operators ##

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
        function ($f)(x::Integer, B::BitArray)
            if x == 0
                u = uint64(0)
            elseif x == 1
                u = ~uint64(0)
            else
                error("invalid BitArray value")
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
        ($f)(A::BitArray, x::Integer) = ($f)(x, A)
    end
end

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

## nnz & find ##

_jl_bx64(x) = x - ((x >>> 1) & 0x7777777777777777) - ((x >>> 2) & 0x3333333333333333) - ((x >>> 3) & 0x1111111111111111)

function _jl_count_bits_in_chunk(c::Uint64)
    return int(mod((_jl_bx64(c) + (_jl_bx64(c) >>> 4)) & 0x0f0f0f0f0f0f0f0f, 0xff))
end

function nnz(B::BitArray)
    n = 0
    for i = 1:length(B.chunks)
        n += _jl_count_bits_in_chunk(B.chunks[i])
    end
    return n
end

function find(B::BitArray)
    nnzA = nnz(B)
    I = Array(Int, nnzA)
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
    return (I, J, ones(Int, length(I)))
end

nonzeros(B::BitArray) = ones(Int, nnz(B))

## Reductions ##

# TODO
# areduce
# functions with regions

sum(B::BitArray) = nnz(B)

prod(B::BitArray) = (nnz(B) == length(B) ? 1 : 0)

min(B::BitArray) = prod(B)
max(B::BitArray) = (nnz(B) > 0 ? 1 : 0)

## map over arrays ##

# TODO

## Transpose ##

function filter(f::Function, Bs::BitArray)
    boolmap::Array{Bool} = map(f, Bs)
    Bs[boolmap]
end

## Permute ##

transpose(B::BitVector) = reshape(B, 1, length(B))
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

# TODO
