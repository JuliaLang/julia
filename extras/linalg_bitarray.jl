## linalg_bitarray.jl: Basic Linear Algebra functions for BitArrays ##

require("bitarray")

import Base.dot, Base.triu, Base.tril, Base./, Base.\, Base.gradient
import Base.svd, Base.qr, Base.*, Base.diag, Base.diagm, Base.kron
import Base.issym, Base.ishermitian, Base.istriu, Base.istril, Base.findmax
import Base.findmin

function dot{T,S}(x::BitVector{T}, y::BitVector{S})
    # simplest way to mimic Array dot behavior
    s = zero(one(T) * one(S))
    for i = 1 : length(x.chunks)
        s += count_ones(x.chunks[i] & y.chunks[i])
    end
    return s
end

## slower than the unpacked version, which is MUCH slower
#  than blas'd (this one saves storage though, keeping it commented
#  just in case)
#function aTb{T,S}(A::BitMatrix{T}, B::BitMatrix{S})
    #(mA, nA) = size(A)
    #(mB, nB) = size(B)
    #C = zeros(promote_type(T,S), nA, nB)
    #if mA != mB; error("*: argument shapes do not match"); end
    #if mA == 0; return C; end
    #col_ch = _jl_num_bit_chunks(mA)
    ## TODO: avoid using aux chunks and copy (?)
    #aux_chunksA = zeros(Uint64, col_ch)
    #aux_chunksB = [zeros(Uint64, col_ch) for j=1:nB]
    #for j = 1:nB
        #_jl_copy_chunks(aux_chunksB[j], 1, B.chunks, (j-1)*mA+1, mA)
    #end
    #for i = 1:nA
        #_jl_copy_chunks(aux_chunksA, 1, A.chunks, (i-1)*mA+1, mA)
        #for j = 1:nB
            #for k = 1:col_ch
                #C[i, j] += count_ones(aux_chunksA[k] & aux_chunksB[j][k])
            #end
        #end
    #end
    #return C
#end

#aCb{T, S}(A::BitMatrix{T}, B::BitMatrix{S}) = aTb(A, B)

function triu{T}(B::BitMatrix{T}, k::Int)
    m,n = size(B)
    A = bitzeros(T, m,n)
    for i = max(k+1,1):n
        j = clamp((i - 1) * m + 1, 1, i * m)
        _jl_copy_chunks(A.chunks, j, B.chunks, j, min(i-k, m))
    end
    return A
end
triu(B::BitMatrix, k::Integer) = triu(B, int(k))

function tril{T}(B::BitMatrix{T}, k::Int)
    m,n = size(B)
    A = bitzeros(T, m, n)
    for i = 1:min(n, m+k)
        j = clamp((i - 1) * m + i - k, 1, i * m)
        _jl_copy_chunks(A.chunks, j, B.chunks, j, max(m-i+k+1, 0))
    end
    return A
end
tril(B::BitMatrix, k::Integer) = tril(B, int(k))

## Matrix multiplication and division

#disambiguations
(*){T<:Integer,S<:Integer}(A::BitMatrix{T}, B::BitVector{S}) = bitunpack(A) * bitunpack(B)
(*){T<:Integer,S<:Integer}(A::BitVector{T}, B::BitMatrix{S}) = bitunpack(A) * bitunpack(B)
(*){T<:Integer,S<:Integer}(A::BitMatrix{T}, B::BitMatrix{S}) = bitunpack(A) * bitunpack(B)
(*){T<:Integer}(A::BitMatrix{T}, B::AbstractVector) = (*)(bitunpack(A), B)
(*){T<:Integer}(A::BitVector{T}, B::AbstractMatrix) = (*)(bitunpack(A), B)
(*){T<:Integer}(A::BitMatrix{T}, B::AbstractMatrix) = (*)(bitunpack(A), B)
(*){T<:Integer}(A::AbstractVector, B::BitMatrix{T}) = (*)(A, bitunpack(B))
(*){T<:Integer}(A::AbstractMatrix, B::BitVector{T}) = (*)(A, bitunpack(B))
(*){T<:Integer}(A::AbstractMatrix, B::BitMatrix{T}) = (*)(A, bitunpack(B))
#end disambiguations

for f in (:/, :\, :*)
    @eval begin
        ($f)(A::BitArray, B::BitArray) = ($f)(bitunpack(A), bitunpack(B))
        ($f)(A::BitArray, B::AbstractArray) = ($f)(bitunpack(A), B)
        ($f)(A::AbstractArray, B::BitArray) = ($f)(A, bitunpack(B))
    end
end

# specialized Bool versions

#disambiguations (TODO: improve!)
(*)(A::BitMatrix{Bool}, B::BitVector{Bool}) = bitpack(bitunpack(A) * bitunpack(B))
(*)(A::BitVector{Bool}, B::BitMatrix{Bool}) = bitpack(bitunpack(A) * bitunpack(B))
(*)(A::BitMatrix{Bool}, B::BitMatrix{Bool}) = bitpack(bitunpack(A) * bitunpack(B))
#end disambiguations

# TODO: improve this!
(*)(A::BitArray{Bool}, B::BitArray{Bool}) = bitpack(bitunpack(A) * bitunpack(B))

## diff and gradient

# TODO: this could be improved (is it worth it?)
gradient(F::BitVector) = gradient(bitunpack(F))
gradient(F::BitVector, h::Real) = gradient(bitunpack(F), h)
gradient(F::Vector, h::BitVector) = gradient(F, bitunpack(h))
gradient(F::BitVector, h::Vector) = gradient(bitunpack(F), h)
gradient(F::BitVector, h::BitVector) = gradient(bitunpack(F), bitunpack(h))

## diag and related

function diag(B::BitMatrix)
    n = min(size(B))
    v = similar(B, n)
    for i = 1:n
        v[i] = B[i,i]
    end
    return v
end

function diagm{T}(v::Union(BitVector{T},BitMatrix{T}))
    if isa(v, BitMatrix)
        if (size(v,1) != 1 && size(v,2) != 1)
            error("Input should be nx1 or 1xn")
        end
    end

    n = numel(v)
    a = bitzeros(T, n, n)
    for i=1:n
        a[i,i] = v[i]
    end

    return a
end

## norm and rank

svd(A::BitMatrix) = svd(float(A))

qr(A::BitMatrix) = qr(float(A))

## kron

function kron{T,S}(a::BitVector{T}, b::BitVector{S})
    m = length(a)
    n = length(b)
    R = BitArray(promote_type(T,S), m, n)
    zS = zero(S)
    for j = 1:n
        if b[j] != zS
            _jl_copy_chunks(R.chunks, (j-1)*m+1, a.chunks, 1, m)
        end
    end
    return R
end

function kron{T,S}(a::BitMatrix{T}, b::BitMatrix{S})
    mA,nA = size(a)
    mB,nB = size(b)
    R = bitzeros(promote_type(T,S), mA*mB, nA*nB)

    zT = zero(T)
    for i = 1:mA
        ri = (1:mB)+(i-1)*mB
        for j = 1:nA
            if a[i,j] != zT
                rj = (1:nB)+(j-1)*nB
                R[ri,rj] = b
            end
        end
    end
    return R
end

## Structure query functions

function issym(A::BitMatrix)
    m, n = size(A)
    if m != n; error("matrix must be square, got $(m)x$(n)"); end
    return nnz(A - A.') == 0
end

ishermitian(A::BitMatrix) = issym(A)

function _jl_nonzero_chunks(chunks::Vector{Uint64}, pos0::Int, pos1::Int)

    k0, l0 = _jl_get_chunks_id(pos0)
    k1, l1 = _jl_get_chunks_id(pos1)

    delta_k = k1 - k0

    z = uint64(0)
    u = ~z
    if delta_k == 0
        msk_0 = (u << l0) & ~(u << l1 << 1)
    else
        msk_0 = (u << l0)
        msk_1 = ~(u << l1 << 1)
    end

    if (chunks[k0] & msk_0) != z
        return true
    end

    if delta_k == 0
        return false
    end

    for i = k0 + 1 : k1 - 1
        if chunks[i] != z
            return true
        end
    end

    if (chunks[k1] & msk_1) != z
        return true
    end

    return false
end

function istriu(A::BitMatrix)
    m, n = size(A)
    for j = 1:min(n,m-1)
        stride = (j-1)*m
        if _jl_nonzero_chunks(A.chunks, stride+j+1, stride+m)
            return false
        end
    end
    return true
end

function istril(A::BitMatrix)
    m, n = size(A)
    if m == 0 || n == 0
        return true
    end
    for j = 2:n
        stride = (j-1)*m
        if _jl_nonzero_chunks(A.chunks, stride+1, stride+min(j-1,m))
            return false
        end
    end
    return true
end

function findmax(a::BitArray)
    if length(a) == 0
        return (typemin(eltype(a)), 0)
    end
    m = zero(eltype(a))
    o = one(eltype(a))
    mi = 1
    ti = 1
    for i=1:length(a.chunks)
        k = trailing_zeros(a.chunks[i])
        ti += k
        if k != 64
            m = o
            mi = ti
            break
        end
    end
    return (m, mi)
end

function findmin(a::BitArray)
    if length(a) == 0
        return (typemax(eltype(a)), 0)
    end
    m = one(eltype(a))
    z = zero(eltype(a))
    mi = 1
    ti = 1
    for i=1:length(a.chunks) - 1
        k = trailing_ones(a.chunks[i])
        ti += k
        if k != 64
            return (z, ti)
        end
    end
    l = (@_mod64 (length(a)-1)) + 1
    msk = @_mskr l
    k = trailing_ones(a.chunks[end] & msk)
    ti += k
    if k != l
        m = z
        mi = ti
    end
    return (m, mi)
end
