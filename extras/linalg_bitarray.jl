## linalg_bitarray.jl: Basic Linear Algebra functions for BitArrays ##

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
    #z = zero(eltype(C))
    #if mA != mB; error("*: argument shapes do not match"); end
    #if mA == 0; return C; end
    #col_ch = _jl_num_bit_chunks(mA)
    ## TODO: avoid using aux chunks and copy (?)
    #aux_chunksA = zeros(Uint64, col_ch)
    #aux_chunksB = [zeros(Uint64, col_ch) | j=1:nB]
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

## Matrix multiplication

(*){T<:Integer}(A::BitArray{T}, B::BitArray{T}) = bitunpack(A) * bitunpack(B)

#disambiguations
(*){T<:Integer}(A::BitMatrix{T}, B::BitVector{T}) = bitunpack(A) * bitunpack(B)
(*){T<:Integer}(A::BitVector{T}, B::BitMatrix{T}) = bitunpack(A) * bitunpack(B)
(*){T<:Integer}(A::BitMatrix{T}, B::BitMatrix{T}) = bitunpack(A) * bitunpack(B)
(*){T<:Integer}(A::BitMatrix{T}, B::AbstractVector) = (*)(bitunpack(A), B)
(*){T<:Integer}(A::BitVector{T}, B::AbstractMatrix) = (*)(bitunpack(A), B)
(*){T<:Integer}(A::BitMatrix{T}, B::AbstractMatrix) = (*)(bitunpack(A), B)
(*){T<:Integer}(A::AbstractVector, B::BitMatrix{T}) = (*)(A, bitunpack(B))
(*){T<:Integer}(A::AbstractMatrix, B::BitVector{T}) = (*)(A, bitunpack(B))
(*){T<:Integer}(A::AbstractMatrix, B::BitMatrix{T}) = (*)(A, bitunpack(B))
#end disambiguations

for f in (:/, :\, :*)
    @eval begin
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
