function dot(x::BitVector, y::BitVector)
    # simplest way to mimic Array dot behavior
    s = 0
    for i = 1 : length(x.chunks)
        s += count_ones(x.chunks[i] & y.chunks[i])
    end
    return s
end

## slower than the unpacked version, which is MUCH slower
#  than blas'd (this one saves storage though, keeping it commented
#  just in case)
#function aTb(A::BitMatrix, B::BitMatrix)
    #(mA, nA) = size(A)
    #(mB, nB) = size(B)
    #C = falses(nA, nB)
    #if mA != mB; error("*: argument shapes do not match"); end
    #if mA == 0; return C; end
    #col_ch = num_bit_chunks(mA)
    ## TODO: avoid using aux chunks and copy (?)
    #aux_chunksA = zeros(Uint64, col_ch)
    #aux_chunksB = [zeros(Uint64, col_ch) for j=1:nB]
    #for j = 1:nB
        #copy_chunks(aux_chunksB[j], 1, B.chunks, (j-1)*mA+1, mA)
    #end
    #for i = 1:nA
        #copy_chunks(aux_chunksA, 1, A.chunks, (i-1)*mA+1, mA)
        #for j = 1:nB
            #for k = 1:col_ch
                ## TODO: improve
                #C[i, j] += count_ones(aux_chunksA[k] & aux_chunksB[j][k])
            #end
        #end
    #end
    #return C
#end

#aCb{T, S}(A::BitMatrix{T}, B::BitMatrix{S}) = aTb(A, B)

function triu(B::BitMatrix, k::Int)
    m,n = size(B)
    A = falses(m,n)
    for i = max(k+1,1):n
        j = clamp((i - 1) * m + 1, 1, i * m)
        copy_chunks(A.chunks, j, B.chunks, j, min(i-k, m))
    end
    return A
end
triu(B::BitMatrix, k::Integer) = triu(B, int(k))

function tril(B::BitMatrix, k::Int)
    m,n = size(B)
    A = falses(m, n)
    for i = 1:min(n, m+k)
        j = clamp((i - 1) * m + i - k, 1, i * m)
        copy_chunks(A.chunks, j, B.chunks, j, max(m-i+k+1, 0))
    end
    return A
end
tril(B::BitMatrix, k::Integer) = tril(B, int(k))

# TODO: improve this!
(*)(A::BitArray, B::BitArray) = bitpack(bitunpack(A) * bitunpack(B))
(*)(A::BitArray, B::Array{Bool}) = bitpack(bitunpack(A) * B)
(*)(A::Array{Bool}, B::BitArray) = bitpack(A * bitunpack(B))
(*)(A::BitArray, B::AbstractArray) = bitunpack(A) * B
(*)(A::AbstractArray, B::BitArray) = A * bitunpack(B)

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

function diagm(v::Union(BitVector,BitMatrix))
    if isa(v, BitMatrix)
        if (size(v,1) != 1 && size(v,2) != 1)
            error("Input should be nx1 or 1xn")
        end
    end

    n = length(v)
    a = falses(n, n)
    for i=1:n
        a[i,i] = v[i]
    end

    return a
end

## norm and rank

svd(A::BitMatrix) = svd(float(A))

qr(A::BitMatrix) = qr(float(A))

## kron

function kron(a::BitVector, b::BitVector)
    m = length(a)
    n = length(b)
    R = BitArray(m, n)
    zS = zero(S)
    for j = 1:n
        if b[j] != zS
            copy_chunks(R.chunks, (j-1)*m+1, a.chunks, 1, m)
        end
    end
    return R
end

function kron(a::BitMatrix, b::BitMatrix)
    mA,nA = size(a)
    mB,nB = size(b)
    R = falses(mA*mB, nA*nB)

    for i = 1:mA
        ri = (1:mB)+(i-1)*mB
        for j = 1:nA
            if a[i,j]
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
    if m != n; return false; end
    return nnz(A - A.') == 0
end

ishermitian(A::BitMatrix) = issym(A)

function nonzero_chunks(chunks::Vector{Uint64}, pos0::Int, pos1::Int)

    k0, l0 = get_chunks_id(pos0)
    k1, l1 = get_chunks_id(pos1)

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
        if nonzero_chunks(A.chunks, stride+j+1, stride+m)
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
        if nonzero_chunks(A.chunks, stride+1, stride+min(j-1,m))
            return false
        end
    end
    return true
end

function findmax(a::BitArray)
    if length(a) == 0
        error("findmax: array is empty")
    end
    m = false
    mi = 1
    ti = 1
    for i=1:length(a.chunks)
        k = trailing_zeros(a.chunks[i])
        ti += k
        if k != 64
            m = true
            mi = ti
            break
        end
    end
    return (m, mi)
end

function findmin(a::BitArray)
    if length(a) == 0
        error("findmin: array is empty")
    end
    m = true
    mi = 1
    ti = 1
    for i=1:length(a.chunks) - 1
        k = trailing_ones(a.chunks[i])
        ti += k
        if k != 64
            return (false, ti)
        end
    end
    l = (@_mod64 (length(a)-1)) + 1
    msk = @_mskr l
    k = trailing_ones(a.chunks[end] & msk)
    ti += k
    if k != l
        m = false
        mi = ti
    end
    return (m, mi)
end
