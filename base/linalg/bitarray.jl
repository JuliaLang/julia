function dot(x::BitVector, y::BitVector)
    # simplest way to mimic Array dot behavior
    length(x) == length(y) || throw(DimensionMismatch())
    s = 0
    xc = x.chunks
    yc = y.chunks
    @inbounds for i = 1 : length(xc)
        s += count_ones(xc[i] & yc[i])
    end
    s
end

## slower than the unpacked version, which is MUCH slower
#  than blas'd (this one saves storage though, keeping it commented
#  just in case)
#function aTb(A::BitMatrix, B::BitMatrix)
    #(mA, nA) = size(A)
    #(mB, nB) = size(B)
    #C = falses(nA, nB)
    #if mA != mB; throw(DimensionMismatch()) end
    #if mA == 0; return C; end
    #col_ch = num_bit_chunks(mA)
    ## TODO: avoid using aux chunks and copy (?)
    #aux_chunksA = zeros(UInt64, col_ch)
    #aux_chunksB = [zeros(UInt64, col_ch) for j=1:nB]
    #for j = 1:nB
        #Base.copy_chunks!(aux_chunksB[j], 1, B.chunks, (j-1)*mA+1, mA)
    #end
    #for i = 1:nA
        #Base.copy_chunks!(aux_chunksA, 1, A.chunks, (i-1)*mA+1, mA)
        #for j = 1:nB
            #for k = 1:col_ch
                ## TODO: improve
                #C[i, j] += count_ones(aux_chunksA[k] & aux_chunksB[j][k])
            #end
        #end
    #end
    #C
#end

#aCb{T, S}(A::BitMatrix{T}, B::BitMatrix{S}) = aTb(A, B)

function triu(B::BitMatrix, k::Integer=0)
    m,n = size(B)
    A = falses(m,n)
    Ac = A.chunks
    Bc = B.chunks
    for i = max(k+1,1):n
        j = clamp((i - 1) * m + 1, 1, i * m)
        Base.copy_chunks!(Ac, j, Bc, j, min(i-k, m))
    end
    A
end

function tril(B::BitMatrix, k::Integer=0)
    m,n = size(B)
    A = falses(m, n)
    Ac = A.chunks
    Bc = B.chunks
    for i = 1:min(n, m+k)
        j = clamp((i - 1) * m + i - k, 1, i * m)
        Base.copy_chunks!(Ac, j, Bc, j, max(m-i+k+1, 0))
    end
    A
end

## diff and gradient

# TODO: this could be improved (is it worth it?)
gradient(F::BitVector) = gradient(bitunpack(F))
gradient(F::BitVector, h::Real) = gradient(bitunpack(F), h)
gradient(F::Vector, h::BitVector) = gradient(F, bitunpack(h))
gradient(F::BitVector, h::Vector) = gradient(bitunpack(F), h)
gradient(F::BitVector, h::BitVector) = gradient(bitunpack(F), bitunpack(h))

## diag and related

function diag(B::BitMatrix)
    n = minimum(size(B))
    v = similar(B, n)
    for i = 1:n
        v[i] = B[i,i]
    end
    v
end

function diagm(v::Union(BitVector,BitMatrix))
    isa(v, BitMatrix) && size(v,1)==1 || size(v,2)==1 || throw(DimensionMismatch())
    n = length(v)
    a = falses(n, n)
    for i=1:n
        a[i,i] = v[i]
    end
    a
end

## norm and rank

svd(A::BitMatrix) = svd(float(A))
qr(A::BitMatrix) = qr(float(A))

## kron

function kron(a::BitVector, b::BitVector)
    m = length(a)
    n = length(b)
    R = falses(n * m)
    Rc = R.chunks
    bc = b.chunks
    for j = 1:m
        a[j] && Base.copy_chunks!(Rc, (j-1)*n+1, bc, 1, n)
    end
    R
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
    R
end

## Structure query functions

issym(A::BitMatrix) = size(A, 1)==size(A, 2) && countnz(A - A.')==0
ishermitian(A::BitMatrix) = issym(A)

function nonzero_chunks(chunks::Vector{UInt64}, pos0::Int, pos1::Int)
    k0, l0 = Base.get_chunks_id(pos0)
    k1, l1 = Base.get_chunks_id(pos1)

    delta_k = k1 - k0

    z = uint64(0)
    u = ~z
    if delta_k == 0
        msk_0 = (u << l0) & ~(u << l1 << 1)
    else
        msk_0 = (u << l0)
        msk_1 = ~(u << l1 << 1)
    end

    @inbounds begin
        (chunks[k0] & msk_0) == z || return true
        delta_k == 0 && return false
        for i = k0 + 1 : k1 - 1
            chunks[i] == z || return true
        end
        (chunks[k1] & msk_1)==z || return true
    end
    return false
end

function istriu(A::BitMatrix)
    m, n = size(A)
    for j = 1:min(n,m-1)
        stride = (j-1)*m
        nonzero_chunks(A.chunks, stride+j+1, stride+m) && return false
    end
    return true
end

function istril(A::BitMatrix)
    m, n = size(A)
    (m == 0 || n == 0) && return true
    for j = 2:n
        stride = (j-1)*m
        nonzero_chunks(A.chunks, stride+1, stride+min(j-1,m)) && return false
    end
    return true
end

function findmax(a::BitArray)
    length(a)==0 && error("findmax: array is empty")
    m, mi = false, 1
    ti = 1
    ac = a.chunks
    for i=1:length(ac)
        @inbounds k = trailing_zeros(ac[i])
        ti += k
        k==64 || return (true, ti)
    end
    return m, mi
end

function findmin(a::BitArray)
    length(a)==0 && error("findmin: array is empty")
    m, mi = true, 1
    ti = 1
    ac = a.chunks
    for i = 1:length(ac)-1
        @inbounds k = trailing_ones(ac[i])
        ti += k
        k==64 || return (false, ti)
    end
    l = (Base.@_mod64 (length(a)-1)) + 1
    msk = Base.@_mskr l
    @inbounds k = trailing_ones(ac[end] & msk)
    ti += k
    k==l || return (false, ri)
    return m, mi
end
