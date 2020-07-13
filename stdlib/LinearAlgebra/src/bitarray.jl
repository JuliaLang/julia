# This file is a part of Julia. License is MIT: https://julialang.org/license

function dot(x::BitVector, y::BitVector)
    # simplest way to mimic Array dot behavior
    length(x) == length(y) || throw(DimensionMismatch())
    s = 0
    xc = x.chunks
    yc = y.chunks
    @inbounds for i = 1:length(xc)
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

#aCb(A::BitMatrix{T}, B::BitMatrix{S}) where {T,S} = aTb(A, B)

function triu(B::BitMatrix, k::Integer=0)
    m,n = size(B)
    if !(-m + 1 <= k <= n + 1)
        throw(ArgumentError(string("the requested diagonal, $k, must be at least",
            "$(-m + 1) and at most $(n + 1) in an $m-by-$n matrix")))
    end
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
    if !(-m - 1 <= k <= n - 1)
        throw(ArgumentError(string("the requested diagonal, $k, must be at least ",
            "$(-m - 1) and at most $(n - 1) in an $m-by-$n matrix")))
    end
    A = falses(m, n)
    Ac = A.chunks
    Bc = B.chunks
    for i = 1:min(n, m+k)
        j = clamp((i - 1) * m + i - k, 1, i * m)
        Base.copy_chunks!(Ac, j, Bc, j, max(m-i+k+1, 0))
    end
    A
end

## diag

function diag(B::BitMatrix)
    n = minimum(size(B))
    v = similar(B, n)
    for i = 1:n
        v[i] = B[i,i]
    end
    v
end

## norm and rank

svd(A::BitMatrix) = svd(float(A))
qr(A::BitMatrix) = qr(float(A))

## kron

@inline function kron!(R::BitVector, a::BitVector, b::BitVector)
    m = length(a)
    n = length(b)
    @boundscheck length(R) == n*m || throw(DimensionMismatch())
    Rc = R.chunks
    bc = b.chunks
    for j = 1:m
        a[j] && Base.copy_chunks!(Rc, (j-1)*n+1, bc, 1, n)
    end
    return R
end

function kron(a::BitVector, b::BitVector)
    m = length(a)
    n = length(b)
    R = falses(n * m)
    return @inbounds kron!(R, a, b)
end

function kron!(R::BitMatrix, a::BitMatrix, b::BitMatrix)
    mA,nA = size(a)
    mB,nB = size(b)
    @boundscheck size(R) == (mA*mB, nA*nB) || throw(DimensionMismatch())

    for i = 1:mA
        ri = (1:mB) .+ ((i-1)*mB)
        for j = 1:nA
            if a[i,j]
                rj = (1:nB) .+ ((j-1)*nB)
                R[ri,rj] = b
            end
        end
    end
    return R
end

function kron(a::BitMatrix, b::BitMatrix)
    mA,nA = size(a)
    mB,nB = size(b)
    R = falses(mA*mB, nA*nB)
    return @inbounds kron!(R, a, b)
end

## Structure query functions

issymmetric(A::BitMatrix) = size(A, 1)==size(A, 2) && count(!iszero, A - copy(A'))==0
ishermitian(A::BitMatrix) = issymmetric(A)

function nonzero_chunks(chunks::Vector{UInt64}, pos0::Int, pos1::Int)
    k0, l0 = Base.get_chunks_id(pos0)
    k1, l1 = Base.get_chunks_id(pos1)

    delta_k = k1 - k0

    z = UInt64(0)
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
        stride = (j-1) * m
        nonzero_chunks(A.chunks, stride+j+1, stride+m) && return false
    end
    return true
end

function istril(A::BitMatrix)
    m, n = size(A)
    (m == 0 || n == 0) && return true
    for j = 2:n
        stride = (j-1) * m
        nonzero_chunks(A.chunks, stride+1, stride+min(j-1,m)) && return false
    end
    return true
end

# fast 8x8 bit transpose from Henry S. Warrens's "Hacker's Delight"
# http://www.hackersdelight.org/hdcodetxt/transpose8.c.txt
function transpose8x8(x::UInt64)
    y = x
    t = xor(y, y >>> 7) & 0x00aa00aa00aa00aa
    y = xor(y, t, t << 7)
    t = xor(y, y >>> 14) & 0x0000cccc0000cccc
    y = xor(y, t, t << 14)
    t = xor(y, y >>> 28) & 0x00000000f0f0f0f0
    return xor(y, t, t << 28)
end

function form_8x8_chunk(Bc::Vector{UInt64}, i1::Int, i2::Int, m::Int, cgap::Int, cinc::Int, nc::Int, msk8::UInt64)
    x = UInt64(0)

    k, l = Base.get_chunks_id(i1 + (i2 - 1) * m)
    r = 0
    for j = 1:8
        k > nc && break
        x |= ((Bc[k] >>> l) & msk8) << r
        if l + 8 >= 64 && nc > k
            r0 = 8 - Base._mod64(l + 8)
            x |= (Bc[k + 1] & (msk8 >>> r0)) << (r + r0)
        end
        k += cgap + (l + cinc >= 64 ? 1 : 0)
        l = Base._mod64(l + cinc)
        r += 8
    end
    return x
end

# note: assumes B is filled with 0's
function put_8x8_chunk(Bc::Vector{UInt64}, i1::Int, i2::Int, x::UInt64, m::Int, cgap::Int, cinc::Int, nc::Int, msk8::UInt64)
    k, l = Base.get_chunks_id(i1 + (i2 - 1) * m)
    r = 0
    for j = 1:8
        k > nc && break
        Bc[k] |= ((x >>> r) & msk8) << l
        if l + 8 >= 64 && nc > k
            r0 = 8 - Base._mod64(l + 8)
            Bc[k + 1] |= ((x >>> (r + r0)) & (msk8 >>> r0))
        end
        k += cgap + (l + cinc >= 64 ? 1 : 0)
        l = Base._mod64(l + cinc)
        r += 8
    end
    return
end

adjoint(B::Union{BitVector,BitMatrix}) = Adjoint(B)
transpose(B::Union{BitVector,BitMatrix}) = Transpose(B)
Base.copy(B::Adjoint{Bool,BitMatrix}) = transpose!(falses(size(B)), B.parent)
Base.copy(B::Transpose{Bool,BitMatrix}) = transpose!(falses(size(B)), B.parent)
function transpose!(C::BitMatrix, B::BitMatrix)
    @boundscheck size(C) == reverse(size(B)) || throw(DimensionMismatch())
    l1, l2 = size(B)

    cgap1, cinc1 = Base._div64(l1), Base._mod64(l1)
    cgap2, cinc2 = Base._div64(l2), Base._mod64(l2)

    Bc = B.chunks
    Cc = C.chunks

    nc = length(Bc)

    for i = 1:8:l1
        msk8_1 = UInt64(0xff)
        if (l1 < i + 7)
            msk8_1 >>>= i + 7 - l1
        end

        for j = 1:8:l2
            x = form_8x8_chunk(Bc, i, j, l1, cgap1, cinc1, nc, msk8_1)
            x = transpose8x8(x)

            msk8_2 = UInt64(0xff)
            if (l2 < j + 7)
                msk8_2 >>>= j + 7 - l2
            end

            put_8x8_chunk(Cc, j, i, x, l2, cgap2, cinc2, nc, msk8_2)
        end
    end
    return C
end
