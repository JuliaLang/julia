## sparse matrix multiplication

function (*){TvA,TiA,TvB,TiB}(A::SparseMatrixCSC{TvA,TiA}, B::SparseMatrixCSC{TvB,TiB})
    Tv = promote_type(TvA, TvB)
    Ti = promote_type(TiA, TiB)
    A  = convert(SparseMatrixCSC{Tv,Ti}, A)
    B  = convert(SparseMatrixCSC{Tv,Ti}, B)
    return A * B
end

(*){TvA,TiA}(A::SparseMatrixCSC{TvA,TiA}, X::BitArray{1}) = invoke(*, (SparseMatrixCSC, AbstractVector), A, X)
# In matrix-vector multiplication, the correct orientation of the vector is assumed.
function A_mul_B!(α::Number, A::SparseMatrixCSC, x::AbstractVector, β::Number, y::AbstractVector)
    A.n == length(x) || throw(DimensionMismatch(""))
    A.m == length(y) || throw(DimensionMismatch(""))
    for i = 1:A.m; y[i] *= β; end
    nzv = A.nzval
    rv = A.rowval
    for col = 1 : A.n
        αx = α*x[col]
        @inbounds for k = A.colptr[col] : (A.colptr[col+1]-1)
            y[rv[k]] += nzv[k]*αx
        end
    end
    return y
end
(*){TA,S,Tx}(A::SparseMatrixCSC{TA,S}, x::AbstractVector{Tx}) = A_mul_B!(1, A, x, 0, zeros(promote_type(TA,Tx), A.m))

(*)(X::BitArray{1}, A::SparseMatrixCSC) = invoke(*, (AbstractVector, SparseMatrixCSC), X, A)
# In vector-matrix multiplication, the correct orientation of the vector is assumed.
# XXX: this is wrong (i.e. not what Arrays would do)!!
function (*){T1,T2}(X::AbstractVector{T1}, A::SparseMatrixCSC{T2})
    if A.m != length(X); error("mismatched dimensions"); end
    Y = zeros(promote_type(T1,T2), A.n)
    nzv = A.nzval
    rv = A.rowval
    for col = 1 : A.n
        for k = A.colptr[col] : (A.colptr[col+1]-1)
            Y[col] += X[rv[k]] * nzv[k]
        end
    end
    return Y
end

(*){TvA,TiA}(A::SparseMatrixCSC{TvA,TiA}, X::BitArray{2}) = invoke(*, (SparseMatrixCSC, AbstractMatrix), A, X)
function (*){TvA,TiA,TX}(A::SparseMatrixCSC{TvA,TiA}, X::AbstractMatrix{TX})
    mX, nX = size(X)
    if A.n != mX; error("mismatched dimensions"); end
    Y = zeros(promote_type(TvA,TX), A.m, nX)
    nzv = A.nzval
    rv = A.rowval
    colptr = A.colptr
    for multivec_col = 1:nX
        for col = 1 : A.n
            Xc = X[col, multivec_col]
            @inbounds for k = colptr[col] : (colptr[col+1]-1)
                Y[rv[k], multivec_col] += nzv[k] * Xc
            end
        end
    end
    return Y
end

(*){TvA,TiA}(X::BitArray{2}, A::SparseMatrixCSC{TvA,TiA}) = invoke(*, (AbstractMatrix, SparseMatrixCSC), X, A)
function (*){TX,TvA,TiA}(X::AbstractMatrix{TX}, A::SparseMatrixCSC{TvA,TiA})
    mX, nX = size(X)
    if nX != A.m; error("mismatched dimensions"); end
    Y = zeros(promote_type(TX,TvA), mX, A.n)
    for multivec_row = 1:mX
        for col = 1 : A.n
            for k = A.colptr[col] : (A.colptr[col+1]-1)
                Y[multivec_row, col] += X[multivec_row, A.rowval[k]] * A.nzval[k]
            end
        end
    end
    return Y
end

# Sparse matrix multiplication as described in [Gustavson, 1978]:
# http://www.cse.iitb.ac.in/graphics/~anand/website/include/papers/matrix/fast_matrix_mul.pdf

function (*){Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti})
    mA, nA = size(A)
    mB, nB = size(B)
    if nA != mB; error("mismatched dimensions"); end

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    colptrB = B.colptr; rowvalB = B.rowval; nzvalB = B.nzval
    # TODO: Need better estimation of result space
    nnzC = min(mA*nB, length(nzvalA) + length(nzvalB))
    colptrC = Array(Ti, nB+1)
    rowvalC = Array(Ti, nnzC)
    nzvalC = Array(Tv, nnzC)

    @inbounds begin
        ip = 1
        xb = zeros(Ti, mA)
        x  = zeros(Tv, mA)
        for i in 1:nB
            if ip + mA - 1 > nnzC
                resize!(rowvalC, nnzC + max(nnzC,mA))
                resize!(nzvalC, nnzC + max(nnzC,mA))
                nnzC = length(nzvalC)
            end
            colptrC[i] = ip
            for jp in colptrB[i]:(colptrB[i+1] - 1)
                nzB = nzvalB[jp]
                j = rowvalB[jp]
                for kp in colptrA[j]:(colptrA[j+1] - 1)
                    nzC = nzvalA[kp] * nzB
                    k = rowvalA[kp]
                    if xb[k] != i
                        rowvalC[ip] = k
                        ip += 1
                        xb[k] = i
                        x[k] = nzC
                    else
                        x[k] += nzC
                    end
                end
            end
            for vp in colptrC[i]:(ip - 1)
                nzvalC[vp] = x[rowvalC[vp]]
            end
        end
        colptrC[nB+1] = ip
    end

    splice!(rowvalC, colptrC[end]:length(rowvalC))
    splice!(nzvalC, colptrC[end]:length(nzvalC))

    # The Gustavson algorithm does not guarantee the product to have sorted row indices.
    Cunsorted = SparseMatrixCSC(mA, nB, colptrC, rowvalC, nzvalC)
    Ct = Cunsorted.'
    Ctt = Base.SparseMatrix.transpose!(Ct, SparseMatrixCSC(mA, nB, colptrC, rowvalC, nzvalC))

    return Ctt
end

## triu, tril

function triu{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, k::Integer)
    m,n = size(S)
    colptr = Array(Ti, n+1)
    nnz = 0
    for col = 1 : min(max(k+1,1), n+1)
        colptr[col] = 1
    end
    for col = max(k+1,1) : n
        for c1 = S.colptr[col] : S.colptr[col+1]-1
            if S.rowval[c1] > col - k
                break;
            end
            nnz += 1
        end
        colptr[col+1] = nnz+1
    end
    rowval = Array(Ti, nnz)
    nzval = Array(Tv, nnz)
    A = SparseMatrixCSC{Tv,Ti}(m, n, colptr, rowval, nzval)
    for col = max(k+1,1) : n
        c1 = S.colptr[col]
        for c2 = A.colptr[col] : A.colptr[col+1]-1
            A.rowval[c2] = S.rowval[c1]
            A.nzval[c2] = S.nzval[c1]
            c1 += 1
        end
    end
    return A
end
triu{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, k::Integer) = triu(S, int(k))

function tril{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, k::Integer)
    m,n = size(S)
    colptr = Array(Ti, n+1)
    nnz = 0
    colptr[1] = 1
    for col = 1 : min(n, m+k)
        l1 = S.colptr[col+1]-1
        for c1 = 0 : (l1 - S.colptr[col])
            if S.rowval[l1 - c1] < col - k
                break;
            end
            nnz += 1
        end
        colptr[col+1] = nnz+1
    end
    for col = max(min(n, m+k)+2,1) : n+1
        colptr[col] = nnz+1
    end
    rowval = Array(Ti, nnz)
    nzval = Array(Tv, nnz)
    A = SparseMatrixCSC{Tv,Ti}(m, n, colptr, rowval, nzval)
    for col = 1 : min(n, m+k)
        c1 = S.colptr[col+1]-1
        l2 = A.colptr[col+1]-1
        for c2 = 0 : l2 - A.colptr[col]
            A.rowval[l2 - c2] = S.rowval[c1]
            A.nzval[l2 - c2] = S.nzval[c1]
            c1 -= 1
        end
    end
    return A
end
tril{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, k::Integer) = tril(S, int(k))

## diff

function sparse_diff1{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti})
    m,n = size(S)
    if m <= 1
        return SparseMatrixCSC{Tv,Ti}(0, n, ones(n+1), Ti[], Tv[])
    end
    colptr = Array(Ti, n+1)
    numnz = 2 * nnz(S) # upper bound; will shrink later
    rowval = Array(Ti, numnz)
    nzval = Array(Tv, numnz)
    numnz = 0
    colptr[1] = 1
    for col = 1 : n
        last_row = 0
        last_val = 0
        for k = S.colptr[col] : S.colptr[col+1]-1
            row = S.rowval[k]
            val = S.nzval[k]
            if row > 1
                if row == last_row + 1
                    nzval[numnz] += val
                    if nzval[numnz] == zero(Tv)
                        numnz -= 1
                    end
                else
                    numnz += 1
                    rowval[numnz] = row - 1
                    nzval[numnz] = val
                end
            end
            if row < m
                numnz += 1
                rowval[numnz] = row
                nzval[numnz] = -val
            end
            last_row = row
            last_val = val
        end
        colptr[col+1] = numnz+1
    end
    splice!(rowval, numnz+1:length(rowval))
    splice!(nzval, numnz+1:length(nzval))
    return SparseMatrixCSC{Tv,Ti}(m-1, n, colptr, rowval, nzval)
end

function sparse_diff2{Tv,Ti}(a::SparseMatrixCSC{Tv,Ti})

    m,n = size(a)
    colptr = Array(Ti, max(n,1))
    numnz = 2 * nnz(a) # upper bound; will shrink later
    rowval = Array(Ti, numnz)
    nzval = Array(Tv, numnz)

    z = zero(Tv)

    colptr_a = a.colptr
    rowval_a = a.rowval
    nzval_a = a.nzval

    ptrS = 1
    colptr[1] = 1

    if n == 0
        return SparseMatrixCSC{Tv,Ti}(m, n, colptr, rowval, nzval)
    end

    startA = colptr_a[1]
    stopA = colptr_a[2]

    rA = startA : stopA - 1
    rowvalA = rowval_a[rA]
    nzvalA = nzval_a[rA]
    lA = stopA - startA

    for col = 1:n-1
        startB, stopB = startA, stopA
        startA = colptr_a[col+1]
        stopA = colptr_a[col+2]

        rowvalB = rowvalA
        nzvalB = nzvalA
        lB = lA

        rA = startA : stopA - 1
        rowvalA = rowval_a[rA]
        nzvalA = nzval_a[rA]
        lA = stopA - startA

        ptrB = 1
        ptrA = 1

        while ptrA <= lA && ptrB <= lB
            rowA = rowvalA[ptrA]
            rowB = rowvalB[ptrB]
            if rowA < rowB
                rowval[ptrS] = rowA
                nzval[ptrS] = nzvalA[ptrA]
                ptrS += 1
                ptrA += 1
            elseif rowB < rowA
                rowval[ptrS] = rowB
                nzval[ptrS] = -nzvalB[ptrB]
                ptrS += 1
                ptrB += 1
            else
                res = nzvalA[ptrA] - nzvalB[ptrB]
                if res != z
                    rowval[ptrS] = rowA
                    nzval[ptrS] = res
                    ptrS += 1
                end
                ptrA += 1
                ptrB += 1
            end
        end

        while ptrA <= lA
            rowval[ptrS] = rowvalA[ptrA]
            nzval[ptrS] = nzvalA[ptrA]
            ptrS += 1
            ptrA += 1
        end

        while ptrB <= lB
            rowval[ptrS] = rowvalB[ptrB]
            nzval[ptrS] = -nzvalB[ptrB]
            ptrS += 1
            ptrB += 1
        end

        colptr[col+1] = ptrS
    end
    splice!(rowval, ptrS:length(rowval))
    splice!(nzval, ptrS:length(nzval))
    return SparseMatrixCSC{Tv,Ti}(m, n-1, colptr, rowval, nzval)
end

function diff(a::SparseMatrixCSC, dim::Integer)
    if dim == 1
        sparse_diff1(a)
    else
        sparse_diff2(a)
    end
end


## norm and rank

# TODO

# kron

function kron{Tv,Ti}(a::SparseMatrixCSC{Tv,Ti}, b::SparseMatrixCSC{Tv,Ti})
    numnzA = nnz(a)
    numnzB = nnz(b)

    numnz = numnzA * numnzB

    mA,nA = size(a)
    mB,nB = size(b)

    m,n = mA*mB, nA*nB

    colptr = Array(Ti, n+1)
    rowval = Array(Ti, numnz)
    nzval = Array(Tv, numnz)

    colptr[1] = 1

    colptrA = a.colptr
    colptrB = b.colptr
    rowvalA = a.rowval
    rowvalB = b.rowval
    nzvalA = a.nzval
    nzvalB = b.nzval

    col = 1

    for j = 1:nA
        startA = colptrA[j]
        stopA = colptrA[j+1]-1
        lA = stopA - startA + 1

        for i = 1:nB
            startB = colptrB[i]
            stopB = colptrB[i+1]-1
            lB = stopB - startB + 1

            r = (1:lB) + (colptr[col]-1)
            rB = startB:stopB

            colptr[col+1] = colptr[col] + lA * lB
            col += 1

            for ptrA = startA : stopA
                rowval[r] = (rowvalA[ptrA]-1)*mB + rowvalB[rB]
                nzval[r] = nzvalA[ptrA] * nzvalB[rB]
                r += lB
            end
        end
    end

    return SparseMatrixCSC{Tv,Ti}(m, n, colptr, rowval, nzval)
end

## det, inv, cond

inv(A::SparseMatrixCSC) = error("The inverse of a sparse matrix can often be dense and can cause the computer to run out of memory. If you are sure you have enough memory, please convert your matrix to a dense matrix.")

# TODO

## scale methods

# multiply by diagonal matrix as vector
function scale!{Tv,Ti}(C::SparseMatrixCSC{Tv,Ti}, A::SparseMatrixCSC, b::Vector)
    m, n = size(A)
    if n != length(b) || size(A) != size(C)
        error("argument dimensions do not match")
    end
    numnz = nnz(A)
    C.colptr = convert(Array{Ti}, A.colptr)
    C.rowval = convert(Array{Ti}, A.rowval)
    C.nzval = Array(Tv, numnz)
    for col = 1:n, p = A.colptr[col]:(A.colptr[col+1]-1)
        C.nzval[p] = A.nzval[p] * b[col]
    end
    return C
end

function scale!{Tv,Ti}(C::SparseMatrixCSC{Tv,Ti}, b::Vector, A::SparseMatrixCSC)
    m, n = size(A)
    if n != length(b) || size(A) != size(C)
        error("argument dimensions do not match")
    end
    numnz = nnz(A)
    C.colptr = convert(Array{Ti}, A.colptr)
    C.rowval = convert(Array{Ti}, A.rowval)
    C.nzval = Array(Tv, numnz)
    for col = 1:n, p = A.colptr[col]:(A.colptr[col+1]-1)
        C.nzval[p] = A.nzval[p] * b[A.rowval[p]]
    end
    return C
end

scale{Tv,Ti,T}(A::SparseMatrixCSC{Tv,Ti}, b::Vector{T}) =
    scale!(SparseMatrixCSC(size(A,1),size(A,2),Ti[],Ti[],promote_type(Tv,T)[]), A, b)

scale{T,Tv,Ti}(b::Vector{T}, A::SparseMatrixCSC{Tv,Ti}) =
    scale!(SparseMatrixCSC(size(A,1),size(A,2),Ti[],Ti[],promote_type(Tv,T)[]), b, A)
