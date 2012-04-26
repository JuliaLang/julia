## linalg_sparse.jl: Basic Linear Algebra functions for sparse representations ##

#TODO? probably there's no use at all for these
# dot(x::SparseAccumulator, y::SparseAccumulator)
# cross(a::SparseAccumulator, b::SparseAccumulator) =


## Matrix multiplication

# In matrix-vector multiplication, the correct orientation of the vector is assumed.
function (*){T1,T2}(A::SparseMatrixCSC{T1}, X::Vector{T2})
    Y = zeros(promote_type(T1,T2), A.m)
    for col = 1 : A.n, k = A.colptr[col] : (A.colptr[col+1]-1)
        Y[A.rowval[k]] += A.nzval[k] * X[col]
    end
    return Y
end

# In vector-matrix multiplication, the correct orientation of the vector is assumed.
# XXX: this is wrong (i.e. not what Arrays would do)!!
function (*){T1,T2}(X::Vector{T1}, A::SparseMatrixCSC{T2})
    Y = zeros(promote_type(T1,T2), A.n)
    for col = 1 : A.n, k = A.colptr[col] : (A.colptr[col+1]-1)
        Y[col] += X[A.rowval[k]] * A.nzval[k]
    end
    return Y
end

function (*){T1,T2}(A::SparseMatrixCSC{T1}, X::Matrix{T2})
    mX, nX = size(X)
    if A.n != mX; error("error in *: mismatched dimensions"); end
    Y = zeros(promote_type(T1,T2), A.m, nX)
    for multivec_col = 1:nX
        for col = 1 : A.n
            for k = A.colptr[col] : (A.colptr[col+1]-1)
                Y[A.rowval[k], multivec_col] += A.nzval[k] * X[col, multivec_col]
            end
        end
    end
    return Y
end

function (*){T1,T2}(X::Matrix{T1}, A::SparseMatrixCSC{T2})
    mX, nX = size(X)
    if nX != A.m; error("error in *: mismatched dimensions"); end
    Y = zeros(promote_type(T1,T2), mX, A.n)
    for multivec_row = 1:mX
        for col = 1 : A.n
            for k = A.colptr[col] : (A.colptr[col+1]-1)
                Y[multivec_row, col] += X[multivec_row, A.rowval[k]] * A.nzval[k]
            end
        end
    end
    return Y
end

# sparse matmul (sparse * sparse)
function (*){TvX,TiX,TvY,TiY}(X::SparseMatrixCSC{TvX,TiX}, Y::SparseMatrixCSC{TvY,TiY})
    mX, nX = size(X)
    mY, nY = size(Y)
    if nX != mY; error("error in *: mismatched dimensions"); end
    Tv = promote_type(TvX, TvY)
    Ti = promote_type(TiX, TiY)

    colptr = Array(Ti, nY+1)
    colptr[1] = 1
    nnz_res = nnz(X) + nnz(Y)
    rowval = Array(Ti, nnz_res)  # TODO: Need better estimation of result space
    nzval = Array(Tv, nnz_res)

    colptrY = Y.colptr
    rowvalY = Y.rowval
    nzvalY = Y.nzval

    spa = SparseAccumulator(Tv, Ti, mX);
    for y_col = 1:nY
        for y_elt = colptrY[y_col] : (colptrY[y_col+1]-1)
            x_col = rowvalY[y_elt]
            _jl_spa_axpy(spa, nzvalY[y_elt], X, x_col)
        end
        (rowval, nzval) = _jl_spa_store_reset(spa, y_col, colptr, rowval, nzval)
    end

    SparseMatrixCSC(mX, nY, colptr, rowval, nzval)
end

## triu, tril

function triu{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, k::Int)
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

function tril{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, k::Int)
    m,n = size(S)
    colptr = Array(Ti, n+1)
    nnz = 0
    colptr[1] = 1
    for col = 1 : min(n, m+k)
        l1 = S.colptr[col+1]-1
        for c1 = 0 : l1 - S.colptr[col]
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

function _jl_sparse_diff1{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti})
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
    del(rowval, numnz+1:length(rowval))
    del(nzval, numnz+1:length(nzval))
    return SparseMatrixCSC{Tv,Ti}(m-1, n, colptr, rowval, nzval)
end

function _jl_sparse_diff2{Tv,Ti}(a::SparseMatrixCSC{Tv,Ti})

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
    del(rowval, ptrS:length(rowval))
    del(nzval, ptrS:length(nzval))
    return SparseMatrixCSC{Tv,Ti}(m, n-1, colptr, rowval, nzval)
end

function diff(a::SparseMatrixCSC, dim::Integer)
    if dim == 1
        _jl_sparse_diff1(a)
    else
        _jl_sparse_diff2(a)
    end
end

## diag and related

diag(A::SparseMatrixCSC) = [ A[i,i] | i=1:min(size(A)) ]

function diagm{Tv,Ti}(v::SparseMatrixCSC{Tv,Ti})
    if (size(v,1) != 1 && size(v,2) != 1)
        error("Input should be nx1 or 1xn")
    end

    n = numel(v)
    numnz = nnz(v)
    colptr = Array(Ti, n+1)
    rowval = Array(Ti, numnz)
    nzval = Array(Tv, numnz)

    if size(v,1) == 1
        copy_to(colptr, 1, v.colptr, 1, n+1)
        ptr = 1
        for col = 1:n
            if colptr[col] != colptr[col+1]
                rowval[ptr] = col
                nzval[ptr] = v.nzval[ptr]
                ptr += 1
            end
        end
    else
        copy_to(rowval, 1, v.rowval, 1, numnz)
        copy_to(nzval, 1, v.nzval, 1, numnz)
        colptr[1] = 1
        ptr = 1
        col = 1
        while col <= n && ptr <= numnz
            while rowval[ptr] > col
                colptr[col+1] = colptr[col]
                col += 1
            end
            colptr[col+1] = colptr[col] + 1
            ptr += 1
            col += 1
        end
        if col <= n
            colptr[(col+1):(n+1)] = colptr[col]
        end
    end

    return SparseMatrixCSC{Tv,Ti}(n, n, colptr, rowval, nzval)
end

function spdiagm{T}(v::Union(AbstractVector{T},AbstractMatrix{T}))
    if isa(v, AbstractMatrix)
        if (size(v,1) != 1 && size(v,2) != 1)
            error("Input should be nx1 or 1xn")
        end
    end

    n = numel(v)
    numnz = nnz(v)
    colptr = Array(Int32, n+1)
    rowval = Array(Int32, numnz)
    nzval = Array(T, numnz)

    colptr[1] = 1

    z = zero(T)

    ptr = 1
    for col=1:n
        x = v[col]
        if x != z
            colptr[col+1] = colptr[col] + 1
            rowval[ptr] = col
            nzval[ptr] = x
            ptr += 1
        else
            colptr[col+1] = colptr[col]
        end
    end

    return SparseMatrixCSC{T,Int32}(n, n, colptr, rowval, nzval)
end

## norm and rank

# TODO

## trace

function trace{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti})
    t = zero(Tv)
    for col=1:min(size(A))
        first = A.colptr[col]
        last = A.colptr[col+1]-1
        while first <= last
            mid = (first + last) >> 1
            row = A.rowval[mid]
            if row == col
                t += A.nzval[mid]
                break
            elseif row > col
                last = mid - 1
            else
                first = mid + 1
            end
        end
    end
    return t
end

# kron

function kron{TvA,TvB,TiA,TiB}(a::SparseMatrixCSC{TvA,TiA}, b::SparseMatrixCSC{TvB,TiB})
    Tv = promote_type(TvA,TvB)
    Ti = promote_type(TiA,TiB)

    numnzA = nnz(a)
    numnzB = nnz(b)

    numnz = numnzA * numnzB

    mA,nA = size(a)
    mB,nB = size(b)

    m, n = mA*mB, nA * nB

    colptr = Array(Ti, n+1)
    rowval = Array(Ti, numnz)
    nzval = Array(Tv, numnz)

    fill!(colptr, 0)
    fill!(rowval, 0)
    fill!(nzval, 0)

    colptr[1] = 1

    colptrA = a.colptr
    colptrB = b.colptr
    rowvalA = a.rowval
    rowvalB = b.rowval
    nzvalA = a.nzval
    nzvalB = b.nzval

    col = 1
    ptr = 1

    for j = 1:nA
        startA = colptrA[j]
        stopA = colptrA[j+1]-1
        lA = stopA - startA + 1

        for l = 1:nB
            startB = colptrB[l]
            stopB = colptrB[l+1]-1
            lB = stopB - startB + 1

            colptr[col+1] = colptr[col] + lA * lB
            col += 1

            for ptrA = startA : stopA
                rowA = rowvalA[ptrA]
                xA = nzvalA[ptrA]

                rowval[ptr:ptr+lB-1] = (rowA-1)*mB + rowvalB[startB:stopB]
                nzval[ptr:ptr+lB-1] = xA * nzvalB[startB:stopB]
                ptr += lB
            end
        end
    end

    return SparseMatrixCSC{Tv,Ti}(m, n, colptr, rowval, nzval)
end

## det, inv, cond

# TODO

## Structure query functions

function issym(A::SparseMatrixCSC)
    m, n = size(A)
    if m != n; error("matrix must be square, got $(m)x$(n)"); end
    return nnz(A - A.') == 0
end

function ishermitian(A::SparseMatrixCSC)
    m, n = size(A)
    if m != n; error("matrix must be square, got $(m)x$(n)"); end
    return nnz(A - A') == 0
end

function istril(A::SparseMatrixCSC)
    for col = 1:A.n, i = A.colptr[col]:(A.colptr[col]-1)
        if A.rowval[i] < col && A.nzval[i] != 0; return false; end
    end
    return true
end

function istriu(A::SparseMatrixCSC)
    for col = 1:A.n, i = A.colptr[col]:(A.colptr[col]-1)
        if A.rowval[i] > col && A.nzval[i] != 0; return false; end
    end
    return true
end

## diagmm

# multiply by diagonal matrix as vector
function diagmm!{Tv,Ti}(C::SparseMatrixCSC{Tv,Ti}, A::SparseMatrixCSC, b::Vector)
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

function diagmm!{Tv,Ti}(C::SparseMatrixCSC{Tv,Ti}, b::Vector, A::SparseMatrixCSC)
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

diagmm{Tv,Ti,T}(A::SparseMatrixCSC{Tv,Ti}, b::Vector{T}) =
    diagmm!(SparseMatrixCSC(size(A,1),size(A,2),Ti[],Ti[],promote_type(Tv,T)[]), A, b)

diagmm{T,Tv,Ti}(b::Vector{T}, A::SparseMatrixCSC{Tv,Ti}) =
    diagmm!(SparseMatrixCSC(size(A,1),size(A,2),Ti[],Ti[],promote_type(Tv,T)[]), b, A)
