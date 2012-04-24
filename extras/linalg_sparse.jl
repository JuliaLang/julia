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
    for col = max(k+1,1) : S.n
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
