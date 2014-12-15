## sparse matrix multiplication

function (*){TvA,TiA,TvB,TiB}(A::SparseMatrixCSC{TvA,TiA}, B::SparseMatrixCSC{TvB,TiB})
    Tv = promote_type(TvA, TvB)
    Ti = promote_type(TiA, TiB)
    A  = convert(SparseMatrixCSC{Tv,Ti}, A)
    B  = convert(SparseMatrixCSC{Tv,Ti}, B)
    A * B
end

(*){TvA,TiA}(A::SparseMatrixCSC{TvA,TiA}, X::BitArray{1}) = invoke(*, (SparseMatrixCSC, AbstractVector), A, X)
# In matrix-vector multiplication, the correct orientation of the vector is assumed.
function A_mul_B!(α::Number, A::SparseMatrixCSC, x::AbstractVector, β::Number, y::AbstractVector)
    A.n == length(x) || throw(DimensionMismatch())
    A.m == length(y) || throw(DimensionMismatch())
    if β != 1
        β != 0 ? scale!(y,β) : fill!(y,zero(eltype(y)))
    end
    nzv = A.nzval
    rv = A.rowval
    for col = 1 : A.n
        αx = α*x[col]
        @inbounds for k = A.colptr[col] : (A.colptr[col+1]-1)
            y[rv[k]] += nzv[k]*αx
        end
    end
    y
end
A_mul_B!(y::AbstractVector, A::SparseMatrixCSC, x::AbstractVector) = A_mul_B!(one(eltype(x)), A, x, zero(eltype(y)), y)

function *{TA,S,Tx}(A::SparseMatrixCSC{TA,S}, x::AbstractVector{Tx})
    T = promote_type(TA,Tx)
    A_mul_B!(one(T), A, x, zero(T), Array(T, A.m))
end

function Ac_mul_B!(α::Number, A::SparseMatrixCSC, x::AbstractVector, β::Number, y::AbstractVector)
    A.n == length(y) || throw(DimensionMismatch())
    A.m == length(x) || throw(DimensionMismatch())
    nzv = A.nzval
    rv = A.rowval
    zro = zero(eltype(y))
    @inbounds begin
        for i = 1 : A.n
            if β != 1
                if β != 0
                    y[i] *= β
                else
                    y[i] = zro
                end
            end
            tmp = zero(eltype(y))
            for j = A.colptr[i] : (A.colptr[i+1]-1)
                tmp += conj(nzv[j])*x[rv[j]]
            end
            y[i] += α*tmp
        end
    end
    y
end
Ac_mul_B!(y::AbstractVector, A::SparseMatrixCSC, x::AbstractVector) = Ac_mul_B!(one(eltype(x)), A, x, zero(eltype(y)), y)
function At_mul_B!(α::Number, A::SparseMatrixCSC, x::AbstractVector, β::Number, y::AbstractVector)
    A.n == length(y) || throw(DimensionMismatch())
    A.m == length(x) || throw(DimensionMismatch())
    nzv = A.nzval
    rv = A.rowval
    zro = zero(eltype(y))
    @inbounds begin
        for i = 1 : A.n
            if β != 1
                if β != 0
                    y[i] *= β
                else
                    y[i] = zro
                end
            end
            tmp = zero(eltype(y))
            for j = A.colptr[i] : (A.colptr[i+1]-1)
                tmp += nzv[j]*x[rv[j]]
            end
            y[i] += α*tmp
        end
    end
    y
end
At_mul_B!(y::AbstractVector, A::SparseMatrixCSC, x::AbstractVector) = At_mul_B!(one(eltype(x)), A, x, zero(eltype(y)), y)
function Ac_mul_B{TA,S,Tx}(A::SparseMatrixCSC{TA,S}, x::AbstractVector{Tx})
    T = promote_type(TA, Tx)
    Ac_mul_B!(one(T), A, x, zero(T), Array(T, A.n))
end
function At_mul_B{TA,S,Tx}(A::SparseMatrixCSC{TA,S}, x::AbstractVector{Tx})
    T = promote_type(TA, Tx)
    At_mul_B!(one(T), A, x, zero(T), Array(T, A.n))
end

*(X::BitArray{1}, A::SparseMatrixCSC) = invoke(*, (AbstractVector, SparseMatrixCSC), X, A)
# In vector-matrix multiplication, the correct orientation of the vector is assumed.
# XXX: this is wrong (i.e. not what Arrays would do)!!
function *{T1,T2}(X::AbstractVector{T1}, A::SparseMatrixCSC{T2})
    A.m==length(X) || throw(DimensionMismatch())
    Y = zeros(promote_type(T1,T2), A.n)
    nzv = A.nzval
    rv = A.rowval
    for col =1:A.n, k=A.colptr[col]:(A.colptr[col+1]-1)
        Y[col] += X[rv[k]] * nzv[k]
    end
    Y
end

*{TvA,TiA}(A::SparseMatrixCSC{TvA,TiA}, X::BitArray{2}) = invoke(*, (SparseMatrixCSC, AbstractMatrix), A, X)
function (*){TvA,TiA,TX}(A::SparseMatrixCSC{TvA,TiA}, X::AbstractMatrix{TX})
    mX, nX = size(X)
    A.n==mX || throw(DimensionMismatch())
    Y = zeros(promote_type(TvA,TX), A.m, nX)
    nzv = A.nzval
    rv = A.rowval
    colptr = A.colptr
    for multivec_col=1:nX, col=1:A.n
        Xc = X[col, multivec_col]
        @inbounds for k = colptr[col] : (colptr[col+1]-1)
           Y[rv[k], multivec_col] += nzv[k] * Xc
        end
    end
    Y
end

*{TvA,TiA}(X::BitArray{2}, A::SparseMatrixCSC{TvA,TiA}) = invoke(*, (AbstractMatrix, SparseMatrixCSC), X, A)
# TODO: Tridiagonal * Sparse should be implemented more efficiently
*{TX,TvA,TiA}(X::Tridiagonal{TX}, A::SparseMatrixCSC{TvA,TiA}) = invoke(*, (Tridiagonal, AbstractMatrix), X, A)
*{TvA,TiA}(X::Triangular, A::SparseMatrixCSC{TvA,TiA}) = full(X)*A
function *{TX,TvA,TiA}(X::AbstractMatrix{TX}, A::SparseMatrixCSC{TvA,TiA})
    mX, nX = size(X)
    nX == A.m || throw(DimensionMismatch())
    Y = zeros(promote_type(TX,TvA), mX, A.n)
    for multivec_row=1:mX, col=1:A.n, k=A.colptr[col]:(A.colptr[col+1]-1)
        Y[multivec_row, col] += X[multivec_row, A.rowval[k]] * A.nzval[k]
    end
    Y
end

# Sparse matrix multiplication as described in [Gustavson, 1978]:
# http://www.cse.iitb.ac.in/graphics/~anand/website/include/papers/matrix/fast_matrix_mul.pdf

*{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti}) = spmatmul(A,B)

function spmatmul{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti};
                         sortindices::Symbol = :sortcols)
    mA, nA = size(A)
    mB, nB = size(B)
    nA==mB || throw(DimensionMismatch())

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

    deleteat!(rowvalC, colptrC[end]:length(rowvalC))
    deleteat!(nzvalC, colptrC[end]:length(nzvalC))

    # The Gustavson algorithm does not guarantee the product to have sorted row indices.
    Cunsorted = SparseMatrixCSC(mA, nB, colptrC, rowvalC, nzvalC)
    C = Base.SparseMatrix.sortSparseMatrixCSC!(Cunsorted, sortindices=sortindices)
    return C
end

## solvers
function A_ldiv_B!(A::SparseMatrixCSC, b::AbstractVecOrMat)
    if iseltype(b, Complex); A = complex(A); end

    if istril(A)
        # TODO: Fix diagonal case. Diagonal(A.nzval) needs to handle
        # the case where there are zeros on the diagonal and error out.
        # It also does not work in the complex case. VBS.
        #if istriu(A); return A_ldiv_B!(Diagonal(A.nzval), b); end
        return fwdTriSolve!(A, b)
    end
    if istriu(A); return bwdTriSolve!(A, b); end
    return A_ldiv_B!(lufact(A),b)
end

function fwdTriSolve!(A::SparseMatrixCSC, B::AbstractVecOrMat)
# forward substitution for CSC matrices
    n = length(B)
    if isa(B, Vector)
        nrowB = n
        ncolB = 1
    else
        nrowB, ncolB = size(B)
    end
    ncol = chksquare(A)
    if nrowB != ncol
        throw(DimensionMismatch("A is $(ncol)X$(ncol) and B has length $(n)"))
    end

    aa = A.nzval
    ja = A.rowval
    ia = A.colptr

    joff = 0
    for k = 1:ncolB
        for j = 1:(nrowB-1)
            jb = joff + j
            i1 = ia[j]
            i2 = ia[j+1]-1
            B[jb] /= aa[i1]
            bj = B[jb]
            for i = i1+1:i2
                B[joff+ja[i]] -= bj*aa[i]
            end
        end
        joff += nrowB
        B[joff] /= aa[end]
    end
    return B
end

function bwdTriSolve!(A::SparseMatrixCSC, B::AbstractVecOrMat)
# backward substitution for CSC matrices
    n = length(B)
    if isa(B, Vector)
        nrowB = n
        ncolB = 1
    else
        nrowB, ncolB = size(B)
    end
    ncol = chksquare(A)
    if nrowB != ncol throw(DimensionMismatch("A is $(ncol)X$(ncol) and B has length $(n)")) end

    aa = A.nzval
    ja = A.rowval
    ia = A.colptr

    joff = 0
    for k = 1:ncolB
        for j = nrowB:-1:2
            jb = joff + j
            i1 = ia[j]
            i2 = ia[j+1]-1
            B[jb] /= aa[i2]
            bj = B[jb]
            for i = i2-1:-1:i1
                B[joff+ja[i]] -= bj*aa[i]
            end
        end
        B[joff+1] /= aa[1]
        joff += nrowB
    end
   return B
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
            S.rowval[c1] > col - k && break
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
    A
end

function tril{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, k::Integer)
    m,n = size(S)
    colptr = Array(Ti, n+1)
    nnz = 0
    colptr[1] = 1
    for col = 1 : min(n, m+k)
        l1 = S.colptr[col+1]-1
        for c1 = 0 : (l1 - S.colptr[col])
            S.rowval[l1 - c1] < col - k && break
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
    A
end

## diff

function sparse_diff1{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti})
    m,n = size(S)
    m > 1 || return SparseMatrixCSC{Tv,Ti}(0, n, ones(n+1), Ti[], Tv[])
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
                    nzval[numnz]==zero(Tv) && (numnz -= 1)
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
    deleteat!(rowval, numnz+1:length(rowval))
    deleteat!(nzval, numnz+1:length(nzval))
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

    n == 0 && return SparseMatrixCSC{Tv,Ti}(m, n, colptr, rowval, nzval)

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
    deleteat!(rowval, ptrS:length(rowval))
    deleteat!(nzval, ptrS:length(nzval))
    return SparseMatrixCSC{Tv,Ti}(m, n-1, colptr, rowval, nzval)
end

diff(a::SparseMatrixCSC, dim::Integer)= dim==1 ? sparse_diff1(a) : sparse_diff2(a)

## norm and rank
vecnorm(A::SparseMatrixCSC, p::Real=2) = vecnorm(A.nzval, p)

function norm(A::SparseMatrixCSC,p::Real=2)
    m, n = size(A)
    if m == 0 || n == 0 || isempty(A)
        return float(real(zero(eltype(A))))
    elseif m == 1 || n == 1
        # TODO: compute more efficiently using A.nzval directly
        return norm(full(A), p)
    else
        Tnorm = typeof(float(real(zero(eltype(A)))))
        Tsum = promote_type(Float64,Tnorm)
        if p==1
            nA::Tsum = 0
            for j=1:n
                colSum::Tsum = 0
                for i = A.colptr[j]:A.colptr[j+1]-1
                    colSum += abs(A.nzval[i])
                end
                nA = max(nA, colSum)
            end
            return convert(Tnorm, nA)
        elseif p==2
            throw(ArgumentError("2-norm not yet implemented for sparse matrices. Try norm(full(A)) or norm(A, p) where p=1 or Inf."))
        elseif p==Inf
            rowSum = zeros(Tsum,m)
            for i=1:length(A.nzval)
                rowSum[A.rowval[i]] += abs(A.nzval[i])
            end
            return convert(Tnorm, maximum(rowSum))
        end
    end
    throw(ArgumentError("invalid p-norm p=$p. Valid: 1, Inf"))
end

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

    @inbounds for j = 1:nA
        startA = colptrA[j]
        stopA = colptrA[j+1]-1
        lA = stopA - startA + 1

        for i = 1:nB
            startB = colptrB[i]
            stopB = colptrB[i+1]-1
            lB = stopB - startB + 1

            ptr_range = (1:lB) + (colptr[col]-1)

            colptr[col+1] = colptr[col] + lA * lB
            col += 1

            for ptrA = startA : stopA
                ptrB = startB
                for ptr = ptr_range
                    rowval[ptr] = (rowvalA[ptrA]-1)*mB + rowvalB[ptrB]
                    nzval[ptr] = nzvalA[ptrA] * nzvalB[ptrB]
                    ptrB += 1
                end
                ptr_range += lB
            end
        end
    end
    SparseMatrixCSC(m, n, colptr, rowval, nzval)
end

function kron{Tv1,Ti1,Tv2,Ti2}(A::SparseMatrixCSC{Tv1,Ti1}, B::SparseMatrixCSC{Tv2,Ti2})
    Tv_res = promote_type(Tv1, Tv2)
    Ti_res = promote_type(Ti1, Ti2)
    A = convert(SparseMatrixCSC{Tv_res,Ti_res}, A)
    B = convert(SparseMatrixCSC{Tv_res,Ti_res}, B)
    return kron(A,B)
end

kron(A::SparseMatrixCSC, B::VecOrMat) = kron(A, sparse(B))
kron(A::VecOrMat, B::SparseMatrixCSC) = kron(sparse(A), B)

## det, inv, cond

inv(A::SparseMatrixCSC) = error("The inverse of a sparse matrix can often be dense and can cause the computer to run out of memory. If you are sure you have enough memory, please convert your matrix to a dense matrix.")

# TODO

## scale methods

# multiply by diagonal matrix as vector
function scale!{Tv,Ti}(C::SparseMatrixCSC{Tv,Ti}, A::SparseMatrixCSC, b::Vector)
    m, n = size(A)
    (n==length(b) && size(A)==size(C)) || throw(DimensionMismatch())
    numnz = nnz(A)
    C.colptr = convert(Array{Ti}, A.colptr)
    C.rowval = convert(Array{Ti}, A.rowval)
    C.nzval = Array(Tv, numnz)
    for col = 1:n, p = A.colptr[col]:(A.colptr[col+1]-1)
        C.nzval[p] = A.nzval[p] * b[col]
    end
    C
end

function scale!{Tv,Ti}(C::SparseMatrixCSC{Tv,Ti}, b::Vector, A::SparseMatrixCSC)
    m, n = size(A)
    (n==length(b) && size(A)==size(C)) || throw(DimensionMismatch())
    numnz = nnz(A)
    C.colptr = convert(Array{Ti}, A.colptr)
    C.rowval = convert(Array{Ti}, A.rowval)
    C.nzval = Array(Tv, numnz)
    for col = 1:n, p = A.colptr[col]:(A.colptr[col+1]-1)
        C.nzval[p] = A.nzval[p] * b[A.rowval[p]]
    end
    C
end

scale{Tv,Ti,T}(A::SparseMatrixCSC{Tv,Ti}, b::Vector{T}) =
    scale!(SparseMatrixCSC(size(A,1),size(A,2),Ti[],Ti[],promote_type(Tv,T)[]), A, b)

scale{T,Tv,Ti}(b::Vector{T}, A::SparseMatrixCSC{Tv,Ti}) =
    scale!(SparseMatrixCSC(size(A,1),size(A,2),Ti[],Ti[],promote_type(Tv,T)[]), b, A)

chol(A::SparseMatrixCSC) = error("Use cholfact() instead of chol() for sparse matrices.")
lu(A::SparseMatrixCSC) = error("Use lufact() instead of lu() for sparse matrices.")
eig(A::SparseMatrixCSC) = error("Use eigs() instead of eig() for sparse matrices.")
