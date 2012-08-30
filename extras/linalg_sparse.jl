## linalg_sparse.jl: Basic Linear Algebra functions for sparse representations ##

require("sparse.jl")

## Matrix multiplication

# In matrix-vector multiplication, the correct orientation of the vector is assumed.
function (*){T1,T2}(A::SparseMatrixCSC{T1}, X::Vector{T2})
    if A.n != length(X); error("mismatched dimensions"); end
    Y = zeros(promote_type(T1,T2), A.m)
    for col = 1 : A.n, k = A.colptr[col] : (A.colptr[col+1]-1)
        Y[A.rowval[k]] += A.nzval[k] * X[col]
    end
    return Y
end

# In vector-matrix multiplication, the correct orientation of the vector is assumed.
# XXX: this is wrong (i.e. not what Arrays would do)!!
function (*){T1,T2}(X::Vector{T1}, A::SparseMatrixCSC{T2})
    if A.m != length(X); error("mismatched dimensions"); end
    Y = zeros(promote_type(T1,T2), A.n)
    for col = 1 : A.n, k = A.colptr[col] : (A.colptr[col+1]-1)
        Y[col] += X[A.rowval[k]] * A.nzval[k]
    end
    return Y
end

function (*){T1,T2}(A::SparseMatrixCSC{T1}, X::Matrix{T2})
    mX, nX = size(X)
    if A.n != mX; error("mismatched dimensions"); end
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
    if nX != A.m; error("mismatched dimensions"); end
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

# Sparse matrix multiplication as described in [Gustavson, 1978]:
# http://www.cse.iitb.ac.in/graphics/~anand/website/include/papers/matrix/fast_matrix_mul.pdf

function (*){TvA,TiA,TvB,TiB}(A::SparseMatrixCSC{TvA,TiA}, B::SparseMatrixCSC{TvB,TiB})
    mA, nA = size(A)
    mB, nB = size(B)
    if nA != mB; error("mismatched dimensions"); end
    Tv = promote_type(TvA, TvB)
    Ti = promote_type(TiA, TiB)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    colptrB = B.colptr; rowvalB = B.rowval; nzvalB = B.nzval
    # TODO: Need better estimation of result space
    nnzC = min(mA*nB, length(nzvalA) + length(nzvalB))
    colptrC = Array(Ti, nB+1)
    rowvalC = Array(Ti, nnzC)
    nzvalC = Array(Tv, nnzC)

    ip = 1
    xb = zeros(Ti, mA)
    x  = zeros(Tv, mA)
    for i in 1:nB
        if ip + mA - 1 > nnzC
            rowvalC = grow(rowvalC, max(nnzC,mA))
            nzvalC = grow(nzvalC, max(nnzC,mA))
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

    rowvalC = del(rowvalC, colptrC[end]:length(rowvalC))
    nzvalC = del(nzvalC, colptrC[end]:length(nzvalC))
    return SparseMatrixCSC(mA, nB, colptrC, rowvalC, nzvalC)
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

diag(A::SparseMatrixCSC) = [ A[i,i] for i=1:min(size(A)) ]

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

function istriu(A::SparseMatrixCSC)
    for col = 1:min(A.n,A.m-1)
        l1 = A.colptr[col+1]-1
        for i = 0 : (l1 - A.colptr[col])
            if A.rowval[l1-i] <= col
                break
            end
            if A.nzval[l1-i] != 0
                return false
            end
        end
    end
    return true
end

function istril(A::SparseMatrixCSC)
    for col = 2:A.n
        for i = A.colptr[col] : (A.colptr[col+1]-1)
            if A.rowval[i] >= col
                break
            end
            if A.nzval[i] != 0
                return false
            end
        end
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


##### Tridiagonal solver #####
# Allocation-free variants
# Note that solve is non-aliasing, so you can use the same array for
# input and output
function solve(x::AbstractArray, xrng::Ranges{Int}, M::Tridiagonal, rhs::AbstractArray, rhsrng::Ranges{Int})
    d = M.d
    N = length(d)
    if length(xrng) != N || length(rhsrng) != N
        error("dimension mismatch")
    end
    dl = M.dl
    du = M.du
    dutmp = M.dutmp
    rhstmp = M.rhstmp
    xstart = first(xrng)
    xstride = step(xrng)
    rhsstart = first(rhsrng)
    rhsstride = step(rhsrng)
    # Forward sweep
    denom = d[1]
    dulast = du[1] / denom
    dutmp[1] = dulast
    rhslast = rhs[rhsstart] / denom
    rhstmp[1] = rhslast
    irhs = rhsstart+rhsstride
    for i in 2:N-1
        dltmp = dl[i-1]
        denom = d[i] - dltmp*dulast
        dulast = du[i] / denom
        dutmp[i] = dulast
        rhslast = (rhs[irhs] - dltmp*rhslast)/denom
        rhstmp[i] = rhslast
        irhs += rhsstride
    end
    dltmp = dl[N-1]
    denom = d[N] - dltmp*dulast
    xlast = (rhs[irhs] - dltmp*rhslast)/denom
    # Backward sweep
    ix = xstart + (N-2)*xstride
    x[ix+xstride] = xlast
    for i in N-1:-1:1
        xlast = rhstmp[i] - dutmp[i]*xlast
        x[ix] = xlast
        ix -= xstride
    end
    return x
end
solve(x::StridedVector, M::Tridiagonal, rhs::StridedVector) = solve(x, 1:length(x), M, rhs, 1:length(rhs))
function solve(M::Tridiagonal, rhs::StridedVector)
    x = similar(rhs)
    solve(x, M, rhs)
end
function solve(X::StridedMatrix, M::Tridiagonal, B::StridedMatrix)
    if size(B, 1) != size(M, 1)
        error("dimension mismatch")
    end
    if size(X) != size(B)
        error("dimension mismatch in output")
    end
    m, n = size(B)
    r = 1:m
    for j = 1:n
        r.start = (j-1)*m+1
        solve(X, r, M, B, r)
    end
    return X
end
function solve(M::Tridiagonal, B::StridedMatrix)
    X = similar(B)
    solve(X, M, B)
end

# User-friendly solver
typealias LapackScalar Union(Float64,Float32,Complex128,Complex64)
function \{T<:LapackScalar}(M::Tridiagonal{T}, rhs::StridedVecOrMat{T})
    if stride(rhs, 1) == 1
        x = copy(rhs)
        Mc = copy(M)
        Mlu, x = _jl_lapack_gtsv(Mc, x)
        return x
    end
    solve(M, rhs)
end
\(M::Tridiagonal, rhs::Union(StridedVector,StridedMatrix)) = solve(M, rhs)

# Tridiagonal multiplication
function mult(x::AbstractArray, xrng::Ranges{Int}, M::Tridiagonal, v::AbstractArray, vrng::Ranges{Int})
    dl = M.dl
    d = M.d
    du = M.du
    N = length(d)
    xi = first(xrng)
    xstride = step(xrng)
    vi = first(vrng)
    vstride = step(vrng)
    x[xi] = d[1]*v[vi] + du[1]*v[vi+vstride]
    xi += xstride
    for i = 2:N-1
        x[xi] = dl[i-1]*v[vi] + d[i]*v[vi+vstride] + du[i]*v[vi+2*vstride]
        xi += xstride
        vi += vstride
    end
    x[xi] = dl[N-1]*v[vi] + d[N]*v[vi+vstride]
    return x
end
mult(x::StridedVector, M::Tridiagonal, v::StridedVector) = mult(x, 1:length(x), M, v, 1:length(v))
function mult(X::StridedMatrix, M::Tridiagonal, B::StridedMatrix)
    if size(B, 1) != size(M, 1)
        error("dimension mismatch")
    end
    if size(X) != size(B)
        error("dimension mismatch in output")
    end
    m, n = size(B)
    r = 1:m
    for j = 1:n
        r.start = (j-1)*m+1
        solve(X, r, M, B, r)
    end
    return X
end
mult(X::StridedMatrix, M1::Tridiagonal, M2::Tridiagonal) = mult(X, M1, full(M2))
function *(M::Tridiagonal, B::Union(StridedVector,StridedMatrix))
    X = similar(B)
    mult(X, M, B)
end


# Lapack routines
_jl_liblapack = dlopen("libopenblas") # FIXME: use choice in build_h.jl?
typealias LapackChar Char
# Decompositions
for (gttrf, pttrf, elty) in
    ((:dgttrf_,:dpttrf_,:Float64),
     (:sgttrf_,:spttrf_,:Float32),
     (:zgttrf_,:zpttrf_,:Complex128),
     (:cgttrf_,:cpttrf_,:Complex64))
    @eval begin
        function _jl_lapack_gttrf(M::Tridiagonal{$elty})
            info = zero(Int32)
            n    = int32(length(M.d))
            ipiv = Array(Int32, n)
            ccall(dlsym(_jl_liblapack, $string(gttrf)),
                  Void,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{Int32}, Ptr{Int32}),
                  &n, M.dl, M.d, M.du, M.dutmp, ipiv, &info)
            if info != 0 throw(LapackException(info)) end
            M, ipiv
        end
        function _jl_lapack_pttrf(M::Tridiagonal{$elty})
            info = zero(Int32)
            n    = int32(length(M.d))
            ccall(dlsym(_jl_liblapack, $string(pttrf)),
                  Void,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                  &n, M.d, M.dl, &info)
            if info != 0 throw(LapackException(info)) end
            M
        end
    end
end
# Direct solvers
for (gtsv, ptsv, elty) in
    ((:dgtsv_,:dptsv_,:Float64),
     (:sgtsv_,:sptsv,:Float32),
     (:zgtsv_,:zptsv,:Complex128),
     (:cgtsv_,:cptsv,:Complex64))
    @eval begin
        function _jl_lapack_gtsv(M::Tridiagonal{$elty}, B::StridedVecOrMat{$elty})
            if stride(B,1) != 1
                error("_jl_lapack_gtsv: matrix columns must have contiguous elements");
            end
            info = zero(Int32)
            n    = int32(length(M.d))
            nrhs = int32(size(B, 2))
            ldb  = int32(stride(B, 2))
            ccall(dlsym(_jl_liblapack, $string(gtsv)),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{Int32}, Ptr{Int32}),
                  &n, &nrhs, M.dl, M.d, M.du, B, &ldb, &info)
            if info != 0 throw(LapackException(info)) end
            M, B
        end
        function _jl_lapack_ptsv(M::Tridiagonal{$elty}, B::StridedVecOrMat{$elty})
            if stride(B,1) != 1
                error("_jl_lapack_ptsv: matrix columns must have contiguous elements");
            end
            info = zero(Int32)
            n    = int32(length(M.d))
            nrhs = int32(size(B, 2))
            ldb  = int32(stride(B, 2))
            ccall(dlsym(_jl_liblapack, $string(ptsv)),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{Int32}, Ptr{Int32}),
                  &n, &nrhs, M.d, M.dl, B, &ldb, &info)
            if info != 0 throw(LapackException(info)) end
            M, B
        end
    end
end
# Solvers using decompositions
for (gttrs, pttrs, elty) in
    ((:dgttrs_,:dpttrs_,:Float64),
     (:sgttrs_,:spttrs,:Float32),
     (:zgttrs_,:zpttrs,:Complex128),
     (:cgttrs_,:cpttrs,:Complex64))
    @eval begin
        function _jl_lapack_gttrs(trans::LapackChar, M::Tridiagonal{$elty}, ipiv::Vector{Int32}, B::StridedVecOrMat{$elty})
            if stride(B,1) != 1
                error("_jl_lapack_gttrs: matrix columns must have contiguous elements");
            end
            info = zero(Int32)
            n    = int32(length(M.d))
            nrhs = int32(size(B, 2))
            ldb  = int32(stride(B, 2))
            ccall(dlsym(_jl_liblapack, $string(gttrs)),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &trans, &n, &nrhs, M.dl, M.d, M.du, M.dutmp, ipiv, B, &ldb, &info)
            if info != 0 throw(LapackException(info)) end
            B
        end
        function _jl_lapack_pttrs(M::Tridiagonal{$elty}, B::StridedVecOrMat{$elty})
            if stride(B,1) != 1
                error("_jl_lapack_pttrs: matrix columns must have contiguous elements");
            end
            info = zero(Int32)
            n    = int32(length(M.d))
            nrhs = int32(size(B, 2))
            ldb  = int32(stride(B, 2))
            ccall(dlsym(_jl_liblapack, $string(pttrs)),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{Int32}, Ptr{Int32}),
                  &n, &nrhs, M.d, M.dl, B, &ldb, &info)
            if info != 0 throw(LapackException(info)) end
            B
        end
    end
end
# Eigenvalue-eigenvector (symmetric only)
for (stev, elty) in
    ((:dstev_,:Float64),
     (:sstev_,:Float32),
     (:zstev_,:Complex128),
     (:cstev_,:Complex64))
    @eval begin
        function _jl_lapack_stev(Z::Array, M::Tridiagonal{$elty})
            n    = int32(length(M.d))
            if isempty(Z)
                job = 'N'
                ldz = 1
                work = Array($elty, 0)
                Ztmp = work
            else
                if stride(Z,1) != 1
                    error("_jl_lapack_stev: eigenvector matrix columns must have contiguous elements");
                end
                if size(Z, 1) != n
                    error("_jl_lapack_stev: eigenvector matrix columns are not of the correct size")
                end
                Ztmp = Z
                job = 'V'
                ldz  = int32(stride(Z, 2))
                work = Array($elty, max(1, 2*n-2))
            end
            info = zero(Int32)
            ccall(dlsym(_jl_liblapack, $string(stev)),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32},
                   Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &job, &n, M.d, M.dl, Ztmp, &ldz, work, &info)
            if info != 0 throw(LapackException(info)) end
            M.d
        end
    end
end
function _jl_lapack_stev(job::LapackChar, M::Tridiagonal)
    if job == 'N' || job == 'n'
        Z = []
    elseif job == 'V' || job == 'v'
        n = length(M.d)
        Z = Array(eltype(M), n, n)
    else
        error("Job type not recognized")
    end
    D = _jl_lapack_stev(Z, M)
    return D, Z
end
eig(M::Tridiagonal) = _jl_lapack_stev('V', copy(M))


##### Woodbury matrix routines #####
function *(W::Woodbury, B::StridedVecOrMat)
    return W.A*B + W.U*(W.C*(W.V*B))
end
function \(W::Woodbury, R::StridedVecOrMat)
    AinvR = W.A\R
    return AinvR - W.A\(W.U*(W.Cp*(W.V*AinvR)))
end
# Allocation-free solver for arbitrary strides (requires that W.A has a
# non-aliasing "solve" routine, e.g., is tridiagonal)
function solve(x::AbstractArray, xrng::Ranges{Int}, W::Woodbury, rhs::AbstractArray, rhsrng::Ranges{Int})
    solve(W.tmpN1, 1:length(W.tmpN1), W.A, rhs, rhsrng)
    A_mul_B(W.tmpk1, W.V, W.tmpN1)
    A_mul_B(W.tmpk2, W.Cp, W.tmpk1)
    A_mul_B(W.tmpN2, W.U, W.tmpk2)
    solve(W.tmpN2, W.A, W.tmpN2)
    indx = first(xrng)
    xinc = step(xrng)
    for i = 1:length(W.tmpN2)
        x[indx] = W.tmpN1[i] - W.tmpN2[i]
        indx += xinc
    end
end
solve(x::AbstractVector, W::Woodbury, rhs::AbstractVector) = solve(x, 1:length(x), W, rhs, 1:length(rhs))
function solve(W::Woodbury, rhs::AbstractVector)
    x = similar(rhs)
    solve(x, W, rhs)
end
function solve(X::StridedMatrix, W::Woodbury, B::StridedMatrix)
    if size(B, 1) != size(W, 1)
        error("dimension mismatch")
    end
    if size(X) != size(B)
        error("dimension mismatch in output")
    end
    m, n = size(B)
    r = 1:m
    for j = 1:n
        r.start = (j-1)*m+1
        solve(X, r, W, B, r)
    end
    return X
end
function solve(W::Woodbury, B::StridedMatrix)
    X = similar(B)
    solve(X, W, B)
end
