# This file is a part of Julia. License is MIT: https://julialang.org/license

import Base.LinAlg: checksquare

## sparse matrix multiplication

function (*)(A::SparseMatrixCSC{TvA,TiA}, B::SparseMatrixCSC{TvB,TiB}) where {TvA,TiA,TvB,TiB}
    (*)(sppromote(A, B)...)
end
for f in (:A_mul_Bt, :A_mul_Bc,
          :At_mul_B, :Ac_mul_B,
          :At_mul_Bt, :Ac_mul_Bc)
    @eval begin
        function ($f)(A::SparseMatrixCSC{TvA,TiA}, B::SparseMatrixCSC{TvB,TiB}) where {TvA,TiA,TvB,TiB}
            ($f)(sppromote(A, B)...)
        end
    end
end

function sppromote(A::SparseMatrixCSC{TvA,TiA}, B::SparseMatrixCSC{TvB,TiB}) where {TvA,TiA,TvB,TiB}
    Tv = promote_type(TvA, TvB)
    Ti = promote_type(TiA, TiB)
    A  = convert(SparseMatrixCSC{Tv,Ti}, A)
    B  = convert(SparseMatrixCSC{Tv,Ti}, B)
    A, B
end

# In matrix-vector multiplication, the correct orientation of the vector is assumed.

for (f, op, transp) in ((:A_mul_B, :identity, false),
                        (:Ac_mul_B, :adjoint, true),
                        (:At_mul_B, :identity, true))
    @eval begin
        function $(Symbol(f,:!))(α::Number, A::SparseMatrixCSC, B::StridedVecOrMat, β::Number, C::StridedVecOrMat)
            if $transp
                A.n == size(C, 1) || throw(DimensionMismatch())
                A.m == size(B, 1) || throw(DimensionMismatch())
            else
                A.n == size(B, 1) || throw(DimensionMismatch())
                A.m == size(C, 1) || throw(DimensionMismatch())
            end
            size(B, 2) == size(C, 2) || throw(DimensionMismatch())
            nzv = A.nzval
            rv = A.rowval
            if β != 1
                β != 0 ? scale!(C, β) : fill!(C, zero(eltype(C)))
            end
            for k = 1:size(C, 2)
                for col = 1:A.n
                    if $transp
                        tmp = zero(eltype(C))
                        @inbounds for j = A.colptr[col]:(A.colptr[col + 1] - 1)
                            tmp += $(op)(nzv[j])*B[rv[j],k]
                        end
                        C[col,k] += α*tmp
                    else
                        αxj = α*B[col,k]
                        @inbounds for j = A.colptr[col]:(A.colptr[col + 1] - 1)
                            C[rv[j], k] += nzv[j]*αxj
                        end
                    end
                end
            end
            C
        end

        function $(f)(A::SparseMatrixCSC{TA,S}, x::StridedVector{Tx}) where {TA,S,Tx}
            T = promote_type(TA, Tx)
            $(Symbol(f,:!))(one(T), A, x, zero(T), similar(x, T, A.n))
        end
        function $(f)(A::SparseMatrixCSC{TA,S}, B::StridedMatrix{Tx}) where {TA,S,Tx}
            T = promote_type(TA, Tx)
            $(Symbol(f,:!))(one(T), A, B, zero(T), similar(B, T, (A.n, size(B, 2))))
        end
    end
end

# For compatibility with dense multiplication API. Should be deleted when dense multiplication
# API is updated to follow BLAS API.
A_mul_B!(C::StridedVecOrMat, A::SparseMatrixCSC, B::StridedVecOrMat) = A_mul_B!(one(eltype(B)), A, B, zero(eltype(C)), C)
Ac_mul_B!(C::StridedVecOrMat, A::SparseMatrixCSC, B::StridedVecOrMat) = Ac_mul_B!(one(eltype(B)), A, B, zero(eltype(C)), C)
At_mul_B!(C::StridedVecOrMat, A::SparseMatrixCSC, B::StridedVecOrMat) = At_mul_B!(one(eltype(B)), A, B, zero(eltype(C)), C)


function (*)(X::StridedMatrix{TX}, A::SparseMatrixCSC{TvA,TiA}) where {TX,TvA,TiA}
    mX, nX = size(X)
    nX == A.m || throw(DimensionMismatch())
    Y = zeros(promote_type(TX,TvA), mX, A.n)
    rowval = A.rowval
    nzval = A.nzval
    @inbounds for multivec_row=1:mX, col = 1:A.n, k=A.colptr[col]:(A.colptr[col+1]-1)
        Y[multivec_row, col] += X[multivec_row, rowval[k]] * nzval[k]
    end
    Y
end

function (*)(D::Diagonal, A::SparseMatrixCSC)
    T = Base.promote_op(*, eltype(D), eltype(A))
    scale!(LinAlg.copy_oftype(A, T), D.diag, A)
end
function (*)(A::SparseMatrixCSC, D::Diagonal)
    T = Base.promote_op(*, eltype(D), eltype(A))
    scale!(LinAlg.copy_oftype(A, T), A, D.diag)
end

# Sparse matrix multiplication as described in [Gustavson, 1978]:
# http://dl.acm.org/citation.cfm?id=355796

(*)(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti}) where {Tv,Ti} = spmatmul(A,B)
for (f, opA, opB) in ((:A_mul_Bt, :identity, :transpose),
                      (:A_mul_Bc, :identity, :adjoint),
                      (:At_mul_B, :transpose, :identity),
                      (:Ac_mul_B, :adjoint, :identity),
                      (:At_mul_Bt, :transpose, :transpose),
                      (:Ac_mul_Bc, :adjoint, :adjoint))
    @eval begin
        function ($f)(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti}) where {Tv,Ti}
            spmatmul(($opA)(A), ($opB)(B))
        end
    end
end

function spmatmul(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti};
                  sortindices::Symbol = :sortcols) where {Tv,Ti}
    mA, nA = size(A)
    mB, nB = size(B)
    nA==mB || throw(DimensionMismatch())

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    colptrB = B.colptr; rowvalB = B.rowval; nzvalB = B.nzval
    # TODO: Need better estimation of result space
    nnzC = min(mA*nB, length(nzvalA) + length(nzvalB))
    colptrC = Vector{Ti}(nB+1)
    rowvalC = Vector{Ti}(nnzC)
    nzvalC = Vector{Tv}(nnzC)

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
    C = SparseArrays.sortSparseMatrixCSC!(Cunsorted, sortindices=sortindices)
    return C
end

## solvers
function fwdTriSolve!(A::SparseMatrixCSCUnion, B::AbstractVecOrMat)
# forward substitution for CSC matrices
    nrowB, ncolB  = size(B, 1), size(B, 2)
    ncol = LinAlg.checksquare(A)
    if nrowB != ncol
        throw(DimensionMismatch("A is $(ncol) columns and B has $(nrowB) rows"))
    end

    aa = getnzval(A)
    ja = getrowval(A)
    ia = getcolptr(A)

    joff = 0
    for k = 1:ncolB
        for j = 1:nrowB
            i1 = ia[j]
            i2 = ia[j + 1] - 1

            # loop through the structural zeros
            ii = i1
            jai = ja[ii]
            while ii <= i2 && jai < j
                ii += 1
                jai = ja[ii]
            end

            # check for zero pivot and divide with pivot
            if jai == j
                bj = B[joff + jai]/aa[ii]
                B[joff + jai] = bj
                ii += 1
            else
                throw(LinAlg.SingularException(j))
            end

            # update remaining part
            for i = ii:i2
                B[joff + ja[i]] -= bj*aa[i]
            end
        end
        joff += nrowB
    end
    B
end

function bwdTriSolve!(A::SparseMatrixCSCUnion, B::AbstractVecOrMat)
# backward substitution for CSC matrices
    nrowB, ncolB = size(B, 1), size(B, 2)
    ncol = LinAlg.checksquare(A)
    if nrowB != ncol
        throw(DimensionMismatch("A is $(ncol) columns and B has $(nrowB) rows"))
    end

    aa = getnzval(A)
    ja = getrowval(A)
    ia = getcolptr(A)

    joff = 0
    for k = 1:ncolB
        for j = nrowB:-1:1
            i1 = ia[j]
            i2 = ia[j + 1] - 1

            # loop through the structural zeros
            ii = i2
            jai = ja[ii]
            while ii >= i1 && jai > j
                ii -= 1
                jai = ja[ii]
            end

            # check for zero pivot and divide with pivot
            if jai == j
                bj = B[joff + jai]/aa[ii]
                B[joff + jai] = bj
                ii -= 1
            else
                throw(LinAlg.SingularException(j))
            end

            # update remaining part
            for i = ii:-1:i1
                B[joff + ja[i]] -= bj*aa[i]
            end
        end
        joff += nrowB
    end
    B
end

A_ldiv_B!(L::LowerTriangular{T,<:SparseMatrixCSCUnion{T}}, B::StridedVecOrMat) where {T} = fwdTriSolve!(L.data, B)
A_ldiv_B!(U::UpperTriangular{T,<:SparseMatrixCSCUnion{T}}, B::StridedVecOrMat) where {T} = bwdTriSolve!(U.data, B)

(\)(L::LowerTriangular{T,<:SparseMatrixCSCUnion{T}}, B::SparseMatrixCSC) where {T} = A_ldiv_B!(L, Array(B))
(\)(U::UpperTriangular{T,<:SparseMatrixCSCUnion{T}}, B::SparseMatrixCSC) where {T} = A_ldiv_B!(U, Array(B))

function A_rdiv_B!(A::SparseMatrixCSC{T}, D::Diagonal{T}) where T
    dd = D.diag
    if (k = length(dd)) ≠ A.n
        throw(DimensionMismatch("size(A, 2)=$(A.n) should be size(D, 1)=$k"))
    end
    nonz = nonzeros(A)
    @inbounds for j in 1:k
        ddj = dd[j]
        if iszero(ddj)
            throw(LinAlg.SingularException(j))
        end
        for i in nzrange(A, j)
            nonz[i] /= ddj
        end
    end
    A
end

A_rdiv_Bc!(A::SparseMatrixCSC{T}, D::Diagonal{T}) where {T} = A_rdiv_B!(A, adjoint(D))
A_rdiv_Bt!(A::SparseMatrixCSC{T}, D::Diagonal{T}) where {T} = A_rdiv_B!(A, D)

## triu, tril

function triu(S::SparseMatrixCSC{Tv,Ti}, k::Integer=0) where {Tv,Ti}
    m,n = size(S)
    if !(-m + 1 <= k <= n + 1)
        throw(ArgumentError(string("the requested diagonal, $k, must be at least ",
            "$(-m + 1) and at most $(n + 1) in an $m-by-$n matrix")))
    end
    colptr = Vector{Ti}(n+1)
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
    rowval = Vector{Ti}(nnz)
    nzval = Vector{Tv}(nnz)
    A = SparseMatrixCSC(m, n, colptr, rowval, nzval)
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

function tril(S::SparseMatrixCSC{Tv,Ti}, k::Integer=0) where {Tv,Ti}
    m,n = size(S)
    if !(-m - 1 <= k <= n - 1)
        throw(ArgumentError(string("the requested diagonal, $k, must be at least ",
            "$(-m - 1) and at most $(n - 1) in an $m-by-$n matrix")))
    end
    colptr = Vector{Ti}(n+1)
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
    rowval = Vector{Ti}(nnz)
    nzval = Vector{Tv}(nnz)
    A = SparseMatrixCSC(m, n, colptr, rowval, nzval)
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

function sparse_diff1(S::SparseMatrixCSC{Tv,Ti}) where {Tv,Ti}
    m,n = size(S)
    m > 1 || return SparseMatrixCSC(0, n, ones(Ti,n+1), Ti[], Tv[])
    colptr = Vector{Ti}(n+1)
    numnz = 2 * nnz(S) # upper bound; will shrink later
    rowval = Vector{Ti}(numnz)
    nzval = Vector{Tv}(numnz)
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
    return SparseMatrixCSC(m-1, n, colptr, rowval, nzval)
end

function sparse_diff2(a::SparseMatrixCSC{Tv,Ti}) where {Tv,Ti}
    m,n = size(a)
    colptr = Vector{Ti}(max(n,1))
    numnz = 2 * nnz(a) # upper bound; will shrink later
    rowval = Vector{Ti}(numnz)
    nzval = Vector{Tv}(numnz)

    z = zero(Tv)

    colptr_a = a.colptr
    rowval_a = a.rowval
    nzval_a = a.nzval

    ptrS = 1
    colptr[1] = 1

    n == 0 && return SparseMatrixCSC(m, n, colptr, rowval, nzval)

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
    return SparseMatrixCSC(m, n-1, colptr, rowval, nzval)
end

diff(a::SparseMatrixCSC, dim::Integer)= dim==1 ? sparse_diff1(a) : sparse_diff2(a)

## norm and rank
vecnorm(A::SparseMatrixCSC, p::Real=2) = vecnorm(view(A.nzval, 1:nnz(A)), p)

function norm(A::SparseMatrixCSC,p::Real=2)
    m, n = size(A)
    if m == 0 || n == 0 || isempty(A)
        return float(real(zero(eltype(A))))
    elseif m == 1 || n == 1
        # TODO: compute more efficiently using A.nzval directly
        return norm(Array(A), p)
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
            throw(ArgumentError("2-norm not yet implemented for sparse matrices. Try norm(Array(A)) or norm(A, p) where p=1 or Inf."))
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

# TODO rank

# cond
function cond(A::SparseMatrixCSC, p::Real=2)
    if p == 1
        normAinv = normestinv(A)
        normA = norm(A, 1)
        return normA * normAinv
    elseif p == Inf
        normAinv = normestinv(A')
        normA = norm(A, Inf)
        return normA * normAinv
    elseif p == 2
        throw(ArgumentError("2-norm condition number is not implemented for sparse matrices, try cond(Array(A), 2) instead"))
    else
        throw(ArgumentError("second argument must be either 1 or Inf, got $p"))
    end
end

function normestinv(A::SparseMatrixCSC{T}, t::Integer = min(2,maximum(size(A)))) where T
    maxiter = 5
    # Check the input
    n = checksquare(A)
    F = factorize(A)
    if t <= 0
        throw(ArgumentError("number of blocks must be a positive integer"))
    end
    if t > n
        throw(ArgumentError("number of blocks must not be greater than $n"))
    end
    ind = Vector{Int64}(n)
    ind_hist = Vector{Int64}(maxiter * t)

    Ti = typeof(float(zero(T)))

    S = zeros(T <: Real ? Int : Ti, n, t)

    function _rand_pm1!(v)
        for i in eachindex(v)
            v[i] = rand()<0.5 ? 1 : -1
        end
    end

    function _any_abs_eq(v,n::Int)
        for vv in v
            if abs(vv)==n
                return true
            end
        end
        return false
    end

    # Generate the block matrix
    X = Matrix{Ti}(n, t)
    X[1:n,1] = 1
    for j = 2:t
        while true
            _rand_pm1!(view(X,1:n,j))
            yaux = X[1:n,j]' * X[1:n,1:j-1]
            if !_any_abs_eq(yaux,n)
                break
            end
        end
    end
    scale!(X, 1 ./ n)

    iter = 0
    local est
    local est_old
    est_ind = 0
    while iter < maxiter
        iter += 1
        Y = F \ X
        est = zero(real(eltype(Y)))
        est_ind = 0
        for i = 1:t
            y = norm(Y[1:n,i], 1)
            if y > est
                est = y
                est_ind = i
            end
        end
        if iter == 1
            est_old = est
        end
        if est > est_old || iter == 2
            ind_best = est_ind
        end
        if iter >= 2 && est <= est_old
            est = est_old
            break
        end
        est_old = est
        S_old = copy(S)
        for j = 1:t
            for i = 1:n
                S[i,j] = Y[i,j]==0 ? one(Y[i,j]) : sign(Y[i,j])
            end
        end

        if T <: Real
            # Check whether cols of S are parallel to cols of S or S_old
            for j = 1:t
                while true
                    repeated = false
                    if j > 1
                        saux = S[1:n,j]' * S[1:n,1:j-1]
                        if _any_abs_eq(saux,n)
                            repeated = true
                        end
                    end
                    if !repeated
                        saux2 = S[1:n,j]' * S_old[1:n,1:t]
                        if _any_abs_eq(saux2,n)
                            repeated = true
                        end
                    end
                    if repeated
                        _rand_pm1!(view(S,1:n,j))
                    else
                        break
                    end
                end
            end
        end

        # Use the conjugate transpose (adjoint)
        Z = F' \ S
        h_max = zero(real(eltype(Z)))
        h = zeros(real(eltype(Z)), n)
        h_ind = 0
        for i = 1:n
            h[i] = norm(Z[i,1:t], Inf)
            if h[i] > h_max
                h_max = h[i]
                h_ind = i
            end
            ind[i] = i
        end
        if iter >=2 && ind_best == h_ind
            break
        end
        p = sortperm(h, rev=true)
        h = h[p]
        permute!(ind, p)
        if t > 1
            addcounter = t
            elemcounter = 0
            while addcounter > 0 && elemcounter < n
                elemcounter = elemcounter + 1
                current_element = ind[elemcounter]
                found = false
                for i = 1:t * (iter - 1)
                    if current_element == ind_hist[i]
                        found = true
                        break
                    end
                end
                if !found
                    addcounter = addcounter - 1
                    for i = 1:current_element - 1
                        X[i,t-addcounter] = 0
                    end
                    X[current_element,t-addcounter] = 1
                    for i = current_element + 1:n
                        X[i,t-addcounter] = 0
                    end
                    ind_hist[iter * t - addcounter] = current_element
                else
                    if elemcounter == t && addcounter == t
                        break
                    end
                end
            end
        else
            ind_hist[1:t] = ind[1:t]
            for j = 1:t
                for i = 1:ind[j] - 1
                    X[i,j] = 0
                end
                X[ind[j],j] = 1
                for i = ind[j] + 1:n
                    X[i,j] = 0
                end
            end
        end
    end
    return est
end

# kron

function kron(a::SparseMatrixCSC{Tv,Ti}, b::SparseMatrixCSC{Tv,Ti}) where {Tv,Ti}
    numnzA = nnz(a)
    numnzB = nnz(b)

    numnz = numnzA * numnzB

    mA,nA = size(a)
    mB,nB = size(b)

    m,n = mA*mB, nA*nB

    colptr = Vector{Ti}(n+1)
    rowval = Vector{Ti}(numnz)
    nzval = Vector{Tv}(numnz)

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

            ptr_range = (1:lB) .+ (colptr[col]-1)

            colptr[col+1] = colptr[col] + lA * lB
            col += 1

            for ptrA = startA : stopA
                ptrB = startB
                for ptr = ptr_range
                    rowval[ptr] = (rowvalA[ptrA]-1)*mB + rowvalB[ptrB]
                    nzval[ptr] = nzvalA[ptrA] * nzvalB[ptrB]
                    ptrB += 1
                end
                ptr_range = ptr_range .+ lB
            end
        end
    end
    SparseMatrixCSC(m, n, colptr, rowval, nzval)
end

function kron(A::SparseMatrixCSC{Tv1,Ti1}, B::SparseMatrixCSC{Tv2,Ti2}) where {Tv1,Ti1,Tv2,Ti2}
    Tv_res = promote_type(Tv1, Tv2)
    Ti_res = promote_type(Ti1, Ti2)
    A = convert(SparseMatrixCSC{Tv_res,Ti_res}, A)
    B = convert(SparseMatrixCSC{Tv_res,Ti_res}, B)
    return kron(A,B)
end

kron(A::SparseMatrixCSC, B::VecOrMat) = kron(A, sparse(B))
kron(A::VecOrMat, B::SparseMatrixCSC) = kron(sparse(A), B)

function kron(x::SparseVector{Tv,Ti},y::SparseVector{Tv,Ti}) where {Tv,Ti}
    nnzx = nnz(x)
    nnzy = nnz(y)
    nnzz = nnzx*nnzy # number of nonzeros in new vector
    nzind = Vector{Ti}(nnzz) # the indices of nonzeros
    nzval = Vector{Tv}(nnzz) # the values of nonzeros
    @inbounds for i = 1:nnzx, j = 1:nnzy
        this_ind = (i-1)*nnzy+j
        nzind[this_ind] = (x.nzind[i]-1)*y.n + y.nzind[j]
        nzval[this_ind] = x.nzval[i] * y.nzval[j]
    end
    return SparseVector(x.n*y.n,nzind,nzval)
end

function kron(x::SparseVector{Tv1,Ti1}, y::SparseVector{Tv2,Ti2}) where {Tv1,Ti1,Tv2,Ti2}
    Tv_res = promote_type(Tv1, Tv2)
    Ti_res = promote_type(Ti1, Ti2)
    x2 = convert(SparseVector{Tv_res,Ti_res}, x)
    y2 = convert(SparseVector{Tv_res,Ti_res}, y)
    return kron(x2,y2)
end

kron(x::SparseVector{Tv,Ti}, y::AbstractVector) where {Tv,Ti} = kron(x, sparse(y))
kron(x::AbstractVector, y::SparseVector{Tv,Ti}) where {Tv,Ti} = kron(sparse(x), y)

## det, inv, cond

inv(A::SparseMatrixCSC) = error("The inverse of a sparse matrix can often be dense and can cause the computer to run out of memory. If you are sure you have enough memory, please convert your matrix to a dense matrix.")

# TODO

## scale methods

# Copy colptr and rowval from one sparse matrix to another
function copyinds!(C::SparseMatrixCSC, A::SparseMatrixCSC)
    if C.colptr !== A.colptr
        resize!(C.colptr, length(A.colptr))
        copy!(C.colptr, A.colptr)
    end
    if C.rowval !== A.rowval
        resize!(C.rowval, length(A.rowval))
        copy!(C.rowval, A.rowval)
    end
end

# multiply by diagonal matrix as vector
function scale!(C::SparseMatrixCSC, A::SparseMatrixCSC, b::Vector)
    m, n = size(A)
    (n==length(b) && size(A)==size(C)) || throw(DimensionMismatch())
    copyinds!(C, A)
    Cnzval = C.nzval
    Anzval = A.nzval
    resize!(Cnzval, length(Anzval))
    for col = 1:n, p = A.colptr[col]:(A.colptr[col+1]-1)
        @inbounds Cnzval[p] = Anzval[p] * b[col]
    end
    C
end

function scale!(C::SparseMatrixCSC, b::Vector, A::SparseMatrixCSC)
    m, n = size(A)
    (m==length(b) && size(A)==size(C)) || throw(DimensionMismatch())
    copyinds!(C, A)
    Cnzval = C.nzval
    Anzval = A.nzval
    Arowval = A.rowval
    resize!(Cnzval, length(Anzval))
    for col = 1:n, p = A.colptr[col]:(A.colptr[col+1]-1)
        @inbounds Cnzval[p] = Anzval[p] * b[Arowval[p]]
    end
    C
end

function scale!(C::SparseMatrixCSC, A::SparseMatrixCSC, b::Number)
    size(A)==size(C) || throw(DimensionMismatch())
    copyinds!(C, A)
    resize!(C.nzval, length(A.nzval))
    scale!(C.nzval, A.nzval, b)
    C
end

function scale!(C::SparseMatrixCSC, b::Number, A::SparseMatrixCSC)
    size(A)==size(C) || throw(DimensionMismatch())
    copyinds!(C, A)
    resize!(C.nzval, length(A.nzval))
    scale!(C.nzval, b, A.nzval)
    C
end

scale!(A::SparseMatrixCSC, b::Number) = (scale!(A.nzval, b); A)
scale!(b::Number, A::SparseMatrixCSC) = (scale!(b, A.nzval); A)

for f in (:\, :Ac_ldiv_B, :At_ldiv_B)
    @eval begin
        function ($f)(A::SparseMatrixCSC, B::AbstractVecOrMat)
            m, n = size(A)
            if m == n
                if istril(A)
                    if istriu(A)
                        return ($f)(Diagonal(Vector(diag(A))), B)
                    else
                        return ($f)(LowerTriangular(A), B)
                    end
                elseif istriu(A)
                    return ($f)(UpperTriangular(A), B)
                end
                if ishermitian(A)
                    return ($f)(Hermitian(A), B)
                end
                return ($f)(lufact(A), B)
            else
                return ($f)(qrfact(A), B)
            end
        end
        ($f)(::SparseMatrixCSC, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
    end
end

function factorize(A::SparseMatrixCSC)
    m, n = size(A)
    if m == n
        if istril(A)
            if istriu(A)
                return Diagonal(A)
            else
                return LowerTriangular(A)
            end
        elseif istriu(A)
            return UpperTriangular(A)
        end
        if ishermitian(A)
            return factorize(Hermitian(A))
        end
        return lufact(A)
    else
        return qrfact(A)
    end
end

# function factorize(A::Symmetric{Float64,SparseMatrixCSC{Float64,Ti}}) where Ti
#     F = cholfact(A)
#     if LinAlg.issuccess(F)
#         return F
#     else
#         ldltfact!(F, A)
#         return F
#     end
# end
function factorize(A::LinAlg.RealHermSymComplexHerm{Float64,<:SparseMatrixCSC})
    F = cholfact(A)
    if LinAlg.issuccess(F)
        return F
    else
        ldltfact!(F, A)
        return F
    end
end

chol(A::SparseMatrixCSC) = error("Use cholfact() instead of chol() for sparse matrices.")
lu(A::SparseMatrixCSC) = error("Use lufact() instead of lu() for sparse matrices.")
eig(A::SparseMatrixCSC) = error("Use eigs() instead of eig() for sparse matrices.")

function Base.cov(X::SparseMatrixCSC, vardim::Int=1; corrected::Bool=true)
    a, b = size(X)
    n, p = vardim == 1 ? (a, b) : (b, a)

    # The covariance can be decomposed into two terms
    # 1/(n - 1) ∑ (x_i - x̄)*(x_i - x̄)' = 1/(n - 1) (∑ x_i*x_i' - n*x̄*x̄')
    # which can be evaluated via a sparse matrix-matrix product

    # Compute ∑ x_i*x_i' = X'X using sparse matrix-matrix product
    out = Matrix(Base.unscaled_covzm(X, vardim))

    # Compute x̄
    x̄ᵀ = mean(X, vardim)

    # Subtract n*x̄*x̄' from X'X
    @inbounds for j in 1:p, i in 1:p
        out[i,j] -= x̄ᵀ[i] * x̄ᵀ[j]' * n
    end

    # scale with the sample size n or the corrected sample size n - 1
    return scale!(out, inv(n - corrected))
end
