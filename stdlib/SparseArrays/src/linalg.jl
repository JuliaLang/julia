# This file is a part of Julia. License is MIT: https://julialang.org/license

import LinearAlgebra: checksquare, sym_uplo
using Random: rand!

# In matrix-vector multiplication, the correct orientation of the vector is assumed.
const StridedOrTriangularMatrix{T} = Union{StridedMatrix{T}, LowerTriangular{T}, UnitLowerTriangular{T}, UpperTriangular{T}, UnitUpperTriangular{T}}
const AdjOrTransStridedOrTriangularMatrix{T} = Union{StridedOrTriangularMatrix{T},Adjoint{<:Any,<:StridedOrTriangularMatrix{T}},Transpose{<:Any,<:StridedOrTriangularMatrix{T}}}

for HermSym ∈ [:Hermitian, :Symmetric]
    @eval begin
        function (+)(A::SparseMatrixCSC, B::$HermSym{<:Any,<:AbstractSparseMatrix})
            C  = sparse(B)
            C .= A .+ C
        end
        function (+)(A::SparseMatrixCSC, B::$HermSym)
            C  = collect(B)
            C .= A .+ C
        end
        (+)(A::$HermSym{<:Any,<:AbstractSparseMatrix}, B::SparseMatrixCSC) = B + A
        (+)(A::$HermSym, B::SparseMatrixCSC) = B + A

        function +(A::$HermSym{<:Any, <:SparseMatrixCSC}, B::$HermSym{<:Any, <:SparseMatrixCSC})
            if A.uplo == B.uplo
                $HermSym(parent(A) + parent(B), sym_uplo(A.uplo))
            elseif A.uplo == 'U'
                $HermSym(parent(A) + permutedims(parent(B)), :U)
            else
                $HermSym(permutedims(parent(A)) + parent(B), :U)
            end
        end
    end
end


function mul!(C::StridedVecOrMat, A::AbstractSparseMatrixCSC, B::Union{StridedVector,AdjOrTransStridedOrTriangularMatrix}, α::Number, β::Number)
    size(A, 2) == size(B, 1) || throw(DimensionMismatch())
    size(A, 1) == size(C, 1) || throw(DimensionMismatch())
    size(B, 2) == size(C, 2) || throw(DimensionMismatch())
    nzv = nonzeros(A)
    rv = rowvals(A)
    if β != 1
        β != 0 ? rmul!(C, β) : fill!(C, zero(eltype(C)))
    end
    for k = 1:size(C, 2)
        @inbounds for col = 1:size(A, 2)
            αxj = B[col,k] * α
            for j = getcolptr(A)[col]:(getcolptr(A)[col + 1] - 1)
                C[rv[j], k] += nzv[j]*αxj
            end
        end
    end
    C
end
*(A::AbstractSparseMatrixCSC{TA}, x::StridedVector{Tx}) where {TA,Tx} =
    (T = promote_op(matprod, TA, Tx); mul!(similar(x, T, size(A, 1)), A, x, true, false))
*(A::AbstractSparseMatrixCSC{TA}, B::AdjOrTransStridedOrTriangularMatrix{Tx}) where {TA,Tx} =
    (T = promote_op(matprod, TA, Tx); mul!(similar(B, T, (size(A, 1), size(B, 2))), A, B, true, false))

function mul!(C::StridedVecOrMat, adjA::Adjoint{<:Any,<:AbstractSparseMatrixCSC}, B::Union{StridedVector,AdjOrTransStridedOrTriangularMatrix}, α::Number, β::Number)
    A = adjA.parent
    size(A, 2) == size(C, 1) || throw(DimensionMismatch())
    size(A, 1) == size(B, 1) || throw(DimensionMismatch())
    size(B, 2) == size(C, 2) || throw(DimensionMismatch())
    nzv = nonzeros(A)
    rv = rowvals(A)
    if β != 1
        β != 0 ? rmul!(C, β) : fill!(C, zero(eltype(C)))
    end
    for k = 1:size(C, 2)
        @inbounds for col = 1:size(A, 2)
            tmp = zero(eltype(C))
            for j = getcolptr(A)[col]:(getcolptr(A)[col + 1] - 1)
                tmp += adjoint(nzv[j])*B[rv[j],k]
            end
            C[col,k] += tmp * α
        end
    end
    C
end
*(adjA::Adjoint{<:Any,<:AbstractSparseMatrixCSC}, x::StridedVector{Tx}) where {Tx} =
    (T = promote_op(matprod, eltype(adjA), Tx); mul!(similar(x, T, size(adjA, 1)), adjA, x, true, false))
*(adjA::Adjoint{<:Any,<:AbstractSparseMatrixCSC}, B::AdjOrTransStridedOrTriangularMatrix) =
    (T = promote_op(matprod, eltype(adjA), eltype(B)); mul!(similar(B, T, (size(adjA, 1), size(B, 2))), adjA, B, true, false))

function mul!(C::StridedVecOrMat, transA::Transpose{<:Any,<:AbstractSparseMatrixCSC}, B::Union{StridedVector,AdjOrTransStridedOrTriangularMatrix}, α::Number, β::Number)
    A = transA.parent
    size(A, 2) == size(C, 1) || throw(DimensionMismatch())
    size(A, 1) == size(B, 1) || throw(DimensionMismatch())
    size(B, 2) == size(C, 2) || throw(DimensionMismatch())
    nzv = nonzeros(A)
    rv = rowvals(A)
    if β != 1
        β != 0 ? rmul!(C, β) : fill!(C, zero(eltype(C)))
    end
    for k = 1:size(C, 2)
        @inbounds for col = 1:size(A, 2)
            tmp = zero(eltype(C))
            for j = getcolptr(A)[col]:(getcolptr(A)[col + 1] - 1)
                tmp += transpose(nzv[j])*B[rv[j],k]
            end
            C[col,k] += tmp * α
        end
    end
    C
end
*(transA::Transpose{<:Any,<:AbstractSparseMatrixCSC}, x::StridedVector{Tx}) where {Tx} =
    (T = promote_op(matprod, eltype(transA), Tx); mul!(similar(x, T, size(transA, 1)), transA, x, true, false))
*(transA::Transpose{<:Any,<:AbstractSparseMatrixCSC}, B::AdjOrTransStridedOrTriangularMatrix) =
    (T = promote_op(matprod, eltype(transA), eltype(B)); mul!(similar(B, T, (size(transA, 1), size(B, 2))), transA, B, true, false))

# For compatibility with dense multiplication API. Should be deleted when dense multiplication
# API is updated to follow BLAS API.
mul!(C::StridedVecOrMat, A::AbstractSparseMatrixCSC, B::Union{StridedVector,AdjOrTransStridedOrTriangularMatrix}) =
    mul!(C, A, B, true, false)
mul!(C::StridedVecOrMat, adjA::Adjoint{<:Any,<:AbstractSparseMatrixCSC}, B::Union{StridedVector,AdjOrTransStridedOrTriangularMatrix}) =
    mul!(C, adjA, B, true, false)
mul!(C::StridedVecOrMat, transA::Transpose{<:Any,<:AbstractSparseMatrixCSC}, B::Union{StridedVector,AdjOrTransStridedOrTriangularMatrix}) =
    mul!(C, transA, B, true, false)

function mul!(C::StridedVecOrMat, X::AdjOrTransStridedOrTriangularMatrix, A::AbstractSparseMatrixCSC, α::Number, β::Number)
    mX, nX = size(X)
    nX == size(A, 1) || throw(DimensionMismatch())
    mX == size(C, 1) || throw(DimensionMismatch())
    size(A, 2) == size(C, 2) || throw(DimensionMismatch())
    rv = rowvals(A)
    nzv = nonzeros(A)
    if β != 1
        β != 0 ? rmul!(C, β) : fill!(C, zero(eltype(C)))
    end
    @inbounds for multivec_row=1:mX, col = 1:size(A, 2), k=getcolptr(A)[col]:(getcolptr(A)[col+1]-1)
        C[multivec_row, col] += α * X[multivec_row, rv[k]] * nzv[k] # perhaps suboptimal position of α?
    end
    C
end
*(X::AdjOrTransStridedOrTriangularMatrix, A::AbstractSparseMatrixCSC{TvA}) where {TvA} =
    (T = promote_op(matprod, eltype(X), TvA); mul!(similar(X, T, (size(X, 1), size(A, 2))), X, A, true, false))

function mul!(C::StridedVecOrMat, X::AdjOrTransStridedOrTriangularMatrix, adjA::Adjoint{<:Any,<:AbstractSparseMatrixCSC}, α::Number, β::Number)
    A = adjA.parent
    mX, nX = size(X)
    nX == size(A, 2) || throw(DimensionMismatch())
    mX == size(C, 1) || throw(DimensionMismatch())
    size(A, 1) == size(C, 2) || throw(DimensionMismatch())
    rv = rowvals(A)
    nzv = nonzeros(A)
    if β != 1
        β != 0 ? rmul!(C, β) : fill!(C, zero(eltype(C)))
    end
    @inbounds for col = 1:size(A, 2), k=getcolptr(A)[col]:(getcolptr(A)[col+1]-1), multivec_col=1:mX
        C[multivec_col, rv[k]] += α * X[multivec_col, col] * adjoint(nzv[k]) # perhaps suboptimal position of α?
    end
    C
end
*(X::AdjOrTransStridedOrTriangularMatrix, adjA::Adjoint{<:Any,<:AbstractSparseMatrixCSC}) =
    (T = promote_op(matprod, eltype(X), eltype(adjA)); mul!(similar(X, T, (size(X, 1), size(adjA, 2))), X, adjA, true, false))

function mul!(C::StridedVecOrMat, X::AdjOrTransStridedOrTriangularMatrix, transA::Transpose{<:Any,<:AbstractSparseMatrixCSC}, α::Number, β::Number)
    A = transA.parent
    mX, nX = size(X)
    nX == size(A, 2) || throw(DimensionMismatch())
    mX == size(C, 1) || throw(DimensionMismatch())
    size(A, 1) == size(C, 2) || throw(DimensionMismatch())
    rv = rowvals(A)
    nzv = nonzeros(A)
    if β != 1
        β != 0 ? rmul!(C, β) : fill!(C, zero(eltype(C)))
    end
    @inbounds for col = 1:size(A, 2), k=getcolptr(A)[col]:(getcolptr(A)[col+1]-1), multivec_col=1:mX
        C[multivec_col, rv[k]] += α * X[multivec_col, col] * transpose(nzv[k]) # perhaps suboptimal position of α?
    end
    C
end
*(X::AdjOrTransStridedOrTriangularMatrix, transA::Transpose{<:Any,<:AbstractSparseMatrixCSC}) =
    (T = promote_op(matprod, eltype(X), eltype(transA)); mul!(similar(X, T, (size(X, 1), size(transA, 2))), X, transA, true, false))

function (*)(D::Diagonal, A::AbstractSparseMatrixCSC)
    T = Base.promote_op(*, eltype(D), eltype(A))
    mul!(LinearAlgebra.copy_oftype(A, T), D, A)
end
function (*)(A::AbstractSparseMatrixCSC, D::Diagonal)
    T = Base.promote_op(*, eltype(D), eltype(A))
    mul!(LinearAlgebra.copy_oftype(A, T), A, D)
end

# Sparse matrix multiplication as described in [Gustavson, 1978]:
# http://dl.acm.org/citation.cfm?id=355796

*(A::AbstractSparseMatrixCSC, B::AbstractSparseMatrixCSC) = spmatmul(A,B)
*(A::AbstractSparseMatrixCSC, B::Adjoint{<:Any,<:AbstractSparseMatrixCSC}) = spmatmul(A, copy(B))
*(A::AbstractSparseMatrixCSC, B::Transpose{<:Any,<:AbstractSparseMatrixCSC}) = spmatmul(A, copy(B))
*(A::Transpose{<:Any,<:AbstractSparseMatrixCSC}, B::AbstractSparseMatrixCSC) = spmatmul(copy(A), B)
*(A::Adjoint{<:Any,<:AbstractSparseMatrixCSC}, B::AbstractSparseMatrixCSC) = spmatmul(copy(A), B)
*(A::Adjoint{<:Any,<:AbstractSparseMatrixCSC}, B::Adjoint{<:Any,<:AbstractSparseMatrixCSC}) = spmatmul(copy(A), copy(B))
*(A::Transpose{<:Any,<:AbstractSparseMatrixCSC}, B::Transpose{<:Any,<:AbstractSparseMatrixCSC}) = spmatmul(copy(A), copy(B))

# Gustavson's matrix multiplication algorithm revisited.
# The result rowval vector is already sorted by construction.
# The auxiliary Vector{Ti} xb is replaced by a Vector{Bool} of same length.
# The optional argument controlling a sorting algorithm is obsolete.
# depending on expected execution speed the sorting of the result column is
# done by a quicksort of the row indices or by a full scan of the dense result vector.
# The last is faster, if more than ≈ 1/32 of the result column is nonzero.
# TODO: extend to SparseMatrixCSCUnion to allow for SubArrays (view(X, :, r)).
function spmatmul(A::AbstractSparseMatrixCSC{TvA,TiA}, B::AbstractSparseMatrixCSC{TvB,TiB}) where {TvA,TiA,TvB,TiB}
    Tv = promote_op(matprod, TvA, TvB)
    Ti = promote_type(TiA, TiB)
    mA, nA = size(A)
    nB = size(B, 2)
    nA == size(B, 1) || throw(DimensionMismatch())

    rowvalA = rowvals(A); nzvalA = nonzeros(A)
    rowvalB = rowvals(B); nzvalB = nonzeros(B)
    nnzC = max(estimate_mulsize(mA, nnz(A), nA, nnz(B), nB) * 11 ÷ 10, mA)
    colptrC = Vector{Ti}(undef, nB+1)
    rowvalC = Vector{Ti}(undef, nnzC)
    nzvalC = Vector{Tv}(undef, nnzC)

    @inbounds begin
        ip = 1
        xb = fill(false, mA)
        for i in 1:nB
            if ip + mA - 1 > nnzC
                nnzC += max(mA, nnzC>>2)
                resize!(rowvalC, nnzC)
                resize!(nzvalC, nnzC)
            end
            colptrC[i] = ip0 = ip
            k0 = ip - 1
            for jp in nzrange(B, i)
                nzB = nzvalB[jp]
                j = rowvalB[jp]
                for kp in nzrange(A, j)
                    nzC = nzvalA[kp] * nzB
                    k = rowvalA[kp]
                    if xb[k]
                        nzvalC[k+k0] += nzC
                    else
                        nzvalC[k+k0] = nzC
                        xb[k] = true
                        rowvalC[ip] = k
                        ip += 1
                    end
                end
            end
            if ip > ip0
                if prefer_sort(ip-k0, mA)
                    # in-place sort of indices. Effort: O(nnz*ln(nnz)).
                    sort!(rowvalC, ip0, ip-1, QuickSort, Base.Order.Forward)
                    for vp = ip0:ip-1
                        k = rowvalC[vp]
                        xb[k] = false
                        nzvalC[vp] = nzvalC[k+k0]
                    end
                else
                    # scan result vector (effort O(mA))
                    for k = 1:mA
                        if xb[k]
                            xb[k] = false
                            rowvalC[ip0] = k
                            nzvalC[ip0] = nzvalC[k+k0]
                            ip0 += 1
                        end
                    end
                end
            end
        end
        colptrC[nB+1] = ip
    end

    resize!(rowvalC, ip - 1)
    resize!(nzvalC, ip - 1)

    # This modification of Gustavson algorithm has sorted row indices
    C = SparseMatrixCSC(mA, nB, colptrC, rowvalC, nzvalC)
    return C
end

# estimated number of non-zeros in matrix product
# it is assumed, that the non-zero indices are distributed independently and uniformly
# in both matrices. Over-estimation is possible if that is not the case.
function estimate_mulsize(m::Integer, nnzA::Integer, n::Integer, nnzB::Integer, k::Integer)
    p = (nnzA / (m * n)) * (nnzB / (n * k))
    p >= 1 ? m*k : p > 0 ? Int(ceil(-expm1(log1p(-p) * n)*m*k)) : 0 # (1-(1-p)^n)*m*k
end

# determine if sort! shall be used or the whole column be scanned
# based on empirical data on i7-3610QM CPU
# measuring runtimes of the scanning and sorting loops of the algorithm.
# The parameters 6 and 3 might be modified for different architectures.
prefer_sort(nz::Integer, m::Integer) = m > 6 && 3 * ilog2(nz) * nz < m

# minimal number of bits required to represent integer; ilog2(n) >= log2(n)
ilog2(n::Integer) = sizeof(n)<<3 - leading_zeros(n)

# Frobenius dot/inner product: trace(A'B)
function dot(A::AbstractSparseMatrixCSC{T1,S1},B::AbstractSparseMatrixCSC{T2,S2}) where {T1,T2,S1,S2}
    m, n = size(A)
    size(B) == (m,n) || throw(DimensionMismatch("matrices must have the same dimensions"))
    r = dot(zero(T1), zero(T2))
    @inbounds for j = 1:n
        ia = getcolptr(A)[j]; ia_nxt = getcolptr(A)[j+1]
        ib = getcolptr(B)[j]; ib_nxt = getcolptr(B)[j+1]
        if ia < ia_nxt && ib < ib_nxt
            ra = rowvals(A)[ia]; rb = rowvals(B)[ib]
            while true
                if ra < rb
                    ia += oneunit(S1)
                    ia < ia_nxt || break
                    ra = rowvals(A)[ia]
                elseif ra > rb
                    ib += oneunit(S2)
                    ib < ib_nxt || break
                    rb = rowvals(B)[ib]
                else # ra == rb
                    r += dot(nonzeros(A)[ia], nonzeros(B)[ib])
                    ia += oneunit(S1); ib += oneunit(S2)
                    ia < ia_nxt && ib < ib_nxt || break
                    ra = rowvals(A)[ia]; rb = rowvals(B)[ib]
                end
            end
        end
    end
    return r
end

function dot(x::AbstractVector, A::AbstractSparseMatrixCSC, y::AbstractVector)
    require_one_based_indexing(x, y)
    m, n = size(A)
    (length(x) == m && n == length(y)) || throw(DimensionMismatch())
    if iszero(m) || iszero(n)
        return dot(zero(eltype(x)), zero(eltype(A)), zero(eltype(y)))
    end
    T = promote_type(eltype(x), eltype(A), eltype(y))
    r = zero(T)
    rvals = getrowval(A)
    nzvals = getnzval(A)
    @inbounds for col in 1:n
        ycol = y[col]
        if !iszero(ycol)
            temp = zero(T)
            for k in nzrange(A, col)
                temp += adjoint(x[rvals[k]]) * nzvals[k]
            end
            r += temp * ycol
        end
    end
    return r
end
function dot(x::SparseVector, A::AbstractSparseMatrixCSC, y::SparseVector)
    m, n = size(A)
    length(x) == m && n == length(y) || throw(DimensionMismatch())
    if iszero(m) || iszero(n)
        return dot(zero(eltype(x)), zero(eltype(A)), zero(eltype(y)))
    end
    r = zero(promote_type(eltype(x), eltype(A), eltype(y)))
    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    ynzind = nonzeroinds(y)
    ynzval = nonzeros(y)
    Acolptr = getcolptr(A)
    Arowval = getrowval(A)
    Anzval = getnzval(A)
    for (yi, yv) in zip(ynzind, ynzval)
        A_ptr_lo = Acolptr[yi]
        A_ptr_hi = Acolptr[yi+1] - 1
        if A_ptr_lo <= A_ptr_hi
            r += _spdot(dot, 1, length(xnzind), xnzind, xnzval,
                                            A_ptr_lo, A_ptr_hi, Arowval, Anzval) * yv
        end
    end
    r
end

## triangular sparse handling

possible_adjoint(adj::Bool, a::Real) = a
possible_adjoint(adj::Bool, a) = adj ? adjoint(a) : a

const UnitDiagonalTriangular = Union{UnitUpperTriangular,UnitLowerTriangular}

const LowerTriangularPlain{T} = Union{
            LowerTriangular{T,<:SparseMatrixCSCUnion{T}},
            UnitLowerTriangular{T,<:SparseMatrixCSCUnion{T}}}

const LowerTriangularWrapped{T} = Union{
            Adjoint{T,<:UpperTriangular{T,<:SparseMatrixCSCUnion{T}}},
            Adjoint{T,<:UnitUpperTriangular{T,<:SparseMatrixCSCUnion{T}}},
            Transpose{T,<:UpperTriangular{T,<:SparseMatrixCSCUnion{T}}},
            Transpose{T,<:UnitUpperTriangular{T,<:SparseMatrixCSCUnion{T}}}} where T

const UpperTriangularPlain{T} = Union{
            UpperTriangular{T,<:SparseMatrixCSCUnion{T}},
            UnitUpperTriangular{T,<:SparseMatrixCSCUnion{T}}}

const UpperTriangularWrapped{T} = Union{
            Adjoint{T,<:LowerTriangular{T,<:SparseMatrixCSCUnion{T}}},
            Adjoint{T,<:UnitLowerTriangular{T,<:SparseMatrixCSCUnion{T}}},
            Transpose{T,<:LowerTriangular{T,<:SparseMatrixCSCUnion{T}}},
            Transpose{T,<:UnitLowerTriangular{T,<:SparseMatrixCSCUnion{T}}}} where T

const UpperTriangularSparse{T} = Union{
            UpperTriangularWrapped{T}, UpperTriangularPlain{T}} where T

const LowerTriangularSparse{T} = Union{
            LowerTriangularWrapped{T}, LowerTriangularPlain{T}} where T

const TriangularSparse{T} = Union{
            LowerTriangularSparse{T}, UpperTriangularSparse{T}} where T

## triangular multipliers
function lmul!(A::TriangularSparse{T}, B::StridedVecOrMat{T}) where T
    require_one_based_indexing(A, B)
    nrowB, ncolB  = size(B, 1), size(B, 2)
    ncol = LinearAlgebra.checksquare(A)
    if nrowB != ncol
        throw(DimensionMismatch("A is $(ncol) columns and B has $(nrowB) rows"))
    end
    _lmul!(A, B)
end

# forward multiplication for UpperTriangular SparseCSC matrices
function _lmul!(U::UpperTriangularPlain, B::StridedVecOrMat)
    A = U.data
    unit = U isa UnitDiagonalTriangular

    nrowB, ncolB  = size(B, 1), size(B, 2)
    aa = getnzval(A)
    ja = getrowval(A)
    ia = getcolptr(A)

    joff = 0
    for k = 1:ncolB
        for j = 1:nrowB
            i1 = ia[j]
            i2 = ia[j + 1] - 1
            done = unit

            bj = B[joff + j]
            for ii = i1:i2
                jai = ja[ii]
                aii = aa[ii]
                if jai < j
                    B[joff + jai] += aii * bj
                elseif jai == j
                    if !unit
                        B[joff + j] *= aii
                        done = true
                    end
                else
                    break
                end
            end
            if !done
                B[joff + j] -= B[joff + j]
            end
        end
        joff += nrowB
    end
    B
end

# backward multiplication for LowerTriangular SparseCSC matrices
function _lmul!(L::LowerTriangularPlain, B::StridedVecOrMat)
    A = L.data
    unit = L isa UnitDiagonalTriangular

    nrowB, ncolB = size(B, 1), size(B, 2)
    aa = getnzval(A)
    ja = getrowval(A)
    ia = getcolptr(A)

    joff = 0
    for k = 1:ncolB
        for j = nrowB:-1:1
            i1 = ia[j]
            i2 = ia[j + 1] - 1
            done = unit

            bj = B[joff + j]
            for ii = i2:-1:i1
                jai = ja[ii]
                aii = aa[ii]
                if jai > j
                    B[joff + jai] += aii * bj
                elseif jai == j
                    if !unit
                        B[joff + j] *= aii
                        done = true
                    end
                else
                    break
                end
            end
            if !done
                B[joff + j] -= B[joff + j]
            end
        end
        joff += nrowB
    end
    B
end

# forward multiplication for adjoint and transpose of LowerTriangular CSC matrices
function _lmul!(U::UpperTriangularWrapped, B::StridedVecOrMat)
    A = U.parent.data
    unit = U.parent isa UnitDiagonalTriangular
    adj = U isa Adjoint

    nrowB, ncolB  = size(B, 1), size(B, 2)
    aa = getnzval(A)
    ja = getrowval(A)
    ia = getcolptr(A)
    Z = zero(eltype(A))

    joff = 0
    for k = 1:ncolB
        for j = 1:nrowB
            i1 = ia[j]
            i2 = ia[j + 1] - 1
            akku = Z
            j0 = !unit ? j : j + 1

            # loop through column j of A - only structural non-zeros
            for ii = i2:-1:i1
                jai = ja[ii]
                if jai >= j0
                    aai = possible_adjoint(adj, aa[ii])
                    akku += B[joff + jai] * aai
                else
                    break
                end
            end
            if unit
                akku += B[joff + j]
            end
            B[joff + j] = akku
        end
        joff += nrowB
    end
    B
end

# backward multiplication with adjoint and transpose of LowerTriangular CSC matrices
function _lmul!(L::LowerTriangularWrapped, B::StridedVecOrMat)
    A = L.parent.data
    unit = L.parent isa UnitDiagonalTriangular
    adj = L isa Adjoint

    nrowB, ncolB  = size(B, 1), size(B, 2)
    aa = getnzval(A)
    ja = getrowval(A)
    ia = getcolptr(A)
    Z = zero(eltype(A))

    joff = 0
    for k = 1:ncolB
        for j = nrowB:-1:1
            i1 = ia[j]
            i2 = ia[j + 1] - 1
            akku = Z
            j0 = !unit ? j : j - 1

            # loop through column j of A - only structural non-zeros
            for ii = i1:i2
                jai = ja[ii]
                if jai <= j0
                    aai = possible_adjoint(adj, aa[ii])
                    akku += B[joff + jai] * aai
                else
                    break
                end
            end
            if unit
                akku += B[joff + j]
            end
            B[joff + j] = akku
        end
        joff += nrowB
    end
    B
end

## triangular solvers
function ldiv!(A::TriangularSparse{T}, B::StridedVecOrMat{T}) where T
    require_one_based_indexing(A, B)
    nrowB, ncolB  = size(B, 1), size(B, 2)
    ncol = LinearAlgebra.checksquare(A)
    if nrowB != ncol
        throw(DimensionMismatch("A is $(ncol) columns and B has $(nrowB) rows"))
    end
    _ldiv!(A, B)
end

# forward substitution for LowerTriangular CSC matrices
function _ldiv!(L::LowerTriangularPlain, B::StridedVecOrMat)
    A = L.data
    unit = L isa UnitDiagonalTriangular

    nrowB, ncolB  = size(B, 1), size(B, 2)
    aa = getnzval(A)
    ja = getrowval(A)
    ia = getcolptr(A)

    joff = 0
    for k = 1:ncolB
        for j = 1:nrowB
            i1 = ia[j]
            i2 = ia[j + 1] - 1

            # find diagonal element
            ii = searchsortedfirst(ja, j, i1, i2, Base.Order.Forward)
            jai = ii > i2 ? zero(eltype(ja)) : ja[ii]

            bj = B[joff + j]
            # check for zero pivot and divide with pivot
            if jai == j
                if !unit
                    bj /= aa[ii]
                    B[joff + j] = bj
                end
                ii += 1
            elseif !unit
                throw(LinearAlgebra.SingularException(j))
            end

            # update remaining part
            for i = ii:i2
                B[joff + ja[i]] -= bj * aa[i]
            end
        end
        joff += nrowB
    end
    B
end

# backward substitution for UpperTriangular CSC matrices
function _ldiv!(U::UpperTriangularPlain, B::StridedVecOrMat)
    A = U.data
    unit = U isa UnitDiagonalTriangular

    nrowB, ncolB = size(B, 1), size(B, 2)
    aa = getnzval(A)
    ja = getrowval(A)
    ia = getcolptr(A)

    joff = 0
    for k = 1:ncolB
        for j = nrowB:-1:1
            i1 = ia[j]
            i2 = ia[j + 1] - 1

            # find diagonal element
            ii = searchsortedlast(ja, j, i1, i2, Base.Order.Forward)
            jai = ii < i1 ? zero(eltype(ja)) : ja[ii]

            bj = B[joff + j]
            # check for zero pivot and divide with pivot
            if jai == j
                if !unit
                    bj /= aa[ii]
                    B[joff + j] = bj
                end
                ii -= 1
            elseif !unit
                throw(LinearAlgebra.SingularException(j))
            end

            # update remaining part
            for i = ii:-1:i1
                B[joff + ja[i]] -= bj * aa[i]
            end
        end
        joff += nrowB
    end
    B
end

# forward substitution for adjoint and transpose of UpperTriangular CSC matrices
function _ldiv!(L::LowerTriangularWrapped, B::StridedVecOrMat)
    A = L.parent.data
    unit = L.parent isa UnitDiagonalTriangular
    adj = L isa Adjoint

    nrowB, ncolB  = size(B, 1), size(B, 2)
    aa = getnzval(A)
    ja = getrowval(A)
    ia = getcolptr(A)

    joff = 0
    for k = 1:ncolB
        for j = 1:nrowB
            i1 = ia[j]
            i2 = ia[j + 1] - 1
            akku = B[joff + j]
            done = false

            # loop through column j of A - only structural non-zeros
            for ii = i1:i2
                jai = ja[ii]
                if jai < j
                    aai = possible_adjoint(adj, aa[ii])
                    akku -= B[joff + jai] * aai
                elseif jai == j
                    if !unit
                        aai = possible_adjoint(adj, aa[ii])
                        akku /= aai
                    end
                    done = true
                    break
                else
                    break
                end
            end
            if !done && !unit
                throw(LinearAlgebra.SingularException(j))
            end
            B[joff + j] = akku
        end
        joff += nrowB
    end
    B
end

# backward substitution for adjoint and transpose of LowerTriangular CSC matrices
function _ldiv!(U::UpperTriangularWrapped, B::StridedVecOrMat)
    A = U.parent.data
    unit = U.parent isa UnitDiagonalTriangular
    adj = U isa Adjoint

    nrowB, ncolB = size(B, 1), size(B, 2)
    aa = getnzval(A)
    ja = getrowval(A)
    ia = getcolptr(A)

    joff = 0
    for k = 1:ncolB
        for j = nrowB:-1:1
            i1 = ia[j]
            i2 = ia[j + 1] - 1
            akku = B[joff + j]
            done = false

            # loop through column j of A - only structural non-zeros
            for ii = i2:-1:i1
                jai = ja[ii]
                if jai > j
                    aai = possible_adjoint(adj, aa[ii])
                    akku -= B[joff + jai] * aai
                elseif jai == j
                    if !unit
                        aai = possible_adjoint(adj, aa[ii])
                        akku /= aai
                    end
                    done = true
                    break
                else
                    break
                end
            end
            if !done && !unit
                throw(LinearAlgebra.SingularException(j))
            end
            B[joff + j] = akku
        end
        joff += nrowB
    end
    B
end

(\)(L::TriangularSparse, B::AbstractSparseMatrixCSC) = ldiv!(L, Array(B))
(*)(L::TriangularSparse, B::AbstractSparseMatrixCSC) = lmul!(L, Array(B))

## end of triangular

# y .= A * x
mul!(y::StridedVecOrMat, A::SparseMatrixCSCSymmHerm, x::StridedVecOrMat) = mul!(y,A,x,1,0)

# C .= α * A * B + β * C
function mul!(C::StridedVecOrMat{T}, sA::SparseMatrixCSCSymmHerm, B::StridedVecOrMat,
              α::Number, β::Number) where T
    fuplo = sA.uplo == 'U' ? nzrangeup : nzrangelo
    _mul!(fuplo, C, sA, B, T(α), T(β))
end

function _mul!(nzrang::Function, C::StridedVecOrMat{T}, sA, B, α, β) where T
    A = sA.data
    n = size(A, 2)
    m = size(B, 2)
    n == size(B, 1) == size(C, 1) && m == size(C, 2) || throw(DimensionMismatch())
    rv = rowvals(A)
    nzv = nonzeros(A)
    let z = T(0), sumcol=z, αxj=z, aarc=z, α = α
        if β != 1
            β != 0 ? rmul!(C, β) : fill!(C, z)
        end
        @inbounds for k = 1:m
            for col = 1:n
                αxj = B[col,k] * α
                sumcol = z
                for j = nzrang(A, col)
                    row = rv[j]
                    aarc = nzv[j]
                    if row == col
                        sumcol += (sA isa Hermitian ? real : identity)(aarc) * B[row,k]
                    else
                        C[row,k] += aarc * αxj
                        sumcol += (sA isa Hermitian ? adjoint : transpose)(aarc) * B[row,k]
                    end
                end
                C[col,k] += α * sumcol
            end
        end
    end
    C
end

# row range up to and including diagonal
function nzrangeup(A, i)
    r = nzrange(A, i); r1 = r.start; r2 = r.stop
    rv = rowvals(A)
    @inbounds r2 < r1 || rv[r2] <= i ? r : r1:searchsortedlast(rv, i, r1, r2, Forward)
end
# row range from diagonal (included) to end
function nzrangelo(A, i)
    r = nzrange(A, i); r1 = r.start; r2 = r.stop
    rv = rowvals(A)
    @inbounds r2 < r1 || rv[r1] >= i ? r : searchsortedfirst(rv, i, r1, r2, Forward):r2
end
## end of symmetric/Hermitian

\(A::Transpose{<:Real,<:Hermitian{<:Real,<:AbstractSparseMatrixCSC}}, B::Vector) = A.parent \ B
\(A::Transpose{<:Complex,<:Hermitian{<:Complex,<:AbstractSparseMatrixCSC}}, B::Vector) = copy(A) \ B
\(A::Transpose{<:Number,<:Symmetric{<:Number,<:AbstractSparseMatrixCSC}}, B::Vector) = A.parent \ B

function rdiv!(A::AbstractSparseMatrixCSC{T}, D::Diagonal{T}) where T
    dd = D.diag
    if (k = length(dd)) ≠ size(A, 2)
        throw(DimensionMismatch("size(A, 2)=$(size(A, 2)) should be size(D, 1)=$k"))
    end
    nonz = nonzeros(A)
    @inbounds for j in 1:k
        ddj = dd[j]
        if iszero(ddj)
            throw(LinearAlgebra.SingularException(j))
        end
        for i in nzrange(A, j)
            nonz[i] /= ddj
        end
    end
    A
end

rdiv!(A::AbstractSparseMatrixCSC{T}, adjD::Adjoint{<:Any,<:Diagonal{T}}) where {T} =
    (D = adjD.parent; rdiv!(A, conj(D)))
rdiv!(A::AbstractSparseMatrixCSC{T}, transD::Transpose{<:Any,<:Diagonal{T}}) where {T} =
    (D = transD.parent; rdiv!(A, D))

function ldiv!(D::Diagonal{T}, A::AbstractSparseMatrixCSC{T}) where {T}
    # require_one_based_indexing(A)
    if size(A, 1) != length(D.diag)
        throw(DimensionMismatch("diagonal matrix is $(length(D.diag)) by $(length(D.diag)) but right hand side has $(size(A, 1)) rows"))
    end
    nonz = nonzeros(A)
    Arowval = rowvals(A)
    b = D.diag
    for i=1:length(b)
        iszero(b[i]) && throw(SingularException(i))
    end
    @inbounds for col = 1:size(A, 2), p = getcolptr(A)[col]:(getcolptr(A)[col + 1] - 1)
        nonz[p] = b[Arowval[p]] \ nonz[p]
    end
    A
end
ldiv!(adjD::Adjoint{<:Any,<:Diagonal{T}}, A::AbstractSparseMatrixCSC{T}) where {T} =
    (D = adjD.parent; ldiv!(conj(D), A))
ldiv!(transD::Transpose{<:Any,<:Diagonal{T}}, A::AbstractSparseMatrixCSC{T}) where {T} =
    (D = transD.parent; ldiv!(D, A))

## triu, tril

function triu(S::AbstractSparseMatrixCSC{Tv,Ti}, k::Integer=0) where {Tv,Ti}
    m,n = size(S)
    colptr = Vector{Ti}(undef, n+1)
    nnz = 0
    for col = 1 : min(max(k+1,1), n+1)
        colptr[col] = 1
    end
    for col = max(k+1,1) : n
        for c1 = getcolptr(S)[col] : getcolptr(S)[col+1]-1
            rowvals(S)[c1] > col - k && break
            nnz += 1
        end
        colptr[col+1] = nnz+1
    end
    rowval = Vector{Ti}(undef, nnz)
    nzval = Vector{Tv}(undef, nnz)
    A = SparseMatrixCSC(m, n, colptr, rowval, nzval)
    for col = max(k+1,1) : n
        c1 = getcolptr(S)[col]
        for c2 = getcolptr(A)[col] : getcolptr(A)[col+1]-1
            rowvals(A)[c2] = rowvals(S)[c1]
            nonzeros(A)[c2] = nonzeros(S)[c1]
            c1 += 1
        end
    end
    A
end

function tril(S::AbstractSparseMatrixCSC{Tv,Ti}, k::Integer=0) where {Tv,Ti}
    m,n = size(S)
    colptr = Vector{Ti}(undef, n+1)
    nnz = 0
    colptr[1] = 1
    for col = 1 : min(n, m+k)
        l1 = getcolptr(S)[col+1]-1
        for c1 = 0 : (l1 - getcolptr(S)[col])
            rowvals(S)[l1 - c1] < col - k && break
            nnz += 1
        end
        colptr[col+1] = nnz+1
    end
    for col = max(min(n, m+k)+2,1) : n+1
        colptr[col] = nnz+1
    end
    rowval = Vector{Ti}(undef, nnz)
    nzval = Vector{Tv}(undef, nnz)
    A = SparseMatrixCSC(m, n, colptr, rowval, nzval)
    for col = 1 : min(n, m+k)
        c1 = getcolptr(S)[col+1]-1
        l2 = getcolptr(A)[col+1]-1
        for c2 = 0 : l2 - getcolptr(A)[col]
            rowvals(A)[l2 - c2] = rowvals(S)[c1]
            nonzeros(A)[l2 - c2] = nonzeros(S)[c1]
            c1 -= 1
        end
    end
    A
end

## diff

function sparse_diff1(S::AbstractSparseMatrixCSC{Tv,Ti}) where {Tv,Ti}
    m,n = size(S)
    m > 1 || return SparseMatrixCSC(0, n, fill(one(Ti),n+1), Ti[], Tv[])
    colptr = Vector{Ti}(undef, n+1)
    numnz = 2 * nnz(S) # upper bound; will shrink later
    rowval = Vector{Ti}(undef, numnz)
    nzval = Vector{Tv}(undef, numnz)
    numnz = 0
    colptr[1] = 1
    for col = 1 : n
        last_row = 0
        last_val = 0
        for k = getcolptr(S)[col] : getcolptr(S)[col+1]-1
            row = rowvals(S)[k]
            val = nonzeros(S)[k]
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

function sparse_diff2(a::AbstractSparseMatrixCSC{Tv,Ti}) where {Tv,Ti}
    m,n = size(a)
    colptr = Vector{Ti}(undef, max(n,1))
    numnz = 2 * nnz(a) # upper bound; will shrink later
    rowval = Vector{Ti}(undef, numnz)
    nzval = Vector{Tv}(undef, numnz)

    z = zero(Tv)

    colptr_a = getcolptr(a)
    rowval_a = rowvals(a)
    nzval_a = nonzeros(a)

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

diff(a::AbstractSparseMatrixCSC; dims::Integer) = dims==1 ? sparse_diff1(a) : sparse_diff2(a)

## norm and rank
norm(A::AbstractSparseMatrixCSC, p::Real=2) = norm(view(nonzeros(A), 1:nnz(A)), p)

function opnorm(A::AbstractSparseMatrixCSC, p::Real=2)
    m, n = size(A)
    if m == 0 || n == 0 || isempty(A)
        return float(real(zero(eltype(A))))
    elseif m == 1
        if p == 1
            return norm(nzvalview(A), Inf)
        elseif p == 2
            return norm(nzvalview(A), 2)
        elseif p == Inf
            return norm(nzvalview(A), 1)
        end
    elseif n == 1 && p in (1, 2, Inf)
        return norm(nzvalview(A), p)
    else
        Tnorm = typeof(float(real(zero(eltype(A)))))
        Tsum = promote_type(Float64,Tnorm)
        if p==1
            nA::Tsum = 0
            for j=1:n
                colSum::Tsum = 0
                for i = getcolptr(A)[j]:getcolptr(A)[j+1]-1
                    colSum += abs(nonzeros(A)[i])
                end
                nA = max(nA, colSum)
            end
            return convert(Tnorm, nA)
        elseif p==2
            throw(ArgumentError("2-norm not yet implemented for sparse matrices. Try opnorm(Array(A)) or opnorm(A, p) where p=1 or Inf."))
        elseif p==Inf
            rowSum = zeros(Tsum,m)
            for i=1:length(nonzeros(A))
                rowSum[rowvals(A)[i]] += abs(nonzeros(A)[i])
            end
            return convert(Tnorm, maximum(rowSum))
        end
    end
    throw(ArgumentError("invalid operator p-norm p=$p. Valid: 1, Inf"))
end

# TODO rank

# cond
function cond(A::AbstractSparseMatrixCSC, p::Real=2)
    if p == 1
        normAinv = opnormestinv(A)
        normA = opnorm(A, 1)
        return normA * normAinv
    elseif p == Inf
        normAinv = opnormestinv(copy(A'))
        normA = opnorm(A, Inf)
        return normA * normAinv
    elseif p == 2
        throw(ArgumentError("2-norm condition number is not implemented for sparse matrices, try cond(Array(A), 2) instead"))
    else
        throw(ArgumentError("second argument must be either 1 or Inf, got $p"))
    end
end

function opnormestinv(A::AbstractSparseMatrixCSC{T}, t::Integer = min(2,maximum(size(A)))) where T
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
    ind = Vector{Int64}(undef, n)
    ind_hist = Vector{Int64}(undef, maxiter * t)

    Ti = typeof(float(zero(T)))

    S = zeros(T <: Real ? Int : Ti, n, t)

    function _any_abs_eq(v,n::Int)
        for vv in v
            if abs(vv)==n
                return true
            end
        end
        return false
    end

    # Generate the block matrix
    X = Matrix{Ti}(undef, n, t)
    X[1:n,1] .= 1
    for j = 2:t
        while true
            rand!(view(X,1:n,j), (-1, 1))
            yaux = X[1:n,j]' * X[1:n,1:j-1]
            if !_any_abs_eq(yaux,n)
                break
            end
        end
    end
    rmul!(X, inv(n))

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
                        rand!(view(S,1:n,j), (-1, 1))
                    else
                        break
                    end
                end
            end
        end

        # Use the conjugate transpose
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

## kron

# sparse matrix ⊗ sparse matrix
function kron(A::AbstractSparseMatrixCSC{T1,S1}, B::AbstractSparseMatrixCSC{T2,S2}) where {T1,S1,T2,S2}
    nnzC = nnz(A)*nnz(B)
    mA, nA = size(A); mB, nB = size(B)
    mC, nC = mA*mB, nA*nB
    colptrC = Vector{promote_type(S1,S2)}(undef, nC+1)
    rowvalC = Vector{promote_type(S1,S2)}(undef, nnzC)
    nzvalC  = Vector{typeof(one(T1)*one(T2))}(undef, nnzC)
    colptrC[1] = 1
    col = 1
    @inbounds for j = 1:nA
        startA = getcolptr(A)[j]
        stopA = getcolptr(A)[j+1] - 1
        lA = stopA - startA + 1
        for i = 1:nB
            startB = getcolptr(B)[i]
            stopB = getcolptr(B)[i+1] - 1
            lB = stopB - startB + 1
            ptr_range = (1:lB) .+ (colptrC[col]-1)
            colptrC[col+1] = colptrC[col] + lA*lB
            col += 1
            for ptrA = startA : stopA
                ptrB = startB
                for ptr = ptr_range
                    rowvalC[ptr] = (rowvals(A)[ptrA]-1)*mB + rowvals(B)[ptrB]
                    nzvalC[ptr] = nonzeros(A)[ptrA] * nonzeros(B)[ptrB]
                    ptrB += 1
                end
                ptr_range = ptr_range .+ lB
            end
        end
    end
    return SparseMatrixCSC(mC, nC, colptrC, rowvalC, nzvalC)
end

# sparse vector ⊗ sparse vector
function kron(x::SparseVector{T1,S1}, y::SparseVector{T2,S2}) where {T1,S1,T2,S2}
    nnzx = nnz(x); nnzy = nnz(y)
    nnzz = nnzx*nnzy # number of nonzeros in new vector
    nzind = Vector{promote_type(S1,S2)}(undef, nnzz) # the indices of nonzeros
    nzval = Vector{typeof(one(T1)*one(T2))}(undef, nnzz) # the values of nonzeros
    @inbounds for i = 1:nnzx, j = 1:nnzy
        this_ind = (i-1)*nnzy+j
        nzind[this_ind] = (nonzeroinds(x)[i]-1)*length(y::SparseVector) + nonzeroinds(y)[j]
        nzval[this_ind] = nonzeros(x)[i] * nonzeros(y)[j]
    end
    return SparseVector(length(x::SparseVector)*length(y::SparseVector), nzind, nzval)
end

# sparse matrix ⊗ sparse vector & vice versa
kron(A::AbstractSparseMatrixCSC, x::SparseVector) = kron(A, SparseMatrixCSC(x))
kron(x::SparseVector, A::AbstractSparseMatrixCSC) = kron(SparseMatrixCSC(x), A)

# sparse vec/mat ⊗ vec/mat and vice versa
kron(A::Union{SparseVector,AbstractSparseMatrixCSC}, B::VecOrMat) = kron(A, sparse(B))
kron(A::VecOrMat, B::Union{SparseVector,AbstractSparseMatrixCSC}) = kron(sparse(A), B)

# sparse vec/mat ⊗ Diagonal and vice versa
kron(A::Diagonal{T}, B::Union{SparseVector{S}, AbstractSparseMatrixCSC{S}}) where {T<:Number, S<:Number} = kron(sparse(A), B)
kron(A::Union{SparseVector{T}, AbstractSparseMatrixCSC{T}}, B::Diagonal{S}) where {T<:Number, S<:Number} = kron(A, sparse(B))

# sparse outer product
kron(A::SparseVectorUnion, B::AdjOrTransSparseVectorUnion) = A .* B

## det, inv, cond

inv(A::AbstractSparseMatrixCSC) = error("The inverse of a sparse matrix can often be dense and can cause the computer to run out of memory. If you are sure you have enough memory, please convert your matrix to a dense matrix.")

# TODO

## scale methods

# Copy colptr and rowval from one sparse matrix to another
function copyinds!(C::AbstractSparseMatrixCSC, A::AbstractSparseMatrixCSC)
    if getcolptr(C) !== getcolptr(A)
        resize!(getcolptr(C), length(getcolptr(A)))
        copyto!(getcolptr(C), getcolptr(A))
    end
    if rowvals(C) !== rowvals(A)
        resize!(rowvals(C), length(rowvals(A)))
        copyto!(rowvals(C), rowvals(A))
    end
end

# multiply by diagonal matrix as vector
function mul!(C::AbstractSparseMatrixCSC, A::AbstractSparseMatrixCSC, D::Diagonal)
    m, n = size(A)
    b    = D.diag
    (n==length(b) && size(A)==size(C)) || throw(DimensionMismatch())
    copyinds!(C, A)
    Cnzval = nonzeros(C)
    Anzval = nonzeros(A)
    resize!(Cnzval, length(Anzval))
    for col = 1:n, p = getcolptr(A)[col]:(getcolptr(A)[col+1]-1)
        @inbounds Cnzval[p] = Anzval[p] * b[col]
    end
    C
end

function mul!(C::AbstractSparseMatrixCSC, D::Diagonal, A::AbstractSparseMatrixCSC)
    m, n = size(A)
    b    = D.diag
    (m==length(b) && size(A)==size(C)) || throw(DimensionMismatch())
    copyinds!(C, A)
    Cnzval = nonzeros(C)
    Anzval = nonzeros(A)
    Arowval = rowvals(A)
    resize!(Cnzval, length(Anzval))
    for col = 1:n, p = getcolptr(A)[col]:(getcolptr(A)[col+1]-1)
        @inbounds Cnzval[p] = b[Arowval[p]] * Anzval[p]
    end
    C
end

function mul!(C::AbstractSparseMatrixCSC, A::AbstractSparseMatrixCSC, b::Number)
    size(A)==size(C) || throw(DimensionMismatch())
    copyinds!(C, A)
    resize!(nonzeros(C), length(nonzeros(A)))
    mul!(nonzeros(C), nonzeros(A), b)
    C
end

function mul!(C::AbstractSparseMatrixCSC, b::Number, A::AbstractSparseMatrixCSC)
    size(A)==size(C) || throw(DimensionMismatch())
    copyinds!(C, A)
    resize!(nonzeros(C), length(nonzeros(A)))
    mul!(nonzeros(C), b, nonzeros(A))
    C
end

function rmul!(A::AbstractSparseMatrixCSC, b::Number)
    rmul!(nonzeros(A), b)
    return A
end

function lmul!(b::Number, A::AbstractSparseMatrixCSC)
    lmul!(b, nonzeros(A))
    return A
end

function rmul!(A::AbstractSparseMatrixCSC, D::Diagonal)
    m, n = size(A)
    (n == size(D, 1)) || throw(DimensionMismatch())
    Anzval = nonzeros(A)
    @inbounds for col = 1:n, p = getcolptr(A)[col]:(getcolptr(A)[col + 1] - 1)
         Anzval[p] = Anzval[p] * D.diag[col]
    end
    return A
end

function lmul!(D::Diagonal, A::AbstractSparseMatrixCSC)
    m, n = size(A)
    (m == size(D, 2)) || throw(DimensionMismatch())
    Anzval = nonzeros(A)
    Arowval = rowvals(A)
    @inbounds for col = 1:n, p = getcolptr(A)[col]:(getcolptr(A)[col + 1] - 1)
        Anzval[p] = D.diag[Arowval[p]] * Anzval[p]
    end
    return A
end

function \(A::AbstractSparseMatrixCSC, B::AbstractVecOrMat)
    require_one_based_indexing(A, B)
    m, n = size(A)
    if m == n
        if istril(A)
            if istriu(A)
                return \(Diagonal(Vector(diag(A))), B)
            else
                return \(LowerTriangular(A), B)
            end
        elseif istriu(A)
            return \(UpperTriangular(A), B)
        end
        if ishermitian(A)
            return \(Hermitian(A), B)
        end
        return \(lu(A), B)
    else
        return \(qr(A), B)
    end
end
for (xformtype, xformop) in ((:Adjoint, :adjoint), (:Transpose, :transpose))
    @eval begin
        function \(xformA::($xformtype){<:Any,<:AbstractSparseMatrixCSC}, B::AbstractVecOrMat)
            A = xformA.parent
            require_one_based_indexing(A, B)
            m, n = size(A)
            if m == n
                if istril(A)
                    if istriu(A)
                        return \($xformop(Diagonal(Vector(diag(A)))), B)
                    else
                        return \($xformop(LowerTriangular(A)), B)
                    end
                elseif istriu(A)
                    return \($xformop(UpperTriangular(A)), B)
                end
                if ishermitian(A)
                    return \($xformop(Hermitian(A)), B)
                end
                return \($xformop(lu(A)), B)
            else
                return \($xformop(qr(A)), B)
            end
        end
    end
end

function factorize(A::AbstractSparseMatrixCSC)
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
        return lu(A)
    else
        return qr(A)
    end
end

# function factorize(A::Symmetric{Float64,AbstractSparseMatrixCSC{Float64,Ti}}) where Ti
#     F = cholesky(A)
#     if LinearAlgebra.issuccess(F)
#         return F
#     else
#         ldlt!(F, A)
#         return F
#     end
# end
function factorize(A::LinearAlgebra.RealHermSymComplexHerm{Float64,<:AbstractSparseMatrixCSC})
    F = cholesky(A; check = false)
    if LinearAlgebra.issuccess(F)
        return F
    else
        ldlt!(F, A)
        return F
    end
end

eigen(A::AbstractSparseMatrixCSC) =
    error("eigen(A) not supported for sparse matrices. Use for example eigs(A) from the Arpack package instead.")
