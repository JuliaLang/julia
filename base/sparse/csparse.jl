# These functions are based on C functions in the CSparse library by Tim Davis.
# These are pure Julia implementations, and do not link to the CSparse library.
# CSparse can be downloaded from http://www.cise.ufl.edu/research/sparse/CSparse/CSparse.tar.gz
# CSparse is Copyright (c) 2006-2007, Timothy A. Davis and released under
# Lesser GNU Public License, version 2.1 or later.  A copy of the license can be
# downloaded from http://www.gnu.org/licenses/lgpl-2.1.html

# Because these functions are based on code covered by LGPL-2.1+ the same license
# must apply to the code in this file which is
# Copyright (c) 2013-2014 Viral Shah, Douglas Bates and other contributors

# Based on Direct Methods for Sparse Linear Systems, T. A. Davis, SIAM, Philadelphia, Sept. 2006.
# Section 2.4: Triplet form
# http://www.cise.ufl.edu/research/sparse/CSparse/

"""
    sparse(I,J,V,[m,n,combine])

Create a sparse matrix `S` of dimensions `m x n` such that `S[I[k], J[k]] = V[k]`.
The `combine` function is used to combine duplicates. If `m` and `n` are not
specified, they are set to `maximum(I)` and `maximum(J)` respectively. If the
`combine` function is not supplied, duplicates are added by default. All elements
of `I` must satisfy `1 <= I[k] <= m`, and all elements of `J` must satisfy `1 <= J[k] <= n`.
"""
function sparse{Tv,Ti<:Integer}(I::AbstractVector{Ti},
                                J::AbstractVector{Ti},
                                V::AbstractVector{Tv},
                                nrow::Integer, ncol::Integer,
                                combine::Union{Function,Base.Func})

    N = length(I)
    if N != length(J) || N != length(V)
        throw(ArgumentError("triplet I,J,V vectors must be the same length"))
    end
    if N == 0
        return spzeros(eltype(V), Ti, nrow, ncol)
    end

    # Work array
    Wj = Array(Ti, max(nrow,ncol)+1)

    # Allocate sparse matrix data structure
    # Count entries in each row
    Rnz = zeros(Ti, nrow+1)
    Rnz[1] = 1
    nz = 0
    for k=1:N
        iind = I[k]
        iind > 0 || throw(ArgumentError("all I index values must be > 0"))
        iind <= nrow || throw(ArgumentError("all I index values must be ≤ the number of rows"))
        if V[k] != 0
            Rnz[iind+1] += 1
            nz += 1
        end
    end
    Rp = cumsum(Rnz)
    Ri = Array(Ti, nz)
    Rx = Array(Tv, nz)

    # Construct row form
    # place triplet (i,j,x) in column i of R
    # Use work array for temporary row pointers
    @simd for i=1:nrow; @inbounds Wj[i] = Rp[i]; end

    @inbounds for k=1:N
        iind = I[k]
        jind = J[k]
        jind > 0 || throw(ArgumentError("all J index values must be > 0"))
        jind <= ncol || throw(ArgumentError("all J index values must be ≤ the number of columns"))
        p = Wj[iind]
        Vk = V[k]
        if Vk != 0
            Wj[iind] += 1
            Rx[p] = Vk
            Ri[p] = jind
        end
    end

    # Reset work array for use in counting duplicates
    @simd for j=1:ncol; @inbounds Wj[j] = 0; end

    # Sum up duplicates and squeeze
    anz = 0
    @inbounds for i=1:nrow
        p1 = Rp[i]
        p2 = Rp[i+1] - 1
        pdest = p1

        for p = p1:p2
            j = Ri[p]
            pj = Wj[j]
            if pj >= p1
                Rx[pj] = combine(Rx[pj], Rx[p])
            else
                Wj[j] = pdest
                if pdest != p
                    Ri[pdest] = j
                    Rx[pdest] = Rx[p]
                end
                pdest += one(Ti)
            end
        end

        Rnz[i] = pdest - p1
        anz += (pdest - p1)
    end

    # Transpose from row format to get the CSC format
    RiT = Array(Ti, anz)
    RxT = Array(Tv, anz)

    # Reset work array to build the final colptr
    Wj[1] = 1
    @simd for i=2:(ncol+1); @inbounds Wj[i] = 0; end
    @inbounds for j = 1:nrow
        p1 = Rp[j]
        p2 = p1 + Rnz[j] - 1
        for p = p1:p2
            Wj[Ri[p]+1] += 1
        end
    end
    RpT = cumsum(Wj[1:(ncol+1)])

    # Transpose
    @simd for i=1:length(RpT); @inbounds Wj[i] = RpT[i]; end
    @inbounds for j = 1:nrow
        p1 = Rp[j]
        p2 = p1 + Rnz[j] - 1
        for p = p1:p2
            ind = Ri[p]
            q = Wj[ind]
            Wj[ind] += 1
            RiT[q] = j
            RxT[q] = Rx[p]
        end
    end

    return SparseMatrixCSC(nrow, ncol, RpT, RiT, RxT)
end

## Transpose and apply f

# Based on Direct Methods for Sparse Linear Systems, T. A. Davis, SIAM, Philadelphia, Sept. 2006.
# Section 2.5: Transpose
# http://www.cise.ufl.edu/research/sparse/CSparse/
function ftranspose!{Tv,Ti}(T::SparseMatrixCSC{Tv,Ti}, S::SparseMatrixCSC{Tv,Ti}, f)
    (mS, nS) = size(S)
    nnzS = nnz(S)
    colptr_S = S.colptr
    rowval_S = S.rowval
    nzval_S = S.nzval

    (mT, nT) = size(T)
    colptr_T = T.colptr
    rowval_T = T.rowval
    nzval_T = T.nzval

    fill!(colptr_T, 0)
    colptr_T[1] = 1
    for i=1:nnzS
        @inbounds colptr_T[rowval_S[i]+1] += 1
    end
    cumsum!(colptr_T, colptr_T)

    w = copy(colptr_T)
    @inbounds for j = 1:nS, p = colptr_S[j]:(colptr_S[j+1]-1)
        ind = rowval_S[p]
        q = w[ind]
        w[ind] += 1
        rowval_T[q] = j
        nzval_T[q] = f(nzval_S[p])
    end

    return T
end

function ftranspose{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, f)
    (nT, mT) = size(S)
    nnzS = nnz(S)
    colptr_T = Array(Ti, nT+1)
    rowval_T = Array(Ti, nnzS)
    nzval_T = Array(Tv, nnzS)

    T = SparseMatrixCSC(mT, nT, colptr_T, rowval_T, nzval_T)
    return ftranspose!(T, S, f)
end

function transpose!{Tv,Ti}(T::SparseMatrixCSC{Tv,Ti}, S::SparseMatrixCSC{Tv,Ti})
    ftranspose!(T, S, IdFun())
end

function transpose{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti})
    ftranspose(S, IdFun())
end

function ctranspose!{Tv,Ti}(T::SparseMatrixCSC{Tv,Ti}, S::SparseMatrixCSC{Tv,Ti})
    ftranspose!(T, S, ConjFun())
end

function ctranspose{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti})
    ftranspose(S, ConjFun())
end

# Compute the elimination tree of A using triu(A) returning the parent vector.
# A root node is indicated by 0. This tree may actually be a forest in that
# there may be more than one root, indicating complete separability.
# A trivial example is speye(n, n) in which every node is a root.

"""
    etree(A[, post])

Compute the elimination tree of a symmetric sparse matrix `A` from `triu(A)`
and, optionally, its post-ordering permutation.
"""
function etree{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, postorder::Bool)
    m,n = size(A)
    Ap = A.colptr
    Ai = A.rowval
    parent = zeros(Ti, n)
    ancestor = zeros(Ti, n)
    for k in 1:n, p in Ap[k]:(Ap[k+1] - 1)
        i = Ai[p]
        while i != 0 && i < k
            inext = ancestor[i] # inext = ancestor of i
            ancestor[i] = k     # path compression
            if (inext == 0) parent[i] = k end # no anc., parent is k
            i = inext
        end
    end
    if !postorder return parent end
    head = zeros(Ti,n)                   # empty linked lists
    next = zeros(Ti,n)
    for j in n:-1:1                      # traverse in reverse order
        if (parent[j] == 0); continue; end # j is a root
        next[j] = head[parent[j]]        # add j to list of its parent
        head[parent[j]] = j
    end
    stack = Ti[]
    sizehint!(stack, n)
    post = zeros(Ti,n)
    k = 1
    for j in 1:n
        if (parent[j] != 0) continue end # skip j if it is not a root
        push!(stack, j)                  # place j on the stack
        while (length(stack) > 0)        # while (stack is not empty)
            p = stack[end]               # p = top of stack
            i = head[p]                  # i = youngest child of p
            if (i == 0)
                pop!(stack)
                post[k] = p       # node p is the kth postordered node
                k += 1
            else
            head[p] = next[i]           # remove i from children of p
            push!(stack, i)
            end
        end
    end
    parent, post
end

etree(A::SparseMatrixCSC) = etree(A, false)

# find nonzero pattern of Cholesky L[k,1:k-1] using etree and triu(A[:,k])
# based on cs_ereach p. 43, "Direct Methods for Sparse Linear Systems"
function ereach{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, k::Integer, parent::Vector{Ti})
    m,n = size(A); Ap = A.colptr; Ai = A.rowval
    s = Ti[]; sizehint!(s, n)            # to be used as a stack
    visited = falses(n)
    visited[k] = true
    for p in Ap[k]:(Ap[k+1] - 1)
        i = Ai[p]                # A[i,k] is nonzero
        if i > k continue end    # only use upper triangular part of A
        while !visited[i]        # traverse up etree
            push!(s,i)           # L[k,i] is nonzero
            visited[i] = true
            i = parent[i]
        end
    end
    s
end

# based on cs_permute p. 21, "Direct Methods for Sparse Linear Systems"
function csc_permute{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, pinv::Vector{Ti}, q::Vector{Ti})
    m, n = size(A)
    Ap = A.colptr
    Ai = A.rowval
    Ax = A.nzval
    lpinv = length(pinv)
    if m != lpinv
        throw(DimensionMismatch(
            "the number of rows of sparse matrix A must equal the length of pinv, $m != $lpinv"))
    end
    lq = length(q)
    if n != lq
        throw(DimensionMismatch(
            "the number of columns of sparse matrix A must equal the length of q, $n != $lq"))
    end
    if !isperm(pinv) || !isperm(q)
        throw(ArgumentError("both pinv and q must be permutations"))
    end
    C = copy(A); Cp = C.colptr; Ci = C.rowval; Cx = C.nzval
    nz = one(Ti)
    for k in 1:n
        Cp[k] = nz
        j = q[k]
        for t = Ap[j]:(Ap[j+1]-1)
            Cx[nz] = Ax[t]
            Ci[nz] = pinv[Ai[t]]
            nz += one(Ti)
        end
    end
    Cp[n + 1] = nz
    (C.').' # double transpose to order the columns
end


# based on cs_symperm p. 21, "Direct Methods for Sparse Linear Systems"
# form A[p,p] for a symmetric A stored in the upper triangle
doc"""
    symperm(A, p)

Return the symmetric permutation of `A`, which is `A[p,p]`. `A` should be
symmetric, sparse, and only contain nonzeros in the upper triangular part of the
matrix is stored. This algorithm ignores the lower triangular part of the
matrix. Only the upper triangular part of the result is returned.
"""
function symperm{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, pinv::Vector{Ti})
    m, n = size(A)
    if m != n
        throw(DimensionMismatch("sparse matrix A must be square"))
    end
    Ap = A.colptr
    Ai = A.rowval
    Ax = A.nzval
    if !isperm(pinv)
        throw(ArgumentError("pinv must be a permutation"))
    end
    lpinv = length(pinv)
    if n != lpinv
        throw(DimensionMismatch(
            "dimensions of sparse matrix A must equal the length of pinv, $((m,n)) != $lpinv"))
    end
    C = copy(A); Cp = C.colptr; Ci = C.rowval; Cx = C.nzval
    w = zeros(Ti,n)
    for j in 1:n  # count entries in each column of C
        j2 = pinv[j]
        for p in Ap[j]:(Ap[j+1]-1)
            (i = Ai[p]) > j || (w[max(pinv[i],j2)] += one(Ti))
        end
    end
    Cp[:] = cumsum(vcat(one(Ti),w))
    copy!(w,Cp[1:n]) # needed to be consistent with cs_cumsum
    for j in 1:n
        j2 = pinv[j]
        for p = Ap[j]:(Ap[j+1]-1)
            (i = Ai[p]) > j && continue
            i2 = pinv[i]
            ind = max(i2,j2)
            Ci[q = w[ind]] = min(i2,j2)
            w[ind] += 1
            Cx[q] = Ax[p]
        end
    end
    (C.').' # double transpose to order the columns
end

# Based on Direct Methods for Sparse Linear Systems, T. A. Davis, SIAM, Philadelphia, Sept. 2006.
# Section 2.7: Removing entries from a matrix
# http://www.cise.ufl.edu/research/sparse/CSparse/
function fkeep!{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, f, other)
    nzorig = nnz(A)
    nz = 1
    colptr = A.colptr
    rowval = A.rowval
    nzval = A.nzval
    @inbounds for j = 1:A.n
        p = colptr[j]                 # record current position
        colptr[j] = nz                # set new position
        while p < colptr[j+1]
            if f(rowval[p], j, nzval[p], other)
                nzval[nz] = nzval[p]
                rowval[nz] = rowval[p]
                nz += 1
            end
            p += 1
        end
    end
    colptr[A.n + 1] = nz
    nz -= 1
    if nz < nzorig
        resize!(nzval, nz)
        resize!(rowval, nz)
    end
    A
end


immutable DropTolFun <: Func{4} end
call(::DropTolFun, i,j,x,other) = abs(x)>other
immutable DropZerosFun <: Func{4} end
call(::DropZerosFun, i,j,x,other) = x!=0
immutable TriuFun <: Func{4} end
call(::TriuFun, i,j,x,other) = j>=i + other
immutable TrilFun <: Func{4} end
call(::TrilFun, i,j,x,other) = i>=j - other

droptol!(A::SparseMatrixCSC, tol) = fkeep!(A, DropTolFun(), tol)
dropzeros!(A::SparseMatrixCSC) = fkeep!(A, DropZerosFun(), nothing)
dropzeros(A::SparseMatrixCSC) = dropzeros!(copy(A))

function triu!(A::SparseMatrixCSC, k::Integer=0)
    m,n = size(A)
    if (k > 0 && k > n) || (k < 0 && -k > m)
        throw(BoundsError())
    end
    fkeep!(A, TriuFun(), k)
end

function tril!(A::SparseMatrixCSC, k::Integer=0)
    m,n = size(A)
    if (k > 0 && k > n) || (k < 0 && -k > m)
        throw(BoundsError())
    end
    fkeep!(A, TrilFun(), k)
end
