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
        while (!isempty(stack))        # while (stack is not empty)
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
"""
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