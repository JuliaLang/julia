# These functions are based on C functions in the CSparse library by Tim Davis
# CSparse can be downloaded from http://www.cise.ufl.edu/research/sparse/CSparse/CSparse.tar.gz
# CSparse is Copyright (c) 2006-2007, Timothy A. Davis and released under
# Lesser GNU Public License, version 2.1 or later.  A copy of the license can be
# downloaded from http://www.gnu.org/licenses/lgpl-2.1.html

# Because these functions are based on code covered by LGPL-2.1+ the same license
# must apply to the code in this file which is
# Copyright (c) 2013 Douglas M. Bates, Viral Shah and other contributors

# Compute the elimination tree of A using triu(A) returning the parent vector.
# A root node is indicated by 0. This tree may actually be a forest in that
# there may be more than one root, indicating complete separability.
# A trivial example is speye(n, n) in which every node is a root.
function etree(A::SparseMatrixCSC, postorder::Bool)
    m,n = size(A); Ap = A.colptr; Ai = A.rowval; T = eltype(Ai)
    parent = zeros(T, n); ancestor = zeros(T, n)
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
    head = zeros(T,n)                   # empty linked lists
    next = zeros(T,n)
    for j in n:-1:1                      # traverse in reverse order
        if (parent[j] == 0) continue end # j is a root
        next[j] = head[parent[j]]        # add j to list of its parent
        head[parent[j]] = j
    end
    stack = T[]
    sizehint(stack, n)
    post = zeros(T,n)
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

# find nonzero pattern of Cholesky L(k,1:k-1) using etree and triu(A(:,k))
# based on cs_ereach p. 43, "Direct Methods for Sparse Linear Systems"
function ereach{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, k::Integer, parent::Vector{Ti})
    m,n = size(A); Ap = A.colptr; Ai = A.rowval
    s = Ti[]; sizehint(s, n)            # to be used as a stack
    visited = falses(n)
    visited[k] = true
    for p in Ap[k]:(Ap[k+1] - 1)
        i = Ai[p]                # A(i,k) is nonzero
        if i > k continue end    # only use upper triangular part of A
        while !visited[i]        # traverse up etree
            push!(s,i)           # L(k,i) is nonzero
            visited[i] = true
            i = parent[i]
        end
    end
    s
end

# based on cs_permute p. 21, "Direct Methods for Sparse Linear Systems"
function csc_permute{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, pinv::Vector{Ti}, q::Vector{Ti})
    m,n = size(A); Ap = A.colptr; Ai = A.rowval; Ax = A.nzval
    if length(pinv) != m || length(q) !=  n 
        error("Dimension mismatch, size(A) = $(size(A)), length(pinv) = $(length(pinv)) and length(q) = $(length(q))")
    end
    if !isperm(pinv) || !isperm(q) error("Both pinv and q must be permutations") end
    C = copy(A); Cp = C.colptr; Ci = C.rowval; Cx = C.nzval
    nz = zero(Ti)
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
    (C.').'                    # double transpose to order the columns
end

# based on cs_symperm p. 21, "Direct Methods for Sparse Linear Systems"
# form A[p,p] for a symmetric A stored in the upper triangle
function csc_symperm{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, pinv::Vector{Ti})
    m,n = size(A); Ap = A.colptr; Ai = A.rowval; Ax = A.nzval
    if m != n || length(pinv) != m
        error("A must be square, size(A) = $(size(A)), length(pinv) = $(length(pinv))")
    end
    if !isperm(pinv) error("Both pinv must be a permutation") end
    C = copy(A); Cp = C.colptr; Ci = C.rowval; Cx = C.nzval
    w = Array(Ti,n)
    for j in 1:n                   # count entries in each column of C
        j2 = pinv[j]
        for p = Ap[j]:(Ap[j+1]-1)
            i = Ai[p]
            if i > j continue end
            w[pinv[i]] += one(Ti)
        end
    end
    Cp[:] = cumsum(vcat(one(Ti),w))
    for j in 1:n                   # count entries in each column of C
        j2 = pinv[j]
        for p = Ap[j]:(Ap[j+1]-1)
            i = Ai[p]
            if i > j continue end
            i2 = pinv[i]
            q = w[max(i2,j2)]
            Ci[q] = min(i2,j2)
            Cx[q] = Ax[p]
        end
    end
    (C.').'                    # double transpose to order the columns
end
