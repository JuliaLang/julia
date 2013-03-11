abstract AbstractSparseMatrix{Tv,Ti} <: AbstractMatrix{Tv}

issparse(A::AbstractArray) = false
issparse(S::AbstractSparseMatrix) = true

eltype{Tv}(S::AbstractSparseMatrix{Tv}) = Tv
indtype{Tv,Ti}(S::AbstractSparseMatrix{Tv,Ti}) = Ti

# Compressed sparse columns data structure
# Assumes that no zeros are stored in the data structure
# Assumes that row values in rowval for each colum are sorted 
#      issorted(rowval[colptr[i]]:rowval[colptr[i+1]]-1) == true
type SparseMatrixCSC{Tv,Ti<:Integer} <: AbstractSparseMatrix{Tv,Ti}
    m::Int                  # Number of rows
    n::Int                  # Number of columns
    colptr::Vector{Ti}      # Column i is in colptr[i]:(colptr[i+1]-1)
    rowval::Vector{Ti}      # Row values of nonzeros
    nzval::Vector{Tv}       # Nonzero values
end

function SparseMatrixCSC(Tv::Type, Ti::Type, m::Integer, n::Integer, numnz::Integer)
    colptr = Array(Ti, n+1)
    rowval = Array(Ti, numnz)
    nzval = Array(Tv, numnz)

    colptr[1] = 1
    colptr[end] = numnz+1
    SparseMatrixCSC{Tv,Ti}(int(m), int(n), colptr, rowval, nzval)
end

function SparseMatrixCSC(m::Integer, n::Integer, colptr::Vector, rowval::Vector, nzval::Vector)
    return SparseMatrixCSC(int(m), int(n), colptr, rowval, nzval)
end

size(S::SparseMatrixCSC) = (S.m, S.n)
nnz(S::SparseMatrixCSC) = int(S.colptr[end]-1)

function show(io::IO, S::SparseMatrixCSC)
    println(io, S.m, "x", S.n, " sparse matrix with ", nnz(S), " nonzeros:")

    half_screen_rows = div(tty_rows() - 8, 2)
    pad = ndigits(max(S.m,S.n))
    k = 0
    for col = 1:S.n, k = S.colptr[col] : (S.colptr[col+1]-1)
        if k < half_screen_rows || k > nnz(S)-half_screen_rows
            println(io, "\t[", rpad(S.rowval[k], pad), ", ", lpad(col, pad), "]  =  ",
                    sprint(showcompact, S.nzval[k]))
        elseif k == half_screen_rows
            println("\t\u22ee")
        end
        k += 1
    end
end

## Reinterpret and Reshape

function reinterpret{T,Tv,Ti}(::Type{T}, a::SparseMatrixCSC{Tv,Ti})
    if sizeof(T) != sizeof(Tv)
        error("SparseMatrixCSC reinterpret is only supported for element types of the same size")
    end
    mA, nA = size(a)
    colptr = copy(a.colptr)
    rowval = copy(a.rowval)
    nzval  = reinterpret(Tv, a.nzval)
    return SparseMatrixCSC{T,Ti}(mA, nA, colptr, rowval, nzval)
end

function sparse_compute_reshaped_colptr_and_rowval(colptrS, rowvalS, mS, nS, colptrA, rowvalA, mA, nA)
    colptrS[1] = 1

    colA = 1
    colS = 1
    ptr = 1

    while colA <= nA
        while ptr <= colptrA[colA+1]-1
            rowA = rowvalA[ptr]
            i = (colA - 1) * mA + rowA - 1
            colSn = div(i, mS) + 1
            rowS = mod(i, mS) + 1
            while colS < colSn
                colptrS[colS+1] = ptr
                colS += 1
            end
            rowvalS[ptr] = rowS
            ptr += 1
        end
        colA += 1
    end
    while colS <= nS
        colptrS[colS+1] = ptr
        colS += 1
    end
end

function reinterpret{T,Tv,Ti,N}(::Type{T}, a::SparseMatrixCSC{Tv,Ti}, dims::NTuple{N,Int})
    if sizeof(T) != sizeof(Tv)
        error("SparseMatrixCSC reinterpret is only supported for element types of the same size")
    end
    if prod(dims) != length(a)
        error("reinterpret: invalid dimensions")
    end
    mS,nS = dims
    mA,nA = size(a)
    numnz = nnz(a)
    colptr = Array(Ti, nS+1)
    rowval = Array(Ti, numnz)
    nzval = reinterpret(T, a.nzval)

    sparse_compute_reshaped_colptr_and_rowval(colptr, rowval, mS, nS, a.colptr, a.rowval, mA, nA)

    return SparseMatrixCSC{T,Ti}(mS, nS, colptr, rowval, nzval)
end

function reshape{Tv,Ti}(a::SparseMatrixCSC{Tv,Ti}, dims::NTuple{2,Int})
    if prod(dims) != length(a)
        error("reshape: invalid dimensions")
    end
    mS,nS = dims
    mA,nA = size(a)
    numnz = nnz(a)
    colptr = Array(Ti, nS+1)
    rowval = Array(Ti, numnz)
    nzval = a.nzval

    sparse_compute_reshaped_colptr_and_rowval(colptr, rowval, mS, nS, a.colptr, a.rowval, mA, nA)

    return SparseMatrixCSC{Tv,Ti}(mS, nS, colptr, rowval, nzval)
end

## Constructors

copy(S::SparseMatrixCSC) =
    SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), copy(S.nzval))

similar(S::SparseMatrixCSC, Tv::Type) = 
    SparseMatrixCSC(S.m, S.n, similar(S.colptr), similar(S.rowval), Array(Tv, length(S.rowval)))

similar(S::SparseMatrixCSC, Tv::Type, d::(Int,Int)) = spzeros(Tv, d[1], d[2])

function similar(A::SparseMatrixCSC, Tv::Type, Ti::Type)
    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval

    colptr = Array(Ti, length(colptrA))
    rowval = Array(Ti, length(rowvalA))
    nzval  = Array(Tv, length(nzvalA))

    for i=1:length(colptr)
        colptr[i] = colptrA[i]
    end

    for i=1:length(rowval)
        rowval[i] = rowvalA[i]
    end

    SparseMatrixCSC(S.m, S.n, similar(S.colptr), similar(S.rowval), Array(Tv, length(S.rowval)))
end

convert{Tv,Ti}(::Type{SparseMatrixCSC{Tv,Ti}}, S::SparseMatrixCSC) =
    SparseMatrixCSC(S.m, S.n, convert(Vector{Ti},S.colptr), convert(Vector{Ti},S.rowval), convert(Vector{Tv},S.nzval))

convert(::Type{SparseMatrixCSC}, M::Matrix) = sparse(M)

convert(::Type{Matrix}, S::SparseMatrixCSC) = dense(S)

full(S::SparseMatrixCSC) = dense(S)

function dense{Tv}(S::SparseMatrixCSC{Tv})
    A = zeros(Tv, S.m, S.n)
    for col = 1 : S.n, k = S.colptr[col] : (S.colptr[col+1]-1)
        A[S.rowval[k], col] = S.nzval[k]
    end
    return A
end

# Construct a sparse vector

sparsevec{K<:Integer,V}(d::Dict{K,V}, len::Int) = sparsevec(keys(d), values(d), len)

sparsevec{K<:Integer,V}(d::Dict{K,V}) = sparsevec(keys(d), values(d))

sparsevec(I::AbstractVector, V, m::Integer) = sparsevec(I, V, m, +)

sparsevec(I::AbstractVector, V) = sparsevec(I, V, max(I), +)

function sparsevec(I::AbstractVector, V, m::Integer, combine::Function)
    nI = length(I)
    if isa(V, Number); V = fill(V, nI); end
    p = sortperm(I)
    I = I[p]
    V = V[p]
    sparse_IJ_sorted!(I, ones(Int, nI), V, m, 1, combine)
end

function sparsevec(a::Vector)
    n = length(a)
    I = find(a)
    J = ones(Int, n)
    V = nonzeros(a)
    return sparse_IJ_sorted!(I,J,V,n,1,+)
end

sparse(a::Vector) = sparsevec(a)

function sparse(A::Matrix)
    m, n = size(A)
    (I, J, V) = findn_nzs(A)
    return sparse_IJ_sorted!(I,J,V,m,n)
end

sparse(S::SparseMatrixCSC) = copy(S)

sparse_IJ_sorted!(I,J,V,m,n) = sparse_IJ_sorted!(I,J,V,m,n,+)

sparse_IJ_sorted!(I,J,V::AbstractVector{Bool},m,n) = sparse_IJ_sorted!(I,J,V,m,n,|)

function sparse_IJ_sorted!{Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti},
                                        V::AbstractVector,
                                        m::Integer, n::Integer, combine::Function)

    m = m < 0 ? 0 : m
    n = n < 0 ? 0 : n
    if length(V) == 0; return spzeros(m,n); end

    cols = zeros(Ti, n+1)
    cols[1] = 1  # For cumsum purposes
    cols[J[1] + 1] = 1

    lastdup = 1
    ndups = 0
    I_lastdup = I[1]
    J_lastdup = J[1]

    for k=2:length(I)
        if I[k] == I_lastdup && J[k] == J_lastdup
            V[lastdup] = combine(V[lastdup], V[k])
            ndups += 1
        else
            cols[J[k] + 1] += 1
            lastdup = k-ndups
            I_lastdup = I[k]
            J_lastdup = J[k]
            if ndups != 0
                I[lastdup] = I_lastdup
                V[lastdup] = V[k]
            end
        end
    end

    colptr = cumsum(cols)

    # Allow up to 20% slack
    if ndups > 0.2*length(I)
        numnz = length(I)-ndups
        I = I[1:numnz]
        V = V[1:numnz]
    end

    return SparseMatrixCSC(m, n, colptr, I, V)
end

## sparse() can take its inputs in unsorted order

sparse(I,J,v::Number) = sparse(I, J, fill(v,length(I)), int(max(I)), int(max(J)), +)

sparse(I,J,V::AbstractVector) = sparse(I, J, V, int(max(I)), int(max(J)), +)

sparse(I,J,v::Number,m,n) = sparse(I, J, fill(v,length(I)), int(m), int(n), +)

sparse(I,J,V::AbstractVector,m,n) = sparse(I, J, V, int(m), int(n), +)

sparse(I,J,V::AbstractVector{Bool},m,n) = sparse(I, J, V, int(m), int(n), |)

sparse(I,J,v::Number,m,n,combine::Function) = sparse(I, J, fill(v,length(I)), int(m), int(n), combine)

# Based on Direct Methods for Sparse Linear Systems, T. A. Davis, SIAM, Philadelphia, Sept. 2006.
# Section 2.4: Triplet form
# http://www.cise.ufl.edu/research/sparse/CSparse/
function sparse{Tv,Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti}, 
                                V::AbstractVector{Tv},
                                nrow::Integer, ncol::Integer, combine::Function)

    if length(I) == 0; return spzeros(eltype(V),nrow,ncol); end

    # Work array
    Wj = Array(Ti, max(nrow,ncol)+1)

    # Allocate sparse matrix data structure
    # Count entries in each row
    nz = length(I)
    Rnz = zeros(Ti, nrow+1)
    Rnz[1] = 1
    for k=1:nz
        Rnz[I[k]+1] += 1
    end
    Rp = cumsum(Rnz)
    Ri = Array(Ti, nz)
    Rx = Array(Tv, nz)

    # Construct row form
    # place triplet (i,j,x) in column i of R
    # Use work array for temporary row pointers
    for i=1:nrow; Wj[i] = Rp[i]; end

    for k=1:nz
        ind = I[k]
        p = Wj[ind]
        Wj[ind] += 1
        Rx[p] = V[k]
        Ri[p] = J[k]
    end

    # Reset work array for use in counting duplicates
    for j=1:ncol; Wj[j] = 0; end

    # Sum up duplicates and squeeze
    anz = 0
    for i=1:nrow
        p1 = Rp[i]
        p2 = Rp[i+1] - 1
        pdest = p1

        for p = p1:p2
            j = Ri[p]
            pj = Wj[j]
            if pj >= p1
                Rx[pj] = combine (Rx[pj], Rx[p])
            else
                Wj[j] = pdest
                if pdest != p
                    Ri[pdest] = j
                    Rx[pdest] = Rx[p]
                end
                pdest += 1
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
    for i=2:(ncol+1); Wj[i] = 0; end
    for j = 1:nrow
        p1 = Rp[j]
        p2 = p1 + Rnz[j] - 1        
        for p = p1:p2
            Wj[Ri[p]+1] += 1
        end
    end
    RpT = cumsum(Wj[1:(ncol+1)])

    # Transpose 
    for i=1:length(RpT); Wj[i] = RpT[i]; end
    for j = 1:nrow
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

function find(S::SparseMatrixCSC)
    sz = size(S)
    I, J = findn(S)
    return sub2ind(sz, I, J)
end

function findn{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti})
    numnz = nnz(S)
    I = Array(Ti, numnz)
    J = Array(Ti, numnz)

    count = 1
    for col = 1 : S.n, k = S.colptr[col] : (S.colptr[col+1]-1)
        if S.nzval[k] != 0
            I[count] = S.rowval[k]
            J[count] = col
            count += 1
        else
            println("Warning: sparse matrix contains explicit stored zeros.")
        end
    end

    if numnz != count-1
        I = I[1:count]
        J = J[1:count]
    end

    return (I, J)
end

function findn_nzs{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti})
    numnz = nnz(S)
    I = Array(Ti, numnz)
    J = Array(Ti, numnz)
    V = Array(Tv, numnz)

    count = 1
    for col = 1 : S.n, k = S.colptr[col] : (S.colptr[col+1]-1)
        if S.nzval[k] != 0
            I[count] = S.rowval[k]
            J[count] = col
            V[count] = S.nzval[k]
            count += 1
        else
            println("Warning: sparse matrix contains explicit stored zeros.")
        end
    end

    if numnz != count-1
        I = I[1:count]
        J = J[1:count]
        V = V[1:count]
    end

    return (I, J, V)
end

function sprand(m::Integer, n::Integer, density::FloatingPoint, rng::Function, v)
    numnz = int(m*n*density)
    I = rand!(1:m, Array(Int, numnz))
    J = rand!(1:n, Array(Int, numnz))
    S = sparse(I, J, v, m, n)
    if !isbool(v)
        S.nzval = rng(nnz(S))
    end

    return S
end

sprand(m::Integer, n::Integer, density::FloatingPoint, rng::Function) = sprand(m,n,density,rng, 1.0)
sprand(m::Integer, n::Integer, density::FloatingPoint)  = sprand(m,n,density,rand, 1.0)
sprandn(m::Integer, n::Integer, density::FloatingPoint) = sprand(m,n,density,randn, 1.0)
sprandbool(m::Integer, n::Integer, density::FloatingPoint) = sprand(m,n,density,randbool, true)

spones{T}(S::SparseMatrixCSC{T}) =
     SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), ones(T, S.colptr[end]-1))

spzeros(m::Integer) = spzeros(m, m)
spzeros(m::Integer, n::Integer) = spzeros(Float64, m, n)
spzeros(Tv::Type, m::Integer) = spzeros(Tv, m, m)
spzeros(Tv::Type, m::Integer, n::Integer) =
    SparseMatrixCSC(m, n, ones(Int, n+1), Array(Int, 0), Array(Tv, 0))

speye(n::Integer) = speye(Float64, n)
speye(T::Type, n::Integer) = speye(T, n, n)
speye(m::Integer, n::Integer) = speye(Float64, m, n)
speye{T}(S::SparseMatrixCSC{T}) = speye(T, size(S, 1), size(S, 2))

function speye(T::Type, m::Integer, n::Integer)
    x = min(m,n)
    rowval = [1:x]
    colptr = [rowval, fill(int(x+1), n+1-x)]
    nzval  = ones(T, x)
    return SparseMatrixCSC(m, n, colptr, rowval, nzval)
end

function one{T}(S::SparseMatrixCSC{T})
    m,n = size(S)
    if m != n; error("Multiplicative identity only defined for square matrices!"); end;
    speye(T, m)
end

## Transpose

# Based on Direct Methods for Sparse Linear Systems, T. A. Davis, SIAM, Philadelphia, Sept. 2006.
# Section 2.5: Transpose
# http://www.cise.ufl.edu/research/sparse/CSparse/
function transpose{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti})
    (nT, mT) = size(S)
    nnzS = nnz(S)
    colptr_S = S.colptr
    rowval_S = S.rowval
    nzval_S = S.nzval

    rowval_T = Array(Ti, nnzS)
    nzval_T = Array(Tv, nnzS)

    w = zeros(Ti, nT+1)
    w[1] = 1
    for i=1:nnzS
        w[rowval_S[i]+1] += 1
    end
    colptr_T = cumsum(w)
    w = copy(colptr_T)

    for j = 1:mT, p = colptr_S[j]:(colptr_S[j+1]-1)
        ind = rowval_S[p]
        q = w[ind]
        w[ind] += 1
        rowval_T[q] = j
        nzval_T[q] = nzval_S[p]
    end

    SparseMatrixCSC(mT, nT, colptr_T, rowval_T, nzval_T)
end

function ctranspose{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti})
    (nT, mT) = size(S)
    nnzS = nnz(S)
    colptr_S = S.colptr
    rowval_S = S.rowval
    nzval_S = S.nzval

    rowval_T = Array(Ti, nnzS)
    nzval_T = Array(Tv, nnzS)

    w = zeros(Ti, nT+1)
    w[1] = 1
    for i=1:nnzS
        w[rowval_S[i]+1] += 1
    end
    colptr_T = cumsum(w)
    w = copy(colptr_T)

    for j = 1:mT, p = colptr_S[j]:(colptr_S[j+1]-1)
        ind = rowval_S[p]
        q = w[ind]
        w[ind] += 1
        rowval_T[q] = j
        nzval_T[q] = conj(nzval_S[p])
    end

    SparseMatrixCSC(mT, nT, colptr_T, rowval_T, nzval_T)
end

## Unary arithmetic operators

for op in (:-, )
    @eval begin

        function ($op){Tv,Ti}(A::SparseMatrixCSC{Tv,Ti})
            B = copy(A)
            nzvalB = B.nzval
            for i=1:length(nzvalB)
                nzvalB[i] = ($op)(nzvalB[i])
            end
            return B
        end
        
    end
end

## Binary arithmetic operators

for op in (:+, :-, :.*, :.^)
    @eval begin

        function ($op){Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti})
            if size(A,1) != size(B,1) || size(A,2) != size(B,2)
                error("Incompatible sizes")
            end

            (m, n) = size(A)

            # TODO: Need better method to estimate result space
            nnzS = nnz(A) + nnz(B)
            colptrS = Array(Ti, A.n+1)
            rowvalS = Array(Ti, nnzS)
            nzvalS = Array(Tv, nnzS)

            z = zero(Tv)

            colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
            colptrB = B.colptr; rowvalB = B.rowval; nzvalB = B.nzval

            ptrS = 1
            colptrS[1] = 1

            for col = 1:n
                ptrA::Int  = colptrA[col]
                stopA::Int = colptrA[col+1]
                ptrB::Int  = colptrB[col]
                stopB::Int = colptrB[col+1]

                while ptrA < stopA && ptrB < stopB
                    rowA = rowvalA[ptrA]
                    rowB = rowvalB[ptrB]
                    if rowA < rowB
                        res = ($op)(nzvalA[ptrA], z)
                        if res != z
                            rowvalS[ptrS] = rowA
                            nzvalS[ptrS] = res
                            ptrS += 1
                        end
                        ptrA += 1
                    elseif rowB < rowA
                        res = ($op)(z, nzvalB[ptrB])
                        if res != z
                            rowvalS[ptrS] = rowB
                            nzvalS[ptrS] = res
                            ptrS += 1
                        end
                        ptrB += 1
                    else
                        res = ($op)(nzvalA[ptrA], nzvalB[ptrB])
                        if res != z
                            rowvalS[ptrS] = rowA
                            nzvalS[ptrS] = res
                            ptrS += 1
                        end
                        ptrA += 1
                        ptrB += 1
                    end
                end

                while ptrA < stopA
                    res = ($op)(nzvalA[ptrA], z)
                    if res != z
                        rowA = rowvalA[ptrA]
                        rowvalS[ptrS] = rowA
                        nzvalS[ptrS] = res
                        ptrS += 1
                    end
                    ptrA += 1
                end

                while ptrB < stopB
                    res = ($op)(z, nzvalB[ptrB])
                    if res != z
                        rowB = rowvalB[ptrB]
                        rowvalS[ptrS] = rowB
                        nzvalS[ptrS] = res
                        ptrS += 1
                    end
                    ptrB += 1
                end

                colptrS[col+1] = ptrS
            end

            delete!(rowvalS, colptrS[end]:length(rowvalS))
            delete!(nzvalS, colptrS[end]:length(nzvalS))
            return SparseMatrixCSC(m, n, colptrS, rowvalS, nzvalS)
        end

    end # quote
end # macro

(+)(A::SparseMatrixCSC, B::Union(Array,Number)) = (+)(dense(A), B)
(+)(A::Union(Array,Number), B::SparseMatrixCSC) = (+)(A, dense(B))

(-)(A::SparseMatrixCSC, B::Union(Array,Number)) = (-)(dense(A), B)
(-)(A::Union(Array,Number), B::SparseMatrixCSC) = (-)(A, dense(B))

(.*)(A::SparseMatrixCSC, B::Number) = SparseMatrixCSC(A.m, A.n, copy(A.colptr), copy(A.rowval), A.nzval .* B)
(.*)(A::Number, B::SparseMatrixCSC) = SparseMatrixCSC(B.m, B.n, copy(B.colptr), copy(B.rowval), A .* B.nzval)
(.*)(A::SparseMatrixCSC, B::Array) = (.*)(A, sparse(B))
(.*)(A::Array, B::SparseMatrixCSC) = (.*)(sparse(A), B)

(./)(A::SparseMatrixCSC, B::Number) = SparseMatrixCSC(A.m, A.n, copy(A.colptr), copy(A.rowval), A.nzval ./ B)
(./)(A::Number, B::SparseMatrixCSC) = (./)(A, dense(B))
(./)(A::SparseMatrixCSC, B::Array) = (./)(dense(A), B)
(./)(A::Array, B::SparseMatrixCSC) = (./)(A, dense(B))
(./)(A::SparseMatrixCSC, B::SparseMatrixCSC) = (./)(dense(A), dense(B))

(.\)(A::SparseMatrixCSC, B::Number) = (.\)(dense(A), B)
(.\)(A::Number, B::SparseMatrixCSC) = SparseMatrixCSC(B.m, B.n, copy(B.colptr), copy(B.rowval), B.nzval .\ A)
(.\)(A::SparseMatrixCSC, B::Array) = (.\)(dense(A), B)
(.\)(A::Array, B::SparseMatrixCSC) = (.\)(A, dense(B))
(.\)(A::SparseMatrixCSC, B::SparseMatrixCSC) = (.\)(dense(A), dense(B))

(.^)(A::SparseMatrixCSC, B::Number) = SparseMatrixCSC(A.m, A.n, copy(A.colptr), copy(A.rowval), A.nzval .^ B)
(.^)(A::Number, B::SparseMatrixCSC) = (.^)(A, dense(B))
(.^)(A::SparseMatrixCSC, B::Array) = (.^)(dense(A), B)
(.^)(A::Array, B::SparseMatrixCSC) = (.^)(A, dense(B))

# Reductions

# TODO: Should the results of sparse reductions be sparse?
function reducedim{Tv,Ti}(f::Function, A::SparseMatrixCSC{Tv,Ti}, region, v0)
    if region == 1

        S = Array(Tv, 1, A.n)
        for i = 1 : A.n
            Si = v0
            ccount = 0
            for j = A.colptr[i] : A.colptr[i+1]-1
                Si = f(Si, A.nzval[j])
                ccount += 1
            end
            if ccount != A.m; Si = f(Si, zero(Tv)); end
            S[i] = Si
        end
        return S

    elseif region == 2

        S = fill(v0, A.m, 1)
        rcounts = zeros(Ti, A.m)
        for i = 1 : A.n, j = A.colptr[i] : A.colptr[i+1]-1
            row = A.rowval[j]
            S[row] = f(S[row], A.nzval[j])
            rcounts[row] += 1
        end
        for i = 1:A.m
            if rcounts[i] != A.n; S[i] = f(S[i], zero(Tv)); end
        end
        return S

    elseif region == (1,2)

        S = v0
        for i = 1 : A.n, j = A.colptr[i] : A.colptr[i+1]-1
            S = f(S, A.nzval[j])
        end
        if nnz(A) != A.m*A.n; S = f(S, zero(Tv)); end

        return [S]

    else

        error("Invalid value for region")

    end
end

max{T}(A::SparseMatrixCSC{T}) =
    isempty(A) ? error("max: argument is empty") : reducedim(max,A,(1,2),typemin(T))
max{T}(A::SparseMatrixCSC{T}, b::(), region) =
    isempty(A) ? similar(A, reduced_dims0(A,region)) : reducedim(max,A,region,typemin(T))

min{T}(A::SparseMatrixCSC{T}) =
    isempty(A) ? error("min: argument is empty") : reducedim(min,A,(1,2),typemax(T))
min{T}(A::SparseMatrixCSC{T}, b::(), region) =
    isempty(A) ? similar(A, reduced_dims0(A,region)) : reducedim(min,A,region,typemax(T))

sum{T}(A::SparseMatrixCSC{T}) = reducedim(+,A,(1,2),zero(T))
sum{T}(A::SparseMatrixCSC{T}, region)  = reducedim(+,A,region,zero(T))

prod{T}(A::SparseMatrixCSC{T}) = reducedim(*,A,(1,2),one(T))
prod{T}(A::SparseMatrixCSC{T}, region) = reducedim(*,A,region,one(T))

#all(A::SparseMatrixCSC{Bool}, region) = reducedim(all,A,region,true)
#any(A::SparseMatrixCSC{Bool}, region) = reducedim(any,A,region,false)
#sum(A::SparseMatrixCSC{Bool}, region) = reducedim(+,A,region,0,Int)
#sum(A::SparseMatrixCSC{Bool}) = nnz(A)

## getindex
getindex(A::SparseMatrixCSC, i::Integer) = getindex(A, ind2sub(size(A),i))
getindex(A::SparseMatrixCSC, I::(Integer,Integer)) = getindex(A, I[1], I[2])

function getindex{T}(A::SparseMatrixCSC{T}, i0::Integer, i1::Integer)
    if !(1 <= i0 <= A.m && 1 <= i1 <= A.n); error(BoundsError); end
    first = A.colptr[i1]
    last = A.colptr[i1+1]-1
    while first <= last
        mid = (first + last) >> 1
        t = A.rowval[mid]
        if t == i0
            return A.nzval[mid]
        elseif t > i0
            last = mid - 1
        else
            first = mid + 1
        end
    end
    return zero(T)
end

getindex{T<:Integer}(A::SparseMatrixCSC, I::AbstractVector{T}, j::Integer) = getindex(A,I,[j])
getindex{T<:Integer}(A::SparseMatrixCSC, i::Integer, J::AbstractVector{T}) = getindex(A,[i],J)

function getindex_cols{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, J::AbstractVector)

    (m, n) = size(A)
    nJ = length(J)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval

    colptrS = Array(Ti, nJ+1)
    colptrS[1] = 1
    nnzS = 0

    for j = 1:nJ
        col = J[j]
        nnzS += colptrA[col+1] - colptrA[col]
        colptrS[j+1] = nnzS + 1
    end

    rowvalS = Array(Ti, nnzS)
    nzvalS  = Array(Tv, nnzS)
    ptrS = 0

    for j = 1:nJ
        col = J[j]

        for k = colptrA[col]:colptrA[col+1]-1
            ptrS += 1
            rowvalS[ptrS] = rowvalA[k]
            nzvalS[ptrS] = nzvalA[k]
        end
    end

    return SparseMatrixCSC(m, nJ, colptrS, rowvalS, nzvalS)

end

# TODO: See if growing arrays is faster than pre-computing structure
# and then populating nonzeros
# TODO: Use binary search in cases where nI >> nnz(A[:,j]) or nI << nnz(A[:,j])
function getindex_I_sorted{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::Vector, J::AbstractVector)

    (m, n) = size(A)
    nI = length(I)
    nJ = length(J)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval

    I_ref = falses(m)
    I_ref[I] = true

    I_repeat = zeros(Int, m)
    for i=1:nI; I_repeat[I[i]] += 1; end

    colptrS = Array(Ti, nJ+1)
    colptrS[1] = 1
    nnzS = 0

    # Form the structure of the result and compute space
    for j = 1:nJ
        col = J[j]

        for k = colptrA[col]:colptrA[col+1]-1
            rowA = rowvalA[k]
            
            if I_ref[rowA]
                for r = 1:I_repeat[rowA]
                    nnzS += 1
                end
            end

        end
        colptrS[j+1] = nnzS+1
    end

    # Populate the values in the result
    rowvalS = Array(Ti, nnzS)
    nzvalS  = Array(Tv, nnzS)
    ptrS    = 1

    fI = zeros(Ti, m)
    for k=1:nI
        Ik = I[k]
        if fI[Ik] == 0; fI[Ik] = k; end
    end

    for j = 1:nJ
        col = J[j]

        for k = colptrA[col]:colptrA[col+1]-1
            rowA = rowvalA[k]
            
            if I_ref[rowA]
                for r = 1:I_repeat[rowA]
                    rowvalS[ptrS] = fI[rowA] + r - 1
                    nzvalS[ptrS] = nzvalA[k]
                    ptrS += 1
                end
            end

        end
    end

    return SparseMatrixCSC(nI, nJ, colptrS, rowvalS, nzvalS)
end

# getindex_I_sorted based on merging of sorted lists
function getindex_I_sorted_old{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::Vector, J::AbstractVector)

    (m, n) = size(A)
    nI = length(I)
    nJ = length(J)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval

    colptrS = Array(Ti, nJ+1)
    colptrS[1] = 1
    nnzS = 0

    # Form the structure of the result and compute space
    for j = 1:nJ
        col = J[j]

        ptrI::Int = 1

        ptrA::Int = colptrA[col]
        stopA::Int = colptrA[col+1]

        while ptrI <= nI && ptrA < stopA
            rowA = rowvalA[ptrA]
            rowI = I[ptrI]

            if rowI > rowA
                ptrA += 1
            elseif rowI < rowA
                ptrI += 1
            else
                nnzS += 1
                ptrI += 1
            end
        end
        colptrS[j+1] = nnzS+1

    end

    fI = find(I)

    # Populate the values in the result
    rowvalS = Array(Ti, nnzS)
    nzvalS  = Array(Tv, nnzS)
    ptrS = 0

    for j = 1:nJ
        col = J[j]

        ptrI::Int = 1

        ptrA::Int = colptrA[col]
        stopA::Int = colptrA[col+1]

        while ptrI <= nI && ptrA < stopA
            rowA = rowvalA[ptrA]
            rowI = I[ptrI]

            if rowI > rowA
                ptrA += 1
            elseif rowI < rowA
                ptrI += 1
            else
                ptrS += 1
                rowvalS[ptrS] = fI[ptrI]
                nzvalS[ptrS] = nzvalA[ptrA]
                ptrI += 1
            end
        end

    end

    return SparseMatrixCSC(nI, nJ, colptrS, rowvalS, nzvalS)
end

function getindex_general{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::Vector, J::AbstractVector)
    (m, n) = size(A)
    nI = length(I)
    nJ = length(J)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval

    nnzS = 0

    pI = sortperm(I); I = I[pI]
    fI = find(I)

    W = zeros(Int, nI + 1) # Keep row counts
    W[1] = 1               # For cumsum later

    # Form the structure of the result and compute space
    for j = 1:nJ
        col = J[j]

        ptrI::Int = 1

        ptrA::Int = colptrA[col]
        stopA::Int = colptrA[col+1]

        while ptrI <= nI && ptrA < stopA
            rowA = rowvalA[ptrA]
            rowI = I[ptrI]

            if rowI > rowA
                ptrA += 1
            elseif rowI < rowA
                ptrI += 1
            else
                W[fI[pI[ptrI]]+1] += 1
                nnzS += 1
                ptrI += 1
            end
        end

    end

    colptrS_T = cumsum(W)

    # Populate the values in the result, but transposed
    rowvalS_T = Array(Ti, nnzS)
    nzvalS_T  = Array(Tv, nnzS)
    for i=1:nI; W[i] = 0; end     # Zero out W to store row positions

    for j = 1:nJ
        col = J[j]

        ptrI::Int = 1

        ptrA::Int = colptrA[col]
        stopA::Int = colptrA[col+1]

        while ptrI <= nI && ptrA < stopA
            rowA = rowvalA[ptrA]
            rowI = I[ptrI]

            if rowI > rowA
                ptrA += 1
            elseif rowI < rowA
                ptrI += 1
            else
                rowS = fI[pI[ptrI]]
                k = colptrS_T[rowS] + W[rowS]
                rowvalS_T[k] = j
                nzvalS_T[k] = nzvalA[ptrA]
                W[rowS] += 1
                ptrI += 1
            end
        end

    end

    # Transpose so that rows are in sorted order and return
    S_T = SparseMatrixCSC(nJ, nI, colptrS_T, rowvalS_T, nzvalS_T)
    return S_T.'

end

# S = A[I, J]
function getindex{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector)
    m = size(A, 1)

    if isa(I, Range) || isa(I, Range1); I = [I]; end

    if I == 1:m
        return getindex_cols(A, J)
    elseif issorted(I)
        return getindex_I_sorted(A, I, J)
    else
        return getindex_general(A, I, J)
    end

end

## setindex!
setindex!(A::SparseMatrixCSC, v, i::Integer) = setindex!(A, v, ind2sub(size(A),i)...)

function setindex!{T,Ti}(A::SparseMatrixCSC{T,Ti}, v, i0::Integer, i1::Integer)
    i0 = convert(Ti, i0)
    i1 = convert(Ti, i1)
    if !(1 <= i0 <= A.m && 1 <= i1 <= A.n); error(BoundsError); end
    v = convert(T, v)
    if v == 0 #either do nothing or delete entry if it exists
        first = A.colptr[i1]
        last = A.colptr[i1+1]-1
        loc = -1
        while first <= last
            mid = (first + last) >> 1
            t = A.rowval[mid]
            if t == i0
                loc = mid
                break
            elseif t > i0
                last = mid - 1
            else
                first = mid + 1
            end
        end
        if loc != -1
            delete!(A.rowval, loc)
            delete!(A.nzval, loc)
            for j = (i1+1):(A.n+1)
                A.colptr[j] = A.colptr[j] - 1
            end
        end
        return A
    end
    first = A.colptr[i1]
    last = A.colptr[i1+1]-1
    #find i such that A.rowval[i] = i0, or A.rowval[i-1] < i0 < A.rowval[i]
    while last - first >= 3
        mid = (first + last) >> 1
        t = A.rowval[mid]
        if t == i0
            A.nzval[mid] = v
            return A
        elseif t > i0
            last = mid - 1
        else
            first = mid + 1
        end
    end
    if last - first == 2
        mid = first + 1
        if A.rowval[mid] == i0
            A.nzval[mid] = v
            return A
        elseif A.rowval[mid] > i0
            if A.rowval[first] == i0
                A.nzval[first] = v
                return A
            elseif A.rowval[first] < i0
                i = first+1
            else #A.rowval[first] > i0
                i = first
            end
        else #A.rowval[mid] < i0
            if A.rowval[last] == i0
                A.nzval[last] = v
                return A
            elseif A.rowval[last] > i0
                i = last
            else #A.rowval[last] < i0
                i = last+1
            end
        end
    elseif last - first == 1
        if A.rowval[first] == i0
            A.nzval[first] = v
            return A
        elseif A.rowval[first] > i0
            i = first
        else #A.rowval[first] < i0
            if A.rowval[last] == i0
                A.nzval[last] = v
                return A
            elseif A.rowval[last] < i0
                i = last+1
            else #A.rowval[last] > i0
                i = last
            end
        end
    elseif last == first
        if A.rowval[first] == i0
            A.nzval[first] = v
            return A
        elseif A.rowval[first] < i0
            i = first+1
        else #A.rowval[first] > i0
            i = first
        end
    else #last < first to begin with
        i = first
    end
    insert!(A.rowval, i, i0)
    insert!(A.nzval, i, v)
    for j = (i1+1):(A.n+1)
        A.colptr[j] = A.colptr[j] + 1
    end
    return A
end

setindex!(A::SparseMatrixCSC, v::AbstractMatrix, i::Integer, J::AbstractVector) = setindex!(A, v, [i], J)
setindex!(A::SparseMatrixCSC, v::AbstractMatrix, I::AbstractVector, j::Integer) = setindex!(A, v, I, [j])

setindex!{Tv}(A::SparseMatrixCSC{Tv}, x::Number, I::AbstractVector, J::AbstractVector) = 
    setindex!(A, fill(x::Tv, (length(I), length(J))), I, J)

setindex!{Tv}(A::SparseMatrixCSC{Tv}, S::Matrix{Tv}, I::AbstractVector, J::AbstractVector) = 
      setindex!(A, sparse(S), I, J)

# A[I,J] = B
function setindex!{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector)
    if size(B,1) != length(I) || size(B,2) != length(J)
        return("error in setindex!: mismatched dimensions")
    end

    issortedI = issorted(I)
    issortedJ = issorted(J)

    if ~issortedI && ~issortedJ
        pI = sortperm(I); I = I[pI]
        pJ = sortperm(J); J = J[pJ]
        B = B[pI, pJ]
    elseif ~issortedI
        pI = sortperm(I); I = I[pI]
        B = B[pI,:]
    else ~issortedJ
        pJ = sortperm(J); J = J[pJ]
        B = B[:, pJ]
    end

    m, n = size(A)
    mB, nB = size(B)

    nI = length(I)
    nJ = length(J)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    colptrB = B.colptr; rowvalB = B.rowval; nzvalB = B.nzval

    nnzS = nnz(A) + nnz(B)
    colptrS = Array(Ti, n+1)
    rowvalS = Array(Ti, nnzS)
    nzvalS = Array(Tv, nnzS)
    
    colptrS[1] = 1
    colB = 1
    asgn_col = J[colB]

    I_asgn = falses(m)
    I_asgn[I] = true

    ptrS = 1

    for col = 1:n

        # Copy column of A if it is not being assigned into
        if colB > nJ || col != J[colB]
            colptrS[col+1] = colptrS[col] + (colptrA[col+1]-colptrA[col])
            
            for k = colptrA[col]:colptrA[col+1]-1
                rowvalS[ptrS] = rowvalA[k]
                nzvalS[ptrS] = nzvalA[k]
                ptrS += 1
            end
            continue
        end

        ptrA::Int  = colptrA[col]
        stopA::Int = colptrA[col+1]
        ptrB::Int  = colptrB[colB]
        stopB::Int = colptrB[colB+1]

        while ptrA < stopA && ptrB < stopB
            rowA = rowvalA[ptrA]
            rowB = I[rowvalB[ptrB]]
            if rowA < rowB
                if ~I_asgn[rowA]
                    rowvalS[ptrS] = rowA
                    nzvalS[ptrS] = nzvalA[ptrA]
                    ptrS += 1
                end
                ptrA += 1
            elseif rowB < rowA
                rowvalS[ptrS] = rowB
                nzvalS[ptrS] = nzvalB[ptrB]
                ptrS += 1
                ptrB += 1
            else
                rowvalS[ptrS] = rowB
                nzvalS[ptrS] = nzvalB[ptrB]
                ptrS += 1
                ptrB += 1
                ptrA += 1
            end
        end

        while ptrA < stopA
            rowA = rowvalA[ptrA]
            if ~I_asgn[rowA]
                rowvalS[ptrS] = rowA
                nzvalS[ptrS] = nzvalA[ptrA]
                ptrS += 1
            end
            ptrA += 1
        end

        while ptrB < stopB
            rowB = I[rowvalB[ptrB]]
            rowvalS[ptrS] = rowB
            nzvalS[ptrS] = nzvalB[ptrB]
            ptrS += 1
            ptrB += 1
        end

        colptrS[col+1] = ptrS
        colB += 1
    end

    delete!(rowvalS, colptrS[end]:length(rowvalS))
    delete!(nzvalS, colptrS[end]:length(nzvalS))

    A.colptr = colptrS
    A.rowval = rowvalS
    A.nzval = nzvalS
    return A
end

# Sparse concatenation

function vcat(X::SparseMatrixCSC...)
    num = length(X)
    mX = [ size(x, 1) for x in X ] 
    nX = [ size(x, 2) for x in X ]
    n = nX[1]
    for i = 2 : num
        if nX[i] != n; error("error in vcat: mismatched dimensions"); end
    end
    m = sum(mX)

    Tv = promote_type(map(x->eltype(x.nzval), X)...)
    Ti = promote_type(map(x->eltype(x.rowval), X)...)

    colptr = Array(Ti, n + 1)
    nnzX = [ nnz(x) for x in X ]
    nnz_res = sum(nnzX)
    rowval = Array(Ti, nnz_res)
    nzval = Array(Tv, nnz_res)

    colptr[1] = 1
    for c = 1 : n
        mX_sofar = 0
        rr1 = colptr[c]
        for i = 1 : num
            rX1 = X[i].colptr[c]
            rX2 = X[i].colptr[c + 1] - 1
            rr2 = rr1 + (rX2 - rX1)

            rowval[rr1 : rr2] = X[i].rowval[rX1 : rX2] + mX_sofar
            nzval[rr1 : rr2] = X[i].nzval[rX1 : rX2]
            mX_sofar += mX[i]
            rr1 = rr2 + 1
        end
        colptr[c + 1] = rr1
    end
    SparseMatrixCSC(m, n, colptr, rowval, nzval)
end

function hcat(X::SparseMatrixCSC...)
    num = length(X)
    mX = [ size(x, 1) for x in X ]
    nX = [ size(x, 2) for x in X ]
    m = mX[1]
    for i = 2 : num
        if mX[i] != m; error("error in hcat: mismatched dimensions"); end
    end
    n = sum(nX)

    Tv = promote_type(map(x->eltype(x.nzval), X)...)
    Ti = promote_type(map(x->eltype(x.rowval), X)...)

    colptr = Array(Ti, n + 1)
    nnzX = [ nnz(x) for x in X ]
    nnz_res = sum(nnzX)
    rowval = Array(Ti, nnz_res)
    nzval = Array(Tv, nnz_res)

    nnz_sofar = 0
    nX_sofar = 0
    for i = 1 : num
        colptr[(1 : nX[i] + 1) + nX_sofar] = X[i].colptr + nnz_sofar
        rowval[(1 : nnzX[i]) + nnz_sofar] = X[i].rowval
        nzval[(1 : nnzX[i]) + nnz_sofar] = X[i].nzval
        nnz_sofar += nnzX[i]
        nX_sofar += nX[i]
    end

    SparseMatrixCSC(m, n, colptr, rowval, nzval)
end

function hvcat(rows::(Int...), X::SparseMatrixCSC...)
    nbr = length(rows)  # number of block rows

    tmp_rows = Array(SparseMatrixCSC, nbr)
    k = 0
    for i = 1 : nbr
        tmp_rows[i] = hcat(X[(1 : rows[i]) + k]...)
        k += rows[i]
    end
    vcat(tmp_rows...)
end

function mmread(filename::ASCIIString, infoonly::Bool)
#      Reads the contents of the Matrix Market file 'filename'
#      into a matrix, which will be either sparse or dense,
#      depending on the Matrix Market format indicated by
#      'coordinate' (coordinate sparse storage), or
#      'array' (dense array storage).  The data will be duplicated
#      as appropriate if symmetry is indicated in the header. (Not yet
#      implemented).
#
#      If infoonly is true information on the size and structure is
#      returned.
    mmfile = open(filename,"r")
    tokens = split(chomp(readline(mmfile)))
    if length(tokens) != 5 error("Not enough words on header line") end
    if tokens[1] != "%%MatrixMarket" error("Not a valid MatrixMarket header.") end
    (head1, rep, field, symm) = map(lowercase, tokens[2:5])
    if head1 != "matrix"
        error("This seems to be a MatrixMarket $head1 file, not a MatrixMarket matrix file")
    end
    if field != "real" error("non-float fields not yet allowed") end

    ll   = readline(mmfile)         # Read through comments, ignoring them
    while length(ll) > 0 && ll[1] == '%' ll = readline(mmfile) end
    dd     = int(split(ll))         # Read dimensions
    rows   = dd[1]
    cols   = dd[2]
    entries = rep == "coordinate" ? dd[3] : rows * cols
    if infoonly return rows, cols, entries, rep, field, symm end
    if rep == "coordinate"
        rr = Array(Int, entries)
        cc = Array(Int, entries)
        xx = Array(Float64, entries)
        for i in 1:entries
            flds = split(readline(mmfile))
            rr[i] = int32(flds[1])
            cc[i] = int32(flds[2])
            xx[i] = float64(flds[3])
        end
        return sparse(rr, cc, xx, rows, cols)
    end
    reshape([float64(readline(mmfile)) for i in 1:entries], (rows,cols))
end

mmread(filename::ASCIIString) = mmread(filename, false)

## expand a colptr or rowptr into a dense index vector
function expandptr{T<:Integer}(V::Vector{T})
    if V[1] != 1 error("expandptr: first index must be one") end
    res = similar(V, (int64(V[end]-1),))
    for i in 1:(length(V)-1), j in V[i]:(V[i+1] - 1) res[j] = i end
    res
end

# Based on Direct Methods for Sparse Linear Systems, T. A. Davis, SIAM, Philadelphia, Sept. 2006.
# Section 2.7: Removing entries from a matrix
# http://www.cise.ufl.edu/research/sparse/CSparse/
function fkeep!{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, f, other)
    nzorig = nnz(A)
    nz = 1
    for j = 1:A.n
        p = A.colptr[j]                 # record current position
        A.colptr[j] = nz                # set new position
        while p < A.colptr[j+1]
            if f(A.rowval[p], j, A.nzval[p], other)
                A.nzval[nz] = A.nzval[p]
                A.rowval[nz] = A.rowval[p]
                nz += 1
            end
            p += 1
        end
    end
    A.colptr[A.n + 1] = nz
    nz -= 1
    if nz < nzorig
        resize!(A.nzval, nz)
        resize!(A.rowval, nz)
    end
    A
end

droptol!(A::SparseMatrixCSC, tol) = fkeep!(A, (i,j,x,other)->abs(x)>other, tol)
dropzeros!(A::SparseMatrixCSC) = fkeep!(A, (i,j,x,other)->x!=zero(Tv), None)
triu!(A::SparseMatrixCSC) = fkeep!(A, (i,j,x,other)->(j>=i), None)
triu(A::SparseMatrixCSC) = triu!(copy(A))
tril!(A::SparseMatrixCSC) = fkeep!(A, (i,j,x,other)->(i>=j), None)
tril(A::SparseMatrixCSC) = tril!(copy(A))
