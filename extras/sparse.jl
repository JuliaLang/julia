# overloads
import Base.size, Base.nnz, Base.eltype, Base.show, Base.reinterpret, Base.copy
import Base.reshape, Base.similar, Base.convert, Base.find, Base.findn
import Base.one, Base.transpose, Base.ctranspose, Base.+, Base.-, Base.(.*)
import Base.(./), Base.(.\), Base.(.^), Base.sum, Base.ref, Base.assign
import Base.vcat, Base.hcat, Base.cat, Base.hvcat, Base.length, Base.findn_nzs
import Base.full, Base.\, Base.areduce, Base.min, Base.max, Base.sum, Base.prod

# Compressed sparse columns data structure
# Assumes that no zeros are stored in the data structure
type SparseMatrixCSC{Tv,Ti<:Union(Int32,Int64)} <: AbstractMatrix{Tv}
    m::Int                  # Number of rows
    n::Int                  # Number of columns
    colptr::Vector{Ti}      # Column i is in colptr[i]:(colptr[i+1]-1)
    rowval::Vector{Ti}      # Row values of nonzeros
    nzval::Vector{Tv}       # Nonzero values
end

function SparseMatrixCSC(Tv::Type, m::Int, n::Int, numnz::Integer)
    Ti = Int32
    colptr = Array(Ti, n+1)
    rowval = Array(Ti, numnz)
    nzval = Array(Tv, numnz)

    colptr[1] = 1
    colptr[end] = numnz+1
    SparseMatrixCSC{Tv,Ti}(m, n, colptr, rowval, nzval)
end

function SparseMatrixCSC(m::Int32, n::Int32, colptr, rowval, nzval)
    return SparseMatrixCSC(int(m), int(n), colptr, rowval, nzval)
end

issparse(A::AbstractArray) = false
issparse(S::SparseMatrixCSC) = true

size(S::SparseMatrixCSC) = (S.m, S.n)
nnz(S::SparseMatrixCSC) = S.colptr[end]-1

eltype{T}(S::SparseMatrixCSC{T}) = T
indtype{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}) = Ti

function show(io, S::SparseMatrixCSC)
    println(io, S.m, "x", S.n, " sparse matrix with ", nnz(S), " nonzeros:")

    half_screen_rows = div(tty_rows() - 8, 2)
    pad = ndigits(max(S.m,S.n))
    k = 0
    for col = 1:S.n, k = S.colptr[col] : (S.colptr[col+1]-1)
        if k < half_screen_rows || k > nnz(S)-half_screen_rows
            println(io, "\t[", rpad(S.rowval[k], pad), ", ", lpad(col, pad), "]  =  ",
                    sprint(showcompact, S.nzval[k]))
        elseif k == half_screen_rows
            println(io, "\t."); println(io, "\t."); println(io, "\t.");
        end
        k += 1
    end
end

## Reinterpret and Reshape

function reinterpret{T,Tv,Ti}(::Type{T}, a::SparseMatrixCSC{Tv,Ti})
    if sizeof(T) != sizeof(Tv)
        error("SparseMatrixCSC reinterpret is only supported for element types of the same size")
    end
    mA,nA = size(a)
    colptr = copy(a.colptr)
    rowval = copy(a.rowval)
    nzval = reinterpret(T, a.nzval)
    return SparseMatrixCSC{T,Ti}(mA, nA, colptr, rowval, nzval)
end

function _jl_sparse_compute_reshaped_colptr_and_rowval(colptrS, rowvalS, mS, nS, colptrA, rowvalA, mA, nA)
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
    if prod(dims) != numel(a)
        error("reinterpret: invalid dimensions")
    end
    mS,nS = dims
    mA,nA = size(a)
    numnz = nnz(a)
    colptr = Array(Ti, nS+1)
    rowval = Array(Ti, numnz)
    nzval = reinterpret(T, a.nzval)

    _jl_sparse_compute_reshaped_colptr_and_rowval(colptr, rowval, mS, nS, a.colptr, a.rowval, mA, nA)

    return SparseMatrixCSC{T,Ti}(mS, nS, colptr, rowval, nzval)
end

function reshape{Tv,Ti}(a::SparseMatrixCSC{Tv,Ti}, dims::NTuple{2,Int})
    if prod(dims) != numel(a)
        error("reshape: invalid dimensions")
    end
    mS,nS = dims
    mA,nA = size(a)
    numnz = nnz(a)
    colptr = Array(Ti, nS+1)
    rowval = Array(Ti, numnz)
    nzval = a.nzval

    _jl_sparse_compute_reshaped_colptr_and_rowval(colptr, rowval, mS, nS, a.colptr, a.rowval, mA, nA)

    return SparseMatrixCSC{Tv,Ti}(mS, nS, colptr, rowval, nzval)
end

## Constructors

function similar(S::SparseMatrixCSC)
    T = SparseMatrixCSC(S.m, S.n, similar(S.colptr), similar(S.rowval), similar(S.nzval))
    T.colptr[end] = length(T.nzval)+1 # Used to compute nnz
end

copy(S::SparseMatrixCSC) =
    SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), copy(S.nzval))

function convert{T}(::Type{Matrix{T}}, S::SparseMatrixCSC{T})
    A = zeros(T, int(S.m), int(S.n))
    for col = 1 : S.n, k = S.colptr[col] : (S.colptr[col+1]-1)
        A[S.rowval[k], col] = S.nzval[k]
    end
    return A
end

full{T}(S::SparseMatrixCSC{T}) = convert(Matrix{T}, S)

function sparse(A::Matrix)
    m, n = size(A)
    (I, J, V) = findn_nzs(A)
    return sparse_IJsorted!(I,J,V,m,n,+)
end

sparse_IJsorted!(I,J,V,m,n) = sparse_IJsorted!(I,J,V,m,n,+)

function sparse_IJsorted!{Ti<:Union(Int32,Int64)}(I::AbstractVector{Ti}, J::AbstractVector{Ti},
                                                    V::AbstractVector,
                                                    m::Int, n::Int, combine::Function)

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

sparse(I,J,v::Number,m,n,combine::Function) = sparse(I, J, fill(v,length(I)), int(m), int(n), combine)

# Based on http://www.cise.ufl.edu/research/sparse/cholmod/CHOLMOD/Core/cholmod_triplet.c
function sparse{Tv,Ti<:Union(Int32,Int64)}(I::AbstractVector{Ti}, J::AbstractVector{Ti}, 
                                           V::AbstractVector{Tv},
                                           nrow::Int, ncol::Int, combine::Function)

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

function sprand(m::Int, n::Int, density::FloatingPoint, rng::Function)
    numnz = int(m*n*density)
    I = randival!(1, m, Array(Int32, numnz))
    J = randival!(1, n, Array(Int32, numnz))
    S = sparse(I, J, 1.0, m, n)
    S.nzval = rng(nnz(S))

    return S
end

sprand(m::Int, n::Int, density::FloatingPoint)  = sprand(m,n,density,rand)
sprandn(m::Int, n::Int, density::FloatingPoint) = sprand(m,n,density,randn)

spones{T}(S::SparseMatrixCSC{T}) =
     SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), ones(T, S.colptr[end]-1))

spzeros(m::Int) = spzeros(m, m)
spzeros(m::Int, n::Int) = spzeros(Float64, m, n)
spzeros(Tv::Type, m::Int) = spzeros(Tv, m, m)
spzeros(Tv::Type, m::Int, n::Int) =
    SparseMatrixCSC(m, n, ones(Int32, n+1), Array(Int32, 0), Array(Tv, 0))

speye(n::Int) = speye(Float64, n)
speye(T::Type, n::Int) = speye(T, n, n)
speye(m::Int, n::Int) = speye(Float64, m, n)
speye{T}(S::SparseMatrixCSC{T}) = speye(T, size(S, 1), size(S, 2))

function speye(T::Type, m::Int, n::Int)
    x = int32(min(m,n))
    rowval = [int32(1):x]
    colptr = [rowval, int32((x+1)*ones(Int32, n+1-x))]
    nzval  = ones(T, x)
    return SparseMatrixCSC(m, n, colptr, rowval, nzval)
end

function one{T}(S::SparseMatrixCSC{T})
    m,n = size(S)
    if m != n; error("Multiplicative identity only defined for square matrices!"); end;
    speye(T, m)
end

## Transpose

# Based on: http://www.cise.ufl.edu/research/sparse/CSparse/CSparse/Source/cs_transpose.c
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


## Binary operators

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

            rowvalS = del(rowvalS, colptrS[end]:length(rowvalS))
            nzvalCS = del(nzvalS, colptrS[end]:length(nzvalS))
            return SparseMatrixCSC(m, n, colptrS, rowvalS, nzvalS)
        end

    end # quote
end # macro

(+)(A::SparseMatrixCSC, B::Union(Array,Number)) = (+)(full(A), B)
(+)(A::Union(Array,Number), B::SparseMatrixCSC) = (+)(A, full(B))

(-)(A::SparseMatrixCSC, B::Union(Array,Number)) = (-)(full(A), B)
(-)(A::Union(Array,Number), B::SparseMatrixCSC) = (-)(A, full(B))

(.*)(A::SparseMatrixCSC, B::Number) = SparseMatrixCSC(A.m, A.n, copy(A.colptr), copy(A.rowval), A.nzval .* B)
(.*)(A::Number, B::SparseMatrixCSC) = SparseMatrixCSC(B.m, B.n, copy(B.colptr), copy(B.rowval), A .* B.nzval)
(.*)(A::SparseMatrixCSC, B::Array) = (.*)(A, sparse(B))
(.*)(A::Array, B::SparseMatrixCSC) = (.*)(sparse(A), B)

(./)(A::SparseMatrixCSC, B::Number) = SparseMatrixCSC(A.m, A.n, copy(A.colptr), copy(A.rowval), A.nzval ./ B)
(./)(A::Number, B::SparseMatrixCSC) = (./)(A, full(B))
(./)(A::SparseMatrixCSC, B::Array) = (./)(full(A), B)
(./)(A::Array, B::SparseMatrixCSC) = (./)(A, full(B))
(./)(A::SparseMatrixCSC, B::SparseMatrixCSC) = (./)(full(A), full(B))

(.\)(A::SparseMatrixCSC, B::Number) = (.\)(full(A), B)
(.\)(A::Number, B::SparseMatrixCSC) = SparseMatrixCSC(B.m, B.n, copy(B.colptr), copy(B.rowval), B.nzval .\ A)
(.\)(A::SparseMatrixCSC, B::Array) = (.\)(full(A), B)
(.\)(A::Array, B::SparseMatrixCSC) = (.\)(A, full(B))
(.\)(A::SparseMatrixCSC, B::SparseMatrixCSC) = (.\)(full(A), full(B))

(.^)(A::SparseMatrixCSC, B::Number) = SparseMatrixCSC(A.m, A.n, copy(A.colptr), copy(A.rowval), A.nzval .^ B)
(.^)(A::Number, B::SparseMatrixCSC) = (.^)(A, full(B))
(.^)(A::SparseMatrixCSC, B::Array) = (.^)(full(A), B)
(.^)(A::Array, B::SparseMatrixCSC) = (.^)(A, full(B))

# Reductions

# TODO: Should the results of sparse reductions be sparse?
function areduce{Tv,Ti}(f::Function, A::SparseMatrixCSC{Tv,Ti}, region::Dimspec, v0)
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

max{T}(A::SparseMatrixCSC{T}) = areduce(max,A,(1,2),typemin(T))
max{T}(A::SparseMatrixCSC{T}, b::(), region::Dimspec) = areduce(max,A,region,typemin(T))

min{T}(A::SparseMatrixCSC{T}) = areduce(min,A,(1,2),typemax(T))
min{T}(A::SparseMatrixCSC{T}, b::(), region::Dimspec) = areduce(min,A,region,typemax(T))

sum{T}(A::SparseMatrixCSC{T}) = areduce(+,A,(1,2),zero(T))
sum{T}(A::SparseMatrixCSC{T}, region::Dimspec)  = areduce(+,A,region,zero(T))

prod{T}(A::SparseMatrixCSC{T}) = areduce(*,A,(1,2),one(T))
prod{T}(A::SparseMatrixCSC{T}, region::Dimspec) = areduce(*,A,region,one(T))

#all(A::SparseMatrixCSC{Bool}, region::Dimspec) = areduce(all,A,region,true)
#any(A::SparseMatrixCSC{Bool}, region::Dimspec) = areduce(any,A,region,false)
#sum(A::SparseMatrixCSC{Bool}, region::Dimspec) = areduce(+,A,region,0,Int)
#sum(A::SparseMatrixCSC{Bool}) = nnz(A)

## ref
ref(A::SparseMatrixCSC, i::Integer) = ref(A, ind2sub(size(A),i))
ref(A::SparseMatrixCSC, I::(Integer,Integer)) = ref(A, I[1], I[2])

function ref{T}(A::SparseMatrixCSC{T}, i0::Integer, i1::Integer)
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

ref{T<:Integer}(A::SparseMatrixCSC, I::AbstractVector{T}, j::Integer) = ref(A,I,[j])
ref{T<:Integer}(A::SparseMatrixCSC, i::Integer, J::AbstractVector{T}) = ref(A,[i],J)

function ref{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector)

    (m, n) = size(A)
    nI = length(I)
    nJ = length(J)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval

    if ~issorted(I)
        (I, pI) = sortperm(I)
    else
        pI = 1:nI
    end

    # Form the structure of the result and compute space
    colptrS = Array(Ti, nJ+1)
    colptrS[1] = 1
    nnzS = 0

    for j = 1:nJ
        col = J[j]

        ptrI = 1
        for ptrA = colptrA[col]:colptrA[col+1]-1
            rowA = rowvalA[ptrA]
            
            while ptrI <= nI
                rowI = I[ptrI]
                if rowI > rowA
                    break
                elseif rowA == rowI
                    nnzS += 1
                    break
                end
                ptrI += 1
            end

        end
        colptrS[j+1] = nnzS+1

    end

    # Populate the values in the result
    rowvalS = Array(Ti, nnzS)
    nzvalS  = Array(Tv, nnzS)
    ptrS = 0

    for j = 1:nJ
        col = J[j]

        ptrI = 1
        for ptrA = colptrA[col]:colptrA[col+1]-1
            rowA = rowvalA[ptrA]
            
            while ptrI <= nI
                rowI = I[ptrI]
                if rowI > rowA
                    break
                elseif rowA == rowI
                    ptrS += 1
                    rowvalS[ptrS] = pI[ptrI]
                    nzvalS[ptrS]  = nzvalA[ptrA]
                    break
                end
                ptrI += 1
            end

        end

    end

    return SparseMatrixCSC(nI, nJ, colptrS, rowvalS, nzvalS)
end

## assign
assign(A::SparseMatrixCSC, v, i::Integer) = assign(A, v, ind2sub(size(A),i)...)

function assign{T,T_int}(A::SparseMatrixCSC{T,T_int}, v, i0::Integer, i1::Integer)
    i0 = convert(T_int, i0)
    i1 = convert(T_int, i1)
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
            del(A.rowval, loc)
            del(A.nzval, loc)
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
    insert(A.rowval, i, i0)
    insert(A.nzval, i, v)
    for j = (i1+1):(A.n+1)
        A.colptr[j] = A.colptr[j] + 1
    end
    return A
end

assign(A::SparseMatrixCSC, v::AbstractMatrix, i::Integer, J::AbstractVector) = assign(A, v, [i], J)
assign(A::SparseMatrixCSC, v::AbstractMatrix, I::AbstractVector, J::Integer) = assign(A, v, I, [j])

#TODO: assign where v is sparse
function assign{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, v::AbstractMatrix, I::AbstractVector, J::AbstractVector)
    if size(v,1) != length(I) || size(v,2) != length(J)
        return("error in assign: mismatched dimensions")
    end
    m, n = size(A,1), size(A,2)
    est = nnz(A) + numel(v)
    colptr = Array(Ti, n+1)
    colptr[:] = A.colptr[:]
    rowval = Array(Ti, est)
    nzval = Array(Tv, est)
    Js, Jp = sortperm(J)
    A_col = 1
    j = 1
    spa = SparseAccumulator(Tv, Ti, m)
    j_max = size(v,2)
    while A_col <= n
        if j > j_max
            temp2 = A.colptr[A_col]:(A.colptr[n+1]-1)
            offs = colptr[A_col]-A.colptr[A_col]
            temp1 = temp2 + offs
            colptr[A_col:(n+1)] = A.colptr[A_col:(n+1)] + offs
            rowval[temp1] = A.rowval[temp2]
            nzval[temp1] = A.nzval[temp2]
            break
        end
        if A_col < Js[j]
            temp2 = A.colptr[A_col]:(A.colptr[Js[j]]-1)
            offs = colptr[A_col]-A.colptr[A_col]
            temp1 = temp2 + offs
            colptr[A_col:Js[j]] = A.colptr[A_col:Js[j]] + offs
            rowval[temp1] = A.rowval[temp2]
            nzval[temp1] = A.nzval[temp2]
        end
        A_col = Js[j]
        _jl_spa_set(spa, A, A_col)
        spa[I] = v[:,Jp[j]]
        (rowval, nzval) = _jl_spa_store_reset(spa, A_col, colptr, rowval, nzval)
        A_col += 1
        j += 1
    end
    A.colptr = colptr
    A.rowval = rowval
    A.nzval = nzval
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


## SparseAccumulator and related functions

type SparseAccumulator{Tv,Ti} <: AbstractVector{Tv}
    vals::Vector{Tv}
    flags::Vector{Bool}
    indexes::Vector{Ti}
    nvals::Integer
end

show{T}(io, S::SparseAccumulator{T}) = invoke(show, (Any,Any), io, S)

function SparseAccumulator{Tv,Ti}(::Type{Tv}, ::Type{Ti}, s::Integer)
    SparseAccumulator(zeros(Tv,int(s)), falses(int(s)), Array(Ti,int(s)), 0)
end

SparseAccumulator(s::Integer) = SparseAccumulator(Float64, Int32, s)

length(S::SparseAccumulator) = length(S.vals)

# store spa and reset
function _jl_spa_store_reset{T}(S::SparseAccumulator{T}, col, colptr, rowval, nzval)
    vals = S.vals
    flags = S.flags
    indexes = S.indexes
    nvals = S.nvals
    z = zero(T)

    start = colptr[col]

    if nvals > length(nzval) - start
        rowval = grow(rowval, length(rowval))
        nzval = grow(nzval, length(nzval))
    end
    _jl_quicksort(indexes, 1, nvals)
    offs = 1
    for i=1:nvals
        pos = indexes[i]
        if vals[pos] != z
            rowval[start + i - offs] = pos
            nzval[start + i - offs] = vals[pos]
            vals[pos] = z
        else
            offs += 1
        end
        flags[pos] = false
    end

    colptr[col+1] = start + nvals
    S.nvals = 0
    return (rowval, nzval)
end

# Set spa S to be the i'th column of A
function _jl_spa_set{T}(S::SparseAccumulator{T}, A::SparseMatrixCSC{T}, i::Integer)
    m = A.m
    if length(S) != m; error("mismatched dimensions"); end

    z = zero(T)
    offs = A.colptr[i]-1
    nvals = A.colptr[i+1] - offs - 1
    S.indexes[1:nvals] = A.rowval[(offs+1):(offs+nvals)]
    S.nvals = nvals
    j = 1
    for k = 1:m
        if j <= nvals && k == S.indexes[j]
            S.vals[k] = A.nzval[offs+j]
            S.flags[k] = true
            j += 1
        else
            S.vals[k] = z
            S.flags[k] = false
        end
    end
    return S
end

ref{T}(S::SparseAccumulator{T}, i::Integer) = S.flags[i] ? S.vals[i] : zero(T)

function assign(S::SparseAccumulator, v, i::Integer)
    if v == 0
        if S.flags[i]
            S.vals[i] = v
            S.flags[i] = false
            #find value of i in indexes and swap it out
            j = 1
            n = S.nvals
            while j <= n
                if S.indexes[j] == i
                    S.indexes[j] = S.indexes[n]
                    S.indexes[n] = i
                    break
                end
                j += 1
            end
            if j > n; error("unexpected error in SPA assign"); end
            S.nvals -= 1
        end
    else
        if S.flags[i]
            S.vals[i] = v
        else
            S.flags[i] = true
            S.vals[i] = v
            S.nvals += 1
            S.indexes[S.nvals] = i
        end
    end
    return S
end

function mmread(filename::ASCIIString, infoonly::Bool)
#      Reads the contents of the Matrix Market file 'filename'
#      into a matrix, which will be either sparse or full,
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
        rr = Array(Int32, entries)
        cc = Array(Int32, entries)
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

## expand a colptr or rowptr into a full index vector
function expandptr{T<:Integer}(V::Vector{T})
    if V[1] != 1 error("expandptr: first index must be one") end
    res = similar(V, (int64(V[end]-1),))
    for i in 1:(length(V)-1), j in V[i]:(V[i+1] - 1) res[j] = i end
    res
end

# Based on the function cs_fkeep from the CSparse library
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
    nz -= 1
    A.colptr[A.n + 1] = nz
    if nz < nzorig
        grow(A.nzval, nz - nzorig)
        grow(A.rowval, nz - nzorig)
    end
    A
end

dropzeros!{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}) = fkeep!(A, (i,j,x,other)->x!=zero(Tv), 0)
droptol!{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, tol::Tv) = fkeep!(A, (i,j,x,other)->abs(x)>other, tol)
