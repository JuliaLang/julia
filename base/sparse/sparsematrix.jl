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

SparseMatrixCSC{Tv,Ti}(m::Integer, n::Integer, colptr::Vector{Ti}, rowval::Vector{Ti}, nzval::Vector{Tv}) =
    SparseMatrixCSC(int(m), int(n), colptr, rowval, nzval)

size(S::SparseMatrixCSC) = (S.m, S.n)
nnz(S::SparseMatrixCSC) = int(S.colptr[end]-1)

function show(io::IO, S::SparseMatrixCSC)
    print(io, S.m, "x", S.n, " sparse matrix with ", nnz(S), " ", eltype(S), " nonzeros:")

    half_screen_rows = div(Base.tty_rows() - 8, 2)
    pad = ndigits(max(S.m,S.n))
    k = 0
    sep = "\n\t"
    for col = 1:S.n, k = S.colptr[col] : (S.colptr[col+1]-1)
        if k < half_screen_rows || k > nnz(S)-half_screen_rows
            print(io, sep, '[', rpad(S.rowval[k], pad), ", ", lpad(col, pad), "]  =  ",
                    sprint(showcompact, S.nzval[k]))
        elseif k == half_screen_rows
            print(io, sep, '\u22ee')
        end
        k += 1
    end
end

## Reinterpret and Reshape

function reinterpret{T,Tv,Ti}(::Type{T}, a::SparseMatrixCSC{Tv,Ti})
    if sizeof(T) != sizeof(Tv)
        throw(ArgumentError("SparseMatrixCSC reinterpret is only supported for element types of the same size"))
    end
    mA, nA = size(a)
    colptr = copy(a.colptr)
    rowval = copy(a.rowval)
    nzval  = reinterpret(T, a.nzval)
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
        throw(ArgumentError("SparseMatrixCSC reinterpret is only supported for element types of the same size"))
    end
    if prod(dims) != length(a)
        throw(DimensionMismatch("new dimensions $(dims) must be consistent with array size $(length(a))"))
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
        throw(DimensionMismatch("new dimensions $(dims) must be consistent with array size $(length(a))"))
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

similar(S::SparseMatrixCSC, Tv::NonTupleType) =
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

function convert{Tv,Ti,TvS,TiS}(::Type{SparseMatrixCSC{Tv,Ti}}, S::SparseMatrixCSC{TvS,TiS})
    if Tv == TvS && Ti == TiS
        return S
    else
        return SparseMatrixCSC(S.m, S.n, 
                               convert(Vector{Ti},S.colptr), 
                               convert(Vector{Ti},S.rowval), 
                               convert(Vector{Tv},S.nzval))
    end
end

function convert{Tv,TvS,TiS}(::Type{SparseMatrixCSC{Tv}}, S::SparseMatrixCSC{TvS,TiS})
    if Tv == TvS
        return S
    else
        return SparseMatrixCSC(S.m, S.n,
                               S.colptr,
                               S.rowval,
                               convert(Vector{Tv},S.nzval))
    end
end

function convert{Tv,Ti}(::Type{SparseMatrixCSC{Tv,Ti}}, M::Matrix)
    m, n = size(M)
    (I, J, V) = findnz(M)
    return sparse_IJ_sorted!(convert(Vector{Ti},I), 
                             convert(Vector{Ti},J), 
                             convert(Vector{Tv},V), 
                             m, n)
end

convert(::Type{Matrix}, S::SparseMatrixCSC) = full(S)

function full{Tv}(S::SparseMatrixCSC{Tv})
    A = zeros(Tv, S.m, S.n)
    for col = 1 : S.n, k = S.colptr[col] : (S.colptr[col+1]-1)
        A[S.rowval[k], col] = S.nzval[k]
    end
    return A
end

float(S::SparseMatrixCSC) = SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), float(copy(S.nzval)))

complex(S::SparseMatrixCSC) = SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), complex(copy(S.nzval)))

complex(A::SparseMatrixCSC, B::SparseMatrixCSC) = A + im*B

# Construct a sparse vector

sparsevec{K<:Integer,V}(d::Dict{K,V}, len::Int) = sparsevec(collect(keys(d)), collect(values(d)), len)

sparsevec{K<:Integer,V}(d::Dict{K,V}) = sparsevec(collect(keys(d)), collect(values(d)))

sparsevec(I::AbstractVector, V, m::Integer) = sparsevec(I, V, m, +)

sparsevec(I::AbstractVector, V) = sparsevec(I, V, maximum(I), +)

function sparsevec(I::AbstractVector, V, m::Integer, combine::Function)
    nI = length(I)
    if isa(V, Number); V = fill(V, nI); end
    p = sortperm(I)
    I = I[p]
    m >= I[end] || throw(DimensionMismatch("indices cannot be larger than length of vector"))
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

## Construct a sparse matrix

sparse{Tv}(A::Matrix{Tv}) = convert(SparseMatrixCSC{Tv,Int}, A)

sparse(S::SparseMatrixCSC) = copy(S)

sparse_IJ_sorted!(I,J,V,m,n) = sparse_IJ_sorted!(I,J,V,m,n,+)

sparse_IJ_sorted!(I,J,V::AbstractVector{Bool},m,n) = sparse_IJ_sorted!(I,J,V,m,n,|)

function sparse_IJ_sorted!{Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti},
                                        V::AbstractVector,
                                        m::Integer, n::Integer, combine::Function)

    m = m < 0 ? 0 : m
    n = n < 0 ? 0 : n
    if length(V) == 0; return spzeros(eltype(V),Ti,m,n); end

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

## sparse() can take its inputs in unsorted order (the parent method is now in jlsparse.jl)

dimlub(I) = length(I)==0 ? 0 : int(maximum(I)) #least upper bound on required sparse matrix dimension

sparse(I,J,v::Number) = sparse(I, J, fill(v,length(I)), dimlub(I), dimlub(J), +)

sparse(I,J,V::AbstractVector) = sparse(I, J, V, dimlub(I), dimlub(J), +)

sparse(I,J,v::Number,m,n) = sparse(I, J, fill(v,length(I)), int(m), int(n), +)

sparse(I,J,V::AbstractVector,m,n) = sparse(I, J, V, int(m), int(n), +)

sparse(I,J,V::AbstractVector{Bool},m,n) = sparse(I, J, V, int(m), int(n), |)

sparse(I,J,v::Number,m,n,combine::Function) = sparse(I, J, fill(v,length(I)), int(m), int(n), combine)

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
        if S.nzval[k] != zero(Tv)
            I[count] = S.rowval[k]
            J[count] = col
            count += 1
        else
            Base.warn_once("sparse matrix contains explicit stored zeros")
        end
    end

    count -= 1
    if numnz != count
        I = I[1:count]
        J = J[1:count]
    end

    return (I, J)
end

function findnz{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti})
    numnz = nnz(S)
    I = Array(Ti, numnz)
    J = Array(Ti, numnz)
    V = Array(Tv, numnz)

    count = 1
    for col = 1 : S.n, k = S.colptr[col] : (S.colptr[col+1]-1)
        if S.nzval[k] != zero(Tv)
            I[count] = S.rowval[k]
            J[count] = col
            V[count] = S.nzval[k]
            count += 1
        else
            Base.warn_once("sparse matrix contains explicit stored zeros")
        end
    end

    count -= 1
    if numnz != count
        I = I[1:count]
        J = J[1:count]
        V = V[1:count]
    end

    return (I, J, V)
end

function sprand(m::Integer, n::Integer, density::FloatingPoint, rng::Function, v)
    0 <= density <= 1 || throw(ArgumentError("density must be between 0 and 1"))
    N = n*m
    # if density < 0.5, we'll randomly generate the indices to set
    #        otherwise, we'll randomly generate the indices to skip
    K = (density > 0.5) ? N*(1-density) : N*density
    # Use Newton's method to invert the birthday problem
    l = log(1.0-1.0/N)
    k = K
    k = k + ((1-K/N)*exp(-k*l) - 1)/l
    k = k + ((1-K/N)*exp(-k*l) - 1)/l # for K<N/2, 2 iterations suffice
    ik = int(k)
    ind = sort(rand(1:N, ik))
    uind = Array(Int, 0)   # unique indices
    sizehint(uind, int(N*density))
    if density < 0.5
        if ik == 0
            return sparse(Int[],Int[],Array(eltype(v),0),m,n)
        end
        j = ind[1]
        push!(uind, j)
        uj = j
        for i = 2:length(ind)
            j = ind[i]
            if j != uj
                push!(uind, j)
                uj = j
            end
        end
    else
        push!(ind, N+1) # sentinel
        ii = 1
        for i = 1:N
            if i != ind[ii]
                push!(uind, i)
            else
                while (i == ind[ii])
                    ii += 1
                end
            end
        end
    end
    I, J = ind2sub((m,n), uind)
    if !iseltype(v,Bool)
        return sparse_IJ_sorted!(I, J, rng(length(uind)), m, n, +)  # it will never need to combine
    else
        return sparse_IJ_sorted!(I, J, trues(length(uind)), m, n, +)
    end
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
spzeros(Tv::Type, Ti::Type, m::Integer, n::Integer) =
    SparseMatrixCSC(m, n, ones(Ti, n+1), Array(Ti, 0), Array(Tv, 0))

speye(n::Integer) = speye(Float64, n)
speye(T::Type, n::Integer) = speye(T, n, n)
speye(m::Integer, n::Integer) = speye(Float64, m, n)
speye{T}(S::SparseMatrixCSC{T}) = speye(T, size(S, 1), size(S, 2))
eye(S::SparseMatrixCSC) = speye(S)

function speye(T::Type, m::Integer, n::Integer)
    x = min(m,n)
    rowval = [1:x]
    colptr = [rowval, fill(int(x+1), n+1-x)]
    nzval  = ones(T, x)
    return SparseMatrixCSC(m, n, colptr, rowval, nzval)
end

function one{T}(S::SparseMatrixCSC{T})
    m,n = size(S)
    if m != n; throw(DimensionMismatch("multiplicative identity only defined for square matrices")); end
    speye(T, m)
end

## Unary arithmetic and boolean operators

for op in (:-, )
    @eval begin

        function ($op)(A::SparseMatrixCSC)
            B = copy(A)
            nzvalB = B.nzval
            for i=1:length(nzvalB)
                nzvalB[i] = ($op)(nzvalB[i])
            end
            return B
        end
        
    end
end

## Binary arithmetic and boolean operators

for (op, restype) in ( (:+, Nothing), (:-, Nothing), (:.*, Nothing), (:.^, Nothing), 
                       (:(.<), Bool) )
    @eval begin

        function ($op){TvA,TiA,TvB,TiB}(A::SparseMatrixCSC{TvA,TiA}, B::SparseMatrixCSC{TvB,TiB})
            Tv = promote_type(TvA, TvB)
            Ti = promote_type(TiA, TiB)
            A  = convert(SparseMatrixCSC{Tv,Ti}, A)
            B  = convert(SparseMatrixCSC{Tv,Ti}, B)
            return ($op)(A, B)
        end

        function ($op){Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti})
            if size(A,1) != size(B,1) || size(A,2) != size(B,2)
                throw(DimensionMismatch(""))
            end

            (m, n) = size(A)

            # TODO: Need better method to estimate result space
            nnzS = nnz(A) + nnz(B)
            colptrS = Array(Ti, A.n+1)
            rowvalS = Array(Ti, nnzS)
            if $restype == Nothing
                nzvalS = Array(Tv, nnzS)
            else
                nzvalS = Array($restype, nnzS)
            end

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

            splice!(rowvalS, colptrS[end]:length(rowvalS))
            splice!(nzvalS, colptrS[end]:length(nzvalS))
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

(.<)(A::SparseMatrixCSC, B::Number) = (.<)(full(A), B)
(.<)(A::Number, B::SparseMatrixCSC) = (.<)(A, full(B))

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
        throw(ArgumentError("invalid value for region; must be 1, 2, or (1,2)"))
    end
end

maximum{T}(A::SparseMatrixCSC{T}) =
    isempty(A) ? throw(ArgumentError("argument must not be empty")) : reducedim(scalarmax,A,(1,2),typemin(T))
maximum{T}(A::SparseMatrixCSC{T}, region) =
    isempty(A) ? similar(A, reduced_dims0(A,region)) : reducedim(scalarmax,A,region,typemin(T))

minimum{T}(A::SparseMatrixCSC{T}) =
    isempty(A) ? throw(ArgumentError("argument must not be empty")) : reducedim(scalarmin,A,(1,2),typemax(T))
minimum{T}(A::SparseMatrixCSC{T}, region) =
    isempty(A) ? similar(A, reduced_dims0(A,region)) : reducedim(scalarmin,A,region,typemax(T))

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
    if !(1 <= i0 <= A.m && 1 <= i1 <= A.n); throw(BoundsError()); end
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

# logical getindex

getindex(A::SparseMatrixCSC, I::Integer, J::AbstractVector{Bool}) = A[I,find(J)]
getindex(A::SparseMatrixCSC, I::AbstractVector{Bool}, J::Integer) = A[find(I),J]
getindex(A::SparseMatrixCSC, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = A[find(I),find(J)]
getindex{T<:Integer}(A::SparseMatrixCSC, I::AbstractVector{T}, J::AbstractVector{Bool}) = A[I,find(J)]
getindex{T<:Integer}(A::SparseMatrixCSC, I::AbstractVector{Bool}, J::AbstractVector{T}) = A[find(I),J]

## setindex!
setindex!(A::SparseMatrixCSC, v, i::Integer) = setindex!(A, v, ind2sub(size(A),i)...)

function setindex!{T,Ti}(A::SparseMatrixCSC{T,Ti}, v, i0::Integer, i1::Integer)
    i0 = convert(Ti, i0)
    i1 = convert(Ti, i1)
    if !(1 <= i0 <= A.m && 1 <= i1 <= A.n); throw(BoundsError()); end
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
            splice!(A.rowval, loc)
            splice!(A.nzval, loc)
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

setindex!{T<:Integer}(A::SparseMatrixCSC, v::AbstractMatrix, i::Integer, J::AbstractVector{T}) = setindex!(A, v, [i], J)
setindex!{T<:Integer}(A::SparseMatrixCSC, v::AbstractMatrix, I::AbstractVector{T}, j::Integer) = setindex!(A, v, I, [j])

setindex!{Tv,T<:Integer}(A::SparseMatrixCSC{Tv}, x::Number, I::AbstractVector{T}, J::AbstractVector{T}) =
      setindex!(A, fill(x::Tv, length(I), length(J)), I, J)

setindex!{Tv,Ti,T<:Integer}(A::SparseMatrixCSC{Tv,Ti}, S::Matrix, I::AbstractVector{T}, J::AbstractVector{T}) =
      setindex!(A, convert(SparseMatrixCSC{Tv,Ti}, S), I, J)

# A[I,J] = B
function setindex!{Tv,Ti,T<:Integer}(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti}, I::AbstractVector{T}, J::AbstractVector{T})
    if size(B,1) != length(I) || size(B,2) != length(J)
        throw(DimensionMismatch(""))
    end

    issortedI = issorted(I)
    issortedJ = issorted(J)

    if !issortedI && !issortedJ
        pI = sortperm(I); I = I[pI]
        pJ = sortperm(J); J = J[pJ]
        B = B[pI, pJ]
    elseif !issortedI
        pI = sortperm(I); I = I[pI]
        B = B[pI,:]
    else !issortedJ
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

    splice!(rowvalS, colptrS[end]:length(rowvalS))
    splice!(nzvalS, colptrS[end]:length(nzvalS))

    A.colptr = colptrS
    A.rowval = rowvalS
    A.nzval = nzvalS
    return A
end

# Logical setindex!

setindex!(A::SparseMatrixCSC, x::Matrix, I::Integer, J::AbstractVector{Bool}) = setindex!(A, sparse(x), I, find(J))
setindex!(A::SparseMatrixCSC, x::Matrix, I::AbstractVector{Bool}, J::Integer) = setindex!(A, sparse(x), find(I), J)
setindex!(A::SparseMatrixCSC, x::Matrix, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = setindex!(A, sparse(x), find(I), find(J))
setindex!{T<:Integer}(A::SparseMatrixCSC, x::Matrix, I::AbstractVector{T}, J::AbstractVector{Bool}) = setindex!(A, sparse(x), I, find(J))
setindex!{T<:Integer}(A::SparseMatrixCSC, x::Matrix, I::AbstractVector{Bool}, J::AbstractVector{T}) = setindex!(A, sparse(x), find(I),J)

setindex!(A::Matrix, x::SparseMatrixCSC, I::Integer, J::AbstractVector{Bool}) = setindex!(A, full(x), I, find(J))
setindex!(A::Matrix, x::SparseMatrixCSC, I::AbstractVector{Bool}, J::Integer) = setindex!(A, full(x), find(I), J)
setindex!(A::Matrix, x::SparseMatrixCSC, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = setindex!(A, full(x), find(I), find(J))
setindex!{T<:Integer}(A::Matrix, x::SparseMatrixCSC, I::AbstractVector{T}, J::AbstractVector{Bool}) = setindex!(A, full(x), I, find(J))
setindex!{T<:Integer}(A::Matrix, x::SparseMatrixCSC, I::AbstractVector{Bool}, J::AbstractVector{T}) = setindex!(A, full(x), find(I), J)

# Sparse concatenation

function vcat(X::SparseMatrixCSC...)
    num = length(X)
    mX = [ size(x, 1) for x in X ] 
    nX = [ size(x, 2) for x in X ]
    n = nX[1]
    for i = 2 : num
        if nX[i] != n; throw(DimensionMismatch("")); end
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
        if mX[i] != m; throw(DimensionMismatch("")); end
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

## Structure query functions

function issym(A::SparseMatrixCSC)
    m, n = size(A)
    if m != n; return false; end
    return nnz(A - A.') == 0
end

function ishermitian(A::SparseMatrixCSC)
    m, n = size(A)
    if m != n; return false; end
    return nnz(A - A') == 0
end

function istriu{Tv}(A::SparseMatrixCSC{Tv})
    for col = 1:min(A.n,A.m-1)
        l1 = A.colptr[col+1]-1
        for i = 0 : (l1 - A.colptr[col])
            if A.rowval[l1-i] <= col
                break
            end
            if A.nzval[l1-i] != zero(Tv)
                return false
            end
        end
    end
    return true
end

function istril{Tv}(A::SparseMatrixCSC{Tv})
    for col = 2:A.n
        for i = A.colptr[col] : (A.colptr[col+1]-1)
            if A.rowval[i] >= col
                break
            end
            if A.nzval[i] != zero(Tv)
                return false
            end
        end
    end
    return true
end

# Create a sparse diagonal matrix by specifying multiple diagonals 
# packed into a tuple, alongside their diagonal offsets and matrix shape

function spdiagm_internal(B, d)
    ndiags = length(d)
    if length(B) != ndiags; throw(ArgumentError("first argument should be a tuple of length(d)=$ndiags arrays of diagonals")); end
    ncoeffs = 0
    for vec in B
        ncoeffs += length(vec)
    end
    I = Array(Int, ncoeffs)
    J = Array(Int, ncoeffs)
    V = Array(eltype(B[1]), ncoeffs)
    id = 0
    i = 0
    for vec in B
        id += 1
        diag = d[id]
        numel = length(vec)
        if diag < 0
            row = -diag
            col = 0
        elseif diag > 0
            row = 0
            col = diag
        else
            row = 0
            col = 0
        end
        range = 1+i:numel+i
        I[range] = row+1:row+numel
        J[range] = col+1:col+numel
        copy!(sub(V, range), vec)
        i += numel
    end

    return (I,J,V)
end

function spdiagm(B, d, m::Integer, n::Integer)
    (I,J,V) = spdiagm_internal(B, d)
    return sparse(I,J,V,m,n)
end

function spdiagm(B, d)
    (I,J,V) = spdiagm_internal(B, d)
    return sparse(I,J,V)
end

spdiagm(B::AbstractVector, d::Number, m::Integer, n::Integer) = spdiagm((B,), (d,), m, n)

spdiagm(B::AbstractVector, d::Number=0) = spdiagm((B,), (d,))

## expand a colptr or rowptr into a dense index vector
function expandptr{T<:Integer}(V::Vector{T})
    if V[1] != 1 throw(ArgumentError("first index must be one")) end
    res = similar(V, (int64(V[end]-1),))
    for i in 1:(length(V)-1), j in V[i]:(V[i+1] - 1) res[j] = i end
    res
end

## diag and related using an iterator

type SpDiagIterator{Tv,Ti}
    A::SparseMatrixCSC{Tv,Ti}
    n::Int
end
SpDiagIterator(A::SparseMatrixCSC) = SpDiagIterator(A,minimum(size(A)))

length(d::SpDiagIterator) = d.n
start(d::SpDiagIterator) = 1
done(d::SpDiagIterator, j) = j > d.n

function next{Tv}(d::SpDiagIterator{Tv}, j)
    p = d.A.colptr; i = d.A.rowval;
    first = p[j]
    last = p[j+1]-1
    while first <= last
        mid = (first + last) >> 1
        r = i[mid]
        if r == j
            return (d.A.nzval[mid], j+1)
        elseif r > j
            last = mid - 1
        else
            first = mid + 1
        end
    end
    return (zero(Tv), j+1)
end

function trace{Tv}(A::SparseMatrixCSC{Tv})
    if size(A,1) != size(A,2)
        throw(DimensionMismatch("expected square matrix"))
    end
    s = zero(Tv)
    for d in SpDiagIterator(A)
        s += d
    end
    s
end

diag(A::SparseMatrixCSC) = [d for d in SpDiagIterator(A)]

function diagm{Tv,Ti}(v::SparseMatrixCSC{Tv,Ti})
    if (size(v,1) != 1 && size(v,2) != 1)
        throw(DimensionMismatch("input should be nx1 or 1xn"))
    end

    n = length(v)
    numnz = nnz(v)
    colptr = Array(Ti, n+1)
    rowval = Array(Ti, numnz)
    nzval = Array(Tv, numnz)

    if size(v,1) == 1
        copy!(colptr, 1, v.colptr, 1, n+1)
        ptr = 1
        for col = 1:n
            if colptr[col] != colptr[col+1]
                rowval[ptr] = col
                nzval[ptr] = v.nzval[ptr]
                ptr += 1
            end
        end
    else
        copy!(rowval, 1, v.rowval, 1, numnz)
        copy!(nzval, 1, v.nzval, 1, numnz)
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
