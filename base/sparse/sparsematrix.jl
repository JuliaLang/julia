# Compressed sparse columns data structure
# Assumes that no zeros are stored in the data structure
# Assumes that row values in rowval for each column are sorted
#      issorted(rowval[colptr[i]:(colptr[i+1]-1)]) == true

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
countnz(S::SparseMatrixCSC) = countnz(S.nzval)

nonzeros(S::SparseMatrixCSC) = S.nzval
rowvals(S::SparseMatrixCSC) = S.rowval
nzrange(S::SparseMatrixCSC, col::Integer) = S.colptr[col]:(S.colptr[col+1]-1)

function Base.showarray(io::IO, S::SparseMatrixCSC;
                        header::Bool=true, limit::Bool=Base._limit_output,
                        rows = Base.tty_size()[1], repr=false)
    # TODO: repr?

    if header
        print(io, S.m, "x", S.n, " sparse matrix with ", nnz(S), " ", eltype(S), " entries:")
    end

    if limit
        half_screen_rows = div(rows - 8, 2)
    else
        half_screen_rows = typemax(Int)
    end
    pad = ndigits(max(S.m,S.n))
    k = 0
    sep = "\n\t"
    for col = 1:S.n, k = S.colptr[col] : (S.colptr[col+1]-1)
        if k < half_screen_rows || k > nnz(S)-half_screen_rows
            print(io, sep, '[', rpad(S.rowval[k], pad), ", ", lpad(col, pad), "]  =  ")
            showcompact(io, S.nzval[k])
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

function sparse_compute_reshaped_colptr_and_rowval{Ti}(colptrS::Vector{Ti}, rowvalS::Vector{Ti}, mS::Int, nS::Int, colptrA::Vector{Ti}, rowvalA::Vector{Ti}, mA::Int, nA::Int)
    lrowvalA = length(rowvalA)
    maxrowvalA = (lrowvalA > 0) ? maximum(rowvalA) : zero(Ti)
    ((length(colptrA) == (nA+1)) && (maximum(colptrA) <= (lrowvalA+1)) && (maxrowvalA <= mA)) || throw(BoundsError())

    colptrS[1] = 1
    colA = 1
    colS = 1
    ptr = 1

    @inbounds while colA <= nA
        offsetA = (colA - 1) * mA
        while ptr <= colptrA[colA+1]-1
            rowA = rowvalA[ptr]
            i = offsetA + rowA - 1
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
    @inbounds while colS <= nS
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
    rowval = similar(a.rowval)
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
    rowval = similar(a.rowval)
    nzval = copy(a.nzval)

    sparse_compute_reshaped_colptr_and_rowval(colptr, rowval, mS, nS, a.colptr, a.rowval, mA, nA)

    return SparseMatrixCSC{Tv,Ti}(mS, nS, colptr, rowval, nzval)
end

## Constructors

copy(S::SparseMatrixCSC) =
    SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), copy(S.nzval))

similar(S::SparseMatrixCSC, Tv::NonTupleType=eltype(S))   = SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), Array(Tv, length(S.nzval)))
similar{Tv,Ti,TvNew}(S::SparseMatrixCSC{Tv,Ti}, ::Type{TvNew}, ::Type{Ti}) = similar(S, TvNew)
similar{Tv,Ti,TvNew,TiNew}(S::SparseMatrixCSC{Tv,Ti}, ::Type{TvNew}, ::Type{TiNew}) = SparseMatrixCSC(S.m, S.n, convert(Array{TiNew},S.colptr), convert(Array{TiNew}, S.rowval), Array(TvNew, length(S.nzval)))
similar{Tv}(S::SparseMatrixCSC, ::Type{Tv}, d::NTuple{Integer}) = spzeros(Tv, d...)

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
convert{T}(::Type{AbstractMatrix{T}}, A::SparseMatrixCSC) = convert(SparseMatrixCSC{T}, A)
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

function vec{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti})
    colptr = Array(Ti,2)
    rowval = similar(S.rowval)
    lS = length(S)
    sparse_compute_reshaped_colptr_and_rowval(colptr, rowval, lS, 1, S.colptr, S.rowval, S.m, S.n)
    SparseMatrixCSC{Tv,Ti}(lS, 1, colptr, rowval, copy(S.nzval))
end

sparsevec(A::AbstractMatrix) = reshape(sparse(A), (length(A),1))
sparsevec(S::SparseMatrixCSC) = vec(S)

sparsevec{K<:Integer,V}(d::Dict{K,V}, len::Int) = sparsevec(collect(keys(d)), collect(values(d)), len)

sparsevec{K<:Integer,V}(d::Dict{K,V}) = sparsevec(collect(keys(d)), collect(values(d)))

sparsevec(I::AbstractVector, V, m::Integer) = sparsevec(I, V, m, +)

sparsevec(I::AbstractVector, V) = sparsevec(I, V, maximum(I), +)

function sparsevec(I::AbstractVector, V, m::Integer, combine::Function)
    nI = length(I)
    if isa(V, Number); V = fill(V, nI); end
    p = sortperm(I)
    @inbounds I = I[p]
    (nI==0 || m >= I[end]) || throw(DimensionMismatch("indices cannot be larger than length of vector"))
    (nI==0 || I[1] > 0) || throw(BoundsError())
    V = V[p]
    sparse_IJ_sorted!(I, ones(eltype(I), nI), V, m, 1, combine)
end

function sparsevec(a::Vector)
    n = length(a)
    I = find(a)
    J = ones(Int, n)
    V = a[I]
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
    L = length(I)

    @inbounds for k=2:L
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
    if ndups > 0.2*L
        numnz = L-ndups
        deleteat!(I, (numnz+1):L)
        deleteat!(V, (numnz+1):length(V))
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
    @inbounds for col = 1 : S.n, k = S.colptr[col] : (S.colptr[col+1]-1)
        if S.nzval[k] != 0
            I[count] = S.rowval[k]
            J[count] = col
            count += 1
        end
    end

    count -= 1
    if numnz != count
        deleteat!(I, (count+1):numnz)
        deleteat!(J, (count+1):numnz)
    end

    return (I, J)
end

function findnz{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti})
    numnz = nnz(S)
    I = Array(Ti, numnz)
    J = Array(Ti, numnz)
    V = Array(Tv, numnz)

    count = 1
    @inbounds for col = 1 : S.n, k = S.colptr[col] : (S.colptr[col+1]-1)
        if S.nzval[k] != 0
            I[count] = S.rowval[k]
            J[count] = col
            V[count] = S.nzval[k]
            count += 1
        end
    end

    count -= 1
    if numnz != count
        deleteat!(I, (count+1):numnz)
        deleteat!(J, (count+1):numnz)
        deleteat!(V, (count+1):numnz)
    end

    return (I, J, V)
end

function sprand{T}(m::Integer, n::Integer, density::FloatingPoint,
                   rng::Function,::Type{T}=eltype(rng(1)))
    0 <= density <= 1 || throw(ArgumentError("$density not in [0,1]"))
    N = n*m
    N == 0 && return spzeros(T,m,n)
    N == 1 && return rand() <= density ? sparse(rng(1)) : spzeros(T,1,1)

    I, J = Array(Int, 0), Array(Int, 0) # indices of nonzero elements
    sizehint!(I, int(N*density))
    sizehint!(J, int(N*density))

    # density of nonzero columns:
    L = log1p(-density)
    coldensity = -expm1(m*L) # = 1 - (1-density)^m
    colsparsity = exp(m*L) # = 1 - coldensity
    L = 1/L

    rows = Array(Int, 0)
    for j in randsubseq(1:n, coldensity)
        # To get the right statistics, we *must* have a nonempty column j
        # even if p*m << 1.   To do this, we use an approach similar to
        # the one in randsubseq to compute the expected first nonzero row k,
        # except given that at least one is nonzero (via Bayes' rule);
        # carefully rearranged to avoid excessive roundoff errors.
        k = ceil(log(colsparsity + rand()*coldensity) * L)
        ik = k < 1 ? 1 : k > m ? m : int(k) # roundoff-error/underflow paranoia
        randsubseq!(rows, 1:m-ik, density)
        push!(rows, m-ik+1)
        append!(I, rows)
        nrows = length(rows)
        Jlen = length(J)
        resize!(J, Jlen+nrows)
        @inbounds for i = Jlen+1:length(J)
            J[i] = j
        end
    end
    return sparse_IJ_sorted!(I, J, rng(length(I)), m, n, +)  # it will never need to combine
end

sprand(m::Integer, n::Integer, density::FloatingPoint) = sprand(m,n,density,rand,Float64)
sprandn(m::Integer, n::Integer, density::FloatingPoint) = sprand(m,n,density,randn,Float64)
truebools(n::Integer) = ones(Bool, n)
sprandbool(m::Integer, n::Integer, density::FloatingPoint) = sprand(m,n,density,truebools,Bool)

spones{T}(S::SparseMatrixCSC{T}) =
     SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), ones(T, S.colptr[end]-1))

spzeros(m::Integer, n::Integer) = spzeros(Float64, m, n)
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

# Operations that may map nonzeros to zero, and zero to zero
# Result is sparse
for op in (:ceil, :floor, :trunc, :round,
           :sin, :tan, :asin, :atan,
           :sinh, :tanh, :asinh, :atanh,
           :sinpi, :cosc,
           :sind, :tand, :asind, :atand)
    @eval begin
        function ($op){Tv,Ti}(A::SparseMatrixCSC{Tv,Ti})
            nfilledA = nnz(A)
            colptrB = Array(Ti, A.n+1)
            rowvalB = Array(Ti, nfilledA)
            nzvalB = Array(Tv, nfilledA)

            k = 0 # number of additional zeros introduced by op(A)
            @inbounds for i = 1 : A.n
                colptrB[i] = A.colptr[i] - k
                for j = A.colptr[i] : A.colptr[i+1]-1
                    opAj = $(op)(A.nzval[j])
                    if opAj == 0
                        k += 1
                    else
                        rowvalB[j - k] = A.rowval[j]
                        nzvalB[j - k] = opAj
                    end
                end
            end
            colptrB[end] = A.colptr[end] - k
            deleteat!(rowvalB, colptrB[end]:nfilledA)
            deleteat!(nzvalB, colptrB[end]:nfilledA)
            return SparseMatrixCSC(A.m, A.n, colptrB, rowvalB, nzvalB)
        end
    end # quote
end # macro

for op in (:ceil, :floor, :trunc, :round)
    @eval begin
        function ($op){T,Tv,Ti}(::Type{T},A::SparseMatrixCSC{Tv,Ti})
            nfilledA = nnz(A)
            colptrB = Array(Ti, A.n+1)
            rowvalB = Array(Ti, nfilledA)
            nzvalB = Array(T, nfilledA)

            k = 0 # number of additional zeros introduced by op(A)
            @inbounds for i = 1 : A.n
                colptrB[i] = A.colptr[i] - k
                for j = A.colptr[i] : A.colptr[i+1]-1
                    opAj = $(op)(A.nzval[j])
                    if opAj == 0
                        k += 1
                    else
                        rowvalB[j - k] = A.rowval[j]
                        nzvalB[j - k] = opAj
                    end
                end
            end
            colptrB[end] = A.colptr[end] - k
            deleteat!(rowvalB, colptrB[end]:nfilledA)
            deleteat!(nzvalB, colptrB[end]:nfilledA)
            return SparseMatrixCSC(A.m, A.n, colptrB, rowvalB, nzvalB)
        end

    end # quote
end # macro



# Operations that map nonzeros to nonzeros, and zeros to zeros
# Result is sparse
for op in (:-, :abs, :abs2, :log1p, :expm1)
    @eval begin

        function ($op)(A::SparseMatrixCSC)
            B = similar(A)
            nzvalB = B.nzval
            nzvalA = A.nzval
            @simd for i=1:length(nzvalB)
                @inbounds nzvalB[i] = ($op)(nzvalA[i])
            end
            return B
        end

    end
end

function conj!(A::SparseMatrixCSC)
    nzvalA = A.nzval
    @simd for i=1:length(nzvalA)
        @inbounds nzvalA[i] = conj(nzvalA[i])
    end
    return A
end

conj(A::SparseMatrixCSC) = conj!(copy(A))

# Operations that map nonzeros to nonzeros, and zeros to nonzeros
# Result is dense
for op in (:cos, :cosh, :acos, :sec, :csc, :cot, :acot, :sech,
           :csch, :coth, :asech, :acsch, :cospi, :sinc, :cosd,
           :cotd, :cscd, :secd, :acosd, :acotd, :log, :log2, :log10,
           :exp, :exp2, :exp10)
    @eval begin

        function ($op){Tv}(A::SparseMatrixCSC{Tv})
            B = fill($(op)(zero(Tv)), size(A))
            @inbounds for col = 1 : A.n
                for j = A.colptr[col] : A.colptr[col+1]-1
                    row = A.rowval[j]
                    nz = A.nzval[j]
                    B[row,col] = $(op)(nz)
                end
            end
            return B
        end

    end
end

## Binary arithmetic and boolean operators

for (op, restype) in ( (:+, Void), (:-, Void), (:.*, Void),
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
            nzvalS = Array($(restype==Void ? (:Tv) : restype), nnzS)

            z = zero(Tv)

            colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
            colptrB = B.colptr; rowvalB = B.rowval; nzvalB = B.nzval

            ptrS = 1
            colptrS[1] = 1

            @inbounds for col = 1:n
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

            deleteat!(rowvalS, colptrS[end]:length(rowvalS))
            deleteat!(nzvalS, colptrS[end]:length(nzvalS))
            return SparseMatrixCSC(m, n, colptrS, rowvalS, nzvalS)
        end

    end # quote
end # macro

(.+)(A::SparseMatrixCSC, B::Number) = full(A) .+ B
( +)(A::SparseMatrixCSC, B::Array ) = full(A)  + B
(.+)(A::Number, B::SparseMatrixCSC) = A .+ full(B)
( +)(A::Array , B::SparseMatrixCSC) = A  + full(B)

(.-)(A::SparseMatrixCSC, B::Number) = full(A) .- B
( -)(A::SparseMatrixCSC, B::Array ) = full(A)  - B
(.-)(A::Number, B::SparseMatrixCSC) = A .- full(B)
( -)(A::Array , B::SparseMatrixCSC) = A  - full(B)

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

(.^)(A::SparseMatrixCSC, B::Number) =
    B==0 ? sparse(ones(typeof(one(eltype(A)).^B), A.m, A.n)) :
           SparseMatrixCSC(A.m, A.n, copy(A.colptr), copy(A.rowval), A.nzval .^ B)
(.^)(A::Number, B::SparseMatrixCSC) = (.^)(A, full(B))
(.^)(A::SparseMatrixCSC, B::Array) = (.^)(full(A), B)
(.^)(A::Array, B::SparseMatrixCSC) = (.^)(A, full(B))

(.<)(A::SparseMatrixCSC, B::Number) = (.<)(full(A), B)
(.<)(A::Number, B::SparseMatrixCSC) = (.<)(A, full(B))

# Reductions

# TODO: Should the results of sparse reductions be sparse?
function reducedim{Tv,Ti}(f::Function, A::SparseMatrixCSC{Tv,Ti}, region, v0)
    if region == 1 || region == (1,)

        S = Array(Tv, 1, A.n)
        @inbounds for i = 1 : A.n
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

    elseif region == 2 || region == (2,)

        S = fill(v0, A.m, 1)
        rcounts = zeros(Ti, A.m)
        @inbounds for i = 1 : A.n, j = A.colptr[i] : A.colptr[i+1]-1
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
        @inbounds for i = 1 : A.n, j = A.colptr[i] : A.colptr[i+1]-1
            S = f(S, A.nzval[j])
        end
        if nnz(A) != A.m*A.n; S = f(S, zero(Tv)); end

        return fill(S, 1, 1)

    else
        throw(ArgumentError("invalid value for region; must be 1, 2, or (1,2)"))
    end
end

function maximum{T}(A::SparseMatrixCSC{T})
    isempty(A) && throw(ArgumentError("argument must not be empty"))
    m = maximum(A.nzval)
    nnz(A)!=length(A) ? max(m,zero(T)) : m
end

maximum{T}(A::SparseMatrixCSC{T}, region) =
    isempty(A) ? similar(A, reduced_dims0(A,region)) : reducedim(Base.scalarmax,A,region,typemin(T))

function minimum{T}(A::SparseMatrixCSC{T})
    isempty(A) && throw(ArgumentError("argument must not be empty"))
    m = minimum(A.nzval)
    nnz(A)!=length(A) ? min(m,zero(T)) : m
end

minimum{T}(A::SparseMatrixCSC{T}, region) =
    isempty(A) ? similar(A, reduced_dims0(A,region)) : reducedim(Base.scalarmin,A,region,typemax(T))

# findmax/min and indmax/min methods
function _findz{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, rows=1:A.m, cols=1:A.n)
    colptr = A.colptr; rowval = A.rowval; nzval = A.nzval
    zval = zero(Tv)
    col = cols[1]; row = 0
    rowmin = rows[1]; rowmax = rows[end]
    allrows = (rows == 1:A.m)
    @inbounds while col <= cols[end]
        r1::Int = colptr[col]
        r2::Int = colptr[col+1] - 1
        if !allrows && (r1 <= r2)
            r1 = searchsortedfirst(rowval, rowmin, r1, r2, Forward)
            (r1 <= r2 ) && (r2 = searchsortedlast(rowval, rowmax, r1, r2, Forward) + 1)
        end
        row = rowmin
        (r1 > r2) && (return sub2ind(size(A),row,col))
        while (r1 <= r2) && (row == rowval[r1]) && (nzval[r1] != zval)
            r1 += 1
            row += 1
        end
        (row <= rowmax) && (return sub2ind(size(A),row,col))
        col += 1
    end
    return 0
end

macro _findr(op, A, region, Tv, Ti)
    esc(quote
    N = nnz($A)
    L = length($A)
    (L == 0) && error("array must be non-empty")

    colptr = $A.colptr; rowval = $A.rowval; nzval = $A.nzval; m = $A.m; n = $A.n
    zval = zero($Tv)
    szA = size($A)

    if $region == 1 || $region == (1,)
        (N == 0) && (return (fill(zval,1,n), fill(convert($Ti,1),1,n)))
        S = Array($Tv, n); I = Array($Ti, n)
        @inbounds for i = 1 : n
            Sc = zval; Ic = _findz($A, 1:m, i:i)
            if Ic == 0
                j = colptr[i]
                Ic = sub2ind(szA, rowval[j], i)
                Sc = nzval[j]
            end
            for j = colptr[i] : colptr[i+1]-1
                if ($op)(nzval[j], Sc)
                    Sc = nzval[j]
                    Ic = sub2ind(szA, rowval[j], i)
                end
            end
            S[i] = Sc; I[i] = Ic
        end
        return(reshape(S,1,n), reshape(I,1,n))
    elseif $region == 2 || $region == (2,)
        (N == 0) && (return (fill(zval,m,1), fill(convert($Ti,1),m,1)))
        S = Array($Tv, m); I = Array($Ti, m)
        @inbounds for row in 1:m
            S[row] = zval; I[row] = _findz($A, row:row, 1:n)
            if I[row] == 0
                I[row] = sub2ind(szA, row, 1)
                S[row] = A[row,1]
            end
        end
        @inbounds for i = 1 : n, j = colptr[i] : colptr[i+1]-1
            row = rowval[j]
            if ($op)(nzval[j], S[row])
                S[row] = nzval[j]
                I[row] = sub2ind(szA, row, i)
            end
        end
        return (reshape(S,m,1), reshape(I,m,1))
    elseif $region == (1,2)
        (N == 0) && (return (fill(zval,1,1), fill(convert($Ti,1),1,1)))
        hasz = nnz($A) != length($A)
        Sv = hasz ? zval : nzval[1]
        Iv::($Ti) = hasz ? _findz($A) : 1
        @inbounds for i = 1 : $A.n, j = colptr[i] : (colptr[i+1]-1)
            if ($op)(nzval[j], Sv)
                Sv = nzval[j]
                Iv = sub2ind(szA, rowval[j], i)
            end
        end
        return (fill(Sv,1,1), fill(Iv,1,1))
    else
        throw(ArgumentError("invalid value for region; must be 1, 2, or (1,2)"))
    end
    end) #quote
end

findmin{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, region) = @_findr(<, A, region, Tv, Ti)
findmax{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, region) = @_findr(>, A, region, Tv, Ti)
findmin{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}) = (r=findmin(A,(1,2)); (r[1][1], r[2][1]))
findmax{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}) = (r=findmax(A,(1,2)); (r[1][1], r[2][1]))

indmin(A::SparseMatrixCSC) = findmin(A)[2]
indmax(A::SparseMatrixCSC) = findmax(A)[2]


sum{T}(A::SparseMatrixCSC{T})          = sum(A.nzval)
sum{T}(A::SparseMatrixCSC{T}, region)  = reducedim(+,A,region,zero(T))

prod{T}(A::SparseMatrixCSC{T})         = nnz(A)!=length(A) ? zero(T) : prod(A.nzval)
prod{T}(A::SparseMatrixCSC{T}, region) = reducedim(*,A,region,one(T))

mean(A::SparseMatrixCSC, region::Integer) = sum(A, region) / size(A, region)

#all(A::SparseMatrixCSC{Bool}, region) = reducedim(all,A,region,true)
#any(A::SparseMatrixCSC{Bool}, region) = reducedim(any,A,region,false)
#sum(A::SparseMatrixCSC{Bool}, region) = reducedim(+,A,region,0,Int)
#sum(A::SparseMatrixCSC{Bool}) = countnz(A)

## getindex
function rangesearch(haystack::Range, needle)
    (i,rem) = divrem(needle - first(haystack), step(haystack))
    (rem==0 && 1<=i+1<=length(haystack)) ? i+1 : 0
end

getindex(A::SparseMatrixCSC, i::Integer) = getindex(A, ind2sub(size(A),i))
getindex(A::SparseMatrixCSC, I::(Integer,Integer)) = getindex(A, I[1], I[2])

function getindex{T}(A::SparseMatrixCSC{T}, i0::Integer, i1::Integer)
    if !(1 <= i0 <= A.m && 1 <= i1 <= A.n); throw(BoundsError()); end
    r1 = int(A.colptr[i1])
    r2 = int(A.colptr[i1+1]-1)
    (r1 > r2) && return zero(T)
    r1 = searchsortedfirst(A.rowval, i0, r1, r2, Forward)
    ((r1 > r2) || (A.rowval[r1] != i0)) ? zero(T) : A.nzval[r1]
end

getindex{T<:Integer}(A::SparseMatrixCSC, I::AbstractVector{T}, j::Integer) = getindex(A,I,[j])
getindex{T<:Integer}(A::SparseMatrixCSC, i::Integer, J::AbstractVector{T}) = getindex(A,[i],J)

function getindex_cols{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, J::AbstractVector)
    # for indexing whole columns
    (m, n) = size(A)
    nJ = length(J)
    (maximum(J) <= n) || throw(BoundsError())

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval

    colptrS = Array(Ti, nJ+1)
    colptrS[1] = 1
    nnzS = 0

    @inbounds for j = 1:nJ
        col = J[j]
        nnzS += colptrA[col+1] - colptrA[col]
        colptrS[j+1] = nnzS + 1
    end

    rowvalS = Array(Ti, nnzS)
    nzvalS  = Array(Tv, nnzS)
    ptrS = 0

    @inbounds for j = 1:nJ
        col = J[j]
        for k = colptrA[col]:colptrA[col+1]-1
            ptrS += 1
            rowvalS[ptrS] = rowvalA[k]
            nzvalS[ptrS] = nzvalA[k]
        end
    end
    return SparseMatrixCSC(m, nJ, colptrS, rowvalS, nzvalS)
end

function getindex{Tv,Ti<:Integer}(A::SparseMatrixCSC{Tv,Ti}, I::Range, J::AbstractVector)
    # Ranges for indexing rows
    (m, n) = size(A)
    # whole columns:
    if I == 1:m
        return getindex_cols(A, J)
    end

    nI = length(I)
    nJ = length(J)
    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    colptrS = Array(Ti, nJ+1)
    colptrS[1] = 1
    nnzS = 0

    # Form the structure of the result and compute space
    for j = 1:nJ
        @inbounds col = J[j]
        for k in colptrA[col]:colptrA[col+1]-1
            if rowvalA[k] in I; nnzS += 1 end # `in` is fast for ranges
        end
        @inbounds colptrS[j+1] = nnzS+1
    end

    # Populate the values in the result
    rowvalS = Array(Ti, nnzS)
    nzvalS  = Array(Tv, nnzS)
    ptrS    = 1

    @inbounds for j = 1:nJ
        col = J[j]
        for k = colptrA[col]:colptrA[col+1]-1
            rowA = rowvalA[k]
            i = rangesearch(I, rowA)
            if i > 0
                rowvalS[ptrS] = i
                nzvalS[ptrS] = nzvalA[k]
                ptrS += 1
            end
        end
    end

    return SparseMatrixCSC(nI, nJ, colptrS, rowvalS, nzvalS)
end

function getindex_I_sorted{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector)
    # Sorted vectors for indexing rows.
    # Similar to getindex_general but without the transpose trick.
    (m, n) = size(A)
    nI = length(I)
    nJ = length(J)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    colptrS = Array(Ti, nJ+1)
    colptrS[1] = 1
    nnzS = 0

    # Form the structure of the result and compute space
    for j = 1:nJ
        @inbounds col = J[j]
        ptrI::Int = 1 # runs through I
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
        @inbounds colptrS[j+1] = nnzS+1
    end

    # Populate the values in the result
    rowvalS = Array(Ti, nnzS)
    nzvalS  = Array(Tv, nnzS)
    ptrS    = 1

    @inbounds for j = 1:nJ
        col = J[j]
        ptrI::Int = 1 # runs through I
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
                rowvalS[ptrS] = ptrI
                nzvalS[ptrS] = nzvalA[ptrA]
                ptrS += 1
                ptrI += 1
            end
        end
    end
    return SparseMatrixCSC(nI, nJ, colptrS, rowvalS, nzvalS)
end


function getindex_general{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector)
    # Anything for indexing rows.
    # This sorts I first then does some trick with constructing the transpose.
    (m, n) = size(A)
    nI = length(I)
    nJ = length(J)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    nnzS = 0
    pI = sortperm(I)
    @inbounds I = I[pI]
    fI = find(I)
    W = zeros(Ti, nI + 1) # Keep row counts
    W[1] = 1               # For cumsum later

    # Form the structure of the result and compute space
    for j = 1:nJ
        @inbounds col = J[j]

        ptrI::Int = 1

        ptrA::Int = colptrA[col]
        stopA::Int = colptrA[col+1]

        @inbounds while ptrI <= nI && ptrA < stopA
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
    @simd for i=1:nI; @inbounds W[i] = 0; end     # Zero out W to store row positions

    @inbounds for j = 1:nJ
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


# the general case:
function getindex{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector)
    if issorted(I)
        return getindex_I_sorted(A, I, J)
    else
        return getindex_general(A, I, J)
    end
end

# logical getindex
getindex{Tv,Ti<:Integer}(A::SparseMatrixCSC{Tv,Ti}, I::Range{Bool}, J::AbstractVector{Bool}) = error("Cannot index with Range{Bool}")
getindex{Tv,Ti<:Integer,T<:Integer}(A::SparseMatrixCSC{Tv,Ti}, I::Range{Bool}, J::AbstractVector{T}) = error("Cannot index with Range{Bool}")

getindex{T<:Integer}(A::SparseMatrixCSC, I::Range{T}, J::AbstractVector{Bool}) = A[I,find(J)]
getindex(A::SparseMatrixCSC, I::Integer, J::AbstractVector{Bool}) = A[I,find(J)]
getindex(A::SparseMatrixCSC, I::AbstractVector{Bool}, J::Integer) = A[find(I),J]
getindex(A::SparseMatrixCSC, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = A[find(I),find(J)]
getindex{T<:Integer}(A::SparseMatrixCSC, I::AbstractVector{T}, J::AbstractVector{Bool}) = A[I,find(J)]
getindex{T<:Integer}(A::SparseMatrixCSC, I::AbstractVector{Bool}, J::AbstractVector{T}) = A[find(I),J]

function getindex{Tv}(A::SparseMatrixCSC{Tv}, I::AbstractArray{Bool})
    checkbounds(A, I)
    n = sum(I)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    colptrB = Int[1,n+1]
    rowvalB = Array(Int, n)
    nzvalB = Array(Tv, n)
    c = 1
    rowB = 1

    @inbounds for col in 1:A.n
        r1 = colptrA[col]
        r2 = colptrA[col+1]-1

        for row in 1:A.m
            if I[row, col]
                while (r1 <= r2) && (rowvalA[r1] < row)
                    r1 += 1
                end
                if (r1 <= r2) && (rowvalA[r1] == row)
                    nzvalB[c] = nzvalA[r1]
                    rowvalB[c] = rowB
                    c += 1
                end
                rowB += 1
                (rowB > n) && break
            end
        end
        (rowB > n) && break
    end
    colptrB[end] = c
    n = length(nzvalB)
    if n > (c-1)
        deleteat!(nzvalB, c:n)
        deleteat!(rowvalB, c:n)
    end
    SparseMatrixCSC(n, 1, colptrB, rowvalB, nzvalB)
end

function getindex{Tv}(A::SparseMatrixCSC{Tv}, I::AbstractArray)
    szA = size(A); colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval

    n = length(I)
    outm = size(I,1)
    outn = size(I,2)
    szB = (outm, outn)
    colptrB = zeros(Int, outn+1)
    rowvalB = Array(Int, n)
    nzvalB = Array(Tv, n)

    colB = 1
    rowB = 1
    colptrB[colB] = 1
    idxB = 1

    for i in 1:n
        row,col = ind2sub(szA, I[i])
        for r in colptrA[col]:(colptrA[col+1]-1)
            @inbounds if rowvalA[r] == row
                rowB,colB = ind2sub(szB, i)
                colptrB[colB+1] += 1
                rowvalB[idxB] = rowB
                nzvalB[idxB] = nzvalA[r]
                idxB += 1
                break
            end
        end
    end
    colptrB = cumsum(colptrB)
    if n > (idxB-1)
        deleteat!(nzvalB, idxB:n)
        deleteat!(rowvalB, idxB:n)
    end
    SparseMatrixCSC(outm, outn, colptrB, rowvalB, nzvalB)
end


## setindex!
setindex!(A::SparseMatrixCSC, v, i::Integer) = setindex!(A, v, ind2sub(size(A),i)...)

function setindex!{T,Ti}(A::SparseMatrixCSC{T,Ti}, v, i0::Integer, i1::Integer)
    i0 = convert(Ti, i0)
    i1 = convert(Ti, i1)
    if !(1 <= i0 <= A.m && 1 <= i1 <= A.n); throw(BoundsError()); end
    v = convert(T, v)
    r1 = int(A.colptr[i1])
    r2 = int(A.colptr[i1+1]-1)
    if v == 0 #either do nothing or delete entry if it exists
        if r1 <= r2
            r1 = searchsortedfirst(A.rowval, i0, r1, r2, Forward)
            if (r1 <= r2) && (A.rowval[r1] == i0)
                deleteat!(A.rowval, r1)
                deleteat!(A.nzval, r1)
                @simd for j = (i1+1):(A.n+1)
                    @inbounds A.colptr[j] -= 1
                end
            end
        end
        return A
    end
    i = (r1 > r2) ? r1 : searchsortedfirst(A.rowval, i0, r1, r2, Forward)

    if (i <= r2) && (A.rowval[i] == i0)
        A.nzval[i] = v
    else
        insert!(A.rowval, i, i0)
        insert!(A.nzval, i, v)
        @simd for j = (i1+1):(A.n+1)
            @inbounds A.colptr[j] += 1
        end
    end
    return A
end

setindex!{T<:Integer}(A::SparseMatrixCSC, v::AbstractMatrix, i::Integer, J::AbstractVector{T}) = setindex!(A, v, [i], J)
setindex!{T<:Integer}(A::SparseMatrixCSC, v::AbstractMatrix, I::AbstractVector{T}, j::Integer) = setindex!(A, v, I, [j])

setindex!{T<:Integer}(A::SparseMatrixCSC, x::Number, i::Integer, J::AbstractVector{T}) = setindex!(A, x, [i], J)
setindex!{T<:Integer}(A::SparseMatrixCSC, x::Number, I::AbstractVector{T}, j::Integer) = setindex!(A, x, I, [j])

setindex!{Tv,T<:Integer}(A::SparseMatrixCSC{Tv}, x::Number, I::AbstractVector{T}, J::AbstractVector{T}) =
    (0 == x) ? spdelete!(A, I, J) : spset!(A, convert(Tv,x), I, J)

function spset!{Tv,Ti<:Integer}(A::SparseMatrixCSC{Tv}, x::Tv, I::AbstractVector{Ti}, J::AbstractVector{Ti})
    !issorted(I) && (@inbounds I = I[sortperm(I)])
    !issorted(J) && (@inbounds J = J[sortperm(J)])

    m, n = size(A)
    lenI = length(I)
    ((I[end] > m) || (J[end] > n)) && throw(DimensionMismatch(""))
    nnzA = nnz(A) + lenI * length(J)

    colptrA = colptr = A.colptr
    rowvalA = rowval = A.rowval
    nzvalA = nzval = A.nzval

    rowidx = 1
    nadd = 0
    @inbounds for col in 1:n
        rrange = colptr[col]:(colptr[col+1]-1)
        (nadd > 0) && (colptrA[col] = colptr[col] + nadd)

        if col in J
            if isempty(rrange) # set new vals only
                nincl = lenI
                if nadd == 0
                    colptrA = copy(colptr)
                    rowvalA = Array(Ti, nnzA); copy!(rowvalA, 1, rowval, 1, length(rowval))
                    nzvalA = Array(Tv, nnzA); copy!(nzvalA, 1, nzval, 1, length(nzval))
                end
                r = rowidx:(rowidx+nincl-1)
                rowvalA[r] = I
                nzvalA[r] = x
                rowidx += nincl
                nadd += nincl
            else # set old + new vals
                old_ptr = rrange[1]
                old_stop = rrange[end]
                new_ptr = 1
                new_stop = lenI

                while true
                    old_row = rowval[old_ptr]
                    new_row = I[new_ptr]
                    if old_row < new_row
                        rowvalA[rowidx] = old_row
                        nzvalA[rowidx] = nzval[old_ptr]
                        rowidx += 1
                        old_ptr += 1
                    else
                        if old_row == new_row
                            old_ptr += 1
                        else
                            if nadd == 0
                                colptrA = copy(colptr)
                                rowvalA = Array(Ti, nnzA); copy!(rowvalA, 1, rowval, 1, length(rowval))
                                nzvalA = Array(Tv, nnzA); copy!(nzvalA, 1, nzval, 1, length(nzval))
                            end
                            nadd += 1
                        end
                        rowvalA[rowidx] = new_row
                        nzvalA[rowidx] = x
                        rowidx += 1
                        new_ptr += 1
                    end

                    if old_ptr > old_stop
                        if new_ptr <= new_stop
                            if nadd == 0
                                colptrA = copy(colptr)
                                rowvalA = Array(Ti, nnzA); copy!(rowvalA, 1, rowval, 1, length(rowval))
                                nzvalA = Array(Tv, nnzA); copy!(nzvalA, 1, nzval, 1, length(nzval))
                            end
                            r = rowidx:(rowidx+(new_stop-new_ptr))
                            rowvalA[r] = I[new_ptr:new_stop]
                            nzvalA[r] = x
                            rowidx += length(r)
                            nadd += length(r)
                        end
                        break
                    end

                    if new_ptr > new_stop
                        nincl = old_stop-old_ptr+1
                        copy!(rowvalA, rowidx, rowval, old_ptr, nincl)
                        copy!(nzvalA, rowidx, nzval, old_ptr, nincl)
                        rowidx += nincl
                        break
                    end
                end
            end
        elseif !isempty(rrange) # set old vals only
            nincl = length(rrange)
            copy!(rowvalA, rowidx, rowval, rrange[1], nincl)
            copy!(nzvalA, rowidx, nzval, rrange[1], nincl)
            rowidx += nincl
        end
    end

    if nadd > 0
        colptrA[n+1] = rowidx
        deleteat!(rowvalA, rowidx:nnzA)
        deleteat!(nzvalA, rowidx:nnzA)

        A.colptr = colptrA
        A.rowval = rowvalA
        A.nzval = nzvalA
    end
    return A
end

function spdelete!{Tv,Ti<:Integer}(A::SparseMatrixCSC{Tv}, I::AbstractVector{Ti}, J::AbstractVector{Ti})
    m, n = size(A)
    nnzA = nnz(A)
    (nnzA == 0) && (return A)

    !issorted(I) && (@inbounds I = I[sortperm(I)])
    !issorted(J) && (@inbounds J = J[sortperm(J)])

    ((I[end] > m) || (J[end] > n)) && throw(DimensionMismatch(""))

    colptr = colptrA = A.colptr
    rowval = rowvalA = A.rowval
    nzval = nzvalA = A.nzval
    rowidx = 1
    ndel = 0
    @inbounds for col in 1:n
        rrange = colptr[col]:(colptr[col+1]-1)
        (ndel > 0) && (colptrA[col] = colptr[col] - ndel)
        if isempty(rrange) || !(col in J)
            nincl = length(rrange)
            if(ndel > 0) && !isempty(rrange)
                copy!(rowvalA, rowidx, rowval, rrange[1], nincl)
                copy!(nzvalA, rowidx, nzval, rrange[1], nincl)
            end
            rowidx += nincl
        else
            for ridx in rrange
                if rowval[ridx] in I
                    if ndel == 0
                        colptrA = copy(colptr)
                        rowvalA = copy(rowval)
                        nzvalA = copy(nzval)
                    end
                    ndel += 1
                else
                    if ndel > 0
                        rowvalA[rowidx] = rowval[ridx]
                        nzvalA[rowidx] = nzval[ridx]
                    end
                    rowidx += 1
                end
            end
        end
    end

    if ndel > 0
        colptrA[n+1] = rowidx
        deleteat!(rowvalA, rowidx:nnzA)
        deleteat!(nzvalA, rowidx:nnzA)

        A.colptr = colptrA
        A.rowval = rowvalA
        A.nzval = nzvalA
    end
    return A
end

setindex!{Tv,Ti,T<:Integer}(A::SparseMatrixCSC{Tv,Ti}, S::Matrix, I::AbstractVector{T}, J::AbstractVector{T}) =
      setindex!(A, convert(SparseMatrixCSC{Tv,Ti}, S), I, J)

setindex!{Tv,Ti,T<:Integer}(A::SparseMatrixCSC{Tv,Ti}, v::AbstractVector, I::AbstractVector{T}, j::Integer) = setindex!(A, v, I, [j])
setindex!{Tv,Ti,T<:Integer}(A::SparseMatrixCSC{Tv,Ti}, v::AbstractVector, i::Integer, J::AbstractVector{T}) = setindex!(A, v, [i], J)
setindex!{Tv,Ti,T<:Integer}(A::SparseMatrixCSC{Tv,Ti}, v::AbstractVector, I::AbstractVector{T}, J::AbstractVector{T}) =
      setindex!(A, reshape(v, length(I), length(J)), I, J)


# A[I,J] = B
function setindex!{Tv,Ti,T<:Integer}(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti}, I::AbstractVector{T}, J::AbstractVector{T})
    if size(B,1) != length(I) || size(B,2) != length(J)
        throw(DimensionMismatch(""))
    end

    issortedI = issorted(I)
    issortedJ = issorted(J)

    if !issortedI && !issortedJ
        pI = sortperm(I); @inbounds I = I[pI]
        pJ = sortperm(J); @inbounds J = J[pJ]
        B = B[pI, pJ]
    elseif !issortedI
        pI = sortperm(I); @inbounds I = I[pI]
        B = B[pI,:]
    else !issortedJ
        pJ = sortperm(J); @inbounds J = J[pJ]
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

    @inbounds for col = 1:n

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

    deleteat!(rowvalS, colptrS[end]:length(rowvalS))
    deleteat!(nzvalS, colptrS[end]:length(nzvalS))

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

function setindex!{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, x, I::AbstractArray{Bool,2})
    checkbounds(A, I)
    n = sum(I)
    (n == 0) && (return A)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    colptrB = colptrA; rowvalB = rowvalA; nzvalB = nzvalA
    nadd = ndel = 0
    bidx = xidx = 1
    r1 = r2 = 0

    @inbounds for col in 1:A.n
        r1 = int(colptrA[col])
        r2 = int(colptrA[col+1]-1)

        for row in 1:A.m
            if I[row, col]
                v = isa(x, AbstractArray) ? x[xidx] : x
                xidx += 1

                if r1 <= r2
                    copylen = searchsortedfirst(rowvalA, row, r1, r2, Forward) - r1
                    if (copylen > 0)
                        if (nadd > 0) || (ndel > 0)
                            copy!(rowvalB, bidx, rowvalA, r1, copylen)
                            copy!(nzvalB, bidx, nzvalA, r1, copylen)
                        end
                        bidx += copylen
                        r1 += copylen
                    end
                end

                # 0: no change, 1: update, 2: delete, 3: add new
                mode = ((r1 <= r2) && (rowvalA[r1] == row)) ? ((v == 0) ? 2 : 1) : ((v == 0) ? 0 : 3)

                if (mode > 1) && (nadd == 0) && (ndel == 0)
                    # copy storage to take changes
                    colptrB = copy(colptrA)
                    memreq = (x == 0) ? 0 : n
                    rowvalB = Array(Ti, length(rowvalA)+memreq); copy!(rowvalB, 1, rowvalA, 1, r1-1)
                    nzvalB = Array(Tv, length(nzvalA)+memreq); copy!(nzvalB, 1, nzvalA, 1, r1-1)
                end
                if mode == 1
                    rowvalB[bidx] = row
                    nzvalB[bidx] = v
                    bidx += 1
                    r1 += 1
                elseif mode == 2
                    r1 += 1
                    ndel += 1
                elseif mode == 3
                    rowvalB[bidx] = row
                    nzvalB[bidx] = v
                    bidx += 1
                    nadd += 1
                end
                (xidx > n) && break
            end # if I[row, col]
        end # for row in 1:A.m

        if ((nadd != 0) || (ndel != 0))
            l = r2-r1+1
            if l > 0
                copy!(rowvalB, bidx, rowvalA, r1, l)
                copy!(nzvalB, bidx, nzvalA, r1, l)
                bidx += l
            end
            colptrB[col+1] = bidx

            if (xidx > n) && (length(colptrB) > (col+1))
                diff = nadd - ndel
                colptrB[(col+2):end] = colptrA[(col+2):end] .+ diff
                r1 = colptrA[col+1]
                r2 = colptrA[end]-1
                l = r2-r1+1
                if l > 0
                    copy!(rowvalB, bidx, rowvalA, r1, l)
                    copy!(nzvalB, bidx, nzvalA, r1, l)
                    bidx += l
                end
            end
        else
            bidx = colptrA[col+1]
        end
        (xidx > n) && break
    end # for col in 1:A.n

    if (nadd != 0) || (ndel != 0)
        n = length(nzvalB)
        if n > (bidx-1)
            deleteat!(nzvalB, bidx:n)
            deleteat!(rowvalB, bidx:n)
        end
        A.nzval = nzvalB; A.rowval = rowvalB; A.colptr = colptrB
    end
    A
end


function setindex!{Tv,Ti,T<:Real}(A::SparseMatrixCSC{Tv,Ti}, x, I::AbstractVector{T})
    n = length(I)
    (n == 0) && (return A)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval; szA = size(A)
    colptrB = colptrA; rowvalB = rowvalA; nzvalB = nzvalA
    nadd = ndel = 0
    bidx = aidx = 1

    S = issorted(I) ? (1:n) : sortperm(I)
    sxidx = r1 = r2 = 0

    lastcol = 0
    (nrowA, ncolA) = szA
    @inbounds for xidx in 1:n
        sxidx = S[xidx]
        (sxidx < n) && (I[sxidx] == I[sxidx+1]) && continue

        row,col = ind2sub(szA, I[sxidx])
        ((row > nrowA) || (col > ncolA)) && throw(BoundsError())
        v = isa(x, AbstractArray) ? x[sxidx] : x

        if col > lastcol
            r1 = int(colptrA[col])
            r2 = int(colptrA[col+1] - 1)

            # copy from last position till current column
            if (nadd > 0) || (ndel > 0)
                colptrB[(lastcol+1):col] = colptrA[(lastcol+1):col] .+ (nadd - ndel)
                copylen = r1 - aidx
                if copylen > 0
                    copy!(rowvalB, bidx, rowvalA, aidx, copylen)
                    copy!(nzvalB, bidx, nzvalA, aidx, copylen)
                    aidx += copylen
                    bidx += copylen
                end
            else
                aidx = bidx = r1
            end
            lastcol = col
        end

        if r1 <= r2
            copylen = searchsortedfirst(rowvalA, row, r1, r2, Forward) - r1
            if (copylen > 0)
                if (nadd > 0) || (ndel > 0)
                    copy!(rowvalB, bidx, rowvalA, r1, copylen)
                    copy!(nzvalB, bidx, nzvalA, r1, copylen)
                end
                bidx += copylen
                r1 += copylen
                aidx += copylen
            end
        end

        # 0: no change, 1: update, 2: delete, 3: add new
        mode = ((r1 <= r2) && (rowvalA[r1] == row)) ? ((v == 0) ? 2 : 1) : ((v == 0) ? 0 : 3)

        if (mode > 1) && (nadd == 0) && (ndel == 0)
            # copy storage to take changes
            colptrB = copy(colptrA)
            memreq = (x == 0) ? 0 : n
            rowvalB = Array(Ti, length(rowvalA)+memreq); copy!(rowvalB, 1, rowvalA, 1, r1-1)
            nzvalB = Array(Tv, length(nzvalA)+memreq); copy!(nzvalB, 1, nzvalA, 1, r1-1)
        end
        if mode == 1
            rowvalB[bidx] = row
            nzvalB[bidx] = v
            bidx += 1
            aidx += 1
            r1 += 1
        elseif mode == 2
            r1 += 1
            aidx += 1
            ndel += 1
        elseif mode == 3
            rowvalB[bidx] = row
            nzvalB[bidx] = v
            bidx += 1
            nadd += 1
        end
    end

    # copy the rest
    @inbounds if (nadd > 0) || (ndel > 0)
        colptrB[(lastcol+1):end] = colptrA[(lastcol+1):end] .+ (nadd - ndel)
        r1 = colptrA[end]-1
        copylen = r1 - aidx + 1
        if copylen > 0
            copy!(rowvalB, bidx, rowvalA, aidx, copylen)
            copy!(nzvalB, bidx, nzvalA, aidx, copylen)
            aidx += copylen
            bidx += copylen
        end

        n = length(nzvalB)
        if n > (bidx-1)
            deleteat!(nzvalB, bidx:n)
            deleteat!(rowvalB, bidx:n)
        end
        A.nzval = nzvalB; A.rowval = rowvalB; A.colptr = colptrB
    end
    A
end



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
    @inbounds for c = 1 : n
        mX_sofar = 0
        rr1 = colptr[c]
        for i = 1 : num
            XI = X[i]
            rX1 = XI.colptr[c]
            rX2 = XI.colptr[c + 1] - 1
            rr2 = rr1 + (rX2 - rX1)

            rowval[rr1 : rr2] = XI.rowval[rX1 : rX2] .+ mX_sofar
            nzval[rr1 : rr2] = XI.nzval[rX1 : rX2]
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
    @inbounds for i = 1 : num
        XI = X[i]
        colptr[(1 : nX[i] + 1) + nX_sofar] = XI.colptr .+ nnz_sofar
        if nnzX[i] == length(XI.rowval)
            rowval[(1 : nnzX[i]) + nnz_sofar] = XI.rowval
            nzval[(1 : nnzX[i]) + nnz_sofar] = XI.nzval
        else
            rowval[(1 : nnzX[i]) + nnz_sofar] = XI.rowval[1:nnzX[i]]
            nzval[(1 : nnzX[i]) + nnz_sofar] = XI.nzval[1:nnzX[i]]
        end
        nnz_sofar += nnzX[i]
        nX_sofar += nX[i]
    end

    SparseMatrixCSC(m, n, colptr, rowval, nzval)
end

function hvcat(rows::(Int...), X::SparseMatrixCSC...)
    nbr = length(rows)  # number of block rows

    tmp_rows = Array(SparseMatrixCSC, nbr)
    k = 0
    @inbounds for i = 1 : nbr
        tmp_rows[i] = hcat(X[(1 : rows[i]) + k]...)
        k += rows[i]
    end
    vcat(tmp_rows...)
end

function blkdiag(X::SparseMatrixCSC...)
    num = length(X)
    mX = [ size(x, 1) for x in X ]
    nX = [ size(x, 2) for x in X ]
    m = sum(mX)
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
    mX_sofar = 0
    for i = 1 : num
        colptr[(1 : nX[i] + 1) + nX_sofar] = X[i].colptr .+ nnz_sofar
        rowval[(1 : nnzX[i]) + nnz_sofar] = X[i].rowval .+ mX_sofar
        nzval[(1 : nnzX[i]) + nnz_sofar] = X[i].nzval
        nnz_sofar += nnzX[i]
        nX_sofar += nX[i]
        mX_sofar += mX[i]
    end

    SparseMatrixCSC(m, n, colptr, rowval, nzval)
end

## Structure query functions

function issym(A::SparseMatrixCSC)
    m, n = size(A)
    if m != n; return false; end
    return countnz(A - A.') == 0
end

function ishermitian(A::SparseMatrixCSC)
    m, n = size(A)
    if m != n; return false; end
    return countnz(A - A') == 0
end

function istriu{Tv}(A::SparseMatrixCSC{Tv})
    m, n = size(A)
    colptr = A.colptr
    rowval = A.rowval
    nzval  = A.nzval

    for col = 1:min(n, m-1)
        l1 = colptr[col+1]-1
        for i = 0 : (l1 - colptr[col])
            if rowval[l1-i] <= col
                break
            end
            if nzval[l1-i] != 0
                return false
            end
        end
    end
    return true
end

function istril{Tv}(A::SparseMatrixCSC{Tv})
    m, n = size(A)
    colptr = A.colptr
    rowval = A.rowval
    nzval  = A.nzval

    for col = 2:n
        for i = colptr[col] : (colptr[col+1]-1)
            if rowval[i] >= col
                break
            end
            if nzval[i] != 0
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
    A = d.A
    r1 = int(A.colptr[j])
    r2 = int(A.colptr[j+1]-1)
    (r1 > r2) && (return (zero(Tv), j+1))
    r1 = searchsortedfirst(A.rowval, j, r1, r2, Forward)
    (((r1 > r2) || (A.rowval[r1] != j)) ? zero(Tv) : A.nzval[r1], j+1)
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

# Sort all the indices in each column of a CSC sparse matrix
# sortSparseMatrixCSC!(A, sortindices = :sortcols)        # Sort each column with sort()
# sortSparseMatrixCSC!(A, sortindices = :doubletranspose) # Sort with a double transpose
function sortSparseMatrixCSC!{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}; sortindices::Symbol = :sortcols)
    if sortindices == :doubletranspose
        nB, mB = size(A)
        B = SparseMatrixCSC(mB, nB, Array(Ti, nB+1), similar(A.rowval), similar(A.nzval))
        transpose!(B, A)
        transpose!(A, B)
        return A
    end

    m, n = size(A)
    colptr = A.colptr; rowval = A.rowval; nzval = A.nzval

    index = zeros(Ti, m)
    row = zeros(Ti, m)
    val = zeros(Tv, m)

    for i = 1:n
        @inbounds col_start = colptr[i]
        @inbounds col_end = (colptr[i+1] - 1)

        numrows = col_end - col_start + 1
        if numrows <= 1
            continue
        elseif numrows == 2
            f = col_start
            s = f+1
            if rowval[f] > rowval[s]
                @inbounds rowval[f], rowval[s] = rowval[s], rowval[f]
                @inbounds nzval[f],  nzval[s]  = nzval[s],  nzval[f]
            end
            continue
        end

        jj = 1
        @simd for j = col_start:col_end
            @inbounds row[jj] = rowval[j]
            @inbounds val[jj] = nzval[j]
            jj += 1
        end

        sortperm!(pointer_to_array(pointer(index), numrows),
                  pointer_to_array(pointer(row), numrows))

        jj = 1;
        @simd for j = col_start:col_end
            @inbounds rowval[j] = row[index[jj]]
            @inbounds nzval[j] = val[index[jj]]
            jj += 1
        end
    end

    return A
end
