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
type SparseMatrixCSR{Tv,Ti<:Integer} <: AbstractSparseMatrix{Tv,Ti}
    m::Int                  # Number of rows
    n::Int                  # Number of columns
    rowptr::Vector{Ti}      # Row i is in rowptr[i]:(rowptr[i+1]-1)
    colval::Vector{Ti}      # Column values of nonzeros
    nzval::Vector{Tv}       # Nonzero values
end
typealias CompressedSparseMatrix{Tv,Ti} Union(SparseMatrixCSC{Tv,Ti}, SparseMatrixCSR{Tv,Ti})

immutable CompressedSparseStore{Tv,Ti}
    indptr::Vector{Ti}
    indval::Vector{Ti}
    nzval::Vector{Tv}
end

immutable TripletSparseStore{Tv,Ti}
    rows::AbstractVector{Ti}
    cols::AbstractVector{Ti}
    vals::AbstractVector{Tv}
end

SparseMatrixCSC{Tv,Ti<:Integer}(m::Integer, n::Integer, colptr::Vector{Ti}, rowval::Vector{Ti}, nzval::Vector{Tv}) =
    SparseMatrixCSC{Tv,Ti}(int(m), int(n), colptr, rowval, nzval)

SparseMatrixCSR{Tv,Ti<:Integer}(m::Integer, n::Integer, rowptr::Vector{Ti}, colval::Vector{Ti}, nzval::Vector{Tv}) =
    SparseMatrixCSR{Tv,Ti}(int(m), int(n), rowptr, colval, nzval)

sptype{T<:CompressedSparseMatrix}(::Type{T}) = (T <: SparseMatrixCSC) ? CSC : CSR
sptype{T<:CompressedSparseMatrix}(::T) = sptype(T)
types{Tv,Ti}(::CompressedSparseMatrix{Tv,Ti}) = (Tv,Ti)
types{T<:CompressedSparseMatrix}(::Type{T}) = types(T.types[1])
types{Tv,Ti}(::Type{SparseMatrixCSC{Tv,Ti}}) = (Tv,Ti)
types{Tv,Ti}(::Type{SparseMatrixCSR{Tv,Ti}}) = (Tv,Ti)

spaxis{T<:CompressedSparseMatrix}(::T) = (T <: SparseMatrixCSC) ? 2 : 1
nspaxis{T<:CompressedSparseMatrix}(S::T) = spaxis(S) $ 3
spdim(S::CompressedSparseMatrix) = size(S, spaxis(S))
nspdim(S::CompressedSparseMatrix) = size(S, nspaxis(S))

getindptr{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}) = S.colptr
getindval{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}) = S.rowval
setindptr!{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, colptr::Vector{Ti}) = (S.colptr = colptr)
setindval!{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, rowval::Vector{Ti}) = (S.rowval = rowval)

getindptr{Tv,Ti}(S::SparseMatrixCSR{Tv,Ti}) = S.rowptr
getindval{Tv,Ti}(S::SparseMatrixCSR{Tv,Ti}) = S.colval
setindptr!{Tv,Ti}(S::SparseMatrixCSR{Tv,Ti}, rowptr::Vector{Ti}) = (S.rowptr = rowptr)
setindval!{Tv,Ti}(S::SparseMatrixCSR{Tv,Ti}, colval::Vector{Ti}) = (S.colval = colval)

valsptype(format) = (format === CSC) || (format === CSR) || error("unknown sparse matrix format $format")

function sparse{Tv,Ti}(m::Integer, n::Integer, data::CompressedSparseStore{Tv,Ti}; format=CSC)
    valsptype(format)
    ((format === CSC) ? SparseMatrixCSC : SparseMatrixCSR)(m, n, data.indptr, data.indval, data.nzval)
end

size(S::CompressedSparseMatrix) = (S.m, S.n)
size(S::CompressedSparseMatrix, d::Integer) = (d == 1) ? S.m : (d == 2) ? S.n : 1
nnz(S::CompressedSparseMatrix) = int(getindptr(S)[end]-1)
countnz(S::CompressedSparseMatrix) = countnz(S.nzval)

summary(a::CompressedSparseMatrix) = string(dims2string(size(a)), " $(sptype(a)) Sparse Matrix with $(nnz(a)) $(eltype(a)) entries:")
writemime(io::IO, ::MIME"text/plain", v::CompressedSparseMatrix) = with_output_limit(()->showarray(io, v, header=true, repr=false))

function showarray(io::IO, S::CompressedSparseMatrix;
                        header::Bool=true, limit::Bool=Base._limit_output,
                        rows = Base.tty_size()[1], repr=false)
    # TODO: repr?
    header && print(io, summary(S))

    half_screen_rows = limit ? div(rows - 8, 2) : typemax(Int)
    pad = ndigits(max(S.m,S.n))
    k = 0
    sep = "\n\t"

    caxis = spaxis(S)
    ncaxis = nspaxis(S)
    rowcol = Array(Int,2)
    for cidx = 1:spdim(S), k = getindptr(S)[cidx] : (getindptr(S)[cidx+1]-1)
        if k < half_screen_rows || k > nnz(S)-half_screen_rows
            rowcol[caxis] = cidx
            rowcol[ncaxis] = getindval(S)[k]
            print(io, sep, '[', rpad(rowcol[1], pad), ", ", lpad(rowcol[2], pad), "]  =  ")
            showcompact(io, S.nzval[k])
        elseif k == half_screen_rows
            print(io, sep, '\u22ee')
        end
        k += 1
    end
end

## Reinterpret and Reshape

function reinterpret{T,Tv,Ti}(::Type{T}, a::CompressedSparseMatrix{Tv,Ti})
    (sizeof(T) == sizeof(Tv)) || throw(ArgumentError("SparseMatrix reinterpret is only supported for element types of the same size"))

    mA, nA = size(a)
    iptr = copy(getindptr(a))
    ival = copy(getindval(a))
    nzval  = reinterpret(T, a.nzval)
    sparse(mA, nA, CompressedSparseStore(iptr, ival, nzval), format=sptype(a))
end

function sparse_compute_reshaped_indptr_and_indval{Ti}(indptrS::Vector{Ti}, indvalS::Vector{Ti}, mS::Int, nS::Int, indptrA::Vector{Ti}, indvalA::Vector{Ti}, mA::Int, nA::Int)
    indptrS[1] = 1

    colA = 1
    colS = 1
    ptr = 1

    while colA <= nA
        while ptr <= indptrA[colA+1]-1
            rowA = indvalA[ptr]
            i = (colA - 1) * mA + rowA - 1
            colSn = div(i, mS) + 1
            rowS = mod(i, mS) + 1
            while colS < colSn
                indptrS[colS+1] = ptr
                colS += 1
            end
            indvalS[ptr] = rowS
            ptr += 1
        end
        colA += 1
    end
    while colS <= nS
        indptrS[colS+1] = ptr
        colS += 1
    end
end

function reinterpret{T,Tv,Ti,N}(::Type{T}, a::CompressedSparseMatrix{Tv,Ti}, dims::NTuple{N,Int})
    (sizeof(T) == sizeof(Tv)) || throw(ArgumentError("SparseMatrix reinterpret is only supported for element types of the same size"))
    (prod(dims) == length(a)) || throw(DimensionMismatch("new dimensions $(dims) must be consistent with array size $(length(a))"))

    caxis = spaxis(a)
    ncaxis = nspaxis(a)

    mS,nS = (dims[ncaxis], dims[caxis])
    mA,nA = (size(a,ncaxis), size(a,caxis))
    numnz = nnz(a)
    iptr = Array(Ti, nS+1)
    ival = Array(Ti, numnz)
    nzval = reinterpret(T, a.nzval)

    sparse_compute_reshaped_indptr_and_indval(iptr, ival, mS, nS, getindptr(a), getindval(a), mA, nA)

    return sparse(dims[1], dims[2], CompressedSparseStore{T,Ti}(iptr, ival, nzval), format=sptype(a))
end

function reshape{Tv,Ti}(a::CompressedSparseMatrix{Tv,Ti}, dims::NTuple{2,Int})
    (prod(dims) == length(a)) || throw(DimensionMismatch("new dimensions $(dims) must be consistent with array size $(length(a))"))

    caxis = spaxis(a)
    ncaxis = nspaxis(a)

    mS,nS = (dims[ncaxis], dims[caxis])
    mA,nA = (size(a,ncaxis), size(a,caxis))
    numnz = nnz(a)
    iptr = Array(Ti, nS+1)
    ival = Array(Ti, numnz)
    nzval = a.nzval

    sparse_compute_reshaped_indptr_and_indval(iptr, ival, mS, nS, getindptr(a), getindval(a), mA, nA)

    return sparse(dims[1], dims[2], CompressedSparseStore{Tv,Ti}(iptr, ival, nzval), format=sptype(a))
end

## Constructors

copy(S::CompressedSparseMatrix) = sparse(S.m, S.n, CompressedSparseStore(copy(getindptr(S)), copy(getindval(S)), copy(S.nzval)), format=sptype(S))

similar(S::CompressedSparseMatrix, Tv::NonTupleType=eltype(S))   = 
    sparse(S.m, S.n, CompressedSparseStore(copy(getindptr(S)), copy(getindval(S)), Array(Tv, length(S.nzval))), format=sptype(S))
similar{Tv,Ti,TvNew}(S::CompressedSparseMatrix{Tv}, ::Type{TvNew}, ::Type{Ti}) = similar(S, TvNew)
similar{Tv,Ti,TvNew,TiNew}(S::CompressedSparseMatrix{Tv,Ti}, ::Type{TvNew}, ::Type{TiNew}) = 
    sparse(S.m, S.n, CompressedSparseStore(convert(Array{TiNew},getindptr(S)), convert(Array{TiNew}, getindval(S)), Array(TvNew, length(S.nzval))), format=sptype(S))
similar{Tv}(S::CompressedSparseMatrix, ::Type{Tv}, d::NTuple{Integer}) = spzeros(Tv, d..., format=sptype(S))

convert{T<:CompressedSparseMatrix}(::Type{T}, S::T) = S
convert{TvNew,Tv,Ti}(::Type{SparseMatrixCSC{TvNew}}, S::CompressedSparseMatrix{Tv,Ti}) = convert(SparseMatrixCSC{TvNew,Ti}, S)
convert{TvNew,Tv,Ti}(::Type{SparseMatrixCSR{TvNew}}, S::CompressedSparseMatrix{Tv,Ti}) = convert(SparseMatrixCSR{TvNew,Ti}, S)
function convert{T1<:CompressedSparseMatrix,T2<:CompressedSparseMatrix}(::Type{T1}, S::T2)
    if sptype(T1) != sptype(T2) 
        S = S'
        S = sparse(S.m, S.n, CompressedSparseStore(getindptr(S), getindval(S), S.nzval), format=sptype(T1))
    end
    (Tv,Ti) = types(T2)
    (TvNew,TiNew) = types(T1)
    (Tv == TvNew) && (Ti == TiNew) && (return S)

    if Ti === TiNew
        iptr = getindptr(S)
        ival = getindval(S)
    else
        iptr = convert(Vector{TiNew}, getindptr(S))
        ival = convert(Vector{TiNew}, getindval(S))
    end
    nzval = (Tv === TvNew) ? S.nzval : convert(Vector{TvNew}, S.nzval)

    sparse(S.m, S.n, CompressedSparseStore(iptr, ival, nzval), format=sptype(S))
end
function convert{T<:CompressedSparseMatrix}(::Type{T}, M::Matrix)
    m, n = size(M)
    (I, J, V) = findnz(M)
    (Tv,Ti) = types(T)
    return sparse_IJ_sorted!(convert(Vector{Ti},I), 
                             convert(Vector{Ti},J), 
                             convert(Vector{Tv},V), 
                             m, n, sptype(T))
end
convert(::Type{Matrix}, S::CompressedSparseMatrix) = full(S)

function full{Tv}(S::CompressedSparseMatrix{Tv})
    A = zeros(Tv, S.m, S.n)

    caxis = spaxis(S)
    cdim = spdim(S)
    if sptype(S) === CSC
        for cidx = 1:cdim, k = getindptr(S)[cidx] : (getindptr(S)[cidx+1]-1)
            A[getindval(S)[k], cidx] = S.nzval[k]
        end
    else
        for cidx = 1:cdim, k = getindptr(S)[cidx] : (getindptr(S)[cidx+1]-1)
            A[cidx, getindval(S)[k]] = S.nzval[k]
        end
    end
    return A
end

float(S::CompressedSparseMatrix) = sparse(S.m, S.n, CompressedSparseStore(copy(getindptr(S)), copy(getindval(S)), float(copy(S.nzval))), format=sptype(S))
complex(S::CompressedSparseMatrix) = sparse(S.m, S.n, CompressedSparseStore(copy(getindptr(S)), copy(getindval(S)), complex(copy(S.nzval))), format=sptype(S))
complex(A::SparseMatrixCSC, B::SparseMatrixCSC) = A + im*B
complex(A::SparseMatrixCSR, B::SparseMatrixCSR) = A + im*B

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
    sparse_IJ_sorted!(I, ones(eltype(I), nI), V, m, 1, combine,CSC)
end

function sparsevec(a::Vector)
    n = length(a)
    I = find(a)
    J = ones(Int, n)
    V = a[I]
    return sparse_IJ_sorted!(I,J,V,n,1,+,CSC)
end

sparse(a::Vector) = sparsevec(a)

## Construct a sparse matrix

function sparse{Tv}(A::Matrix{Tv}; format=CSC)
    valsptype(format)
    convert((format === CSC) ? SparseMatrixCSC{Tv,Int} : SparseMatrixCSR{Tv,Int}, A)
end

sparse(S::CompressedSparseMatrix) = copy(S)

sparse_IJ_sorted!(I,J,V,m,n,format) = sparse_IJ_sorted!(I,J,V,m,n,+,format)
sparse_IJ_sorted!(I,J,V::AbstractVector{Bool},m,n,format) = sparse_IJ_sorted!(I,J,V,m,n,|,format)
# TODO: remove sortperm
function sparse_IJ_sorted!{Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti}, V::AbstractVector,
                                        m::Integer, n::Integer, combine::Function, format)
    (length(V) == 0) && (return spzeros(eltype(V),Ti,m,n, format=format))
    if format === CSC
        #s = sortperm(J)
        #(iptr, ival, V) = compress_IJ_sorted!(I[s], J[s], V, m, n, combine)
        (iptr, ival, V) = compress_IJ_sorted!(I, J, V, m, n, combine)
    else
        s = sortperm(I)
        (iptr, ival, V) = compress_IJ_sorted!(J[s], I[s], V, n, m, combine)
    end
    return sparse(m, n, CompressedSparseStore(iptr, ival, V), format=format)
end

function compress_IJ_sorted!{Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti}, V::AbstractVector,
                                        m::Integer, n::Integer, combine::Function)
    m = m < 0 ? 0 : m
    n = n < 0 ? 0 : n

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

    iptr = cumsum(cols)

    # Allow up to 20% slack
    if ndups > 0.2*length(I)
        numnz = length(I)-ndups
        I = I[1:numnz]
        V = V[1:numnz]
    end

    return (iptr, I, V)
end

## sparse() can take its inputs in unsorted order (the parent method is now in jlsparse.jl)

dimlub(I) = length(I)==0 ? 0 : int(maximum(I)) #least upper bound on required sparse matrix dimension

sparse{Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti}, v::Number; format=CSC)                             = 
    sparse(dimlub(I), dimlub(J), TripletSparseStore(I,J,fill(v,length(I))), +, format=format)
sparse{Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti}, V::AbstractVector; format=CSC)                     = 
    sparse(dimlub(I), dimlub(J), TripletSparseStore(I, J, V), +, format=format)
sparse{Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti}, v::Number, m, n; format=CSC)                       = 
    sparse(int(m), int(n), TripletSparseStore(I, J, fill(v,length(I))), +, format=format)
sparse{Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti}, V::AbstractVector, m, n; format=CSC)               = 
    sparse(int(m), int(n), TripletSparseStore(I, J, V), +, format=format)
sparse{Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti}, V::AbstractVector{Bool}, m, n; format=CSC)         = 
    sparse(int(m), int(n), TripletSparseStore(I, J, V), |, format=format)
sparse{Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti}, v::Number, m, n, combine::Function; format=CSC)    = 
    sparse(int(m), int(n), TripletSparseStore(I, J, fill(v,length(I))), combine, format=format)

function find(S::CompressedSparseMatrix)
    sz = size(S)
    I, J = findn(S)
    return sub2ind(sz, I, J)
end

function findn{Tv,Ti}(S::CompressedSparseMatrix{Tv,Ti})
    numnz = nnz(S)
    I = Array(Ti, numnz)
    J = Array(Ti, numnz)

    count = 1
    for cidx = 1:spdim(S), k = getindptr(S)[cidx] : (getindptr(S)[cidx+1]-1)
        if S.nzval[k] != 0
            I[count] = getindval(S)[k]
            J[count] = cidx
            count += 1
        end
    end

    count -= 1
    if numnz != count
        deleteat!(I, (count+1):numnz)
        deleteat!(J, (count+1):numnz)
    end

    return (sptype(S) === CSC) ? (I, J) : (J, I)
end

function findnz{Tv,Ti}(S::CompressedSparseMatrix{Tv,Ti})
    numnz = nnz(S)
    I = Array(Ti, numnz)
    J = Array(Ti, numnz)
    V = Array(Tv, numnz)

    count = 1
    for cidx = 1:spdim(S), k = getindptr(S)[cidx] : (getindptr(S)[cidx+1]-1)
        if S.nzval[k] != 0
            I[count] = getindval(S)[k]
            J[count] = cidx
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

    return (sptype(S) === CSC) ? (I, J, V) : (J, I, V)
end

nonzeros(S::CompressedSparseMatrix) = S.nzval

function sprand{T}(m::Integer, n::Integer, density::FloatingPoint,
                   rng::Function,::Type{T}=eltype(rng(1)); format=CSC)
    0 <= density <= 1 || throw(ArgumentError("$density not in [0,1]"))
    N = n*m
    N == 0 && return spzeros(T,m,n, format=format)
    N == 1 && return rand() <= density ? sparse(reshape(rng(1),1,1), format=format) : spzeros(T,1,1, format=format)

    I, J = Array(Int, 0), Array(Int, 0) # indices of nonzero elements
    sizehint(I, int(N*density))
    sizehint(J, int(N*density))

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
        for i = Jlen+1:length(J)
            J[i] = j
        end
    end
    return sparse_IJ_sorted!(I, J, rng(length(I)), m, n, +, format)  # it will never need to combine
end

sprand(m::Integer, n::Integer, density::FloatingPoint; format=CSC) = sprand(m,n,density,rand,Float64,format=format)
sprandn(m::Integer, n::Integer, density::FloatingPoint; format=CSC) = sprand(m,n,density,randn,Float64,format=format)
truebools(n::Integer) = ones(Bool, n)
sprandbool(m::Integer, n::Integer, density::FloatingPoint; format=CSC) = sprand(m,n,density,truebools,Bool,format=format)

spones{T}(S::CompressedSparseMatrix{T}) = sparse(S.m, S.n, CompressedSparseStore(copy(getindptr(S)), copy(getindval(S)), ones(T, getindptr(S)[end]-1)), format=sptype(S))

spzeros(m::Integer, n::Integer; format=CSC) = spzeros(Float64, m, n, format=format)
spzeros(Tv::Type, m::Integer, n::Integer; format=CSC) = sparse(m, n, CompressedSparseStore(ones(Int, ((format===CSC)?n:m)+1), Array(Int, 0), Array(Tv, 0)), format=format)
spzeros(Tv::Type, Ti::Type, m::Integer, n::Integer; format=CSC) = sparse(m, n, CompressedSparseStore(ones(Ti, ((format===CSC)?n:m)+1), Array(Ti, 0), Array(Tv, 0)), format=format)

speye(n::Integer; format=CSC) = speye(Float64, n, format=format)
speye(T::Type, n::Integer; format=CSC) = speye(T, n, n, format=format)
speye(m::Integer, n::Integer; format=CSC) = speye(Float64, m, n, format=format)
speye{T}(S::CompressedSparseMatrix{T}) = speye(T, size(S, 1), size(S, 2), format=sptype(S))
eye(S::CompressedSparseMatrix) = speye(S)

function speye(T::Type, m::Integer, n::Integer; format=CSC)
    x = min(m,n)
    ival = [1:x]
    iptr = [ival, fill(int(x+1), ((format===CSC)?n:m)+1-x)]
    nzval  = ones(T, x)
    return sparse(m, n, CompressedSparseStore(iptr, ival, nzval), format=format)
end

function one{T}(S::CompressedSparseMatrix{T})
    m,n = size(S)
    if m != n; throw(DimensionMismatch("multiplicative identity only defined for square matrices")); end
    speye(T, m, format=sptype(S))
end

## Unary arithmetic and boolean operators

# Operations that may map nonzeros to zero, and zero to zero
# Result is sparse
for (op, restype) in ((:iceil, Int), (:ceil, Nothing), 
                      (:ifloor, Int), (:floor, Nothing),
                      (:itrunc, Int), (:trunc, Nothing),
                      (:iround, Int), (:round, Nothing),
                      (:sin, Nothing), (:tan, Nothing), 
                      (:sinh, Nothing), (:tanh, Nothing), 
                      (:asin, Nothing), (:atan, Nothing), 
                      (:asinh, Nothing), (:atanh, Nothing), 
                      (:sinpi, Nothing), (:cosc, Nothing), 
                      (:sind, Nothing), (:tand, Nothing), 
                      (:asind, Nothing), (:atand, Nothing) )
    @eval begin

        function ($op){Tv,Ti}(A::CompressedSparseMatrix{Tv,Ti})
            cdim = spdim(A)
            nfilledA = nnz(A)
            indptrB = Array(Ti, cdim+1)
            indvalB = Array(Ti, nfilledA)
            nzvalB = Array($(restype==Nothing ? (:Tv) : restype), nfilledA)

            k = 0 # number of additional zeros introduced by op(A)
            for i = 1 : cdim
                indptrB[i] = getindptr(A)[i] - k
                for j = getindptr(A)[i] : getindptr(A)[i+1]-1
                    opAj = $(op)(A.nzval[j])
                    if opAj == 0
                        k += 1
                    else
                        indvalB[j - k] = getindval(A)[j]
                        nzvalB[j - k] = opAj
                    end
                end
            end
            indptrB[end] = getindptr(A)[end] - k
            deleteat!(indvalB, indptrB[end]:nfilledA)
            deleteat!(nzvalB, indptrB[end]:nfilledA)
            return sparse(A.m, A.n, CompressedSparseStore(indptrB, indvalB, nzvalB), format=sptype(A))
        end

    end # quote
end # macro

# Operations that map nonzeros to nonzeros, and zeros to zeros
# Result is sparse
for op in (:-, :abs, :abs2, :log1p, :expm1)
    @eval begin

        function ($op)(A::CompressedSparseMatrix)
            B = similar(A)
            nzvalB = B.nzval
            nzvalA = A.nzval
            for i=1:length(nzvalB)
                nzvalB[i] = ($op)(nzvalA[i])
            end
            return B
        end

    end
end

# Operations that map nonzeros to nonzeros, and zeros to nonzeros
# Result is dense
for op in (:cos, :cosh, :acos, :sec, :csc, :cot, :acot, :sech, 
           :csch, :coth, :asech, :acsch, :cospi, :sinc, :cosd, 
           :cotd, :cscd, :secd, :acosd, :acotd, :log, :log2, :log10,
           :exp, :exp2, :exp10)
    @eval begin

        function ($op){Tv}(A::CompressedSparseMatrix{Tv})
            B = fill($(op)(zero(Tv)), size(A))

            caxis = spaxis(A)
            ncaxis = nspaxis(A)
            cdim = spdim(A)
            rowcol = Array(Int,2)

            for cidx = 1 : cdim
                rowcol[caxis] = cidx
                for j = getindptr(A)[cidx] : getindptr(A)[cidx+1]-1
                    rowcol[ncaxis] = getindval(A)[j]
                    nz = A.nzval[j]
                    B[rowcol[1],rowcol[2]] = $(op)(nz)
                end
            end
            return B
        end
        
    end
end

## Binary arithmetic and boolean operators

for (op, restype) in ( (:+, Nothing), (:-, Nothing), (:.*, Nothing),
                       (:(.<), Bool) )
    @eval begin
        function ($op){T1<:CompressedSparseMatrix, T2<:CompressedSparseMatrix}(A::T1, B::T2)
            (TvA,TiA) = types(T1)
            (TvB,TiB) = types(T2)
            Tv = promote_type(TvA, TvB)
            Ti = promote_type(TiA, TiB)
            T = (sptype(A) === CSC) ? SparseMatrixCSC{Tv,Ti} : SparseMatrixCSR{Tv,Ti}
            return ($op)(convert(T,A), convert(T,B))
        end

        function ($op){T<:CompressedSparseMatrix}(A::T, B::T)
            (size(A) == size(B)) || throw(DimensionMismatch(""))

            (m, n) = size(A)
            cdim = spdim(A)
            (Tv,Ti) = types(A)

            # TODO: Need better method to estimate result space
            nnzS = nnz(A) + nnz(B)
            indptrS = Array(Ti, cdim+1)
            indvalS = Array(Ti, nnzS)
            nzvalS = Array($(restype==Nothing ? (:Tv) : restype), nnzS)

            z = zero(Tv)

            indptrA = getindptr(A); indvalA = getindval(A); nzvalA = A.nzval
            indptrB = getindptr(B); indvalB = getindval(B); nzvalB = B.nzval

            ptrS = 1
            indptrS[1] = 1

            for cidx = 1:cdim
                ptrA::Int  = indptrA[cidx]
                stopA::Int = indptrA[cidx+1]
                ptrB::Int  = indptrB[cidx]
                stopB::Int = indptrB[cidx+1]

                while ptrA < stopA && ptrB < stopB
                    rowA = indvalA[ptrA]
                    rowB = indvalB[ptrB]
                    if rowA < rowB
                        res = ($op)(nzvalA[ptrA], z)
                        if res != z
                            indvalS[ptrS] = rowA
                            nzvalS[ptrS] = res
                            ptrS += 1
                        end
                        ptrA += 1
                    elseif rowB < rowA
                        res = ($op)(z, nzvalB[ptrB])
                        if res != z
                            indvalS[ptrS] = rowB
                            nzvalS[ptrS] = res
                            ptrS += 1
                        end
                        ptrB += 1
                    else
                        res = ($op)(nzvalA[ptrA], nzvalB[ptrB])
                        if res != z
                            indvalS[ptrS] = rowA
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
                        rowA = indvalA[ptrA]
                        indvalS[ptrS] = rowA
                        nzvalS[ptrS] = res
                        ptrS += 1
                    end
                    ptrA += 1
                end

                while ptrB < stopB
                    res = ($op)(z, nzvalB[ptrB])
                    if res != z
                        rowB = indvalB[ptrB]
                        indvalS[ptrS] = rowB
                        nzvalS[ptrS] = res
                        ptrS += 1
                    end
                    ptrB += 1
                end

                indptrS[cidx+1] = ptrS
            end

            deleteat!(indvalS, indptrS[end]:length(indvalS))
            deleteat!(nzvalS, indptrS[end]:length(nzvalS))
            return sparse(m, n, CompressedSparseStore(indptrS, indvalS, nzvalS), format=sptype(A))
        end
    end # quote
end # macro

(.+)(A::CompressedSparseMatrix, B::Number) = full(A) .+ B
( +)(A::CompressedSparseMatrix, B::Array ) = full(A)  + B
(.+)(A::Number, B::CompressedSparseMatrix) = A .+ full(B)
( +)(A::Array , B::CompressedSparseMatrix) = A  + full(B)

(.-)(A::CompressedSparseMatrix, B::Number) = full(A) .- B
( -)(A::CompressedSparseMatrix, B::Array ) = full(A)  - B
(.-)(A::Number, B::CompressedSparseMatrix) = A .- full(B)
( -)(A::Array,  B::CompressedSparseMatrix) = A  - full(B)

(.*)(A::CompressedSparseMatrix, B::Number) = sparse(A.m, A.n, CompressedSparseStore(copy(getindptr(A)), copy(getindval(A)), A.nzval .* B), format=sptype(A))
(.*)(A::Number, B::CompressedSparseMatrix) = sparse(B.m, B.n, CompressedSparseStore(copy(getindptr(B)), copy(getindval(B)), A .* B.nzval), format=sptype(B))
(.*)(A::CompressedSparseMatrix, B::Array) = (.*)(A, sparse(B, format=sptype(A)))
(.*)(A::Array, B::CompressedSparseMatrix) = (.*)(sparse(A, format=sptype(B)), B)

(./)(A::CompressedSparseMatrix, B::Number) = sparse(A.m, A.n, CompressedSparseStore(copy(getindptr(A)), copy(getindval(A)), A.nzval ./ B), format=sptype(A))
(./)(A::Number, B::CompressedSparseMatrix) = (./)(A, full(B))
(./)(A::CompressedSparseMatrix, B::Array) = (./)(full(A), B)
(./)(A::Array, B::CompressedSparseMatrix) = (./)(A, full(B))
(./)(A::CompressedSparseMatrix, B::CompressedSparseMatrix) = (./)(full(A), full(B))

(.\)(A::CompressedSparseMatrix, B::Number) = (.\)(full(A), B)
(.\)(A::Number, B::CompressedSparseMatrix) = sparse(B.m, B.n, CompressedSparseStore(copy(getindptr(B)), copy(getindval(B)), B.nzval .\ A), format=sptype(B))
(.\)(A::CompressedSparseMatrix, B::Array) = (.\)(full(A), B)
(.\)(A::Array, B::CompressedSparseMatrix) = (.\)(A, full(B))
(.\)(A::CompressedSparseMatrix, B::CompressedSparseMatrix) = (.\)(full(A), full(B))

(.^)(A::CompressedSparseMatrix, B::Number) =
    B==0 ? sparse(ones(typeof(one(eltype(A)).^B), A.m, A.n), format=sptype(A)) :
           sparse(A.m, A.n, CompressedSparseStore(copy(getindptr(A)), copy(getindval(A)), A.nzval .^ B), format=sptype(A))
(.^)(A::Number, B::CompressedSparseMatrix) = (.^)(A, full(B))
(.^)(A::CompressedSparseMatrix, B::Array) = (.^)(full(A), B)
(.^)(A::Array, B::CompressedSparseMatrix) = (.^)(A, full(B))

(.<)(A::CompressedSparseMatrix, B::Number) = (.<)(full(A), B)
(.<)(A::Number, B::CompressedSparseMatrix) = (.<)(A, full(B))

# Reductions

# TODO: Should the results of sparse reductions be sparse?
function reducedim{Tv,Ti}(f::Function, A::CompressedSparseMatrix{Tv,Ti}, region, v0)
    cdim = spdim(A)
    ncdim = nspdim(A)

    if (length(region) == 1) && (region[1] == spaxis(A))

        S = fill(v0, ncdim, 1)
        counts = zeros(Ti, ncdim)
        for i = 1 : cdim, j = getindptr(A)[i] : getindptr(A)[i+1]-1
            ncidx = getindval(A)[j]
            S[ncidx] = f(S[ncidx], A.nzval[j])
            counts[ncidx] += 1
        end
        for i = 1:ncdim
            if counts[i] != cdim; S[i] = f(S[i], zero(Tv)); end
        end
        return S

    elseif (length(region) == 1) && (region[1] == nspaxis(A))

        S = Array(Tv, 1, cdim)
        for i = 1 : cdim
            Si = v0
            count = 0
            for j = getindptr(A)[i] : getindptr(A)[i+1]-1
                Si = f(Si, A.nzval[j])
                count += 1
            end
            if count != ncdim; Si = f(Si, zero(Tv)); end
            S[i] = Si
        end
        return S

    elseif region == (1,2)

        S = v0
        for i = 1 : cdim, j = getindptr(A)[i] : getindptr(A)[i+1]-1
            S = f(S, A.nzval[j])
        end
        if nnz(A) != A.m*A.n; S = f(S, zero(Tv)); end

        return fill(S, 1, 1)

    else
        throw(ArgumentError("invalid value for region; must be 1, 2, or (1,2)"))
    end
end

function maximum{T}(A::CompressedSparseMatrix{T})
    isempty(A) && throw(ArgumentError("argument must not be empty"))
    m = maximum(A.nzval)
    nnz(A)!=length(A) ? max(m,zero(T)) : m
end

maximum{T}(A::CompressedSparseMatrix{T}, region) =
    isempty(A) ? similar(A, reduced_dims0(A,region)) : reducedim(Base.scalarmax,A,region,typemin(T))

function minimum{T}(A::CompressedSparseMatrix{T})
    isempty(A) && throw(ArgumentError("argument must not be empty"))
    m = minimum(A.nzval)
    nnz(A)!=length(A) ? min(m,zero(T)) : m
end

minimum{T}(A::CompressedSparseMatrix{T}, region) =
    isempty(A) ? similar(A, reduced_dims0(A,region)) : reducedim(Base.scalarmin,A,region,typemax(T))

sum{T}(A::CompressedSparseMatrix{T})          = sum(A.nzval)
sum{T}(A::CompressedSparseMatrix{T}, region)  = reducedim(+,A,region,zero(T))

prod{T}(A::CompressedSparseMatrix{T})         = nnz(A)!=length(A) ? zero(T) : prod(A.nzval)
prod{T}(A::CompressedSparseMatrix{T}, region) = reducedim(*,A,region,one(T))

#all(A::SparseMatrixCSC{Bool}, region) = reducedim(all,A,region,true)
#any(A::SparseMatrixCSC{Bool}, region) = reducedim(any,A,region,false)
#sum(A::SparseMatrixCSC{Bool}, region) = reducedim(+,A,region,0,Int)
#sum(A::SparseMatrixCSC{Bool}) = countnz(A)

## getindex
function rangesearch(haystack::Range, needle)
    (i,rem) = divrem(needle - first(haystack), step(haystack))
    (rem==0 && 1<=i+1<=length(haystack)) ? i+1 : 0
end

getindex{T<:CompressedSparseMatrix}(A::T, i::Integer) = getindex(A, ind2sub(size(A),i))
getindex{T<:CompressedSparseMatrix}(A::T, I::(Integer,Integer)) = getindex(A, I[1], I[2])

function getindex_single{Tv,Ti}(iptr::Vector{Ti}, ival::Vector{Ti}, nzval::Vector{Tv}, ic::Int, inc::Int)
    r1 = int(iptr[ic])
    r2 = int(iptr[ic+1]-1)
    (r1 > r2) && return zero(Tv)
    r1 = searchsortedfirst(ival, inc, r1, r2, Forward)
    ((r1 > r2) || (ival[r1] != inc)) ? zero(Tv) : nzval[r1]
end

getindex{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, i0::Integer, i1::Integer) = 
    (1 <= i0 <= A.m && 1 <= i1 <= A.n) ? getindex_single(A.colptr, A.rowval, A.nzval, i1, i0) : throw(BoundsError())
getindex{Tv,Ti}(A::SparseMatrixCSR{Tv,Ti}, i0::Integer, i1::Integer) = 
    (1 <= i0 <= A.m && 1 <= i1 <= A.n) ? getindex_single(A.rowptr, A.colval, A.nzval, i0, i1) : throw(BoundsError())

getindex{T<:Integer}(A::CompressedSparseMatrix, I::AbstractVector{T}, j::Integer) = getindex(A, I, [j])
getindex{T<:Integer}(A::CompressedSparseMatrix, i::Integer, J::AbstractVector{T}) = getindex(A, [i], J)

function getindex_along_caxis{Tv,Ti}(iptr::Vector{Ti}, ival::Vector{Ti}, nzval::Vector{Tv}, X::AbstractVector)
    nX = length(X)

    indptrS = Array(Ti, nX+1)
    indptrS[1] = 1
    nnzS = 0

    for x = 1:nX
        @inbounds cidx = X[x]
        nnzS += iptr[cidx+1] - iptr[cidx]
        @inbounds indptrS[x+1] = nnzS + 1
    end

    indvalS = Array(Ti, nnzS)
    nzvalS  = Array(Tv, nnzS)
    ptrS = 0

    for x = 1:nX
        @inbounds cidx = X[x]

        for ncidx = iptr[cidx]:iptr[cidx+1]-1
            ptrS += 1
            indvalS[ptrS] = ival[ncidx]
            nzvalS[ptrS] = nzval[ncidx]
        end
    end
    (indptrS, indvalS, nzvalS)
end

getindex_cols{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, J::AbstractVector) = 
    sparse(A.m, length(J), CompressedSparseStore(getindex_along_caxis(getindptr(A), getindval(A), A.nzval, J)...), format=CSC)
getindex_rows{Tv,Ti}(A::SparseMatrixCSR{Tv,Ti}, I::AbstractVector) = 
    sparse(length(I), A.n, CompressedSparseStore(getindex_along_caxis(getindptr(A), getindval(A), A.nzval, I)...), format=CSR)

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
        @simd for k in colptrA[col]:colptrA[col+1]-1
            if rowvalA[k] in I; nnzS += 1 end # `in` is fast for ranges
        end
        @inbounds colptrS[j+1] = nnzS+1
    end

    # Populate the values in the result
    rowvalS = Array(Ti, nnzS)
    nzvalS  = Array(Tv, nnzS)
    ptrS    = 1

    for j = 1:nJ
        @inbounds col = J[j]
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

function getindex{Tv,Ti<:Integer}(A::SparseMatrixCSR{Tv,Ti}, I::AbstractVector, J::Range)
    # Ranges for indexing rows
    (m, n) = size(A)
    # whole columns:
    if J == 1:n
        return getindex_rows(A, I)
    end

    nI = length(I)
    nJ = length(J)
    rowptrA = A.rowptr; colvalA = A.colval; nzvalA = A.nzval
    rowptrS = Array(Ti, nI+1)
    rowptrS[1] = 1
    nnzS = 0

    # Form the structure of the result and compute space
    for i = 1:nI
        @inbounds row = I[i]
        @simd for k in rowptrA[row]:rowptrA[row+1]-1
            if colvalA[k] in J; nnzS += 1 end # `in` is fast for ranges
        end
        @inbounds rowptrS[i+1] = nnzS+1
    end

    # Populate the values in the result
    colvalS = Array(Ti, nnzS)
    nzvalS  = Array(Tv, nnzS)
    ptrS    = 1

    for i = 1:nI
        @inbounds row = I[i]
        for k = rowptrA[row]:rowptrA[row+1]-1
            colA = colvalA[k]
            j = rangesearch(J, colA)
            if j > 0
                colvalS[ptrS] = j
                nzvalS[ptrS] = nzvalA[k]
                ptrS += 1
            end
        end
    end

    return SparseMatrixCSR(nI, nJ, rowptrS, colvalS, nzvalS)
end


# TODO: See if growing arrays is faster than pre-computing structure
# and then populating nonzeros
# TODO: Use binary search in cases where nI >> nnz(A[:,j]) or nI << nnz(A[:,j])
#=
function getindex_sorted{Tv,Ti}(m::Int, n::Int, iptr::Vector{Ti}, ival::Vector{Ti}, nzval::Vector{Tv}, I::AbstractVector, J::AbstractVector)
    nI = length(I)
    nJ = length(J)

    I_ref = falses(m)
    I_ref[I] = true

    I_repeat = zeros(Int, m)
    for i=1:nI; I_repeat[I[i]] += 1; end

    indptrS = Array(Ti, nJ+1)
    indptrS[1] = 1
    nnzS = 0

    # Form the structure of the result and compute space
    for j = 1:nJ
        col = J[j]

        for k = iptr[col]:iptr[col+1]-1
            rowA = ival[k]
            
            if I_ref[rowA]
                for r = 1:I_repeat[rowA]
                    nnzS += 1
                end
            end

        end
        indptrS[j+1] = nnzS+1
    end

    # Populate the values in the result
    indvalS = Array(Ti, nnzS)
    nzvalS  = Array(Tv, nnzS)
    ptrS    = 1

    fI = zeros(Ti, m)
    for k=1:nI
        Ik = I[k]
        if fI[Ik] == 0; fI[Ik] = k; end
    end

    for j = 1:nJ
        col = J[j]

        for k = iptr[col]:iptr[col+1]-1
            rowA = ival[k]
            
            if I_ref[rowA]
                for r = 1:I_repeat[rowA]
                    indvalS[ptrS] = fI[rowA] + r - 1
                    nzvalS[ptrS] = nzval[k]
                    ptrS += 1
                end
            end

        end
    end

    return (indptrS, indvalS, nzvalS)
end
=#

function getindex_sorted{Tv,Ti}(m::Int, n::Int, iptr::Vector{Ti}, ival::Vector{Ti}, nzval::Vector{Tv}, I::AbstractVector, J::AbstractVector)
    # Sorted vectors for indexing rows.
    # Similar to getindex_general but without the transpose trick.
    nI = length(I)
    nJ = length(J)
    nV = length(ival)
    (nV == length(nzval)) || throw(BoundsError())

    indptrS = Array(Ti, nJ+1)
    indptrS[1] = 1
    nnzS = 0

    # Form the structure of the result and compute space
    for j = 1:nJ
        @inbounds col = J[j]
        ptrI::Int = 1 # runs through I
        ptrA::Int = iptr[col]
        stopA::Int = iptr[col+1]
        (stopA <= (nV+1)) || throw(BoundsError())
        @inbounds while ptrI <= nI && ptrA < stopA
            rowA = ival[ptrA]
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
        @inbounds indptrS[j+1] = nnzS+1
    end

    # Populate the values in the result
    indvalS = Array(Ti, nnzS)
    nzvalS  = Array(Tv, nnzS)
    ptrS    = 1

    @inbounds for j = 1:nJ
        col = J[j]
        ptrI::Int = 1 # runs through I
        ptrA::Int = iptr[col]
        stopA::Int = iptr[col+1]

        while ptrI <= nI && ptrA < stopA
            rowA = ival[ptrA]
            rowI = I[ptrI]

            if rowI > rowA
                ptrA += 1
            elseif rowI < rowA
                ptrI += 1
            else
                indvalS[ptrS] = ptrI
                nzvalS[ptrS] = nzval[ptrA]
                ptrS += 1
                ptrI += 1
            end
        end
    end
    return (indptrS, indvalS, nzvalS)
end

function getindex_I_sorted{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector)
    (indptrS, indvalS, nzvalS) = getindex_sorted(A.m, A.n, getindptr(A), getindval(A), A.nzval, I, J)
    SparseMatrixCSC(length(I), length(J), indptrS, indvalS, nzvalS)
end
function getindex_J_sorted{Tv,Ti}(A::SparseMatrixCSR{Tv,Ti}, I::AbstractVector, J::AbstractVector)
    (indptrS, indvalS, nzvalS) = getindex_sorted(A.n, A.m, getindptr(A), getindval(A), A.nzval, J, I)
    SparseMatrixCSR(length(I), length(J), indptrS, indvalS, nzvalS)
end

function getindex_general{Tv,Ti}(iptr::Vector{Ti}, ival::Vector{Ti}, nzval::Vector{Tv}, NC::Vector, C::AbstractVector)
    # Anything for indexing rows.
    # This sorts I first then does some trick with constructing the transpose.
    nNC = length(NC)
    nC = length(C)
    nV = length(ival)
    (nV == length(nzval)) || throw(BoundsError())

    nnzS = 0
    pNC = sortperm(NC); NC = NC[pNC]
    fNC = find(NC)
    W = zeros(Ti, nNC + 1)   # Keep counts for the non compressed axis
    W[1] = 1                  # For cumsum later

    # Form the structure of the result and compute space
    for idx = 1:nC
        @inbounds cidx = C[idx]
        ptrNC::Int = 1
        ptrA::Int = iptr[cidx]
        stopA::Int = iptr[cidx+1]
        (stopA <= (nV+1)) || throw(BoundsError())
        @inbounds while ptrNC <= nNC && ptrA < stopA
            idxA = ival[ptrA]
            idxNC = NC[ptrNC]
            if idxNC > idxA
                ptrA += 1
            elseif idxNC < idxA
                ptrNC += 1
            else
                W[fNC[pNC[ptrNC]]+1] += 1
                nnzS += 1
                ptrNC += 1
            end
        end
    end

    indptrS_T = cumsum(W)

    # Populate the values in the result, but transposed
    indvalS_T = Array(Ti, nnzS)
    nzvalS_T  = Array(Tv, nnzS)
    @simd for i=1:nNC; @inbounds W[i] = 0; end     # Zero out W to store positions

    @inbounds for idx = 1:nC
        cidx = C[idx]
        ptrNC::Int = 1
        ptrA::Int = iptr[cidx]
        stopA::Int = iptr[cidx+1]
        while ptrNC <= nNC && ptrA < stopA
            idxA = ival[ptrA]
            idxNC = NC[ptrNC]
            if idxNC > idxA
                ptrA += 1
            elseif idxNC < idxA
                ptrNC += 1
            else
                idxS = fNC[pNC[ptrNC]]
                k = indptrS_T[idxS] + W[idxS]
                indvalS_T[k] = idx
                nzvalS_T[k] = nzval[ptrA]
                W[idxS] += 1
                ptrNC += 1
            end
        end
    end

    (indptrS_T, indvalS_T, nzvalS_T)
end

function getindex_general{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::Vector, J::AbstractVector)
    (indptrS_T, indvalS_T, nzvalS_T) = getindex_general(getindptr(A), getindval(A), A.nzval, I, J)
    # Transpose so that rows are in sorted order and return
    S_T = SparseMatrixCSC(length(J), length(I), indptrS_T, indvalS_T, nzvalS_T)
    return S_T.'
end

function getindex_general{Tv,Ti}(A::SparseMatrixCSR{Tv,Ti}, I::AbstractVector, J::Vector)
    (indptrS_T, indvalS_T, nzvalS_T) = getindex_general(getindptr(A), getindval(A), A.nzval, J, I)
    # Transpose so that cols are in sorted order and return
    S_T = SparseMatrixCSR(length(J), length(I), indptrS_T, indvalS_T, nzvalS_T)
    return S_T.'
end

# S = A[I, J]
function getindex{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector)
    if issorted(I)
        return getindex_I_sorted(A, I, J)
    else
        return getindex_general(A, I, J)
    end
end

function getindex{Tv,Ti}(A::SparseMatrixCSR{Tv,Ti}, I::AbstractVector, J::AbstractVector)
    if issorted(J)
        return getindex_J_sorted(A, I, J)
    else
        return getindex_general(A, I, J)
    end
end

# logical getindex
getindex{Tv,Ti<:Integer}(A::SparseMatrixCSC{Tv,Ti}, I::Range{Bool}, J::AbstractVector{Bool}) = error("Cannot index with Range{Bool}")
getindex{Tv,Ti<:Integer,T<:Integer}(A::SparseMatrixCSC{Tv,Ti}, I::Range{Bool}, J::AbstractVector{T}) = error("Cannot index with Range{Bool}")
getindex{Tv,Ti<:Integer}(A::SparseMatrixCSR{Tv,Ti}, I::AbstractVector{Bool}, J::Range{Bool}) = error("Cannot index with Range{Bool}")
getindex{Tv,Ti<:Integer,T<:Integer}(A::SparseMatrixCSR{Tv,Ti}, I::AbstractVector{T}, J::Range{Bool}) = error("Cannot index with Range{Bool}")
  
getindex{T<:Integer}(A::SparseMatrixCSC, I::Range{T}, J::AbstractVector{Bool}) = A[I,find(J)]
getindex{T<:Integer}(A::SparseMatrixCSR, I::AbstractVector{Bool}, J::Range{T}) = A[find(I),J]

getindex(A::CompressedSparseMatrix, I::Integer, J::AbstractVector{Bool}) = A[I,find(J)]
getindex(A::CompressedSparseMatrix, I::AbstractVector{Bool}, J::Integer) = A[find(I),J]
getindex(A::SparseMatrixCSC, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = A[find(I),find(J)]
getindex(A::SparseMatrixCSR, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = A[find(I),find(J)]
getindex{T<:Integer}(A::SparseMatrixCSC, I::AbstractVector{T}, J::AbstractVector{Bool}) = A[I,find(J)]
getindex{T<:Integer}(A::SparseMatrixCSR, I::AbstractVector{T}, J::AbstractVector{Bool}) = A[I,find(J)]
getindex{T<:Integer}(A::SparseMatrixCSC, I::AbstractVector{Bool}, J::AbstractVector{T}) = A[find(I),J]
getindex{T<:Integer}(A::SparseMatrixCSR, I::AbstractVector{Bool}, J::AbstractVector{T}) = A[find(I),J]

function getindex{Tv,Ti}(A::CompressedSparseMatrix{Tv,Ti}, I::AbstractArray)
    szA = size(A); indptrA = getindptr(A); indvalA = getindval(A); nzvalA = A.nzval

    n = length(I)
    outm = size(I,1)
    outn = size(I,2)
    szB = (outm, outn)
    Ts = sptype(A)
    indptrB = zeros(Int, ((Ts===CSC)?outn:outm)+1)
    indvalB = Array(Int, n)
    nzvalB = Array(Tv, n)

    spidxB = nspidxB = 1
    indptrB[spidxB] = 1
    idxB = 1
    i_nsp = nspaxis(A); i_sp = spaxis(A)

    for i in 1:n
        subA = ind2sub(szA, I[i])
        @inbounds nspidxA = subA[i_nsp]
        @inbounds spidxA = subA[i_sp]
        subB = ind2sub(szB, i)
        @inbounds nspidxB = subB[i_nsp]
        @inbounds spidxB = subB[i_sp]
        
        for r in indptrA[spidxA]:(indptrA[spidxA+1]-1)
            if indvalA[r] == nspidxA
                indptrB[spidxB+1] += 1
                indvalB[idxB] = nspidxB
                nzvalB[idxB] = nzvalA[r]
                idxB += 1
                break
            end
        end
    end
    indptrB = cumsum(indptrB)
    if n > (idxB-1)
        deleteat!(nzvalB, idxB:n)
        deleteat!(indvalB, idxB:n)
    end
    sparse(outm, outn, CompressedSparseStore(indptrB, indvalB, nzvalB), format=Ts)
end
getindex{Tv,Ti}(A::CompressedSparseMatrix{Tv,Ti}, I::AbstractArray{Bool}) = getindex(A, find(I))


## setindex!
setindex!(A::CompressedSparseMatrix, v, i::Integer) = setindex!(A, v, ind2sub(size(A),i)...)

function setindex_single!{Tv,Ti}(iptr::Vector{Ti}, ival::Vector{Ti}, nzval::Vector{Tv}, v, i0::Integer, i1::Integer)
    i0 = convert(Ti, i0)
    i1 = convert(Ti, i1)
    v = convert(Tv, v)

    r1 = int(iptr[i1])
    r2 = int(iptr[i1+1]-1)
    N = length(iptr)
    if v == 0 #either do nothing or delete entry if it exists
        if r1 <= r2
            r1 = searchsortedfirst(ival, i0, r1, r2, Forward)
            if (r1 <= r2) && (ival[r1] == i0)
                deleteat!(ival, r1)
                deleteat!(ival, r1)
                for j = (i1+1):N
                    iptr[j] -= 1
                end
            end
        end
    else
        i = (r1 > r2) ? r1 : searchsortedfirst(ival, i0, r1, r2, Forward)

        if (i <= r2) && (ival[i] == i0)
            nzval[i] = v
        else
            insert!(ival, i, i0)
            insert!(nzval, i, v)
            for j = (i1+1):N
                iptr[j] += 1
            end
        end
    end
    nothing
end

function setindex!{Tv,Ti}(A::CompressedSparseMatrix{Tv,Ti}, v, i0::Integer, i1::Integer)
    (1 <= i0 <= A.m && 1 <= i1 <= A.n) || throw(BoundsError())
    Ts = sptype(A)
    setindex_single!(getindptr(A), getindval(A), A.nzval, v, (Ts===CSC)?i0:i1, (Ts===CSC)?i1:i0) 
    A
end

setindex!{T<:Integer}(A::CompressedSparseMatrix, v::AbstractMatrix, i::Integer, J::AbstractVector{T}) = setindex!(A, v, [i], J)
setindex!{T<:Integer}(A::CompressedSparseMatrix, v::AbstractMatrix, I::AbstractVector{T}, j::Integer) = setindex!(A, v, I, [j])

setindex!{T<:Integer}(A::CompressedSparseMatrix, x::Number, i::Integer, J::AbstractVector{T}) = setindex!(A, x, [i], J)
setindex!{T<:Integer}(A::CompressedSparseMatrix, x::Number, I::AbstractVector{T}, j::Integer) = setindex!(A, x, I, [j])

setindex!{Tv,T<:Integer}(A::CompressedSparseMatrix{Tv}, x::Number, I::AbstractVector{T}, J::AbstractVector{T}) = 
    (0 == x) ? spdelete!(A, I, J) : spset!(A, convert(Tv,x), I, J)

function spset!{Tv,Ti<:Integer}(A::CompressedSparseMatrix{Tv}, x::Tv, I::AbstractVector{Ti}, J::AbstractVector{Ti})
    !issorted(I) && (I = I[sortperm(I)])
    !issorted(J) && (J = J[sortperm(J)])

    ((I[end] > size(A,1)) || (J[end] > size(A,2))) && throw(DimensionMismatch(""))
    nnzA = nnz(A) + length(I) * length(J)

    indptrA = iptr = getindptr(A)
    indvalA = ival = getindval(A)
    nzvalA = nzval = A.nzval

    SPX = (sptype(A) === CSC) ? J : I
    NSPX = (sptype(A) === CSC) ? I : J

    ncidx = 1
    nadd = 0
    for cidx in 1:spdim(A)
        rrange = iptr[cidx]:(iptr[cidx+1]-1)
        (nadd > 0) && (indptrA[cidx] = iptr[cidx] + nadd)

        if cidx in SPX
            if isempty(rrange) # set new vals only
                nincl = length(NSPX)
                if nadd == 0
                    indptrA = copy(iptr)
                    indvalA = Array(Ti, nnzA); copy!(indvalA, 1, ival, 1, length(ival))
                    nzvalA = Array(Tv, nnzA); copy!(nzvalA, 1, nzval, 1, length(nzval))
                end
                r = ncidx:(ncidx+nincl-1)
                indvalA[r] = NSPX
                nzvalA[r] = x
                ncidx += nincl
                nadd += nincl
            else # set old + new vals
                old_ptr = rrange[1]
                old_stop = rrange[end]
                new_ptr = 1
                new_stop = length(NSPX)

                while true
                    old_row = ival[old_ptr]
                    new_row = NSPX[new_ptr]
                    if old_row < new_row
                        indvalA[ncidx] = old_row
                        nzvalA[ncidx] = nzval[old_ptr]
                        ncidx += 1
                        old_ptr += 1
                    else
                        if old_row == new_row
                            old_ptr += 1
                        else
                            if nadd == 0
                                indptrA = copy(iptr)
                                indvalA = Array(Ti, nnzA); copy!(indvalA, 1, ival, 1, length(ival))
                                nzvalA = Array(Tv, nnzA); copy!(nzvalA, 1, nzval, 1, length(nzval))
                            end
                            nadd += 1
                        end
                        indvalA[ncidx] = new_row
                        nzvalA[ncidx] = x
                        ncidx += 1
                        new_ptr += 1
                    end

                    if old_ptr > old_stop
                        if new_ptr <= new_stop
                            if nadd == 0
                                indptrA = copy(iptr)
                                indvalA = Array(Ti, nnzA); copy!(indvalA, 1, ival, 1, length(ival))
                                nzvalA = Array(Tv, nnzA); copy!(nzvalA, 1, nzval, 1, length(nzval))
                            end
                            r = ncidx:(ncidx+(new_stop-new_ptr))
                            indvalA[r] = NSPX[new_ptr:new_stop]
                            nzvalA[r] = x
                            ncidx += length(r)
                            nadd += length(r)
                        end
                        break
                    end

                    if new_ptr > new_stop
                        nincl = old_stop-old_ptr+1
                        copy!(indvalA, ncidx, ival, old_ptr, nincl)
                        copy!(nzvalA, ncidx, nzval, old_ptr, nincl)
                        ncidx += nincl
                        break
                    end
                end
            end
        elseif !isempty(rrange) # set old vals only
            nincl = length(rrange)
            copy!(indvalA, ncidx, ival, rrange[1], nincl)
            copy!(nzvalA, ncidx, nzval, rrange[1], nincl)
            ncidx += nincl
        end
    end

    if nadd > 0
        indptrA[end] = ncidx
        deleteat!(indvalA, ncidx:nnzA)
        deleteat!(nzvalA, ncidx:nnzA)
        
        setindptr!(A, indptrA)
        setindval!(A, indvalA)
        A.nzval = nzvalA
    end
    return A
end

function spdelete!{Tv,Ti<:Integer}(A::CompressedSparseMatrix{Tv}, I::AbstractVector{Ti}, J::AbstractVector{Ti})
    nnzA = nnz(A)
    (nnzA == 0) && (return A)

    !issorted(I) && (I = I[sortperm(I)])
    !issorted(J) && (J = J[sortperm(J)])

    ((I[end] > size(A,1)) || (J[end] > size(A,2))) && throw(DimensionMismatch(""))

    iptr = indptrA = getindptr(A)
    ival = indvalA = getindval(A)
    nzval = nzvalA = A.nzval

    SPX = (sptype(A) === CSC) ? J : I
    NSPX = (sptype(A) === CSC) ? I : J

    ncidx = 1
    ndel = 0
    for cidx in 1:spdim(A)
        rrange = iptr[cidx]:(iptr[cidx+1]-1)
        (ndel > 0) && (indptrA[cidx] = iptr[cidx] - ndel)
        if isempty(rrange) || !(cidx in SPX) 
            nincl = length(rrange)
            if(ndel > 0) && !isempty(rrange) 
                copy!(indvalA, ncidx, ival, rrange[1], nincl)
                copy!(nzvalA, ncidx, nzval, rrange[1], nincl)
            end
            ncidx += nincl
        else
            for ridx in rrange
                if ival[ridx] in NSPX
                    if ndel == 0
                        indptrA = copy(iptr)
                        indvalA = copy(ival)
                        nzvalA = copy(nzval)
                    end
                    ndel += 1
                else
                    if ndel > 0
                        indvalA[ncidx] = ival[ridx]
                        nzvalA[ncidx] = nzval[ridx]
                    end
                    ncidx += 1
                end
            end
        end
    end

    if ndel > 0
        indptrA[end] = ncidx
        deleteat!(indvalA, ncidx:nnzA)
        deleteat!(nzvalA, ncidx:nnzA)
        
        setindptr!(A, indptrA)
        setindval!(A, indvalA)
        A.nzval = nzvalA
    end
    return A
end

setindex!{Tv,Ti,T<:Integer}(A::CompressedSparseMatrix{Tv,Ti}, S::Matrix, I::AbstractVector{T}, J::AbstractVector{T}) =
      setindex!(A, convert(typeof(A), S), I, J)

setindex!{Tv,Ti,T<:Integer}(A::CompressedSparseMatrix{Tv,Ti}, v::AbstractVector, I::AbstractVector{T}, j::Integer) = setindex!(A, v, I, [j])
setindex!{Tv,Ti,T<:Integer}(A::CompressedSparseMatrix{Tv,Ti}, v::AbstractVector, i::Integer, J::AbstractVector{T}) = setindex!(A, v, [i], J)
setindex!{Tv,Ti,T<:Integer}(A::CompressedSparseMatrix{Tv,Ti}, v::AbstractVector, I::AbstractVector{T}, J::AbstractVector{T}) =
      setindex!(A, reshape(v, length(I), length(J)), I, J)


# A[I,J] = B
function setindex!{Tv,Ti,T<:Integer}(A::CompressedSparseMatrix{Tv,Ti}, B::CompressedSparseMatrix{Tv,Ti}, I::AbstractVector{T}, J::AbstractVector{T})
    (sptype(A) == sptype(B)) || (B = convert(typeof(A), B))
    Ts = sptype(A)
    (size(B,1) != length(I) || size(B,2) != length(J)) && throw(DimensionMismatch(""))

    issortedI = issorted(I); issortedJ = issorted(J)

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

    (indptrS, indvalS, nzvalS) = setindex_general!(getindptr(A), getindval(A), A.nzval, nspdim(A), 
                                        getindptr(B), getindval(B), B.nzval, 
                                        (Ts===CSC)?I:J, (Ts===CSC)?J:I)
    setindptr!(A, indptrS)
    setindval!(A, indvalS)
    A.nzval = nzvalS
    return A
end

function setindex_general!{Tv,Ti,T<:Integer}(indptrA::Vector{Ti}, indvalA::Vector{Ti}, nzvalA::Vector{Tv}, nspsz::Int,
            indptrB::Vector{Ti}, indvalB::Vector{Ti}, nzvalB::Vector{Tv},
            I::AbstractVector{T}, J::AbstractVector{T})
    nJ = length(J)

    nnzS = indptrA[end] + indptrB[end] - 2
    indptrS = Array(Ti, length(indptrA))
    indvalS = Array(Ti, nnzS)
    nzvalS = Array(Tv, nnzS)
    
    indptrS[1] = 1
    colB = 1
    asgn_col = J[colB]

    I_asgn = falses(nspsz)
    I_asgn[I] = true

    ptrS = 1

    for col = 1:(length(indptrA)-1)

        # Copy column of A if it is not being assigned into
        if colB > nJ || col != J[colB]
            indptrS[col+1] = indptrS[col] + (indptrA[col+1]-indptrA[col])
            
            for k = indptrA[col]:indptrA[col+1]-1
                indvalS[ptrS] = indvalA[k]
                nzvalS[ptrS] = nzvalA[k]
                ptrS += 1
            end
            continue
        end

        ptrA::Int  = indptrA[col]
        stopA::Int = indptrA[col+1]
        ptrB::Int  = indptrB[colB]
        stopB::Int = indptrB[colB+1]

        while ptrA < stopA && ptrB < stopB
            rowA = indvalA[ptrA]
            rowB = I[indvalB[ptrB]]
            if rowA < rowB
                if ~I_asgn[rowA]
                    indvalS[ptrS] = rowA
                    nzvalS[ptrS] = nzvalA[ptrA]
                    ptrS += 1
                end
                ptrA += 1
            elseif rowB < rowA
                indvalS[ptrS] = rowB
                nzvalS[ptrS] = nzvalB[ptrB]
                ptrS += 1
                ptrB += 1
            else
                indvalS[ptrS] = rowB
                nzvalS[ptrS] = nzvalB[ptrB]
                ptrS += 1
                ptrB += 1
                ptrA += 1
            end
        end

        while ptrA < stopA
            rowA = indvalA[ptrA]
            if ~I_asgn[rowA]
                indvalS[ptrS] = rowA
                nzvalS[ptrS] = nzvalA[ptrA]
                ptrS += 1
            end
            ptrA += 1
        end

        while ptrB < stopB
            rowB = I[indvalB[ptrB]]
            indvalS[ptrS] = rowB
            nzvalS[ptrS] = nzvalB[ptrB]
            ptrS += 1
            ptrB += 1
        end

        indptrS[col+1] = ptrS
        colB += 1
    end

    deleteat!(indvalS, indptrS[end]:length(indvalS))
    deleteat!(nzvalS, indptrS[end]:length(nzvalS))
    (indptrS, indvalS, nzvalS)
end


# Logical setindex!

setindex!(A::CompressedSparseMatrix, x::Matrix, I::Integer, J::AbstractVector{Bool}) = setindex!(A, sparse(x, format=sptype(A)), I, find(J))
setindex!(A::CompressedSparseMatrix, x::Matrix, I::AbstractVector{Bool}, J::Integer) = setindex!(A, sparse(x, format=sptype(A)), find(I), J)
setindex!(A::CompressedSparseMatrix, x::Matrix, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = setindex!(A, sparse(x, format=sptype(A)), find(I), find(J))
setindex!{T<:Integer}(A::CompressedSparseMatrix, x::Matrix, I::AbstractVector{T}, J::AbstractVector{Bool}) = setindex!(A, sparse(x, format=sptype(A)), I, find(J))
setindex!{T<:Integer}(A::CompressedSparseMatrix, x::Matrix, I::AbstractVector{Bool}, J::AbstractVector{T}) = setindex!(A, sparse(x, format=sptype(A)), find(I),J)

setindex!(A::Matrix, x::CompressedSparseMatrix, I::Integer, J::AbstractVector{Bool}) = setindex!(A, full(x), I, find(J))
setindex!(A::Matrix, x::CompressedSparseMatrix, I::AbstractVector{Bool}, J::Integer) = setindex!(A, full(x), find(I), J)
setindex!(A::Matrix, x::CompressedSparseMatrix, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = setindex!(A, full(x), find(I), find(J))
setindex!{T<:Integer}(A::Matrix, x::CompressedSparseMatrix, I::AbstractVector{T}, J::AbstractVector{Bool}) = setindex!(A, full(x), I, find(J))
setindex!{T<:Integer}(A::Matrix, x::CompressedSparseMatrix, I::AbstractVector{Bool}, J::AbstractVector{T}) = setindex!(A, full(x), find(I), J)

function setindex!(A::CompressedSparseMatrix, x, I::AbstractArray{Bool,2}) 
    checkbounds(A, I)
    setindex!(A, x, find(I))
end

sortidx_colfirst{T<:Real}(I::AbstractVector{T}, sz::NTuple{2,Int}) = sortperm(I)
function sortidx_rowfirst{T<:Real}(I::AbstractVector{T}, sz::NTuple{2,Int})
    nr = sz[1]
    NI = I .- 1
    l = length(NI)
    @inbounds for i in 1:l
        NI[i] = I[i] - nr * div(NI[i], nr)
    end
    sortperm(NI)
end

function setindex!{Tv,Ti,T<:Real}(A::CompressedSparseMatrix{Tv,Ti}, x, I::AbstractVector{T})
    n = length(I)
    (n == 0) && (return A)

    indptrB = indptrA = getindptr(A); indvalB = indvalA = getindval(A); nzvalB = nzvalA = A.nzval; 
    szA = size(A); caxis = spaxis(A); ncaxis = nspaxis(A)
    nadd = ndel = 0
    bidx = aidx = 0

    S = isa(A, SparseMatrixCSC) ? sortidx_colfirst(I, szA) : sortidx_rowfirst(I, szA)
    sxidx = r1 = r2 = 0

    n = length(I)
    lastcidx = 0
    for xidx in 1:n
        sxidx = S[xidx]
        (sxidx < n) && (I[sxidx] == I[sxidx+1]) && continue

        subA = ind2sub(szA, I[sxidx])
        ncidxA = subA[ncaxis]
        cidxA = subA[caxis]
        v = isa(x, AbstractArray) ? x[sxidx] : x

        if cidxA > lastcidx
            r1 = int(indptrA[cidxA])
            r2 = int(indptrA[cidxA+1] - 1)

            # copy from last position till current column
            if (nadd > 0) || (ndel > 0)
                ndiff = nadd - ndel
                @inbounds for i in (lastcidx+1):cidxA
                    indptrB[i] = indptrA[i] + ndiff
                end
                copylen = r1 - aidx
                if copylen > 0
                    copy!(indvalB, bidx, indvalA, aidx, copylen)
                    copy!(nzvalB, bidx, nzvalA, aidx, copylen)
                    aidx += copylen
                    bidx += copylen
                end
            else
                aidx = bidx = r1
            end
            lastcidx = cidxA
        end 

        if r1 <= r2
            copylen = searchsortedfirst(indvalA, ncidxA, r1, r2, Forward) - r1
            if copylen > 0
                if (nadd > 0) || (ndel > 0)
                    copy!(indvalB, bidx, indvalA, r1, copylen)
                    copy!(nzvalB, bidx, nzvalA, r1, copylen)
                end
                bidx += copylen
                r1 += copylen
                aidx += copylen
            end
        end

        # 0: no change, 1: update, 2: delete, 3: add new
        mode = ((r1 <= r2) && (indvalA[r1] == ncidxA)) ? ((v == 0) ? 2 : 1) : ((v == 0) ? 0 : 3)

        if (mode > 1) && (nadd == 0) && (ndel == 0)
            # copy storage to take changes
            indptrB = copy(indptrA)
            memreq = (x == 0) ? 0 : n
            indvalB = Array(Ti, length(indvalA)+memreq); copy!(indvalB, 1, indvalA, 1, r1-1)
            nzvalB = Array(Tv, length(nzvalA)+memreq); copy!(nzvalB, 1, nzvalA, 1, r1-1)
        end
        if mode == 1
            indvalB[bidx] = ncidxA
            nzvalB[bidx] = v
            bidx += 1
            aidx += 1
            r1 += 1
        elseif mode == 2
            r1 += 1
            aidx += 1
            ndel += 1
        elseif mode == 3
            indvalB[bidx] = ncidxA
            nzvalB[bidx] = v
            bidx += 1
            nadd += 1
        end
    end

    # copy the rest
    if (nadd > 0) || (ndel > 0)
        ndiff = nadd - ndel
        @inbounds for i in (lastcidx+1):length(indptrA)
            indptrB[i] = indptrA[i] + ndiff
        end
        r1 = indptrA[end]-1
        copylen = r1 - aidx + 1
        if copylen > 0
            copy!(indvalB, bidx, indvalA, aidx, copylen)
            copy!(nzvalB, bidx, nzvalA, aidx, copylen)
            aidx += copylen
            bidx += copylen
        end

        n = length(nzvalB)
        if n > (bidx-1)
            deleteat!(nzvalB, bidx:n)
            deleteat!(indvalB, bidx:n)
        end
        A.nzval = nzvalB; setindval!(A, indvalB); setindptr!(A, indptrB)
    end
    A
end

#=
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
    for xidx in 1:n
        sxidx = S[xidx]
        (sxidx < n) && (I[sxidx] == I[sxidx+1]) && continue

        row,col = ind2sub(szA, I[sxidx])
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
    if (nadd > 0) || (ndel > 0)
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
=#

# Sparse concatenation

function cat_along_ncaxis(X::CompressedSparseMatrix...)
    num = length(X)
    mX = [ nspdim(x) for x in X ] 
    nX = [ spdim(x) for x in X ]
    n = nX[1]
    for i = 2 : num
        if nX[i] != n; throw(DimensionMismatch("")); end
    end
    m = sum(mX)

    Tv = promote_type(map(x->eltype(x.nzval), X)...)
    Ti = promote_type(map(x->eltype(getindval(x)), X)...)

    iptr = Array(Ti, n + 1)
    nnzX = [ nnz(x) for x in X ]
    nnz_res = sum(nnzX)
    ival = Array(Ti, nnz_res)
    nzval = Array(Tv, nnz_res)

    iptr[1] = 1
    for c = 1 : n
        mX_sofar = 0
        rr1 = iptr[c]
        for i = 1 : num
            rX1 = getindptr(X[i])[c]
            rX2 = getindptr(X[i])[c + 1] - 1
            rr2 = rr1 + (rX2 - rX1)

            ival[rr1 : rr2] = getindval(X[i])[rX1 : rX2] .+ mX_sofar
            nzval[rr1 : rr2] = X[i].nzval[rX1 : rX2]
            mX_sofar += mX[i]
            rr1 = rr2 + 1
        end
        iptr[c + 1] = rr1
    end
    (m, n, iptr, ival, nzval)
end

function vcat(X::SparseMatrixCSC...)
    (ncdim, cdim, iptr, ival, nzval) = cat_along_ncaxis(X...)
    SparseMatrixCSC(ncdim, cdim, iptr, ival, nzval)    
end
function hcat(X::SparseMatrixCSR...)
    (ncdim, cdim, iptr, ival, nzval) = cat_along_ncaxis(X...)
    SparseMatrixCSR(cdim, ncdim, iptr, ival, nzval)    
end

function cat_along_caxis(X::CompressedSparseMatrix...)
    num = length(X)
    mX = [ nspdim(x) for x in X ]
    nX = [ spdim(x) for x in X ]
    m = mX[1]
    for i = 2 : num
        if mX[i] != m; throw(DimensionMismatch("")); end
    end
    n = sum(nX)

    Tv = promote_type(map(x->eltype(x.nzval), X)...)
    Ti = promote_type(map(x->eltype(getindval(x)), X)...)

    iptr = Array(Ti, n + 1)
    nnzX = [ nnz(x) for x in X ]
    nnz_res = sum(nnzX)
    ival = Array(Ti, nnz_res)
    nzval = Array(Tv, nnz_res)

    nnz_sofar = 0
    nX_sofar = 0
    for i = 1 : num
        iptr[(1 : nX[i] + 1) + nX_sofar] = getindptr(X[i]) .+ nnz_sofar
        ival[(1 : nnzX[i]) + nnz_sofar] = getindval(X[i])
        nzval[(1 : nnzX[i]) + nnz_sofar] = X[i].nzval
        nnz_sofar += nnzX[i]
        nX_sofar += nX[i]
    end

    (m, n, iptr, ival, nzval)
end

function hcat(X::SparseMatrixCSC...)
    (ncdim, cdim, iptr, ival, nzval) = cat_along_caxis(X...)
    SparseMatrixCSC(ncdim, cdim, iptr, ival, nzval)
end

function vcat(X::SparseMatrixCSR...)
    (ncdim, cdim, iptr, ival, nzval) = cat_along_caxis(X...)
    SparseMatrixCSR(cdim, ncdim, iptr, ival, nzval)
end

function hvcat{T<:CompressedSparseMatrix}(rows::(Int...), X::T...)
    nbr = length(rows)  # number of block rows

    tmp_rows = Array(T, nbr)
    k = 0
    for i = 1 : nbr
        tmp_rows[i] = hcat(X[(1 : rows[i]) + k]...)
        k += rows[i]
    end
    vcat(tmp_rows...)
end

function blkdiag{T<:CompressedSparseMatrix}(X::T...)
    num = length(X)
    mX = [ nspdim(x) for x in X ]
    nX = [ spdim(x) for x in X ]
    m = sum(mX)
    n = sum(nX)

    Tv = promote_type(map(x->eltype(x.nzval), X)...)
    Ti = promote_type(map(x->eltype(getindval(x)), X)...)

    iptr = Array(Ti, n + 1)
    nnzX = [ nnz(x) for x in X ]
    nnz_res = sum(nnzX)
    ival = Array(Ti, nnz_res)
    nzval = Array(Tv, nnz_res)

    nnz_sofar = 0
    nX_sofar = 0
    mX_sofar = 0
    for i = 1 : num
        iptr[(1 : nX[i] + 1) + nX_sofar] = getindptr(X[i]) .+ nnz_sofar
        ival[(1 : nnzX[i]) + nnz_sofar] = getindval(X[i]) .+ mX_sofar
        nzval[(1 : nnzX[i]) + nnz_sofar] = X[i].nzval
        nnz_sofar += nnzX[i]
        nX_sofar += nX[i]
        mX_sofar += mX[i]
    end

    if T <: SparseMatrixCSC
        SparseMatrixCSC(m, n, iptr, ival, nzval)
    else
        SparseMatrixCSR(n, m, iptr, ival, nzval)
    end
end

## Structure query functions

function issym(A::CompressedSparseMatrix)
    m, n = size(A)
    if m != n; return false; end
    return countnz(A - A.') == 0
end

function ishermitian(A::CompressedSparseMatrix)
    m, n = size(A)
    if m != n; return false; end
    return countnz(A - A') == 0
end

function istric{Tv}(A::CompressedSparseMatrix{Tv})
    iptr = getindptr(A)
    ival = getindval(A)
    nzval = A.nzval
    for spidx = 1:min(spdim(A),nspdim(A)-1)
        l1 = iptr[spidx+1]-1
        for i = 0 : (l1 - iptr[spidx])
            if ival[l1-i] <= spidx
                break
            end
            if nzval[l1-i] != 0
                return false
            end
        end
    end
    return true
end

function istrinc{Tv}(A::CompressedSparseMatrix{Tv})
    iptr = getindptr(A)
    ival = getindval(A)
    nzval = A.nzval
    for spidx = 2:spdim(A)
        for i = iptr[spidx] : (iptr[spidx+1]-1)
            if ival[i] >= spidx
                break
            end
            if nzval[i] != 0
                return false
            end
        end
    end
    return true
end

istriu{Tv}(A::SparseMatrixCSC{Tv}) = istric(A)
istril{Tv}(A::SparseMatrixCSC{Tv}) = istrinc(A)
istriu{Tv}(A::SparseMatrixCSR{Tv}) = istrinc(A)
istril{Tv}(A::SparseMatrixCSR{Tv}) = istric(A)

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

## expand a indptr or rowptr into a dense index vector
function expandptr{T<:Integer}(V::Vector{T})
    if V[1] != 1 throw(ArgumentError("first index must be one")) end
    res = similar(V, (int64(V[end]-1),))
    for i in 1:(length(V)-1), j in V[i]:(V[i+1] - 1) res[j] = i end
    res
end

## diag and related using an iterator

type SpDiagIterator{Tv,Ti}
    A::CompressedSparseMatrix{Tv,Ti}
    n::Int
end
SpDiagIterator(A::CompressedSparseMatrix) = SpDiagIterator(A,minimum(size(A)))

length(d::SpDiagIterator) = d.n
start(d::SpDiagIterator) = 1
done(d::SpDiagIterator, j) = j > d.n

function next{Tv}(d::SpDiagIterator{Tv}, j)
    p = getindptr(d.A); i = getindval(d.A);
    r1 = int(p[j])
    r2 = int(p[j+1]-1)
    (r1 > r2) && (return (zero(Tv), j+1))
    r1 = searchsortedfirst(i, j, r1, r2, Forward)
    (((r1 > r2) || (i[r1] != j)) ? zero(Tv) : d.A.nzval[r1], j+1)
end

function trace{Tv}(A::CompressedSparseMatrix{Tv})
    if size(A,1) != size(A,2)
        throw(DimensionMismatch("expected square matrix"))
    end
    s = zero(Tv)
    for d in SpDiagIterator(A)
        s += d
    end
    s
end

diag(A::CompressedSparseMatrix) = [d for d in SpDiagIterator(A)]

function diagm{Tv,Ti}(v::CompressedSparseMatrix{Tv,Ti})
    if (size(v,1) != 1 && size(v,2) != 1)
        throw(DimensionMismatch("input should be nx1 or 1xn"))
    end

    n = length(v)
    numnz = nnz(v)
    iptr = Array(Ti, n+1)
    ival = Array(Ti, numnz)
    nzval = Array(Tv, numnz)

    if size(v,1) == 1
        copy!(iptr, 1, getindptr(v), 1, n+1)
        ptr = 1
        for col = 1:n
            if iptr[col] != iptr[col+1]
                ival[ptr] = col
                nzval[ptr] = v.nzval[ptr]
                ptr += 1
            end
        end
    else
        copy!(ival, 1, getindval(v), 1, numnz)
        copy!(nzval, 1, v.nzval, 1, numnz)
        iptr[1] = 1
        ptr = 1
        col = 1
        while col <= n && ptr <= numnz
            while ival[ptr] > col
                iptr[col+1] = iptr[col]
                col += 1
            end
            iptr[col+1] = iptr[col] + 1
            ptr += 1
            col += 1
        end
        if col <= n
            iptr[(col+1):(n+1)] = iptr[col]
        end
    end

    sparse(n, n, CompressedSparseStore(iptr, ival, nzval), format=sptype(v))
end
