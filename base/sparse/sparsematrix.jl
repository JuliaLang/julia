# This file is a part of Julia. License is MIT: http://julialang.org/license

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

    function SparseMatrixCSC(m::Integer, n::Integer, colptr::Vector{Ti}, rowval::Vector{Ti}, nzval::Vector{Tv})
        m < 0 && throw(ArgumentError("number of rows (m) must be ≥ 0, got $m"))
        n < 0 && throw(ArgumentError("number of columns (n) must be ≥ 0, got $n"))
        new(Int(m), Int(n), colptr, rowval, nzval)
    end
end
function SparseMatrixCSC(m::Integer, n::Integer, colptr::Vector, rowval::Vector, nzval::Vector)
    Tv = eltype(nzval)
    Ti = promote_type(eltype(colptr), eltype(rowval))
    SparseMatrixCSC{Tv,Ti}(m, n, colptr, rowval, nzval)
end

size(S::SparseMatrixCSC) = (S.m, S.n)

"""
    nnz(A)

Returns the number of stored (filled) elements in a sparse array.
"""
nnz(S::SparseMatrixCSC) = Int(S.colptr[end]-1)
countnz(S::SparseMatrixCSC) = countnz(S.nzval)

"""
    nonzeros(A)

Return a vector of the structural nonzero values in sparse array `A`. This
includes zeros that are explicitly stored in the sparse array. The returned
vector points directly to the internal nonzero storage of `A`, and any
modifications to the returned vector will mutate `A` as well. See `rowvals(A)`
and `nzrange(A, col)`.
"""
nonzeros(S::SparseMatrixCSC) = S.nzval

"""
    rowvals(A::SparseMatrixCSC)

Return a vector of the row indices of `A`. Any modifications to the returned
vector will mutate `A` as well. Providing access to how the row indices are
stored internally can be useful in conjuction with iterating over structural
nonzero values. See also `nonzeros(A)` and `nzrange(A, col)`.
"""
rowvals(S::SparseMatrixCSC) = S.rowval

"""
    nzrange(A::SparseMatrixCSC, col)

Return the range of indices to the structural nonzero values of a sparse matrix
column. In conjunction with `nonzeros(A)` and `rowvals(A)`, this allows for
convenient iterating over a sparse matrix :

    A = sparse(I,J,V)
    rows = rowvals(A)
    vals = nonzeros(A)
    m, n = size(A)
    for i = 1:n
       for j in nzrange(A, i)
          row = rows[j]
          val = vals[j]
          # perform sparse wizardry...
       end
    end
"""
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
            if isassigned(S.nzval, k)
                showcompact(io, S.nzval[k])
            else
                print(io, Base.undef_ref_str)
            end
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
    return SparseMatrixCSC(mA, nA, colptr, rowval, nzval)
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

    return SparseMatrixCSC(mS, nS, colptr, rowval, nzval)
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

    return SparseMatrixCSC(mS, nS, colptr, rowval, nzval)
end

## Constructors

copy(S::SparseMatrixCSC) =
    SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), copy(S.nzval))

similar(S::SparseMatrixCSC, Tv::Type=eltype(S))   = SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), Array(Tv, length(S.nzval)))
similar{Tv,Ti,TvNew,TiNew}(S::SparseMatrixCSC{Tv,Ti}, ::Type{TvNew}, ::Type{TiNew}) = SparseMatrixCSC(S.m, S.n, convert(Array{TiNew},S.colptr), convert(Array{TiNew}, S.rowval), Array(TvNew, length(S.nzval)))
similar{Tv, N}(S::SparseMatrixCSC, ::Type{Tv}, d::NTuple{N, Integer}) = spzeros(Tv, d...)

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

function convert{Tv,Ti}(::Type{SparseMatrixCSC{Tv,Ti}}, M::AbstractMatrix)
    m, n = size(M)
    (I, J, V) = findnz(M)
    return sparse_IJ_sorted!(convert(Vector{Ti},I),
                             convert(Vector{Ti},J),
                             convert(Vector{Tv},V),
                             m, n)
end
convert{T}(::Type{AbstractMatrix{T}}, A::SparseMatrixCSC) = convert(SparseMatrixCSC{T}, A)
convert(::Type{Matrix}, S::SparseMatrixCSC) = full(S)


"""
    full(S)

Convert a sparse matrix or vector `S` into a dense matrix or vector.
"""
full

function full{Tv}(S::SparseMatrixCSC{Tv})
    # Handle cases where zero(Tv) is not defined but the array is dense.
    # (Should we really worry about this?)
    A = length(S) == nnz(S) ? Array(Tv, S.m, S.n) : zeros(Tv, S.m, S.n)
    for col = 1 : S.n, k = S.colptr[col] : (S.colptr[col+1]-1)
        A[S.rowval[k], col] = S.nzval[k]
    end
    return A
end

float(S::SparseMatrixCSC) = SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), float(copy(S.nzval)))

complex(S::SparseMatrixCSC) = SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), complex(copy(S.nzval)))

complex(A::SparseMatrixCSC, B::SparseMatrixCSC) = A + im*B

# Construct a sparse vector

# Note that unlike `vec` for arrays, this does not share data
vec(S::SparseMatrixCSC) = S[:]

"""
    sparse(A)

Convert an AbstractMatrix `A` into a sparse matrix.
"""
sparse{Tv}(A::AbstractMatrix{Tv}) = convert(SparseMatrixCSC{Tv,Int}, A)

sparse(S::SparseMatrixCSC) = copy(S)

sparse_IJ_sorted!(I,J,V,m,n) = sparse_IJ_sorted!(I,J,V,m,n,AddFun())

sparse_IJ_sorted!(I,J,V::AbstractVector{Bool},m,n) = sparse_IJ_sorted!(I,J,V,m,n,OrFun())

function sparse_IJ_sorted!{Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti},
                                        V::AbstractVector,
                                        m::Integer, n::Integer, combine::Union{Function,Func})

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

## sparse() can take its inputs in unsorted order (the parent method is now in csparse.jl)

dimlub(I) = length(I)==0 ? 0 : Int(maximum(I)) #least upper bound on required sparse matrix dimension

sparse(I,J,v::Number) = sparse(I, J, fill(v,length(I)), dimlub(I), dimlub(J), AddFun())

sparse(I,J,V::AbstractVector) = sparse(I, J, V, dimlub(I), dimlub(J), AddFun())

sparse(I,J,v::Number,m,n) = sparse(I, J, fill(v,length(I)), Int(m), Int(n), AddFun())

sparse(I,J,V::AbstractVector,m,n) = sparse(I, J, V, Int(m), Int(n), AddFun())

sparse(I,J,V::AbstractVector{Bool},m,n) = sparse(I, J, V, Int(m), Int(n), OrFun())

sparse(I,J,v::Number,m,n,combine::Union{Function,Func}) = sparse(I, J, fill(v,length(I)), Int(m), Int(n), combine)

function sparse(T::SymTridiagonal)
    m = length(T.dv)
    return sparse([1:m;2:m;1:m-1],[1:m;1:m-1;2:m],[T.dv;T.ev;T.ev], Int(m), Int(m))
end

function sparse(T::Tridiagonal)
    m = length(T.d)
    return sparse([1:m;2:m;1:m-1],[1:m;1:m-1;2:m],[T.d;T.dl;T.du], Int(m), Int(m))
end

function sparse(B::Bidiagonal)
    m = length(B.dv)
    B.isupper || return sparse([1:m;2:m],[1:m;1:m-1],[B.dv;B.ev], Int(m), Int(m)) # lower bidiagonal
    return sparse([1:m;1:m-1],[1:m;2:m],[B.dv;B.ev], Int(m), Int(m)) # upper bidiagonal
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



import Base.Random.GLOBAL_RNG
function sprand_IJ(r::AbstractRNG, m::Integer, n::Integer, density::AbstractFloat)
    ((m < 0) || (n < 0)) && throw(ArgumentError("invalid Array dimensions"))
    0 <= density <= 1 || throw(ArgumentError("$density not in [0,1]"))
    N = n*m

    I, J = Array(Int, 0), Array(Int, 0) # indices of nonzero elements
    sizehint!(I, round(Int,N*density))
    sizehint!(J, round(Int,N*density))

    # density of nonzero columns:
    L = log1p(-density)
    coldensity = -expm1(m*L) # = 1 - (1-density)^m
    colsparsity = exp(m*L) # = 1 - coldensity
    iL = 1/L

    rows = Array(Int, 0)
    for j in randsubseq(r, 1:n, coldensity)
        # To get the right statistics, we *must* have a nonempty column j
        # even if p*m << 1.   To do this, we use an approach similar to
        # the one in randsubseq to compute the expected first nonzero row k,
        # except given that at least one is nonzero (via Bayes' rule);
        # carefully rearranged to avoid excessive roundoff errors.
        k = ceil(log(colsparsity + rand(r)*coldensity) * iL)
        ik = k < 1 ? 1 : k > m ? m : Int(k) # roundoff-error/underflow paranoia
        randsubseq!(r, rows, 1:m-ik, density)
        push!(rows, m-ik+1)
        append!(I, rows)
        nrows = length(rows)
        Jlen = length(J)
        resize!(J, Jlen+nrows)
        @inbounds for i = Jlen+1:length(J)
            J[i] = j
        end
    end
    I, J
end

"""
```rst
..  sprand([rng],m,[n],p::AbstractFloat,[rfn])

Create a random length ``m`` sparse vector or ``m`` by ``n`` sparse matrix, in
which the probability of any element being nonzero is independently given by
``p`` (and hence the mean density of nonzeros is also exactly ``p``). Nonzero
values are sampled from the distribution specified by ``rfn``. The uniform
distribution is used in case ``rfn`` is not specified. The optional ``rng``
argument specifies a random number generator, see :ref:`Random Numbers
<random-numbers>`.
```
"""
function sprand{T}(r::AbstractRNG, m::Integer, n::Integer, density::AbstractFloat,
                rfn::Function, ::Type{T}=eltype(rfn(r,1)))
    N = m*n
    N == 0 && return spzeros(T,m,n)
    N == 1 && return rand(r) <= density ? sparse(rfn(r,1)) : spzeros(T,1,1)

    I,J = sprand_IJ(r, m, n, density)
    sparse_IJ_sorted!(I, J, rfn(r,length(I)), m, n, AddFun())  # it will never need to combine
end

function sprand{T}(m::Integer, n::Integer, density::AbstractFloat,
                rfn::Function, ::Type{T}=eltype(rfn(1)))
    N = m*n
    N == 0 && return spzeros(T,m,n)
    N == 1 && return rand() <= density ? sparse(rfn(1)) : spzeros(T,1,1)

    I,J = sprand_IJ(GLOBAL_RNG, m, n, density)
    sparse_IJ_sorted!(I, J, rfn(length(I)), m, n, AddFun())  # it will never need to combine
end

sprand(r::AbstractRNG, m::Integer, n::Integer, density::AbstractFloat) = sprand(r,m,n,density,rand,Float64)
sprand(m::Integer, n::Integer, density::AbstractFloat) = sprand(GLOBAL_RNG,m,n,density)
sprandn(r::AbstractRNG, m::Integer, n::Integer, density::AbstractFloat) = sprand(r,m,n,density,randn,Float64)

"""
    sprandn(m[,n],p::AbstractFloat)

Create a random sparse vector of length `m` or sparse matrix of size `m` by `n`
with the specified (independent) probability `p` of any entry being nonzero,
where nonzero values are sampled from the normal distribution.
"""
sprandn( m::Integer, n::Integer, density::AbstractFloat) = sprandn(GLOBAL_RNG,m,n,density)

truebools(r::AbstractRNG, n::Integer) = ones(Bool, n)
sprandbool(r::AbstractRNG, m::Integer, n::Integer, density::AbstractFloat) = sprand(r,m,n,density,truebools,Bool)

"""
    sprandbool(m[,n],p)

Create a random `m` by `n` sparse boolean matrix or length `m` sparse boolean
vector with the specified (independent) probability `p` of any entry being
`true`.
"""
sprandbool(m::Integer, n::Integer, density::AbstractFloat) = sprandbool(GLOBAL_RNG,m,n,density)

"""
    spones(S)

Create a sparse array with the same structure as that of `S`, but with every nonzero
element having the value `1.0`.
"""
spones{T}(S::SparseMatrixCSC{T}) =
     SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), ones(T, S.colptr[end]-1))

"""
    spzeros(m[,n])

Create a sparse vector of length `m` or sparse matrix of size `m x n`. This
sparse array will not contain any nonzero values. No storage will be allocated
for nonzero values during construction.
"""
spzeros(m::Integer, n::Integer) = spzeros(Float64, m, n)
spzeros(Tv::Type, m::Integer, n::Integer) = spzeros(Tv, Int, m, n)
function spzeros(Tv::Type, Ti::Type, m::Integer, n::Integer)
    ((m < 0) || (n < 0)) && throw(ArgumentError("invalid Array dimensions"))
    SparseMatrixCSC(m, n, ones(Ti, n+1), Array(Ti, 0), Array(Tv, 0))
end


speye(n::Integer) = speye(Float64, n)
speye(T::Type, n::Integer) = speye(T, n, n)
speye(m::Integer, n::Integer) = speye(Float64, m, n)
speye{T}(S::SparseMatrixCSC{T}) = speye(T, size(S, 1), size(S, 2))
eye(S::SparseMatrixCSC) = speye(S)

"""
    speye(type,m[,n])

Create a sparse identity matrix of specified type of size `m x m`. In case `n` is supplied,
create a sparse identity matrix of size `m x n`.
"""
function speye(T::Type, m::Integer, n::Integer)
    ((m < 0) || (n < 0)) && throw(ArgumentError("invalid Array dimensions"))
    x = min(m,n)
    rowval = [1:x;]
    colptr = [rowval; fill(Int(x+1), n+1-x)]
    nzval  = ones(T, x)
    return SparseMatrixCSC(m, n, colptr, rowval, nzval)
end

function one{T}(S::SparseMatrixCSC{T})
    m,n = size(S)
    if m != n; throw(DimensionMismatch("multiplicative identity only defined for square matrices")); end
    speye(T, m)
end

## Unary arithmetic and boolean operators

macro _unary_op_nz2z_z2z(op,A,Tv,Ti)
    esc(quote
        nfilledA = nnz($A)
        colptrB = Array($Ti, $A.n+1)
        rowvalB = Array($Ti, nfilledA)
        nzvalB = Array($Tv, nfilledA)

        nzvalA = $A.nzval
        colptrA = $A.colptr
        rowvalA = $A.rowval

        k = 0 # number of additional zeros introduced by op(A)
        @inbounds for i = 1 : $A.n
            colptrB[i] = colptrA[i] - k
            for j = colptrA[i] : colptrA[i+1]-1
                opAj = $(op)(nzvalA[j])
                if opAj == 0
                    k += 1
                else
                    rowvalB[j - k] = rowvalA[j]
                    nzvalB[j - k] = opAj
                end
            end
        end
        colptrB[end] = $A.colptr[end] - k
        deleteat!(rowvalB, colptrB[end]:nfilledA)
        deleteat!(nzvalB, colptrB[end]:nfilledA)
        return SparseMatrixCSC($A.m, $A.n, colptrB, rowvalB, nzvalB)
    end) # quote
end

# Operations that may map nonzeros to zero, and zero to zero
# Result is sparse
for op in (:ceil, :floor, :trunc, :round,
           :sin, :tan, :asin, :atan,
           :sinh, :tanh, :asinh, :atanh,
           :sinpi, :cosc,
           :sind, :tand, :asind, :atand)
    @eval begin
        $(op){Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}) = @_unary_op_nz2z_z2z($op,A,Tv,Ti)
    end # quote
end # macro

for op in (:real, :imag)
    @eval begin
        ($op){Tv<:Complex,Ti}(A::SparseMatrixCSC{Tv,Ti}) = @_unary_op_nz2z_z2z($op,A,Tv.parameters[1],Ti)
    end # quote
end # macro
real{Tv<:Number,Ti}(A::SparseMatrixCSC{Tv,Ti}) = copy(A)
imag{Tv<:Number,Ti}(A::SparseMatrixCSC{Tv,Ti}) = spzeros(Tv, Ti, A.m, A.n)

for op in (:ceil, :floor, :trunc, :round)
    @eval begin
        ($op){T,Tv,Ti}(::Type{T},A::SparseMatrixCSC{Tv,Ti}) = @_unary_op_nz2z_z2z($op,A,T,Ti)
    end # quote
end # macro



# Operations that map nonzeros to nonzeros, and zeros to zeros
# Result is sparse
for op in (:-, :log1p, :expm1)
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

function abs{Tv<:Complex,Ti}(A::SparseMatrixCSC{Tv,Ti})
    T = Tv.parameters[1]
    (T <: Integer) && (T = (T <: BigInt) ? BigFloat : Float64)
    @_unary_op_nz2z_z2z(abs,A,T,Ti)
end
abs2{Tv<:Complex,Ti}(A::SparseMatrixCSC{Tv,Ti}) = @_unary_op_nz2z_z2z(abs2,A,Tv.parameters[1],Ti)
for op in (:abs, :abs2)
    @eval begin
        function ($op){Tv<:Number,Ti}(A::SparseMatrixCSC{Tv,Ti})
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


## Broadcasting kernels specialized for returning a SparseMatrixCSC

# Operations with zero result if both operands are zero
function gen_broadcast_body_sparse(f::Function, is_first_sparse::Bool)
    F = Expr(:quote, f)
    quote
        Base.Broadcast.check_broadcast_shape(size(B), A_1)
        Base.Broadcast.check_broadcast_shape(size(B), A_2)

        colptrB = B.colptr; rowvalB = B.rowval; nzvalB = B.nzval
        colptr1 = A_1.colptr; rowval1 = A_1.rowval; nzval1 = A_1.nzval
        colptr2 = A_2.colptr; rowval2 = A_2.rowval; nzval2 = A_2.nzval

        nnzB = nnz(A_1) * div(B.n, A_1.n) * div(B.m, A_1.m)  +
               nnz(A_2) * div(B.n, A_2.n) * div(B.m, A_2.m)
        if length(rowvalB) < nnzB
            resize!(rowvalB, nnzB)
        end
        if length(nzvalB) < nnzB
            resize!(nzvalB, nnzB)
        end
        z = zero(Tv)

        ptrB = 1
        colptrB[1] = 1

        Tr1 = eltype(rowval1)
        Tr2 = eltype(rowval2)

        @inbounds for col = 1:B.n
            ptr1::Int  = A_1.n == 1 ? colptr1[1] : colptr1[col]
            stop1::Int = A_1.n == 1 ? colptr1[2] : colptr1[col+1]
            ptr2::Int  = A_2.n == 1 ? colptr2[1] : colptr2[col]
            stop2::Int = A_2.n == 1 ? colptr2[2] : colptr2[col+1]

            if  A_1.m ==  A_2.m || (A_1.m == 1 && ptr1 == stop1) || (A_2.m == 1 && ptr2 == stop2)
                while ptr1 < stop1 && ptr2 < stop2
                    row1 = rowval1[ptr1]
                    row2 = rowval2[ptr2]
                    if row1 < row2
                        res = ($F)(nzval1[ptr1], z)
                        if res != z
                            rowvalB[ptrB] = row1
                            nzvalB[ptrB] = res
                            ptrB += 1
                        end
                        ptr1 += 1
                    elseif row2 < row1
                        res = ($F)(z, nzval2[ptr2])
                        if res != z
                            rowvalB[ptrB] = row2
                            nzvalB[ptrB] = res
                            ptrB += 1
                        end
                        ptr2 += 1
                    else
                        res = ($F)(nzval1[ptr1], nzval2[ptr2])
                        if res != z
                            rowvalB[ptrB] = row1
                            nzvalB[ptrB] = res
                            ptrB += 1
                        end
                        ptr1 += 1
                        ptr2 += 1
                    end
                end

                while ptr1 < stop1
                    res = ($F)(nzval1[ptr1], z)
                    if res != z
                        row1 = rowval1[ptr1]
                        rowvalB[ptrB] = row1
                        nzvalB[ptrB] = res
                        ptrB += 1
                    end
                    ptr1 += 1
                end

                while ptr2 < stop2
                    res = ($F)(z, nzval2[ptr2])
                    if res != z
                        row2 = rowval2[ptr2]
                        rowvalB[ptrB] = row2
                        nzvalB[ptrB] = res
                        ptrB += 1
                    end
                    ptr2 += 1
                end
            elseif  A_1.m != 1  # A_1.m != 1 && A_2.m == 1
                scalar2 = A_2.nzval[ptr2]
                row1 = ptr1 < stop1 ? rowval1[ptr1] : -one(Tr1)
                for row2 = one(Tr2):Tr2(B.m)
                    if ptr1 >= stop1 || row1 != row2
                        res = ($F)(z, scalar2)
                        if res != z
                            rowvalB[ptrB] = row2
                            nzvalB[ptrB] = res
                            ptrB += 1
                        end
                    else
                        res = ($F)(nzval1[ptr1], scalar2)
                        if res != z
                            rowvalB[ptrB] = row1
                            nzvalB[ptrB] = res
                            ptrB += 1
                        end
                        ptr1 += 1
                        row1 = ptr1 < stop1 ? rowval1[ptr1] :  -one(Tr1)
                    end
                end
            else  # A_1.m == 1 && A_2.m != 1
                scalar1 = nzval1[ptr1]
                row2 = ptr2 < stop2 ? rowval2[ptr2] :  -one(Tr2)
                for row1 = one(Tr1):Tr1(B.m)
                    if ptr2 >= stop2 || row1 != row2
                        res = ($F)(scalar1, z)
                        if res != z
                            rowvalB[ptrB] = row1
                            nzvalB[ptrB] = res
                            ptrB += 1
                        end
                    else
                        res = ($F)(scalar1, nzval2[ptr2])
                        if res != z
                            rowvalB[ptrB] = row2
                            nzvalB[ptrB] = res
                            ptrB += 1
                        end
                        ptr2 += 1
                        row2 = ptr2 < stop2 ? rowval2[ptr2] :  -one(Tr2)
                    end
                end
            end
            colptrB[col+1] = ptrB
        end
        deleteat!(rowvalB, colptrB[end]:length(rowvalB))
        deleteat!(nzvalB, colptrB[end]:length(nzvalB))
        nothing
    end
end

function gen_broadcast_function_sparse(genbody::Function, f::Function, is_first_sparse::Bool)
    body = genbody(f, is_first_sparse)
    @eval let
        local _F_
        function _F_{Tv,Ti}(B::SparseMatrixCSC{Tv,Ti}, A_1, A_2)
            $body
        end
        _F_
    end
end

# Operations with zero result if any operand is zero
# A_1 or A_2 (or both) are sparse.
# is_first_sparse == true => A_1 is sparse
# is_first_sparse == false => A_2 is sparse
function gen_broadcast_body_zpreserving(f::Function, is_first_sparse::Bool)
    F = Expr(:quote, f)
    if is_first_sparse
        A1 = :(A_1)
        A2 = :(A_2)
        op1 = :(val1)
        op2 = :(val2)
    else
        A1 = :(A_2)
        A2 = :(A_1)
        op1 = :(val2)
        op2 = :(val1)
    end
    quote
        Base.Broadcast.check_broadcast_shape(size(B), $A1)
        Base.Broadcast.check_broadcast_shape(size(B), $A2)

        nnzB = nnz($A1) * div(B.n, ($A1).n) * div(B.m, ($A1).m)
        if length(B.rowval) < nnzB
            resize!(B.rowval, nnzB)
        end
        if length(B.nzval) < nnzB
            resize!(B.nzval, nnzB)
        end
        z = zero(Tv)

        ptrB = 1
        B.colptr[1] = 1

        @inbounds for col = 1:B.n
            ptr1::Int  = ($A1).n == 1 ? ($A1).colptr[1] : ($A1).colptr[col]
            stop1::Int = ($A1).n == 1 ? ($A1).colptr[2] : ($A1).colptr[col+1]
            col2 = size($A2, 2) == 1 ? 1 : col
            row = 1
            while ptr1 < stop1 && row <= B.m
                if ($A1).m != 1
                    row = ($A1).rowval[ptr1]
                end
                row2 = size($A2, 1) == 1 ? 1 : row
                val1 = ($A1).nzval[ptr1]
                val2 = ($A2)[row2,col2]
                res = ($F)($op1, $op2)
                if res != z
                    B.rowval[ptrB] = row
                    B.nzval[ptrB] = res
                    ptrB += 1
                end
                if ($A1).m != 1
                    ptr1 += 1
                else
                    row += 1
                end
            end
            B.colptr[col+1] = ptrB
        end
        deleteat!(B.rowval, B.colptr[end]:length(B.rowval))
        deleteat!(B.nzval, B.colptr[end]:length(B.nzval))
        nothing
    end
end

for (Bsig, A1sig, A2sig, gbb, funcname) in
    (
     (SparseMatrixCSC   , SparseMatrixCSC  ,  SparseMatrixCSC,  :gen_broadcast_body_sparse, :broadcast!),
     (SparseMatrixCSC   , SparseMatrixCSC  ,  Array,  :gen_broadcast_body_zpreserving, :broadcast_zpreserving!),
     (SparseMatrixCSC   , Array  ,  SparseMatrixCSC,  :gen_broadcast_body_zpreserving, :broadcast_zpreserving!),
     (SparseMatrixCSC   , Number  ,  SparseMatrixCSC,  :gen_broadcast_body_zpreserving, :broadcast_zpreserving!),
     (SparseMatrixCSC   , SparseMatrixCSC  ,  Number,  :gen_broadcast_body_zpreserving, :broadcast_zpreserving!),
     (SparseMatrixCSC   , BitArray  ,  SparseMatrixCSC,  :gen_broadcast_body_zpreserving, :broadcast_zpreserving!),
     (SparseMatrixCSC   , SparseMatrixCSC  ,  BitArray,  :gen_broadcast_body_zpreserving, :broadcast_zpreserving!),
     )
    @eval let cache = Dict{Function,Function}()
        global $funcname
        function $funcname(f::Function, B::$Bsig, A1::$A1sig, A2::$A2sig)
            func       = @get! cache  f  gen_broadcast_function_sparse($gbb, f, ($A1sig) <: SparseMatrixCSC)
            func(B, A1, A2)
            B
        end
    end  # let broadcast_cache
end


broadcast{Tv1,Ti1,Tv2,Ti2}(f::Function, A_1::SparseMatrixCSC{Tv1,Ti1}, A_2::SparseMatrixCSC{Tv2,Ti2}) =
                 broadcast!(f, spzeros(promote_type(Tv1, Tv2), promote_type(Ti1, Ti2), broadcast_shape(A_1, A_2)...), A_1, A_2)

broadcast_zpreserving!(args...) = broadcast!(args...)
broadcast_zpreserving(args...) = broadcast(args...)
broadcast_zpreserving{Tv1,Ti1,Tv2,Ti2}(f::Function, A_1::SparseMatrixCSC{Tv1,Ti1}, A_2::SparseMatrixCSC{Tv2,Ti2}) =
                 broadcast_zpreserving!(f, spzeros(promote_type(Tv1, Tv2), promote_type(Ti1, Ti2), broadcast_shape(A_1, A_2)...), A_1, A_2)
broadcast_zpreserving{Tv,Ti}(f::Function, A_1::SparseMatrixCSC{Tv,Ti}, A_2::Union{Array,BitArray,Number}) =
                 broadcast_zpreserving!(f, spzeros(promote_eltype(A_1, A_2), Ti, broadcast_shape(A_1, A_2)...), A_1, A_2)
broadcast_zpreserving{Tv,Ti}(f::Function, A_1::Union{Array,BitArray,Number}, A_2::SparseMatrixCSC{Tv,Ti}) =
                 broadcast_zpreserving!(f, spzeros(promote_eltype(A_1, A_2), Ti, broadcast_shape(A_1, A_2)...), A_1, A_2)


## Binary arithmetic and boolean operators

for (op, pro) in ((+,   :eltype_plus),
                  (-,   :eltype_plus),
                  (min, :promote_eltype),
                  (max, :promote_eltype),
                  (&,   :promote_eltype),
                  (|,   :promote_eltype),
                  ($,   :promote_eltype))
    body = gen_broadcast_body_sparse(op, true)
    OP = Symbol(string(op))
    @eval begin
        function ($OP){Tv1,Ti1,Tv2,Ti2}(A_1::SparseMatrixCSC{Tv1,Ti1}, A_2::SparseMatrixCSC{Tv2,Ti2})
            if size(A_1,1) != size(A_2,1) || size(A_1,2) != size(A_2,2)
                throw(DimensionMismatch(""))
            end
            Tv = ($pro)(A_1, A_2)
            B =  spzeros(Tv, promote_type(Ti1, Ti2), broadcast_shape(A_1, A_2)...)
            $body
            B
        end
    end
end # macro

(.+)(A::SparseMatrixCSC, B::Number) = full(A) .+ B
( +)(A::SparseMatrixCSC, B::Array ) = full(A)  + B
(.+)(A::Number, B::SparseMatrixCSC) = A .+ full(B)
( +)(A::Array , B::SparseMatrixCSC) = A  + full(B)

(.-)(A::SparseMatrixCSC, B::Number) = full(A) .- B
( -)(A::SparseMatrixCSC, B::Array ) = full(A)  - B
(.-)(A::Number, B::SparseMatrixCSC) = A .- full(B)
( -)(A::Array , B::SparseMatrixCSC) = A  - full(B)

(.*)(A::AbstractArray, B::AbstractArray) = broadcast_zpreserving(*, A, B)
(.*)(A::SparseMatrixCSC, B::Number) = SparseMatrixCSC(A.m, A.n, copy(A.colptr), copy(A.rowval), A.nzval .* B)
(.*)(A::Number, B::SparseMatrixCSC) = SparseMatrixCSC(B.m, B.n, copy(B.colptr), copy(B.rowval), A .* B.nzval)

(./)(A::SparseMatrixCSC, B::Number) = SparseMatrixCSC(A.m, A.n, copy(A.colptr), copy(A.rowval), A.nzval ./ B)
(./)(A::Number, B::SparseMatrixCSC) = (./)(A, full(B))
(./)(A::SparseMatrixCSC, B::Array) = (./)(full(A), B)
(./)(A::Array, B::SparseMatrixCSC) = (./)(A, full(B))
(./)(A::SparseMatrixCSC, B::SparseMatrixCSC) = (./)(full(A), full(B))

(.\)(A::SparseMatrixCSC, B::Number) = (.\)(full(A), B)
(.\)(A::Number, B::SparseMatrixCSC) = SparseMatrixCSC(B.m, B.n, copy(B.colptr), copy(B.rowval), A .\ B.nzval )
(.\)(A::SparseMatrixCSC, B::Array) = (.\)(full(A), B)
(.\)(A::Array, B::SparseMatrixCSC) = (.\)(A, full(B))
(.\)(A::SparseMatrixCSC, B::SparseMatrixCSC) = (.\)(full(A), full(B))

(.^)(A::SparseMatrixCSC, B::Number) =
    B==0 ? sparse(ones(typeof(one(eltype(A)).^B), A.m, A.n)) :
           SparseMatrixCSC(A.m, A.n, copy(A.colptr), copy(A.rowval), A.nzval .^ B)
(.^)(::Irrational{:e}, B::SparseMatrixCSC) = exp(B)
(.^)(A::Number, B::SparseMatrixCSC) = (.^)(A, full(B))
(.^)(A::SparseMatrixCSC, B::Array) = (.^)(full(A), B)
(.^)(A::Array, B::SparseMatrixCSC) = (.^)(A, full(B))

.+{Tv1,Ti1,Tv2,Ti2}(A_1::SparseMatrixCSC{Tv1,Ti1}, A_2::SparseMatrixCSC{Tv2,Ti2}) =
   broadcast!(+, spzeros(eltype_plus(A_1, A_2), promote_type(Ti1, Ti2), broadcast_shape(A_1, A_2)...), A_1, A_2)

function .-{Tva,Tia,Tvb,Tib}(A::SparseMatrixCSC{Tva,Tia}, B::SparseMatrixCSC{Tvb,Tib})
    broadcast!(-, spzeros(eltype_plus(A, B), promote_type(Tia, Tib), broadcast_shape(A, B)...), A, B)
end

## element-wise comparison operators returning SparseMatrixCSC ##
.<{Tv1,Ti1,Tv2,Ti2}(A_1::SparseMatrixCSC{Tv1,Ti1}, A_2::SparseMatrixCSC{Tv2,Ti2}) = broadcast!(<, spzeros( Bool, promote_type(Ti1, Ti2), broadcast_shape(A_1, A_2)...), A_1, A_2)
.!={Tv1,Ti1,Tv2,Ti2}(A_1::SparseMatrixCSC{Tv1,Ti1}, A_2::SparseMatrixCSC{Tv2,Ti2}) = broadcast!(!=, spzeros( Bool, promote_type(Ti1, Ti2), broadcast_shape(A_1, A_2)...), A_1, A_2)

## full equality
function ==(A1::SparseMatrixCSC, A2::SparseMatrixCSC)
    size(A1)!=size(A2) && return false
    vals1, vals2 = nonzeros(A1), nonzeros(A2)
    rows1, rows2 = rowvals(A1), rowvals(A2)
    m, n = size(A1)
    @inbounds for i = 1:n
        nz1,nz2 = nzrange(A1,i), nzrange(A2,i)
        j1,j2 = first(nz1), first(nz2)
        # step through the rows of both matrices at once:
        while j1<=last(nz1) && j2<=last(nz2)
            r1,r2 = rows1[j1], rows2[j2]
            if r1==r2
                vals1[j1]!=vals2[j2] && return false
                j1+=1
                j2+=1
            else
                if r1<r2
                    vals1[j1]!=0 && return false
                    j1+=1
                else
                    vals2[j2]!=0 && return false
                    j2+=1
                end
            end
        end
        # finish off any left-overs:
        for j = j1:last(nz1)
            vals1[j]!=0 && return false
        end
        for j = j2:last(nz2)
            vals2[j]!=0 && return false
        end
    end
    return true
end

## Reductions

# In general, output of sparse matrix reductions will not be sparse,
# and computing reductions along columns into SparseMatrixCSC is
# non-trivial, so use Arrays for output
Base.reducedim_initarray{R}(A::SparseMatrixCSC, region, v0, ::Type{R}) =
    fill!(Array(R,Base.reduced_dims(A,region)), v0)
Base.reducedim_initarray0{R}(A::SparseMatrixCSC, region, v0, ::Type{R}) =
    fill!(Array(R,Base.reduced_dims0(A,region)), v0)

# General mapreduce
function _mapreducezeros(f, op, T::Type, nzeros::Int, v0)
    nzeros == 0 && return v0

    # Reduce over first zero
    zeroval = f(zero(T))
    v = op(v0, zeroval)
    isequal(v, v0) && return v

    # Reduce over remaining zeros
    for i = 2:nzeros
        lastv = v
        v = op(v, zeroval)
        # Bail out early if we reach a fixed point
        isequal(v, lastv) && break
    end

    v
end

function Base._mapreduce{T}(f, op, ::Base.LinearSlow, A::SparseMatrixCSC{T})
    z = nnz(A)
    n = length(A)
    if z == 0
        if n == 0
            Base.mr_empty(f, op, T)
        else
            _mapreducezeros(f, op, T, n-z-1, f(zero(T)))
        end
    else
        _mapreducezeros(f, op, T, n-z, Base._mapreduce(f, op, A.nzval))
    end
end

# Specialized mapreduce for AddFun/MulFun
_mapreducezeros(f, ::Base.AddFun, T::Type, nzeros::Int, v0) =
    nzeros == 0 ? v0 : f(zero(T))*nzeros + v0
_mapreducezeros(f, ::Base.MulFun, T::Type, nzeros::Int, v0) =
    nzeros == 0 ? v0 : f(zero(T))^nzeros * v0

function Base._mapreduce{T}(f, op::Base.MulFun, A::SparseMatrixCSC{T})
    nzeros = length(A)-nnz(A)
    if nzeros == 0
        # No zeros, so don't compute f(0) since it might throw
        Base._mapreduce(f, op, A.nzval)
    else
        v = f(zero(T))^(nzeros)
        # Bail out early if initial reduction value is zero
        v == zero(T) ? v : v*Base._mapreduce(f, op, A.nzval)
    end
end

# General mapreducedim
function _mapreducerows!{T}(f, op, R::AbstractArray, A::SparseMatrixCSC{T})
    colptr = A.colptr
    rowval = A.rowval
    nzval = A.nzval
    m, n = size(A)
    @inbounds for col = 1:n
        r = R[1, col]
        @simd for j = colptr[col]:colptr[col+1]-1
            r = op(r, f(nzval[j]))
        end
        R[1, col] = _mapreducezeros(f, op, T, m-(colptr[col+1]-colptr[col]), r)
    end
    R
end

function _mapreducecols!{Tv,Ti}(f, op, R::AbstractArray, A::SparseMatrixCSC{Tv,Ti})
    colptr = A.colptr
    rowval = A.rowval
    nzval = A.nzval
    m, n = size(A)
    rownz = fill(convert(Ti, n), m)
    @inbounds for col = 1:n
        @simd for j = colptr[col]:colptr[col+1]-1
            row = rowval[j]
            R[row, 1] = op(R[row, 1], f(nzval[j]))
            rownz[row] -= 1
        end
    end
    @inbounds for i = 1:m
        R[i, 1] = _mapreducezeros(f, op, Tv, rownz[i], R[i, 1])
    end
    R
end

function Base._mapreducedim!{T}(f, op, R::AbstractArray, A::SparseMatrixCSC{T})
    lsiz = Base.check_reducedims(R,A)
    isempty(A) && return R

    if size(R, 1) == size(R, 2) == 1
        # Reduction along both columns and rows
        R[1, 1] = mapreduce(f, op, A)
    elseif size(R, 1) == 1
        # Reduction along rows
        _mapreducerows!(f, op, R, A)
    elseif size(R, 2) == 1
        # Reduction along columns
        _mapreducecols!(f, op, R, A)
    else
        # Reduction along a dimension > 2
        # Compute op(R, f(A))
        m, n = size(A)
        nzval = A.nzval
        if length(nzval) == m*n
            # No zeros, so don't compute f(0) since it might throw
            for col = 1:n
                @simd for row = 1:size(A, 1)
                    @inbounds R[row, col] = op(R[row, col], f(nzval[(col-1)*m+row]))
                end
            end
        else
            colptr = A.colptr
            rowval = A.rowval
            zeroval = f(zero(T))
            @inbounds for col = 1:n
                lastrow = 0
                for j = colptr[col]:colptr[col+1]-1
                    row = rowval[j]
                    @simd for i = lastrow+1:row-1 # Zeros before this nonzero
                        R[i, col] = op(R[i, col], zeroval)
                    end
                    R[row, col] = op(R[row, col], f(nzval[j]))
                    lastrow = row
                end
                @simd for i = lastrow+1:m         # Zeros at end
                    R[i, col] = op(R[i, col], zeroval)
                end
            end
        end
    end
    R
end

# Specialized mapreducedim for AddFun cols to avoid allocating a
# temporary array when f(0) == 0
function _mapreducecols!{Tv,Ti}(f, op::Base.AddFun, R::AbstractArray, A::SparseMatrixCSC{Tv,Ti})
    nzval = A.nzval
    m, n = size(A)
    if length(nzval) == m*n
        # No zeros, so don't compute f(0) since it might throw
        for col = 1:n
            @simd for row = 1:size(A, 1)
                @inbounds R[row, 1] = op(R[row, 1], f(nzval[(col-1)*m+row]))
            end
        end
    else
        colptr = A.colptr
        rowval = A.rowval
        zeroval = f(zero(Tv))
        if isequal(zeroval, zero(Tv))
            # Case where f(0) == 0
            @inbounds for col = 1:size(A, 2)
                @simd for j = colptr[col]:colptr[col+1]-1
                    R[rowval[j], 1] += f(nzval[j])
                end
            end
        else
            # Case where f(0) != 0
            rownz = fill(convert(Ti, n), m)
            @inbounds for col = 1:size(A, 2)
                @simd for j = colptr[col]:colptr[col+1]-1
                    row = rowval[j]
                    R[row, 1] += f(nzval[j])
                    rownz[row] -= 1
                end
            end
            for i = 1:m
                R[i, 1] += rownz[i]*zeroval
            end
        end
    end
    R
end

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

#all(A::SparseMatrixCSC{Bool}, region) = reducedim(all,A,region,true)
#any(A::SparseMatrixCSC{Bool}, region) = reducedim(any,A,region,false)
#sum(A::SparseMatrixCSC{Bool}, region) = reducedim(+,A,region,0,Int)
#sum(A::SparseMatrixCSC{Bool}) = countnz(A)

## getindex
function rangesearch(haystack::Range, needle)
    (i,rem) = divrem(needle - first(haystack), step(haystack))
    (rem==0 && 1<=i+1<=length(haystack)) ? i+1 : 0
end

getindex(A::SparseMatrixCSC, I::Tuple{Integer,Integer}) = getindex(A, I[1], I[2])

function getindex{T}(A::SparseMatrixCSC{T}, i0::Integer, i1::Integer)
    if !(1 <= i0 <= A.m && 1 <= i1 <= A.n); throw(BoundsError()); end
    r1 = Int(A.colptr[i1])
    r2 = Int(A.colptr[i1+1]-1)
    (r1 > r2) && return zero(T)
    r1 = searchsortedfirst(A.rowval, i0, r1, r2, Forward)
    ((r1 > r2) || (A.rowval[r1] != i0)) ? zero(T) : A.nzval[r1]
end

getindex{T<:Integer}(A::SparseMatrixCSC, i::Integer, J::AbstractVector{T}) = getindex(A,[i],J)

# Colon translation
getindex(A::SparseMatrixCSC, ::Colon, ::Colon) = copy(A)
getindex(A::SparseMatrixCSC, i, ::Colon)       = getindex(A, i, 1:size(A, 2))

function getindex_cols{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, J::AbstractVector)
    # for indexing whole columns
    (m, n) = size(A)
    nJ = length(J)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval

    colptrS = Array(Ti, nJ+1)
    colptrS[1] = 1
    nnzS = 0

    @inbounds for j = 1:nJ
        col = J[j]
        1 <= col <= n || throw(BoundsError())
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
    nI == 0 || (minimum(I) >= 1 && maximum(I) <= m) || throw(BoundsError())
    nJ = length(J)
    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    colptrS = Array(Ti, nJ+1)
    colptrS[1] = 1
    nnzS = 0

    # Form the structure of the result and compute space
    @inbounds for j = 1:nJ
        col = J[j]
        1 <= col <= n || throw(BoundsError())
        @simd for k in colptrA[col]:colptrA[col+1]-1
            nnzS += rowvalA[k] in I # `in` is fast for ranges
        end
        colptrS[j+1] = nnzS+1
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

    nI   = length(I)
    nzA  = nnz(A)
    avgM = div(nzA,n)
    # Heuristics based on experiments discussed in:
    # https://github.com/JuliaLang/julia/issues/12860
    # https://github.com/JuliaLang/julia/pull/12934
    alg = ((m > nzA) && (m > nI)) ? 0 :
          ((nI - avgM) > 2^8) ? 1 :
          ((avgM - nI) > 2^10) ? 0 : 2

    (alg == 0) ? getindex_I_sorted_bsearch_A(A, I, J) :
    (alg == 1) ? getindex_I_sorted_bsearch_I(A, I, J) :
    getindex_I_sorted_linear(A, I, J)
end

function getindex_I_sorted_bsearch_A{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector)
    const nI = length(I)
    const nJ = length(J)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    colptrS = Array(Ti, nJ+1)
    colptrS[1] = 1

    ptrS = 1
    # determine result size
    @inbounds for j = 1:nJ
        col = J[j]
        ptrI::Int = 1 # runs through I
        ptrA::Int = colptrA[col]
        stopA::Int = colptrA[col+1]-1
        if ptrA <= stopA
            while ptrI <= nI
                rowI = I[ptrI]
                ptrI += 1
                (rowvalA[ptrA] > rowI) && continue
                ptrA = searchsortedfirst(rowvalA, rowI, ptrA, stopA, Base.Order.Forward)
                (ptrA <= stopA) || break
                if rowvalA[ptrA] == rowI
                    ptrS += 1
                end
            end
        end
        colptrS[j+1] = ptrS
    end

    rowvalS = Array(Ti, ptrS-1)
    nzvalS  = Array(Tv, ptrS-1)

    # fill the values
    ptrS = 1
    @inbounds for j = 1:nJ
        col = J[j]
        ptrI::Int = 1 # runs through I
        ptrA::Int = colptrA[col]
        stopA::Int = colptrA[col+1]-1
        if ptrA <= stopA
            while ptrI <= nI
                rowI = I[ptrI]
                if rowvalA[ptrA] <= rowI
                    ptrA = searchsortedfirst(rowvalA, rowI, ptrA, stopA, Base.Order.Forward)
                    (ptrA <= stopA) || break
                    if rowvalA[ptrA] == rowI
                        rowvalS[ptrS] = ptrI
                        nzvalS[ptrS] = nzvalA[ptrA]
                        ptrS += 1
                    end
                end
                ptrI += 1
            end
        end
    end
    return SparseMatrixCSC(nI, nJ, colptrS, rowvalS, nzvalS)
end

function getindex_I_sorted_linear{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector)
    const nI = length(I)
    const nJ = length(J)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    colptrS = Array(Ti, nJ+1)
    colptrS[1] = 1
    cacheI = zeros(Int, A.m)

    ptrS   = 1
    # build the cache and determine result size
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
                (cacheI[rowA] == 0) && (cacheI[rowA] = ptrI)
                ptrS += 1
                ptrI += 1
            end
        end
        colptrS[j+1] = ptrS
    end

    rowvalS = Array(Ti, ptrS-1)
    nzvalS  = Array(Tv, ptrS-1)

    # fill the values
    ptrS = 1
    @inbounds for j = 1:nJ
        col = J[j]
        ptrA::Int = colptrA[col]
        stopA::Int = colptrA[col+1]
        while ptrA < stopA
            rowA = rowvalA[ptrA]
            ptrI = cacheI[rowA]
            if ptrI > 0
                while ptrI <= nI && I[ptrI] == rowA
                    rowvalS[ptrS] = ptrI
                    nzvalS[ptrS] = nzvalA[ptrA]
                    ptrS += 1
                    ptrI += 1
                end
            end
            ptrA += 1
        end
    end
    return SparseMatrixCSC(nI, nJ, colptrS, rowvalS, nzvalS)
end

function getindex_I_sorted_bsearch_I{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector)
    const nI = length(I)
    const nJ = length(J)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    colptrS = Array(Ti, nJ+1)
    colptrS[1] = 1

    m = A.m

    # cacheI is used first to store num occurrences of each row in columns of interest
    # and later to store position of first occurrence of each row in I
    cacheI = zeros(Int, m)

    # count rows
    @inbounds for j = 1:nJ
        col = J[j]
        for ptrA in colptrA[col]:(colptrA[col+1]-1)
            cacheI[rowvalA[ptrA]] += 1
        end
    end

    # fill cache and count nnz
    ptrS::Int = 0
    ptrI::Int = 1
    @inbounds for j = 1:m
        cval = cacheI[j]
        (cval == 0) && continue
        ptrI = searchsortedfirst(I, j, ptrI, nI, Base.Order.Forward)
        cacheI[j] = ptrI
        while ptrI <= nI && I[ptrI] == j
            ptrS += cval
            ptrI += 1
        end
        if ptrI > nI
            @simd for i=(j+1):m; @inbounds cacheI[i]=ptrI; end
            break
        end
    end
    rowvalS = Array(Ti, ptrS)
    nzvalS  = Array(Tv, ptrS)
    colptrS[nJ+1] = ptrS+1

    # fill the values
    ptrS = 1
    @inbounds for j = 1:nJ
        col = J[j]
        ptrA::Int = colptrA[col]
        stopA::Int = colptrA[col+1]
        while ptrA < stopA
            rowA = rowvalA[ptrA]
            ptrI = cacheI[rowA]
            (ptrI > nI) && break
            if ptrI > 0
                while I[ptrI] == rowA
                    rowvalS[ptrS] = ptrI
                    nzvalS[ptrS] = nzvalA[ptrA]
                    ptrS += 1
                    ptrI += 1
                    (ptrI > nI) && break
                end
            end
            ptrA += 1
        end
        colptrS[j+1] = ptrS
    end
    return SparseMatrixCSC(nI, nJ, colptrS, rowvalS, nzvalS)
end

function permute_rows!{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti}, pI::Vector{Int})
    (m, n) = size(S)
    colptrS = S.colptr; rowvalS = S.rowval; nzvalS = S.nzval
    # preallocate temporary sort space
    nr = min(nnz(S), m)
    rowperm = Array(Int, nr)
    rowvalTemp = Array(Ti, nr)
    nzvalTemp = Array(Tv, nr)

    @inbounds for j in 1:n
        rowrange = colptrS[j]:(colptrS[j+1]-1)
        nr = length(rowrange)
        (nr > 0) || continue
        k = 1
        for i in rowrange
            rowA = rowvalS[i]
            rowvalTemp[k] = pI[rowA]
            nzvalTemp[k] = nzvalS[i]
            k += 1
        end
        sortperm!(pointer_to_array(pointer(rowperm), nr), pointer_to_array(pointer(rowvalTemp), nr))
        k = 1
        for i in rowrange
            kperm = rowperm[k]
            rowvalS[i] = rowvalTemp[kperm]
            nzvalS[i] = nzvalTemp[kperm]
            k += 1
        end
    end
    S
end

function getindex_general{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector)
    pI = sortperm(I)
    @inbounds Is = I[pI]
    permute_rows!(getindex_I_sorted(A, Is, J), pI)
end

# the general case:
function getindex{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector)
    (m, n) = size(A)

    if !isempty(J)
        minj, maxj = extrema(J)
        ((minj < 1) || (maxj > n)) && throw(BoundsError())
    end

    if !isempty(I)
        mini, maxi = extrema(I)
        ((mini < 1) || (maxi > m)) && throw(BoundsError())
    end

    if isempty(I) || isempty(J) || (0 == nnz(A))
        return spzeros(Tv, Ti, length(I), length(J))
    end

    if issorted(I)
        return getindex_I_sorted(A, I, J)
    else
        return getindex_general(A, I, J)
    end
end

function getindex{Tv}(A::SparseMatrixCSC{Tv}, I::AbstractArray)
    szA = size(A)
    nA = szA[1]*szA[2]
    colptrA = A.colptr
    rowvalA = A.rowval
    nzvalA = A.nzval

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
        ((I[i] < 1) | (I[i] > nA)) && throw(BoundsError())
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

# logical getindex
getindex{Tv,Ti<:Integer}(A::SparseMatrixCSC{Tv,Ti}, I::Range{Bool}, J::AbstractVector{Bool}) = error("Cannot index with Range{Bool}")
getindex{Tv,Ti<:Integer,T<:Integer}(A::SparseMatrixCSC{Tv,Ti}, I::Range{Bool}, J::AbstractVector{T}) = error("Cannot index with Range{Bool}")

getindex{T<:Integer}(A::SparseMatrixCSC, I::Range{T}, J::AbstractVector{Bool}) = A[I,find(J)]
getindex(A::SparseMatrixCSC, I::Integer, J::AbstractVector{Bool}) = A[I,find(J)]
getindex(A::SparseMatrixCSC, I::AbstractVector{Bool}, J::Integer) = A[find(I),J]
getindex(A::SparseMatrixCSC, I::AbstractVector{Bool}, J::AbstractVector{Bool}) = A[find(I),find(J)]
getindex{T<:Integer}(A::SparseMatrixCSC, I::AbstractVector{T}, J::AbstractVector{Bool}) = A[I,find(J)]
getindex{T<:Integer}(A::SparseMatrixCSC, I::AbstractVector{Bool}, J::AbstractVector{T}) = A[find(I),J]

## setindex!
function setindex!{T,Ti}(A::SparseMatrixCSC{T,Ti}, v, i0::Integer, i1::Integer)
    i0 = convert(Ti, i0)
    i1 = convert(Ti, i1)
    if !(1 <= i0 <= A.m && 1 <= i1 <= A.n); throw(BoundsError()); end
    v = convert(T, v)
    r1 = Int(A.colptr[i1])
    r2 = Int(A.colptr[i1+1]-1)
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

# Colon translation
setindex!(A::SparseMatrixCSC, x, ::Colon)          = setindex!(A, x, 1:length(A))
setindex!(A::SparseMatrixCSC, x, ::Colon, ::Colon) = setindex!(A, x, 1:size(A, 1), 1:size(A,2))
setindex!(A::SparseMatrixCSC, x, ::Colon, j::Union{Integer, AbstractVector}) = setindex!(A, x, 1:size(A, 1), j)
setindex!(A::SparseMatrixCSC, x, i::Union{Integer, AbstractVector}, ::Colon) = setindex!(A, x, i, 1:size(A, 2))

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

setindex!{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, x, I::AbstractVector{Bool}) = throw(BoundsError())
function setindex!{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, x, I::AbstractMatrix{Bool})
    checkbounds(A, I)
    n = sum(I)
    (n == 0) && (return A)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    colptrB = colptrA; rowvalB = rowvalA; nzvalB = nzvalA
    nadd = ndel = 0
    bidx = xidx = 1
    r1 = r2 = 0

    @inbounds for col in 1:A.n
        r1 = Int(colptrA[col])
        r2 = Int(colptrA[col+1]-1)

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
            r1 = Int(colptrA[col])
            r2 = Int(colptrA[col+1] - 1)

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
    m = sum(mX)
    n = nX[1]

    for i = 2 : num
        if nX[i] != n
            throw(DimensionMismatch("All inputs to vcat should have the same number of columns"))
        end
    end

    Tv = eltype(X[1].nzval)
    Ti = eltype(X[1].rowval)
    for i = 2:length(X)
        Tv = promote_type(Tv, eltype(X[i].nzval))
        Ti = promote_type(Ti, eltype(X[i].rowval))
    end

    nnzX = [ nnz(x) for x in X ]
    nnz_res = sum(nnzX)
    colptr = Array(Ti, n + 1)
    rowval = Array(Ti, nnz_res)
    nzval  = Array(Tv, nnz_res)

    colptr[1] = 1
    for c = 1:n
        mX_sofar = 0
        ptr_res = colptr[c]
        for i = 1 : num
            colptrXi = X[i].colptr
            col_length = (colptrXi[c + 1] - 1) - colptrXi[c]
            ptr_Xi = colptrXi[c]

            stuffcol!(X[i], colptr, rowval, nzval,
                      ptr_res, ptr_Xi, col_length, mX_sofar)

            ptr_res += col_length + 1
            mX_sofar += mX[i]
        end
        colptr[c + 1] = ptr_res
    end
    SparseMatrixCSC(m, n, colptr, rowval, nzval)
end

@inline function stuffcol!(Xi::SparseMatrixCSC, colptr, rowval, nzval,
                           ptr_res, ptr_Xi, col_length, mX_sofar)
    colptrXi = Xi.colptr
    rowvalXi = Xi.rowval
    nzvalXi  = Xi.nzval

    for k=ptr_res:(ptr_res + col_length)
        @inbounds rowval[k] = rowvalXi[ptr_Xi] + mX_sofar
        @inbounds nzval[k]  = nzvalXi[ptr_Xi]
        ptr_Xi += 1
    end
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

function hvcat(rows::Tuple{Vararg{Int}}, X::SparseMatrixCSC...)
    nbr = length(rows)  # number of block rows

    tmp_rows = Array(SparseMatrixCSC, nbr)
    k = 0
    @inbounds for i = 1 : nbr
        tmp_rows[i] = hcat(X[(1 : rows[i]) + k]...)
        k += rows[i]
    end
    vcat(tmp_rows...)
end

"""
    blkdiag(A...)

Concatenate matrices block-diagonally. Currently only implemented for sparse matrices.
"""
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

squeeze(S::SparseMatrixCSC, dims::Dims) = throw(ArgumentError("squeeze is not available for sparse matrices"))

## Structure query functions
issym(A::SparseMatrixCSC) = is_hermsym(A, IdFun())

ishermitian(A::SparseMatrixCSC) = is_hermsym(A, ConjFun())

function is_hermsym(A::SparseMatrixCSC, check::Func)
    m, n = size(A)
    if m != n; return false; end

    colptr = A.colptr
    rowval = A.rowval
    nzval = A.nzval
    tracker = copy(A.colptr)
    @inbounds for col = 1:A.n
        for p = tracker[col]:colptr[col+1]-1
            val = nzval[p]
            if val == 0; continue; end # In case of explicit zeros
            row = rowval[p]
            if row < col; return false; end
            if row == col # Diagonal element
                if val != check(val)
                    return false
                end
            else
                row2 = rowval[tracker[row]]
                if row2 > col; return false; end
                if row2 == col # A[i,j] and A[j,i] exists
                    if val != check(nzval[tracker[row]])
                        return false
                    end
                    tracker[row] += 1
                end
            end
        end
    end
    return true
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
    V = Array(promote_type(map(eltype, B)...), ncoeffs)
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

"""
    spdiagm(B, d[, m, n])

Construct a sparse diagonal matrix. `B` is a tuple of vectors containing the diagonals and
`d` is a tuple containing the positions of the diagonals. In the case the input contains only
one diagonal, `B` can be a vector (instead of a tuple) and `d` can be the diagonal position
(instead of a tuple), defaulting to 0 (diagonal). Optionally, `m` and `n` specify the size
of the resulting sparse matrix.
"""
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
    res = similar(V, (Int64(V[end]-1),))
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
    r1 = Int(A.colptr[j])
    r2 = Int(A.colptr[j+1]-1)
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

    return SparseMatrixCSC(n, n, colptr, rowval, nzval)
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

## rotations

function rot180(A::SparseMatrixCSC)
    I,J,V = findnz(A)
    m,n = size(A)
    for i=1:length(I)
        I[i] = m - I[i] + 1
        J[i] = n - J[i] + 1
    end
    return sparse(I,J,V,m,n)
end

function rotr90(A::SparseMatrixCSC)
    I,J,V = findnz(A)
    m,n = size(A)
    #old col inds are new row inds
    for i=1:length(I)
        I[i] = m - I[i] + 1
    end
    return sparse(J, I, V, n, m)
end

function rotl90(A::SparseMatrixCSC)
    I,J,V = findnz(A)
    m,n = size(A)
    #old row inds are new col inds
    for i=1:length(J)
        J[i] = n - J[i] + 1
    end
    return sparse(J, I, V, n, m)
end

## hashing

# End the run and return the current hash
@inline function hashrun(val, runlength::Int, h::UInt)
    if runlength == 0
        return h
    elseif runlength > 1
        h += Base.hashrle_seed
        h = hash(runlength, h)
    end
    hash(val, h)
end

function hash{T}(A::SparseMatrixCSC{T}, h::UInt)
    h += Base.hashaa_seed
    sz = size(A)
    h += hash(sz)

    colptr = A.colptr
    rowval = A.rowval
    nzval = A.nzval
    lastidx = 0
    runlength = 0
    lastnz = zero(T)
    @inbounds for col = 1:size(A, 2)
        for j = colptr[col]:colptr[col+1]-1
            nz = nzval[j]
            isequal(nz, zero(T)) && continue
            idx = sub2ind(sz, rowval[j], col)
            if idx != lastidx+1 || !isequal(nz, lastnz)  # Run is over
                h = hashrun(lastnz, runlength, h)        # Hash previous run
                h = hashrun(0, idx-lastidx-1, h)         # Hash intervening zeros

                runlength = 1
                lastnz = nz
            else
                runlength += 1
            end
            lastidx = idx
        end
    end
    h = hashrun(lastnz, runlength, h) # Hash previous run
    hashrun(0, length(A)-lastidx, h)  # Hash zeros at end
end

## Statistics

# This is the function that does the reduction underlying var/std
function Base.centralize_sumabs2!{S,Tv,Ti}(R::AbstractArray{S}, A::SparseMatrixCSC{Tv,Ti}, means::AbstractArray)
    lsiz = Base.check_reducedims(R,A)
    size(means) == size(R) || error("size of means must match size of R")
    isempty(R) || fill!(R, zero(S))
    isempty(A) && return R

    colptr = A.colptr
    rowval = A.rowval
    nzval = A.nzval
    m = size(A, 1)
    n = size(A, 2)

    if size(R, 1) == size(R, 2) == 1
        # Reduction along both columns and rows
        R[1, 1] = Base.centralize_sumabs2(A, means[1])
    elseif size(R, 1) == 1
        # Reduction along rows
        @inbounds for col = 1:n
            mu = means[col]
            r = convert(S, (m-colptr[col+1]+colptr[col])*abs2(mu))
            @simd for j = colptr[col]:colptr[col+1]-1
                r += abs2(nzval[j] - mu)
            end
            R[1, col] = r
        end
    elseif size(R, 2) == 1
        # Reduction along columns
        rownz = fill(convert(Ti, n), m)
        @inbounds for col = 1:n
            @simd for j = colptr[col]:colptr[col+1]-1
                row = rowval[j]
                R[row, 1] += abs2(nzval[j] - means[row])
                rownz[row] -= 1
            end
        end
        for i = 1:m
            R[i, 1] += rownz[i]*abs2(means[i])
        end
    else
        # Reduction along a dimension > 2
        @inbounds for col = 1:n
            lastrow = 0
            @simd for j = colptr[col]:colptr[col+1]-1
                row = rowval[j]
                for i = lastrow+1:row-1
                    R[i, col] = abs2(means[i, col])
                end
                R[row, col] = abs2(nzval[j] - means[row, col])
                lastrow = row
            end
            for i = lastrow+1:m
                R[i, col] = abs2(means[i, col])
            end
        end
    end
    return R
end

## Uniform matrix arithmetic

(+)(A::SparseMatrixCSC, J::UniformScaling) = A + J.λ * speye(A)
(-)(A::SparseMatrixCSC, J::UniformScaling) = A - J.λ * speye(A)
(-)(J::UniformScaling, A::SparseMatrixCSC) = J.λ * speye(A) - A
