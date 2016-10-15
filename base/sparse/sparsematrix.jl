# This file is a part of Julia. License is MIT: http://julialang.org/license

# Compressed sparse columns data structure
# Assumes that no zeros are stored in the data structure
# Assumes that row values in rowval for each column are sorted
#      issorted(rowval[colptr[i]:(colptr[i+1]-1)]) == true

immutable SparseMatrixCSC{Tv,Ti<:Integer} <: AbstractSparseMatrix{Tv,Ti}
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

```jldoctest
julia> A = speye(3)
3×3 sparse matrix with 3 Float64 nonzero entries:
        [1, 1]  =  1.0
        [2, 2]  =  1.0
        [3, 3]  =  1.0

julia> nnz(A)
3
```
"""
nnz(S::SparseMatrixCSC) = Int(S.colptr[end]-1)
countnz(S::SparseMatrixCSC) = countnz(S.nzval)

"""
    nonzeros(A)

Return a vector of the structural nonzero values in sparse array `A`. This
includes zeros that are explicitly stored in the sparse array. The returned
vector points directly to the internal nonzero storage of `A`, and any
modifications to the returned vector will mutate `A` as well. See
[`rowvals`](:func:`rowvals`) and [`nzrange`](:func:`nzrange`).

```jldoctest
julia> A = speye(3)
3×3 sparse matrix with 3 Float64 nonzero entries:
        [1, 1]  =  1.0
        [2, 2]  =  1.0
        [3, 3]  =  1.0

julia> nonzeros(A)
3-element Array{Float64,1}:
 1.0
 1.0
 1.0
```
"""
nonzeros(S::SparseMatrixCSC) = S.nzval

"""
    rowvals(A::SparseMatrixCSC)

Return a vector of the row indices of `A`. Any modifications to the returned
vector will mutate `A` as well. Providing access to how the row indices are
stored internally can be useful in conjunction with iterating over structural
nonzero values. See also [`nonzeros`](:func:`nonzeros`) and [`nzrange`](:func:`nzrange`).

```jldoctest
julia> A = speye(3)
3×3 sparse matrix with 3 Float64 nonzero entries:
        [1, 1]  =  1.0
        [2, 2]  =  1.0
        [3, 3]  =  1.0

julia> rowvals(A)
3-element Array{Int64,1}:
 1
 2
 3
```
"""
rowvals(S::SparseMatrixCSC) = S.rowval

"""
    nzrange(A::SparseMatrixCSC, col::Integer)

Return the range of indices to the structural nonzero values of a sparse matrix
column. In conjunction with [`nonzeros`](:func:`nonzeros`) and
[`rowvals`](:func:`rowvals`), this allows for convenient iterating over a sparse matrix :

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

function Base.show(io::IO, ::MIME"text/plain", S::SparseMatrixCSC)
    print(io, S.m, "×", S.n, " sparse matrix with ", nnz(S), " ", eltype(S), " nonzero entries")
    if nnz(S) != 0
        print(io, ":")
        show(io, S)
    end
end

function Base.show(io::IO, S::SparseMatrixCSC)
    if nnz(S) == 0
        return show(io, MIME("text/plain"), S)
    end

    limit::Bool = get(io, :limit, false)
    if limit
        rows = displaysize(io)[1]
        half_screen_rows = div(rows - 8, 2)
    else
        half_screen_rows = typemax(Int)
    end
    pad = ndigits(max(S.m,S.n))
    k = 0
    sep = "\n\t"
    io = IOContext(io)
    if !haskey(io, :compact)
        io = IOContext(io, :compact => true)
    end
    for col = 1:S.n, k = S.colptr[col] : (S.colptr[col+1]-1)
        if k < half_screen_rows || k > nnz(S)-half_screen_rows
            print(io, sep, '[', rpad(S.rowval[k], pad), ", ", lpad(col, pad), "]  =  ")
            if isassigned(S.nzval, k)
                Base.show(io, S.nzval[k])
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
    colptr = Array{Ti}(nS+1)
    rowval = similar(a.rowval)
    nzval = reinterpret(T, a.nzval)

    sparse_compute_reshaped_colptr_and_rowval(colptr, rowval, mS, nS, a.colptr, a.rowval, mA, nA)

    return SparseMatrixCSC(mS, nS, colptr, rowval, nzval)
end

function copy{T,P<:SparseMatrixCSC}(ra::ReshapedArray{T,2,P})
    mS,nS = size(ra)
    a = parent(ra)
    mA,nA = size(a)
    numnz = nnz(a)
    colptr = similar(a.colptr, nS+1)
    rowval = similar(a.rowval)
    nzval = copy(a.nzval)

    sparse_compute_reshaped_colptr_and_rowval(colptr, rowval, mS, nS, a.colptr, a.rowval, mA, nA)

    return SparseMatrixCSC(mS, nS, colptr, rowval, nzval)
end

## Constructors

copy(S::SparseMatrixCSC) =
    SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), copy(S.nzval))

function copy!(A::SparseMatrixCSC, B::SparseMatrixCSC)
    # If the two matrices have the same length then all the
    # elements in A will be overwritten.
    if length(A) == length(B)
        resize!(A.nzval, length(B.nzval))
        resize!(A.rowval, length(B.rowval))
        if size(A) == size(B)
            # Simple case: we can simply copy the internal fields of B to A.
            copy!(A.colptr, B.colptr)
            copy!(A.rowval, B.rowval)
        else
            # This is like a "reshape B into A".
            sparse_compute_reshaped_colptr_and_rowval(A.colptr, A.rowval, A.m, A.n, B.colptr, B.rowval, B.m, B.n)
        end
    else
        length(A) >= length(B) || throw(BoundsError())
        lB = length(B)
        nnzA = nnz(A)
        nnzB = nnz(B)
        # Up to which col, row, and ptr in rowval/nzval will A be overwritten?
        lastmodcolA = div(lB - 1, A.m) + 1
        lastmodrowA = mod(lB - 1, A.m) + 1
        lastmodptrA = A.colptr[lastmodcolA]
        while lastmodptrA < A.colptr[lastmodcolA+1] && A.rowval[lastmodptrA] <= lastmodrowA
            lastmodptrA += 1
        end
        lastmodptrA -= 1
        if lastmodptrA >= nnzB
            # A will have fewer non-zero elements; unmodified elements are kept at the end.
            deleteat!(A.rowval, nnzB+1:lastmodptrA)
            deleteat!(A.nzval, nnzB+1:lastmodptrA)
        else
            # A will have more non-zero elements; unmodified elements are kept at the end.
            resize!(A.rowval, nnzB + nnzA - lastmodptrA)
            resize!(A.nzval, nnzB + nnzA - lastmodptrA)
            copy!(A.rowval, nnzB+1, A.rowval, lastmodptrA+1, nnzA-lastmodptrA)
            copy!(A.nzval, nnzB+1, A.nzval, lastmodptrA+1, nnzA-lastmodptrA)
        end
        # Adjust colptr accordingly.
        @inbounds for i in 2:length(A.colptr)
            A.colptr[i] += nnzB - lastmodptrA
        end
        sparse_compute_reshaped_colptr_and_rowval(A.colptr, A.rowval, A.m, lastmodcolA-1, B.colptr, B.rowval, B.m, B.n)
    end
    copy!(A.nzval, B.nzval)
    return A
end

similar(S::SparseMatrixCSC, Tv::Type=eltype(S)) = SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), Array{Tv}(length(S.nzval)))
similar{Tv,Ti,TvNew,TiNew}(S::SparseMatrixCSC{Tv,Ti}, ::Type{TvNew}, ::Type{TiNew}) = SparseMatrixCSC(S.m, S.n, convert(Array{TiNew},S.colptr), convert(Array{TiNew}, S.rowval), Array{TvNew}(length(S.nzval)))
@inline similar{Tv}(S::SparseMatrixCSC, ::Type{Tv}, d::Dims) = spzeros(Tv, d...)

# convert'ing between SparseMatrixCSC types
convert{Tv}(::Type{AbstractMatrix{Tv}}, A::SparseMatrixCSC) = convert(SparseMatrixCSC{Tv}, A)
convert{Tv}(::Type{SparseMatrixCSC{Tv}}, S::SparseMatrixCSC) = convert(SparseMatrixCSC{Tv,eltype(S.colptr)}, S)
convert{Tv,Ti}(::Type{SparseMatrixCSC{Tv,Ti}}, S::SparseMatrixCSC{Tv,Ti}) = S
function convert{Tv,Ti}(::Type{SparseMatrixCSC{Tv,Ti}}, S::SparseMatrixCSC)
    eltypeTicolptr = convert(Vector{Ti}, S.colptr)
    eltypeTirowval = convert(Vector{Ti}, S.rowval)
    eltypeTvnzval = convert(Vector{Tv}, S.nzval)
    return SparseMatrixCSC(S.m, S.n, eltypeTicolptr, eltypeTirowval, eltypeTvnzval)
end
# convert'ing from other matrix types to SparseMatrixCSC (also see sparse())
convert(::Type{SparseMatrixCSC}, M::Matrix) = sparse(M)
convert{Tv}(::Type{SparseMatrixCSC}, M::AbstractMatrix{Tv}) = convert(SparseMatrixCSC{Tv,Int}, M)
convert{Tv}(::Type{SparseMatrixCSC{Tv}}, M::AbstractMatrix{Tv}) = convert(SparseMatrixCSC{Tv,Int}, M)
function convert{Tv,Ti}(::Type{SparseMatrixCSC{Tv,Ti}}, M::AbstractMatrix)
    (I, J, V) = findnz(M)
    eltypeTiI = convert(Vector{Ti}, I)
    eltypeTiJ = convert(Vector{Ti}, J)
    eltypeTvV = convert(Vector{Tv}, V)
    return sparse_IJ_sorted!(eltypeTiI, eltypeTiJ, eltypeTvV, size(M)...)
end
# convert'ing from SparseMatrixCSC to other matrix types
function convert{Tv}(::Type{Matrix}, S::SparseMatrixCSC{Tv})
    # Handle cases where zero(Tv) is not defined but the array is dense.
    A = length(S) == nnz(S) ? Array{Tv}(S.m, S.n) : zeros(Tv, S.m, S.n)
    for Sj in 1:S.n
        for Sk in nzrange(S, Sj)
            Si = S.rowval[Sk]
            Sv = S.nzval[Sk]
            A[Si, Sj] = Sv
        end
    end
    return A
end
convert(::Type{Array}, S::SparseMatrixCSC) = convert(Matrix, S)
full(S::SparseMatrixCSC) = convert(Array, S)

"""
    full(S)

Convert a sparse matrix or vector `S` into a dense matrix or vector.

```jldoctest
julia> A = speye(3)
3×3 sparse matrix with 3 Float64 nonzero entries:
        [1, 1]  =  1.0
        [2, 2]  =  1.0
        [3, 3]  =  1.0

julia> full(A)
3×3 Array{Float64,2}:
 1.0  0.0  0.0
 0.0  1.0  0.0
 0.0  0.0  1.0
```
"""
full

float(S::SparseMatrixCSC) = SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), float.(S.nzval))

complex(S::SparseMatrixCSC) = SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), complex(copy(S.nzval)))

complex(A::SparseMatrixCSC, B::SparseMatrixCSC) = A + im*B

# Construct a sparse vector

# Note that unlike `vec` for arrays, this does not share data
vec(S::SparseMatrixCSC) = S[:]

"""
    sparse(A)

Convert an AbstractMatrix `A` into a sparse matrix.

```jldoctest
julia> A = eye(3)
3×3 Array{Float64,2}:
 1.0  0.0  0.0
 0.0  1.0  0.0
 0.0  0.0  1.0

julia> sparse(A)
3×3 sparse matrix with 3 Float64 nonzero entries:
        [1, 1]  =  1.0
        [2, 2]  =  1.0
        [3, 3]  =  1.0
```
"""
sparse{Tv}(A::AbstractMatrix{Tv}) = convert(SparseMatrixCSC{Tv,Int}, A)

sparse(S::SparseMatrixCSC) = copy(S)

sparse_IJ_sorted!(I,J,V,m,n) = sparse_IJ_sorted!(I,J,V,m,n,+)

sparse_IJ_sorted!(I,J,V::AbstractVector{Bool},m,n) = sparse_IJ_sorted!(I,J,V,m,n,|)

function sparse_IJ_sorted!{Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti},
                                        V::AbstractVector,
                                        m::Integer, n::Integer, combine::Function)

    m = m < 0 ? 0 : m
    n = n < 0 ? 0 : n
    if isempty(V); return spzeros(eltype(V),Ti,m,n); end

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

    colptr = cumsum!(similar(cols), cols)

    # Allow up to 20% slack
    if ndups > 0.2*L
        numnz = L-ndups
        deleteat!(I, (numnz+1):L)
        deleteat!(V, (numnz+1):length(V))
    end

    return SparseMatrixCSC(m, n, colptr, I, V)
end

"""
    sparse(I, J, V,[ m, n, combine])

Create a sparse matrix `S` of dimensions `m x n` such that `S[I[k], J[k]] = V[k]`. The
`combine` function is used to combine duplicates. If `m` and `n` are not specified, they
are set to `maximum(I)` and `maximum(J)` respectively. If the `combine` function is not
supplied, `combine` defaults to `+` unless the elements of `V` are Booleans in which case
`combine` defaults to `|`. All elements of `I` must satisfy `1 <= I[k] <= m`, and all
elements of `J` must satisfy `1 <= J[k] <= n`. Numerical zeros in (`I`, `J`, `V`) are
retained as structural nonzeros; to drop numerical zeros, use [`dropzeros!`](:func:`dropzeros!`).

For additional documentation and an expert driver, see `Base.SparseArrays.sparse!`.

```jldoctest
julia> Is = [1; 2; 3];

julia> Js = [1; 2; 3];

julia> Vs = [1; 2; 3];

julia> sparse(Is, Js, Vs)
3×3 sparse matrix with 3 Int64 nonzero entries:
        [1, 1]  =  1
        [2, 2]  =  2
        [3, 3]  =  3
```
"""
function sparse{Tv,Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti}, V::AbstractVector{Tv}, m::Integer, n::Integer, combine)
    coolen = length(I)
    if length(J) != coolen || length(V) != coolen
        throw(ArgumentError(string("the first three arguments' lengths must match, ",
              "length(I) (=$(length(I))) == length(J) (= $(length(J))) == length(V) (= ",
              "$(length(V)))")))
    end

    if m == 0 || n == 0 || coolen == 0
        if coolen != 0
            if n == 0
                throw(ArgumentError("column indices J[k] must satisfy 1 <= J[k] <= n"))
            elseif m == 0
                throw(ArgumentError("row indices I[k] must satisfy 1 <= I[k] <= m"))
            end
        end
        SparseMatrixCSC(m, n, ones(Ti, n+1), Vector{Ti}(), Vector{Tv}())
    else
        # Allocate storage for CSR form
        csrrowptr = Vector{Ti}(m+1)
        csrcolval = Vector{Ti}(coolen)
        csrnzval = Vector{Tv}(coolen)

        # Allocate storage for the CSC form's column pointers and a necessary workspace
        csccolptr = Vector{Ti}(n+1)
        klasttouch = Vector{Ti}(n)

        # Allocate empty arrays for the CSC form's row and nonzero value arrays
        # The parent method called below automagically resizes these arrays
        cscrowval = Vector{Ti}()
        cscnzval = Vector{Tv}()

        sparse!(I, J, V, m, n, combine, klasttouch,
                csrrowptr, csrcolval, csrnzval,
                csccolptr, cscrowval, cscnzval )
    end
end

sparse(I::AbstractVector, J::AbstractVector, V::AbstractVector, m::Integer, n::Integer, combine) =
    sparse(AbstractVector{Int}(I), AbstractVector{Int}(J), V, m, n, combine)

"""
    sparse!{Tv,Ti<:Integer}(
        I::AbstractVector{Ti}, J::AbstractVector{Ti}, V::AbstractVector{Tv},
        m::Integer, n::Integer, combine, klasttouch::Vector{Ti},
        csrrowptr::Vector{Ti}, csrcolval::Vector{Ti}, csrnzval::Vector{Tv},
        [csccolptr::Vector{Ti}], [cscrowval::Vector{Ti}, cscnzval::Vector{Tv}] )

Parent of and expert driver for [`sparse`](:func:`sparse`);
see [`sparse`](:func:`sparse`) for basic usage. This method
allows the user to provide preallocated storage for `sparse`'s intermediate objects and
result as described below. This capability enables more efficient successive construction
of `SparseMatrixCSC`s from coordinate representations, and also enables extraction of an
unsorted-column representation of the result's transpose at no additional cost.

This method consists of three major steps: (1) Counting-sort the provided coordinate
representation into an unsorted-row CSR form including repeated entries. (2) Sweep through
the CSR form, simultaneously calculating the desired CSC form's column-pointer array,
detecting repeated entries, and repacking the CSR form with repeated entries combined;
this stage yields an unsorted-row CSR form with no repeated entries. (3) Counting-sort the
preceding CSR form into a fully-sorted CSC form with no repeated entries.

Input arrays `csrrowptr`, `csrcolval`, and `csrnzval` constitute storage for the
intermediate CSR forms and require `length(csrrowptr) >= m + 1`,
`length(csrcolval) >= length(I)`, and `length(csrnzval >= length(I))`. Input
array `klasttouch`, workspace for the second stage, requires `length(klasttouch) >= n`.
Optional input arrays `csccolptr`, `cscrowval`, and `cscnzval` constitute storage for the
returned CSC form `S`. `csccolptr` requires `length(csccolptr) >= n + 1`. If necessary,
`cscrowval` and `cscnzval` are automatically resized to satisfy
`length(cscrowval) >= nnz(S)` and `length(cscnzval) >= nnz(S)`; hence, if `nnz(S)` is
unknown at the outset, passing in empty vectors of the appropriate type (`Vector{Ti}()`
and `Vector{Tv}()` respectively) suffices, or calling the `sparse!` method
neglecting `cscrowval` and `cscnzval`.

On return, `csrrowptr`, `csrcolval`, and `csrnzval` contain an unsorted-column
representation of the result's transpose.

You may reuse the input arrays' storage (`I`, `J`, `V`) for the output arrays
(`csccolptr`, `cscrowval`, `cscnzval`). For example, you may call
`sparse!(I, J, V, csrrowptr, csrcolval, csrnzval, I, J, V)`.

For the sake of efficiency, this method performs no argument checking beyond
`1 <= I[k] <= m` and `1 <= J[k] <= n`. Use with care. Testing with `--check-bounds=yes`
is wise.

This method runs in `O(m, n, length(I))` time. The HALFPERM algorithm described in
F. Gustavson, "Two fast algorithms for sparse matrices: multiplication and permuted
transposition," ACM TOMS 4(3), 250-269 (1978) inspired this method's use of a pair of
counting sorts.
"""
function sparse!{Tv,Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti},
        V::AbstractVector{Tv}, m::Integer, n::Integer, combine, klasttouch::Vector{Ti},
        csrrowptr::Vector{Ti}, csrcolval::Vector{Ti}, csrnzval::Vector{Tv},
        csccolptr::Vector{Ti}, cscrowval::Vector{Ti}, cscnzval::Vector{Tv} )

    # Compute the CSR form's row counts and store them shifted forward by one in csrrowptr
    fill!(csrrowptr, 0)
    coolen = length(I)
    @inbounds for k in 1:coolen
        Ik = I[k]
        if 1 > Ik || m < Ik
            throw(ArgumentError("row indices I[k] must satisfy 1 <= I[k] <= m"))
        end
        csrrowptr[Ik+1] += 1
    end

    # Compute the CSR form's rowptrs and store them shifted forward by one in csrrowptr
    countsum = 1
    csrrowptr[1] = 1
    @inbounds for i in 2:(m+1)
        overwritten = csrrowptr[i]
        csrrowptr[i] = countsum
        countsum += overwritten
    end

    # Counting-sort the column and nonzero values from J and V into csrcolval and csrnzval
    # Tracking write positions in csrrowptr corrects the row pointers
    @inbounds for k in 1:coolen
        Ik, Jk = I[k], J[k]
        if 1 > Jk || n < Jk
            throw(ArgumentError("column indices J[k] must satisfy 1 <= J[k] <= n"))
        end
        csrk = csrrowptr[Ik+1]
        csrrowptr[Ik+1] = csrk+1
        csrcolval[csrk] = Jk
        csrnzval[csrk] = V[k]
    end
    # This completes the unsorted-row, has-repeats CSR form's construction

    # Sweep through the CSR form, simultaneously (1) caculating the CSC form's column
    # counts and storing them shifted forward by one in csccolptr; (2) detecting repeated
    # entries; and (3) repacking the CSR form with the repeated entries combined.
    #
    # Minimizing extraneous communication and nonlocality of reference, primarily by using
    # only a single auxiliary array in this step, is the key to this method's performance.
    fill!(csccolptr, 0)
    fill!(klasttouch, 0)
    writek = 1
    newcsrrowptri = 1
    origcsrrowptri = 1
    origcsrrowptrip1 = csrrowptr[2]
    @inbounds for i in 1:m
        for readk in origcsrrowptri:(origcsrrowptrip1-1)
            j = csrcolval[readk]
            if klasttouch[j] < newcsrrowptri
                klasttouch[j] = writek
                if writek != readk
                    csrcolval[writek] = j
                    csrnzval[writek] = csrnzval[readk]
                end
                writek += 1
                csccolptr[j+1] += 1
            else
                klt = klasttouch[j]
                csrnzval[klt] = combine(csrnzval[klt], csrnzval[readk])
            end
        end
        newcsrrowptri = writek
        origcsrrowptri = origcsrrowptrip1
        origcsrrowptrip1 != writek && (csrrowptr[i+1] = writek)
        i < m && (origcsrrowptrip1 = csrrowptr[i+2])
    end

    # Compute the CSC form's colptrs and store them shifted forward by one in csccolptr
    countsum = 1
    csccolptr[1] = 1
    @inbounds for j in 2:(n+1)
        overwritten = csccolptr[j]
        csccolptr[j] = countsum
        countsum += overwritten
    end

    # Now knowing the CSC form's entry count, resize cscrowval and cscnzval if necessary
    cscnnz = countsum - 1
    length(cscrowval) < cscnnz && resize!(cscrowval, cscnnz)
    length(cscnzval) < cscnnz && resize!(cscnzval, cscnnz)

    # Finally counting-sort the row and nonzero values from the CSR form into cscrowval and
    # cscnzval. Tracking write positions in csccolptr corrects the column pointers.
    @inbounds for i in 1:m
        for csrk in csrrowptr[i]:(csrrowptr[i+1]-1)
            j = csrcolval[csrk]
            x = csrnzval[csrk]
            csck = csccolptr[j+1]
            csccolptr[j+1] = csck+1
            cscrowval[csck] = i
            cscnzval[csck] = x
        end
    end

    SparseMatrixCSC(m, n, csccolptr, cscrowval, cscnzval)
end
function sparse!{Tv,Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti},
        V::AbstractVector{Tv}, m::Integer, n::Integer, combine, klasttouch::Vector{Ti},
        csrrowptr::Vector{Ti}, csrcolval::Vector{Ti}, csrnzval::Vector{Tv},
        csccolptr::Vector{Ti} )
    sparse!(I, J, V, m, n, combine, klasttouch,
            csrrowptr, csrcolval, csrnzval,
            csccolptr, Vector{Ti}(), Vector{Tv}() )
end
function sparse!{Tv,Ti<:Integer}(I::AbstractVector{Ti}, J::AbstractVector{Ti},
        V::AbstractVector{Tv}, m::Integer, n::Integer, combine, klasttouch::Vector{Ti},
        csrrowptr::Vector{Ti}, csrcolval::Vector{Ti}, csrnzval::Vector{Tv} )
    sparse!(I, J, V, m, n, combine, klasttouch,
            csrrowptr, csrcolval, csrnzval,
            Vector{Ti}(n+1), Vector{Ti}(), Vector{Tv}() )
end

dimlub(I) = isempty(I) ? 0 : Int(maximum(I)) #least upper bound on required sparse matrix dimension

sparse(I,J,v::Number) = sparse(I, J, fill(v,length(I)))

sparse(I,J,V::AbstractVector) = sparse(I, J, V, dimlub(I), dimlub(J))

sparse(I,J,v::Number,m,n) = sparse(I, J, fill(v,length(I)), Int(m), Int(n))

sparse(I,J,V::AbstractVector,m,n) = sparse(I, J, V, Int(m), Int(n), +)

sparse(I,J,V::AbstractVector{Bool},m,n) = sparse(I, J, V, Int(m), Int(n), |)

sparse(I,J,v::Number,m,n,combine::Function) = sparse(I, J, fill(v,length(I)), Int(m), Int(n), combine)

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

## Transposition and permutation methods

"""
    halfperm!{Tv,Ti,Tq<:Integer}(X::SparseMatrixCSC{Tv,Ti}, A::SparseMatrixCSC{Tv,Ti},
        q::AbstractVector{Tq}, f::Function = identity)

Column-permute and transpose `A`, simultaneously applying `f` to each entry of `A`, storing
the result `(f(A)Q)^T` (`map(f, transpose(A[:,q]))`) in `X`.

`X`'s dimensions must match those of `transpose(A)` (`X.m == A.n` and `X.n == A.m`), and `X`
must have enough storage to accommodate all allocated entries in `A` (`length(X.rowval) >= nnz(A)`
and  `length(X.nzval) >= nnz(A)`). Column-permutation `q`'s length must match `A`'s column
count (`length(q) == A.n`).

This method is the parent of several methods performing transposition and permutation
operations on `SparseMatrixCSC`s. As this method performs no argument checking, prefer
the safer child methods (`[c]transpose[!]`, `permute[!]`) to direct use.

This method implements the `HALFPERM` algorithm described in F. Gustavson, "Two fast
algorithms for sparse matrices: multiplication and permuted transposition," ACM TOMS 4(3),
250-269 (1978). The algorithm runs in `O(A.m, A.n, nnz(A))` time and requires no space
beyond that passed in.
"""
function halfperm!{Tv,Ti,Tq<:Integer}(X::SparseMatrixCSC{Tv,Ti}, A::SparseMatrixCSC{Tv,Ti},
        q::AbstractVector{Tq}, f::Function = identity)
    _computecolptrs_halfperm!(X, A)
    _distributevals_halfperm!(X, A, q, f)
    return X
end
"""
Helper method for `halfperm!`. Computes `transpose(A[:,q])`'s column pointers, storing them
shifted one position forward in `X.colptr`; `_distributevals_halfperm!` fixes this shift.
"""
function _computecolptrs_halfperm!{Tv,Ti}(X::SparseMatrixCSC{Tv,Ti}, A::SparseMatrixCSC{Tv,Ti})
    # Compute `transpose(A[:,q])`'s column counts. Store shifted forward one position in X.colptr.
    fill!(X.colptr, 0)
    @inbounds for k in 1:nnz(A)
        X.colptr[A.rowval[k] + 1] += 1
    end
    # Compute `transpose(A[:,q])`'s column pointers. Store shifted forward one position in X.colptr.
    X.colptr[1] = 1
    countsum = 1
    @inbounds for k in 2:(A.m + 1)
        overwritten = X.colptr[k]
        X.colptr[k] = countsum
        countsum += overwritten
    end
end
"""
Helper method for `halfperm!`. With `transpose(A[:,q])`'s column pointers shifted one
position forward in `X.colptr`, computes `map(f, transpose(A[:,q]))` by appropriately
distributing `A.rowval` and `f`-transformed `A.nzval` into `X.rowval` and `X.nzval`
respectively. Simultaneously fixes the one-position-forward shift in `X.colptr`.
"""
function _distributevals_halfperm!{Tv,Ti,Tq<:Integer}(X::SparseMatrixCSC{Tv,Ti},
        A::SparseMatrixCSC{Tv,Ti}, q::AbstractVector{Tq}, f::Function)
    @inbounds for Xi in 1:A.n
        Aj = q[Xi]
        for Ak in nzrange(A, Aj)
            Ai = A.rowval[Ak]
            Xk = X.colptr[Ai + 1]
            X.rowval[Xk] = Xi
            X.nzval[Xk] = f(A.nzval[Ak])
            X.colptr[Ai + 1] += 1
        end
    end
    return # kill potential type instability
end

function ftranspose!{Tv,Ti}(X::SparseMatrixCSC{Tv,Ti}, A::SparseMatrixCSC{Tv,Ti}, f::Function)
    # Check compatibility of source argument A and destination argument X
    if X.n != A.m
        throw(DimensionMismatch(string("destination argument `X`'s column count, ",
            "`X.n (= $(X.n))`, must match source argument `A`'s row count, `A.m (= $(A.m))`")))
    elseif X.m != A.n
        throw(DimensionMismatch(string("destination argument `X`'s row count,
            `X.m (= $(X.m))`, must match source argument `A`'s column count, `A.n (= $(A.n))`")))
    elseif length(X.rowval) < nnz(A)
        throw(ArgumentError(string("the length of destination argument `X`'s `rowval` ",
            "array, `length(X.rowval) (= $(length(X.rowval)))`, must be greater than or ",
            "equal to source argument `A`'s allocated entry count, `nnz(A) (= $(nnz(A)))`")))
    elseif length(X.nzval) < nnz(A)
        throw(ArgumentError(string("the length of destination argument `X`'s `nzval` ",
            "array, `length(X.nzval) (= $(length(X.nzval)))`, must be greater than or ",
            "equal to source argument `A`'s allocated entry count, `nnz(A) (= $(nnz(A)))`")))
    end
    halfperm!(X, A, 1:A.n, f)
end
transpose!{Tv,Ti}(X::SparseMatrixCSC{Tv,Ti}, A::SparseMatrixCSC{Tv,Ti}) = ftranspose!(X, A, identity)
ctranspose!{Tv,Ti}(X::SparseMatrixCSC{Tv,Ti}, A::SparseMatrixCSC{Tv,Ti}) = ftranspose!(X, A, conj)

function ftranspose{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, f::Function)
    X = SparseMatrixCSC(A.n, A.m, Vector{Ti}(A.m+1), Vector{Ti}(nnz(A)), Vector{Tv}(nnz(A)))
    halfperm!(X, A, 1:A.n, f)
end
transpose{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}) = ftranspose(A, identity)
ctranspose{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}) = ftranspose(A, conj)

"""
    unchecked_noalias_permute!{Tv,Ti,Tp<:Integer,Tq<:Integer}(X::SparseMatrixCSC{Tv,Ti},
        A::SparseMatrixCSC{Tv,Ti}, p::AbstractVector{Tp}, q::AbstractVector{Tq},
        C::SparseMatrixCSC{Tv,Ti})

See [`permute!`](:func:`Base.SparseArrays.permute!`) for basic usage. Parent of `permute[!]`
methods operating on `SparseMatrixCSC`s that assume none of `X`, `A`, and `C` alias each
other. As this method performs no argument checking, prefer the safer child methods
(`permute[!]`) to direct use.

This method consists of two major steps: (1) Column-permute (`Q`,`I[:,q]`) and transpose `A`
to generate intermediate result `(AQ)^T` (`transpose(A[:,q])`) in `C`. (2) Column-permute
(`P^T`, I[:,p]) and transpose intermediate result `(AQ)^T` to generate result
`((AQ)^T P^T)^T = PAQ` (`A[p,q]`) in `X`.

The first step is a call to `halfperm!`, and the second is a variant on `halfperm!` that
avoids an unnecessary length-`nnz(A)` array-sweep and associated recomputation of column
pointers. See [`halfperm!`](:func:Base.SparseArrays.halfperm!) for additional algorithmic
information.

See also: [`unchecked_aliasing_permute!`](:func:`Base.SparseArrays.unchecked_aliasing_permute!`)
"""
function unchecked_noalias_permute!{Tv,Ti,Tp<:Integer,Tq<:Integer}(X::SparseMatrixCSC{Tv,Ti},
        A::SparseMatrixCSC{Tv,Ti}, p::AbstractVector{Tp}, q::AbstractVector{Tq},
        C::SparseMatrixCSC{Tv,Ti})
    halfperm!(C, A, q)
    _computecolptrs_permute!(X, A, q, X.colptr)
    _distributevals_halfperm!(X, C, p, identity)
    return X
end
"""
    unchecked_aliasing_permute!{Tv,Ti,Tp<:Integer,Tq<:Integer}(
        A::SparseMatrixCSC{Tv,Ti}, p::AbstractVector{Tp}, q::AbstractVector{Tq},
        C::SparseMatrixCSC{Tv,Ti}, workcolptr::Vector{Ti})

See [`permute!`](:func:`Base.SparseArrays.permute!`) for basic usage. Parent of `permute!`
methods operating on `SparseMatrixCSC`s where the source and destination matrices are the
same. See [`unchecked_noalias_permute!`](:func:`Base.SparseArrays.unchecked_noalias_permute`)
for additional information; these methods are identical but for this method's requirement of
the additional `workcolptr`, `length(workcolptr) >= A.n + 1`, which enables efficient
handling of the source-destination aliasing.
"""
function unchecked_aliasing_permute!{Tv,Ti,Tp<:Integer,Tq<:Integer}(
        A::SparseMatrixCSC{Tv,Ti}, p::AbstractVector{Tp}, q::AbstractVector{Tq},
        C::SparseMatrixCSC{Tv,Ti}, workcolptr::Vector{Ti})
    halfperm!(C, A, q)
    _computecolptrs_permute!(A, A, q, workcolptr)
    _distributevals_halfperm!(A, C, p, identity)
    return A
end
"""
Helper method for `unchecked_noalias_permute!` and `unchecked_aliasing_permute!`.
Computes `PAQ`'s column pointers, storing them shifted one position forward in `X.colptr`;
`_distributevals_halfperm!` fixes this shift. Saves some work relative to
`_computecolptrs_halfperm!` as described in `uncheckednoalias_permute!`'s documentation.
"""
function _computecolptrs_permute!{Tv,Ti,Tq<:Integer}(X::SparseMatrixCSC{Tv,Ti},
        A::SparseMatrixCSC{Tv,Ti}, q::AbstractVector{Tq}, workcolptr::Vector{Ti})
    # Compute `A[p,q]`'s column counts. Store shifted forward one position in workcolptr.
    @inbounds for k in 1:A.n
        workcolptr[k+1] = A.colptr[q[k] + 1] - A.colptr[q[k]]
    end
    # Compute `A[p,q]`'s column pointers. Store shifted forward one position in X.colptr.
    X.colptr[1] = 1
    countsum = 1
    @inbounds for k in 2:(X.n + 1)
        overwritten = workcolptr[k]
        X.colptr[k] = countsum
        countsum += overwritten
    end
end

"""
Helper method for `permute` and `permute!` methods operating on `SparseMatrixCSC`s.
Checks compatibility of source argument `A`, row-permutation argument `p`, and
column-permutation argument `q`.
"""
function _checkargs_sourcecompatperms_permute!{Tv,Ti,Tp<:Integer,Tq<:Integer}(
        A::SparseMatrixCSC{Tv,Ti}, p::AbstractVector{Tp}, q::AbstractVector{Tq})
     if length(q) != A.n
         throw(DimensionMismatch(string("the length of column-permutation argument `q`, ",
             "`length(q) (= $(length(q)))`, must match source argument `A`'s column ",
             "count, `A.n (= $(A.n))`")))
     elseif length(p) != A.m
         throw(DimensionMismatch(string("the length of row-permutation argument `p`, ",
             "`length(p) (= $(length(p)))`, must match source argument `A`'s row count, ",
             "`A.m (= $(A.m))`")))
     end
end
"""
Helper method for `permute` and `permute!` methods operating on `SparseMatrixCSC`s.
Checks whether row- and column- permutation arguments `p` and `q` are valid permutations.
"""
function _checkargs_permutationsvalid_permute!{Ti<:Integer,Tp<:Integer,Tq<:Integer}(
        p::AbstractVector{Tp}, pcheckspace::Vector{Ti},
        q::AbstractVector{Tq}, qcheckspace::Vector{Ti})
    if !_ispermutationvalid_permute!(p, pcheckspace)
        throw(ArgumentError("row-permutation argument `p` must be a valid permutation"))
    elseif !_ispermutationvalid_permute!(q, qcheckspace)
        throw(ArgumentError("column-permutation argument `q` must be a valid permutation"))
    end
end
function _ispermutationvalid_permute!{Ti<:Integer,Tp<:Integer}(perm::AbstractVector{Tp},
        checkspace::Vector{Ti})
    n = length(perm)
    checkspace[1:n] = 0
    for k in perm
        (0 < k <= n) && ((checkspace[k] $= 1) == 1) || return false
    end
    return true
end
"""
Helper method for `permute` and `permute!` methods operating on `SparseMatrixCSC`s.
Checks compatibility of source argument `A` and destination argument `X`.
"""
function _checkargs_sourcecompatdest_permute!{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti},
        X::SparseMatrixCSC{Tv,Ti})
    if X.m != A.m
        throw(DimensionMismatch(string("destination argument `X`'s row count, ",
            "`X.m (= $(X.m))`, must match source argument `A`'s row count, `A.m (= $(A.m))`")))
    elseif X.n != A.n
        throw(DimensionMismatch(string("destination argument `X`'s column count, ",
            "`X.n (= $(X.n))`, must match source argument `A`'s column count, `A.n (= $(A.n))`")))
    elseif length(X.rowval) < nnz(A)
        throw(ArgumentError(string("the length of destination argument `X`'s `rowval` ",
            "array, `length(X.rowval) (= $(length(X.rowval)))`, must be greater than or ",
            "equal to source argument `A`'s allocated entry count, `nnz(A) (= $(nnz(A)))`")))
    elseif length(X.nzval) < nnz(A)
        throw(ArgumentError(string("the length of destination argument `X`'s `nzval` ",
            "array, `length(X.nzval) (= $(length(X.nzval)))`, must be greater than or ",
            "equal to source argument `A`'s allocated entry count, `nnz(A) (= $(nnz(A)))`")))
    end
end
"""
Helper method for `permute` and `permute!` methods operating on `SparseMatrixCSC`s.
Checks compatibility of source argument `A` and intermediate result argument `C`.
"""
function _checkargs_sourcecompatworkmat_permute!{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti},
        C::SparseMatrixCSC{Tv,Ti})
    if C.n != A.m
        throw(DimensionMismatch(string("intermediate result argument `C`'s column count, ",
            "`C.n (= $(C.n))`, must match source argument `A`'s row count, `A.m (= $(A.m))`")))
    elseif C.m != A.n
        throw(DimensionMismatch(string("intermediate result argument `C`'s row count, ",
            "`C.m (= $(C.m))`, must match source argument `A`'s column count, `A.n (= $(A.n))`")))
    elseif length(C.rowval) < nnz(A)
        throw(ArgumentError(string("the length of intermediate result argument `C`'s ",
            "`rowval` array, `length(C.rowval) (= $(length(C.rowval)))`, must be greater than ",
            "or equal to source argument `A`'s allocated entry count, `nnz(A) (= $(nnz(A)))`")))
    elseif length(C.nzval) < nnz(A)
        throw(ArgumentError(string("the length of intermediate result argument `C`'s ",
            "`rowval` array, `length(C.nzval) (= $(length(C.nzval)))`, must be greater than ",
            "or equal to source argument `A`'s allocated entry count, `nnz(A)` (= $(nnz(A)))")))
    end
end
"""
Helper method for `permute` and `permute!` methods operating on `SparseMatrixCSC`s.
Checks compatibility of source argument `A` and workspace argument `workcolptr`.
"""
function _checkargs_sourcecompatworkcolptr_permute!{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti},
        workcolptr::Vector{Ti})
    if length(workcolptr) <= A.n
        throw(DimensionMismatch(string("argument `workcolptr`'s length, ",
            "`length(workcolptr) (= $(length(workcolptr)))`, must exceed source argument ",
            "`A`'s column count, `A.n (= $(A.n))`")))
    end
end
"""
    permute!{Tv,Ti,Tp<:Integer,Tq<:Integer}(X::SparseMatrixCSC{Tv,Ti}, A::SparseMatrixCSC{Tv,Ti},
        p::AbstractVector{Tp}, q::AbstractVector{Tq}[, C::SparseMatrixCSC{Tv,Ti}])

Bilaterally permute `A`, storing result `PAQ` (`A[p,q]`) in `X`. Stores intermediate result
`(AQ)^T` (`transpose(A[:,q])`) in optional argument `C` if present. Requires that none of
`X`, `A`, and, if present, `C` alias each other; to store result `PAQ` back into `A`, use
the following method lacking `X`:

    permute!{Tv,Ti,Tp<:Integer,Tq<:Integer}(A::SparseMatrixCSC{Tv,Ti}, p::AbstractVector{Tp},
        q::AbstractVector{Tq}[, C::SparseMatrixCSC{Tv,Ti}[, workcolptr::Vector{Ti}]])

`X`'s dimensions must match those of `A` (`X.m == A.m` and `X.n == A.n`), and `X` must
have enough storage to accommodate all allocated entries in `A` (`length(X.rowval) >= nnz(A)`
and `length(X.nzval) >= nnz(A)`). Column-permutation `q`'s length must match `A`'s column
count (`length(q) == A.n`). Row-permutation `p`'s length must match `A`'s row count
(`length(p) == A.m`).

`C`'s dimensions must match those of `transpose(A)` (`C.m == A.n` and `C.n == A.m`), and `C`
must have enough storage to accommodate all allocated entries in `A` (`length(C.rowval)` >= nnz(A)`
and `length(C.nzval) >= nnz(A)`).

For additional (algorithmic) information, and for versions of these methods that forgo
argument checking, see (unexported) parent methods [`unchecked_noalias_permute!`](:func:`Base.SparseArrays.unchecked_noalias_permute!`)
and [`unchecked_aliasing_permute!`](:func:`Base.SparseArrays.unchecked_aliasing_permute!`).

See also: [`permute`](:func:`Base.SparseArrays.permute`)
"""
function permute!{Tv,Ti,Tp<:Integer,Tq<:Integer}(X::SparseMatrixCSC{Tv,Ti},
        A::SparseMatrixCSC{Tv,Ti}, p::AbstractVector{Tp}, q::AbstractVector{Tq})
    _checkargs_sourcecompatdest_permute!(A, X)
    _checkargs_sourcecompatperms_permute!(A, p, q)
    C = SparseMatrixCSC(A.n, A.m, Vector{Ti}(A.m + 1), Vector{Ti}(nnz(A)), Vector{Tv}(nnz(A)))
    _checkargs_permutationsvalid_permute!(p, C.colptr, q, X.colptr)
    unchecked_noalias_permute!(X, A, p, q, C)
end
function permute!{Tv,Ti,Tp<:Integer,Tq<:Integer}(X::SparseMatrixCSC{Tv,Ti},
        A::SparseMatrixCSC{Tv,Ti}, p::AbstractVector{Tp}, q::AbstractVector{Tq},
        C::SparseMatrixCSC{Tv,Ti})
    _checkargs_sourcecompatdest_permute!(A, X)
    _checkargs_sourcecompatperms_permute!(A, p, q)
    _checkargs_sourcecompatworkmat_permute!(A, C)
    _checkargs_permutationsvalid_permute!(p, C.colptr, q, X.colptr)
    unchecked_noalias_permute!(X, A, p, q, C)
end
function permute!{Tv,Ti,Tp<:Integer,Tq<:Integer}(A::SparseMatrixCSC{Tv,Ti},
        p::AbstractVector{Tp}, q::AbstractVector{Tq})
    _checkargs_sourcecompatperms_permute!(A, p, q)
    C = SparseMatrixCSC(A.n, A.m, Vector{Ti}(A.m + 1), Vector{Ti}(nnz(A)), Vector{Tv}(nnz(A)))
    workcolptr = Vector{Ti}(A.n + 1)
    _checkargs_permutationsvalid_permute!(p, C.colptr, q, workcolptr)
    unchecked_aliasing_permute!(A, p, q, C, workcolptr)
end
function permute!{Tv,Ti,Tp<:Integer,Tq<:Integer}(A::SparseMatrixCSC{Tv,Ti},
        p::AbstractVector{Tp}, q::AbstractVector{Tq}, C::SparseMatrixCSC{Tv,Ti})
    _checkargs_sourcecompatperms_permute!(A, p, q)
    _checkargs_sourcecompatworkmat_permute!(A, C)
    workcolptr = Vector{Ti}(A.n + 1)
    _checkargs_permutationsvalid_permute!(p, C.colptr, q, workcolptr)
    unchecked_aliasing_permute!(A, p, q, C, workcolptr)
end
function permute!{Tv,Ti,Tp<:Integer,Tq<:Integer}(A::SparseMatrixCSC{Tv,Ti},
        p::AbstractVector{Tp}, q::AbstractVector{Tq}, C::SparseMatrixCSC{Tv,Ti},
        workcolptr::Vector{Ti})
    _checkargs_sourcecompatperms_permute!(A, p, q)
    _checkargs_sourcecompatworkmat_permute!(A, C)
    _checkargs_sourcecompatworkcolptr_permute!(A, workcolptr)
    _checkargs_permutationsvalid_permute!(p, C.colptr, q, workcolptr)
    unchecked_aliasing_permute!(A, p, q, C, workcolptr)
end
"""
    permute{Tv,Ti,Tp<:Integer,Tq<:Integer}(A::SparseMatrixCSC{Tv,Ti}, p::AbstractVector{Tp},
        q::AbstractVector{Tq})

Bilaterally permute `A`, returning `PAQ` (`A[p,q]`). Column-permutation `q`'s length must
match `A`'s column count (`length(q) == A.n`). Row-permutation `p`'s length must match `A`'s
row count (`length(p) == A.m`).

For expert drivers and additional information, see [`permute!`](:func:`Base.SparseArrays.permute!`).
"""
function permute{Tv,Ti,Tp<:Integer,Tq<:Integer}(A::SparseMatrixCSC{Tv,Ti},
        p::AbstractVector{Tp}, q::AbstractVector{Tq})
    _checkargs_sourcecompatperms_permute!(A, p, q)
    X = SparseMatrixCSC(A.m, A.n, Vector{Ti}(A.n + 1), Vector{Ti}(nnz(A)), Vector{Tv}(nnz(A)))
    C = SparseMatrixCSC(A.n, A.m, Vector{Ti}(A.m + 1), Vector{Ti}(nnz(A)), Vector{Tv}(nnz(A)))
    _checkargs_permutationsvalid_permute!(p, C.colptr, q, X.colptr)
    unchecked_noalias_permute!(X, A, p, q, C)
end

## fkeep! and children tril!, triu!, droptol!, dropzeros[!]

"""
    fkeep!(A::AbstractSparseArray, f, trim::Bool = true)

Keep elements of `A` for which test `f` returns `true`. `f`'s signature should be

    f(i::Integer, [j::Integer,] x) -> Bool

where `i` and `j` are an element's row and column indices and `x` is the element's
value. This method makes a single sweep
through `A`, requiring `O(A.n, nnz(A))`-time for matrices and `O(nnz(A))`-time for vectors
and no space beyond that passed in. If `trim` is `true`, this method trims `A.rowval` or `A.nzind` and
`A.nzval` to length `nnz(A)` after dropping elements.
"""
function fkeep!(A::SparseMatrixCSC, f, trim::Bool = true)
    An = A.n
    Acolptr = A.colptr
    Arowval = A.rowval
    Anzval = A.nzval

    # Sweep through columns, rewriting kept elements in their new positions
    # and updating the column pointers accordingly as we go.
    Awritepos = 1
    oldAcolptrAj = 1
    @inbounds for Aj in 1:An
        for Ak in oldAcolptrAj:(Acolptr[Aj+1]-1)
            Ai = Arowval[Ak]
            Ax = Anzval[Ak]
            # If this element should be kept, rewrite in new position
            if f(Ai, Aj, Ax)
                if Awritepos != Ak
                    Arowval[Awritepos] = Ai
                    Anzval[Awritepos] = Ax
                end
                Awritepos += 1
            end
        end
        oldAcolptrAj = Acolptr[Aj+1]
        Acolptr[Aj+1] = Awritepos
    end

    # Trim A's storage if necessary and desired
    if trim
        Annz = Acolptr[end] - 1
        if length(Arowval) != Annz
            resize!(Arowval, Annz)
        end
        if length(Anzval) != Annz
            resize!(Anzval, Annz)
        end
    end

    A
end

function tril!(A::SparseMatrixCSC, k::Integer = 0, trim::Bool = true)
    if k > A.n-1 || k < 1-A.m
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($(A.m),$(A.n))"))
    end
    fkeep!(A, (i, j, x) -> i + k >= j, trim)
end
function triu!(A::SparseMatrixCSC, k::Integer = 0, trim::Bool = true)
    if k > A.n-1 || k < 1-A.m
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($(A.m),$(A.n))"))
    end
    fkeep!(A, (i, j, x) -> j >= i + k, trim)
end

droptol!(A::SparseMatrixCSC, tol, trim::Bool = true) =
    fkeep!(A, (i, j, x) -> abs(x) > tol, trim)

"""
    dropzeros!(A::SparseMatrixCSC, trim::Bool = true)

Removes stored numerical zeros from `A`, optionally trimming resulting excess space from
`A.rowval` and `A.nzval` when `trim` is `true`.

For an out-of-place version, see [`dropzeros`](:func:`dropzeros`). For
algorithmic information, see [`Base.SparseArrays.fkeep!`](:func:`Base.SparseArrays.fkeep!`).
"""
dropzeros!(A::SparseMatrixCSC, trim::Bool = true) = fkeep!(A, (i, j, x) -> x != 0, trim)
"""
    dropzeros(A::SparseMatrixCSC, trim::Bool = true)

Generates a copy of `A` and removes stored numerical zeros from that copy, optionally
trimming excess space from the result's `rowval` and `nzval` arrays when `trim` is `true`.

For an in-place version and algorithmic information, see [`dropzeros!`](:func:`dropzeros!`).
"""
dropzeros(A::SparseMatrixCSC, trim::Bool = true) = dropzeros!(copy(A), trim)


## Find methods

function find(S::SparseMatrixCSC)
    sz = size(S)
    I, J = findn(S)
    return sub2ind(sz, I, J)
end

function findn{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti})
    numnz = nnz(S)
    I = Array{Ti}(numnz)
    J = Array{Ti}(numnz)

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
    I = Array{Ti}(numnz)
    J = Array{Ti}(numnz)
    V = Array{Tv}(numnz)

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

    I, J = Array{Int}(0), Array{Int}(0) # indices of nonzero elements
    sizehint!(I, round(Int,N*density))
    sizehint!(J, round(Int,N*density))

    # density of nonzero columns:
    L = log1p(-density)
    coldensity = -expm1(m*L) # = 1 - (1-density)^m
    colsparsity = exp(m*L) # = 1 - coldensity
    iL = 1/L

    rows = Array{Int}(0)
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
    sprand([rng],[type],m,[n],p::AbstractFloat,[rfn])

Create a random length `m` sparse vector or `m` by `n` sparse matrix, in
which the probability of any element being nonzero is independently given by
`p` (and hence the mean density of nonzeros is also exactly `p`). Nonzero
values are sampled from the distribution specified by `rfn` and have the type `type`. The uniform
distribution is used in case `rfn` is not specified. The optional `rng`
argument specifies a random number generator, see [Random Numbers](:ref:`Random Numbers <random-numbers>`).
"""
function sprand{T}(r::AbstractRNG, m::Integer, n::Integer, density::AbstractFloat,
                rfn::Function, ::Type{T}=eltype(rfn(r,1)))
    N = m*n
    N == 0 && return spzeros(T,m,n)
    N == 1 && return rand(r) <= density ? sparse([1], [1], rfn(r,1)) : spzeros(T,1,1)

    I,J = sprand_IJ(r, m, n, density)
    sparse_IJ_sorted!(I, J, rfn(r,length(I)), m, n, +)  # it will never need to combine
end

function sprand{T}(m::Integer, n::Integer, density::AbstractFloat,
                rfn::Function, ::Type{T}=eltype(rfn(1)))
    N = m*n
    N == 0 && return spzeros(T,m,n)
    N == 1 && return rand() <= density ? sparse([1], [1], rfn(1)) : spzeros(T,1,1)

    I,J = sprand_IJ(GLOBAL_RNG, m, n, density)
    sparse_IJ_sorted!(I, J, rfn(length(I)), m, n, +)  # it will never need to combine
end

truebools(r::AbstractRNG, n::Integer) = ones(Bool, n)

sprand(m::Integer, n::Integer, density::AbstractFloat) = sprand(GLOBAL_RNG,m,n,density)

sprand(r::AbstractRNG, m::Integer, n::Integer, density::AbstractFloat) = sprand(r,m,n,density,rand,Float64)
sprand{T}(r::AbstractRNG, ::Type{T}, m::Integer, n::Integer, density::AbstractFloat) = sprand(r,m,n,density,(r, i) -> rand(r, T, i), T)
sprand(r::AbstractRNG, ::Type{Bool}, m::Integer, n::Integer, density::AbstractFloat) = sprand(r,m,n,density, truebools, Bool)
sprand{T}(::Type{T}, m::Integer, n::Integer, density::AbstractFloat) = sprand(GLOBAL_RNG, T, m, n, density)


"""
    sprandn([rng], m[,n],p::AbstractFloat)

Create a random sparse vector of length `m` or sparse matrix of size `m` by `n`
with the specified (independent) probability `p` of any entry being nonzero,
where nonzero values are sampled from the normal distribution. The optional `rng`
argument specifies a random number generator, see [Random Numbers](:ref:`Random Numbers <random-numbers>`).
"""
sprandn(r::AbstractRNG, m::Integer, n::Integer, density::AbstractFloat) = sprand(r,m,n,density,randn,Float64)
sprandn(m::Integer, n::Integer, density::AbstractFloat) = sprandn(GLOBAL_RNG,m,n,density)


"""
    spones(S)

Create a sparse array with the same structure as that of `S`, but with every nonzero
element having the value `1.0`.

```jldoctest
julia> A = sparse([1,2,3,4],[2,4,3,1],[5.,4.,3.,2.])
4×4 sparse matrix with 4 Float64 nonzero entries:
        [4, 1]  =  2.0
        [1, 2]  =  5.0
        [3, 3]  =  3.0
        [2, 4]  =  4.0

julia> spones(A)
4×4 sparse matrix with 4 Float64 nonzero entries:
        [4, 1]  =  1.0
        [1, 2]  =  1.0
        [3, 3]  =  1.0
        [2, 4]  =  1.0
```

Note the difference from [`speye`](:func:`speye`).
"""
spones{T}(S::SparseMatrixCSC{T}) =
     SparseMatrixCSC(S.m, S.n, copy(S.colptr), copy(S.rowval), ones(T, S.colptr[end]-1))

"""
    spzeros([type,]m[,n])

Create a sparse vector of length `m` or sparse matrix of size `m x n`. This
sparse array will not contain any nonzero values. No storage will be allocated
for nonzero values during construction. The type defaults to `Float64` if not
specified.
"""
spzeros(m::Integer, n::Integer) = spzeros(Float64, m, n)
spzeros(Tv::Type, m::Integer, n::Integer) = spzeros(Tv, Int, m, n)
function spzeros(Tv::Type, Ti::Type, m::Integer, n::Integer)
    ((m < 0) || (n < 0)) && throw(ArgumentError("invalid Array dimensions"))
    SparseMatrixCSC(m, n, ones(Ti, n+1), Array{Ti}(0), Array{Tv}(0))
end
# de-splatting variant
spzeros(Tv::Type, Ti::Type, sz::Tuple{Integer,Integer}) = spzeros(Tv, Ti, sz[1], sz[2])

speye(n::Integer) = speye(Float64, n)
speye(T::Type, n::Integer) = speye(T, n, n)
speye(m::Integer, n::Integer) = speye(Float64, m, n)

"""
    speye(S)

Create a sparse identity matrix with the same size as `S`.

```jldoctest
julia> A = sparse([1,2,3,4],[2,4,3,1],[5.,4.,3.,2.])
4×4 sparse matrix with 4 Float64 nonzero entries:
        [4, 1]  =  2.0
        [1, 2]  =  5.0
        [3, 3]  =  3.0
        [2, 4]  =  4.0

julia> speye(A)
4×4 sparse matrix with 4 Float64 nonzero entries:
        [1, 1]  =  1.0
        [2, 2]  =  1.0
        [3, 3]  =  1.0
        [4, 4]  =  1.0
```

Note the difference from [`spones`](:func:`spones`).
"""
speye{T}(S::SparseMatrixCSC{T}) = speye(T, size(S, 1), size(S, 2))
eye(S::SparseMatrixCSC) = speye(S)

"""
    speye([type,]m[,n])

Create a sparse identity matrix of size `m x m`. When `n` is supplied,
create a sparse identity matrix of size `m x n`. The type defaults to `Float64`
if not specified.
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

"""
Helper macro for the unary broadcast definitions below. Takes parent method `fp` and a set
of desired child methods `fcs`, and builds an expression defining each of the child methods
such that `broadcast(::typeof(fc), A::SparseMatrixCSC) = fp(fc, A)`.
"""
macro _enumerate_childmethods(fp, fcs...)
    fcexps = Expr(:block)
    for fc in fcs
        push!(fcexps.args, :( $(esc(:broadcast))(::typeof($(esc(fc))), A::SparseMatrixCSC) = $(esc(fp))($(esc(fc)), A) ) )
    end
    return fcexps
end

# Operations that map zeros to zeros and may map nonzeros to zeros, yielding a sparse matrix
"""
Takes unary function `f` that maps zeros to zeros and may map nonzeros to zeros, and returns
a new `SparseMatrixCSC{TiA,TvB}` `B` generated by applying `f` to each nonzero entry in
`A` and retaining only the resulting nonzeros.
"""
function _broadcast_unary_nz2z_z2z_T{TvA,TiA,TvB}(f::Function, A::SparseMatrixCSC{TvA,TiA}, ::Type{TvB})
    Bcolptr = Array{TiA}(A.n + 1)
    Browval = Array{TiA}(nnz(A))
    Bnzval = Array{TvB}(nnz(A))
    Bk = 1
    @inbounds for j in 1:A.n
        Bcolptr[j] = Bk
        for Ak in nzrange(A, j)
            x = f(A.nzval[Ak])
            if x != 0
                Browval[Bk] = A.rowval[Ak]
                Bnzval[Bk] = x
                Bk += 1
            end
        end
    end
    Bcolptr[A.n + 1] = Bk
    resize!(Browval, Bk - 1)
    resize!(Bnzval, Bk - 1)
    return SparseMatrixCSC(A.m, A.n, Bcolptr, Browval, Bnzval)
end
function _broadcast_unary_nz2z_z2z{Tv}(f::Function, A::SparseMatrixCSC{Tv})
    _broadcast_unary_nz2z_z2z_T(f, A, Tv)
end
@_enumerate_childmethods(_broadcast_unary_nz2z_z2z,
    sin, sinh, sind, asin, asinh, asind,
    tan, tanh, tand, atan, atanh, atand,
    sinpi, cosc, ceil, floor, trunc, round)
broadcast(::typeof(real), A::SparseMatrixCSC) = copy(A)
broadcast{Tv,Ti}(::typeof(imag), A::SparseMatrixCSC{Tv,Ti}) = spzeros(Tv, Ti, A.m, A.n)
broadcast{TTv}(::typeof(real), A::SparseMatrixCSC{Complex{TTv}}) = _broadcast_unary_nz2z_z2z_T(real, A, TTv)
broadcast{TTv}(::typeof(imag), A::SparseMatrixCSC{Complex{TTv}}) = _broadcast_unary_nz2z_z2z_T(imag, A, TTv)
ceil{To}(::Type{To}, A::SparseMatrixCSC) = _broadcast_unary_nz2z_z2z_T(ceil, A, To)
floor{To}(::Type{To}, A::SparseMatrixCSC) = _broadcast_unary_nz2z_z2z_T(floor, A, To)
trunc{To}(::Type{To}, A::SparseMatrixCSC) = _broadcast_unary_nz2z_z2z_T(trunc, A, To)
round{To}(::Type{To}, A::SparseMatrixCSC) = _broadcast_unary_nz2z_z2z_T(round, A, To)

# Operations that map zeros to zeros and map nonzeros to nonzeros, yielding a sparse matrix
"""
Takes unary function `f` that maps zeros to zeros and nonzeros to nonzeros, and returns a
new `SparseMatrixCSC{TiA,TvB}` `B` generated by applying `f` to each nonzero entry in `A`.
"""
function _broadcast_unary_nz2nz_z2z_T{TvA,TiA,TvB}(f::Function, A::SparseMatrixCSC{TvA,TiA}, ::Type{TvB})
    Bcolptr = Vector{TiA}(A.n + 1)
    Browval = Vector{TiA}(nnz(A))
    Bnzval = Vector{TvB}(nnz(A))
    copy!(Bcolptr, 1, A.colptr, 1, A.n + 1)
    copy!(Browval, 1, A.rowval, 1, nnz(A))
    @inbounds @simd for k in 1:nnz(A)
        Bnzval[k] = f(A.nzval[k])
    end
    return SparseMatrixCSC(A.m, A.n, Bcolptr, Browval, Bnzval)
end
function _broadcast_unary_nz2nz_z2z{Tv}(f::Function, A::SparseMatrixCSC{Tv})
    _broadcast_unary_nz2nz_z2z_T(f, A, Tv)
end
@_enumerate_childmethods(_broadcast_unary_nz2nz_z2z,
    log1p, expm1, abs, abs2, conj)
broadcast{TTv}(::typeof(abs2), A::SparseMatrixCSC{Complex{TTv}}) = _broadcast_unary_nz2nz_z2z_T(abs2, A, TTv)
broadcast{TTv}(::typeof(abs), A::SparseMatrixCSC{Complex{TTv}}) = _broadcast_unary_nz2nz_z2z_T(abs, A, TTv)
broadcast{TTv<:Integer}(::typeof(abs), A::SparseMatrixCSC{Complex{TTv}}) = _broadcast_unary_nz2nz_z2z_T(abs, A, Float64)
broadcast{TTv<:BigInt}(::typeof(abs), A::SparseMatrixCSC{Complex{TTv}}) = _broadcast_unary_nz2nz_z2z_T(abs, A, BigFloat)
function conj!(A::SparseMatrixCSC)
    @inbounds @simd for k in 1:nnz(A)
        A.nzval[k] = conj(A.nzval[k])
    end
    return A
end

# Operations that map both zeros and nonzeros to zeros, yielding a dense matrix
"""
Takes unary function `f` that maps both zeros and nonzeros to nonzeros, constructs a new
`Matrix{TvB}` `B`, populates all entries of `B` with the result of a single `f(one(zero(Tv)))`
call, and then, for each stored entry in `A`, calls `f` on the entry's value and stores the
result in the corresponding location in `B`.
"""
function _broadcast_unary_nz2nz_z2nz{Tv}(f::Function, A::SparseMatrixCSC{Tv})
    B = fill(f(zero(Tv)), size(A))
    @inbounds for j in 1:A.n
        for k in nzrange(A, j)
            i = A.rowval[k]
            x = A.nzval[k]
            B[i,j] = f(x)
        end
    end
    return B
end
@_enumerate_childmethods(_broadcast_unary_nz2nz_z2nz,
    log, log2, log10, exp, exp2, exp10, sinc, cospi,
    cos, cosh, cosd, acos, acosd,
    cot, coth, cotd, acot, acotd,
    sec, sech, secd, asech,
    csc, csch, cscd, acsch)


## Broadcasting kernels specialized for returning a SparseMatrixCSC

# Operations with zero result if both operands are zero
function gen_broadcast_body_sparse(f::Function, is_first_sparse::Bool)
    F = Expr(:quote, f)
    quote
        Base.Broadcast.check_broadcast_indices(indices(B), A_1)
        Base.Broadcast.check_broadcast_indices(indices(B), A_2)

        colptrB = B.colptr; rowvalB = B.rowval; nzvalB = B.nzval
        colptr1 = A_1.colptr; rowval1 = A_1.rowval; nzval1 = A_1.nzval
        colptr2 = A_2.colptr; rowval2 = A_2.rowval; nzval2 = A_2.nzval

        nnzB = isempty(B) ? 0 : (nnz(A_1) * div(B.n, A_1.n) * div(B.m, A_1.m)  +
                                 nnz(A_2) * div(B.n, A_2.n) * div(B.m, A_2.m))
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
        Base.Broadcast.check_broadcast_indices(indices(B), $A1)
        Base.Broadcast.check_broadcast_indices(indices(B), $A2)

        nnzB = isempty(B) ? 0 :
               nnz($A1) * div(B.n, ($A1).n) * div(B.m, ($A1).m)
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
                 broadcast!(f, spzeros(promote_type(Tv1, Tv2), promote_type(Ti1, Ti2), to_shape(broadcast_indices(A_1, A_2))), A_1, A_2)

@inline broadcast_zpreserving!(args...) = broadcast!(args...)
@inline broadcast_zpreserving(args...) = broadcast(args...)
broadcast_zpreserving{Tv1,Ti1,Tv2,Ti2}(f::Function, A_1::SparseMatrixCSC{Tv1,Ti1}, A_2::SparseMatrixCSC{Tv2,Ti2}) =
                 broadcast_zpreserving!(f, spzeros(promote_type(Tv1, Tv2), promote_type(Ti1, Ti2), to_shape(broadcast_indices(A_1, A_2))), A_1, A_2)
broadcast_zpreserving{Tv,Ti}(f::Function, A_1::SparseMatrixCSC{Tv,Ti}, A_2::Union{Array,BitArray,Number}) =
                 broadcast_zpreserving!(f, spzeros(promote_eltype(A_1, A_2), Ti, to_shape(broadcast_indices(A_1, A_2))), A_1, A_2)
broadcast_zpreserving{Tv,Ti}(f::Function, A_1::Union{Array,BitArray,Number}, A_2::SparseMatrixCSC{Tv,Ti}) =
                 broadcast_zpreserving!(f, spzeros(promote_eltype(A_1, A_2), Ti, to_shape(broadcast_indices(A_1, A_2))), A_1, A_2)


## Binary arithmetic and boolean operators

for op in (+, -, min, max, &, |, $)
    body = gen_broadcast_body_sparse(op, true)
    OP = Symbol(string(op))
    @eval begin
        function ($OP){Tv1,Ti1,Tv2,Ti2}(A_1::SparseMatrixCSC{Tv1,Ti1}, A_2::SparseMatrixCSC{Tv2,Ti2})
            if size(A_1,1) != size(A_2,1) || size(A_1,2) != size(A_2,2)
                throw(DimensionMismatch(""))
            end
            Tv = promote_op($op, Tv1, Tv2)
            B =  spzeros(Tv, promote_type(Ti1, Ti2), to_shape(broadcast_indices(A_1, A_2)))
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
(.^)(::Irrational{:e}, B::SparseMatrixCSC) = exp.(B)
(.^)(A::Number, B::SparseMatrixCSC) = (.^)(A, full(B))
(.^)(A::SparseMatrixCSC, B::Array) = (.^)(full(A), B)
(.^)(A::Array, B::SparseMatrixCSC) = (.^)(A, full(B))

.+{Tv1,Ti1,Tv2,Ti2}(A_1::SparseMatrixCSC{Tv1,Ti1}, A_2::SparseMatrixCSC{Tv2,Ti2}) =
    broadcast!(+, spzeros(promote_op(+, Tv1, Tv2), promote_type(Ti1, Ti2), to_shape(broadcast_indices(A_1, A_2))), A_1, A_2)

function .-{Tva,Tia,Tvb,Tib}(A::SparseMatrixCSC{Tva,Tia}, B::SparseMatrixCSC{Tvb,Tib})
    broadcast!(-, spzeros(promote_op(-, Tva, Tvb), promote_type(Tia, Tib), to_shape(broadcast_indices(A, B))), A, B)
end

## element-wise comparison operators returning SparseMatrixCSC ##
.<{Tv1,Ti1,Tv2,Ti2}(A_1::SparseMatrixCSC{Tv1,Ti1}, A_2::SparseMatrixCSC{Tv2,Ti2}) = broadcast!(<, spzeros( Bool, promote_type(Ti1, Ti2), to_shape(broadcast_indices(A_1, A_2))), A_1, A_2)
.!={Tv1,Ti1,Tv2,Ti2}(A_1::SparseMatrixCSC{Tv1,Ti1}, A_2::SparseMatrixCSC{Tv2,Ti2}) = broadcast!(!=, spzeros( Bool, promote_type(Ti1, Ti2), to_shape(broadcast_indices(A_1, A_2))), A_1, A_2)

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
    fill!(Array{R}(Base.reduced_dims(A,region)), v0)
Base.reducedim_initarray0{R}(A::SparseMatrixCSC, region, v0, ::Type{R}) =
    fill!(Array{R}(Base.reduced_dims0(A,region)), v0)

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

# Specialized mapreduce for +/*
_mapreducezeros(f, ::typeof(+), T::Type, nzeros::Int, v0) =
    nzeros == 0 ? v0 : f(zero(T))*nzeros + v0
_mapreducezeros(f, ::typeof(*), T::Type, nzeros::Int, v0) =
    nzeros == 0 ? v0 : f(zero(T))^nzeros * v0

function Base._mapreduce{T}(f, op::typeof(*), A::SparseMatrixCSC{T})
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

# Specialized mapreducedim for + cols to avoid allocating a
# temporary array when f(0) == 0
function _mapreducecols!{Tv,Ti}(f, op::typeof(+), R::AbstractArray, A::SparseMatrixCSC{Tv,Ti})
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

    if isequal($region, 1) || isequal($region, (1,))
        (N == 0) && (return (fill(zval,1,n), fill(convert($Ti,1),1,n)))
        S = Array{$Tv}(n); I = Array{$Ti}(n)
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
    elseif isequal($region, 2) || isequal($region, (2,))
        (N == 0) && (return (fill(zval,m,1), fill(convert($Ti,1),m,1)))
        S = Array{$Tv}(m); I = Array{$Ti}(m)
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
    elseif isequal($region, (1,2))
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

# Colon translation
getindex(A::SparseMatrixCSC, ::Colon, ::Colon) = copy(A)
getindex(A::SparseMatrixCSC, i, ::Colon)       = getindex(A, i, 1:size(A, 2))
getindex(A::SparseMatrixCSC, ::Colon, i)       = getindex(A, 1:size(A, 1), i)

function getindex_cols{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, J::AbstractVector)
    # for indexing whole columns
    (m, n) = size(A)
    nJ = length(J)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval

    colptrS = Array{Ti}(nJ+1)
    colptrS[1] = 1
    nnzS = 0

    @inbounds for j = 1:nJ
        col = J[j]
        1 <= col <= n || throw(BoundsError())
        nnzS += colptrA[col+1] - colptrA[col]
        colptrS[j+1] = nnzS + 1
    end

    rowvalS = Array{Ti}(nnzS)
    nzvalS  = Array{Tv}(nnzS)
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
    colptrS = Array{Ti}(nJ+1)
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
    rowvalS = Array{Ti}(nnzS)
    nzvalS  = Array{Tv}(nnzS)
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
    colptrS = Array{Ti}(nJ+1)
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

    rowvalS = Array{Ti}(ptrS-1)
    nzvalS  = Array{Tv}(ptrS-1)

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
    colptrS = Array{Ti}(nJ+1)
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

    rowvalS = Array{Ti}(ptrS-1)
    nzvalS  = Array{Tv}(ptrS-1)

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
    colptrS = Array{Ti}(nJ+1)
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
    rowvalS = Array{Ti}(ptrS)
    nzvalS  = Array{Tv}(ptrS)
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
    rowperm = Array{Int}(nr)
    rowvalTemp = Array{Ti}(nr)
    nzvalTemp = Array{Tv}(nr)

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
        sortperm!(unsafe_wrap(Array, pointer(rowperm), nr), unsafe_wrap(Array, pointer(rowvalTemp), nr))
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
    rowvalB = Array{Int}(n)
    nzvalB = Array{Tv}(n)

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
function setindex!{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, v, i::Integer, j::Integer)
    setindex!(A, convert(Tv, v), convert(Ti, i), convert(Ti, j))
end
function setindex!{Tv,Ti<:Integer}(A::SparseMatrixCSC{Tv,Ti}, v::Tv, i::Ti, j::Ti)
    if !((1 <= i <= A.m) & (1 <= j <= A.n))
        throw(BoundsError(A, (i,j)))
    end
    coljfirstk = Int(A.colptr[j])
    coljlastk = Int(A.colptr[j+1] - 1)
    searchk = searchsortedfirst(A.rowval, i, coljfirstk, coljlastk, Base.Order.Forward)
    if searchk <= coljlastk && A.rowval[searchk] == i
        # Column j contains entry A[i,j]. Update and return
        A.nzval[searchk] = v
        return A
    end
    # Column j does not contain entry A[i,j]. If v is nonzero, insert entry A[i,j] = v
    # and return. If to the contrary v is zero, then simply return.
    if v != 0
        insert!(A.rowval, searchk, i)
        insert!(A.nzval, searchk, v)
        @simd for m in (j + 1):(A.n + 1)
            @inbounds A.colptr[m] += 1
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

function setindex!{TvA,TiI<:Integer,TiJ<:Integer}(A::SparseMatrixCSC{TvA}, x::Number,
        I::AbstractVector{TiI}, J::AbstractVector{TiJ})
    if isempty(I) || isempty(J); return A; end
    if !issorted(I); I = sort(I); end
    if !issorted(J); J = sort(J); end
    if (I[1] < 1 || I[end] > A.m) || (J[1] < 1 || J[end] > A.n)
        throw(BoundsError(A, (I, J)))
    end
    if x == 0
        _spsetz_setindex!(A, I, J)
    else
        _spsetnz_setindex!(A, convert(TvA, x), I, J)
    end
end
"""
Helper method for immediately preceding setindex! method. For all (i,j) such that i in I and
j in J, assigns zero to A[i,j] if A[i,j] is a presently-stored entry, and otherwise does nothing.
"""
function _spsetz_setindex!{TvA,TiI<:Integer,TiJ<:Integer}(A::SparseMatrixCSC{TvA},
        I::AbstractVector{TiI}, J::AbstractVector{TiJ})
    lengthI = length(I)
    for j in J
        coljAfirstk = A.colptr[j]
        coljAlastk = A.colptr[j+1] - 1
        coljAfirstk > coljAlastk && continue
        kA = coljAfirstk
        kI = 1
        entrykArow = A.rowval[kA]
        entrykIrow = I[kI]
        while true
            if entrykArow < entrykIrow
                kA += 1
                kA > coljAlastk && break
                entrykArow = A.rowval[kA]
            elseif entrykArow > entrykIrow
                kI += 1
                kI > lengthI && break
                entrykIrow = I[kI]
            else # entrykArow == entrykIrow
                A.nzval[kA] = 0
                kA += 1
                kI += 1
                (kA > coljAlastk || kI > lengthI) && break
                entrykArow = A.rowval[kA]
                entrykIrow = I[kI]
            end
        end
    end
end
"""
Helper method for immediately preceding setindex! method. For all (i,j) such that i in I
and j in J, assigns x to A[i,j] if A[i,j] is a presently-stored entry, and allocates and
assigns x to A[i,j] if A[i,j] is not presently stored.
"""
function _spsetnz_setindex!{Tv,TiI<:Integer,TiJ<:Integer}(A::SparseMatrixCSC{Tv}, x::Tv,
        I::AbstractVector{TiI}, J::AbstractVector{TiJ})
    m, n = size(A)
    lenI = length(I)

    nnzA = nnz(A) + lenI * length(J)

    rowvalA = rowval = A.rowval
    nzvalA = nzval = A.nzval

    rowidx = 1
    nadd = 0
    @inbounds for col in 1:n
        rrange = nzrange(A, col)
        if nadd > 0
            A.colptr[col] = A.colptr[col] + nadd
        end

        if col in J
            if isempty(rrange) # set new vals only
                nincl = lenI
                if nadd == 0
                    rowval = copy(rowvalA)
                    nzval = copy(nzvalA)
                    resize!(rowvalA, nnzA)
                    resize!(nzvalA, nnzA)
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
                                rowval = copy(rowvalA)
                                nzval = copy(nzvalA)
                                resize!(rowvalA, nnzA)
                                resize!(nzvalA, nnzA)
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
                                rowval = copy(rowvalA)
                                nzval = copy(nzvalA)
                                resize!(rowvalA, nnzA)
                                resize!(nzvalA, nnzA)
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
        A.colptr[n+1] = rowidx
        deleteat!(rowvalA, rowidx:nnzA)
        deleteat!(nzvalA, rowidx:nnzA)
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

    if (!isempty(I) && (I[1] < 1 || I[end] > m)) || (!isempty(J) && (J[1] < 1 || J[end] > n))
        throw(BoundsError(A, (I, J)))
    end

    if isempty(I) || isempty(J)
        return A
    end

    nI = length(I)
    nJ = length(J)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    colptrB = B.colptr; rowvalB = B.rowval; nzvalB = B.nzval

    nnzS = nnz(A) + nnz(B)

    colptrS = copy(A.colptr)
    rowvalS = copy(A.rowval)
    nzvalS = copy(A.nzval)

    resize!(rowvalA, nnzS)
    resize!(nzvalA, nnzS)

    colB = 1
    asgn_col = J[colB]

    I_asgn = falses(m)
    I_asgn[I] = true

    ptrS = 1

    @inbounds for col = 1:n

        # Copy column of A if it is not being assigned into
        if colB > nJ || col != J[colB]
            colptrA[col+1] = colptrA[col] + (colptrS[col+1]-colptrS[col])

            for k = colptrS[col]:colptrS[col+1]-1
                rowvalA[ptrS] = rowvalS[k]
                nzvalA[ptrS] = nzvalS[k]
                ptrS += 1
            end
            continue
        end

        ptrA::Int  = colptrS[col]
        stopA::Int = colptrS[col+1]
        ptrB::Int  = colptrB[colB]
        stopB::Int = colptrB[colB+1]

        while ptrA < stopA && ptrB < stopB
            rowA = rowvalS[ptrA]
            rowB = I[rowvalB[ptrB]]
            if rowA < rowB
                rowvalA[ptrS] = rowA
                nzvalA[ptrS] = I_asgn[rowA] ? zero(Tv) : nzvalS[ptrA]
                ptrS += 1
                ptrA += 1
            elseif rowB < rowA
                if nzvalB[ptrB] != zero(Tv)
                    rowvalA[ptrS] = rowB
                    nzvalA[ptrS] = nzvalB[ptrB]
                    ptrS += 1
                end
                ptrB += 1
            else
                rowvalA[ptrS] = rowB
                nzvalA[ptrS] = nzvalB[ptrB]
                ptrS += 1
                ptrB += 1
                ptrA += 1
            end
        end

        while ptrA < stopA
            rowA = rowvalS[ptrA]
            rowvalA[ptrS] = rowA
            nzvalA[ptrS] = I_asgn[rowA] ? zero(Tv) : nzvalS[ptrA]
            ptrS += 1
            ptrA += 1
        end

        while ptrB < stopB
            rowB = I[rowvalB[ptrB]]
            if nzvalB[ptrB] != zero(Tv)
                rowvalA[ptrS] = rowB
                nzvalA[ptrS] = nzvalB[ptrB]
                ptrS += 1
            end
            ptrB += 1
        end

        colptrA[col+1] = ptrS
        colB += 1
    end

    deleteat!(rowvalA, colptrA[end]:length(rowvalA))
    deleteat!(nzvalA, colptrA[end]:length(nzvalA))

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
    nadd = 0
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
                        if (nadd > 0)
                            copy!(rowvalB, bidx, rowvalA, r1, copylen)
                            copy!(nzvalB, bidx, nzvalA, r1, copylen)
                        end
                        bidx += copylen
                        r1 += copylen
                    end
                end

                # 0: no change, 1: update, 2: add new
                mode = ((r1 <= r2) && (rowvalA[r1] == row)) ? 1 : ((v == 0) ? 0 : 2)

                if (mode > 1) && (nadd == 0)
                    # copy storage to take changes
                    colptrA = copy(colptrB)
                    memreq = isa(x, Number) && x == 0 ? 0 : n
                    # this x == 0 check and approach doesn't jive with use of v above
                    # and may not make sense generally, as scalar x == 0 probably
                    # means this section should never be called. also may not be generic.
                    # TODO: clean this up, maybe separate scalar and array X cases
                    rowvalA = copy(rowvalB)
                    nzvalA = copy(nzvalB)
                    resize!(rowvalB, length(rowvalA)+memreq)
                    resize!(nzvalB, length(rowvalA)+memreq)
                end
                if mode == 1
                    rowvalB[bidx] = row
                    nzvalB[bidx] = v
                    bidx += 1
                    r1 += 1
                elseif mode == 2
                    rowvalB[bidx] = row
                    nzvalB[bidx] = v
                    bidx += 1
                    nadd += 1
                end
                (xidx > n) && break
            end # if I[row, col]
        end # for row in 1:A.m

        if (nadd != 0)
            l = r2-r1+1
            if l > 0
                copy!(rowvalB, bidx, rowvalA, r1, l)
                copy!(nzvalB, bidx, nzvalA, r1, l)
                bidx += l
            end
            colptrB[col+1] = bidx

            if (xidx > n) && (length(colptrB) > (col+1))
                diff = nadd
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

    if (nadd != 0)
        n = length(nzvalB)
        if n > (bidx-1)
            deleteat!(nzvalB, bidx:n)
            deleteat!(rowvalB, bidx:n)
        end
    end
    A
end


function setindex!{Tv,Ti,T<:Real}(A::SparseMatrixCSC{Tv,Ti}, x, I::AbstractVector{T})
    n = length(I)
    (n == 0) && (return A)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval; szA = size(A)
    colptrB = colptrA; rowvalB = rowvalA; nzvalB = nzvalA
    nadd = 0
    bidx = aidx = 1

    S = issorted(I) ? (1:n) : sortperm(I)
    sxidx = r1 = r2 = 0

    if (!isempty(I) && (I[S[1]] < 1 || I[S[end]] > length(A)))
        throw(BoundsError(A, I))
    end

    isa(x, AbstractArray) && setindex_shape_check(x, length(I))

    lastcol = 0
    (nrowA, ncolA) = szA
    @inbounds for xidx in 1:n
        sxidx = S[xidx]
        (sxidx < n) && (I[sxidx] == I[sxidx+1]) && continue

        row,col = ind2sub(szA, I[sxidx])
        v = isa(x, AbstractArray) ? x[sxidx] : x

        if col > lastcol
            r1 = Int(colptrA[col])
            r2 = Int(colptrA[col+1] - 1)

            # copy from last position till current column
            if (nadd > 0)
                colptrB[(lastcol+1):col] = colptrA[(lastcol+1):col] .+ nadd
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
                if (nadd > 0)
                    copy!(rowvalB, bidx, rowvalA, r1, copylen)
                    copy!(nzvalB, bidx, nzvalA, r1, copylen)
                end
                bidx += copylen
                r1 += copylen
                aidx += copylen
            end
        end

        # 0: no change, 1: update, 2: add new
        mode = ((r1 <= r2) && (rowvalA[r1] == row)) ? 1 : ((v == 0) ? 0 : 2)

        if (mode > 1) && (nadd == 0)
            # copy storage to take changes
            colptrA = copy(colptrB)
            memreq = isa(x, Number) && x == 0 ? 0 : n
            # see comment/TODO for same statement in preceding logical setindex! method
            rowvalA = copy(rowvalB)
            nzvalA = copy(nzvalB)
            resize!(rowvalB, length(rowvalA)+memreq)
            resize!(nzvalB, length(rowvalA)+memreq)
        end
        if mode == 1
            rowvalB[bidx] = row
            nzvalB[bidx] = v
            bidx += 1
            aidx += 1
            r1 += 1
        elseif mode == 2
            rowvalB[bidx] = row
            nzvalB[bidx] = v
            bidx += 1
            nadd += 1
        end
    end

    # copy the rest
    @inbounds if (nadd > 0)
        colptrB[(lastcol+1):end] = colptrA[(lastcol+1):end] .+ nadd
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
    end
    A
end

## dropstored! methods
"""
    dropstored!(A::SparseMatrixCSC, i::Integer, j::Integer)

Drop entry `A[i,j]` from `A` if `A[i,j]` is stored, and otherwise do nothing.
"""
function dropstored!(A::SparseMatrixCSC, i::Integer, j::Integer)
    if !((1 <= i <= A.m) & (1 <= j <= A.n))
        throw(BoundsError(A, (i,j)))
    end
    coljfirstk = Int(A.colptr[j])
    coljlastk = Int(A.colptr[j+1] - 1)
    searchk = searchsortedfirst(A.rowval, i, coljfirstk, coljlastk, Base.Order.Forward)
    if searchk <= coljlastk && A.rowval[searchk] == i
        # Entry A[i,j] is stored. Drop and return.
        deleteat!(A.rowval, searchk)
        deleteat!(A.nzval, searchk)
        @simd for m in j:(A.n + 1)
            @inbounds A.colptr[m] -= 1
        end
    end
    return A
end
"""
    dropstored!(A::SparseMatrix, I::AbstractVector{<:Integer}, J::AbstractVector{<:Integer})

For each `(i,j)` where `i in I` and `j in J`, drop entry `A[i,j]` from `A` if `A[i,j]` is
stored and otherwise do nothing. Derivative forms:

    dropstored!(A::SparseMatrixCSC, i::Integer, J::AbstractVector{<:Integer})
    dropstored!(A::SparseMatrixCSC, I::AbstractVector{<:Integer}, j::Integer)
"""
function dropstored!{TiI<:Integer,TiJ<:Integer}(A::SparseMatrixCSC,
        I::AbstractVector{TiI}, J::AbstractVector{TiJ})
    m, n = size(A)
    nnzA = nnz(A)
    (nnzA == 0) && (return A)

    !issorted(I) && (I = sort(I))
    !issorted(J) && (J = sort(J))

    if (!isempty(I) && (I[1] < 1 || I[end] > m)) || (!isempty(J) && (J[1] < 1 || J[end] > n))
        throw(BoundsError(A, (I, J)))
    end

    if isempty(I) || isempty(J)
        return A
    end

    rowval = rowvalA = A.rowval
    nzval = nzvalA = A.nzval
    rowidx = 1
    ndel = 0
    @inbounds for col in 1:n
        rrange = nzrange(A, col)
        if ndel > 0
            A.colptr[col] = A.colptr[col] - ndel
        end

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
                        rowval = copy(rowvalA)
                        nzval = copy(nzvalA)
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
        A.colptr[n+1] = rowidx
        deleteat!(rowvalA, rowidx:nnzA)
        deleteat!(nzvalA, rowidx:nnzA)
    end
    return A
end
dropstored!{T<:Integer}(A::SparseMatrixCSC, i::Integer, J::AbstractVector{T}) = dropstored!(A, [i], J)
dropstored!{T<:Integer}(A::SparseMatrixCSC, I::AbstractVector{T}, j::Integer) = dropstored!(A, I, [j])
dropstored!(A::SparseMatrixCSC, ::Colon, j::Union{Integer,AbstractVector}) = dropstored!(A, 1:size(A,1), j)
dropstored!(A::SparseMatrixCSC, i::Union{Integer,AbstractVector}, ::Colon) = dropstored!(A, i, 1:size(A,2))
dropstored!(A::SparseMatrixCSC, ::Colon, ::Colon) = dropstored!(A, 1:size(A,1), 1:size(A,2))
dropstored!(A::SparseMatrixCSC, ::Colon) = dropstored!(A, :, :)
# TODO: Several of the preceding methods are optimization candidates.
# TODO: Implement linear indexing methods for dropstored! ?
# TODO: Implement logical indexing methods for dropstored! ?


# Sparse concatenation

function vcat(X::SparseMatrixCSC...)
    num = length(X)
    mX = Int[ size(x, 1) for x in X ]
    nX = Int[ size(x, 2) for x in X ]
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

    nnzX = Int[ nnz(x) for x in X ]
    nnz_res = sum(nnzX)
    colptr = Array{Ti}(n + 1)
    rowval = Array{Ti}(nnz_res)
    nzval  = Array{Tv}(nnz_res)

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
    mX = Int[ size(x, 1) for x in X ]
    nX = Int[ size(x, 2) for x in X ]
    m = mX[1]
    for i = 2 : num
        if mX[i] != m; throw(DimensionMismatch("")); end
    end
    n = sum(nX)

    Tv = promote_type(map(x->eltype(x.nzval), X)...)
    Ti = promote_type(map(x->eltype(x.rowval), X)...)

    colptr = Array{Ti}(n + 1)
    nnzX = Int[ nnz(x) for x in X ]
    nnz_res = sum(nnzX)
    rowval = Array{Ti}(nnz_res)
    nzval = Array{Tv}(nnz_res)

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

"""
    blkdiag(A...)

Concatenate matrices block-diagonally. Currently only implemented for sparse matrices.
"""
function blkdiag(X::SparseMatrixCSC...)
    num = length(X)
    mX = Int[ size(x, 1) for x in X ]
    nX = Int[ size(x, 2) for x in X ]
    m = sum(mX)
    n = sum(nX)

    Tv = promote_type(map(x->eltype(x.nzval), X)...)
    Ti = promote_type(map(x->eltype(x.rowval), X)...)

    colptr = Array{Ti}(n + 1)
    nnzX = Int[ nnz(x) for x in X ]
    nnz_res = sum(nnzX)
    rowval = Array{Ti}(nnz_res)
    nzval = Array{Tv}(nnz_res)

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
issymmetric(A::SparseMatrixCSC) = is_hermsym(A, identity)

ishermitian(A::SparseMatrixCSC) = is_hermsym(A, conj)

function is_hermsym(A::SparseMatrixCSC, check::Function)
    m, n = size(A)
    if m != n; return false; end

    colptr = A.colptr
    rowval = A.rowval
    nzval = A.nzval
    tracker = copy(A.colptr)
    for col = 1:A.n
        # `tracker` is updated such that, for symmetric matrices,
        # the loop below starts from an element at or below the
        # diagonal element of column `col`"
        for p = tracker[col]:colptr[col+1]-1
            val = nzval[p]
            row = rowval[p]

            # Ignore stored zeros
            if val == 0
                continue
            end

            # If the matrix was symmetric we should have updated
            # the tracker to start at the diagonal or below. Here
            # we are above the diagonal so the matrix can't be symmetric.
            if row < col
                return false
            end

            # Diagonal element
            if row == col
                if val != check(val)
                    return false
                end
            else
                offset = tracker[row]

                # If the matrix is unsymmetric, there might not exist
                # a rowval[offset]
                if offset > length(rowval)
                    return false
                end

                row2 = rowval[offset]

                # row2 can be less than col if the tracker didn't
                # get updated due to stored zeros in previous elements.
                # We therefore "catch up" here while making sure that
                # the elements are actually zero.
                while row2 < col
                    if nzval[offset] != 0
                        return false
                    end
                    offset += 1
                    row2 = rowval[offset]
                    tracker[row] += 1
                end

                # Non zero A[i,j] exists but A[j,i] does not exist
                if row2 > col
                    return false
                end

                # A[i,j] and A[j,i] exists
                if row2 == col
                    if val != check(nzval[offset])
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
    I = Array{Int}(ncoeffs)
    J = Array{Int}(ncoeffs)
    V = Array{promote_type(map(eltype, B)...)}(ncoeffs)
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
        copy!(view(V, range), vec)
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

```jldoctest
julia> spdiagm(([1,2,3,4],[4,3,2,1]),(-1,1))
5×5 sparse matrix with 8 Int64 nonzero entries:
        [2, 1]  =  1
        [1, 2]  =  4
        [3, 2]  =  2
        [2, 3]  =  3
        [4, 3]  =  3
        [3, 4]  =  2
        [5, 4]  =  4
        [4, 5]  =  1
```
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
    for i in 1:(length(V)-1), j in V[i]:(V[i+1] - 1); res[j] = i end
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

diag{Tv}(A::SparseMatrixCSC{Tv}) = Tv[d for d in SpDiagIterator(A)]

function diagm{Tv,Ti}(v::SparseMatrixCSC{Tv,Ti})
    if (size(v,1) != 1 && size(v,2) != 1)
        throw(DimensionMismatch("input should be nx1 or 1xn"))
    end

    n = length(v)
    numnz = nnz(v)
    colptr = Array{Ti}(n+1)
    rowval = Array{Ti}(numnz)
    nzval = Array{Tv}(numnz)

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
        B = SparseMatrixCSC(mB, nB, Array{Ti}(nB+1), similar(A.rowval), similar(A.nzval))
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

        sortperm!(unsafe_wrap(Array, pointer(index), numrows),
                  unsafe_wrap(Array, pointer(row), numrows))

        jj = 1
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
