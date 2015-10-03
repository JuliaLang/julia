# This file is a part of Julia. License is MIT: http://julialang.org/license

### Common definitions

import Base: Func, AddFun, MulFun, MaxFun, MinFun, SubFun

immutable ComplexFun <: Func{2} end
call(::ComplexFun, x::Real, y::Real) = complex(x, y)

immutable DotFun <: Func{2} end
call(::DotFun, x::Number, y::Number) = conj(x) * y

typealias UnaryOp Union{Function, Func{1}}
typealias BinaryOp Union{Function, Func{2}}

### The SparseVector

### Types

immutable SparseVector{Tv,Ti<:Integer} <: AbstractSparseVector{Tv,Ti}
    n::Int              # the number of elements
    nzind::Vector{Ti}   # the indices of nonzeros
    nzval::Vector{Tv}   # the values of nonzeros

    function SparseVector(n::Integer, nzind::Vector{Ti}, nzval::Vector{Tv})
        n >= 0 || throw(ArgumentError("The number of elements must be non-negative."))
        length(nzind) == length(nzval) ||
            throw(ArgumentError("index and value vectors must be the same length"))
        new(convert(Int, n), nzind, nzval)
    end
end

SparseVector{Tv,Ti}(n::Integer, nzind::Vector{Ti}, nzval::Vector{Tv}) =
    SparseVector{Tv,Ti}(n, nzind, nzval)

### Basic properties

length(x::SparseVector) = x.n
size(x::SparseVector) = (x.n,)
nnz(x::SparseVector) = length(x.nzval)
countnz(x::SparseVector) = countnz(x.nzval)
nonzeros(x::SparseVector) = x.nzval
nonzeroinds(x::SparseVector) = x.nzind

similar{T}(x::SparseVector, ::Type{T}, D::Dims) = spzeros(T, D...)

### Construct empty sparse vector

spzeros(len::Integer) = spzeros(Float64, len)
spzeros{T}(::Type{T}, len::Integer) = SparseVector(len, Int[], T[])

# Construction of same structure, but with all ones
spones{T}(x::SparseVector{T}) = SparseVector(x.n, copy(x.nzind), ones(T, length(x.nzval)))

### Construction from lists of indices and values

function _sparsevector!{Ti<:Integer}(I::Vector{Ti}, V::Vector, len::Integer)
    # pre-condition: no duplicate indexes in I
    if !isempty(I)
        p = sortperm(I)
        permute!(I, p)
        permute!(V, p)
    end
    SparseVector(len, I, V)
end

function _sparsevector!{Tv,Ti<:Integer}(I::Vector{Ti}, V::Vector{Tv}, len::Integer, combine::BinaryOp)
    if !isempty(I)
        p = sortperm(I)
        permute!(I, p)
        permute!(V, p)
        m = length(I)

        # advances to the first non-zero element
        r = 1     # index of last processed entry
        while r <= m
            if V[r] == 0
                r += 1
            else
                break
            end
        end
        r > m && return SparseVector(len, Ti[], Tv[])

        # move r-th to l-th
        l = 1       # length of processed part
        i = I[r]    # row-index of current element
        if r > 1
            I[l] = i; V[l] = V[r]
        end

        # main loop
        while r < m
            r += 1
            i2 = I[r]
            if i2 == i  # accumulate r-th to the l-th entry
                V[l] = combine(V[l], V[r])
            else  # advance l, and move r-th to l-th
                pv = V[l]
                if pv != 0
                    l += 1
                end
                i = i2
                if l < r
                    I[l] = i; V[l] = V[r]
                end
            end
        end
        if V[l] == 0
            l -= 1
        end
        if l < m
            resize!(I, l)
            resize!(V, l)
        end
    end
    SparseVector(len, I, V)
end

"""
    sparsevec(I, V, [m, combine])

Create a sparse vector `S` of length `m` such that `S[I[k]] = V[k]`.
Duplicates are combined using the `combine` function, which defaults to
`+` if it is not provided.
"""
function sparsevec{Tv,Ti<:Integer}(I::AbstractVector{Ti}, V::AbstractVector{Tv}, combine::BinaryOp)
    length(I) == length(V) ||
        throw(ArgumentError("index and value vectors must be the same length"))
    len = 0
    for i in I
        i >= 1 || error("Index must be positive.")
        if i > len
            len = i
        end
    end
    _sparsevector!(collect(Ti, I), collect(Tv, V), len, combine)
end

function sparsevec{Tv,Ti<:Integer}(I::AbstractVector{Ti}, V::AbstractVector{Tv}, len::Integer, combine::BinaryOp)
    length(I) == length(V) ||
        throw(ArgumentError("index and value vectors must be the same length"))
    maxi = convert(Ti, len)
    for i in I
        1 <= i <= maxi || throw(ArgumentError("An index is out of bound."))
    end
    _sparsevector!(collect(Ti, I), collect(Tv, V), len, combine)
end

sparsevec{Ti<:Integer}(I::AbstractVector{Ti}, V::AbstractVector) =
    sparsevec(I, V, AddFun())

sparsevec{Ti<:Integer}(I::AbstractVector{Ti}, V::AbstractVector, len::Integer) =
    sparsevec(I, V, len, AddFun())

sparsevec{Ti<:Integer}(I::AbstractVector{Ti}, v::Number, combine::BinaryOp) =
    sparsevec(I, fill(v, length(I)), combine)

sparsevec{Ti<:Integer}(I::AbstractVector{Ti}, v::Number, len::Integer, combine::BinaryOp) =
    sparsevec(I, fill(v, length(I)), len, combine)

sparsevec{Ti<:Integer}(I::AbstractVector{Ti}, v::Number) =
    sparsevec(I, v, AddFun())

sparsevec{Ti<:Integer}(I::AbstractVector{Ti}, v::Number, len::Integer) =
    sparsevec(I, v, len, AddFun())


### Construction from dictionary
"""
    sparsevec(D::Dict, [m])

Create a sparse vector of length `m` where the nonzero indices are keys from
the dictionary, and the nonzero values are the values from the dictionary.
"""
function sparsevec{Tv,Ti<:Integer}(dict::Associative{Ti,Tv})
    m = length(dict)
    nzind = Array(Ti, m)
    nzval = Array(Tv, m)

    cnt = 0
    len = zero(Ti)
    for (k, v) in dict
        k >= 1 || throw(ArgumentError("index must be positive."))
        if k > len
            len = k
        end
        if v != 0
            cnt += 1
            @inbounds nzind[cnt] = k
            @inbounds nzval[cnt] = v
        end
    end
    resize!(nzind, cnt)
    resize!(nzval, cnt)
    _sparsevector!(nzind, nzval, len)
end

function sparsevec{Tv,Ti<:Integer}(dict::Associative{Ti,Tv}, len::Integer)
    m = length(dict)
    nzind = Array(Ti, m)
    nzval = Array(Tv, m)

    cnt = 0
    maxk = convert(Ti, len)
    for (k, v) in dict
        1 <= k <= maxk || throw(ArgumentError("an index (key) is out of bound."))
        if v != 0
            cnt += 1
            @inbounds nzind[cnt] = k
            @inbounds nzval[cnt] = v
        end
    end
    resize!(nzind, cnt)
    resize!(nzval, cnt)
    _sparsevector!(nzind, nzval, len)
end


### Element access

function setindex!{Tv,Ti<:Integer}(x::SparseVector{Tv,Ti}, v::Tv, i::Ti)
    checkbounds(x, i)
    nzind = nonzeroinds(x)
    nzval = nonzeros(x)

    m = length(nzind)
    k = searchsortedfirst(nzind, i)
    if 1 <= k <= m && nzind[k] == i  # i found
        if v == 0
            deleteat!(nzind, k)
            deleteat!(nzval, k)
        else
            nzval[k] = v
        end
    else  # i not found
        if v != 0
            insert!(nzind, k, i)
            insert!(nzval, k, v)
        end
    end
    x
end

setindex!{Tv, Ti<:Integer}(x::SparseVector{Tv,Ti}, v, i::Integer) =
    setindex!(x, convert(Tv, v), convert(Ti, i))


### Conversion

# convert SparseMatrixCSC to SparseVector
function convert{Tv,Ti<:Integer}(::Type{SparseVector{Tv,Ti}}, s::SparseMatrixCSC{Tv,Ti})
    size(s, 2) == 1 || throw(ArgumentError("The input argument must have a single-column."))
    SparseVector(s.m, s.rowval, s.nzval)
end

convert{Tv,Ti}(::Type{SparseVector{Tv}}, s::SparseMatrixCSC{Tv,Ti}) =
    convert(SparseVector{Tv,Ti}, s)

convert{Tv,Ti}(::Type{SparseVector}, s::SparseMatrixCSC{Tv,Ti}) =
    convert(SparseVector{Tv,Ti}, s)

# convert Vector to SparseVector

"""
    sparsevec(A)

Convert a vector `A` into a sparse vector of length `m`.
"""
sparsevec{T}(a::AbstractVector{T}) = convert(SparseVector{T, Int}, a)
sparsevec(a::AbstractArray) = sparsevec(vec(a))
sparsevec(a::AbstractSparseArray) = vec(a)
sparse(a::AbstractVector) = sparsevec(a)

function _dense2sparsevec{Tv,Ti}(s::AbstractArray{Tv}, initcap::Ti)
    # pre-condition: initcap > 0; the initcap determines the index type
    n = length(s)
    cap = initcap
    nzind = Array(Ti, cap)
    nzval = Array(Tv, cap)
    c = 0
    @inbounds for i = 1:n
        v = s[i]
        if v != 0
            if c >= cap
                cap *= 2
                resize!(nzind, cap)
                resize!(nzval, cap)
            end
            c += 1
            nzind[c] = i
            nzval[c] = v
        end
    end
    if c < cap
        resize!(nzind, c)
        resize!(nzval, c)
    end
    SparseVector(n, nzind, nzval)
end

convert{Tv,Ti}(::Type{SparseVector{Tv,Ti}}, s::AbstractVector{Tv}) =
    _dense2sparsevec(s, convert(Ti, max(8, div(length(s), 8))))

convert{Tv}(::Type{SparseVector{Tv}}, s::AbstractVector{Tv}) =
    convert(SparseVector{Tv,Int}, s)

convert{Tv}(::Type{SparseVector}, s::AbstractVector{Tv}) =
    convert(SparseVector{Tv,Int}, s)


# convert between different types of SparseVector
convert{Tv,Ti,TvS,TiS}(::Type{SparseVector{Tv,Ti}}, s::SparseVector{TvS,TiS}) =
    SparseVector{Tv,Ti}(s.n, convert(Vector{Ti}, s.nzind), convert(Vector{Tv}, s.nzval))

convert{Tv,TvS,TiS}(::Type{SparseVector{Tv}}, s::SparseVector{TvS,TiS}) =
    SparseVector{Tv,TiS}(s.n, s.nzind, convert(Vector{Tv}, s.nzval))


### Rand Construction
sprand{T}(n::Integer, p::AbstractFloat, rfn::Function, ::Type{T}) = sprand(GLOBAL_RNG, n, p, rfn, T)
function sprand{T}(r::AbstractRNG, n::Integer, p::AbstractFloat, rfn::Function, ::Type{T})
    I = randsubseq(r, 1:convert(Int, n), p)
    V = rfn(r, T, length(I))
    SparseVector(n, I, V)
end

sprand(n::Integer, p::AbstractFloat, rfn::Function) = sprand(GLOBAL_RNG, n, p, rfn)
function sprand(r::AbstractRNG, n::Integer, p::AbstractFloat, rfn::Function)
    I = randsubseq(r, 1:convert(Int, n), p)
    V = rfn(r, length(I))
    SparseVector(n, I, V)
end

sprand{T}(n::Integer, p::AbstractFloat, ::Type{T}) = sprand(GLOBAL_RNG, n, p, rand, T)
sprand(n::Integer, p::AbstractFloat) = sprand(GLOBAL_RNG, n, p, rand)
sprand{T}(r::AbstractRNG, n::Integer, p::AbstractFloat, ::Type{T}) = sprand(r, n, p, rand, T)
sprand(r::AbstractRNG, n::Integer, p::AbstractFloat) = sprand(r, n, p, rand)

sprandn(n::Integer, p::AbstractFloat) = sprand(GLOBAL_RNG, n, p, randn)
sprandn(r::AbstractRNG, n::Integer, p::AbstractFloat) = sprand(r, n, p, randn)

sprandbool(n::Integer, p::AbstractFloat) = sprand(GLOBAL_RNG, n, p, truebools)
sprandbool(r::AbstractRNG, n::Integer, p::AbstractFloat) = sprand(r, n, p, truebools)

## Indexing into Matrices can return SparseVectors

function getindex(x::SparseMatrixCSC, ::Colon, j::Integer)
    checkbounds(x, :, j)
    r1 = convert(Int, x.colptr[j])
    r2 = convert(Int, x.colptr[j+1]) - 1
    SparseVector(x.m, x.rowval[r1:r2], x.nzval[r1:r2])
end

function getindex(x::SparseMatrixCSC, I::UnitRange, j::Integer)
    checkbounds(x, I, j)
    # Get the selected column
    c1 = convert(Int, x.colptr[j])
    c2 = convert(Int, x.colptr[j+1]) - 1
    # Restrict to the selected rows
    r1 = searchsortedfirst(x.rowval, first(I), c1, c2, Forward)
    r2 = searchsortedlast(x.rowval, last(I), c1, c2, Forward)
    SparseVector(length(I), x.rowval[r1:r2] - first(I) + 1, x.nzval[r1:r2])
end

# In the general case, we piggy back upon SparseMatrixCSC's optimized solution
@inline function getindex{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::Integer)
    M = A[I, [J]]
    SparseVector(M.m, M.rowval, M.nzval)
end


# Logical and linear indexing into SparseMatrices
getindex{Tv}(A::SparseMatrixCSC{Tv}, I::AbstractVector{Bool}) = _logical_index(A, I) # Ambiguities
getindex{Tv}(A::SparseMatrixCSC{Tv}, I::AbstractArray{Bool}) = _logical_index(A, I)
function _logical_index{Tv}(A::SparseMatrixCSC{Tv}, I::AbstractArray{Bool})
    checkbounds(A, I)
    n = sum(I)

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
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
    n = length(nzvalB)
    if n > (c-1)
        deleteat!(nzvalB, c:n)
        deleteat!(rowvalB, c:n)
    end
    SparseVector(n, rowvalB, nzvalB)
end

# TODO: huge optimizations are available for I::Range and ::Colon
getindex(A::SparseMatrixCSC, ::Colon) = A[1:end]
function getindex{Tv}(A::SparseMatrixCSC{Tv}, I::AbstractVector)
    szA = size(A)
    nA = szA[1]*szA[2]
    colptrA = A.colptr
    rowvalA = A.rowval
    nzvalA = A.nzval

    n = length(I)
    rowvalB = Array(Int, n)
    nzvalB = Array(Tv, n)

    rowB = 1
    idxB = 1

    for i in 1:n
        ((I[i] < 1) | (I[i] > nA)) && throw(BoundsError(A, I))
        row,col = ind2sub(szA, I[i])
        for r in colptrA[col]:(colptrA[col+1]-1)
            @inbounds if rowvalA[r] == row
                rowvalB[idxB] = i
                nzvalB[idxB] = nzvalA[r]
                idxB += 1
                break
            end
        end
    end
    if n > (idxB-1)
        deleteat!(nzvalB, idxB:n)
        deleteat!(rowvalB, idxB:n)
    end
    SparseVector(n, rowvalB, nzvalB)
end


### Generic functions operating on AbstractSparseVector

### getindex

function _spgetindex{Tv,Ti}(m::Int, nzind::AbstractVector{Ti}, nzval::AbstractVector{Tv}, i::Integer)
    ii = searchsortedfirst(nzind, convert(Ti, i))
    (ii <= m && nzind[ii] == i) ? nzval[ii] : zero(Tv)
end

function getindex{Tv}(x::AbstractSparseVector{Tv}, i::Integer)
    checkbounds(x, i)
    _spgetindex(nnz(x), nonzeroinds(x), nonzeros(x), i)
end

function getindex{Tv,Ti}(x::AbstractSparseVector{Tv,Ti}, I::UnitRange)
    checkbounds(x, I)
    xlen = length(x)
    i0 = first(I)
    i1 = last(I)

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    m = length(xnzind)

    # locate the first j0, s.t. xnzind[j0] >= i0
    j0 = searchsortedfirst(xnzind, i0)
    # locate the last j1, s.t. xnzind[j1] <= i1
    j1 = searchsortedlast(xnzind, i1, j0, m, Forward)

    # compute the number of non-zeros
    jrgn = j0:j1
    mr = length(jrgn)
    rind = Array(Ti, mr)
    rval = Array(Tv, mr)
    if mr > 0
        c = 0
        for j in jrgn
            c += 1
            rind[c] = convert(Ti, xnzind[j] - i0 + 1)
            rval[c] = xnzval[j]
        end
    end
    SparseVector(length(I), rind, rval)
end

getindex{Tv,Ti}(x::AbstractSparseVector{Tv,Ti}, I::AbstractVector{Bool}) = x[find(I)]
getindex{Tv,Ti}(x::AbstractSparseVector{Tv,Ti}, I::AbstractArray{Bool}) = x[find(I)]
@inline function getindex{Tv,Ti}(x::AbstractSparseVector{Tv,Ti}, I::AbstractVector)
    # SparseMatrixCSC has a nicely optimized routine for this; punt
    S = SparseMatrixCSC(x.n, 1, [1,length(x.nzind)+1], x.nzind, x.nzval)
    S[I, 1]
end

function getindex{Tv,Ti}(x::AbstractSparseVector{Tv,Ti}, I::AbstractArray)
    # punt to SparseMatrixCSC
    S = SparseMatrixCSC(x.n, 1, [1,length(x.nzind)+1], x.nzind, x.nzval)
    S[I]
end

getindex(x::AbstractSparseVector, ::Colon) = copy(x)

### show and friends

function showarray(io::IO, x::AbstractSparseVector;
                   header::Bool=true, limit::Bool=Base._limit_output,
                   rows = Base.tty_size()[1], repr=false)

    n = length(x)
    nzind = nonzeroinds(x)
    nzval = nonzeros(x)
    xnnz = length(nzind)

    if header
        println(io, summary(x))
    end
    half_screen_rows = limit ? div(rows - 8, 2) : typemax(Int)
    pad = ndigits(n)
    sep = "\n\t"
    for k = 1:length(nzind)
        if k < half_screen_rows || k > xnnz - half_screen_rows
            print(io, "  ", '[', rpad(nzind[k], pad), "]  =  ")
            showcompact(io, nzval[k])
            println(io)
        elseif k == half_screen_rows
            println(io, "   ", " "^pad, "   \u22ee")
        end
    end
end

function summary(x::AbstractSparseVector)
    string("Sparse vector of length ", length(x), ", with ", length(nonzeros(x)),
           " ",  eltype(x), " nonzero entries:")
end

show(io::IO, x::AbstractSparseVector) = showarray(io, x)
writemime(io::IO, ::MIME"text/plain", x::AbstractSparseVector) = Base.with_output_limit(()->show(io, x))

### Conversion to matrix

function convert{TvD,TiD,Tv,Ti}(::Type{SparseMatrixCSC{TvD,TiD}}, x::AbstractSparseVector{Tv,Ti})
    n = length(x)
    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    m = length(xnzind)
    colptr = TiD[1, m+1]
    # Note that this *cannot* share data like normal array conversions, since
    # modifying one would put the other in an inconsistent state
    rowval = collect(TiD, xnzind)
    nzval = collect(TvD, xnzval)
    SparseMatrixCSC(n, 1, colptr, rowval, nzval)
end

convert{TvD,Tv,Ti}(::Type{SparseMatrixCSC{TvD}}, x::AbstractSparseVector{Tv,Ti}) =
    convert(SparseMatrixCSC{TvD,Ti}, x)

convert{Tv,Ti}(::Type{SparseMatrixCSC}, x::AbstractSparseVector{Tv,Ti}) =
    convert(SparseMatrixCSC{Tv,Ti}, x)


### Array manipulation

function full{Tv}(x::AbstractSparseVector{Tv})
    n = length(x)
    n == 0 && return Array(Tv, 0)
    nzind = nonzeroinds(x)
    nzval = nonzeros(x)
    r = zeros(Tv, n)
    for i = 1:length(nzind)
        r[nzind[i]] = nzval[i]
    end
    return r
end

vec(x::AbstractSparseVector) = x
copy(x::AbstractSparseVector) =
    SparseVector(length(x), copy(nonzeroinds(x)), copy(nonzeros(x)))

function reinterpret{T,Tv}(::Type{T}, x::AbstractSparseVector{Tv})
    sizeof(T) == sizeof(Tv) ||
        throw(ArgumentError("reinterpret of sparse vectors only supports element types of the same size."))
    SparseVector(length(x), copy(nonzeroinds(x)), reinterpret(T, nonzeros(x)))
end

float{Tv<:AbstractFloat}(x::AbstractSparseVector{Tv}) = x
float(x::AbstractSparseVector) =
    SparseVector(length(x), copy(nonzeroinds(x)), float(nonzeros(x)))

complex{Tv<:Complex}(x::AbstractSparseVector{Tv}) = x
complex(x::AbstractSparseVector) =
    SparseVector(length(x), copy(nonzeroinds(x)), complex(nonzeros(x)))


### Concatenation

function hcat{Tv,Ti}(X::AbstractSparseVector{Tv,Ti}...)
    # check sizes
    n = length(X)
    m = length(X[1])
    tnnz = nnz(X[1])
    for j = 2:n
        length(X[j]) == m ||
            throw(DimensionMismatch("Inconsistent column lengths."))
        tnnz += nnz(X[j])
    end

    # construction
    colptr = Array(Ti, n+1)
    nzrow = Array(Ti, tnnz)
    nzval = Array(Tv, tnnz)
    roff = 1
    @inbounds for j = 1:n
        xj = X[j]
        xnzind = nonzeroinds(xj)
        xnzval = nonzeros(xj)
        colptr[j] = roff
        copy!(nzrow, roff, xnzind)
        copy!(nzval, roff, xnzval)
        roff += length(xnzind)
    end
    colptr[n+1] = roff
    SparseMatrixCSC{Tv,Ti}(m, n, colptr, nzrow, nzval)
end

function vcat{Tv,Ti}(X::AbstractSparseVector{Tv,Ti}...)
    # check sizes
    n = length(X)
    tnnz = 0
    for j = 1:n
        tnnz += nnz(X[j])
    end

    # construction
    rnzind = Array(Ti, tnnz)
    rnzval = Array(Tv, tnnz)
    ir = 0
    len = 0
    @inbounds for j = 1:n
        xj = X[j]
        xnzind = nonzeroinds(xj)
        xnzval = nonzeros(xj)
        xnnz = length(xnzind)
        for i = 1:xnnz
            rnzind[ir + i] = xnzind[i] + len
        end
        copy!(rnzval, ir+1, xnzval)
        ir += xnnz
        len += length(xj)
    end
    SparseVector(len, rnzind, rnzval)
end

### math functions

### Unary Map

# zero-preserving functions (z->z, nz->nz)
for op in [:-, :abs, :abs2, :conj]
    @eval begin
        $(op)(x::AbstractSparseVector) =
            SparseVector(length(x), copy(nonzeroinds(x)), $(op)(nonzeros(x)))
    end
end

# functions f, such that
#   f(x) can be zero or non-zero when x != 0
#   f(x) = 0 when x == 0
#
macro unarymap_nz2z_z2z(op, TF)
    esc(quote
        function $(op){Tv<:$(TF),Ti<:Integer}(x::AbstractSparseVector{Tv,Ti})
            R = typeof($(op)(zero(Tv)))
            xnzind = nonzeroinds(x)
            xnzval = nonzeros(x)
            m = length(xnzind)

            ynzind = Array(Ti, m)
            ynzval = Array(R, m)
            ir = 0
            @inbounds for j = 1:m
                i = xnzind[j]
                v = $(op)(xnzval[j])
                if v != zero(v)
                    ir += 1
                    ynzind[ir] = i
                    ynzval[ir] = v
                end
            end
            resize!(ynzind, ir)
            resize!(ynzval, ir)
            SparseVector(length(x), ynzind, ynzval)
        end
    end)
end

real{T<:Real}(x::AbstractSparseVector{T}) = x
@unarymap_nz2z_z2z real Complex

imag{Tv<:Real,Ti<:Integer}(x::AbstractSparseVector{Tv,Ti}) = SparseVector(length(x), Ti[], Tv[])
@unarymap_nz2z_z2z imag Complex

for op in [:floor, :ceil, :trunc, :round]
    @eval @unarymap_nz2z_z2z $(op) Real
end

for op in [:log1p, :expm1,
           :sin, :tan, :sinpi, :sind, :tand,
           :asin, :atan, :asind, :atand,
           :sinh, :tanh, :asinh, :atanh]
    @eval @unarymap_nz2z_z2z $(op) Number
end

# function that does not preserve zeros

macro unarymap_z2nz(op, TF)
    esc(quote
        function $(op){Tv<:$(TF),Ti<:Integer}(x::AbstractSparseVector{Tv,Ti})
            v0 = $(op)(zero(Tv))
            R = typeof(v0)
            xnzind = nonzeroinds(x)
            xnzval = nonzeros(x)
            n = length(x)
            m = length(xnzind)
            y = fill(v0, n)
            @inbounds for j = 1:m
                y[xnzind[j]] = $(op)(xnzval[j])
            end
            y
        end
    end)
end

for op in [:exp, :exp2, :exp10, :log, :log2, :log10,
           :cos, :csc, :cot, :sec, :cospi,
           :cosd, :cscd, :cotd, :secd,
           :acos, :acot, :acosd, :acotd,
           :cosh, :csch, :coth, :sech,
           :acsch, :asech]
    @eval @unarymap_z2nz $(op) Number
end


### Binary Map

# mode:
# 0: f(nz, nz) -> nz, f(z, nz) -> z, f(nz, z) ->  z
# 1: f(nz, nz) -> z/nz, f(z, nz) -> nz, f(nz, z) -> nz
# 2: f(nz, nz) -> z/nz, f(z, nz) -> z/nz, f(nz, z) -> z/nz

function _binarymap{Tx,Ty}(f::BinaryOp,
                           x::AbstractSparseVector{Tx},
                           y::AbstractSparseVector{Ty},
                           mode::Int)

    0 <= mode <= 2 || throw(ArgumentError("Incorrect mode $mode."))
    R = typeof(f(zero(Tx), zero(Ty)))
    n = length(x)
    length(y) == n || throw(DimensionMismatch())

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    ynzind = nonzeroinds(y)
    ynzval = nonzeros(y)
    mx = length(xnzind)
    my = length(ynzind)
    (mx == 0 || my == 0) && return SparseVector(R, 0)
    cap = (mode == 0 ? min(mx, my) : mx + my)::Int

    rind = Array(Int, cap)
    rval = Array(R, cap)
    ir = 0
    ix = 1
    iy = 1

    ir = (
        mode == 0 ? _binarymap_mode_0!(f, mx, my,
            xnzind, xnzval, ynzind, ynzval, rind, rval) :
        mode == 1 ? _binarymap_mode_1!(f, mx, my,
            xnzind, xnzval, ynzind, ynzval, rind, rval) :
        _binarymap_mode_2!(f, mx, my,
            xnzind, xnzval, ynzind, ynzval, rind, rval)
    )::Int

    resize!(rind, ir)
    resize!(rval, ir)
    return SparseVector(n, rind, rval)
end

function _binarymap_mode_0!(f::BinaryOp, mx::Int, my::Int,
                            xnzind, xnzval, ynzind, ynzval, rind, rval)
    # f(nz, nz) -> nz, f(z, nz) -> z, f(nz, z) ->  z
    ir = 0; ix = 1; iy = 1
    @inbounds while ix <= mx && iy <= my
        jx = xnzind[ix]
        jy = ynzind[iy]
        if jx == jy
            v = f(xnzval[ix], ynzval[iy])
            ir += 1; rind[ir] = jx; rval[ir] = v
            ix += 1; iy += 1
        elseif jx < jy
            ix += 1
        else
            iy += 1
        end
    end
    return ir
end

function _binarymap_mode_1!{Tx,Ty}(f::BinaryOp, mx::Int, my::Int,
                                   xnzind, xnzval::AbstractVector{Tx},
                                   ynzind, ynzval::AbstractVector{Ty},
                                   rind, rval)
    # f(nz, nz) -> z/nz, f(z, nz) -> nz, f(nz, z) -> nz
    ir = 0; ix = 1; iy = 1
    @inbounds while ix <= mx && iy <= my
        jx = xnzind[ix]
        jy = ynzind[iy]
        if jx == jy
            v = f(xnzval[ix], ynzval[iy])
            if v != zero(v)
                ir += 1; rind[ir] = jx; rval[ir] = v
            end
            ix += 1; iy += 1
        elseif jx < jy
            v = f(xnzval[ix], zero(Ty))
            ir += 1; rind[ir] = jx; rval[ir] = v
            ix += 1
        else
            v = f(zero(Tx), ynzval[iy])
            ir += 1; rind[ir] = jy; rval[ir] = v
            iy += 1
        end
    end
    @inbounds while ix <= mx
        v = f(xnzval[ix], zero(Ty))
        ir += 1; rind[ir] = xnzind[ix]; rval[ir] = v
        ix += 1
    end
    @inbounds while iy <= my
        v = f(zero(Tx), ynzval[iy])
        ir += 1; rind[ir] = ynzind[iy]; rval[ir] = v
        iy += 1
    end
    return ir
end

function _binarymap_mode_2!{Tx,Ty}(f::BinaryOp, mx::Int, my::Int,
                                   xnzind, xnzval::AbstractVector{Tx},
                                   ynzind, ynzval::AbstractVector{Ty},
                                   rind, rval)
    # f(nz, nz) -> z/nz, f(z, nz) -> z/nz, f(nz, z) -> z/nz
    ir = 0; ix = 1; iy = 1
    @inbounds while ix <= mx && iy <= my
        jx = xnzind[ix]
        jy = ynzind[iy]
        if jx == jy
            v = f(xnzval[ix], ynzval[iy])
            if v != zero(v)
                ir += 1; rind[ir] = jx; rval[ir] = v
            end
            ix += 1; iy += 1
        elseif jx < jy
            v = f(xnzval[ix], zero(Ty))
            if v != zero(v)
                ir += 1; rind[ir] = jx; rval[ir] = v
            end
            ix += 1
        else
            v = f(zero(Tx), ynzval[iy])
            if v != zero(v)
                ir += 1; rind[ir] = jy; rval[ir] = v
            end
            iy += 1
        end
    end
    @inbounds while ix <= mx
        v = f(xnzval[ix], zero(Ty))
        if v != zero(v)
            ir += 1; rind[ir] = xnzind[ix]; rval[ir] = v
        end
        ix += 1
    end
    @inbounds while iy <= my
        v = f(zero(Tx), ynzval[iy])
        if v != zero(v)
            ir += 1; rind[ir] = ynzind[iy]; rval[ir] = v
        end
        iy += 1
    end
    return ir
end

function _binarymap{Tx,Ty}(f::BinaryOp,
                           x::AbstractVector{Tx},
                           y::AbstractSparseVector{Ty},
                           mode::Int)

    0 <= mode <= 2 || throw(ArgumentError("Incorrect mode $mode."))
    R = typeof(f(zero(Tx), zero(Ty)))
    n = length(x)
    length(y) == n || throw(DimensionMismatch())

    ynzind = nonzeroinds(y)
    ynzval = nonzeros(y)
    m = length(ynzind)

    dst = Array(R, n)
    if mode == 0
        ii = 1
        @inbounds for i = 1:m
            j = ynzind[i]
            while ii < j
                dst[ii] = zero(R); ii += 1
            end
            dst[j] = f(x[j], ynzval[i]); ii += 1
        end
        @inbounds while ii <= n
            dst[ii] = zero(R); ii += 1
        end
    else # mode >= 1
        ii = 1
        @inbounds for i = 1:m
            j = ynzind[i]
            while ii < j
                dst[ii] = f(x[ii], zero(Ty)); ii += 1
            end
            dst[j] = f(x[j], ynzval[i]); ii += 1
        end
        @inbounds while ii <= n
            dst[ii] = f(x[ii], zero(Ty)); ii += 1
        end
    end
    return dst
end

function _binarymap{Tx,Ty}(f::BinaryOp,
                           x::AbstractSparseVector{Tx},
                           y::AbstractVector{Ty},
                           mode::Int)

    0 <= mode <= 2 || throw(ArgumentError("Incorrect mode $mode."))
    R = typeof(f(zero(Tx), zero(Ty)))
    n = length(x)
    length(y) == n || throw(DimensionMismatch())

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    m = length(xnzind)

    dst = Array(R, n)
    if mode == 0
        ii = 1
        @inbounds for i = 1:m
            j = xnzind[i]
            while ii < j
                dst[ii] = zero(R); ii += 1
            end
            dst[j] = f(xnzval[i], y[j]); ii += 1
        end
        @inbounds while ii <= n
            dst[ii] = zero(R); ii += 1
        end
    else # mode >= 1
        ii = 1
        @inbounds for i = 1:m
            j = xnzind[i]
            while ii < j
                dst[ii] = f(zero(Tx), y[ii]); ii += 1
            end
            dst[j] = f(xnzval[i], y[j]); ii += 1
        end
        @inbounds while ii <= n
            dst[ii] = f(zero(Tx), y[ii]); ii += 1
        end
    end
    return dst
end


### Binary arithmetics: +, -, *

for (vop, fun, mode) in [(:_vadd, :AddFun, 1),
                         (:_vsub, :SubFun, 1),
                         (:_vmul, :MulFun, 0)]
    @eval begin
        $(vop)(x::AbstractSparseVector, y::AbstractSparseVector) = _binarymap($(fun)(), x, y, $mode)
        $(vop)(x::StridedVector, y::AbstractSparseVector) = _binarymap($(fun)(), x, y, $mode)
        $(vop)(x::AbstractSparseVector, y::StridedVector) = _binarymap($(fun)(), x, y, $mode)
    end
end

# to workaround the ambiguities with BitVector
.*(x::BitVector, y::AbstractSparseVector{Bool}) = _vmul(x, y)
.*(x::AbstractSparseVector{Bool}, y::BitVector) = _vmul(x, y)

# definition of operators

for (op, vop) in [(:+, :_vadd), (:(.+), :_vadd),
                  (:-, :_vsub), (:(.-), :_vsub),
                  (:.*, :_vmul)]
    @eval begin
        $(op)(x::AbstractSparseVector, y::AbstractSparseVector) = $(vop)(x, y)
        $(op)(x::StridedVector, y::AbstractSparseVector) = $(vop)(x, y)
        $(op)(x::AbstractSparseVector, y::StridedVector) = $(vop)(x, y)
    end
end

# definition of other binary functions

for (op, fun, TF, mode) in [(:max, :MaxFun, :Real, 2),
                            (:min, :MinFun, :Real, 2),
                            (:complex, :ComplexFun, :Real, 1)]
    @eval begin
        $(op){Tx<:$(TF),Ty<:$(TF)}(x::AbstractSparseVector{Tx}, y::AbstractSparseVector{Ty}) =
            _binarymap($(fun)(), x, y, $mode)
        $(op){Tx<:$(TF),Ty<:$(TF)}(x::StridedVector{Tx}, y::AbstractSparseVector{Ty}) =
            _binarymap($(fun)(), x, y, $mode)
        $(op){Tx<:$(TF),Ty<:$(TF)}(x::AbstractSparseVector{Tx}, y::StridedVector{Ty}) =
            _binarymap($(fun)(), x, y, $mode)
    end
end

### Reduction

sum(x::AbstractSparseVector) = sum(nonzeros(x))
sumabs(x::AbstractSparseVector) = sumabs(nonzeros(x))
sumabs2(x::AbstractSparseVector) = sumabs2(nonzeros(x))

function maximum{T<:Real}(x::AbstractSparseVector{T})
    n = length(x)
    n > 0 || throw(ArgumentError("maximum over empty array is not allowed."))
    m = nnz(x)
    (m == 0 ? zero(T) :
     m == n ? maximum(nonzeros(x)) :
     max(zero(T), maximum(nonzeros(x))))::T
end

function minimum{T<:Real}(x::AbstractSparseVector{T})
    n = length(x)
    n > 0 || throw(ArgumentError("minimum over empty array is not allowed."))
    m = nnz(x)
    (m == 0 ? zero(T) :
     m == n ? minimum(nonzeros(x)) :
     min(zero(T), minimum(nonzeros(x))))::T
end

maxabs{T<:Number}(x::AbstractSparseVector{T}) = maxabs(nonzeros(x))
minabs{T<:Number}(x::AbstractSparseVector{T}) = nnz(x) < length(x) ? abs(zero(T)) : minabs(nonzeros(x))

vecnorm(x::AbstractSparseVector, p::Real=2) = vecnorm(nonzeros(x), p)

### linalg.jl

# Transpose
# (The only sparse matrix structure in base is CSC, so a one-row sparse matrix is worse than dense)
transpose(x::SparseVector) = _ct(IdFun(), x)
ctranspose(x::SparseVector) = _ct(ConjFun(), x)
function _ct{T}(f, x::SparseVector{T})
    isempty(x) && return Array(T, 1, 0)
    A = zeros(T, 1, length(x))
    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    for (i,v) in zip(xnzind, xnzval)
        @inbounds A[i] = f(v)
    end
    A
end

### BLAS Level-1

# axpy

function LinAlg.axpy!(a::Number, x::AbstractSparseVector, y::StridedVector)
    length(x) == length(y) || throw(DimensionMismatch())
    nzind = nonzeroinds(x)
    nzval = nonzeros(x)
    m = length(nzind)

    if a == one(a)
        for i = 1:m
            @inbounds ii = nzind[i]
            @inbounds v = nzval[i]
            y[ii] += v
        end
    elseif a == -one(a)
        for i = 1:m
            @inbounds ii = nzind[i]
            @inbounds v = nzval[i]
            y[ii] -= v
        end
    else
        for i = 1:m
            @inbounds ii = nzind[i]
            @inbounds v = nzval[i]
            y[ii] += a * v
        end
    end
    return y
end


# scale

scale!(x::AbstractSparseVector, a::Real) = (scale!(nonzeros(x), a); x)
scale!(x::AbstractSparseVector, a::Complex) = (scale!(nonzeros(x), a); x)
scale!(a::Real, x::AbstractSparseVector) = scale!(nonzeros(x), a)
scale!(a::Complex, x::AbstractSparseVector) = scale!(nonzeros(x), a)

scale(x::AbstractSparseVector, a::Real) =
    SparseVector(length(x), copy(nonzeroinds(x)), scale(nonzeros(x), a))
scale(x::AbstractSparseVector, a::Complex) =
    SparseVector(length(x), copy(nonzeroinds(x)), scale(nonzeros(x), a))

scale(a::Real, x::AbstractSparseVector) = scale(x, a)
scale(a::Complex, x::AbstractSparseVector) = scale(x, a)

*(x::AbstractSparseVector, a::Number) = scale(x, a)
*(a::Number, x::AbstractSparseVector) = scale(x, a)
.*(x::AbstractSparseVector, a::Number) = scale(x, a)
.*(a::Number, x::AbstractSparseVector) = scale(x, a)


# dot

function dot{Tx<:Number,Ty<:Number}(x::StridedVector{Tx}, y::AbstractSparseVector{Ty})
    n = length(x)
    length(y) == n || throw(DimensionMismatch())
    nzind = nonzeroinds(y)
    nzval = nonzeros(y)
    s = zero(Tx) * zero(Ty)
    for i = 1:length(nzind)
        s += conj(x[nzind[i]]) * nzval[i]
    end
    return s
end

function dot{Tx<:Number,Ty<:Number}(x::AbstractSparseVector{Tx}, y::AbstractVector{Ty})
    n = length(y)
    length(x) == n || throw(DimensionMismatch())
    nzind = nonzeroinds(x)
    nzval = nonzeros(x)
    s = zero(Tx) * zero(Ty)
    for i = 1:length(nzind)
        s += conj(nzval[i]) * y[nzind[i]]
    end
    return s
end

function _spdot(f::BinaryOp,
                xj::Int, xj_last::Int, xnzind, xnzval,
                yj::Int, yj_last::Int, ynzind, ynzval)
    # dot product between ranges of non-zeros,
    s = zero(eltype(xnzval)) * zero(eltype(ynzval))
    @inbounds while xj <= xj_last && yj <= yj_last
        ix = xnzind[xj]
        iy = ynzind[yj]
        if ix == iy
            s += f(xnzval[xj], ynzval[yj])
            xj += 1
            yj += 1
        elseif ix < iy
            xj += 1
        else
            yj += 1
        end
    end
    s
end

function dot{Tx<:Number,Ty<:Number}(x::AbstractSparseVector{Tx}, y::AbstractSparseVector{Ty})
    is(x, y) && return sumabs2(x)
    n = length(x)
    length(y) == n || throw(DimensionMismatch())

    xnzind = nonzeroinds(x)
    ynzind = nonzeroinds(y)
    xnzval = nonzeros(x)
    ynzval = nonzeros(y)

    _spdot(DotFun(),
           1, length(xnzind), xnzind, xnzval,
           1, length(ynzind), ynzind, ynzval)
end


### BLAS-2 / dense A * sparse x -> dense y

# A_mul_B

function *{Ta,Tx}(A::StridedMatrix{Ta}, x::AbstractSparseVector{Tx})
    m, n = size(A)
    length(x) == n || throw(DimensionMismatch())
    Ty = promote_type(Ta, Tx)
    y = Array(Ty, m)
    A_mul_B!(y, A, x)
end

A_mul_B!{Tx,Ty}(y::StridedVector{Ty}, A::StridedMatrix, x::AbstractSparseVector{Tx}) =
    A_mul_B!(one(Tx), A, x, zero(Ty), y)

function A_mul_B!(α::Number, A::StridedMatrix, x::AbstractSparseVector, β::Number, y::StridedVector)
    m, n = size(A)
    length(x) == n && length(y) == m || throw(DimensionMismatch())
    m == 0 && return y
    if β != one(β)
        β == zero(β) ? fill!(y, zero(eltype(y))) : scale!(y, β)
    end
    α == zero(α) && return y

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    @inbounds for i = 1:length(xnzind)
        v = xnzval[i]
        if v != zero(v)
            j = xnzind[i]
            αv = v * α
            for r = 1:m
                y[r] += A[r,j] * αv
            end
        end
    end
    return y
end

# At_mul_B

function At_mul_B{Ta,Tx}(A::StridedMatrix{Ta}, x::AbstractSparseVector{Tx})
    m, n = size(A)
    length(x) == m || throw(DimensionMismatch())
    Ty = promote_type(Ta, Tx)
    y = Array(Ty, n)
    At_mul_B!(y, A, x)
end

At_mul_B!{Tx,Ty}(y::StridedVector{Ty}, A::StridedMatrix, x::AbstractSparseVector{Tx}) =
    At_mul_B!(one(Tx), A, x, zero(Ty), y)

function At_mul_B!(α::Number, A::StridedMatrix, x::AbstractSparseVector, β::Number, y::StridedVector)
    m, n = size(A)
    length(x) == m && length(y) == n || throw(DimensionMismatch())
    n == 0 && return y
    if β != one(β)
        β == zero(β) ? fill!(y, zero(eltype(y))) : scale!(y, β)
    end
    α == zero(α) && return y

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    _nnz = length(xnzind)
    _nnz == 0 && return y

    s0 = zero(eltype(A)) * zero(eltype(x))
    @inbounds for j = 1:n
        s = zero(s0)
        for i = 1:_nnz
            s += A[xnzind[i], j] * xnzval[i]
        end
        y[j] += s * α
    end
    return y
end


### BLAS-2 / sparse A * sparse x -> dense y

function densemv(A::SparseMatrixCSC, x::AbstractSparseVector; trans::Char='N')
    xlen::Int
    ylen::Int
    m, n = size(A)
    if trans == 'N' || trans == 'n'
        xlen = n; ylen = m
    elseif trans == 'T' || trans == 't' || trans == 'C' || trans == 'c'
        xlen = m; ylen = n
    else
        throw(ArgumentError("Invalid trans character $trans"))
    end
    xlen == length(x) || throw(DimensionMismatch())
    T = promote_type(eltype(A), eltype(x))
    y = Array(T, ylen)
    if trans == 'N' || trans == 'N'
        A_mul_B!(y, A, x)
    elseif trans == 'T' || trans == 't'
        At_mul_B!(y, A, x)
    elseif trans == 'C' || trans == 'c'
        Ac_mul_B!(y, A, x)
    else
        throw(ArgumentError("Invalid trans character $trans"))
    end
    y
end

# A_mul_B

A_mul_B!{Tx,Ty}(y::StridedVector{Ty}, A::SparseMatrixCSC, x::AbstractSparseVector{Tx}) =
    A_mul_B!(one(Tx), A, x, zero(Ty), y)

function A_mul_B!(α::Number, A::SparseMatrixCSC, x::AbstractSparseVector, β::Number, y::StridedVector)
    m, n = size(A)
    length(x) == n && length(y) == m || throw(DimensionMismatch())
    m == 0 && return y
    if β != one(β)
        β == zero(β) ? fill!(y, zero(eltype(y))) : scale!(y, β)
    end
    α == zero(α) && return y

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    Acolptr = A.colptr
    Arowval = A.rowval
    Anzval = A.nzval

    @inbounds for i = 1:length(xnzind)
        v = xnzval[i]
        if v != zero(v)
            αv = v * α
            j = xnzind[i]
            for r = A.colptr[j]:(Acolptr[j+1]-1)
                y[Arowval[r]] += Anzval[r] * αv
            end
        end
    end
    return y
end

# At_mul_B

At_mul_B!{Tx,Ty}(y::StridedVector{Ty}, A::SparseMatrixCSC, x::AbstractSparseVector{Tx}) =
    At_mul_B!(one(Tx), A, x, zero(Ty), y)

At_mul_B!{Tx,Ty}(α::Number, A::SparseMatrixCSC, x::AbstractSparseVector{Tx}, β::Number, y::StridedVector{Ty}) =
    _At_or_Ac_mul_B!(MulFun(), α, A, x, β, y)

Ac_mul_B!{Tx,Ty}(y::StridedVector{Ty}, A::SparseMatrixCSC, x::AbstractSparseVector{Tx}) =
    Ac_mul_B!(one(Tx), A, x, zero(Ty), y)

Ac_mul_B!{Tx,Ty}(α::Number, A::SparseMatrixCSC, x::AbstractSparseVector{Tx}, β::Number, y::StridedVector{Ty}) =
    _At_or_Ac_mul_B!(DotFun(), α, A, x, β, y)

function _At_or_Ac_mul_B!{Tx,Ty}(tfun::BinaryOp,
                                 α::Number, A::SparseMatrixCSC, x::AbstractSparseVector{Tx},
                                 β::Number, y::StridedVector{Ty})
    m, n = size(A)
    length(x) == m && length(y) == n || throw(DimensionMismatch())
    n == 0 && return y
    if β != one(β)
        β == zero(β) ? fill!(y, zero(eltype(y))) : scale!(y, β)
    end
    α == zero(α) && return y

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    Acolptr = A.colptr
    Arowval = A.rowval
    Anzval = A.nzval
    mx = length(xnzind)

    for j = 1:n
        # s <- dot(A[:,j], x)
        s = _spdot(tfun, Acolptr[j], Acolptr[j+1]-1, Arowval, Anzval,
                   1, mx, xnzind, xnzval)
        @inbounds y[j] += s * α
    end
    return y
end


### BLAS-2 / sparse A * sparse x -> dense y

function *(A::SparseMatrixCSC, x::AbstractSparseVector)
    y = densemv(A, x)
    initcap = min(nnz(A), size(A,1))
    _dense2sparsevec(y, initcap)
end

At_mul_B(A::SparseMatrixCSC, x::AbstractSparseVector) =
    _At_or_Ac_mul_B(MulFun(), A, x)

Ac_mul_B(A::SparseMatrixCSC, x::AbstractSparseVector) =
    _At_or_Ac_mul_B(DotFun(), A, x)

function _At_or_Ac_mul_B{TvA,TiA,TvX,TiX}(tfun::BinaryOp, A::SparseMatrixCSC{TvA,TiA}, x::AbstractSparseVector{TvX,TiX})
    m, n = size(A)
    length(x) == m || throw(DimensionMismatch())
    Tv = promote_type(TvA, TvX)
    Ti = promote_type(TiA, TiX)

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    Acolptr = A.colptr
    Arowval = A.rowval
    Anzval = A.nzval
    mx = length(xnzind)

    ynzind = Array(Ti, n)
    ynzval = Array(Tv, n)

    jr = 0
    for j = 1:n
        s = _spdot(tfun, Acolptr[j], Acolptr[j+1]-1, Arowval, Anzval,
                   1, mx, xnzind, xnzval)
        if s != zero(s)
            jr += 1
            ynzind[jr] = j
            ynzval[jr] = s
        end
    end
    if jr < n
        resize!(ynzind, jr)
        resize!(ynzval, jr)
    end
    SparseVector(n, ynzind, ynzval)
end
