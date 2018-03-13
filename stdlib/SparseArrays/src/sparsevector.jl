# This file is a part of Julia. License is MIT: https://julialang.org/license

### Common definitions

import Base: sort, findall
import LinearAlgebra: promote_to_array_type, promote_to_arrays_

### The SparseVector

### Types

"""
    SparseVector{Tv,Ti<:Integer} <: AbstractSparseVector{Tv,Ti}

Vector type for storing sparse vectors.
"""
struct SparseVector{Tv,Ti<:Integer} <: AbstractSparseVector{Tv,Ti}
    n::Int              # Length of the sparse vector
    nzind::Vector{Ti}   # Indices of stored values
    nzval::Vector{Tv}   # Stored values, typically nonzeros

    function SparseVector{Tv,Ti}(n::Integer, nzind::Vector{Ti}, nzval::Vector{Tv}) where {Tv,Ti<:Integer}
        n >= 0 || throw(ArgumentError("The number of elements must be non-negative."))
        length(nzind) == length(nzval) ||
            throw(ArgumentError("index and value vectors must be the same length"))
        new(convert(Int, n), nzind, nzval)
    end
end

SparseVector(n::Integer, nzind::Vector{Ti}, nzval::Vector{Tv}) where {Tv,Ti} =
    SparseVector{Tv,Ti}(n, nzind, nzval)

# Define an alias for a view of a whole column of a SparseMatrixCSC. Many methods can be written for the
# union of such a view and a SparseVector so we define an alias for such a union as well
const SparseColumnView{T}  = SubArray{T,1,<:SparseMatrixCSC,Tuple{Base.Slice{Base.OneTo{Int}},Int},false}
const SparseVectorUnion{T} = Union{SparseVector{T}, SparseColumnView{T}}

### Basic properties

length(x::SparseVector)   = x.n
size(x::SparseVector)     = (x.n,)
nnz(x::SparseVector)      = length(x.nzval)
count(f, x::SparseVector) = count(f, x.nzval) + f(zero(eltype(x)))*(length(x) - nnz(x))

nonzeros(x::SparseVector) = x.nzval
function nonzeros(x::SparseColumnView)
    rowidx, colidx = parentindices(x)
    A = parent(x)
    @inbounds y = view(A.nzval, nzrange(A, colidx))
    return y
end

nonzeroinds(x::SparseVector) = x.nzind
function nonzeroinds(x::SparseColumnView)
    rowidx, colidx = parentindices(x)
    A = parent(x)
    @inbounds y = view(A.rowval, nzrange(A, colidx))
    return y
end


## similar
#
# parent method for similar that preserves stored-entry structure (for when new and old dims match)
_sparsesimilar(S::SparseVector, ::Type{TvNew}, ::Type{TiNew}) where {TvNew,TiNew} =
    SparseVector(S.n, copyto!(similar(S.nzind, TiNew), S.nzind), similar(S.nzval, TvNew))
# parent method for similar that preserves nothing (for when old and new dims differ, and new is 1d)
_sparsesimilar(S::SparseVector, ::Type{TvNew}, ::Type{TiNew}, dims::Dims{1}) where {TvNew,TiNew} =
    SparseVector(dims..., similar(S.nzind, TiNew, 0), similar(S.nzval, TvNew, 0))
# parent method for similar that preserves storage space (for old and new dims differ, and new is 2d)
_sparsesimilar(S::SparseVector, ::Type{TvNew}, ::Type{TiNew}, dims::Dims{2}) where {TvNew,TiNew} =
    SparseMatrixCSC(dims..., fill(one(TiNew), last(dims)+1), similar(S.nzind, TiNew), similar(S.nzval, TvNew))
# The following methods hook into the AbstractArray similar hierarchy. The first method
# covers similar(A[, Tv]) calls, which preserve stored-entry structure, and the latter
# methods cover similar(A[, Tv], shape...) calls, which preserve nothing if the dims
# specify a SparseVector result and storage space if the dims specify a SparseMatrixCSC result.
similar(S::SparseVector{<:Any,Ti}, ::Type{TvNew}) where {Ti,TvNew} =
    _sparsesimilar(S, TvNew, Ti)
similar(S::SparseVector{<:Any,Ti}, ::Type{TvNew}, dims::Union{Dims{1},Dims{2}}) where {Ti,TvNew} =
    _sparsesimilar(S, TvNew, Ti, dims)
# The following methods cover similar(A, Tv, Ti[, shape...]) calls, which specify the
# result's index type in addition to its entry type, and aren't covered by the hooks above.
# The calls without shape again preserve stored-entry structure, whereas those with
# one-dimensional shape preserve nothing, and those with two-dimensional shape
# preserve storage space.
similar(S::SparseVector, ::Type{TvNew}, ::Type{TiNew}) where{TvNew,TiNew} =
    _sparsesimilar(S, TvNew, TiNew)
similar(S::SparseVector, ::Type{TvNew}, ::Type{TiNew}, dims::Union{Dims{1},Dims{2}}) where {TvNew,TiNew} =
    _sparsesimilar(S, TvNew, TiNew, dims)
similar(S::SparseVector, ::Type{TvNew}, ::Type{TiNew}, m::Integer) where {TvNew,TiNew} =
    _sparsesimilar(S, TvNew, TiNew, (m,))
similar(S::SparseVector, ::Type{TvNew}, ::Type{TiNew}, m::Integer, n::Integer) where {TvNew,TiNew} =
    _sparsesimilar(S, TvNew, TiNew, (m, n))

## Alias detection and prevention
using Base: dataids, unaliascopy
Base.dataids(S::SparseVector) = (dataids(S.nzind)..., dataids(S.nzval)...)
Base.unaliascopy(S::SparseVector) = typeof(S)(S.n, unaliascopy(S.nzind), unaliascopy(S.nzval))

### Construct empty sparse vector

spzeros(len::Integer) = spzeros(Float64, len)
spzeros(::Type{T}, len::Integer) where {T} = SparseVector(len, Int[], T[])
spzeros(::Type{Tv}, ::Type{Ti}, len::Integer) where {Tv,Ti<:Integer} = SparseVector(len, Ti[], Tv[])

LinearAlgebra.fillstored!(x::SparseVector, y) = (fill!(x.nzval, y); x)

### Construction from lists of indices and values

function _sparsevector!(I::Vector{<:Integer}, V::Vector, len::Integer)
    # pre-condition: no duplicate indices in I
    if !isempty(I)
        p = sortperm(I)
        permute!(I, p)
        permute!(V, p)
    end
    SparseVector(len, I, V)
end

function _sparsevector!(I::Vector{<:Integer}, V::Vector, len::Integer, combine::Function)
    if !isempty(I)
        p = sortperm(I)
        permute!(I, p)
        permute!(V, p)
        m = length(I)
        r = 1
        l = 1       # length of processed part
        i = I[r]    # row-index of current element

        # main loop
        while r < m
            r += 1
            i2 = I[r]
            if i2 == i  # accumulate r-th to the l-th entry
                V[l] = combine(V[l], V[r])
            else  # advance l, and move r-th to l-th
                pv = V[l]
                l += 1
                i = i2
                if l < r
                    I[l] = i; V[l] = V[r]
                end
            end
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
`+` if no `combine` argument is provided, unless the elements of `V` are Booleans
in which case `combine` defaults to `|`.

# Examples
```jldoctest
julia> II = [1, 3, 3, 5]; V = [0.1, 0.2, 0.3, 0.2];

julia> sparsevec(II, V)
5-element SparseVector{Float64,Int64} with 3 stored entries:
  [1]  =  0.1
  [3]  =  0.5
  [5]  =  0.2

julia> sparsevec(II, V, 8, -)
8-element SparseVector{Float64,Int64} with 3 stored entries:
  [1]  =  0.1
  [3]  =  -0.1
  [5]  =  0.2

julia> sparsevec([1, 3, 1, 2, 2], [true, true, false, false, false])
3-element SparseVector{Bool,Int64} with 3 stored entries:
  [1]  =  true
  [2]  =  false
  [3]  =  true
```
"""
function sparsevec(I::AbstractVector{<:Integer}, V::AbstractVector, combine::Function)
    length(I) == length(V) ||
        throw(ArgumentError("index and value vectors must be the same length"))
    len = 0
    for i in I
        i >= 1 || error("Index must be positive.")
        if i > len
            len = i
        end
    end
    _sparsevector!(Vector(I), Vector(V), len, combine)
end

function sparsevec(I::AbstractVector{<:Integer}, V::AbstractVector, len::Integer, combine::Function)
    length(I) == length(V) ||
        throw(ArgumentError("index and value vectors must be the same length"))
    for i in I
        1 <= i <= len || throw(ArgumentError("An index is out of bound."))
    end
    _sparsevector!(Vector(I), Vector(V), len, combine)
end

sparsevec(I::AbstractVector, V::Union{Number, AbstractVector}, args...) =
    sparsevec(Vector{Int}(I), V, args...)

sparsevec(I::AbstractVector, V::Union{Number, AbstractVector}) =
    sparsevec(I, V, +)

sparsevec(I::AbstractVector, V::Union{Number, AbstractVector}, len::Integer) =
    sparsevec(I, V, len, +)

sparsevec(I::AbstractVector, V::Union{Bool, AbstractVector{Bool}}) =
    sparsevec(I, V, |)

sparsevec(I::AbstractVector, V::Union{Bool, AbstractVector{Bool}}, len::Integer) =
    sparsevec(I, V, len, |)

sparsevec(I::AbstractVector, v::Number, combine::Function) =
    sparsevec(I, fill(v, length(I)), combine)

sparsevec(I::AbstractVector, v::Number, len::Integer, combine::Function) =
    sparsevec(I, fill(v, length(I)), len, combine)


### Construction from dictionary
"""
    sparsevec(d::Dict, [m])

Create a sparse vector of length `m` where the nonzero indices are keys from
the dictionary, and the nonzero values are the values from the dictionary.

# Examples
```jldoctest
julia> sparsevec(Dict(1 => 3, 2 => 2))
2-element SparseVector{Int64,Int64} with 2 stored entries:
  [1]  =  3
  [2]  =  2
```
"""
function sparsevec(dict::AbstractDict{Ti,Tv}) where {Tv,Ti<:Integer}
    m = length(dict)
    nzind = Vector{Ti}(undef, m)
    nzval = Vector{Tv}(undef, m)

    cnt = 0
    len = zero(Ti)
    for (k, v) in dict
        k >= 1 || throw(ArgumentError("index must be positive."))
        if k > len
            len = k
        end
        cnt += 1
        @inbounds nzind[cnt] = k
        @inbounds nzval[cnt] = v
    end
    resize!(nzind, cnt)
    resize!(nzval, cnt)
    _sparsevector!(nzind, nzval, len)
end

function sparsevec(dict::AbstractDict{Ti,Tv}, len::Integer) where {Tv,Ti<:Integer}
    m = length(dict)
    nzind = Vector{Ti}(undef, m)
    nzval = Vector{Tv}(undef, m)

    cnt = 0
    maxk = convert(Ti, len)
    for (k, v) in dict
        1 <= k <= maxk || throw(ArgumentError("an index (key) is out of bound."))
        cnt += 1
        @inbounds nzind[cnt] = k
        @inbounds nzval[cnt] = v
    end
    resize!(nzind, cnt)
    resize!(nzval, cnt)
    _sparsevector!(nzind, nzval, len)
end


### Element access

function setindex!(x::SparseVector{Tv,Ti}, v::Tv, i::Ti) where {Tv,Ti<:Integer}
    checkbounds(x, i)
    nzind = nonzeroinds(x)
    nzval = nonzeros(x)

    m = length(nzind)
    k = searchsortedfirst(nzind, i)
    if 1 <= k <= m && nzind[k] == i  # i found
        nzval[k] = v
    else  # i not found
        if v != 0
            insert!(nzind, k, i)
            insert!(nzval, k, v)
        end
    end
    x
end

setindex!(x::SparseVector{Tv,Ti}, v, i::Integer) where {Tv,Ti<:Integer} =
    setindex!(x, convert(Tv, v), convert(Ti, i))


### dropstored!
"""
    dropstored!(x::SparseVector, i::Integer)

Drop entry `x[i]` from `x` if `x[i]` is stored and otherwise do nothing.

# Examples
```jldoctest
julia> x = sparsevec([1, 3], [1.0, 2.0])
3-element SparseVector{Float64,Int64} with 2 stored entries:
  [1]  =  1.0
  [3]  =  2.0

julia> Base.SparseArrays.dropstored!(x, 3)
3-element SparseVector{Float64,Int64} with 1 stored entry:
  [1]  =  1.0

julia> Base.SparseArrays.dropstored!(x, 2)
3-element SparseVector{Float64,Int64} with 1 stored entry:
  [1]  =  1.0
```
"""
function dropstored!(x::SparseVector, i::Integer)
    if !(1 <= i <= x.n)
        throw(BoundsError(x, i))
    end
    searchk = searchsortedfirst(x.nzind, i)
    if searchk <= length(x.nzind) && x.nzind[searchk] == i
        # Entry x[i] is stored. Drop and return.
        deleteat!(x.nzind, searchk)
        deleteat!(x.nzval, searchk)
    end
    return x
end
# TODO: Implement linear collection indexing methods for dropstored! ?
# TODO: Implement logical indexing methods for dropstored! ?


### Conversion

# convert SparseMatrixCSC to SparseVector
function SparseVector{Tv,Ti}(s::SparseMatrixCSC{Tv,Ti}) where {Tv,Ti<:Integer}
    size(s, 2) == 1 || throw(ArgumentError("The input argument must have a single-column."))
    SparseVector(s.m, s.rowval, s.nzval)
end

SparseVector{Tv}(s::SparseMatrixCSC{Tv,Ti}) where {Tv,Ti} = SparseVector{Tv,Ti}(s)

SparseVector(s::SparseMatrixCSC{Tv,Ti}) where {Tv,Ti} = SparseVector{Tv,Ti}(s)

# convert Vector to SparseVector

"""
    sparsevec(A)

Convert a vector `A` into a sparse vector of length `m`.

# Examples
```jldoctest
julia> sparsevec([1.0, 2.0, 0.0, 0.0, 3.0, 0.0])
6-element SparseVector{Float64,Int64} with 3 stored entries:
  [1]  =  1.0
  [2]  =  2.0
  [5]  =  3.0
```
"""
sparsevec(a::AbstractVector{T}) where {T} = SparseVector{T, Int}(a)
sparsevec(a::AbstractArray) = sparsevec(vec(a))
sparsevec(a::AbstractSparseArray) = vec(a)
sparsevec(a::AbstractSparseVector) = vec(a)
sparse(a::AbstractVector) = sparsevec(a)

function _dense2sparsevec(s::AbstractArray{Tv}, initcap::Ti) where {Tv,Ti}
    # pre-condition: initcap > 0; the initcap determines the index type
    n = length(s)
    cap = initcap
    nzind = Vector{Ti}(undef, cap)
    nzval = Vector{Tv}(undef, cap)
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

SparseVector{Tv,Ti}(s::AbstractVector{Tv}) where {Tv,Ti} =
    _dense2sparsevec(s, convert(Ti, max(8, div(length(s), 8))))

SparseVector{Tv}(s::AbstractVector{Tv}) where {Tv} = SparseVector{Tv,Int}(s)

SparseVector(s::AbstractVector{Tv}) where {Tv} = SparseVector{Tv,Int}(s)


# convert between different types of SparseVector
SparseVector{Tv}(s::SparseVector{Tv}) where {Tv} = s
SparseVector{Tv,Ti}(s::SparseVector{Tv,Ti}) where {Tv,Ti} = s
SparseVector{Tv,Ti}(s::SparseVector) where {Tv,Ti} =
    SparseVector{Tv,Ti}(s.n, convert(Vector{Ti}, s.nzind), convert(Vector{Tv}, s.nzval))

SparseVector{Tv}(s::SparseVector{<:Any,Ti}) where {Tv,Ti} =
    SparseVector{Tv,Ti}(s.n, s.nzind, convert(Vector{Tv}, s.nzval))

convert(T::Type{<:SparseVector}, m::AbstractVector) = m isa T ? m : T(m)

convert(T::Type{<:SparseVector}, m::SparseMatrixCSC) = T(m)
convert(T::Type{<:SparseMatrixCSC}, v::SparseVector) = T(v)

### copying
function prep_sparsevec_copy_dest!(A::SparseVector, lB, nnzB)
    lA = length(A)
    lA >= lB || throw(BoundsError())
    # If the two vectors have the same length then all the elements in A will be overwritten.
    if length(A) == lB
        resize!(A.nzval, nnzB)
        resize!(A.nzind, nnzB)
    else
        nnzA = nnz(A)

        lastmodindA = searchsortedlast(A.nzind, lB)
        if lastmodindA >= nnzB
            # A will have fewer non-zero elements; unmodified elements are kept at the end.
            deleteat!(A.nzind, nnzB+1:lastmodindA)
            deleteat!(A.nzval, nnzB+1:lastmodindA)
        else
            # A will have more non-zero elements; unmodified elements are kept at the end.
            resize!(A.nzind, nnzB + nnzA - lastmodindA)
            resize!(A.nzval, nnzB + nnzA - lastmodindA)
            copyto!(A.nzind, nnzB+1, A.nzind, lastmodindA+1, nnzA-lastmodindA)
            copyto!(A.nzval, nnzB+1, A.nzval, lastmodindA+1, nnzA-lastmodindA)
        end
    end
end

function copyto!(A::SparseVector, B::SparseVector)
    prep_sparsevec_copy_dest!(A, length(B), nnz(B))
    copyto!(A.nzind, B.nzind)
    copyto!(A.nzval, B.nzval)
    return A
end

function copyto!(A::SparseVector, B::SparseMatrixCSC)
    prep_sparsevec_copy_dest!(A, length(B), nnz(B))

    ptr = 1
    @assert length(A.nzind) >= length(B.rowval)
    maximum(B.colptr)-1 <= length(B.rowval) || throw(BoundsError())
    @inbounds for col=1:length(B.colptr)-1
        offsetA = (col - 1) * B.m
        while ptr <= B.colptr[col+1]-1
            A.nzind[ptr] = B.rowval[ptr] + offsetA
            ptr += 1
        end
    end
    copyto!(A.nzval, B.nzval)
    return A
end

copyto!(A::SparseMatrixCSC, B::SparseVector{TvB,TiB}) where {TvB,TiB} =
    copyto!(A, SparseMatrixCSC{TvB,TiB}(B.n, 1, TiB[1, length(B.nzind)+1], B.nzind, B.nzval))


### Rand Construction
sprand(n::Integer, p::AbstractFloat, rfn::Function, ::Type{T}) where {T} = sprand(GLOBAL_RNG, n, p, rfn, T)
function sprand(r::AbstractRNG, n::Integer, p::AbstractFloat, rfn::Function, ::Type{T}) where T
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

sprand(n::Integer, p::AbstractFloat) = sprand(GLOBAL_RNG, n, p, rand)

sprand(r::AbstractRNG, n::Integer, p::AbstractFloat) = sprand(r, n, p, rand)
sprand(r::AbstractRNG, ::Type{T}, n::Integer, p::AbstractFloat) where {T} = sprand(r, n, p, (r, i) -> rand(r, T, i))
sprand(r::AbstractRNG, ::Type{Bool}, n::Integer, p::AbstractFloat) = sprand(r, n, p, truebools)
sprand(::Type{T}, n::Integer, p::AbstractFloat) where {T} = sprand(GLOBAL_RNG, T, n, p)

sprandn(n::Integer, p::AbstractFloat) = sprand(GLOBAL_RNG, n, p, randn)
sprandn(r::AbstractRNG, n::Integer, p::AbstractFloat) = sprand(r, n, p, randn)

## Indexing into Matrices can return SparseVectors

# Column slices
function getindex(x::SparseMatrixCSC, ::Colon, j::Integer)
    checkbounds(x, :, j)
    r1 = convert(Int, x.colptr[j])
    r2 = convert(Int, x.colptr[j+1]) - 1
    SparseVector(x.m, x.rowval[r1:r2], x.nzval[r1:r2])
end

function getindex(x::SparseMatrixCSC, I::AbstractUnitRange, j::Integer)
    checkbounds(x, I, j)
    # Get the selected column
    c1 = convert(Int, x.colptr[j])
    c2 = convert(Int, x.colptr[j+1]) - 1
    # Restrict to the selected rows
    r1 = searchsortedfirst(x.rowval, first(I), c1, c2, Forward)
    r2 = searchsortedlast(x.rowval, last(I), c1, c2, Forward)
    SparseVector(length(I), [x.rowval[i] - first(I) + 1 for i = r1:r2], x.nzval[r1:r2])
end

# In the general case, we piggy back upon SparseMatrixCSC's optimized solution
@inline function getindex(A::SparseMatrixCSC, I::AbstractVector, J::Integer)
    M = A[I, [J]]
    SparseVector(M.m, M.rowval, M.nzval)
end

# Row slices
getindex(A::SparseMatrixCSC, i::Integer, ::Colon) = A[i, 1:end]
function Base.getindex(A::SparseMatrixCSC{Tv,Ti}, i::Integer, J::AbstractVector) where {Tv,Ti}
    checkbounds(A, i, J)
    nJ = length(J)
    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval

    nzinds = Vector{Ti}()
    nzvals = Vector{Tv}()

    # adapted from SparseMatrixCSC's sorted_bsearch_A
    ptrI = 1
    @inbounds for j = 1:nJ
        col = J[j]
        rowI = i
        ptrA = Int(colptrA[col])
        stopA = Int(colptrA[col+1]-1)
        if ptrA <= stopA
            if rowvalA[ptrA] <= rowI
                ptrA = searchsortedfirst(rowvalA, rowI, ptrA, stopA, Base.Order.Forward)
                if ptrA <= stopA && rowvalA[ptrA] == rowI
                    push!(nzinds, j)
                    push!(nzvals, nzvalA[ptrA])
                end
            end
            ptrI += 1
        end
    end
    return SparseVector(nJ, nzinds, nzvals)
end


# Logical and linear indexing into SparseMatrices
getindex(A::SparseMatrixCSC, I::AbstractVector{Bool}) = _logical_index(A, I) # Ambiguities
getindex(A::SparseMatrixCSC, I::AbstractArray{Bool}) = _logical_index(A, I)
function _logical_index(A::SparseMatrixCSC{Tv}, I::AbstractArray{Bool}) where Tv
    checkbounds(A, I)
    n = sum(I)
    nnzB = min(n, nnz(A))

    colptrA = A.colptr; rowvalA = A.rowval; nzvalA = A.nzval
    rowvalB = Vector{Int}(undef, nnzB)
    nzvalB = Vector{Tv}(undef, nnzB)
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
    if nnzB > (c-1)
        deleteat!(nzvalB, c:nnzB)
        deleteat!(rowvalB, c:nnzB)
    end
    SparseVector(n, rowvalB, nzvalB)
end

# TODO: further optimizations are available for ::Colon and other types of AbstractRange
getindex(A::SparseMatrixCSC, ::Colon) = A[1:end]

function getindex(A::SparseMatrixCSC{Tv}, I::AbstractUnitRange) where Tv
    checkbounds(A, I)
    szA = size(A)
    nA = szA[1]*szA[2]
    colptrA = A.colptr
    rowvalA = A.rowval
    nzvalA = A.nzval

    n = length(I)
    nnzB = min(n, nnz(A))
    rowvalB = Vector{Int}(undef, nnzB)
    nzvalB = Vector{Tv}(undef, nnzB)

    rowstart,colstart = Base._ind2sub(szA, first(I))
    rowend,colend = Base._ind2sub(szA, last(I))

    idxB = 1
    @inbounds for col in colstart:colend
        minrow = (col == colstart ? rowstart : 1)
        maxrow = (col == colend ? rowend : szA[1])
        for r in colptrA[col]:(colptrA[col+1]-1)
            rowA = rowvalA[r]
            if minrow <= rowA <= maxrow
                rowvalB[idxB] = Base._sub2ind(szA, rowA, col) - first(I) + 1
                nzvalB[idxB] = nzvalA[r]
                idxB += 1
            end
        end
    end
    if nnzB > (idxB-1)
        deleteat!(nzvalB, idxB:nnzB)
        deleteat!(rowvalB, idxB:nnzB)
    end
    SparseVector(n, rowvalB, nzvalB)
end

function getindex(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector) where {Tv,Ti}
    szA = size(A)
    nA = szA[1]*szA[2]
    colptrA = A.colptr
    rowvalA = A.rowval
    nzvalA = A.nzval

    n = length(I)
    nnzB = min(n, nnz(A))
    rowvalB = Vector{Ti}(undef, nnzB)
    nzvalB = Vector{Tv}(undef, nnzB)

    idxB = 1
    for i in 1:n
        ((I[i] < 1) | (I[i] > nA)) && throw(BoundsError(A, I))
        row,col = Base._ind2sub(szA, I[i])
        for r in colptrA[col]:(colptrA[col+1]-1)
            @inbounds if rowvalA[r] == row
                if idxB <= nnzB
                    rowvalB[idxB] = i
                    nzvalB[idxB] = nzvalA[r]
                    idxB += 1
                else # this can happen if there are repeated indices in I
                    push!(rowvalB, i)
                    push!(nzvalB, nzvalA[r])
                end
                break
            end
        end
    end
    if nnzB > (idxB-1)
        deleteat!(nzvalB, idxB:nnzB)
        deleteat!(rowvalB, idxB:nnzB)
    end
    SparseVector(n, rowvalB, nzvalB)
end

function findall(x::SparseVector)
    if !(eltype(x) <: Bool)
        Base.depwarn("In the future `findall(A)` will only work on boolean collections. Use `findall(x->x!=0, A)` instead.", :findall)
    end
    return findall(x->x!=0, x)
end

function findall(p::Function, x::SparseVector{<:Any,Ti}) where Ti
    if p(zero(eltype(x)))
        return invoke(findall, Tuple{Function, Any}, p, x)
    end
    numnz = nnz(x)
    I = Vector{Ti}(undef, numnz)

    nzind = x.nzind
    nzval = x.nzval

    count = 1
    @inbounds for i = 1 : numnz
        if p(nzval[i])
            I[count] = nzind[i]
            count += 1
        end
    end

    count -= 1
    if numnz != count
        deleteat!(I, (count+1):numnz)
    end

    return I
end
findall(p::Base.OccursIn, x::SparseVector{<:Any,Ti}) where {Ti} =
    invoke(findall, Tuple{Base.OccursIn, AbstractArray}, p, x)

function findnz(x::SparseVector{Tv,Ti}) where {Tv,Ti}
    numnz = nnz(x)

    I = Vector{Ti}(undef, numnz)
    V = Vector{Tv}(undef, numnz)

    nzind = x.nzind
    nzval = x.nzval

    @inbounds for i = 1 : numnz
        I[i] = nzind[i]
        V[i] = nzval[i]
    end

    return (I, V)
end

function _sparse_findnextnz(v::SparseVector, i::Integer)
    n = searchsortedfirst(v.nzind, i)
    if n > length(v.nzind)
        return nothing
    else
        return v.nzind[n]
    end
end

function _sparse_findprevnz(v::SparseVector, i::Integer)
    n = searchsortedlast(v.nzind, i)
    if iszero(n)
        return nothing
    else
        return v.nzind[n]
    end
end

### Generic functions operating on AbstractSparseVector

### getindex

function _spgetindex(m::Int, nzind::AbstractVector{Ti}, nzval::AbstractVector{Tv}, i::Integer) where {Tv,Ti}
    ii = searchsortedfirst(nzind, convert(Ti, i))
    (ii <= m && nzind[ii] == i) ? nzval[ii] : zero(Tv)
end

function getindex(x::AbstractSparseVector, i::Integer)
    checkbounds(x, i)
    _spgetindex(nnz(x), nonzeroinds(x), nonzeros(x), i)
end

function getindex(x::AbstractSparseVector{Tv,Ti}, I::AbstractUnitRange) where {Tv,Ti}
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
    rind = Vector{Ti}(undef, mr)
    rval = Vector{Tv}(undef, mr)
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

getindex(x::AbstractSparseVector, I::AbstractVector{Bool}) = x[findall(I)]
getindex(x::AbstractSparseVector, I::AbstractArray{Bool}) = x[findall(I)]
@inline function getindex(x::AbstractSparseVector{Tv,Ti}, I::AbstractVector) where {Tv,Ti}
    # SparseMatrixCSC has a nicely optimized routine for this; punt
    S = SparseMatrixCSC(x.n, 1, Ti[1,length(x.nzind)+1], x.nzind, x.nzval)
    S[I, 1]
end

function getindex(x::AbstractSparseVector{Tv,Ti}, I::AbstractArray) where {Tv,Ti}
    # punt to SparseMatrixCSC
    S = SparseMatrixCSC(x.n, 1, Ti[1,length(x.nzind)+1], x.nzind, x.nzval)
    S[I]
end

getindex(x::AbstractSparseVector, ::Colon) = copy(x)

### show and friends

function show(io::IO, ::MIME"text/plain", x::AbstractSparseVector)
    xnnz = length(nonzeros(x))
    print(io, length(x), "-element ", typeof(x), " with ", xnnz,
           " stored ", xnnz == 1 ? "entry" : "entries")
    if xnnz != 0
        println(io, ":")
        show(io, x)
    end
end

show(io::IO, x::AbstractSparseVector) = show(convert(IOContext, io), x)
function show(io::IOContext, x::AbstractSparseVector)
    # TODO: make this a one-line form
    n = length(x)
    nzind = nonzeroinds(x)
    nzval = nonzeros(x)
    xnnz = length(nzind)
    if length(nzind) == 0
        return show(io, MIME("text/plain"), x)
    end
    limit::Bool = get(io, :limit, false)
    half_screen_rows = limit ? div(displaysize(io)[1] - 8, 2) : typemax(Int)
    pad = ndigits(n)
    if !haskey(io, :compact)
        io = IOContext(io, :compact => true)
    end
    for k = 1:length(nzind)
        if k < half_screen_rows || k > xnnz - half_screen_rows
            print(io, "  ", '[', rpad(nzind[k], pad), "]  =  ")
            if isassigned(nzval, Int(k))
                show(io, nzval[k])
            else
                print(io, Base.undef_ref_str)
            end
            k != length(nzind) && println(io)
        elseif k == half_screen_rows
            println(io, "   ", " "^pad, "   \u22ee")
        end
    end
end

### Conversion to matrix

function SparseMatrixCSC{Tv,Ti}(x::AbstractSparseVector) where {Tv,Ti}
    n = length(x)
    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    m = length(xnzind)
    colptr = Ti[1, m+1]
    # Note that this *cannot* share data like normal array conversions, since
    # modifying one would put the other in an inconsistent state
    rowval = Vector{Ti}(xnzind)
    nzval = Vector{Tv}(xnzval)
    SparseMatrixCSC(n, 1, colptr, rowval, nzval)
end

SparseMatrixCSC{Tv}(x::AbstractSparseVector{<:Any,Ti}) where {Tv,Ti} = SparseMatrixCSC{Tv,Ti}(x)

SparseMatrixCSC(x::AbstractSparseVector{Tv,Ti}) where {Tv,Ti} = SparseMatrixCSC{Tv,Ti}(x)

function Vector(x::AbstractSparseVector{Tv}) where Tv
    n = length(x)
    n == 0 && return Vector{Tv}()
    nzind = nonzeroinds(x)
    nzval = nonzeros(x)
    r = zeros(Tv, n)
    for k in 1:nnz(x)
        i = nzind[k]
        v = nzval[k]
        r[i] = v
    end
    return r
end
Array(x::AbstractSparseVector) = Vector(x)

### Array manipulation

vec(x::AbstractSparseVector) = x
copy(x::AbstractSparseVector) =
    SparseVector(length(x), copy(nonzeroinds(x)), copy(nonzeros(x)))

float(x::AbstractSparseVector{<:AbstractFloat}) = x
float(x::AbstractSparseVector) =
    SparseVector(length(x), copy(nonzeroinds(x)), float(nonzeros(x)))

complex(x::AbstractSparseVector{<:Complex}) = x
complex(x::AbstractSparseVector) =
    SparseVector(length(x), copy(nonzeroinds(x)), complex(nonzeros(x)))


### Concatenation

# Without the first of these methods, horizontal concatenations of SparseVectors fall
# back to the horizontal concatenation method that ensures that combinations of
# sparse/special/dense matrix/vector types concatenate to SparseMatrixCSCs, instead
# of _absspvec_hcat below. The <:Integer qualifications are necessary for correct dispatch.
hcat(X::SparseVector{Tv,Ti}...) where {Tv,Ti<:Integer} = _absspvec_hcat(X...)
hcat(X::AbstractSparseVector{Tv,Ti}...) where {Tv,Ti<:Integer} = _absspvec_hcat(X...)
function _absspvec_hcat(X::AbstractSparseVector{Tv,Ti}...) where {Tv,Ti}
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
    colptr = Vector{Ti}(undef, n+1)
    nzrow = Vector{Ti}(undef, tnnz)
    nzval = Vector{Tv}(undef, tnnz)
    roff = 1
    @inbounds for j = 1:n
        xj = X[j]
        xnzind = nonzeroinds(xj)
        xnzval = nonzeros(xj)
        colptr[j] = roff
        copyto!(nzrow, roff, xnzind)
        copyto!(nzval, roff, xnzval)
        roff += length(xnzind)
    end
    colptr[n+1] = roff
    SparseMatrixCSC{Tv,Ti}(m, n, colptr, nzrow, nzval)
end

# Without the first of these methods, vertical concatenations of SparseVectors fall
# back to the vertical concatenation method that ensures that combinations of
# sparse/special/dense matrix/vector types concatenate to SparseMatrixCSCs, instead
# of _absspvec_vcat below. The <:Integer qualifications are necessary for correct dispatch.
vcat(X::SparseVector{Tv,Ti}...) where {Tv,Ti<:Integer} = _absspvec_vcat(X...)
vcat(X::AbstractSparseVector{Tv,Ti}...) where {Tv,Ti<:Integer} = _absspvec_vcat(X...)
function vcat(X::SparseVector...)
    commeltype = promote_type(map(eltype, X)...)
    commindtype = promote_type(map(indtype, X)...)
    vcat(map(x -> SparseVector{commeltype,commindtype}(x), X)...)
end
function _absspvec_vcat(X::AbstractSparseVector{Tv,Ti}...) where {Tv,Ti}
    # check sizes
    n = length(X)
    tnnz = 0
    for j = 1:n
        tnnz += nnz(X[j])
    end

    # construction
    rnzind = Vector{Ti}(undef, tnnz)
    rnzval = Vector{Tv}(undef, tnnz)
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
        copyto!(rnzval, ir+1, xnzval)
        ir += xnnz
        len += length(xj)
    end
    SparseVector(len, rnzind, rnzval)
end

hcat(Xin::Union{Vector, AbstractSparseVector}...) = hcat(map(sparse, Xin)...)
vcat(Xin::Union{Vector, AbstractSparseVector}...) = vcat(map(sparse, Xin)...)
# Without the following method, vertical concatenations of SparseVectors with Vectors
# fall back to the vertical concatenation method that ensures that combinations of
# sparse/special/dense matrix/vector types concatenate to SparseMatrixCSCs (because
# the vcat method immediately above is less specific, being defined in AbstractSparseVector
# rather than SparseVector).
vcat(X::Union{Vector,SparseVector}...) = vcat(map(sparse, X)...)


### Concatenation of un/annotated sparse/special/dense vectors/matrices

# TODO: These methods and definitions should be moved to a more appropriate location,
# particularly some future equivalent of base/linalg/special.jl dedicated to interactions
# between a broader set of matrix types.

# TODO: A definition similar to the third exists in base/linalg/bidiag.jl. These definitions
# should be consolidated in a more appropriate location, e.g. base/linalg/special.jl.
const _SparseArrays = Union{SparseVector, SparseMatrixCSC, LinearAlgebra.RowVector{<:Any,<:SparseVector}, Adjoint{<:Any,<:SparseVector}, Transpose{<:Any,<:SparseVector}}
const _SpecialArrays = Union{Diagonal, Bidiagonal, Tridiagonal, SymTridiagonal}
const _SparseConcatArrays = Union{_SpecialArrays, _SparseArrays}

const _Symmetric_SparseConcatArrays{T,A<:_SparseConcatArrays} = Symmetric{T,A}
const _Hermitian_SparseConcatArrays{T,A<:_SparseConcatArrays} = Hermitian{T,A}
const _Triangular_SparseConcatArrays{T,A<:_SparseConcatArrays} = LinearAlgebra.AbstractTriangular{T,A}
const _Annotated_SparseConcatArrays = Union{_Triangular_SparseConcatArrays, _Symmetric_SparseConcatArrays, _Hermitian_SparseConcatArrays}

const _Symmetric_DenseArrays{T,A<:Matrix} = Symmetric{T,A}
const _Hermitian_DenseArrays{T,A<:Matrix} = Hermitian{T,A}
const _Triangular_DenseArrays{T,A<:Matrix} = LinearAlgebra.AbstractTriangular{T,A}
const _Annotated_DenseArrays = Union{_Triangular_DenseArrays, _Symmetric_DenseArrays, _Hermitian_DenseArrays}
const _Annotated_Typed_DenseArrays{T} = Union{_Triangular_DenseArrays{T}, _Symmetric_DenseArrays{T}, _Hermitian_DenseArrays{T}}

const _SparseConcatGroup = Union{Vector, Adjoint{<:Any,<:Vector}, Transpose{<:Any,<:Vector}, LinearAlgebra.RowVector{<:Any,<:Vector}, Matrix, _SparseConcatArrays, _Annotated_SparseConcatArrays, _Annotated_DenseArrays}
const _DenseConcatGroup = Union{Vector, Adjoint{<:Any,<:Vector}, Transpose{<:Any,<:Vector}, LinearAlgebra.RowVector{<:Any, <:Vector}, Matrix, _Annotated_DenseArrays}
const _TypedDenseConcatGroup{T} = Union{Vector{T}, Adjoint{T,Vector{T}}, Transpose{T,Vector{T}}, LinearAlgebra.RowVector{T,Vector{T}}, Matrix{T}, _Annotated_Typed_DenseArrays{T}}

# Concatenations involving un/annotated sparse/special matrices/vectors should yield sparse arrays
function cat(catdims, Xin::_SparseConcatGroup...)
    X = map(x -> SparseMatrixCSC(issparse(x) ? x : sparse(x)), Xin)
    T = promote_eltype(Xin...)
    Base.cat_t(catdims, T, X...)
end
function hcat(Xin::_SparseConcatGroup...)
    X = map(x -> SparseMatrixCSC(issparse(x) ? x : sparse(x)), Xin)
    hcat(X...)
end
function vcat(Xin::_SparseConcatGroup...)
    X = map(x -> SparseMatrixCSC(issparse(x) ? x : sparse(x)), Xin)
    vcat(X...)
end
function hvcat(rows::Tuple{Vararg{Int}}, X::_SparseConcatGroup...)
    nbr = length(rows)  # number of block rows

    tmp_rows = Vector{SparseMatrixCSC}(undef, nbr)
    k = 0
    @inbounds for i = 1 : nbr
        tmp_rows[i] = hcat(X[(1 : rows[i]) .+ k]...)
        k += rows[i]
    end
    vcat(tmp_rows...)
end

# make sure UniformScaling objects are converted to sparse matrices for concatenation
promote_to_array_type(A::Tuple{Vararg{Union{_SparseConcatGroup,UniformScaling}}}) = SparseMatrixCSC
promote_to_array_type(A::Tuple{Vararg{Union{_DenseConcatGroup,UniformScaling}}}) = Matrix
promote_to_arrays_(n::Int, ::Type{SparseMatrixCSC}, J::UniformScaling) = sparse(J, n, n)

# Concatenations strictly involving un/annotated dense matrices/vectors should yield dense arrays
cat(catdims, xs::_DenseConcatGroup...) = Base.cat_t(catdims, promote_eltype(xs...), xs...)
vcat(A::Vector...) = Base.typed_vcat(promote_eltype(A...), A...)
vcat(A::_DenseConcatGroup...) = Base.typed_vcat(promote_eltype(A...), A...)
hcat(A::Vector...) = Base.typed_hcat(promote_eltype(A...), A...)
hcat(A::_DenseConcatGroup...) = Base.typed_hcat(promote_eltype(A...), A...)
hvcat(rows::Tuple{Vararg{Int}}, xs::_DenseConcatGroup...) = Base.typed_hvcat(promote_eltype(xs...), rows, xs...)
# For performance, specially handle the case where the matrices/vectors have homogeneous eltype
cat(catdims, xs::_TypedDenseConcatGroup{T}...) where {T} = Base.cat_t(catdims, T, xs...)
vcat(A::_TypedDenseConcatGroup{T}...) where {T} = Base.typed_vcat(T, A...)
hcat(A::_TypedDenseConcatGroup{T}...) where {T} = Base.typed_hcat(T, A...)
hvcat(rows::Tuple{Vararg{Int}}, xs::_TypedDenseConcatGroup{T}...) where {T} = Base.typed_hvcat(T, rows, xs...)


### math functions

### Unary Map

# zero-preserving functions (z->z, nz->nz)
-(x::SparseVector) = SparseVector(length(x), copy(nonzeroinds(x)), -(nonzeros(x)))

# functions f, such that
#   f(x) can be zero or non-zero when x != 0
#   f(x) = 0 when x == 0
#
macro unarymap_nz2z_z2z(op, TF)
    esc(quote
        function $(op)(x::AbstractSparseVector{Tv,Ti}) where Tv<:$(TF) where Ti<:Integer
            R = typeof($(op)(zero(Tv)))
            xnzind = nonzeroinds(x)
            xnzval = nonzeros(x)
            m = length(xnzind)

            ynzind = Vector{Ti}(undef, m)
            ynzval = Vector{R}(undef, m)
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

# the rest of real, conj, imag are handled correctly via AbstractArray methods
@unarymap_nz2z_z2z real Complex
conj(x::SparseVector{<:Complex}) = SparseVector(length(x), copy(nonzeroinds(x)), conj(nonzeros(x)))
imag(x::AbstractSparseVector{Tv,Ti}) where {Tv<:Real,Ti<:Integer} = SparseVector(length(x), Ti[], Tv[])
@unarymap_nz2z_z2z imag Complex

# function that does not preserve zeros

macro unarymap_z2nz(op, TF)
    esc(quote
        function $(op)(x::AbstractSparseVector{Tv,<:Integer}) where Tv<:$(TF)
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

### Binary Map

# mode:
# 0: f(nz, nz) -> nz, f(z, nz) -> z, f(nz, z) ->  z
# 1: f(nz, nz) -> z/nz, f(z, nz) -> nz, f(nz, z) -> nz
# 2: f(nz, nz) -> z/nz, f(z, nz) -> z/nz, f(nz, z) -> z/nz

function _binarymap(f::Function,
                    x::AbstractSparseVector{Tx},
                    y::AbstractSparseVector{Ty},
                    mode::Int) where {Tx,Ty}
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
    cap = (mode == 0 ? min(mx, my) : mx + my)::Int

    rind = Vector{Int}(undef, cap)
    rval = Vector{R}(undef, cap)
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

function _binarymap_mode_0!(f::Function, mx::Int, my::Int,
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

function _binarymap_mode_1!(f::Function, mx::Int, my::Int,
                            xnzind, xnzval::AbstractVector{Tx},
                            ynzind, ynzval::AbstractVector{Ty},
                            rind, rval) where {Tx,Ty}
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

function _binarymap_mode_2!(f::Function, mx::Int, my::Int,
                            xnzind, xnzval::AbstractVector{Tx},
                            ynzind, ynzval::AbstractVector{Ty},
                            rind, rval) where {Tx,Ty}
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

function _binarymap(f::Function,
                    x::AbstractVector{Tx},
                    y::AbstractSparseVector{Ty},
                    mode::Int) where {Tx,Ty}
    0 <= mode <= 2 || throw(ArgumentError("Incorrect mode $mode."))
    R = typeof(f(zero(Tx), zero(Ty)))
    n = length(x)
    length(y) == n || throw(DimensionMismatch())

    ynzind = nonzeroinds(y)
    ynzval = nonzeros(y)
    m = length(ynzind)

    dst = Vector{R}(undef, n)
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

function _binarymap(f::Function,
                    x::AbstractSparseVector{Tx},
                    y::AbstractVector{Ty},
                    mode::Int) where {Tx,Ty}
    0 <= mode <= 2 || throw(ArgumentError("Incorrect mode $mode."))
    R = typeof(f(zero(Tx), zero(Ty)))
    n = length(x)
    length(y) == n || throw(DimensionMismatch())

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    m = length(xnzind)

    dst = Vector{R}(undef, n)
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

for (vop, fun, mode) in [(:_vadd, :+, 1),
                         (:_vsub, :-, 1),
                         (:_vmul, :*, 0)]
    @eval begin
        $(vop)(x::AbstractSparseVector, y::AbstractSparseVector) = _binarymap($(fun), x, y, $mode)
        $(vop)(x::AbstractVector, y::AbstractSparseVector) = _binarymap($(fun), x, y, $mode)
        $(vop)(x::AbstractSparseVector, y::AbstractVector) = _binarymap($(fun), x, y, $mode)
    end
end

# to workaround the ambiguities with BitVector
broadcast(::typeof(*), x::BitVector, y::AbstractSparseVector{Bool}) = _vmul(x, y)
broadcast(::typeof(*), x::AbstractSparseVector{Bool}, y::BitVector) = _vmul(x, y)

# definition of operators

for (op, vop) in [(:+, :_vadd), (:-, :_vsub), (:*, :_vmul)]
    op != :* && @eval begin
        $(op)(x::AbstractSparseVector, y::AbstractSparseVector) = $(vop)(x, y)
        $(op)(x::AbstractVector, y::AbstractSparseVector) = $(vop)(x, y)
        $(op)(x::AbstractSparseVector, y::AbstractVector) = $(vop)(x, y)
    end
    @eval begin
        broadcast(::typeof($op), x::AbstractSparseVector, y::AbstractSparseVector) = $(vop)(x, y)
        broadcast(::typeof($op), x::AbstractVector, y::AbstractSparseVector) = $(vop)(x, y)
        broadcast(::typeof($op), x::AbstractSparseVector, y::AbstractVector) = $(vop)(x, y)
    end
end

# definition of other binary functions

broadcast(::typeof(min), x::SparseVector{<:Real}, y::SparseVector{<:Real}) = _binarymap(min, x, y, 2)
broadcast(::typeof(min), x::AbstractSparseVector{<:Real}, y::AbstractSparseVector{<:Real}) = _binarymap(min, x, y, 2)
broadcast(::typeof(min), x::AbstractVector{<:Real}, y::AbstractSparseVector{<:Real}) = _binarymap(min, x, y, 2)
broadcast(::typeof(min), x::AbstractSparseVector{<:Real}, y::AbstractVector{<:Real}) = _binarymap(min, x, y, 2)

broadcast(::typeof(max), x::SparseVector{<:Real}, y::SparseVector{<:Real}) = _binarymap(max, x, y, 2)
broadcast(::typeof(max), x::AbstractSparseVector{<:Real}, y::AbstractSparseVector{<:Real}) = _binarymap(max, x, y, 2)
broadcast(::typeof(max), x::AbstractVector{<:Real}, y::AbstractSparseVector{<:Real}) = _binarymap(max, x, y, 2)
broadcast(::typeof(max), x::AbstractSparseVector{<:Real}, y::AbstractVector{<:Real}) = _binarymap(max, x, y, 2)

complex(x::AbstractSparseVector{<:Real}, y::AbstractSparseVector{<:Real}) = _binarymap(complex, x, y, 1)
complex(x::AbstractVector{<:Real}, y::AbstractSparseVector{<:Real}) = _binarymap(complex, x, y, 1)
complex(x::AbstractSparseVector{<:Real}, y::AbstractVector{<:Real}) = _binarymap(complex, x, y, 1)

### Reduction

sum(x::AbstractSparseVector) = sum(nonzeros(x))

function maximum(x::AbstractSparseVector{T}) where T<:Real
    n = length(x)
    n > 0 || throw(ArgumentError("maximum over empty array is not allowed."))
    m = nnz(x)
    (m == 0 ? zero(T) :
     m == n ? maximum(nonzeros(x)) :
     max(zero(T), maximum(nonzeros(x))))::T
end

function minimum(x::AbstractSparseVector{T}) where T<:Real
    n = length(x)
    n > 0 || throw(ArgumentError("minimum over empty array is not allowed."))
    m = nnz(x)
    (m == 0 ? zero(T) :
     m == n ? minimum(nonzeros(x)) :
     min(zero(T), minimum(nonzeros(x))))::T
end

for f in [:sum, :maximum, :minimum], op in [:abs, :abs2]
    SV = :AbstractSparseVector
    if f == :minimum
        @eval ($f)(::typeof($op), x::$SV{T}) where {T<:Number} = nnz(x) < length(x) ? ($op)(zero(T)) : ($f)($op, nonzeros(x))
    else
        @eval ($f)(::typeof($op), x::$SV) = ($f)($op, nonzeros(x))
    end
end

vecnorm(x::SparseVectorUnion, p::Real=2) = vecnorm(nonzeros(x), p)

### linalg.jl

# Transpose
# (The only sparse matrix structure in base is CSC, so a one-row sparse matrix is worse than dense)
transpose(sv::SparseVector) = Transpose(sv)
adjoint(sv::SparseVector) = Adjoint(sv)

### BLAS Level-1

# axpy

function LinearAlgebra.axpy!(a::Number, x::SparseVectorUnion, y::AbstractVector)
    length(x) == length(y) || throw(DimensionMismatch())
    nzind = nonzeroinds(x)
    nzval = nonzeros(x)
    m = length(nzind)

    if a == oneunit(a)
        for i = 1:m
            @inbounds ii = nzind[i]
            @inbounds v = nzval[i]
            y[ii] += v
        end
    elseif a == -oneunit(a)
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


# scaling

function rmul!(x::SparseVectorUnion, a::Real)
    rmul!(nonzeros(x), a)
    return x
end
function rmul!(x::SparseVectorUnion, a::Complex)
    rmul!(nonzeros(x), a)
    return x
end
function lmul!(a::Real, x::SparseVectorUnion)
    rmul!(nonzeros(x), a)
    return x
end
function lmul!(a::Complex, x::SparseVectorUnion)
    rmul!(nonzeros(x), a)
    return x
end

(*)(x::SparseVectorUnion, a::Number) = SparseVector(length(x), copy(nonzeroinds(x)), nonzeros(x) * a)
(*)(a::Number, x::SparseVectorUnion) = SparseVector(length(x), copy(nonzeroinds(x)), a * nonzeros(x))
(/)(x::SparseVectorUnion, a::Number) = SparseVector(length(x), copy(nonzeroinds(x)), nonzeros(x) / a)

# dot
function dot(x::AbstractVector{Tx}, y::SparseVectorUnion{Ty}) where {Tx<:Number,Ty<:Number}
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

function dot(x::SparseVectorUnion{Tx}, y::AbstractVector{Ty}) where {Tx<:Number,Ty<:Number}
    n = length(y)
    length(x) == n || throw(DimensionMismatch())
    nzind = nonzeroinds(x)
    nzval = nonzeros(x)
    s = zero(Tx) * zero(Ty)
    @inbounds for i = 1:length(nzind)
        s += conj(nzval[i]) * y[nzind[i]]
    end
    return s
end

function _spdot(f::Function,
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

function dot(x::SparseVectorUnion{<:Number}, y::SparseVectorUnion{<:Number})
    x === y && return sum(abs2, x)
    n = length(x)
    length(y) == n || throw(DimensionMismatch())

    xnzind = nonzeroinds(x)
    ynzind = nonzeroinds(y)
    xnzval = nonzeros(x)
    ynzval = nonzeros(y)

    _spdot(dot,
           1, length(xnzind), xnzind, xnzval,
           1, length(ynzind), ynzind, ynzval)
end


### BLAS-2 / dense A * sparse x -> dense y

# lowrankupdate (BLAS.ger! like)
function LinearAlgebra.lowrankupdate!(A::StridedMatrix, x::AbstractVector, y::SparseVectorUnion, α::Number = 1)
    nzi = nonzeroinds(y)
    nzv = nonzeros(y)
    @inbounds for (j,v) in zip(nzi,nzv)
        αv = α*conj(v)
        for i in axes(x, 1)
            A[i,j] += x[i]*αv
        end
    end
    return A
end

# * and mul!

function (*)(A::StridedMatrix{Ta}, x::AbstractSparseVector{Tx}) where {Ta,Tx}
    m, n = size(A)
    length(x) == n || throw(DimensionMismatch())
    Ty = promote_type(Ta, Tx)
    y = Vector{Ty}(undef, m)
    mul!(y, A, x)
end

mul!(y::AbstractVector{Ty}, A::StridedMatrix, x::AbstractSparseVector{Tx}) where {Tx,Ty} =
    mul!(y, A, x, one(Tx), zero(Ty))

function mul!(y::AbstractVector, A::StridedMatrix, x::AbstractSparseVector, α::Number, β::Number)
    m, n = size(A)
    length(x) == n && length(y) == m || throw(DimensionMismatch())
    m == 0 && return y
    if β != one(β)
        β == zero(β) ? fill!(y, zero(eltype(y))) : rmul!(y, β)
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

# * and mul!(C, transpose(A), B)

function *(transA::Transpose{<:Any,<:StridedMatrix{Ta}}, x::AbstractSparseVector{Tx}) where {Ta,Tx}
    A = transA.parent
    m, n = size(A)
    length(x) == m || throw(DimensionMismatch())
    Ty = promote_type(Ta, Tx)
    y = Vector{Ty}(undef, n)
    mul!(y, transpose(A), x)
end

mul!(y::AbstractVector{Ty}, transA::Transpose{<:Any,<:StridedMatrix}, x::AbstractSparseVector{Tx}) where {Tx,Ty} =
    (A = transA.parent; mul!(y, transpose(A), x, one(Tx), zero(Ty)))

function mul!(y::AbstractVector, transA::Transpose{<:Any,<:StridedMatrix}, x::AbstractSparseVector, α::Number, β::Number)
    A = transA.parent
    m, n = size(A)
    length(x) == m && length(y) == n || throw(DimensionMismatch())
    n == 0 && return y
    if β != one(β)
        β == zero(β) ? fill!(y, zero(eltype(y))) : rmul!(y, β)
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

function densemv(A::SparseMatrixCSC, x::AbstractSparseVector; trans::AbstractChar='N')
    local xlen::Int, ylen::Int
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
    y = Vector{T}(undef, ylen)
    if trans == 'N' || trans == 'N'
        mul!(y, A, x)
    elseif trans == 'T' || trans == 't'
        mul!(y, transpose(A), x)
    elseif trans == 'C' || trans == 'c'
        mul!(y, adjoint(A), x)
    else
        throw(ArgumentError("Invalid trans character $trans"))
    end
    y
end

# * and mul!

mul!(y::AbstractVector{Ty}, A::SparseMatrixCSC, x::AbstractSparseVector{Tx}) where {Tx,Ty} =
    mul!(y, A, x, one(Tx), zero(Ty))

function mul!(y::AbstractVector, A::SparseMatrixCSC, x::AbstractSparseVector, α::Number, β::Number)
    m, n = size(A)
    length(x) == n && length(y) == m || throw(DimensionMismatch())
    m == 0 && return y
    if β != one(β)
        β == zero(β) ? fill!(y, zero(eltype(y))) : rmul!(y, β)
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

# * and *(Tranpose(A), B)

mul!(y::AbstractVector{Ty}, transA::Transpose{<:Any,<:SparseMatrixCSC}, x::AbstractSparseVector{Tx}) where {Tx,Ty} =
    (A = transA.parent; mul!(y, transpose(A), x, one(Tx), zero(Ty)))

mul!(y::AbstractVector, transA::Transpose{<:Any,<:SparseMatrixCSC}, x::AbstractSparseVector, α::Number, β::Number) =
    (A = transA.parent; _At_or_Ac_mul_B!(*, y, A, x, α, β))

mul!(y::AbstractVector{Ty}, adjA::Adjoint{<:Any,<:SparseMatrixCSC}, x::AbstractSparseVector{Tx}) where {Tx,Ty} =
    (A = adjA.parent; mul!(y, adjoint(A), x, one(Tx), zero(Ty)))

mul!(y::AbstractVector, adjA::Adjoint{<:Any,<:SparseMatrixCSC}, x::AbstractSparseVector, α::Number, β::Number) =
    (A = adjA.parent; _At_or_Ac_mul_B!(dot, y, A, x, α, β))

function _At_or_Ac_mul_B!(tfun::Function,
                          y::AbstractVector, A::SparseMatrixCSC, x::AbstractSparseVector,
                          α::Number, β::Number)
    m, n = size(A)
    length(x) == m && length(y) == n || throw(DimensionMismatch())
    n == 0 && return y
    if β != one(β)
        β == zero(β) ? fill!(y, zero(eltype(y))) : rmul!(y, β)
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

*(transA::Transpose{<:Any,<:SparseMatrixCSC}, x::AbstractSparseVector) =
    (A = transA.parent; _At_or_Ac_mul_B(*, A, x))

*(adjA::Adjoint{<:Any,<:SparseMatrixCSC}, x::AbstractSparseVector) =
    (A = adjA.parent; _At_or_Ac_mul_B(dot, A, x))

function _At_or_Ac_mul_B(tfun::Function, A::SparseMatrixCSC{TvA,TiA}, x::AbstractSparseVector{TvX,TiX}) where {TvA,TiA,TvX,TiX}
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

    ynzind = Vector{Ti}(undef, n)
    ynzval = Vector{Tv}(undef, n)

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


# define matrix division operations involving triangular matrices and sparse vectors
# the valid left-division operations are A[t|c]_ldiv_B[!] and \
# the valid right-division operations are A(t|c)_rdiv_B[t|c][!]
# see issue #14005 for discussion of these methods
for isunittri in (true, false), islowertri in (true, false)
    unitstr = isunittri ? "Unit" : ""
    halfstr = islowertri ? "Lower" : "Upper"
    tritype = :(LinearAlgebra.$(Symbol(unitstr, halfstr, "Triangular")))

    # build out-of-place left-division operations
    for (istrans, applyxform, xformtype, xformop) in (
            (false, false, :identity,  :identity),
            (true,  true,  :Transpose, :transpose),
            (true,  true,  :Adjoint,   :adjoint) )

        # broad method where elements are Numbers
        xformtritype = applyxform ? :($xformtype{<:TA,<:$tritype{<:Any,<:AbstractMatrix}}) :
                                    :($tritype{<:TA,<:AbstractMatrix})
        @eval function \(xformA::$xformtritype, b::SparseVector{Tb}) where {TA<:Number,Tb<:Number}
            A = $(applyxform ? :(xformA.parent) : :(xformA) )
            TAb = $(isunittri ?
                :(typeof(zero(TA)*zero(Tb) + zero(TA)*zero(Tb))) :
                :(typeof((zero(TA)*zero(Tb) + zero(TA)*zero(Tb))/one(TA))) )
            LinearAlgebra.ldiv!($xformop(convert(AbstractArray{TAb}, A)), convert(Array{TAb}, b))
        end

        # faster method requiring good view support of the
        # triangular matrix type. hence the StridedMatrix restriction.
        xformtritype = applyxform ? :($xformtype{<:TA,<:$tritype{<:Any,<:StridedMatrix}}) :
                                    :($tritype{<:TA,<:StridedMatrix})
        @eval function \(xformA::$xformtritype, b::SparseVector{Tb}) where {TA<:Number,Tb<:Number}
            A = $(applyxform ? :(xformA.parent) : :(xformA) )
            TAb = $(isunittri ?
                :(typeof(zero(TA)*zero(Tb) + zero(TA)*zero(Tb))) :
                :(typeof((zero(TA)*zero(Tb) + zero(TA)*zero(Tb))/one(TA))) )
            r = convert(Array{TAb}, b)
            # If b has no nonzero entries, then r is necessarily zero. If b has nonzero
            # entries, then the operation involves only b[nzrange], so we extract and
            # operate on solely b[nzrange] for efficiency.
            if nnz(b) != 0
                nzrange = $( (islowertri && !istrans) || (!islowertri && istrans) ?
                    :(b.nzind[1]:b.n) :
                    :(1:b.nzind[end]) )
                nzrangeviewr = view(r, nzrange)
                nzrangeviewA = $tritype(view(A.data, nzrange, nzrange))
                LinearAlgebra.ldiv!($xformop(convert(AbstractArray{TAb}, nzrangeviewA)), nzrangeviewr)
            end
            r
        end

        # fallback where elements are not Numbers
        xformtritype = applyxform ? :($xformtype{<:Any,<:$tritype}) : :($tritype)
        @eval function \(xformA::$xformtritype, b::SparseVector)
            A = $(applyxform ? :(xformA.parent) : :(xformA) )
            LinearAlgebra.ldiv!($xformop(A), copy(b))
        end
    end

    # build in-place left-division operations
    for (istrans, applyxform, xformtype, xformop) in (
            (false, false, :identity,  :identity),
            (true,  true,  :Transpose, :transpose),
            (true,  true,  :Adjoint,   :adjoint) )
        xformtritype = applyxform ? :($xformtype{<:Any,<:$tritype{<:Any,<:StridedMatrix}}) :
                                    :($tritype{<:Any,<:StridedMatrix})

        # the generic in-place left-division methods handle these cases, but
        # we can achieve greater efficiency where the triangular matrix provides
        # good view support. hence the StridedMatrix restriction.
        @eval function ldiv!(xformA::$xformtritype, b::SparseVector)
            A = $(applyxform ? :(xformA.parent) : :(xformA) )
            # If b has no nonzero entries, the result is necessarily zero and this call
            # reduces to a no-op. If b has nonzero entries, then...
            if nnz(b) != 0
                # densify the relevant part of b in one shot rather
                # than potentially repeatedly reallocating during the solve
                $( (islowertri && !istrans) || (!islowertri && istrans) ?
                    :(_densifyfirstnztoend!(b)) :
                    :(_densifystarttolastnz!(b)) )
                # this operation involves only the densified section, so
                # for efficiency we extract and operate on solely that section
                # furthermore we operate on that section as a dense vector
                # such that dispatch has a chance to exploit, e.g., tuned BLAS
                nzrange = $( (islowertri && !istrans) || (!islowertri && istrans) ?
                    :(b.nzind[1]:b.n) :
                    :(1:b.nzind[end]) )
                nzrangeviewbnz = view(b.nzval, nzrange .- (b.nzind[1] - 1))
                nzrangeviewA = $tritype(view(A.data, nzrange, nzrange))
                LinearAlgebra.ldiv!($xformop(nzrangeviewA), nzrangeviewbnz)
            end
            b
        end
    end
end

# helper functions for in-place matrix division operations defined above
"Densifies `x::SparseVector` from its first nonzero (`x[x.nzind[1]]`) through its end (`x[x.n]`)."
function _densifyfirstnztoend!(x::SparseVector)
    # lengthen containers
    oldnnz = nnz(x)
    newnnz = x.n - x.nzind[1] + 1
    resize!(x.nzval, newnnz)
    resize!(x.nzind, newnnz)
    # redistribute nonzero values over lengthened container
    # initialize now-allocated zero values simultaneously
    nextpos = newnnz
    @inbounds for oldpos in oldnnz:-1:1
        nzi = x.nzind[oldpos]
        nzv = x.nzval[oldpos]
        newpos = nzi - x.nzind[1] + 1
        newpos < nextpos && (x.nzval[newpos+1:nextpos] = 0)
        newpos == oldpos && break
        x.nzval[newpos] = nzv
        nextpos = newpos - 1
    end
    # finally update lengthened nzinds
    x.nzind[2:end] = (x.nzind[1]+1):x.n
    x
end
"Densifies `x::SparseVector` from its beginning (`x[1]`) through its last nonzero (`x[x.nzind[end]]`)."
function _densifystarttolastnz!(x::SparseVector)
    # lengthen containers
    oldnnz = nnz(x)
    newnnz = x.nzind[end]
    resize!(x.nzval, newnnz)
    resize!(x.nzind, newnnz)
    # redistribute nonzero values over lengthened container
    # initialize now-allocated zero values simultaneously
    nextpos = newnnz
    @inbounds for oldpos in oldnnz:-1:1
        nzi = x.nzind[oldpos]
        nzv = x.nzval[oldpos]
        nzi < nextpos && (x.nzval[nzi+1:nextpos] = 0)
        nzi == oldpos && (nextpos = 0; break)
        x.nzval[nzi] = nzv
        nextpos = nzi - 1
    end
    nextpos > 0 && (x.nzval[1:nextpos] = 0)
    # finally update lengthened nzinds
    x.nzind[1:newnnz] = 1:newnnz
    x
end

#sorting
function sort(x::SparseVector{Tv,Ti}; kws...) where {Tv,Ti}
    allvals = push!(copy(nonzeros(x)),zero(Tv))
    sinds = sortperm(allvals;kws...)
    n,k = length(x),length(allvals)
    z = findfirst(isequal(k),sinds)::Int
    newnzind = Vector{Ti}(1:k-1)
    newnzind[z:end] .+= n-k+1
    newnzvals = allvals[deleteat!(sinds[1:k],z)]
    SparseVector(n,newnzind,newnzvals)
end

function fkeep!(x::SparseVector, f, trim::Bool = true)
    n = x.n
    nzind = x.nzind
    nzval = x.nzval

    x_writepos = 1
    @inbounds for xk in 1:nnz(x)
        xi = nzind[xk]
        xv = nzval[xk]
        # If this element should be kept, rewrite in new position
        if f(xi, xv)
            if x_writepos != xk
                nzind[x_writepos] = xi
                nzval[x_writepos] = xv
            end
            x_writepos += 1
        end
    end

    # Trim x's storage if necessary and desired
    if trim
        x_nnz = x_writepos - 1
        if length(nzind) != x_nnz
            resize!(nzval, x_nnz)
            resize!(nzind, x_nnz)
        end
    end

    x
end

droptol!(x::SparseVector, tol; trim::Bool = true) = fkeep!(x, (i, x) -> abs(x) > tol, trim)

"""
    dropzeros!(x::SparseVector; trim::Bool = true)

Removes stored numerical zeros from `x`, optionally trimming resulting excess space from
`x.nzind` and `x.nzval` when `trim` is `true`.

For an out-of-place version, see [`dropzeros`](@ref). For
algorithmic information, see `fkeep!`.
"""
dropzeros!(x::SparseVector; trim::Bool = true) = fkeep!(x, (i, x) -> x != 0, trim)

"""
    dropzeros(x::SparseVector; trim::Bool = true)

Generates a copy of `x` and removes numerical zeros from that copy, optionally trimming
excess space from the result's `nzind` and `nzval` arrays when `trim` is `true`.

For an in-place version and algorithmic information, see [`dropzeros!`](@ref).

# Examples
```jldoctest
julia> A = sparsevec([1, 2, 3], [1.0, 0.0, 1.0])
3-element SparseVector{Float64,Int64} with 3 stored entries:
  [1]  =  1.0
  [2]  =  0.0
  [3]  =  1.0

julia> dropzeros(A)
3-element SparseVector{Float64,Int64} with 2 stored entries:
  [1]  =  1.0
  [3]  =  1.0
```
"""
dropzeros(x::SparseVector; trim::Bool = true) = dropzeros!(copy(x), trim = trim)


function _fillnonzero!(arr::SparseMatrixCSC{Tv, Ti}, val) where {Tv,Ti}
    m, n = size(arr)
    resize!(arr.colptr, n+1)
    resize!(arr.rowval, m*n)
    resize!(arr.nzval, m*n)
    copyto!(arr.colptr, 1:m:n*m+1)
    fill!(arr.nzval, val)
    index = 1
    @inbounds for _ in 1:n
        for i in 1:m
            arr.rowval[index] = Ti(i)
            index += 1
        end
    end
    arr
end

function _fillnonzero!(arr::SparseVector{Tv,Ti}, val) where {Tv,Ti}
    n = arr.n
    resize!(arr.nzind, n)
    resize!(arr.nzval, n)
    @inbounds for i in 1:n
        arr.nzind[i] = Ti(i)
    end
    fill!(arr.nzval, val)
    arr
end

import Base.fill!
function fill!(A::Union{SparseVector, SparseMatrixCSC}, x)
    T = eltype(A)
    xT = convert(T, x)
    if xT == zero(T)
        fill!(A.nzval, xT)
    else
        _fillnonzero!(A, xT)
    end
    return A
end
