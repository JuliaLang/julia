# This file is a part of Julia. License is MIT: https://julialang.org/license

import LinearAlgebra: AbstractTriangular

"""
    SparseMatrixCSCSymmHerm

`Symmetric` or `Hermitian` of a `SparseMatrixCSC` or `SparseMatrixCSCView`.
"""
const SparseMatrixCSCSymmHerm{Tv,Ti} = Union{Symmetric{Tv,<:SparseMatrixCSCUnion{Tv,Ti}},
                                            Hermitian{Tv,<:SparseMatrixCSCUnion{Tv,Ti}}}

const AbstractTriangularSparse{Tv,Ti} = AbstractTriangular{Tv,<:SparseMatrixCSCUnion{Tv,Ti}}

# converting Symmetric/Hermitian/AbstractTriangular/SubArray of SparseMatrixCSC
# and Transpose/Adjoint of AbstractTriangular of SparseMatrixCSC to SparseMatrixCSC
for wr in (Symmetric, Hermitian, Transpose, Adjoint,
           UpperTriangular, LowerTriangular, UnitUpperTriangular, UnitLowerTriangular,
           SubArray)

    @eval SparseMatrixCSC(A::$wr) = _sparsem(A)
    @eval SparseMatrixCSC{Tv}(A::$wr{Tv}) where Tv = _sparsem(A)
    @eval SparseMatrixCSC{Tv}(A::$wr) where Tv = SparseMatrixCSC{Tv}(_sparsem(A))
    @eval SparseMatrixCSC{Tv,Ti}(A::$wr) where {Tv,Ti} = SparseMatrixCSC{Tv,Ti}(_sparsem(A))
end

"""
    iswrsparse(::S)
    iswrsparse(::Type{S})

Returns `true` if type `S` is backed by a sparse array, and `false` otherwise.
"""
iswrsparse(::T) where T<:AbstractArray = iswrsparse(T)
iswrsparse(::Type) = false
iswrsparse(::Type{T}) where T<:AbstractSparseArray = true

"""
    depth(::Type{S})

Returns 0 for unwrapped S, and nesting depth for wrapped (nested) abstract arrays.
"""
depth(::T) where T = depth(T)
depth(::Type{T}) where T<:AbstractArray = 0

for wr in (Symmetric, Hermitian,
           LowerTriangular, UnitLowerTriangular, UpperTriangular, UnitUpperTriangular,
           Transpose, Adjoint, SubArray,
           Diagonal, Bidiagonal, Tridiagonal, SymTridiagonal)

    pl = wr === SubArray ? :($wr{<:Any,<:Any,T}) : :($wr{<:Any,T})
    @eval iswrsparse(::Type{<:$pl}) where T = iswrsparse(T)
    @eval depth(::Type{<:$pl}) where T = depth(T) + 1
end

# convert parent and re-wrap in same wrapper
_sparsewrap(A::Symmetric) = Symmetric(_sparsem(parent(A)), A.uplo == 'U' ? :U : :L)
_sparsewrap(A::Hermitian) = Hermitian(_sparsem(parent(A)), A.uplo == 'U' ? :U : :L)
_sparsewrap(A::SubArray) = SubArray(_sparsem(parent(A)), A.indices)
for ty in ( LowerTriangular, UnitLowerTriangular,
            UpperTriangular, UnitUpperTriangular,
            Transpose, Adjoint)

    @eval _sparsewrap(A::$ty) = $ty(_sparsem(parent(A)))
end
function _sparsewrap(A::Union{Diagonal,Bidiagonal,Tridiagonal,SymTridiagonal})
    dropzeros!(sparse(A))
end

"""
    unwrap(A::AbstractMatrix)

In case A is a wrapper type (`SubArray, Symmetric, Adjoint, SubArray, Triangular, Tridiagonal`, etc.)
convert to `Matrix` or `SparseMatrixCSC`, depending on final storage type of A.
For other types return A itself.
"""
unwrap(A::Any) = A
unwrap(A::AbstractMatrix) = iswrsparse(A) ? convert(SparseMatrixCSC, A) : convert(Array, A)

# For pure sparse matrices and vectors return A.
# For wrapped sparse matrices or vectors convert to SparseMatrixCSC.
# Handle nested wrappers properly.
# Use abstract matrix fallback if A is not sparse.
function _sparsem(@nospecialize A::AbstractArray{Tv}) where Tv
    if iswrsparse(A)
        if depth(A) >= 1
            _sparsem(_sparsewrap(A))
        else
            A
        end
    else
        # explicitly call abstract matrix fallback using getindex(A,...)
        invoke(SparseMatrixCSC{Tv,Int}, Tuple{AbstractMatrix}, A)
    end
end

_sparsem(A::AbstractSparseMatrix) = A
_sparsem(A::AbstractSparseVector) = A

# Transpose/Adjoint of sparse vector (returning sparse matrix)
function _sparsem(A::Union{Transpose{<:Any,<:AbstractSparseVector},Adjoint{<:Any,<:AbstractSparseVector}})
    B = parent(A)
    n = length(B)
    Ti = eltype(nonzeroinds(B))
    fadj = A isa Transpose ? transpose : adjoint
    colptr = fill!(Vector{Ti}(undef, n + 1), 0)
    colptr[1] = 1
    colptr[nonzeroinds(B) .+ 1] .= 1
    cumsum!(colptr, colptr)
    rowval = fill!(similar(nonzeroinds(B)), 1)
    nzval = fadj.(nonzeros(B))
    SparseMatrixCSC(1, n, colptr, rowval, nzval)
end

function _sparsem(A::Union{Transpose{<:Any,<:AbstractSparseMatrixCSC},Adjoint{<:Any,<:AbstractSparseMatrixCSC}})
    ftranspose(parent(A), A isa Transpose ? transpose : adjoint)
end

# Symmetric/Hermitian of sparse matrix
_sparsem(A::SparseMatrixCSCSymmHerm) = _sparsem(A.uplo == 'U' ? nzrangeup : nzrangelo, A)
# Triangular of sparse matrix
_sparsem(A::UpperTriangular{T,<:AbstractSparseMatrix}) where T = triu(A.data)
_sparsem(A::LowerTriangular{T,<:AbstractSparseMatrix}) where T = tril(A.data)
# view of sparse matrix
_sparsem(S::SubArray{<:Any,2,<:AbstractSparseMatrixCSC}) = getindex(S.parent,S.indices...)

# 4 cases: (Symmetric|Hermitian) variants (:U|:L)
function _sparsem(fnzrange::Function, sA::SparseMatrixCSCSymmHerm{Tv}) where {Tv}
    A = sA.data
    rowval = rowvals(A)
    nzval = nonzeros(A)
    m, n = size(A)
    Ti = eltype(rowval)
    fadj = sA isa Symmetric ? transpose : adjoint
    newcolptr = Vector{Ti}(undef, n+1)
    diagmap = fadj == transpose ? identity : real

    newcolptr[1] = 1
    colrange = fnzrange === nzrangeup ? (1:n) : (n:-1:1)
    @inbounds for j = colrange
        r = fnzrange(A, j); r1 = r.start; r2 = r.stop
        newcolptr[j+1] = r2 - r1 + 1
        for k = r1:r2
            row = rowval[k]
            if row != j
                newcolptr[row+1] += 1
            end
        end
    end
    cumsum!(newcolptr, newcolptr)
    nz = newcolptr[n+1] - 1
    newrowval = Vector{Ti}(undef, nz)
    newnzval = Vector{Tv}(undef, nz)
    @inbounds for j = 1:n
        newk = newcolptr[j]
        for k = fnzrange(A, j)
            i = rowval[k]
            nzv = nzval[k]
            if i != j
                newrowval[newk] = i
                newnzval[newk] = nzv
                newk += 1
                ni = newcolptr[i]
                newrowval[ni] = j
                newnzval[ni] = fadj(nzv)
                newcolptr[i] = ni + 1
            else
                newrowval[newk] = i
                newnzval[newk] = diagmap(nzv)
                newk += 1
            end
        end
        newcolptr[j] = newk
    end
    _sparse_gen(m, n, newcolptr, newrowval, newnzval)
end

# 2 cases: Unit(Upper|Lower)Triangular{Tv,AbstractSparseMatrixCSC}
function _sparsem(A::AbstractTriangularSparse{Tv}) where Tv
    S = A.data
    rowval = rowvals(S)
    nzval = nonzeros(S)
    m, n = size(S)
    Ti = eltype(rowval)
    fnzrange = A isa Union{UpperTriangular,UnitUpperTriangular} ? nzrangeup : nzrangelo
    unit = A isa Union{UnitUpperTriangular,UnitLowerTriangular}
    nz = nnz(S) + n * unit
    newcolptr = Vector{Ti}(undef, n+1)
    newrowval = Vector{Ti}(undef, nz)
    newnzval = Vector{Tv}(undef, nz)
    newcolptr[1] = 1
    uplo = fnzrange == nzrangeup
    newk = 1
    @inbounds for j = 1:n
        newkk = newk
        if unit
            newk += !uplo
        end
        r = fnzrange(S, j); r1 = r.start; r2 = r.stop
        for k = r1:r2
            i = rowval[k]
            if i != j || i == j && !unit
                newrowval[newk] = i
                newnzval[newk] = nzval[k]
                newk += 1
            end
        end
        if unit
            uplo && (newkk = newk)
            newrowval[newkk] = j
            newnzval[newkk] = one(Tv)
            newk += uplo
        end
        newcolptr[j+1] = newk
    end
    nz = newcolptr[n+1] - 1
    resize!(newrowval, nz)
    resize!(newnzval, nz)
    SparseMatrixCSC(m, n, newcolptr, newrowval, newnzval)
end

# 8 cases: (Transpose|Adjoint){Tv,[Unit](Upper|Lower)Triangular}
function _sparsem(taA::Union{Transpose{Tv,<:AbstractTriangularSparse},
                             Adjoint{Tv,<:AbstractTriangularSparse}}) where {Tv}

    sA = taA.parent
    A = sA.data
    rowval = rowvals(A)
    nzval = nonzeros(A)
    m, n = size(A)
    Ti = eltype(rowval)
    fnzrange = sA isa Union{UpperTriangular,UnitUpperTriangular} ? nzrangeup : nzrangelo
    fadj = taA isa Transpose ? transpose : adjoint
    unit = sA isa Union{UnitUpperTriangular,UnitLowerTriangular}
    uplo = A isa Union{UpperTriangular,UnitUpperTriangular}

    newcolptr = Vector{Ti}(undef, n+1)
    fill!(newcolptr, unit)
    newcolptr[1] = 1
    @inbounds for j = 1:n
        for k = fnzrange(A, j)
            i = rowval[k]
            if i != j || i == j && !unit
                newcolptr[i+1] += 1
            end
        end
    end
    cumsum!(newcolptr, newcolptr)
    nz = newcolptr[n+1] - 1
    newrowval = Vector{Ti}(undef, nz)
    newnzval = Vector{Tv}(undef, nz)

    @inbounds for j = 1:n
        if !uplo && unit
            ni = newcolptr[j]
            newrowval[ni] = j
            newnzval[ni] = fadj(one(Tv))
            newcolptr[j] = ni + 1
        end
        for k = fnzrange(A, j)
            i = rowval[k]
            nzv = nzval[k]
            if i != j || i == j && !unit
                ni = newcolptr[i]
                newrowval[ni] = j
                newnzval[ni] = fadj(nzv)
                newcolptr[i] = ni + 1
            end
        end
        if uplo && unit
            ni = newcolptr[j]
            newrowval[ni] = j
            newnzval[ni] = fadj(one(Tv))
            newcolptr[j] = ni + 1
        end
    end
    _sparse_gen(n, m, newcolptr, newrowval, newnzval)
end

function _sparse_gen(m, n, newcolptr, newrowval, newnzval)
    @inbounds for j = n:-1:1
        newcolptr[j+1] = newcolptr[j]
    end
    newcolptr[1] = 1
    SparseMatrixCSC(m, n, newcolptr, newrowval, newnzval)
end

