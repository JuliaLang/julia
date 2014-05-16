abstract AbstractSparseArray{Tv,Ti,N} <: AbstractArray{Tv,N}

typealias AbstractSparseVector{Tv,Ti} AbstractSparseArray{Tv,Ti,1}
typealias AbstractSparseMatrix{Tv,Ti} AbstractSparseArray{Tv,Ti,2}

issparse(A::AbstractArray) = false
issparse(S::AbstractSparseArray) = true

indtype{Tv,Ti}(S::AbstractSparseArray{Tv,Ti}) = Ti

const CSR = :csr
const CSC = :csc

abstract CompressedSparseMatrix{Tv,Ti,Ts} <: AbstractSparseMatrix{Tv,Ti}

