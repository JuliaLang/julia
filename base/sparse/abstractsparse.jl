abstract AbstractSparseMatrix{Tv,Ti} <: StoredArray{Tv,2}

issparse(A::AbstractArray) = false
issparse(S::AbstractSparseMatrix) = true

indtype{Tv,Ti}(S::AbstractSparseMatrix{Tv,Ti}) = Ti
