abstract AbstractSparseMatrix{Tv,Ti} <: AbstractMatrix{Tv}

issparse(A::AbstractArray) = false
issparse(S::AbstractSparseMatrix) = true

eltype{Tv}(S::AbstractSparseMatrix{Tv}) = Tv
indtype{Tv,Ti}(S::AbstractSparseMatrix{Tv,Ti}) = Ti
