# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract AbstractSparseArray{Tv,Ti,N} <: AbstractArray{Tv,N}

typealias AbstractSparseVector{Tv,Ti} AbstractSparseArray{Tv,Ti,1}
typealias AbstractSparseMatrix{Tv,Ti} AbstractSparseArray{Tv,Ti,2}

issparse(A::AbstractArray) = false
issparse(S::AbstractSparseArray) = true

indtype{Tv,Ti}(S::AbstractSparseArray{Tv,Ti}) = Ti
