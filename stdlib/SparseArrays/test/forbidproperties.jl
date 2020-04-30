# This file is a part of Julia. License is MIT: https://julialang.org/license

using SparseArrays
Base.getproperty(S::SparseMatrixCSC, ::Symbol) = error("use accessor function")
Base.getproperty(S::SparseVector, ::Symbol) = error("use accessor function")
