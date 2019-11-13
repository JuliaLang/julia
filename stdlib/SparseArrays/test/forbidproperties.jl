using SparseArrays
Base.getproperty(S::SparseMatrixCSC, ::Symbol) = error("use accessor function")
Base.getproperty(S::SparseVector, ::Symbol) = error("use accessor function")
