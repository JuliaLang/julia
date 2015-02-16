module SparseMatrix

using Base: NonTupleType
using Base.Sort: Forward
using Base.LinAlg: AbstractTriangular

importall Base
importall Base.LinAlg

export AbstractSparseArray, AbstractSparseMatrix, AbstractSparseVector, SparseMatrixCSC,
       blkdiag, dense, droptol!, dropzeros!, etree, issparse, nnz, nonzeros, nzrange,
       rowvals, sparse, sparsevec, spdiagm, speye, spones, sprand, sprandbool, sprandn,
       spzeros, symperm

include("sparse/abstractsparse.jl")
include("sparse/sparsematrix.jl")
include("sparse/csparse.jl")

include("sparse/linalg.jl")
include("sparse/umfpack.jl")
include("sparse/cholmod.jl")
include("sparse/spqr.jl")

end # module SparseMatrix
