include("sparse/abstractsparse.jl")

module Sparse

importall Base
import Base.NonTupleType, Base.float, Base.showarray, Base.dims2string

export SparseMatrixCSC, SparseCSC, SparseVector, SparseVecOrMat,
       blkdiag, dense, diag, diagm, droptol!, dropzeros!, etree, full, 
       getindex, ishermitian, issparse, issym, istril, istriu, 
       setindex!, sparse, sparsevec, spdiagm, speye, spones, 
       sprand, sprandbool, sprandn, spzeros, symperm, trace, tril, tril!, 
       triu, triu!, writemime, summary, showarray

include("sparse/sparsematrix.jl")
include("sparse/csparse.jl")

end # module Sparse
