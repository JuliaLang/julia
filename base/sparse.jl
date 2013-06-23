include("sparse/abstractsparse.jl")

module SparseMatrix

importall Base
import Base.NonTupleType

export SparseMatrixCSC, 
       dense, diag, diagm, droptol!, dropzeros!, etree, full, 
       getindex, ishermitian, issparse, issym, istril, istriu, 
       setindex!, sparse, sparsevec, spdiagm, speye, spones, 
       sprand, sprandbool, sprandn, spzeros, trace, tril, tril!, 
       triu, triu!

include("sparse/sparsematrix.jl")
include("sparse/csparse.jl")

end # module SparseMatrix
