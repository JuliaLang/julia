include("sparse/abstractsparse.jl")

module SparseMatrix

importall Base
importall Base.LinAlg

import Base.NonTupleType, Base.float, Base.Order, Base.Sort.Forward
import Base.transpose!, Base.ctranspose!
import Base.LinAlg.AbstractTriangular

export SparseMatrixCSC,
       blkdiag, dense, diag, diagm, droptol!, dropzeros!, etree, full,
       getindex, ishermitian, issparse, issym, istril, istriu, nnz,
       setindex!, sparse, sparsevec, spdiagm, speye, spones,
       sprand, sprandbool, sprandn, spzeros, symperm, trace, tril, tril!,
       triu, triu!, nonzeros, rowvals, nzrange

include("sparse/sparsematrix.jl")
include("sparse/csparse.jl")

include("sparse/linalg.jl")
include("sparse/umfpack.jl")
include("sparse/cholmod.jl")

end # module SparseMatrix
