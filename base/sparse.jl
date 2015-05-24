# This file is a part of Julia. License is MIT: http://julialang.org/license

module Sparse

using Base: Func, AddFun, SubFun, OrFun, ConjFun, IdFun
using Base.Sort: Forward
using Base.LinAlg: AbstractTriangular

importall Base
importall Base.LinAlg
import Base.promote_eltype
import Base.@get!
import Base.Broadcast.eltype_plus, Base.Broadcast.broadcast_shape

export AbstractSparseArray, AbstractSparseMatrix, AbstractSparseVector,
       SparseVector, SparseMatrixCSC,
       blkdiag, dense, droptol!, dropzeros!, etree, issparse, nnz, nonzeros, nzrange,
       rowvals, sparse, sparsevec, spdiagm, speye, spones, sprand, sprandbool, sprandn,
       spzeros, symperm

include("sparse/abstractsparse.jl")
include("sparse/sparsevector.jl")
include("sparse/sparsematrix.jl")
include("sparse/csparse.jl")

include("sparse/linalg.jl")
if Base.USE_GPL_LIBS
    include("sparse/umfpack.jl")
    include("sparse/cholmod.jl")
    include("sparse/spqr.jl")
end

end # module SparseMatrix
