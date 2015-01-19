## Sparse matrix performance
include("../perfutil.jl")

include("getindex.jl")
sparse_getindex_perf()

include("fem.jl")
fem_perf()
