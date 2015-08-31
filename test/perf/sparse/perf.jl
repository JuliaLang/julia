# This file is a part of Julia. License is MIT: http://julialang.org/license

## Sparse matrix performance
include("../perfutil.jl")

include("getindex.jl")
sparse_getindex_perf()

include("fem.jl")
fem_perf()
