# This file is a part of Julia. License is MIT: https://julialang.org/license

include("sparsedir/sparse.jl")
if Base.USE_GPL_LIBS
    include("sparsedir/umfpack.jl")
    include("sparsedir/cholmod.jl")
    include("sparsedir/spqr.jl")
end
