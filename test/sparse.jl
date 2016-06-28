# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "sparse" begin
include("sparsedir/sparse.jl")
include("sparsedir/sparsevector.jl")
if Base.USE_GPL_LIBS
    include("sparsedir/umfpack.jl")
    include("sparsedir/cholmod.jl")
    include("sparsedir/spqr.jl")
end
end
