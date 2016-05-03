# This file is a part of Julia. License is MIT: http://julialang.org/license

include("../perfutil.jl")

include("eig.jl")
include("factorizations.jl")

# The test below is not a perf test but it takes too long for the normal test suite.
# This tests a work around for a bug in LAPACK where the work space is truncated
# to a too small number because the number is too big to be represented as a Float32.
# See the calculation for lwork in our sgesdd wrapper in lapack.jl.
println("testing work space bug in sgesdd")
svd(rand(Float32, 9537, 9537))
