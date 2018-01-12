# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using SuiteSparse, SparseArrays

if Base.USE_GPL_LIBS
   include("umfpack.jl")
   include("cholmod.jl")
   include("spqr.jl")
end
