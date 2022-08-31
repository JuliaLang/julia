# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using InteractiveUtils

@enum BCOption bc_default bc_on bc_off
opts = Base.JLOptions()
bc_opt = BCOption(opts.check_bounds)

# These tests act as a simple guard to ensure we don't break our pass pipeline
# They are NOT a promise of any kind of performance for any particular syntax
# Tests may be marked as broken at any time depending on the PR

# The file is organized into a few basic patterns of optimizations we
# want to occur, namely:
# Don't have a good example of what O3 should do, but should definitely be at least O2
include("./llvmpipeline_opt_2.jl")
