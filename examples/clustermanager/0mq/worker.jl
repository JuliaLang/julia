# This file is a part of Julia. License is MIT: http://julialang.org/license

include("ZMQCM.jl")

start_worker(parse(Int,ARGS[1]), ARGS[2])
