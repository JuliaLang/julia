# This file is a part of Julia. License is MIT: https://julialang.org/license

@assert length(ARGS) == 1
embedded_cmd_path = abspath(ARGS[1])
include("embedding-signals-common-test.jl")
