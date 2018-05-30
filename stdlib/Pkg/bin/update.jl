#!/usr/bin/env julia
# This file is a part of Julia. License is MIT: https://julialang.org/license

pushfirst!(LOAD_PATH, joinpath(@__DIR__, "..", "ext"))

include("loadmeta.jl")
include("utils.jl")
include("gitmeta.jl")
include("genstdlib.jl")
include("generate.jl")
