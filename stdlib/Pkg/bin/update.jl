#!/usr/bin/env julia

pushfirst!(LOAD_PATH, joinpath(@__DIR__, "..", "ext"))

include("loadmeta.jl")
include("utils.jl")
include("gitmeta.jl")
include("genstdlib.jl")
include("generate.jl")
