#!/usr/bin/env julia

pushfirst!(LOAD_PATH, joinpath(@__DIR__, "..", "ext"))

include("loadmeta.jl")
include("generate.jl")
