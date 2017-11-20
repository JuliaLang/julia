#!/usr/bin/env julia

unshift!(LOAD_PATH, joinpath(@__DIR__, "..", "ext"))

include("loadmeta.jl")
include("generate.jl")
