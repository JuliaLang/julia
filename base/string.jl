# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract DirectIndexString <: AbstractString

include("ascii.jl")
include("strings/types.jl")
include("strings/basic.jl")
include("strings/search.jl")
include("strings/util.jl")
include("strings/io.jl")
