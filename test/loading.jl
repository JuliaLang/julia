# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

include("test_sourcepath.jl")
thefname = "the fname!//\\&\0\1*"
@test include_string("@__FILE__", thefname) == "$thefname"
@test include_string("@__FILE__") == "none"
@test basename(@__FILE__) == "loading.jl"
@test isabspath(@__FILE__)
