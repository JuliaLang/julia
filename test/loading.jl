# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

@test @__LINE__ == 5

include("test_sourcepath.jl")
thefname = "the fname!//\\&\0\1*"
@test include_string("include_string_test() = @__FILE__", thefname)() == Base.source_path()
@test include_string("Base.source_path()", thefname) == Base.source_path()
@test basename(@__FILE__) == "loading.jl"
@test isabspath(@__FILE__)
