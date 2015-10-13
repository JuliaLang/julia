# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

@test @__LINE__ == 5

include("test_sourcepath.jl")
thefname = "the fname!//\\&\0\1*"
@test include_string("include_string_test() = @__FILE__", thefname)() == Base.source_path()
@test include_string("Base.source_path()", thefname) == Base.source_path()
@test basename(@__FILE__) == "loading.jl"
@test isabspath(@__FILE__)

# Issue #5789 and PR #13542:
let true_filename = "cAsEtEsT.jl", lowered_filename="casetest.jl"
    touch(true_filename)
    @test Base.isfile_casesensitive(true_filename)
    @test !Base.isfile_casesensitive(lowered_filename)
    rm(true_filename)
end

# Test Unicode normalization; pertinent for OS X
let nfc_name = "\U00F4.jl"
    touch(nfc_name)
    @test Base.isfile_casesensitive(nfc_name)
    rm(nfc_name)
end
