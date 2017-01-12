# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

@test @__LINE__ == 5

include("test_sourcepath.jl")
thefname = "the fname!//\\&\1*"
include_string_test_func = include_string("include_string_test() = @__FILE__", thefname)
@test include_string_test_func() == Base.source_path()
@test include_string("Base.source_path()", thefname) == Base.source_path()
@test basename(@__FILE__) == "loading.jl"
@test isabspath(@__FILE__)

@test isdir(@__DIR__)
@test @__DIR__() == dirname(@__FILE__)

# Issue #5789 and PR #13542:
mktempdir() do dir
    cd(dir) do
        let true_filename = "cAsEtEsT.jl", lowered_filename="casetest.jl"
            touch(true_filename)
            @test Base.isfile_casesensitive(true_filename)
            @test !Base.isfile_casesensitive(lowered_filename)

            # check that case-sensitivity only applies to basename of a path:
            if isfile(lowered_filename) # case-insensitive filesystem
                mkdir("cAsEtEsT")
                touch(joinpath("cAsEtEsT", true_filename))
                @test Base.isfile_casesensitive(joinpath("casetest", true_filename))
                @test !Base.isfile_casesensitive(joinpath("casetest", lowered_filename))
            end
        end

        # Test Unicode normalization; pertinent for OS X
        let nfc_name = "\U00F4.jl"
            touch(nfc_name)
            @test Base.isfile_casesensitive(nfc_name)
        end
    end
end

empty!(LOAD_PATH)
push!(LOAD_PATH, @__DIR__)
let paddedname = "Atest_sourcepathZ"
    filename = SubString(paddedname, 2, length(paddedname)-1)
    @test Base.find_in_path(filename) == abspath("$(paddedname[2:end-1]).jl")
end

immutable CustomLoader
    path::String
end
push!(LOAD_PATH, CustomLoader("abc"))
let name = randstring(20)
    @test_throws ArgumentError Base.find_in_path(name)
    Base.load_hook(prefix::CustomLoader, name::String, found) = joinpath(prefix.path, name)
    @test Base.find_in_path(name) == joinpath("abc", name)
end
@test Base.find_in_path("test_sourcepath") == joinpath("abc", "test_sourcepath")
Base.load_hook(prefix::CustomLoader, name::String, found::String) = found
@test Base.find_in_path("test_sourcepath") == abspath("test_sourcepath.jl")
empty!(LOAD_PATH)
