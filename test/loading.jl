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

SAVED_LOAD_PATH = copy(LOAD_PATH)
empty!(LOAD_PATH)
dir = abspath(@__DIR__)
push!(LOAD_PATH, dir)

@test Base.find_in_path("test_sourcepath") == joinpath(dir, "test_sourcepath.jl")
@test Base.find_in_path(GenericString("test_sourcepath")) == joinpath(dir, "test_sourcepath.jl")
LOAD_PATH[end] = GenericString(LOAD_PATH[end])
@test Base.find_in_path("test_sourcepath") == joinpath(dir, "test_sourcepath.jl")

immutable CustomLoader
    path::String
end
push!(LOAD_PATH, CustomLoader("abc"))
let name = randstring(20)
    @test_throws ArgumentError Base.find_in_path(name, nothing)
    Base.load_hook(prefix::CustomLoader, name::String, found) = joinpath(prefix.path, name)
    @test Base.find_in_path(name, nothing) == joinpath("abc", name)
end
@test Base.find_in_path("test_sourcepath", nothing) == joinpath("abc", "test_sourcepath")
Base.load_hook(prefix::CustomLoader, name::String, found::String) = found
@test Base.find_in_path("test_sourcepath", nothing) == joinpath(dir, "test_sourcepath.jl")

empty!(LOAD_PATH)
append!(LOAD_PATH, SAVED_LOAD_PATH)
@test LOAD_PATH == SAVED_LOAD_PATH
