# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.Test

# Tests for @__LINE__ inside and outside of macros
@test @__LINE__() == 6

macro macro_caller_lineno()
    line = current_location()::Int
    :($line)
end

@test @macro_caller_lineno() == @__LINE__

# @__LINE__ in a macro expands to the location of the macro caller
macro emit_LINE()
    quote
        @__LINE__
    end
end
@test @emit_LINE() == @__LINE__

# @__LINE__ expands to location of calling macro in a two-level macro expansion,
# not the top level.
macro nested_LINE_expansion()
    quote
        @emit_LINE()
    end
end
@test @nested_LINE_expansion() == @__LINE__()-3

# @__LINE__ ignores any macro in a multi-level expansion if there's no line
# nodes in the AST.
macro nested_LINE_expansion2()
    :(@emit_LINE())
end
@test @nested_LINE_expansion2() == @__LINE__()



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

struct CustomLoader
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
