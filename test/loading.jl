# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

# Tests for @__LINE__ inside and outside of macros
@test (@__LINE__) == 6

macro macro_caller_lineno()
    @test 9 == (@__LINE__) != __source__.line > 12
    return __source__.line
end

@test @macro_caller_lineno() == (@__LINE__) > 12

# @__LINE__ in a macro expands to the location of the macrocall in the source
# while __source__.line is the location of the macro caller
macro nested_LINE_expansion()
    return quote
        return (@emit_LINE, $(__source__.line))
    end
end
macro nested_LINE_expansion2()
    return :((@emit_LINE, $(__source__.line)))
end
macro emit_LINE()
    return quote
        (@__LINE__, $(__source__.line))
    end
end
@test (@emit_LINE) == ((@__LINE__) - 3, @__LINE__)
@test @nested_LINE_expansion() == ((@__LINE__() - 4, @__LINE__() - 12), @__LINE__())
@test @nested_LINE_expansion2() == ((@__LINE__() - 5, @__LINE__() - 9), @__LINE__())

loaded_files = String[]
push!(Base.include_callbacks, (mod::Module, fn::String) -> push!(loaded_files, fn))
include("test_sourcepath.jl")
@test length(loaded_files) == 1 && endswith(loaded_files[1], "test_sourcepath.jl")
pop!(Base.include_callbacks)
thefname = "the fname!//\\&\1*"
include_string_test_func = include_string(@__MODULE__, "include_string_test() = @__FILE__", thefname)
@test include_string_test_func() == thefname
@test include_string(@__MODULE__, "Base.source_path()", thefname) == Base.source_path()
@test basename(@__FILE__) == "loading.jl"
@test isabspath(@__FILE__)

@test isdir(@__DIR__)
@test @__DIR__() == dirname(@__FILE__)
let exename = `$(Base.julia_cmd()) --compiled-modules=yes --startup-file=no`,
    wd = sprint(show, abspath(pwd(), "")),
    s_dir = sprint(show, joinpath(realpath(tempdir()), ""))
    @test wd != s_dir
    @test readchomp(`$exename -E "@__DIR__" -i`) == wd
    @test readchomp(`$exename -E "cd(()->eval(:(@__DIR__)), $s_dir)" -i`) == s_dir
    @test readchomp(`$exename -E "@__DIR__"`) == wd # non-interactive
end

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
