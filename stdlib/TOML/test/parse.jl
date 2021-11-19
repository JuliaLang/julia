# This file is a part of Julia. License is MIT: https://julialang.org/license

using TOML, Test
using TOML: ParserError

@testset "TOML.(try)parse(file) entrypoints" begin
    dict = Dict{String,Any}("a" => 1)
    str = "a = 1"; invalid_str = "a"
    path, io = mktemp(); write(io, str); close(io)
    invalid_path, io = mktemp(); write(io, invalid_str); close(io)
    p = TOML.Parser()
    # TOML.parse
    @test TOML.parse(str) == TOML.parse(SubString(str)) ==
          TOML.parse(IOBuffer(str)) ==
          TOML.parse(p, str) == TOML.parse(p, SubString(str)) ==
          TOML.parse(p, IOBuffer(str)) == dict
    @test_throws ParserError TOML.parse(invalid_str)
    @test_throws ParserError TOML.parse(SubString(invalid_str))
    @test_throws ParserError TOML.parse(IOBuffer(invalid_str))
    @test_throws ParserError TOML.parse(p, invalid_str)
    @test_throws ParserError TOML.parse(p, SubString(invalid_str))
    @test_throws ParserError TOML.parse(p, IOBuffer(invalid_str))
    # TOML.tryparse
    @test TOML.tryparse(str) == TOML.tryparse(SubString(str)) ==
          TOML.tryparse(IOBuffer(str)) ==
          TOML.tryparse(p, str) == TOML.tryparse(p, SubString(str)) ==
          TOML.tryparse(p, IOBuffer(str)) == dict
    @test TOML.tryparse(invalid_str) isa ParserError
    @test TOML.tryparse(SubString(invalid_str)) isa ParserError
    @test TOML.tryparse(IOBuffer(invalid_str)) isa ParserError
    @test TOML.tryparse(p, invalid_str) isa ParserError
    @test TOML.tryparse(p, SubString(invalid_str)) isa ParserError
    @test TOML.tryparse(p, IOBuffer(invalid_str)) isa ParserError
    # TOML.parsefile
    @test TOML.parsefile(path) == TOML.parsefile(SubString(path)) ==
          TOML.parsefile(p, path) == TOML.parsefile(p, SubString(path)) == dict
    @test_throws ParserError TOML.parsefile(invalid_path)
    @test_throws ParserError TOML.parsefile(SubString(invalid_path))
    @test_throws ParserError TOML.parsefile(p, invalid_path)
    @test_throws ParserError TOML.parsefile(p, SubString(invalid_path))
    @test_throws ErrorException TOML.parsefile(homedir())
    @test_throws ErrorException TOML.parsefile(p, homedir())
    # TOML.tryparsefile
    @test TOML.tryparsefile(path) == TOML.tryparsefile(SubString(path)) ==
          TOML.tryparsefile(p, path) == TOML.tryparsefile(p, SubString(path)) == dict
    @test TOML.tryparsefile(invalid_path) isa ParserError
    @test TOML.tryparsefile(SubString(invalid_path)) isa ParserError
    @test TOML.tryparsefile(p, invalid_path) isa ParserError
    @test TOML.tryparsefile(p, SubString(invalid_path)) isa ParserError
    @test_throws ErrorException TOML.tryparsefile(homedir())
    @test_throws ErrorException TOML.tryparsefile(p, homedir())
end
