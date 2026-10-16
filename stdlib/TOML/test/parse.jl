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
    @test TOML.parse("a\t=1") == dict
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

# Minimal insertion-ordered dict to test the `dicttype` keyword argument
# without depending on OrderedCollections
struct OrderedTestDict <: AbstractDict{String, Any}
    keys::Vector{String}
    vals::Vector{Any}
end
OrderedTestDict() = OrderedTestDict(String[], Any[])
function Base.setindex!(d::OrderedTestDict, v, k)
    i = findfirst(==(k), d.keys)
    if i === nothing
        push!(d.keys, k); push!(d.vals, v)
    else
        d.vals[i] = v
    end
    return d
end
function Base.get(d::OrderedTestDict, k, default)
    i = findfirst(==(k), d.keys)
    return i === nothing ? default : d.vals[i]
end
function Base.get!(f::Union{Function, Type}, d::OrderedTestDict, k)
    i = findfirst(==(k), d.keys)
    i === nothing || return d.vals[i]
    return d[k] = f()
end
Base.haskey(d::OrderedTestDict, k) = findfirst(==(k), d.keys) !== nothing
Base.length(d::OrderedTestDict) = length(d.keys)
Base.iterate(d::OrderedTestDict, i=1) = i > length(d.keys) ? nothing : (d.keys[i] => d.vals[i], i + 1)

@testset "dicttype" begin
    str = """
    zebra = 1
    alpha = 2

    [server]
    port = 8080
    host = "localhost"

    [[fruits]]
    name = "apple"
    color = "red"

    [inline]
    t = {z = 1, a = 2}
    """
    for d in (TOML.parse(str; dicttype=OrderedTestDict),
              TOML.parse(IOBuffer(str); dicttype=OrderedTestDict),
              TOML.tryparse(str; dicttype=OrderedTestDict),
              TOML.parse(TOML.Parser{OrderedTestDict}(str)),
              TOML.parse(TOML.Parser{OrderedTestDict}(), str),
              TOML.parse(TOML.Parser{OrderedTestDict}(IOBuffer(str))))
        @test d isa OrderedTestDict
        @test collect(keys(d)) == ["zebra", "alpha", "server", "fruits", "inline"]
        @test d["server"] isa OrderedTestDict
        @test collect(keys(d["server"])) == ["port", "host"]
        @test d["fruits"][1] isa OrderedTestDict
        @test collect(keys(d["fruits"][1])) == ["name", "color"]
        @test d["inline"]["t"] isa OrderedTestDict
        @test collect(keys(d["inline"]["t"])) == ["z", "a"]
    end
    mktemp() do path, io
        write(io, str); close(io)
        @test TOML.parsefile(path; dicttype=OrderedTestDict) isa OrderedTestDict
        @test TOML.tryparsefile(path; dicttype=OrderedTestDict) isa OrderedTestDict
    end
    err = TOML.tryparse("a = 1\na = 2"; dicttype=OrderedTestDict)
    @test err isa ParserError
    @test err.table isa OrderedTestDict
    # parser reuse keeps the dict type
    p = TOML.Parser{OrderedTestDict}()
    @test TOML.parse(p, "a = 1") isa OrderedTestDict
    @test TOML.parse(p, "b = 2") isa OrderedTestDict
    @inferred TOML.parse(p, str)
    # `@inferred` types keyword values with `typeof`, so check the `Type{D}` kwarg method directly
    @test Base.infer_return_type(Core.kwcall, (NamedTuple{(:dicttype,), Tuple{Type{OrderedTestDict}}}, typeof(TOML.parse), String)) === OrderedTestDict
end
