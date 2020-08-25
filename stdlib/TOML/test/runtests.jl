using Test
using Dates

using TOML: TOML, parse, tryparse, ParserError, Internals, print

function roundtrip(data)
    mktemp() do file, io
        data_parsed = TOML.parse(data)
        TOML.print(io, data_parsed)
        close(io)
        data_roundtrip = TOML.parsefile(file)
        return isequal(data_parsed, data_roundtrip)
    end
end

include("readme.jl")
include("toml_test.jl")
include("values.jl")
include("invalids.jl")
include("error_printing.jl")
include("print.jl")

@inferred TOML.parse("foo = 3")