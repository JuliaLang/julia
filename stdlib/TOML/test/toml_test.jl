# This file is a part of Julia. License is MIT: https://julialang.org/license

using TOML

using Test
using Dates

const jsnval = Dict{String,Function}(
    "string" =>identity,
    "float"    => (s -> Base.parse(Float64, s)),
    "integer"  => (s -> Base.parse(Int64, s)),
    "datetime" => (s -> Base.parse(DateTime, s, dateformat"yyyy-mm-ddTHH:MM:SSZ")),
    "array"    => (a -> map(jsn2data, a)),
    "bool"     => (b -> b == "true")
)

function jsn2data(jsn)
    if "type" in keys(jsn)
        jsnval[jsn["type"]](jsn["value"])
    elseif jsn isa Vector
        [jsn2data(v) for v in jsn]
    else
        Dict{String,Any}([k => jsn2data(v) for (k, v) in jsn])
    end
end


#########
# Valid #
#########

valid_test_folder = joinpath(@__DIR__, "testfiles", "valid")

function check_valid(f)
    fp = joinpath(valid_test_folder, f)
    jsn = jsn2data(@eval include($fp * ".jl"))
    tml = TOML.parsefile(fp * ".toml")
    return isequal(tml, jsn)
end

@testset "valid" begin

@test_broken check_valid("datetime-timezone")
@test_broken check_valid("datetime")
@test check_valid("example")
@test check_valid("implicit-and-explicit-after")
@test check_valid("implicit-and-explicit-before")
@test check_valid("implicit-groups")
@test check_valid("newline-crlf")
@test check_valid("newline-lf")

end


###########
# Invalid #
###########

invalid_test_folder = joinpath(@__DIR__, "testfiles", "invalid")

# TODO: Check error type
function check_invalid(f)
    fp = joinpath(invalid_test_folder, f)
    tml = TOML.tryparsefile(fp * ".toml")
    return tml isa TOML.Internals.ParserError
end
