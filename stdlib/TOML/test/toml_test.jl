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

@test check_valid("array-empty")
@test check_valid("array-nospaces")
@test check_valid("array-string-quote-comma-2")
@test check_valid("array-string-quote-comma")
@test check_valid("array-string-with-comma")
@test check_valid("array-table-array-string-backslash")
@test check_valid("arrays-hetergeneous")
@test check_valid("arrays-nested")
@test check_valid("arrays")
@test check_valid("bool")
@test check_valid("comments-at-eof")
@test check_valid("comments-at-eof2")
@test check_valid("comments-everywhere")
@test_broken check_valid("datetime-timezone")
@test_broken check_valid("datetime")
@test check_valid("double-quote-escape")
@test check_valid("empty")
@test check_valid("escaped-escape")
@test check_valid("example")
@test check_valid("exponent-part-float")
@test check_valid("float-exponent")
@test check_valid("float-underscore")
@test check_valid("float")
@test check_valid("implicit-and-explicit-after")
@test check_valid("implicit-and-explicit-before")
@test check_valid("implicit-groups")
@test check_valid("inline-table-array")
@test check_valid("inline-table")
@test check_valid("integer-underscore")
@test check_valid("integer")
@test check_valid("key-equals-nospace")
@test check_valid("key-numeric")
@test check_valid("key-space")
@test check_valid("key-special-chars")
@test check_valid("keys-with-dots")
@test check_valid("long-float")
@test check_valid("long-integer")
@test check_valid("multiline-string")
@test check_valid("nested-inline-table-array")
@test check_valid("newline-crlf")
@test check_valid("newline-lf")
if Sys.iswindows() &&
    # Sometimes git normalizes the line endings
    contains(read(joinpath(valid_test_folder, "raw-multiline-string-win.toml"), String), '\r')
    @test check_valid("raw-multiline-string-win")
else
    @test check_valid("raw-multiline-string")
end
@test check_valid("raw-string")
@test check_valid("right-curly-brace-after-boolean")
@test check_valid("string-empty")
@test check_valid("string-escapes")
@test check_valid("string-nl")
@test check_valid("string-simple")
@test check_valid("string-with-pound")
@test check_valid("table-array-implicit")
@test check_valid("table-array-many")
@test check_valid("table-array-nest")
@test check_valid("table-array-one")
@test check_valid("table-array-table-array")
@test check_valid("table-empty")
@test check_valid("table-no-eol")
@test check_valid("table-sub-empty")
@test check_valid("table-whitespace")
@test check_valid("table-with-literal-string")
@test check_valid("table-with-pound")
@test check_valid("table-with-single-quotes")
@test check_valid("underscored-float")
@test check_valid("underscored-integer")
@test check_valid("unicode-escape")
@test check_valid("unicode-literal")

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

@test check_invalid("datetime-malformed-no-leads")
@test check_invalid("datetime-malformed-no-secs")
@test check_invalid("datetime-malformed-no-t")
@test check_invalid("datetime-malformed-with-milli")
@test check_invalid("duplicate-key-table")
@test check_invalid("duplicate-keys")
@test check_invalid("duplicate-tables")
@test check_invalid("empty-implicit-table")
@test check_invalid("empty-table")
@test check_invalid("float-leading-zero-neg")
@test check_invalid("float-leading-zero-pos")
@test check_invalid("float-leading-zero")
@test check_invalid("float-no-leading-zero")
@test check_invalid("float-no-trailing-digits")
@test check_invalid("float-underscore-after-point")
@test check_invalid("float-underscore-after")
@test check_invalid("float-underscore-before-point")
@test check_invalid("float-underscore-before")
@test check_invalid("inline-table-linebreak")
@test check_invalid("integer-leading-zero-neg")
@test check_invalid("integer-leading-zero-pos")
@test check_invalid("integer-leading-zero")
@test check_invalid("integer-underscore-after")
@test check_invalid("integer-underscore-before")
@test check_invalid("integer-underscore-double")
@test check_invalid("key-after-array")
@test check_invalid("key-after-table")
@test check_invalid("key-empty")
@test check_invalid("key-hash")
@test check_invalid("key-newline")
@test check_invalid("key-no-eol")
@test check_invalid("key-open-bracket")
@test check_invalid("key-single-open-bracket")
@test check_invalid("key-space")
@test check_invalid("key-start-bracket")
@test check_invalid("key-two-equals")
@test check_invalid("llbrace")
@test check_invalid("multi-line-inline-table")
@test check_invalid("multi-line-string-no-close")
@test check_invalid("rrbrace")
@test check_invalid("string-bad-byte-escape")
@test check_invalid("string-bad-codepoint")
@test check_invalid("string-bad-escape")
@test check_invalid("string-bad-slash-escape")
@test check_invalid("string-bad-uni-esc")
@test check_invalid("string-byte-escapes")
@test check_invalid("string-no-close")
@test check_invalid("table-array-implicit")
@test check_invalid("table-array-malformed-bracket")
@test check_invalid("table-array-malformed-empty")
@test check_invalid("table-empty")
@test check_invalid("table-nested-brackets-close")
@test check_invalid("table-nested-brackets-open")
@test check_invalid("table-whitespace")
@test check_invalid("table-with-pound")
@test check_invalid("text-after-array-entries")
@test check_invalid("text-after-integer")
@test check_invalid("text-after-string")
@test check_invalid("text-after-table")
@test check_invalid("text-before-array-separator")
@test check_invalid("text-in-array")
