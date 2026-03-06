# This file is a part of Julia. License is MIT: https://julialang.org/license

using TOML

using Test
using Dates

testfiles = get_data()

const jsnval = Dict{String,Function}(
    "string" =>identity,
    "float"    => (s -> Base.parse(Float64, s)),
    "integer"  => (s -> Base.parse(Int64, s)),
    "datetime" => (s -> Base.parse(DateTime, endswith(s, 'Z') ? chop(s) : s)),
    "datetime-local" => (s -> Base.parse(DateTime, endswith(s, 'Z') ? chop(s) : s)),
    "date-local" => (s -> Base.parse(DateTime, endswith(s, 'Z') ? chop(s) : s)),
    "time-local" => (s -> Base.parse(Time, s)),
    "array"    => (a -> map(jsn2data, a)),
    "bool"     => (b -> b == "true")
)

function jsn2data(jsn)
    if jsn isa Dict && length(jsn) == 2 && haskey(jsn, "type") && jsn["type"] isa String && haskey(jsn, "value")
        jsnval[jsn["type"]](jsn["value"])
    elseif jsn isa Vector
        [jsn2data(v) for v in jsn]
    elseif jsn isa Dict
        Dict{String,Any}([k => jsn2data(v) for (k, v) in jsn])
    else
        jsn
    end
end


#########
# Valid #
#########

function check_valid(f)
    jsn = try jsn2data(JSONX.parsefile(f * ".json"))
    # Some files cannot be represented with julias DateTime (timezones)
    catch
        return false
    end
    tml = TOML.tryparsefile(f * ".toml")
    tml isa TOML.Internals.ParserError && return false
    return isequal(tml, jsn)
end

@testset "valid" begin

failures = [
    # Cannot represent timezone offsets with Julia DateTime
    "valid/comment/everywhere.toml",
    "valid/datetime/datetime.toml",
    "valid/datetime/edge.toml",
    "valid/datetime/milliseconds.toml",
    "valid/datetime/no-seconds.toml",
    "valid/datetime/timezone.toml",
    "valid/spec-1.0.0/offset-date-time-0.toml",
    "valid/spec-1.1.0/common-27.toml",
    "valid/spec-1.1.0/common-29.toml",
    "valid/spec-example-1-compact.toml",
    "valid/spec-example-1.toml",
    # TOML 1.1 features not yet fully supported
    "valid/spec-1.0.0/string-4.toml",
    "valid/spec-1.0.0/string-7.toml",
    "valid/spec-1.1.0/common-12.toml",
    "valid/spec-1.1.0/common-16.toml",
    "valid/spec-1.1.0/common-19.toml",
    "valid/string/ends-in-whitespace-escape.toml",
    "valid/string/hex-escape.toml",
    "valid/string/multiline-empty.toml",
    "valid/string/multiline-quotes.toml",
    "valid/string/multiline.toml",
    "valid/string/raw-multiline.toml",
]

n_files_valid = 0
valid_test_folder = joinpath(testfiles, "valid")
for (root, dirs, files) in walkdir(valid_test_folder)
    for f in files
        if endswith(f, ".toml")
            n_files_valid += 1
            file = joinpath(root, f)
            rel = relpath(file, testfiles)
            if Sys.iswindows()
                rel = replace(rel, '\\' => '/')
            end
            v = check_valid(splitext(file)[1])
            @test v broken=rel in failures
        end
    end
end
@test n_files_valid >= 100

end # testset


###########
# Invalid #
###########

# TODO: Check error type
function check_invalid(f)
    tml = try TOML.tryparsefile(f)
    catch
        return false
    end
    return tml isa TOML.Internals.ParserError
end

@testset "invalid" begin

failures = [
    "invalid/control/bare-cr.toml",
    "invalid/control/comment-cr.toml",
    "invalid/control/comment-del.toml",
    "invalid/control/comment-ff.toml",
    "invalid/control/comment-lf.toml",
    "invalid/control/comment-null.toml",
    "invalid/control/comment-us.toml",
    "invalid/control/multi-cr.toml",
    "invalid/control/multi-del.toml",
    "invalid/control/multi-lf.toml",
    "invalid/control/multi-null.toml",
    "invalid/control/multi-us.toml",
    "invalid/control/rawmulti-cr.toml",
    "invalid/control/rawmulti-del.toml",
    "invalid/control/rawmulti-lf.toml",
    "invalid/control/rawmulti-null.toml",
    "invalid/control/rawmulti-us.toml",
    "invalid/control/rawstring-cr.toml",
    "invalid/control/rawstring-del.toml",
    "invalid/control/rawstring-lf.toml",
    "invalid/control/rawstring-null.toml",
    "invalid/control/rawstring-us.toml",
    "invalid/control/string-bs.toml",
    "invalid/control/string-cr.toml",
    "invalid/control/string-del.toml",
    "invalid/control/string-lf.toml",
    "invalid/control/string-null.toml",
    "invalid/control/string-us.toml",
    "invalid/datetime/no-secs.toml",
    "invalid/encoding/bad-codepoint.toml",
    "invalid/integer/invalid-hex-03.toml",
    "invalid/encoding/bad-utf8-in-comment.toml",
    "invalid/encoding/bad-utf8-in-multiline-literal.toml",
    "invalid/encoding/bad-utf8-in-multiline.toml",
    "invalid/encoding/bad-utf8-in-string-literal.toml",
    "invalid/encoding/bad-utf8-in-string.toml",
    "invalid/inline-table/linebreak-01.toml",
    "invalid/inline-table/linebreak-02.toml",
    "invalid/inline-table/linebreak-03.toml",
    "invalid/inline-table/linebreak-04.toml",
    "invalid/inline-table/trailing-comma.toml",
    "invalid/key/multiline-key-01.toml",
    "invalid/key/multiline-key-02.toml",
    "invalid/key/multiline-key-03.toml",
    "invalid/key/multiline-key-04.toml",
    "invalid/key/newline-04.toml",
    "invalid/key/newline-05.toml",
    "invalid/local-date/year-3digits.toml",
    "invalid/local-datetime/no-secs.toml",
    "invalid/local-time/no-secs.toml",
    "invalid/local-time/time-no-leads-01.toml",
    "invalid/spec-1.0.0/table-9-0.toml",
    "invalid/spec-1.0.0/table-9-1.toml",
    "invalid/spec-1.1.0/common-46-0.toml",
    "invalid/spec-1.1.0/common-46-1.toml",
    "invalid/string/basic-byte-escapes.toml",
    "invalid/table/duplicate-key-10.toml",
    "invalid/table/append-with-dotted-keys-02.toml",
    "invalid/table/append-with-dotted-keys-05.toml",
    "invalid/table/duplicate-key-04.toml",
    "invalid/table/duplicate-key-05.toml",
    "invalid/table/multiline-key-01.toml",
    "invalid/table/multiline-key-02.toml",
    "invalid/table/redefine-02.toml",
    "invalid/table/redefine-03.toml",
]

n_invalid = 0
invalid_test_folder = joinpath(testfiles, "invalid")
for (root, dirs, files) in walkdir(invalid_test_folder)
    for f in files
        if endswith(f, ".toml")
            n_invalid += 1
            file = joinpath(root, f)
            rel = relpath(file, testfiles)
            if Sys.iswindows()
                rel = replace(rel, '\\' => '/')
            end
            v = check_invalid(file)
            @test v broken=rel in failures
        end
    end
end
@test n_invalid > 50

end # testset
