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

function check_valid(f)
    jsn = try jsn2data(@eval include($f * ".jl"))
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
    "valid/spec-example-1.toml",
    "valid/spec-example-1-compact.toml",
    "valid/datetime/datetime.toml",
    "valid/comment/everywhere.toml",
    "valid/datetime/milliseconds.toml",
    "valid/datetime/timezone.toml",
    "valid/string/multiline-quotes.toml",
    "valid/string/multiline.toml",
    "valid/float/zero.toml", # this one has a buggy .json file
    "valid/string/escape-esc.toml",
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
    "invalid/control/comment-del.toml",
    "invalid/control/comment-lf.toml",
    "invalid/control/comment-null.toml",
    "invalid/control/comment-us.toml",
    "invalid/control/comment-cr.toml",
    "invalid/datetime/time-no-leads.toml",
    "invalid/control/multi-del.toml",
    "invalid/control/multi-lf.toml",
    "invalid/control/multi-null.toml",
    "invalid/control/multi-us.toml",
    "invalid/control/rawmulti-del.toml",
    "invalid/control/rawmulti-lf.toml",
    "invalid/control/rawmulti-null.toml",
    "invalid/control/rawmulti-us.toml",
    "invalid/control/rawstring-del.toml",
    "invalid/control/rawstring-lf.toml",
    "invalid/control/rawstring-null.toml",
    "invalid/control/rawstring-us.toml",
    "invalid/control/string-bs.toml",
    "invalid/control/string-del.toml",
    "invalid/control/string-lf.toml",
    "invalid/control/string-null.toml",
    "invalid/control/string-us.toml",
    "invalid/encoding/bad-utf8-in-comment.toml",
    "invalid/encoding/bad-utf8-in-string.toml",
    "invalid/key/multiline.toml",
    "invalid/table/append-with-dotted-keys-2.toml",
    "invalid/table/duplicate-key-dotted-table.toml",
    "invalid/table/duplicate-key-dotted-table2.toml",
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
