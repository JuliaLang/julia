# This file is a part of Julia. License is MIT: https://julialang.org/license

using TOML

using Downloads
using Tar
using p7zip_jll
using Test
using Dates

include("jsonx.jl")

# Download the official toml-test suite and extract it for testing.

const version = "2.1.0"
const url = "https://github.com/toml-lang/toml-test/archive/refs/tags/v$(version).tar.gz"

function get_toml_test_data()
    tmp = mktempdir()
    path = joinpath(tmp, basename(url))
    retry(Downloads.download, delays=fill(10,5))(url, path)
    Tar.extract(`$(p7zip_jll.p7zip()) x $path -so`, joinpath(tmp, "testfiles"))
    return joinpath(tmp, "testfiles", "toml-test-$version", "tests")
end


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

const testfiles = get_toml_test_data()
const toml_1_1_files = Set(readlines(joinpath(testfiles, "files-toml-1.1.0")))

@testset "toml-test" begin

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

failures_valid = [
    # Cannot represent timezone offsets with Julia DateTime
    "valid/comment/everywhere.toml",
    "valid/datetime/datetime.toml",
    "valid/datetime/milliseconds.toml",
    "valid/datetime/no-seconds.toml",
    "valid/datetime/timezone.toml",
    "valid/spec-1.1.0/common-27.toml",
    "valid/spec-1.1.0/common-29.toml",
    "valid/spec-1.1.0/common-12.toml",
    "valid/spec-1.1.0/common-16.toml",
    "valid/spec-1.1.0/common-19.toml",
    "valid/spec-example-1-compact.toml",
    "valid/spec-example-1.toml",
    "valid/string/ends-in-whitespace-escape.toml",
    "valid/string/hex-escape.toml",
    "valid/string/multiline-empty.toml",
    "valid/string/multiline-quotes.toml",
    "valid/string/multiline.toml",
    "valid/string/raw-multiline.toml",
]

n_files_valid = 0
tested_valid = Set{String}()
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
            rel in toml_1_1_files || continue
            push!(tested_valid, rel)
            v = check_valid(splitext(file)[1])
            @test v broken=rel in failures_valid context = rel
        end
    end
end
@test n_files_valid >= 100
# Ensure no stale entries in the failures list
@assert failures_valid ⊆ tested_valid "stale entries in failures_valid: $(setdiff(failures_valid, tested_valid))"

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

failures_invalid = [
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
    "invalid/encoding/bad-codepoint.toml",
    "invalid/integer/invalid-hex-03.toml",
    "invalid/encoding/bad-utf8-in-comment.toml",
    "invalid/encoding/bad-utf8-in-multiline-literal.toml",
    "invalid/encoding/bad-utf8-in-multiline.toml",
    "invalid/encoding/bad-utf8-in-string-literal.toml",
    "invalid/encoding/bad-utf8-in-string.toml",
]

n_invalid = 0
tested_invalid = Set{String}()
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
            rel in toml_1_1_files || continue
            push!(tested_invalid, rel)
            v = check_invalid(file)
            @test v broken=rel in failures_invalid context=rel
        end
    end
end
@test n_invalid > 50
# Ensure no stale entries in the failures list
@assert failures_invalid ⊆ tested_invalid "stale entries in failures_invalid: $(setdiff(failures_invalid, tested_invalid))"

end # testset

end # testset
