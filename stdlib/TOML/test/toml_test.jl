# This file is a part of Julia. License is MIT: https://julialang.org/license

using TOML

using Test
using Dates

include("jsonx.jl")

# Download the official toml-test suite and extract it for testing.

using Downloads
using Tar
using p7zip_jll

const url = "https://github.com/toml-lang/toml-test/archive/refs/tags/v2.1.0.tar.gz"
const version = "2.1.0"

# From Pkg
function exe7z()
    # If the JLL is available, use the wrapper function defined in there
    if p7zip_jll.is_available()
        return p7zip_jll.p7zip()
    end
    return Cmd([find7z()])
end

function find7z()
    name = "7z"
    Sys.iswindows() && (name = "$name.exe")
    for dir in (joinpath("..", "libexec"), ".")
        path = normpath(Sys.BINDIR::String, dir, name)
        isfile(path) && return path
    end
    path = Sys.which(name)
    path !== nothing && return path
    error("7z binary not found")
end

function get_data()
    tmp = mktempdir()
    path = joinpath(tmp, basename(url))
    retry(Downloads.download, delays=fill(10,5))(url, path)
    Tar.extract(`$(exe7z()) x $path -so`, joinpath(tmp, "testfiles"))
    return joinpath(tmp, "testfiles", "toml-test-$version", "tests")
end

const testfiles = get_data()

# Use the official TOML 1.1 file list to only test files relevant to our parser version
const toml_1_1_files = Set(readlines(joinpath(testfiles, "files-toml-1.1.0")))

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
    "valid/spec-1.1.0/common-27.toml",
    "valid/spec-1.1.0/common-29.toml",
    "valid/spec-example-1-compact.toml",
    "valid/spec-example-1.toml",
]

n_files_valid = 0
valid_test_folder = joinpath(testfiles, "valid")
for (root, dirs, files) in walkdir(valid_test_folder)
    for f in files
        if endswith(f, ".toml")
            file = joinpath(root, f)
            rel = relpath(file, testfiles)
            if Sys.iswindows()
                rel = replace(rel, '\\' => '/')
            end
            rel in toml_1_1_files || continue
            n_files_valid += 1
            v = check_valid(splitext(file)[1])
            @test v broken=rel in failures context=f
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
    # Bare CR (\r without \n) not yet rejected
    "invalid/control/bare-cr.toml",
    "invalid/control/comment-cr.toml",
    "invalid/control/multi-cr.toml",
    "invalid/control/rawmulti-cr.toml",
    "invalid/control/rawstring-cr.toml",
    "invalid/control/string-cr.toml",
    # Bad UTF-8 encoding not validated
    "invalid/encoding/bad-codepoint.toml",
    "invalid/encoding/bad-utf8-in-comment.toml",
    "invalid/encoding/bad-utf8-in-multiline-literal.toml",
    "invalid/encoding/bad-utf8-in-multiline.toml",
    "invalid/encoding/bad-utf8-in-string-literal.toml",
    "invalid/encoding/bad-utf8-in-string.toml",
]

n_invalid = 0
invalid_test_folder = joinpath(testfiles, "invalid")
for (root, dirs, files) in walkdir(invalid_test_folder)
    for f in files
        if endswith(f, ".toml")
            file = joinpath(root, f)
            rel = relpath(file, testfiles)
            if Sys.iswindows()
                rel = replace(rel, '\\' => '/')
            end
            rel in toml_1_1_files || continue
            n_invalid += 1
            v = check_invalid(file)
            @test v broken=rel in failures context=f
        end
    end
end
@test n_invalid > 50

end # testset
