using TOML

using Test
using Dates

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
    # Some files cannot be reprsented with julias DateTime (timezones)
    catch
        return false
    end
    tml = TOML.tryparsefile(f * ".toml")
    tml isa TOML.Internals.ParserError && return false
    return isequal(tml, jsn)
end

@testset "valid" begin

failures = [
    "testfiles/valid/spec-example-1-compact.toml",
    "testfiles/valid/spec-example-1.toml",
    "testfiles/valid/comment/tricky.toml",
    "testfiles/valid/datetime/datetime.toml",
    "testfiles/valid/datetime/milliseconds.toml",
    "testfiles/valid/datetime/timezone.toml",
    "testfiles/valid/float/zero.toml",
    "testfiles/valid/string/multiline-quotes.toml",
]

valid_test_folder = joinpath(@__DIR__, "testfiles", "valid")
for (root, dirs, files) in walkdir(valid_test_folder)
    for f in files
        if endswith(f, ".toml")
            file = joinpath(root, f)
            rel = relpath(file, @__DIR__)
            if Sys.iswindows()
                rel = replace(rel, '\\' => '/')
            end
            v = check_valid(splitext(file)[1])
            if rel in failures
                @test_broken v
            else
                @test v
            end
        end
    end
end

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
    "testfiles/invalid/control/comment-del.toml",
    "testfiles/invalid/control/comment-lf.toml",
    "testfiles/invalid/control/comment-null.toml",
    "testfiles/invalid/control/comment-us.toml",
    "testfiles/invalid/control/multi-del.toml",
    "testfiles/invalid/control/multi-lf.toml",
    "testfiles/invalid/control/multi-null.toml",
    "testfiles/invalid/control/multi-us.toml",
    "testfiles/invalid/control/rawmulti-del.toml",
    "testfiles/invalid/control/rawmulti-lf.toml",
    "testfiles/invalid/control/rawmulti-null.toml",
    "testfiles/invalid/control/rawmulti-us.toml",
    "testfiles/invalid/control/rawstring-del.toml",
    "testfiles/invalid/control/rawstring-lf.toml",
    "testfiles/invalid/control/rawstring-null.toml",
    "testfiles/invalid/control/rawstring-us.toml",
    "testfiles/invalid/control/string-bs.toml",
    "testfiles/invalid/control/string-del.toml",
    "testfiles/invalid/control/string-lf.toml",
    "testfiles/invalid/control/string-null.toml",
    "testfiles/invalid/control/string-us.toml",
    "testfiles/invalid/encoding/bad-utf8-in-comment.toml",
    "testfiles/invalid/encoding/bad-utf8-in-string.toml",
    "testfiles/invalid/integer/negative-bin.toml",
    "testfiles/invalid/integer/negative-hex.toml",
    "testfiles/invalid/integer/negative-hex.toml",
    "testfiles/invalid/integer/negative-oct.toml",
    "testfiles/invalid/integer/positive-bin.toml",
    "testfiles/invalid/integer/positive-hex.toml",
    "testfiles/invalid/integer/positive-oct.toml",
    "testfiles/invalid/key/multiline.toml",
]

invalid_test_folder = joinpath(@__DIR__, "testfiles", "invalid")
for (root, dirs, files) in walkdir(invalid_test_folder)
    for f in files
        if endswith(f, ".toml")
            file = joinpath(root, f)
            rel = relpath(file, @__DIR__)
            if Sys.iswindows()
                rel = replace(rel, '\\' => '/')
            end
            v = check_invalid(file)
            if rel in failures
                @test_broken v
            else
                @test v
            end
        end
    end
end

end # testset
