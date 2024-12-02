# This file is a part of Julia. License is MIT: https://julialang.org/license

# Convert test(set) results to a Buildkite-compatible JSON representation.
# Based on <https://buildkite.com/docs/test-analytics/importing-json#json-test-results-data-reference>.

module BuildkiteTestJSON

using Test
using Dates

export write_testset_json_files

# Bootleg JSON writer

"""
    json_repr(io::IO, value; kwargs...) -> Nothing

Obtain a JSON representation of `value`, and print it to `io`.

This may not be the best, most feature-complete, or fastest implementation.
However, it works for its intended purpose.
"""
function json_repr end

function json_repr(io::IO, val::String; indent::Int=0)
    print(io, '"')
    escape_string(io, val, ('"',))
    print(io, '"')
end
json_repr(io::IO, val::Integer; indent::Int=0) = print(io, val)
json_repr(io::IO, val::Float64; indent::Int=0) = print(io, val)
function json_repr(io::IO, val::AbstractVector; indent::Int=0)
    print(io, '[')
    for i in eachindex(val)
        print(io, '\n', ' '^(indent + 2))
        json_repr(io, val[i]; indent=indent+2)
        i == lastindex(val) || print(io, ',')
    end
    print(io, '\n', ' '^indent, ']')
end
function json_repr(io::IO, val::Dict; indent::Int=0)
    print(io, '{')
    for (i, (k, v)) in enumerate(pairs(val))
        print(io, '\n', ' '^(indent + 2))
        json_repr(io, string(k))
        print(io, ": ")
        json_repr(io, v; indent=indent+2)
        i === length(val) || print(io, ',')
    end
    print(io, '\n', ' '^indent, '}')
end
json_repr(io::IO, val::Any; indent::Int=0) = json_repr(io, string(val))

# Test result processing

function result_dict(testset::Test.DefaultTestSet, prefix::String="")
    Dict{String, Any}(
        "id" => Base.UUID(rand(UInt128)),
        "scope" => join((prefix, testset.description), '/'),
        "history" => if !isnothing(testset.time_end)
            Dict{String, Any}(
                "start_at" => testset.time_start,
                "end_at" => testset.time_end,
                "duration" => testset.time_end - testset.time_start)
        else
            Dict{String, Any}("start_at" => testset.time_start, "duration" => 0.0)
        end)
end

function result_dict(result::Test.Result)
    file, line = if !hasproperty(result, :source) || isnothing(result.source)
        "unknown", 0
    else
        something(result.source.file, "unknown"), result.source.line
    end
    status = if result isa Test.Pass && result.test_type === :skipped
        "skipped"
    elseif result isa Test.Pass
        "passed"
    elseif result isa Test.Fail || result isa Test.Error
        "failed"
    else
        "unknown"
    end
    data = Dict{String, Any}(
        "name" => "$(result.test_type): $(result.orig_expr)",
        "location" => string(file, ':', line),
        "file_name" => file,
        "result" => status)
    add_failure_info!(data, result)
end

function add_failure_info!(data::Dict{String, Any}, result::Test.Result)
    if result isa Test.Fail
        data["failure_reason"] = if result.test_type === :test && !isnothing(result.data)
            "Evaluated: $(result.data)"
        elseif result.test_type === :test_throws_nothing
            "No exception thrown"
        elseif result.test_type === :test_throws_wrong
            "Wrong exception type thrown"
        else
            "unknown"
        end
    elseif result isa Test.Error
        data["failure_reason"] = if result.test_type === :test_error
            if occursin("\nStacktrace:\n", result.backtrace)
                err, trace = split(result.backtrace, "\nStacktrace:\n", limit=2)
                data["failure_expanded"] =
                    [Dict{String,Any}("expanded" => split(err, '\n'),
                                      "backtrace" => split(trace, '\n'))]
            end
            "Exception (unexpectedly) thrown during test"
        elseif result.test_type === :test_nonbool
            "Expected the expression to evaluate to a Bool, not a $(typeof(result.data))"
        elseif result.test_type === :test_unbroken
            "Expected this test to be broken, but it passed"
        else
            "unknown"
        end
    end
    data
end

function collect_results!(results::Vector{Dict{String, Any}}, testset::Test.DefaultTestSet, prefix::String="")
    common_data = result_dict(testset, prefix)
    result_offset = length(results) + 1
    result_counts = Dict{Tuple{String, String}, Int}()
    for (i, result) in enumerate(testset.results)
        if result isa Test.Result
            rdata = result_dict(result)
            rid = (rdata["location"], rdata["result"])
            if haskey(result_counts, rid)
                result_counts[rid] += 1
            else
                result_counts[rid] = 1
                push!(results, merge(common_data, rdata))
            end
        elseif result isa Test.DefaultTestSet
            collect_results!(results, result, common_data["scope"])
        end
    end
    # Modify names to hold `result_counts`
    for i in result_offset:length(results)
        result = results[i]
        rid = (result["location"], result["result"])
        if get(result_counts, rid, 0) > 1
            result["name"] = replace(result["name"], r"^([^:]):" =>
                SubstitutionString("\\1 (x$(result_counts[rid])):"))
        end
    end
    results
end

function write_testset_json_files(dir::String, testset::Test.DefaultTestSet)
    data = Dict{String, Any}[]
    collect_results!(data, testset)
    files = String[]
    # Buildkite is limited to 5000 results per file https://buildkite.com/docs/test-analytics/importing-json
    for (i, chunk) in enumerate(Iterators.partition(data, 5000))
        res_file = joinpath(dir, "results_$i.json")
        open(io -> json_repr(io, chunk), res_file, "w")
        push!(files, res_file)
    end
    return files
end

end
