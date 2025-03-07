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
        json_repr(io, val[i]; indent=indent + 2)
        i == lastindex(val) || print(io, ',')
    end
    print(io, '\n', ' '^indent, ']')
end
function json_repr(io::IO, val::Dict; indent::Int=0)
    print(io, '{')
    len = length(val)
    for (i, (k, v)) in enumerate(pairs(val))
        print(io, '\n', ' '^(indent + 2))
        json_repr(io, string(k))
        print(io, ": ")
        json_repr(io, v; indent=indent + 2)
        i == len || print(io, ',')
    end
    print(io, '\n', ' '^indent, '}')
end
json_repr(io::IO, val::Any; indent::Int=0) = json_repr(io, string(val))

# Test result processing

function result_dict(testset::Test.DefaultTestSet, prefix::String="")
    scope = if isempty(prefix)
        testset.description == "Overall" ? "" : testset.description
    else
        join((prefix, testset.description), '/')
    end
    data = Dict{String,Any}(
        "id" => Base.UUID(rand(UInt128)),
        "scope" => scope,
        "tags" => Dict{String,String}(
            "job_label" => get(ENV, "BUILDKITE_LABEL", "unknown"),
            "job_id" => get(ENV, "BUILDKITE_JOB_ID", "unknown"),
            "job_group" => get(ENV, "BUILDKITE_GROUP_LABEL", "unknown"),
            "os" => string(Sys.KERNEL),
            "arch" => string(Sys.ARCH),
            "julia_version" => string(VERSION),
            "testset" => testset.description,
        ),
        "history" => if !isnothing(testset.time_end)
            Dict{String,Any}(
                "start_at" => testset.time_start,
                "end_at" => testset.time_end,
                "duration" => testset.time_end - testset.time_start)
        else
            Dict{String,Any}("start_at" => testset.time_start, "duration" => 0.0)
        end)
    return data
end

# Test paths on runners are often in deep directories, so just make them contain enough information
# to be able to identify the file. Also convert Windows-style paths to Unix-style paths so tests can
# be grouped by file.
function generalize_file_paths(path::AbstractString)
    pathsep = Sys.iswindows() ? '\\' : '/'
    path = replace(path,
        string(Sys.STDLIB, pathsep) => "",
        string(normpath(Sys.BUILD_ROOT_PATH), pathsep) => "",
        string(dirname(Sys.BINDIR), pathsep) => ""
    )
    return Sys.iswindows() ? replace(path, "\\" => "/") : path
end

# passed, failed, skipped, or unknown
function get_status(result)
    if result isa Test.Pass && result.test_type === :skipped
        "skipped"
    elseif result isa Test.Broken
        "skipped" # buildkite don't have a "broken" status
    elseif result isa Test.Pass
        "passed"
    elseif result isa Test.Fail || result isa Test.Error
        "failed"
    else
        "unknown"
    end
end

function result_dict(result::Test.Result)
    file, line = if !hasproperty(result, :source) || isnothing(result.source)
        "unknown", 0
    else
        something(result.source.file, "unknown"), result.source.line
    end
    file = generalize_file_paths(string(file))

    status = get_status(result)

    result_show = sprint(show, result; context=:color => false)
    firstline = split(result_show, '\n')[1]
    primary_reason = split(firstline, " at ")[1]

    data = Dict{String,Any}(
        "name" => "$(primary_reason). Expression: $(result.orig_expr)",
        "location" => string(file, ':', line),
        "file_name" => file,
        "result" => status)

    job_label = replace(get(ENV, "BUILDKITE_LABEL", "job label not found"), r":\w+:\s*" => "")
    if result isa Test.Fail || result isa Test.Error
        data["failure_reason"] = generalize_file_paths(firstline) * " | $job_label"
        err_trace = split(result_show, "\nStacktrace:\n", limit=2)
        if length(err_trace) == 2
            err, trace = err_trace
            data["failure_expanded"] = [Dict{String,Any}("expanded" => split(err, '\n'), "backtrace" => split(trace, '\n'))]
        else
            data["failure_expanded"] = [Dict{String,Any}("expanded" => split(result_show, '\n'), "backtrace" => [])]
        end
    end
    return data
end

function collect_results!(results::Vector{Dict{String,Any}}, testset::Test.DefaultTestSet, prefix::String="")
    common_data = result_dict(testset, prefix)
    result_offset = length(results) + 1
    result_counts = Dict{Tuple{String,String},Int}()
    get_rid(rdata) = (rdata["location"], rdata["result"])
    for (i, result) in enumerate(testset.results)
        if result isa Test.Result
            rdata = result_dict(result)
            rid = get_rid(rdata)
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
        rid = get_rid(result)
        if get(result_counts, rid, 0) > 1
            result["name"] = replace(result["name"], r"^([^:]):" =>
                SubstitutionString("\\1 (x$(result_counts[rid])):"))
        end
    end
    return results
end

function write_testset_json_files(dir::String, testset::Test.DefaultTestSet)
    data = Dict{String,Any}[]
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
